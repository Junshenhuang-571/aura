! aura_config.f90 — configuration management (~/.config/aura/config.json)
! MVP: minimal JSON key/value reader+writer for our own config schema.
! Format is identical to what json-fortran would read/write; swap in
! json-fortran later by replacing load()/save() internals only.
module aura_config
    use iso_fortran_env, only: i4 => int32
    implicit none
    private

    type, public :: aura_cfg
        character(len=:), allocatable :: shell_path      ! default shell
        character(len=:), allocatable :: model_path      ! local LLM weights
        character(len=:), allocatable :: tokenizer_path  ! tokenizer for ak models
        character(len=:), allocatable :: theme           ! colour theme name
        character(len=:), allocatable :: ai_provider     ! legacy: "local" | "openai"
        character(len=:), allocatable :: ai_backend      ! "auto"|"ollama"|"native"|"rule"
        character(len=:), allocatable :: model_format    ! "ak" | "gguf"
        character(len=:), allocatable :: ollama_host     ! e.g. 127.0.0.1
        character(len=:), allocatable :: ollama_model    ! e.g. gpt-oss:20b
        integer(i4) :: history_lines = 1000
    contains
        procedure :: load, save
        procedure, private :: defaults
    end type

    public :: config_dir_path

contains

    function config_dir_path() result(p)
        character(len=:), allocatable :: p
        character(len=512) :: home, appdata
        integer :: ios
        call get_environment_variable('HOME', home, status=ios)
        if (ios /= 0 .or. len_trim(home) == 0) then
            call get_environment_variable('APPDATA', appdata, status=ios)
            if (ios == 0 .and. len_trim(appdata) > 0) then
                p = trim(appdata) // '\aura'
            else
                p = '.aura'
            end if
        else
            if (index(get_environment('OS'), 'Windows') > 0) then
                p = trim(home)//'/.config/aura'
            else
                p = trim(home)//'/.config/aura'
            end if
        end if
    end function

    subroutine defaults(self)
        class(aura_cfg), intent(inout) :: self
        character(len=512) :: env
        integer :: ios
        self%shell_path = ''
        ! On Windows prefer cmd.exe; $SHELL from git-bash/msys points to a
        ! POSIX path that a native Windows CreateProcess cannot execute.
        call get_environment_variable('OS', env, status=ios)
        if (ios == 0 .and. index(env, 'Windows') > 0) then
            call get_environment_variable('COMSPEC', env, status=ios)
            if (ios == 0 .and. len_trim(env) > 0) then
                self%shell_path = trim(env) // ' /k'
            else
                self%shell_path = 'C:\Windows\System32\cmd.exe /k'
            end if
        else
            call get_environment_variable('SHELL', env, status=ios)
            if (ios == 0 .and. len_trim(env) > 0) then
                self%shell_path = trim(env)
            else
                self%shell_path = '/bin/bash'
            end if
        end if
        self%model_path = 'models/stories15M.bin'
        self%tokenizer_path = 'models/tokenizer.bin'
        self%theme = 'dark'
        self%ai_provider = 'local'
        self%ai_backend = 'auto'        ! ollama if reachable, else offline rules
        self%model_format = 'ak'
        self%ollama_host = '127.0.0.1'
        self%ollama_model = 'gpt-oss:20b'
        self%history_lines = 1000_i4
    end subroutine

    ! crude but robust flat-JSON reader: finds "key": "value" pairs
    subroutine load(self, path)
        class(aura_cfg), intent(inout) :: self
        character(len=*), intent(in), optional :: path
        character(len=1024) :: fname, line, key, val
        integer :: u, ios, p1, p2, p3
        logical :: exists

        call self%defaults()
        fname = trim(config_dir_path()) // merge('/config.json', '\config.json', get_sep_is_slash())
        if (present(path)) fname = path
        inquire (file=trim(fname), exist=exists)
        if (.not. exists) return     ! keep defaults

        open (newunit=u, file=trim(fname), status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(A)', iostat=ios) line
            if (ios /= 0) exit
            p1 = index(line, '"')
            if (p1 == 0) cycle
            p2 = index(line(p1 + 1:), '"') + p1
            if (p2 <= p1) cycle
            key = line(p1 + 1:p2 - 1)
            ! value starts after the colon; find its opening quote
            p3 = index(line(p2 + 1:), '"') + p2
            if (p3 <= p2) then
                val = ''
            else
                ! closing quote is the NEXT quote after the opening one
                p1 = index(line(p3 + 1:), '"') + p3
                if (p1 <= p3) then
                    val = ''     ! unterminated string — ignore
                else
                    val = line(p3 + 1:p1 - 1)
                    ! JSON unescape: only \\ -> \ and \" -> " ; a lone \ before
                    ! any other char is a Windows path separator — keep it.
                    p1 = 1
                    do
                        if (p1 > len_trim(val)) exit
                        p3 = index(val(p1:len_trim(val)), '\')
                        if (p3 == 0) exit
                        p3 = p1 + p3 - 1          ! absolute position of backslash
                        if (p3 < len_trim(val)) then
                            select case (val(p3 + 1:p3 + 1))
                            case ('\')
                                val = val(:p3)//val(p3 + 2:)
                            case ('"')
                                val = val(:p3)//val(p3 + 2:)
                                cycle
                            case default
                                p1 = p3 + 2        ! literal path backslash, skip it
                                continue
                            end select
                        else
                            exit                   ! trailing lone backslash: keep
                        end if
                        p1 = p3 + 1                ! advance past collapsed pair
                    end do
                end if
            end if
            select case (trim(key))
            case ('shell');   self%shell_path = trim(val)
            case ('model');   self%model_path = trim(val)
            case ('theme');   self%theme = trim(val)
            case ('provider');self%ai_provider = trim(val)
            case ('backend');self%ai_backend = trim(val)
            case ('tokenizer');self%tokenizer_path = trim(val)
            case ('format'); self%model_format = trim(val)
            case ('ollama_host');self%ollama_host = trim(val)
            case ('ollama_model');self%ollama_model = trim(val)
            case ('history_lines')
                read (val, *, iostat=ios) self%history_lines
                if (ios /= 0) self%history_lines = 1000_i4
            end select
        end do
        close (u)
    end subroutine

    pure logical function get_sep_is_slash()
        get_sep_is_slash = .true.
    end function

    subroutine save(self, path)
        class(aura_cfg), intent(inout) :: self
        character(len=*), intent(in), optional :: path
        character(len=1024) :: fname, dir
        integer :: u, ios

        fname = join_path(config_dir_path(), 'config.json')
        if (present(path)) fname = path
        dir = fname(1:max(index(fname, '/', back=.true.), index(fname, '\', back=.true.)))
        call mkdir_p(dir)

        open (newunit=u, file=trim(fname), status='replace', action='write', iostat=ios)
        if (ios /= 0) return
        write (u, '(A)') '{'
        write (u, '(A)') '  "shell": "'//json_escape(self%shell_path)//'",'
        write (u, '(A)') '  "model": "'//json_escape(self%model_path)//'",'
        write (u, '(A)') '  "tokenizer": "'//json_escape(self%tokenizer_path)//'",'
        write (u, '(A)') '  "theme": "'//json_escape(self%theme)//'",'
        write (u, '(A)') '  "provider": "'//json_escape(self%ai_provider)//'",'
        write (u, '(A)') '  "backend": "'//json_escape(self%ai_backend)//'",'
        write (u, '(A)') '  "format": "'//json_escape(self%model_format)//'",'
        write (u, '(A)') '  "ollama_host": "'//json_escape(self%ollama_host)//'",'
        write (u, '(A)') '  "ollama_model": "'//json_escape(self%ollama_model)//'",'
        write (u, '(A,I0,A)') '  "history_lines": ', self%history_lines, ''
        write (u, '(A)') '}'
        close (u)
    end subroutine

    pure function json_escape(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer :: i
        r = ''
        do i = 1, len_trim(s)
            select case (s(i:i))
            case ('"'); r = r//'\"'
            case ('\'); r = r//'\\'
            case default; r = r//s(i:i)
            end select
        end do
    end function

    function join_path(a, b) result(p)
        character(len=*), intent(in) :: a, b
        character(len=:), allocatable :: p
        p = trim(a)//'/'//trim(b)
    end function

    subroutine mkdir_p(dir)
        character(len=*), intent(in) :: dir
        character(len=1100) :: cmd
        cmd = 'mkdir -p "'//trim(dir)//'" 2>/dev/null || mkdir "'//trim(dir)//'" 2>nul'
        call execute_command_line(cmd, wait=.true.)
    end subroutine

    function get_environment(name) result(val)
        character(len=*), intent(in) :: name
        character(len=512) :: val
        integer :: ios
        call get_environment_variable(name, val, status=ios)
        if (ios /= 0) val = ''
    end function

end module aura_config
