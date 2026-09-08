! aura_workspace.f90 — workspace registry with per-workspace AI context.
! A workspace = a named project scope: its own cwd, its own tabs, and its own
! AI conversation context (system prompt + history). Persisted to JSON.
module aura_workspace
    use iso_fortran_env, only: i4 => int32
    use iso_c_binding, only: c_long_long
    use aura_config, only: config_dir_path
    use aura_session, only: term_session
    implicit none
    private

    integer, parameter, public :: MAX_MSGS = 64
    integer, parameter, public :: MAX_TABS = 8
    integer, parameter, public :: MAX_WORKSPACES = 16

    type, public :: ai_msg
        character(len=:), allocatable :: role
        character(len=:), allocatable :: content
    end type

    type, public :: ai_context
        character(len=:), allocatable :: system_prompt
        type(ai_msg), allocatable :: msgs(:)
        integer(i4) :: n_msgs = 0
    contains
        procedure :: add_message
        procedure :: clear_history
        procedure :: ctx_to_json => ai_ctx_to_json
        procedure :: ctx_from_json => ai_ctx_from_json
    end type

    type, public :: workspace
        character(len=:), allocatable :: name
        character(len=:), allocatable :: cwd
        character(len=:), allocatable :: remote_target
        integer(i4) :: remote_port = 22
        type(ai_context) :: ai
        character(len=512) :: tab_cwds(MAX_TABS)
        integer(i4) :: n_tabs = 0
        integer(i4) :: active_tab = 1
        ! per-workspace session state (tabs) — allocatable to avoid stack overflow
        type(term_session), allocatable :: sess(:)
        integer(c_long_long), allocatable :: handle(:)
        character(len=32), allocatable :: titles(:)
        integer(i4) :: n_sess = 0
        integer(i4) :: active = 1
        logical :: scroll_mode = .false.
        integer :: scroll_top = 1
    contains
        procedure :: ws_to_json => workspace_to_json
        procedure :: ws_from_json => workspace_from_json
    end type

    type, public :: workspace_registry
        type(workspace), pointer :: items(:)
        integer(i4) :: n = 0
        integer(i4) :: active = 1
    contains
        procedure :: remove
        procedure :: switch_to
        procedure :: current
        procedure :: save
        procedure :: load
    end type

    public :: default_workspace
    public :: reg_add, reg_add_remote
contains

    subroutine add_message(self, role, content)
        class(ai_context), intent(inout) :: self
        character(len=*), intent(in) :: role, content
        integer(i4) :: i
        if (.not. allocated(self%msgs)) allocate(self%msgs(MAX_MSGS))
        if (self%n_msgs >= MAX_MSGS) then
            do i = 2, MAX_MSGS - 1
                self%msgs(i) = self%msgs(i + 1)
            end do
            self%n_msgs = MAX_MSGS - 1
        end if
        self%n_msgs = self%n_msgs + 1
        self%msgs(self%n_msgs)%role = role
        self%msgs(self%n_msgs)%content = content
    end subroutine

    subroutine reg_add_remote(reg, name, target, cwd, port, idx)
        type(workspace_registry), intent(inout) :: reg
        character(len=*), intent(in) :: name, target, cwd
        integer, intent(in) :: port
        integer(i4), intent(out) :: idx
        call reg_add(reg, name, cwd, idx)
        if (idx < 1) return
        reg%items(idx)%remote_target = target
        reg%items(idx)%remote_port = max(1, port)
    end subroutine

    subroutine clear_history(self)
        class(ai_context), intent(inout) :: self
        self%n_msgs = 0
        if (allocated(self%msgs)) deallocate(self%msgs)
    end subroutine

    function ai_ctx_to_json(self) result(s)
        class(ai_context), intent(in) :: self
        character(len=:), allocatable :: s
        character(len=:), allocatable :: msg_esc
        integer(i4) :: i
        s = '{' // new_line('a') // '    "system": "' // json_escape(self%system_prompt) // '",'
        if (self%n_msgs > 0) then
            s = s // new_line('a') // '    "messages": ['
            do i = 1, self%n_msgs
                msg_esc = json_escape(self%msgs(i)%content)
                s = s // new_line('a') // '      {"role": "' // &
                    trim(self%msgs(i)%role) // '", "content": "' // msg_esc // '"}'
                if (i < self%n_msgs) s = s // ','
            end do
            s = s // new_line('a') // '    ]'
        end if
        s = s // new_line('a') // '  }'
    end function

    subroutine ai_ctx_from_json(self, text)
        class(ai_context), intent(inout) :: self
        character(len=*), intent(in) :: text
        self%n_msgs = 0
        if (.not. allocated(self%msgs)) allocate(self%msgs(MAX_MSGS))
        self%system_prompt = extract_json_string(text, 'system')
    end subroutine

    function workspace_to_json(self) result(s)
        class(workspace), intent(in) :: self
        character(len=:), allocatable :: s
        character(len=:), allocatable :: ai_json
        integer(i4) :: i
        ai_json = self%ai%ctx_to_json()
        s = '{' // new_line('a') // '  "name": "' // json_escape(self%name) // '",' // &
            new_line('a') // '  "cwd": "' // json_escape(self%cwd) // '",' // &
            new_line('a') // '  "remote_target": "' // json_escape(self%remote_target) // '",' // &
            new_line('a') // '  "remote_port": ' // int2str(self%remote_port) // ',' // &
            new_line('a') // '  "active_tab": ' // int2str(self%active_tab) // ',' // &
            new_line('a') // '  "tabs": ['
        do i = 1, self%n_tabs
            s = s // '"' // json_escape(self%tab_cwds(i)) // '"'
            if (i < self%n_tabs) s = s // ', '
        end do
        s = s // '],' // new_line('a') // '  "ai": ' // ai_json // &
            new_line('a') // '}'
    end function

    subroutine workspace_from_json(self, text)
        class(workspace), intent(inout) :: self
        character(len=*), intent(in) :: text
        self%name = extract_json_string(text, 'name')
        self%cwd = extract_json_string(text, 'cwd')
        self%remote_target = extract_json_string(text, 'remote_target')
        self%remote_port = extract_json_integer(text, 'remote_port', 22)
        self%n_tabs = 0
        self%active_tab = 1
        call self%ai%ctx_from_json(text)
    end subroutine

    subroutine reg_add(reg, name, cwd, idx)
        type(workspace_registry), intent(inout) :: reg
        character(len=*), intent(in) :: name, cwd
        integer(i4), intent(out) :: idx
        if (reg%n >= MAX_WORKSPACES) then
            idx = -1
            return
        end if
        reg%n = reg%n + 1
        if (.not. associated(reg%items)) allocate(reg%items(MAX_WORKSPACES))
        idx = reg%n
        reg%items(idx)%name = name
        reg%items(idx)%cwd = cwd
        reg%items(idx)%remote_target = ''
        reg%items(idx)%remote_port = 22
        reg%items(idx)%ai%system_prompt = default_system_prompt(name, cwd)
        reg%items(idx)%n_tabs = 0
        reg%items(idx)%active_tab = 1
        allocate(reg%items(idx)%sess(MAX_TABS))
        allocate(reg%items(idx)%handle(MAX_TABS))
        allocate(reg%items(idx)%titles(MAX_TABS))
    end subroutine

    subroutine remove(self, idx)
        class(workspace_registry), intent(inout) :: self
        integer(i4), intent(in) :: idx
        integer(i4) :: i
        if (idx < 1 .or. idx > self%n) return
        do i = idx, self%n - 1
            self%items(i) = self%items(i + 1)
        end do
        self%n = self%n - 1
        if (self%active > self%n) self%active = max(1, self%n)
    end subroutine

    subroutine switch_to(self, idx)
        class(workspace_registry), intent(inout) :: self
        integer(i4), intent(in) :: idx
        if (idx >= 1 .and. idx <= self%n) self%active = idx
    end subroutine

    function current(self) result(w)
        class(workspace_registry), intent(inout) :: self
        type(workspace), pointer :: w
        integer(i4) :: idx
        if (self%n == 0) call reg_add(self, 'default', '.', idx)
        w => self%items(self%active)
    end function

    subroutine save(self)
        class(workspace_registry), intent(in) :: self
        character(len=:), allocatable :: path, dir
        integer :: u, i
        dir = config_dir_path()
        call ensure_dir(trim(dir))
        path = trim(dir) // '/workspaces.json'
        open (newunit=u, file=path, status='replace', action='write')
        write (u, '(A)') '{'
        write (u, '(A)') '  "active": ' // int2str(self%active) // ','
        write (u, '(A)') '  "workspaces": ['
        do i = 1, self%n
            call write_indented(u, self%items(i)%ws_to_json(), 4)
            if (i < self%n) write (u, '(A)') ','
        end do
        write (u, '(A)') '  ]'
        write (u, '(A)') '}'
        close (u)
    end subroutine

    subroutine load(self)
        class(workspace_registry), intent(inout) :: self
        character(len=:), allocatable :: path, dir, content
        integer :: u, ios, p, q, depth, i
        character(len=512) :: buf
        dir = config_dir_path()
        path = trim(dir) // '/workspaces.json'
        open (newunit=u, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        content = ''
        do
            read (u, '(A)', iostat=ios) buf
            if (ios /= 0) exit
            content = content // trim(buf) // new_line('a')
        end do
        close (u)
        self%n = 0
        if (.not. associated(self%items)) allocate(self%items(MAX_WORKSPACES))
        p = index(content, '"active"')
        if (p > 0) then
            p = index(content(p + 8:), ':')
            if (p > 0) then
                p = p + 8 + p - 1
                q = verify(content(p:), '0123456789')
                if (q > 1) read (content(p:p + q - 2), *) self%active
            end if
        end if
        p = index(content, '"workspaces"')
        if (p == 0) return
        p = index(content(p:), '[')
        if (p == 0) return
        p = p + p - 1 + 1
        depth = 0
        q = p
        do i = p, len(content)
            if (content(i:i) == '{') then
                if (depth == 0) q = i
                depth = depth + 1
            else if (content(i:i) == '}') then
                depth = depth - 1
                if (depth == 0) then
                    self%n = self%n + 1
                    call self%items(self%n)%ws_from_json(content(q:i))
                end if
            end if
        end do
        if (self%n == 0) self%active = 1
    end subroutine

    function default_system_prompt(name, cwd) result(s)
        character(len=*), intent(in) :: name, cwd
        character(len=:), allocatable :: s
        s = 'You are Aura, an AI assistant working in the "' // trim(name) // &
            '" workspace at ' // trim(cwd) // '. Help the user with terminal commands ' // &
            'and coding tasks scoped to this project.'
    end function

    function int2str(i) result(s)
        integer(i4), intent(in) :: i
        character(len=16) :: s
        write (s, '(I0)') i; s = adjustl(s)
    end function

    function json_escape(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer :: i
        r = ''
        do i = 1, len(s)
            select case (s(i:i))
            case ('\'); r = r // '\\'
            case ('"'); r = r // '\"'
            case (new_line('a')); r = r // '\n'
            case default; r = r // s(i:i)
            end select
        end do
    end function

    function json_unescape(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer :: i
        r = ''; i = 1
        do while (i <= len(s))
            if (s(i:i) == '\' .and. i < len(s)) then
                i = i + 1
                select case (s(i:i))
                case ('n'); r = r // new_line('a')
                case default; r = r // s(i:i)
                end select
            else
                r = r // s(i:i)
            end if
            i = i + 1
        end do
    end function

    function extract_json_string(text, key) result(val)
        character(len=*), intent(in) :: text, key
        character(len=:), allocatable :: val
        integer :: p, q
        val = ''
        p = index(text, '"' // trim(key) // '"')
        if (p == 0) return
        p = p + len_trim(key) + 2
        p = index(text(p:), ':')
        if (p == 0) return
        p = p + p - 1 + 1
        p = index(text(p:), '"')
        if (p == 0) return
        p = p + p - 1 + 1
        q = index(text(p:), '"')
        if (q == 0) return
        val = json_unescape(text(p:p + q - 2))
    end function

    function extract_json_integer(text, key, fallback) result(value)
        character(len=*), intent(in) :: text, key
        integer, intent(in) :: fallback
        integer :: value, p, start, q, ios
        character(len=32) :: digits
        value = fallback
        p = index(text, '"' // trim(key) // '"')
        if (p == 0) return
        start = p
        p = index(text(start:), ':')
        if (p == 0) return
        p = start + p
        do while (p <= len(text) .and. text(p:p) == ' ')
            p = p + 1
        end do
        q = p
        do while (q <= len(text) .and. text(q:q) >= '0' .and. text(q:q) <= '9')
            q = q + 1
        end do
        if (q <= p) return
        digits = ''
        digits(:min(len(digits), q - p)) = text(p:p + min(len(digits), q - p) - 1)
        read (digits, *, iostat=ios) value
        if (ios /= 0) value = fallback
    end function

    subroutine ensure_dir(d)
        character(len=*), intent(in) :: d
        call execute_command_line('mkdir -p "' // trim(d) // '"', wait=.false.)
    end subroutine

    subroutine write_indented(u, json, indent)
        integer, intent(in) :: u, indent
        character(len=*), intent(in) :: json
        character(len=512) :: buf
        integer :: i, start, slen
        character(len=16) :: pad
        pad = repeat(' ', indent)
        slen = len_trim(json); start = 1
        do while (start <= slen)
            i = index(json(start:), new_line('a'))
            if (i == 0) then
                buf = pad // json(start:slen)
                write (u, '(A)') trim(buf); exit
            else
                buf = pad // json(start:start + i - 2)
                write (u, '(A)') trim(buf); start = start + i
            end if
        end do
    end subroutine

    function default_workspace() result(w)
        type(workspace) :: w
        w%name = 'default'; w%cwd = '.'; w%remote_target = ''; w%remote_port = 22
        w%ai%system_prompt = default_system_prompt('default', '.')
        w%n_tabs = 0; w%active_tab = 1
    end function

end module aura_workspace
