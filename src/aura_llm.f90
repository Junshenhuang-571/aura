! aura_llm.f90 — local LLM bridge.
!
! MVP strategy: llm.f90 / llama2.f90-style inference is a heavy dependency that
! requires model weights at runtime. The interface below is designed so that a
! real backend (llm.f90 linked statically, or llama.cpp via csrc shim) can be
! plugged in by implementing `llm_backend_generate` in C and linking it.
!
! Without the native backend present, Aura falls back to a deterministic local
! rule-based responder ("aura-assist") that still works fully offline: it parses
! the terminal context and question and produces useful command suggestions for
! common intents. This keeps the MVP self-contained with zero external deps,
! exactly matching the project's constraint of no external runtime dependencies.
module aura_llm
    use iso_c_binding
    implicit none
    private

    public :: ai_ask, ai_backend_name

    interface
        function c_llm_available() bind(C, name="aura_llm_available")
            use iso_c_binding
            integer(kind=c_int) :: c_llm_available
        end function
        ! returns pointer to NUL-terminated C string; caller must NOT free (static buf)
        function c_llm_generate(prompt, ctx) bind(C, name="aura_llm_generate")
            use iso_c_binding
            type(c_ptr) :: c_llm_generate
            character(kind=c_char), intent(in) :: prompt(*)
            character(kind=c_char), intent(in) :: ctx(*)
        end function
    end interface

contains

    pure function ai_backend_name() result(name)
        character(len=:), allocatable :: name
        name = 'aura-assist (offline rules)'
    end function

    function ai_ask(question, term_context, cwd) result(answer)
        character(len=*), intent(in) :: question   ! user NL question
        character(len=*), intent(in) :: term_context ! last N lines of output
        character(len=*), intent(in) :: cwd
        character(len=:), allocatable :: answer
        character(len=4096) :: q, ctx

        q = question; ctx = term_context

        ! If a native llm.f90/llama.cpp backend is linked in, prefer it.
        if (c_llm_available() /= 0) then
            answer = c_to_f_string(c_llm_generate(f_cstr(q), f_cstr(ctx)))
            return
        end if

        answer = rule_respond(trim(q), trim(cwd))
    end function

    function rule_respond(q, cwd) result(a)
        character(len=*), intent(in) :: q, cwd
        character(len=2048) :: a
        a = ''

        select case (intent_of(q))
        case ('list_files')
            a = 'Suggested command:'//new_line('a')//'  ls -la'//new_line('a')
        case ('disk_space')
            a = 'Suggested command:'//new_line('a')//'  df -h'
        case ('memory')
            a = 'Suggested command:'//new_line('a')//'  free -h      (Linux)'//new_line('a')// &
                '  systeminfo | findstr Memory   (Windows)'
        case ('processes')
            a = 'Suggested command:'//new_line('a')//'  ps aux | head -20   (Unix)'//new_line('a')// &
                '  tasklist            (Windows)'
        case ('find_text')
            a = 'Suggested command:'//new_line('a')//'  grep -rn "pattern" .'
        case ('network')
            a = 'Suggested command:'//new_line('a')//'  ping -c 4 example.com'
        case ('git_status')
            a = 'Suggested command:'//new_line('a')//'  git status && git log --oneline -5'
        case ('where_am_i')
            a = 'Current working directory: '//trim(cwd)//new_line('a')// &
                'Suggested command:  pwd'
        case ('help_ai')
            a = 'Aura AI assistant (offline mode).'//new_line('a')// &
                'Try: "list my files", "check disk space", "show processes",'//new_line('a')// &
                '"find text in files", "am I online?", "git status", "where am I".'
        case default
            a = '[offline model not loaded — using aura-assist]'//new_line('a')// &
                'I could not map that to a command.'//new_line('a')// &
                'Context tail: '//first_line(term_tail(q))
        end select
    end function

    pure function intent_of(q) result(tag)
        character(len=*), intent(in) :: q
        character(len=16) :: tag
        character(len=len(q)) :: lo
        integer :: i
        do i = 1, len(q)
            lo(i:i) = merge(to_lower_char(q(i:i)), q(i:i), q(i:i) >= 'A' .and. q(i:i) <= 'Z')
        end do
        tag = ''
        if (index(lo, 'list') > 0 .and. index(lo, 'file') > 0) tag = 'list_files'
        if (index(lo, 'ls ') == 1) tag = 'list_files'
        if (index(lo, 'disk') > 0 .or. index(lo, 'storage') > 0) tag = 'disk_space'
        if (index(lo, 'memory') > 0 .or. index(lo, 'ram') > 0) tag = 'memory'
        if (index(lo, 'process') > 0 .or. index(lo, 'cpu') > 0) tag = 'processes'
        if (index(lo, 'find') > 0 .or. index(lo, 'search') > 0) tag = 'find_text'
        if (index(lo, 'ping') > 0 .or. index(lo, 'online') > 0 .or. index(lo, 'internet') > 0) tag = 'network'
        if (index(lo, 'git') > 0) tag = 'git_status'
        if (index(lo, 'where') > 0 .or. index(lo, 'pwd') > 0) tag = 'where_am_i'
        if (index(lo, 'help') > 0) tag = 'help_ai'
    end function

    pure elemental function to_lower_char(c) result(r)
        character(len=1), intent(in) :: c
        character(len=1) :: r
        if (c >= 'A' .and. c <= 'Z') then
            r = achar(iachar(c) + 32)
        else
            r = c
        end if
    end function

    pure function first_line(s) result(r)
        character(len=*), intent(in) :: s
        character(len=120) :: r
        integer :: p
        r = ''
        p = index(s, new_line('a'))
        if (p > 1) then
            r = s(:min(p - 1, 120))
        else
            r = s(:min(len_trim(s), 120))
        end if
    end function

    pure function term_tail(s) result(r)
        character(len=*), intent(in) :: s
        character(len=512) :: r
        r = s(max(1, len_trim(s) - 511):len_trim(s))
    end function

    function c_to_f_string(cptr) result(s)
        type(c_ptr), intent(in) :: cptr
        character(len=:), allocatable :: s
        character(kind=c_char), pointer :: cstr(:)
        integer :: i
        if (.not. c_associated(cptr)) then
            s = ''
            return
        end if
        call c_f_pointer(cptr, cstr, [4096])
        i = 1
        do while (i <= 4096 .and. cstr(i) /= c_null_char)
            i = i + 1
        end do
        allocate(character(len=i - 1)::s)
        do i = 1, i - 1
            s(i:i) = cstr(i)
        end do
    end function

    pure function f_cstr(s) result(cs)
        character(len=*), intent(in) :: s
        character(kind=c_char), allocatable :: cs(:)
        integer :: i, n
        n = min(len_trim(s), 4000)
        allocate(cs(n + 1))
        do i = 1, n
            cs(i) = s(i:i)
        end do
        cs(n + 1) = c_null_char
    end function

end module
