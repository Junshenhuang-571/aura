! aura_llm.f90 — local LLM bridge.
!
! Three backends, all fully offline-capable:
!   1. native  — pure-Fortran inference via llama2_mod (llm.f90 / llama2.c port).
!                Zero external deps; model weights bundled in models/.
!   2. ollama  — local OpenAI-compatible server at 127.0.0.1:11434 (no cloud).
!   3. rule    — deterministic offline responder (aura-assist), always works.
!
! ai_ask_native / ai_ask_rule are the building blocks; aura_ai routes between
! them (and ollama) per the configured backend. The native generate entry
! points are bind(C) so they remain callable from C if a C shim is desired.
module aura_llm
    use iso_c_binding
    use llama2_mod
    implicit none
    private

    public :: ai_ask, ai_ask_native, ai_ask_rule, ai_backend_name, ai_init_native

    ! static buffer returned to C callers of aura_llm_generate
    character(kind=c_char), save, target :: g_out(16384)
    character(len=512) :: g_model = 'models/stories15M.bin'
    character(len=512) :: g_tok   = 'models/tokenizer.bin'

contains

    ! configure which model/tokenizer the native engine loads
    subroutine ai_init_native(model, tok)
        character(len=*), intent(in) :: model, tok
        g_model = model
        g_tok = tok
        call set_native_paths(model, tok)
    end subroutine

    pure function ai_backend_name() result(name)
        character(len=:), allocatable :: name
        name = 'aura-assist (offline rules)'
    end function

    ! ----- native (pure-Fortran) backend -----
    function ai_ask_native(question, ctx) result(answer)
        character(len=*), intent(in) :: question, ctx
        character(len=:), allocatable :: answer
        character(len=:), allocatable :: out
        call generate_text(question, 200, out)
        if (len_trim(out) == 0) then
            answer = '[native model not loaded]'
        else
            answer = out
        end if
    end function

    ! ----- rule (offline, always works) backend -----
    function ai_ask_rule(q, cwd) result(a)
        character(len=*), intent(in) :: q, cwd
        character(len=:), allocatable :: a
        a = rule_respond(trim(q), trim(cwd))
    end function

    ! ai_ask: prefer native if a model file is present, else rule.
    ! (aura_ai may override routing for ollama / auto modes.)
    function ai_ask(question, term_context, cwd) result(answer)
        character(len=*), intent(in) :: question
        character(len=*), intent(in) :: term_context
        character(len=*), intent(in) :: cwd
        character(len=:), allocatable :: answer
        logical :: ex
        inquire (file=trim(g_model), exist=ex)
        if (ex) then
            answer = ai_ask_native(question, term_context)
        else
            answer = ai_ask_rule(question, cwd)
        end if
    end function

    ! ===== bind(C) entry points (for an optional C shim) =====
    function c_llm_available() bind(C, name="aura_llm_available") result(r)
        integer(kind=c_int) :: r
        logical :: ex
        inquire (file=trim(g_model), exist=ex)
        r = merge(1_c_int, 0_c_int, ex)
    end function

    function c_llm_generate(prompt, ctx) bind(C, name="aura_llm_generate") result(p)
        type(c_ptr) :: p
        character(kind=c_char), intent(in) :: prompt(*)
        character(kind=c_char), intent(in) :: ctx(*)
        character(len=:), allocatable :: q, c, out
        integer :: i, n
        q = cptr_to_fstr(prompt)
        c = cptr_to_fstr(ctx)
        call generate_text(q, 200, out)
        g_out = c_null_char
        n = min(len_trim(out), size(g_out) - 1)
        do i = 1, n
            g_out(i) = out(i:i)
        end do
        g_out(n + 1) = c_null_char
        p = c_loc(g_out)
    end function

    function cptr_to_fstr(cstr) result(s)
        character(kind=c_char), intent(in) :: cstr(*)
        character(len=:), allocatable :: s
        integer :: i
        i = 1
        do while (cstr(i) /= c_null_char)
            i = i + 1
        end do
        allocate (character(len=i - 1) :: s)
        do i = 1, i - 1
            s(i:i) = cstr(i)
        end do
    end function

    ! ===== rule responder (deterministic, offline) =====
    function rule_respond(q, cwd) result(a)
        character(len=*), intent(in) :: q, cwd
        character(len=:), allocatable :: a
        a = ''
        select case (intent_of(q))
        case ('list_files')
            a = 'Suggested command:'//new_line('a')//'  ls -la'
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
                'I could not map that to a command. Common intents:'//new_line('a')// &
                'list files, disk space, memory, processes, find text, network, git status, where am I.'
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

end module aura_llm
