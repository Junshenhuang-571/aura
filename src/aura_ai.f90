! aura_ai.f90 — AI drawer brain: routes between backends per config.
!   backend "auto"   -> ollama if reachable, else offline rules
!   backend "ollama" -> local Ollama HTTP, fallback to rules if unreachable
!   backend "native" -> pure-Fortran llm.f90 engine (aura_llm)
!   backend "rule"   -> deterministic offline responder
module aura_ai
    use iso_c_binding, only: c_int, c_char, c_null_char
    use aura_llm, only: ai_ask, ai_ask_native, ai_ask_rule, ai_init_native
    use aura_config, only: aura_cfg
    implicit none
    private

    public :: ai_query, ai_provider_name, ai_init

    integer, parameter :: RESP_SIZE = 65536
    character(len=64) :: g_host = '127.0.0.1'
    character(len=64) :: g_model = 'gpt-oss:20b'
    character(len=16) :: g_backend = 'auto'
    character(len=64) :: g_provider = 'aura-assist (offline rules)'
    type(aura_cfg), pointer :: g_cfg => null()

contains

    subroutine ai_init(cfg)
        type(aura_cfg), target, intent(in) :: cfg
        g_cfg => cfg
        g_backend = merge(trim(cfg%ai_backend), 'auto', len_trim(cfg%ai_backend) > 0)
        g_host = merge(trim(cfg%ollama_host), '127.0.0.1', len_trim(cfg%ollama_host) > 0)
        g_model = merge(trim(cfg%ollama_model), 'gpt-oss:20b', len_trim(cfg%ollama_model) > 0)
        call ai_init_native(trim(cfg%model_path), trim(cfg%tokenizer_path))
    end subroutine

    pure function ai_provider_name() result(s)
        character(len=:), allocatable :: s
        s = trim(g_provider)
    end function

    ! Ask the LLM. question = user text; ctx = last lines of terminal; cwd.
    ! Answer includes "CMD:" line with suggested command when applicable.
    function ai_query(question, ctx, cwd) result(answer)
        character(len=*), intent(in) :: question, ctx, cwd
        character(len=:), allocatable :: answer
        character(len=2048) :: sysprompt, usermsg
        character(len=8192) :: body
        character(len=RESP_SIZE) :: raw
        character(len=64) :: host
        integer(c_int) :: status
        integer :: blen, port

        host = g_host
        port = 11434

        select case (g_backend)
        case ('rule')
            answer = ai_ask_rule(question, cwd)
            g_provider = 'aura-assist (offline rules)'
            return
        case ('native')
            answer = ai_ask_native(question, ctx)
            g_provider = 'llm.f90 (native, pure Fortran)'
            return
        case ('ollama')
            answer = ollama_ask(question, ctx, cwd, host, port, status, raw, sysprompt, usermsg, body, blen)
            if (status == 200 .and. len_trim(answer) > 0) then
                g_provider = 'ollama:'//trim(g_model)//' @ '//trim(host)
            else
                answer = ai_ask_rule(question, cwd)
                g_provider = 'aura-assist (offline rules) [ollama unreachable]'
            end if
            return
        case default   ! 'auto'
            answer = ollama_ask(question, ctx, cwd, host, port, status, raw, sysprompt, usermsg, body, blen)
            if (status == 200 .and. len_trim(answer) > 0) then
                g_provider = 'ollama:'//trim(g_model)//' @ '//trim(host)
            else
                answer = ai_ask_rule(question, cwd)
                g_provider = 'aura-assist (offline rules) [ollama unreachable]'
            end if
            return
        end select
    end function

    function ollama_ask(question, ctx, cwd, host, port, status, raw, sysprompt, usermsg, body, blen) &
            result(answer)
        character(len=*), intent(in) :: question, ctx, cwd, host
        integer, intent(in) :: port
        integer(c_int), intent(out) :: status
        character(len=RESP_SIZE), intent(out) :: raw
        character(len=*), intent(inout) :: sysprompt, usermsg, body
        integer, intent(out) :: blen
        character(len=:), allocatable :: answer
        interface
            function aura_http_post_c(host, port, path, body, resp, resp_size) &
                    bind(C, name='aura_http_post')
                import c_int, c_char
                integer(kind=c_int) :: aura_http_post_c
                character(kind=c_char), intent(in) :: host(*), path(*), body(*)
                integer(kind=c_int), value :: port, resp_size
                character(kind=c_char) :: resp(*)
            end function
        end interface
        sysprompt = &
            'You are Aura, an assistant inside a terminal. Given recent terminal '// &
            'output and a question, reply EXACTLY in this format:'//new_line('a')// &
            'CMD: <one shell command that solves it, or nothing>'//new_line('a')// &
            'WHY: <one short sentence>'
        usermsg = 'Terminal context:'//new_line('a')//trim(ctx)// &
                  new_line('a')//'CWD: '//trim(cwd)// &
                  new_line('a')//'Question: '//trim(question)
        body = ''
        body = '{"model":"'//trim(g_model)//'","messages":['// &
               '{"role":"system","content":'//trim(json_escape_str(sysprompt))//'},'// &
               '{"role":"user","content":'//trim(json_escape_str(usermsg))//'}],'// &
               '"stream":false}'
        blen = len_trim(body)
        raw = ''
        status = aura_http_post_c(f_cstr(host), int(port, c_int), &
                                  f_cstr('/v1/chat/completions'), &
                                  f_cstr_len(body, blen), raw, int(RESP_SIZE, c_int))
        if (status == 200) then
            answer = extract_content(raw)
        else
            answer = ''
        end if
    end function

    function fallback(question, cwd) result(a)
        character(len=*), intent(in) :: question, cwd
        character(len=:), allocatable :: a
        a = '[offline - ollama not reachable]'//new_line('a')// &
            trim(ai_ask(question, '', cwd))
    end function

    ! Extract first "content":"..." string from chat-completion JSON.
    ! Uses an explicit length counter (out_len) instead of trim() so that
    ! spaces inside the reply are NOT stripped (trim() would eat a trailing
    ! space on the next appended character).
    function extract_content(raw) result(out)
        character(len=*), intent(in) :: raw
        character(len=4096) :: out
        integer :: p, i, out_len
        logical :: esc

        out = ''
        out_len = 0
        p = index(raw, '"content"')
        if (p == 0) return
        ! advance to opening quote of the value
        i = p + len('"content"')
        do while (i <= len_trim(raw) .and. raw(i:i) /= '"')
            i = i + 1
        end do
        if (i > len_trim(raw)) return
        i = i + 1                       ! first char inside the string
        esc = .false.
        do while (i <= len_trim(raw))
            if (esc) then
                select case (raw(i:i))
                case ('n')
                    if (out_len < 4090) then; out_len = out_len + 1; out(out_len:out_len) = new_line('a'); end if
                case ('t')
                    if (out_len < 4090) then; out_len = out_len + 1; out(out_len:out_len) = achar(9); end if
                case default
                    if (out_len < 4090) then; out_len = out_len + 1; out(out_len:out_len) = raw(i:i); end if
                end select
                esc = .false.
            else if (raw(i:i) == '\') then
                esc = .true.
            else if (raw(i:i) == '"') then
                return                  ! closing quote
            else
                if (out_len < 4090) then; out_len = out_len + 1; out(out_len:out_len) = raw(i:i); end if
            end if
            i = i + 1
        end do
    end function

    ! Escape a Fortran string into a JSON string literal (returns trimmed).
    pure function json_escape_str(sv) result(js)
        character(len=*), intent(in) :: sv
        character(len=6144) :: js
        integer :: i, o
        character(len=1) :: ch

        js = '"'
        o = 2
        do i = 1, min(len_trim(sv), 3000)
            ch = sv(i:i)
            select case (ch)
            case ('"')
                js(o:o) = '\'; js(o+1:o+1) = '"'; o = o + 2
            case ('\')
                js(o:o) = '\'; js(o+1:o+1) = '\'; o = o + 2
            case (achar(10))
                js(o:o) = '\'; js(o+1:o+1) = 'n'; o = o + 2
            case (achar(13))
                ! skip CR
            case default
                js(o:o) = ch; o = o + 1
            end select
            if (o > 6100) exit
        end do
        js(o:o) = '"'
        js(o+1:) = ' '
    end function

    pure function f_cstr(sv) result(cs)
        character(len=*), intent(in) :: sv
        character(kind=c_char) :: cs(len_trim(sv) + 1)
        integer :: i, n
        n = len_trim(sv)
        do i = 1, n
            cs(i) = sv(i:i)
        end do
        cs(n + 1) = c_null_char
    end function

    pure function f_cstr_len(sv, n) result(cs)
        character(len=*), intent(in) :: sv
        integer, intent(in) :: n
        character(kind=c_char) :: cs(n + 1)
        integer :: i
        do i = 1, n
            cs(i) = sv(i:i)
        end do
        cs(n + 1) = c_null_char
    end function

end module
