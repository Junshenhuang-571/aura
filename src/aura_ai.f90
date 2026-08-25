! aura_ai.f90 — AI drawer brain: Ollama (OpenAI-compatible /chat/completions)
! with automatic rule-based fallback. Builds prompt from terminal context.
module aura_ai
    use iso_c_binding, only: c_int, c_char, c_null_char
    use aura_llm, only: ai_ask
    implicit none
    private

    public :: ai_query, ai_provider_name

    integer, parameter :: RESP_SIZE = 65536
    character(len=*), parameter :: DEF_HOST = '127.0.0.1'
    integer, parameter :: DEF_PORT = 11434
    character(len=*), parameter :: DEF_MODEL = 'gpt-oss:20b'

contains

    pure function ai_provider_name() result(s)
        character(len=:), allocatable :: s
        s = 'ollama:'//DEF_MODEL//' @ '//DEF_HOST
    end function

    ! Ask the LLM. question = user text; ctx = last lines of terminal; cwd.
    ! Answer includes "CMD:" line with suggested command when applicable.
    function ai_query(question, ctx, cwd) result(answer)
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

        character(len=*), intent(in) :: question, ctx, cwd
        character(len=:), allocatable :: answer
        character(len=2048) :: sysprompt, usermsg
        character(len=8192) :: body
        character(len=RESP_SIZE) :: raw
        character(len=64) :: host
        integer(c_int) :: status
        integer :: blen

        host = DEF_HOST

        sysprompt = &
            'You are Aura, an assistant inside a terminal. Given recent terminal '// &
            'output and a question, reply EXACTLY in this format:'//new_line('a')// &
            'CMD: <one shell command that solves it, or nothing>'//new_line('a')// &
            'WHY: <one short sentence>'

        usermsg = 'Terminal context:'//new_line('a')//trim(ctx)// &
                  new_line('a')//'CWD: '//trim(cwd)// &
                  new_line('a')//'Question: '//trim(question)

        body = ''
        body = '{"model":"'//DEF_MODEL//'","messages":['// &
               '{"role":"system","content":'//trim(json_escape_str(sysprompt))//'},'// &
               '{"role":"user","content":'//trim(json_escape_str(usermsg))//'}],'// &
               '"stream":false}'
        blen = len_trim(body)

        raw = ''
        status = aura_http_post_c(f_cstr(host), int(DEF_PORT, c_int), &
                                  f_cstr('/v1/chat/completions'), &
                                  f_cstr_len(body, blen), raw, int(RESP_SIZE, c_int))

        if (status == 200) then
            answer = extract_content(raw)
            if (len_trim(answer) == 0) answer = fallback(question, cwd)
        else
            answer = fallback(question, cwd)
        end if
    end function

    function fallback(question, cwd) result(a)
        character(len=*), intent(in) :: question, cwd
        character(len=:), allocatable :: a
        a = '[offline - ollama not reachable]'//new_line('a')// &
            trim(ai_ask(question, '', cwd))
    end function

    ! Extract first "content":"..." string from chat-completion JSON.
    function extract_content(raw) result(out)
        character(len=*), intent(in) :: raw
        character(len=4096) :: out
        integer :: p, i
        logical :: esc

        out = ''
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
                    out = trim(out)//new_line('a')
                case ('t')
                    out = trim(out)//achar(9)
                case default
                    out = trim(out)//raw(i:i)
                end select
                esc = .false.
            else if (raw(i:i) == '\') then
                esc = .true.
            else if (raw(i:i) == '"') then
                return                  ! closing quote
            else
                out = trim(out)//raw(i:i)
            end if
            if (len_trim(out) >= 4000) return
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
