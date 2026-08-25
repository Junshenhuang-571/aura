! e2e_final.f90 — full session-layer end-to-end test (PTY + ANSI + scrollback + AI)
program e2e_final
    use iso_fortran_env, only: i4 => int32, int32
    use iso_c_binding, only: c_long_long
    use aura_session
    use aura_pty
    use aura_llm
    implicit none

    type(term_session) :: sess
    integer(c_long_long) :: h
    integer(i4) :: rc
    character(len=:), allocatable :: ctx, answer
    logical :: ok

    ok = .true.

    ! 1. spawn cmd.exe through ConPTY
    call init_sess(sess)
    rc = pty_spawn('C:\Windows\System32\cmd.exe /k', 100, 30, h)
    if (rc /= 0) then
        print *, 'FAIL spawn'; stop 1
    end if
    sess%alive = .true.
    call drain_ms(sess, h, 2500)
    print *, '[1] PTY spawned and banner captured:', len_trim(sess%last_lines(50)) > 0

    ! 2. run a command through the PTY and verify output lands in scrollback
    rc = pty_write(h, 'echo AURA_MARKER_OK')
    call drain_ms(sess, h, 3000)
    ctx = sess%last_lines(30)
    if (index(ctx, 'AURA_MARKER_OK') > 0) then
        print *, '[2] PASS: command round-trip; marker in context'
    else
        print *, '[2] FAIL: no marker. tail:'
        write (*, '(A)') ctx
        ok = .false.
    end if

    ! 3. ANSI parsing: screen should contain the prompt text
    block
        character(len=:), allocatable :: scr
        scr = sess%render_text()
        if (index(scr, 'aura') > 0 .or. index(scr, '>') > 0) then
            print *, '[3] PASS: screen render shows prompt'
        else
            print *, '[3] WARN: screen render empty-ish'
        end if
    end block

    ! 4. AI assistant on the captured context
    answer = ai_ask('list my files', ctx, 'C:\Users\junsh\projects\aura')
    if (index(answer, 'ls -la') > 0) then
        print *, '[4] PASS: AI suggestion generated'
    else
        print *, '[4] FAIL AI: ', trim(answer); ok = .false.
    end if
    print *, '--- AI says ---'
    write (*, '(A)') answer

    ! 5. second command still works after AI query
    rc = pty_write(h, 'echo SECOND_OK')
    call drain_ms(sess, h, 3000)
    if (index(sess%last_lines(10), 'SECOND_OK') > 0) then
        print *, '[5] PASS: session continues'
    else
        print *, '[5] FAIL: session dead'; ok = .false.
    end if

    call pty_close(h)

    if (ok) then
        print *
        print *, 'E2E FINAL: ALL PASS'
    else
        stop 1
    end if

contains
    subroutine init_sess(s)
        type(term_session), intent(inout) :: s
        s%n_lines = 0
        s%pending = ''
        call s%screen%resize(30_int32, 100_int32)
    end subroutine

    subroutine drain_ms(s, hh, ms_budget)
        type(term_session), intent(inout) :: s
        integer(c_long_long), intent(in) :: hh
        integer, intent(in) :: ms_budget
        character(len=:), allocatable :: buf
        integer :: got, c0, c1, rate
        call system_clock(c0, rate)
        do
            got = pty_read(hh, buf)
            if (got < 0) then
                s%alive = .false.; return
            else if (got > 0) then
                call s%append_output(buf)
                call system_clock(c0, rate)
            else
                call system_clock(c1)
                if (real(c1-c0)/real(rate)*1000.0 > real(ms_budget)) return
            end if
        end do
    end subroutine
end program
