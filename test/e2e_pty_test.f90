! e2e_pty_test.f90 — end-to-end: spawn shell via ConPTY, run commands, verify output
program e2e
    use iso_fortran_env, only: i4 => int32
    use iso_c_binding, only: c_long_long
    use aura_session
    use aura_pty
    implicit none

    type(term_session) :: sess
    integer(c_long_long) :: h
    integer(i4) :: rc, spins
    logical :: saw_echo, saw_output

    call init_sess(sess)
    rc = pty_spawn('C:\Windows\System32\cmd.exe', 100, 30, h)
    if (rc /= 0) then
        print *, 'FAIL: spawn rc=', rc
        stop 1
    end if
    sess%alive = .true.
    print *, 'PTY spawned OK'

    call drain_ms(sess, h, 1500)
    print *, '--- banner ---'
    print *, '(banner chars captured:', len_trim(sess%last_lines(50)), ')'

    rc = pty_write(h, 'echo AURA_E2E_MARKER_12345')
    saw_echo = .false.; saw_output = .false.
    do spins = 1, 40
        call drain_ms(sess, h, 250)
        if (index(sess%last_lines(60), 'AURA_E2E_MARKER_12345') > 0) then
            saw_output = .true.
            exit
        end if
    end do

    print *, '--- session tail ---'
    write (*, '(A)') sess%last_lines(20)

    if (saw_output) then
        print *, 'E2E PASS: command round-trip through ConPTY verified'
    else
        print *, 'E2E FAIL: marker not seen'
        stop 1
    end if
    call pty_close(h)

contains
    subroutine init_sess(s)
        use iso_fortran_env
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
                s%alive = .false.
                return
            else if (got > 0) then
                call s%append_output(buf)
                call system_clock(c0, rate)
            else
                call system_clock(c1)
                if (real(c1 - c0)/real(rate)*1000.0 > real(ms_budget)) return
            end if
        end do
    end subroutine
end program
