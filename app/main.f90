! aura.f90 — Aura: AI-enhanced terminal (MVP)
!
! Modes:
!   aura            -> interactive CLI terminal (works everywhere, no GTK needed)
!   aura --gui      -> GTK GUI when gtk-fortran is linked (falls back to CLI)
!   aura --ask "…"  -> one-shot AI query using last session context
!   aura --config   -> write default config to ~/.config/aura/config.json
program aura_main
    use iso_fortran_env, only: i4 => int32
    use iso_c_binding, only: c_long_long
    use aura_ansi
    use aura_session
    use aura_pty
    use aura_llm
    use aura_config
    use aura_gui
    implicit none

    character(len=4096) :: arg1, question_arg
    type(aura_cfg) :: cfg
    integer :: narg, rc
    logical :: want_gui

    narg = command_argument_count()
    call get_command_argument(1, arg1)
    if (narg == 0) then
        want_gui = .false.
    else
        want_gui = (trim(arg1) == '--gui')
        question_arg = ''
        if (trim(arg1) == '--ask' .and. narg >= 2) call get_command_argument(2, question_arg)
    end if

    ! Load (or initialize) configuration
    call cfg%load()
    if (trim(arg1) == '--config' .or. narg == -1) then
        call cfg%save()
        print *, 'Config written to ', config_dir_path(), '/config.json'
        stop 0
    end if

    if (want_gui) then
        rc = gui_main(cfg)
        if (rc <= 0) then
            print *, 'Running in CLI mode instead.'
            call run_cli(cfg)
        end if
    else if (narg >= 2 .and. trim(arg1) == '--ask') then
        call run_ask(cfg, trim(question_arg))
    else
        call run_cli(cfg)
    end if

contains

    ! ------------------------------------------------------------------
    ! One-shot AI ask: spawns a throwaway session context is not possible,
    ! so we answer with cwd + a fresh shell probe of the last output dir.
    subroutine run_ask(cfg, q)
        type(aura_cfg), intent(inout) :: cfg
        character(len=*), intent(in) :: q
        character(len=:), allocatable :: answer, ctx
        character(len=512) :: cwd
        integer(i4) :: rc
        integer(c_long_long) :: h
        character(len=:), allocatable :: buf
        type(term_session) :: sess

        call get_environment_variable('PWD', cwd)
        if (len_trim(cwd) == 0) call get_environment_variable('CD', cwd)

        ! quick context probe: run a directory listing in the PTY and read briefly
        call blank_session(sess)
        rc = pty_spawn('ls -la 2>nul || dir', 80, 24, h)
        ctx = ''
        if (rc == 0) then
            call drain(sess, h, 700)
            ctx = sess%last_lines(15)
            call pty_close(h)
        end if

        answer = ai_ask(q, ctx, trim(cwd))
        print *, '--- Aura AI ---'
        print *, trim(answer)
        print *, '---------------'
    end subroutine

    ! ------------------------------------------------------------------
    ! Interactive CLI terminal: PTY loop with line-based local input,
    ! ANSI-parsed screen rendering, /ai command for the assistant.
    subroutine run_cli(cfg)
        type(aura_cfg), intent(inout) :: cfg
        type(term_session) :: sess
        character(len=4096) :: cmdline
        character(len=2048) :: userline, q
        integer(i4) :: rc
        integer(c_long_long) :: h
        character(len=:), allocatable :: buf

        cmdline = trim(cfg%shell_path)
        if (len_trim(cmdline) == 0) cmdline = default_shell()

        print *, 'Aura terminal (MVP) — shell: ', trim(cmdline)
        print *, 'Type commands normally; /ai <question> asks the assistant; /exit quits.'
        print *

        call blank_session(sess)
        rc = pty_spawn(cmdline, 100, 30, h)
        if (rc /= 0) then
            print *, 'ERROR: failed to spawn shell "', trim(cmdline), '"'
            stop 1
        end if
        sess%alive = .true.

        do while (sess%alive)
            ! pump any pending PTY output
            call drain(sess, h, 120)
            call render(sess)

            if (.not. sess%alive) exit
            read (*, '(A)', iostat=rc) userline
            if (rc /= 0) exit                       ! stdin closed
            userline = adjustl(userline)

            if (userline(1:5) == '/exit') exit
            if (userline(1:3) == '/ai') then
                q = userline(4:)
                call handle_ai(sess, h, trim(q))
                cycle
            end if
            if (len_trim(userline) > 0) then
                if (pty_write(h, trim(userline)) < 0) exit
            end if
        end do

        call pty_close(h)
        print *, 'Bye.'
    end subroutine

    subroutine blank_session(s)
        type(term_session), intent(inout) :: s
        s%n_lines = 0
        s%pending = ''
        s%input_line = ''
        s%cwd = ''
        call s%screen%resize(30_i4, 100_i4)
    end subroutine

    subroutine drain(sess, h, ms_budget)
        type(term_session), intent(inout) :: sess
        integer(c_long_long), intent(in) :: h
        integer, intent(in) :: ms_budget
        character(len=:), allocatable :: buf
        integer :: spins, got
        real :: t0, t1, dt
        integer :: clock0, clock1, rate

        call system_clock(clock0, rate)
        do
            got = pty_read(h, buf)
            if (got < 0) then
                sess%alive = .false.
                return
            end if
            if (got > 0) then
                call sess%append_output(buf)
                call system_clock(clock0, rate)     ! reset idle timer on data
            else
                call system_clock(clock1)
                if (real(clock1 - clock0)/real(rate)*1000.0 > real(ms_budget)) return
            end if
        end do
    end subroutine

    subroutine render(sess)
        type(term_session), intent(inout) :: sess
        character(len=:), allocatable :: txt
        txt = sess%render_text()
        if (len_trim(txt) > 0) print *, txt
    end subroutine

    subroutine handle_ai(sess, h, q)
        type(term_session), intent(inout) :: sess
        integer(c_long_long), intent(in) :: h
        character(len=*), intent(in) :: q
        character(len=:), allocatable :: answer, ctx
        character(len=512) :: cwd

        call get_environment_variable('PWD', cwd)
        if (len_trim(cwd) == 0) call get_environment_variable('CD', cwd)
        ctx = sess%last_lines(10)

        answer = ai_ask(q, ctx, trim(cwd))
        print *
        print *, '=== Aura AI ==='
        print *, trim(answer)
        print *, '==============='
        print *
        print *, '(/ok <command> sends it; or continue typing)'
    end subroutine

    pure function default_shell() result(s)
        character(len=64) :: s
        s = '/bin/bash'
    end function

end program aura_main
