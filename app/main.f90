! aura.f90 (phase B) — TUI terminal emulator event loop.
!
!   aura            -> full-screen terminal (raw keys, streaming grid render)
!   aura --cli      -> legacy line-based mode (debugging)
!   aura --ask "…"  -> one-shot AI query
!   aura --config   -> write default config
!
! Workspaces: each workspace is a named project scope with its own cwd, tabs,
! and AI conversation context. The TUI operates on the active workspace.
program aura_main
    use iso_fortran_env, only: i4 => int32
    use iso_c_binding, only: c_long_long
    use aura_ansi
    use aura_session
    use aura_pty
    use aura_keys
    use aura_render
    use aura_ai
    use aura_config
    use aura_theme
    use aura_workspace
    use aura_workbench
    use aura_gui
    implicit none

    integer, parameter :: MAX_SESS = 8
    integer(i4) :: GRID_ROWS = 30, GRID_COLS = 100

    type(workspace_registry) :: ws_reg
    type(workspace), pointer :: ws

    type(aura_cfg) :: cfg
    integer :: con_cols, con_rows
    integer(i4) :: rc_dummy

    rc_dummy = 0
    con_cols = 100; con_rows = 30

    ! ---- dispatch on args ----
    block
        character(len=4096) :: arg1
        character(len=2048) :: q
        if (command_argument_count() >= 1) then
            call get_command_argument(1, arg1)
            select case (trim(arg1))
            case ('--config')
                call cfg%load(); call cfg%save()
                print *, 'Config written.'
                stop 0
            case ('--cli')
                call run_legacy_cli()
                stop 0
            case ('--ask')
                q = ''
                if (command_argument_count() >= 2) call get_command_argument(2, q)
                call cfg%load()
                call ai_init(cfg)
                print *, trim(ai_query(trim(q), '', '.'))
                stop 0
            case ('--workbench')
                call run_workbench_command()
                stop 0
            case ('--gui')
                call cfg%load()
                rc_dummy = gui_main(cfg)
                if (rc_dummy /= 0) stop 1
                stop 0
            end select
        end if
    end block

    ! ---- boot TUI ----
    block
        integer :: u
        integer(i4) :: idx
        integer :: di
        open (newunit=u, file='aura_boot.log', status='replace', action='write')
        write (u, '(A)') 'boot: start'
        flush (u)
        call cfg%load()
        call ai_init(cfg)
        write (u, '(A)') 'boot: cfg loaded, shell='//trim(cfg%shell_path)
        flush (u)

        ! load workspaces (or create default)
        call ws_reg%load()
        if (ws_reg%n == 0) then
            call reg_add(ws_reg, 'default', '.', idx)
        end if
        ws => ws_reg%current()

        ! spawn initial session in the active workspace
        call spawn_session(cfg%shell_path)
        write (u, '(A,I0)') 'boot: spawned sessions=', ws%n_sess
        flush (u)
        ! DEBUG: show workspace structure
        write (u, '(A,I0)') 'boot: ws_reg%n=', ws_reg%n
        do di = 1, ws_reg%n
            write (u, '(A,I0,A,A)') 'boot: ws(', di, ') name='
            write (u, '(A)') trim(ws_reg%items(di)%name)
            write (u, '(A,I0)') 'boot:   n_sess=', ws_reg%items(di)%n_sess
            write (u, '(A,I0)') 'boot:   active=', ws_reg%items(di)%active
        end do
        ws => ws_reg%current()

        if (ws%n_sess == 0) then
            block
                use iso_c_binding, only: c_int, c_char, c_null_char
                integer :: m
                interface
                    function aura_msgbox(text, title) bind(C, name='aura_msgbox')
                        use iso_c_binding, only: c_int, c_char
                        integer(kind=c_int) :: aura_msgbox
                        character(kind=c_char), intent(in) :: text(*), title(*)
                    end function
                end interface
                character(kind=c_char) :: mt(256), mtt(17)
                integer :: i
                mt = c_null_char; mtt = c_null_char
                do i = 1, min(len_trim(cfg%shell_path), 250)
                    mt(i) = cfg%shell_path(i:i)
                end do
                mtt(1:17) = (/ 'A','u','r','a',' ','e','r','r','o','r',':',' ','s','p','a','w','n' /)
                m = aura_msgbox(mt, mtt)
            end block
            close (u)
            print *, 'ERROR: could not spawn shell "', trim(cfg%shell_path), '"'
            stop 1
        end if
        call keys_raw_enter()
        call apply_theme()
        write (u, '(A)') 'boot: raw mode entered, entering loop'
        flush (u)
        call tui_loop()
        write (u, '(A)') 'boot: tui_loop returned'
        call ws_reg%save()
        call keys_raw_exit()
        call reset_theme()
        close (u)
    end block

    ! close all sessions in all workspaces
    block
        integer :: i, j
        do i = 1, ws_reg%n
            ws => ws_reg%items(i)
            do j = 1, ws%n_sess
                if (ws%sess(j)%alive) call pty_close(ws%handle(j))
            end do
        end do
    end block
    print *, 'Aura closed.'

contains

    ! Headless adapter used by the GUI foundation.  The GUI can display the
    ! same manifest and invoke these stages without embedding a compiler,
    ! MPI implementation, or scheduler API.
    subroutine run_workbench_command()
        type(wb_manifest) :: manifest
        logical :: ok
        character(len=:), allocatable :: message, cmd
        character(len=2048) :: path, action, template, run_id
        integer :: exit_code
        type(wb_run_record) :: record
        logical :: record_ok
        path = 'aura-workbench.toml'
        action = 'summary'
        template = ''
        run_id = 'cli-run'
        if (command_argument_count() >= 2) call get_command_argument(2, path)
        if (command_argument_count() >= 3) call get_command_argument(3, action)
        if (command_argument_count() >= 4) call get_command_argument(4, template)
        if (command_argument_count() >= 5) call get_command_argument(5, run_id)
        call manifest%load(trim(path), ok, message)
        if (.not. ok) then
            print '(A)', 'workbench: '//trim(message)
            return
        end if
        select case (trim(action))
        case ('summary')
            print '(A)', 'Project: '//trim(manifest%name)
            print '(A)', 'Template: '//trim(manifest%default_template)
            print '(A)', 'Build: '//trim(manifest%command('build', template))
            print '(A)', 'Run: '//trim(manifest%command('run', template))
            print '(A,I0)', 'Sweep points: ', manifest%sweep_count()
            print '(A)', 'Tracking: '//trim(manifest%tracking_directory)
        case ('build', 'run', 'test', 'visualize', 'browse', 'monitor-results')
            if (trim(action) == 'run') then
                cmd = manifest%command('run', template)
                call manifest%start_run(trim(run_id), cmd, record, record_ok)
            end if
            if (trim(action) == 'monitor-results') then
                call manifest%execute('monitor', template, exit_code, cmd)
            else
                call manifest%execute(trim(action), template, exit_code, cmd)
            end if
            if (trim(action) == 'run') call manifest%finish_run(trim(run_id), exit_code, ok=record_ok)
            print '(A)', trim(cmd)
            if (exit_code /= 0) print '(A,I0)', 'workbench exit code: ', exit_code
        case ('monitor')
            call manifest%read_run(trim(template), record, ok)
            if (ok) then
                print '(A)', 'Run '//trim(record%id)//': '//trim(record%state)
                print '(A,I0)', 'Exit code: ', record%exit_code
                if (len_trim(record%result_path) > 0) print '(A)', 'Result: '//trim(record%result_path)
            else
                print '(A)', 'Run status not found.'
            end if
        case default
            print '(A)', 'Usage: aura --workbench [manifest] [summary|build|run|test|visualize|browse|monitor] [template|run-id]'
        end select
    end subroutine

    ! Legacy line-based mode kept for debugging / non-TTY environments.
    subroutine run_legacy_cli()
        use iso_c_binding, only: c_long_long
        type(term_session) :: s
        character(len=2048) :: userline
        integer(c_long_long) :: h
        integer(i4) :: rc
        character(len=:), allocatable :: buf
        logical :: alive

        call init_blank(s)
        if (pty_spawn(trim(cfg%shell_path), GRID_COLS, GRID_ROWS, h) /= 0) then
            print *, 'ERROR: cannot spawn ', trim(cfg%shell_path)
            return
        end if
        s%alive = .true.
        alive = .true.
        print *, 'Aura legacy CLI — /exit quits.'
        do while (alive)
            do
                rc = pty_read(h, buf)
                if (rc < 0) then
                    alive = .false.; exit
                else if (rc > 0) then
                    write (*, '(A)', advance='no') buf
                else
                    exit
                end if
            end do
            if (.not. alive) exit
            read (*, '(A)', iostat=rc) userline
            if (rc /= 0) exit
            userline = adjustl(userline)
            if (userline(1:5) == '/exit') exit
            if (len_trim(userline) > 0) rc = pty_write(h, trim(userline))
        end do
        call pty_close(h)
    end subroutine

    subroutine tui_loop()
        logical :: running
        type(key_event) :: ev
        integer :: i, loopcount
        integer :: con_cols_chk, con_rows_chk
        character(len=512) :: cfg_dir
        logical :: dbg_once

        running = .true.
        loopcount = 0
        dbg_once = .false.
        call con_get_size_f(con_cols_chk, con_rows_chk)
        cfg_dir = config_dir_path()
        open (newunit=i, file='aura_boot.log', status='old', action='write', iostat=i)
        if (i == 0) then
            write (i, '(A,A)') 'config_dir_path()="', trim(cfg_dir), '"'
            write (i, '(A,I0)') 'boot: ws_reg%n=', ws_reg%n
            write (i, '(A,I0)') 'boot: current ws n_sess=', ws%n_sess, ' active=', ws%active
            close (i)
        end if
        do while (running)
            ws => ws_reg%current()
            do i = 1, ws%n_sess
                if (ws%sess(i)%alive) call drain_session(ws%sess(i), ws%handle(i), 5)
            end do
            ! clamp active to valid range; do NOT reset it — tab switching
            ! must persist across loop iterations
            if (ws%n_sess == 0) then
                ws%active = 1
            else
                ws%active = max(1, min(ws%active, ws%n_sess))
            end if

            call con_get_size_f(con_cols, con_rows)

            ! 2. render
            if (.not. ws%scroll_mode) then
                call render_grid(ws%sess(ws%active)%screen)
            else
                call render_scrollback_view(ws%sess(ws%active)%scrollback, &
                    int(ws%sess(ws%active)%n_lines), ws%scroll_top, con_rows, con_cols)
            end if
            call render_status_line(ws_reg, con_cols)

            ! 3. key event
            if (poll_key(30, ev)) then
                ! DEBUG: log key event to file
                block
                    integer :: kf
                    logical :: kf_exists
                    inquire (file='aura_keys.log', exist=kf_exists)
                    if (kf_exists) then
                        open (newunit=kf, file='aura_keys.log', position='append', action='write')
                    else
                        open (newunit=kf, file='aura_keys.log', status='new', action='write')
                    end if
                    write (kf, '(A,I0,A,I0,A,I0,A,I0,A,I0)') &
                        'kind=', ev%kind, ' cp=', ev%codepoint, ' sp=', ev%special, &
                        ' ctrl=', merge(1, 0, ev%ctrl), ' shift=', merge(1, 0, ev%shift), &
                        ' alt=', merge(1, 0, ev%alt)
                    close (kf)
                end block
                select case (ev%kind)
                case (KEV_RESIZE)
                    if (con_refresh_size_f() /= 0) then
                        call con_get_size_f(con_cols, con_rows)
                        if (con_cols >= 20 .and. con_rows >= 6) then
                            GRID_COLS = con_cols
                            GRID_ROWS = con_rows
                            call ws%sess(ws%active)%screen%resize(int(GRID_ROWS, i4), int(GRID_COLS, i4))
                            call pty_resize(ws%handle(ws%active), GRID_COLS, GRID_ROWS)
                        end if
                    end if
                    call invalidate_mirror()
                case (KEV_CHAR)
                    if (is_reserved(ev)) then
                        running = handle_reserved(ev)
                    else if (ws%scroll_mode) then
                        ws%scroll_mode = .false.
                        call invalidate_mirror()
                    else
                        call send_key_vt(ev, ws%handle(ws%active))
                    end if
                case (KEV_SPECIAL)
                    select case (ev%special)
                    case (KEY_F1)
                        ws%active = max(1, ws%active - 1)
                        call invalidate_mirror()
                    case (KEY_F2)
                        ws%active = min(ws%n_sess, ws%active + 1)
                        call invalidate_mirror()
                    case (KEY_LEFT)
                        if (ev%ctrl) then
                            ws%active = max(1, ws%active - 1)
                            call invalidate_mirror()
                        else
                            call send_key_vt(ev, ws%handle(ws%active))
                        end if
                    case (KEY_RIGHT)
                        if (ev%ctrl) then
                            ws%active = min(ws%n_sess, ws%active + 1)
                            call invalidate_mirror()
                        else
                            call send_key_vt(ev, ws%handle(ws%active))
                        end if
                    case (KEY_PGUP)
                        call do_scroll(-10)
                    case (KEY_PGDN)
                        call do_scroll(+10)
                    case default
                        if (.not. ws%scroll_mode) call send_key_vt(ev, ws%handle(ws%active))
                    end select
                end select
            end if

            ! 4. reap dead active session
            if (.not. ws%sess(ws%active)%alive) then
                call pty_close(ws%handle(ws%active))
                call remove_session(ws%active)
                if (ws%n_sess == 0) running = .false.
            end if
        end do
    end subroutine

    logical function is_reserved(ev) result(r)
        type(key_event), intent(in) :: ev
        r = .false.
        if (ev%kind == KEV_CHAR) then
            if (ev%ctrl) then
                if (ev%codepoint == iachar('a') .or. ev%codepoint == iachar('t') &
                    .or. ev%codepoint == iachar('w') .or. ev%codepoint == iachar('r')) r = .true.
                if (ev%codepoint == iachar('v') .and. ev%shift) r = .true.
            end if
        end if
    end function

    logical function handle_reserved(ev) result(keep_running)
        type(key_event), intent(in) :: ev
        integer(i4) :: idx
        character(len=:), allocatable :: transcript
        keep_running = .true.

        ! Ctrl+Shift+A -> AI drawer (the prompt is also the voice-input target)
        if (ev%ctrl .and. ev%shift .and. ev%codepoint == iachar('a')) then
            call ai_drawer()
            return
        end if
        ! Ctrl+Shift+V records/transcribes through the configured external adapter.
        if (ev%ctrl .and. ev%shift .and. ev%codepoint == iachar('v')) then
            transcript = capture_voice_text()
            if (len_trim(transcript) > 0) call ai_drawer(transcript)
            return
        end if
        ! Ctrl+R -> cycle workspace
        if (ev%ctrl .and. ev%codepoint == iachar('r')) then
            call ws_reg%switch_to(mod(ws_reg%active, ws_reg%n) + 1)
            ws => ws_reg%current()
            ws%scroll_mode = .false.
            if (ws%n_sess == 0) call spawn_session(cfg%shell_path)
            call invalidate_mirror()
            return
        end if
        ! Ctrl+T -> new tab in active workspace
        if (ev%ctrl .and. ev%codepoint == iachar('t')) then
            if (ws%n_sess < MAX_SESS) then
                call spawn_session(cfg%shell_path)
                call invalidate_mirror()
            end if
            return
        end if

        ! Ctrl+W -> workspace picker (create/switch/close)
        if (ev%ctrl .and. ev%codepoint == iachar('w')) then
            call workspace_picker()
            ws => ws_reg%current()
            call invalidate_mirror()
            return
        end if
    end function

    subroutine send_key_vt(ev, h)
        type(key_event), intent(in) :: ev
        integer(c_long_long), intent(in) :: h
        character(len=1) :: vt(16)
        integer :: nvt
        call key_to_vt(ev, vt, nvt)
        if (nvt > 0) then
            rc_dummy = pty_write_bytes(h, vt, nvt)
        end if
    end subroutine

    subroutine do_scroll(delta)
        integer, intent(in) :: delta
        integer :: maxtop, vis
        vis = con_rows - 2
        maxtop = max(1, int(ws%sess(ws%active)%n_lines) - vis + 1)
        if (.not. ws%scroll_mode) then
            ws%scroll_mode = .true.
            ws%scroll_top = max(1, int(ws%sess(ws%active)%n_lines) - vis + 1)
            return
        end if
        ws%scroll_top = min(max(1, ws%scroll_top + delta), maxtop)
    end subroutine

    subroutine spawn_session(shellcmd)
        character(len=*), intent(in) :: shellcmd
        integer :: qc, qr
        character(len=:), allocatable :: command
        if (ws%n_sess >= MAX_SESS) return
        call con_get_size_f(qc, qr)
        if (qc < 20) qc = 80
        if (qr < 6)  qr = 24
        GRID_COLS = qc
        GRID_ROWS = qr
        call init_blank(ws%sess(ws%n_sess + 1))
        if (len_trim(ws%remote_target) > 0) then
            command = 'ssh -p '//int_to_string(ws%remote_port)//' '//trim(ws%remote_target)
        else
            command = trim(shellcmd)
        end if
        if (pty_spawn(command, GRID_COLS, GRID_ROWS, ws%handle(ws%n_sess + 1)) /= 0) return
        ws%n_sess = ws%n_sess + 1
        ws%sess(ws%n_sess)%alive = .true.
        if (len_trim(ws%remote_target) > 0) then
            write (ws%titles(ws%n_sess), '(A,I0)') 'ssh', ws%n_sess
            if (len_trim(ws%cwd) > 0 .and. trim(ws%cwd) /= '.') then
                rc_dummy = pty_write_text(ws%handle(ws%n_sess), 'cd "'//trim(ws%cwd)//'"')
                rc_dummy = pty_write_bytes(ws%handle(ws%n_sess), (/ achar(13) /), 1)
            end if
        else
            write (ws%titles(ws%n_sess), '(A,I0)') 'shell', ws%n_sess
        end if
        ws%active = ws%n_sess
    end subroutine

    subroutine remove_session(idx)
        integer, intent(in) :: idx
        integer :: j
        do j = idx, MAX_SESS - 1
            ws%sess(j) = ws%sess(j + 1)
            ws%handle(j) = ws%handle(j + 1)
            ws%titles(j) = ws%titles(j + 1)
        end do
        ws%n_sess = ws%n_sess - 1
        if (ws%active > ws%n_sess) ws%active = max(1, ws%n_sess)
        call invalidate_mirror()
    end subroutine

    subroutine init_blank(s)
        type(term_session), intent(inout) :: s
        s%n_lines = 0
        s%pending = ''
        s%input_line = ''
        s%cwd = ''
        s%alive = .false.
        if (.not. allocated(s%scrollback)) allocate(character(len=LINE_LEN)::s%scrollback(MAX_LINES))
        call s%screen%resize(GRID_ROWS, GRID_COLS)
    end subroutine

    subroutine drain_session(s, h, ms_budget)
        type(term_session), intent(inout) :: s
        integer(c_long_long), intent(in) :: h
        integer, intent(in) :: ms_budget
        character(len=:), allocatable :: buf
        integer :: got, c0, c1, rate
        call system_clock(c0, rate)
        do
            got = pty_read(h, buf)
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

    ! ---- workspace picker ----
    subroutine workspace_picker()
        character(len=1024) :: qlin
        integer :: top, ln, qlen, i
        logical :: done
        type(key_event) :: aev

        top = max(1, con_rows - 10)
        call clear_drawer(top)
        call draw_hline(top, con_cols)
        call con_write_at_s(2, top, b_dot()//' Workspaces', C_ACCENT, C_BG, .true., .false.)
        call con_write_at_s(con_cols - 18, top, ' name+Enter new', C_DIM, C_BG, .false., .false.)
        ln = top + 1
        do i = 1, ws_reg%n
            if (i == ws_reg%active) then
                call con_write_at_s(2, ln, ' > '//trim(ws_reg%items(i)%name), C_ACCENT2, C_BG, .true., .false.)
            else
                call con_write_at_s(2, ln, '   '//trim(ws_reg%items(i)%name), C_DIM, C_BG, .false., .false.)
            end if
            ln = ln + 1
        end do
        qlin = ''
        qlen = 0
        done = .false.
        do while (.not. done)
            call con_write_at_s(3, con_rows - 3, b_arrow()//' '//qlin(:qlen)//'_   (Esc closes)', C_ACCENT2, C_BG, .false., .false.)
            if (.not. poll_key(-1, aev)) cycle
            if (aev%kind /= KEV_CHAR) cycle
            if (aev%codepoint == 27) then
                done = .true.
            else if (aev%codepoint == 13 .or. aev%codepoint == 10) then
                done = .true.
                if (qlen > 0) then
                    call add_workspace_spec(trim(qlin(:qlen)), i)
                    ws_reg%active = int(i, i4)
                    ws => ws_reg%current()
                    if (ws%n_sess == 0) call spawn_session(cfg%shell_path)
                end if
            else if (aev%codepoint == 8 .or. aev%codepoint == 127) then
                if (qlen > 0) qlen = qlen - 1
            else if (.not. aev%ctrl .and. aev%codepoint >= 32 .and. qlen < len(qlin)) then
                qlen = qlen + 1
                qlin(qlen:qlen) = achar(min(aev%codepoint, 255))
            end if
        end do
        call clear_drawer(top)
        call invalidate_mirror()
    end subroutine

    ! ---- AI drawer ----
    subroutine ai_drawer(seed)
        character(len=*), intent(in), optional :: seed
        character(len=1024) :: qlin
        character(len=:), allocatable :: answer, cmdline
        integer :: drawer_top, ln, qlen
        character(len=512) :: cwdv
        logical :: done, want_insert, want_exec, keep_open
        type(key_event) :: aev

        call get_environment_variable('PWD', cwdv)
        if (len_trim(cwdv) == 0) call get_environment_variable('CD', cwdv)

        drawer_top = max(1, con_rows - 8)
        call draw_hline(drawer_top, con_cols)
        call con_write_at_s(2, drawer_top, ' AURA AI  /  COMPUTATIONAL ASSIST', C_ACCENT, C_BG, .true., .false.)
        call con_write_at_s(con_cols - 31, drawer_top, ' Voice-ready prompt  |  Esc close', C_DIM, C_BG, .false., .false.)
        qlin = ''
        qlen = 0
        if (present(seed)) then
            qlen = min(len(qlin), len_trim(seed))
            if (qlen > 0) qlin(:qlen) = seed(:qlen)
        end if
        done = .false.
        do while (.not. done)
            call con_write_at_s(3, drawer_top + 2, '> '//qlin(:qlen)//'_', C_ACCENT2, C_BG, .false., .false.)
            if (.not. poll_key(-1, aev)) cycle
            if (aev%kind /= KEV_CHAR) cycle
            if (aev%codepoint == 27) then
                done = .true.
            else if (aev%codepoint == 13 .or. aev%codepoint == 10) then
                done = .true.
                if (qlen > 0) then
                    answer = ai_query(qlin(:qlen), ws%sess(ws%active)%last_lines(15), trim(cwdv))
                    ! record in workspace AI context
                    call ws%ai%add_message('user', qlin(:qlen))
                    call ws%ai%add_message('assistant', answer)
                    want_insert = .false.; want_exec = .false.; keep_open = .false.
                    call answer_view(drawer_top, answer, want_insert, want_exec, keep_open)
                    if (want_insert .or. want_exec) then
                        cmdline = suggested_command(answer)
                        if (len_trim(cmdline) > 0) then
                            rc_dummy = pty_write_text(ws%handle(ws%active), trim(cmdline))
                            if (want_exec) &
                                rc_dummy = pty_write_bytes(ws%handle(ws%active), (/ achar(13) /), 1)
                            call invalidate_mirror()
                        end if
                    end if
                end if
            else if (aev%codepoint == 8 .or. aev%codepoint == 127) then
                if (qlen > 0) qlen = qlen - 1
            else if (.not. aev%ctrl .and. aev%codepoint >= 32 .and. qlen < len(qlin)) then
                qlen = qlen + 1
                qlin(qlen:qlen) = achar(min(aev%codepoint, 255))
            end if
        end do
        call clear_drawer(drawer_top)
        call invalidate_mirror()
    end subroutine

    subroutine add_workspace_spec(spec, idx)
        character(len=*), intent(in) :: spec
        integer, intent(out) :: idx
        integer :: p1, p2, p3, port, ios
        character(len=256) :: name, target, cwd, port_text
        name = ''; target = ''; cwd = ''; port_text = ''
        p1 = index(spec, '|')
        p2 = 0; p3 = 0
        if (p1 > 0) p2 = index(spec(p1 + 1:), '|') + p1
        if (p2 > p1) p3 = index(spec(p2 + 1:), '|') + p2
        if (p1 > 1 .and. p2 > p1 .and. p3 > p2) then
            name = spec(:p1 - 1)
            target = spec(p1 + 1:p2 - 1)
            port_text = spec(p2 + 1:p3 - 1)
            cwd = spec(p3 + 1:)
            read (port_text, *, iostat=ios) port
            if (ios /= 0) port = 22
            call reg_add_remote(ws_reg, trim(name), trim(target), trim(cwd), port, idx)
        else
            call reg_add(ws_reg, trim(spec), '.', idx)
        end if
        if (idx > 0) then
            ws_reg%active = int(idx, i4)
            ws => ws_reg%current()
            if (ws%n_sess == 0) call spawn_session(cfg%shell_path)
        end if
    end subroutine

    function capture_voice_text() result(transcript)
        character(len=:), allocatable :: transcript
        character(len=1024) :: record_cmd, transcribe_cmd, wav_path, txt_path
        character(len=4096) :: line
        integer :: u, ios, exitstat
        transcript = ''
        if (len_trim(cfg%stt_record_command) == 0 .or. len_trim(cfg%stt_transcribe_command) == 0) then
            call con_write_at_s(2, max(1, con_rows - 4), &
                'Voice adapter not configured. Set stt_record_command and stt_transcribe_command.', &
                C_WARN, C_BG, .false., .false.)
            return
        end if
        wav_path = 'aura_voice.wav'
        txt_path = 'aura_voice.txt'
        record_cmd = replace_token(cfg%stt_record_command, '{wav}', wav_path)
        transcribe_cmd = replace_token(cfg%stt_transcribe_command, '{wav}', wav_path)
        call execute_command_line(trim(record_cmd), wait=.true., exitstat=exitstat)
        if (exitstat /= 0) return
        call execute_command_line(trim(transcribe_cmd)//' > "'//trim(txt_path)//'"', &
                                  wait=.true., exitstat=exitstat)
        if (exitstat /= 0) return
        open (newunit=u, file=trim(txt_path), status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(transcript) > 0) transcript = transcript//' '
            transcript = transcript//trim(line)
        end do
        close (u)
    end function

    function replace_token(command, token, value) result(out)
        character(len=*), intent(in) :: command, token, value
        character(len=:), allocatable :: out
        integer :: p
        out = command
        p = index(out, token)
        if (p > 0) out = out(:p - 1)//value//out(p + len(token):)
    end function

    function int_to_string(value) result(out)
        integer, intent(in) :: value
        character(len=16) :: out
        write (out, '(I0)') value
        out = adjustl(out)
    end function

    subroutine clear_drawer(top_row)
        integer, intent(in) :: top_row
        character(len=1024) :: blank
        integer :: r
        blank = repeat(' ', min(con_cols, 1000))
        do r = top_row, con_rows - 2
            call con_write_at_f(0, r, blank(:min(con_cols, 1000)))
        end do
    end subroutine

    subroutine answer_view(top_row, ans, want_insert, want_exec, keep_open)
        integer, intent(in) :: top_row
        character(len=*), intent(in) :: ans
        logical, intent(out) :: want_insert, want_exec, keep_open
        character(len=1024) :: linebuf
        integer :: ln, pos0, nl
        type(key_event) :: aev

        want_insert = .false.; want_exec = .false.; keep_open = .false.
        call clear_drawer(top_row)
        call draw_hline(top_row, con_cols)
        call con_write_at_s(2, top_row, b_dot()//' Aura AI', C_ACCENT, C_BG, .true., .false.)
        ln = top_row + 1
        pos0 = 1
        do while (pos0 <= len_trim(ans) .and. ln < con_rows - 3)
            nl = index(ans(pos0:), new_line('a'))
            if (nl == 0) then
                linebuf = ans(pos0:)
                pos0 = len_trim(ans) + 1
            else
                linebuf = ans(pos0:pos0 + nl - 2)
                pos0 = pos0 + nl
            end if
            call relabel_ai_line(linebuf)
            if (index(linebuf, 'Suggested command:') /= 0) then
                call con_write_at_s(2, ln, linebuf, C_ACCENT2, C_BG, .true., .false.)
            else if (index(linebuf, 'Why:') /= 0) then
                call con_write_at_s(2, ln, linebuf, C_DIM, C_BG, .false., .false.)
            else
                call con_write_at_s(2, ln, adjustl(linebuf), C_FG, C_BG, .false., .false.)
            end if
            ln = ln + 1
        end do
        call con_write_at_s(2, con_rows - 3, '[Ctrl+E] insert   [Ctrl+J] run   [Esc] close', C_ACCENT, C_BG, .false., .false.)
        do
            if (.not. poll_key(-1, aev)) cycle
            if (aev%kind /= KEV_CHAR) cycle
            if (aev%ctrl .and. aev%codepoint == iachar('e')) then
                want_insert = .true.; return
            else if (aev%ctrl .and. aev%codepoint == iachar('j')) then
                want_exec = .true.; return
            else if (aev%codepoint == 27) then
                return
            end if
        end do
    end subroutine

    function suggested_command(ans) result(cmd)
        character(len=*), intent(in) :: ans
        character(len=512) :: cmd
        integer :: p, e
        cmd = ''
        p = index(ans, 'CMD:')
        if (p == 0) return
        p = p + 4
        do while (p <= len_trim(ans) .and. ans(p:p) == ' ')
            p = p + 1
        end do
        e = index(ans(p:), new_line('a'))
        if (e == 0) then
            cmd = ans(p:)
        else
            cmd = ans(p:p + e - 2)
        end if
    end function

    ! Draw a boxed horizontal rule: ├─...─┤
    subroutine draw_hline(row0, ncols)
        use iso_c_binding, only: c_int
        integer, intent(in) :: row0, ncols
        integer(c_int) :: cc, rr
        integer(kind=2) :: w(256)
        integer :: j, lim
        lim = min(ncols, 250)
        w(1) = int(z'251C', kind=2)
        do j = 2, lim - 1
            w(j) = int(z'2500', kind=2)
        end do
        if (lim >= 2) w(lim) = int(z'2524', kind=2)
        cc = 0_c_int; rr = int(row0, c_int)
        call con_write_at_w(cc, rr, w, int(lim, c_int))
    end subroutine

    subroutine relabel_ai_line(line)
        character(len=*), intent(inout) :: line
        integer :: p
        p = index(line, 'CMD:')
        if (p /= 0) line = 'Suggested command: '//adjustl(line(p + 4:))
        p = index(line, 'WHY:')
        if (p /= 0) line = 'Why: '//adjustl(line(p + 4:))
    end subroutine

end program
