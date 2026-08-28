! aura_render.f90 — diff renderer: paints the active session's cell grid to the
! host console with minimal updates; scroll-mode painter; status-line tab bar.
module aura_render
    use iso_fortran_env, only: i4 => int32
    use iso_c_binding, only: c_int, c_char
    use aura_ansi
    use aura_keys
    use aura_theme
    use aura_workspace
    implicit none
    private

    ! tracks what we last painted at each cell (screen mirror)
    type :: paint_state
        type(term_cell), allocatable :: last(:, :)
        integer(i4) :: rows = 0, cols = 0
        logical :: valid = .false.
    end type

    type(paint_state) :: mirror

    public :: render_grid, render_status_line, render_full_repaint_flag
    public :: invalidate_mirror, render_scrollback_view

contains

    subroutine ensure_mirror(r, c)
        integer(i4), intent(in) :: r, c
        if (allocated(mirror%last)) deallocate(mirror%last)
        allocate(mirror%last(r, c))
        call blank_all(mirror%last)
        mirror%rows = r; mirror%cols = c
        mirror%valid = .true.
    end subroutine

    subroutine invalidate_mirror()
        mirror%valid = .false.
    end subroutine

    pure logical function render_full_repaint_flag()
        render_full_repaint_flag = .not. mirror%valid
    end function

    ! Paint the grid. Only changed cells are written (diff vs mirror).
    ! rows/cols are 0-based console coordinates for line starts.
    subroutine render_grid(screen)
        type(ansi_parser), intent(inout) :: screen
        integer :: r, c
        character(len=1), allocatable :: run(:)
        integer :: run_len, run_start

        if (.not. mirror%valid .or. mirror%rows /= screen%rows .or. &
            mirror%cols /= screen%cols) then
            call ensure_mirror(screen%rows, screen%cols)
        end if

        do r = 1, screen%rows
            c = 1
            do while (c <= screen%cols)
                if (cells_equal(screen%cells(r, c), mirror%last(r, c))) then
                    c = c + 1
                    cycle
                end if
                ! start a dirty run
                run_start = c
                allocate(character(len=1)::run(screen%cols - c + 1))
                run_len = 0
                do while (c <= screen%cols)
                    if (cells_equal(screen%cells(r, c), mirror%last(r, c))) exit
                    run_len = run_len + 1
                    run(run_len) = screen%cells(r, c)%ch
                    c = c + 1
                end do
                call write_run(r - 1, run_start - 1, run, run_len, screen, r, run_start)
                deallocate(run)
            end do
        end do
        mirror%last = screen%cells
        ! Position the host cursor where the PTY thinks it is, and show it.
        call con_set_cursor_f(screen%cur_col - 1, screen%cur_row - 1)
        call con_show_cursor_f()
    end subroutine

    subroutine write_run(row0, col0, chars, nchars, screen, srow, scol)
        use iso_c_binding, only: c_int, c_char
        integer, intent(in) :: row0, col0, nchars, srow, scol
        character(len=1), intent(in) :: chars(:)
        type(ansi_parser), intent(in) :: screen
        character(kind=c_char) :: sgr(32)
        character(len=512) :: buf
        integer(kind=2) :: w(512)
        integer :: i, k, slen, seg_start, cidx
        integer :: prev_fg, prev_bg
        logical :: prev_bold, prev_rev
        integer(c_int) :: cc, rr
        type(term_cell) :: cur
        interface
            subroutine aura_con_write_raw(bytes, n) bind(C, name='aura_con_write_raw')
                use iso_c_binding, only: c_int, c_char
                character(kind=c_char), intent(in) :: bytes(*)
                integer(kind=c_int), value :: n
            end subroutine
        end interface

        ! Walk the run and emit an SGR escape whenever the cell attributes
        ! change (color/bold/reverse), then write the text in segments.
        ! This is required because a single "dirty run" may span multiple
        ! colors (e.g. "normal REDTEXT done").
        prev_fg = -1; prev_bg = -1; prev_bold = .false.; prev_rev = .false.
        seg_start = 1
        k = 0
        do i = 1, min(nchars, 500)
            cidx = scol + i - 1
            cur = screen%cells(srow, cidx)
            if (i > 1) then
                if (cur%fg /= prev_fg .or. cur%bg /= prev_bg &
                    .or. cur%bold .neqv. prev_bold .or. cur%reverse .neqv. prev_rev) then
                        ! flush the segment collected so far
                    if (k > 0) then
                        cc = int(col0 + (seg_start - 1), c_int)
                        rr = int(row0, c_int)
                        call con_write_at_w(cc, rr, w, int(k, c_int))
                        k = 0
                    end if
                    seg_start = i
                end if
            end if
            ! emit SGR if this cell's attributes differ from the previous
            if (cur%fg /= prev_fg .or. cur%bg /= prev_bg &
                .or. cur%bold .neqv. prev_bold .or. cur%reverse .neqv. prev_rev) then
                slen = build_sgr(cur, sgr)
                if (slen > 0) call aura_con_write_raw(sgr, int(slen, c_int))
                prev_fg = cur%fg; prev_bg = cur%bg
                prev_bold = cur%bold; prev_rev = cur%reverse
            end if
            k = k + 1
            w(k) = int(ichar(chars(i)), kind=2)
        end do
        if (k > 0) then
            cc = int(col0 + (seg_start - 1), c_int)
            rr = int(row0, c_int)
            call con_write_at_w(cc, rr, w, int(k, c_int))
        end if
        ! update mirror for this run incl attributes
        mirror%last(srow, scol:scol + nchars - 1) = screen%cells(srow, scol:scol + nchars - 1)
    end subroutine

    ! Build an SGR escape sequence for a cell's attributes. Returns byte length.
    ! sgr must be at least 32 chars. Emits nothing (len=0) for default attributes.
    function build_sgr(cell, sgr) result(slen)
        type(term_cell), intent(in) :: cell
        character(kind=c_char), intent(out) :: sgr(*)
        integer :: slen
        integer :: p, n
        character(len=24) :: s
        s = ''
        n = 0
        if (cell%reverse) then
            n = n + 1; s(n:n) = '7'
        end if
        if (cell%bold) then
            if (n > 0) then; n = n + 1; s(n:n) = ';'; end if
            n = n + 1; s(n:n) = '1'
        end if
        if (cell%fg /= 7) then
            if (n > 0) then; n = n + 1; s(n:n) = ';'; end if
            write (s(n+1:n+7), '(A,I2.2)') '38;5;', cell%fg
            n = n + 7
        end if
        if (cell%bg /= 0) then
            if (n > 0) then; n = n + 1; s(n:n) = ';'; end if
            write (s(n+1:n+7), '(A,I2.2)') '48;5;', cell%bg
            n = n + 7
        end if
        if (n == 0) then
            slen = 0
            return
        end if
        ! wrap in ESC[ ... m
        sgr(1) = achar(27); sgr(2) = '['
        do p = 1, n
            sgr(2 + p) = s(p:p)
        end do
        sgr(3 + n) = 'm'
        slen = 3 + n
    end function

    ! Status line at the bottom: styled session tab bar + AI hint.
    subroutine render_status_line(reg, cols)
        type(workspace_registry), intent(inout) :: reg
        integer, intent(in) :: cols
        type(workspace), pointer :: ws
        integer :: i, pos, rr
        character(len=16) :: num
        rr = rows_for_status()
        ws => reg%current()
        ! clear the whole strip first
        call con_write_at_s(0, rr, repeat(' ', min(cols, 500)), C_BG, C_BG, .false., .false.)
        pos = 1
        ! brand mark + workspace name
        call con_write_at_s(pos, rr, b_dot()//' '//trim(ws%name), C_ACCENT, C_BG, .true., .false.)
        pos = pos + 2 + len_trim(ws%name) + 2
        ! tabs
        do i = 1, ws%n_sess
            if (i == ws%active) then
                write (num, '(I0,A)') i, ':*'
                call con_write_at_s(pos, rr, ' '//trim(num)//' '//adjustl(ws%titles(i))//' ', &
                                    C_BG, C_ACCENT, .true., .false.)
            else
                write (num, '(I0,A)') i, ':'
                call con_write_at_s(pos, rr, ' '//trim(num)//' '//adjustl(ws%titles(i))//' ', &
                                    C_DIM, C_BG, .false., .false.)
            end if
            pos = pos + 2 + len_trim(num) + 1 + len_trim(adjustl(ws%titles(i))) + 2
            if (pos > cols - 16) exit
        end do
        if (ws%scroll_mode) then
            call con_write_at_s(max(0, cols - 11), rr, ' [SCROLL] ', C_WARN, C_BG, .true., .false.)
        else
            call con_write_at_s(max(0, cols - 11), rr, ' Ctrl+A AI', C_ACCENT2, C_BG, .false., .false.)
        end if
    end subroutine

    function rows_for_status() result(r)
        use aura_keys, only: con_get_size_f
        integer :: r, c2
        call con_get_size_f(c2, r)
        r = r - 1     ! last row, 0-based
    end function

    ! Scroll mode: paint lines from the scrollback ring instead of grid.
    ! sb holds the lines (plain text); start_line is 1-based index of top row.
    subroutine render_scrollback_view(sb, n_total, start_line, rows, cols)
        integer, intent(in) :: n_total, start_line, rows, cols
        character(len=*), intent(in) :: sb(:)
        character(len=4096) :: viewline
        integer :: r, idx

        do r = 0, rows - 2          ! keep last row for status
            idx = start_line + r
            viewline = ''
            if (idx >= 1 .and. idx <= n_total) then
                viewline = adjustl(sb(idx))
            end if
            call pad_and_write(r, viewline, cols)
        end do
    end subroutine

    subroutine pad_and_write(row0, text, cols)
        use iso_c_binding, only: c_int
        integer, intent(in) :: row0, cols
        character(len=*), intent(in) :: text
        integer(kind=2) :: w(1024)
        integer :: j, lim
        integer(c_int) :: cc, rr
        character(len=1024) :: padded

        padded = repeat(' ', min(cols, 1000))
        lim = min(cols, 1000, len(text))
        padded(1:max(1,lim)) = text(1:max(1,lim))
        do j = 1, min(cols, 1000)
            w(j) = int(ichar(padded(j:j)), kind=2)
        end do
        cc = 0_c_int
        rr = int(row0, c_int)
        call con_write_at_w(cc, rr, w, int(min(cols, 1000), c_int))
    end subroutine

    pure logical function cells_equal(a, b)
        type(term_cell), intent(in) :: a, b
        cells_equal = a%ch == b%ch .and. a%fg == b%fg .and. a%bg == b%bg .and. &
                      a%bold .eqv. b%bold .and. a%reverse .eqv. b%reverse
    end function

end module
