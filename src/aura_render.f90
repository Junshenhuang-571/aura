! aura_render.f90 — diff renderer: paints the active session's cell grid to the
! host console with minimal updates; scroll-mode painter; status-line tab bar.
module aura_render
    use iso_fortran_env, only: i4 => int32
    use aura_ansi
    use aura_keys
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
    end subroutine

    subroutine write_run(row0, col0, chars, nchars, screen, srow, scol)
        use iso_c_binding, only: c_int
        integer, intent(in) :: row0, col0, nchars, srow, scol
        character(len=1), intent(in) :: chars(:)
        type(ansi_parser), intent(in) :: screen
        character(len=512) :: buf
        integer(kind=4) :: w(512)
        integer :: i, k
        integer(c_int) :: cc, rr

        k = 0
        do i = 1, min(nchars, 500)
            k = k + 1
            w(k) = int(ichar(chars(i)), kind=4)
        end do
        cc = int(col0, c_int); rr = int(row0, c_int)
        call con_write_at_w(cc, rr, w, int(k, c_int))
        ! update mirror for this run incl attributes
        mirror%last(srow, scol:scol + nchars - 1) = screen%cells(srow, scol:scol + nchars - 1)
    end subroutine

    ! Status line at the bottom: tab bar showing sessions.
    subroutine render_status_line(n_sessions, active_idx, titles, scroll_mode, cols)
        integer, intent(in) :: n_sessions, active_idx, cols
        character(len=*), intent(in) :: titles(:)
        logical, intent(in) :: scroll_mode
        character(len=1024) :: line
        integer :: i, pos
        character(len=16) :: num

        line = repeat(' ', max(0, min(cols, 1024)))
        pos = 1
        do i = 1, n_sessions
            if (i == active_idx) then
                write (num, '(I0,A)') i, ':*'
            else
                write (num, '(I0,A)') i, ': '
            end if
            line(pos:) = trim(num)//' '//adjustl(titles(i))
            pos = pos + len_trim(num) + 1 + len_trim(adjustl(titles(i))) + 2
            if (pos > cols - 12) exit
        end do
        if (scroll_mode) then
            line(max(1, cols - 11):cols) = ' [SCROLL]   '
        else
            line(max(1, cols - 11):cols) = ' Ctrl+Sh+A AI'
        end if
        ! draw inverse-video-ish by writing row rows (last console row, 0-based rows-1)
        block
            use iso_c_binding, only: c_int
            integer(c_int) :: cc, rr
            integer(kind=4) :: w(1024)
            integer :: j
            do j = 1, min(cols, len_trim(line))
                w(j) = int(ichar(line(j:j)), kind=4)
            end do
            cc = 0_c_int
            rr = int(rows_for_status(), c_int)
            call con_write_at_w(cc, rr, w, int(min(cols, len_trim(line)), c_int))
        end block
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
        integer(kind=4) :: w(1024)
        integer :: j, lim
        integer(c_int) :: cc, rr
        character(len=1024) :: padded

        padded = repeat(' ', min(cols, 1000))
        lim = min(cols, 1000, len(text))
        padded(1:max(1,lim)) = text(1:max(1,lim))
        do j = 1, min(cols, 1000)
            w(j) = int(ichar(padded(j:j)), kind=4)
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
