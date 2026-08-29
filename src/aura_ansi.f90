! aura_ansi.f90 — ANSI/VT escape-sequence parser
! MVP scope: SGR colours/styles, cursor movement, clear screen/line, scroll.
module aura_ansi
    use iso_fortran_env, only: i4 => int32
    implicit none
    public :: blank_all
    private

    ! Parsed cell: character + colour attributes
    type, public :: term_cell
        character(len=1)  :: ch      = ' '
        integer(i4)       :: fg      = 7
        integer(i4)       :: bg      = 0
        logical           :: bold    = .false.
        logical           :: reverse = .false.
    end type

    type, public :: ansi_parser
        type(term_cell), allocatable :: cells(:, :)   ! screen buffer (rows, cols)
        type(term_cell), allocatable :: saved_cells(:, :) ! main screen when alt active
        logical     :: alt_screen = .false.
        integer(i4) :: cur_row = 1, cur_col = 1
        integer(i4) :: rows = 24, cols = 80
        integer(i4) :: cur_fg = 7, cur_bg = 0
        logical     :: cur_bold = .false., cur_reverse = .false.
        integer     :: state = 0          ! 0=text, 1=esc, 2=csi
        character(len=64) :: csi_buf = ''
    contains
        procedure :: feed
        procedure :: resize
        procedure :: scroll_check
        procedure, private :: put_char
        procedure, private :: handle_csi
        procedure, private :: apply_sgr
    end type

contains

    subroutine resize(self, r, c)
        class(ansi_parser), intent(inout) :: self
        integer(i4), intent(in) :: r, c
        if (allocated(self%cells)) deallocate(self%cells)
        allocate(self%cells(r, c))
        call blank_all(self%cells)
        self%rows = r; self%cols = c
        self%cur_row = 1
        self%cur_col = 1
    end subroutine

    pure subroutine blank_all(cells)
        type(term_cell), intent(inout) :: cells(:, :)
        integer :: i, j
        do j = 1, size(cells, 2)
            do i = 1, size(cells, 1)
                cells(i, j)%ch = ' '
                cells(i, j)%fg = 7; cells(i, j)%bg = 0
                cells(i, j)%bold = .false.; cells(i, j)%reverse = .false.
            end do
        end do
    end subroutine

    subroutine feed(self, text)
        class(ansi_parser), intent(inout) :: self
        character(len=*), intent(in) :: text
        integer :: i, n
        character :: ch
        n = len(text)
        do i = 1, n
            ch = text(i:i)
            select case (self%state)
            case (0)
                select case (ch)
                case (achar(27))
                    self%state = 1; self%csi_buf = ''
                case (achar(13))
                    self%cur_col = 1
                case (achar(10))
                    self%cur_row = self%cur_row + 1
                    call self%scroll_check()
                    self%cur_col = 1
                case (achar(8))
                    if (self%cur_col > 1) self%cur_col = self%cur_col - 1
                case (achar(9))
                    self%cur_col = min(self%cols, ((self%cur_col - 1)/8 + 1)*8 + 1)
                case default
                    if (iachar(ch) >= 32 .or. ch == ' ') call self%put_char(ch)
                end select
            case (1)
                select case (ch)
                case ('[')
                    self%state = 2; self%csi_buf = ''
                case (']')
                    self%state = 3                 ! OSC: swallow until BEL or ST
                case default
                    self%state = 0
                end select
            case (2)
                if ((ch >= '0' .and. ch <= '9') .or. ch == ';' .or. ch == '?' .or. ch == '-') then
                    self%csi_buf = trim(self%csi_buf) // ch
                else
                    call self%handle_csi(ch)
                    self%state = 0
                end if
            case (3)                               ! OSC: swallow until BEL or ESC\
                if (ch == achar(7)) then
                    self%state = 0                 ! BEL terminator
                else if (ch == achar(27)) then
                    self%state = 4                 ! possible ST (ESC \)
                end if
            case (4)                               ! OSC-ST continuation (after ESC)
                if (ch == '\') then
                    self%state = 0                 ! ST = ESC \
                else
                    self%state = 0                 ! mismatched; bail to ground
                end if
            end select
        end do
    end subroutine

    subroutine scroll_check(self)
        class(ansi_parser), intent(inout) :: self
        integer :: j
        if (self%cur_row > self%rows) then
            self%cells(1:self%rows - 1, :) = self%cells(2:self%rows, :)
            do j = 1, self%cols
                self%cells(self%rows, j)%ch = ' '
            end do
            self%cur_row = self%rows
        end if
    end subroutine

    subroutine put_char(self, ch)
        class(ansi_parser), intent(inout) :: self
        character, intent(in) :: ch
        if (self%cur_col > self%cols) then
            self%cur_col = 1
            self%cur_row = self%cur_row + 1
            call self%scroll_check()
        end if
        ! Clamp cursor to valid bounds to prevent segfault
        self%cur_row = max(1, min(self%rows, self%cur_row))
        self%cur_col = max(1, min(self%cols, self%cur_col))
        self%cells(self%cur_row, self%cur_col)%ch = ch
        self%cells(self%cur_row, self%cur_col)%fg = self%cur_fg
        self%cells(self%cur_row, self%cur_col)%bg = self%cur_bg
        self%cells(self%cur_row, self%cur_col)%bold = self%cur_bold
        self%cells(self%cur_row, self%cur_col)%reverse = self%cur_reverse
        self%cur_col = self%cur_col + 1
    end subroutine

    subroutine get_param(buf, idx, defval, val)
        character(len=*), intent(in) :: buf
        integer, intent(in) :: idx, defval
        integer, intent(out) :: val
        integer :: i, start, cnt, ios
        val = defval
        start = 1; cnt = 0
        if (len_trim(buf) == 0) return
        do i = 1, len_trim(buf) + 1
            if (i > len_trim(buf)) then
                cnt = cnt + 1
                if (cnt == idx) then
                    if (start <= len_trim(buf)) then
                        read (buf(start:len_trim(buf)), *, iostat=ios) val
                    end if
                    return
                end if
                exit
            else if (buf(i:i) == ';') then
                cnt = cnt + 1
                if (cnt == idx) then
                    read(buf(start:i - 1), *, iostat=ios) val
                    if (ios /= 0) val = defval
                    return
                end if
                start = i + 1
            end if
        end do
    end subroutine

    subroutine handle_csi(self, final)
        class(ansi_parser), intent(inout) :: self
        character, intent(in) :: final
        character(len=:), allocatable :: buf
        integer :: p, sub, fg, bg

        buf = trim(self%csi_buf)

        select case (final)
        case ('H', 'f')
            call get_param(buf, 1, 1, p); self%cur_row = max(1, min(self%rows, p))
            call get_param(buf, 2, 1, p); self%cur_col = max(1, min(self%cols, p))
        case ('A')
            call get_param(buf, 1, 1, p); self%cur_row = max(1, self%cur_row - max(1, p))
        case ('B')
            call get_param(buf, 1, 1, p); self%cur_row = min(self%rows, self%cur_row + max(1, p))
        case ('C')
            call get_param(buf, 1, 1, p); self%cur_col = min(self%cols, self%cur_col + max(1, p))
        case ('D')
            call get_param(buf, 1, 1, p); self%cur_col = max(1, self%cur_col - max(1, p))
        case ('E')
            call get_param(buf, 1, 1, p)
            self%cur_row = min(self%rows, self%cur_row + max(1, p)); self%cur_col = 1
        case ('F')
            call get_param(buf, 1, 1, p)
            self%cur_row = max(1, self%cur_row - max(1, p)); self%cur_col = 1
        case ('G')
            call get_param(buf, 1, 1, p); self%cur_col = max(1, min(self%cols, p))
        case ('d')
            call get_param(buf, 1, 1, p); self%cur_row = max(1, min(self%rows, p))
        case ('J')
            call get_param(buf, 1, 0, p)
            select case (p)
            case (2, 3)
                call blank_all(self%cells)
            case (1)
                self%cells(1:self%cur_row - 1, :)%ch = ' '
                self%cells(self%cur_row, :self%cur_col)%ch = ' '
            case default
                self%cells(self%cur_row, self%cur_col:)%ch = ' '
                self%cells(min(self%rows,self%cur_row+1):, :)%ch = ' '
            end select
        case ('K')
            call get_param(buf, 1, 0, p)
            select case (p)
            case (1)
                self%cells(self%cur_row, 1:max(1, self%cur_col - 1))%ch = ' '
            case (2)
                self%cells(self%cur_row, :)%ch = ' '
            case default
                self%cells(self%cur_row, self%cur_col:)%ch = ' '
            end select
        case ('m')
            call self%apply_sgr(buf)
        case ('h', 'l')
            ! private modes: ?1049/?47/?1047 alt screen, ?25 cursor visibility
            if (len(buf) > 0 .and. buf(1:1) == '?') then
                call get_param(buf(2:), 1, 0, p)
                select case (p)
                case (1049, 47, 1047)
                    if (final == 'h' .and. .not. self%alt_screen) then
                        if (.not. allocated(self%saved_cells)) &
                            allocate(self%saved_cells(self%rows, self%cols))
                        self%saved_cells = self%cells
                        call blank_all(self%cells)
                        self%cur_row = 1; self%cur_col = 1
                        self%alt_screen = .true.
                    else if (final == 'l' .and. self%alt_screen) then
                        if (size(self%saved_cells, 1) == self%rows .and. &
                            size(self%saved_cells, 2) == self%cols) then
                            self%cells = self%saved_cells
                        end if
                        self%alt_screen = .false.
                    end if
                end select
            end if
        case default
            ! unhandled CSI final — ignore (MVP scope)
            continue
        end select
        if (.false.) then
            sub = 0; fg = 0; bg = 0   ! silence unused-var warnings for future work
        end if
    end subroutine

    subroutine apply_sgr(self, buf)
        class(ansi_parser), intent(inout) :: self
        character(len=*), intent(in) :: buf
        integer :: nparts, i, code, base
        integer :: codes(32)

        nparts = 0
        if (len_trim(buf) == 0) then
            codes(1) = 0; nparts = 1
        else
            do i = 1, len_trim(buf)
                if (buf(i:i) == ';') nparts = nparts + 1
            end do
            nparts = nparts + 1
            nparts = min(nparts, 32)
            do i = 1, nparts
                call get_param(buf, i, 0, codes(i))
            end do
        end if

        i = 1
        do while (i <= nparts)
            code = codes(i)
            select case (code)
            case (0)
                self%cur_fg = 7; self%cur_bg = 0
                self%cur_bold = .false.; self%cur_reverse = .false.
            case (1);  self%cur_bold = .true.
            case (22); self%cur_bold = .false.
            case (7);  self%cur_reverse = .true.
            case (27); self%cur_reverse = .false.
            case (30:37)
                self%cur_fg = code - 30
            case (39)
                self%cur_fg = 7
            case (40:47)
                self%cur_bg = code - 40
            case (49)
                self%cur_bg = 0
            case (90:97)
                self%cur_fg = code - 90 + 8
            case (100:107)
                self%cur_bg = code - 100 + 8
            case (38, 48)
                base = merge(30, 40, code == 38)
                if (i + 1 <= nparts .and. codes(i + 1) == 5) then
                    ! 256-colour: map to nearest of 16 for MVP
                    if (i + 2 <= nparts) then
                        if (base == 30) then
                            self%cur_fg = min(15_i4, int(codes(i + 2)/43, kind=i4) + merge(12, 0, codes(i + 2)/43 >= 2))
                            if (codes(i + 2) < 8) self%cur_fg = codes(i + 2)
                            if (codes(i + 2) >= 8 .and. codes(i + 2) < 16) self%cur_fg = codes(i + 2)
                        else
                            self%cur_bg = min(15_i4, max(0_i4, int(codes(i + 2)/43, kind=i4)))
                            if (codes(i + 2) < 16) self%cur_bg = codes(i + 2)
                        end if
                    end if
                    i = i + 2
                else
                    i = i + 2   ! skip unsupported extended colour forms
                end if
            case default
                continue
            end select
            i = i + 1
        end do
    end subroutine

end module aura_ansi
