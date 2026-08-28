! aura_session.f90 — terminal session: PTY handle + scrollback + ANSI screen state
module aura_session
    use iso_fortran_env, only: i4 => int32, i8 => int64
    use aura_ansi
    implicit none
    private

    integer(i4), parameter, public :: MAX_LINES = 4096
    integer(i4), parameter, public :: LINE_LEN = 256

    type, public :: term_session
        character(len=:), allocatable :: scrollback(:)  ! allocatable to avoid stack overflow
        integer(i4)                  :: n_lines = 0
        character(len=:), allocatable:: pending
        type(ansi_parser)            :: screen
        integer(i8)   :: pty_handle = 0
        logical                      :: alive = .false.
        character(len=512)           :: cwd = ''
        character(len=LINE_LEN)      :: input_line = ''
    contains
        procedure :: append_output
        procedure :: last_lines
        procedure :: push_line
        procedure :: render_text
    end type

contains

    subroutine append_output(self, chunk)
        class(term_session), intent(inout) :: self
        character(len=*), intent(in) :: chunk
        integer :: i, start, n
        character :: ch
        logical :: in_esc

        ! Feed everything to the screen parser (it consumes escapes),
        ! and simultaneously accumulate printable scrollback lines.
        call self%screen%feed(chunk)

        n = len(chunk)
        start = 1; in_esc = .false.
        do i = 1, n
            ch = chunk(i:i)
            select case (ch)
            case (achar(10))
                call self%push_line(self%pending//chunk(start:i - 1))
                self%pending = ''
                start = i + 1
            case (achar(27))
                in_esc = .true.
            case ('m', 'H', 'J', 'K', 'A', 'B', 'C', 'D')
                if (in_esc) then
                    in_esc = .false.
                    if (start <= i - 2) self%pending = self%pending  ! drop escape text below
                end if
            case default
                if (iachar(ch) < 32) then
                    if (start <= i - 1 .and. .not. in_esc) &
                        self%pending = strip_tail(self%pending)//chunk(start:i - 1)
                    start = i + 1
                end if
            end select
        end do
        if (.not. in_esc .and. start <= n) &
            self%pending = strip_tail(self%pending)//chunk(start:n)
    end subroutine

    pure function strip_tail(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        r = trim(s)
    end function

    subroutine push_line(self, line)
        class(term_session), intent(inout) :: self
        character(len=*), intent(in) :: line
        integer(i4) :: i
        character(len=LINE_LEN) :: cleaned
        if (.not. allocated(self%scrollback)) allocate(character(len=LINE_LEN)::self%scrollback(MAX_LINES))
        cleaned = strip_ctrl(line)
        if (self%n_lines >= MAX_LINES) then
            do i = 1, MAX_LINES - 1
                self%scrollback(i) = self%scrollback(i + 1)
            end do
            self%n_lines = MAX_LINES - 1
        end if
        self%n_lines = self%n_lines + 1
        self%scrollback(self%n_lines) = cleaned(:min(LINE_LEN, len(cleaned)))
    end subroutine

    pure function strip_ctrl(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer :: i
        character :: ch
        logical :: esc
        r = ''; esc = .false.
        do i = 1, len_trim(s)
            ch = s(i:i)
            if (esc) then
                if (ch == 'm' .or. ch == 'H' .or. ch == 'J' .or. ch == 'K' .or. &
                    ch == 'A' .or. ch == 'B' .or. ch == 'C' .or. ch == 'D') esc = .false.
            else if (ch == achar(27)) then
                esc = .true.
            else if (iachar(ch) >= 32) then
                r = r//ch
            else if (ch == achar(9)) then
                r = r//'    '
            end if
        end do
    end function

    function last_lines(self, n) result(text)
        class(term_session), intent(in) :: self
        integer, intent(in) :: n
        character(len=:), allocatable :: text
        integer :: i, lo
        text = ''
        lo = max(1, self%n_lines - n + 1)
        do i = lo, self%n_lines
            text = text//trim(self%scrollback(i))//new_line('a')
        end do
        if (len_trim(self%pending) > 0) text = text//trim(strip_ctrl(self%pending))
    end function

    ! Plain-text render of current screen (used by CLI renderer & tests)
    function render_text(self) result(text)
        class(term_session), intent(in) :: self
        character(len=self%screen%cols) :: rowbuf
        character(len=:), allocatable :: text
        integer :: r, c
        logical :: nonblank
        text = ''
        do r = 1, self%screen%rows
            nonblank = .false.
            do c = self%screen%cols, 1, -1
                if (self%screen%cells(r, c)%ch /= ' ') then
                    nonblank = .true.; exit
                end if
            end do
            if (.not. nonblank) cycle
            rowbuf = ' '
            do c = 1, self%screen%cols
                rowbuf(c:c) = self%screen%cells(r, c)%ch
            end do
            text = text//trim(rowbuf)//new_line('a')
        end do
    end function

end module
