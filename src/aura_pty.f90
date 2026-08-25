! aura_pty.f90 — pseudo-terminal management, cross-platform facade.
!
! Unix:   forkpty() from libutil via ISO_C_BINDING (see csrc/aura_pty_unix.c)
! Windows: ConPTY (CreatePseudoConsole) via ISO_C_BINDING (csrc/aura_pty_win.c)
!
! The Fortran side only talks to the C ABI declared here, so both backends
! expose identical semantics: spawn -> (read/write/close), plus exit status.
module aura_pty
    use iso_c_binding
    use iso_fortran_env, only: i4 => int32
    implicit none
    private

    public :: pty_spawn, pty_read, pty_write, pty_write_bytes, pty_write_text, pty_resize, pty_close

    interface
        ! returns 0 on success; fills handle + initial pid token
        function c_pty_spawn(cmdline, cols, rows, handle_out) bind(C, name="aura_pty_spawn")
            use iso_c_binding
            integer(kind=c_int)          :: c_pty_spawn
            character(kind=c_char), intent(in) :: cmdline(*)
            integer(kind=c_int), value   :: cols, rows
            integer(kind=c_long_long), intent(out) :: handle_out
        end function

        function c_pty_read(handle, buf, buflen) bind(C, name="aura_pty_read")
            use iso_c_binding
            integer(kind=c_int)          :: c_pty_read     ! bytes read, <0 = EOF/error
            integer(kind=c_long_long), value :: handle
            character(kind=c_char), intent(out) :: buf(*)
            integer(kind=c_int), value   :: buflen
        end function

        function c_pty_write(handle, buf, nbytes) bind(C, name="aura_pty_write")
            use iso_c_binding
            integer(kind=c_int)          :: c_pty_write
            integer(kind=c_long_long), value :: handle
            character(kind=c_char), intent(in) :: buf(*)
            integer(kind=c_int), value   :: nbytes
        end function

        subroutine c_pty_resize(handle, cols, rows) bind(C, name="aura_pty_resize")
            use iso_c_binding
            integer(kind=c_long_long), value :: handle
            integer(kind=c_int), value   :: cols, rows
        end subroutine

        subroutine c_pty_close(handle) bind(C, name="aura_pty_close")
            use iso_c_binding
            integer(kind=c_long_long), value :: handle
        end subroutine
    end interface

contains

    function pty_spawn(cmdline, cols, rows, handle) result(rc)
        character(len=*), intent(in)  :: cmdline
        integer(i4), intent(in)       :: cols, rows
        integer(c_long_long), intent(out) :: handle
        integer(i4)                   :: rc
        rc = int(c_pty_spawn(f_cstr(cmdline), int(cols, c_int), int(rows, c_int), handle), i4)
    end function

    ! Returns bytes read into `buf`; 0 = nothing available right now;
    ! negative = session ended.
    function pty_read(handle, buf) result(nbytes)
        integer(c_long_long), intent(in) :: handle
        character(len=:), allocatable, intent(out) :: buf
        character(kind=c_char), target :: cbuf(4096)
        integer(c_int) :: n
        integer :: i, nbytes
        n = c_pty_read(handle, cbuf, int(size(cbuf), c_int))
        if (n <= 0) then
            buf = ''
            nbytes = int(n, i4)
            return
        end if
        allocate(character(len=n)::buf)
        do i = 1, n
            buf(i:i) = cbuf(i)
        end do
        nbytes = int(n, i4)
    end function

    function pty_write(handle, text) result(rc)
        integer(c_long_long), intent(in) :: handle
        character(len=*), intent(in) :: text
        integer(i4) :: rc
        rc = int(c_pty_write(handle, f_cstr(text // achar(13)//achar(10)), &
                             int(len_trim(text) + 2, c_int)), i4)
    end function

    ! Write raw bytes (no CR/LF appended) — used by the key pass-through.
    function pty_write_bytes(handle, bytes, nbytes) result(rc)
        integer(c_long_long), intent(in) :: handle
        character(len=1), intent(in) :: bytes(:)
        integer, intent(in) :: nbytes
        integer(i4) :: rc
        rc = int(c_pty_write(handle, c_bytes(bytes, nbytes), int(nbytes, c_int)), i4)
    end function

    function pty_write_text(handle, text) result(rc)
        integer(c_long_long), intent(in) :: handle
        character(len=*), intent(in) :: text
        integer(i4) :: rc
        rc = int(c_pty_write(handle, f_cstr(text), int(len_trim(text), c_int)), i4)
    end function

    pure function c_bytes(b, n) result(cs)
        integer, intent(in) :: n
        character(len=1), intent(in) :: b(:)
        character(kind=c_char) :: cs(n)
        integer :: i
        do i = 1, n
            cs(i) = b(i)
        end do
    end function

    subroutine pty_resize(handle, cols, rows)
        integer(c_long_long), intent(in) :: handle
        integer(i4), intent(in) :: cols, rows
        call c_pty_resize(handle, int(cols, c_int), int(rows, c_int))
    end subroutine

    subroutine pty_close(handle)
        integer(c_long_long), intent(in) :: handle
        call c_pty_close(handle)
    end subroutine

    pure function f_cstr(s) result(cs)
        character(len=*), intent(in) :: s
        character(kind=c_char), allocatable :: cs(:)
        integer :: i, n
        n = len_trim(s)
        allocate(cs(n + 1))
        do i = 1, n
            cs(i) = s(i:i)
        end do
        cs(n + 1) = c_null_char
    end function

end module aura_pty
