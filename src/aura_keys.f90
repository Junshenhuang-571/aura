! aura_keys.f90 — Fortran facade over the console bridge: raw key polling,
! key classification (reserved vs pass-through), UTF-8 encoding of chars.
module aura_keys
    use iso_c_binding, only: c_int
    use iso_fortran_env, only: wchar => int32
    implicit none
    private

    ! key event as seen by Fortran
    integer, parameter, public :: KEV_NONE   = 0
    integer, parameter, public :: KEV_CHAR   = 1
    integer, parameter, public :: KEV_SPECIAL= 2
    integer, parameter, public :: KEV_RESIZE = 3

    type, public :: key_event
        integer :: kind = 0        ! KEV_*
        integer :: codepoint = 0   ! char keys (unicode)
        integer :: special = 0     ! 1..22 for Up/Down/.../F12
        logical :: ctrl = .false., shift = .false., alt = .false.
    end type

    ! special ids
    integer, parameter, public :: KEY_UP=1, KEY_DOWN=2, KEY_LEFT=3, KEY_RIGHT=4
    integer, parameter, public :: KEY_HOME=5, KEY_END=6, KEY_PGUP=7, KEY_PGDN=8
    integer, parameter, public :: KEY_DEL=9,  KEY_INS=10

    public :: keys_raw_enter, keys_raw_exit, poll_key, key_to_vt
    public :: con_write_at_f, con_get_size_f, con_write_at_w

    interface
        subroutine aura_con_raw_enter() bind(C, name='aura_con_raw_enter')
        end subroutine
        subroutine aura_con_raw_exit() bind(C, name='aura_con_raw_exit')
        end subroutine
        subroutine aura_con_hide_cursor() bind(C, name='aura_con_hide_cursor')
        end subroutine
        subroutine aura_con_show_cursor() bind(C, name='aura_con_show_cursor')
        end subroutine
        subroutine aura_con_get_size(cols, rows) bind(C, name='aura_con_get_size')
            import c_int
            integer(kind=c_int) :: cols, rows
        end subroutine
        subroutine aura_con_write_at(col, row, text, nchars) bind(C, name='aura_con_write_at')
            import c_int
            integer(kind=c_int), value :: col, row, nchars
            integer(kind=4), intent(in) :: text(*)
        end subroutine
        function aura_con_poll_key_wrapper(wait_ms, ev_type, ch, spk, ctrl, shift, alt) &
                bind(C, name='aura_con_poll_key_wrapper')
            import c_int
            integer(kind=c_int), value :: wait_ms
            integer(kind=c_int) :: ev_type, ch, spk, ctrl, shift, alt
            integer(kind=c_int) :: aura_con_poll_key_wrapper
        end function
    end interface

contains

    subroutine keys_raw_enter()
        call aura_con_raw_enter()
        call aura_con_hide_cursor()
    end subroutine

    subroutine keys_raw_exit()
        call aura_con_show_cursor()
        call aura_con_raw_exit()
    end subroutine

    subroutine con_get_size_f(cols, rows)
        integer, intent(out) :: cols, rows
        integer(c_int) :: c, r
        call aura_con_get_size(c, r)
        cols = int(c); rows = int(r)
    end subroutine

    ! Poll with timeout. Returns .true. if an event arrived.
    function poll_key(wait_ms, ev) result(got)
        integer, intent(in) :: wait_ms
        type(key_event), intent(out) :: ev
        logical :: got
        integer(c_int) :: r, t, chv, spv, cv, sv, av

        ev%kind = KEV_NONE; ev%codepoint = 0; ev%special = 0
        ev%ctrl = .false.; ev%shift = .false.; ev%alt = .false.
        t = 0; chv = 0; spv = 0; cv = 0; sv = 0; av = 0
        r = aura_con_poll_key_wrapper(int(wait_ms, c_int), t, chv, spv, cv, sv, av)
        got = r /= 0
        if (.not. got) return
        select case (int(t))
        case (1)
            ev%kind = KEV_CHAR
            ev%codepoint = int(chv)
        case (2)
            ev%kind = KEV_SPECIAL
            ev%special = int(spv)
        case (3)
            ev%kind = KEV_RESIZE
        end select
        ev%ctrl = int(cv) /= 0
        ev%shift = int(sv) /= 0
        ev%alt = int(av) /= 0
    end function

    ! Encode a key event as the VT/byte sequence the child PTY expects.
    ! Returns length in n; seq holds bytes.
    subroutine key_to_vt(ev, seq, n)
        type(key_event), intent(in) :: ev
        character(len=1), intent(out) :: seq(16)
        integer, intent(out) :: n

        n = 0
        select case (ev%kind)
        case (KEV_CHAR)
            if (ev%ctrl) then
                if (ev%codepoint >= iachar('a') .and. ev%codepoint <= iachar('z')) then
                    seq(1) = achar(ev%codepoint - iachar('a') + 1)
                    n = 1
                else if (ev%codepoint >= iachar('@') .and. ev%codepoint <= iachar('_')) then
                    seq(1) = achar(ev%codepoint - iachar('@'))
                    n = 1
                else
                    seq(1) = achar(ev%codepoint); n = 1
                end if
            else if (ev%codepoint < 128) then
                seq(1) = achar(ev%codepoint); n = 1
            else
                call utf8_encode(ev%codepoint, seq, n)
            end if
        case (KEV_SPECIAL)
            select case (ev%special)
            case (KEY_UP);    seq(1:3) = (/ achar(27), '[', 'A' /); n = 3
            case (KEY_DOWN);  seq(1:3) = (/ achar(27), '[', 'B' /); n = 3
            case (KEY_RIGHT); seq(1:3) = (/ achar(27), '[', 'C' /); n = 3
            case (KEY_LEFT);  seq(1:3) = (/ achar(27), '[', 'D' /); n = 3
            case (KEY_HOME);  seq(1:4) = (/ achar(27), '[', '1', '~' /); n = 4
            case (KEY_END);   seq(1:4) = (/ achar(27), '[', '4', '~' /); n = 4
            case (KEY_PGUP);  seq(1:4) = (/ achar(27), '[', '5', '~' /); n = 4
            case (KEY_PGDN);  seq(1:4) = (/ achar(27), '[', '6', '~' /); n = 4
            case (KEY_DEL);   seq(1:4) = (/ achar(27), '[', '3', '~' /); n = 4
            case (KEY_INS);   seq(1:4) = (/ achar(27), '[', '2', '~' /); n = 4
            case default
                if (ev%special >= 11 .and. ev%special <= 14) then
                    seq(1:3) = (/ achar(27), 'O', achar(iachar('P') + ev%special - 11) /)
                    n = 3
                end if
            end select
        end select
    end subroutine

    ! Raw UTF-16 code-unit array writer (used by the diff renderer).
    subroutine con_write_at_w(col, row, w, n)
        use iso_c_binding, only: c_int
        integer(c_int), intent(in) :: col, row, n
        integer(kind=4), intent(in) :: w(*)
        call aura_con_write_at(col, row, w(1:max(1,int(n))), n)
    end subroutine

    ! Write a UTF-8 Fortran string at (row,col), converting to UTF-16 for the console.
    ! col/row are 0-based here to match the C bridge contract.
    subroutine con_write_at_f(col, row, s)
        integer, intent(in) :: col, row
        character(len=*), intent(in) :: s
        integer(kind=4) :: w(512)
        integer :: n, i, cp, nbytes
        integer(c_int) :: c, r

        n = 0
        i = 1
        do while (i <= len_trim(s) .and. n < 500)
            cp = ichar(s(i:i))
            if (cp >= 240 .and. i + 3 <= len(s)) then        ! 4-byte
                cp = iand(ishft(iand(cp, 7), 18) + ishft(iand(ichar(s(i+1:i+1)), 63), 12) &
                    + ishft(iand(ichar(s(i+2:i+2)), 63), 6) + iand(ichar(s(i+3:i+3)), 63), 0)
                nbytes = 4
            else if (cp >= 224 .and. i + 2 <= len(s)) then   ! 3-byte
                cp = iand(ishft(iand(cp, 15), 12) + ishft(iand(ichar(s(i+1:i+1)), 63), 6) &
                    + iand(ichar(s(i+2:i+2)), 63), 0)
                nbytes = 3
            else if (cp >= 192 .and. i + 1 <= len(s)) then   ! 2-byte
                cp = ishft(iand(cp, 31), 6) + iand(ichar(s(i+1:i+1)), 63)
                nbytes = 2
            else                                             ! 1-byte / literal
                nbytes = 1
            end if
            n = n + 1
            w(n) = int(cp, kind=4)
            i = i + nbytes
        end do
        c = int(col, c_int); r = int(row, c_int)
        call aura_con_write_at(c, r, w, int(n, c_int))
    end subroutine

    pure subroutine utf8_encode(cp, seq, n)
        integer, intent(in) :: cp
        character(len=1), intent(out) :: seq(4)
        integer, intent(out) :: n
        if (cp < 128) then
            seq(1) = achar(cp); n = 1
        else if (cp < 2048) then
            seq(1) = achar(192 + ishft(cp, -6))
            seq(2) = achar(128 + iand(cp, 63)); n = 2
        else if (cp < 65536) then
            seq(1) = achar(224 + ishft(cp, -12))
            seq(2) = achar(128 + iand(ishft(cp, -6), 63))
            seq(3) = achar(128 + iand(cp, 63)); n = 3
        else
            seq(1) = achar(240 + ishft(cp, -18))
            seq(2) = achar(128 + iand(ishft(cp, -12), 63))
            seq(3) = achar(128 + iand(ishft(cp, -6), 63))
            seq(4) = achar(128 + iand(cp, 63)); n = 4
        end if
    end subroutine

end module
