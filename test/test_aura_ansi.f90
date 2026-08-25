! test_aura_ansi.f90 — unit tests for the ANSI parser + session scrollback
program test_aura_ansi
    use iso_fortran_env, only: int32
    use aura_ansi
    use aura_session
    implicit none
    integer :: fails
    type(ansi_parser) :: p
    type(term_session) :: s

    fails = 0

    ! --- basic text + wrap
    call p%resize(5_int32, 10_int32)
    call p%feed('hello')
    if (p%cells(1,1)%ch /= 'h' .or. p%cells(1,5)%ch /= 'o') then
        print *, 'FAIL: plain text placement'; fails = fails + 1
    end if

    ! --- newline scroll
    call p%feed(achar(13)//achar(10))
    call p%feed('world')
    if (p%cells(2,1)%ch /= 'w') then
        print *, 'FAIL: CR/LF'; fails = fails + 1
    end if

    ! --- SGR colour
    call p%resize(3_int32, 20_int32)
    call p%feed(achar(27)//'[31mR'//achar(27)//'[0m')
    if (p%cells(1,1)%ch /= 'R' .or. p%cells(1,1)%fg /= 1) then
        print *, 'FAIL: SGR red fg, got fg=', p%cells(1,1)%fg; fails = fails + 1
    end if

    ! --- cursor move + overwrite
    call p%resize(3_int32, 20_int32)
    call p%feed('ABCD')
    call p%feed(achar(27)//'[2D')
    call p%feed('XY')
    if (p%cells(1,3)%ch /= 'X' .or. p%cells(1,4)%ch /= 'Y' .or. p%cells(1,2)%ch /= 'B') then
        print *, 'FAIL: cursor back + overwrite'; fails = fails + 1
    end if

    ! --- clear screen
    call p%resize(3_int32, 20_int32)
    call p%feed('junk everywhere')
    call p%feed(achar(27)//'[2J')
    if (p%cells(1,1)%ch /= ' ') then
        print *, 'FAIL: clear screen'; fails = fails + 1
    end if

    ! --- OSC swallowing (title sequence must not render)
    call p%resize(3_int32, 40_int32)
    call p%feed(achar(27)//']0;my title'//achar(7))
    call p%feed('ok')
    if (p%cells(1,1)%ch /= 'o') then
        print *, 'FAIL: OSC swallow'; fails = fails + 1
    end if

    ! --- session scrollback strips escapes
    call blank_session_test(s)
    call s%append_output(achar(27)//'[32mgreen text'//achar(10))
    call s%append_output('plain line'//achar(10))
    if (s%n_lines /= 2) then
        print *, 'FAIL: scrollback count=', s%n_lines; fails = fails + 1
    else if (trim(s%scrollback(1)) /= 'green text') then
        print *, 'FAIL: escape stripped -> [', trim(s%scrollback(1)), ']'; fails = fails + 1
    end if

    ! --- last_lines context helper
    block
        character(len=:), allocatable :: ctx
        ctx = s%last_lines(1)
        if (index(ctx, 'plain line') == 0) then
            print *, 'FAIL: last_lines content'; fails = fails + 1
        end if
    end block

    if (fails == 0) then
        print *, 'ALL TESTS PASSED'
    else
        print *, fails, ' TEST(S) FAILED'
        stop 1
    end if

contains
    subroutine blank_session_test(s)
        use iso_fortran_env, only: i4 => int32
        type(term_session), intent(inout) :: s
        s%n_lines = 0
        s%pending = ''
        call s%screen%resize(24_int32, 80_int32)
    end subroutine
end program
