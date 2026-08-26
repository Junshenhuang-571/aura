! aura_theme.f90 — cohesive dark palette + ANSI/SGR + box-drawing helpers.
! Pure, dependency-free. Other modules `use` this for color.
! ANSI 256 colors. Aura's look: near-black bg, light fg, purple + teal accents.
module aura_theme
    implicit none
    private
    public :: C_BG, C_FG, C_DIM, C_BORDER, C_ACCENT, C_ACCENT2, &
              C_SUCCESS, C_WARN, C_ERR, C_TITLE, &
              sgr, sgr_reset, con_s, &
              b_h, b_v, b_tl, b_tr, b_bl, b_br, b_ml, b_mr, b_dot, b_arrow

    ! palette (ANSI 256)
    integer, parameter :: C_BG     = 234   ! #1c1c1c near-black
    integer, parameter :: C_FG     = 252   ! #d0d0d0 light grey
    integer, parameter :: C_DIM    = 244   ! #808080 muted
    integer, parameter :: C_BORDER = 239   ! #4e4e4e subtle lines
    integer, parameter :: C_ACCENT = 141  ! #af87ff purple
    integer, parameter :: C_ACCENT2= 43   ! #00d7af teal
    integer, parameter :: C_SUCCESS= 78   ! #5fff5f green
    integer, parameter :: C_WARN   = 214  ! #ffaf00 amber
    integer, parameter :: C_ERR    = 203  ! #ff5f5f red
    integer, parameter :: C_TITLE  = 147  ! #afffd7 mint

contains

    ! Build an SGR escape "ESC[...m" for the given attributes.
    ! fg/bg = -1 means leave default. Returns ESC[0m when no attrs set.
    function sgr(fg, bg, bold, rev) result(s)
        integer, intent(in) :: fg, bg
        logical, intent(in) :: bold, rev
        character(len=40) :: s
        integer :: codes(8), nc, i, k
        nc = 0
        if (rev) then
            nc = nc + 1; codes(nc) = 7
        end if
        if (bold) then
            nc = nc + 1; codes(nc) = 1
        end if
        if (fg >= 0) then
            nc = nc + 1; codes(nc) = 38
            nc = nc + 1; codes(nc) = 5
            nc = nc + 1; codes(nc) = fg
        end if
        if (bg >= 0) then
            nc = nc + 1; codes(nc) = 48
            nc = nc + 1; codes(nc) = 5
            nc = nc + 1; codes(nc) = bg
        end if
        if (nc == 0) then
            s = achar(27)//'[0m'
            return
        end if
        s = achar(27)//'['
        k = 3
        do i = 1, nc
            if (i > 1) then; k = k + 1; s(k:k) = ';'; end if
            write (s(k+1:k+3), '(I0)') codes(i)
            k = k + 3
        end do
        k = k + 1; s(k:k) = 'm'
        s(k+1:) = ' '
    end function

    function sgr_reset() result(s)
        character(len=4) :: s
        s = achar(27)//'[0m'
    end function

    ! Convenience: return the escape-wrapped string for a colored write.
    function con_s(text, fg, bg, bold, rev) result(out)
        character(len=*), intent(in) :: text
        integer, intent(in) :: fg, bg
        logical, intent(in) :: bold, rev
        character(len=:), allocatable :: out
        out = trim(sgr(fg, bg, bold, rev))//text//sgr_reset()
    end function

    ! Box-drawing glyphs as explicit UTF-8 byte sequences (BMP, < 65536).
    function b_h() result(s)
        character(len=3) :: s
        s = achar(226)//achar(148)//achar(128)
    end function
    function b_v() result(s)
        character(len=3) :: s
        s = achar(226)//achar(148)//achar(130)
    end function
    function b_tl() result(s)
        character(len=3) :: s
        s = achar(226)//achar(148)//achar(140)
    end function
    function b_tr() result(s)
        character(len=3) :: s
        s = achar(226)//achar(148)//achar(144)
    end function
    function b_bl() result(s)
        character(len=3) :: s
        s = achar(226)//achar(148)//achar(148)
    end function
    function b_br() result(s)
        character(len=3) :: s
        s = achar(226)//achar(148)//achar(152)
    end function
    function b_ml() result(s)
        character(len=3) :: s
        s = achar(226)//achar(148)//achar(156)
    end function
    function b_mr() result(s)
        character(len=3) :: s
        s = achar(226)//achar(148)//achar(164)
    end function
    function b_dot() result(s)
        character(len=3) :: s
        s = achar(226)//achar(151)//achar(134)
    end function
    function b_arrow() result(s)
        character(len=3) :: s
        s = achar(226)//achar(157)//achar(175)
    end function

end module aura_theme
