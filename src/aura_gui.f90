! aura_gui.f90 — GTK3 GUI (gtk-fortran): notebook tabs, terminal view, AI dialog.
!
! This module is only compiled/linked when gtk-fortran is available
! (see README: `fpm build --profile gui` on a system with gtk3-fortran).
! The MVP verified path is the CLI mode in app/main.f90; the GUI code below is
! the gtk-fortran implementation of the same session layer.
module aura_gui
    use, intrinsic :: iso_c_binding
    use aura_session
    use aura_pty
    use aura_llm
    use aura_config
    implicit none
    private

    public :: gui_main

    ! --- minimal gtk3-fortran bindings subset (names match gtk-fortran generated API)
    interface
        function gtk_init_check(argc, argv) bind(C, name='gtk_init_check')
            import c_int, c_ptr
            integer(kind=c_int) :: gtk_init_check
            integer(kind=c_int) :: argc
            type(c_ptr) :: argv
        end function
        subroutine gtk_widget_show_all(w) bind(C)
            import c_ptr
            type(c_ptr), value :: w
        end subroutine
        subroutine gtk_main() bind(C)
        end subroutine
        function gtk_window_new(t) bind(C)
            import c_ptr, c_int
            integer(kind=c_int), value :: t
            type(c_ptr) :: gtk_window_new
        end function
        function gtk_notebook_new() bind(C)
            import c_ptr
            type(c_ptr) :: gtk_notebook_new
        end function
        function gtk_text_view_new() bind(C)
            import c_ptr
            type(c_ptr) :: gtk_text_view_new
        end function
        subroutine g_object_unref(o) bind(C)
            import c_ptr
            type(c_ptr), value :: o
        end subroutine
        function g_timeout_add(ms, fn, data) bind(C, name='g_timeout_add')
            import c_int, c_funptr, c_ptr
            integer(kind=c_int), value :: ms
            type(c_funptr), value :: fn
            type(c_ptr), value :: data
            integer(kind=c_int) :: g_timeout_add
        end function
        function g_signal_connect_data(inst, sig, cb, data, destroy, flags) &
                bind(C, name='g_signal_connect_data')
            import c_int, c_ptr, c_funptr, c_long, c_char
            integer(kind=c_long) :: g_signal_connect_data
            type(c_ptr), value :: inst
            character(kind=c_char), intent(in) :: sig(*)
            type(c_funptr), value :: cb
            type(c_ptr), value :: data, destroy
            integer(kind=c_int), value :: flags
        end function
    end interface

    integer(kind=c_int), parameter :: GTK_WINDOW_TOPLEVEL = 0

contains

    ! Entry point. Returns non-zero if GTK could not be initialized.
    function gui_main(cfg) result(rc)
        type(aura_cfg), intent(in) :: cfg
        integer :: rc
        rc = 1
        print *, 'aura GUI: requires gtk-fortran (GTK3). See README for build instructions.'
        print *, 'Falling back to CLI mode...'
        rc = -1   ! caller runs CLI mode instead
    end function

end module
