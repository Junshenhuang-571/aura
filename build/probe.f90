! stack-overflow bisect: link main.o pieces progressively.
! Test 1: main.o + ansi + config only (stub the rest via -D? not possible).
! Instead: check if the overflow happens at program startup by adding a
! print BEFORE anything else. We can't modify main without recompiling,
! so recompile a probe copy here:
program probe_main
    use iso_fortran_env, only: i4 => int32
    use iso_c_binding, only: c_long_long
    use aura_ansi
    use aura_session
    use aura_pty
    use aura_keys
    use aura_render
    use aura_ai
    use aura_config
    implicit none

    integer, parameter :: MAX_SESS = 8
    integer(i4), parameter :: GRID_ROWS = 30, GRID_COLS = 100

    type(term_session), allocatable :: sess(:)
    integer(c_long_long), allocatable :: handle(:)
    character(len=32), allocatable :: titles(:)
    integer :: n_sess, active
    logical :: scroll_mode
    integer :: scroll_top
    type(aura_cfg) :: cfg
    integer :: con_cols, con_rows
    integer(i4) :: rc_dummy

    print *, 'PROBE: entered program'
    flush (6)

    n_sess = 0; active = 0; scroll_mode = .false.; scroll_top = 1
    con_cols = 100; con_rows = 30
    rc_dummy = 0
    allocate(sess(MAX_SESS), handle(MAX_SESS), titles(MAX_SESS))
    print *, 'PROBE: allocated'
    flush (6)

    block
        character(len=4096) :: arg1
        if (command_argument_count() >= 1) then
            call get_command_argument(1, arg1)
            select case (trim(arg1))
            case ('--config')
                call cfg%load(); call cfg%save()
                print *, 'Config written.'
                stop 0
            end select
        end if
    end block
end program
