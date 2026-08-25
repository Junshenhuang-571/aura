program bisect7
    use aura_render
use iso_fortran_env, only: int32
    use aura_ansi
    use aura_keys
    implicit none
    type(ansi_parser) :: p

    call p%resize(30_int32, 100_int32)
    print *, 'render_grid...'
    flush (6)
    call render_grid(p)
    print *, 'grid ok'
    flush (6)
    call render_status_line(1, 1, (/'shell'/), .false., 100)
    print *, 'status ok'
end program
