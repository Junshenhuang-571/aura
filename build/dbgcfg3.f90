program dbgcfg3
    use aura_config
    implicit none
    type(aura_cfg) :: cfg
    character(len=:), allocatable :: p
    integer :: u, ios
    character(len=256) :: line

    ! simulate what load() does, step by step
    p = trim(config_dir_path()) // '/config.json'
    print *, 'fname=[', trim(p), ']'
    open (newunit=u, file=trim(p), status='old', action='read', iostat=ios)
    print *, 'open ios=', ios
    if (ios == 0) then
        do
            read (u, '(A)', iostat=ios) line
            if (ios /= 0) exit
            print *, 'LINE: [', trim(line), ']'
        end do
        close (u)
    end if
end program
