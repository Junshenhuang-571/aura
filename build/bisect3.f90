program bisect3
    use aura_config
    use aura_ai
    implicit none
    character(len=4096) :: arg1
    type(aura_cfg) :: cfg
    integer :: n

    n = command_argument_count()
    print *, 'nargs=', n
    if (n >= 1) then
        call get_command_argument(1, arg1)
        select case (trim(arg1))
        case ('--config')
            print *, 'branch config entered'
            flush (6)
            call cfg%load()
            print *, 'loaded'
            call cfg%save()
            print *, 'saved'
        case default
            print *, 'other branch:', trim(arg1)
        end select
    end if
end program
