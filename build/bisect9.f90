! minimal repro of the --config crash path: exact copy of main's arg block
program bisect9
    use aura_config
    implicit none
    character(len=4096) :: arg1
    type(aura_cfg) :: cfg

    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg1)
        select case (trim(arg1))
        case ('--config')
            call cfg%load(); call cfg%save()
            print *, 'Config written.'
            stop 0
        end select
    end if
end program
