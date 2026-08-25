program dbgcfg
    use aura_config
    implicit none
    type(aura_cfg) :: cfg
    character(len=:), allocatable :: p
    logical :: ex
    p = config_dir_path()
    print *, 'dir=[', p, ']'
    call cfg%load()
    print *, 'shell=[', trim(cfg%shell_path), ']'
    ex = .false.
    inquire (file=trim(p)//'/config.json', exist=ex)
    print *, 'exists=', ex
end program
