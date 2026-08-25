program dbgcfg4
    use aura_config
    implicit none
    type(aura_cfg) :: cfg

    call cfg%load(path='C:\Users\junsh\.config\aura\config.json')
    print *, 'B load(explicit): shell=[', trim(cfg%shell_path), '] len=', len_trim(cfg%shell_path)
end program
