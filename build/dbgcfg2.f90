program dbgcfg2
    use aura_config
    implicit none
    type(aura_cfg) :: cfg
    call cfg%defaults()
    print *, 'after defaults: shell=[', trim(cfg%shell_path), '] len=', len_trim(cfg%shell_path)
end program
