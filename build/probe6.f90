program probe6
    use aura_session
    use aura_config
    implicit none
    type(aura_cfg) :: cfg
    print *, 'P6 enter'
    flush (6)
    call cfg%load()
    call cfg%save()
    print *, 'P6 done'
end program
