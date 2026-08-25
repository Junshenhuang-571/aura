program probe7
    use aura_session
    use aura_config
    implicit none
    type(aura_cfg) :: cfg
    print *, 'P7 enter'
    flush (6)
    call cfg%load(); call cfg%save()
    print *, 'P7 done'
end program
