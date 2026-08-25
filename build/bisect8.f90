program bisect8
    use aura_config
    implicit none
    type(aura_cfg) :: cfg
    print *, 'A'
    flush(6)
    call cfg%load()
    print *, 'B'
    call cfg%save()
    print *, 'C'
end program
