program probe9
    use aura_session
    use aura_config
    use aura_llm
    use aura_ai
    implicit none
    type(aura_cfg) :: cfg
    character(len=:), allocatable :: ans

    print *, 'P9 enter'
    flush (6)
    call cfg%load(); call cfg%save()
    print *, 'P9 cfg done'
    flush (6)
    ans = ai_query('list files', '', '.')
    print *, 'P9 query ok'
end program
