program probe8
    use aura_session
    use aura_config
    use aura_llm
    implicit none
    type(aura_cfg) :: cfg
    character(len=:), allocatable :: ans

    print *, 'P8 enter'
    flush (6)
    call cfg%load(); call cfg%save()
    print *, 'P8 cfg done'
    flush (6)
    ans = ai_ask('list files', '', '.')
    print *, 'P8 ask ok: ', trim(ans)
end program
