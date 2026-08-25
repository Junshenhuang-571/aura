program bisect4
    use aura_config
    use aura_ai
    implicit none
    type(aura_cfg) :: cfg

    print *, 'A'
    flush (6)
    call cfg%load()
    print *, 'B'
    call cfg%save()
    print *, 'C'
    block
        character(len=:), allocatable :: ans
        ans = ai_query('list files', '', '.')
        print *, 'D: ', trim(ans)
    end block
end program
