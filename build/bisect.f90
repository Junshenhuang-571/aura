program bisect
    use aura_config
    use aura_llm
    implicit none
    type(aura_cfg) :: cfg
    character(len=:), allocatable :: ans

    print *, 'step1: config load'
    call cfg%load()
    print *, 'step2: ok, shell=', trim(cfg%shell_path)
    flush (6)

    ans = ai_ask('check disk space', '', '.')
    print *, 'step3: ai_ask ok'
    print *, trim(ans)
end program
