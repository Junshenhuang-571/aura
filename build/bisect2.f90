program bisect2
    use aura_ai
    implicit none
    character(len=:), allocatable :: ans

    print *, 'calling ai_query...'
    flush (6)
    ans = ai_query('check disk space', 'C:\> dir', '.')
    print *, 'ai_query ok:'
    print *, trim(ans)
end program
