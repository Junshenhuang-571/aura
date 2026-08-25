program dbgparse
    implicit none
    character(len=1024) :: line, key, val
    integer :: p1, p2, p3

    line = '  "shell": "C:\\WINDOWS\\system32\\cmd.exe /k",'
    p1 = index(line, '"')
    print *, 'p1=', p1
    p2 = index(line(p1 + 1:), '"') + p1
    print *, 'p2=', p2, ' key=[', line(p1 + 1:p2 - 1), ']'
    p3 = index(line(p2 + 1:), '"') + p2
    print *, 'p3=', p3
    if (p3 > p2) then
        val = line(p2 + 2:p3 - 1)
        print *, 'raw val=[', trim(val), '] len=', len_trim(val)
    end if
end program
