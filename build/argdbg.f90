program argdbg
    character(len=256) :: a
    integer :: n
    n = command_argument_count()
    print *, 'nargs=', n
    if (n >= 1) then
        call get_command_argument(1, a)
        print *, 'arg1=[', trim(a), '] len=', len_trim(a)
        print *, 'cmp--config=', trim(a) == '--config'
    end if
end program
