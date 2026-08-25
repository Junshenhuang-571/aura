program dbgenv
    implicit none
    character(len=512) :: env
    integer :: ios
    call get_environment_variable('OS', env, status=ios)
    print *, 'OS ios=', ios, ' val=[', trim(env), ']'
    call get_environment_variable('COMSPEC', env, status=ios)
    print *, 'COMSPEC ios=', ios, ' val=[', trim(env), ']'
end program
