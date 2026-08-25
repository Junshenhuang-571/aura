program minrepro
    use iso_c_binding
    implicit none
    interface
        function aura_http_post(host, port, path, body, resp, resp_size) bind(C, name='aura_http_post')
            import c_int, c_char
            integer(kind=c_int) :: aura_http_post
            character(kind=c_char), intent(in) :: host(*), path(*), body(*)
            integer(kind=c_int), value :: port, resp_size
            character(kind=c_char) :: resp(*)
        end function
    end interface
    character(kind=c_char) :: h(16), p(24), b(8)
    character(len=200) :: resp
    integer(c_int) :: st
    integer :: i

    do i = 1, 16
        h(i) = c_null_char
    end do
    h(1)='1'; h(2)='2'; h(3)='7'; h(4)='.'; h(5)='0'; h(6)='.'; h(7)='0'; h(8)='.'; h(9)='1'
    p = c_null_char
    p(1:20) = (/ '/','v','1','/','c','h','a','t','/','c','o','m','p','l','e','t','i','o','n','s' /)
    b = c_null_char
    b(1)='{'; b(2)='}'

    print *, 'calling post...'
    flush (6)
    st = aura_http_post(h, 11434_c_int, p, b, resp, int(len(resp), c_int))
    print *, 'status=', st
end program
