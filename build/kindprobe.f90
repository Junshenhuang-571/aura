program kp
    use iso_c_binding
    print *, 'int kinds:', kind(c_int), selected_char_kind('c'), selected_char_kind('iso_10646')
end program
