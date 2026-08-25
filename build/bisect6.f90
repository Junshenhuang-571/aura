program bisect6
    use aura_keys
    implicit none
    type(key_event) :: ev
    logical :: got

    print *, 'calling keys_raw_enter'
    flush (6)
    call keys_raw_enter()
    print *, 'raw enter ok'
    got = poll_key(10, ev)
    print *, 'poll ok, got=', got
    call keys_raw_exit()
end program
