! llama2_mod.f90 — refactored from rbitr/llm.f90 `program llama2` into a
! callable module so Aura can run inference in-process (no subprocess, model
! loaded once). Pure Fortran, zero external deps. Targets stories15M.bin
! (llama2.c ak format) + tokenizer.bin.
!
! Public: generate_text(prompt, maxtok, outstr)
module llama2_mod
    use iso_c_binding
    use precision_module
    use weight_module          ! also provides precision_module (wp)
    implicit none
    private
    public :: generate_text, set_native_paths

    ! --- model params for stories15M.bin (llama2.c ak export) ---
    integer, parameter :: emb_dim = 288
    integer, parameter :: hidden_dim = 768
    integer, parameter :: n_layers = 6
    integer, parameter :: n_heads = 6
    integer, parameter :: n_kv_heads = 6
    integer, parameter :: kv_head_size = n_kv_heads * (emb_dim / n_heads)
    integer, parameter :: vocab_size = 32000
    integer :: seq_len = 256

    ! configurable model paths (set via set_native_paths; defaults for stories15M)
    character(len=512), save :: native_model = 'models/stories15M.bin'
    character(len=512), save :: native_tok   = 'models/tokenizer.bin'

    ! loaded-once state
    type(TransformerWeights), save :: weights
    character(:), allocatable, save :: vocab(:)
    real(kind=wp), allocatable, save :: scores(:)
    integer, allocatable, save :: vocab_len(:)
    integer, save :: max_len = 0
    logical, save :: loaded = .false.

contains

    subroutine set_native_paths(m, t)
        character(len=*), intent(in) :: m, t
        native_model = m
        native_tok = t
    end subroutine

    subroutine generate_text(prompt, maxtok, outstr)
        character(len=*), intent(in) :: prompt
        integer, intent(in) :: maxtok
        character(len=:), allocatable, intent(out) :: outstr

        type(RunState) :: s
        real(kind=wp), allocatable :: logits(:)
        integer :: pos, token, tok_len, cur
        integer, allocatable :: prompt_tokens(:)
        real(kind=wp) :: temperature
        character(:), allocatable :: p
        integer :: kv_head_size, head_size

        if (.not. loaded) then
            call load_ak(trim(native_model), trim(native_tok))
            if (.not. loaded) then
                outstr = ''
                return
            end if
        end if

        temperature = 1.0_wp
        head_size = emb_dim / n_heads
        kv_head_size = n_kv_heads * head_size

        if (maxtok <= seq_len) seq_len = maxtok

        ! state dict (matches upstream allocation)
        allocate(s%att(seq_len, n_heads))
        allocate(s%key_cache(kv_head_size, seq_len, n_layers))
        allocate(s%value_cache(kv_head_size, seq_len, n_layers))
        s%att(:,:) = 0; s%key_cache(:,:,:) = 0; s%value_cache(:,:,:) = 0; s%times = 0

        p = prompt
        prompt_tokens = bpe_encode(p)

        token = 2   ! <s> BOS
        cur = 1
        allocate(character(len=maxtok*max_len) :: outstr)
        do pos = 1, seq_len
            logits = transformer(token, pos, s, weights)
            if (pos <= size(prompt_tokens)) then
                token = prompt_tokens(pos)
            else
                if (temperature == 0) then
                    token = maxloc(logits, DIM=1)
                else
                    logits = softmax(logits/temperature, vocab_size)
                    token = sample(logits)
                end if
            end if
            tok_len = vocab_len(token)
            outstr(cur:cur+tok_len-1) = vocab(token)(1:tok_len)
            cur = cur + tok_len
        end do
        outstr = outstr(1:cur-1)
    end subroutine generate_text

    ! Load the ak-format weights (stories15M.bin) + tokenizer.bin, once.
    ! Layout replicated from karpathy/llama2.c legacy export (shared classifier):
    !   token_emb(d,vocab) rms_att(d,L) wqkv(d,3d,L) wo(d,d,L)
    !   rms_ffn(d,L) w13(d,2h,L) w2(h,d,L) rms_final(d)   [wcls = token_emb]
    ! NOTE: read whole arrays contiguously; strided subarray reads misalign STREAM.
    subroutine load_ak(model_file, tok_file)
        character(len=*), intent(in) :: model_file, tok_file
        integer :: dummy(7), n, score, tok_len, l
        character(:), allocatable :: tmpstr
        integer :: kv_head_size, head_size

        head_size = emb_dim / n_heads
        kv_head_size = n_kv_heads * head_size

        open(UNIT=5, FILE=model_file, FORM='UNFORMATTED', &
             ACCESS='STREAM', STATUS='OLD', POSITION='REWIND', ACTION='READ')
        read(5) dummy   ! config header (ignored; params are module constants)

        allocate(weights%token_embedding_table(emb_dim, vocab_size)); read(5) weights%token_embedding_table
        allocate(weights%rms_att_weight(emb_dim, n_layers));           read(5) weights%rms_att_weight

        allocate(weights%wqkv(emb_dim, 3*emb_dim, n_layers));          read(5) weights%wqkv
        allocate(weights%wo(emb_dim, emb_dim, n_layers));             read(5) weights%wo
        allocate(weights%rms_ffn_weight(emb_dim, n_layers));          read(5) weights%rms_ffn_weight
        allocate(weights%w13(emb_dim, 2*hidden_dim, n_layers));       read(5) weights%w13
        allocate(weights%w2(hidden_dim, emb_dim, n_layers));          read(5) weights%w2
        allocate(weights%rms_final_weight(emb_dim));                  read(5) weights%rms_final_weight

        ! shared classifier: reuse token embedding (no separate wcls tensor)
        allocate(weights%wcls(emb_dim, vocab_size))
        weights%wcls = weights%token_embedding_table
        close(5)

        ! tokenizer.bin
        open(UNIT=5, FILE=tok_file, FORM='UNFORMATTED', &
             ACCESS='STREAM', STATUS='OLD', POSITION='REWIND', ACTION='READ')
        read(5) max_len
        allocate(character(len=max_len) :: vocab(vocab_size))
        allocate(scores(vocab_size)); allocate(vocab_len(vocab_size))
        do n = 1, vocab_size
            read(5) score
            read(5) tok_len
            allocate(character(tok_len) :: tmpstr)
            read(5) tmpstr
            vocab(n) = tmpstr
            scores(n) = real(score, kind=wp)
            vocab_len(n) = tok_len
            deallocate(tmpstr)
        end do
        close(5)

        loaded = .true.
    end subroutine load_ak

    ! functions (lifted verbatim from upstream program llama2)
    function time_ms() result(t_ms)
        real(kind=wp) :: t_ms
        integer(4) :: ms
        call system_clock(ms)
        t_ms = real(ms)
    end function

    function sample(p) result(i)
        real(kind=wp) :: p(:)
        integer :: i
        real(kind=wp) :: r, cdf
        call random_number(r)
        cdf = 0
        do i = 1, size(p)
            cdf = cdf + p(i)
            if (r < cdf) return
        end do
        i = size(p)
    end function

    function rmsnorm(x, w) result(xr)
        real(kind=wp) :: x(:), w(:)
        real(kind=wp) :: xr(size(x))
        real(kind=wp) :: xn
        xn = sqrt(dot_product(x, x)/size(x) + 1e-5_wp)
        xr = x*w/xn
    end function

    pure function softmax(x, s) result(p)
        real(kind=wp), intent(in) :: x(:)
        integer, intent(in) :: s
        real(kind=wp) :: p(size(x))
        real(kind=wp) :: xi(s)
        p(:) = 0
        xi = exp(x(:s) - maxval(x(:s)))
        p(:s) = xi/sum(xi)
    end function

    function transformer(token, pos, s, w) result(logits)
        integer, intent(in) :: token, pos
        type(RunState) :: s
        type(TransformerWeights), intent(in) :: w
        real(kind=wp) :: logits(vocab_size)
        real(kind=wp) :: x(emb_dim)
        real(kind=wp) :: xb(emb_dim)
        real(kind=wp), target :: qkv(emb_dim + 2*kv_head_size)
        real(kind=wp), pointer :: q(:), k(:), v(:)
        real(kind=wp) :: q0, q1, k0, k1, fcr, fci, v0, v1, freq, rval
        integer :: head_dim
        real(kind=wp) :: q_t(emb_dim/n_heads)
        real(kind=wp) :: k_t(emb_dim/n_heads)
        real(kind=wp) :: v_t(emb_dim/n_heads)
        real(kind=wp) :: xbh(emb_dim/n_heads)
        real(kind=wp) :: a
        integer :: kv_mul
        real(kind=wp), target :: hb13(2*hidden_dim)
        real(kind=wp), pointer :: hb(:), hb2(:)
        integer :: l, i, h, t, head_size, ix
        real(kind=wp) :: time
        head_size = emb_dim/n_heads
        logits(:) = 0
        x = w%token_embedding_table(:, token)
        do l = 1, n_layers
            time = time_ms()
            xb = rmsnorm(x, w%rms_att_weight(:, l))
            do ix = 1, size(qkv)
                qkv(ix) = dot_product(xb, (w%wqkv(:, ix, l)))
            end do
            q => qkv(1:emb_dim)
            k => qkv((emb_dim+1):(emb_dim+kv_head_size))
            v => qkv((emb_dim+kv_head_size+1):(emb_dim+2*kv_head_size))
            s%times(1) = s%times(1) + (time_ms() - time)
            time = time_ms()
            do i = 1, emb_dim, 2
                head_dim = mod(i, head_size)
                freq = 1.0_wp / (10000.0_wp ** (real(head_dim, kind=wp) / head_size))
                rval = pos * freq
                fcr = cos(rval); fci = sin(rval)
                q0 = q(i); q1 = q(i+1)
                q(i) = q0*fcr - q1*fci
                q(i+1) = q0*fci + q1*fcr
                if (i < kv_head_size) then
                    k0 = k(i); k1 = k(i+1)
                    k(i) = k0*fcr - k1*fci
                    k(i+1) = k0*fci + k1*fcr
                end if
            end do
            s%times(2) = s%times(2) + (time_ms() - time)
            s%key_cache(:, pos, l) = k
            s%value_cache(:, pos, l) = v
            xb(:) = 0
            time = time_ms()
            kv_mul = n_heads / n_kv_heads
            do h = 0, (n_heads-1)
                q_t = q((h*head_size+1):((h+1)*head_size))
                do t = 1, (pos)
                    k_t = s%key_cache(((h/kv_mul)*head_size+1):(((h+1)/kv_mul)*head_size), t, l)
                    s%att(t, h+1) = dot_product(q_t, k_t)/sqrt(real(head_size, wp))
                end do
                s%att(:, h+1) = softmax(s%att(:, h+1), pos)
                xbh(:) = 0
                do t = 1, (pos)
                    v_t = s%value_cache(((h/kv_mul)*head_size+1):(((h+1)/kv_mul)*head_size), t, l)
                    a = s%att(t, h+1)
                    xbh = xbh + a*v_t
                end do
                xb((h*head_size+1):((h+1)*head_size)) = xbh
            end do
            s%times(3) = s%times(3) + (time_ms() - time)
            time = time_ms()
            do ix = 1, emb_dim
                x(ix) = x(ix) + dot_product(xb, w%wo(:, ix, l))
            end do
            xb = rmsnorm(x, w%rms_ffn_weight(:, l))
            do ix = 1, size(hb13)
                hb13(ix) = dot_product(xb, w%w13(:, ix, l))
            end do
            hb => hb13(1:hidden_dim)
            hb2 => hb13((hidden_dim+1):(2*hidden_dim))
            hb = hb*(1/(1+exp(-hb)))
            hb = hb*hb2
            do ix = 1, emb_dim
                x(ix) = x(ix) + dot_product(hb, w%w2(:, ix, l))
            end do
            s%times(4) = s%times(4) + (time_ms() - time)
        end do
        time = time_ms()
        x = rmsnorm(x, w%rms_final_weight)
        do ix = 1, vocab_size
            logits(ix) = dot_product(x, w%wcls(:, ix))
        end do
        s%times(5) = s%times(5) + (time_ms() - time)
    end function

    function lookup(s, l) result(ind)
        character(len=*) :: s
        integer :: l
        integer :: i, ind
        do i = 1, size(vocab)
            if (vocab(i) == s .and. vocab_len(i) == l) then
                ind = i
                return
            end if
        end do
        ind = -1
    end function

    function bpe_encode(text) result(tokens)
        character(len=*) :: text
        integer, allocatable :: tokens(:)
        integer, allocatable :: tmp_tokens(:)
        integer :: i, ind, best_id, t1, t2
        real(kind=wp) :: score, best_score
        character(:), dimension(:), allocatable :: running_merge
        integer, allocatable :: running_merge_len(:)
        allocate(tokens(len(text)))
        do i = 1, len(text)
            tokens(i) = lookup(text(i:i), 1)
        end do
        do while(1==1)
            allocate(character(len=2*max_len) :: running_merge(size(tokens)-1))
            allocate(running_merge_len(size(tokens)-1))
            do i = 1, (size(tokens)-1)
                t1 = vocab_len(tokens(i))
                t2 = vocab_len(tokens(i+1))
                running_merge(i) = vocab(tokens(i))(1:t1)//vocab(tokens(i+1))(1:t2)
                running_merge_len(i) = t1 + t2
            end do
            best_id = -1
            best_score = -1e10_wp
            do i = 1, (size(tokens)-1)
                ind = lookup(running_merge(i), running_merge_len(i))
                if (ind > 0) then
                    score = scores(ind)
                    if (score > best_score) then
                        best_score = score
                        best_id = i
                    end if
                end if
            end do
            if (best_id == -1) exit
            allocate(tmp_tokens(size(tokens)-1))
            tmp_tokens(1:(best_id-1)) = tokens(1:(best_id-1))
            tmp_tokens(best_id) = lookup(running_merge(best_id), running_merge_len(best_id))
            tmp_tokens((best_id+1):) = tokens((best_id+2):)
            deallocate(tokens)
            call move_alloc(tmp_tokens, tokens)
            deallocate(running_merge)
            deallocate(running_merge_len)
        end do
    end function

end module llama2_mod
