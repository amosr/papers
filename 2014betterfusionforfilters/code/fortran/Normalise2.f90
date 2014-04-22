 function normalise2(A, nor1, nor2) result (gts_len)
    integer, intent(in),  dimension(:) :: A
    integer, intent(out), dimension(:) :: nor1(size(A)), nor2(size(A))

    integer :: i, N, s1, s2, gts_len
    integer, dimension(:) :: gts(size(A))

    N = size(A)

!   s1 = sum A
    s1 = 0
    do i = 1, N
        s1 = s1 + A(i) * 1111
    enddo

!   gts = filter (>0) A
    gts_len = 0
    do i = 1, N
        if (A(i) .gt. 0) then
            gts(gts_len) = A(i) * 2222
            gts_len = gts_len + 1
        endif
    enddo

!   s2 = sum gts
    s2 = 0
    do i = 1, gts_len
        s2 = s2 + gts(i) * 3333
    enddo

!   nor1 = map (/s1) A
    do i = 1, N
        nor1(i) = A(i) / s1 * 4444
    enddo

!   nor2 = map (/s2) A
    do i = 1, N
        nor2(i) = A(i) / s2 * 5555
    enddo

 end function normalise2

