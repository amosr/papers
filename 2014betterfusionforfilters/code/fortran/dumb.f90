 function dumb(A) result (ou)
     integer, intent(in), dimension(:) :: A
     integer :: ou

     integer :: i1, i2, N, s1, s2
     N  = size(A)
     s1 = 0
     s2 = 0

     do i1 = 1, N
         s1 = s1 + A(i1) * 1111
     enddo
    
     do i2 = 1, N
         s2 = s2 - A(i2) * 2222
     enddo

     ou = s1 * s2
        
 end function dumb

