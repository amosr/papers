# mark_description "Intel(R) Fortran Intel(R) 64 Compiler XE for applications running on Intel(R) 64, Version 14.0.2.139 Build 2";
# mark_description "0140121";
# mark_description "-O3 -c -save-temps -fsource-asm -FaNormalise2.s";
	.file "Normalise2.f90"
	.section	__TEXT, __text
L_TXTST0:
# -- Begin  _normalise2_
# mark_begin;
       .align    4
	.globl _normalise2_
_normalise2_:
# parameter 1: %rdi
# parameter 2: %rsi
# parameter 3: %rdx
L_B1.1:                         # Preds L_B1.0

###  function normalise2(A, nor1, nor2) result (gts_len)

L____tag_value__normalise2_.1:                                  #1.11
        pushq     %rbp                                          #1.11
L____tag_value__normalise2_.3:                                  #
        movq      %rsp, %rbp                                    #1.11
L____tag_value__normalise2_.4:                                  #
        subq      $48, %rsp                                     #1.11
        xorl      %eax, %eax                                    #1.11
        movq      %rbx, -40(%rbp)                               #1.11
        movq      %rsi, %r8                                     #1.11
L____tag_value__normalise2_.6:                                  #
        movq      48(%rdi), %rbx                                #1.11
        testq     %rbx, %rbx                                    #1.11
        movq      %r15, -32(%rbp)                               #1.11
        movq      %rdx, %r9                                     #1.11
L____tag_value__normalise2_.7:                                  #
        cmovle    %rax, %rbx                                    #1.11
        movslq    %ebx, %rsi                                    #1.11
        testq     %rsi, %rsi                                    #1.11
        movq      %r14, -24(%rbp)                               #1.11
        cmovg     %rsi, %rax                                    #1.11
        shlq      $2, %rax                                      #1.11
        movq      %r13, -16(%rbp)                               #1.11
        movq      %r12, -8(%rbp)                                #1.11
        addq      $15, %rax                                     #1.11
        andq      $-16, %rax                                    #1.11
        subq      %rax, %rsp                                    #1.11
        movq      %rsp, %rax                                    #1.11
L____tag_value__normalise2_.8:                                  #
                                # LOE rax rsi rdi r8 r9 ebx
L_B1.65:                        # Preds L_B1.1

###     integer, intent(in),  dimension(:) :: A
###     integer, intent(out), dimension(:) :: nor1(size(A)), nor2(size(A))
### 
###     integer :: i, N, s1, s2, gts_len
###     integer, dimension(:) :: gts(size(A))
### 
###     N = size(A)
### 
### !   s1 = sum A
###     s1 = 0

        xorl      %ecx, %ecx                                    #11.5

###     do i = 1, N

        testl     %ebx, %ebx                                    #12.5
        jle       L_B1.25       # Prob 0%                       #12.5
                                # LOE rax rsi rdi r8 r9 ecx ebx
L_B1.2:                         # Preds L_B1.65

###         s1 = s1 + A(i) * 1111

        movq      56(%rdi), %r10                                #13.19
        cmpq      $4, %r10                                      #12.5
        movq      (%rdi), %rdx                                  #13.19
        jne       L_B1.19       # Prob 50%                      #12.5
                                # LOE rax rdx rsi rdi r8 r9 r10 ecx ebx
L_B1.3:                         # Preds L_B1.2
        cmpq      $4, %rsi                                      #12.5
        jl        L_B1.59       # Prob 10%                      #12.5
                                # LOE rax rdx rsi rdi r8 r9 ecx ebx
L_B1.4:                         # Preds L_B1.3
        movq      %rdx, %r11                                    #12.5
        andq      $15, %r11                                     #12.5
        testl     %r11d, %r11d                                  #12.5
        je        L_B1.7        # Prob 50%                      #12.5
                                # LOE rax rdx rsi rdi r8 r9 ecx ebx r11d
L_B1.5:                         # Preds L_B1.4
        testl     $3, %r11d                                     #12.5
        jne       L_B1.59       # Prob 10%                      #12.5
                                # LOE rax rdx rsi rdi r8 r9 ecx ebx r11d
L_B1.6:                         # Preds L_B1.5
        negl      %r11d                                         #12.5
        addl      $16, %r11d                                    #12.5
        shrl      $2, %r11d                                     #12.5
                                # LOE rax rdx rsi rdi r8 r9 ecx ebx r11d
L_B1.7:                         # Preds L_B1.6 L_B1.4
        movl      %r11d, %r10d                                  #12.5
        lea       4(%r10), %r12                                 #12.5
        cmpq      %r12, %rsi                                    #12.5
        jl        L_B1.59       # Prob 10%                      #12.5
                                # LOE rax rdx rsi rdi r8 r9 r10 ecx ebx r11d
L_B1.8:                         # Preds L_B1.7
        movl      %esi, %r13d                                   #12.5
        negl      %r11d                                         #12.5
        addl      %r13d, %r11d                                  #12.5
        xorl      %r12d, %r12d                                  #12.5
        andl      $3, %r11d                                     #12.5
        subl      %r11d, %r13d                                  #12.5
        movslq    %r13d, %r11                                   #12.5
        testq     %r10, %r10                                    #12.5
        jbe       L_B1.12       # Prob 3%                       #12.5
                                # LOE rax rdx rsi rdi r8 r9 r10 r11 r12 ecx ebx
L_B1.10:                        # Preds L_B1.8 L_B1.10
        imull     $1111, (%rdx,%r12,4), %r14d                   #13.24
        incq      %r12                                          #12.5
        addl      %r14d, %ecx                                   #13.9
        cmpq      %r10, %r12                                    #12.5
        jb        L_B1.10       # Prob 82%                      #12.5
                                # LOE rax rdx rsi rdi r8 r9 r10 r11 r12 ecx ebx
L_B1.12:                        # Preds L_B1.10 L_B1.8
        movdqa    L_2il0floatpacket.5(%rip), %xmm0              #13.24
        movd      %ecx, %xmm2                                   #11.5
        movdqa    L_2il0floatpacket.4(%rip), %xmm1              #13.24
        psrlq     $32, %xmm0                                    #13.24
                                # LOE rax rdx rsi rdi r8 r9 r10 r11 ebx xmm0 xmm1 xmm2
L_B1.13:                        # Preds L_B1.13 L_B1.12
        movdqa    (%rdx,%r10,4), %xmm3                          #13.19
        addq      $4, %r10                                      #12.5
        movdqa    L_2il0floatpacket.5(%rip), %xmm4              #13.24
        cmpq      %r11, %r10                                    #12.5
        pmuludq   %xmm3, %xmm4                                  #13.24
        psrlq     $32, %xmm3                                    #13.24
        pmuludq   %xmm0, %xmm3                                  #13.24
        pand      %xmm1, %xmm4                                  #13.24
        psllq     $32, %xmm3                                    #13.24
        por       %xmm3, %xmm4                                  #13.24
        paddd     %xmm4, %xmm2                                  #13.9
        jb        L_B1.13       # Prob 82%                      #12.5
                                # LOE rax rdx rsi rdi r8 r9 r10 r11 ebx xmm0 xmm1 xmm2
L_B1.14:                        # Preds L_B1.13
        movdqa    %xmm2, %xmm0                                  #11.5
        psrldq    $8, %xmm0                                     #11.5
        paddd     %xmm0, %xmm2                                  #11.5
        movdqa    %xmm2, %xmm1                                  #11.5
        psrlq     $32, %xmm1                                    #11.5
        paddd     %xmm1, %xmm2                                  #11.5
        movd      %xmm2, %ecx                                   #11.5
                                # LOE rax rdx rsi rdi r8 r9 r11 ecx ebx
L_B1.15:                        # Preds L_B1.14 L_B1.59
        cmpq      %rsi, %r11                                    #12.5
        jae       L_B1.18       # Prob 3%                       #12.5
                                # LOE rax rdx rsi rdi r8 r9 r11 ecx ebx
L_B1.17:                        # Preds L_B1.15 L_B1.17
        imull     $1111, (%rdx,%r11,4), %r10d                   #13.24
        incq      %r11                                          #12.5
        addl      %r10d, %ecx                                   #13.9
        cmpq      %rsi, %r11                                    #12.5
        jb        L_B1.17       # Prob 82%                      #12.5
                                # LOE rax rdx rsi rdi r8 r9 r11 ecx ebx
L_B1.18:                        # Preds L_B1.15 L_B1.23 L_B1.17
        testl     %ebx, %ebx                                    #12.5
        jmp       L_B1.25       # Prob 100%                     #12.5
                                # LOE rax rsi rdi r8 r9 ecx ebx
L_B1.19:                        # Preds L_B1.2
        movl      %ebx, %r12d                                   #12.5
        xorl      %r11d, %r11d                                  #12.5
        shrl      $31, %r12d                                    #12.5
        movl      $1, %r14d                                     #12.5
        addl      %ebx, %r12d                                   #12.5
        xorl      %r13d, %r13d                                  #
        sarl      $1, %r12d                                     #12.5
        movslq    %r12d, %r12                                   #12.5
        testq     %r12, %r12                                    #12.5
        jbe       L_B1.23       # Prob 3%                       #12.5
                                # LOE rax rdx rsi rdi r8 r9 r10 r11 r12 r13 ecx ebx r14d
L_B1.20:                        # Preds L_B1.19
        movq      %rdx, %r14                                    #
        xorl      %r15d, %r15d                                  #12.5
        subq      %r10, %r14                                    #
        movq      %r9, -48(%rbp)                                #
        lea       (%r14,%r10,2), %r14                           #
                                # LOE rax rdx rsi rdi r8 r10 r11 r12 r13 r14 ecx ebx r15d
L_B1.21:                        # Preds L_B1.21 L_B1.20
        imull     $1111, (%rdx,%r13,2), %r9d                    #13.24
        incq      %r11                                          #12.5
        addl      %r9d, %ecx                                    #13.9
        imull     $1111, (%r14,%r13,2), %r9d                    #13.24
        addq      %r10, %r13                                    #12.5
        addl      %r9d, %r15d                                   #13.9
        cmpq      %r12, %r11                                    #12.5
        jb        L_B1.21       # Prob 63%                      #12.5
                                # LOE rax rdx rsi rdi r8 r10 r11 r12 r13 r14 ecx ebx r15d
L_B1.22:                        # Preds L_B1.21
        movq      -48(%rbp), %r9                                #
        addl      %r15d, %ecx                                   #12.5
        lea       1(%r11,%r11), %r14d                           #13.9
                                # LOE rax rdx rsi rdi r8 r9 r10 ecx ebx r14d
L_B1.23:                        # Preds L_B1.22 L_B1.19
        lea       -1(%r14), %r11d                               #12.5
        cmpl      %r11d, %ebx                                   #12.5
        jbe       L_B1.18       # Prob 3%                       #12.5
                                # LOE rax rdx rsi rdi r8 r9 r10 ecx ebx r14d
L_B1.24:                        # Preds L_B1.23
        movslq    %r14d, %r14                                   #13.19
        subq      %r10, %rdx                                    #13.9
        imulq     %r14, %r10                                    #13.19
        imull     $1111, (%r10,%rdx), %edx                      #13.24
        addl      %edx, %ecx                                    #13.9
        testl     %ebx, %ebx                                    #13.9
                                # LOE rax rsi rdi r8 r9 ecx ebx
L_B1.25:                        # Preds L_B1.18 L_B1.65 L_B1.24

###     enddo
### 
### !   gts = filter (>0) A
###     gts_len = 0

        movl      $0, %edx                                      #17.5

###     do i = 1, N

        jle       L_B1.37       # Prob 50%                      #18.5
                                # LOE rax rdx rsi rdi r8 r9 ecx ebx
L_B1.26:                        # Preds L_B1.25
        movl      %ebx, %r12d                                   #18.5
        xorl      %r11d, %r11d                                  #18.5
        shrl      $31, %r12d                                    #18.5
        movl      $1, %r15d                                     #18.5
        addl      %ebx, %r12d                                   #18.5
        xorl      %r14d, %r14d                                  #
        sarl      $1, %r12d                                     #18.5
        movslq    %r12d, %r12                                   #18.5

###         if (A(i) .gt. 0) then

        movq      (%rdi), %r13                                  #19.13
        testq     %r12, %r12                                    #18.5
        movq      56(%rdi), %r10                                #19.13
        jbe       L_B1.34       # Prob 3%                       #18.5
                                # LOE rax rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 ecx ebx r15d
L_B1.27:                        # Preds L_B1.26
        movq      %r13, %r15                                    #
        subq      %r10, %r15                                    #
        movq      %r9, -48(%rbp)                                #
        lea       (%r15,%r10,2), %r15                           #
                                # LOE rax rdx rsi rdi r8 r10 r11 r12 r13 r14 r15 ecx ebx
L_B1.28:                        # Preds L_B1.32 L_B1.27
        movl      (%r13,%r14,2), %r9d                           #19.13
        testl     %r9d, %r9d                                    #19.18
        jle       L_B1.30       # Prob 16%                      #19.18
                                # LOE rax rdx rsi rdi r8 r10 r11 r12 r13 r14 r15 ecx ebx r9d
L_B1.29:                        # Preds L_B1.28

###             gts(gts_len) = A(i) * 2222

        imull     $2222, %r9d, %r9d                             #20.13
        movl      %r9d, -4(%rax,%rdx,4)                         #20.13

###             gts_len = gts_len + 1

        incq      %rdx                                          #21.13
                                # LOE rax rdx rsi rdi r8 r10 r11 r12 r13 r14 r15 ecx ebx
L_B1.30:                        # Preds L_B1.29 L_B1.28
        movl      (%r15,%r14,2), %r9d                           #19.13
        testl     %r9d, %r9d                                    #19.18
        jle       L_B1.32       # Prob 16%                      #19.18
                                # LOE rax rdx rsi rdi r8 r10 r11 r12 r13 r14 r15 ecx ebx r9d
L_B1.31:                        # Preds L_B1.30
        imull     $2222, %r9d, %r9d                             #20.13
        movl      %r9d, -4(%rax,%rdx,4)                         #20.13
        incq      %rdx                                          #21.13
                                # LOE rax rdx rsi rdi r8 r10 r11 r12 r13 r14 r15 ecx ebx
L_B1.32:                        # Preds L_B1.31 L_B1.30
        incq      %r11                                          #18.5
        addq      %r10, %r14                                    #18.5
        cmpq      %r12, %r11                                    #18.5
        jb        L_B1.28       # Prob 63%                      #18.5
                                # LOE rax rdx rsi rdi r8 r10 r11 r12 r13 r14 r15 ecx ebx
L_B1.33:                        # Preds L_B1.32
        movq      -48(%rbp), %r9                                #
        lea       1(%r11,%r11), %r15d                           #19.18
                                # LOE rax rdx rsi rdi r8 r9 r10 r13 ecx ebx r15d
L_B1.34:                        # Preds L_B1.33 L_B1.26
        lea       -1(%r15), %r11d                               #18.5
        cmpl      %r11d, %ebx                                   #18.5
        jbe       L_B1.37       # Prob 3%                       #18.5
                                # LOE rax rdx rsi rdi r8 r9 r10 r13 ecx ebx r15d
L_B1.35:                        # Preds L_B1.34
        movslq    %r15d, %r15                                   #19.13
        subq      %r10, %r13                                    #19.18
        imulq     %r15, %r10                                    #19.13
        movl      (%r10,%r13), %r10d                            #19.13
        testl     %r10d, %r10d                                  #19.18
        jle       L_B1.37       # Prob 16%                      #19.18
                                # LOE rax rdx rsi rdi r8 r9 ecx ebx r10d
L_B1.36:                        # Preds L_B1.35
        imull     $2222, %r10d, %r10d                           #20.13
        movl      %r10d, -4(%rax,%rdx,4)                        #20.13
        incq      %rdx                                          #21.13
                                # LOE rax rdx rsi rdi r8 r9 ecx ebx
L_B1.37:                        # Preds L_B1.36 L_B1.35 L_B1.34 L_B1.25

###         endif
###     enddo
### 
### !   s2 = sum gts
###     s2 = 0

        xorl      %r11d, %r11d                                  #26.5

###     do i = 1, gts_len

        movl      %edx, %r10d                                   #27.5
        testq     %rdx, %rdx                                    #27.5
        jle       L_B1.46       # Prob 50%                      #27.5
                                # LOE rax rdx rsi rdi r8 r9 ecx ebx r10d r11d
L_B1.38:                        # Preds L_B1.37
        movslq    %edx, %rdx                                    #27.5
        cmpq      $4, %rdx                                      #27.5
        jl        L_B1.62       # Prob 10%                      #27.5
                                # LOE rax rdx rsi rdi r8 r9 ecx ebx r10d r11d
L_B1.39:                        # Preds L_B1.38
        movl      %edx, %r12d                                   #27.5
        xorl      %r11d, %r11d                                  #27.5

###         s2 = s2 + gts(i) * 3333

        movdqa    L_2il0floatpacket.3(%rip), %xmm0              #28.26
        andl      $-4, %r12d                                    #27.5
        movslq    %r12d, %r12                                   #27.5
        pxor      %xmm2, %xmm2                                  #26.5
        movdqa    L_2il0floatpacket.4(%rip), %xmm1              #28.26
        psrlq     $32, %xmm0                                    #28.26
                                # LOE rax rdx rsi rdi r8 r9 r11 r12 ecx ebx r10d xmm0 xmm1 xmm2
L_B1.40:                        # Preds L_B1.40 L_B1.39
        movdqa    (%rax,%r11,4), %xmm3                          #28.19
        addq      $4, %r11                                      #27.5
        movdqa    L_2il0floatpacket.3(%rip), %xmm4              #28.26
        cmpq      %r12, %r11                                    #27.5
        pmuludq   %xmm3, %xmm4                                  #28.26
        psrlq     $32, %xmm3                                    #28.26
        pmuludq   %xmm0, %xmm3                                  #28.26
        pand      %xmm1, %xmm4                                  #28.26
        psllq     $32, %xmm3                                    #28.26
        por       %xmm3, %xmm4                                  #28.26
        paddd     %xmm4, %xmm2                                  #28.9
        jb        L_B1.40       # Prob 82%                      #27.5
                                # LOE rax rdx rsi rdi r8 r9 r11 r12 ecx ebx r10d xmm0 xmm1 xmm2
L_B1.41:                        # Preds L_B1.40
        movdqa    %xmm2, %xmm0                                  #26.5
        psrldq    $8, %xmm0                                     #26.5
        paddd     %xmm0, %xmm2                                  #26.5
        movdqa    %xmm2, %xmm1                                  #26.5
        psrlq     $32, %xmm1                                    #26.5
        paddd     %xmm1, %xmm2                                  #26.5
        movd      %xmm2, %r11d                                  #26.5
                                # LOE rax rdx rsi rdi r8 r9 r12 ecx ebx r10d r11d
L_B1.42:                        # Preds L_B1.41 L_B1.62
        cmpq      %rdx, %r12                                    #27.5
        jae       L_B1.46       # Prob 3%                       #27.5
                                # LOE rax rdx rsi rdi r8 r9 r12 ecx ebx r10d r11d
L_B1.44:                        # Preds L_B1.42 L_B1.44
        imull     $3333, (%rax,%r12,4), %r13d                   #28.26
        incq      %r12                                          #27.5
        addl      %r13d, %r11d                                  #28.9
        cmpq      %rdx, %r12                                    #27.5
        jb        L_B1.44       # Prob 82%                      #27.5
                                # LOE rax rdx rsi rdi r8 r9 r12 ecx ebx r10d r11d
L_B1.46:                        # Preds L_B1.44 L_B1.37 L_B1.42

###     enddo
### 
### !   nor1 = map (/s1) A
###     do i = 1, N

        testl     %ebx, %ebx                                    #32.5
        jle       L_B1.58       # Prob 0%                       #32.5
                                # LOE rsi rdi r8 r9 ecx r10d r11d
L_B1.47:                        # Preds L_B1.46

###         nor1(i) = A(i) / s1 * 4444

        movq      56(%rdi), %rbx                                #33.19
        cmpq      $4, %rbx                                      #32.5
        movq      (%rdi), %r12                                  #33.19
        je        L_B1.53       # Prob 50%                      #32.5
                                # LOE rbx rsi r8 r9 r12 ecx r10d r11d
L_B1.48:                        # Preds L_B1.47
        xorl      %r13d, %r13d                                  #32.5
        xorl      %edi, %edi                                    #
                                # LOE rbx rsi rdi r8 r9 r12 r13 ecx r10d r11d
L_B1.49:                        # Preds L_B1.48 L_B1.49
        movl      (%rdi,%r12), %eax                             #33.24
        addq      %rbx, %rdi                                    #32.5
        cltd                                                    #33.24
        idivl     %ecx                                          #33.24
        imull     $4444, %eax, %r14d                            #33.9
        movl      %r14d, (%r8,%r13,4)                           #33.9
        incq      %r13                                          #32.5
        cmpq      %rsi, %r13                                    #32.5
        jb        L_B1.49       # Prob 82%                      #32.5
                                # LOE rbx rsi rdi r8 r9 r12 r13 ecx r10d r11d
L_B1.50:                        # Preds L_B1.49

###     enddo
### 
### !   nor2 = map (/s2) A
###     do i = 1, N

        xorl      %edi, %edi                                    #37.5
        xorl      %ecx, %ecx                                    #
                                # LOE rcx rbx rsi rdi r9 r12 r10d r11d
L_B1.51:                        # Preds L_B1.50 L_B1.51

###         nor2(i) = A(i) / s2 * 5555

        movl      (%rcx,%r12), %eax                             #38.24
        addq      %rbx, %rcx                                    #37.5
        cltd                                                    #38.24
        idivl     %r11d                                         #38.24
        imull     $5555, %eax, %r8d                             #38.9
        movl      %r8d, (%r9,%rdi,4)                            #38.9
        incq      %rdi                                          #37.5
        cmpq      %rsi, %rdi                                    #37.5
        jb        L_B1.51       # Prob 82%                      #37.5
        jmp       L_B1.58       # Prob 100%                     #37.5
                                # LOE rcx rbx rsi rdi r9 r12 r10d r11d
L_B1.53:                        # Preds L_B1.47
        xorl      %ebx, %ebx                                    #32.5
                                # LOE rbx rsi r8 r9 r12 ecx r10d r11d
L_B1.54:                        # Preds L_B1.53 L_B1.54
        movl      (%r12,%rbx,4), %eax                           #33.24
        cltd                                                    #33.24
        idivl     %ecx                                          #33.24
        imull     $4444, %eax, %edi                             #33.9
        movl      %edi, (%r8,%rbx,4)                            #33.9
        incq      %rbx                                          #32.5
        cmpq      %rsi, %rbx                                    #32.5
        jb        L_B1.54       # Prob 82%                      #32.5
                                # LOE rbx rsi r8 r9 r12 ecx r10d r11d
L_B1.55:                        # Preds L_B1.54
        xorl      %ecx, %ecx                                    #37.5
                                # LOE rcx rsi r9 r12 r10d r11d
L_B1.56:                        # Preds L_B1.55 L_B1.56
        movl      (%r12,%rcx,4), %eax                           #38.24
        cltd                                                    #38.24
        idivl     %r11d                                         #38.24
        imull     $5555, %eax, %ebx                             #38.9
        movl      %ebx, (%r9,%rcx,4)                            #38.9
        incq      %rcx                                          #37.5
        cmpq      %rsi, %rcx                                    #37.5
        jb        L_B1.56       # Prob 82%                      #37.5
                                # LOE rcx rsi r9 r12 r10d r11d
L_B1.58:                        # Preds L_B1.56 L_B1.51 L_B1.46

###     enddo
### 
###  end function normalise2

        movl      %r10d, %eax                                   #41.2
        movq      -40(%rbp), %rbx                               #41.2
L____tag_value__normalise2_.11:                                 #
        movq      -8(%rbp), %r12                                #41.2
L____tag_value__normalise2_.12:                                 #
        movq      -16(%rbp), %r13                               #41.2
L____tag_value__normalise2_.13:                                 #
        movq      -24(%rbp), %r14                               #41.2
L____tag_value__normalise2_.14:                                 #
        movq      -32(%rbp), %r15                               #41.2
L____tag_value__normalise2_.15:                                 #
        movq      %rbp, %rsp                                    #41.2
        popq      %rbp                                          #41.2
L____tag_value__normalise2_.16:                                 #
        ret                                                     #41.2
L____tag_value__normalise2_.17:                                 #
                                # LOE
L_B1.59:                        # Preds L_B1.3 L_B1.5 L_B1.7    # Infreq
        xorl      %r11d, %r11d                                  #12.5
        jmp       L_B1.15       # Prob 100%                     #12.5
                                # LOE rax rdx rsi rdi r8 r9 r11 ecx ebx
L_B1.62:                        # Preds L_B1.38                 # Infreq
        xorl      %r12d, %r12d                                  #27.5
        jmp       L_B1.42       # Prob 100%                     #27.5
        .align    4
L____tag_value__normalise2_.23:                                 #
                                # LOE rax rdx rsi rdi r8 r9 r12 ecx ebx r10d r11d
# mark_end;
	.section	__DATA, __data
# -- End  _normalise2_
	.section	__TEXT, __const
	.align 4
L_2il0floatpacket.3:
	.long	0x00000d05,0x00000d05,0x00000d05,0x00000d05
	.align 4
L_2il0floatpacket.4:
	.long	0xffffffff,0x00000000,0xffffffff,0x00000000
	.align 4
L_2il0floatpacket.5:
	.long	0x00000457,0x00000457,0x00000457,0x00000457
	.section	__DATA, __data
	.globl _normalise2_.eh
// -- Begin SEGMENT __eh_frame
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
__eh_frame_seg:
L.__eh_frame_seg:
EH_frame0:
L_fde_cie_0:
	.long 0x00000014
	.long 0x00000000
	.long 0x00527a01
	.long 0x01107801
	.long 0x08070c10
	.long 0x01900190
_normalise2_.eh:
	.long 0x00000074
	.long 0x0000001c
	.quad L____tag_value__normalise2_.1-_normalise2_.eh-0x8
	.set L_Qlab1,L____tag_value__normalise2_.23-L____tag_value__normalise2_.1
	.quad L_Qlab1
	.short 0x0400
	.set L_lab1,L____tag_value__normalise2_.3-L____tag_value__normalise2_.1
	.long L_lab1
	.short 0x100e
	.byte 0x04
	.set L_lab2,L____tag_value__normalise2_.4-L____tag_value__normalise2_.3
	.long L_lab2
	.long 0x8610060c
	.short 0x0402
	.set L_lab3,L____tag_value__normalise2_.6-L____tag_value__normalise2_.4
	.long L_lab3
	.short 0x0783
	.byte 0x04
	.set L_lab4,L____tag_value__normalise2_.7-L____tag_value__normalise2_.6
	.long L_lab4
	.short 0x068f
	.byte 0x04
	.set L_lab5,L____tag_value__normalise2_.8-L____tag_value__normalise2_.7
	.long L_lab5
	.long 0x048d038c
	.short 0x058e
	.byte 0x04
	.set L_lab6,L____tag_value__normalise2_.11-L____tag_value__normalise2_.8
	.long L_lab6
	.short 0x04c3
	.set L_lab7,L____tag_value__normalise2_.12-L____tag_value__normalise2_.11
	.long L_lab7
	.short 0x04cc
	.set L_lab8,L____tag_value__normalise2_.13-L____tag_value__normalise2_.12
	.long L_lab8
	.short 0x04cd
	.set L_lab9,L____tag_value__normalise2_.14-L____tag_value__normalise2_.13
	.long L_lab9
	.short 0x04ce
	.set L_lab10,L____tag_value__normalise2_.15-L____tag_value__normalise2_.14
	.long L_lab10
	.short 0x04cf
	.set L_lab11,L____tag_value__normalise2_.16-L____tag_value__normalise2_.15
	.long L_lab11
	.short 0x04c6
	.set L_lab12,L____tag_value__normalise2_.17-L____tag_value__normalise2_.16
	.long L_lab12
	.long 0x02860783
	.long 0x048d038c
	.long 0x068f058e
# End
	.subsections_via_symbols
