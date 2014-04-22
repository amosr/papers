# mark_description "Intel(R) Fortran Intel(R) 64 Compiler XE for applications running on Intel(R) 64, Version 14.0.2.139 Build 2";
# mark_description "0140121";
# mark_description "-O3 -c -save-temps -fsource-asm -Fadumb.s";
	.file "dumb.f90"
	.section	__TEXT, __text
L_TXTST0:
# -- Begin  _dumb_
# mark_begin;
       .align    4
	.globl _dumb_
_dumb_:
# parameter 1: %rdi
L_B1.1:                         # Preds L_B1.0

###  function dumb(A) result (ou)

L____tag_value__dumb_.1:                                        #1.11
        pushq     %rbx                                          #1.11
L____tag_value__dumb_.3:                                        #
        pushq     %rbp                                          #1.11
L____tag_value__dumb_.5:                                        #
        movq      %rdi, %rdx                                    #1.11

###      integer, intent(in), dimension(:) :: A
###      integer :: ou
### 
###      integer :: i1, i2, N, s1, s2
###      N  = size(A)

        xorl      %ebx, %ebx                                    #6.6

###      s1 = 0

        xorl      %eax, %eax                                    #7.6

###      s2 = 0

        xorl      %r8d, %r8d                                    #8.6
        movq      48(%rdx), %rcx                                #6.6
        testq     %rcx, %rcx                                    #6.6
        cmovle    %rbx, %rcx                                    #6.6
        movl      %ecx, %esi                                    #6.6

### 
###      do i1 = 1, N

        testl     %esi, %esi                                    #10.6
        jle       L_B1.30       # Prob 0%                       #10.6
                                # LOE rdx rcx rbx r12 r13 r14 r15 eax esi r8d
L_B1.2:                         # Preds L_B1.1

###          s1 = s1 + A(i1) * 1111

        movq      56(%rdx), %rbp                                #11.20
        cmpq      $4, %rbp                                      #10.6
        movq      (%rdx), %rdi                                  #11.20
        jne       L_B1.24       # Prob 50%                      #10.6
                                # LOE rcx rbx rbp rdi r12 r13 r14 r15 eax esi r8d
L_B1.3:                         # Preds L_B1.2
        movslq    %ecx, %rbp                                    #10.6
        cmpq      $4, %rbp                                      #10.6
        jl        L_B1.31       # Prob 10%                      #10.6
                                # LOE rbx rbp rdi r12 r13 r14 r15 eax r8d
L_B1.4:                         # Preds L_B1.3
        movq      %rdi, %rdx                                    #10.6
        andq      $15, %rdx                                     #10.6
        testl     %edx, %edx                                    #10.6
        je        L_B1.7        # Prob 50%                      #10.6
                                # LOE rbx rbp rdi r12 r13 r14 r15 eax edx r8d
L_B1.5:                         # Preds L_B1.4
        testb     $3, %dl                                       #10.6
        jne       L_B1.31       # Prob 10%                      #10.6
                                # LOE rbx rbp rdi r12 r13 r14 r15 eax edx r8d
L_B1.6:                         # Preds L_B1.5
        negl      %edx                                          #10.6
        addl      $16, %edx                                     #10.6
        shrl      $2, %edx                                      #10.6
                                # LOE rbx rbp rdi r12 r13 r14 r15 eax edx r8d
L_B1.7:                         # Preds L_B1.6 L_B1.4
        movl      %edx, %ecx                                    #10.6
        lea       4(%rcx), %rsi                                 #10.6
        cmpq      %rsi, %rbp                                    #10.6
        jl        L_B1.31       # Prob 10%                      #10.6
                                # LOE rcx rbx rbp rdi r12 r13 r14 r15 eax edx r8d
L_B1.8:                         # Preds L_B1.7
        movl      %ebp, %esi                                    #10.6
        negl      %edx                                          #10.6
        addl      %esi, %edx                                    #10.6
        andl      $3, %edx                                      #10.6
        subl      %edx, %esi                                    #10.6
        movslq    %esi, %rdx                                    #10.6
        testq     %rcx, %rcx                                    #10.6
        jbe       L_B1.12       # Prob 3%                       #10.6
                                # LOE rdx rcx rbx rbp rdi r12 r13 r14 r15 eax r8d
L_B1.10:                        # Preds L_B1.8 L_B1.10
        movl      (%rdi,%rbx,4), %r10d                          #11.20
        incq      %rbx                                          #10.6
        imull     $1111, %r10d, %r9d                            #11.26

###      enddo
###     
###      do i1 = 1, N
###          s2 = s2 - A(i1) * 2222

        imull     $-2222, %r10d, %r11d                          #15.26
        addl      %r9d, %eax                                    #11.10
        addl      %r11d, %r8d                                   #15.10
        cmpq      %rcx, %rbx                                    #10.6
        jb        L_B1.10       # Prob 82%                      #10.6
                                # LOE rdx rcx rbx rbp rdi r12 r13 r14 r15 eax r8d
L_B1.12:                        # Preds L_B1.10 L_B1.8
        movd      %eax, %xmm1                                   #7.6
        lea       (%rdi,%rcx,4), %rax                           #10.6
        movd      %r8d, %xmm0                                   #8.6
        testq     $15, %rax                                     #10.6
        je        L_B1.16       # Prob 60%                      #10.6
                                # LOE rdx rcx rbp rdi r12 r13 r14 r15 xmm0 xmm1
L_B1.13:                        # Preds L_B1.12
        movdqa    L_2il0floatpacket.3(%rip), %xmm3              #15.28
        movdqa    L_2il0floatpacket.1(%rip), %xmm4              #11.26
        movdqa    %xmm3, %xmm2                                  #15.26
        movdqa    L_2il0floatpacket.2(%rip), %xmm5              #11.26
        psrlq     $32, %xmm4                                    #11.26
        psrlq     $32, %xmm2                                    #15.26
        .align    4
                                # LOE rdx rcx rbp rdi r12 r13 r14 r15 xmm0 xmm1 xmm2 xmm3 xmm4 xmm5
L_B1.14:                        # Preds L_B1.14 L_B1.13
        movdqu    (%rdi,%rcx,4), %xmm9                          #11.20
        movdqa    %xmm9, %xmm8                                  #11.26
        movdqa    %xmm4, %xmm6                                  #11.26
        movdqa    L_2il0floatpacket.1(%rip), %xmm7              #11.26
        psrlq     $32, %xmm8                                    #11.26
        pmuludq   %xmm9, %xmm7                                  #11.26
        addq      $4, %rcx                                      #10.6
        pmuludq   %xmm8, %xmm6                                  #11.26
        pmuludq   %xmm3, %xmm9                                  #15.26
        pmuludq   %xmm2, %xmm8                                  #15.26
        pand      %xmm5, %xmm7                                  #11.26
        psllq     $32, %xmm6                                    #11.26
        pand      %xmm5, %xmm9                                  #15.26
        psllq     $32, %xmm8                                    #15.26
        por       %xmm6, %xmm7                                  #11.26
        por       %xmm8, %xmm9                                  #15.26
        paddd     %xmm7, %xmm1                                  #11.10
        psubd     %xmm9, %xmm0                                  #15.10
        cmpq      %rdx, %rcx                                    #10.6
        jb        L_B1.14       # Prob 82%                      #10.6
        jmp       L_B1.19       # Prob 100%                     #10.6
                                # LOE rdx rcx rbp rdi r12 r13 r14 r15 xmm0 xmm1 xmm2 xmm3 xmm4 xmm5
L_B1.16:                        # Preds L_B1.12
        movdqa    L_2il0floatpacket.3(%rip), %xmm3              #15.28
        movdqa    L_2il0floatpacket.1(%rip), %xmm4              #11.26
        movdqa    %xmm3, %xmm2                                  #15.26
        movdqa    L_2il0floatpacket.2(%rip), %xmm5              #11.26
        psrlq     $32, %xmm4                                    #11.26
        psrlq     $32, %xmm2                                    #15.26
        .align    4
                                # LOE rdx rcx rbp rdi r12 r13 r14 r15 xmm0 xmm1 xmm2 xmm3 xmm4 xmm5
L_B1.17:                        # Preds L_B1.17 L_B1.16
        movdqa    (%rdi,%rcx,4), %xmm9                          #11.20
        movdqa    %xmm4, %xmm6                                  #11.26
        movdqa    %xmm9, %xmm8                                  #11.26
        addq      $4, %rcx                                      #10.6
        movdqa    L_2il0floatpacket.1(%rip), %xmm7              #11.26
        psrlq     $32, %xmm8                                    #11.26
        pmuludq   %xmm9, %xmm7                                  #11.26
        cmpq      %rdx, %rcx                                    #10.6
        pmuludq   %xmm8, %xmm6                                  #11.26
        pmuludq   %xmm3, %xmm9                                  #15.26
        pmuludq   %xmm2, %xmm8                                  #15.26
        pand      %xmm5, %xmm7                                  #11.26
        psllq     $32, %xmm6                                    #11.26
        pand      %xmm5, %xmm9                                  #15.26
        psllq     $32, %xmm8                                    #15.26
        por       %xmm6, %xmm7                                  #11.26
        por       %xmm8, %xmm9                                  #15.26
        paddd     %xmm7, %xmm1                                  #11.10
        psubd     %xmm9, %xmm0                                  #15.10
        jb        L_B1.17       # Prob 82%                      #10.6
                                # LOE rdx rcx rbp rdi r12 r13 r14 r15 xmm0 xmm1 xmm2 xmm3 xmm4 xmm5
L_B1.19:                        # Preds L_B1.17 L_B1.14
        movdqa    %xmm0, %xmm2                                  #8.6
        psrldq    $8, %xmm2                                     #8.6
        paddd     %xmm2, %xmm0                                  #8.6
        movdqa    %xmm0, %xmm3                                  #8.6
        psrlq     $32, %xmm3                                    #8.6
        paddd     %xmm3, %xmm0                                  #8.6
        movd      %xmm0, %r8d                                   #8.6
        movdqa    %xmm1, %xmm0                                  #7.6
        psrldq    $8, %xmm0                                     #7.6
        paddd     %xmm0, %xmm1                                  #7.6
        movdqa    %xmm1, %xmm4                                  #7.6
        psrlq     $32, %xmm4                                    #7.6
        paddd     %xmm4, %xmm1                                  #7.6
        movd      %xmm1, %eax                                   #7.6
                                # LOE rdx rbp rdi r12 r13 r14 r15 eax r8d
L_B1.20:                        # Preds L_B1.19 L_B1.31
        cmpq      %rbp, %rdx                                    #10.6
        jae       L_B1.30       # Prob 3%                       #10.6
                                # LOE rdx rbp rdi r12 r13 r14 r15 eax r8d
L_B1.22:                        # Preds L_B1.20 L_B1.22
        movl      (%rdi,%rdx,4), %ebx                           #11.20
        incq      %rdx                                          #10.6
        imull     $1111, %ebx, %ecx                             #11.26
        imull     $-2222, %ebx, %r9d                            #15.26
        addl      %ecx, %eax                                    #11.10
        addl      %r9d, %r8d                                    #15.10
        cmpq      %rbp, %rdx                                    #10.6
        jb        L_B1.22       # Prob 82%                      #10.6
        jmp       L_B1.30       # Prob 100%                     #10.6
                                # LOE rdx rbp rdi r12 r13 r14 r15 eax r8d
L_B1.24:                        # Preds L_B1.2
        movl      %esi, %ecx                                    #10.6
        movl      $1, %r9d                                      #10.6
        shrl      $31, %ecx                                     #10.6
        movq      %rbx, %rdx                                    #
        addl      %esi, %ecx                                    #10.6
        sarl      $1, %ecx                                      #10.6
        movslq    %ecx, %rcx                                    #10.6
        testq     %rcx, %rcx                                    #10.6
        jbe       L_B1.28       # Prob 3%                       #10.6
                                # LOE rdx rcx rbx rbp rdi r12 r13 r14 r15 eax esi r8d r9d
L_B1.25:                        # Preds L_B1.24
        movq      %rdi, %r9                                     #
        xorl      %r10d, %r10d                                  #10.6
        subq      %rbp, %r9                                     #
        xorl      %r11d, %r11d                                  #10.6
        movq      %r15, -8(%rsp)                                #
        lea       (%r9,%rbp,2), %r9                             #
L____tag_value__dumb_.7:                                        #
                                # LOE rdx rcx rbx rbp rdi r9 r12 r13 r14 eax esi r8d r10d r11d
L_B1.26:                        # Preds L_B1.26 L_B1.25
        imull     $1111, (%rdx,%rdi), %r15d                     #11.26
        incq      %rbx                                          #10.6
        addl      %r15d, %eax                                   #11.10
        imull     $-2222, (%rdx,%rdi), %r15d                    #15.26
        addl      %r15d, %r8d                                   #15.10
        imull     $1111, (%rdx,%r9), %r15d                      #11.26
        addl      %r15d, %r10d                                  #11.10
        imull     $-2222, (%rdx,%r9), %r15d                     #15.26
        lea       (%rdx,%rbp,2), %rdx                           #10.6
        addl      %r15d, %r11d                                  #15.10
        cmpq      %rcx, %rbx                                    #10.6
        jb        L_B1.26       # Prob 63%                      #10.6
                                # LOE rdx rcx rbx rbp rdi r9 r12 r13 r14 eax esi r8d r10d r11d
L_B1.27:                        # Preds L_B1.26
        movq      -8(%rsp), %r15                                #
L____tag_value__dumb_.8:                                        #
        addl      %r11d, %r8d                                   #10.6
        addl      %r10d, %eax                                   #10.6
        lea       1(%rbx,%rbx), %r9d                            #11.10
                                # LOE rbp rdi r12 r13 r14 r15 eax esi r8d r9d
L_B1.28:                        # Preds L_B1.27 L_B1.24
        lea       -1(%r9), %edx                                 #10.6
        cmpl      %edx, %esi                                    #10.6
        jbe       L_B1.30       # Prob 3%                       #10.6
                                # LOE rbp rdi r12 r13 r14 r15 eax r8d r9d
L_B1.29:                        # Preds L_B1.28
        movslq    %r9d, %r9                                     #11.20
        subq      %rbp, %rdi                                    #11.10
        imulq     %r9, %rbp                                     #11.20
        movl      (%rbp,%rdi), %ecx                             #11.20
        imull     $1111, %ecx, %edx                             #11.26
        imull     $-2222, %ecx, %ebx                            #15.26
        addl      %edx, %eax                                    #11.10
        addl      %ebx, %r8d                                    #15.10
                                # LOE r12 r13 r14 r15 eax r8d
L_B1.30:                        # Preds L_B1.22 L_B1.20 L_B1.1 L_B1.29 L_B1.28
                                #      

###      enddo
### 
###      ou = s1 * s2

        imull     %r8d, %eax                                    #18.6

###         
###  end function dumb

L____tag_value__dumb_.9:                                        #20.2
        popq      %rbp                                          #20.2
L____tag_value__dumb_.10:                                       #
        popq      %rbx                                          #20.2
L____tag_value__dumb_.12:                                       #
        ret                                                     #20.2
L____tag_value__dumb_.13:                                       #
                                # LOE
L_B1.31:                        # Preds L_B1.3 L_B1.5 L_B1.7    # Infreq
        movq      %rbx, %rdx                                    #10.6
        jmp       L_B1.20       # Prob 100%                     #10.6
        .align    4
L____tag_value__dumb_.16:                                       #
                                # LOE rdx rbp rdi r12 r13 r14 r15 eax r8d
# mark_end;
	.section	__DATA, __data
# -- End  _dumb_
	.section	__TEXT, __const
	.align 4
L_2il0floatpacket.1:
	.long	0x00000457,0x00000457,0x00000457,0x00000457
	.align 4
L_2il0floatpacket.2:
	.long	0xffffffff,0x00000000,0xffffffff,0x00000000
	.align 4
L_2il0floatpacket.3:
	.long	0x000008ae,0x000008ae,0x000008ae,0x000008ae
	.section	__DATA, __data
	.globl _dumb_.eh
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
_dumb_.eh:
	.long 0x00000054
	.long 0x0000001c
	.quad L____tag_value__dumb_.1-_dumb_.eh-0x8
	.set L_Qlab1,L____tag_value__dumb_.16-L____tag_value__dumb_.1
	.quad L_Qlab1
	.short 0x0400
	.set L_lab1,L____tag_value__dumb_.3-L____tag_value__dumb_.1
	.long L_lab1
	.long 0x0283100e
	.byte 0x04
	.set L_lab2,L____tag_value__dumb_.5-L____tag_value__dumb_.3
	.long L_lab2
	.long 0x0386180e
	.byte 0x04
	.set L_lab3,L____tag_value__dumb_.7-L____tag_value__dumb_.5
	.long L_lab3
	.short 0x048f
	.byte 0x04
	.set L_lab4,L____tag_value__dumb_.8-L____tag_value__dumb_.7
	.long L_lab4
	.short 0x04cf
	.set L_lab5,L____tag_value__dumb_.9-L____tag_value__dumb_.8
	.long L_lab5
	.short 0x04c6
	.set L_lab6,L____tag_value__dumb_.10-L____tag_value__dumb_.9
	.long L_lab6
	.long 0x04c3100e
	.set L_lab7,L____tag_value__dumb_.12-L____tag_value__dumb_.10
	.long L_lab7
	.short 0x080e
	.byte 0x04
	.set L_lab8,L____tag_value__dumb_.13-L____tag_value__dumb_.12
	.long L_lab8
	.long 0x0283180e
	.short 0x0386
# End
	.subsections_via_symbols
