	.file	"gprof_test.cc"
	.text
	.globl	_Z1av
	.type	_Z1av, @function
_Z1av:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$0, -8(%rbp)
	movl	$0, -4(%rbp)
.L3:
	movl	-8(%rbp), %eax
	leal	1(%rax), %edx
	movl	%edx, -8(%rbp)
	cmpl	$99999, %eax
	setle	%al
	testb	%al, %al
	je	.L2
	movl	-8(%rbp), %eax
	addl	%eax, -4(%rbp)
	jmp	.L3
.L2:
	movl	-4(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	_Z1av, .-_Z1av
	.globl	_Z1bv
	.type	_Z1bv, @function
_Z1bv:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$0, -8(%rbp)
	movl	$0, -4(%rbp)
.L7:
	movl	-8(%rbp), %eax
	leal	1(%rax), %edx
	movl	%edx, -8(%rbp)
	cmpl	$399999, %eax
	setle	%al
	testb	%al, %al
	je	.L6
	movl	-8(%rbp), %eax
	addl	%eax, -4(%rbp)
	jmp	.L7
.L6:
	movl	-4(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	_Z1bv, .-_Z1bv
	.globl	main
	.type	main, @function
main:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	$100000, -12(%rbp)
.L13:
	movl	-12(%rbp), %eax
	leal	-1(%rax), %edx
	movl	%edx, -12(%rbp)
	testl	%eax, %eax
	setne	%al
	testb	%al, %al
	je	.L10
	call	_Z1av
	call	_Z1bv
	movl	$0, -8(%rbp)
	movl	$0, -4(%rbp)
.L12:
	movl	-8(%rbp), %eax
	leal	1(%rax), %edx
	movl	%edx, -8(%rbp)
	cmpl	$29999, %eax
	setle	%al
	testb	%al, %al
	je	.L13
	movl	-8(%rbp), %eax
	addl	%eax, -4(%rbp)
	jmp	.L12
.L10:
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.10) 5.4.0 20160609"
	.section	.note.GNU-stack,"",@progbits
