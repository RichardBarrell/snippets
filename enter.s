	.file	"enter.c"
	.text
	.globl	a
	.type	a, @function
a:
.LFB0:
	.cfi_startproc
	leal	1(%rdi), %eax
	ret
	.cfi_endproc
.LFE0:
	.size	a, .-a
	.section	.text.startup,"ax",@progbits
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	movl	$100000000, %edx
.L3:
	movl	$40, %edi
	call	a
	decl	%edx
	jne	.L3
	xorl	%eax, %eax
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.ident	"GCC: (GNU) 6.4.1 20170727 (Red Hat 6.4.1-1)"
	.section	.note.GNU-stack,"",@progbits
