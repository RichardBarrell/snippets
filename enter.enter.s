	.file	"enter.c"
	.text
	.globl	a
	.type	a, @function
a:
	enter	$0, $0
	movl	8(%ebp), %eax
	popl	%ebp
	incl	%eax
	ret
	.size	a, .-a
	.section	.text.startup,"ax",@progbits
	.globl	main
	.type	main, @function
main:
	pushl	%ebp
	movl	$100000000, %edx
	movl	%esp, %ebp
.L4:
	pushl	$40
	call	a
	decl	%edx
	popl	%eax
	jne	.L4
	xorl	%eax, %eax
	leave
	ret
	.size	main, .-main
	.ident	"GCC: (GNU) 6.4.1 20170727 (Red Hat 6.4.1-1)"
	.section	.note.GNU-stack,"",@progbits
