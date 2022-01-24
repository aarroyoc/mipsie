	.data
loop_str:	.asciiz "\nHello\n"

	.text
loop:	li $v0, 4
	la $a0, loop_str
	syscall
	j loop
