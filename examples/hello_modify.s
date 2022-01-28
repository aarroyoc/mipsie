	.data
hello_world:	.asciiz "Hello World\n"
	.text
main:	la $t0, hello_world
	li $s0, 90
	sb $s0, 6($t0)

	li $v0, 4
	move $a0, $t0
	syscall

	li $v0, 10
	syscall
	
