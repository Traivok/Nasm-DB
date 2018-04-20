extern printf

section .data
	n	dd 4294967295
	dois dq 2.0
	i 	dq  0.0
	sinal dq -1.0
	acc dq 0.0
	pi dq 0.0
	msg db 'Valor = %lf', 10,0		;string para printf
section .text
global main
	main:
		mov ecx, 0
		mov ecx, dword [n]
		.laco:
			;denominador
			fld1
			fld qword [i]
			fld qword  [dois]
			fmulp
			faddp
			;denominador

			fld qword [sinal]
			fchs
			fst qword[sinal]
			fxch st1
			fdivp st1, st0

			fld qword [acc]
			faddp 
			fstp qword [acc]

			fld qword [i]
			fld1
			faddp
			fstp qword [i]

			loop .laco

		fld qword[acc]
		
		fld qword[dois]
		fld qword[dois]
		faddp
		fmulp
		
		fstp qword [pi]


	push dword[pi + 4]
	push dword[pi]
	push msg
	call printf
	add esp, 12

	jmp FIM
	
FIM:
MOV EAX, 1 ; exit syscall
MOV EBX, 0 ; program return
INT 80H ; syscall interruption
