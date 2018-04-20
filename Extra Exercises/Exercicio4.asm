;;; This code calculates Leibniz's Formulae for pi.
;; The formulae is the sum of [(-1)^i]/[(2*i) + 1] from i = 0 to infinity = pi/4
;; To run this, first compile with nasm:
;; $ nasm -f elf Exercicio4.asm
;; Then link with gcc (ubuntu 64-bit)
;; $ gcc -m32 -o ex4 Exercicio4.o
;; And execute by typing
;; ./ex4

extern printf						; Specifies C printf function from stdio.h

section .data
	n	dd 4294967295				; max value of double word, for some neat precision
	dois dq 2.0						; for quick loading on FPU stack. value has to be specified as floating point, else we should use integer specific instructions.
	i 	dq  0.0						; counter
	sinal dq -1.0					; signal of numerator (keeps switching)
	acc dq 0.0						; acumulator for the sum
	pi dq 0.0						; to store pi's value at the end of the sum
	msg db 'Valor = %lf', 10,0		; string as parameter to printf
section .text
global main							; it has to be main since we're using gcc linker.
	main:
		mov ecx, 0					
		mov ecx, dword[n]			; puts the number of iterations on ecx reg.
		.laco:
			; denominator
			fld1					; loads 1 into the stack
			fld qword[i]			; loads i into the stack
			fld qword[dois]			; loads 2 into the stack
			fmulp					; 2*i and pops the stack
			faddp					; 2*i + 1 and pops the stack
			; denominator

			; numerator
			fld qword[sinal]		; loads the "1" with current signal
			fchs					; swap current signal
			fst qword[sinal]		; commit swap to memory to be swapped again next iteration
			; numerator

			; dividing
			fxch st1				; swap the contents of thestack (numerator with denominator)
			fdivp st1, st0			; divides 1 or -1 by (2*i + 1)
			; dividing

			; summing
			fld qword[acc]			; loads the sum acumulator into the stack
			faddp 					; adds the division with the accumulator and pops the stack
			fstp qword acc]			; stores the updated acumulator value into memory and pops the stack
			; summing

			; incrementing i
			fld qword[i]			; loads i value into the stack
			fld1					; loads 1 into the stack
			faddp					; adds i+1 and pops the stack
			fstp qword[i]			; saves the updated i value to memory and pops stack.
			; incrementing i

			loop .laco				; loops until ecx = 0

		; obtaining pi
		fld qword[acc]				; loads acumulator value into the stack
		fld qword[dois]				; loads 2 into the stack
		fld qword[dois]				; loads 2 into the stack again
		faddp						; adds 2+2 and pops the stack
		fmulp						; accumulator*4 (since we calculated pi/4) and pops the stack
		; obtaining pi
		
		fstp qword[pi]				; stores the value of pi into the memory and pops the stack (leaving it empty)

		; printing pi
		push dword[pi + 4]			; pushing half the memory space of pi into the stack (we can only push 4 bytes a time)
		push dword[pi]				; pushing the other half (printf second parameter pushed)
		push msg					; pushing the printf first parameter
		call printf					; calling printf to show us pi
		add esp, 12					; reseting the esp pointer so we don't have to pop the stack
		; printing pi

		jmp END						; goodbye, caroline


END:
	mov eax, 1 						; exit syscall
	mov ebx, 0 						; program return
	int 80H 						; syscall interruption
