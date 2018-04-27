;;;; This code calculates the value of sin(x) by Taylor Series.
;;;; To run this, first compile with nasm:
;; $ nasm -f elf Exercicio5.asm
;;;; Then link with gcc (ubuntu 64-bit)
;; $ gcc -m32 -o ex4 Exercicio5.o
;; And execute by typing
;; ./ex5
;;;; The input/output system is implemented in C language
;;;; The function that calculates the sin(x) is made in nasm assembly x86
;; The program reads from user an angle in degrees (float)
;; Then it reads the maximum difference between the calculated value and the value calculated by FSIN
;; The output is the calculated value of sin(x) and the number of iterations

extern printf						; Specifies C printf function from stdio.h
extern scanf                        ; Specifies C scanf function from stdio.h

section .data
	n_iter	dd 0				    ; counts number of iterations
	dois dq 2.0						; for quick loading on FPU stack. value has to be specified as floating point, else we should use integer specific instructions.
	n 	dq  0.0						; current iteration
	sinal dq -1.0					; signal of numerator (keeps switching)
	acc dq 0.0						; acumulator for the sum
	sin dq 0.0						; to store sin's value at the end of the sum
    x dq 0.0                        ; to store angle user input
    e dq 0.0                        ; to store error user input

    real_sin dq 0.0                 ; to store fsin calculated value

	out_1 db 'Calculated sin(x) by Taylor = %lf', 10,0		; string as parameter to printf
    out_2 db 'Calculated sin(x) by fsin = %lf', 10,0		; string as parameter to printf
    out_3 db 'Number of Iterations: %d', 10,0

    out_0 db 'Insira valor do angulo em Graus:', 10,0
    in_1 db '%lf %lf', 0

section .text
global main							; it has to be main since we're using gcc linker.
	main:
		mov ecx, 0					
		mov ecx, dword[n]			; puts the number of iterations on ecx reg.

        push out_0
        call printf
        add esp, 4

        push dword[e+4]
        push dword[e]
        push dword[x+4]
        push dword[x]
        push in_1
        call scanf
        add esp, 20 
        ;;;;;;;

        push dword[e+4]
        push dword[e]
        push out_1
        call printf
        add esp, 12 
        push dword[x+4]
        push dword[x]
        push out_2
        call printf
        add esp, 12 

        ;;;;;;

        jmp END

		.while:
			; denominator
			fld1					; loads 1 into the stack
			fld qword[n]			; loads n into the stack
			fld qword[dois]			; loads 2 into the stack
			fmulp					; 2*n and pops the stack
			faddp					; 2*n + 1 and pops the stack
			; denominator

            ;factorial

            ;factorial

			; numerator
			fld qword[sinal]		; loads the "1" with current signal
			fchs					; swap current signal
			fst qword[sinal]		; commit swap to memory to be swapped again next iteration
			; numerator

			; dividing
			fxch st1				; swap the contents of thestack (numerator with denominator)
			fdivp st1, st0			; divides 1 or -1 by (2*n + 1)
			; dividing

            ;multiply

            ;multiply

			; summing
			fld qword[acc]			; loads the sum acumulator into the stack
			faddp 					; adds the division with the accumulator and pops the stack
			fstp qword acc]			; stores the updated acumulator value into memory and pops the stack
			; summing

			; incrementing n
			fld qword[n]			; loads n value into the stack
			fld1					; loads 1 into the stack
			faddp					; adds n+1 and pops the stack
			fstp qword[n]			; saves the updated n value to memory and pops stack.
			; incrementing n

            ;comparing actual value with fsin
            ;comparting actual value with fsin

			jge .while				; loops until ecx = 0

		; obtaining sin
		fld qword[acc]				; loads acumulator value into the stack
		fld qword[dois]				; loads 2 into the stack
		fld qword[dois]				; loads 2 into the stack again
		faddp						; adds 2+2 and pops the stack
		fmulp						; accumulator*4 (since we calculated sin/4) and pops the stack
		; obtaining sin
		
		fstp qword[sin]				; stores the value of sin into the memory and pops the stack (leaving it empty)

		; printing sin
		push dword[sin + 4]			; pushing half the memory space of sin into the stack (we can only push 4 bytes a time)
		push dword[sin]				; pushing the other half (printf second parameter pushed)
		push out_1					; pushing the printf first parameter
		call printf					; calling printf to show us sin
		add esp, 12					; reseting the esp pointer so we don't have to pop the stack
		; printing sin

		jmp END						; goodbye, caroline


END:
	mov eax, 1 						; exit syscall
	mov ebx, 0 						; program return
	int 80H 						; syscall interruption
