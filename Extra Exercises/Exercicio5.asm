;;;; This code calculates the value of sin(x) by Taylor Series.
;;;; To run this, first compile with nasm:
;; $ nasm -f elf Exercicio5.asm
;;;; Then link with gcc (ubuntu 64-bit)
;; $ gcc -m32 -o ex5 Exercicio5.o
;; And execute by typing
;; ./ex5
;; or:
;; alias e="nasm -f elf Exercicio5.asm && gcc -m32 -o  ex5 Exercicio5.o && ./ex5"
;; e
;;;; The input/output system is implemented in C language
;;;; The function that calculates the sin(x) is made in nasm assembly x86
;; The program reads from user an angle in degrees (float)
;; Then it reads the maximum difference between the calculated value and the value calculated by FSIN
;; The output is the calculated value of sin(x) and the number of iterations

extern printf						; Specifies C printf function from stdio.h
extern scanf                        ; Specifies C scanf function from stdio.h

section .data
    MAX dd 4294967295				; max value of double word, for some neat precision
	n_iter	dd 0				    ; counts number of iterations
	dois dq 2.0						; for quick loading on FPU stack. value has to be specified as floating point, else we should use integer specific instructions.
	n 	dq  0.0						; current iteration
	sinal dq -1.0					; signal of numerator (keeps switching)
	acc dq 0.0						; acumulator for the sum
	sin dq 0.0						; to store sin's value at the end of the sum
    x dq 0.0                        ; to store angle user input
    e dq 0.0                        ; to store error user input
    fat dq 1.0                      ; stores factorial
    two_n_plus_one dq 0.0           ;stores 2n+1 (obviously)
    pi_by_180 dq 0.017453292
    x_squared dq 0.0                ;stores x²
    aux dq 0.0
    real_sin dq 0.0                 ; to store fsin calculated value

    out_0 db 'Insira valor do angulo em Graus e a Precisao desejada:', 10,0
	out_1 db 'Calculated sin(x) by Taylor = %lf', 10,0		; string as parameter to printf
    out_2 db 'Calculated sin(x) by fsin = %lf', 10,0		; string as parameter to printf
    out_3 db 'Number of Iterations: %d', 10,0

    in_1 db '%lf %lf', 0

    debug_real_sin db 'valor calculado de fsin: %lf',10, 0
    debug db 'valor atual de sin: %lf',10,0
    debug_radians db 'valor convertido em rad: %lf', 10, 0
    debug_general db 'debug: %lf',10,0

section .text
global main							; it has to be main since we're using gcc linker.
	main:
        push out_0
        call printf
        add esp, 4

        push dword e
        push dword x 
        push in_1
        call scanf
        add esp, 12 

        ;converts to radians
        fld qword[x]
        fld qword[pi_by_180]
        fmulp
        fst qword[x]            ;stores new value of x in radians

        fsin
        fstp qword[real_sin]
        ;converts to radians

        ;creating x²
        fld qword[x]
        fld qword[x]
        fmulp
        fstp qword[x_squared]
        ;creating x²

        ;;;DEBUG
        ;push dword[real_sin + 4]
        ;push dword[real_sin]
        ;push debug_real_sin
        ;call printf
        ;add esp, 12
        ;;;DEBUG

        mov ecx, 0					

		.while:
			; denominator
			fld1					; loads 1 into the stack
			fld qword[n]			; loads n into the stack
			fld qword[dois]			; loads 2 into the stack
			fmulp					; 2*n and pops the stack
			faddp					; 2*n + 1 and pops the stack
            fst qword[two_n_plus_one]
			; denominator

            ;factorial
            cmp ecx, 0
            je .continue
            .factorial:
                fld qword[dois]
                fld qword[n]
                fmulp
                fld qword[fat]
                fmulp
                fmulp
            ;factorial

            ;power
            .pow:
                fld qword[x]
                fld qword[x_squared]
                fmulp
                fstp qword[x]
            ;power

            .continue:            
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
                .multiply:
                    fld qword[x]
                    fmulp
                ;multiply

                ; summing
                fld qword[acc]			; loads the sum acumulator into the stack
                faddp 					; adds the division with the accumulator and pops the stack
                fstp qword [acc]			; stores the updated acumulator value into memory and pops the stack
                ; summing

                ; incrementing n
                fld qword[n]			; loads n value into the stack
                fld1					; loads 1 into the stack
                faddp					; adds n+1 and pops the stack
                fstp qword[n]			; saves the updated n value to memory and pops stack.
                ; incrementing n

                ; counting iterations
                inc ecx
                mov dword[n_iter], ecx
                ; counting iterations

                ;cmp ecx, 200
                ;ja .bosta

                ;comparing actual value with fsin
                fld qword[e]
                fld qword[real_sin]
                fld qword[acc]
                fsubp
                fabs
                fst qword [aux]
                fcomip st0, st1
                fstp qword[e]
                ;comparing actual value with fsin
                
        ja .while
    
    ;.bosta:

    ; moving calculated sin to appropriate variable
    fld qword[acc]
    fstp qword[sin]
    ; moving calculated sin to appropriate variable       


    ; printing sin by taylor
    push dword[sin + 4]			; pushing half the memory space of sin into the stack (we can only push 4 bytes a time)
    push dword[sin]				; pushing the other half (printf second parameter pushed)
    push out_1					; pushing the printf first parameter
    call printf					; calling printf to show us sin
    add esp, 12					; reseting the esp pointer so we don't have to pop the stack
    ; printing sin by taylor

    ; printing sin by fsin
    push dword[real_sin + 4]			; pushing half the memory space of sin into the stack (we can only push 4 bytes a time)
    push dword[real_sin]				; pushing the other half (printf second parameter pushed)
    push out_2					; pushing the printf first parameter
    call printf					; calling printf to show us sin
    add esp, 12					; reseting the esp pointer so we don't have to pop the stack
    ; printing sin by fsin

    ; printing number of iterations
    push dword[n_iter]			; pushing half the memory space of sin into the stack (we can only push 4 bytes a time)
    push out_3					; pushing the printf first parameter
    call printf					; calling printf to show us sin
    add esp, 8					; reseting the esp pointer so we don't have to pop the stack
    ; printing number of iterations


    ;;;DEBUG
    pushfd
    push dword[aux + 4]
    push dword[aux]
    push debug_general
    call printf
    add esp, 12
    ;;;DEBUG
    
    ;;;DEBUG
    push dword[e + 4]
    push dword[e]
    push debug_general
    call printf
    add esp, 12
    popfd
    ;;;DEBUG
        

	jmp END						; goodbye, caroline


END:
	mov eax, 1 						; exit syscall
	mov ebx, 0 						; program return
	int 80H 						; syscall interruption
