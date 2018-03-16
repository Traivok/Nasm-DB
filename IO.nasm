org 0x7c00
jmp 0x0000:start

myStringBoladona db "Cuan dificil es,\ncuando todo baja,\nno bajar tambien.\n"
	
start:
	xor ax, ax		; reg init
	mov ds, ax 		; reg init
	mov es, ax 		; reg init	
	mov ss, ax		; stack init
	mov sp, 0x7c00

	pusha	 		; save state
	mov si, [stringBoladona] ; printstr uses si as parameter
	call printstr		 ; call it
	popa			 ; get previous state

	j done
	
;;; print string
;; @reg: ax, bx
;; @param: use si to print
printstr:
.start:

	lodsb 		; si -> al
	cmp al, 0
	je .done 	; if (end of string) return
	jmp .print 	; else print current char

.print:
	mov ah, 0xe 	; print char and move cursor foward
	mov bh, 0 	; page number
	mov bl, 0xf 	; white color
	int 10h 	; video interrupt
	
	jmp .start 
.done:
	ret

;;; read string
;; @reg: ax
;; @param: use di to read
readstr:
	.read:
		mov ah, 0 	; read keystroke
		int 16h		; keyboard interrupt

		cmp al, 0xd 	; compare al with 'enter'
		je .done
	
		stosb
		jmp .read	
	
	.done:
		mov al, 0 	; insert '\0'
		stosb
		ret

;;; read (verbosely) string from di and print char by char
;; @reg: ax, bx
;; @param: set di to destination string
readvstr:		
	.read:
		mov ah, 0 	; read keystroke
		int 16h		; keyboard interrupt

		cmp al, 0xd 	; compare al with 'enter'
		je .done
	
		stosb
		jmp .print
	
	.print:
		mov ah, 0xe 	; call number
		mov bh, 0	; page number
		mov bl, 0xf	; white color
		int 10h

		jmp .read

	.done:
		call println 	; print line
		mov al, 0 	; insert '\0'
		stosb
	
		ret 		; return
	
;;; print line (\n)
;; @reg: ax, bx
println:
	mov ah, 0xe ; char print
	mov bh, 0 ; page number
	mov bl, 0xf ; white color
	mov al, 13 ; vertical tab
	int 10h ; visual interrupt
	
	mov ah, 0xe ; char print
	mov bh, 0 ; page number
	mov bl, 0xf ; white color
	mov al, 10 ; backspace
	int 10h ; visual interrupt	

	ret

;;; string to integer -- int atoi(string*) 
;; @reg: ax, dx, bl, si
;; @param: use si as string
;; @ret: dl as int result
atoi:
	xor ax, ax 		; init
	mov dx, ax
.convert:	
	lodsb

	cmp al, '0'
	jb .done 		; character below '0'
	
	cmp al, '9'
	ja .done		; character above '9'

	sub al, '0'		; convert ascii to (0-9) int

	xchg dl, al 		; this swap is needed because mul

	mov bl, 10		; supose 12 from 123 string was computed, then 123 = (12*10) + 3
	mul bl			; prepare data for next unit digit
	add dl, al		; insert new digit into data
	
	jmp .convert
	
.done:
	ret

;;; integer to string -- string	to_string(int*)
;; @reg: ax, bl, sp, di
;; @param: use ax as number input
;; @ret: di as string output
tostring:
	
	push 0 			; push '\0' end of string

.convert:			; convert every digit of integer input into characters
	
	mov bl, 10		; let number = 123, then, after div, 12 will be al, and 3 will be ah
	div bl			; so, we need to push 3 onto stack and recursively convert (number/10) until the result be zero 
	add ah, '0'		; convert remainder to ascii...

	mov dl, ah		; (although the remainder is stored to ah, the stosb works with al)
	push dx			; ...and push it	

	cmp al, 0		; base case condition
	je .concat
	
	mov ah, 0		; the remainder was pushed onto stack, we dont need it anymore so AX = [3, 12] -> [0, 12]
	jmp .convert
	
.concat:			; concat every char of stack into a string
	
	pop ax			; get top of stack and pop it
	
	stosb			; store al at di
	
	cmp al, 0 		; if end of string
	je .done		; goto done
	jmp .concat
	
.done:
	ret
	
;;; inverts the string 
;; @reg: ax, si, di
;; @param: use si as the string input
;; @ret: di as the inverted string output
str_inverter:

	push 0 ; '\0' end of string

.stack:

	xor ax, ax ; ax = 0
	lodsb ; si -> al
	cmp al, 0
	je .inverter ; can't put the 0 into the stack
	push ax ; else put ax into the stack
	jmp .stack 

.inverter: 

	pop ax
	stosb ; al -> di
	cmp al, 0 ; end of the string
	je .done
	jmp .inverter

.done:

	ret

;;; Set lowercase letters to uppercase
;; @reg: al
;; @param: si, the source string
;; @ret: di, the output string
toUpper:
	lodsb 			; get a char of input string
	
	cmp al, 0		; check if its the end of string
	je .done		; in case of that, go to done

	; checking if the char is in [a,z] interval
	cmp al, 'a'
	jb .store

	cmp al, 'z'
	ja .store 	; if it's, process it

	jmp .convert	; else, convert to upperCase

	.convert:
		add al, -32 	; a - A is 32, so subtract 32 from al
		jmp .store 		; and store it
	.store:
		stosb
		jmp toUpper
	.done:
		ret

done:
	jmp $ 			; infinity jump

times 510-($-$$) db 0   ; fill unused boot sector with 0s
dw 0xAA55               ; The standard PC boot signature
