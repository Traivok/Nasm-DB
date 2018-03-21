org 0x7e00        ; endereço de memória em que o programa será carregado
jmp 0x0000:start  ; far jump - seta cs para 0

; reservando espaços para as variáveis de leitura
username times 21 db 0  ;for storing client's username
cpf times 12 db 0       ;for storing client's cpf
agency times 5 db 0     ;for storing client's bank agency
acc times 7 db 0        ;for storing client's bank account
buf times 5 db 0        ;general purpose keyboard buffer

; frases do menu principal
title db ' Welcome to the SafeMoney Bank System ', 0
subtitle db ' Please select your operation below ', 0
option1 db ' 1. Create new account ', 0
option2 db ' 2. Show existing account ', 0
option3 db ' 3. Edit existing account ', 0
option4 db ' 4. Delete existing account ', 0
option5 db ' 5. List SafeMoney agencies ', 0
option6 db ' 6. List SafeMoney accounts ', 0
option7 db ' 7. Exit SafeMoney Bank ', 0
invopt db ' Invalid command provided. Please try again. ', 0

start:
    xor ax, ax  ; zera ax
    mov ds, ax  ; zera ds
    mov es, ax  ; zera es
    mov ss, ax	; zera stack
	mov sp, 0x7c00

    pusha	    ; save state

    ;; Print main menu routine.
    mainmenu:
        call clearScr       ; First things first, let's start with a fresh screen.

        mov si, title       ; printstr uses si as parameter
        call printstr       ; call it
        call println        ; print a line break
       
        mov si, subtitle    ; preparing for printstr
        call printstr       ; calling
        call println        ; print a line break
        call println        ; print a line break

        mov si, option1     ; preparing for printstr
        call printstr       ; calling
        call println        ; print a line break

        mov si, option2     ; preparing for printstr
        call printstr       ; calling
        call println        ; print a line break

        mov si, option3     ; preparing for printstr
        call printstr       ; calling
        call println        ; print a line break

        mov si, option4     ; preparing for printstr
        call printstr       ; calling
        call println        ; print a line break

        mov si, option5     ; preparing for printstr
        call printstr       ; calling
        call println        ; print a line break

        mov si, option6     ; preparing for printstr
        call printstr       ; calling
        call println        ; print a line break

        mov si, option7     ; preparing for printstr
        call printstr       ; calling
        call println        ; print a line break

        jmp readfromuser
    
    ;; Routine for reading desired option from user.
    readfromuser:
        mov di, buf         ; readstr saves the keyboard input on the memory pointed by di
        call readvstr       ; reads user input
        mov si, buf         ; preparing for atoi
        call atoi           ; converts user input to integer number and saves at dl
        call clearScr       ; clears out the string after reading
        jmp redirect        ; jumps to redirection routine

    redirect:
        cmp dl, 1           ; compares the value read by the keyboard
        je create           ; jumps to the create section

        cmp dl, 2           ; compares the value read by the keyboard
        je show             ; jumps to the show section

        cmp dl, 3           ; compares the value read by the keyboard
        je edit             ; jumps to the edit section
        
        cmp dl, 4           ; compares the value read by the keyboard
        je delete           ; jumps to the delete section

        cmp dl, 5           ; compares the value read by the keyboard
        je listagencies     ; jumps to the listagencies section

        cmp dl, 6           ; compares the value read by the keyboard
        je listaccounts     ; jumps to the listaccounts section

        cmp dl, 7           ; compares the value read by the keyboard
        je exit             ; jumps to the exit section

        jmp exception       ; if no number from 1 - 7 was provided, jump to exception and throw out an error.

    popa	    ; get previous state

    create:
        ; create code goes here
        jmp done

    show:
        ; show code goes here
        jmp done

    edit:
        ; edit code goes here
        jmp done

    delete:
        ; delete code goes here
        jmp done
    
    listagencies:
        ; list agencies code goes here
        jmp done
    
    listaccounts:
        ; list accounts code goes here
        jmp done

    exit:
        ; bye
        jmp done

    exception:
        mov si, invopt      ; preparing for printstr
        call printstr       ; calling
        call println        ; print a line break
        jmp mainmenu        ; back to main menu so user can select another option

    

jmp done













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

;;; Clears out the entire screen of text, so when printing something new, you can start fresh.
;; @desc: al has the info of the desired video mode. The available ones are [00h, 03h, 13h]. When int 10h is called and ah is set to 0, the video mode is set.
;; @reg: ah, al, bl
;; @param: none
;; @ret: none

clearScr:
    pusha
    mov ah, 0x00    ; ah = 0, sets video mode when int 10h is called
    mov al, 0x03    ; text mode. 80x25. 16 colors. 8 pages. 
    int 0x10        ; set video mode to the one specified at al
    popa
    ret

done:
	jmp $ 			; infinity jump
