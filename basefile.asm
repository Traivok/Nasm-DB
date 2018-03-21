org 0x7c00
jmp 0x0000:START

;;; BEGIN OF ARRAY SECTION
CAPACITY EQU 6
NAME 	 TIMES CAPACITY * 20 DB 0 	; array of 20 characters by item
CPF 	 TIMES CAPACITY 	 DD 0	; all above will use double word integer
COD_AG 	 TIMES CAPACITY 	 DW 0
COD_AC 	 TIMES CAPACITY 	 DW 1
LENGTH 	 					 DW 0	; the current of size of DB starts empty
;;; END OF ARRAY SECTION

;;; BEGIN OF FLAGS SECTION
GET_ALL_AGENCIES 				DB 0  ; when an update/create/remove of agency number, be called, set to 1
AGENCIES 	TIMES CAPACITY 		DW 1  ; all unique agencies of this DB
;;; END OF FLAGS SECTION

;;; BEGIN OF IO SECTION
IO_NAME TIMES 21 DB 0  ; for storing client's username
IO_CPF 			 DD 0  ; for storing client's cpf
IO_AG			 DW 0  ; for storing client's bank agency
IO_AC 			 DW 0  ; for storing client's bank account
BUFF 	TIMES 21 DW 0  ; general purpose keyboard buffer
;;; END OF IO SECTION

;;; MAIN MENU OPTIONS
title       DB ' Welcome to the SafeMoney Bank System '         , 0
subtitle    DB ' Please select your operation below '           , 0
option1     DB ' 1. Create new account '                        , 0
option2     DB ' 2. Show existing account '                     , 0
option3     DB ' 3. Edit existing account '                     , 0
option4     DB ' 4. Delete existing account '                   , 0
option5     DB ' 5. List SafeMoney agencies '                   , 0
option6     DB ' 6. List SafeMoney accounts '                   , 0
option7     DB ' 7. Exit SafeMoney Bank '                       , 0
invopt      DB ' Invalid command provided. Please try again. '  , 0
;;; END OF MAIN MENU OPTIONS


;%macro 
	;TODO MACRO MEM-T0-MEM
;%endmacro

start:
    xor ax, ax  ; zera ax
    mov ds, ax  ; zera ds
    mov es, ax  ; zera es
    mov ss, ax	; zera stack
	mov sp, 0x7c00

    call INIT

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
        jmp END

    show:
        ; show code goes here
        jmp END

    edit:
        ; edit code goes here
        jmp END

    delete:
        ; delete code goes here
        jmp END
    
    listagencies:
        ; list agencies code goes here
        jmp END
    
    listaccounts:
        ; list accounts code goes here
        jmp END

    exit:
        ; bye
        jmp END

    exception:
        mov si, invopt      ; preparing for printstr
        call printstr       ; calling
        call println        ; print a line break
        jmp mainmenu        ; back to main menu so user can select another option

    

jmp END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;IO FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DATABASE FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; initialize booleans, flags and stuff
INIT:	
	mov word [LENGTH], 0 ; reset size of DB
	mov word [GET_ALL_AGENCIES], 0
	ret

;;; return the index of the last entry that matches with the provider Account
;; @reg: 	CX, AX, EBX
;; @param:	BX that contains the Account number
;; @ret:	CX will be the index of that element
;; @ret:	AX will be 0 if not found, else 1
QUERY_AC:
	.start:
		mov cx, LENGTH							; sets the cx register with the current number of entries on the db
		mov ax, 0								; resets ax
	.while:
		cmp word [COD_AG + cx], bx				; compares the agency code at index cx to see if it matches the provided one
		je found								; if yes, jump to found
		loop .while								; else, search again, one position back (since we're searching from the end)
	.found:
		mov word [IO_AC], bx					; moves the provided (now also found!) account number to the account number buffer
		
		mov ebx, dword [CPF + cx]				; moves the found CPF on the position cx to the ebx register (intermediary)
		mov dword [IO_CPF], ebx					; moves the content of the ebx register to the CPF memory buffer
		
		mov bx, word [COD_AG + cx]				; moves the found acency on the position cx to the bx register (intermediary)
		mov word [IO_AG], bx					; moves the content of the bx register to the agency memory buffer

		push cx									; save cx (index) on the stack
		mov ax, 20								; sets ax to 20
		mul cx									; multiplies ax * cx, now [ax = (cx*20)] (since we need to navigate through the whole list of names)
		mov cx, 20								; sets cx back to 20 to navigate on a single name (name buffer)

		.movName:								; IO_NAME[i] = NAME[ (length * 20) - i ] 
			mov bl, byte [NAME + ax]			; ax contains (length * 20) - i, since NAME is the whole database entries
			mov byte [IO_NAME + cx], bl			; cx contains i, since IO_NAME only contains one entry
			dec ax								; decrements ax
			loop .movName						; loops back to fill up the name

		pop cx									; index of entry
		mov ax, 1								; found flag

	.end:
		ret

END:
	jmp $