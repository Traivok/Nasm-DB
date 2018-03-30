org 0x7e00
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
SEPARATOR 		 DB '-------------------------------------', 0
TEST_PROMPT		 DB 'hello world!',	0	
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

;;; CREATE OPTIONS
prompt1		DB ' Please type the name of the account owner '	, 0
prompt2		DB ' Please type the CPF of the account owner '		, 0
prompt3		DB ' Please type the Agency of the new account '	, 0
prompt4		DB ' Please type the Code of the new account '		, 0
;;; END OF CREATE OPTIONS

;;; OUTPUT MESSAGES
name_info	DB ' Client Name: '									, 0
cpf_info	DB ' Client CPF: '									, 0
ag_info		DB ' Client Agency: '								, 0
ac_info		DB ' Client Account: '								, 0
;;; END OF OUTPUT MESSAGES

;%macro 
	;TODO MACRO MEM-T0-MEM
;%endmacro

START:
    xor ax, ax  ; zera ax
    mov ds, ax  ; zera ds
    mov es, ax  ; zera es
    mov ss, ax	; zera stack
	mov sp, 0x7c00

    call INIT

    pusha	    ; save state

    ;; Print main menu routine.
    mainmenu:
        ;call clearScr       ; First things first, let's start with a fresh screen.
		mov si, SEPARATOR
		call printstr
		call println

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
        mov di, BUFF        ; readstr saves the keyboard input on the memory pointed by di
        call readvstr       ; reads user input
        mov si, BUFF        ; preparing for atoi
        call atoi           ; converts user input to integer number and saves at dl
        call clearScr       ; clears out the string after reading
        jmp redirect        ; jumps to redirection routine

    redirect:
        cmp dl, 1           ; compares the value read by the keyboard
        je CREATE           ; jumps to the CREATE section

        cmp dl, 2           ; compares the value read by the keyboard
        je SHOW             ; jumps to the SHOW section

        cmp dl, 3           ; compares the value read by the keyboard
        je EDIT             ; jumps to the EDIT section
        
        cmp dl, 4           ; compares the value read by the keyboard
        je DELETE           ; jumps to the DELETE section

        cmp dl, 5           ; compares the value read by the keyboard
        je LISTAGENCIES     ; jumps to the LISTAGENCIES section

        cmp dl, 6           ; compares the value read by the keyboard
        je LISTACCOUNTS     ; jumps to the LISTACCOUNTS section

        cmp dl, 7           ; compares the value read by the keyboard
        je EXIT             ; jumps to the EXIT section

		;;;;;;;;;;;;;;;;;;;;;;;DEV ONLY;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		cmp dl, 8
		call PRINT_ALL_ENTRIES
		jmp mainmenu
		;;;;;;;;;;;;;;;;;;;;;;;DEV ONLY;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


        jmp EXCEPTION       ; if no number from 1 - 7 was provided, jump to EXCEPTION and throw out an error.

    popa	    ; get previous state

	CREATE:
		.start:
			mov cx, [LENGTH]

		.nameread:
			call clearScr       ; fresh screen.

			mov si, prompt1     ; printstr uses si as parameter
			call printstr       ; call it
			call println        ; print a line break

			mov di, IO_NAME		; Points to IO_NAME memory
			call readvstr		; read string to that memory position
		.namestore:
			mov si, IO_NAME		; points the source to IO_NAME

			mov ax, 20			; sets ax to 20
			mul cx				; multiplies ax * cx, now [ax = (cx*20)] (since we need to navigate through the whole list of names)

			mov cx, 20 					; sets cx to 20 so we can iterate
			mov di, NAME					
			add di, ax					; points destination index to the start of the first empty name space 

		.storechar:
			lodsb					; reads a character from IO_NAME and saves at AL
			stosb					; picks the character at al and saves at [NAME + ax]
			loop .storechar	

		;;; debug
		;	mov si, TEST_PROMPT
		;	call printstr
		;	mov si, NAME
		;	call printstr
		;	mov di, BUFF
		;	call readvstr
				
		.cpfread:
			call clearScr       ; fresh screen.

			mov si, prompt2     ; printstr uses si as parameter
			call printstr       ; call it
			call println        ; print a line break

			mov di, BUFF		; Points to BUFF memory
			call readvstr		; read string to that memory position

			xor edx, edx		; puts zero on edx to make sure that (value at dl == value at edx)

			call atoi           ; converts user input to integer number and saves at dl

		.cpfstore:
			mov bx, cx
			mov dword [IO_CPF + bx], edx

		.agencyread:
			call clearScr       ; fresh screen.

			mov si, prompt3     ; printstr uses si as parameter
			call printstr       ; call it
			call println        ; print a line break

			mov di, BUFF		; Points to BUFF memory
			call readvstr		; read string to that memory position

			xor edx, edx

			call atoi           ; converts user input to integer number and saves at dl

		.agencystore:
			mov bx, cx
			mov word [COD_AG + bx], dx

		.accountread:
			call clearScr       ; fresh screen.

			mov si, prompt4     ; printstr uses si as parameter
			call printstr       ; call it
			call println        ; print a line break

			mov di, BUFF		; Points to BUFF memory
			call readvstr		; read string to that memory position

			xor edx, edx

			call atoi           ; converts user input to integer number and saves at dl
		.accountstore:
			mov bx, cx
			mov word [COD_AC + bx], dx

		.updateLen:
			inc word [LENGTH]
		
		.resetFlag:
			mov byte [GET_ALL_AGENCIES], 1

        jmp mainmenu

    SHOW:
        ; SHOW code goes here
        jmp END

    EDIT:
        ; EDIT code goes here
        jmp END

    DELETE:
        ; DELETE code goes here
        jmp END
    
    LISTAGENCIES:
        ; list agencies code goes here
        jmp END
    
    LISTACCOUNTS:
        ; list accounts code goes here
        jmp END

    EXIT:
        ; bye
        jmp END

    EXCEPTION:
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
;; @reg: 	CX, AX, BX, EDX
;; @param:	DX that contains the Account number
;; @ret:	CX will be the index of that element
;; @ret:	AX will be 0 if not found, else 1
;; @ret: 	IO FIELD will be setted with queried entry, if found
QUERY_AC:
	.start:
		mov cx, LENGTH							; sets the cx register with the current number of entries on the db
		mov bx, LENGTH							; sets the bx register with the current number of entries on the db		
	.while:
		cmp word [COD_AG + bx], bx				; compares the agency code at index cx to see if it matches the provided one
		je .found								; if yes, jump to found
		loop .while								; else, search again, one position back (since we're searching from the end)
	.notFound:
    		mov ax, 0								; resets ax
        	jmp .end
	.found:

		call COPY_TO_OUTPUT 							; move entry data to output
		mov ax, 1								; found flag

	.end:
		ret

;;; copy an entry of BD to IO cache
;; @reg: 	AX*, CX
;; @param:	CX index of some entry
COPY_TO_OUTPUT:
	.start:
		push edx						; DX will be used as an auxiliar variable
		push bx							; BX will be used as index register

		mov bx, cx 						; assigns bx = index of entry

		mov dx, word [COD_AC + bx] 					; move the found Account number to aux reg
		mov word [IO_AC], dx 					; and move it to IO memory
	
		mov edx, dword [CPF + bx]				; moves the found CPF on the position cx to the edx register (intermediary)
		mov dword [IO_CPF], edx					; moves the content of the edx register to the CPF memory buffer
		
		mov dx, word [COD_AG + bx]				; moves the found agency on the position cx to the dx register (intermediary)
		mov word [IO_AG], dx					; moves the content of the dx register to the agency memory buffer

		push cx									; save cx (index) on the stack
		mov ax, 20								; sets ax to 20
		mul cx									; multiplies ax * cx, now [ax = (cx*20)] (since we need to navigate through the whole list of names)
		mov cx, 20								; sets cx back to 20 to navigate on a single name (name buffer)

		.prepareToMove:
			mov di, IO_NAME
			push bx								; saves original index on stack
			mov bx, ax							; moves ax to bx since only bx can be index reg
			lea si, [NAME + bx]					; bx = ax wich contains (length * 20) - i, since NAME is the whole database entries
			pop bx								; goes back to original index value (cx)

		.movName:										 					
			movsb 
			loop .movName	
												; IO_NAME[i] = NAME[ (length * 20) - i ] 
		;	push bx								
		;	mov bx, ax							
		;	mov bl, byte [NAME + bx]			;
		;	pop bx								
		;	mov byte [IO_NAME + bx], bl			; bx = cx wich contains i, since IO_NAME only contains one entry
		;	dec ax								; decrements ax
		;	loop .movName						; loops back to fill up the name
		
		;mov si, IO_NAME
		;call printstr
		;mov di, BUFF
		;call readvstr
		
		pop cx									; index of entry
	
	.end:
		pop bx
		pop edx
		ret


;;; transform number into string and print it
;; @reg: AX SI DI
;; @param: AX output NUMBER to parse
;; @ret: BUFF string
;;;-----------------------------TO DO, MODIFY TOSTRING TO USE EAX AS INPUT---------------------;;;
PRINT_NUMBER_FIELD:	

	mov di, BUFF		; BUFF will be the string d
	call tostring		; transform AX to string
	
	mov si, BUFF		; use BUFF as printstr parameter
	call printstr		; call printstr
	call println		; print a new line

	ret	
	
;;; print an entry using IO memory
;; @warning: it will modify BUFF
PRINT_ENTRY:
	.start:

		push si				; save state
		push di
		push ax

		mov si, SEPARATOR 		; print an output separator
		call printstr
		call println

		mov si, name_info		; print name
		call printstr

		mov si, IO_NAME			; print name attribute 
		call printstr
		call println

		;; mov ax, [CPF] CPF NEED EAX

		mov si, ac_info			; print account
		call printstr

		mov ax, [IO_AC]			; print Account attribute
		call PRINT_NUMBER_FIELD

		mov si, ag_info			; print agency
		call printstr

		mov ax, [IO_AG]			; finally print Agency attribute
		call PRINT_NUMBER_FIELD
	
	.end:
		pop ax				; return to the previous state
		pop di
		pop si
		ret

;;; print all of Database
;; @warning: it will modify BUFF
PRINT_ALL_ENTRIES:
	.start:
		;mov word [LENGTH], 6 ;debug
		push cx				; save state
		mov cx, 0 			; get size of DB

	.while:
		call COPY_TO_OUTPUT 		; copy DB[CX] to output
		;call SAY_HI	;debug
		call PRINT_ENTRY		; print DB[CX] using output cache
		inc cx
		cmp cx, [LENGTH]		; check if all entries was printed
		jb .while			; while ( |cx| < [LENGTH] )

	.end:
		pop cx				; load previous cx
		ret

;;; Debugging purposes
SAY_HI:
	mov si, TEST_PROMPT       ; printstr uses si as parameter
    call printstr       ; call it
	ret


END:
	jmp $

