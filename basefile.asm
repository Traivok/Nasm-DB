org 0x7e00
jmp 0x0000:START

;;; BEGIN OF ARRAY SECTION
CAPACITY EQU 6

NAME_LEN EQU 21 	;20 characters + '\0'
CPF_LEN  EQU 12		;11 characters + '\0'
AG_LEN   EQU 06		;05 characters + '\0'
AC_LEN   EQU 07		;06 characters + '\0'
	
NAME 	 TIMES CAPACITY * (NAME_LEN + 1)	DB 0 	; array of 20 characters by item
CPF 	 TIMES CAPACITY * (CPF_LEN  + 1)	DB 0	; all above will use double word integer
COD_AG 	 TIMES CAPACITY * (AG_LEN   + 1)	DB 0
COD_AC 	 TIMES CAPACITY * (AC_LEN   + 1)	DB 0
LENGTH 	 				DW 0	; the current of size of DB starts empty
GOIDA					DW 0

AGENCIES TIMES CAPACITY * (AG_LEN + 1) 	DB 0  ; all unique agencies of this DB
AGENCIES_ARRAY_LEN			DW 0  ; lenght of AGENCIES array
;;; END OF ARRAY SECTION

;;; BEGIN OF FLAGS SECTION
GET_ALL_AGENCIES 			DB 1  ; when an update/create/remove of agency number, be called, set to 1
CUR_AGENCY_PRESENT			DB 0 
;;; END OF FLAGS SECTION

;;; BEGIN OF IO SECTION
IO_NAME  TIMES NAME_LEN + 1 DB 0  ; for storing client's username
IO_CPF 	 TIMES CPF_LEN  + 1 DB 0  ; for storing client's cpf
IO_AG	 TIMES AG_LEN   + 1 DB 0  ; for storing client's bank agency
IO_AC 	 TIMES AC_LEN   + 1 DB 0  ; for storing client's bank account
BUFF 	 TIMES 32     	    DB 0  ; general purpose keyboard buffer
AG_QUERY TIMES 32	    DB 0  ; agency number query goes here
SEPARATOR 		    DB '-------------------------------------', 0
TEST_PROMPT		    DB 'hello world!',	0	
;;; END OF IO SECTION

;;; MAIN MENU OPTIONS
title       DB ' Welcome to the SafeMoney Bank System '        			, 0
subtitle    DB ' Please select your operation below '           		, 0
option1     DB ' 1. Create new account '                    	    	, 0
option2     DB ' 2. Show existing account '             	    	    , 0
option3     DB ' 3. Edit existing account '           	       			, 0
option4     DB ' 4. Delete existing account '                   		, 0
option5     DB ' 5. List SafeMoney agencies '               		    , 0
option6     DB ' 6. List SafeMoney accounts '              	    		, 0
option7     DB ' 7. Exit SafeMoney Bank '                       		, 0
invopt      DB ' Invalid command provided. Please try again. '	  		, 0
;;; END OF MAIN MENU OPTIONS

;;; CREATE OPTIONS
prompt1		DB ' Please type the name of the account owner '		, 0
prompt2		DB ' Please type the CPF of the account owner '			, 0
prompt3		DB ' Please type the Agency of the new account '		, 0
prompt4		DB ' Please type the Code of the new account '			, 0
;;; END OF CREATE OPTIONS

;;; DELETE MESSAGES
del 		DB ' Please type the Code of the account you want to remove ', 0
error		DB ' Error. Invalid Code.'									 , 0
;;; END OF DELETE MESSAGES

;;; EDIT MESSAGES
edit_prompt1	DB ' Please type the Code of the account you want to edit '	, 0
edit_prompt2	DB ' The Code you have entered is not registered '			, 0
edit_prompt3	DB ' Would you like to change the name of the account owner? ', 13, 10, ' Press 1 to yes, 0 to no ', 0
edit_prompt4	DB ' Would you like to change the CPF of the account owner? ', 13, 10, ' Press 1 to yes, 0 to no ', 0
edit_prompt5	DB ' Would you like to change the Agency of the account? ', 13, 10, ' Press 1 to yes, 0 to no ', 0
edit_prompt6	DB ' CURRENT NAME: ', 0
edit_prompt7	DB ' CURRENT CPF: ', 0
edit_prompt8	DB ' CURRENT AGENCY: ', 0
;;; END OF EDIT MESSAGES

;;; OUTPUT MESSAGES
name_info	 DB ' Client Name: '								, 0
cpf_info	 DB ' Client CPF: '									, 0
ag_info		 DB ' Client Agency: '								, 0
ac_info	 	 DB ' Client Account: '								, 0
agency_query_msg DB ' Insert an Agency Code: '					, 0
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
        call clearScr       ; First things first, let's start with a fresh screen.
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

		push cx
		.nameread:
			call clearScr       ; fresh screen.

			mov si, prompt1     ; printstr uses si as parameter
			call printstr       ; call it
			call println        ; print a line break

			mov di, IO_NAME		; Points to IO_NAME memory
			call readvstr		; read string to that memory position
		.namestore:
			mov si, IO_NAME		; points the source to IO_NAME

			mov ax, NAME_LEN	; sets ax to 20
			mul cx				; multiplies ax * cx, now [ax = (cx*20)] (since we need to navigate through the whole list of names)

			mov cx, NAME_LEN 	; sets cx to 20 so we can iterate
			mov di, NAME					
			add di, ax			; points destination index to the start of the first empty name space 
			
			call STORESTRING
		pop cx

		push cx
		.cpfread:
			call clearScr       ; fresh screen.

			mov si, prompt2     ; printstr uses si as parameter
			call printstr       ; call it
			call println        ; print a line break

			mov di, IO_CPF		; Points to BUFF memory
			call readvstr		; read string to that memory position
		.cpfstore:
			mov si, IO_CPF		; points the source to IO_CPF

			mov ax, CPF_LEN		; sets ax to 11
			mul cx				; multiplies ax * cx, now [ax = (cx*11)] (since we need to navigate through the whole list of names)

			mov cx, CPF_LEN 	; sets cx to 11 so we can iterate
			mov di, CPF					
			add di, ax			; points destination index to the start of the first empty cpf space 
			
			call STORESTRING
		pop cx

		push cx
		.agencyread:
			call clearScr       ; fresh screen.

			mov si, prompt3     ; printstr uses si as parameter
			call printstr       ; call it
			call println        ; print a line break

			mov di, IO_AG		; Points to BUFF memory
			call readvstr		; read string to that memory position

		.agencystore:
			mov si, IO_AG		; points the source to IO_AG

			mov ax, AG_LEN		; sets ax to 11
			mul cx				; multiplies ax * cx, now [ax = (cx*11)] (since we need to navigate through the whole list of names)

			mov cx, AG_LEN 		; sets cx to 11 so we can iterate
			mov di, COD_AG					
			add di, ax			; points destination index to the start of the first empty cpf space 
			
			call STORESTRING
		pop cx

		push cx
		.accountread:
			call clearScr       ; fresh screen.

			mov si, prompt4     ; printstr uses si as parameter
			call printstr       ; call it
			call println        ; print a line break

			mov di, IO_AC		; Points to BUFF memory
			call readvstr		; read string to that memory position

		.accountstore:
			mov si, IO_AC		; points the source to IO_AC

			mov ax, AC_LEN		; sets ax to 11
			mul cx				; multiplies ax * cx, now [ax = (cx*11)] (since we need to navigate through the whole list of names)

			mov cx, AC_LEN 		; sets cx to 11 so we can iterate
			mov di, COD_AC					
			add di, ax			; points destination index to the start of the first empty cpf space 
			
			call STORESTRING
		pop cx

		.updateLen:
			inc word [LENGTH]
		
		.resetFlag:
			mov byte [GET_ALL_AGENCIES], 1

        jmp mainmenu

    SHOW:
        ; SHOW code goes here
        jmp END

    EDIT:

    	mov si, edit_prompt1 ; Please type the Code of the account you want to edit
    	call printstr
    	call println

	   	call FIND_AC ; dx has the account

	   	cmp dx, 0 ; (dx = -1) -> invalid code
	   	jge EDIT_NAME ; continue if valid

	   	call clearScr  ; otherwise clears the screen and prints the invalid code warning
	   	mov si, edit_prompt2
	   	call printstr
	   	call println

	   	jmp EDIT ; back to the beginning

	EDIT_NAME:

		; first print the current name of the owner
		call clearScr   ; fresh screen
		mov si, edit_prompt6
		call printstr

		mov ax, NAME_LEN ; ax = 21
	   	mul dx ; ax = dx*ax

		mov si, NAME
		add si, ax
		call printstr
		call println

		; then ask if the user wants to change it and reads the answer
		mov si, edit_prompt3
		call printstr
		call println

		.read:
			mov ah, 0 	; read keystroke
			int 16h		; keyboard interrupt

			cmp al, '0' 		; al has the answer
			je EDIT_CPF
			cmp al, '1'
			jne .read 		; keeps reading if not 0 or 1

		; the actual change of the name
		call clearScr   ; fresh screen
		mov si, prompt1	
		call printstr
		call println

	   	mov ax, NAME_LEN ; ax = 20
	   	mul dx ; ax = dx*ax
	   	mov di, NAME
	   	add di, ax ; points destination index to the right place to be edited

	   	call readvstr

	EDIT_CPF:

		; first print the current cpf of the owner
		call clearScr   ; fresh screen
		mov si, edit_prompt7
		call printstr

		mov ax, CPF_LEN ; ax = 11
	   	mul dx ; ax = dx*ax
		mov si, CPF
		add si, ax

		mov cx, CPF_LEN
		.print:
			lodsb 		; si -> al
			mov ah, 0xe 	; print char and move cursor foward
			mov bh, 0 	; page number
			mov bl, 0xf 	; white color
			int 10h 	; video interrupt
			loop .print
		call println

		; then ask if the user wants to change it and reads the answer
		mov si, edit_prompt4
		call printstr
		call println

		.read:
			mov ah, 0 	; read keystroke
			int 16h		; keyboard interrupt
		cmp al, '0' 		; al has the answer
		je EDIT_AG
		cmp al, '1'
		jne .read; keeps reading if not 0 or 1

		;the actual change of the CPF
		call clearScr   ; fresh screen
		mov si, prompt2
		call printstr
		call println

	   	mov ax, CPF_LEN ; ax = 11
	   	mul dx ; ax = dx*ax
	   	mov di, CPF
	   	add di, ax ; points destination index to the right place to be edited

	   	call readvstr

	EDIT_AG:

		; first print the current agency of the account
		call clearScr   ; fresh screen
		mov si, edit_prompt8
		call printstr

		mov ax, AG_LEN ; ax = 5+1
	   	mul dx ; ax = dx*ax
	   	mov si, COD_AG
		add si, ax

		mov cx, AG_LEN
		.print:
			lodsb 		; si -> al
			mov ah, 0xe 	; print char and move cursor foward
			mov bh, 0 	; page number
			mov bl, 0xf 	; white color
			int 10h 	; video interrupt
			loop .print
		call println

		; then ask if the user wants to change it and reads the answer
		mov si, edit_prompt5
		call printstr
		call println

		.read:
			mov ah, 0 	; read keystroke
			int 16h		; keyboard interrupt
		cmp al, '0' 		; al has the answer
		je .END
		cmp al, '1'
		jne .read		; keeps reading if not 0 or 1

		;the actual change of the agency
		call clearScr   ; fresh screen
		mov si, prompt1	
		call printstr
		call println

	   	mov ax, AG_LEN ; ax = 5
	   	mul dx ; ax = dx*ax
	   	mov di, COD_AG
	   	add di, ax ; points destination index to the right place to be edited

	   	call readvstr

	.END:
        jmp mainmenu

    DELETE:
        .start:

        	mov si, del
        	call printstr
        	call println

            call FIND_AC
            	
           	xor ax, ax
           	xor bx, bx
           	xor cx, cx
            
           	mov cx , dx
            	
           	cmp cx, -1
           	je .msgerror
           
      	.CPF:
        ;bx cadastros-1
       	;cx cadastro a ser deletado MENOS 1
       	;dx comeca em 1
        	mov dx, 1
            mov bx, [LENGTH] 		;bx recebe o total de cadastros
            dec bx
            sub bx, cx				;bx = bx -cx, agora bx recebe a quantidade de cadastros que serão deslocados 1 indice à esquerda
            mov ax, CPF_LEN			;ax recebe o tamanho de caracteres de um CPF
            mul bx					;ax=bx*ax , ax recebe a quantidade de caracteres que serão deslocados 1 posição à esquerda
            mov bx, ax				; bx recebe o valor de ax
            
            mov ax, 0				; mover 0 para ax

            mov di, CPF				;di aponta para o inicio do vetor de CPFs
            mov ax, CPF_LEN			;ax recebe o tamanho de caracteres de um CPF 
            mul cx					;ax=ax*cx , ax recebe a quantidade de caracteres que não serão alterados de lugar
            add di, ax				;di agora aponta para o primeiro caractere a ser alterado
            mov si, CPF+CPF_LEN		;si aponta para o inicio do segundo CPF
            add si, ax				;si agora aponta para o primeiro caractere a ser movido para onde di esta apontando
            cmp bx,0				;comparar bx com 0
            je .done				;caso igual pular para a função .done
            jmp .del1				;caso nao, ir para del1
             
        .del1:
            lodsb 				;carrega em al o caractere apontado por si
            stosb 				;carrega na posicao apontada por di o caractere contido em al 
            inc dx 				;incrementa dx
            cmp dx, bx			;compara dx com bc
            je .AG 				;caso iguais, a remoção do CPF esta concluida
            jmp .del1			;caso nao, continuamos a remoção
        .AG:
        	mov dx, 1
        	mov bx, [LENGTH]
        	dec bx
        	sub bx, cx
        	mov ax, AG_LEN
        	mul bx
        	mov bx, ax

        	mov ax, 0

	        mov di, COD_AG
	        mov ax, AG_LEN 
	        mul cx
	        add di, ax
	        mov si, COD_AG+AG_LEN
	        add si, ax
	        cmp bx,0
	        je .done
	        jmp .del2
	     
        .del2:
            lodsb
            stosb
            inc dx 
            cmp dx, bx
            je .CC
        	jmp .del2
        .CC:
         	mov dx, 1
            mov bx, [LENGTH]
            dec bx
            sub bx, cx
            mov ax, AC_LEN
            mul bx
            mov bx, ax

            mov ax, 0

            mov di, COD_AC
            mov ax, AC_LEN 
            mul cx
            add di, ax
            mov si, COD_AC+AC_LEN
            add si, ax
            cmp bx,0
            je .done
            jmp .del3
         
        .del3:
            lodsb
            stosb
            inc dx 
            cmp dx, bx
            je .NAME
            jmp .del3
        .NAME:
        	mov dx, 1
            mov bx, [LENGTH]
            dec bx
            sub bx, cx
            mov ax, NAME_LEN
            mul bx
            mov bx, ax

            mov ax, 0

            mov di, NAME
            mov ax, NAME_LEN
            mul cx
            add di, ax
            mov si, NAME+NAME_LEN
            add si, ax
            cmp bx,0
            je .done
            jmp .del4
         
        .del4:
            lodsb
            stosb
            inc dx 
            cmp dx, bx
            je .done
            jmp .del4

        .done:
        	mov cx, [LENGTH]
        	dec cx
        	mov [LENGTH], cx
        	jmp mainmenu
    	.msgerror:
    		mov si, error		
    		call printstr
    		call println
    		jmp .start
          
        jmp mainmenu
    
    LISTAGENCIES:
        ; list agencies code goes here
	call LIST_ALL_AGENCIES
	jmp mainmenu
    
    LISTACCOUNTS:
	; list accounts code goes here
	mov si, agency_query_msg
	call printstr
	
	mov di, AG_QUERY
	call readvstr

	mov si, AG_QUERY
	call LIST_BY_AGENCY
	
	jmp mainmenu

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
;; @reg: 	CX
;; @param:	CX index of some entry
COPY_TO_OUTPUT:
	.start:
		;push edx						; DX will be used as an auxiliar variable
		;push bx							; BX will be used as index register
		;mov bx, cx 						; assigns bx = index of entry

		;mov dx, word [COD_AC + bx] 					; move the found Account number to aux reg
		;mov word [IO_AC], dx 					; and move it to IO memory
	
		;mov edx, dword [CPF + bx]				; moves the found CPF on the position cx to the edx register (intermediary)
		;mov dword [IO_CPF], edx					; moves the content of the edx register to the CPF memory buffer
		
		;mov dx, word [COD_AG + bx]				; moves the found agency on the position cx to the dx register (intermediary)
		;mov word [IO_AG], dx					; moves the content of the dx register to the agency memory buffer

		pusha

		.prepareToMoveName:
			push cx									; save cx (index) on the stack
			mov ax, NAME_LEN								; sets ax to 20
			mul cx									; multiplies ax * cx, now [ax = (cx*20)] (since we need to navigate through the whole list of names)
			mov cx, NAME_LEN								; sets cx back to 20 to navigate on a single name (name buffer)
			mov di, IO_NAME
			push bx								; saves original index on stack
			mov bx, ax							; moves ax to bx since only bx can be index reg
			lea si, [NAME + bx]					; bx = ax wich contains (length * 20) - i, since NAME is the whole database entries
			pop bx								; goes back to original index value (cx)
		.movName:										 					
			movsb 
			loop .movName	
			pop cx									; cx = index of entry
			call .appendEndOFString			

		.prepareToMoveCPF:
			push cx									; save cx (index) on the stack
			mov ax, CPF_LEN								; sets ax to 20
			mul cx									; multiplies ax * cx, now [ax = (cx*20)] (since we need to navigate through the whole list of CPFs)
			mov cx, CPF_LEN								; sets cx back to 20 to navigate on a single CPF (CPF buffer)
			mov di, IO_CPF
			push bx								; saves original index on stack
			mov bx, ax							; moves ax to bx since only bx can be index reg
			lea si, [CPF + bx]					; bx = ax wich contains (length * 20) - i, since CPF is the whole database entries
			pop bx								; goes back to original index value (cx)
		.movCPF:										 					
			movsb 
			loop .movCPF	
			pop cx									; cx = index of entry
			call .appendEndOFString			
	
		.prepareToMoveAC:
			push cx									; save cx (index) on the stack
			mov ax, AC_LEN								; sets ax to 20
			mul cx									; multiplies ax * cx, now [ax = (cx*20)] (since we need to navigate through the whole list of ACs)
			mov cx, AC_LEN								; sets cx back to 20 to navigate on a single AC (AC buffer)
			mov di, IO_AC
			push bx								; saves original index on stack
			mov bx, ax							; moves ax to bx since only bx can be index reg
			lea si, [COD_AC + bx]					; bx = ax wich contains (length * 20) - i, since AC is the whole database entries
			pop bx								; goes back to original index value (cx)
		.movAC:										 					
			movsb 
			loop .movAC	
			pop cx
			call .appendEndOFString			
	
		.prepareToMoveAG:
			push cx									; save cx (index) on the stAGk
			mov ax, AG_LEN								; sets ax to 20
			mul cx									; multiplies ax * cx, now [ax = (cx*20)] (since we need to navigate through the whole list of AGs)
			mov cx, AG_LEN								; sets cx bAGk to 20 to navigate on a single AG (AG buffer)
			mov di, IO_AG
			push bx								; saves original index on stAGk
			mov bx, ax							; moves ax to bx since only bx can be index reg
			lea si, [COD_AG + bx]					; bx = ax wich contains (length * 20) - i, since AG is the whole database entries
			pop bx								; goes bAGk to original index value (cx)
		.movAG:										 					
			movsb 
			loop .movAG	
			pop cx
			call .appendEndOFString			
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
	
	
	.end:
		;pop bx
				;pop edx
		popa
		ret
	.appendEndOFString:
		push ax
		mov ah, 0
		stosb
		pop ax
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

		mov si, name_info		; print name info for user
		call printstr

		mov si, IO_NAME			; print name attribute 
		call printstr
		call println

		mov si, cpf_info		; print cpf  info for user
		call printstr

		mov si, IO_CPF			;print CPF attribute
		call printstr
		call println

		mov si, ac_info			; print account  info for user
		call printstr

		mov si, IO_AC			; print Account attribute
		call printstr
		call println

		mov si, ag_info			; print agency info for user
		call printstr

		mov si, IO_AG			; finally print Agency attribute
		call printstr
		call println
	
	.end:
		pop ax				; return to the previous state
		pop di
		pop si
		ret

FIND_AC:
	
	mov di, BUFF
	call readvstr
	xor bx, bx
	xor ax, ax
	xor dx, dx
	xor cx, cx
	.find:
		mov si, COD_AC
		add si, bx
		lodsb
		mov cl, al
		mov si, BUFF
		add si, dx
		lodsb
		cmp cl, al
		je .equall
		jmp .continue
		.equall:
				inc ch
				jmp .continue
		.continue:
				inc bx
				inc dx
				cmp dx, 6
				je  .avaliable			
				jmp .find
		.avaliable:
				cmp ch, 6
				je .encontrado
				jmp .zeragem
		.zeragem:
				xor ax, ax
				mov ax, [GOIDA]
        		inc ax
        		mov [GOIDA], ax
				xor ax, ax
				xor dx, dx
				mov ax, [LENGTH]
				mov dx, [GOIDA]
				cmp ax, dx
			
				je .naoencontrado
				mov dx, 0
				mov ch, 0
				jmp .find
		.encontrado:
				xor dx, dx
				mov dx, [GOIDA]
				xor ax, ax
				mov [GOIDA], ax
				ret

		.naoencontrado:
				xor dx, dx
				mov [GOIDA], dx
				mov dx, -1
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
		push di
		call readvstr
		pop di
		inc cx
		cmp cx, [LENGTH]		; check if all entries was printed
		jb .while			; while ( |cx| < [LENGTH] )

	.end:
		pop cx				; load previous cx
		ret

;;; print all accounts given some agency
;; @param: SI Agency number 
LIST_BY_AGENCY:
	.start:
		pusha
		mov cx, 0
		
	.findAddress:
		
		mov ax, AG_LEN		; set ax to reserved size of ag length
		mul cx			; multiplies it by index
		mov bx, ax		; base mode only works with BX

		lea di, [COD_AG + bx]	; load effective address of entry	
	
	.cmpWithSource:
		push cx			; save state, cx will be used as counter in REPE
		push si			; si will be change in REPE
	
		mov cx, AG_LEN		; check AG_LEN amount of characters

		.while:
			cmpsb		   	; cmp DI[i] with SI[i]
			jne .checkEquality 	; if DI[i] != SI[i], break this loop 
			loop .while
	
		.checkEquality:
	
			mov bx, cx		; cx is used as DB index 

			pop si			; return to previous state
			pop cx

			cmp bx, 0		; check if all chars are equal
			jne .checkRange		; if not, check if db reach to its end

	.found:				; else, print account founded
		pusha
		call COPY_TO_OUTPUT	; copy that entry to OUTPUT cache
		call PRINT_ENTRY	; and print that	
		mov di, BUFF
		call readstr
		popa
	
	.checkRange:
		inc cx
		cmp cx, [LENGTH]

		je .end			; if db reached to its end, return

		jmp .findAddress	; else, test another entry
	
	.end:
		popa
		ret
;;; check if source string number is present on AGENCIES cacche
;; @param: SI, the agency number
FILTER_DUP_AGENCIES:	
	.start:
		pusha
		
		mov byte [CUR_AGENCY_PRESENT], 0
	
		mov bx, 0		; base mode only works with BX
		mov dx, 0		; index ageny
	
	.findAddress:

		cmp dx, [AGENCIES_ARRAY_LEN]
		je .end
		inc dx
	
		lea di, [AGENCIES + bx]	; load effective address of entry	
		add bx, AG_LEN
	
		push si			; and si
		mov cx, AG_LEN		; get str max size 
	
	.comparison:
		cmpsb			; compare characcters 
		jne .posComp
		loop .comparison	; else, continue until the end of current string
	
		mov byte [CUR_AGENCY_PRESENT],  1 ; the SI string is present

		pop si		; remove si of stack
		popa		; restore state
		ret		; and return
	
	.posComp:	
		pop si
		jne .findAddress		
	
	.end:
		popa
		ret	

;;; Append source Agency string into rear of AGENCIES
;; @param: SI as agency string
APPEND_INTO_AGENCIES:
	.start:
		pusha

		mov ax, AG_LEN
		mul word [AGENCIES_ARRAY_LEN]
		mov bx, ax
		lea di, [AGENCIES + bx]

		mov cx, AG_LEN
		inc word [AGENCIES_ARRAY_LEN]

	.insert:
		movsb
		loop .insert
	
	.end:
		popa
		ret

;;; list all different agencies from database
LIST_ALL_AGENCIES:
	.start:
		pusha
		cmp byte [GET_ALL_AGENCIES], 0
		je .print		

		mov cx, 0
		mov byte [AGENCIES_ARRAY_LEN], 0
	
	.findAddress:
		mov ax, AG_LEN		; set ax to reserved size of ag length
		mul cx			; multiplies it by index
		mov bx, ax		; base mode only works with BX

		lea si, [COD_AG + bx]	; load effective address of entry	

		call FILTER_DUP_AGENCIES
		cmp byte [CUR_AGENCY_PRESENT], 1
		je .notAppend
	
		call APPEND_INTO_AGENCIES
	
	.notAppend:
	
		inc cx
		cmp cx, [LENGTH]
		je .print
		jmp .findAddress
	
	.print:
		mov cx, 0

	.while:
		cmp cx, word [AGENCIES_ARRAY_LEN]
		je .end

		mov ax, AG_LEN
		mul cx
		mov bx, ax

		lea si, [AGENCIES+BX]
		call printstr
		call println

		mov si, SEPARATOR
		call printstr
		call println

		mov di, BUFF
		call readstr	
	
		inc cx
		jmp .while
	
	.end:
		popa
		ret
	
;;; Store string from si at memory position pointed by di
;; @reg: cx, where cx is string size
STORESTRING:
	.start:
		lodsb				; reads a character from SI and saves at AL
		stosb				; picks the character at AL and saves at DI
		loop .start
	.end:
		mov al, 0
		stosb
		ret

;;; Debugging purposes
SAY_HI:
	mov si, TEST_PROMPT       ; printstr uses si as parameter
    call printstr       ; call it
	ret


END:
	; exits the program
	mov ax,0x5307
	mov bx,0x0001
	mov cx,0x0003
	int 0x15
	jmp $