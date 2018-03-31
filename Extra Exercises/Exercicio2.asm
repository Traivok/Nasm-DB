org 0x7e00
jmp 0x0000:START

;;; BEGIN OF ARRAY SECTION
CAPACITY EQU 6

NAME_LEN EQU 20
CPF_LEN  EQU 11
AG_LEN   EQU 05
AC_LEN   EQU 06
	
NAME 	 TIMES CAPACITY * (NAME_LEN + 1)	DB 0 	; array of 20 characters by item
CPF 	 TIMES CAPACITY * (CPF_LEN  + 1)	DB 0	; all above will use double word integer
COD_AG 	 TIMES CAPACITY * (AG_LEN   + 1)	DB 0
COD_AC 	 TIMES CAPACITY * (AC_LEN   + 1)	DB 0
LENGTH 	 				DW 0	; the current of size of DB starts empty

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
title       DB ' Welcome to the SafeMoney Bank System '         , 0
subtitle    DB ' Please select your operation below '           , 0
option1     DB ' 1. Create new account '                        , 0
option2     DB ' 2. Show existing accounts '                     , 0
invopt      DB ' Invalid command provided. Please try again. '  , 0
;;; END OF MAIN MENU OPTIONS

;;; CREATE OPTIONS
prompt1		DB ' Please type the name of the account owner '	, 0
prompt2		DB ' Please type the CPF of the account owner '		, 0
prompt3		DB ' Please type the Agency of the new account '	, 0
prompt4		DB ' Please type the Code of the new account '		, 0
;;; END OF CREATE OPTIONS

;;; OUTPUT MESSAGES
name_info	 DB ' Client Name: '								, 0
cpf_info	 DB ' Client CPF: '									, 0
ag_info		 DB ' Client Agency: '								, 0
ac_info	 	 DB ' Client Account: '								, 0
agency_query_msg DB ' Insert an Agency Code: '					, 0
;;; END OF OUTPUT MESSAGES

START:
    xor ax, ax  ; zera ax
    mov ds, ax  ; zera ds
    mov es, ax  ; zera es
    mov ss, ax	; zera stack
	mov sp, 0x7c00

    call INIT

	;; putting CREATE and LIST_ALL as interrupts
	mov di, 0x100				; di = 256, so interrupt is 0x40 (40h) since 0x40*4 = 64*4 = 256
	mov word[di], CREATE		;[256] = CREATE
	mov word[di + 2], 0			

	mov di, 0x200					; di = 512, so interrupt is 0x80 (80h) since 0x80*4 = 128*4 = 512
	mov word[di], PRINT_ALL_ENTRIES ;[512] = PRINT_ALL_ENTRIES
	mov word[di + 2], 0

	;;; now the interrupts 40h and 80h are set

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
        je .new_entry           ; jumps to the CREATE section

        cmp dl, 2           ; compares the value read by the keyboard
        je .list_all             ; jumps to the PRINT_ALL_ENTRIES section

        jmp EXCEPTION       ; if no number from 1 - 7 was provided, jump to EXCEPTION and throw out an error.

    popa	    ; get previous state

		.new_entry:
			int 40h
			jmp mainmenu
		
		.list_all:
			int 80h
			jmp mainmenu

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
		.end:
        	iret ;interrupt return

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


;;; copy an entry of BD to IO cache
;; @reg: 	CX
;; @param:	CX index of some entry
COPY_TO_OUTPUT:
	.start:
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

;;; print all of Database
;; @warning: it will modify BUFF
PRINT_ALL_ENTRIES:
	.start:
		push cx				; save state
		mov cx, 0 			; get size of DB

	.while:
		call COPY_TO_OUTPUT 		; copy DB[CX] to output
		call PRINT_ENTRY		; print DB[CX] using output cache
		push di
		call readvstr
		pop di
		inc cx
		cmp cx, [LENGTH]		; check if all entries was printed
		jb .while			; while ( |cx| < [LENGTH] )

	.end:
		pop cx				; load previous cx
		iret 				; interrupt return
	
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

END:
	jmp $

