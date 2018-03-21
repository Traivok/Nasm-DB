org 0x7c00
jmp 0x0000:START

;;; BEGIN OF ARRAY SECTION
CAPACITY equ 6
NAME 	 times CAPACITY * 20 DB 0 	; array of 20 characters by item
CPF 	 times CAPACITY 	 DD 0	; all above will use double word integer
COD_AG 	 times CAPACITY 	 DW 0
COD_AC 	 times CAPACITY 	 DW 1
LENGTH 	 					 DW 0	; the current of size of DB starts empty
;;; END OF ARRAY SECTION

;;; BEGIN OF FLAGS SECTION
GET_ALL_AGENCIES 				DB 0  ; when an update/create/remove of agency number, be called, set to 1
AGENCIES 	times CAPACITY 		DW 1  ; all unique agencies of this DB
;;; END OF FLAGS SECTION

;;; BEGIN OF IO SECTION
IO_NAME times 21 DB 0  ; for storing client's username
IO_CPF 			 DD 0  ; for storing client's cpf
IO_AG			 DW 0  ; for storing client's bank agency
IO_AC 			 DW 0  ; for storing client's bank account
BUFF 	times 21 DW 0  ; general purpose keyboard buffer
;;; END OF IO SECTION

;%macro 
	;TODO MACRO MEM-T0-MEM
;%endmacro

START:
	xor ax, ax
	mov ds, ax
	mov es, ax

	call INIT

	jmp END

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
	.while:
		cmp word [COD_AG + cx], bx				; compares the agency code at index cx to see if it matches the provided one
		je .found								; if yes, jump to found
		loop .while								; else, search again, one position back (since we're searching from the end)
    .notFound:
    	mov ax, 0								; resets ax
        jmp .end
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

times 510-($-$$) db 0   ; fill remainder of boot sector with 0s
dw 0xAA55               ; The standard PC boot signature