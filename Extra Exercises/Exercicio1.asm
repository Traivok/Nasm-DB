
org 0x7c00
jmp 0x0000:start

string db "AP2 de IHS", 0

start:
	
	xor ax, ax
	mov ds, ax
	mov es, ax
	mov di, 0x100
	
	mov word[di], printstr
	mov word[di + 2], 0

	mov bx, string
	mov cx, 10
	int 40h
	jmp end

printstr:

		.start:
			mov si, bx

		.while:
			lodsb 		; si -> al
			mov ah, 0xe ; print char and move cursor foward
			mov bh, 0 	; page number
			mov bl, 0xf ; white color
			int 10h 	; video interrupt

			loop .while

		.done:
			iret

end:
	jmp $

times 510-($-$$) db 0   ; fill remainder of boot sector with 0s
dw 0xAA55               ; The standard PC boot signature