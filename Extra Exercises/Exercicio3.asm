[bits 16]
[org 0x7c00]

    jmp 0:kernel_start

gdt_start:

gdt_null:
    dd 0x0
    dd 0x0

gdt_code: ; 0x8
    dw 0xffff
    dw 0x0
    db 0x0
    db 10011010b
    db 11001111b
    db 0x0
gdt_data: ; 0x10
    dw 0xffff
    dw 0x0
    db 0x0
    db 10010010b
    db 11001111b
    db 0x0
gdt_to_real: ; 0x18
    dw 0xffff
    dw 0x0
    db 0x0
    db 10011010b
    db 00001111b
    db 0x0  
gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start
    dd gdt_start

CODE_SEG equ gdt_code - gdt_start
DATA_SEG equ gdt_data - gdt_start
BACK_REAL equ gdt_to_real - gdt_start

presentation db 13, 10, '  Who are we?', 13, 10, '    gchf - Gabi Correa', 13, 10, '    hscs - Heitor Carvalho', 13, 10, '    jraf - Jose Ricardo', 13, 10, '    kams - Kevin Andrews', 13, 10, '    rlf4 - Ricardo Fagundes', 13, 10, 13, 10, 13, 10, 0
arrive16 db '  Estamos 16 bits aehoo!', 13, 10, 13, 10, 13, 10, 0
arrive32 db '[Entramos em 32 bits - protected mode!]', 0


kernel_start:
    call clearScr
    mov ax, 0
    mov ss, ax
    mov sp, 0xFFFC

    mov ax, 0
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax

    mov si, presentation
    call print

    mov si, arrive16
    call print

    cli
    lgdt [gdt_descriptor]
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax
    jmp 0x8:b32

[bits 32]

VIDEO_MEMORY equ 0x0B8708
WHITE_ON_BLACK equ 0x0f

print32:
    pusha
    mov edx, VIDEO_MEMORY
.loop:
    mov al, [ebx]
    mov ah, WHITE_ON_BLACK
    cmp al, 0
    je .done
    mov [edx], ax
    add ebx, 1
    add edx, 2
    jmp .loop
.done:
    popa
    ret

b32:
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax

    mov ebp, 0x2000
    mov esp, ebp

    mov ebx, arrive32
    call print32

    ; start the trip of going back
    cli

    jmp 0x18:pre_back_real

pre_back_real:
    mov eax, cr0
    xor eax, 0x1	; Disable paging bit & disable 16-bit pmode.
    mov cr0, eax

    sti

    [BITS 16]

	xor ax, ax
	mov ds, ax
	mov es, ax
	mov gs, ax
	mov fs, ax
	mov ss, ax
    
    jmp 0:back_to_16


backto16 db 13,10,'  Voltamos aos 16 bits :(', 0

back_to_16:     
    mov sp, 0x8000
    mov bp, sp

    ;.clearScr:
    ;    mov ah, 0x00    ; ah = 0, sets video mode when int 10h is called
    ;    mov al, 0x03    ; text mode. 80x25. 16 colors. 8 pages. 
    ;    int 0x10        ; set video mode to the one specified at al

    mov si, backto16
    call print

 .exit:
    jmp $

print:
    pusha
    mov ah, 14
    mov bh, 0
.loop:
    lodsb
    cmp al, 0
    je .done
    int 0x10
    jmp .loop
.done:
    popa
    ret

clearScr:
    pusha
    mov ah, 0x00    ; ah = 0, sets video mode when int 10h is called
    mov al, 0x03    ; text mode. 80x25. 16 colors. 8 pages. 
    int 0x10        ; set video mode to the one specified at al
    popa
    ret

times 510-($-$$) db 0   ; fill remainder of boot sector with 0s
dw 0xAA55               ; The standard PC boot signature
