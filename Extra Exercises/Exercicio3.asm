[bits 16]
[org 0x7c00]

    jmp 0:kernel_start

gdt_start:

gdt_null:
    dd 0x0
    dd 0x0

gdt_code:
    dw 0xffff
    dw 0x0
    db 0x0
    db 10011010b
    db 11001111b
    db 0x0

gdt_data:
    dw 0xffff
    dw 0x0
    db 0x0
    db 10010010b
    db 11001111b
    db 0x0

gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start
    dd gdt_start

CODE_SEG equ gdt_code - gdt_start
DATA_SEG equ gdt_data - gdt_start

arrive16 db 'estamos 16 bits aehoo', 0
arrive32 db 'estamos 32 bits - protected mode!', 0


kernel_start:
    mov ax, 0
    mov ss, ax
    mov sp, 0xFFFC

    mov ax, 0
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax

    mov si, arrive16
    call print

    cli
    lgdt [gdt_descriptor]
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax
    jmp CODE_SEG:b32

[bits 32]

VIDEO_MEMORY equ 0xb8000
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
    mov ax, DATA_SEG
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax

    mov ebp, 0x2000
    mov esp, ebp

    mov ebx, arrive32
    call print32

    ;going back

    cli
    
    mov eax, cr0
    xor eax, 0x1	; Disable paging bit & disable 16-bit pmode.
    mov cr0, eax

    jmp 0x7c00:back_to_16

[bits 16]

backto16 db 'voltamos 16 bits - uau!',0

VIDEO_MEMORY_16 equ 0x0B85A0
COLOR_RED       equ 0x40

idt_real:
	dw 0x3ff		; 256 entries, 4b each = 1K
	dd 0	

back_to_16:
    mov ax, 0
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax    
    mov sp, 0xFFFC
    
    sti
    
    mov si, backto16

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
    jmp $

[SECTION signature start=0x7dfe]
dw 0AA55h
