org 0x7C00
bits 16

%define ENDLINE 0x0D, 0x0A

start:
    jmp main

;----------
; prints a string to the screen
; Inputs:
;    - ds:si: points to string
puts:
    push si
    push ax

.loop:
    lodsb
    or al, al ; check if next char is null
    jz .done
    
    mov ah, 0x0e
    mov bh, 0
    int 0x10 ; bios interrupt

    jmp .loop


.done:
    pop ax
    pop si
    ret

main:
    ; cannot write to ds and es directly
    mov ax, 0
    mov ds, ax
    mov es, ax

    ; setup stack that will grow down from loaded memory location as not to overwrite our program
    mov ss, ax
    mov sp, 0x7C00

    ; print message to screen
    mov si, new_message
    call puts

    hlt

.halt:
    jmp .halt

new_message: db 'Hello, world!', ENDLINE, 0

times 510-($-$$) db 0
dw 0AA55h