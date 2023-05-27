org 0x0
bits 16

%define ENDLINE 0x0D, 0x0A

start:
    ; print message to screen
    mov si, new_message
    call puts

.halt:
    cli
    hlt

;----------
; prints a string to the screen
; Inputs:
;    - ds:si: points to string
puts:
    push si
    push ax
    push bx

.loop:
    lodsb
    or al, al ; check if next char is null
    jz .done
    
    mov ah, 0x0e
    mov bh, 0
    int 0x10 ; bios interrupt

    jmp .loop


.done:
    pop bx
    pop ax
    pop si
    ret

new_message: db 'Hello, world from kernel!', ENDLINE, 0
