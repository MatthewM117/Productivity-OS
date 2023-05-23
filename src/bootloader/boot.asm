org 0x7C00
bits 16

%define ENDLINE 0x0D, 0x0A

; fat12 header
jmp short start
nop

bdb_oem:                   db 'MSWIN4.1'
bdb_bytes_per_sector:      dw 512
bdb_sectors_per_cluster:   db 1
bdb_reserved_sectors:      dw 1
bdb_fat_count:             db 2
bdb_dir_entries_count:     dw 0E0h
bdb_total_sectors:         dw 2880 ; 2880 * 512 = 1.44mb
bdb_media_descriptor_type: db 0F0h ; F0 = 3.5" floppy disk
bdb_sectors_per_fat:       dw 9
bdb_sectors_per_track:     dw 18
bdb_heads:                 dw 2
bdb_hidden_sectors:        dd 0
bdb_large_sector_count:    dd 0

; extended boot sector
ebr_drive_number:          db 0
                           db 0 ; reserved byte
ebr_signature:             db 29h
ebr_volume_id:             db 12h, 34h, 56h, 78h ; serial number. can change to anything
ebr_volume_label:          db 'MATTHEW  OS' ; can be anything, but make sure its padded with spaces. 11 bytes
ebr_system_id:             db 'FAT12   ' ; 8 bytes, padded with spaces

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

    ; read something from floppy disk (bios should set dl to drive number)
    mov [ebr_drive_number], dl
    mov ax, 1 ; lba = 1 (2nd sector from disk)
    mov cl, 1 ; sectors to read
    mov bx, 0x7E00 ; data should be after the bootloader
    call read_sectors

    ; print message to screen
    mov si, new_message
    call puts

    cli ; disable interrupts to ensure cpu can't get out of halt state
    hlt

floppy_error:
    mov si, failed_read_message
    call puts
    jmp wait_key_and_reboot

wait_key_and_reboot:
    mov ah, 0
    int 16h ; waits for keypress
    jmp 0FFFFh:0 ; reboot (jump to beginning of bios)

.halt:
    cli ; disable interrupts to ensure cpu can't get out of halt state
    hlt

; disk routines

; converts lba address to chs address
; Inputs:
;   - ax: lba address
; Returns: 
;   - cx [bits 0-5]: sector number
;   - cx [bits 6-15]: cylinder
;   - dh: head
lba_to_chs:
    push ax
    push dx

    xor dx, dx ; dx = 0
    ; ax = lba / sectors per track
    ; dx = lba % sectors per track
    div word [bdb_sectors_per_track]
    inc dx ; dx = (lba % sectors per track + 1) = sector
    mov cx, dx ; cx = sector

    xor dx, dx
    ; ax = (lba / sectors per track) / heads = cylinder
    ; dx = (lba / sectors per track) % heads = head
    div word [bdb_heads]

    mov dh, dl ; dh = head
    mov ch, al ; ch = cylinder (lower 8 bits)
    shl ah, 6
    or cl, ah ; put upper 2 bits of cylinder in cl

    pop ax
    mov dl, al
    pop ax
    ret

; reads sectors from disk
; real floppy disks can be unreliable. retry at least 3 times
; Inputs:
;   - ax: lba address
;   - cl: num of sectors to read (max = 128)
;   - dl: drive number
;   - es:bx: mem address to store read data
read_sectors:
    ; save the registers that will be modified
    push ax
    push bx
    push cx
    push dx
    push di

    push cx ; temp save cl
    call lba_to_chs
    pop ax ; al = num sectors to read
    mov ah, 02h
    mov di, 3 ; num of times to retry

.retry:
    pusha ; push all registers to the stack
    stc ; set carry flag incase bios doesn't
    int 13h ; carry flag cleared. end loop
    jnc .done ; if carry not set
    popa

    ; if failed
    popa
    call disk_reset
    dec di ; decrement di
    test di, di
    jnz .retry ; if di != 0

.fail:
    jmp floppy_error

.done:
    popa
    ; restore modified registers
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; resets disk controller
; Inputs:
;   - d1: drive number
disk_reset:
    pusha
    mov ah, 0
    stc
    int 13h
    jc floppy_error
    popa
    ret

new_message: db 'Hello, world!', ENDLINE, 0
failed_read_message: db 'Read from disk failed', ENDLINE, 0 

times 510-($-$$) db 0
dw 0AA55h