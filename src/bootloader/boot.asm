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
    ; cannot write to ds and es directly
    mov ax, 0
    mov ds, ax
    mov es, ax

    ; setup stack that will grow down from loaded memory location as not to overwrite our program
    mov ss, ax
    mov sp, 0x7C00

    ; some bioses may start at 07c0:0000 instead of 0000:7c00
    push es
    push word .after
    retf

.after:
    ; read something from floppy disk (bios should set dl to drive number)
    mov [ebr_drive_number], dl
    
    mov si, loading_message
    call puts

    ; reading drive parameters
    push es
    mov ah, 08h
    int 13h
    jc floppy_error
    pop es

    and cl, 0x3F ; remove top 2 bits
    xor ch, ch
    mov [bdb_sectors_per_track], cx

    inc dh
    mov [bdb_heads], dh

    ; lba/root directory = reserved + fats * sectors per fat
    mov ax, [bdb_sectors_per_fat] 
    mov bl, [bdb_fat_count]
    xor bh, bh
    mul bx ; ax = (fats * sectors per fat)
    add ax, [bdb_reserved_sectors] ; ax = lba of root directory
    push ax

    ; computing size of root directory = (32 * num of entries) / bytes per sector
    mov ax, [bdb_dir_entries_count]
    shl ax, 5 ; ax *= 32
    xor dx, dx
    div word [bdb_bytes_per_sector] ; num of sectors needed to be read

    test dx, dx ; if dx != 0, add 1
    jz .root_dir_after
    inc ax ; remained != 0, add 1

.root_dir_after:
    mov cl, al ; cl = num of sectors to read
    pop ax ; lba of root dir
    mov dl, [ebr_drive_number]
    mov bx, buffer
    call read_sectors

    ; search for kernel.bin
    xor bx, bx
    mov di, buffer

.search_kernel:
    mov si, file_kernel_bin
    mov cx, 11 ; length of file_kernel_bin name
    push di
    repe cmpsb
    pop di
    je .found_kernel

    add di, 32
    inc bx
    cmp bx, [bdb_dir_entries_count]
    jl .search_kernel

    jmp kernel_not_found_error

.found_kernel:
    mov ax, [di + 26] ; first logical cluster field. offset = 26
    mov [kernel_cluster], ax

    ; load fat from disk into memory
    mov ax, [bdb_reserved_sectors]
    mov bx, buffer
    mov cl, [bdb_sectors_per_fat]
    mov dl, [ebr_drive_number]
    call read_sectors

    ; read kernel and process fat chain
    mov bx, KERNEL_LOAD_SEGMENT
    mov es, bx
    mov bx, KERNEL_LOAD_OFFSET

.load_kernel_loop:
    ; reading next sector
    mov ax, [kernel_cluster]
    ; first cluster = (kernel_cluster - 2) * sectors per cluster + start sector
    ; start sector = reserved + fats + root dir size = 1 + 18 + 134 = 33
    add ax, 31 ; change this hardcoded value later

    mov cl, 1
    mov dl, [ebr_drive_number]
    call read_sectors

    add bx, [bdb_bytes_per_sector] ; will overflow if kernel.bin file is >64kb

    ; calculate next cluster location
    mov ax, [kernel_cluster]
    mov cx, 3
    mul cx
    mov cx, 2
    div cx ; ax = index of entry in fat, dx = cluster % 2

    mov si, buffer
    add si, ax
    mov ax, [ds:si]

    or dx, dx
    jz .even

.odd:
    shr ax, 4
    jmp .next_cluster

.even:
    and ax, 0x0FFF

.next_cluster:
    cmp ax, 0x0FF8 ; end of chain
    jae .read_done

    mov [kernel_cluster], ax
    jmp .load_kernel_loop

.read_done:
    ; jump to kernel
    mov dl, [ebr_drive_number] ; boot device in dl
    mov ax,  KERNEL_LOAD_SEGMENT ; set segment registers
    mov ds, ax
    mov es, ax
    jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET

    jmp wait_key_and_reboot

    cli ; disable interrupts to ensure cpu can't get out of halt state
    hlt

floppy_error:
    mov si, failed_read_message
    call puts
    jmp wait_key_and_reboot

kernel_not_found_error:
    mov si, kernel_not_found_message
    call puts
    jmp wait_key_and_reboot

wait_key_and_reboot:
    mov ah, 0
    int 16h ; waits for keypress
    jmp 0FFFFh:0 ; reboot (jump to beginning of bios)

.halt:
    cli ; disable interrupts to ensure cpu can't get out of halt state
    hlt


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

loading_message: db 'Loading...', ENDLINE, 0
failed_read_message: db 'Read from disk failed', ENDLINE, 0 
kernel_not_found_message: db 'kernel.bin file not found', ENDLINE, 0 
file_kernel_bin: db 'KERNEL  BIN'
kernel_cluster: dw 0

KERNEL_LOAD_SEGMENT: equ 0x2000
KERNEL_LOAD_OFFSET: equ 0

times 510-($-$$) db 0
dw 0AA55h

buffer: