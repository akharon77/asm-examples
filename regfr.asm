.model tiny
.code
.286
locals @@
org 100h

TOP_OFFSET          equ 0h
MIDDLE_OFFSET       equ 3h
BOTTOM_OFFSET       equ 6h

TRUE                equ 0FFh

CNT_REGS            equ 12d
REG_LEN             equ 2d
FRAME_WIDTH         equ 10d
FRAME_HEIGHT        equ 14d
BUFFER_SIZE         equ FRAME_WIDTH * FRAME_HEIGHT

;------------------------------------------
; Exit
;------------------------------------------
; In:   AL = exit code
; Out:  N/A
; Dstr: N/A
;------------------------------------------
Exit macro
    nop
    mov ah, 4ch
    int 21h
    nop

endm
;------------------------------------------

SetIntr macro num
    nop
    LoadESIntrTable
    mov bx, num&h * 4d

    mov ax, es:[bx]
    mov cs:[old_&num&_ofs], ax
    mov ax, es:[bx + 2]
    mov cs:[old_&num&_seg], ax

    mov es:[bx], offset New&num
    mov ax, cs
    mov es:[bx + 2], ax
    nop
endm

;------------------------------------------
; ExitResident
;------------------------------------------
; In:   AL = exit code
; Out:  N/A
; Dstr: N/A
;------------------------------------------
ExitResident macro
    nop
    mov ah, 31h
    mov dx, offset program_end
    shr dx, 4               ; paragraph size is 16 bytes
    inc dx
    int 21h
    nop

endm
;------------------------------------------

;------------------------------------------
; FillScreen
;------------------------------------------
; In:   AH:AL = attr:sym 
; Out:  None
; Dstr: None
;------------------------------------------
FillScreen macro
    nop
    mov di, 0
    mov cx, 80d * 25d
    rep stosw
    nop

endm
;------------------------------------------

;------------------------------------------
; LoadESVideo
;------------------------------------------
; In:   None
; Out:  ES = 0b800h
; Dstr: BX
;------------------------------------------
LoadESVideo macro
    nop
    mov bx, 0b800h
    mov es, bx
    nop
        
endm
;------------------------------------------

;------------------------------------------
; LoadESIntrTable
;------------------------------------------
; In:   None
; Out:  ES = 0
; Dstr: BX
;------------------------------------------
LoadESIntrTable macro
    nop
    xor bx, bx
    mov es, bx
    nop
        
endm
;------------------------------------------

;------------------------------------------
; Mul80
;------------------------------------------
; In:   AX
; Out:  AX = 80 * AX
; Dstr: None
;------------------------------------------
Mul80 macro
    nop
    push bx         ; сохраним значение bx
    
    mov bx, ax
    shl ax, 6       ; x * 80 = x * 2^6 + x * 2^4 = x << 6 + x << 4
    shl bx, 4
    add ax, bx

    pop bx          ; забрали значение ax с al
    nop

endm
;------------------------------------------

start:

    cli
    SetIntr 09
    SetIntr 08
    sti

    ; call New08

    ; mov di, 80d * 2d * 5d + 40d * 2d
    
; next:
    ; in al, 60h
    ; mov ah, 0Ch
    ; mov es:[di], ax

    ; cmp al, 1
    ; jne Next
    
    ; mov si, CMD_TAIL_OFFSET
    ; call CalcPos

    ; push ax
    ; mov si, offset Preset0
    ; call MakeBorder

    ; pop bx
    ; add bh, 2
    ; add bl, 2
    ; mov si, CMD_TAIL_OFFSET
    ; call StrOut

    mov al, 0
    ExitResident
    ; Exit

New09 proc
    pusha
    pushf
    push es

    LoadESVideo

    mov ah, 4eh

    in al, 60h
    cmp al, 2
    jne @@no_upd_status

    not cs:[frame_status]

@@no_upd_status:

    in al, 61h                  ; мигаем bit 7, 1000000b = 80h
    or al, 80h
    out 61h, al
    and al, not 80h
    out 61h, al

    mov al, 20h                 ; подаем 20h на порт 20h
    out 20h, al

    pop es
    popf
    popa

    db 0eah                     ; jmp far
old_09_ofs dw 0
old_09_seg dw 0

    iret

endp

New08 proc
    pusha
    pushf
    push ds es

    call SaveRegs

    cmp cs:[frame_status], TRUE
    jne @@no_frame

    push ds
    mov bx, cs
    mov ds, bx

    mov bx, cs
    mov es, bx

    mov di, offset draw_buffer
    mov ax, 0
    mov bh, FRAME_WIDTH
    mov bl, FRAME_HEIGHT
    mov si, offset preset0
    call DrawBorderToBuffer

    mov di, offset draw_buffer + FRAME_WIDTH * 2h + 2h
    call PrintRegsNameToBuffer

    mov cx, CNT_REGS
    mov di, offset draw_buffer + FRAME_WIDTH * 2h + 2h + REG_LEN + 2h
    mov si, offset saved_regs
@@next:
    push cx
    mov bx, cs:[si]
    add si, 2h
    push si
    call PrintReg
    pop si
    pop cx
    add di, FRAME_WIDTH * 2h
    loop @@next

    ; call CmpDrawAndVideo

    LoadESVideo
    mov si, offset draw_buffer
    mov di, 0h
    call OutBuffer

    pop ds

@@no_frame:

    mov al, 20h
    out 20h, al

    pop es ds
    popf
    popa

    db 0eah                     ; jmp far
old_08_ofs dw 0
old_08_seg dw 0

    iret

endp

;------------------------------------------
; CmpDrawAndVideo
;------------------------------------------
; In:   DS:SI
; Out:
; Dstr:
;------------------------------------------
; CmpDrawAndVideo proc
;     mov cx, BUFFER_SIZE
;     add si, cx
;     cmp ds:[si + cx], es:[di]
;     sub si, cx
;     ret
; 
; endp
;------------------------------------------

;------------------------------------------
; OutBuffer
;------------------------------------------
; In:   DS:SI
; Out:
; Dstr:
;------------------------------------------
OutBuffer proc
    cld
    mov cx, FRAME_HEIGHT
@@next_x:
    push cx
    mov cx, FRAME_WIDTH
@@next_y:
    lodsw
    stosw
    loop @@next_y
    pop cx
    add di, 80d * 2d
    sub di, 2d * FRAME_WIDTH
    loop @@next_x
    ret

endp
;------------------------------------------

;------------------------------------------
; LoadBorderChars
;------------------------------------------
; In:   
; Out:  
; Dstr: 
;------------------------------------------
LoadBorderChars macro

        nop
        push si
        lodsw
        xchg ax, bx
        xchg bh, bl

        lodsb
        xchg al, dh
        pop si
        nop

endm
;------------------------------------------

;------------------------------------------
; PrintRegsNameToBuffer 
;------------------------------------------
; In:   ES:DI = buffer address
; Out: 
; Dstr:
;------------------------------------------
PrintRegsNameToBuffer proc
    push ds
    mov cx, CNT_REGS

    mov bx, cs
    mov ds, bx

    mov si, offset reg_names

; перекидываем из ds:si в es:di

@@next:
    mov ah, 7h

    push cx
    mov cx, REG_LEN
    @@next_chr_reg:
        lodsb
        stosw
    loop @@next_chr_reg
    pop cx

    add di, FRAME_WIDTH * 2d - REG_LEN * 2d

    loop @@next
    pop ds

    ret

endp
;------------------------------------------

;------------------------------------------
; SaveRegs
;------------------------------------------
; In:
; Out: 
; Dstr: CX, DI
;------------------------------------------
SaveRegs proc
    push ss es cs ds bp sp di si dx cx bx ax
    mov cx, CNT_REGS
    mov di, offset saved_regs
@@next:
    pop cs:[di]
    add di, 2h
    loop @@next
    ret
endp
;------------------------------------------

;------------------------------------------
; PrintReg 
;------------------------------------------
; In:   ES:DI = buffer address
;       BX    = reg
; Out: 
; Dstr: AX, CX, DX, SI
;------------------------------------------
PrintReg proc
    add di, 2h * 4h
    mov cx, 4h
    std

@@next:
    mov ax, bx
    and ax, 0Fh
    mov si, offset hexAlph
    add si, ax
    mov al, cs:[si]
    mov ah, 7h
    stosw
    shr bx, 4
    loop @@next

    ret
endp
;------------------------------------------

;------------------------------------------
; DrawBorderToBuffer
;------------------------------------------
; In:   BH:BL = width:height
;       DS:SI = preset address
;       ES:DI = buffer address
; Out: 
; Dstr: AX, BX, DX
;------------------------------------------
DrawBorderToBuffer proc
    mov cx, bx          ; количество строк 
    and cx, 0FFh
    mov dl, bh          ; ширина рамочки

@@next:
    cmp cl, bl
    je  @@first

    cmp cl, 1h
    je @@last

@@middle:                   ;*
    add si, MIDDLE_OFFSET   ;*
    LoadBorderChars         ;*
    sub si, MIDDLE_OFFSET   ;*
    jmp @@line              ;*
                            ;*
@@first:                    ;* выбор нужного набора символов mid left right
    LoadBorderChars         ;*
    jmp @@line              ;*
                            ;*
@@last:                     ;*
    add si, BOTTOM_OFFSET   ;*
    LoadBorderChars         ;*
    sub si, BOTTOM_OFFSET   ;*

@@line:
    push cx
    call MakeLine
    pop cx

    loop @@next

    ret
endp
;------------------------------------------

;------------------------------------------
; MakeLine
;------------------------------------------
; In:   ES:DI    = where
;       BH:BL:DH = mid, left, right chars
;       DL       = lenght
; Out:
; Dstr: DI, AX, CX
;------------------------------------------
MakeLine proc

    cld
    mov ah, 0Fh
    mov al, bl
    stosw

    mov al, bh
    mov cx, dx
    and cx, 0FFh
    sub cx, 2
    rep stosw

    mov al, dh
    stosw

    ret
endp
;------------------------------------------

;------------------------------------------
; CoordToOffset
;------------------------------------------
; In:   AH:AL = x:y
; Out:  AX    = (80d * x + y) * 2
; Dstr: None
;------------------------------------------
CoordToOffset proc

    push bx         ; сохраним значение bx
    push ax         ; сохраним значение y, которое в al
    shr ax, 8       ; оставим в ax значение x
    
    Mul80

    pop bx          ; забрали значение ax с al
    and bx, 0FFh    ; обнулили старшие 8 бит
    add ax, bx
    shl ax, 1       ; домножили итог на 2

    pop bx          ; восстановили bx

    ret
endp
;------------------------------------------

.data

preset0 db 0cdh, 0c9h, 0bbh, " ", 0bah, 0bah, 0cdh, 0c8h, 0bch
preset1 db "-## ##-##"

frame_status db 0

reg_names db "ax", "bx", "cx", "dx", "si", "di", "sp", "bp", "ds", "cs", "es", "ss"
saved_regs dw CNT_REGS dup (0)

hexAlph db "0123456789ABCDEF"

save_buffer dw BUFFER_SIZE dup (0)
draw_buffer dw BUFFER_SIZE dup (0)

program_end:

end start

