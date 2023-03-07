.model tiny
.code
.286
locals @@
org 100h

TOP_OFFSET          equ 0
MIDDLE_OFFSET       equ 3
BOTTOM_OFFSET       equ 6

TRUE                equ 0FFh

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

New09 proc
    pusha
    pushf
    push es

    LoadESVideo

    mov bx, 80d * 5d * 2d + 40d * 2d    ; вывод на экран
    mov ah, 4eh

    in al, 60h
    cmp al, 2
    jne @@no_upd_status

    not cs:[frame_status]

@@no_upd_status:

    mov es:[bx], ax
    mov al, cs:[frame_status]
    mov es:[bx + 2], ax

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

    cmp cs:[frame_status], TRUE
    jne @@no_frame

    push ds
    mov di, cs
    mov ds, di

    LoadESVideo
    mov ax, 0
    mov bx, 0A0Ah
    mov si, offset preset0
    call MakeBorder

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
; MakeBorder
;------------------------------------------
; Assumes: ES = video memory address
; In:   AH:AL = left  top    x:y
;       BH:BL = width:height
;       SI    = preset address
; Out:  
; Dstr: AX, BX, DX
;------------------------------------------
MakeBorder proc
    call CoordToOffset  ; ax = offset

    mov di, ax          ; указали на начало рамочки
    mov cx, bx          ; количество строк 
    and cx, 0FFh
    mov dl, bh          ; ширина рамочки

@@next:
    push di             ; запомнили первый столбец текущей строки
    cmp cl, bl
    je  @@first

    cmp cl, 1
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

    pop di              ; вернулись в первый столбец
    add di, 80d * 2d    ; перешли на следующую строчку

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
; StrOut
;------------------------------------------
; In:   DS:SI = from
;       BH:BL = x:y
; Out:
; Dstr: AX, DX
;------------------------------------------
StrOut proc
    mov ax, bx
    call CoordToOffset
    mov di, ax
    push di

    mov dl, 0Fh         ; базовый атрибут
    cld

    jmp @@next

@@next_str:
    pop di
    add di, 80d * 2d
    push di

@@next:
    lodsb

    cmp al, "%"
    je @@attr

    cmp al, "$"
    je @@end

    cmp al, "\"
    je @@next_str

    jmp @@out_sym

@@attr:
    push bx
    call InputNum
    mov dl, bl
    pop bx
    jmp @@next

@@out_sym:
    stosb
    mov al, dl
    stosb
    jmp @@next

@@end:
    pop di
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

;------------------------------------------
; InputNum
;------------------------------------------
; In:   DS:SI = from 
; Out:  BX    = num
; Dstr: AX, SI, DX
;------------------------------------------
InputNum proc
    mov ah, 0
    mov bx, 0
    
@@next:
    lodsb

    cmp al, "%"
    je @@end

    cmp al, "9"
    jbe @@digit

    cmp al, "F"
    jbe @@char

@@digit:
    sub ax, "0"
    jmp @@dig_by_dig

@@char:
    sub ax, "A"
    add ax, 0Ah

@@dig_by_dig:
    shl bx, 4
    add bx, ax

    jmp @@next
    
@@end: 
    ret

endp
;------------------------------------------

.data

preset0 db 0cdh, 0c9h, 0bbh, " ", 0bah, 0bah, 0cdh, 0c8h, 0bch
preset1 db "-## ##-##"

frame_status db 0

program_end:

end start

