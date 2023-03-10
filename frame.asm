.model tiny
.code
.186
locals @@
org 100h

TOP_OFFSET          equ 0
MIDDLE_OFFSET       equ 3
BOTTOM_OFFSET       equ 6
CMD_TAIL_LEN_OFFSET equ 80h
CMD_TAIL_OFFSET     equ 82h

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
; Mul80
;------------------------------------------
; In:   AX
; Out:  AX = 80 * AX
; Dstr: None
;------------------------------------------
Mul80 macro
    nop
    push bx         ; ???????? ???????? bx
    
    mov bx, ax
    shl ax, 6       ; x * 80 = x * 2^6 + x * 2^4 = x << 6 + x << 4
    shl bx, 4
    add ax, bx

    pop bx          ; ??????? ???????? ax ? al
    nop

endm
;------------------------------------------

Start:
    LoadESVideo

    mov ax, 31h
    FillScreen
    
    mov si, CMD_TAIL_OFFSET
    call CalcPos

    push ax
    mov si, offset Preset0
    call MakeBorder

    pop bx
    add bh, 2
    add bl, 2
    mov si, CMD_TAIL_OFFSET
    call StrOut

    mov al, 0
    Exit

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
; In:   AH:AL = left  top    x:y
;       BH:BL = width:height
;       
; Out:  
; Dstr: AX, BX, DX
;------------------------------------------
MakeBorder proc
    call CoordToOffset  ; ax = offset

    mov di, ax          ; ??????? ?? ?????? ???????
    mov cx, bx          ; ?????????? ????? 
    and cx, 0FFh
    mov dl, bh          ; ?????? ???????

@@next:
    push di             ; ????????? ?????? ??????? ??????? ??????
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
@@first:                    ;* ????? ??????? ?????? ???????? mid left right
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

    pop di              ; ????????? ? ?????? ???????
    add di, 80d * 2d    ; ??????? ?? ????????? ???????

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

    mov dl, 0Fh         ; ??????? ???????
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

    push bx         ; ???????? ???????? bx
    push ax         ; ???????? ???????? y, ??????? ? al
    shr ax, 8       ; ??????? ? ax ???????? x
    
    Mul80

    pop bx          ; ??????? ???????? ax ? al
    and bx, 0FFh    ; ???????? ??????? 8 ???
    add ax, bx
    shl ax, 1       ; ????????? ???? ?? 2

    pop bx          ; ???????????? bx

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

;------------------------------------------
; CalcPos
;------------------------------------------
; In:   DS:SI = string
; Out:  AH:AL = left top x:y
;       BH:BL = width:height
; Dstr: 
;------------------------------------------
CalcPos proc
    mov dx, 1                           ; DH:DL = width:height
    mov ch, 0
    mov bl, 0
    mov cl, ds:[CMD_TAIL_LEN_OFFSET]
    
@@next:
    lodsb

    cmp al, "\"
    je @@new_str
    
    cmp al, "%"
    je @@num

    inc bl
    jmp @@end_loop

@@num:
    push es
    push di
    mov cx, ds
    mov es, cx
    mov di, si
    repne scasb
    mov si, di
    pop di
    pop es
    jmp @@end_loop

@@new_str:
    cmp bl, dh
    jbe @@last_width
    mov dh, bl

@@last_width:
    mov bl, 0
    inc dl

@@end_loop:
    cmp al, "$"
    jne @@next

    cmp bl, dh
    jbe @@end
    mov dh, bl

@@end:
    add dh, 3           ; ?????? $
    add dl, 4
    mov bh, dh
    mov bl, dl
    mov ah, 12d
    mov al, 40d
    shr dh, 1
    shr dl, 1
    sub ah, dl
    sub al, dh
    ret

endp
;------------------------------------------

.data

preset0:
    db 0cdh, 0c9h, 0bbh, " ", 0bah, 0bah, 0cdh, 0c8h, 0bch
    db "-## ##-##"

end start

