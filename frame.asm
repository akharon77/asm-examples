.model tiny
.code
.186
locals @@
org 100h

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
    push bx         ; сохраним значение bx
    
    mov bx, ax
    shl ax, 6       ; x * 80 = x * 2^6 + x * 2^4 = x << 6 + x << 4
    shl bx, 4
    add ax, bx

    pop bx          ; забрали значение ax с al
    nop

endm
;------------------------------------------


Start:
    LoadESVideo
    
    mov ax, 4E8h
    mov bx, 80d * 2d * 12d + 20d * 2d
    mov cx, 2d
    call PrintNumSys

    mov ax, 0C28h
    mov bx, 0A03h
    call MakeBorder

    mov al, 0
    Exit

;------------------------------------------
; PrintNumSys
;------------------------------------------
; In:   AX = number
;       BX = offset
;       CX = base
; Out:  
; Dstr: AX, BX, DX
;------------------------------------------

PrintNumSys proc

@@next:
        mov dx, 0   
        div cx
                        
        cmp dx, 0Ah
        jb  @@digit
        jae @@char

@@digit:  
        add dx, '0'
        jmp @@write_sym

@@char:
        sub dx, 0Ah
        add dx, 'A'

@@write_sym:
        mov es:[bx], dl
        sub bx, 2
        cmp ax, 0
        jne @@next
        
@@end:
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
        lodsw
        xchg ax, bx
        xchg bh, bl

        lodsb
        xchg al, dh
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


    mov di, ax          ; указали на начало рамочки

    mov cx, bx          ; количество строк 
    and cx, 0FFh

    mov dl, bh          ; ширина рамочки

@@next:
    push di             ; запомнили первый столбец текущей строки

    cld
    cmp cl, bl
    je  @@first
    cmp cl, 1
    je @@last

@@middle:
    mov si, offset Preset0 + 3
    LoadBorderChars

    jmp @@line

@@first:
    mov si, offset Preset0
    LoadBorderChars
    jmp @@line

@@last:
    mov si, offset Preset0 + 6
    LoadBorderChars

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

Preset0:
    db 0cdh, 0c9h, 0bbh, " ", 0bah, 0bah, 0cdh, 0c8h, 0bch

end start

