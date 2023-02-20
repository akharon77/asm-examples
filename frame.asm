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
; MakeBorder
;------------------------------------------
; In:   AH:AL = left  top    x:y
;       BH:BL = width:height
;       
; Out:  
; Dstr: AX, BX, DX
;------------------------------------------

MakeBorder proc

    call CoordToOffset
    mov di, ax

    mov dl, bh

    push bx
    mov bx, 2324h
    mov dh, 21h
    call MakeLine
    pop bx

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
; Dstr: None
;------------------------------------------

MakeLine proc

    cld
    mov ah, 0Fh
    mov al, bl
    stosw

    mov al, bh
    push cx
    mov cx, dx
    and cx, 0FFh
    sub cx, 2
    rep stosw
    pop cx

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

end start

