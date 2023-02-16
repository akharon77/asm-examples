.model tiny
.code
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

Start:
    LoadESVideo
    
    mov ax, 4E8h
    mov bx, 80d * 2d * 12d + 20d * 2d
    mov cx, 2d
    call PrintNumSys

    mov ax, 0C28h
    mov bx, 0A0Ah
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

next@@:
        mov dx, 0   
        div cx
                        
        cmp dx, 0Ah
        jb  digit@@
        jae char@@

digit@@:  
        add dx, '0'
        jmp write_sym@@

char@@:
        sub dx, 0Ah
        add dx, 'A'

write_sym@@:
        mov es:[bx], dl
        sub bx, 2
        cmp ax, 0
        jne next@@
        
end@@:
        ret
endp
;------------------------------------------

;------------------------------------------
; MakeBorder
;------------------------------------------
; In:   AH:AL = left  top    x:y
;       BH:BL = right bottom x:y
;       
; Out:  
; Dstr: AX, BX, DX
;------------------------------------------

MakeBorder proc

    call CoordToOffset
    xchg ax, bx

    call CoordToOffset
    xchg ax, bx

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

    push dx
    push ax
    shr ax, 8
    
    push bx
    mov bx, 80d
    mul bx
    pop bx

    pop dx
    mov dh, 0
    add ax, dx

    push bx
    mov bx, 2
    mul bx
    pop bx

    pop dx

    ret
endp

;------------------------------------------

end start

