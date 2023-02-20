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
; Print10NumSys
;------------------------------------------
; In:   AX = number
;       BX = offset
; Out:  
; Dstr: AX, BX, DX
;------------------------------------------
PrintNumSys proc

@@next:
        mov dx, 0   
        div 10
                        
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


end start

