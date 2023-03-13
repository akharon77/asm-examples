.model tiny
.code
.286
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

start:
    mov bx, cs
    mov es, bx
    mov di, offset buf
    cld

ent_code_loop:
    mov ah, 1h
    int 21h
    stosb

    cmp al, 24h
    jne ent_code_loop       ; don't check password length

    sub di, offset buf
    mov cx, di
    dec cx                  ; length of entered password

    mov ax, 0h              ; evaluate hash
    mov si, offset buf

hash_loop:
    shl ax, 1h              ; mul 42 const

    mov bx, ax
    shl bx, 1h
    add ax, bx

    mov bx, ax
    shl ax, 1h
    add ax, bx
    shl ax, 1h
    add ax, bx

    mov bx, cs:[si]
    and bx, 0FFh
    add ax, bx
    inc si

    loop hash_loop

    mov dx, offset err_msg
    cmp ax, [hash_orig]
    jne no_ok
    mov dx, offset ok_msg
no_ok:
    mov ah, 09h
    int 21h
    
    mov al, 0
    Exit

program_end:

.data

ok_msg    db 0Dh, "Access granted", "$"
err_msg   db 0Dh, "Wrong password", "$"
buf       db 16d dup (0h)
hash_orig dw 0F9E8h
; qwertyuiopasdfghö 0F616h

end start

