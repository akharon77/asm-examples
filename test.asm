.model tiny
.code
org 100h

start:

next:
    mov ax, 0AAAAh
    mov bx, 0BBBBh
    mov cx, 0CCCCh
    mov dx, 0DDDDh
    jmp next

end start

