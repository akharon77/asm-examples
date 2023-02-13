.model tiny
.code
org 100h

start:

    mov bx, 0b800h
    mov es, bx              ; установили es на видеопамять

    mov si, 81h             ; установили ax на начало аргумента в psp
    mov cl, ds:[80h]        ; записали длину аргумента

    mov bx, 80d * 2d * 12d + 40d * 2d
    sub bx, cx              ; центровка
    and bl, 0FEh

    mov dl, 00001000b

strcr:

    lodsb                   ; записали символ из ds:si в al
    mov es:[bx],     al     ; вывели на экран
    mov es:[bx + 1], dl

    add bx, 2
    inc dx
    and dl, 10001111b
    or  dl, 10000010b
    loop strcr

    mov ax, 4c00h
    int 21h

end start
