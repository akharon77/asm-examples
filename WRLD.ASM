.model tiny
.code
org 100h

start:	
	mov ah, 09h
	mov dx, offset meow_msg
	int 21h
	
	mov ax, 4c00h
	int 21h

.data

meow_msg db "Hello, World! Murr-meow", "$"

end start