.model tiny
.code
org 100h

start:	
	mov ah, 02h
	mov dl, 21h
	int 21h
	
	mov ax, 4c00h
	int 21h

.data

; meow_msg db "Hello, World! Murr-meow", "$"

end start