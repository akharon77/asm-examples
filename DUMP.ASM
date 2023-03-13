 00000000: E83400                    call 00000037 
 00000003: E80B00                    call 00000011 
 00000006: E85700                    call 00000060 
 00000009: E89F00                    call 000000AB 
 0000000C: B8004C                    mov ax,4C00 
 0000000F: CD21                      int 21 
 00000011: B402                      mov ah,02 
 00000013: CD1A                      int 1A 
 00000015: 803E8A0200                cmp byte ptr [028A],00 
 0000001A: 7407                      jz 00000023 
 0000001C: 880E5702                  mov byte ptr [0257],cl 
 00000020: EB06                      jmp 00000028 
 00000022: 90                        nop 
 00000023: C6065702FF                mov byte ptr [0257],FF 
 00000028: C3                        ret 
 00000029: 02C1                      add al,cl 
 0000002B: 32E4                      xor ah,ah 
 0000002D: B67B                      mov dh,7B 
 0000002F: F6F6                      div dh 
 00000031: 80C41F                    add ah,1F 
 00000034: 8AC4                      mov al,ah 
 00000036: C3                        ret 
 00000037: 8CDF                      mov di,ds 
 00000039: 8EC7                      mov es,di 
 0000003B: BF7602                    mov di,0276 
 0000003E: B401                      mov ah,01 
 00000040: CD21                      int 21 
 00000042: AA                        stosb es:[di],al 
 00000043: 3C0D                      cmp al,0D 
 00000045: 75F9                      jnz 00000040 
 00000047: 4F                        dec di 
 00000048: C60524                    mov byte ptr [di],24 
 0000004B: C3                        ret 
 0000004C: 33DB                      xor bx,bx 
 0000004E: 33C0                      xor ax,ax 
 00000050: BB6500                    mov bx,0065 
 00000053: F7E3                      mul bx 
 00000055: 8A1C                      mov bl,byte ptr [si] 
 00000057: 03C3                      add ax,bx 
 00000059: 46                        inc si 
 0000005A: 803C24                    cmp byte ptr [si],24 
 0000005D: 75F1                      jnz 00000050 
 0000005F: C3                        ret 
 00000060: C606540200                mov byte ptr [0254],00 
 00000065: 90                        nop 
 00000066: BF7602                    mov di,0276 
 00000069: E86800                    call 000000D4 
 0000006C: 8BF3                      mov si,bx 
 0000006E: BF5802                    mov di,0258 
 00000071: E86000                    call 000000D4 
 00000074: 3BF3                      cmp si,bx 
 00000076: 7406                      jz 0000007E 
 00000078: A05702                    mov al,[0257] 
 0000007B: A25402                    mov [0254],al 
 0000007E: BE7602                    mov si,0276 
 00000081: BF7602                    mov di,0276 
 00000084: B128                      mov cl,28 
 00000086: FC                        cld 
 00000087: AC                        lodsb al,ds:[si] 
 00000088: 3C24                      cmp al,24 
 0000008A: 7406                      jz 00000092 
 0000008C: E89AFF                    call 00000029 
 0000008F: AA                        stosb es:[di],al 
 00000090: EBF5                      jmp 00000087 
 00000092: BE5802                    mov si,0258 
 00000095: E8B4FF                    call 0000004C 
 00000098: 8BF8                      mov di,ax 
 0000009A: BE7602                    mov si,0276 
 0000009D: E8ACFF                    call 0000004C 
 000000A0: 3BF8                      cmp di,ax 
 000000A2: 7406                      jz 000000AA 
 000000A4: A05702                    mov al,[0257] 
 000000A7: A25402                    mov [0254],al 
 000000AA: C3                        ret 
 000000AB: 803E540200                cmp byte ptr [0254],00 
 000000B0: 7508                      jnz 000000BA 
 000000B2: BE6602                    mov si,0266 
 000000B5: B40A                      mov ah,0A 
 000000B7: EB06                      jmp 000000BF 
 000000B9: 90                        nop 
 000000BA: BE6E02                    mov si,026E 
 000000BD: B40C                      mov ah,0C 
 000000BF: BFD007                    mov di,07D0 
 000000C2: E80100                    call 000000C6 
 000000C5: C3                        ret 
 000000C6: BA00B8                    mov dx,B800 
 000000C9: 8EC2                      mov es,dx 
 000000CB: FC                        cld 
 000000CC: AC                        lodsb al,ds:[si] 
 000000CD: AB                        stosw es:[di],ax 
 000000CE: 803C24                    cmp byte ptr [si],24 
 000000D1: 75F9                      jnz 000000CC 
 000000D3: C3                        ret 
 000000D4: FC                        cld 
 000000D5: B024                      mov al,24 
 000000D7: 33DB                      xor bx,bx 
 000000D9: 43                        inc bx 
 000000DA: AE                        scasb es:[di],al 
 000000DB: 75FC                      jnz 000000D9 
 000000DD: 4B                        dec bx 
 000000DE: C3                        ret 
 000000DF: 55                        push bp 
 000000E0: 8BEC                      mov bp,sp 
 000000E2: FC                        cld 

