.model small
.data
	;first digit translator
	ascii1 DB 10h dup ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
	;second digit translator
	ascii2 DB 10h dup ('0'), 10h dup ('1'), 10h dup ('2'), 10h dup ('3'), 10h dup ('4'), 10h dup ('5'), 10h dup ('6'), 10h dup ('7'), 10h dup ('8'), 10h dup ('9'), 10h dup ('A'), 10h dup ('B'), 10h dup ('C'), 10h dup ('D'), 10h dup ('E'), 10h dup ('F')
.code
START:
    ;initializing data segment 
    mov ax, @data
    mov ds, ax

    ;setting extra segment to screen memory 
    mov ax, 0b800h
    mov es, ax
	
	mov ax, 0h		;reset AX
	mov dx, 0h		;reset DX
	mov ax, 03c8fh	;input value of AX
	mov dl, al		;seperate AL and AH by inserting AL into DL
	mov di, dx		;use index for traversing array
	mov bl, ascii2[di]	;translates the left digit of AL to ascii
	mov bh, 47h
	mov es:[2000], bx	;print
	mov bl, ascii1[di]
	mov bh, 47h
	mov es:[2000+2], bx	;translates the right digit of AL to ascii
	
	mov dx, 0h		;reset DX
	mov dl, ah		;seperate AL and AH by inserting AH into DL this time
	mov di, dx				;rest is pretty much the same but with AH
	mov bl, ascii2[di]
	mov bh, 47h
	mov es:[2000-4], bx	
	mov bl, ascii1[di]
	mov bh, 47h
	mov es:[2000-2], bx
	
	
    ;return to OS
    mov ax, 4c00h
    int 21h
end