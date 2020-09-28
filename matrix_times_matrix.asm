.model small
.stack 100h

N EQU 3
;N EQU 4 ;test

.data
	MAT1 DB 2, 3, 1, 0Ah, 8, 1, 0Fh, 5, 4
	;MAT1 DB 2, 3, 1, 7, 3, 0Dh, 16h, 8, 1, 0, 0Eh, 1 ,0fh, 5, 4, 6 ;test
	MAT2 DB 7, 3, 9, 0, 0Eh, 1,  6, 0Bh, 2
	;MAT2 DB 0Bh, 2, 0Dh, 2, 5, 7, 0Ch, 3, 2, 1, 2, 23h, 1Ah, 5, 2, 7 ;test
	RESULT DW N*N dup(0)
	
.code
START:
    ;initializing data segment 
    mov ax, @data
    mov ds, ax

    ;setting extra segment to screen memory 
    mov ax, 0b800h
    mov es, ax
	
	;our code
	;Initilations 
	mov ax, 0
	mov si, 0		;vector index
	mov di, 0		;RESULT index (0 to (N-1))
	mov cl, N		;counter for the amount of columns to calculate 
	mov dx, 0		;keeps track of the number of the current coloumn (out of N) 
	
L2:	call MATRIX_MUL			;Call the MATRIX_MUL Procedure 
	inc dx					;next row
	mov si, 0				;resest result index
	add si, dx				;add offset to MAT2 index
	inc di					;add offset to RESULT index
	inc di					;twice (because result is WORD)
	loop L2

    ;return to OS
	mov ax, 4c00h
    int 21h
	
	;This procedure is a copy of the code from the previous assignment but modified to be able to multiple N*N matrixes with
	;each other. This code calculates the result 1 column at a time
	; SIDE NOTE
		;I wish I started over on this code from scratch because as I was writing it I realized a better way to implement
		;the multiplication process. Mainly what i could have changed is calculating the result 1 row at a time instead . 
		;The way i did it i had to make uncomfortable jumps in the indexes of 2 maxrixes instead of only 1. 
		;But everything works nonetheless. This comment was meant more for me. sorry for the rant :)
	MATRIX_MUL PROC
	push cx					;save these values in the stack to keep track of exzternal counter and RESULT index
	push di
	
	mov cx, N				;Reset loop counter. Need to run N times per coloumn
	mov bx, 0				;reset MAT1 pointer
	;This is the loop that calculates the actual column
L1:	mov al, MAT1[bx]		;insert current MAT1 value to AL
	imul MAT2[si]			;multiply above line by current MAT2 value
	add RESULT[di], ax		;add it to current result slut
	add si, N 				;inc MAT2 index by N (next Row)
	inc bx					;move to the next value in MAT1 
	cmp si, N*N				;check if si reached the last row in MAT2
	JL L1					;if si < N*N then jump. From to the L1 label to here the code will run N times. 
							;up the this loop the code calculates 1 value in the result matrix
	;Now we need to check which value out of the current column we need to calculate. If we calculated all of them then we jump
	; otherwise we loop back with an offset and calculate the next value
	mov si, 0				;reset the MAT2 index
	add si, dx 				;add the column offset
	add di, 2*N				;inc result index to next row , twice (because result is WORD)
	dec cl					;check if we went through all the whole column
	JNZ L1
	
	pop di					;reload the values
	pop cx
	RET
	MATRIX_MUL ENDP

end START
