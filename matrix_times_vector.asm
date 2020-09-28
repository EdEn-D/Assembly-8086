.model small

N EQU 3

.data
	MAT DB 2, 3, 1, 0Ah, 8, 1, 0Fh, 5, 4
	VEC DB 7, 0Dh, 6
	RESULT DW N dup (0)
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
	mov bx, 0		;MAT line number (column pointer)
	mov di, 0		;RESULT index (0 to (N-1))
	mov cl, N
	
L1:	mov al, MAT[bx]			;insert current MAT value to AL
	imul VEC[si]			;multiply above line by current VEC value
	add RESULT[di], ax		;add it to current result slot
	inc si					;inc vector index
	inc bx
	CMP si, N				;check for last vector index
	JNE L1					;if not, loop back to L1
	mov si, 0				;reset the vector index
	inc di					;inc result index
	inc di					;twice
	dec cl
	JNZ L1
	
	
    ;return to OS
    mov ax, 4c00h
    int 21h
end START

;/////////////////////////////////////////////////////

;Answers to questions
;1. RESULT is a word array becuse muliplying 8 bit registers results in a 16 bit 
;	number at most, therefore we need a word instead of a byte
;2. I used the debugger to check the RESULT array after running the code on mulitple sizes
;	of matrixs inorder to convince myself that my code works correctly
;3. Yes there would be. I have two possible problems that may arise first one is the inability
;	to access the information in the memory because i would need an larger than 16 bits. Also 
;	for 16 bits, when the MSB will be 1 it will see it as negative values and not be able to reach those values.
;	Also we will probably run out of space in the data segment