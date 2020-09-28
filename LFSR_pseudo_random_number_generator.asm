.model small

seed EQU 0FA85h
time EQU 160d

.data
	;ascii translator
	ascii DB 10h dup ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
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
	;mov dx, 1
	mov bx, seed	;bx will start off as the seed and at each iteration will contain the value of that current 16 bits that need to be manipulated
	mov ax, bx		;copy the seed
	mov si, time	;time counter
	mov di, 16d		;print counter
	mov bp, 0 		;print location pointer
	
L1:	mov dx, 1		;initialize lsb of the register that will be used to store the calculations
	mov ax, bx		;copy current bx for cycle
	; get 6th bit
	mov cl, 05h		;shift counter
	shr ax, cl		;shift until the required bit will be in the lsb
	and dx, ax		;copy 6th bit to dx
	;get 8th bit
	mov cl, 02h		;and repeat...
	shr ax, cl
	xor dx, ax		;6 xor 8 (in lsb)
	;get 13th bit
	mov cl, 05h
	shr ax, cl
	xor dx, ax		;(6 xor 8) xor 13
	;get 15th bit
	mov cl, 02h
	shr ax, cl
	xor dx, ax		;[(6 xor 8) xor 13] xor 15
	;get 16th bit
	shr ax, 1
	xor dx, ax		;{[(6 xor 8) xor 13] xor 15} xor 16
	
	not dx			;not the result for NXOR, now dx contains the new bit after the final calculation
	
	and dx, 1		;keep only the LSB of dx
	ror dx, 1		;mov the LSB to MSB of dx
	shr bx, 1		;shift current bx by 1
	or bx, dx		;push the final XNOR calculation to the MSB of bx
					;bx now has the final 16 bits for the current iteration
	dec di			;check if need to print
	cmp di, 0
	jnz L2			;jump to L2 if dont need to print
	
	;print every 16 iterations
	mov cl, 4		;shift register count
	;print first digit
	mov dx, 0
	mov dl, bh		;first print high
	shr dl, cl		;shift 4 times to get 'tens' digit
	mov di, dx
	mov	dl, ascii[di]		;get ascii value of dl
	mov dh, 3fh				;color
	;or dh, 128d
	mov es:[0A0h*12+bp], dx	;print
	add bp, 2
	;print second digit
	mov dx, 0
	mov dl, bh		;first print high
	and dl, 0fh		;keep only the 4 LSB bits
	mov di, dx
	mov	dl, ascii[di]		;get ascii value of dl
	mov dh, 3fh				;color
	mov es:[0A0h*12+bp], dx	;print 
	add bp, 2
	
	;print third digit
	mov dx, 0
	mov dl, bl		;first print high
	shr dl, cl		;shift 4 times to get 'tens' digit
	mov di, dx
	mov	dl, ascii[di]		;get ascii value of dl
	mov dh, 3fh				;color
	mov es:[0A0h*12+bp], dx	;print 
	add bp, 2
	;print forth digit
	mov dx, 0
	mov dl, bl		;first print high
	and dl, 0fh		;keep only the 4 LSB bits
	mov di, dx
	mov	dl, ascii[di]		;get ascii value of dl
	mov dh, 3fh				;color
	mov es:[0A0h*12+bp], dx	;print 
	add bp, 2
	
	
	add bp, 8				; add spaces to look nice
	mov di, 16				;reset print counter
	
L2:	dec si		;check repeat condition
	jnz L1
	
	
    ;return to OS
    mov ax, 4c00h
    int 21h
end START

;////////////////////////////////////////////////////////////
;Answer to question
;Currently with only 160 iterations there is no repeating cycle that i was able to spot. however for
;more iterations i was able to spot a cycle. didn't count how mant iterations. That wasn't the question :) 