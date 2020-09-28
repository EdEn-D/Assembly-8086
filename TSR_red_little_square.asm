.model tiny
.code
	; ####################################################################################
	; Eden Djanashvili 316262153
	; Or Matityahu 312328453
	; 14.06.20
	; This code changes the previous code to works as model tiny and uses TSR to keep the updated ISR in memory when the program
	; exits.
	
	; How it works:
	; We modified it so it complies with model tiny. Reserving space for the PSP. Change all byte names to actual values. 
	; Save position of the symbol in memory rather then in a register. Adding segments for variables
	; ####################################################################################
	org 100h
Here:
	jmp START

	CLK db 0
	POS dw 2000d
	UP equ 10b
	DOWN equ 01b
	LEFT equ 00b
	RIGHT equ 11b
    Old_int_off dw ?
    Old_int_seg dw ?
    seed DW 0000h
	
NewISR8 proc far
    pushf
    call DWORD PTR cs:[Old_int_off]; JMP to the old int8

	;Preform LFSR once and increment the time keeping register (cx) in order to count the amount of 08h interupts. 
	; Every 9 interupts (half a second) we call moveSymbol and move the 4040h according to the LSBs of the LFSR
	; Otherwise exit the ISR
    call LFSR
    inc cs:CLK
    cmp cs:CLK, 09
    jb skip
    mov cs:CLK, 0
    call moveSymbol
    
	skip:

    iret
NewISR8 endp

moveSymbol proc uses bx
    push ax
    mov ax, 0b800h
    mov es, ax
	pop ax

	push ds
	;mov bx, cs
	;mov ds, bx
	
    
    mov al, bl  ; copy LFSR LSBs
    and al, 11b ; Mask off 2 LSBs
    push ax
    CMP AL, UP ; compare with UP key scan code
	JZ moveUp ; execute moveUp
	CMP AL, DOWN
	JZ moveDown
	CMP AL, LEFT
	JZ moveLeft
	CMP AL, RIGHT
	JZ moveRight

moveUp:
	;Boundery condition
	CMP word ptr cs:POS, 0A0h-2
	JLE doneMove	; skip movement if out of bounds

	mov ax, 0020h
	mov bx, cs:POS
	mov es:[bx], ax ; clear previous spot
	sub word ptr cs:POS, 0A0h ; move up 
	mov ax, 4040h
	mov bx, cs:POS
	mov es:[bx], ax ; print in new location
	JMP doneMove

moveDown:
	;Boundery condition
	CMP word ptr cs:POS, 0A0h*24
	JGE doneMove 

	mov ax, 0020h
	mov bx, cs:POS
	mov es:[bx], ax ; clear previous spot
	add word ptr cs:POS, 0A0h ; move down 
	mov ax, 4040h
	mov bx, cs:POS
	mov es:[bx], ax ; print in new location
	JMP doneMove

moveLeft:
	;Boundery condition
	push ax
	push dx
	mov ax, cs:POS	; move cursor posion to ax
	mov dh, 0A0h
	div dh		;divide by rows
	cmp ah, 0	; compare remainder (X position)
	pop dx
	pop ax
	JZ doneMove

	mov ax, 0020h
	mov bx, cs:POS
	mov es:[bx], ax ; clear previous spot
	sub word ptr cs:POS, 2 ; move left 
	mov ax, 4040h
	mov bx, cs:POS
	mov es:[bx], ax ; print in new location
	JMP doneMove

moveRight:
	;Boundery condition
	push ax
	push dx
	mov ax, cs:POS	; move cursor posion to ax
	mov dh, 0A0h	
	div dh		;divide by rows
	cmp ah, 158	; compare remainder (X position)
	pop dx
	pop ax
	JZ doneMove

	mov ax, 0020h
	mov bx, cs:POS
	mov es:[bx], ax ; clear previous spot
	add word ptr cs:POS, 2 ; move right .
	mov ax, 4040h
	mov bx, cs:POS
	mov es:[bx], ax ; print in new location
	JMP doneMove

doneMove:

	pop ax
	pop ds
    ret
moveSymbol endp

; This procedure takes the current time (min and sec) and sets the seed
setSeed proc
	push AX
	push BX
	push CX
	push DX

	mov al, 0Bh ; status register B address (0Bh)
	out 70h, al ; set read address to 0Bh (status register B)
	in al, 71h ; read status register B (read from address 0Bh)
	out 71h, al ; write status register B

	mov ax,0
	;Get minutes
	mov al, 02h
	out 70h, al
	in al, 71h
	mov dl, al
	;Get seconds 
	mov al, 00h
	out 70h, al
	in al, 71h
	mov dh, al
	mov dh, al
	;save starting time in memory
	mov bx, cs:[offset seed]	; di is offset of the time variable passed
	mov cs:[bx], dx

    pop DX
	pop CX
	pop BX
	pop AX
	ret
setSeed endp

; This procedure takes a seed and preforms one LFSR rotaion on it and returns it in BX while saving the state in the seed
LFSR proc 
    push AX
	push CX
	push DX

	mov bx, seed	;bx will start off as the seed and at each iteration will contain the value of that current 16 bits that need to be manipulated
	mov ax, bx		;copy the seed
	
	mov dx, 1		;initialize lsb of the register that will be used to store the calculations
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
    mov seed, bx    ;save bx for next iteration
    
    pop DX
	pop CX
	pop AX
    ret
LFSR endp

START:

    ;setting extra segment to IVT table 
    mov bx, 0h
    mov es, bx
	cli
	;our code
;Initilations
    mov cx, 0
    call setSeed
    mov di, 0A0h*12+80 ; calculate middle of screen

; GET INTERRUPT VECTOR
    mov ah, 35h    
    mov al, 08h    ; Entry: AL = interrupt number
    int 21h        ; Return: ES:BX -> current interrupt handler

; Save old int 08h offset and segment
    ;cli
    mov cs:Old_int_off, bx
    mov cs:Old_int_seg, es
    
; Setting new ISR address in IVT
    push ds
    mov dx, offset NewISR8
    mov ax, cs;seg NewISR8
    mov ds, ax
    

; SET INTERRUPT VECTOR
    mov ah, 25h     
    mov al, 08h     ; AL = interrupt number
    int 21h         ; DS:DX -> new interrupt handler
    pop ds
	sti

escProg:
	;sti
    mov dx, START
    int 27h
   ;return to OS
   ;.exit
    ;mov ax, 4c00h
    ;int 21h
end Here


; Set carriage
;	mov dh, 15
;	mov dl, 30
;	mov ah, 02h
;	int 10h

	

;; GET INTERRUPT VECTOR
;    mov ah, 35h    
;    mov al, 08h    ; Entry: AL = interrupt number
;    int 21h        ; Return: ES:BX -> current interrupt handler
;
;; Save old int 08h offset and segment
;    ;cli
;    mov cs:Old_int_off, bx
;    mov cs:Old_int_seg, es
;    
;; Setting new ISR address in IVT
;    push ds
;    mov dx, offset NewISR8
;    mov ax, cs;seg NewISR8
;    mov ds, ax
;    
;
;; SET INTERRUPT VECTOR
;    mov ah, 25h     
;    mov al, 08h     ; AL = interrupt number
;    int 21h         ; DS:DX -> new interrupt handler
;    pop ds
;	sti