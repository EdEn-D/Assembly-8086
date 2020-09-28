.model small
.stack 100h
.data
	; ####################################################################################
	; Eden Djanashvili 316262153
	; Or Matityahu 312328453
	; 14.06.20
	; This program adds a loser message as well as a deleting rows each second to make 
	; this game so much more entertaining for the whole family
	
	; How it works:
	; We initialize a time array and at the very beggining we sample the time
	; At the end of the program (when the user reaches the blue square) we sample the time again
	; this time by passing the endTime array to the time sampling code and compare to the starting time
	; We check which second out of 3600 seconds in one hour the user starts the game, and on which second he finishes

	; UPDATE 7.3:
	; Added a countdown clock of 15 seconds, that decreses each time the 08h timer interpt is called 18 times.
	; Each second the new interupt procedure also calls a procedure to delete a row and shrink the winning area
	; ####################################################################################

	; arrow keys scan codes (when released)
	UP equ 0C8h
	DOWN equ 0D0h
	LEFT equ 0CBh
	RIGHT equ 0CDh

    Old_int_off dw 1111h
    Old_int_seg dw 3333h

	BLACK_SPACE equ 0020h
	SYMBOL equ 2E40h ; '@' symbol on with background
	CLK db 15d	; we use this variable in memory to count down 15 seconds until the user loses 
	TMR db 00h	; we use this to keep track of the amount of 08h interups until we reach 1 second
	; thes two words will hold coresponding times to comapare at the end
	startTime dw 0000d	
	endTime dw 0000d
	; end game messages
	fast db 'You are fast like Sonic$'
	okay db 'Okay$'
	slow db "That's Slow$"
	lost db "YOU LOSER$"
.code
START:
    ;initializing data segment 
    mov ax, @data
    mov ds, ax

    ;setting extra segment to IVT table 
    mov ax, 0h
    mov es, ax
	
	;our code
	;Initilations
	call reMapIvt 	; change what the 08h interupt does	

	;setting extra segment to screen memory 
    mov ax, 0b800h
    mov es, ax

	; masking interups for keyboard to work
	in al, 21h
	or al, 02h
	out 21h, al
	call init ; read proc comments on top of proc...

	; We will use di as the XY coordinate
	;di+-2 		; move right/left respectively
	;di+- 0A0h 	; move down/up respectively

; KEY DETECTION
keyDet: 
; Stopping conditon - blue corner was reached
	CMP di, 0A0h*5-2	; check the row number, if we are not in range to stop we continue 
	JA cont

	push ax
	push dx
	; Here we check the column number by using the remainder from the division of the rows
	mov ax, di	; move cursor posion to ax
	mov dh, 0A0h	
	div dh		;divide by rows

	; we take the current CLK position and calculate where the boundry is.
	mov dh, CLK
	dec dh
	shl dh, 1
	cmp ah, dh	; compare remainder (X position) - if reached the blue corner then end program

	pop dx
	pop ax
	JBE finishGame
	
	

cont:
	;check loser
	cmp CLK, 0	; if timer reached 0 then finish game
	ja notLost
	jmp finishGame
notLost:
	IN AL, 64h ; check for keyboard status
	TEST AL, 01h ; was there any?
	JZ cont ; wait for key press otherwise

 	IN AL, 60h ; get scan code
	CMP AL, 01 ; Escape key
	JZ escProg
	CMP AL, UP ; compare with UP key scan code
	JZ moveUp ; execute moveUp
	CMP AL, DOWN
	JZ moveDown
	CMP AL, LEFT
	JZ moveLeft
	CMP AL, RIGHT
	JZ moveRight
	;else:
	jmp keyDet

moveUp:
	;Boundery condition
	CMP di, 0A0h-2
	JLE keyDet	; skip movement if out of bounds

	mov es:[di], BLACK_SPACE ; clear previous spot
	sub di, 0A0h ; move up 
	dec si	; row tracker down
	mov es:[di], SYMBOL ; print in new location
	JMP keyDet

moveDown:
	;Boundery condition
	CMP di, 0A0h*24
	JGE keyDet 

	mov es:[di], BLACK_SPACE ; clear previous spot
	add di, 0A0h ; move down 
	inc si	; row tracker up
	mov es:[di], SYMBOL ; print in new location
	JMP keyDet

moveLeft:
	;Boundery condition
	push ax
	push dx
	mov ax, di	; move cursor posion to ax
	mov dh, 0A0h
	div dh		;divide by rows
	cmp ah, 0	; compare remainder (X position)
	pop dx
	pop ax
	JZ keyDet

	mov es:[di], BLACK_SPACE ; clear previous spot
	sub di, 2 ; move left 
	mov es:[di], SYMBOL ; print in new location
	JMP keyDet

moveRight:
	;Boundery condition
	push ax
	push dx
	mov ax, di	; move cursor posion to ax
	mov dh, 0A0h	
	div dh		;divide by rows
	cmp ah, 158	; compare remainder (X position)
	pop dx
	pop ax
	JZ keyDet

	mov es:[di], BLACK_SPACE ; clear previous spot
	add di, 2 ; move right .
	mov es:[di], SYMBOL ; print in new location
	JMP keyDet

finishGame:
call endGame

escProg:
	call revertIVT ; Restore the IVT which we have changed

	; Restore keyboard hardware interrupt
	mov ax, 0
	in al, 21h
	and al, 11111101b
	out 21h, al
	
	;return to OS
    mov ax, 4c00h
    int 21h

; Redirect the interupt handler in the IVT to modify each interupt
reMapIvt proc uses ax bx dx es ds
	; GET INTERRUPT VECTOR
    mov ah, 35h    
    mov al, 08h    ; Entry: AL = interrupt number
    int 21h        ; Return: ES:BX -> current interrupt handler

	; Save old int 08h offset and segment
    cli
    mov Old_int_off, bx
    mov Old_int_seg, es
    
	; Setting new ISR address in IVT
    push ds
    mov dx, offset NewISR8
    mov ax, seg NewISR8
    mov ds, ax
    

	; SET INTERRUPT VECTOR
    mov ah, 25h     
    mov al, 08h     ; AL = interrupt number
    int 21h         ; DS:DX -> new interrupt handler
    pop ds
	sti
	ret
reMapIvt endp

; Remap the old interput back inorder to exit the program properly
revertIVT proc
	cli
	; Setting old ISR address in IVT
    push ds
    mov dx, Old_int_off
    mov ax, Old_int_seg
    mov ds, ax

	; SET INTERRUPT VECTOR
    mov ah, 25h     
    mov al, 08h     ; AL = interrupt number
    int 21h         ; DS:DX -> new interrupt handler
    pop ds
	sti

	ret
revertIVT endp

; calls the old interupt and adds some conditions to erase a coloumn each second (every 18 interups)
NewISR8 proc 
    pushf
    call DWORD PTR [Old_int_off]; JMP to the old int8
    
	; Check if ~1 has passed
	inc TMR
	cmp TMR, 18d
	jb skip
	mov TMR, 0 ; Reset TMR if 1 second has passed
	dec CLK	
	call delCol ; Delete colomn, decrease winning area
	

	skip:
    iret
NewISR8 endp

; Erases a column each time called according to the current time on the CLK
delCol proc uses cx di bx
	mov cx, 5	; amount of rows to erase
	mov di, 0	; number row for each loop
	mov bx, 0
	mov bl, CLK	; number of column to erase
	shl bx, 1
	erase:	; erase the current column row by row
	mov es:[bx+di], BLACK_SPACE	
	add di, 0A0h
	loop erase

	ret
delCol endp

; initilize values: get start time, draw target rectangle, print char in the middle of the screen
init proc
	mov di, offset startTime ; pass starting time to getCurrTime procedure 
	call getCurrTime
	mov di, 0
	call corner	;proc that colors the top left corner

	mov bh, 46 ; color
	mov bl, '@' ; @ char
	mov di, 0A0h*12+80 ; calculate middle of screen
	;mov di, 0A0h*6+50 ; test
	mov si, 12	; row tracker 
	mov es:[di], bx ; print @ char in the middle
	ret
init endp

; We color the corner using a nested loop
corner proc
	push bx
	push ax
	mov ah, 48	; cyan color
	mov al, ' '

	mov cx, 5 ; initialize outer loop counter
	colorOuter:
	push cx ; save outer loop counter
	mov cx, 15
	color:			; inner loop, color column
	mov es:[bx], ax	; print cyan space
	add bx, 2
	loop color
	add bx, 0A0h-30 ; reset cursor back to left most column
	pop cx ; outer loop counter
	loop colorOuter ; outer loop
	
	pop ax
	pop bx
	ret
corner endp

; Gets the current time and updates the variable passed to it thorugh di 
getCurrTime proc
	push AX
	push BX
	push CX
	push DX

	mov al, 0Bh ; status register B address (0Bh)
	out 70h, al ; set read address to 0Bh (status register B)
	in al, 71h ; read status register B (read from address 0Bh)
	or al, 100b ; mask off 2nd bit, set to output in binary
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
	;save starting time in memory
	mov bx, di	; di is offset of the time variable passed
	mov ds:[bx], dx

	; move the second calculation here to compare seconds easier later
	;Calculate starting time
	mov ax, 0
	mov bx, di
	mov al, 60d
	mov ch, ds:[bx] 	; minutes
	mul ch				; convert to seconds (multiply 60 times the minute number)

	mov ch, 0		
	mov cl, ds:[bx+1]	; get seconds
	add ax, cx			; add the seconds to the converted minutes to get starting second
	; Write converted time back to memory 
	mov ds:[bx], ax	

	pop DX
	pop CX
	pop BX
	pop AX
	ret
getCurrTime endp

; Compares the times of the beginnig and end of the game and prints message accordingly 
endGame proc
	push AX
	push BX
	push CX
	push DX

	; get current time when the player reaches blue zone
	mov di, offset endTime
	call getCurrTime

	mov bx, endTime
	mov dx, startTime
	sub bx, dx			; get second difference 

	mov ax, 0
	; Set carriage
	mov dh, 15
	mov dl, 30
	mov ah, 02h
	int 10h

	; check loser
	cmp CLK, 0
	jbe loser

	cmp bx, 10d	; > 10 [sec] : "That's slow"
	ja slowTime
	cmp bx, 5d	; 5 – 10 [sec] : "Okay"
	ja okayTime
	; else 0 – 5 [sec] : "You are fast" 
fastTime:
	;print fast
	mov dx, offset fast
	mov ah, 09h
	int 21h
	jmp finish
okayTime:
	;print okay
	mov dx, offset okay
	mov ah, 09h
	int 21h
	jmp finish
slowTime:
	;print slow
	mov dx, offset slow
	mov ah, 09h
	int 21h
	jmp finish
loser:
	;print lost
	mov dx, offset lost
	mov ah, 09h
	int 21h
finish:
	pop DX
	pop CX
	pop BX
	pop AX
	ret
endGame endp

end START