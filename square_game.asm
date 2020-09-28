.model small
.stack 100h
.data

	; ####################################################################################
	; 04.06.20
	; Part 1
	; This program simply prints a '@' symbol at the middle of the screen, and allows the user to move using the
	; arrow keys in 4 directions while leaving behind a black blank symbol. 
	; It prevents the user from the moving past the borders of the DosBox window
	; arrow keys scan codes (when released)
	; It also allows the user to press the Esc key to get out of the program to pevent an infinate loop
	
	; How it works: 
	; After initializing the @ in the center we loop and check for a proper key press.
	; We jump depending on the key pressed and every movement has a border check to make sure we dont
	; advence if we reached the borders of the window. 
	; After every movment we jump back to the key detection until the user presses the Esc key and the program ends
	
	; Part 2
	; This program adds a blue square to the previous program. If the user enters the blue square then the
	; program ends
	
	; How it works:
	; The blue square is created with a nested loop in a procedure defined at the very bottom 
	; of the code and is called in the beggining of our code.
	; We make sure to check the game end conditions (blue square reached) at the key detection stage
	; Some other changes were made for convinece 
	; Part 3
	; This part of the program adds time keeping code and displays a message accordingly
	
	; How it works:
	; We initialize a time array and at the very beggining we sample the time
	; At the end of the program (when the user reaches the blue square) we sample the time again
	; this time by passing the endTime array to the time sampling code and compare to the starting time
	; We check which second out of 3600 seconds in one hour the user starts the game, and on which second he finishes
	; ####################################################################################

	; arrow keys scan codes (when released)
	UP equ 0C8h
	DOWN equ 0D0h
	LEFT equ 0CBh
	RIGHT equ 0CDh

	BLACK_SPACE equ 0020h
	SYMBOL equ 2E40h ; '@' symbol on with background
	; thes two words will hold coresponding times to comapare at the end
	startTime dw 0000d	
	endTime dw 0000d
	; end game messages
	fast db 'You are fast like Sonic$'
	okay db 'Okay$'
	slow db "That's Slow$"
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
	cli ; clear interrupt enable flag (disable interrupts)

	mov di, offset startTime
	call getCurrTime
	mov di, 0
	;call endGame
	call corner	;proc that colors the top left corner

	mov bh, 46 ; color
	mov bl, '@' ; @ char
	mov di, 0A0h*12+80 ; calculate middle of screen
	;mov di, 0A0h*6+50 ; test
	mov si, 12	; row tracker 
	mov es:[di], bx ; print @ char in the middle

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
	cmp ah, 28	; compare remainder (X position) - if reached the blue corner then end program

	pop dx
	pop ax
	JBE finishGame
	
	

cont:
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
 ;return to OS
    mov ax, 4c00h
    int 21h

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
finish:
	pop DX
	pop CX
	pop BX
	pop AX
	ret
endGame endp

end START


