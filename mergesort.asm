.model small
.stack 100h


.data
    array db 0FFh, 3h, 7h, 0BH, 4h, 2h, 8h, 0ah, 5h, 1h, 6h, 4h, 12h, 00h ;test
    ;array db 9h, 3h, 7h, 0BH, 4h, 2h, 8h, 0ah, 5h, 1h, 6h, 4h, 12h ;test
    ;array db 5, 1, 50h, 20h, 60h, 30h, 6h, 15h	; test
    ;array db 5, 1, 26, 182, 5, 253, 98, 182	
	len equ ($-array) ; len becomes the length of `array` in bytes
    FML db len dup (0)  ; this is the array we use for merging the subarrays - the name reflects our emotional state while writing this code
	
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
    mov bx, offset array        ; Initialize array address argument
    mov di, 0                   ; Initialize base index
    mov cx, len-1               ; Initialize head index
    mov ax, 0                   ; initialize ax = 0
    call MergeSort

    ;return to OS
	mov ax, 4c00h
    int 21h

    MergeSort proc
    cmp di, cx      ; stopping codition, check if the base index has reached the head index (meaning the length of the subarray is 1)
    jne cont        ; if they are not equal then continue, otherwise return
    ret
cont:   
    push di  ; Save base index
    push cx  ; Save head index
mid:            ;find midpoint - the sub array needs to be split in half, this small block of code find the midpoint between the base and head index 
    push ax     
    mov ax, cx      ; copy head to ax
    sub ax, di      ; do (head - base)
    inc ax          ; +1
    shr ax, 1       ; divide by 2 (not ax holds the amount of elements from cx to the middle of the sub array)
    sub cx, ax      ; head - midpoint = new cx is the midpoint of the subarray
    pop ax

left: ; Pass left parameters    
    call MergeSort   ;[base(di), middle(new cx)]
    ; Prepare indexes for passing right subarray parameters
    mov di, cx       ; the base of the right subarray is now the head of the left subarray + 1
    add di, 1
    mov bp, sp
    mov cx, [bp]     ;[bp] is original CX (head index of the array that is being split into 2 left and right subarrays)
right: ; Pass right parameters
    call MergeSort              ;[base(new cx), end(original cx)]

mergeBoth: ;pass left and right into merge
    mov bp, sp
    mov si, [bp+2]  ;assign si to be the original base index of the array (di(original))
    call Merge

    pop cx ; Restore previous end index
    pop di ; Restore previous start index
    ret
    MergeSort endp

    Merge proc
    push bx
    mov bx, si  ;save original starting index

;reset FML - this block of code simply resets our merging array. Might not be super necessary however it cleans the array and makes following the code a less dreadful experience
    mov bp, offset FML
    push cx
    mov cx, len
L1: mov byte ptr ds:[bp], 0
    inc bp
    loop L1
    pop cx

    mov ax, 0           ; reset ax
    mov bp, offset FML  ; get FML address

    mov dx, 0
    mov dx, di  ; left subarray's head index di-1=DX
    dec dx
    ; right now what we do is follow 4 indexs which we will use to itterate through both subarrays and merge them together
    ; we have SI and DX which are the base and head indexes (respectively) of the left subarray
    ; we have DI and CX  which are the base and head indexes (respectively) of the right subarray
    ; we will increment SI and DI accordingly until the base indexes surpass the head indexes which is our stopping condition
    ; CX and DX don't change, only SI and DI
compare: 
    ; stopping condition - test if compare is done
    cmp di, cx  ;if di <= cx then not done, continue with merging
    jbe notDone
    cmp si, dx
    jbe notDone
    jmp done ; DI > CX AND SI > DX THEN done 

notDone:
    cmp si, dx ; if si > dx then skip allocation, meaning the base index exceeded the head index, so only the right subarray remains unmerged
    mov al, byte ptr ds:[offset array+di] ;DI starting index of right subarray, ending index CX
    ja pushRight

    cmp di, cx  ; similar comparision here
    mov ah, byte ptr ds:[offset array+si] ;SI starting index of left subarray, ending index di-1=DX
    ja pushLeft

    cmp ah, al   ;check if current left or right element is smaller and merge it accordingly
    jbe pushLeft ; left <= right Skip to pushLeft
pushRight:  
    mov ds:[bp], al ; merge current element from right subarray into the next available slot in FML
    inc bp          ; increment next slot in FML
    inc di          ; increment base index of right subarray
    jmp compare ; go to next comparison
pushLeft:
    mov ds:[bp], ah ; same as pushRight but for left subarray :)
    inc bp
    inc si 
    jmp compare
done:
    ; Now the merging is done, time to write back the merged subarray (FML) into our original array
    ; number of times to write back = (CX - BX) + 1 => CX
    push di         ; we will use DI as the index to itterate through FML
    mov di, 0       
    sub cx, bx      ; loop counter (CX - BX)
    inc cx          ; +1
    mov ax, 0       ; we no longer need AX so we can reset and use it for writing back  
    ; WB = Write Back
WB: mov al, byte ptr ds:[offset FML + di]
    mov  byte ptr ds:[offset array + bx], al    ; copy contents of FML to array
    inc bx          ; BX is the original starting index of the array which we saved in the first couple lines of the merge process which we now use to know where to write back
    inc di
    loop WB ; loop till loop counter = 0

    pop di
    pop bx
    ret
    Merge endp
end START