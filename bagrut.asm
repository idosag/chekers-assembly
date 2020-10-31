IDEAL
MODEL small
 

STACK 0f700h

FILE_NAME_IN  equ 'Pro.bmp'
Rect_Down equ 23
Rect_Right_Or_Left equ 37
BMP_WIDTH = 34
BMP_HEIGHT = 20

SMALL_BMP_HEIGHT = 40
SMALL_BMP_WIDTH = 40

RET_MINIMAX equ [word ptr bp +6] 
temp_player equ [word ptr bp +4]
SCORE equ [word ptr bp - 2]
SAVE_EATEN equ [word ptr bp - 4]
 

DATASEG
     board db 0,?,0,?,0,?,0,?
	       db ?,0,?,0,?,0,?,0
		   db 0,?,0,?,0,?,0,?
	       db 1,0,1,0,1,0,1,0
		   db 0,1,0,1,0,1,0,1
	       db ?,0,?,0,?,0,?,0
		   db 0,?,0,?,0,?,0,?
	       db ?,0,?,0,?,0,?,0
	 last_board db 0,5,0,5,0,5,0,5
	       db 5,0,5,0,5,0,5,0
		   db 0,5,0,5,0,5,0,5
		   db 1,0,1,0,1,0,1,0
		   db 0,1,0,1,0,1,0,1
		   db 5,0,5,0,5,0,5,0
		   db 0,5,0,5,0,5,0,5
		   db 5,0,5,0,5,0,5,0
	 minimax_save db 640 dup (?)
	 save_index dw 0
    RndCurrentPos dw start
    OneBmpLine 	db BMP_WIDTH dup (0)  ; One Color line read buffer
   
    ScrLine 	db BMP_WIDTH dup (0)  ; One Color line read buffer

	;BMP File data
	depth dw 3
	best_mov_di dw ?
	best_mov_si dw ?
	l_temp_player db 0
	FileName 	db FILE_NAME_IN ,0
    current_score dw 0
	FileHandle	dw ?
	Header 	    db 54 dup(0)
	Palette 	db 400h dup (0)
	B_eaten db 0
	W_eaten db 0
	start_game db 'pro.bmp',0
	Wsoldier db 'w.bmp',0
	Bsoldier db 'b.bmp',0
	options db 'o.bmp',0
	Erase db 'd.bmp',0
	B_win db 'kb.bmp',0
	W_win db 'kw.bmp',0
	open db 'open.bmp',0
	tmp1 dw ?
	tmp2 dw ?
	tmp3 dw ?
	direction db ?
	turn db 0; if 1 you white if 0 you black
	BmpFileErrorMsg    	db 'Error At Opening Bmp File ',FILE_NAME_IN, 0dh, 0ah,'$'
	ErrorFile           db 0
    BB db "BB..",'$'
	; array for mouse int 33 ax=09 (not a must) 64 bytes
	 W_last db 0
	 B_last db 0
	 eat_option db 0
	 burn db 0
	 could_eat dw 0
	 cnt db 0
	Color db ?
	Xclick dw ?
	Yclick dw ?
	Xp dw ?
	Yp dw ?
	SquareSize dw ?
	 there_is_movment_option db 0
	BmpLeft dw ?
	BmpTop dw ?
	BmpColSize dw ?
	player db 0; the white always starts
	black_winpRowSize dw ?
	 BmpRowSize dw ?
	 black_win db 0
	white_win db 0
	start_B_eaten db 0
	start_W_eaten db 0
	finish db 0
	
CODESEG
      
 
start:
	mov ax, @data
	mov ds, ax
	
	call SetGraphic
	;open the start menue
	mov dx, offset open
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], 320
	mov [BmpRowSize] ,200
	call OpenShowBmp
	;-----------------------
	;show mouse
	 mov ax,1
	 int 33h
	 
	 ;wait for choose of one of the options by click on the option
waiting:	
	
	mov ax,5
	mov bx,0
	int 33h
	cmp bx,0
	jz waiting
	test ax,00000001b
	jz waiting
	shr cx,1; div by 2
	cmp dx,60
	jb waiting
	cmp dx,80
	ja other_option
	cmp cx,90
	jb waiting
	cmp cx,210
	ja waiting
	call single_player; single_player was choosen
	jmp exit
other_option:	
	cmp dx,110
	jb waiting
	cmp dx,130
	ja waiting
	cmp cx,90
	jb waiting
	cmp cx,210
	ja waiting
	call multiplayer; multiplayer was choosen
	jmp exit
	
	
	
     



	
exit:
	 
	
	;show mouse
	 mov ax,1
	 int 33h
	 ; wait for key press
	mov ah,0
	int 16h
	;out from graphic mose
	mov ax,2
	int 10h

	
	mov ax, 4c00h
	int 21h					

;==========================
;==========================
;===== Procedures  Area ===
;==========================
;==========================
; Description  : get RND between any bl and bh includs (max 0 -255)
; Input        : 1. Bl = min (from 0) , BH , Max (till 255)
; 			     2. RndCurrentPos a  word variable,   help to get good rnd number
; 				 	Declre it at DATASEG :  RndCurrentPos dw ,0
;				 3. EndOfCsLbl: is label at the end of the program one line above END start		
; Output:        Al - rnd num from bl to bh  (example 50 - 150)
; More Info:
; 	Bl must be less than Bh 
; 	in order to get good random value again and agin the Code segment size should be 
; 	at least the number of times the procedure called at the same second ... 
; 	for example - if you call to this proc 50 times at the same second  - 
; 	Make sure the cs size is 50 bytes or more 
; 	(if not, make it to be more) 
proc RandomByCs
    push es
	push si
	push di
	
	mov ax, 40h
	mov	es, ax
	
	sub bh,bl  ; we will make rnd number between 0 to the delta between bl and bh
			   ; Now bh holds only the delta
	cmp bh,0
	jz @@ExitP
 
	mov di, [word RndCurrentPos]
	call MakeMask ; will put in si the right mask according the delta (bh) (example for 28 will put 31)
	
RandLoop: ;  generate random number 
	mov ax, [es:06ch] ; read timer counter
	mov ah, [byte cs:di] ; read one byte from memory (from semi random byte at cs)
	xor al, ah ; xor memory and counter
	
	; Now inc di in order to get a different number next time
	inc di
	cmp di,(EndOfCsLbl - start - 1)
	jb @@Continue
	mov di, offset start
@@Continue:
	mov [word RndCurrentPos], di
	
	and ax, si ; filter result between 0 and si (the mask)
	cmp al,bh    ;do again if  above the delta
	ja RandLoop
	
	add al,bl  ; add the lower limit to the rnd num
		 
@@ExitP:	
	pop di
	pop si
	pop es
	ret
endp RandomByCs



; make mask acording to bh size 
; output Si = mask put 1 in all bh range
; example  if bh 4 or 5 or 6 or 7 si will be 7
; 		   if Bh 64 till 127 si will be 127
Proc MakeMask    
    push bx

	mov si,1
    
@@again:
	shr bh,1
	cmp bh,0
	jz @@EndProc
	
	shl si,1 ; add 1 to si at right
	inc si
	
	jmp @@again
	
@@EndProc:
    pop bx
	ret
endp  MakeMask




;================================================
; Description  : check if one player won the game
; Input        : 1. board a array in ds
; 			     2. turn a variable in ds
; Output:         1. white_win a variable in ds
; 			     2. finish a variable in ds
; 			     3. black_win
;================================================
proc check_win
     push cx
	 push si
@@test:
;check the first row
	 mov cx,8
	 mov si, 0
     test [turn],00000001b; check which color is the enemy soldiers
	 jnz @@white
	 jmp @@black
@@white:
     
	 
	 
@@up_row1:	
     cmp [board+si],2
	 jne @@loop1
     mov [white_win],1
	 mov [finish],1
@@loop1:	 
     inc si
	 loop @@up_row1
;---------------


; check the last row
     mov cx,8
	 mov si, 0
     add si,56;move to the last row	 
@@down_row1:	
     cmp [board+si],3
	 jne @@loop2
     mov [black_win],1
	 mov [finish],1
@@loop2:	 
     inc si
	 loop @@down_row1
;-----------------
	 jmp @@ret
 
	 
	 
@@black:
	
@@up_row2:	
     cmp [Board+si],3
	 jne @@loop3
     mov [black_win],1
	 mov [finish],1
@@loop3:	 
     inc si
	 loop @@up_row2
;---------------


; check the last row
     mov cx,8
	 mov si, 0
     add si,56;move to the last row	 
@@down_row2:	
     cmp [board+si],2
	 jne @@loop4
     mov [white_win],1
	 mov [finish],1
@@loop4:	 
     inc si
	 loop @@down_row2
;----------------
	 jmp @@ret
	 
	 
	 
	 
@@ret:
     pop si
     pop cx
     ret
endp check_win



;================================================
; Description  : order the board that the up is white soldiers and the down are black soldiers
; Input        : 1. board a array in ds
; Output:         1. board a array in ds
; 			     2. turn a variable in ds
; Register Usage: si, cx
;================================================
proc game1
; put the first line
	 mov si, offset Board
	 mov cx,4
@@line1:
	 inc si
     mov [byte ptr si],2
	 inc si
	 loop @@line1
;-------------

; put the second line
	 mov cx,4
@@line2:
     mov [byte ptr si],2
	 inc si
	 inc si
	 loop @@line2
;---------------	 
	 
	 
; put the third line	 
	 mov cx,4
	 
@@line3:
	 inc si
     mov [byte ptr si],2
	 inc si
	 loop @@line3
;---------------

;two empty lines
	 mov cx,4
@@empty1:
     mov [byte ptr si],1
	 inc si
	 inc si
	 loop @@empty1
	 mov cx,4
@@empty2:
     inc si
     mov [byte ptr si],1
	 inc si
	 loop @@empty2
	mov cx,4
	
;------------------

; put the fourth line
@@line4:
     mov [byte ptr si],3
	 inc si
	 inc si
	 loop @@line4
	 
	 
;----------------

; put the fifth line
	mov cx,4
@@line5:
     inc si
    mov [byte ptr si],3
	inc si
	loop @@line5
;----------------

; put the sixth line
	mov cx,4
@@line6:
    mov [byte ptr si],3
	 inc si
	 inc si
	 loop @@line6
;-------------------

	 call draw_board
	 mov [turn],0; black is the bottom of the board
	 
	 ret
endp game1


;================================================
; Description  : order the board that the up is black soldiers and the down are white soldiers
; Input        : 1. board a array in ds
; Output:         1. board a array in ds
; 			     2. turn a variable in ds
; Register Usage: si, cx
;================================================
proc game2
	 mov si, offset Board
	 ;draw the first line of the black soldiers
	 mov cx,4
@@line1:
	 inc si
     mov [byte ptr si],3 ;color of soldier
	 inc si
	 loop @@line1
;draw the second line of the black soldiers
	 mov cx,4
@@line2:
     mov [byte ptr si],3;color of soldier
	 inc si
	 inc si
	 loop @@line2
;draw the third line of the black soldiers	 
	 mov cx,4
	 
@@line3:
	 inc si
     mov [byte ptr si],3;color of soldier
	 inc si
	 loop @@line3
; two rows space
	 mov cx,4
@@empty1:
     mov [byte ptr si],1; empty 
	 inc si
	 inc si
	 loop @@empty1
	 mov cx,4
@@empty2:
     inc si
     mov [byte ptr si],1; empty
	 inc si
	 loop @@empty2
;draw the first line of the white soldiers
	mov cx,4
@@line4:
     mov [byte ptr si],2;color of soldier
	 inc si
	 inc si
	 loop @@line4
	 mov cx,4
;draw the second line of the white soldiers
@@line5:
     inc si
     mov [byte ptr si],2;color of soldier
	 inc si
	 loop @@line5
	 mov cx,4
;draw the third line of the white soldiers
@@line6:
    mov [byte ptr si],2;color of soldier
	 inc si
	 inc si
	 loop @@line6
	 call draw_board
	 mov [turn],1; you white
	 
	 ret
endp game2


;================================================
; Description  : show the player's up movment options
; Input        : 1. cx the col of the press
; 			     2. dx the row of the press
; 			     3. turn a variable in ds
; Output:         1. board a array in ds
; 			     2. there_is_movment_option a variable in ds
;================================================
proc show_options_up
     push cx
     push dx
	 push ax
	 push bx
	 push si
	 push di
	 mov di,dx; save the value of dx
	 xor dx,dx; mov dx,0
	 cmp si,8; check if up momvment is possible by possision
	 jae @@check_last
	 jmp @@ret; if no exit

@@check_last:
	  cmp [W_last],1
	  je @@white
	  cmp [B_last],1
	  jne @@test
	  jmp @@black
 @@test:
     test [turn],00000001b; check which color is the enemy soldiers
	 jnz @@white
	 jmp @@black
@@white:
;calculate the place in the array of the press	 
     mov ax,cx
	 sub ax,15
	 mov bx,Rect_Right_Or_Left
	 div bx
	 mov si,ax
	 mov ax,di
	 sub ax,10
	 mov dx,0
	 mov bx, Rect_Down
	 div bx
	 mov dx,0
	 mov bx,8
	 mul bx
	 add si,ax
	 mov di,offset board
;-------------------------------------
	 cmp [board+si],2; check if pressed possible place
	 jne @@next
	 sub si,9; up left
     cmp [board+si],3; check eat option
     je @@check_possible_eat1
     cmp [board+si],2; check regular option 
     je @@skip_option
     cmp [board+si],1; if possible
	 jne @@skip_option
     mov [byte ptr board+si],4
     mov [there_is_movment_option],1;there is moving option	 
	 jmp @@skip_option
@@check_possible_eat1:
      push si
	  add si,9
      call eat_black_up
	  pop si
	  
	  
@@skip_option:	  
	  add si,2; up right
	  cmp [board+si],3; check eat option
      je @@check_possible_eat2
      cmp [board+si],2; check regular option
      je @@next
      cmp [board+si],1; if possible
	  jne @@next
      mov [byte ptr board+si],4	 
	   mov [there_is_movment_option],1;there is moving option	 
	  jmp @@next
@@check_possible_eat2:
      push si
	  add si,7
      call eat_black_up
	  pop si	  
@@next:
     jmp @@ret
@@black:	 
;calculate the place in the array of the press	 
	 mov ax,cx
	 sub ax,15
	 mov bx,Rect_Right_Or_Left
	 div bx
	 mov si,ax
	 mov ax,di
	 sub ax,10
	 mov dx,0
	 mov bx, Rect_Down
	 div bx
	 mov dx,0
	 mov bx,8
	 mul bx
	 add si,ax
	 mov di,offset board
;--------------------------------------------------

	 
	 cmp [board+si],3; check if pressed possible place
	 jne @@ret
	 sub si,9; up left
     cmp [board+si],2; check eat option
     je @@check_possible_eat3
     cmp [board+si],3; check regular option
     je @@skip_option2
     cmp [board+si],1; if posssible
	 jne @@skip_option2
     mov [byte ptr board+si],4	 
	 mov [there_is_movment_option],1;there is moving option	 
	 jmp @@skip_option2
@@check_possible_eat3:
      push si
	  add si,9
      call eat_white_up
	  pop si
	  
	  
@@skip_option2:	  
	  add si,2; up right
	  cmp [board+si],2; check eat option
      je @@check_possible_eat4
      cmp [board+si],3; check regular option
      je @@ret
      cmp [board+si],1; if posssible
	  jne @@ret
      mov [byte ptr board+si],4	
       mov [there_is_movment_option],1;there is moving option	 	  
	  jmp @@ret
@@check_possible_eat4:
      push si
	  add si,7
      call eat_white_up
	  pop si	  
@@ret:
     call show_option_in_random_place
     call draw_board
     pop di
     pop si
	 pop bx
     pop ax
     pop dx	 
	 pop cx
	 ret
endp show_options_up


;================================================
; Description  : show the player's down movment options
; Input        : 1. cx the col of the press
; 			     2. dx the row of the press
; 			     3. turn a variable in ds
; Output:         1. board a array in ds
; 			     2. there_is_movment_option a variable in ds
;================================================
proc show_options_down
     push cx
     push dx
	 push ax
	 push bx
	 push si
	 push di
	 mov di,dx; save the value of dx
	 mov dx,0
	 cmp si,56; check if up momvment is possible by possision
	 jb @@check_last
	 jmp @@ret

@@check_last:	 
	  cmp [W_last],1
	  je @@white
	  cmp [B_last],1
	  jne @@test
	  jmp @@black
 @@test:
     test [turn],00000001b; check which color is the enemy soldiers
	 jz @@white
	 jmp @@black
@@white:	 
;calculate the place in the array of the press	
     mov ax,cx
	 sub ax,15
	 mov bx,Rect_Right_Or_Left
	 div bx
	 mov si,ax
	 mov ax,di
	 sub ax,10
	 mov dx,0
	 mov bx, Rect_Down
	 div bx
	 mov dx,0
	 mov bx,8
	 mul bx
	 add si,ax
	 mov di,offset board
;-------------------------------------	 
	 cmp [board+si],2; check if pressed possible place
	 jne @@next
	 add si,9; down right
     cmp [board+si],3; check eat option
     je @@check_possible_eat1
     cmp [board+si],2; check regular option
     je @@skip_option
     cmp [board+si],1; if posssible
	 jne @@skip_option
     mov [byte ptr board+si],4	 
	  mov [there_is_movment_option],1;there is moving option	 
	 jmp @@skip_option
@@check_possible_eat1:
      push si
	  sub si,9
      call eat_black_down
	  pop si
@@skip_option:	  
	  sub si,2; down left
	  cmp [board+si],3; check eat option
      je @@check_possible_eat2
      cmp [board+si],2; check regular option
      je @@next
      cmp [board+si],1; if posssible
	  jne @@next
      mov [byte ptr board+si],4	 
	   mov [there_is_movment_option],1;there is moving option	 
	  jmp @@next
@@check_possible_eat2:
      push si
	  sub si,7
      call eat_black_down
	  pop si	  
@@next:
     jmp @@ret
@@black:
;calculate the place in the array of the press		 
	 mov ax,cx
	 sub ax,15
	 mov bx,Rect_Right_Or_Left
	 div bx
	 mov si,ax
	 mov ax,di
	 sub ax,10
	 mov dx,0
	 mov bx, Rect_Down
	 div bx
	 mov dx,0
	 mov bx,8
	 mul bx
	 add si,ax
	 mov di,offset board
;----------------------------------------	 
	 cmp [board+si],3; check if pressed possible place
	 jne @@ret
	 add si,9; down right 
     cmp [board+si],2; check eat option
     je @@check_possible_eat3
     cmp [board+si],3; check regular option
     je @@skip_option2
     cmp [board+si],1; if posssible	 
	 jne @@skip_option2
     mov [byte ptr board+si],4	
     mov [there_is_movment_option],1;there is moving option	 	 
	 jmp @@skip_option2
@@check_possible_eat3:
      push si
	  sub si,9
      call eat_white_down
	  pop si
@@skip_option2:	  
	  sub si,2; down left
	  cmp [board+si],2; check eat option
      je @@check_possible_eat4
      cmp [board+si],3; check regular option
      je @@ret
      cmp [board+si],1; if posssible
	  jne @@ret
      mov [byte ptr board+si],4
       mov [there_is_movment_option],1;there is moving option	 	  
	  jmp @@ret
@@check_possible_eat4:
      push si
	  sub si,7
      call eat_white_down
	  pop si	  
@@ret:
     call show_option_in_random_place
     call draw_board
     pop di
     pop si
	 pop bx
     pop ax
     pop dx
     pop cx	 
	 ret
endp show_options_down



;================================================
; Description  : show options for up eating black soldiers 
; Input        : 1. si place at the array
; 			     2. board a array in ds
; 			     3. turn a variable in ds
; Output:         1. board a array in ds
; 			     2. eat_option a variable in ds
; Register Usage: si
;================================================
proc eat_black_up
      cmp [board+si],3;break if the soldier is in the same color
	  je @@jmp_ret
      cmp si,9; break if the soldier is in the first row
	  jb @@jmp_ret
      sub si,9;check possible eat from the up left
	  cmp [board+si],3; check if there is enemy soldier
	  je @@ok; if no break
	  sub si,9
	  jmp @@skip_option
@@ok:	  
	  mov [direction],0; if yes mov direction 0 to keep the laws of the movment
	  sub si,9;left up
	  cmp [board+si],1;possible to eat
	  jne @@other_option1 
	  cmp [direction],0; if no in the direction break
	  jne @@other_option1 
	  mov [byte ptr board+si],4	;if yes enter the option 
	  mov [eat_option],1;there is a option to eat
      push si	  
	  call eat_black_up; check double eat
	  pop si
	  mov [direction],0; if yes mov direction 0 to keep the laws of the movment
	  jmp @@other_option1
@@jmp_ret:; to big jmp
     jmp @@ret
@@other_option1:
      
	  cmp [direction],1; if no in the direction break
	  jne @@skip_option 
	  add si,2;right up
	  cmp [board+si],1;possible to eat
	  je @@ok2; if no break 
	  add si,2
	  jmp @@skip_option
@@ok2:
      mov [byte ptr board+si],4	;if yes enter the option 
      mov [eat_option],1;there is a option to eat	  
	  push si
	  call eat_black_up; check double eat	  
	  pop si
	  sub si,2
@@skip_option:	  
	  add si,11; keep si after the proc and mov it to the up right
	  
	  cmp [board+si],3; check if there is enemy soldier
	  jne @@ret
	  mov [direction],1; if yes mov direction 1 to keep the laws of the movment
	  sub si,9
	  
	  cmp [board+si],1;possible to eat
	  jne @@other_option2
	  cmp [direction],0; if no in the direction break
	  jne @@other_option2
      mov [byte ptr board+si],4;if yes enter the option
	  mov [eat_option],1;there is a option to eat
      push si	  
	  call eat_black_up; check double eat
	  pop si
	  mov [direction],1; if yes mov direction 1 to keep the laws of the movment
@@other_option2:	
       
	  add si,2;right up
	  cmp [board+si],1;possible to eat
	  jne @@ret	
	  cmp [direction],1; if no in the direction break
	  jne @@ret 
      mov [byte ptr board+si],4	 ;if yes enter the option 
	  mov [eat_option],1;there is a option to eat
	  push si
	  call eat_black_up; check double eat
	  pop si
@@ret:	 
	 
	 
	 
	 
	 ret
endp eat_black_up


;================================================
; Description  : show options for up eating white soldiers 
; Input        : 1. si place at the array
; 			     2. board a array in ds
; 			     3. turn a variable in ds
; Output:         1. board a array in ds
; 			     2. eat_option a variable in ds
; Register Usage: si
;================================================
proc eat_white_up
      cmp [board+si],2;break if the soldier is in the same color
	  je @@jmp_ret
      cmp si,9; break if the soldier is in the first row
	  jb @@jmp_ret
      sub si,9;check possible eat from the up left
	  cmp [board+si],2; check if there is enemy soldier
	  je @@ok; if no break
	  sub si,9
	  jmp @@skip_option
@@ok:	  
	  mov [direction],0; if yes mov direction 0 to keep the laws of the movment
	  sub si,9;left up
	  cmp [board+si],1;possible to eat
	  jne @@other_option1 
	  cmp [direction],0; if no in the direction break
	  jne @@other_option1 
	  mov [byte ptr board+si],4	;if yes enter the option 
	  mov [eat_option],1;there is a option to eat
      push si	  
	  call eat_white_up; check double eat
	  pop si
	  mov [direction],0; if yes mov direction 0 to keep the laws of the movment
	  jmp @@other_option1
@@jmp_ret:; to big jmp
     jmp @@ret
@@other_option1:
      
	  cmp [direction],1; if no in the direction break
	  jne @@skip_option 
	  add si,2;right up
	  cmp [board+si],1;possible to eat
	  je @@ok2; if no break 
	  add si,2
	  jmp @@skip_option
@@ok2:
      mov [byte ptr board+si],4	;if yes enter the option 
      mov [eat_option],1;there is a option to eat	  
	  push si
	  call eat_white_up; check double eat	  
	  pop si
	  sub si,2
@@skip_option:	  
	  add si,11; keep si after the proc and mov it to the up right
	  
	  cmp [board+si],2; check if there is enemy soldier
	  jne @@ret
	  mov [direction],1; if yes mov direction 1 to keep the laws of the movment
	  sub si,9
	  
	  cmp [board+si],1;possible to eat
	  jne @@other_option2
	  cmp [direction],0; if no in the direction break
	  jne @@other_option2
      mov [byte ptr board+si],4;if yes enter the option
	  mov [eat_option],1;there is a option to eat
      push si	  
	  call eat_white_up; check double eat
	  pop si
	  mov [direction],1; if yes mov direction 1 to keep the laws of the movment
@@other_option2:	
       
	  add si,2;right up
	  cmp [board+si],1;possible to eat
	  jne @@ret	
	  cmp [direction],1; if no in the direction break
	  jne @@ret 
      mov [byte ptr board+si],4	 ;if yes enter the option 
	  mov [eat_option],1;there is a option to eat
	  push si
	  call eat_white_up; check double eat
	  pop si
@@ret:	 
	 
	 
	 
	 
	 ret
endp eat_white_up

;================================================
; Description  : show options for down eating white soldiers 
; Input        : 1. si place at the array
; 			     2. board a array in ds
; 			     3. turn a variable in ds
; Output:         1. board a array in ds
; 			     2. eat_option a variable in ds
; Register Usage: si
;================================================
proc eat_white_down
       cmp [board+si],2; break if the soldier is in the same color
	  je @@jmp_ret
      cmp si,55; break if the soldier is in the last row
	  ja @@jmp_ret
	  ;check possible eat from the down right
      add si,9
	  cmp [board+si],2; check if there is enemy soldier
	  je @@ok; if no break
	  add si,9
	  jmp @@skip_option
@@ok:
	  mov [direction],0; if yes mov direction 0 to keep the laws of the movment
	  add si,9;right down
	  cmp [board+si],1;possible to eat
	  jne @@other_option1; if no break 
	  cmp [direction],0; if no in the direction break
	  jne @@other_option1 
	  mov [byte ptr board+si],4	 ;if yes enter the option
	  mov [eat_option],1;there is a option to eat
      push si	  
	  call eat_white_down; check double eat
	  pop si
	  mov [direction],0; if yes mov direction 0 to keep the laws of the movment
	  jmp @@other_option1
@@jmp_ret:; to big jmp
     jmp @@ret
@@other_option1:
      
	  cmp [direction],1; if no in the direction break
	  jne @@skip_option 
	  sub si,2;left down
	  cmp [board+si],1;possible to eat
	  je @@ok2; if no break 
	  add si,2
	  jmp @@skip_option
@@ok2:
      mov [byte ptr board+si],4	  ;if yes enter the option
	  mov [eat_option],1;there is a option to eat
	  push si
	  call eat_white_down; check double eat	  
	  pop si
	  add si,2;left down
@@skip_option:	  
	  sub si,11; keep si after the proc
	  cmp [board+si],2; check if there is enemy soldier
	  jne @@ret; if no break
	  mov [direction],1; if yes mov direction 1 to keep the laws of the movmen
	  add si,9;right down
	  cmp [board+si],1;possible to eat
	  jne @@other_option2; if no break 
	  cmp [direction],0; if no in the direction break
	  jne @@other_option2
      mov [byte ptr board+si],4;if yes enter the option
	  mov [eat_option],1;there is a option to eat
      push si	  
	  call eat_white_down; check double eat	
	  pop si
	  mov [direction],1; mov direction 1 to keep the laws of the movment
@@other_option2:	  
	  sub si,2;left down
	  cmp [board+si],1;possible to eat
	  jne @@ret	
	  cmp [direction],1; if no in the direction break
	  jne @@ret 
      mov [byte ptr board+si],4;if yes enter the option	 
      mov [eat_option],1;there is a option to eat	  
	  push si
	  call eat_white_down; check double eat	
	  pop si
@@ret:	 
	 
	 
	 
	 
	 ret
endp eat_white_down

;================================================
; Description  : show options for down eating black soldiers 
; Input        : 1. si place at the array
; 			     2. board a array in ds
; 			     3. turn a variable in ds
; Output:         1. board a array in ds
; 			     2. eat_option a variable in ds
; Register Usage: si
;================================================
proc eat_black_down
      cmp [board+si],3; break if the soldier is in the same color
	  je @@jmp_ret
      cmp si,55; break if the soldier is in the last row
	  ja @@jmp_ret
	  ;check possible eat from the down right
      add si,9
	  cmp [board+si],3; check if there is enemy soldier
	  je @@ok; if no break
	  add si,9
	  jmp @@skip_option
@@ok:
	  mov [direction],0; if yes mov direction 0 to keep the laws of the movment
	  add si,9;right down
	  cmp [board+si],1;possible to eat
	  jne @@other_option1; if no break 
	  cmp [direction],0; if no in the direction break
	  jne @@other_option1 
	  mov [byte ptr board+si],4	 ;if yes enter the option
	  mov [eat_option],1;there is a option to eat
      push si	  
	  call eat_black_down; check double eat
	  pop si
	  mov [direction],0; if yes mov direction 0 to keep the laws of the movment
	  jmp @@other_option1
@@jmp_ret:; to big jmp
     jmp @@ret
@@other_option1:
      
	  cmp [direction],1; if no in the direction break
	  jne @@skip_option 
	  sub si,2;left down
	  cmp [board+si],1;possible to eat
	  je @@ok2; if no break 
	  add si,2
	  jmp @@skip_option
@@ok2:
      mov [byte ptr board+si],4	  ;if yes enter the option
	  mov [eat_option],1;there is a option to eat
	  push si
	  call eat_black_down; check double eat	  
	  pop si
	  add si,2;left down
@@skip_option:	  
	  sub si,11; keep si after the proc
	  cmp [board+si],3; check if there is enemy soldier
	  jne @@ret; if no break
	  mov [direction],1; if yes mov direction 1 to keep the laws of the movmen
	  add si,9;right down
	  cmp [board+si],1;possible to eat
	  jne @@other_option2; if no break 
	  cmp [direction],0; if no in the direction break
	  jne @@other_option2
      mov [byte ptr board+si],4;if yes enter the option
	  mov [eat_option],1;there is a option to eat
      push si	  
	  call eat_black_down; check double eat	
	  pop si
	  mov [direction],1; mov direction 1 to keep the laws of the movment
@@other_option2:	  
	  sub si,2;left down
	  cmp [board+si],1;possible to eat
	  jne @@ret	
	  cmp [direction],1; if no in the direction break
	  jne @@ret 
      mov [byte ptr board+si],4;if yes enter the option	 
      mov [eat_option],1;there is a option to eat	  
	  push si
	  call eat_black_down; check double eat	
	  pop si
@@ret:	 
	 
	 
	 
	 
	 ret
endp eat_black_down



;================================================
; Description  : print the board
; Input        : 1. last_board a array in ds
; 			     2. board a array in ds
; Output:         1. board a array in ds
; 			     2. last_board a array in ds
;                3. screen
; Register Usage: si, di, cx, ax, dx, bx 
;================================================
proc draw_board
     mov ax,2
	 int 33h
     mov si,offset board
	 mov di, offset last_board
	 mov cx,64;number of gray square
	 mov [BmpLeft],15
	 mov [BmpTop],10
@@draw:	 
     push cx
	 push bx 
	 mov bl, [si];board
	 mov bh, [di];last board 
	 cmp bl,bh
	 je @@continue
	 cmp bl,1;gray square
	 jne @@soldiers
	 push si
	 push di
	 push bx
	 mov dx,offset Erase
	 mov [BmpColSize], BMP_WIDTH
	 mov [BmpRowSize] ,BMP_HEIGHT 
	 call OpenShowBmp
	 pop bx
	 pop di
	 pop si
	 jmp @@continue
@@soldiers:	 
	 cmp bl,2;white solier
	 jne @@bsoldier
	 push si
	 push di
	 push bx
	 mov dx,offset Wsoldier
	 mov [BmpColSize], BMP_WIDTH
	 mov [BmpRowSize] ,BMP_HEIGHT 
	 call OpenShowBmp
	 pop bx
	 pop di
	 pop si
	 jmp @@continue
@@bsoldier:	 
	 cmp bl, 3; black soldier
	 jne @@show_option
	 push si
	 push di
	 push bx
     mov dx,offset Bsoldier
	 mov [BmpColSize], BMP_WIDTH
	 mov [BmpRowSize] ,BMP_HEIGHT 
	 call OpenShowBmp
	 pop bx
	  pop di
	  pop si
	 jmp @@continue
@@show_option:
	 cmp bl, 4; option
	 jne @@continue
	 push si
	 push di
	 push bx
     mov dx,offset options
	 mov [BmpColSize], BMP_WIDTH
	 mov [BmpRowSize] ,BMP_HEIGHT 
	 call OpenShowBmp
	 pop bx
	 pop di
	 pop si
     
@@continue:	 
	 pop bx
	 
	 
	 pop cx
;down a row
	 cmp [cnt],7
	 jne  @@dont_do
	 mov [BmpLeft],15
	 add [BmpTop],Rect_Down
	 mov [cnt],0
	 push ax
	 mov al,[si]
	 mov [di],al
	 pop ax
	 inc si
	 inc di
	 dec cx
	 cmp cx,0
	 je @@ret
	 jmp @@draw
;----------------------

; mov right in the row
@@dont_do:	 
	 add [BmpLeft],Rect_Right_Or_Left
	 push ax
	 mov al,[si]
	 mov [di],al
	 pop ax
	 inc si
	 inc di
	 inc [cnt]
	 dec cx
	 cmp cx,0
	 je @@ret
	 jmp @@draw
;-----------------------------



@@ret:
     ; show mouse
     mov ax,1
	 int 33h
     ret
endp draw_board

;================================================
; Description  : close moving options with call to draw_board
; Input        : 1. board a array in ds
; Output:         1. board a array in ds
;================================================
proc close_options
     push cx
	 push si
     mov cx,64; nuber squares
	 mov si,0
@@check:
     cmp [board+si],4; check if there is open option
	 jne @@continue;if no skip the next line
     mov [board+si],1; if yes remove the blue circle
@@continue:
     inc si; next place
     loop @@check 
	 call draw_board
	 
	 pop si
	 pop cx
	 ret
endp close_options

;================================================
; Description  : close moving options without call to draw_board
; Input        : 1. board a array in ds
; Output:         1. board a array in ds
;================================================
proc special_close_options
     push cx
	 push si
     mov cx,64; nuber squares
	 mov si,0
@@check:
     cmp [board+si],4; check if there is open option
	 jne @@continue;if no skip the next line
     mov [board+si],1; if yes remove the blue circle
@@continue:
     inc si; next place
     loop @@check 
	 
	 pop si
	 pop cx
	 ret
endp special_close_options



;================================================
; Description  : show the player's up movment options
; Input        : 1. si the is the moving soldier
; 			     2. turn a variable in ds
; Output:         1. board a array in ds
; 			     2. there_is_movment_option a variable in ds
;================================================
proc special_show_options_up
     push cx
     push dx
	 push ax
	 push bx
	 push si
	 push di
	 mov di,dx

	 jae @@check_last; check if last soldier
	 jmp @@ret

@@check_last:
	  cmp [W_last],1
	  je @@white
	  cmp [B_last],1
	  jne @@test
	  jmp @@black
 @@test:
     test [turn],00000001b; check which color is the enemy soldiers
	 jnz @@white
	 jmp @@black
@@white:	 
     
	 cmp [board+si],2; ; check if pressed possible place
	 jne @@next
	 sub si,9; up right
     cmp [board+si],3; check eat option 
     je @@check_possible_eat1
     cmp [board+si],2; check regular option
     je @@skip_option
     cmp [board+si],1; if possible
	 jne @@skip_option
     mov [byte ptr board+si],4
     mov [there_is_movment_option],1;there is moving option	 
	 jmp @@skip_option
@@check_possible_eat1:
      push si
	  add si,9
      call eat_black_up
	  pop si
@@skip_option:	  
	  add si,2; up left
	  cmp [board+si],3; check eat optio
      je @@check_possible_eat2
      cmp [board+si],2; check regular option
      je @@next
      cmp [board+si],1; if possible
	  jne @@next
      mov [byte ptr board+si],4	 
	   mov [there_is_movment_option],1;there is moving option	 
	  jmp @@next
@@check_possible_eat2:
      push si
	  add si,7
      call eat_black_up
	  pop si	  
@@next:
     jmp @@ret
@@black:	 
	 
	 
	 cmp [board+si],3; check if pressed possible place
	 jne @@ret
	 sub si,9;up right
     cmp [board+si],2; eat option
     je @@check_possible_eat3
     cmp [board+si],3; regular option
     je @@skip_option2
     cmp [board+si],1; if possible 
	 jne @@skip_option2
     mov [byte ptr board+si],4	 
	 mov [there_is_movment_option],1;there is moving option	 
	 jmp @@skip_option2
@@check_possible_eat3:
      push si
	  add si,9
      call eat_white_up
	  pop si
@@skip_option2:	  
	  add si,2; up left
	  cmp [board+si],2; eat option
      je @@check_possible_eat4
      cmp [board+si],3; regular option
      je @@ret
      cmp [board+si],1; if possible
	  jne @@ret
      mov [byte ptr board+si],4	
       mov [there_is_movment_option],1;there is moving option	 	  
	  jmp @@ret
@@check_possible_eat4:
      push si
	  add si,7
      call eat_white_up
	  pop si	  
@@ret:
     pop di
     pop si
	 pop bx
     pop ax
     pop dx	 
	 pop cx
	 ret
endp special_show_options_up


;================================================
; Description  : show the player's down movment options
; Input        : 1. si the is the moving soldier
; 			     2. turn a variable in ds
; Output:         1. board a array in ds
; 			     2. there_is_movment_option a variable in ds
;================================================
proc special_show_options_down
     push dx
	 push ax
	 push bx
	 push si
	 push di
	 push cx
	 cmp si,56
	 jb @@check_last
	 jmp @@ret

@@check_last:	 
	  cmp [W_last],1
	  je @@white
	  cmp [B_last],1
	  jne @@test
	  jmp @@black
 @@test:
     test [turn],00000001b; check which color is the enemy soldiers
	 jz @@white
	 jmp @@black
@@white:	 
     
	 cmp [board+si],2; check if pressed possible place
	 jne @@next
	 add si,9; down right
     cmp [board+si],3; eat option
     je @@check_possible_eat1
     cmp [board+si],2; regular option
     je @@skip_option
     cmp [board+si],1; if possible
	 jne @@skip_option
     mov [byte ptr board+si],4	 
	  mov [there_is_movment_option],1;there is moving option	 
	 jmp @@skip_option
@@check_possible_eat1:
      push si
	  sub si,9
      call eat_black_down
	  pop si
@@skip_option:	  
	  sub si,2; down left
	  cmp [board+si],3; eat option
      je @@check_possible_eat2
      cmp [board+si],2; regular option
      je @@next
      cmp [board+si],1; if possible
	  jne @@next
      mov [byte ptr board+si],4	 
	   mov [there_is_movment_option],1;there is moving option	 
	  jmp @@next
@@check_possible_eat2:
      push si
	  sub si,7
      call eat_black_down
	  pop si	  
@@next:
     jmp @@ret
@@black:	 
	 
	 
	 cmp [board+si],3; check if pressed possible place
	 jne @@ret
	 add si,9; down right
     cmp [board+si],2; eat option
     je @@check_possible_eat3
     cmp [board+si],3; regular option
     je @@skip_option2
     cmp [board+si],1; if possible	 
	 jne @@skip_option2
     mov [byte ptr board+si],4	
     mov [there_is_movment_option],1;there is moving option	 	 
	 jmp @@skip_option2
@@check_possible_eat3:
      push si
	  sub si,9
      call eat_white_down
	  pop si
@@skip_option2:	  
	  sub si,2; down left
	  cmp [board+si],2; eat option
      je @@check_possible_eat4
      cmp [board+si],3; regular option
      je @@ret
      cmp [board+si],1; if possible
	  jne @@ret
      mov [byte ptr board+si],4
       mov [there_is_movment_option],1;there is moving option	 	  
	  jmp @@ret
@@check_possible_eat4:
      push si
	  sub si,7
      call eat_white_down
	  pop si	  
@@ret:
     pop cx
     pop di
     pop si
	 pop bx
     pop ax
     pop dx	 
	 ret
endp special_show_options_down

;================================================
; Description  : move up eating black soldiers 
; Input        : 1. si the eating soldier
; 			     2. di the place of the eating
; 			     3. board a array in ds
; Output:         1. board a array in ds
; 			     2. W_eaten a variable in ds
; 			     3. B_eaten a variable in ds
;================================================
proc special_eat_black_up_move
      ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
	  cmp [board+si],2
	  je @@possible
	  jmp @@ret
@@possible:
      push si
	  push di
	  pop si
	  pop di
	  cmp [board+si],4;check if this is possible
	  je @@optional
      jmp @@ret; if no break
@@optional:	  
	  cmp si,di; if same place breakmo
	  jne @@continue; if not continue
	  jmp @@ret;break
@@continue:	  
	  push di
	  sub di,si;check the distance
	  cmp di,14; if yes mov up right 
	  jne @@other_option
	  pop di
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],2;move the soldier
	  inc [B_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@other_option:	  
      cmp di,18; if yes mov up left 
	  jne @@more_then_one_eat; if no there is more then one to eat
	  pop di
	  mov [board+di],1; delete the image in the current square 
	  sub di,9; up left
	  mov [board+di],1; delete the image in the current square 
	  sub di,9; up left
	  mov [board+di],2;move the soldier
	  inc [B_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@more_then_one_eat:
      add [B_eaten],1; inc the number of the eaten soldiers
	  pop di
	  sub di,18; twice up left
	  cmp [board+di],4
      je @@left_movment
      add di,4; twice up right
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,14
      push si
	  push di
	  pop si
	  pop di; prepare to the next call
	  mov [board+si],2;move the soldier
      call special_eat_black_up_move; more_then_one_eat		
      jmp @@ret	 ;break 
@@left_movment:	  
	  add di,9
	  mov [board+di],1; delete the image in the current square 
	  add di,9
	  mov [board+di],1; delete the image in the current square 
	  sub di,18; twice up left
     push si
	  push di
	  pop si
	  pop di; prepare to the next call
	  mov [board+si],2;move the soldier
      call special_eat_black_up_move; more_then_one_eat	
	  
	  
@@ret:	 
      ; keeps the values of the regesters
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp special_eat_black_up_move

;================================================
; Description  : move up eating black soldiers 
; Input        : 1. si the eating soldier
; 			     2. dx the row of the press
; 			     3. cx the col of the press
; 			     4. board a array in ds
; Output:         1. board a array in ds
; 			     2. W_eaten a variable in ds
; 			     3. B_eaten a variable in ds
;================================================
proc eat_black_up_move
      ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
	  cmp [board+si],2
	  je @@possible
	  jmp @@ret
@@possible:
      ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov di,si
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov si,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add si,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+si],4;check if this is possible
	  je @@optional
      jmp @@ret; if no break
@@optional:	  
	  cmp si,di; if same place break
	  jne @@continue; if not continue
	  jmp @@ret;break
@@continue:	  
	  push di
	  sub di,si;check the distance
	  cmp di,14; if yes mov up right 
	  jne @@other_option
	  pop di
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],2;move the soldier
	  inc [B_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@other_option:	  
      cmp di,18; if yes mov up left 
	  jne @@more_then_one_eat; if no there is more then one to eat
	  pop di
	  mov [board+di],1; delete the image in the current square 
	  sub di,9; up left
	  mov [board+di],1; delete the image in the current square 
	  sub di,9; up left
	  mov [board+di],2;move the soldier
	  inc [B_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@more_then_one_eat:
      add [B_eaten],1; inc the number of the eaten soldiers
	  pop di
	  sub di,18; twice up left
	  cmp [board+di],4
      je @@left_movment
      add di,4; twice up right
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,14
      mov si,di; prepare to the next call
	  mov [board+si],2;move the soldier
      call eat_black_up_move; more_then_one_eat		
      jmp @@ret	 ;break 
@@left_movment:	  
	  add di,9
	  mov [board+di],1; delete the image in the current square 
	  add di,9
	  mov [board+di],1; delete the image in the current square 
	  sub di,18; twice up left
      mov si,di; prepare to the next call
	  mov [board+si],2;move the soldier
      call eat_black_up_move; more_then_one_eat	
	  
	  
@@ret:	 
      ; keeps the values of the regesters
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp eat_black_up_move

;================================================
; Description  : check if there is burn of black soldier
; Input        : 1. turn a variable in ds
; 			     2. eat_option a variable in ds
; 			     3. burn a variable in ds
; 			     4. board a array in ds
; Output:         
; 			     1. could_eat a variable in ds
;================================================
proc check_burn_black
      push cx
	  push si
	  call special_close_options
      mov cx,64; number of sqaures
	  mov si,0; the start of the array
@@check:
      cmp [board+si],3
	  jne @@cant_be
      push si
      test [turn],00000001b; check which color is the enemy soldiers
	  jz @@up
	  cmp si,55
	  ja @@down
      call eat_white_down
	  jmp @@down
@@up:	  
      cmp si,8
      jb @@down
	  call eat_white_up
@@down:	  
      pop si
      cmp [eat_option],1
      jne @@cant_be	  
      mov [could_eat],si
	  mov [burn],1
      
@@cant_be:
     mov [eat_option],0
     inc si; next place
     loop @@check	 
	  
	  
@@ret:	
	  pop si
	  pop cx
	  ret
endp check_burn_black

;================================================
; Description  : check if there is burn of white soldier
; Input        : 1. turn a variable in ds
; 			     2. eat_option a variable in ds
; 			     3. burn a variable in ds
; 			     4. board a array in ds
; Output:         
; 			     1. could_eat a variable in ds
;================================================
proc check_burn_white
      push cx
	  push si
	  call special_close_options
      mov cx,64; number of sqaures
	  mov si,0; the start of the array
@@check:
      cmp [board+si],2
	  jne @@cant_be
      push si
      test [turn],00000001b; check which color is the enemy soldiers
	  jnz @@up
	  cmp si,55
	  ja @@down
      call eat_black_down
	  jmp @@down
@@up:
      cmp si,8
      jb @@down	  
	  call eat_black_up
@@down:	 
      pop si 
      cmp [eat_option],1
      jne @@cant_be	  
      mov [could_eat],si
	  mov [burn],1
      
@@cant_be:
     mov [eat_option],0
     inc si; next place
     loop @@check	 
	  
	  
@@ret:	  
	  pop si
	  pop cx
	  ret
endp check_burn_white

;================================================
; Description  : the thinking of the minimax algorithem
; Input        : 1. turn a variable in ds
; 			     2. B_eaten a variable in ds
; 			     3. W_eaten variable in ds
; 			     4. board a array in ds
; 			     5. start_B_eaten a variable in ds
; 			     6. start_W_eaten variable in ds
; Output:         
; 			     1. current_score a variable in ds
;================================================
proc get_score
     push ax
	 push cx
	 test [turn],00000001b; check the color of the computer
	 jz @@black
	 jmp @@white
@@black:
     cmp [B_eaten],12; check if all the soldiers were eaten
	 jne @@next_check
	 mov [current_score],30000
	 jmp @@ret
	 
@@next_check:
     cmp [W_eaten],12; check if all the soldiers were eaten
	 jne @@next_check2
	 mov [current_score],-30000
	 jmp @@ret
@@next_check2:
     call check_win
     cmp [finish],1; check if the game ended
     jne @@no_special_situation
     cmp [black_win],1
	 je @@lose
	 mov [current_score],30000
	 jmp @@ret
@@lose:	 
     mov [current_score],-30000
	 jmp @@ret

@@no_special_situation:	 
; calculate the change in the soldiers number and use it for the thinking
     mov ax,13
	 mov cx,20
@@mul_by_20_1:	 
     sub al,[start_B_eaten]
	 add al,[B_eaten]
     
	 sub al,[start_W_eaten]
	 add al,[W_eaten]
	 loop @@mul_by_20_1	 
	 jmp @@final
;----------------------------	 
@@white:
     cmp [W_eaten],12; check if all the soldiers were eaten
	 jne @@next_check3
	 mov [current_score],30000
	 jmp @@ret
	 
@@next_check3:
     cmp [B_eaten],12; check if all the soldiers were eaten
	 jne @@next_check4
	 mov [current_score],-30000
	 jmp @@ret
@@next_check4:
     call check_win
     cmp [finish],1; check if the game ended
     jne @@no_special_situation2
     cmp [white_win],1
	 je @@lose2
	 mov [current_score],30000
	 jmp @@ret
@@lose2:	 
     mov [current_score],-30000
	 jmp @@ret

@@no_special_situation2:	
; calculate the change in the soldiers number and use it for the thinking
     mov ax,13	
	 mov cx,20
@@mul_by_20_2:	
     sub al,[start_W_eaten]
	 add al,[W_eaten] 
	 
	 sub al,[start_B_eaten]
	 add al,[B_eaten]
	 loop @@mul_by_20_2
	 jmp @@final
;-----------------------------------	
	 

@@final:
      mov cx,10
	  push di
	  shr di,3
	  add ax, di
	  pop di
      mov [current_score],ax
     
@@ret:	 
     mov [finish],0
	 mov [black_win],0
	 mov [white_win],0
	 pop cx
	 pop ax
	 ret
endp get_score




;================================================
; Description  :  the minimax algorithem
; Input        : 1. turn a variable in ds
; 			     2. B_eaten a variable in ds
; 			     3. W_eaten variable in ds
; 			     4. board a array in ds
; 			     5. minimax_save a array in ds
; 			     6. save_index a variable in ds
; 			     7. finish a variable in ds
; 			     8. start_W_eaten variable in ds
; Output:         
; 			     1. best_mov_di a variable in ds
; 			     2. best_mov_si a variable in ds
;================================================
proc MiniMax
     push bp
	 mov bp,sp
	 cmp sp,52
; check if if finished
	 ja @@no_stack_overflow
	 call get_score
	 mov ax,[current_score]
	 mov RET_MINIMAX,ax
	 jmp @@stack_overflow_ret
@@no_stack_overflow:	 
	 sub sp,4
	 push si
	 push di
	 push cx
	 cmp [depth],0
	 jne @@next_check
	 call get_score
	 mov ax,[current_score]
	 mov RET_MINIMAX,ax
	 jmp @@ret
@@next_check:	 
	 call check_win
	 cmp [finish],1
	 jne @@no_end
	 mov [finish],0
	 call get_score
	 mov ax,[current_score]
	 mov RET_MINIMAX,ax
	 jmp @@ret
;---------------------------------------------
@@no_end:	 
     test [turn],00000001b; check which color is the enemy soldiers
	 jnz @@computer_is_black
	 test temp_player,00000001b
	 jnz @@player_turn 
	 jmp @@computer_turn
@@computer_is_black:
     test temp_player,00000001b
	 jz @@player_turn 
	 jmp @@computer_turn
@@player_turn:
     mov SCORE,10000
     test [turn],00000001b; check which color is the enemy soldiers
	 jnz @@white1
	 jmp @@black1
@@white1:
     
     mov cx,64
	 mov si, 0
@@again1:
     call special_close_options
     cmp [board+si],2
	 je @@ok1
	 jmp @@loop1
@@ok1:	 
     
	 
	 push cx
	 push si
     
	 mov cx,64
	 mov di, 0
@@inside1:	
     call special_show_options_up; check if possible
	 cmp [board+di],4
	 je @@fine
	 jmp @@loop2
@@fine:
	 push si
	 sub si,di
	 cmp si,9 
	 jbe @@regular_move
	 pop si 
	 mov al,[W_eaten]
	 mov ah,[B_eaten]
	 mov SAVE_EATEN,ax; save the numbers of the eaten soldiers
	 push cx
	 push si
	 push di
	 call special_close_options
	 call load; keeps the board
	 call special_show_options_up
	 call special_eat_black_up_move
	 
	 pop di
	 pop si
	 pop cx
;prepare the next call
	 push [depth]
	 dec [depth]
	 push ax
	 mov ax,temp_player
	 inc ax
	 push ax
	 call MiniMax
; get the return value from the call	 
	 pop ax
	 mov RET_MINIMAX,ax
	 pop [depth]; save the value 
	 mov ax,SAVE_EATEN; save the numbers of the eaten soldiers
	 mov [W_eaten],al
	 mov [B_eaten],ah
	 call reload;undo the move
;check if the min is changed	 
	 mov ax,RET_MINIMAX
	 cmp ax,SCORE
	 jl @@switch1
	 
	 jmp @@loop2
@@switch1:
     mov SCORE,ax
	 jmp @@loop2
 @@regular_move:
     pop si
	 call special_close_options
	 call load
	 call special_show_options_up
	 call special_move_up
;prepare the next call	 
	 push [depth]
	 dec [depth]
	 push ax
	 mov ax,temp_player
	 inc ax
	 push ax
	 call MiniMax
; get the return value from the call		 
	 pop ax
	 mov RET_MINIMAX,ax
	 pop [depth]; save the value
	 call reload;undo the move
;check if the min is changed		 
	 mov ax,RET_MINIMAX
	 cmp ax,SCORE
	 jl @@switch2
	 
	 jmp @@loop2
@@switch2:
	 
     mov SCORE,ax
	 jmp @@loop2
	 
	 
@@loop2:
     inc di
	 dec cx
	 cmp cx,0
	 je @@out1
     jmp @@inside1	 
@@out1:	 
	 pop si
	 pop cx
	
	 
	 
	 
	 
	 
@@loop1:
     inc si
     dec cx 
	 cmp cx,0
	 je @@continue1
	 jmp @@again1
@@continue1:	 
	 jmp @@ret
	 
	 
	 
@@black1:
     
	 mov cx,64
	 mov si, 0
@@again2:
     call special_close_options
     cmp [board+si],3
	 je @@ok2
	 jmp @@loop3
@@ok2:
     
	 
	 push cx
	 push si
	 	
	 mov cx,64
	 mov di, offset board
@@inside2:
	 call special_show_options_up

	 cmp [board+di],4
	 je @@fine2
	 
	 jmp @@loop4
@@fine2:
	 push si
	 sub si,di
	 cmp si,9 
	 jbe @@regular_move2
	 pop si 
	 mov al,[W_eaten]
	 mov ah,[B_eaten]
	 mov SAVE_EATEN,ax; save the numbers of the eaten soldiers
	 push cx
	 push si
	 push di
	 call special_close_options
	 call load; keeps the board
	 call special_show_options_up
	 call special_eat_white_up_move
	 call special_close_options
	 pop di
	 pop si
	 pop cx
;prepare the next call	 
	 push [depth]
	 dec [depth]
	 push ax
	 mov ax,temp_player
	 inc ax
	 push ax
	 call MiniMax
; get the return value from the call		 
	 pop ax
	 mov RET_MINIMAX,ax
	 pop [depth]; save the value
	 mov ax, SAVE_EATEN; save the numbers of the eaten soldiers
	 mov [W_eaten],al
	 mov [B_eaten],ah
	 call reload;undo the move
;check if the min is changed		 
	 mov ax,RET_MINIMAX
	 cmp ax,SCORE
	 jl @@switch3
	 
	 jmp @@loop4
@@switch3:
     mov SCORE,ax
	 jmp @@loop4
 @@regular_move2:
     pop si
	 call special_close_options
	 call load; keeps the board
	 call special_show_options_up
	 call special_move_up
	 call special_close_options
;prepare the next call	 
	 push [depth]
	 dec [depth]
	 push ax
	 mov ax,temp_player
	 inc ax
	 push ax
	 call MiniMax
; get the return value from the call		 
	 pop ax
     mov RET_MINIMAX,ax	 
	 pop [depth]; save the value
	 call reload;undo the move
;check if the min is changed	
	 mov ax,RET_MINIMAX
	 cmp ax,SCORE
	 jl @@switch4
	 
	 jmp @@loop4
@@switch4:
     mov SCORE,ax
	 jmp @@loop4
	 
	 
@@loop4:
     inc di
	 dec cx
	 cmp cx,0
	 je @@out2
     jmp @@inside2	 
@@out2: 
	 pop si
	 pop cx
	
	 
	 
	 
	 
	 
@@loop3:
     inc si
     dec cx
	 cmp cx,0
	 je @@continue2
	 jmp @@again2
@@continue2:	 
	 jmp @@ret
	 
	 
	 
	 
@@computer_turn:	 
	 mov SCORE,-10000
     test [turn],00000001b; check which color is the enemy soldiers
	 jz @@white2
	 jmp @@black2
@@white2:
     
     mov cx,64
	 mov si, 0
@@again3:
     call special_close_options
     cmp [board+si],2
	 je @@continue
	 jmp @@loop5
@@continue:	 
    
	 
	 push cx
	 push si
	 
	 mov cx,64
	 mov di, 0
@@inside3:
     
     call special_show_options_down
	 cmp [board+di],4
	 je @@ok4
	 jmp @@loop6
@@ok4:
	 push di
	 sub di,si
	 cmp di,9 
	 ja @@eat_move1
	 jmp @@regular_move3
@@eat_move1:	 
	 pop di 
	 mov al,[W_eaten]
	 mov ah,[B_eaten]
	 mov SAVE_EATEN,ax; save the numbers of the eaten soldiers
	 push cx
	 push si
	 push di
	 call special_close_options
	 call load; keeps the board
	 call special_show_options_down
	 call special_eat_black_down_move
	 pop di
	 pop si
	 pop cx
;prepare the next call
	 push [depth]
	 dec [depth]
	 push ax
	 mov ax,temp_player
	 inc ax
	 push ax
	 call MiniMax
; get the return value from the call		 
	 pop ax
	 mov RET_MINIMAX,ax
	 pop [depth]; save the value
	 mov ax,SAVE_EATEN; save the numbers of the eaten soldiers
	 mov [W_eaten],al
	 mov [B_eaten],ah
	 call reload;undo the move
;check if the max is changed	
	 mov ax,RET_MINIMAX
	 cmp ax,SCORE
	 jg @@switch5
	 
	 jmp @@loop6
@@switch5:
; avoid from wrong returned value
     call load
	 call mov_last_board_to_board
	 call special_show_options_down
	 cmp [board+di],4
	 jne @@before_loop6
	 cmp [board+si],2
	 jne @@before_loop6
@@next_checking1:
	 call reload
     mov SCORE,ax
     mov [best_mov_di],di
	 mov [best_mov_si],si
	 jmp @@loop6
 @@regular_move3:
     pop di
	 call special_close_options
	 call load; keeps the board
	 call special_show_options_down
	 call special_move_down
;prepare the next call
	 push [depth]
	 dec [depth]
	 push ax
	 mov ax,temp_player
	 inc ax
	 push ax
	 call MiniMax
; get the return value from the call		 
	 pop ax
	 mov RET_MINIMAX,ax
	 pop [depth]; save the value
	 call reload;undo the move
;check if the max is changed	
	 mov ax,RET_MINIMAX
	 cmp ax,SCORE
	 jg @@switch6
	 jmp @@loop6
@@switch6:
     call load
	 call mov_last_board_to_board
	 call special_show_options_down
	 cmp [board+di],4
	 jne @@before_loop6
	 cmp [board+si],2
	 jne @@before_loop6
	 call reload
     mov [best_mov_di],di
	 mov [best_mov_si],si
     mov SCORE,ax
	 jmp @@loop6
	 
@@before_loop6:
	 call reload
@@loop6:
     inc di
     dec cx 
     cmp cx,0
     je @@finish1	 
	 jmp @@inside3
	 
@@finish1:	 
	 pop si
	 pop cx
	 
	 jmp @@loop5
@@jmp_to_again3:	 
	 jmp @@again3
	 
@@loop5:
     inc si
     loop @@jmp_to_again3	 
	 jmp @@ret
	 
	 
	 
@@black2:	 
     
	 mov cx,64
	 mov si, 0
@@again4:
     call special_close_options
     cmp [board+si],3
	 je @@ok3
	 jmp @@loop7
@@ok3:
     
	 
	 push cx
	 push si
	 
	 mov cx,64
	 mov di, 0
@@inside4:
     call special_show_options_down
	 cmp [board+di],4
	 je @@fine3
	 jmp @@loop8
@@fine3:
	 push di
	 sub di,si
	 cmp di,9 
	 ja @@eat_move2
	 jmp @@regular_move4
@@eat_move2:
	 pop di
     mov al,[W_eaten]
	 mov ah,[B_eaten]
	 mov SAVE_EATEN,ax	; save the numbers of the eaten soldiers 
	 push cx
	 push si
	 push di
	 call special_close_options
	 call load; keeps the board
	 call special_show_options_down
	 call special_eat_white_down_move
	 pop di
	 pop si
	 pop cx
;prepare the next call	 
	 push [depth]
	 dec [depth]
	 push ax
	 mov ax,temp_player
	 inc ax
	 push ax
	 call MiniMax
; get the return value from the call		 
	 pop ax
	 mov RET_MINIMAX,ax
	 pop [depth]; save the value
	 mov ax,SAVE_EATEN; save the numbers of the eaten soldiers
	 mov [W_eaten],al
	 mov [B_eaten],ah
	 call reload;undo the move
;check if the max is changed	
	 mov ax,RET_MINIMAX
	 cmp ax,SCORE
	 jg @@switch7
	 
	 jmp @@loop8
@@switch7:
     call load
	 call mov_last_board_to_board
	 call special_show_options_down
	 cmp [board+di],4
	 jne @@before_loop8
	 cmp [board+si],3
	 je @@next_checking2
	 jmp @@before_loop8
@@next_checking2:
	 call reload
     mov [best_mov_di],di
	 mov [best_mov_si],si
     mov SCORE,ax
	 jmp @@loop8
 @@regular_move4:
     pop di
	 call special_close_options
	 call load; keeps the board
	 call special_show_options_down
	 call special_move_down
;prepare the next call	 
	 push [depth]
	 dec [depth]
	 push ax
	 mov ax,temp_player
	 inc ax
	 push ax
	 call MiniMax
; get the return value from the call	
	 pop ax
	 mov RET_MINIMAX,ax
	 pop [depth]; save the value
	 call reload;undo the move
;check if the max is changed	
	 mov ax,RET_MINIMAX
	 cmp ax,SCORE
	 jg @@switch8
	 
	 jmp @@loop8
@@switch8:
      call load
	 call mov_last_board_to_board
	 call special_show_options_down
	 cmp [board+di],4
	 jne @@before_loop8
	 cmp [board+si],3
	 jne @@before_loop8
	 call reload
     mov [best_mov_di],di
	 mov [best_mov_si],si
     mov SCORE,ax
	 jmp @@loop8
	 
@@before_loop8:
    call reload	 
@@loop8:
     
     inc di
	 dec cx
	 cmp cx,0
	 je @@out3
     jmp @@inside4	 
@@out3:	 
	 pop si
	 pop cx
	
	 
	 
	 
	 
	 
@@loop7:
     inc si
     dec cx
	 cmp cx,0
	 je @@continue4
	 jmp @@again4
@@continue4:	 
	 jmp @@ret
@@ret:	 
     pop cx
     pop di 
	 pop si
	 add sp,4
@@stack_overflow_ret:
	 pop bp
	 ret 2
endp MiniMax


;================================================
; Description  : call to minimax without parmeters
; Input        : 1. W_eaten a variable in ds
; 			     2. B_eaten a variable in ds
; 			     3. l_temp_player a variable in ds
;                4. start_W_eaten a variable in ds
; 			     5. start_B_eaten a variable in ds
; Output:         1. best_mov_di a variable in ds
; 			     2. best_mov_si a variable in ds
; Register Usage: si
;================================================
proc RecMiniMax
     push ax
	 push bx
	 push cx
	 push dx
	 push si
	 push di
;prepare to the call 
     mov al,[B_eaten]
	 mov [start_B_eaten],al
	 mov al,[W_eaten]
	 mov [start_W_eaten],al
	 push bx
	 mov ah,0
	 mov al,[l_temp_player]
	 push ax
	 call MiniMax
; get the return value from the call		 
	 pop cx
	 mov [there_is_movment_option],0
	 mov [finish],0
	 mov [black_win],0
	 mov [white_win],0
	 mov [eat_option],0
	 pop di
	 pop si
	 pop dx
	 pop cx
	 pop bx
	 pop ax
	 
	 
	 
	 ret
endp RecMiniMax




proc load
     push si
	 push ax
	 push cx
	 push dx
	 push di
	 mov cx,64; number of sqaures
	 mov di,0
	 mov si,[save_index]
@@mov:	 
     cmp si,0
	 jl @@ret
	 cmp si,640
	 jg @@ret
	 mov al,[board+di]
	 mov [minimax_save+si],al
	 inc si
	 inc di
	 loop @@mov

	 add [save_index],64
@@ret: 	 
	 pop di
	 pop dx
	 pop cx
	 pop ax
	 pop si
	 ret
endp load




proc reload
     push di
     push si
	 push ax
	 push cx
	 push dx
	 
	 mov cx,64; number of sqaures
	 mov di,63
	 mov si,[save_index]
	 dec si
@@mov:
     cmp si,0
	 jl @@ret
	 cmp si,640
	 jg @@ret	 
	 mov al,[minimax_save+si]
	 mov [board+di],al
	 dec si
	 dec di
	 loop @@mov

     
	 sub [save_index],64
@@ret: 	 
	 pop dx
	 pop cx
	 pop ax
	 pop si
	 pop di
	 ret
endp reload


;================================================
; Description  : move up eating white soldiers 
; Input        : 1. si the eating soldier
; 			     2. dx the row of the press
; 			     3. cx the col of the press
; 			     4. board a array in ds
; Output:         1. board a array in ds
; 			     2. W_eaten a variable in ds
; 			     3. B_eaten a variable in ds
;================================================
proc eat_white_up_move
      ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
	  cmp [board+si],3
	  je @@possible
	  jmp @@ret
@@possible:
      ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov di,si
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov si,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add si,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+si],4;check if this is possible
	  je @@optional
      jmp @@ret; if no break
@@optional:	  
	  cmp si,di; if same place break
	  jne @@continue; if not continue
	  jmp @@ret;break
@@continue:	  
	  push di
	  sub di,si;check the distance
	  cmp di,14; if yes mov up right 
	  jne @@other_option
	  pop di
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],3;move the soldier
	  inc [W_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@other_option:	  
      cmp di,18; if yes mov up left 
	  jne @@more_then_one_eat; if no there is more then one to eat
	  pop di
	  mov [board+di],1; delete the image in the current square 
	  sub di,9; up left
	  mov [board+di],1; delete the image in the current square 
	  sub di,9; up left
	  mov [board+di],3;move the soldier
	  inc [W_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@more_then_one_eat:
      add [W_eaten],1; inc the number of the eaten soldiers
	  pop di
	  sub di,18; twice up left
	  cmp [board+di],4
      je @@left_movment
      add di,4; twice up right
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,14
      mov si,di; prepare to the next call
	  mov [board+si],3;move the soldier
      call eat_white_up_move; more_then_one_eat		
      jmp @@ret	 ;break 
@@left_movment:	  
	  add di,9
	  mov [board+di],1; delete the image in the current square 
	  add di,9
	  mov [board+di],1; delete the image in the current square 
	  sub di,18; twice up left
      mov si,di; prepare to the next call
	  mov [board+si],3;move the soldier
      call eat_white_up_move; more_then_one_eat	
	  
	  
@@ret:	 
      ; keeps the values of the regesters
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp eat_white_up_move


;================================================
; Description  : move down eating white soldiers 
; Input        : 1. si the eating soldier
; 			     2. dx the row of the press
; 			     3. cx the col of the press
; 			     4. board a array in ds
; Output:         1. board a array in ds
; 			     2. W_eaten a variable in ds
; 			     3. B_eaten a variable in ds
;================================================
proc eat_white_down_move
      ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
	  cmp [board+si],3
	  je @@possible
	  jmp @@ret
@@possible:
      ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov di,si
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov si,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add si,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+si],4;check if this is possible
	  je @@optional
      jmp @@ret; if no break
@@optional:	  
	  cmp si,di; if same place break
	  jne @@continue; if not continue
	  jmp @@ret;break
@@continue:	  
	  push si
	  sub si,di;check the distance
	  cmp si,14; if yes mov up right 
	  jne @@other_option
	  pop si
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],3;move the soldier
	  inc [W_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@other_option:	  
      cmp si,18; if yes mov up left 
	  jne @@more_then_one_eat; if no there is more then one to eat
	  pop si
	  mov [board+di],1; delete the image in the current square 
	  add di,9; up left
	  mov [board+di],1; delete the image in the current square 
	  add di,9; up left
	  mov [board+di],3;move the soldier
	  inc [W_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@more_then_one_eat:
      add [W_eaten],1; inc the number of the eaten soldiers
	  pop si
	  add di,18; twice up left
	  cmp [board+di],4
      je @@left_movment
      sub di,4; twice up right
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,14
      mov si,di; prepare to the next call
	  mov [board+si],3;move the soldier
      call eat_white_down_move; more_then_one_eat		
      jmp @@ret	 ;break 
@@left_movment:	  
	  sub di,9
	  mov [board+di],1; delete the image in the current square 
	  sub di,9
	  mov [board+di],1; delete the image in the current square 
	  add di,18; twice up left
      mov si,di; prepare to the next call
	  mov [board+si],3;move the soldier
      call eat_white_down_move; more_then_one_eat	
	  
	  
@@ret:	 
      ; keeps the values of the regesters
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp eat_white_down_move


;================================================
; Description  : move down eating black soldiers 
; Input        : 1. si the eating soldier
; 			     2. dx the row of the press
; 			     3. cx the col of the press
; 			     4. board a array in ds
; Output:         1. board a array in ds
; 			     2. W_eaten a variable in ds
; 			     3. B_eaten a variable in ds
;================================================
proc eat_black_down_move
      ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
	  cmp [board+si],2
	  je @@possible
	  jmp @@ret
@@possible:
      ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov di,si;save the input of si
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov si,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add si,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+si],4;check if this is possible
	  je @@optional
      jmp @@ret; if no break
@@optional:	  
	  cmp si,di; if same place break
	  jne @@continue; if not continue
	  jmp @@ret;break
@@continue:	  
	  push si
	  sub si,di;check the distance
	  cmp si,14; if yes mov up right 
	  jne @@other_option
	  pop si
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],2;move the soldier
	  inc [B_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@other_option:	  
      cmp si,18; if yes mov up left 
	  jne @@more_then_one_eat; if no there is more then one to eat
	  pop si
	  mov [board+di],1; delete the image in the current square 
	  add di,9; up left
	  mov [board+di],1; delete the image in the current square 
	  add di,9; up left
	  mov [board+di],2;move the soldier
	  inc [B_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@more_then_one_eat:
      add [B_eaten],1; inc the number of the eaten soldiers
	  pop si
	  add di,18; twice up left
	  cmp [board+di],4
      je @@left_movment
      sub di,4; twice up right
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,14
      mov si,di; prepare to the next call
	  mov [board+si],2;move the soldier
      call eat_black_down_move; more_then_one_eat		
      jmp @@ret	 ;break 
@@left_movment:	  
	  sub di,9
	  mov [board+di],1; delete the image in the current square 
	  sub di,9
	  mov [board+di],1; delete the image in the current square 
	  add di,18; twice up left
      mov si,di; prepare to the next call
	  mov [board+si],2;move the soldier
      call eat_black_down_move; more_then_one_eat	
	  
	  
@@ret:	 
      ; keeps the values of the regesters
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp eat_black_down_move



;================================================
; Description  : move up eating white soldiers 
; Input        : 1. si the eating soldier
; 			     2. di the place of the eating soldier after the eating
; 			     3. board a array in ds
; Output:         1. board a array in ds
; 			     2. W_eaten a variable in ds
; 			     3. B_eaten a variable in ds
;================================================
proc special_eat_white_up_move
      ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
	  cmp [board+si],3
	  je @@possible
	  jmp @@ret
@@possible:
      push si
	  push di
	  pop si
	  pop di
	  cmp [board+si],4;check if this is possible
	  je @@optional
      jmp @@ret; if no break
@@optional:	  
	  cmp si,di; if same place break
	  jne @@continue; if not continue
	  jmp @@ret;break
@@continue:	  
	  push di
	  sub di,si;check the distance
	  cmp di,14; if yes mov up right 
	  jne @@other_option
	  pop di
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],3;move the soldier
	  inc [W_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@other_option:	  
      cmp di,18; if yes mov up left 
	  jne @@more_then_one_eat; if no there is more then one to eat
	  pop di
	  mov [board+di],1; delete the image in the current square 
	  sub di,9; up left
	  mov [board+di],1; delete the image in the current square 
	  sub di,9; up left
	  mov [board+di],3;move the soldier
	  inc [W_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@more_then_one_eat:
      add [W_eaten],1; inc the number of the eaten soldiers
	  pop di
	  sub di,18; twice up left
	  cmp [board+di],4
      je @@left_movment
      add di,4; twice up right
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,14
      push si
	  push di
	  pop si
	  pop di; prepare to the next call
	  mov [board+si],3;move the soldier
      call special_eat_white_up_move; more_then_one_eat		
      jmp @@ret	 ;break 
@@left_movment:	  
	  add di,9
	  mov [board+di],1; delete the image in the current square 
	  add di,9
	  mov [board+di],1; delete the image in the current square 
	  sub di,18; twice up left
      push si
	  push di
	  pop si
	  pop di; prepare to the next call
	  mov [board+si],3;move the soldier
      call special_eat_white_up_move; more_then_one_eat	
	  
	  
@@ret:	 
      ; keeps the values of the regesters
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp special_eat_white_up_move


;================================================
; Description  : move down eating white soldiers 
; Input        : 1. si the eating soldier
; 			     2. di the place of the eating soldier after the eating
; 			     3. board a array in ds
; Output:         1. board a array in ds
; 			     2. W_eaten a variable in ds
; 			     3. B_eaten a variable in ds
;================================================
proc special_eat_white_down_move
      ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
	  cmp [board+si],3
	  je @@possible
	  jmp @@ret
@@possible:
      push si
	  push di
	  pop si
	  pop di
	  cmp [board+si],4;check if this is possible
	  je @@optional
      jmp @@ret; if no break
@@optional:	  
	  cmp si,di; if same place break
	  jne @@continue; if not continue
	  jmp @@ret;break
@@continue:	  
	  push si
	  sub si,di;check the distance
	  cmp si,14; if yes mov up right 
	  jne @@other_option
	  pop si
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],3;move the soldier
	  inc [W_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@other_option:	  
      cmp si,18; if yes mov up left 
	  jne @@more_then_one_eat; if no there is more then one to eat
	  pop si
	  mov [board+di],1; delete the image in the current square 
	  add di,9; up left
	  mov [board+di],1; delete the image in the current square 
	  add di,9; up left
	  mov [board+di],3;move the soldier
	  inc [W_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@more_then_one_eat:
      add [W_eaten],1; inc the number of the eaten soldiers
	  pop si
	  add di,18; twice up left
	  cmp [board+di],4
      je @@left_movment
      sub di,4; twice up right
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,14
      push si
	  push di
	  pop si
	  pop di; prepare to the next call
	  mov [board+si],3;move the soldier
      call special_eat_white_down_move; more_then_one_eat		
      jmp @@ret	 ;break 
@@left_movment:	  
	  sub di,9
	  mov [board+di],1; delete the image in the current square 
	  sub di,9
	  mov [board+di],1; delete the image in the current square 
	  add di,18; twice up left
      push si
	  push di
	  pop si
	  pop di; prepare to the next call
	  mov [board+si],3;move the soldier
      call special_eat_white_down_move; more_then_one_eat	
	  
	  
@@ret:	 
      ; keeps the values of the regesters
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp special_eat_white_down_move

;================================================
; Description  : move down eating black soldiers 
; Input        : 1. si the eating soldier
; 			     2. di the place of the eating soldier after the eating
; 			     3. board a array in ds
; Output:         1. board a array in ds
; 			     2. W_eaten a variable in ds
; 			     3. B_eaten a variable in ds
;================================================
proc special_eat_black_down_move
      ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
	  cmp [board+si],2
	  je @@possible
	  jmp @@ret
@@possible:
      push si
	  push di
	  pop si
	  pop di
	  cmp [board+si],4;check if this is possible
	  je @@optional
      jmp @@ret; if no break
@@optional:	  
	  cmp si,di; if same place break
	  jne @@continue; if not continue
	  jmp @@ret;break
@@continue:	  
	  push si
	  sub si,di;check the distance
	  cmp si,14; if yes mov up right 
	  jne @@other_option
	  pop si
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,7
	  mov [board+di],2;move the soldier
	  inc [B_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@other_option:	  
      cmp si,18; if yes mov up left 
	  jne @@more_then_one_eat; if no there is more then one to eat
	  pop si
	  mov [board+di],1; delete the image in the current square 
	  add di,9; up left
	  mov [board+di],1; delete the image in the current square 
	  add di,9; up left
	  mov [board+di],2;move the soldier
	  inc [B_eaten]; inc the number of the eaten soldiers
	  jmp @@ret;break
@@more_then_one_eat:
      add [B_eaten],1; inc the number of the eaten soldiers
	  pop si
	  add di,18; twice up left
	  cmp [board+di],4
      je @@left_movment
      sub di,4; twice up right
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  sub di,7
	  mov [board+di],1; delete the image in the current square 
	  add di,14
      push si
	  push di
	  pop si
	  pop di; prepare to the next call
	  mov [board+si],2;move the soldier
      call special_eat_black_down_move; more_then_one_eat		
      jmp @@ret	 ;break 
@@left_movment:	  
	  sub di,9
	  mov [board+di],1; delete the image in the current square 
	  sub di,9
	  mov [board+di],1; delete the image in the current square 
	  add di,18; twice up left
      push si
	  push di
	  pop si
	  pop di; prepare to the next call
	  mov [board+si],2;move the soldier
      call special_eat_black_down_move; more_then_one_eat	
	  
	  
@@ret:	 
      ; keeps the values of the regesters
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp special_eat_black_down_move

















;================================================
; Description  : move up soldiers without call to draw_board
; Input        : 1. si is the soldier that moving
; 			     2. di the place of the eating soldier after the eating
; 			     3. board a array in ds
; 			     4. turn a variable in ds
; Output:         1. board a array in ds
;================================================
proc special_move_up
     ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
      push si
	  push di
	  pop si
	  pop di
	   cmp [board+si],4
	   jne @@ret
	  test [turn],00000001b;check the directon of moving of each color
	  jz @@black
@@white:
     cmp [board+di],2;check if we move the right soldier
	 jne @@ret
	 cmp di,si;check that its up moving
	 jbe @@ret
	 mov [board+di],1;remov the soldier
	 mov [board+si],2;put the soldier in the new place
	
	 jmp @@ret
     
@@black:
     
     cmp [board+di],3;check if we move the right soldier
	 jne @@ret
	 cmp di,si;check that its up moving
	 jbe @@ret
	 mov [board+di],1;remov the soldier
	 mov [board+si],3;put the soldier in the new place
	 jmp @@ret
	 
	 
@@ret:	 
	 ; keeps the values of the regesters
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp special_move_up

;================================================
; Description  : move up soldiers with call to draw_board
; Input        : 1. si is the soldier that moving
; 			     2. cx the col
; 			     3. board a array in ds
; 			     4. turn a variable in ds
; 			     5.  dx the row
; Output:         1. board a array in ds
;================================================
proc move_up
     ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
      ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov di,si;save the input of si
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov si,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add si,ax
	  pop dx
	  ;----------------------------------------------------
	   cmp [board+si],4
	   jne @@ret
	  test [turn],00000001b;check the directon of moving of each color
	  jz @@black
@@white:
     cmp [board+di],2;check if we move the right soldier
	 jne @@ret
	 cmp di,si;check that its up moving
	 jbe @@ret
	 mov [board+di],1;remov the soldier
	 mov [board+si],2;put the soldier in the new place
	
	 jmp @@ret
     
@@black:
     
     cmp [board+di],3;check if we move the right soldier
	 jne @@ret
	 cmp di,si;check that its up moving
	 jbe @@ret
	 mov [board+di],1;remov the soldier
	 mov [board+si],3;put the soldier in the new place
	 jmp @@ret
	 
	 
@@ret:	 
	 ; keeps the values of the regesters
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp move_up
;================================================
; Description  : move down soldiers with call to draw_board
; Input        : 1. si is the soldier that moving
; 			     2. cx the col
; 			     3. board a array in ds
; 			     4. turn a variable in ds
; 			     5.  dx the row
; Output:         1. board a array in ds
;================================================
proc move_down
     ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
      ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov di,si;save the input of si
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov si,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add si,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+si],4
	  jne @@ret
	 push di
	 test [turn],00000001b;check the directon of moving of each color
	 pop di
	 jz @@white
@@black:
     cmp [board+di],3;check if we move the right soldier
	 jne @@ret
	 cmp di,si;check that its down moving
	 jge @@ret
	 mov [board+di],1;remove the soldier
	 mov [board+si],3;put the soldier in the new place
	 jmp @@ret
@@white:
     cmp [board+di],2;check if we move the right soldier
	 jne @@ret
	 cmp di,si;check that its down moving
	 jge @@ret
	 mov [board+di],1;remove the soldier
	 mov [board+si],2;put the soldier in the new place
	 jmp @@ret


	 jmp @@ret
	 
	 
@@ret:	 
	 ; keeps the values of the regesters
	  call draw_board
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp move_down
;================================================
; Description  : move down soldiers without call to draw_board
; Input        : 1. si is the soldier that moving
; 			     2. di the place of the eating soldier after the eating
; 			     3. board a array in ds
; 			     4. turn a variable in ds
; Output:         1. board a array in ds
;================================================
proc special_move_down
     ; keeps the values of the regesters
      push cx
	  push dx
	  push si
	  push ax
	  push bx
	  push di
	  ;-------------------------
	  push si
	  push di
	  pop si 
	  pop di
	  cmp [board+si],4
	  jne @@ret
	 push di
	 test [turn],00000001b;check the directon of moving of each color
	 pop di
	 jz @@white
@@black:
     cmp [board+di],3;check if we move the right soldier
	 jne @@ret
	 cmp di,si;check that its down moving
	 jge @@ret
	 mov [board+di],1;remove the soldier
	 mov [board+si],3;put the soldier in the new place
	 jmp @@ret
@@white:
     cmp [board+di],2;check if we move the right soldier
	 jne @@ret
	 cmp di,si;check that its down moving
	 jge @@ret
	 mov [board+di],1;remove the soldier
	 mov [board+si],2;put the soldier in the new place
	 jmp @@ret


	 
	 
@@ret:	 
	 ; keeps the values of the regesters
	  pop di
	  pop bx
	  pop ax
	  pop si
	  pop dx
	  pop cx
	  ;--------------------------------
	  ret
endp special_move_down



;================================================
; Description  : show the options for the last black soldier
; Input        : 1. si is the soldier that moving
; 			     2. board a array in ds
; 			     3. turn a variable in ds
; Output:         1. board a array in ds
;================================================
proc spacial_move_option_black
     test [turn],00000001b; check which color is the enemy soldiers
	 jz @@up_ok
	 jmp @@down_ok
;show options
@@up_ok:	 
	 call show_options_up

	 call show_options_down

	 jmp @@ret
; make the movmemt to legal and show options
@@down_ok:
     
	 
	 sub [turn],1
	 call show_options_up
     add [turn],1
	 call show_options_down
     jmp @@ret
@@ret:	 
	 ret
endp spacial_move_option_black


;================================================
; Description  : show the options for the last  white soldier
; Input        : 1. si is the soldier that moving
; 			     2. board a array in ds
; 			     3. turn a variable in ds
; Output:         1. board a array in ds
;================================================
proc spacial_move_option_white
     test [turn],00000001b; check which color is the enemy soldiers
	 jnz @@up_ok
	 jmp @@down_ok
; make the movmemt to legal and show options
@@up_ok:	 
	 call show_options_up
	 sub [turn],1
	 call show_options_down
	 add [turn],1
	 jmp @@ret
; make the movmemt to legal and show options
@@down_ok:
     add [turn],1
	 call show_options_up
     sub [turn],1
	 call show_options_down
	 
     jmp @@ret
@@ret:	 
	 ret
endp spacial_move_option_white






proc OpenShowBmp near
	
	 
	call OpenBmpFile
	cmp [ErrorFile],1
	je @@ExitProc
	
	call ReadBmpHeader
	
	call ReadBmpPalette
	
	call CopyBmpPalette
	
	call ShowBMP
	
	 
	call CloseBmpFile

@@ExitProc:
	ret
endp OpenShowBmp

 
 
	
; input dx filename to open
proc OpenBmpFile	near						 
	mov ah, 3Dh
	xor al, al
	int 21h
	jc @@ErrorAtOpen
	mov [FileHandle], ax
	jmp @@ExitProc
	
@@ErrorAtOpen:
	mov [ErrorFile],1
@@ExitProc:	
	ret
endp OpenBmpFile
 
 
 



proc CloseBmpFile near
	mov ah,3Eh
	mov bx, [FileHandle]
	int 21h
	ret
endp CloseBmpFile




; Read 54 bytes the Header
proc ReadBmpHeader	near					
	push cx
	push dx
	
	mov ah,3fh
	mov bx, [FileHandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	
	pop dx
	pop cx
	ret
endp ReadBmpHeader



proc ReadBmpPalette near ; Read BMP file color palette, 256 colors * 4 bytes (400h)
						 ; 4 bytes for each color BGR + null)			
	push cx
	push dx
	
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	
	pop dx
	pop cx
	
	ret
endp ReadBmpPalette




;================================================
; Description  : copy the last board to board
; Input        : 1. si is the soldier that moving
; 			     2. board a array in ds
; 			     3. last_board  a array in ds
; Output:         1. board a array in ds
;================================================
proc mov_last_board_to_board
     push si
	 push ax
	 push cx
	 push dx
	 mov cx,64; number of sqaures
	 mov si,0
; mov between the arrays
@@mov:	 
	 mov al,[last_board+si]
	 mov [board+si],al
	 inc si
	 loop @@mov
@@ret: 
	 
	 
	 pop dx
	 pop cx
	 pop ax
	 pop si
	 ret
endp mov_last_board_to_board


; Will move out to screen memory the colors
; video ports are 3C8h for number of first color
; and 3C9h for all rest
proc CopyBmpPalette		near					
										
	push cx
	push dx
	
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0  ; black first							
	out dx,al ;3C8h
	inc dx	  ;3C9h
CopyNextColor:
	mov al,[si+2] 		; Red				
	shr al,2 			; divide by 4 Max (cos max is 63 and we have here max 255 ) (loosing color resolution).				
	out dx,al 						
	mov al,[si+1] 		; Green.				
	shr al,2            
	out dx,al 							
	mov al,[si] 		; Blue.				
	shr al,2            
	out dx,al 							
	add si,4 			; Point to next color.  (4 bytes for each color BGR + null)				
								
	loop CopyNextColor
	
	pop dx
	pop cx
	
	ret
endp CopyBmpPalette


 
 

 

 
 
 


proc ShowBMP 
; BMP graphics are saved upside-down.
; Read the graphic line by line (BmpRowSize lines in VGA format),
; displaying the lines from bottom to top.
	push cx
	
	mov ax, 0A000h
	mov es, ax
	
	mov cx,[BmpRowSize]
	
 
	mov ax,[BmpColSize] ; row size must dived by 4 so if it less we must calculate the extra padding bytes
	xor dx,dx
	mov si,4
	div si
	cmp dx,0
	mov bp,0
	jz @@row_ok
	mov bp,4
	sub bp,dx

@@row_ok:	
	mov dx,[BmpLeft]
	
@@NextLine:
	push cx
	push dx
	
	mov di,cx  ; Current Row at the small bmp (each time -1)
	add di,[BmpTop] ; add the Y on entire screen
	
 
	; next 5 lines  di will be  = cx*320 + dx , point to the correct screen line
	dec di
	mov cx,di
	shl cx,6
	shl di,8
	add di,cx
	add di,dx
	 
	; small Read one line
	mov ah,3fh
	mov cx,[BmpColSize]  
	add cx,bp  ; extra  bytes to each row must be divided by 4
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,[BmpColSize]  
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	
	pop dx
	pop cx
	 
	loop @@NextLine
	
	pop cx
	ret
endp ShowBMP 

proc  SetGraphic
	mov ax,13h   ; 320 X 200 
				 ;Mode 13h is an IBM VGA BIOS mode. It is the specific standard 256-color mode 
	int 10h
	ret
endp 	SetGraphic




;================================================
; Description  : main for the minimax
;================================================
proc single_player
;hide mouse
	 mov ax,2
	 int 33h
    mov dx, offset start_game
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], 320
	mov [BmpRowSize] ,200
	
	call OpenShowBmp
     mov bl,0
	  mov bh,1
      call RandomByCs
      cmp al,0
	  jz @@white
      call game1
	  jmp @@Black
@@white:	  
      Call game2
@@Black:	

@@in_game:
     mov [eat_option],0
	 mov [there_is_movment_option],0
     mov ax,1
	 int 33h
	 call check_win
	 cmp [finish],1
	 jne @@next_check 
	 jmp @@ret
@@next_check:
     cmp [B_eaten],12
	 jne @@next_check2 
	 mov [byte ptr white_win],1
	 jmp @@ret
@@next_check2:	 
	 cmp [W_eaten],12
	 jne @@ok
	 mov [byte ptr black_win],1
	 jmp @@ret
@@ok:	 
     test [turn],00000001b; check which color is the enemy soldiers
	 jz @@computer_starts
	 test [player],00000001b
	 jz @@get_input 
	 jmp @@not_your_turn
@@computer_starts:
     test [player],00000001b
	 jnz @@get_input 
	 jmp @@not_your_turn1
	 
	 
@@get_input:	 
     mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz @@in_game
	 test ax,00000001b
	 jz @@in_game
	 shr cx,1;div by 2
	 cmp cx,15; the board boarder 
	 jb @@in_game
	 cmp cx,315; the board boarder 
	 ja @@in_game
	 cmp dx,10; the board boarder 
	 jb @@in_game
	 cmp dx,190; the board boarder 
	 ja @@in_game
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov si,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add si,ax
	  pop dx
	  ;----------------------------------------------------
	 test [player],00000001b
	 jnz @@black_turn
	 jmp @@white_turn
@@black_turn:	 
	 cmp [board+si],3
	 je @@ok3
	 jmp @@in_game
@@ok3:	 
	 cmp [B_eaten],11
	 je @@last_soldier
	 jmp @@not_last_soldier
@@last_soldier:
     mov [B_last],1	 
	 call spacial_move_option_black
	 mov [B_last],0
	 cmp [there_is_movment_option],1;if there are no options to move the player lose
	 je @@continue
	 cmp [eat_option],1;if there are no options to move the player lose
	 je @@continue
	 
	 jmp @@ret 
	 
@@continue:
     
	 
	 
	 
	 
	 mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz @@continue
	 test ax,00000001b
	 jz @@continue
	 shr cx,1;div by 2
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov di,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add di,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+di],4;check if the pressed place is possible for moving
	  je @@check_if_not_the_same_place
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  jmp @@in_game
@@check_if_not_the_same_place:	  
      cmp di,si
      jne @@move
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  call close_options
      jmp @@in_game
@@move:
     cmp di,si
     jb @@down
	 jmp @@up
@@down:
	 push si
	 sub si,di
	 cmp si,9
	 ja @@eat1
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne @@no_burn1
	 pop si
     mov [board+si],1
	 inc [B_eaten]
     call close_options
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 jmp @@in_game
@@no_burn1:
     pop si	 
	 cmp [turn],0
	 je @@no_change1
	 
	 inc [turn]
     call move_up
	 dec [turn]
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp @@in_game
@@no_change1:
     call move_up
	 
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp @@in_game
     
      




@@eat1:
     pop si	
     call eat_white_up_move	 
	 call close_options
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 jmp @@in_game
@@up:  
     
	 push di
	 sub di,si
	 cmp di,9
	 ja @@eat2
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne @@no_burn2
	 pop di
     mov [board+si],1
	 inc [B_eaten]
     call close_options
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 jmp @@in_game
@@no_burn2:	 
     
	 
	 pop di
	 
	 cmp [turn],1
	 je @@no_change2
	 
	 dec [turn]
     call move_down
	 inc [turn]
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp @@in_game
@@no_change2:
     call move_down
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp @@in_game
@@eat2:
     pop di	
     call eat_white_down_move	 
	 call close_options
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 jmp @@in_game
	 
	 
	 
	 
	 
	 
	 
	 
      
@@not_last_soldier:	
 
	 test [turn],00000001b; check which color is the enemy soldiers
	 jnz @@not_your_turn
	 jmp @@your_turn
	 
	 
	 
	 
@@not_your_turn:
;prepare to the call
     mov al,[player]
     mov [l_temp_player],al
     mov [depth],3 
	 call RecMiniMax
;get the returned values
	 mov di,[best_mov_di]
	 mov si,[best_mov_si]
	 call special_show_options_down
	 push di
	 sub di,si
	 cmp di,9
	 ja @@was_eat1
	 pop di
	 
	 
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne @@M_no_burn1
     mov [board+si],1
	 inc [B_eaten]
     call close_options
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 mov [current_score],0
	 inc [player]
	 jmp @@in_game
@@M_no_burn1:
     call load; keeps the board
	 call check_burn_black
	 call reload
	 call special_move_down	 
	 call close_options
	 cmp [burn],1
	 jne @@M_skip1
	 push si
	 mov si,[could_eat]
	 mov [board+si],1
	 pop si
	 inc [B_eaten]
	 call draw_board
@@M_skip1:	 
	 mov [burn],0
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 mov [current_score],0
	 inc [player]
	 jmp @@in_game
@@was_eat1:	 
     pop di
	 call special_eat_white_down_move
	 call close_options
	 mov [current_score],0
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
	 
	 
	 
	 
@@your_turn:	 
	 call show_options_up
	 cmp [there_is_movment_option],1;if there are no options to move
	 je @@continue2
	 cmp [eat_option],1;if there are no options to move
	 je @@continue2
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
	 
@@continue2:
     
	 
	 
	 
	 
	 mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz @@continue2
	 test ax,00000001b
	 jz @@continue2
	 shr cx,1;div by 2
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov di,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add di,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+di],4;check if the pressed place is possible for moving
	  je @@check_if_not_the_same_place2
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  jmp @@in_game
@@check_if_not_the_same_place2:	  
      cmp di,si
      jne @@move2
	   mov [eat_option],0;prepare to the next turn
	  call close_options
	  mov[byte ptr there_is_movment_option],0;prepare to the next turn
      jmp @@in_game
@@move2:
	 push si
	 sub si,di
	 cmp si,9
	 ja @@eat4
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne @@no_burn4
	 pop si
     mov [board+si],1
	 inc [B_eaten]
     call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
@@no_burn4:	 
     
	 
	 pop si
	 call check_burn_black
	 call mov_last_board_to_board
	 call move_up
	 
	 cmp [burn],1
	 jne @@skip2
	 push si
	 mov si,[could_eat]
	 mov [board+si],1
	 pop si
	 inc [B_eaten]
@@skip2:	
	 
	 
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [burn],0
	 jmp @@in_game
@@eat4:
     pop si	
     call eat_white_up_move	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
	 
	 
	 
	 
	 
	 
	 
	 
@@white_turn: 
	 cmp [board+si],2
	 je @@ok2
	 jmp @@in_game
	 
@@ok2:
     
	 
	 cmp [W_eaten],11
	 je @@last_soldier1
	 jmp @@not_last_soldier2
@@last_soldier1:
     mov [W_last],1	 
	 call spacial_move_option_white
	 mov [W_last],0
	 cmp [there_is_movment_option],1;if there are no options to move the player lose
	 je @@continue5
	 cmp [eat_option],1;if there are no options to move the player lose
	 je @@continue5
	 jmp @@ret 
	 
@@continue5:
     
	 
	 
	 
	 
	 mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz @@continue5
	 test ax,00000001b
	 jz @@continue5
	 shr cx,1;div by 2
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov di,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add di,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+di],4;check if the pressed place is possible for moving
	  je @@check_if_not_the_same_place3
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	  mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  jmp @@in_game
@@check_if_not_the_same_place3:	  
      cmp di,si
      jne @@move3
	   mov [eat_option],0;prepare to the next turn
	  call close_options
	  mov[byte ptr there_is_movment_option],0;prepare to the next turn
      jmp @@in_game
@@move3:
     cmp di,si
     jb @@down2
	 jmp @@up1
@@down2:	 
	 push si
	 sub si,di
	 cmp si,9
	 ja @@eat5
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne @@no_burn5
	 pop si
     mov [board+si],1
	 inc [W_eaten]
     call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
@@no_burn5:
     pop si	 
     cmp [turn],1
	 je @@no_change3
	 
	 dec [turn]
     call move_up
	 inc [turn]
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp @@in_game
@@no_change3:
     call move_up
@@eat5:
     pop si	
     call eat_black_up_move	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
@@up1:  
     
	 push di
	 sub di,si
	 cmp di,9
	 ja @@eat6
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne @@no_burn6
	 pop di
     mov [board+si],1
	 inc [W_eaten]
     call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
@@no_burn6:	 
	 
	 pop di
	 cmp [turn],0
	 je @@no_change4
	 
	 dec [turn]
     call move_down
	 inc [turn]
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp @@in_game
@@no_change4:
     call move_down
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
@@eat6:
     pop di	
     call eat_black_down_move	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
	 
	 
	 
	 
	 
	 
	 
	 
      
@@not_last_soldier2:	 
	 test [turn],00000001b; check which color is the enemy soldiers
	 jz @@not_your_turn1
	 jmp @@your_turn1
	 
	 
	 
	 
@@not_your_turn1:
;prepare to the call	 
	 mov al,[player]
     mov [l_temp_player],al
     mov [depth],3
	 
	 call RecMiniMax
;get the returned values	 
	 mov di,[best_mov_di]
	 mov si,[best_mov_si]
	 call special_show_options_down
	 push di
	 sub di,si
	 cmp di,9
	 ja @@was_eat2
	 pop di
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne @@M_no_burn2
     mov [board+si],1
	 inc [W_eaten]
     call close_options
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 mov [current_score],0
	 inc [player]
	 jmp @@in_game
@@M_no_burn2:
     call load; keeps the board
	 call check_burn_white
	 call reload
	 call special_move_down	 
	 call close_options
	 cmp [burn],1
	 jne @@M_skip2
	 push si
	 mov si,[could_eat]
	 mov [board+si],1
	 pop si
	 inc [W_eaten]
	 call draw_board
@@M_skip2:	 
	 mov [burn],0
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 mov [current_score],0
	 inc [player]
	 jmp @@in_game
@@was_eat2:	
     pop di 
	 call special_eat_black_down_move
	 call close_options
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
	 
	 
		
	 
	 
@@your_turn1:	 
	 call show_options_up
	 cmp [there_is_movment_option],1;if there are no options to move
	 je @@continue7
	 cmp [eat_option],1;if there are no options to move
	 je @@continue7
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
	 
@@continue7:
     
	 
	 
	 
	 
	 mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz @@continue7
	 test ax,00000001b
	 jz @@continue7
	 shr cx,1;div by 2
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov di,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add di,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+di],4;check if the pressed place is possible for moving
	  je @@check_if_not_the_same_place5
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  jmp @@in_game
@@check_if_not_the_same_place5:	  
      cmp di,si
      jne @@move8
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
      jmp @@in_game
@@move8:
	 push si
	 sub si,di
	 cmp si,9
	 ja @@eat8
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne @@no_burn8
	 pop si
     mov [board+si],1
	 inc [W_eaten]
     call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
@@no_burn8:	 
     
	 
	 pop si
	 call check_burn_white
	 call mov_last_board_to_board
	 call move_up
	 
	 cmp [burn],1
	 jne @@skip4
	 push si
	 mov si,[could_eat]
	 mov [board+si],1
	 pop si
	 inc [W_eaten]
@@skip4:	
	 
	 
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [burn],0
	 jmp @@in_game
@@eat8:
     pop si	
     call eat_black_up_move	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp @@in_game
	 
	 
@@ret:	 
     ;hide mouse
     mov ax,2
	 int 33h
     cmp [white_win],1
	 jne @@white_lose
	 mov dx,offset W_win
	 mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], 320
	mov [BmpRowSize] ,200 
	 call OpenShowBmp
	 jmp @@white_wins
@@white_lose:
     mov dx,offset B_win
	 mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], 320
	mov [BmpRowSize] ,200
	 call OpenShowBmp	 
@@white_wins:	 
	 ;----------------------------------------------------
	;show mouse
	 mov ax,1
	 int 33h
	 ret
endp single_player

;================================================
; Description  : main for the one vs one  
;================================================
proc multiplayer
     ;hide mouse
	 mov ax,2
	 int 33h
	 
	 mov dx, offset start_game
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], 320
	mov [BmpRowSize] ,200
	
	call OpenShowBmp
     mov bl,0
	  mov bh,1
      call RandomByCs
      cmp al,0
	  jz @@white
      call game1
	  jmp @@Black
@@white:	  
      Call game2
@@Black:	
	 
	 
	 
	 in_game:
     mov ax,1
	 int 33h
	 call check_win
	 cmp [finish],1
	 jne next_check 
	 jmp @@ret
next_check:
     cmp [B_eaten],12
	 jne next_check2 
	 mov [byte ptr white_win],1
	 jmp @@ret
next_check2:	 
	 cmp [W_eaten],12
	 jne ok
	 mov [byte ptr black_win],1
	 jmp @@ret
ok:	 
     mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz in_game
	 test ax,00000001b
	 jz in_game
	 shr cx,1;div by 2
	 cmp cx,15; the board boarder 
	 jb in_game
	 cmp cx,315; the board boarder 
	 ja in_game
	 cmp dx,10; the board boarder 
	 jb in_game
	 cmp dx,190; the board boarder 
	 ja in_game
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov si,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add si,ax
	  pop dx
	  ;----------------------------------------------------
	 test [player],00000001b
	 jnz black_turn
	 jmp white_turn
black_turn:	 
	 cmp [board+si],3
	 je ok3
	 jmp in_game
ok3:	 
	 cmp [B_eaten],11
	 je last_soldier
	 jmp not_last_soldier
last_soldier:
     mov [B_last],1	 
	 call spacial_move_option_black
	 mov [B_last],0
	 cmp [there_is_movment_option],1;if there are no options to move the player lose
	 je continue
	 cmp [eat_option],1;if there are no options to move the player lose
	 je continue
	 
	 jmp @@ret
	 
continue:
     
	 
	 
	 
	 
	 mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz continue
	 test ax,00000001b
	 jz continue
	 shr cx,1;div by 2
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov di,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add di,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+di],4;check if the pressed place is possible for moving
	  je check_if_not_the_same_place
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  jmp in_game
check_if_not_the_same_place:	  
      cmp di,si
      jne move
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  call close_options
      jmp in_game
move:
     cmp di,si
     jb down
	 jmp up
down:
	 push si
	 sub si,di
	 cmp si,9
	 ja eat1
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne no_burn1
	 pop si
     mov [board+si],1
	 inc [B_eaten]
     call close_options
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 jmp in_game
no_burn1:
     pop si	 
	 cmp [turn],0
	 je no_change1
	 
	 inc [turn]
     call move_up
	 dec [turn]
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp in_game
no_change1:
     call move_up
	 
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp in_game
     
      




eat1:
     pop si	
     call eat_white_up_move	 
	 call close_options
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 jmp in_game
up:  
     
	 push di
	 sub di,si
	 cmp di,9
	 ja eat2
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne no_burn2
	 pop di
     mov [board+si],1
	 inc [B_eaten]
     call close_options
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 jmp in_game
no_burn2:	 
     
	 
	 pop di
	 
	 cmp [turn],1
	 je no_change2
	 
	 dec [turn]
     call move_down
	 inc [turn]
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp in_game
no_change2:
     call move_down
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp in_game
eat2:
     pop di	
     call eat_white_down_move	 
	 call close_options
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 jmp in_game
	 
	 
	 
	 
	 
	 
	 
	 
      
not_last_soldier:	 
	 test [turn],00000001b; check which color is the enemy soldiers
	 jnz not_your_turn
	 jmp your_turn
not_your_turn:	 
	 call show_options_down
	 cmp [there_is_movment_option],1;if there are no options to move
	 je continue1
	 cmp [eat_option],1;if there are no options to move
	 je continue1
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
	 
continue1:
     
	 
	 
	 
	 
	 mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz continue1
	 test ax,00000001b
	 jz continue1
	 shr cx,1;div by 2
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov di,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add di,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+di],4;check if the pressed place is possible for moving
	  je check_if_not_the_same_place1
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  jmp in_game
check_if_not_the_same_place1:	  
      cmp si,di
      jne move1
	   mov [eat_option],0;prepare to the next turn
	  call close_options
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
      jmp in_game
move1:
	 push di
	 sub di,si
	 cmp di,9
	 ja eat3
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne no_burn3
	 pop di
     mov [board+si],1
	 inc [B_eaten]
     call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
no_burn3:
     	
     call check_burn_black
     call mov_last_board_to_board	 
	 pop di
     call move_down
	 
	 cmp [burn],1
	 jne skip1
	 push si
	 mov si,[could_eat]
	 mov [board+si],1
	 pop si
	 inc [B_eaten]
skip1:	
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [burn],0
	 jmp in_game
eat3:
     pop di	
     call eat_white_down_move	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
	 
	 
	 
	 
your_turn:	 
	 call show_options_up
	 cmp [there_is_movment_option],1;if there are no options to move
	 je continue2
	 cmp [eat_option],1;if there are no options to move
	 je continue2
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
	 
continue2:
     
	 
	 
	 
	 
	 mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz continue2
	 test ax,00000001b
	 jz continue2
	 shr cx,1;div by 2
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov di,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add di,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+di],4;check if the pressed place is possible for moving
	  je check_if_not_the_same_place2
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  jmp in_game
check_if_not_the_same_place2:	  
      cmp di,si
      jne move2
	   mov [eat_option],0;prepare to the next turn
	  call close_options
	  mov[byte ptr there_is_movment_option],0;prepare to the next turn
      jmp in_game
move2:
	 push si
	 sub si,di
	 cmp si,9
	 ja eat4
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne no_burn4
	 pop si
     mov [board+si],1
	 inc [B_eaten]
     call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
no_burn4:	 
     
	 
	 pop si
	 call check_burn_black
	 call mov_last_board_to_board
	 call move_up
	 
	 cmp [burn],1
	 jne skip2
	 push si
	 mov si,[could_eat]
	 mov [board+si],1
	 pop si
	 inc [B_eaten]
skip2:	
	 
	 
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [burn],0
	 jmp in_game
eat4:
     pop si	
     call eat_white_up_move	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
	 
	 
	 
	 
	 
	 
	 
	 
white_turn: 
	 cmp [board+si],2
	 je ok2
	 jmp in_game
	 
ok2:
     
	 
	 cmp [W_eaten],11
	 je last_soldier1
	 jmp not_last_soldier2
last_soldier1:
     mov [W_last],1	 
	 call spacial_move_option_white
	 mov [W_last],0
	 cmp [there_is_movment_option],1;if there are no options to move the player lose
	 je continue5
	 cmp [eat_option],1;if there are no options to move the player lose
	 je continue5
	 jmp @@ret
	 
continue5:
     
	 
	 
	 
	 
	 mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz continue5
	 test ax,00000001b
	 jz continue5
	 shr cx,1;div by 2
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov di,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add di,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+di],4;check if the pressed place is possible for moving
	  je check_if_not_the_same_place3
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	  mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  jmp in_game
check_if_not_the_same_place3:	  
      cmp di,si
      jne move3
	   mov [eat_option],0;prepare to the next turn
	  call close_options
	  mov[byte ptr there_is_movment_option],0;prepare to the next turn
      jmp in_game
move3:
     cmp di,si
     jb down2
	 jmp up1
down2:	 
	 push si
	 sub si,di
	 cmp si,9
	 ja eat5
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne no_burn5
	 pop si
     mov [board+si],1
	 inc [W_eaten]
     call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
no_burn5:
     pop si	 
     cmp [turn],1
	 je no_change3
	 
	 dec [turn]
     call move_up
	 inc [turn]
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp in_game
no_change3:
     call move_up
eat5:
     pop si	
     call eat_black_up_move	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
up1:  
     
	 push di
	 sub di,si
	 cmp di,9
	 ja eat6
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne no_burn6
	 pop di
     mov [board+si],1
	 inc [W_eaten]
     call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
no_burn6:	 
	 
	 pop di
	 cmp [turn],0
	 je no_change4
	 
	 dec [turn]
     call move_down
	 inc [turn]
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 inc [player]
	 jmp in_game
no_change4:
     call move_down
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
eat6:
     pop di	
     call eat_black_down_move	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
	 
	 
	 
	 
	 
	 
	 
	 
      
not_last_soldier2:	 
	 test [turn],00000001b; check which color is the enemy soldiers
	 jz not_your_turn1
	 jmp your_turn1
not_your_turn1:	 
	 call show_options_down
	 cmp [there_is_movment_option],1;if there are no options to move
	 je continue6
	 cmp [eat_option],1;if there are no options to move
	 je continue6
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
	 
continue6:
     
	 
	 
	 
	 
	 mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz continue6
	 test ax,00000001b
	 jz continue6
	 shr cx,1;div by 2
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov di,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add di,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+di],4;check if the pressed place is possible for moving
	  je check_if_not_the_same_place4
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  jmp in_game
check_if_not_the_same_place4:	  
      cmp di,si
      jne move6
	   mov [eat_option],0;prepare to the next turn
	  call close_options
	  mov[byte ptr there_is_movment_option],0;prepare to the next turn
      jmp in_game
move6:
	 push di
	 sub di,si
	 cmp di,9
	 ja eat7
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne no_burn7
	 pop di
     mov [board+si],1
	 inc [W_eaten]
     call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
no_burn7:
     call check_burn_white	
     call mov_last_board_to_board	 
     call move_down
	 
	 pop di
	 
	 cmp [burn],1
	 jne skip3
	 push si
	 mov si,[could_eat]
	 mov [board+si],1
	 pop si
	 inc [W_eaten]
skip3:	
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [burn],0
	 jmp in_game
eat7:
     pop di	
     call eat_black_down_move	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
	 
	 
	 
	 
your_turn1:	 
	 call show_options_up
	 cmp [there_is_movment_option],1;if there are no options to move
	 je continue7
	 cmp [eat_option],1;if there are no options to move
	 je continue7
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
	 
continue7:
     
	 
	 
	 
	 
	 mov ax,5
	 mov bx,0
	 int 33h
	 cmp bx,0
	 jz continue7
	 test ax,00000001b
	 jz continue7
	 shr cx,1;div by 2
	 ;calculate the place in the array of the press
      push dx
	  mov dx,0
	  mov ax,cx
	  sub ax,15
	  mov bx,Rect_Right_Or_Left
	  div bx
	  mov di,ax
	  pop dx
	  push dx	  
	  mov ax,dx
	  sub ax,10
	  mov dx,0
	  mov bx, Rect_Down
	  div bx
	  mov dx,0
	  mov bx,8
	  mul bx	  
	  add di,ax
	  pop dx
	  ;----------------------------------------------------
	  cmp [board+di],4;check if the pressed place is possible for moving
	  je check_if_not_the_same_place5
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
	  jmp in_game
check_if_not_the_same_place5:	  
      cmp di,si
      jne move8
	  call close_options
	   mov [eat_option],0;prepare to the next turn
	   mov[byte ptr there_is_movment_option],0;prepare to the next turn
      jmp in_game
move8:
	 push si
	 sub si,di
	 cmp si,9
	 ja eat8
	 cmp [eat_option],1;if there are no options to move the player lose
	 jne no_burn8
	 pop si
     mov [board+si],1
	 inc [W_eaten]
     call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game
no_burn8:	 
     
	 
	 pop si
	 call check_burn_white
	 call mov_last_board_to_board
	 call move_up
	 
	 cmp [burn],1
	 jne skip4
	 push si
	 mov si,[could_eat]
	 mov [board+si],1
	 pop si
	 inc [W_eaten]
skip4:	
	 
	 
	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 mov [burn],0
	 jmp in_game
eat8:
     pop si	
     call eat_black_up_move	 
	 call close_options
	 mov [eat_option],0;prepare to the next turn
	 inc [player]
	 mov[byte ptr there_is_movment_option],0;prepare to the next turn
	 jmp in_game



	
@@ret:
	 ;hide mouse
     mov ax,2
	 int 33h
     cmp [white_win],1
	 jne white_lose
	 mov dx,offset W_win
	 mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], 320
	mov [BmpRowSize] ,200 
	 call OpenShowBmp
	 jmp white_wins
white_lose:
     mov dx,offset B_win
	 mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], 320
	mov [BmpRowSize] ,200
	 call OpenShowBmp	 
white_wins:	 
	 ;----------------------------------------------------
   ;show mouse
   
	 mov ax,1
	 int 33h
	 ret
endp multiplayer 
; this proc is for debuging
;================================================
; Description - Write on screen the value of ax (decimal)
;               the practice :  
;				Divide AX by 10 and put the Mod on stack 
;               Repeat Until AX smaller than 10 then print AX (MSB) 
;           	then pop from the stack all what we kept there and show it. 
; INPUT: AX
; OUTPUT: Screen 
; Register Usage: AX  
;================================================
proc ShowAxDecimal
       push ax
	   push bx
	   push cx
	   push dx
	   
	   ; check if negative
	   test ax,08000h
	   jz PositiveAx
			
	   ;  put '-' on the screen
	   push ax
	   mov dl,'-'
	   mov ah,2
	   int 21h
	   pop ax

	   neg ax ; make it positive
PositiveAx:
       mov cx,0   ; will count how many time we did push 
       mov bx,10  ; the divider
   
put_mode_to_stack:
       xor dx,dx
       div bx
       add dl,30h
	   ; dl is the current LSB digit 
	   ; we cant push only dl so we push all dx
       push dx    
       inc cx
       cmp ax,9   ; check if it is the last time to div
       jg put_mode_to_stack

	   cmp ax,0
	   jz pop_next  ; jump if ax was totally 0
       add al,30h  
	   mov dl, al    
  	   mov ah, 2h
	   int 21h        ; show first digit MSB
	       
pop_next: 
       pop ax    ; remove all rest LIFO (reverse) (MSB to LSB)
	   mov dl, al
       mov ah, 2h
	   int 21h        ; show all rest digits
       loop pop_next
		
	   mov dl, ','
       mov ah, 2h
	   int 21h
   
	   pop dx
	   pop cx
	   pop bx
	   pop ax
	   
	   ret
endp ShowAxDecimal

proc show_option_in_random_place
     push bx
	 push ax
@@again:	 
     mov bl,0
	 mov bh,64
	 call RandomByCs
	 mov ah,0
	 mov si,ax
	 cmp [board+si],1
	 jne @@again
     mov [board+si],4

     pop ax
	 pop bx
     ret
endp show_option_in_random_place	 
 
EndOfCsLbl:
END start


