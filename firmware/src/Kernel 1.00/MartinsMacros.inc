	nolist
; Useful Macros

; compare and jump if not equal
; trashes W
cjne	macro	reg,val,addr
		movf	reg,W,1
		xorlw	val
		btfss	STATUS,Z
		goto	addr
		endm

; compare and jump if equal
; trashes W
cje		macro	reg,val,addr
		movf	reg,W,1
		xorlw	val
		btfsc	STATUS,Z
		goto	addr
		endm

;move literal to file
; trashes w

movlf	macro	literal,addr
		if	literal
		movlw	literal
		movwf	addr
		else
		clrf	addr
		endif
		endm

	list
