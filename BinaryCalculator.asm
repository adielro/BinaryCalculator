		LIST 	P=PIC16F877
		include	<P16f877.inc>
 __CONFIG _CP_OFF & _WDT_OFF & _BODEN_OFF & _PWRTE_OFF & _HS_OSC & _WRT_ENABLE_ON & _LVP_OFF & _DEBUG_OFF & _CPD_OFF

		org		0x00		
reset:	goto	start

		org		0x10
start:	
		bcf		STATUS, RP0
		bcf		STATUS, RP1			; Bank0 
		clrf	PORTD
		clrf	PORTE

		bsf		STATUS, RP0			; Bank1 <------
;-------------------------------------------------
		movlw   0x06
        movwf   ADCON1
        bcf     INTCON,GIE 	;Disable all interupts
        movlw   0x0F		;4 Low bits of PORTB are input,4 High bits output
        movwf   TRISB
        bcf     OPTION_REG,0x07 ; RBPU is ON -->Pull UP on PORTB is enabled

		clrf	TRISE		;porte output 
		clrf	TRISD		;portd output

		movwf	OPTION_REG
		bcf		OPTION_REG ,0x7
		clrf	TRISD				; PortD output

		bcf		STATUS, RP0			; Bank0 <------

		bcf		STATUS, C ; clearing the carry bit in status
		bcf		STATUS, Z; clearing the zero bit in status
		clrf	0x30 ; register for A (first number)
		movlw	' '
		movwf	0x35 ; register for negative or positive
		clrf	0x36 ; temp register 
		clrf	0x37 ; temp2 register
		clrf	0x40 ; register for B (second number)
		clrf	0x50 ; register for C (operand)
		clrf	0x60 ; register for the result
		clrf	0x61 ; temp register for power res
		clrf	0x62 ; char or num register ----> 1 == num, 0 == char
		clrf	0x63 ; tepm register in order to check if the result is above MAX
		call	init

; Using keyboard function for A(first number), B(second number), and C(operand id).
; While reciving A, B, and C keys only them will work in that excact order, while numbers are expected, only keys 0 and 1 will work.

		bsf		0x62, 1
		call	keyboard
		call	init_A
		bcf		0x62, 1

		bsf		0x62, 2
		call	keyboard
		call	init_B
		bcf		0x62, 2

		bsf		0x62, 3
		call	keyboard
		call	init_C
		bcf		0x62, 3
		
		call	which_operator ; Using a functin in order to navigate to the right code (operand switch case)
		call	init_result ; Function in order to print the result into the LCD screen.
halas:
		goto halas

init_A:
; This function will is using the keyboard and delay function in order to receive bit for the first number (A).

	incf	0x62
	call	delay
	call	delay
	call	delay
	call	delay

	movlw	0x80	 ;PLACE for the data on the LCD 
	movwf	0x20
	call 	lcdc

	movlw	'A'			; CHAR (the data )
	movwf	0x20
	call 	lcdd



	call	keyboard
	iorwf 	0x30
	rlf		0x30
	addlw	0x30
	movwf	0x20
	call 	lcdd



	call 	delay
	call 	delay
	call 	delay
	call 	delay

	call	keyboard
	iorwf	0x30
	rlf		0x30
	addlw	0x30
	movwf	0x20
	call 	lcdd

	call 	delay
	call 	delay
	call 	delay
	call 	delay

	call	keyboard
	iorwf	0x30
	rlf		0x30
	addlw	0x30
	movwf	0x20
	call 	lcdd

	call 	delay
	call 	delay
	call 	delay
	call 	delay

	call	keyboard
	iorwf	0x30
	addlw	0x30
	movwf	0x20
	call 	lcdd


	call	delay
	call	delay
	call	delay
	call	delay

	decf	0x62

	return



init_B:
; This function is using the keyboard and delay function in order to receive bit for the second number (B).

	call	delay
	call	delay
	call	delay
	call	delay

	movlw	0x85	 ;PLACE for the data on the LCD 
	movwf	0x20
	call 	lcdc

	movlw	'B'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	incf	0x62

	call	keyboard
	iorwf 	0x40
	rlf		0x40
	addlw	0x30
	movwf	0x20
	call 	lcdd

	call 	delay
	call 	delay
	call 	delay
	call 	delay

	call	keyboard
	iorwf	0x40
	rlf		0x40
	addlw	0x30
	movwf	0x20
	call 	lcdd

	call 	delay
	call 	delay
	call 	delay
	call 	delay

	call	keyboard
	iorwf	0x40
	rlf		0x40
	addlw	0x30
	movwf	0x20
	call 	lcdd

	call 	delay
	call 	delay
	call 	delay
	call 	delay

	call	keyboard
	iorwf	0x40
	addlw	0x30
	movwf	0x20
	call 	lcdd

	call	delay
	call	delay
	call	delay
	call	delay

	decf	0x62	

	return



init_C:
; This function is using keyboard and delay funtion in order to fill the operand register (C).	

	call	delay
	call	delay
	call	delay
	call	delay

	movlw	0x8B	 ;PLACE for the data on the LCD 
	movwf	0x20
	call 	lcdc

	movlw	'C'			; CHAR (the data )
	movwf	0x20
	call 	lcdd

	incf	0x62

	call	keyboard
	iorwf 	0x50
	rlf		0x50
	addlw	0x30
	movwf	0x20
	call 	lcdd

	call 	delay
	call 	delay
	call 	delay
	call 	delay

	call	keyboard
	iorwf	0x50
	rlf		0x50
	addlw	0x30
	movwf	0x20
	call 	lcdd

	call 	delay
	call 	delay
	call 	delay
	call 	delay

	call	keyboard
	iorwf	0x50
	rlf		0x50
	addlw	0x30
	movwf	0x20
	call 	lcdd

	call 	delay
	call 	delay
	call 	delay
	call 	delay

	call	keyboard
	iorwf	0x50
	addlw	0x30
	movwf	0x20
	call 	lcdd

	decf	0x62
	
	return

init_result:
; This function is printing "RESULT:" on the second line of the screen and check if the result (0x60 register) is above max value (15), if it is, goto function print_max
; if the value is valid [-15,15], so printing the sign (+,-) and the result (binary, 4 bits) using the print_one and print_zero functions.
	
	movlw	0xC0	 ;PLACE for the data on the LCD 
	movwf	0x20
	call 	lcdc
	
	movlw	'R'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	'e'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	's'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	'u'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	'l'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	't'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	':'			; CHAR (the data )
	movwf	0x20
	call 	lcdd

	bcf	STATUS, C
	movlw	0xf
	movwf	0x63  ; 15
	movfw	0x60  ; result in w
	subwf	0x63  ; f - w ->>> 15 - res
	btfss	STATUS, C
	goto	print_max

	movfw	0x35			; CHAR (the data )
	movwf	0x20
	call 	lcdd

	btfss	0x60, 3
	call	print_zero
	btfsc	0x60, 3
	call	print_one

	btfss	0x60, 2
	call	print_zero
	btfsc	0x60, 2
	call	print_one

	btfss	0x60, 1
	call	print_zero
	btfsc	0x60, 1
	call	print_one

	btfss	0x60, 0
	call	print_zero
	btfsc	0x60, 0
	call	print_one


	return


; This method print '0' to the LCD screen
print_zero:
	movlw	'0'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	return

; This method print '1' to the LCD screen
print_one:
	movlw	'1'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	return



; -----IMPORTANT METHOD------

; this method will check the input to C (r. 0x50)

which_operator:
	
	movlw	1
	subwf	0x50, w
	btfsc	STATUS, Z
	goto sub_operator ; sub has chosen

	movlw	2
	subwf	0x50, w
	btfsc	STATUS, Z
	goto 	mult_operator ; mult has chosen
	
	movlw	3
	subwf	0x50,	w
	btfsc	STATUS, Z	
	goto	divide_operator ; divide has chosen

	movlw	4
	subwf	0x50,	w
	btfsc	STATUS, Z
	goto	power_operator ; power has chosen

	movlw	5
	subwf	0x50,	w
	btfsc	STATUS, Z
	goto	count_ones_A ; ; count ones has chosen

	movlw	6
	subwf	0x50,	w
	btfsc	STATUS, Z
	goto	count_zeros_B ; count zeros has chosen


	movlw	7
	subwf	0x50,	w
	btfsc	STATUS, Z
	goto	ones_pattern ; ;ones adj has chosen

	goto	init_error ; genious move

	return		



; this method will sub B from A. 
; if A is greater than B {
;	 res = A-B -> THE RESULT SIGN IS: '+'
; else 
;    res = B-A -> THE RESULT SIGN IS: '-'
sub_operator:
	movlw	0x2b ;res = A-B -> THE RESULT SIGN IS: '+'
	movwf	0x35
	bsf		STATUS, C ;	
	movfw	0x40
	subwf	0x30, w 
	btfss	STATUS, C ; if 0 it's negative
	goto	negative
	movwf	0x60
	return
negative:
	movlw	0x2d ;B-A -> THE RESULT SIGN IS: '-'
	movwf	0x35
	movfw	0x30
	subwf	0x40, w
	movwf	0x60

	return

; this method will mult A and B.
; if A or B is 0
; 	 than res = 0
;else
;    
mult_operator:
	
	multiply:
	movlw	0
	subwf	0x30, w
	btfsc	STATUS, Z
	goto	return_zero	
	movlw	0
	subwf	0x40, w
	btfsc	STATUS, Z
	goto	return_zero
	
not_zero:
	movlw	0
	addwf	0x30, w ; w = a
	addwf	0x60, 1 ; res = w
	decf	0x40 ; b--;
	btfsc	STATUS, Z
	return
	goto	not_zero
	

return_zero:
	clrf	0x60


	return


divide_operator:
; first we will check if we divide by 0. 
; else, go to divide.
; divide method will check how many times B can fits in A
; if it fits count++;
; result = count
	divide:

	movlw	0
	subwf	0x40, w
	btfsc	STATUS, Z
	goto	divided_by_zero ; if B is ZERO
	
	dividing:
	bcf	STATUS, C
	movfw	0x40
	subwf	0x30, 1
	btfss	STATUS, C
	return
	incf	0x60 ; count++
	goto	dividing ;loop


	
; if B is 0, we print "ERROR" on the LCD
divided_by_zero:
	goto	init_error
	return


; In this function we power A with B, result is shown in C.
; The function mult A by itself B times, while saving the original A and B in temp1 and temp2 registers and going in a loop untill we multed A by itself B times.
power_operator:
; First we check if B = 1, if so, the result is A, and we move it to result (0x60)
	movlw	1
	subwf	0x40, w
	btfss	STATUS, Z
	goto	first_test
	movfw	0x30
	movwf	0x60
	return

first_test:
; if B != 1, we check if A = 0, if so, we check if B is also 0, if both are 0, we print "ERROR" on the lcd ( 0 ^ 0 = ERROR).
; if A != 0 go to next test 
	movlw	0
	subwf	0x30, w
	btfss	STATUS, Z
	goto	next_test
	movlw	0
	subwf	0x40, w
	btfsc	STATUS, Z
	goto	init_error
	clrf	0x60
	return
; if A != 0 and B = 0 then the result is 1, move 1 to 0x60 (result)
; finaly, if A != 0 and B != 0 , go to pow_init
next_test:
	movlw	0
	subwf	0x40, w
	btfss	STATUS, Z
	goto	pow_init
	movlw	1
	movwf	0x60
	return
; move B (0x40) to 0x36 (temp register), then move A (0x30) to B (0x40), and move A to 0x37 (temp register 2)
pow_init:
	movfw	0x40
	movwf	0x36 ; 0x36 = b (temp)
	movfw	0x30
	movwf	0x40 ; b = a
	movwf	0x37 ; temp2 = a

; loop for the power, decrement 0x36 (represent B, temp register 1) if 0x36 = 0, we are done, moving 0x30 to 0x60( result ) and return
pow:
	decf	0x36
	movfw	0x37 
	movwf	0x40 ; b = temp 
	movlw	0
	subwf	0x36, w
	btfss	STATUS, Z
	goto	not_done
	movfw	0x30
	movwf	0x60
	return	

; if 0x36 != 0 (not done yet) we call the mult_operator and moving the result to 0x30
not_done:
	clrf	0x60 ; res = 0
	call	mult_operator
	movfw	0x60
	movwf	0x30
	goto	pow


; This function check how many ones ("1") we have in A (0x30 register), initialzing the result in 0x60.
; The function check each bit in A (3, 2, 1, 0) if the bit is set (1) we increment the result (0x60) by one.
count_ones_A:
	btfsc	0x30, 3
	incf	0x60
	btfsc	0x30, 2
	incf	0x60
	btfsc	0x30, 1
	incf	0x60
	btfsc	0x30, 0
	incf	0x60
	
	return

; This function check how many zeros ("0") we have in B (0x40 register), initialzing the result in 0x60.
; The function check each bit in B (3, 2, 1, 0) if the bit is clear (0) we increment the result (0x60) by one.
count_zeros_B:
	btfss	0x40, 3
	incf	0x60
	btfss	0x40, 2
	incf	0x60
	btfss	0x40, 1
	incf	0x60
	btfss	0x40, 0
	incf	0x60

	return

; This function check how many times we have the pattern "11" in A, if A is "1111" so the result i 4.
; The function check a couple of bits at a time ( 3 & 2, 2 & 1, 1 & 0) if both set (1) we increment the result (0x60) by one.
ones_pattern: ; how many ones adjact to each other
	btfss	0x30, 3
	goto	skip1
	btfsc	0x30, 2
	incf	0x60
skip1:
	btfss	0x30, 2
	goto	skip2
	btfsc	0x30, 1
		incf	0x60
skip2:
	btfss	0x30, 1
	goto	skip3
	btfsc	0x30, 0
	incf	0x60
skip3:
	
return


; This function print "ERROR" to the LCD screen.
; Error occured when we do: "A/0", "0^0".
init_error:
	movlw	0xC6	 ;PLACE for the data on the LCD 
	movwf	0x20
	call 	lcdc

	movlw	'E'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	'R'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	'R'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	'O'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	'R'			; CHAR (the data )
	movwf	0x20
	call 	lcdd

	goto	halas

; This function print "MAX :(" to the LCD screen.
; This occured if the result is above 0xf (15).
print_max:

	movlw	'M'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	'A'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	'X'			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	' '			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	movlw	':'			; CHAR (the data )
	movwf	0x20
	call 	lcdd

	movlw	'('			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	
	goto	halas



; Keyboard function, when the user need to insert "A" only the "A" key will work, so with "B" and "C".
; When the user need to insert "0" or "1" only those keys will work at the time.

keyboard:
;-----------------------------------------------------------------------
wkb:    bcf             PORTB,0x4     ;Line 0 of Matrix is enabled
        bsf             PORTB,0x5
        bsf             PORTB,0x6
        bsf             PORTB,0x7
;-----------------------------------------------------------------------
		btfss			0x62,	0
		goto			skip_1
		btfss           PORTB,0x0     ;Scan for 1,A
        goto            kb01
skip_1:
		btfsc			0x62,	0
		goto			skip_a
		btfss			0x62,	1
		goto			skip_a
        btfss           PORTB,0x3
        goto            kb0a
skip_a:
;-----------------------------------------------------------------------
        bsf             PORTB,0x4	;Line 1 of Matrix is enabled
        bcf             PORTB,0x5
;-----------------------------------------------------------------------
       	btfsc			0x62,	0
		goto			skip_b
		btfss			0x62,	2
		goto			skip_b
		btfss           PORTB,0x3
        goto            kb0b
skip_b:
;-----------------------------------------------------------------------	
        bsf             PORTB,0x5	;Line 2 of Matrix is enabled
        bcf             PORTB,0x6
;-----------------------------------------------------------------------
       	btfsc			0x62,	0
		goto			skip_c
		btfss			0x62,	3
		goto			skip_c
        btfss           PORTB,0x3
        goto            kb0c
skip_c
;-----------------------------------------------------------------------
        bsf             PORTB,0x6	;Line 3 of Matrix is enabled
        bcf             PORTB,0x7
;----------------------------------------------------------------------
		btfss			0x62,	0
		goto			skip_0
        btfss           PORTB,0x1
        goto            kb00
skip_0

;----------------------------------------------------------------------

        goto            wkb

kb00:   movlw           0x00
        goto            disp	
kb01:   movlw           0x01
        goto            disp	
	
kb0a:   movlw           0x0a
        goto            disp	
kb0b:   movlw           0x0b
        goto            disp	
kb0c:   movlw           0x0c
        goto            disp	


disp:   movwf          PORTD
	return



delay:					
		movlw		0xFF		
		movwf		0x41
CONT1:		movlw		0xFF		
		movwf		0x42
CONT2:		decfsz		0x42,f
		goto		CONT2
		decfsz		0x41,f
		goto		CONT1
		return	




;subroutine to initialize LCD
;
init	movlw	0x30
		movwf	0x20
		call 	lcdc
		call	del_41

		movlw	0x30
		movwf	0x20
		call 	lcdc
		call	del_01

		movlw	0x30
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x01		; display clear
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x06		; ID=1,S=0 increment,no  shift 000001 ID S
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x0c		; D=1,C=B=0 set display ,no cursor, no blinking
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x38		; dl=1 ( 8 bits interface,n=12 lines,f=05x8 dots)
		movwf	0x20
		call 	lcdc
		call	mdel
		return



;
;subroutine to write command to LCD
;

lcdc	movlw	0x00		; E=0,RS=0 
		movwf	PORTE
		movf	0x20,w
		movwf	PORTD
		movlw	0x01		; E=1,RS=0
		movwf	PORTE
        call	sdel
		movlw	0x00		; E=0,RS=0
		movwf	PORTE
		return

;
;subroutine to write data to LCD
;

lcdd	movlw		0x02		; E=0, RS=1
		movwf		PORTE
		movf		0x20,w
		movwf		PORTD
        movlw		0x03		; E=1, rs=1  
		movwf		PORTE
		call		sdel
		movlw		0x02		; E=0, rs=1  
		movwf		PORTE
		return

;----------------------------------------------------------
del_41	movlw		0xcd
		movwf		0x23
lulaa6	movlw		0x20
		movwf		0x22
lulaa7	decfsz		0x22,1
		goto		lulaa7
		decfsz		0x23,1
		goto 		lulaa6 
		return


del_01	movlw		0x20
		movwf		0x22
lulaa8	decfsz		0x22,1
		goto		lulaa8
		return


sdel	movlw		0x19		; movlw = 1 cycle
		movwf		0x23		; movwf	= 1 cycle
lulaa2	movlw		0xfa
		movwf		0x22
lulaa1	decfsz		0x22,1		; decfsz= 12 cycle
		goto		lulaa1		; goto	= 2 cycles
		decfsz		0x23,1
		goto 		lulaa2 
		return


mdel	movlw		0x0a
		movwf		0x24
lulaa5	movlw		0x19
		movwf		0x23
lulaa4	movlw		0xfa
		movwf		0x22
lulaa3	decfsz		0x22,1
		goto		lulaa3
		decfsz		0x23,1
		goto 		lulaa4 
		decfsz		0x24,1
		goto		lulaa5
		return








end
