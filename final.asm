			title	"Maze Solver"
			list	p=16f84A
			radix	hex
			include	"p16f84A.inc"

COUNTER		EQU		d'12'
COUNTER2	EQU		d'13'
TIMER3		EQU		d'14'
SELECT		EQU		d'15'
COUNTER     EQU     d'16'

			ORG		0x0
			GOTO	START

			ORG 	0x04

			BTFSC	INTCON,INTF	;RB0 interrupt flag test
			GOTO	RB0Interrupt

			BTFSC	INTCON, T0IF	;timer interrupt flag test
			GOTO	timerInterrupt	
			
START		CLRF	PORTA

			;clearing flags for menu star
			BCF		SELECT,F
			MOVLW	b'10'
			MOVWF	SELECT	

			
			BSF		STATUS,RP0	;jumping to bank 1
			CLRF	TRISA	;all port A out
			
			;CONFIGURING PORTB PINS:
			;RB0 -> OUTPUT TO BUZZER
			;RB4-7 -> INPUTS 
			;RB2 , RB3 -> OUTPUTS TO LEDS
			;RB1 -> OUTPUT TO LCD ENABLE
			MOVLW	0xF0	
			MOVWF	TRISB
			
			MOVLW	b'10000111'	;SETTING OPTION REG to support interrupts
			MOVWF	OPTION_REG
			BCF		STATUS,RP0	;jumping back to bank 0

			CLRF 	PORTA	;clearing PORTA

			CALL	POWERUPDELAY ;delaying 40ms waiting power up LCD
			
			MOVLW	b'00010'
			CALL	ET
			
			MOVLW	b'00010'
			CALL	ET

			MOVLW	b'01000'	;N=1 2 line mode, F=0
			CALL	ET

			MOVLW	b'00000'	;initializing display
			CALL	ET			

			MOVLW	b'01100'	;setting display to ON
			CALL	ET

			MOVLW	b'00000'
			CALL	ET

			MOVLW	b'00110'	;shift off, increment on
			CALL	ET

			CALL 	CLEARDISPLAY

			

WELCOME 	CALL	charSp	;Welcome Screen
			CALL	charSp
			CALL	charSp
			CALL	letterM
			CALL	letterA
			CALL	letterZ
			CALL	letterE
			CALL	charSp
			CALL	letterS
			CALL	letterO
			CALL	letterL
			CALL	letterV
			CALL	letterE
			CALL	letterR

			CALL 	POWERUPDELAY	; delay before clearing the display
			CALL	CLEARDISPLAY	; clear display		



;----------------|
;START MENU		 |
;----------------|

MENU		CALL	charSp  ;Mode Selection Menu
			CALL	charSp
			CALL	charSp
			CALL	charSp
			CALL	charStar
			CALL	letterD
			CALL	letterE
			CALL	letterF
			CALL	letterA
			CALL	letterU
			CALL	letterL
			CALL	letterT

			;Newline
			MOVLW b'01100' ; 3 bits to jump address
			CALL ET
		
			MOVLW b'00000' ; 4 bits to jump address
			CALL ET
			
			CALL	charSp
			CALL	letterO
			CALL	letterB
			CALL	letterS
			CALL	letterT
			CALL	letterA
			CALL	letterC
			CALL	letterL
			CALL	letterE
			CALL	charSp
			CALL	charSp
			CALL	charSp
			CALL	letterM
			CALL	letterA
			CALL	letterZ
			CALL	letterE

			;Initializing first timer delay
			CLRF	TMR0
			MOVLW	d'38'
			MOVWF	TIMER3

			BSF		INTCON,RBIE
			BSF		INTCON,T0IE
			BSF		INTCON,GIE


MENULOOP	GOTO	MENULOOP

;-------------------------|
;END MAIN PROGRAM         |
;-------------------------| 





ET		MOVWF	PORTA
		BSF		PORTB,1; making a falling edge
		NOP
		BCF		PORTB,1
		CALL	PRINTDELAY; this is to wait for LCD to stop executing
		RETURN



POWERUPDELAY	MOVLW	d'00'	; setting up 40ms delay
				MOVWF	COUNTER
				MOVLW	d'51'
				MOVWF	COUNTER2

LOOP	INCFSZ	COUNTER,F
		GOTO	LOOP
		DECFSZ	COUNTER2,F
		GOTO	LOOP
		RETURN

PRINTDELAY	MOVLW	d'00'; SETTING UP 3.85MS DELAY
			MOVWF 	COUNTER	
			MOVLW 	d'5'
			MOVWF	COUNTER2

LOOP1	INCFSZ	COUNTER,F
		GOTO	LOOP1	
		DECFSZ	COUNTER2,F
		GOTO 	LOOP1
		RETURN

CLEARDISPLAY	MOVLW b'00000' ;initializing clear display
				CALL ET
				MOVLW b'00001' ;clear display
				CALL ET
				RETURN







charSp	MOVLW b'10010' ;upper bits of Space character
		CALL ET
		MOVLW b'10000' ;lower bits of Space character
		CALL ET	
		RETURN		
letterV	MOVLW b'10101' ;upper bits of V
		CALL ET
		MOVLW b'10110' ;lower bits of V
		CALL ET	 
		RETURN
letterR	MOVLW b'10101' ;upper bits of R
		CALL ET
		MOVLW b'10010' ;lower bits of R
		CALL ET	 
		RETURN
letterM	MOVLW b'10100' ;upper bits of M
		CALL ET
		MOVLW b'11101' ;lower bits of M
		CALL ET	
		RETURN
letterD	MOVLW b'10100' ;upper bits of D
		CALL ET
		MOVLW b'10100' ;lower bits of D
		CALL ET	
		RETURN
letterE	MOVLW b'10100' ;upper bits of E
		CALL ET
		MOVLW b'10101' ;lower bits of E
		CALL ET	
		RETURN
letterF	MOVLW b'10100' ;upper bits of F
		CALL ET
		MOVLW b'10110' ;lower bits of D
		CALL ET	
		RETURN
letterA	MOVLW b'10100' ;upper bits of A
		CALL ET
		MOVLW b'10001' ;lower bits of A
		CALL ET	
		RETURN
letterU	MOVLW b'10101' ;upper bits of U
		CALL ET
		MOVLW b'10101' ;lower bits of U
		CALL ET	
		RETURN
letterL	MOVLW b'10100' ;upper bits of L
		CALL ET
		MOVLW b'11100' ;lower bits of L
		CALL ET	
		RETURN
letterT	MOVLW b'10101' ;upper bits of T
		CALL ET
		MOVLW b'10100' ;lower bits of T
		CALL ET	
		RETURN
letterO	MOVLW b'10100' ;upper bits of O
		CALL ET
		MOVLW b'11111' ;lower bits of O
		CALL ET	
		RETURN
letterB	MOVLW b'10100' ;upper bits of B
		CALL ET
		MOVLW b'10010' ;lower bits of B
		CALL ET	
		RETURN
letterS	MOVLW b'10101' ;upper bits of S
		CALL ET
		MOVLW b'10011' ;lower bits of S
		CALL ET	
		RETURN
letterC	MOVLW b'10100' ;upper bits of C
		CALL ET
		MOVLW b'10011' ;lower bits of C
		CALL ET	
		RETURN
letterZ	MOVLW b'10101' ;upper bits of Z
		CALL ET
		MOVLW b'11010' ;lower bits of Z
		CALL ET	
		RETURN
charStar	MOVLW b'10010' ;upper bits of Star Character
		CALL ET		
		MOVLW b'11010' ;lower bits of Star Character
		CALL ET	
		RETURN
rectangle	MOVLW	b'11111'
			CALL	ET
			MOVLW	b'11111'
			CALL	ET
			RETURN
empty		MOVLW	b'10101'        ;_
			CALL	ET	
			MOVLW	b'11111'
			CALL	ET
			RETURN
at			MOVLW	b'10100'
			CALL	ET	
			MOVLW	b'10000'
			CALL	ET
			RETURN
comma		MOVLW	b'10010'
			CALL	ET	
			MOVLW	b'11100'
			CALL	ET
			RETURN
zero		MOVLW	b'10011'
			CALL	ET	
			MOVLW	b'10000'
			CALL	ET
			RETURN
five		MOVLW	b'10011'
			CALL	ET	
			MOVLW	b'10101'
			CALL	ET
			RETURN




;---------------TIMER INTERRUPT CODE--------------------------------
timerInterrupt	CALL	POWERUPDELAY
				DECFSZ	TIMER3,F ;decrementing timer3 to get 3s delay
				GOTO	RETTIMER3

				;INCREMENTING START POSITION
				MOVLW	d'49'
				MOVWF	TIMER3
				CLRF	TMR0
				GOTO	INCREMENTPOINTER
				
				
RETTIMER3	BCF	INTCON,T0IF	;CLEARING TIMER INTERRUPT FLAG
			RETFIE
;------------------------------------------------------------------




;---------------INCREMENT POINTER CODE----------------------------
INCREMENTPOINTER	BTFSS	SELECT,0
					GOTO	S00CHECKS1
					BTFSC	SELECT,0
					GOTO	S01CHECKS1

S00CHECKS1		BTFSS	SELECT,1
				GOTO	POSITION1
				BTFSC	SELECT,1
				GOTO	POSITION2

S01CHECKS1		BTFSS	SELECT,1
				GOTO	POSITION3
				GOTO	RETTIMER3

;CLEARING CAN BE OPTIMIZED FURTHER IF NECESSARY
POSITION1		MOVLW	b'01100' ;4AH IS THE LOCATION
				CALL 	ET
		
				MOVLW b'01011' ; 4 bits to jump address
				CALL ET	
				CALL	charSp


				MOVLW b'01000' ; 00 IS THE LOCATION
				CALL ET
		
				MOVLW b'00100' ; 4 bits to jump address
				CALL ET

				CALL	charStar;print star before DEFAULT
				BSF		SELECT,1
				
				GOTO	RETTIMER3

;CLEARING CAN BE OPTIMIZED FURTHER IF NECESSARY
POSITION2		MOVLW 	b'01000' ; clearing position 1
				CALL 	ET
		
				MOVLW 	b'00100' ; 4 bits to jump address
				CALL 	ET
				CALL	charSp		


				MOVLW b'01100' ; 40H IS THE LOCATION
				CALL ET
		
				MOVLW b'00000' ; 4 bits to jump address
				CALL ET	
				
				CALL	charStar;print star before OBSTACLE	

				BSF		SELECT,0
				BCF		SELECT,1
				GOTO	RETTIMER3
				

;CLEARING CAN BE OPTIMIZED FURTHER IF NECESSARY
POSITION3		MOVLW b'01100' ; clearing position 2
				CALL ET
		
				MOVLW 	b'00000' ; clearing position 2
				CALL 	ET	
				CALL	charSp

				MOVLW	b'01100' ;4AH IS THE LOCATION
				CALL 	ET
		
				MOVLW b'01011' ; 4 bits to jump address
				CALL ET	
				
				CALL	charStar;print star before MAZE
				BCF		SELECT,0
				BCF		SELECT,1
				GOTO	RETTIMER3

;-----------------------------------------------------------------------------	

INDA           	MOVLW d'20'
				MOVWF FSR
				MOVLW d'10'
				MOVWF COUNTER3

				CALL CLEARDISPLAY

LOOP1M          BTFSC INDE,0
				CALL rectangle
 				
				BTFSC INDE,1
				CALL empty

				BTFSC INDE,2
				CALL letterS

				BTFSC INDE,3
				CALL letterE

				INCF FSR
				DECFSZ COUNTER3

				GOTO  LOOP1M
				RETURN

INDA1           MOVLW d'30'
				MOVWF FSR
				MOVLW d'10'
				MOVWF COUNTER3

				CALL CLEARDISPLAY

LOOP2M          BTFSC INDE,0
				CALL rectangle
 				
				BTFSC INDE,1
				CALL empty

				BTFSC INDE,2
				CALL letterS

				BTFSC INDE,3
				CALL letterE

				INCF FSR
				DECFSZ COUNTER3

				GOTO  LOOP2M
				RETURN
;-------------------------------------Mode 1 ---------------------------------
MODE1			CALL CLEARDISPLAY
				BSF d'20',0 ;obstacle
				BSF d'21',1 ;empty
				BSF d'22',1 ;empty
				BSF d'23',1 ;empty
				BSF d'24',1 ;empty
				BSF d'25',2 ;S
				BSF d'26',1 ;empty
				BSF d'27',1 ;empty
				BSF d'28',1 ;empty
				BSF d'29',1 ;empty
				BSF d'30',3 ;E
				BSF d'31',0 ;obstacle
				BSF d'32',1 ;empty
				BSF d'33',0 ;obstacle
				BSF d'34',0 ;obstacle
				BSF d'35',1 ;empty
				BSF d'36',1 ;empty
				BSF d'37',1 ;empty
				BSF d'38',0 ;obstacle
				BSF d'39',0 ;obstacle

				BSF PORTB,2
				BCF PORTB,3
				
				MOVLW d'20'
				MOVWF FSR
				MOVLW d'10'
				MOVWF COUNTER3


				CALL INDA
				CALL charSp
				CALL charSp
				CALL charSp
				CALL letterS
				CALL charSp
				CALL at

				CALL INDA1
				CALL charSp
				CALL charSp
				CALL charSp
				CALL zero
				CALL comma
				CALL five


;-----------------------------MODE 2 ----------------------------------------
MODE2			CALL CLEARDISPLAY
				BSF d'20',2 ;S
				BSF d'21',1 ;empty
				BSF d'22',1 ;empty
				BSF d'23',1 ;empty
				BSF d'24',1 ;empty
				BSF d'25',1 ;empty
				BSF d'26',1 ;empty
				BSF d'27',1 ;empty
				BSF d'28',1 ;empty
				BSF d'29',1 ;empty
				BSF d'30',1 ;empty
				BSF d'31',1 ;empty
				BSF d'32',1 ;empty
				BSF d'33',1 ;empty
				BSF d'34',1 ;empty
				BSF d'35',1 ;empty
				BSF d'36',1 ;empty
				BSF d'37',1 ;empty
				BSF d'38',1 ;empty
				BSF d'39',3 ;E
				
			

				BSF PORTB,2
				BCF PORTB,3  ;for leds
				
				MOVLW d'20'
				MOVWF FSR
				MOVLW d'10'
				MOVWF COUNTER3


				CALL INDA
				CALL charSp
				CALL charSp
				CALL charSp
				CALL letterS
				CALL charSp
				CALL at

				CALL INDA1
				CALL charSp
				CALL charSp
				CALL charSp
				CALL zero
				CALL comma
				CALL zero






END