			title	"Maze Solver"
			list	p=16f84A
			radix	hex
			include	"p16f84A.inc"

COUNTER		EQU		d'12'
COUNTER2	EQU		d'13'
TIMER3		EQU		d'14'
SELECT		EQU		d'15'
DEBOUNCECOUNT	EQU		d'16'
ADDRESSING_COUNTER	EQU	d'17'
CURRENT_ADDRESS		EQU	d'18'
OBSTACLE_COUNTER	EQU	d'19'
REMAINING_NUMBER	EQU	d'50'
			ORG		0x0
			GOTO	START

			ORG 	0x04
			BTFSC	INTCON, RBIF	;interrupt for button presses
			GOTO	BUTTONPRESS
			BTFSC	INTCON, T0IF	;timer interrupt flag test
			GOTO	timerInterrupt	


			
START		CLRF	PORTA
			;clearing flags for menu star
			CLRF		SELECT
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
			CLRF	PORTB	;CLEARING PORTB

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

			CALL	CURSOR_MOVERIGHT

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
			
			CALL	NEWLINE
			
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
			BSF		INTCON,GIE

MENULOOP		GOTO	MENULOOP

DEFAULT_START	CALL CLEARDISPLAY
				CALL	SET_BLINKING
				MOVLW	b'110'
				MOVWF	SELECT
				
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
			

				BSF PORTB,3	;turn on RED LED	
				BCF PORTB,2 ;ground GREEN LED
				
				CALL INDA
				CALL charSp
				CALL charSp
				CALL charSp
				CALL letterS
				CALL charSp
				CALL at

				CALL NEWLINE

				CALL INDA1
				CALL charSp
				CALL charSp
				CALL charSp
				CALL nb0
				CALL comma
				CALL nb5

				MOVLW 	b'01000' ; Set Cursor Starting Position
				CALL 	ET
		
				MOVLW 	b'00101' 
				CALL 	ET

				RETFIE


OBSTACLE_START	CALL 	CLEARDISPLAY
				CALL	SET_BLINKING
				MOVLW	b'100'
				MOVWF	SELECT

				BSF 	d'20',2 ;S
				BSF 	d'21',1 ;empty
				BSF 	d'22',1 ;empty
				BSF 	d'23',1 ;empty
				BSF 	d'24',1 ;empty
				BSF 	d'25',1 ;empty
				BSF 	d'26',1 ;empty
				BSF 	d'27',1 ;empty
				BSF 	d'28',1 ;empty
				BSF 	d'29',1 ;empty
				BSF 	d'30',1 ;empty
				BSF 	d'31',1 ;empty
				BSF 	d'32',1 ;empty
				BSF 	d'33',1 ;empty
				BSF 	d'34',1 ;empty
				BSF 	d'35',1 ;empty
				BSF 	d'36',1 ;empty
				BSF 	d'37',1 ;empty
				BSF 	d'38',1 ;empty
				BSF 	d'39',3 ;E

				BSF 	PORTB,2	;TURN ON GREEN LED
				BCF 	PORTB,3 ;GROUND RED LED

				CALL	INDA
				CALL	charSp
				CALL	charSp
				CALL	charSp
				CALL	letterS
				CALL 	charSp
				CALL 	at
				
				CALL	NEWLINE
	
				CALL 	INDA1
				CALL 	charSp
				CALL 	charSp
				CALL 	charSp
				CALL 	nb0
				CALL 	comma
				CALL 	nb5

				MOVLW 	b'01000' ; Set Cursor Starting Position
				CALL 	ET
		
				MOVLW 	b'00001' 
				CALL 	ET

				MOVLW	d'0'
				MOVWF	ADDRESSING_COUNTER
				MOVWF	OBSTACLE_COUNTER				

				MOVLW	d'21'
				MOVWF	CURRENT_ADDRESS
				MOVWF	FSR

				RETFIE



MAZE_START		CALL 	CLEARDISPLAY
				CALL	SET_BLINKING
				MOVLW	b'101'
				MOVWF	SELECT

				BSF 	d'20',1 ;empty
				BSF 	d'21',1 ;empty
				BSF 	d'22',1 ;empty
				BSF 	d'23',1 ;empty
				BSF 	d'24',1 ;empty
				BSF 	d'25',1 ;empty
				BSF 	d'26',1 ;empty
				BSF 	d'27',1 ;empty
				BSF 	d'28',1 ;empty
				BSF 	d'29',1 ;empty
				BSF 	d'30',1 ;empty
				BSF 	d'31',1 ;empty
				BSF 	d'32',1 ;empty
				BSF 	d'33',1 ;empty
				BSF 	d'34',1 ;empty
				BSF 	d'35',1 ;empty
				BSF 	d'36',1 ;empty
				BSF 	d'37',1 ;empty
				BSF 	d'38',1 ;empty
				BSF 	d'39',1 ;empty

				BSF 	PORTB,2	;TURN ON GREEN LED
				BSF 	PORTB,3 ;TURN ON RED LED

				CALL	INDA
				CALL	charSp
				CALL	charSp
				CALL	letterO
				CALL	letterB
				CALL	letterS

				CALL	NEWLINE

				CALL	INDA1
				CALL	charSp
				CALL	letterR
				CALL	letterE
				CALL	letterM
				CALL	charSp
				CALL	nb5

				MOVLW 	b'01000' ; Set Cursor Starting Position
				CALL 	ET
		
				MOVLW 	b'00000' 
				CALL 	ET

				MOVLW	d'0'
				MOVWF	ADDRESSING_COUNTER
				MOVWF	OBSTACLE_COUNTER

				MOVLW	d'0'
				MOVWF	REMAINING_NUMBER

				MOVLW	d'20'
				MOVWF	CURRENT_ADDRESS
				MOVWF	FSR				

				BSF		INTCON,T0IE	;ENABLE TIMER INTERRUPT

				MOVLW	d'50'
				MOVWF	TIMER3
				CLRF	TMR0

				RETFIE

;-------------------------|
;END MAIN PROGRAM         |
;-------------------------| 


BUTTONPRESS		CALL	DEBOUNCE_DELAY	;CALLED WHEN ANY BUTTON IS PRESSED	
				BTFSS	PORTB, 4	;checking if button 1 pressed
				GOTO	MOVE_BUTTON
				BTFSS	PORTB, 5	;checking if button 2 pressed
				GOTO	CONFIRM_BUTTON
				BTFSS	PORTB, 6	;checking if button 3 pressed
				GOTO	START_BUTTON	
				BTFSS	PORTB, 7	;checking if button 4 pressed
				GOTO	END_BUTTON
				GOTO	RETURN_TO_PROGRAM
				
MOVE_BUTTON		BTFSS	SELECT, 2	; if bit 2 of select is cleared, we are in the menu
				GOTO	INCREMENT_POINTER ; used to navigate the menu
				GOTO	MOVE_CURSOR_MAZE ;to navigate the cursor around the maze

CONFIRM_BUTTON	BTFSS	SELECT, 2	; if bit 2 of select is cleared, we are in the menu
				GOTO	CHOOSE_MODE ; used to confirm the mode we are currently pointing at
				BTFSS	SELECT,1	; SELECT = 110 --> DEFAULT MODE / ELSE --> OBSTACLE MODE OR MAZE MODE
				GOTO	PLACE_OBSTACLECHOOSE
				GOTO	SOLVE_MAZE ; we solve the maze directly in default mode

CHOOSE_MODE		BCF		INTCON, RBIF
				BTFSS	SELECT,0
				GOTO	S0
				BTFSC	SELECT,0
				GOTO	S1
S0				BTFSS	SELECT,1
				GOTO	MAZE_START
				BTFSC	SELECT,1
				GOTO	DEFAULT_START
S1				BTFSS	SELECT,1
				GOTO	OBSTACLE_START
				GOTO	RETURN_TO_PROGRAM


START_BUTTON	BTFSS	SELECT, 2
				GOTO	RETURN_TO_PROGRAM
				BTFSS	SELECT, 0
				GOTO	RETURN_TO_PROGRAM
				
				BTFSS	INDF,1	;CHECK IF THE POSITION IS EMPTY
				GOTO	CANNOT_PLACE	;IF NOT EMPTY RETURN

				BTFSC	OBSTACLE_COUNTER,4	;BIT 4 IS SET WHEN START POSITION HAS ALREADY BEEN PLACED
				GOTO	CANNOT_PLACE

				CALL	letterS ;Print S
				CALL	RETURN_TO_PREV_POSITION
				BCF 	PORTB,3 ;GROUND RED LED
				BSF		OBSTACLE_COUNTER,4
				BSF		INDF, 2  ;SET THE ADDRESS AS CONTAINING THE STARTING POSITION
				BCF		INDF, 1 ;SET THE ADDRESS AS NOT EMPTY
				
				GOTO	CHECK_IF_DONE

END_BUTTON		BTFSS	SELECT, 2
				GOTO	RETURN_TO_PROGRAM
				BTFSS	SELECT, 0
				GOTO	RETURN_TO_PROGRAM

				BTFSS	INDF,1 ;CHECK IF THE POSITION IS EMPTY
				GOTO	CANNOT_PLACE ;IF NOT EMPTY RETURN

				BTFSC	OBSTACLE_COUNTER,5 ;BIT 5 IS SET WHEN END POSITION HAS ALREADY BEEN PLACED
				GOTO	CANNOT_PLACE

				CALL	letterE
				CALL	RETURN_TO_PREV_POSITION
				BCF 	PORTB,2	;GROUND GREEN LED
				BSF		OBSTACLE_COUNTER,5
				BSF		INDF, 3 ; SET THE ADDRESS AS CONTAINING THE END POSITION
				BCF		INDF, 1 ; SET THE ADDRESS AS NOT EMPTY
				
				GOTO	CHECK_IF_DONE
	
RETURN_TO_PROGRAM		BCF		INTCON, RBIF	
						RETFIE

CHECK_IF_DONE	BTFSS	OBSTACLE_COUNTER,0 ;CHECK IF ALL 5 OBSTACLES HAVE BEEN PLACED
				GOTO	RETURN_TO_PROGRAM
				BTFSS	OBSTACLE_COUNTER,2
				GOTO	RETURN_TO_PROGRAM
				BTFSS	OBSTACLE_COUNTER,4 ;CHECK IF THE START POSITION HAS BEEN PLACED
				GOTO	RETURN_TO_PROGRAM
				BTFSS	OBSTACLE_COUNTER,5 ;CHECK IF THE END POSITION HAS BEEN PLACED
				GOTO	RETURN_TO_PROGRAM
				GOTO	SOLVE_MAZE


;---------------TIMER INTERRUPT CODE--------------------------------
timerInterrupt	CALL	POWERUPDELAY
				DECFSZ	TIMER3,F ;decrementing timer3 to get 2s delay
				GOTO	RETTIMER3

				;INCREMENTING START POSITION
				MOVLW	d'50'
				MOVWF	TIMER3
				CLRF	TMR0
				BTFSC	ADDRESSING_COUNTER,3	;IF ADDRESSING_COUNTER = 9 --> MOVE TO THE OTHER ROW
				BTFSS	ADDRESSING_COUNTER,0
				GOTO	CURSOR_INCREMENT	
				GOTO	CURSOR_SWITCH_ROW		
				
RETTIMER3	RETFIE
;------------------------------------------------------------------




;---------------INCREMENT POINTER CODE----------------------------
INCREMENT_POINTER	BTFSS	SELECT,0
					GOTO	S00CHECKS1
					BTFSC	SELECT,0
					GOTO	S01CHECKS1

S00CHECKS1		BTFSS	SELECT,1
				GOTO	POSITION1
				BTFSC	SELECT,1
				GOTO	POSITION2

S01CHECKS1		BTFSS	SELECT,1
				GOTO	POSITION3
				GOTO	RETURN_TO_PROGRAM

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
				
				GOTO	RETURN_TO_PROGRAM

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
				GOTO	RETURN_TO_PROGRAM
				

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
				GOTO	RETURN_TO_PROGRAM

;---------------MOVE CURSOR CODE----------------------------

MOVE_CURSOR_MAZE	BTFSS	SELECT,0	;SELECT = 100 --> OBSTACLE MODE / SELECT = 101 --> MAZE MODE
					GOTO	MOVE_CURSOR_OBSTACLE
					GOTO	RETURN_TO_PROGRAM
					;GOTO	MOVE_CURSOR_MAZEMODE

MOVE_CURSOR_OBSTACLE	BTFSS	ADDRESSING_COUNTER, 3
						GOTO	CURSOR_INCREMENT
						BTFSC	ADDRESSING_COUNTER, 4
						GOTO 	SOLVE_MAZE
						GOTO	CURSOR_NEXTLINE

CURSOR_INCREMENT		BTFSC INDF,0
						CALL rectangle
						BTFSC INDF,1
						CALL empty
						BTFSC INDF,2
						CALL letterS
						BTFSC INDF,3
						CALL letterE	
						BTFSS	ADDRESSING_COUNTER, 4 ;IF BIT4 OF ADDRESSING COUNTER IS SET, WE ARE IN THE SECOND LINE AND MOVING BACKWARDS
						CALL	INCREMENT		;MOVE TO THE RIGHT				
						BTFSC	ADDRESSING_COUNTER, 4
						CALL	DECREMENT		;MOVE TO THE LEFT		
						INCF	ADDRESSING_COUNTER
						GOTO	RETURN_TO_PROGRAM

INCREMENT				INCF	CURRENT_ADDRESS
						INCF	FSR
						RETURN

DECREMENT				DECF	CURRENT_ADDRESS
						DECF	FSR
						RETURN

CURSOR_NEXTLINE			MOVLW	b'01100' ;move to ROW1 COLUMN 8
						CALL	ET
						MOVLW	b'01000'
						CALL	ET
						CALL	CURSOR_MOVELEFT
						MOVLW	b'10000'
						MOVWF	ADDRESSING_COUNTER
						MOVLW	d'38'
						MOVWF	CURRENT_ADDRESS
						MOVWF	FSR
						GOTO	RETURN_TO_PROGRAM

CURSOR_SWITCH_ROW		BTFSC	ADDRESSING_COUNTER,4
						GOTO	SWITCH_UP
						GOTO	SWITCH_DOWN

SWITCH_DOWN				MOVLW	b'01100' ; move to ROW1 COLUMN 9
						CALL	ET
						MOVLW	b'01001'
						CALL	ET
						CALL	CURSOR_MOVELEFT
						MOVLW	b'10000'
						MOVWF	ADDRESSING_COUNTER
						MOVLW	d'39'
						MOVWF	CURRENT_ADDRESS
						MOVWF	FSR		;ADJUST FSR
						GOTO	RETURN_TO_PROGRAM

SWITCH_UP				MOVLW	b'01000' ;move to ROW0 COLUMN 0
						CALL	ET
						MOVLW	b'00000'
						CALL 	ET
						CALL	CURSOR_MOVERIGHT
						MOVLW	b'00000'
						MOVWF	ADDRESSING_COUNTER
						MOVLW	d'20'
						MOVWF	CURRENT_ADDRESS
						MOVWF	FSR
						GOTO	RETURN_TO_PROGRAM

;MOVE_CURSOR_MAZEMODE
;-----------------------------------------------------------------------------	

;---------------PLACE OBSTACLES CODE----------------------------

PLACE_OBSTACLECHOOSE	BTFSS	SELECT,0	;SELECT = 100 --> OBSTACLE MODE / SELECT = 101 --> MAZE MODE
						GOTO	PLACE_OBSTACLEMODE
						GOTO	PLACE_MAZEMODE

PLACE_OBSTACLEMODE	CALL	PLACE_OBSTACLE	;PLACE OBSTACLE IN CURRENT POSITION
					;BUZZ SOUND
					INCF	OBSTACLE_COUNTER	;ADJUST NUMBER OF PLACED OBSTACLES									
					BTFSC	OBSTACLE_COUNTER,2 ;CHECK IF 5 OBSTACLES HAVE BEEN PLACED
					BTFSS	OBSTACLE_COUNTER,0
					GOTO	ADJUST_NEXT_POSITION ; ADJUST CURSOR TO KEEP PLACING OBSTACLES
					GOTO	SOLVE_MAZE	;IF WE HAVE PLACE 5 OBSTACLES WE SOLVE THE MAZE

ADJUST_NEXT_POSITION		BTFSS	ADDRESSING_COUNTER, 3	; IF IT IS SET, WE ARE ON ROW0 COLUMN9, OR ROW1 COLUMN0
							GOTO	INCREMENT_NEXT_POSITION	
							BTFSC	ADDRESSING_COUNTER, 4 	; IF IT IS SET, WE ARE ON ROW1 COLUMN0, ELSE WE ARE ON ROW0 COLUMN9
							GOTO	SOLVE_MAZE
							GOTO	CURSOR_NEXTLINE
							
INCREMENT_NEXT_POSITION		BTFSS	ADDRESSING_COUNTER, 4 ;IF BIT4 OF ADDRESSING COUNTER IS SET, WE ARE IN ROW1 AND NEED TO MOVE BACKWARDS
							CALL	INCREMENT		  
							BTFSC	ADDRESSING_COUNTER, 4
							CALL	DECREMENT
							INCF	ADDRESSING_COUNTER
							GOTO	RETURN_TO_PROGRAM


PLACE_MAZEMODE		BTFSS	INDF,1	;CHECK IF THE POSITION IS EMPTY
					GOTO	CANNOT_PLACE	;IF NOT EMPTY RETURN
					BTFSC	OBSTACLE_COUNTER,2 ;CHECK IF 5 OBSTACLES HAVE BEEN PLACED
					BTFSS	OBSTACLE_COUNTER,0
					GOTO	PLACE_OBS_MAZEMODE
					GOTO	CANNOT_PLACE	

PLACE_OBS_MAZEMODE	CALL	PLACE_OBSTACLE
					INCF	OBSTACLE_COUNTER	;INCREMENT OBSTACLE COUNTER
					;ADJUST REMAINING:
					; MOVE TO THE REMAINING NB OF OBSTACLES POSITION
					MOVLW	b'01100'
					CALL	ET
					MOVLW	b'01111'
					CALL	ET
					;WRITE NUMBER

					BTFSS	REMAINING_NUMBER,3
					GOTO	REMAINING_CASE1
					CALL	nb0
					CALL	RETURN_TO_PREV_POSITION
					GOTO	PLACE_MAZEMODE_RETURN

REMAINING_CASE1		BTFSS	REMAINING_NUMBER,2
					GOTO	REMAINING_CASE2
					CALL	nb1
					CALL	RETURN_TO_PREV_POSITION
					GOTO	PLACE_MAZEMODE_RETURN

REMAINING_CASE2		BTFSS	REMAINING_NUMBER,1
					GOTO	REMAINING_CASE3
					CALL	nb2
					CALL	RETURN_TO_PREV_POSITION
					GOTO	PLACE_MAZEMODE_RETURN

REMAINING_CASE3		BTFSS	REMAINING_NUMBER,0
					GOTO	REMAINING_CASE4
					CALL	nb3
					CALL	RETURN_TO_PREV_POSITION
					GOTO	PLACE_MAZEMODE_RETURN

REMAINING_CASE4		CALL	nb4
					CALL	RETURN_TO_PREV_POSITION
					GOTO	PLACE_MAZEMODE_RETURN

					;RETURN TO PREVIOUS POSITION
RETURN_TO_PREV_POSITION		BTFSS	ADDRESSING_COUNTER, 4 ;IF BIT4 = 1 --> WE ARE IN ROW 1, ELSE WE ARE IN ROW 0
							MOVLW	b'01000'	  
							BTFSC	ADDRESSING_COUNTER, 4
							MOVLW	b'01100'
							CALL	ET
							BTFSS	ADDRESSING_COUNTER, 4
							MOVLW	d'20'	;ROW 0 : D'20' to d'29'
							BTFSC	ADDRESSING_COUNTER, 4
							MOVLW	d'30'	;ROW 1 : d'30' to d'39'
							SUBWF	FSR,0	; ADDRESS REGISTER - VALUE IN W = OFFSET
							CALL	ET	
							RETURN					

PLACE_MAZEMODE_RETURN		RLF		REMAINING_NUMBER,1 ;ROTATE IT ONCE TO THE LEFT TO SELECT CORRECT CASE FOR REMAINING NUMBER
							GOTO	CHECK_IF_DONE

CANNOT_PLACE		;BUZZ SOUND
					GOTO	RETURN_TO_PROGRAM


PLACE_OBSTACLE		BSF		INDF, 0 ;SET THE ADDRESS AS CONTAINING AN OBSTACLE
					BCF		INDF, 1 ;SET THE ADDRESS AS NOT EMPTY
					CALL	rectangle
					RETURN


;---------------MAZE SOLVING ALGORITHM----------------------------

SOLVE_MAZE		CALL 	CLEARDISPLAY
				BCF		INTCON,RBIF
				GOTO	WELCOME

;-----------------------------------------------------------------------------
;------CHARACTER DECLARATIONS---------

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
empty		MOVLW	b'10101'
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
nb0			MOVLW	b'10011'
			CALL	ET	
			MOVLW	b'10000'
			CALL	ET
			RETURN
nb1			MOVLW	b'10011'
			CALL	ET	
			MOVLW	b'10001'
			CALL	ET
			RETURN
nb2			MOVLW	b'10011'
			CALL	ET	
			MOVLW	b'10010'
			CALL	ET
			RETURN
nb3			MOVLW	b'10011'
			CALL	ET	
			MOVLW	b'10011'
			CALL	ET
			RETURN
nb4			MOVLW	b'10011'
			CALL	ET	
			MOVLW	b'10100'
			CALL	ET
			RETURN
nb5			MOVLW	b'10011'
			CALL	ET	
			MOVLW	b'10101'
			CALL	ET
			RETURN
NEWLINE		MOVLW b'01100' ; 3 bits to jump address
			CALL ET
		
			MOVLW b'00000' ; 4 bits to jump address
			CALL ET
			RETURN

;------------------------------------------

;------DELAYS AND OTHER FIXED FUNCTIONS---------
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

DEBOUNCE_DELAY	MOVLW	d'249'
				MOVWF	DEBOUNCECOUNT
			
DEBOUNCELOOP	NOP		
				DECFSZ	DEBOUNCECOUNT
				GOTO	DEBOUNCELOOP
				RETURN

CLEARDISPLAY	MOVLW b'00000' ;initializing clear display
				CALL ET
				MOVLW b'00001' ;clear display
				CALL ET
				RETURN

SET_BLINKING	MOVLW	b'00000'	;initializing display
				CALL	ET			

				MOVLW	b'01111'	;setting display to ON
				CALL	ET
				RETURN

CURSOR_MOVELEFT		MOVLW	b'00000';set cursor move position to the left
					CALL	ET
					MOVLW	b'00100'
					CALL	ET
					RETURN

CURSOR_MOVERIGHT	MOVLW	b'00000';set cursor move position to the left
					CALL	ET
					MOVLW	b'00110'
					CALL	ET
					RETURN
;---------------------------

;-----------------INDIRECT ADDRESSING---------------------

INDA           	MOVLW 	d'20'
				MOVWF	FSR
				MOVLW	d'10'
				MOVWF	ADDRESSING_COUNTER
				GOTO	LOOP1M

INDA1           MOVLW d'30'
				MOVWF FSR
				MOVLW d'10'
				MOVWF ADDRESSING_COUNTER

LOOP1M          BTFSC INDF,0
				CALL rectangle
 				
				BTFSC INDF,1
				CALL empty

				BTFSC INDF,2
				CALL letterS

				BTFSC INDF,3
				CALL letterE

				INCF FSR
				DECFSZ ADDRESSING_COUNTER

				GOTO  LOOP1M
				RETURN


END