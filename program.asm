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

;;CONFLICT??
CURRENT_ADDRESS		EQU	d'18'	;KARL
OBSTACLE_COUNTER	EQU	d'19'	;KARL
CURRENT_POSITION	EQU	d'40'	;RAMI
ITERATIONS_COUNTER  EQU d'41'	;RAMI
CURSOR_STARTING_ADDRESS EQU d'42'	;RAMI
CURSOR_POSITION		EQU	d'42'	;RAMI
PREVIOUS_COMMAND	EQU	d'43'	;

MAZE_STATUS		EQU	d'43'	; to check if maze solved, no path, or not solved
;      0th bit: solved, not solved
; 	   1st bit: path, no path

LINE	EQU		d'44'

TEMP	EQU		d'59';RAMI

			ORG		0x0
			GOTO	START

			ORG 	0x04

			;BTFSC	INTCON, T0IF	;timer interrupt flag test
			;GOTO	timerInterrupt	

			BTFSC	INTCON, RBIF	;interrupt for button presses
			GOTO	BUTTONPRESS
			
START		CLRF	PORTA

			;clearing flags for menu star
			CLRF	SELECT
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

			CALL	CLEAR_BLINKING

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

			;POWERING LEDS
			BCF	PORTB, 2
			BCF	PORTB, 3

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

			
			;BSF		INTCON,T0IE
			BSF		INTCON,RBIE
			BSF		INTCON,GIE


MENULOOP		GOTO	MENULOOP

;--------------DATABASE FORMAT----------------
;	OBSTACLE ---> 0
; 	EMPTY	 ---> 1
;	START S  ---> 2
; 	END	  E  ---> 3
;---------------------------------------------

DEFAULT_START	CALL	CLEARDISPLAY
				CALL	SET_BLINKING
				MOVLW	b'110'	;setting base state
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

				;BSF PORTB,3	;turn on RED LED	
				;BCF PORTB,2 ;ground GREEN LED

				MOVLW	d'10'
				MOVWF	CURRENT_POSITION
				MOVLW	d'11'
				SUBWF	CURRENT_POSITION,F
				
				BTFSC	STATUS, Z
				;EQUAL
				BSF		PORTB,2
				BTFSS	STATUS, Z
				;NOT EQUAL
				BSF		PORTB,3
				
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


				;UPDATE CURRENT POSITION
				MOVLW	d'25'
				MOVWF	CURRENT_POSITION

				;STOP BLINKING CURSOR
				CALL	CLEAR_BLINKING
				
				;SOLVE MAZE
				GOTO	SOLVE_MAZE

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


;RETURNING TO MAIN MENU
GOTO	MENU	

MAZE_START		CALL 	CLEARDISPLAY
				CALL	SET_BLINKING
				MOVLW	b'101'
				MOVWF	SELECT

				GOTO	MENULOOP

;RETURNING TO MAIN MENU
GOTO	MENU





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

START_BUTTON;	BTFSS	SELECT, 2
			;	GOTO	RETURN_TO_PROGRAM
			;	BTFSS	SELECT, 0
			;	GOTO	RETURN_TO_PROGRAM
				
			;	BTFSS	INDF,1	;CHECK IF THE POSITION IS EMPTY
			;	GOTO	CANNOT_PLACE	;IF NOT EMPTY RETURN

;				BTFSC	OBSTACLE_COUNTER,4	;BIT 4 IS SET WHEN START POSITION HAS ALREADY BEEN PLACED
;				GOTO	CANNOT_PLACE
;
;				CALL	letterS ;Print S
;				CALL	RETURN_TO_PREV_POSITION
;				BCF 	PORTB,3 ;GROUND RED LED
;				BSF		OBSTACLE_COUNTER,4
;				BSF		INDF, 2  ;SET THE ADDRESS AS CONTAINING THE STARTING POSITION
;				BCF		INDF, 1 ;SET THE ADDRESS AS NOT EMPTY
;				
;				GOTO	CHECK_IF_DONE

END_BUTTON		;BTFSS	SELECT, 2
;				GOTO	RETURN_TO_PROGRAM
;				BTFSS	SELECT, 0
;				GOTO	RETURN_TO_PROGRAM
;
;				BTFSS	INDF,1 ;CHECK IF THE POSITION IS EMPTY
;				GOTO	CANNOT_PLACE ;IF NOT EMPTY RETURN
;
;				BTFSC	OBSTACLE_COUNTER,5 ;BIT 5 IS SET WHEN END POSITION HAS ALREADY BEEN PLACED
;				GOTO	CANNOT_PLACE
;
;				CALL	letterE
;				CALL	RETURN_TO_PREV_POSITION
;				BCF 	PORTB,2	;GROUND GREEN LED
;				BSF		OBSTACLE_COUNTER,5
;				BSF		INDF, 3 ; SET THE ADDRESS AS CONTAINING THE END POSITION
;				BCF		INDF, 1 ; SET THE ADDRESS AS NOT EMPTY
;				
;				GOTO	CHECK_IF_DONE
	
RETURN_TO_PROGRAM		BCF		INTCON, RBIF	
						RETFIE

;---------------TIMER INTERRUPT CODE--------------------------------
timerInterrupt	CALL	POWERUPDELAY
				DECFSZ	TIMER3,F ;decrementing timer3 to get 3s delay
				GOTO	RETTIMER3

				;INCREMENTING START POSITION
				MOVLW	d'49'
				MOVWF	TIMER3
				CLRF	TMR0
				;GOTO	INCREMENT_POINTER		
				
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
					GOTO	MOVE_CURSOR_MAZEMODE

MOVE_CURSOR_OBSTACLE	BTFSS	ADDRESSING_COUNTER, 3
						GOTO	CURSOR_INCREMENT
						BTFSC	ADDRESSING_COUNTER, 4
						GOTO 	SOLVE_MAZE
						GOTO	CURSOR_NEXTLINE

CURSOR_INCREMENT		CALL	empty
						BTFSS	ADDRESSING_COUNTER, 4 ;IF BIT4 OF ADDRESSING COUNTER IS SET, WE ARE IN THE SECOND LINE AND MOVING BACKWARDS
						INCF	CURRENT_ADDRESS		  
						BTFSC	ADDRESSING_COUNTER, 4
						DECF	CURRENT_ADDRESS
						INCF	ADDRESSING_COUNTER
						GOTO	RETURN_TO_PROGRAM

CURSOR_NEXTLINE			MOVLW	b'01100' ;move to required position
						CALL	ET
						MOVLW	b'01000'
						CALL	ET
						CALL	CURSOR_MOVELEFT
						MOVLW	b'10000'
						MOVWF	ADDRESSING_COUNTER
						MOVLW	d'38'
						MOVWF	CURRENT_ADDRESS
						GOTO	RETURN_TO_PROGRAM

MOVE_CURSOR_MAZEMODE
;-----------------------------------------------------------------------------	

;---------------PLACE OBSTACLES CODE----------------------------

PLACE_OBSTACLECHOOSE	BTFSS	SELECT,0	;SELECT = 100 --> OBSTACLE MODE / SELECT = 101 --> MAZE MODE
						GOTO	PLACE_OBSTACLEMODE
						GOTO	PLACE_MAZEMODE

PLACE_OBSTACLEMODE	CALL	PLACE_OBSTACLE	;PLACE OBSTACLE IN CURRENT POSITION
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
							
INCREMENT_NEXT_POSITION		BTFSS	ADDRESSING_COUNTER, 4 ;IF BIT4 OF ADDRESSING COUN TER IS SET, WE ARE IN ROW1 AND NEED TO MOVE BACKWARDS
							INCF	CURRENT_ADDRESS		  
							BTFSC	ADDRESSING_COUNTER, 4
							DECF	CURRENT_ADDRESS
							INCF	ADDRESSING_COUNTER
							GOTO	RETURN_TO_PROGRAM


PLACE_MAZEMODE

PLACE_OBSTACLE		BSF		CURRENT_ADDRESS, 0 ;SET THE ADDRESS AS CONTAINING AN OBSTACLE
					MOVWF	CURRENT_ADDRESS
					MOVLW	FSR
					CALL	rectangle
					RETURN


;---------------MAZE SOLVING ALGORITHM----------------------------

SOLVE_MAZE			CALL	MOVE_CURSOR_TO_CURRENT_POSITION
					CALL	LONGDELAY
					

CHECK_RIGHT			MOVLW	b'0001'
					MOVWF	PREVIOUS_COMMAND

					;-------------CHECKING BOUNDS----------------
					;TOP RIGHT
					MOVLW	d'29'
					SUBWF	CURRENT_POSITION, 0

					;IF EQUAL GOTO CHECK LEFT
					BTFSC	STATUS, Z
					GOTO	CHECK_LEFT

					;BOT RIGHT
					MOVLW	d'39'
					SUBWF	CURRENT_POSITION, 0
					
					;IF EQUAL GOTO CHECK LEFT
					BTFSC	STATUS, Z
					GOTO	CHECK_LEFT					
					;--------------------------------------------
					
					CALL	CURSOR_MOVERIGHT;SETTING CURSOR MOVING DIRECTION

					;---------CHECKING POSITION TO RIGHT OF CURRENT POSITION--------
					MOVLW	b'1'
					ADDWF	CURRENT_POSITION,0	;SAVING CURR_POS + 1 IN W
					MOVWF	FSR
					
					;CHECK IF THIS REG CONTENT IS END
					BTFSC	INDF, 3
					GOTO	SOLVED

					;CHECKING IF CURRENT POSITION IS VISITED
					BTFSC	INDF, 4
					GOTO	CHECK_LEFT

					;CHECK IF THIS REG CONTENT IS EMPTY
					BTFSC	INDF, 1
					GOTO	EMPTY_CASE		
					;----------------------------------------------------------------
					
CHECK_LEFT			MOVLW	b'0010'
					MOVWF	PREVIOUS_COMMAND

					;-------------CHECKING BOUNDS----------------
					;TOP LEFT
					MOVLW	d'20'
					SUBWF	CURRENT_POSITION, 0

					;IF EQUAL GOTO CHECK UP
					BTFSC	STATUS, Z
					GOTO	CHECK_UP

					;BOT LEFT
					MOVLW	d'30'
					SUBWF	CURRENT_POSITION, 0
					
					;IF EQUAL GOTO CHECK UP
					BTFSC	STATUS, Z
					GOTO	CHECK_UP					
					;--------------------------------------------

					CALL	CURSOR_MOVELEFT	;SETTING CURSOR MOVING DIRECTION

					;---------CHECKING POSITION TO LEFT OF CURRENT POSITION--------
					MOVLW	b'1'
					SUBWF	CURRENT_POSITION,0	;SAVING CURR_POS - 1 IN W
					MOVWF	FSR
					
					;CHECK IF THIS REG CONTENT IS END
					BTFSC	INDF, 3
					GOTO	SOLVED

					;CHECKING IF CURRENT POSITION IS VISITED
					BTFSC	INDF, 4
					GOTO	CHECK_UP

					;CHECK IF THIS REG CONTENT IS EMPTY
					BTFSC	INDF, 1
					GOTO	EMPTY_CASE		
					;----------------------------------------------------------------

					;Incrementing cursor position
					CALL	INCREMENT_CURSOR

					;Printing star?
					CALL    charStar
					CALL    LONGDELAY

					;Decrementing current position
					MOVLW	b'1'
					SUBWF	CURRENT_POSITION,1

					;LABELLING CURRENT POSITION AS VISITED
					MOVF	CURRENT_POSITION,0	;MOVING CURRENT_POS CONTENT TO W
					MOVWF	FSR
					BSF		INDF, 4	;SETTING CUR_POS REGISTER AS VISITED

					;INCREMENTING ITERATIONS COUNT
					MOVWF	b'1'
					ADDWF	ITERATIONS_COUNTER, 1
					
	;check if solved
	BTFSS	MAZE_STATUS,0
	GOTO	SOLVE_MAZE

CHECK_UP			MOVLW	b'0100'
					MOVWF	PREVIOUS_COMMAND

					;-------------CHECKING BOUNDS----------------
					;IF FIRST ROW, GOTO CHECK_DOWN
					MOVLW	d'30'
					SUBWF	CURRENT_POSITION, 0
					BTFSC	STATUS, Z
					GOTO	MMM
					BTFSC	STATUS, C
					GOTO	MMM
					GOTO	CHECK_DOWN


MMM					;IF SECOND ROW, CHECK UP
					



					MOVLW	d'20'
					SUBWF	CURRENT_POSITION, 0

					;IF EQUAL GOTO CHECK UP
					BTFSC	STATUS, Z
					GOTO	CHECK_UP

					;BOT LEFT
					MOVLW	d'30'
					SUBWF	CURRENT_POSITION, 0
					
					;IF EQUAL GOTO CHECK UP
					BTFSC	STATUS, Z
					GOTO	CHECK_UP					
					;--------------------------------------------

					;Incrementing cursor position
					CALL	INCREMENT_CURSOR

					;Printing star?
					CALL    charStar
					CALL    LONGDELAY

					;Decrementing current position
					MOVLW	b'1'
					SUBWF	CURRENT_POSITION,1

					;CHECKING IF CURRENT POSITION IS VISITED
					MOVF CURRENT_POSITION, 0
					MOVWF	FSR	
					BTFSC	INDF, 4
					GOTO	CHECK_UP

					;LABELLING CURRENT POSITION AS VISITED
					MOVF	CURRENT_POSITION,0	;MOVING CURRENT_POS CONTENT TO W
					MOVWF	FSR
					BSF		INDF, 4	;SETTING CUR_POS REGISTER AS VISITED

					;INCREMENTING ITERATIONS COUNT
					MOVWF	b'1'
					ADDWF	ITERATIONS_COUNTER, 1
					
	;check if solved
;	BTFSS	MAZE_STATUS,0
;	GOTO	SOLVE_MAZE

CHECK_DOWN			MOVLW	b'1000'
					MOVWF	PREVIOUS_COMMAND

;GOTO	EXIT

SOLVED				;GOTO	EXIT

EMPTY_CASE			;Incrementing cursor position
					CALL	INCREMENT_CURSOR


					;INCREMENT OR DECREMENT CURR POS
					BTFSC	PREVIOUS_COMMAND, 0
					CALL	INCREMENT_POSITION_RIGHT
					BTFSC	PREVIOUS_COMMAND, 1
					CALL	INCREMENT_POSITION_LEFT
					BTFSC	PREVIOUS_COMMAND, 2
					CALL	INCREMENT_POSITION_UP
					BTFSC	PREVIOUS_COMMAND, 3	
					CALL	INCREMENT_POSITION_DOWN

					;Printing star?
					CALL    charStar
					CALL    LONGDELAY

					;LABELLING CURRENT POSITION AS VISITED
					MOVF	CURRENT_POSITION,0	;MOVING CURRENT_POS CONTENT TO W
					MOVWF	FSR
					BSF		INDF, 4	;SETTING CUR_POS REGISTER AS VISITED

					;INCREMENTING ITERATIONS COUNT
					MOVWF	b'1'
					ADDWF	ITERATIONS_COUNTER, 1
					
;check if solved
BTFSS	MAZE_STATUS,0
GOTO	SOLVE_MAZE

EXIT		CALL	CLEARDISPLAY
			MOVLW	b'110'	;setting base state
			MOVWF	SELECT		
			GOTO	MENU
		
;THIS FUNCTION PRINTS THE OBJECT IN THE CURRENT POSITION AGAIN TO INCREMENT THE CURSOR 
INCREMENT_CURSOR		MOVF	CURRENT_POSITION,0	;MOVING CURRENT_POS CONTENT TO W
						MOVWF	FSR
						BTFSS	INDF, 4
						CALL    CHECK_INDF_PRINT  ;CURSOR INCREMENTED BY PRINTING CUR_POS IN CUR_POS
						BTFSC	INDF, 4
						CALL	charStar

						RETURN

INCREMENT_POSITION_RIGHT		MOVLW	d'1'
								ADDWF	CURRENT_POSITION,1
								RETURN
INCREMENT_POSITION_LEFT			MOVLW	d'1'
								SUBWF	CURRENT_POSITION,1
								RETURN
INCREMENT_POSITION_UP			MOVLW	d'10'
								SUBWF	CURRENT_POSITION,1
								RETURN
INCREMENT_POSITION_DOWN			MOVLW	d'10'
								ADDWF	CURRENT_POSITION,1
								RETURN

MOVE_CURSOR_TO_CURRENT_POSITION		MOVF	CURRENT_POSITION, 0
									SUBLW	d'30'

									;IF Z=0, C=0 : NEGATIVE RESULT, GO TO SECOND LINE MOVE
									BTFSC	STATUS, Z	;IF 1, THEN WE'RE SECOND LINE
									GOTO	MOVE_SECONDLINE	
								
									BTFSS	STATUS, C	;IF ZERO THEN SECOND LINE
									GOTO	MOVE_SECONDLINE

									;IF Z=0, C = 1 : POSITIVE RESULT, GOTO TO FIRST LINE MOVE
									BTFSS	STATUS, Z
									GOTO	MOVE_FIRSTLINE

									 
MOVE_FIRSTLINE		MOVLW	d'20'	;WE ARE SUBTRACTING 20 FROM CURRENT POSITION
					SUBWF	CURRENT_POSITION,0
					
					MOVWF	TEMP
					MOVLW b'01000' ; 3 bits to jump address
					CALL ET
		
					MOVF	TEMP,0	;MOVE CURSOR TO CURRENT POSITION
					CALL 	ET
					RETURN
									
									
MOVE_SECONDLINE		MOVF	CURRENT_POSITION, 0
					ADDLW	d'10' ; THIS STORES CURSOR ADDRESS IN W
					MOVWF	TEMP
					MOVLW b'01100' ; 3 bits to jump address
					CALL ET
		
					MOVF	TEMP,0	;MOVE CURSOR TO CURRENT POSITION
					CALL 	ET
					RETURN
		

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

LONGDELAY		MOVLW	d'20'
				MOVWF	TEMP
LOOPLONG 		CALL    POWERUPDELAY
                DECFSZ  TEMP,1
                GOTO    LOOPLONG
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
CLEAR_BLINKING	MOVLW	b'00000'
				CALL	ET
				MOVLW	b'01100'
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

LOOP1M          CALL  CHECK_INDF_PRINT

				INCF FSR
				DECFSZ ADDRESSING_COUNTER

				GOTO  LOOP1M
				RETURN


CHECK_INDF_PRINT	BTFSC INDF,0
					CALL rectangle
 				
					BTFSC INDF,1
					CALL empty

					BTFSC INDF,2
					CALL letterS

					BTFSC INDF,3
					CALL letterE

					RETURN

END