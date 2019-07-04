.include "m2560def.inc"

.def direct = r16				//0xFF = up, 0x00 = down
.def emg = r17					//emergency flag, 0xFF = emergency, 0x00 = normal
.def curr = r18					//requested level
.def leds = r19					//level lift is on/showing on the leds
.def temp = r20
.def debcnt = r21				//debounce counter
.def temp1 = r22				
.def count = r23
.def temp2 = r24

////////////////////////////////macros////////////////////////////
.macro clear			//clear 2 bytes in data memory at a specified location
	push YL
	push YH
	push temp

	ldi YL, low(@0)
	ldi YH, high(@0)
	clr temp
	st Y+, temp
	st Y, temp
	pop temp

	pop YH
	pop YL
.endmacro
.macro convert				//convert a number so that the values can be output on the LED bar
	push temp1				
	push temp2				//2 registers to help keep count and compare values
	clr temp1
	clr temp2				//ensure that both temp 1 and 2 are clear
	mov temp2, temp			//move the number we wish to convert to temp2
	clr temp				//temp contains a value to set the bottom 8 LEDs
	clr count				//count contians a value to set the top 2 LEDs
convloop:
	cp temp1, temp2			//compare the counter temp1 to the number we wish to convert
	breq endconv
	cpi temp1, 8
	brsh convH				//if the bottom 8 LEDs are filled in, start filling in the top 2 LEDs
	lsl temp			
	inc temp				//move all bits to the left and add a bit to the least significant bit
	inc temp1				//increment the counter
	rjmp convloop
convH: 
	lsl count				
	inc count
	inc temp1				//similar to before, but this time for the count register
	rjmp convloop
endconv:
	pop temp2
	pop temp1
.endmacro

.macro debounce				//increment a counter after the push button has been on enough times
inccount:
	lds @0, Bounce
	inc @0
	sts Bounce, @0
.endmacro

.macro strobeOn				//turn the strobe light on
	push temp
	ldi temp, 0b000010
	out PORTA, temp
	pop temp
.endmacro

.macro strobeOff			//turn the strobe light off
	push temp
	ldi temp, 0x00
	out PORTA, temp
	pop temp
.endmacro
///////////////////////lcd_macros//////////////

.equ LCD_RS = 7				//series of macros and equations to help set up the LCD display
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.macro lcd_set
	sbi PORTA, @0
.endmacro

.macro lcd_clr
	cbi PORTA, @0
.endmacro

.macro do_lcd_command
	ldi temp, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro

.macro do_lcd_data
	ldi temp, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro reshuffle				// moves all subsiquent values in data memory over by one to make room for new value
reshuffle_loop:
	st Y+, temp					//load the number in
	mov temp, temp1				//move the next number into r21
	cpi temp1, 0				//if the next number is 0, stop
	ld temp1, Y					//load the next number into r16
	brne reshuffle_loop
.endmacro
.macro motorSlow				//set the motor to spin at a slow speed to show the doors are openning
	push temp
	ldi temp, 0x6A
	sts OCR3BL, temp
	clr temp
	sts OCR3BH, temp
	pop temp
.endmacro

.macro motorFast				//set the motor to spin at a fast speed to show the doors are closing
	push temp
	ldi temp, 0xD5
	sts OCR3BL, temp
	clr temp
	sts OCR3BH, temp
	pop temp
.endmacro

.macro motorOff					//turn the motor off
	push temp
	ldi temp, 0
	sts OCR3BL, temp
	clr temp
	sts OCR3BH, temp
	pop temp
.endmacro

////////////////////////////initialise//////////////
.dseg
TempCounter:	//using for timing operations in the timer interrupt
	.byte 2
Sorted:			//contains the list of requests
	.byte 50
Door:
	.byte 1		//flag for the door;value 1 = open, wait and close; value 2 = open and close without waiting
EmgDoor:
	.byte 1		//flag for the door during an emergency, value 1 = passengers haven't been let out, value 0 = doors have opened and closed
				// on level 1
Bounce:
	.byte 1		//counter for debouncing buttons
.cseg
.org 0x0000	rjmp reset
.org INT0addr rjmp INT0button
.org INT1addr rjmp INT1button
.org OVF0addr rjmp Timer

reset:
	ldi temp, low(RAMEND)
	out SPL, temp
	ldi temp, high(RAMEND)
	out SPH, temp

	ldi temp, 0xF0           //initialise the keypad
	sts DDRL, temp
	ldi direct, 0xFF		//set the direction of the lift to up
	ldi debcnt, 0			//initialise the debounce counter to 0
	sts Bounce, debcnt
	ldi leds, 1				//start the lift at level 1
	ldi emg, 0				//initialise emg to 0 since there is no emergency
	lds curr, Sorted		//load to first value requested to curr
	ldi temp, 1				
	sts Door, temp			//set Door to mode 1: open, wait, close;
	sts EmgDoor, temp		//set EmgDoor to mode 1: passengers have not been released

	ser temp
	out DDRC, temp
	out DDRG, temp			//initialize ports C and G for the LED bar
	out DDRA, temp			//initalize the strobe LED
	out DDRF, temp
	clr temp
	out DDRD, temp
	out PORTF, temp
	out PORTA, temp

	ldi temp, (2 << ISC00)	//set to falling edge trigger
	sts EICRA, temp
	in temp, EIMSK			// enable INT0
	ori temp, (1<<INT0)
	out EIMSK, temp
	ldi temp, (2 << ISC00)	//set to falling edge trigger
	sts EICRA, temp
	in temp, EIMSK			// enable INT1
	ori temp, (1<<INT1)
	out EIMSK, temp

	//initialise the motor output
	ldi temp, 0b00100000
	out DDRE, temp  ;set Port E as output

	ldi temp, 0
	sts OCR3BL, temp
	sts OCR3BH, temp

	ldi temp, (1 << WGM30)|(1 << COM3B1)
	sts TCCR3A, temp
	ldi temp, (1 << cs30)|(1 << wgm32)
	sts TCCR3B, temp
	ldi temp, 0b10000
	out DDRE, temp

	ldi temp, 0
	sts OCR3BL, temp
	sts OCR3BH, temp

	rcall normaldisplay		//set the LCD display to show the current floor and the next stop
	rjmp main

/////////////////////INT0 intterupt/////////////
INT0button:				//close the door
	push temp
	in temp, SREG
	push temp
	push YH
	push YL

	debounce debcnt
	lds debcnt, Bounce
	cpi debcnt, 1		//debounce the button by incrementing a register everytime the interrupt is called
	brlo restoreINT0	//if there have been less than 10 increments, do nothing
	cp leds, curr		
	brne restoreINT0	//if not servicing a request, do nothing
	ldi debcnt, 0		//reset the debounce counter
	sts Bounce, debcnt
	ldi temp, 2			//set door to mode 2: open, close
	sts Door, temp

	lds r24, TempCounter
	lds r25, TempCounter+1
	cpi r24, low(4500)		
	ldi temp, high(4500)
	cpc r25, temp			//if less than 1 sec has passed, keep tempCounter so that the door finishes openning
	brlo restoreINT0		//if more than 1 sec has passed, we are in the wait or close phase
	ldi r24, low(4500)		//therefore, reset tempcounter to count from 1 sec onwards so that the door closes properly without waiting
	ldi r25, high(4500)
	sts TempCounter, r24
	sts TempCounter+1, r25

restoreINT0:
	pop YL
	pop YH
	pop temp
	out SREG, temp
	pop temp
	reti
/////////////////////INT1 intterupt/////////////
INT1button:					//open the door
	push temp
	in temp, SREG
	push temp
	push YH
	push YL

	debounce debcnt
	lds debcnt, Bounce
	cpi debcnt, 10			//same debounce logic as INT0button
	brlo restoreINT1
	cp leds, curr
	brne restoreINT1		//if we are not serving a request, do nothing
	ldi debcnt, 0			//restore debounce counter
	sts Bounce, debcnt
	clear TempCounter		//restarte the timer so that it begins open sequence again
	ldi temp, 1				//set door to mode 1: open, wait, close
	sts Door, Temp

restoreINT1:
	pop YL
	pop YH
	pop temp
	out SREG, temp
	pop temp
	reti

/////////////////////timer interrupt////////////
Timer:
	push temp
	in temp, SREG
	push temp
	push YH
	push YL
	push r25
	push r24

begin_Timer:	
	rcall scan				//check the keypad for inputs
	lds r24, TempCounter
	lds r25, TempCounter+1
	adiw r25:r24, 1			//increment the TempCounter value

	cpi emg, 0xFF			//check if emergency is on
	breq emergencyCont		//if its an emergency, ignore the normal operation of the lift
	strobeoff				//ensure the strobe light is off
	rjmp regularTimerCont	//if not an emergency, keep going with regular operation of the ligt
	
emergencyCont:
	cp leds, curr			
	brne go_level_1			//if we are not servicing a request, immediately go to level 1
	cpi r24, low(4500)		//if currently servicing a request, close the door for 1 sec
	ldi temp, high(4500)
	cpc r25, temp
	brsh go_level_1			//after a second has passed, go to level 1
	motorFast				//if not, spin the motor quickly to show a closing door
	rjmp flash				//flash the lights and LED bar
go_level_1:
	ldi direct, 0			//set the lift to go down
clear_queue:
	clear Sorted
	lds curr, Sorted
	cpi curr, 0				//set curr to 0 so that the lift does not stop for the current request
	brne clear_queue
keepFlashing:
	strobeOn				
	rcall sleep_100ms
	strobeOff
	rcall sleep_100ms		//flash the strobe light
	cpi leds, 1				
	brne movelift_jmp		//keep moving the lift until it is level 1
open_door_emg:
	ldi direct, 0xFF		//once at level 1, reset the lift to go up
	ldi temp, 1
	sts Door, temp			//ensure the Door mode is 1 (open, wait, close)
	rcall Emg_Door_Check	//check if the door has been opened yet
	rjmp EndIF
close_door_sequence:			//door sequence when the close door button is pressed
	keep_opening:
		cpi r24, low(4500)
		ldi temp, high(4500)
		cpc r25, temp
		brsh close_immediate	//after 1 second, begin closing the door
		motorSlow				//spin the motor slowly to show the door is opening
		rjmp flash				//flash the LED bar
	//close the door immediately after finishing the open sequence
	close_immediate:
		cpi r24, low(9000)		//wait another 1 seconds (2 seconds total)
		ldi temp, high(9000)
		cpc r25, temp
		brsh movelift_jmp		//after the doors have closed, move the lift
		motorFast				//spin the motor quickly to show the door is closing
		rjmp flash				//flash the LED bar
movelift_jmp:					//for bridge commands that are outside their range
	rjmp movelift
regularTimerCont:
	lds curr, Sorted		//get a request from data memory
	cpi curr, 0
	brne go_to_request		//if the queue is empty, reset the timer count and do nothing
	clear TempCounter
	rjmp EndIF
go_to_request:
	cp leds, curr
	breq open				//if the lift is at a requested level, open the doors
	cpi r24, low(9000)
	ldi temp, high(9000)	
	cpc r25, temp
	brsh movelift			//if not at a requested level, wait for 2 seconds before moving the lift to the next level
	rjmp NotSecond			//store the registers that keep time in tempCounter

open:							//door opening sequence
	lds temp, Door
	cpi temp, 2
	breq close_door_sequence	//if in mode 2, go to the special sequence
	cpi r24, low(4500)
	ldi temp, high(4500)		//operate for 1 second
	cpc r25, temp
	brsh wait					//after 1 second, begin waiting 
	motorSlow
	rjmp flash
wait:
	cpi r24, low(18000)		//wait another 3 seconds (4 seconds total)
	ldi temp, high(18000)
	cpc r25, temp
	brsh close				//after waiting, begin closing the door
	motorOff				//turn the motor off during the wait period
	rjmp flash				//flash the LED bar
close:
	cpi r24, low(22500)			//wait another 1 seconds (5 seconds total)
	ldi temp, high(22500)
	cpc r25, temp
	brsh move_lift				//after closing the doors, move the lift
	motorFast					//spin the motor quickly to show the doors are closing
	rjmp flash					//flash the LED bar
move_lift:
	rjmp movelift			//move the lift	
flash:						//flash the LED bar
	ldi temp, high(1000)
	AND temp, r25			//Compare the high part of tempcounter to high(1000) effectively create a mask for 1000
	cpi temp, high(1000)
	breq turnOff			//after a fraction of a second, turn the LED bar off
	rjmp turnOn				//otherwise, keep the LED bar on

movelift:				//move the lift to the next level
	motorOff			//ensure the motor is turned off
regular_movement:
	ldi temp, 1
	sts Door, temp		//reset door mode to 1
	ldi debcnt, 0		//reset debounce counter
	sts Bounce, debcnt
	rcall lift			//function that changes the values of Led (ie the current lift level)
	mov temp, leds
	convert				//convert leds so that the correct led bars can be turned on
	out PORTC, temp		//show lights on the bottom 8 bars
	out PORTG, count	//show lights on the top 2 bars
	clear TempCounter	//reset the time
	rjmp EndIF
turnOff:
	strobeOff			//turn off the strobe light
	ldi temp, 0
	out PORTC, temp
	out PORTG, temp		//turn the LED bar off
	rjmp NotSecond

turnOn:
	mov temp, leds	
	convert
	out PORTC, temp
	out PORTG, count	//show the current level the lift is on using the LED bar
NotSecond:
	sts TempCounter, r24	
	sts TempCounter+1, r25	//store the current 'time' in TempCounter

EndIF:					//restore conflict registers

	pop r24
	pop r25
	pop YL
	pop YH
	pop temp
	out SREG, temp
	pop temp
	reti
//////////////////Emg_Door_Check//////////////
Emg_Door_Check:				//opens the door once when the lift reaches level 1 during an emergency
	push temp

	lds temp, EmgDoor
	cpi temp, 1
	brne restore_emg_door	//if the door hasn't been opened yet, turn the motor on, otherwise do nothing
	motorSlow				//move the motor slowly to show the doors are opening
	rcall sleep_1s			//wait for 1 sec whilst flashing the strobe light
	motorOff				//turn the motor off to show the doors are left open
	rcall sleep_1s
	motorFast				//spin the motor quickly to show the doors are closing
	rcall sleep_1s
	motorOff				//turn the motor off at the end
	ldi temp, 0
	sts EmgDoor, temp		//set EmgDoor to 0 to show that the passengers have been let out
	
restore_emg_door:
	pop temp
	ret
/////////////////Function lift////////////////
Lift:					//change the values of leds to simulate the movement of the lift
	push temp
	push count
	push temp1
	lds curr, Sorted	//load the requested level to curr
	cp leds, curr
	brne checkdirect	//if the lift is not there, check the direction of the lift

	rcall clear_request	//if we are there, remove the current request and move the rest of the queue forward by one spot
	lds temp, Sorted	//load the next request into temp
	cpi temp, 0
	breq end			//if there are no requests, do not move the lift
	rjmp checkdirect

checkdirect:	
	lds temp, Sorted
	cp leds, temp
	brlo up				//if the request is on a higher level, move the lift up
	brsh down			//if the request is on a lower level, move the lift down		
up:
	ldi direct, 0xFF	//ensure the direction value is still 0xFF (up)
	inc leds			//increment leds (ie increase the level the lift is currently on)
	rjmp checkend		//check if we have reached the maximum or minimum level
down:
	ldi direct, 0		//ensure the direction value is still 0 (down)
	dec leds			//decrement leds
	rjmp checkend
checkend:
	cpi leds, 10
	breq changedirect	//if the lift is at the top, change the direction of the lift
	cpi leds, 1
	breq changedirect	//if the lift is at the bottom, change the direction of the lift
	rjmp end
	 
changedirect:
	com direct			//compliment the current direction of the lift
end:
	cpi emg, 0xFF
	breq restore_lift	//if its an emergency, finish
	rcall normaldisplay	//if not, display the current level and requests on the lcd screen
restore_lift:
	pop temp1
	pop count
	pop temp
	ret

///////////////////////////main/////////////////////////
main:
	push temp
	push count

	clear TempCounter
	mov temp, leds
	convert
	out PORTC, temp
	out PORTG, count			//show the initial lift level on the LED bar
	ldi temp, 0b00000000		
	out TCCR0A, temp
	ldi temp, 0b00000010
	out TCCR0B, temp
	ldi temp, 1<<TOIE0			//set the timer to interrupt every 128 ms 
	sts TIMSK0, temp
	pop count
	pop temp
	sei
loop:
	rjmp loop

/////////////////////////////////////sleep///////////
//various functions to allow us to delay a process by a specified length of time
.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret
sleep_20ms:
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	ret
sleep_100ms:
	rcall sleep_20ms
	rcall sleep_20ms
	rcall sleep_20ms
	rcall sleep_20ms
	rcall sleep_20ms
	ret
sleep_200ms:
	rcall sleep_100ms
	rcall sleep_100ms
	ret
sleep_1s:						//special case that is only used during an emergency when the doors are opened and closed
	strobeOn					//as the process delays between open, wait and close, the strobe light flashes regularly
	rcall sleep_200ms
	strobeOff
	rcall sleep_200ms
	strobeOn
	rcall sleep_200ms
	strobeOff
	rcall sleep_200ms
	strobeOn
	rcall sleep_200ms
	strobeOff
	ret
////////////////////////move_to_end_of_stack/////////////
Sorted_end:				//function that moves a pointer to the end of the request queue
	push temp
	ldi YL, low(Sorted)
	ldi YH, high(Sorted)

sorted_loop:
	ld temp, Y+			//load a value from the queue to temp
	cpi temp, 0
	brne sorted_loop	//if the value isn't 0, Y is not at the end yet, therefore go again
	sbiw r29:r28, 1		//if it is zero, move Y back by one since it was incremented after loading to temp
	pop temp
	ret

////////////////////////clear request////////////////////
clear_request:					//when a request is serviced, clear it and move to
	push temp					//the rest of the queue forward by one

	ldi YL, low(Sorted)			//initialise Y pointer for data memory
	ldi YH, high(Sorted)
start_clear:
	adiw r29:r28, 1				//move pointer forward by one
	ld temp, Y					//copy that value
	sbiw r29:r28, 1				//move the pointer back
	st Y+, temp					//store the copied value and move the pointer up by one afterwards
	cpi temp, 0					//if the value is not 0 (end of the string) go again
	brne start_clear

	pop temp
	ret
////////////////////////insert requests//////////////////
insert_request:							//insert a request into the string in data memory		
		push temp1
		push YL
		push YH

		ldi YL, low(Sorted)				//initialise Y pointer for data memory
		ldi YH, high(Sorted)
		cpi emg, 0xFF
		breq restore_insert_extended	//if its an emergency, do not add any requests

		ld curr, Y						//load the first request to curr
		cp leds, temp					//temp contains the new request
		breq restore_insert_extended	//if the new request is the same as the current level the lift is on, do nothing 

		cpi direct, 0					//check the direction of the lift
		brne sort_up
		breq sort_down

	sort_up:							//logic if lift moves up
		cp temp, leds					
		brlo append_prep_up				//if the request is lower than the current level, move the pointer to the end of the queue
	prepend_up:							//if it is higher, start from the front of the queue
		ld temp1, Y+
		cp temp1, temp					//compare the new request to the first value in the queue
		breq restore_insert_extended	//if they are equal, do nothing
		cp temp1, leds		
		brlo load_front_up				//if the next request is less than the current floor, add the new request to the queue
		cp temp, temp1			
		brsh prepend_up					//if the new request is higher than the next request, loop through again
		brlo load_front_up				//otherwise, add the new request to the queue
	append_prep_up:
		rcall Sorted_end				//function that moves the Y pointer to the end of the queue
	append_up:
		ld temp1, -Y					//load the queued value onto temp1
		cp temp1, temp					//compare queue to request
		breq restore_insert				//if they are the same, do nothing
		cp temp1, leds
		brsh load_back_up				// if we are at the end of the values smaller than the current level, store the new request
		cp temp, temp1					// compare request with queue
		brsh append_up					// if larger, loop again
		brlo load_back_up				// otherwise add the new request
	
	load_front_up:
		st -Y, temp						//move the pointer back by one and store the new request
		reshuffle						//call a macro that moves every other request over by one place
		rjmp restore_insert
	load_back_up:
		adiw r29:r28, 1					//move the pointer forward by one
		ld temp1, Y						//copy the value currently pointed at into temp1
		st Y, temp						//store the new request where the temp1 value used to be
		reshuffle						//reshuffle the rest of the queue
		rjmp restore_insert
	restore_insert_extended:
		rjmp restore_insert				//rjmp for the bridge commands that are out of range
	sort_down:							//logic if lift moves down
		cp temp, leds		
		brsh append_prep_down			//if the new request is the same or higher than the current level, go to the end of the queue
	prepend_down:
		ld temp1, Y+					//load the first request into temp1
		cp temp1, temp
		breq restore_insert				//if the new request is the same as the queued value, do nothing
		cp temp1, leds					
		brsh load_front_down			//if the queued value is greater than the current level (ie end of the smaller numbers) add the new request
		cp temp, temp1
		brlo prepend_down				//if the new request is less than the queued request, loop again
		brsh load_front_down			//if greater, add the new request to the queue
	append_prep_down:
		rcall Sorted_end				//move to the end of the queue
	append_down:
		ld temp1, -Y
		cp temp1, temp
		breq restore_insert				//if the new request is equal to the queued value, do nothing
		cp temp1, leds
		brlo load_back_down				//if the queued value is greater than the current lift level, store the new request
		cp temp, temp1	
		brlo append_down				//if the new request is less than the queued request, loop again
		brsh load_back_down				//if greater, store the new request
	
	load_front_down:
		st -Y, temp						//store the new request
		reshuffle						//move the rest of the queue over by one
		rjmp restore_insert
	load_back_down:
		adiw r29:r28, 1					
		ld temp1, Y						//copy the next queued value
		st Y, temp						//store the new request in its place
		reshuffle						//move the queue over by one
restore_insert:	
		pop YH
		pop YL							//restore conflict registers
		pop temp1
		ret
////////////////////////scan_functions///////////////////
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F			//equations to help with determining the columns and rows
scan:						//checks what buttons have been pressed on the keypad
	push temp
	push temp1
	push r25				//column mask
	push r23				//row mask
	push r24				//row
	push r21				//column

	lds curr, Sorted		//load the first request into curr
start_scan:
	ldi r25, INITCOLMASK	//initial column mask and scan from column 4
	ldi r21, 0				//start col at column 0

colloop:
	cpi r21, 4				    
	brne keep_scanning      //if all of the columns have not been scanned, continue scanning
	rjmp End_scan			//otherwise, stop scanning
keep_scanning:
	sts PORTL, r25			//store the col mask into PORT L
	
	ldi temp, 0xFF			//delay by using a counter and loop
delaylcd:
	dec temp
	brne delaylcd

	lds temp, PINL
	andi temp, ROWMASK		//Read only the row bits
	cpi temp, 0xF			//Check if any rows are grounded
	breq nextcol			//If not then go to the next column

	ldi r23, INITROWMASK	//scan from top row
	ldi r24, 0				//clear the row

rowloop:
	cpi r24, 4
	breq nextcol			//all rows have been scanned if row is 4. If true, move to the next column
	mov temp1, temp
	and temp1, r23			//compare the row with row mask
	breq convert_input		//if a button was pressed, change that button to a number
	inc r24					//increase the row value
	lsl r23					//logical left shift the row mask
	rjmp rowloop			//loop through to check the next row

nextcol:					//all rows have been scanned, need to shift the column left by 1
	lsl r25					//logical left shift the row mask
	inc r21					//increase the column number
	rjmp colloop			//loop through to check the next column

convert_input:
	cpi r21, 3				//if column is 3 then we have letters so do nothing
	breq letters			//go back to main and repeat process

	cpi r24, 3				//if key pressed is in row 3, then it's either 0 or a symbol
	breq symbols
	mov temp, r24
	lsl temp
	add temp, r24
	add temp, r21
	inc temp				//otherwise, apply maths to the column and row number to obtain the button value
	cpi curr, 0
	brne insert_keypad		//if the request list is not empty, insert the new request
	sts Sorted, temp		//if it is, directly access data memory and store the request.
	rjmp End_scan
insert_keypad:
	rcall insert_request	//sort keypad value into the request queue in data memory 
	rjmp End_scan
letters:
	ldi temp, 0
	rjmp End_scan			//if a letter was pressed, do nothing
symbols:					
	cpi r21, 0				//if it's row 3 and column 0 then it's *
	breq star
	cpi r21, 1				// if row 3, col 1, it's 0
	breq zero
	rjmp End_scan			//otherwise it was '#', so do nothing
zero:
	ldi temp, 10			//if '0' was pressed, add 10 to the queue
add_10:
	rcall insert_request
	rjmp End_scan
star:
	clear TempCounter		//clear timer counter so that emergency process can begin from time 0 regardless of what it was before
	ldi temp, 1
	sts EmgDoor, temp		//set EmgDoor to mode 1 as default
	com emg					//compliment the value of emg so that pushing the same button changes the value to either on or off
	cpi emg, 0xFF
	brne resetdisplay		//if it is not an emergency, set the display back to showing the current level and the next stop
	rcall outputDisplayEm	//otherwise show the emergency on the LCD screen
	rjmp End_scan
resetdisplay:
	rcall normaldisplay		//call the function for the default LDC display
End_scan:
	pop r21
	pop r24
	pop r23
	pop r25
	pop temp1
	pop temp
	ret
//////////////////////////////////display//////////////////
outputDisplayEm:						//shows text on the LCD screen for an emergency

	do_lcd_command 0b00111000			// 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000			// 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000			// 2x5x7
	do_lcd_command 0b00111000			// 2x5x7
	do_lcd_command 0b00001000			// display off
	do_lcd_command 0b00000001			// clear display
	do_lcd_command 0b00000110			// increment, no display shift
	do_lcd_command 0b00001110			// Cursor on, bar, no blink

	do_lcd_data 'E'
	do_lcd_data 'm'
	do_lcd_data 'e'
	do_lcd_data 'r'
	do_lcd_data 'g'
	do_lcd_data 'e'
	do_lcd_data 'n'
	do_lcd_data 'c'
	do_lcd_data 'y'
	//show 'Emergency" on the first row of the LCD screen
	do_lcd_command 0b11000000 //move to 2nd line
	
	do_lcd_data 'C'
	do_lcd_data 'a'
	do_lcd_data 'l'
	do_lcd_data 'l'
	do_lcd_data ' '
	do_lcd_data '0'
	do_lcd_data '0'
	do_lcd_data '0'	
	//show 'Call 000' on the second row
	ret
///////////////////////////////////////////////////////////
normaldisplay:
	do_lcd_command 0b00111000 
	rcall sleep_5ms
	do_lcd_command 0b00111000 
	rcall sleep_1ms
	do_lcd_command 0b00111000 
	do_lcd_command 0b00111000 
	do_lcd_command 0b00001000 
	do_lcd_command 0b00000001 
	do_lcd_command 0b00000110 
	do_lcd_command 0b00001110		//same as before
	
	do_lcd_data 'C'
	do_lcd_data 'u'
	do_lcd_data 'r'
	do_lcd_data 'r'
	do_lcd_data 'e'
	do_lcd_data 'n'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'f'
	do_lcd_data 'l'
	do_lcd_data 'o'
	do_lcd_data 'o'
	do_lcd_data 'r'
	do_lcd_data ' '		//display 'Current floor ' on the first row
	mov temp, leds
	subi temp, -'0'		//change the current floor level to ASCII value
	cpi temp, ':'		//check if the result equals :, ie the original value was 10 
	breq show_10		//if true, show '10' on the first row
	rcall lcd_data		//otherwise show the value stored in temp on the first row
	rcall lcd_wait
	rjmp Next_stop
show_10:
	rcall display_10	//function that displays '10' on the current row
Next_stop:
	do_lcd_command 0b11000000	//move to 2nd line
	do_lcd_data 'N'
	do_lcd_data 'e'
	do_lcd_data 'x'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 's'
	do_lcd_data 't'
	do_lcd_data 'o'
	do_lcd_data 'p'	
	do_lcd_data ' '				//show 'Next stop ' on the second row of the LCD screen
	lds curr, Sorted
	mov temp, curr
	subi temp, -'0'				//change the first queued request to ASCII
	cpi temp, ':'				//check if the result equals :,ie. curr = 10
	breq show_10_again			//if true, show 10 on the second row of the LCD screen
	rcall lcd_data				//otherwise show the value stored in temp
	rcall lcd_wait
	rjmp end_normal_lcd
show_10_again:
	rcall display_10		//function that shows '10' on the LCD screen
end_normal_lcd:
	push YL					//show the rest of the queue
	push YH					//(for testing)
	ldi YL, low(Sorted)		//
	ldi YH, high(Sorted)	//
	adiw r29:r28, 1			//
	test_start:				//
	ld temp, Y+				//
	cpi temp, 0				//
	breq test_stop			//
	subi temp, -'0'			//
	rcall lcd_data			//
	rcall lcd_wait			//
	rjmp test_start			//
	test_stop:				//
	pop YH					//
	pop YL					//
	ret

display_10:					//shows ten on the current row of the lcd screen
	do_lcd_data '1'
	do_lcd_data '0'
	ret

lcd_command:
	out PORTF, temp
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	ret

lcd_data:
	out PORTF, temp
	lcd_set LCD_RS
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	lcd_clr LCD_RS
	ret

lcd_wait:
	push temp
	clr temp
	out DDRF, temp
	out PORTF, temp
	lcd_set LCD_RW
lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	in temp, PINF
	lcd_clr LCD_E
	sbrc temp, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser temp
	out DDRF, temp
	pop temp
	ret