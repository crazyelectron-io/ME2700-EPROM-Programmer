	title "ME2700 Programmer Firmware Rev 1.03"
	subtitle "Copyright (C) 2016 Martin Eberhard"
	list b=4, c=132, n=80
;********************************************************************************
;* ME2700 Programmer Firmware													*
;*																				*
;*Revision History																*
;*	1.00  3 November 2015   M. Eberhard											*
;*      Created																	*
;*  1.01  20 January 2016  M. Eberhard											*
;*      First released version													* 
;*  1.02  2 February 2016  M. Eberhard											*
;*      Add ASCII to BD printout, slight help menu improvements. Default to the	*
;*		EPROM size for UI & US, if no count parameter provided.					*
;*  1.03  4 October 2016  M. Eberhard											*
;*      Reset stack pointer at main to prevent overflows on several ^Cs			*
;********************************************************************************
; Major revision must change if the firmware is not fully compatable with		*
; previous revisions.															*
;********************************************************************************
REV_MAJOR			equ	0x01	;major revision number. Max = 9
REV_MINOR			equ	0x03	;minor revision number. Each nibble: max = 9

;TO DO (Maybe)
;deal with 4-bit (and maybe 1-bit) EPROMs

;********************************************************************************
; Specify the required Kernel Loader revision level. An error message will be	*
; printed if this does not match the kernel in FLASH.							*
;********************************************************************************
REQUIRED_KERN_REV	equ	0x10	;only the most significant nibble counts

;********************************************************************************
;*								GENERAL NOTES									*
;* This program assumes the PIC is clocked at 16 MHz, meaning a 4 MHz CPU       *
;* operating speed, or 0.25 uS instruction cycle time. The internal oscilator	*
;* has an accuracy of +/- 2% (page 369), which must be considered in all timing.*
;*																				*
;* Page references in the comments refer to the appropriate pages in Microchip	*
;* document DS40001303H "PIC18F2XK20/4XK20 28/40/44-Pin Flash Microcontrollers	*
;* with XLP Technology"															*
;*																				*
;* The "Kernel" is actually just the firmware loader, together with a			*
;* collection of primitive subroutines. This code is protected by hardware		*
;* against being overwritten by a firware load. The minimum protectable size	*
;* for this particular PIC is 2K, which was far more than needed for the		*
;* firmware loader itself. For this reason, most of the basic I/O routines and	*
;* routines for acessing EEPROM and the external SRAM were put in the Kernel.	*
;* See the file KernelMemory.inc for kernel subroutines.						*
;*																				*
;* Note that FSR2 is owned by the Kernel interrupt service routines, and should	*
;* not be used here unless interrupts are masked.								*
;*																				*
;* See Help messages for commands and their operation.							*
;********************************************************************************
	page
;********************************************************************************
;* Include standard header and programmer header, and macros					*
;********************************************************************************
TRUE	equ	0xff
FALSE	equ	0
DEBUG	equ	FALSE

	#include	P18F45K20.INC
	#include	ASCII.inc
	#include	18FKernelMemory.inc
	#include	EPROMStructure.inc
	#include	IOPorts.inc
	#include	ME2700.inc

	org	LOADED_CODE

;disable all interrupts while we set things up

			clrf	INTCON					;disable interrupts while we work

;################################################################################
;# Initialization Routine														#
;# Mainly what the Kernel didn't initialize										#
;################################################################################

;Clear 8K-byte EPROM buffer in the external serial RAM (all of it)
init:		setf	ADDRESSL,0					;address counter low byte
			movlw	high(SRAM_SIZE-1)
			movwf	ADDRESSH,0					;address counter high byte

clr_sram:		movlw	0
				rcall	K_WRSRAM				;write W to SRAM at ADDRESSH:ADDRESSL

				movlw	0x01
				subwf	ADDRESSL,F,0
				bc		clr_sram
				subwf	ADDRESSH,F,0
				bc		clr_sram

;Initialize variables that need it
			clrf	ADR_OFFSET					;file address offset
			clrf	BUF_OFFSET					;buffer address offset
			clrf	FLAGS1

;Set up timer 0 as a 15.625 KHz timer, for use with the pacifier
			bcf		INTCON,TMR0IE				;make sure timer 0 interrupt is off
			movlw	(1<<TMR0ON)+0x07			;enable timer, prescaler=256
			movwf	T0CON,0						;(page 145)

;Set up the ADC for over-current Vpp shutdown
			movlw	0x05						;Fosc/16 for a 1 uS Tad, and an 11 uS acquisition time
			movwf	ADCON2,0					;the correct value when Fosc=16 MHz (page 251)

;Re-initialize the receive queue in case of trash from the loader
			movlw	low(RX_QUEUE)
			movwf	RQ_IPTR,0
			movwf	RQ_OPTR,0

;Re-initialize the transmit queue in case of trash from the loader
			movlw	low(TX_QUEUE)
			movwf	TQ_IPTR,0
			movwf	TQ_OPTR,0

;Initialize interrupt flags: queues are empty
			movlw	(1 << TQ_EMPTY)
			movwf	INT_FLAGS,0

			movf	RCREG,W,0					;chuck any junk in the UART receiver
			movf	RCREG,W,0					;it's a double-buffer

;Enable interrupts - we're ready to go
			movlw	(1 << PEIE) | (1 << GIE)	;peripheral ints & global enable only
			movwf	INTCON,0

;Verify that this firmware matches the programmer
			call	K_PROG_ID
			xorlw	ME2700ID
			btfss	STATUS,Z,0
			goto	wrong_programmer

;Verify that this firmware version matches the kernel in FLASH. We only care about
;the major revision level - minor revision levels shouldn't affect compatibility.
;(This will work so long as the wrong kernel's K_PRINTF hasn't changed.) Continue
;running even with the wrong kernel revision.
			lfsr	0,wrongkern_msg-sstrings
			call	K_KERN_REV					;returns revision in W
			andlw	0xf0						;only care about major revision
			xorlw	REQUIRED_KERN_REV
			btfss	STATUS,Z,0
			rcall	cr_printf2

;Print the sign-on message
			lfsr	0,signon_msg-sstrings
			rcall	cr_printf2					;print the signon message

;Set up for the currently-selected EPROM, and report that
			call	etype_setup					;set up Vpp supply, etc.
			call	rep_etype					;report EPROM type
			rcall	rep_dinvert					;report the data inversion state
			lfsr	0,cmdlst_msg-sstrings		;"? for command list"
			rcall	cr_printf2

			call	epower_off					;in particular, turn off busy light

;Fall into main

;################################################################################
;# Main Program Loop															#
;#   Wait for a complete (CR-terminated) line of input from the user, and stash	#
;#     the entire line in the Line Buffer										#
;#   Parse the line for commands and variables									#
;#   Branch to the appropriate command-processing routine based on the first	#
;#     character from the user. (Further decoding is done in the first-level	#
;#     command processing routines.)											#
;#   All command processing routines return to main except the two hex download	#
;#     routines, which branch back to no_prompt between hex records.			#
;#   The stack gets reset here, in case of aborts to main.						#
;################################################################################
main:			clrf	STKPTR					;fix stack in case we aborted
				rcall	do_main					;push main return address on stack
				bra		main

;Print prompt on a new line
do_main:		lfsr	0,prompt_msg-sstrings
				rcall	cr_printf2

;Clear counters used for intel hex and motorola S-record file downloads
;and other variables
				clrf	RECOUNTH				;RECOUNTH:RECOUNTL counts received records
				clrf	RECOUNTL
				clrf	ERRCNTH					;ERRCNTH:ERRCNTL counts bad hex records
				clrf	ERRCNTL
				clrf	COUNTH					;COUNTH:COUNTL counts records loaded into buffer
				clrf	COUNTL

				bcf		FLAGS1,TEST				;tell ewrite_byte that we are not testing

;-----------------------------------------------------------------
; Get a complete, CR-terminated line into the line buffer
;
; Entry at no-prompt is used after receiving a hex data record, so
; that a prompt does not get printed between records, and so that
; the error and record counters continue to accumulate.
;-----------------------------------------------------------------
no_prompt:		bcf		FLAGS1,B_STATE,0		;not yet a blank-check, S5 record, or a checksum command

				movlw	MAXLIN-1				;max input line size
				call	K_GETLIN				;get a line from the user into the LINEBUF
				bz		do_main					;no input or control-C: start over
		
;------------------------------------------------------
; Get the 1st character from the command line, and
; test for hex download command immediately, for speed.
;------------------------------------------------------
				call	K_GETUCHR				;get uppercase chr in W and R0

				xorlw	':'						;beginning of Intel Hex record?
				btfsc	STATUS,Z,0
				bra		cmd_intel

				xorlw	':' ^ 'S'				;beginning of Motorola S-record record?
				btfsc	STATUS,Z,0
				bra		cmd_motorola

				btfsc	KERN_FLAGS,CR_FLAG,0	;is this line just a CR?
				bra		do_main					;yes: just ignore the line.

				movf	POSTDEC1,W,0			;back up to beginning of input line

;--------------------------------------------------------------
; Search command table for match, get its command index into R4
;--------------------------------------------------------------
				movlw	high(main_cmds)
				movwf	TBLPTRH,0
				movlw	low(main_cmds)
				movwf	TBLPTRL,0

				call	K_PARSE					;returns W=command index
				movwf	R4,0					;remember index
			
;--------------------------------------------------------------------------
; Get the first two 16-bit hex parameters that came with the 2-character
; command. gethex4 will return default 0x0000  and INP_FLAG=0 if no
; parameter was provided, and will abort directly to main on bogus hex.
;--------------------------------------------------------------------------
;Get the first parameter and store it in ADDRESSH:ADDRESSL. Set INP_FLAG if a
;parameter was provided, and clear it if not. The first parameter is usually the
;address, though sometimes it is an 8-bit parameter.

				bsf		FLAGS1,DFLT_FLAG,0		;assume we will default

				call	K_GETHEX4				;get the 1st parameter into R2:R1
				btfsc	KERN_FLAGS,ERR_FLAG		;Bogus input?
				bra		cmd_error

				btfsc	KERN_FLAGS,INP_FLAG,0	;any input provided?
				bcf		FLAGS1,DFLT_FLAG,0		;y: do not default

				movff	R1,ADDRESSL				;put first parameter into ADDRESSH:ADDRESSL
				movff	R2,ADDRESSH

;Test to see if the first parameter (the address) is greater than the max allowed
				movf	ADDRESSH,W,0
				sublw	(high(SRAM_SIZE))-1		;carry cleared if ADDRESSH > high(SRAM_SIZE)-1
				btfss	STATUS,C,0
				bra		cmd_error				;error if greater than max address

;Get second parameter, which is a byte count, if it exists. Store it in COUNTH and COUNTL.
;Note that 0000 means to use the entire EPROM or the entire buffer.
				call	K_GETHEX4				;get the 1st parameter
				btfsc	KERN_FLAGS,ERR_FLAG		;Bogus input?
				bra		cmd_error

				movff	R1,COUNTL				;2nd parameter to COUNT
				movff	R2,COUNTH

;If count is 0000 then set it to the buffer size and set FULL_FLAG to remember that
;the range is an entire EPROM
				bcf		FLAGS1,FULL_FLAG,0		;assume partial EPROM operation

				movf	COUNTH,W,0
				iorwf	COUNTL,W,0				;set Z if both bytes were 00

				movlw	high(SRAM_SIZE)			;limit to buffer size
				btfsc	STATUS,Z,0
				movwf	COUNTH,0

;Test to see if second parameter (the count) is greater than the entire buffer
				movf	COUNTH,W,0
				sublw	high(SRAM_SIZE)			;W = high(SRAM_SIZE)- COUNTH
				btfss	STATUS,C,0
				bra		cmd_error				;Carry clear if COUNTH > high(SRAM_SIZE)
				bnz		param2_ok

				iorwf	COUNTL,W,0				;exactly equal to max?
				btfss	STATUS,Z,0
				bra		cmd_error				;no: count is too big

				bsf		FLAGS1,FULL_FLAG,0		;complete buffer operation (no 'range' in messages)
param2_ok:

;-----------------------------------------------------------------------
; Go execute a command
; R4 = command index, 0 means not found
; ADDRESSH:ADDRESSL = 1st parameter that came with the command
; COUNTH:COUNTL = 2nd  parameter that came with the command
; DFLT_FLAG = 1 if no parameters came with the command.
; FULL_FLAG is cleared unless COUNTH:COUNTL = 8K
; All of these command-processing routines return to main when done
;-----------------------------------------------------------------------
jmptab1:	movf	PCL,W,0				;force a load of the PCLATH register (page 62)
			movf	R4,W,0				;command index (a multiple of 2)
			addwf	PCL,F,0

			bra		cmd_error			;Command not found	
			bra		cmd_blankchk		;EB
			bra		cmd_dispeprom		;ED
			bra		cmd_ceedit			;EE
			bra		cmd_dinvert			;DI
			bra		cmd_eplist			;EL
			bra		cmd_program			;EP
			bra		cmd_read			;ER
			bra		cmd_compare			;EC
			bra		cmd_etype			;ET
			bra		cmd_boffset			;BAO
			bra		cmd_foffset			;FAO
			bra		cmd_uploadi			;UI
			bra		cmd_uploads			;US
			bra		cmd_bdisplay		;BD
			bra		cmd_bedit			;BE
			bra		cmd_fill			;BF
			bra		cmd_dstate			;DS
			bra		cmd_echo			;ECHO
			reset						;RESET (just reset here)
			bra		cmd_avpp			;AVPP
			bra		cmd_tas				;TAS
			bra		cmd_vbbvdd			;VBD
			bra		cmd_tvcc			;VCC
			bra		cmd_toe				;TOE
			bra		cmd_tpgm			;TPGM
			bra		cmd_tprog			;TPROG
			bra		cmd_tread			;TREAD
			bra		cmd_tcs				;TCS
			bra		cmd_tvpp			;TVPP
			bra		cmd_thi				;THI
			bra		cmd_rdata			;RD
			bra		cmd_waddress		;WA
			bra		cmd_wdata			;WD
			bra		cmd_help			;?
			bra		cmd_bhelp			;?B
			bra		cmd_nhelp			;?N
			bra		cmd_dhelp			;?D
			bra		cmd_ehelp			;?E
			bra		cmd_fhelp			;?F
			bra		cmd_lhelp			;?L

	if	!DEBUG
jmpend1:	bra		cmd_cksum			;ES
	endif

	if	DEBUG
jmpend1:	bra		cmd_cksum
			goto	cmd_pw				;extra debug cmd for tuning the switcher's pulse width
	endif

;********************************************************************************
; The above jump table must not span a 256-byte page boundary.
;********************************************************************************
	if high(jmptab1)^ high(jmpend1)
	error "Martin sez: Main command jump table spans a page boundary"
	endif

;******=========================================================================*
;* EE *  Invoke Custom EPROM Editor Command
;******
;*==============================================================================*
cmd_ceedit:		goto	do_ceedit		;long jump

;*****==========================================================================*
;* ? *  General Help Command
;*****
; No parameters
;*==============================================================================*
cmd_help:		movlw	low(help_msg)
				movwf	TBLPTRL,0
				movlw	high(help_msg)
				bra		print_help
				
;******=========================================================================*
;* ?B *  Help with Buffer Commands
;******
; No parameters
;*==============================================================================*
cmd_bhelp:		movlw	low(bhelp_msg)
				movwf	TBLPTRL,0
				movlw	high(bhelp_msg)
				bra		print_help
				
;******=========================================================================*
;* ?N *  General notes on ME2700
;******
; No parameters
;*==============================================================================*
cmd_nhelp:		movlw	low(notes2700)
				movwf	TBLPTRL,0
				movlw	high(notes2700)
				bra		print_help
				
;******=========================================================================*
;* ?D *  Help with Diagnostic Commands
;******
; No parameters
;*==============================================================================*
cmd_dhelp:		movlw	low(dhelp_msg)
				movwf	TBLPTRL,0
				movlw	high(dhelp_msg)
				bra		print_help
				
;******=========================================================================*
;* ?E *  Help with EPROM Commands
;******
; No parameters
;*==============================================================================*
cmd_ehelp:		movlw	low(ehelp_msg)
				movwf	TBLPTRL,0
				movlw	high(ehelp_msg)
				bra		print_help
				
;******=========================================================================*
;* ?F *  Help with File Transfer Commands
;******
; No parameters
;*==============================================================================*
cmd_fhelp:		movlw	low(fhelp_msg)
				movwf	TBLPTRL,0
				movlw	high(fhelp_msg)
				bra		print_help
				
;******=========================================================================*
;* ?L *  Help with Loader
;******
; No parameters
;*==============================================================================*
cmd_lhelp:		movlw	low(lhelp_msg)
				movwf	TBLPTRL,0
				movlw	high(lhelp_msg)

print_help:		movwf	TBLPTRH,0
				goto	K_PRINTF			;print the message, return to main

;*****==========================================================================*
;* S *  Incoming Motorola S-Record Command
;*****
; Valid Motorola S-record follows. ('S' already received)
; This routine validates a Motorola S-record, puts the payload into the buffer,
; and checks the checksum. The prompt is not printed after a data record, so
; that we can keep up with reception.
; Supports S1 (data), S5 (record count), and S9 (end-of-file) record types.
; S9 records can be either a complete S9 record, or just 'S9', as you find
; in some S-record files for (for example) the Altair 680
; B_STATE flag is used to remember that we are doing an S5 record
; On Entry:
;   RECOUNTH:RECOUNTL = count of received records so far
;   ERRCNTH:ERRCNTL = count of bad hex records so far
;   COUNTH:COUNTL = count of records loaded into buffer so far
;   B_STATE = 0: not yet a blank-check, S5 record,  or a checksum command
;   ERR_FLAG = 0
;   AO_MANUAL = 1 if the address offset is manual, 0 if automatic
;   AO_INIT = 1 if the automatic address offset has been set
;   ADDR_OFFSET = the address offset
;   EPROM power is off
; On Exit:
;   W,R0,R1,R2,R3,FSR1 trashed
;   RECOUNTH:RECOUNTL = count of received records so far
;   ERRCNTH:ERRCNTL = count of bad hex records so far
;   COUNTH:COUNTL = count of records loaded into buffer so far
;*==============================================================================*
cmd_motorola:	bcf		FLAGS1,INTEL_STATE,0	;not Intel, but Motorola

;Start Motorola checksum at 1 so that 00 is a good result at the end
				movlw	0x01
				movwf	CHECKSUM,0

;Get & validate single-character Motorola record type
				call	K_GETCHR				;get W = record type from line buffer

				xorlw	'1'						;S1 data record?
				bz		mot_s1

				xorlw	'1' ^ '9'				;S9 EOF record?
				bz		mot_s9

				xorlw	'9' ^ '5'				;S5 record-count record?
				bnz		rec_error				;no: unsupported record type

;*------------------------------------------------------------------------------*
; This is a Motorola S5 Record-Count record
; Get the rest of the record. B_STATE will tell us to check the record-count
; when done.
;*------------------------------------------------------------------------------*
mot_s5:			bsf		FLAGS1,B_STATE,0		;remember that this is an S5 record

;Get byte count and subtract 3 to account for address and checksum bytes
;The actual number of data bytes for S5 and S9 records must be 0
mot_nodatarec:	rcall	get_byte_cksum			;get a hex byte into W & R1

				movlw	0x03
				subwf	R1,W,0					;W has record byte count
				bz		download_rec			;0 data bytes: good S5 or S9 record so far

;fall into rec_error if not 0 data bytes

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Common Hex Loader Error Routines: Report bad hex record of some sort, and go
; get another record
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
rec_error:		lfsr	0,recerror_msg-sstrings		;indicate record error
				bra		ha_error

s5_error:		lfsr	0,rcnterror_msg-sstrings	;indicate bad S5 record
				bra		ha_error

cksum_error:	lfsr	0,cksumerror_msg-sstrings	;indicate checksum error

ha_error:		infsnz	ERRCNTL,F,0					;bump bad record count
				incf	ERRCNTH,F,0
				call	printf						;print error msg without leading CR

				bra		dr_next_record

;*------------------------------------------------------------------------------*
; This is a Motorola S1 (data) record 
; Get the byte count and subtract 3 to account for address and checksum bytes
;*------------------------------------------------------------------------------*
mot_s1:			rcall	get_byte_cksum				;get a hex byte into W & R1

				movlw	0x03
				subwf	R1,W,0						;W has record byte count				
				bra		download_rec

;*==============================================================================*
; Subroutine to get an exactly 2-character hex value from the Line Buffer, and
; also add the result to CHECKSUM
; This is as fast as possible so we can keep up with hex record loading
; This routine *IS* cases sensitive - lowercase will cause an error.
; This routine also does not skip over white space.
; Subroutine located here to facilitate rcall access.
; On Entry:
;    FSR1 points to the next chr in the line buffer
; On Exit:
;    FSR1 incremented twice
;    R0 trashed
;    Result in W and R1
;    Result added to CHECKSUM
;    Z set if checksum = 0
;    Abort rudely to ha_error if non-hex character found
; (45 cycles, worst case)
; Trashes R0
;*==============================================================================*
get_byte_cksum:	call	K_GETHEX2				;get hex byte, compute checksum

				btfss	KERN_FLAGS,ERR_FLAG		;hex error?
				return

				lfsr	0,hexerror_msg-sstrings	;indicate hex error
				pop								;chuck return address
				bra		ha_error

;*****==========================================================================*
;* : * Incoming Intel Record Command
;*****
; Valid Intel Hex record follows. (Colon already received)
; Supports 00 (data) and 01 (end-of-file) record types. 0-byte data records
; are also treated as end-of-file records.
; This routine validates an Intel Hex record, puts the payload into the buffer,
; and checks the checksum. The prompt is not printed after a data record, so
; that we can keep up with reception. 
; On Entry:
;   RECOUNTH:RECOUNTL = count of received records so far
;   ERRCNTH:ERRCNTL = count of bad hex records so far
;   COUNTH:COUNTL = count of records loaded into buffer so far
;   B_STATE = 0
;   ERR_FLAG = 0
;   AO_MANUAL = 1 if the address offset is manual, 0 if automatic
;   AO_INIT = 1 if the automatic address offset has been set
;   ADDR_OFFSET = the address offset
;   EPROM power is off
; On Exit:
;   W,R0,R1,R2,R3,FSR1 trashed
;   RECOUNTH:RECOUNTL = count of received records so far
;   ERRCNTH:ERRCNTL = count of bad hex records so far
;   COUNTH:COUNTL = count of records loaded into buffer so far
;*==============================================================================*
cmd_intel:		bsf		FLAGS1,INTEL_STATE,0	;Intel, not Motorola
				clrf	CHECKSUM,0				;start Intel checksum at 0

;get record data byte count into W
				rcall	get_byte_cksum			;get a hex byte into W & R1

;fall into download_rec

;*********----------------------------------------------------------------------*
;* S & : * Common Hex Record Download Routine
;*********
; On Entry:
;   W= number of data bytes expected in the record
;   RECOUNTH:RECOUNTL = count of received records so far
;   ERRCNTH:ERRCNTL = count of bad hex records so far
;   COUNTH:COUNTL = count of records loaded into buffer so far
;   CHECKSUM is initialized for the particular record type
;   EPROM power is off
;   INTEL_STATE = 1 for Intel Hex record, 0 for S-Record
;   B_STATE = 1 for Motorola S5 Record, 0 otherwise
;   ERR_FLAG = 0, meaning that so far, this record fits in the buffer
;   AO_MANUAL = 1 if the address offset is manual, 0 if automatic
;   AO_INIT = 1 if the automatic address offset has been set
;   ADDR_OFFSET = the address offset
; On Exit:
;   Record data payload is in the specified place in the buffer
;   W,R0,R1,R2,R3,ADDRESSH:ADDRESSL,FSR1 trashed
;   RECOUNTH:RECOUNTL = count of received records so far
;   ERRCNTH:ERRCNTL = count of bad hex records so far
;   COUNTH:COUNTL = count of records loaded into buffer so far
; (Around 320 cycles = 80 uS for a worst-case 1-byte record. 9600 baud = 1000
; chr/sec, or about 1mS per character. We can parse an entire record between 2
; received characters, no problem.)
;*------------------------------------------------------------------------------*
download_rec:	movwf	R3,0						;save this record's byte count
				infsnz	RECOUNTL,F,0				;bump hex record count
				incf	RECOUNTH,F,0

;Get the high address byte. If the address offset is set for automatic and is
;uninitialized, then set it to be the same as this record's high address byte,
;and remember that the address offset is now initialized.
				rcall	get_byte_cksum				;get a hex byte into W & R1

				btfsc	FLAGS1,AO_MANUAL,0			;manually-set address offset?
				bra		d_rec1						;y: address offset is okay

				btfss	FLAGS1,AO_INIT,0			;automatic address offset initialized?
				movwf	ADR_OFFSET,0				;n: set it to this record's address

				bsf		FLAGS1,AO_INIT,0			;address offset is now initialized	
d_rec1:

;Subtract off the address offset, and save the result in ADDRESSH.
;We will test the address of each received byte later, so that we
;can tolerate records that have some data within range, and some not.
				movf	ADR_OFFSET,W,0
				subwf	R1,W,0						;W= high(record address) minus offset
				movwf	ADDRESSH,0

;Get address low byte and save it in ADDRESSL, preparing to load data
				rcall	get_byte_cksum				;get address low byte into W & R1
				movwf	ADDRESSL,0					;ADDRESSH:ADDRESSL will be used to move data

;If this is an Intel Hex record, get and validate record-type byte
				btfss	FLAGS1,INTEL_STATE,0
				bra		dr_get_data					;no record-type byte for S-records

				rcall	get_byte_cksum				;get Intel Hex record type byte into R1

				movf	R1,F,0						;test record type for type 0
				bz		dr_get_data					;zero is normal data record

				decfsz	R1,F,0
				bra		rec_error					;any type higher than 1 is an error

;Validate Intel type 1 record
				tstfsz	R3,0						;byte count must be 0 for type 1 records
				bra		rec_error

;If this is a 0-byte record, then don't load any data. Just check its checksum and call it done.
dr_get_data:	movf	R3,W,0						;record byte count
				bz		dr_ckcum

				movwf	R2,0						;R2 will be our loop counter
				bcf		FLAGS1,A_STATE,0			;Fits in buffer so far
;Loop to get R2 bytes of data in this record. If any of this record's data bytes
;do not fit in the buffer, then set ERR_FLAG.
dr_data_loop:		rcall	get_byte_cksum			;get a hex data byte into W & R1

					;test to see if this byte will miss the buffer. Clear C and set ERR_FLAG if so
					movf	ADDRESSH,W,0
					sublw	high(SRAM_SIZE)			;C cleared if high(EPROM address) > high(SRAM_SIZE)
					btfss	STATUS,C,0
					bsf		FLAGS1,A_STATE,0		;remember not to count this page as sucessful

					movf	R1,W,0					;recover received byte
					btfsc	STATUS,C,0				;don't write if wrong page for this byte
					call	K_WRSRAM				;write byte to buffer (trashes W, R0, R1)

					infsnz	ADDRESSL,F,0			;16-bit increment address pointer
					incf	ADDRESSH,F,0

					decf	R2,F,0					;bump & test loop pointer
					bnz		dr_data_loop

;If all bytes in this record landed in the buffer, then count it as a successfully-loaded record.
				btfsc	FLAGS1,A_STATE,0		;any data in this record not fit in the buffer?
				bra		dr_noload

				infsnz	COUNTL,F,0					;bump count of successfully-loaded records
				incf	COUNTH,F,0
dr_noload:

;Get and validate the record checksum
dr_ckcum:		rcall	get_byte_cksum				;get checksum into Z
				bnz		cksum_error

;If this is a 0-byte record, then treat it as the eof. Otherwise, go get another record
				movf	R3,F,0						;record byte count
				bnz		dr_next_record				;not 0 bytes: go get another record

				btfss	FLAGS1,B_STATE,0			;is this an S5 record?
				bra		dr_eof_rec					;no: this 0-byte record is the end of file

;S5 record is done - compare the number of records we have received (in COUNTH:COUNTL)
;to the value in ADDRESSL - the low byte of the address field, which is the record count
				decf	COUNTL,W,0					;don't count this S5 record
				cpfseq	ADDRESSL,0					;compare to the S5 record's record count
				rcall	s5_error					;report S5 record count mismatch

;------------------------------------------------------------
; This record is done, and we expect another one. Print
; CR-LF, and go get another record, without printing a prompt
;------------------------------------------------------------
dr_next_record:	movlw	CR							;K_CONOUT will add the LF
				call	K_CONOUT
				bra		no_prompt					;done with command, no prompt

;*------------------------------------------------------------------------------*
; This is a Motorola  S9 record (which specifies the record count) 
; It may be just 'S9', or may be complete S9 record.
;*------------------------------------------------------------------------------*
mot_s9:			call	K_GETCHR				;test for CR
				decf	FSR1L,F,0				;put pointer back
				btfss	KERN_FLAGS,CR_FLAG,0	;was it the CR?
				bra		mot_nodatarec			;no: go get the rest of the rest of the record

;Fall into dr_eof_rec to finish download
;-----------------------------------------------------
; End of File record encountered.
; Report results
;-----------------------------------------------------
dr_eof_rec:		lfsr	0,reccount_msg-sstrings		;'Records: '
				rcall	cr_printf2
				movff	RECOUNTH,R1					;total record count
				movff	RECOUNTL,R0
				call	K_PRINTDEC

				lfsr	0,errorcount_msg-sstrings	;', Bad Records: '
				call	printf
				movff	ERRCNTH,R1					;bad record count
				movff	ERRCNTL,R0
				call	K_PRINTDEC

				movlw	CR
				call	K_CONOUT					;new line

				movff	COUNTH,R1					;records loaded into buffer
				movff	COUNTL,R0
				call	K_PRINTDEC

				lfsr	0,loadcount_msg-sstrings	;'records loaded in buffer with Address Offset: '
				call	printf

				movf	ADR_OFFSET,W,0				;current address offset
				call	K_PRINTHEX2					;print address offset in hex

				movlw	'h'							;note that this is in hex
				call	K_CONOUT

;-------------------------------------------------------------------------
;Reset automatic address offset settings for next file, and return to main
;-------------------------------------------------------------------------
				btfss	FLAGS1,AO_MANUAL,0		;automatic address offset mode?
				clrf	ADR_OFFSET,0			;y:prepare for next file load
				bcf		FLAGS1,AO_INIT,0		;de-initialize address offset

				return							;finally return to main

;################################################################################
;#						Main Command Execution Routines							#
;################################################################################
;*******========================================================================*
;* BAO * Set Buffer Address Offset Command
;*******
; Sets an offset for the upper 8 bits of address for uploads and downloads
; On Entry:
;	DFLT_FLAG = 1 if the user did not provide an address offset value.
;   ADDRESSL = buffer address offset
;*==============================================================================*
cmd_boffset:	tstfsz	ADDRESSH,0				;no high byte allowed
				bra		cmd_error

				movff	ADDRESSL,BUF_OFFSET		;save input as the new buffer address offset

;Fall into rep_ba_offset to print the address offset, return to main

;*==============================================================================*
; Subroutine to report the buffer address offset
; On Entry:
;*==============================================================================*
rep_ba_offset:	lfsr	0,boffset_msg-sstrings	;'Buffer Address Offset: '
				rcall	cr_printf2

				movf	BUF_OFFSET,W,0			;buffer address offset
				goto	K_PRINTHEX2				;print buffer address offset and return

;*******========================================================================*
;* FAO * Set File Address Offset Command
;*******
; Sets an offset for the upper 8 bits of address for uploads and downloads
; On Entry:
;	DFLT_FLAG = 1 if the user did not provide an address offset value.
;   ADDRESSL = file address offset
;*==============================================================================*
cmd_foffset:	tstfsz	ADDRESSH,0				;no high byte allowed
				bra		cmd_error

				movff	ADDRESSL,ADR_OFFSET		;save input as the new address offset

				bcf		FLAGS1,AO_MANUAL,0		;assume automatic address offset
				bcf		FLAGS1,AO_INIT,0		;address offset is no longer initialized	

				btfss	FLAGS1,DFLT_FLAG,0		;was a parameter provided?
				bsf		FLAGS1,AO_MANUAL,0		;y: address offset is now manual	

;Fall into rep_fa_offset to print the address offset, return to main

;*==============================================================================*
; Subroutine to report the file address offset, which may either be
; a value or 'automatic'
; On Entry:
;   AO_MANUAL  = 1 if manual address offset mode selected
;              = 0 if automatic address offset mode
;   ADR_OFFSET = address offset if manual mode
;*==============================================================================*
rep_fa_offset:	lfsr	0,foffset_msg-sstrings	;'File Address Offset: '
				rcall	cr_printf2

				movf	ADR_OFFSET,W,0			;manually-set address offset

				btfsc	FLAGS1,AO_MANUAL		;manual address offset mode?
				goto	K_PRINTHEX2				;y: print file address offset and return

;File address offset is automatic
				lfsr	0,auto_msg-sstrings		;'automatic'
				goto	printf					;return from there

;*******========================================================================*
;* BD *  Display Buffer Command
;******
; Displays the specified range of buffer contents in human-readable form,
; followed by the checksum of the displayed range
; On Entry:
;    ADDRESSH:ADDRESSL = address
;    COUNTH:COUNTL = byte count
;    DFLT_FLAG = 1 if no parameters came with the command.
;    FULL_FLAG is cleared unless COUNTH:COUNTL = 8K
;*==============================================================================*
cmd_bdisplay:	clrf	CHECKSUM,0				;start buffer (range) checksum

;Print the address, (The first one may not be on a 16-byte boundary)
bd_newline:			call	check_pause			;give user a chance to pause (CTRL-S will pause too)

					call	print_addr			;start each line with its address
					call	save_params			;save address and count for ASCII

					movlw	0x10*3+1			;# of chrs for hex portion of line +1
					movwf	R3,0

;Loop to print hex for every byte on this row. The row end when the address of the
;next byte has bits <3:0> = 0.
bd_loop:				call	K_RDSRAM		;get a buffer byte into W
						call	print_hex_csum	;print it in hex, accumulate checksum

						movlw	' '				;print a space between hex bytes
						call	K_CONOUT

						movlw	0x03			;account for printed hex byte and space
						subwf	R3,F,0			;..so fewer spaces are required later

						call	next_a_count	;bump ADDRESSH:ADDRESSL, decrement COUNT, test
						bz		bd_ascii		;all done? then finish up the ASCII too.

						movlw	0fh				;test for least-sig nibble = 0
						andwf	ADDRESSL,W,0	;Z set if the row is done
						bnz		bd_loop
bd_ascii:

;Space over to the same column (column 56), regardless of how many hex characters were
;printed on this line. R3 has the number of required spaces.
bd_sploop:				movlw	' ';
						call	K_CONOUT
						decf	R3,F,0
						bnz		bd_sploop			

;Loop to print ASCII characters for the line. Replace unprintable characters (below space,
;and also DEL=0x7F) with dots
					call	restore_params		;beginning of row and count

bd_aloop:				call	K_RDSRAM		;get a buffer byte into W
						addlw	0x01			;because 0x7F is also not printable
						andlw	0x7F			;strip parity
						sublw	' '				;unprintable chr? carry set if unprintable
						btfsc	STATUS,C,0
						movlw	' '-'.'-0x01	;dot, pre-distorted

						sublw	' '-0x01		;restore chr or dot

						call	K_CONOUT		;print chr or dot

						call	next_a_count	;bump ADDRESSH:ADDRESSL, decrement COUNT, test
						bz		bd_sum			;All done with entire BD?

						movlw	0fh				;test for least-sig nibble = 0
						andwf	ADDRESSL,W,0	;Z set if the ASCII for this row is done
						bnz		bd_aloop		;finish ASCII for this row

					bra		bd_newline			;next line

;Print buffer sum and quit. 
bd_sum:			lfsr	0,buffer_msg-sstrings	;'Buffer '
				call	prange					;print string, then 'range ' as needed

				lfsr	0,csum_msg-sstrings		;'checksum: '
				call	printf					;print message

				movf	CHECKSUM,W,0			;get buffer checksum
				goto	K_PRINTHEX2				;print & return to main

;******=========================================================================*
;* BE * Edit Buffer Command
;******
; Allows user to interactively modify data in the buffer.
; Second command chr (in R4) must be 'B'
; On Entry:
;   ADDRESSH:ADDRESSL = address
; On Exit:
;   Normal exit is via ^C, detected by K_GETLIN
;*==============================================================================*
cmd_bedit:		bsf		STATUS,Z,0
					
;If Z set, print buffer address, followed by ': '
mb_loop:			btfsc	STATUS,Z,0
					call	print_addr

;print buffer data followed by a space
					call	K_RDSRAM				;get buffer data
					call	K_PRINTHEX2				;..and print it
					movlw	' '						;space seperator
					call	K_CONOUT

;Get user input - either CR to skip, new hex value for this location, or ^C/ESC to quit
					movlw	0x02					;max 2 characters
					call	K_GETLIN				;get user input into buffer (trashes FSR0 & FSR1)
					btfsc	KERN_FLAGS,ERR_FLAG,0	;control-C or ESC?
					return							;y: abort (done)

					call	K_GETHEX4				;get the 1st parameter
					btfsc	KERN_FLAGS,ERR_FLAG		;Bogus input?
					bra		cmd_error

					btfss	KERN_FLAGS,INP_FLAG,0	;clear if no byte found
					bra		mb_nxtbyte				;Z set if no input	

					movff	R1,R4					;temp save new value in R4

					call	K_GETHEX4				;there shouldn't be anything more 	
					btfsc	KERN_FLAGS,INP_FLAG,0	;clear if no byte found
					bra		cmd_error				;huh?

					movf	R4,W,0					;recover user input
					call	K_WRSRAM				;overwrite buffer data with new user data

					movlw	' '						;space separator
					call	K_CONOUT			
mb_nxtbyte:
;Every 8 bytes, print CR and address again.
;This will wrap to the beginning of the buffer if you keep going.
					call	next_a_count			;next address, with wrap
					movlw	0x07					;test for every 8th byte
					andwf	ADDRESSL,W,0			;set Z flag if we need to print the address
					bra		mb_loop

;******=========================================================================*
;* BF * Fill EPROM Buffer Command
;******
; Fills all 8K of the EPROM buffer with the given value
; On Entry:
;   The second command chr (in R4) must be 'B'
;   ADDRESSL = fill value, defaulting to 0.
;   Undocumented: COUNT is the number of bytes to fill, starting at buffer
;   FULL_FLAG is cleared unless COUNTH:COUNTL = 8K
;   address 0
;*==============================================================================*
cmd_fill:		tstfsz	ADDRESSH,0				;no high byte allowed
				bra		cmd_error

				movff	ADDRESSL,R2				;remember fill value

				lfsr	0,buffill_msg-sstrings	;'Buffer filled with '
				rcall	cr_printf2

				clrf	ADDRESSL,0				;point to beginning of buffer
				clrf	ADDRESSH,0

;Loop to fill EPROM buffer
fill_buf_loop:		movf	R2,W,0				;get fill value
					call	K_WRSRAM			;write to buffer

					call	next_a_count		;bump address pointer, dec COUNT
					bnz		fill_buf_loop		;Z set when COUNT is 0000

				movf	R2,W,0					;get fill value again
				goto	K_PRINTHEX2				;tell user fill value, return to main

;******=========================================================================*
;* DS *  Display Programmer State Command
;******
; Displays:
;  Kernel Loader revision
;  Curent EPROM type
;  Data invert state
;  Vpp setting
;  External voltage required
;  Address Offset
;  Echo State
; On Entry:
;    R4 = 2nd character of command
;===============================================================================*
cmd_dstate:		call	K_PRINTSN					;Print this unit's serial number

				lfsr	0,loaderrev_msg-sstrings	;loader rev
				rcall	cr_printf2

				call	K_KERN_REV					;return loader revision in W
				movwf	R0,0
				swapf	R0,W,0						;print the major revision first
				call	K_PRINTHEX1
				movlw	'.'							;decimal point seperator
				call	K_CONOUT
				call	K_KERN_REV					;return loader revision in W
				call	K_PRINTHEX1					;now print the minor revision

				lfsr	0,fwrev_msg-sstrings		;firmware revision message
				rcall	cr_printf

				call	rep_etype					;report current EPROM type
				call	rep_vpp						;Report the Vpp setting

				clrf	R2,0						;no formatting
				movlw	ET_VPPMSG
				btfsc	FLAGS2,EXT_VPP,0			;external supply required?
				call	eds_print					;print the adjustment message

				rcall	rep_fa_offset				;file address offset
				rcall	rep_ba_offset				;buffer address offset
				rcall	rep_dinvert					;report data invert state

				rcall	rep_echo					;report echo state

 if DEBUG
				lfsr	0,shared_msg-sstrings
				rcall	cr_printf
				movf	SHARED,W,0
				call	K_PRINTHEX2
 endif

;Report successful and unsuccessful programmings
				movlw	CR
				call	K_CONOUT

				movlw	PROGCNT						;successful EPROM programmings
				call	K_RDEEPROM
				movwf	R0,0
				movlw	PROGCNT+1					;high byte
				call	K_RDEEPROM
				movwf	R1,0
				call	K_PRINTDEC					;print R1:R0 in decimal

				lfsr	0,ep_msg-sstrings			;" EPROMS programmed "
				rcall	printf

				lfsr	0,eps_msg-sstrings			;"successfully"
				rcall	printf

				movlw	CR
				call	K_CONOUT

				movlw	FAILCNT						;unsuccessful EPROM programmings
				call	K_RDEEPROM
				movwf	R0,0
				movlw	FAILCNT+1					;high byte
				call	K_RDEEPROM
				movwf	R1,0
				call	K_PRINTDEC					;print R1:R0 in decimal

				lfsr	0,ep_msg-sstrings			;" EPROMS programmed "
				rcall	printf

				lfsr	0,epf_msg-sstrings			;"unsuccessfully"
				bra		printf						;print, return to main

;******==========================================================================*
;* ED * Display EPROM Specifications Command (with pretty picture)
;******
; On Entry:
;	DFLT_FLAG set if no parameters came with this command
;   ADDRESSL = requested EPROM
;===============================================================================*
cmd_dispeprom:	tstfsz	ADDRESSH,0				;no high byte allowed 
				bra		cmd_error

				call	save_etype				;save ETYPE while we work

				btfsc	FLAGS1,DFLT_FLAG,0		;any value from user?
				movff	ETYPE_SAVE,ADDRESSL		;n: use the current EPROM
				movf	ADDRESSL,W,0

				sublw	FEPROM_COUNT+CEPROM_COUNT-1	;legal value?
				btfss	STATUS,C,0				;Carry set if ADDRESS <= max allowed EPROM
				bra		cmd_error

				movf	ADDRESSL,W,0			;temp set ETYPE to user input
				call	K_WREEPROM				;K_RDEEPROM left EEADR pointing to ETYPE

				call	dispeprom

				goto	restore_etype			;Done: restore ETYPE and go to main

;******=========================================================================*
;* DI * Set EPROM Data Invert State Command
;******
; Turns EPROM data invert off/on
; On Entry:
;   ADDRESSL = 0 or 1 for off or on
;===============================================================================*
cmd_dinvert:	rcall	test_01					;check for valid user input

				movlw	DINV_STATE				;this variable is in EEPROM
				movwf	EEADR,0					;..at this location

				movf	ADDRESSL,W,0			;result to W, set Z if 0
				btfss	STATUS,Z,0				;if not zero, make it FF
				movlw	0xFF

				call	K_WREEPROM

;Fall into rep_dinvert to report data invert state, return to main

;*==============================================================================*
; Subroutine to report the EPROM data invert state
; Trashes W,R2
;*==============================================================================*
rep_dinvert:	movlw	0x01					;print always
				bra		rep_di

;*==============================================================================*
; Subroutine to report the EPROM data invert state only if it is on
; Trashes W,R2
;*==============================================================================*
rep_dion:		movlw	0x00					;print always

;Fall into rep_di

;*==============================================================================*
; Local subroutine to report the EPROM data invert state
; On Entry:
;    W=0: only report invert state if it is on
;    W<>0: Always report invert state
; Trashes W,R2
;*==============================================================================*
rep_di:			movwf	R2,0					;temp save input

				lfsr	0,dinvert_msg-sstrings	;"EPROM data invert"

				movlw	DINV_STATE				;this variable is in EEPROM
				call	K_RDEEPROM				;Z set if 0
				bz		rep_di1

				rcall	cr_printf
				lfsr	0,on_msg-sstrings
				bra		printf					;always print if invert is on

;Data invert is off, and hterefore only printed if R2<>0
rep_di1:		movf	R2,F,0					;print if invert is off?
				btfsc	STATUS,Z,0
				return							;n: done

				rcall	cr_printf
				lfsr	0,off_msg-sstrings
				bra		printf

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Common Routine
;   Bad command of some sort - print command error message and return to main
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
cmd_error:		lfsr	0,bad_input_msg-sstrings	;indicate error

;Fall into cr_printf2 to print and return to main

;this is here just to allow rcall's instead of calls
cr_printf2:		bra		cr_printf

;******=========================================================================*
;* ET * Set EPROM Type Command
;******
; selects the EPROM type
; On Entry:
;	DFLT_FLAG set if no parameters came with this command
;   ADDRESSL = EPROM type, 0 means none selected
; On Exit:
;   ETYPE is set for the selected EPROM
;   Vpp power supply is set to the correct voltage
;   EXT_VPP is set (and Vpp=0V) iff an external supply is required
;	ADJ_VPP is set iff the onboard supply requires manual adjustment
;	ADJ_VCC is set iff hte programming Vcc requires manual adjustment
;   PMODE1:PMODE0 are set for this EPROM
;	PROGPULSE set for this EPROM (note: this is a 16-bit value)
;*==============================================================================*
cmd_etype:		btfsc	FLAGS1,DFLT_FLAG,0		;any value with the command?
				goto	rep_etype				;no: just report 

				tstfsz	ADDRESSH,0				;no high byte allowed
				bra		cmd_error

				movf	ADDRESSL,W,0
				sublw	FEPROM_COUNT+CEPROM_COUNT-1	;legal value?
				bnc		cmd_error				;Carry set if ADDRESS <= max allowed EPROM

;See if previous EPROM type had custom Vcc or Vpp
				movlw	ET_SPECIAL
				call	etype_getval			;result in W and R0
				movwf	R6,0					;remember for later	

;Save old ETYPE in case this one's bogus
				call	save_etype				;save ETYPE while we work
												;..and set EEADR to ETYPE
;Set the new ETYPE, and make sure it is defined
				movf	ADDRESSL,W,0			;remember EPROM type
				call	K_WREEPROM				;write to address EEADR=ETYPE

				movlw	ET_NAME
				call	etype_getval			;first chr of the name=0 means invalid entry
				iorlw	0x00
				bz		etype_invalid

;Set up programming parameters for this EPROM
				call	rep_etype				;report new EPROM type
				call	etype_setup				;set up Vpp supply, etc.

;Remind user to readjust Vpp and/or Vcc, and remove Intersil jumper, if necessary.
;R6 = previous EPROM type's ET_SPECIAL value
				movlw	ET_SPECIAL
				call	etype_getval			;does new EPROM type have custom voltages?
				andlw	0x03					;just custom Vpp and Vcc bits
				xorlw	0x03					;invert bits
				andwf	R6,F,0					;was custom and is no longer custom?

				lfsr	0,readjvcc_msg-sstrings	;"Readjust 6.2V suppy to 6.20V"
				btfsc	R6,0x00,0
				rcall	cr_printf

				lfsr	0,readjvp_msg-sstrings	;"Readjust Vpp suppy to nominal"
				btfsc	R6,0x01,0
				rcall	cr_printf

				lfsr	0,removej_msg-sstrings	;"Remove Intersil jumper and external supply"
				btfsc	R6,0x02,0
				rcall	cr_printf

				bra		epower_off

;--------------------------------------------------------------------------
; The requested etype entry is invalid. Put the old one back and error-exit
;--------------------------------------------------------------------------
etype_invalid:	rcall	restore_etype			;Restore ETYPE

				lfsr	0,undef_msg-sstrings	;"Error: Undefined custom EPROM"
				bra		cr_printf				;print message, return to main

;******=========================================================================*
;* EC * EPROM Compare to Buffer Command
;******
; Compares EPROM data to buffer data and reports differences
; On Entry:
;   ADDRESSH:ADDRESSL = starting buffer address & EPROM address
;   COUNT_LOW:COUNT_HIGH = byte count of region to checksum
;   FULL_FLAG is cleared unless COUNTH:COUNTL = 8K
;   ERR_FLAG is cleared
;   B_STATE is cleared, telling rep_mismatch this is not an EPROM blank-check
; On Exit:
;    EPROM is powered off
;*==============================================================================*
cmd_compare:	rcall	limit_count			;limit the count to a full EPROM, max

;Tell the user about the buffer address offset and invert state
				rcall	rep_ba_offset
				rcall	rep_dion			;only mention invert state if it is set

;Fall into eb_compare

;*==============================================================================*
; Subroutine to compare EPROM data to buffer data and report differences
; On Entry:
;   ADDRESSH:ADDRESSL = starting buffer address & EPROM address
;   COUNT_LOW:COUNT_HIGH = byte count of region to compare
;   ERR_FLAG is cleared
;   B_STATE is cleared, telling rep_mismatch this is not an EPROM blank-check
; On Exit:
;   ERR_FLAG set if any errors detected
;   The EPROM is powered off
; Trashes W,R0,R3,FSR0
;*==============================================================================*
eb_compare:		rcall	epower_read				;power up the EPROM for reading

				rcall	ep_compare

				lfsr	0,cmpgood_msg-sstrings	;good compare message, just in case
				btfsc	KERN_FLAGS,ERR_FLAG		;any errors?
				lfsr	0,fail_msg-sstrings		;then don't print success message		
				rcall	printf					;finish success/fail message

				bra		epower_off				;power down, return from there

;******=========================================================================*
;* EB * EPROM Blank Check Command
;******
; Verify that the specified region of the EPROM is erased (all bytes=FF)
; On Entry:
;   ADDRESSH:ADDRESSL = starting buffer address & EPROM address
;   COUNT_LOW:COUNT_HIGH = byte count of region to checksum
;   FULL_FLAG is cleared unless COUNTH:COUNTL = 8K
;   ERR_FLAG is cleared
;   EPROM is already powered up for reading
; On Exit:
;   EPROM is powered off
;*==============================================================================*
cmd_blankchk:	rcall	epower_read				;power up the EPROM
				rcall	limit_count				;limit the count to a full EPROM, max

				movlw	DINV_STATE				;Get invert state
				call	K_RDEEPROM
				movwf	R4,0
				comf	R4,F,0					;invert unless eread_byte will be inverting

;Go blank check the EPROM, with R4 being what a blank EPROM byte contains
				bsf		FLAGS1,B_STATE,0		;tell ep_compare this is an EPROM blank-check
				rcall	ep_compare

;Finish printing results
				lfsr	0,isblank_msg-sstrings	;message if blank
				btfsc	KERN_FLAGS,ERR_FLAG		;any errors?
				lfsr	0,fail_msg-sstrings		;then don't print success message
				rcall	printf

				bra		epower_off				;power off, return to main

;********=======================================================================*
;* ECHO * Set Terminal Echo State Command
;********
; Turns Echo off/on
; On Entry:
;   DFLT_FLAG set if no parameters came with this command
;     (in which case, turn echo on.)
;   ADDRESSL = 0 or 1 for off or on
;*==============================================================================*
cmd_echo:		rcall	test_01						;test for legal input value
				bcf		KERN_FLAGS,ECHO_FLAG,0		;turn echo off for the moment

				btfss	FLAGS1,DFLT_FLAG,0			;default to on if no value given
				btfsc	ADDRESSL,0x00,0
				bsf		KERN_FLAGS,ECHO_FLAG,0		;turn echo on

;Fall into rep_echo to report echo state, return to main

;*==============================================================================*
; Subroutine to report the echo state
;*==============================================================================*
rep_echo:		lfsr	0,echostate_msg-sstrings	;echo state
				rcall	cr_printf
			
				lfsr	0,off_msg-sstrings
				btfsc	KERN_FLAGS,ECHO_FLAG,0
				lfsr	0,on_msg-sstrings
				bra		printf						;return from there

;******=========================================================================*
;* EL * List Supported EPROM Types Command
;******
; Print list of supported EPROMs in up to 3 columns. This assumes that the
; total number of EPROMs supported is less than or equal to 3*LISTROWS.
;*==============================================================================*
cmd_eplist:		lfsr	0,elist_msg-sstrings	;"Supported EPROMs:"
				rcall	cr_printf

				rcall	save_etype				;save ETYPE while we work

				clrf	R4,0					;walk through all known EPROM types
				movlw	LISTROWS				;print this many rows
				movwf	R5,0					;row counter				

le_loop:			rcall	le_eprom			;print the 1st column EPROM

					movlw	LISTROWS
					addwf	R4,F,0				;2nd column
					rcall	le_eprom			;print the 2nd column EPROM

					movlw	LISTROWS
					addwf	R4,F,0				;3rd column
					rcall	le_eprom			;print the 3rd column EPROM

					movlw	(2*LISTROWS)-1		;next row, 1st column
					subwf	R4,F,0

					movlw	CR					;next line
					call	K_CONOUT

					decf	R5,F,0
					bnz		le_loop

				bra		restore_etype			;Done: restore ETYPE and go to main
				
;-----------------------------------------------------------------------
; Local suproutine to print the type & name of an EPROM
; If the number is greater than the max, then just return. If the
; first character of the EPROM name is 0 then print "Unassigned".
; On Entry:
;   R4 = the EPROM number
;-----------------------------------------------------------------------
le_eprom:		movf	R4,W,0						;EPROM number
				sublw	FEPROM_COUNT+CEPROM_COUNT-1	;legal EPROM number?
				btfss	STATUS,C,0					;Carry set if ADDRESS <= max allowed EPROM
				return								;n: just return

				movlw	ETYPE						;set ETYPE for etype_getval & eds_print
				movwf	EEADR,0
				movf	R4,W,0
				call	K_WREEPROM

				movf	R4,W,0						;Print the EPROM number
				call	K_PRINTHEX2

				lfsr	0,colonspace_msg-sstrings	;colon and space
				rcall	printf

				lfsr	0,unassgn_msg-sstrings		;"Unassigned"

				movlw	ET_NAME
				call	etype_getval				;read the first chr of the name string
				iorlw	0x00						;0 means unassigned EPROM type
				btfsc	STATUS,Z,0					;if first chr is zero
				bra		printf						;..then print "unassigned" and return

				movlw	.12							;print in 16-wide field
				movwf	R2,0
				movlw	ET_NAME
				bra		eds_print					;print the EPROM name string & return

;******=========================================================================*
;* EP * EPROM Program Command
;******
; Programs EPROM from buffer, with verify
; On Entry:
;   ET_PROGREPS has the required number of programming cycles
;     (cycle limit if smart)
;   ADDRESSH:ADDRESSL=starting buffer address
;   COUNTH:COUNTL=number of bytes to program
;   B_STATE is cleared, so rep_mismatch will know this is not a blank-check
;   ERR_FLAG is cleared
;   EPROM is already powered up for reading
;   Unassigned pins are at 0V
;*==============================================================================*
cmd_program:	rcall	epower_read				;power up the EPROM for reading
				rcall	limit_count				;limit the count to a full EPROM, max
				rcall	save_params				;make copies of ADDRESSH:ADDRESSL and COUNTH:COUNT

;Tell the user about the buffer address offset and data invert state
				rcall	rep_ba_offset
				rcall	rep_dion				;only mention invert state if it is set

;Ask the user if s/he is ready, warning him/her about the danger of a backwards EPROM etc.
				lfsr	0,ready2prog_msg-sstrings
				rcall	ask_yn					;ask if the user is ready

;-------------------------------------------------------------------------------
; Blank-check the selected region of the EPROM, unless it's an EEPROM. We don't
; use the same loop as th EC command because we don't report which bytes fail
; here - we just want a yes/no is the region blank.
; A blank EPROM byte reads as 0xFF, which eread_byte will invert if DINV_STATE
; is set.
;-------------------------------------------------------------------------------
				movlw	ET_SPECIAL				;check for EEPROM bit
				call	etype_getval
				andlw	0x08					;this bit means don't blank check
				bnz		epbc_blank

				movlw	DINV_STATE				;Get invert state
				call	K_RDEEPROM
				movwf	R4,0
				comf	R4,F,0					;invert unless eread_byte will be inverting

epbc_loop:			rcall	eread_byte			;read a byte, which may have been inverted
					xorwf	R4,W,0				;invert unless it is already
					bnz		epbc_notblank
					rcall	next_a_count		;bump address pointer, dec count
					bnz		epbc_loop
					bra		epbc_blank

;If non-blank programming range, ask the user should we continue?
epbc_notblank:	lfsr	0,notblank_msg-sstrings	;Not blank. Program anyway?
				rcall	ask_yn					;abort if user says no
epbc_blank:

;------------------------
; Set up for programming
;-----------------------
				rcall	epower_prog					;set power supplies for programming

				clrf	TMR0H,0						;restart the pacifier timer(not really necessary) 
				clrf	TMR0L,0						;TMR0L must be written after TMR0H (page 146)

				lfsr	0,programing_msg-sstrings	;Announce programming
				rcall	cr_printf
				rcall	wait_txempty				;finish printing message before blocking ints

;---------------------------------------------------
; Programming Phase 1:
; If we are using the simple programming algorithm
; then loop through the EPROM range PROGREPS times.
;Otherwise, loop through the range just once.
;---------------------------------------------------
				movf	PROGREPS,W,0			;R7 counts cycles for simple programming
				tstfsz	SMART1,0				;SMART1 = 0 means not smart
				movlw	0x01					;just one pass for smart programming

				movwf	R7,0					;R7 counts passes for Phase 1

prog_loop1:			movff	SMART1,R6			;get 1st pass programming mode
					rcall	ewrite_range		;write the range once (trashes R0-R5)
					decf	R7,F,0
					bnz		prog_loop1

;---------------------------------------------------
; Programming Phase 2:
; Once more through the EPROM, if specified
;---------------------------------------------------
				movf	SMART2,W,0				;get & test 2nd pass programming mode
				movwf	R6,0
				btfss	STATUS,Z,0				;there may not be a 2nd pass.
				rcall	ewrite_range			;write the range once (trashes R0-R5)

				rcall	epower_off				;turn off the programming voltages etc.

;----------------------------
; Verify the programmed range
;----------------------------
				lfsr	0,verifying_msg-sstrings	;announce verify step
				rcall	cr_printf

;Compare the EPROM to the buffer, and report errors to the console
				rcall	restore_params			;recover original starting address & byte count					

				rcall	eb_compare				;compare EPROM to buffer

;Bump either the successful program or the unsucessful program counter
				movlw	PROGCNT	
				btfsc	KERN_FLAGS,ERR_FLAG		;any errors?
				movlw	FAILCNT
				movwf	R1,0					;temp save

				call	K_RDEEPROM				;load counter low byte
												;..and set EEADR to PROGCNT or FAILCNT
				addlw	0x01					;bump
				movwf	R2,0					;temp save

				call	K_WREEPROM				;write W at address EEADR

				movf	R2,F,0					;need to bump high byte?
				bnz		ep_bumpdone

				incf	R1,W,0					;point to high byte
				call	K_RDEEPROM				;load counter low byte
												;..and set EEADR to PROGCNT+1 or FAILCNT+1
				addlw	0x01					;bump
				call	K_WREEPROM				;write W at address EEADR

ep_bumpdone:	return							;to main
				
;******=========================================================================*
;* ES * EPROM Checksum Command
;******
; Compute and report the low byte of the sum of the entire EPROM's data
; On Entry:
;   ADDRESSH:ADDRESSL = starting buffer address & EPROM address
;   COUNTH:COUNTL = byte count of region to checksum
;   FULL_FLAG is cleared unless COUNTH:COUNTL = 8K
;   ERR_FLAG is cleared
; On Exit:
;   EPROM is powered off
;*==============================================================================*
cmd_cksum:		bsf		FLAGS1,B_STATE,0		;indicate checksum pass

;Fall into cmd_read to recycle code

;******=========================================================================*
;* ER * EPROM Read Command
;******
; EPROM Checksum just computes and reports the low byte of the sum of the entire
;  EPROM's data. EPROM Read also writes the EPROM data into the buffer.
; On Entry:
;   B_STATE cleared for read command, set for checksum command
;   ADDRESSH:ADDRESSL = starting EPROM address
;   (ADDRESSH+BUF_OFFSET):ADDRESSL = starting buffer address
;   COUNTH:COUNTL = byte count of region to checksum
;   FULL_FLAG is cleared unless COUNTH:COUNTL = 8K
;   ERR_FLAG is cleared
; On Exit:
;   EPROM is powered off
;*==============================================================================*
cmd_read:		rcall	epower_read				;power up the EPROM
				rcall	limit_count				;limit the count to a full EPROM, max
				clrf	CHECKSUM,0				;accumulate checksum here

;Tell the user about the buffer address offset for read-buffer only
				btfss	FLAGS1,B_STATE,0		;read or checksum?
				rcall	rep_ba_offset			;read: report buffer address offset

;Tell the user about the data invert state if it is on
				rcall	rep_dion				;only mention invert state if it is set

;Read EPROM, and add up the data for the checksum
esum_rd_loop:		rcall	eread_byte			;read EPROM at ADDRESSH:ADDRESSL,
												;..result in W & R2
					addwf	CHECKSUM,F,0

					movf	BUF_OFFSET,W,0		;buffer address offset
					addwf	ADDRESSH,F,0		;compute buffer address
	
					movf	R2,W,0				;recover EPROM data

					btfss	FLAGS1,B_STATE,0	;read or checksum?
					call	K_WRSRAM			;read: write EPROM data to buffer
												;..at ADDRESSH:ADDRESSL

					movf	BUF_OFFSET,W,0		;buffer address offset
					subwf	ADDRESSH,F,0		;restore EPROM address

					rcall	next_a_count		;bump address pointer, decrement & test count
					bnz		esum_rd_loop

;Print results
				rcall	eprange					;print 'EPROM, then 'range ' if needed

				lfsr	0,read_msg-sstrings		;'read into buffer. '
				btfss	FLAGS1,B_STATE,0		;EPROM read?
				rcall	printf					;print message

				lfsr	0,csum_msg-sstrings		;'checksum: '
				rcall	printf					;print message
				movf	CHECKSUM,W,0			;print checksum
				call	K_PRINTHEX2					

				bra		epower_off				;power EPROM off and return to main

;******=========================================================================*
;* US *  Upload as S-Records Command
;******
; Transmit COUNTH:COUNTL bytes of EPROM buffer contents, starting at address
; ADDRESSH:ADDRESSL, in one of two formats:
; On Entry:
;	ADDRESSH:ADDRESSL = starting address
;	COUNTH:COUNTL = byte count. 0000 means do the whole buffer.
;   FULL_FLAG means limit count to the size of the EPROM
;*==============================================================================*
cmd_uploads:	bcf		FLAGS1,INTEL_STATE,0	;Set for Motorola
				bra		upload_buf

;******=========================================================================*
;* UI *  Upload as Intel Hex Records Command
;******
; Transmit COUNTH:COUNTL bytes of EPROM buffer contents, starting at address
; ADDRESSH:ADDRESSL, in one of two formats:
; On Entry:
;	ADDRESSH:ADDRESSL = starting address
;	COUNTH:COUNTL = byte count. 0000 means do the whole buffer.
;   FULL_FLAG means limit count to the size of the EPROM
;*==============================================================================*
cmd_uploadi:	bsf		FLAGS1,INTEL_STATE,0	;Set for intel

;Fall into upload_buf

;***********--------------------------------------------------------------------*
;* UI & US * Common Upload Routine
;***********
; Transmit COUNTH:COUNTL bytes of EPROM buffer contents, starting at address
; ADDRESSH:ADDRESSL, in a Hex format
; On entry:   
;	INTEL_STATE = 1 for Intel Hex, 0 for S-record
;	ADDRESSH:ADDRESSL = starting address
;	COUNTH:COUNTL = byte count. 0000 means do the whole buffer.
;   FULL_FLAG means limit count to the size of the EPROM
;*------------------------------------------------------------------------------*
;If no count provided, then limit COUNTH:COUNTL to the maximum size of the
;currently-selected EPROM
upload_buf:		btfsc	FLAGS1,FULL_FLAG,0			;default?
				rcall	limit_count					;y: set it to the EPROM size

;Test for COUNTH:COUNTL exceeding the end of the buffer, and adjust to the end of the
;buffer if so. Note that COUNTH:COUNTL = 0 means do the whole buffer.
				movf	ADDRESSL,W,0				;compute max allowed byte count
				sublw	low(SRAM_SIZE)				;W := low(SRAM_SIZE) - ADDRESSL
				movwf	R0,0
				movlw	high(SRAM_SIZE)
				subfwb	ADDRESSH,W,0				;W := high(SRAM_SIZE) - ADDRESSH - borrow
				movwf	R1,0						;R1:R0 = max byte count

				bcf		COUNTH,0x07,0				;guarantee no carries - certainly not >32K
				movf	COUNTH,W,0					;whole buffer?
				iorwf	COUNTL,W,0
				movlw	0x4F						;much more than the whole buffer
				btfsc	STATUS,Z,0
				movwf	COUNTH,0					;this will force a whole buffer upload

				movf	COUNTL,W,0					;compare COUNTH:COUNTL to the max allowed count
				subwf	R0,W,0						;W := R0 - COUNTL
				movf	COUNTH,W,0
				subwfb	R1,W,0						;W := R1 - COUNTH - borrow

				bc		ul_countok	

				movff	R1,COUNTH					;adjust count to the end of the buffer
				movff	R0,COUNTL								
ul_countok:

;Compute byte count for this record: the smaller of the remaining count and 16
upload_loop:		movlw	0x10					;16 bytes per line
					movwf	R3,0					;R3 is loop count

					movf	COUNTH,F,0				;more than 256 left?
					bnz		full_record

					movf	COUNTL,W,0				;at least 16 left?
					andlw	0x0F0
					btfsc	STATUS,Z,0
					movff	COUNTL,R3
full_record:

					clrf	CHECKSUM,0				;start row checksum at 00

;Print CR LF, and either ':', or 'S1'
					lfsr	0,mot_start_rec-sstrings
					btfsc	FLAGS1,INTEL_STATE,0
					lfsr	0,int_start_rec-sstrings
					rcall	cr_printf

;Send byte count for this record				
					movlw	0x00					;adjust byte count for S-records
					btfss	FLAGS1,INTEL_STATE,0
					movlw	0x03					;S-record: account for address, checksum bytes
					addwf	R3,W,0					;bytes per record

					rcall	print_hex_csum			;compute checksum along the way

;Compute & send high byte of this record's address
					movf	ADDRESSH,W,0
					addwf	ADR_OFFSET,W,0			;add on address offset
					rcall	print_hex_csum			;send & accumulate checksum along the way

;Send low byte of this record's address
					movf	ADDRESSL,W,0			;low byte of address
					rcall	print_hex_csum			;accumulate checksum along the way

;If this is an Intel Hex record, send 00 record type
					movlw	0x00
					btfsc	FLAGS1,INTEL_STATE,0
					rcall	print_hex_csum

;Loop to send R3 bytes of data from ADDRESSH:ADDRESSL
up_byte_loop:			call	K_RDSRAM			;get a byte from the buffer
						rcall	print_hex_csum		;send data byte, accumulate checksum
						rcall	next_a_count		;bump ADDRESSH:ADDRESSL
													;..and decrement COUNTH:COUNTL
						decfsz	R3,F,0
						bra		up_byte_loop

;Compute this record's checksum: 
;  for Intel hex, checksum = 2's compliment of the sum
;  for Motorola s-records, checksum = 1's compliment of the sum
					comf	CHECKSUM,F,0			;compliment sum of bytes
					btfsc	FLAGS1,INTEL_STATE,0	;add one if Intel hex
					incf	CHECKSUM,F,0

					movf	CHECKSUM,W,0			;get the final checksum
					rcall	print_hex_csum			;set CHECKSUM up for next record too

;See if we have more data to send

					movf	COUNTH,W,0
					iorwf	COUNTL,W,0
					bnz		upload_loop	

;All done sending data. Send appropriate eof record and quit
				lfsr	0,int_end_rec-sstrings
				btfss	FLAGS1,INTEL_STATE,0
				lfsr	0,mot_end_rec-sstrings
				bra		cr_printf						;print, return to main

;################################################################################
;#							Diagnostic Commands									#
;################################################################################
;********=======================================================================*
;* AVPP * Adjust Vpp Voltage Command
;********
; Selects one of the possible Vpp voltages for adjustment form the Vpp test point
; On Entry:
;	DFLT_FLAG = 1 if the user did not provide an address offset value.
;   ADDRESSL:
;     0 = (off) sets Pulse Width to 0, which will produce 12V.
;     1 = 12.75V
;     2 = 13.15V
;     3 = 21V
;     4 = 25.1V (Note that 26V is just 1 diode drop above the 25V seting.)
;*==============================================================================*
cmd_avpp:	movlw	0x05					;max legal value is 4
			rcall	tcmd_setup				;test, become busy

			movf	ADDRESSL,W,0			;go set the value according to W
			rcall	set_vpp
			rcall	rep_vpp					;report the setting

			movf	ADDRESSL,W,0			;is switcher off?
			bz		avpp_done				;y: no further messages

			lfsr	0,testvpp_msg-sstrings	;"(Measure at TP3)"
			rcall	printf

;Determine if Vpp is on pin 18 or not. If so, remind the user that
;Vpp will be 0.8V higher on pin 18 (for 2708-type EPROMs)
			movlw	ET_PGMVPP				;look up Vpp pin for this EPROM
			call	etype_getval
			andlw	0x07					;just the Vpp bits
			movwf	R0,0
			decfsz	R0,F,0					;0x01 means pin 18

avpp_done:	return							;no final message for pins 19-21

			lfsr	0,vp18_msg-sstrings		;note about pin 18
			bra		cr_printf				;print & return to main

;******=========================================================================*
;* RD * Read EPROM Data Pins Command
;******
; simply reads the eprom data pins and reports the results - 
; this is a test command, not meant for reading the EPROM in general.
; EPROM power, address, etc. are not set up prior to a read.
; On Exit:
;   The data buffer direction is set for reading
;   The data buffer outputs are disabled
;   Port D is tristated
;*==============================================================================*
cmd_rdata:		setf	TRISD					;tristate port D
				bcf		LATB,DBUFDIR,0			;set buffer output direction 
				bcf		LATB,DBUFOEn,0			;enable data buffer output		
				lfsr	0,dataread_msg-sstrings	;print banner
				rcall	cr_printf

;Read the EPROM data pins data and report result
				movf	PORTD,W,0				;read the EPROM data pins

				bsf		LATB,DBUFOEn,0			;disable data buffer outputs
				goto	K_PRINTHEX2				;print it, return to main

;********=======================================================================*
;* TVBD * Test Vbb and Vdd pins
;********
;  TC <0-1> sets the Vbb and Vdd pins to off or on for this EPROM type
; On Entry:
;   ETYPE = current EPROM type
;   ADDRESSL = {0-1} from the user
;*==============================================================================*
cmd_vbbvdd:		movlw	0x02					;User input limit+1
				rcall	tcmd_setup				;get user input into W & R4, test for valid,
												;light busy light

				rcall	vbbvdd2pins				;set the Vbb and Vdd pins according to W
												;returns R1=0 of no Vbb or Vdd pins
												;returns R1=1 if Vdd is 12V on pin 19
												;and Vbb is -5V on pin 21

				lfsr	0,novbbvdd_msg-sstrings	;"\rVbb and Vdd do not exist"
				movf	R1,F,0
				btfsc	STATUS,Z,0
				bra		printf					;print, return to main

				lfsr	0,vbb_msg-sstrings		;"\r-5V Vbb pin 21 and +12V Vdd"
	
				movlw	.19-.17					;Vdd is on pin 19
				movwf	R1,0
				clrf	R5,0					;these pins are on/off
				bra		print_pin				;print pin number from R1, on/off from R4			

;********=======================================================================*
;* TVCC * Test Vcc pin
;********
;  TC <0-2> sets the Vcc pin to {0V,+5V,Programming Vcc for this EPROM type}
; On Entry:
;   ETYPE = current EPROM type
;   ADDRESSL = {0-2} from the user
;*==============================================================================*
cmd_tvcc:	movlw	0x03				;User input limit+1
			rcall	tcmd_setup			;get user input into W & R4, test for valid,
										;light busy light

			rcall	vcc2pin				;set the Vcc pin according to W
										;returns R1=pin number-17 or 0 for none
			lfsr	0,tvcc_msg-sstrings	;"Vcc"
			movlw	.24-.17				;set up for print_pin
			movwf	R1,0				;Vcc is always pin 24	
			clrf	R5,0				;this pin is on/off
			bra		print_pin			;print the pin number and state	from R1 & R4

;********=======================================================================*
;* TPGM * Test PGM pin
;********
;  TC <0/1> sets the PGM pin low/high
; On Entry:
;   ETYPE = current EPROM type
;   ADDRESSL = {0-1} from the user
;*==============================================================================*
cmd_tpgm:	movlw	0x02					;User input limit+1
			rcall	tcmd_setup				;get user input int W & R4, test for valid,
											;light busy light

			call	pgm2pin					;set the PGM pin according to W
											;returns R1=pin number-17 or 0 for none

			lfsr	0,pgm_msg-sstrings+1	;"PGM" (skipping slash)
			setf	R5,0					;this pin is active/inactive
			bra		print_pin				;print pin number from R1, on/off from R4			

;*******========================================================================*
;* TCS * Test CS (or (-CS) pin
;*******
; TCS <0-1> sets the -CS pin to {inactive,active}
; Note that active may be high or low, depending on the EPROM
; Note that CS may also be the PGM or Vpp pin
; On Entry:
;   ETYPE = current EPROM type
;   ADDRESSL = {0-2} from the user
;*==============================================================================*
cmd_tcs:	movlw	0x02				;User input limit+1
			rcall	tcmd_setup			;get user input into W & R4, test for valid,
										;light busy light

			call	cs2pin				;set the -CS pin according to R3
										;returns R1=pin number-17 or 0 for none

			lfsr	0,cs_msg-sstrings	;"CS"	
			setf	R5,0				;this pin is active/inactive
			bra		print_pin			;print pin number and state from R1 & R4

;*******========================================================================*
;* THI * Test stuck-high pin
;*******
; THI <0-1> sets the stuck-high pin to {low/high}
; On Entry:
;   ETYPE = current EPROM type
;   ADDRESSL = {0-1} from the user
;*==============================================================================*
cmd_thi:	movlw	0x02					;User input limit+1
			rcall	tcmd_setup				;get user input int W & R4, test for valid,
											;light busy light

			call	hi2pin					;set correct pin high or low as requested
				
			lfsr	0,stuckhi_msg-sstrings	;"Stuck-high"	
			clrf	R5,0					;this pin is off/on
			bra		print_pin				;print pin number and state from R1 & R4

;********=======================================================================*
;* TVPP * Test Vpp pin
;********
;  TV <0-3> sets the Vpp pin:
;       0 = 0V (off)
;       1 = read mode level
;       2 = programming mode inactive level
;       3 = programming active level
;  Note that Vpp may be from the onboard supply or the external negative supply.
; On Entry:
;   ETYPE = current EPROM type
;   ADDRESSL = {0-2} from the user
;*==============================================================================*
cmd_tvpp:	movlw	0x04						;User input limit+1
			rcall	tcmd_setup					;get user input int W & R4, test for valid,
												;light busy light

			rcall	vpp2pin						;Set Vpp pin according to W
			movff	R1,R5						;returns R1=pin number-17 or 0 for none

			lfsr	0,tvpp_msg-sstrings			;"Vpp pin "
			rcall	cr_printf

			lfsr	0,nonexist_msg-sstrings		;" does not exist"
			movf	R5,F,0						;test for 0 = no Vpp pin
			bz		tvpp_print

			rcall	eset_vpp					;turn on the switching power supply
												;for this EPROM's Vpp voltage

			movlw	.17							;compute pin number
			addwf	R5,W,0
			movwf	R0,0						;set up for K_PRINTDEC
			clrf	R1,0
			call	K_PRINTDEC

			lfsr	0,pwroff_msg-sstrings		;" powered off"
			decf	R4,F,0
			bn		tvpp_print

			lfsr	0,rmode_msg-sstrings		;" read mode"
			bz		tvpp_print

			lfsr	0,pmode_msg-sstrings		; programming mode "
			rcall	printf

			lfsr	0,pinactive_msg-sstrings	;"inactive state"
			dcfsnz	R4,F,0

tvpp_print:	bra		printf						;print, return to main

;Figure out and print the programming voltage
			movlw	ET_VPPSETUP
			call	etype_getval				;get the programming voltage in W and R0
			movff	R5,R2						;pin number minus 17 (since pin 18
												;..has higher voltages)

			goto	pvpp_voltage				;print the voltage & return to main

;*******========================================================================*
;* TOE * Test -OE pin
;*******
;  TC <0-2> sets the -OE pin to (inactive,active,programming level)
; On Entry:
;   ETYPE = current EPROM type
;   ADDRESSL = {0-2} from the user
;*==============================================================================*
cmd_toe:	movlw	0x03				;User input limit+1
			rcall	tcmd_setup			;get user input int W & R4, test for valid,
										;light busy light

			rcall	oen2pin				;Set -OE pin according to W
										;returns R1=pin number-17 or 0 for none

			lfsr	0,oe_msg-sstrings	;"-OE"	
			setf	R5,0				;this pin is active/inactive
			bra		print_pin			;print the pin number and state from R1 & R4

;*******========================================================================*
;* TAS * Test -AS pin
;*******
;  TC <0-1> sets the -AS pin to (inactive,active)
; On Entry:
;   ETYPE = current EPROM type
;   ADDRESSL = {0-1} from the user
;*==============================================================================*
cmd_tas:	movlw	0x02				;User input limit+1
			rcall	tcmd_setup			;get user input int W & R4, test for valid,
										;light busy light

			rcall	asn2pin				;Set -AS pin according to W
										;returns R1=pin number-17 or 0 for none
			lfsr	0,as_msg-sstrings	;"-AS"	
			setf	R5,0				;this pin is active/inactive

;Fall into print_pin to print the pin number and state according to R1 & R4

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Common message routine for test routines
;  R1=0:  Prints "<xxx> pin does not exist"
;  R1<>0: Prints "<xxx> pin nn <off/on/at programming level>
; On Entry:
;   FSR0 points to initial message identifying the pin
;   R1 = pin number nn - 17, 0 means does not exist
;   R4 = 0 for off, 1 for on, 2 for at programming level
;   R5 = 0 if this is an on/off pin, not 0 it is an active/inactive pin
; Trashes W,R0,R1,R2,PRODH,PRODL,FSR0
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
print_pin:		rcall	cr_printf

				lfsr	0,pin_msg-sstrings			;" pin "
				rcall	printf

				lfsr	0,nonexist_msg-sstrings		;" does not exist"
				movf	R1,F,0						;test for 0
				btfsc	STATUS,Z,0
				bra		printf						;done if no such pin - print, return to main

				movlw	.17							;compute pin number
				addwf	R1,W,0
				movwf	R0,0						;set up for K_PRINTDEC
				clrf	R1,0
				call	K_PRINTDEC

				lfsr	0,apl_msg-sstrings			;" at programming level"
				btfsc	R4,0x01,0					;R3 = 2?
				bra		printf						;y: print this message, return to main

				movf	R5,F,0						;is this an on/off pin?
				bz		print_onoff					;y: print on or off		

				movlw	' '
				call	K_CONOUT

				lfsr	0,pactive_msg-sstrings		;"active state"
				btfss	R4,0x00,0
				lfsr	0,pinactive_msg-sstrings	;"inactive state"
				bra		printf						;print and then done with command, return to main

print_onoff:	lfsr	0,on_msg-sstrings			;' on'
				btfss	R4,0x00,0
				lfsr	0,off_msg-sstrings			;' off'
				bra		printf						;done with command, return to main

;******=========================================================================*
;* WD * Write EPROM Data Pins Command
;******
; Writes first parameter (in ADDRESSL) to the EPROM data pins (For
; testing purposes.)
; On Entry:
;   ADDRESSL = (low byte of) user input. (The high byte is just ignored.)
; On Exit:
;   Port D is set up for output
;*==============================================================================*
cmd_wdata:		movlw	0xFF				;all values legal
				rcall	tcmd_setup			;get user input, turn Busy light on

				movwf	PORTD,0				;write data to PIC pins
				bsf		LATB,DBUFDIR,0		;set external data buffer direction 
				clrf	TRISD,0				;enable PIC output
				bcf		LATB,DBUFOEn,0		;enable external data buffer output		

				return						;..to main

;******=========================================================================*
;* WA * Write EPROM Address Pins Command
;******
; Writes first parameter (in ADDRESSH:ADDRESS) to the EPROM address pins,
; based on the selected EPROM type. (For testing purposes.)
; On Entry:
;   ADDRESSH:ADDRESSL = user input address
;*==============================================================================*
cmd_waddress:		bra		address2pins		;..and return to main

;*********======================================================================*
;* TPROG * Test Programming Command
;*********
; Loops writing the buffer to the EPROM until the user says to stop. This is for
; testing purposes only.
; On Entry:
;   ADDRESSH:ADDRESSL = 0
;*==============================================================================*
cmd_tprog:		rcall	epower_prog				;power everything up for programming

				lfsr	0,pulsing_msg-sstrings	;"Pulsing  "
				rcall	cr_printf

				rcall	wait_txempty			;print message before blocking ints

				bsf		FLAGS1,TEST				;tell ewrite_byte that we are testing

tp_loop:			bcf		INTCON,GIE,0		;no interrupts during pulse
					movff	SMART1,R6			;get 1st pass programming mode
					rcall	ewrite_byte			;Write a byte
					movf	SMART2,W,0			;get & test 2nd pass programming mode
					movwf	R6,0
					btfss	STATUS,Z,0			;there may not be a 2nd pass.
					rcall	ewrite_byte			;Write a byte
					bsf		INTCON,GIE,0		;reenable interrupts

					rcall	next_a_count		;bump address

					rcall	pacify				;print pacifier if it's time

					call	K_CHKABORT			;user abort?
					bz		tp_loop				;Z if no user input

				bra		epower_off				;everything off & return

;*********======================================================================*
;* TREAD * Test Reading Command
;*********
; Loops reading the EPROM into the bit bucket until the user says to stop. This
; is for testing purposes only.
; On Entry:
;   ADDRESSH:ADDRESSL = 0
;*==============================================================================*
cmd_tread:		rcall	epower_read				;power up the EPROM

				lfsr	0,reading_msg-sstrings	;"Reading  "
				rcall	cr_printf

;Read EPROM until user aborts
tread_loop:			rcall	eread_byte			;read EPROM at ADDRESSH:ADDRESSL,
												;..result in W & R2

					rcall	pacify				;print pacifier if it's time

					rcall	next_a_count		;bump address pointer

					call	K_CHKABORT			;user abort?
					bz		tread_loop			;Z if no user input

				bra		epower_off				;power EPROM off & return

;################################################################################
;#								Subroutines										#
;################################################################################

;*==============================================================================*
; Subroutine to test for legal 0/1 input
; On Entry:
;   ADDRESSH:ADDRESSL = user input
; exit to pop_cmderr if input
;*==============================================================================*
test_01:		tstfsz	ADDRESSH,0				;no high byte allowed
				bra		error_01

				movf	ADDRESSL,W,0			;low byte of user input
				andlw	0xFE					;anything but 0 or 1?
				bnz		error_01
				return

error_01:		bra		pop_cmderr

;*==============================================================================*
; Subroutine to write one pass through the  EPROM
; Check for control-C after each byte, abort if control-C found
; Print a pacifier as needed
; On Entry:
;   R6 = smart programming mode
;   ADDRESSH_SAVE:ADDRESSL_SAVE=EPROM address
;   (ADDRESSH_SAVE+BUF_OFFSET):ADDRESSL_SAVE=buffer address
;   COUNTH_SAVE,COUNTL_SAVE = number of bytes to program
;   ETYPE has the EPROM type
;   DINV_STATE = FF if the EPROM data is to be inverted
;   EPROM is powered up and ready for writing. If this EPROM requires a
;     pulsed Vpp then Vpp is at the programming-low (inactive) level
;   PGM is at its inactive state
;   EPROM is chip-selected and write-enabled.
;   PROGPULSE, PMODE1 & PMODE2 are valid for the current EPROM
;
; On Exit:
;   EPROM is still powered up for programming
;   trashes W,R1,R2,R3,R4,R5,FSR0, ADDRESSH:ADDRESSL, COUNTH:COUNTL
;   Interrupts are enabled
;
; Numbers in parenthesis are time in uS
; Trashes W,R0,R1,R2,R3,R4,R5
;*==============================================================================*
ewrite_range:	rcall	restore_params		;recover original starting address & byte count

;Loop once through the entire EPROM range. Check for user abort before each byte.
;The pacifier is controlled by a hardware timer - check if it's time for the next
;pacifier step.
ewrite_loop:		rcall	flush_abort		;(.5 + 5.5)abort on ^C or ESC
					rcall	pacify			;(1+.5)print pacifier if it's time

;Write the EPROM byte from ADDRESSH:ADDRESSL
					bcf		INTCON,GIE,0	;(.25)global interrupt masked while we write
					rcall	ewrite_byte		;write a byte to the EPROM (int's are masked)
					bsf		INTCON,GIE,0	;(.25)let Rx and Tx interrupts in

;Move on to next location and test for done
					rcall	next_a_count	;(.5+2.5)bump address, dec and test count
					bnz		ewrite_loop		;(0.50)

				return

;*==============================================================================*
; Subroutine to check the pacifier timer, and print the next pacifier propellor
; step if it's time to do so.
; On Entry:
;  Timer 0 (TMR0H) has been set up such that its high byte increments at 61 Hz.
;  Bit 5 toggles at about 1 Hz
;
; On Exit:
;   Interrupts are masked
; Trashes W,R0
; 1 uS, normally (when the pacifier is not updated)
;*==============================================================================*
pacify:			movf	TMR0L,W,0		;(.25)must read TMR0L to read TMR0H
				btfss	TMR0H,0x05,0	;(.25)time for a pacifier?
				return					;(.5)

;Restart the pacifier timer
				clrf	TMR0H			;Restart timer. Must clear high first
				clrf	TMR0L

;Print the next pacifier propeller step
				movlw	BS				;back up on the screen
				call	K_CONOUT

				incf	PACINDEX,F,0	;bump pointer to next pacifier character
				movlw	0x03			;modula 4
				andwf	PACINDEX,F,0

				movff	PACINDEX,R0

				movlw	'/'				;4th pacifier chr
				dcfsnz	R0,F,0
				movlw	'-'				;1st pacifier chr
				dcfsnz	R0,F,0
				movlw	0x5C			;2nd pacifier chr is a slash '\'
				dcfsnz	R0,F,0
				movlw	'|'				;3rd pacifier chr

				call	K_CONOUT		;print the pacifier
				bra		wait_txempty	;finish printing pacifier, and return

;*==============================================================================*
; Subroutine to write one byte to the EPROM
; On Entry:
;   ETYPE has the EPROM type
;   R6=programming mode:
;      00 means just program once.
;      80 means program until match
;      <6:4> Phase 1 type
;        0: program until match, then program n times
;        1: program until match, then 1 pulse that is n*ET_PROGPULSE long
;        2: program until match (P), then nP times
;        3: program until match, then 1 pulse that is n*P*ET_PROGPULSE long
;        4: Program n times
;        5: Program once, pulse time = n*ET_PROGPULSE
;      <3> set means program data=FF (EEPROM erase)
;      <2:0> Phase n
;   ADDRESSH:ADDRESSL = EPROM address
;   (ADDRESSH+BUF_OFFSET):ADDRESSL = data buffer address
;   SRAM(ADDRESSH:ADDRESSL) = data to be written
;   DINV_STATE = FF if the EPROM data is to be inverted
;   EPROM is powered up and ready for writing. If this EPROM requires a
;     pulsed Vpp then Vpp is at the programming-low (inactive) level
;   PGM signal is at its inactive state (high or low depending on its polarity)
;   EPROM is chip-selected and write-enabled
;   RAM variables are all valid for the current EPROM
;   TEST is set if this subroutine was called from the TPROG command. In this
;     case (and if the selected EPROM type does Smart programming) then we
;     behave as though the byte verified on the last possible try.
; Trashes W,R0,R1,R2,R3,R4,R5
;*==============================================================================*

;Put ADDRESSH:ADDRESSL onto the EPROM's address pins. This must be done before writing
;the data to the EPROM's data pins, because writing to the external low address latch
;requires PIC Port D, which is also the data output port.
ewrite_byte:	rcall	address2pins		;(about 53.5)

;Disable, and then enable the address strobe pin, to strobe the new address into the EPROM.
;Note that pin 20 (which has a 0.047 uF cap) may be the -AS pin, and therefore will be slow.
;Get the buffer data (inverting as required) in the middle, to give the -AS pin time to
;transition.
				movlw	0x00				;(.25)high-going edge of -AS
				rcall	asn2pin				;(11.5+.5)(just returns if there is no -AS pin)

				movlw	DINV_STATE			;(.25)is the data supposed to get inverted?
				call	K_RDEEPROM			;(1.5+.5)
				movwf	R4,0				;(.25) temp save inverter

				movf	BUF_OFFSET,W,0		;(.25)buffer address offset
				addwf	ADDRESSH,F,0		;(.25)compute buffer address

				call	K_RDSRAM			;(about 16) read SRAM at ADDRESSH:ADDRESSL, trash R0
				xorwf	R4,F,0				;(.5) invert buffer data if it needs it, save in R4

				btfsc	R6,0x03,0			;(.5) EEPROM erase?
				setf	R4,0				;y: write FF instead of buffer data

				movf	BUF_OFFSET,W,0		;(.25)buffer address offset
				subwf	ADDRESSH,F,0		;(.25)restore EPROM address

				movlw	0x01				;(.25)low-going (active) edge -AS, leave pin active
				rcall	asn2pin				;(11.5+.5)(just returns if there is no -AS pin)

;Actually write the data in R4 to the EPROM data pins
				bsf		LATB,DBUFDIR,0		;(.25)point the data buffer toward the EPROM
				movff	R4,LATD				;(.5)put the data on the PIC pins
				clrf	TRISD,0				;(.25)enable the PIC outputs
				bcf		LATB,DBUFOEn,0		;(.25)enable the data buffer outputs

;-------------------------------------------------------------------------
; Program the byte using ((PPULSEH:PPULSEL x 10) + 30) uS pulse(s)
; Note that PULSEH:PPULSEL has already been reduced by 3 to account
; for the 30 uS overhead of the pulse_byte loop.
; On Entry:
;   R6=programming mode:
;      00 means just program once.
;      80 means program until match
;      <6:4> Phase 1 type
;        0: program until match, then program n times
;        1: program until match, then 1 pulse that is n*ET_PROGPULSE long
;        2: program until match (P), then nP times
;        3: program until match, then 1 pulse that is n*P*ET_PROGPULSE long
;        4: Program n times
;        5: Program once, pulse time = n*ET_PROGPULSE
;      <3> set means program data=FF (EEPROM erase)
;      <2:0> Phase n
;-------------------------------------------------------------------------
				movf	R6,W,0				;(.25)Smart programming of some sort?
				btfsc	STATUS,Z,0			;(.25)any smart mode enabled?
				bra		pulse_byte			;(.5)n: write it once and return

				andlw	0x07				;get n
				movwf	R5,0

				btfsc	R6,0x06,0			;write until match first?
				bra		smart_n				;n: no write-until-match phase

;----------------------------------------------------------------
; Single-byte Smart programming loop
; Write the EPROM byte until the EPROM data matches the written
; data. Abort the entire write operation if the data does not
; match after PROGREPS tries.
;----------------------------------------------------------------
				movff	PROGREPS,R4				;R4 counts programming tries

;Write the byte once
retry_pulse:		rcall	pulse_byte			;(18.5 uS min before pulse) write the byte once

;Set up to read the EPROM back
					setf	TRISD,0				;(.25)tristate the PIC's data buffers
					bcf		LATB,DBUFDIR,0		;(.25)point the external data buffer toward PIC

;If the CS pin is also the Vpp pin, then set the CS pin active (turning Vpp off)
;so that the EPROM may be read
					movlw	0x01
					btfsc	SHARED,CSVPP,0
					rcall	cs2pin				;set CS active, potentially turning Vpp off

;Enable the EPROM output
					movlw	0x01				;active state for -OE so we can read
					rcall	oen2pin				;(1.5 uS from pin-write through return)

					movlw	.2					;(.25)stall because -OE signal is slow
					call	K_STALL1U			;(3)

					movlw	0x02				;(.25)prepare to put the EPROM back in program mode

;Sample the EPROM data 5 uS after the -OE signal was asserted.
					movff	PORTD,R2			;(.5)read the EPROM data, temp save in R2

;Turn off the EPROM's -OE signal. If -OE is also Vpp, then it will be put back to its programming-inactive
;level. Otherwise, it will be set high (the -OE inactive level).
					rcall	oen2pin				;(1.5 from off)disable EPROM's output buffer. Trashes W,R0,R1,R3

;If the CS pin is also the Vpp pin, then turn Vpp back on. If Vpp is the pulsed signal,
;then set it to its programming-inactive state. Otherwise, turn it on.
					movlw	0x02				;(.25)Vpp programming-inactive state (this order for delay)
					btfsc	FLAGS2,PMODE1,0		;(.5)0 means Vpp pulses (and should now be inactive)
					movlw	0x03				;Vpp programming-active state

					btfsc	SHARED,CSVPP,0		;(.5)
					rcall	vpp2pin				;(.5+.5 from pin write)turn Vpp back on

					movlw	.1					;(.25)stall because -OE signal and Vpp are slow
					call	K_STALL1U			;(2)

;Verify written data. Here, we only look for bits that should be 0, but are still 1, since an
;erased EPROM is all FF's. (Other errors will be caught during verify.)
					movf	R2,W,0				;(.25)W = EPROM data
					movwf	R3,0				;(.25)R3 = EPROM data, in place for rep_mismatch
					movff	LATD,R2				;(.5)R2 = written data (buffer data), in place for rep_mismatch
					xorwf	R2,W,0				;(.25)compare, non-zero bits are mismatches
					andwf	R3,W,0				;(.25)only care about mismatched bits that are still 1 in EPROM
												;Z set if all bits that should be 0 are 0
;Turn data buffer 6 uS min after -OE signal was removed
					bsf		LATB,DBUFDIR,0		;(.25)point the external data buffer toward the EPROM
					clrf	TRISD,0				;(.25)un-tristate the PIC's data buffers

;Move on to overprogramming if the data matches. Abort if too many tries. Otherweise, try again.
					btfss	FLAGS1,TEST,0		;(.25)If this is TPROG testing, then always fail the match
					bz		overprog			;(.25)done with Smart loop if match

					decf	R4,F,0				;(.25)enough retries yet?
					bnz		retry_pulse			;(.5)n: try again

				btfsc	FLAGS1,TEST,0			;is this TPROG testing?
				bra		overprog				;If so, always pass on the last try

;----------------------------------------------------
; Smart programming failure. Report failure and quit. 
;----------------------------------------------------
				bsf		INTCON,GIE,0			;reenable interrupts for rep_mismatch
				rcall	rep_mismatch			;report failing address
				lfsr	0,sfail_msg-sstrings	;"Smart programming failed"
				bra		prog_abort				;power off, return to main

;--------------------------------------------------------------------------------
; Smart programming success. Now for some over-programming.
; On Entry:
;   PROGREPS-(R4-1) = number of tries from previous step (P)
;   R6=programming mode:
;      <6:4> Phase 1 type
;        0: program until match, then program n times
;        1: program until match, then 1 pulse that is n*ET_PROGPULSE long
;        2: program until match (P), then nP times
;        3: program until match, then 1 pulse that is n*P*ET_PROGPULSE long
;        4: Program n times
;        5: Program once, pulse time = n*ET_PROGPULSE
;    R5 =  n
;--------------------------------------------------------------------------------
overprog:	movf	R5,F,0					;test n
			bz		smart_n0				;n=0? then done

;Calculate R5 = n or R5 = nP, depending on R6<5>
			decf	R4,W,0					;count final write
			subwf	PROGREPS,W,0			;calculate P

			mulwf	R5,0					;compute nP
			tstfsz	PRODH,0					;overflow?
			setf	PRODL,0					;y: limit to 255

			btfsc	R6,0x05,0				;nP type algorithm?
			movff	PRODL,R5				;y: replace n with nP

;Fall into smart_n

;---------------------------------------------------------------
; Overprogramming phase
; On Entry:
;   R5 = n or nP, depending on algorithm
;   R6<4> = 0 causes the byte to be written R5 times
;   R6<4> = 1 causes the byte to be written once, with a pulse
;             that is R5*(PPULSEH:PPULSEHL) long   
;---------------------------------------------------------------
;Fat overprogramming pulse or many pulses?
smart_n:		btfsc	R6,0x04,0
				bra		op_fatpulse

;----------------------------------------------------------
; Write the byte R5 times
;----------------------------------------------------------
smart_nloop:		rcall	pulse_byte
					decf	R5,F,0
					bnz		smart_nloop

smart_n0:		return

;-------------------------------------------------------------
; Write once with a pulse that is R5*(PPULSEH:PPULSEHL) long
;-------------------------------------------------------------
op_fatpulse:	movff	PPULSEL,R8			;temp save PPULSE value
				movff	PPULSEH,R9

;Calculate pulse width = R5*(PPULSEH:PPULSEHL), with a max value of 64K
				movf	R5,W,0				;compute pulse width
				mulwf	PPULSEL				;8X16 multiply
				movff	PRODL,PPULSEL		;low byte of result
				movff	PRODH,R4			;partial high byte

				mulwf	PPULSEH
				movf	PRODL,W,0			;add in cross-product
				addwf	R4,W,0
				movwf	PPULSEH				;result in PPULSEH:PPULSEHL
	
				bc		opf_ovflow			;should be no carry
				movf	PRODH,W				;should be nothing in PRODH
				bz		opf_novflow

opf_ovflow:		setf	PPULSEL				;overflow:make it max
				setf	PPULSEH
opf_novflow:

;Overprogram now
				rcall	pulse_byte			;program now with fat pulse

;Restore and return
				movff	R8,PPULSEL			;restore pulse width
				movff	R9,PPULSEH

				return

;*==============================================================================*
; Subroutine to write one EPROM byte once
; On Entry:
;   the address and data are already on the EPROM pins
;   The EPROM is powered up for programming and chip-selected as needed
;  PMODE bits tell programming pulse type
;  PPULSEH:PPULSEH + 30 = pulse width in units of 10 uS (0 means <1 uS pulse)
; (18.5 uS min before initiating strobe pulse)
; Trashes W,R0,R1,R3,EEADR,TBLPTR
;*==============================================================================*
pulse_byte:		btfsc	FLAGS2,FAST_PULSE,0	;(.5) less than 1 uS pulse?
				bra		fast_pgmpulse		;(always on PGM pin)

				btfss	FLAGS2,PMODE1,0		;(.5)Vpp or PGM?
				bra		vpp_pulse			;cleared means Vpp

				movlw	0x01				;(.25)PGM to active
				rcall	pgm2pin				;(.5)start the PGM pulse

;Delay for (PPULSEH:PPULSEL * 10) + 12 uS
				rcall	stall_ppulse		;(.5 + (PPULSEH:PPULSEL * 10) + 12.5 uS)

;End the PGM pulse
				nop							;(.25)stall to add up to 30 uS
				nop							;(.25)
				nop							;(.25)
				movlw	0x00				;(.25)inactive PGM
				rcall	pgm2pin				;(.5+15.5 min) end pulse
;Total overhead = 30 uS

;Fall into stall_between

;*==============================================================================*
; Subroutine to stall between pulses
; On Entry:
;   PPDELH:PPDELL = 0000 means no delay
;   PPDELH:PPDELL = 0001 means EEPROM-style data polling
;   PPDELH:PPDELL > 0001: stall for (PPDELH:PPDELL * 10) + 12.75 uS
;   LATD = data written to EPROM
; Trashes R1,R0
;*==============================================================================*
stall_between:	movf	PPDELL,W,0			;(25)
				iorwf	PPDELH,W,0			;(.25)
				bz		no_stall			;(.25) 0000 means no delay

				decf	PPDELL,W,0			;(.25)EEPROM wait?
				iorwf	PPDELH,W,0			;(.25)
				bz		eepoll				;(.25)y: go poll the EEPROM

				movlw	0x09				;(.5)stall to match stall_pulse's delay
				call	K_STALL1U			;(10)

				movff	PPDELH,R1			;(.25) stall between pulses
				movff	PPDELL,R0			;(.25)

;Fall into stall_r1r0

;--------------------------------------------------------------------------------
; Local subroutine to stall for ((R1:R0+1) * 10) + .25 uS
; Trashes R1,R0
;--------------------------------------------------------------------------------
stall_r1r0:			movlw	0x07			;(.25)
					call	K_STALL1U		;(8)
					nop						;(.25)
					movlw	0x01			;(.25)
					subwf	R0,F,0			;(.25)
					btfss	STATUS,C,0		;(.25)
					subwf	R1,F,0			;(.25)
					bc		stall_r1r0		;(.5/.25)

no_stall:		return						;(.5)

;--------------------------------------------------------------------------------
; EEPROM-style data polling, with timeout
; (Some EEPROMs invert data bit 7 until programming completes.)
; Trashes W,R0,R1,R2,R3,R4
;--------------------------------------------------------------------------------
eepoll:			setf	TRISD,0				;tristate the PIC's data buffers
				bcf		LATB,DBUFDIR,0		;point the external data buffer toward PIC

				clrf	R4,0				;set up timeout timer for about 9 mS

;Poll data (particularly bit 7) until it matches or timeout
poll_loop:			decf	R4,F,0			;(.25)timeout?
					bz		poll_timeout	;(.25)this loop takes 31.5 uS per pass					
					movlw	0x01			;(.25)active state for -OE so we can read
					rcall	oen2pin			;(14+.5) Trashes W,R0,R1,R3

					movlw	2				;(.25)Stall for slow -OE signal
					call	K_STALL1U		;(3)

					movlw	0x00			;(.25)prepare to disable output
					movff	PORTD,R2		;(.5)read the EPROM data, temp save in R2
					rcall	oen2pin			;(14+.5)disable EPROM's output buffer. Trashes W,R0,R1,R3

					movf	R2,W,0			;(.25)compare written data to EPROM data
					xorwf	LATD,W,0		;(.25)data bit 7 does not match until done
					bnz		poll_loop		;(.5)

				return

;Timeout waiting for EEPROM to complete the write
poll_timeout:	lfsr	0,pto_msg-sstrings	;"\rAbort: EEPROM polling timeout"
				bra		msg_abort

;--------------------------------------------------------------------------------
; Local subroutine to program the byte using a ((PPULSEH:PPULSEL x 10) + 30)
; uS Vpp pulse
; Note that PULSEH:PPULSEL has already been reduced by 3 to account
; for the 30 uS overhead of this loop.
;--------------------------------------------------------------------------------
;Start a Vpp pulse
vpp_pulse:		movlw	0x03				;Vpp on to programming level
				rcall	vpp2pin				;[(.5 + .5) since Vpp on]

;Delay for PPULSEH:PPULSEL * 10) + 12 uS
				rcall	stall_ppulse		;((PPULSEH:PPULSEL * 10) + 12 uS)

;Put Vpp at programming-inactive level
				movlw	0x02				;(.25) Vpp to programming-inactive level
				call	vpp2pin				;(16 + .5) turn off pulse & return

;Stall as required
				bra		stall_between		;(.5)

;Total overhead = 30 uS

;*==================================================================================*
; Subroutine to stall for (PPULSEH:PPULSEL * 10) + 12.5 uS (including the call to
; check Vpp for under-voltage as early as possible), and abort if too low.
; On Entry: 
;   The ADC is set up with Tad = 1 uS (so that a complete conversion will take 11 uS)
; Trashes R1,R0
;*==================================================================================*
stall_ppulse:	bsf		ADCON0,GO,0				;(.25)set GO to initiate an ADC conversion
				movff	PPULSEH,R1				;(.5)prepare for stall
				movff	PPULSEL,R0				;(.5)

;Check ADC to make sure Vpp is reasonable. If the EPROM is drawing too much current
;then the hardware current limiter will cause Vpp to drop, which we will detect here.

				movf	VSENSE_LOW,W,0			;(.25)prepare to check for low voltage

spp_adcwait:		btfsc	ADCON0,GO			;(.5)wait for conversion to complete
					bra		spp_adcwait			;(11 uS total time from GO was set)

;about 11.75 uS to here

				subwf	ADRESH,W,0				;(.25)get ADC result 2 uS after initiation
												;W:= measured voltage - lower  limit
				bc		stall_r1r0				;(.5)carry set if ok, so go stall

;--------------------------------------------------------------------------------
; ABORT: Vpp voltage is too low, probably because the EPROM is drawing too much
; current.
; Panic shutdown Vpp and quit.
;--------------------------------------------------------------------------------
				lfsr	0,lvpanic_msg-sstrings	;"Abort! Vpp is too low"

;Fall into prog_abort

;*==============================================================================*
;Programming abort: clean up and go to main
; On Entry:
;   FSR0 = string to print
;*==============================================================================*
prog_abort:		rcall	epower_off				;power off immediately
				bsf		INTCON,GIE,0			;reenable interrupts

				rcall	printf
				goto	main					;rude abort

;*==============================================================================*
; Subroutine to program with a fast (~500 nS) active-low or active-high
; programming pulse on pin the PGM pin. (Short pulse required for EEPROMs)
; After the programming pulse, stall or poll the EEPROM as required.
; Trashes W,R0
;*==============================================================================*
fast_pgmpulse:	movlw	ET_PGMVPP			;figure out which pin is PGM
				rcall	etype_getval
				andlw	0x70				;just get the PGM pin assignment bits
				movwf	R0,0				;so we can swap
				swapf	R0,F,0				;put PGM bits in R0<2:0>

;7-way branch based on R0 - the PGM pin assignment
fjtab:			movf	PCL,W,0				;force a load of the PCLATH register (page 62)
				rlncf	R0,W,0				;command index (times 2 for jump table)
				addwf	PCL,F,0

				return						;if no PGM pin defined
				bra		fast18				;PGM is on pin 18
				bra		fast19				;PGM is on pin 19
				bra		fast20				;PGM is on pin 20
				bra		fast21				;PGM is on pin 21
				bra		fast22				;PGM is on pin 22
fjtend:										;Fall through to fast23 if PGM is on pin 23

;Quick-pulse on pin 23
fast23:			btg		LATC,P23n,0
				nop
				btg		LATC,P23n,0
				bra		stall_between		;stall as needed, then return from there

;Quick-pulse on pin 22
fast22:			btg		LATB,P22n,0
				nop
				btg		LATB,P22n,0
				bra		stall_between		;stall as needed, then return from there

;Quick-pulse on pin 21
fast21:			btg		LATE,P21_0,0
				nop
				btg		LATE,P21_0,0
				bra		stall_between		;stall as needed, then return from there

;Quick-pulse on pin 20
fast20:			btg		LATA,P20_0,0
				nop
				btg		LATA,P20_0,0
				bra		stall_between		;stall as needed, then return from there

;Quick-pulse on pin 19
fast19:			btg		LATA,P19_0,0
				nop
				btg		LATA,P19_0,0
				bra		stall_between		;stall as needed, then return from there

;Quick-pulse on pin 18
fast18:			btg		LATA,P18_0,0
				nop
				btg		LATA,P18_0,0
				bra		stall_between		;stall as needed, then return from there

;*************************************************************
; The above jump table must not span a 256-byte page boundary.
;*************************************************************
	if high(fjtab)^ high(fjtend)
	error "Martin sez: Fast PGM pulse jump table spans a page boundary"
	endif

;*==============================================================================*
; Subroutine to read one byte from the EPROM
; On Entry:
;   ADDRESSH:ADDRESSL = desired address
;   ETYPE has the EPROM type
;   EPROM is powered up for reading
;   EPROM -OE and PGM signals are low (inactive)
;   DINV_STATE = FF if the EPROM data is to be inverted
; On Exit:
;   R2=W = EPROM data, inverted iff DINV_STATE = FF
;   PIC port D is tristated
;   The external data buffer is pointing toward the PIC, and is tristated
; Trashes W,R0,R3,FSR0,TBLPTRH:TBLTRL,REEADR
;*==============================================================================*
eread_byte:	rcall	address2pins		;put the address onto the EPROM pins

			movlw	0x00				;strobe the address, if there is an address strobe
			rcall	asn2pin				;(asn2pin just returns if there isn't.)

			movlw	0x03				;(.25)Remember that pin 20 (which may be the -AS pin)
			call	K_STALL1U			;(4)..is slow due to its 0.047 uF capacitor

			movlw	0x01				;leave pin in the active state
			rcall	asn2pin

			movlw	0x01				;set EPROM chip select active
			rcall	cs2pin				;(1.5 after CS write)

			setf	TRISD				;(.25)tristate the PIC's data buffers
			bcf		LATB,DBUFDIR,0		;(.25)point the external data buffer toward PIC
			bcf		LATB,DBUFOEn,0		;(.25)enable that data buffer's output

			movlw	DINV_STATE			;(.25) get data inversion setting
			call	K_RDEEPROM			;(1.5+.5) this one is in EEPROM

			xorwf	PORTD,W,0			;actually read the EPROM data, invert as needed
			movwf	R2,0				;temp save

			movlw	0x00
			rcall	cs2pin				;set EPROM chip select inactive

			bsf		LATB,DBUFOEn,0		;disable data buffer's output
			movf	R2,W,0				;EPROM data to W for return
			return

;*==============================================================================*
; Subroutine to power EPROM up for programming
; On Entry:
;   ETYPE = current EPROM type
; On Exit:
;   Busy light is on
;   Vcc is on, to its programming level
;   Vpp power supply is set for the correct Vpp voltage, and measured to see
;      that it is roughly correct
;   Chip Select is active, unless it is also the PGM pulse signal
;   -OE is either inactive (logic high), or at +12V (if required for programming)
; Trashes W,R0,R1,R2,R3,R4,FSR0,TBLPTRH:TBLTRL,REEADR
;*==============================================================================*
epower_prog:	bsf		LATE,ADRCLK,0			;turn on the busy light

;Start the Vpp switcher for this EPROM's Vpp voltage. Note that if an external
;(negative) supply is called for, then the onboard Vpp will be turned off. If Vpp
;manual adjustment is required, then the appropriate Vpp voltage (which must
;adjusted) was specified when the EPROM was selected.
				rcall	eset_vpp

;Set EPROM chip select active, unless it is also the PGM pin. Note that this
;will be overwritten very soon if Vpp is on the same pin as (-)CS.
				movlw	0x01
				btfss	SHARED,CSPGM,0
				rcall	cs2pin

;turn on any stuck-high pin, in case it's really a Vdd pin
				movlw	0x01				;on
				rcall	hi2pin

;Set the (-)PGM pin to its inactive state, if there is one
				movlw	0x00
				rcall	pgm2pin

;Turn on Vcc for programming
				movlw	0x02
				rcall	vcc2pin

;Turn on Vbb and Vdd, if they exist
				movlw	0x01
				rcall	vbbvdd2pins	

;Stall to let the Vpp power supply come up to speed
				movlw	.40						;10 mS
				call	K_STALL250U

;Set up -OE either inactive or at its special programming voltage, if there
;is such a thing: it may possibly be +12V or Vpp.
;If Vpp is the same pin as -OE then: if Vpp is pulsed, then it will be set to
;the programming-inactive state. If Vpp is not pulsed, then turn Vpp is on.
				movlw	0x02					;set -OE to its Programming level
				rcall	oen2pin

;If this EPROM pulses Vpp, then put Vpp at programming-inactive level
;Otherwise (for pulsed PGM) put it at the active level.
				movlw	0x02					;programming-inactive level
				btfsc	FLAGS2,PMODE1,0			;0 means Vpp is pulsed
				movlw	0x03					;programming-active level
				rcall	vpp2pin

;Check that the Vpp is roughly correct, using the ADC
				bsf	ADCON0,GO					;start conversion

ep_adcwait:			btfsc	ADCON0,GO			;wait for conversion to complete
					bra		ep_adcwait

				setf	R4,0					;remember too high
				movf	ADRESH,W,0				;get ADC result
				subwf	VSENSE_HIGH,W,0			;W:= upper limit-result, C set if ok
				bnc		panic_vpp

				clrf	R4,0					;remember too low
				movf	VSENSE_LOW,W,0			;lower limit
				subwf	ADRESH,W,0				;W:=result - lower limit, C set if ok
				bnc		panic_vpp

				return

;Vpp is out of spec - abort - too high if R4=FF, too low if R4=0
panic_vpp:		rcall	epower_off				;power off immediately

				lfsr	0,hvpanic_msg-sstrings	;""Abort! Vpp is too high"
				movf	R4,F,0
				btfsc	STATUS,Z,0
				lfsr	0,lvpanic_msg-sstrings	;""Abort! Vpp is too low"

				bra		msg_abort

;*==============================================================================*
; Subroutine to power EPROM up for reading
; On Entry:
;   ETYPE = current EPROM type
; On Exit:
;   Busy light is on
;   Vpp power supply is still on (unchanged)
;   -OE is active, enabling the EPROM's output buffer
; Trashes W,R0,R1,R2,R3,TBLPTRH:TBLTRL,REEADR
;*==============================================================================*
epower_read:	bsf		LATE,ADRCLK,0		;turn on the busy light

;turn on any stuck-high pin, in case it's really a Vdd pin
				movlw	0x01				;on
				rcall	hi2pin

;Set PGM pin inactive
				movlw	0x00
				rcall	pgm2pin

;Turn on Vcc for reading
				movlw	0x01
				rcall	vcc2pin

;Turn on Vbb and Vdd, if they exist
				movlw	0x01
				rcall	vbbvdd2pins	

;Put the reading voltage on Vpp
				movlw	0x01
				rcall	vpp2pin

;Set up -OE for reading
				movlw	0x01			;active for reading
				rcall	oen2pin

;Stall to let power supply come up to speed
				movlw	.4				;1 mS
				goto	K_STALL250U		;return from there

;*==============================================================================*
; Subroutine to turn off power to the EPROM
; On Entry:
;   ETYPE = current EPROM type
; On Exit:
;   Busy light is off
;   All EPROM pins are floating or at 0V
;   Vpp power supply is still on (unchanged)
; Trashes W,R0,R1,R2,R3,TBLPTRH:TBLTRL,REEADR
;*==============================================================================*
epower_off:		movlw	0x00
				rcall	vpp2pin				;disconnect EPROM from Vpp

				movlw	0					;shup down the switching power supply
				rcall	set_vpp				;(its output will drift down to +11.3V)

				movlw	(1<<P22n) ^ 0xFF	;turn everything on port b off (especially Vcc)
				iorwf	LATB,F,0			;..except pin 22 (because pin 22 could be PGM)

				clrf	ADDRESSL,0			;put address lines to 0V
				clrf	ADDRESSH,0
				rcall	address2pins

				movlw	.20					;5 mS
				call	K_STALL250U			;Stall to let power supplies shut down

;The PGM pin, or the -CE pin that serves as the PGM pin, will be on one of pins 18-22.
;Shut these down well after the power supplies are off, to avoid a spurious write.
				setf	LATB,0				;turn off in particular pin 22 (in case it's PGM)
				clrf	LATA,0				;turn off pins 18-20
				clrf	LATE,0				;turn off pin 21 and the Busy light

				return

;*==============================================================================*
; Subroutine to print the buffer row's address as 4 hex digits, followed by ': '
;
; On Entry:
;   ADDRESSH:ADDRESSL has the buffer address of the byte to be printed
; On Exit:
;   This row's address has been printed, followed by a colon
;   CHECKSUM unchanged
;   W, R0, R1, FSR0 trashed
;*==============================================================================*
print_addr:		movlw	CR							;print CR-LF
				call	K_CONOUT

				movf	ADDRESSH,W,0
				call	K_PRINTHEX2					;don't mess up CHECKSUM

				movf	ADDRESSL,W,0				;low byte of address
				call	K_PRINTHEX2					;don't mess up CHECKSUM

				lfsr	0,colonspace_msg-sstrings	; ': '

;Fall into printf

;*==============================================================================*
; Subroutine to print a null-terminated string
; Note that strings must be in the 1st 4K of RAM, because FSR0 is a 12-bit
; register.
; On Entry:
;   FSR0 + sstrings = address of the null-terminated string
; On Exit:
;   W,R0,TBLPTR,FSR0 trashed
;   Receive interrupt (RCIE) is enabled
;*==============================================================================*
printf:			movff	FSR0H,TBLPTRH
				movff	FSR0L,TBLPTRL

do_printf:		movlw	low(sstrings)
				addwf	TBLPTRL,F,0
				movlw	high(sstrings)
				addwfc	TBLPTRH,F,0
				goto	K_PRINTF		;print string at TBLPTRH:TBLPTRL
				
;*==============================================================================*
; Subroutine to print CR/LF, followed by a null-terminated string
; On Entry:
;   FSR0 points to the null-terminated string
; On Exit:
;   W,R0,FSR0,TBLPTR,FSR0 trashed
;   Receive interrupt (RCIE) is enabled
;*==============================================================================*
cr_printf:		movff	FSR0H,TBLPTRH
				movff	FSR0L,TBLPTRL
				movlw	CR
				call	K_CONOUT			;print CRLF. Trashes FSR0.

				bra		do_printf			;print string at TBLPTRH:TBLPTRL

;*==============================================================================*
; Subroutine to check for a pause (space bar) or abort (control-C or ESC) from
; the user
; Note that control-S will also pause via the receive interrupt.
;*==============================================================================*
check_pause:	call	K_CHKABORT			;user abort?
				bz		cp_done				;Z if no user input
				bc		quiet_abort			;C if user abort

				xorlw	' '					;pause?
				bnz		cp_done				;ignore anything else

;Paused. Hang out here until the user types anything else. Abort if requested.
pause_loop:		call	K_CHKABORT			;user abort?
				bz		pause_loop			;Z if no user input
				bc		quiet_abort			;rude abort, no message

cp_done:		return						;normal exit

;*==============================================================================*
; Subroutine to flush any user input in the Rx queue, and check for control-C or
; Escape. Abort if either is found.
; Trashes W,R0,FSR0
; (5.5 uS if no chrs waiting)
;*==============================================================================*
flush_abort:		call	K_CHKABORT		;(4+.5)user abort?
					bc		abort			;(.25)abort if requested
					bnz		flush_abort		;(.25)flush everything in the receive queue

				return						;(.5)

;*==============================================================================*
; Abort operation: print 'abort', power down, and rudely jump to main
;*==============================================================================*
abort:			lfsr		0,abort_msg-sstrings

msg_abort:		rcall		printf

quiet_abort:	rcall		epower_off		;power off, turn off busy light
				goto		main			;hoses stack, but main fixes it

;*==============================================================================*
; Subroutine to ask the user for y (yes) or n (no) (allowing both lowercase and
; uppercase)
; On Entry:
;    FSR0 = ask string
; Aborts if not 'N', returns if 'Y'. Nags until 'Y' or 'N'
; Trashes W, R0, FSR0
;*==============================================================================*
ask_yn:			rcall	cr_printf			;print page 1 ask string
				call	K_ASKYN
				bnz		abort				;NZ means no

				return

;*==============================================================================*
; Subroutine to print W as a 2-character hex value, and add value to CHECKSUM
; On Entry:
;   byte to send is in W
;   CHECKSUM is valid for all prior bytes
; On Exit:
;   W, R0, R1, FSR0, BSR trashed
;   incoming byte has been added to CHECKSUM
;*==============================================================================*
print_hex_csum:	addwf	CHECKSUM,F,0			;accumulate checksum
				goto	K_PRINTHEX2

;*==============================================================================*
; Subroutine to set the count to the maximum count for the current EPROM type,
; if no count was specified, or if the specified count is too large
; On Entry:
;   COUNTH:COUNTL = the specified count, which is definitely not 0000
;   ETYPE has the current EPROM type
;   FULL_FLAG is cleared unless COUNTH:COUNTL = 8K
; On Exit:
;   COUNTH:COUNTL = the byte count of the current EPROM, or the provided count,
;          whichever is lesser
;   FULL_FLAG set if the count was limited to an entire EPROM
; Trashes W,R0,R1
;*==============================================================================*
;Get the (encoded) byte count for the current EPROM type
limit_count:	movlw	ET_BYTES			;figure out how many bytes this EPROM has
				rcall	etype_getval
				andlw	0x07				;W<2:0> values are 0-6 for 256-8192
				movwf	R1,0				;into R1

;Compute the actual byte count from the encoded byte count
				incf	R1,F,0				;make it 1-7

				movlw	0x80				;compute in R2
				movwf	R2,0

lbc_loop:			rlncf	R2,F,0			;compute high byte of count
					decf	R1,F,0
					bnz		lbc_loop		;when done, max count is in R2

;Test to see if the provided byte count is less than the max, and return if so
				movf	COUNTH,W,0
				subwf	R2,W,0				;max count - COUNTH
				bnc		lbc_default			;carry clear if COUNTH > max count
				btfss	STATUS,Z,0			;equal?
				return						;n: no default required

;COUNTL must be 0 if COUNTH is at the max value, and this will be a full EPROM.
lbc_default:	movff	R2,COUNTH			;full EPROM count
				clrf	COUNTL,0
				bsf		FLAGS1,FULL_FLAG,0	;complete EPROM operation (no 'range' in messages)
				return

;*==============================================================================*
; Subroutine to circular-increment ADDRESSH:ADDRESSL (assuming 8K buffer),
; decrement COUNTH:COUNTL, and test for COUNTH:COUNTL=0
; On Exit:
;   ADDRESSH:ADDRESSL has been incremented, and cleared to 0 if it reached 8K
;   COUNTH:COUNTL has been decremented
;   Z set if COUNT has been decremented to 0
; Trashes W
; (2.75 uS)
;*==============================================================================*
next_a_count:	infsnz	ADDRESSL,F,0		;(.5)16-bit increment address
				incf	ADDRESSH,F,0
				bcf		ADDRESSH,0x05,0		;(.25)wrap at 8K
			
				movlw	0x01				;(.25)
				subwf	COUNTL,F,0			;(.25)16-bit decrement counter
				btfss	STATUS,C,0			;(.5)
				decf	COUNTH,F,0

				movf	COUNTH,W,0			;(.25)test counter for 0
				iorwf	COUNTL,W,0			;(.25)
				return						;(.5)with Z flag set if COUNT=0

;*==============================================================================*
; Subroutine to compare the specified range of the EPROM either to the
; buffer or to the erased value
; On Entry:
;   B_STATE set if this is a blank-check
;   R4 = blank value for blank check, taking into account data inversion
;        by eread_byte
;   ADDRESSH:ADDRESSL = EPROM starting address
;   (ADDRESSH+BUF_OFFSET):ADDRESSL = buffer starting address
;   COUNTH:COUNTL = byte count
; On Exit:
;   Results message is partially printed
; Trashes W,R0,R1,R2,R3, ADDRESSH:ADDRESSL,COUNTH:COUNTL
;*==============================================================================*
ep_compare:

;Loop to compare eprom to buffer
ecmp_loop:			rcall	eread_byte			;read EPROM at ADDRESSH:ADDRESSL, result in W
					movwf	R3,0				;remember for rep_mismatch

					movf	BUF_OFFSET,W,0		;buffer address offset
					addwf	ADDRESSH,F,0		;compute buffer address

					movf	R4,W,0				;erased EPROM value, taking into account
												;..possible inversion by eread_byte

					btfss	FLAGS1,B_STATE,0	;blank check or compare?
					call	K_RDSRAM			;compare: read buffer data at ADDRESSH:ADDRESSL

					movwf	R2,0				;remember buffer data for rep_mismatch

					movf	BUF_OFFSET,W,0		;buffer address offset
					subwf	ADDRESSH,F,0		;restore EPROM address

					movf	R2,W,0				;buffer data or erased EPROM value
					xorwf	R3,W,0				;compare to EPROM data
					btfss	STATUS,Z,0			;match?
					rcall	rep_mismatch		;n: report mismatch, allow user to abort

					rcall	next_a_count		;bump address pointer, decrement count
					bnz		ecmp_loop

;Fall into eprange

;*==============================================================================*
; Subroutine to print 'EPROM', followed by 'range ', if needed
; On Entry:
;   FULL_FLAG = 1 if the byte count defaulted to the full EPROM size
;*==============================================================================*
eprange:			lfsr	0,eprom_msg-sstrings	;'EPROM '

;Fall into prange

;*==============================================================================*
; Subroutine to print CR, then message, followed by 'range ', if needed
; On Entry:
;   FSR0 = address of string to print ('EPROM ' or 'Buffer ')
;   FULL_FLAG = 1 if the byte count defaulted to the full EPROM size
;*==============================================================================*
prange:			rcall	cr_printf				;print provided string with initial CR

				lfsr	0,range_msg-sstrings	;'range '
				btfss	FLAGS1,FULL_FLAG,0
				rcall	printf					;printed only if needed

				return

;*==============================================================================*
;* Subroutine to save ADDRESSH:ADDRESSL and COUNTH:COUNTL in save location
;*==============================================================================*
save_params:	movff	ADDRESSH,ADDRESSH_SAVE
				movff	ADDRESSL,ADDRESSL_SAVE

				movff	COUNTH,COUNTH_SAVE
				movff	COUNTL,COUNTL_SAVE
				return

;*==============================================================================*
;* Subroutine to restore ADDRESSH:ADDRESSL and COUNTH:COUNTL from save location
;*==============================================================================*
restore_params:	movff	ADDRESSH_SAVE,ADDRESSH
				movff	ADDRESSL_SAVE,ADDRESSL

				movff	COUNTH_SAVE,COUNTH
				movff	COUNTL_SAVE,COUNTL
				return

;*==============================================================================*
; Abort: Error return for level-1 subroutines
;*==============================================================================*
pop_cmderr:		pop							;chuck subroutine call's return address
				goto		cmd_error

;*==============================================================================*
; Subroutine to set up for one of the test commands
; This subroutine may only be used one call deep
; On Entry:
;   W = max value+1 for the user input
; On Exit:
;   Port D is enabled and set to 0
;   Busy light is on
;   abort if user input is too high
;   W = R4 = user input
;*==============================================================================*
;Valid user input?
tcmd_setup:		tstfsz	ADDRESSH,0			;no high byte allowed
				bra		pop_cmderr

				subwf	ADDRESSL,W,0		;W=user input - limit
				bc		pop_cmderr			;carry clear if limit>user input

;Light "busy" LED, since pins are probably not at 0V anymore
				clrf	PORTD,0				;since we might clock out low address bits
				clrf	TRISD,0
				bsf		LATE,ADRCLK,0
				setf	TRISD,0

				movf	ADDRESSL,W,0		;Set up registers for return
				movwf	R4,0
				return

;*==============================================================================*
; Subroutine to save the current EPROM type
; On Exit:
;   ETYPE_SAVE = current EPROM type
;   EEADR = ETYPE
;*==============================================================================*
save_etype:		movlw	ETYPE					;get EPROM type
				call	K_RDEEPROM
				movwf	ETYPE_SAVE,0		
				return

;*==============================================================================*
; Subroutine to restore the current EPROM type
; On Entry:
;   ETYPE_SAVE = desired EPROM type
;*==============================================================================*
restore_etype:	movlw	ETYPE					;restore EPROM type
				movwf	EEADR,0		
				movf	ETYPE_SAVE,W,0
				goto	K_WREEPROM				;..and return to main

;*==============================================================================*
; Subroutine to report an EPROM/Buffer mismatch
; Prints address, buffer data, EPROM data, unless B_STATE (blank check cmd),
; in which case, doesn't print the buffer data.
; Check to see if the user typed ^C to abort, and aborts if so
; On Entry:
;   ADDRESSH:ADDRESSL = address
;   R2 = buffer data
;   R3 = EPROM data
;   B_STATE set if this is a blank-check, and buffer data shouldn't be reported
; On Exit:
;   W, R0, R1, BSR, FSR0 trashed
;   ERR_FLAG set
;*==============================================================================*
rep_mismatch:	bsf		KERN_FLAGS,ERR_FLAG,0		;remember that we had an error

				rcall	check_pause					;allow user to abort or pause

				lfsr	0,mismatcha_msg-sstrings	;'Error at address '
				rcall	cr_printf					; Trashes R0

				rcall	print_addr					;4-digit address, followed by ': '
													; Trashes R0,R1

				btfsc	FLAGS1,B_STATE,0			;Skip buffer data if EB blank check cmd
				bra		rep_mm_skipb

				lfsr	0,mismatchb_msg-sstrings	;Buffer:
				rcall	printf
				movf	R2,W,0						;buffer data
				call	K_PRINTHEX2

rep_mm_skipb:	lfsr	0,mismatche_msg-sstrings	;EPROM:
				rcall	printf
				movf	R3,W,0						;EPROM data
				call	K_PRINTHEX2

;Fall into wait_txempty to flush transmit queue

;*==============================================================================*
; Subroutine to wait until the Tx Queue is empty
; The queue is empty when the transmit interrupt is shut off
; and not XOFF'd
; Trashes BSR
; (1.25 uS if transmitter is idle)
;*==============================================================================*
wait_txempty:	btfsc	INT_FLAGS,XOFF_STATE,0	;(.5)XOFF'd?
				bra		wait_txempty
	
				btfss	PIE1,TXIE,0				;(.25)is transmitter enabled?
				return							;(.5)
				bra		wait_txempty

;*==============================================================================*
; Subroutine to report the selected EPROM type
; On Entry:
;   ETYPE = EPROM type
; On Entry at rep_etype2:
;   FSR0 = initial message string
; Trashes R0,R1,R2,FSR0
;*==============================================================================*
rep_etype:	lfsr	0,etype_msg-sstrings		;'Current EPROM: '

rep_etype2:	rcall	cr_printf

			movlw	ETYPE						;EEPROM address of variable
			call	K_RDEEPROM					;W=EPROM type
			call	K_PRINTHEX2					;print EPROM type

			lfsr	0,colonspace_msg-sstrings	;': '
			rcall	printf

			clrf	R2,0						;no blanks padded on the end
			movlw	ET_NAME						;lookup the EPROM name

;Fall into eds_print

;*==============================================================================*
; Subroutine to print a message from the EPROM data structure
; If ETYPE specifies an inbuilt EPROM, then et_getval will retrieve a pointer
; to the string, in the format expected by p_estring. If the specified EPROM
; is a custom EPROM (with its parameters stored in the PIC's onboard EEPROM),
; then etype_getval will retrieve the first character of the string itself,
; and EEADR will be pointing to the string in EEPROM.
; On Entry:
;   W = the data structure item (string pointer)
;   R2 = formatted field width
;   ETYPE = the current EPROM type
; Trashes R0,R1,R2
;*==============================================================================*
eds_print:	rcall	etype_getval			;returns w=string index

			btfsc	FLAGS2,CUST_EPROM,0		;custom EPROM (so the record is in EEPROM)?
			goto	K_EEPRINT				;print string from EEPROM, return from there

;Fall into p_estring

;*==============================================================================*
; Subroutine to print an EPROM string from the 2K "estrings" region, with
; formatting (really just a fixed length). Strings for this subroutine are
; null-terminated (with the null being on an even byte), followed by a byte
; that specifies the string's byte count. (Nulls on odd bytes are ignored.)
; If W=0xFF on entry, then don't print anything. This routines check to see of
; the pointer's bits<2:1> got truncated by the /8 by looking the see if the
; character 2 bytese before the beginning of the string is a null - the
; termination of the previous string. If not, it searches forward 2 bytes at a
; time to find the end of the previous string, and then bumps 2 to point to
; the desired string.
; On Entry:
;   W=(string address-estrings)/8
;   R2 = format length, 0 means no formatting
; Trashes W,R0,R2
;*==============================================================================*
p_estring:	movwf	R0,0
			incf	R0,W,0				;FF means nothing to print
			bz		pes_exit			;nothing to print?

;look at  the character that is 2 bytes before the alleged beginning of this string.
;This should be the null-termination of the previous string. If not, hunt for it.
;(The divide-by-8 may have truncated the lower 3 address bits of the string address.)
;Note that this depends on the fact that no string (including its termination and
;byte count) is less than 8 bytes long.	
			movlw	0x08				;R0 is string index/8
			mulwf	R0,0
			movlw	low(estrings-2)
			addwf	PRODL,F,0
			movlw	high(estrings-2)
			addwfc	PRODH,W,0			;W:PRODL = address offset to string index
			movwf	TBLPTRH,0
			movff	PRODL,TBLPTRL		;TBLPTRH:TBLPTRL points to 2 before the string

pal_loop:		tblrd*+					;is this the end of the previous string?
				movf	TABLAT,W,0		;particularly, its null-terminator
				tblrd*+					;skip every other byte, since the null terminator
										;..must be on an even byte
				bnz		pal_loop		;n: keep looking

;Now that we know where it starts (at TBLPTRH:TBLPTRL), print the string
			call	K_PRINTF			;ADDRESSH:ADDRESSL=string address
										;returns with TABLBTRH:TBLPTRL pointing just past the null

;Pad the string out R2 characters wide
			tblrd*+						;read the character count into TABLAT				
			movf	TABLAT,W,0			;chr past the string end is the string length
			subwf	R2,W,0				;carry will be set if string length<=format length
			bnc		pes_exit			;no formatting or string is too long?
			bz		pes_exit			;exact fit?

			movwf	R2,0

;Print R2 blanks to fill out the format
pes_blanks:
				movlw	' '
				call	K_CONOUT
				decf	R2,F,0
				bnz		pes_blanks

pes_exit:	return

;*==============================================================================*
; Subroutine to report the current Vpp voltage
; On Entry:
;   ETYPE = EPROM type
; Trashes R0,FSR0
;*==============================================================================*
rep_vpp:	movf	CCPR1L,W,0					;is switcher off?
			bz		rep_so

			lfsr	0,setvpp_msg-sstrings		;"Vpp is set for "
			rcall	cr_printf

;Get current Vpp comparator input

			movf	CM1CON0,W,0					;get comparator's selected input
			andlw	(1<<C1CH0)+(1<<C1CH1)		;just the comparator input bits (page 267)

;Compute selected comparator voltage (assuming 0.8V diode drop):
;        Pin 18                  Pins 19-21
; C1CH1  C1CH0  Voltage    C1CH1  C1CH0  Voltage
;   0      0     25.9V      0      0     25.1V
;   0      1     21.8V      0      1     21.0V
;   1      0     13.55V     1      0     12.75V
;   1      1     13.95V     1      1     13.15V

			movwf	R2,0
			incf	R2,F,0						;account for 1st dec

			lfsr	0,v1315_msg-sstrings

			dcfsnz	R2,F,0
			lfsr	0,v2510_msg-sstrings

			dcfsnz	R2,F,0
			lfsr	0,v21_msg-sstrings

			dcfsnz	R2,F,0
			lfsr	0,v1270_msg-sstrings

			bra		printf						;print Vpp voltage, return from there

;Report that the switcher is off
rep_so:		lfsr	0,swoff_msg-sstrings		;"Switcher is off"
			bra		cr_printf

;*==============================================================================*
; Subroutine to set up for the current EPROM type
; On Entry:
;   ETYPE = current EPROM type
; On Exit:
;   Vpp power supply is set to the correct voltage
;   The following variables are set up for the current EPROM type:
;     FLAGS2 
;     SHARED
;     SMART1
;     SMART2
;     PROGREPS
;     PPULSEH:PPULSEL (invalid if FLAGS2.FAST_PULSE is set)
;     PPDELH:PPDELL (invalid if FLAGS2.FAST_PULSE is set)
;     VSENSE_LOW
;     VSENSE_HIGH
; Trashes R0,R1,R2,R3,PRODH:PRODL
;*==============================================================================*
etype_setup:	movlw	0x8F				;remove old adjustment bits
				andwf	FLAGS2,F,0

				call	special_v			;any special Vpp requirements?
											;returns R3 = ET_SPECIAL value

				swapf	R3,W,0				;put adjustment bits in W<6:4>
				andlw	0x70				;chuck the rest
				iorwf	FLAGS2,F,0			;and install them in FLAGS2

;Get & save the smart mode
				movlw	ET_SMART1			;Get phase 1 smart mode
				rcall	etype_getval
				movwf	SMART1,0

				movlw	ET_SMART2			;Get phase 2 smart mode
				rcall	etype_getval
				movwf	SMART2,0

;Get & save whether or not the -OE pin is also the Vpp pin
				bcf		SHARED,OEVPP		;assume not for now

				movlw	ET_OECS				;Get -OE pin
				rcall	etype_getval
				swapf	R0,W,0				;put -OE pin in 2:0
				movwf	R2,0				;temp save OE and CS pins

				movlw	ET_PGMVPP
				rcall	etype_getval		;get Vpp pin
				movwf	R3,0				;temp save Vpp and PGM pins

				xorwf	R2,W,0				;compare to -OE pin
				andlw	0x07				;chuck other bits

				btfsc	STATUS,Z,0			;is -OE also Vpp?
				bsf		SHARED,OEVPP		;y: remember

;Get & save whether or not the CS pin is also the Vpp pin
				bcf		SHARED,CSVPP		;assume not for now

				swapf	R2,W,0				;put CS pin in 2:0
				xorwf	R3,W,0				;compare to Vpp pin
				andlw	0x07				;chuck other bits

				btfsc	STATUS,Z,0			;is -OE also Vpp?
				bsf		SHARED,CSVPP		;y: remember

;Get & save whether or not the -OE pin is also the PGM pin
				bcf		SHARED,OEPGM		;assume not for now

				swapf	R3,W,0				;PGM bits to 2:0
				xorwf	R2,W,0				;compare to -OE pin
				andlw	0x07				;chuck other bits

				btfsc	STATUS,Z,0			;is -OE also PGM?
				bsf		SHARED,OEPGM		;y: remember

;Get and save wheter or not the CS pin is also the PGM pin
				bcf		SHARED,CSPGM

				swapf	R2,F,0				;put CS pin in 2:0
				swapf	R3,W,0				;PGM bits to 2:0
				xorwf	R2,W,0				;compare to CS pin
				andlw	0x07				;chuck other bits

				btfsc	STATUS,Z,0			;is CS also PGM?
				bsf		SHARED,CSPGM		;y: remember

;Get & save the repetition count, or the limit for smart programming
				movlw	ET_PROGREPS			;Get encoded programming pulse spec
				rcall	etype_getval
				movwf	PROGREPS,0

;Get, compute, and remember the 16-bit programming pulse width spec for this EPROM
				bsf		FLAGS2,FAST_PULSE,0	;assume fast pulse for a moment

				movlw	ET_PROGPULSE		;Get encoded programming pulse spec
				rcall	etype_getval		;result into W and R0

				andlw	0x7F				;0 means fast pulse (msb is just a multiplier)
				bz		es_fastpp			;FAST_PULSE is already set

				bcf		FLAGS2,FAST_PULSE,0	;this is not a fast pulse
				movf	R0,W				;recover whole value

;Convert ET_PROGPULSE to 10 uS units in PPULSEH:PPULSEL.
				rcall	decode_pulse
				movff	PRODH,PPULSEH		;remember program pulse width
				movff	PRODL,PPULSEL
es_fastpp:

;Convert ET_PPDELAY to 10 uS units in PPDELH:PPDELL.
				movlw	ET_PPDELAY			;Get encoded programming pulse spec
				rcall	etype_getval

				rcall	decode_pulse
				movff	PRODH,PPDELH		;remember program pulse delay
				movff	PRODL,PPDELL

;Figure out which sort of programming pulse and remember in FLAGS2
;Look up this EPROM's Vpp specification. Bits 2:0 are defined as follows:
; Bit    FLAGS2 Bit     Function
;  2        RDVPP     Vpp setting during read: 0 means 0V, 1 means +5V
;  1        PMODE1    0 = Pulsed Vpp, 1 = pulsed PGM
;  0        PMODE0    0 = Vpp returns to 0V, 1 = Vpp returns to +5V (only if PMODE1=0)

				movlw	(0x07 ^ 0xFF)		;strip off old bits
				andwf	FLAGS2,F,0

				movlw	ET_VPPSETUP			;Get Vpp voltage and programming mode bits
				rcall	etype_getval		;fesult in W and R0
				andlw	0x07				;get the pulse type and Vpp read state spec
				iorwf	FLAGS2,F,0			;and install them in FLAGS2

;Get the programming voltage, and look up which pin to use for foldback current limiting,
;also the upper and lower current limits
				clrf	VSENSE_LOW,0		;assume no limit for now
				setf	VSENSE_HIGH,0

				swapf	R0,F,0				;look at ET_VPPSETUP
				movlw	0x07
				andwf	R0,F,0				;no Vpp?
				bz		es_nolimit			;..then no limits

				decf	R0,F,0				;make Vpp voltages 0-4
				btfsc	R0,0x02,0			;R0=4, meaning external supply?
				bra		es_nolimit			;external supply: no limits

				movlw	0x03				;Get just the power supply section, 0-3
				andwf	R0,F,0				;also tests for 0, meaning 12.75V supply
				movlw	0x25				;select AN9, start conversion (page 255)
				btfsc	STATUS,Z,0			;except for 12.75V Vpp
				movlw	0x29				;which uses AN10
				movwf	ADCON0,0			;The ADC is now set up

				movlw	high(LIMTABLE)		;table lookup voltage limits
				movwf	TBLPTRH,0
				rlncf	R0,W,0
				addlw	low(LIMTABLE)
				movwf	TBLPTRL,0
				btfsc	STATUS,C,0
				incf	TBLPTRH,F,0

				tblrd*+
				movff	TABLAT,VSENSE_LOW	;get low limit, remember
				tblrd*+
				movff	TABLAT,VSENSE_HIGH	;get high limit, remember

;Make sure the switcher is turned off. (Its output will then be about 11.3V - the input
;voltage minus the drop across the current limiter.)

es_nolimit:		movlw	0x00
				bra		set_vpp

;*==============================================================================*
; Subroutine to set up the Vpp supply voltage for this EPROM type
; Trashes W,R0,R1
;*==============================================================================*
eset_vpp:		movlw	ET_VPPSETUP			;Get Vpp voltage and programming mode bits
				rcall	etype_getval		;result in W and R0
				swapf	R0,W,0				;get the Vpp voltage spec into W<2:0>
				andlw	0x07				;chuck all other bits

;Fall into set_vpp

;*==============================================================================*
; Subroutine to set the Vpp switching power supply voltage
; On Entry:
;   W = desired voltage:
;      0 = 12V (input voltage) (sets pulse width to 0, without changing
;              the comparator input)
;      1 = 12.75V, use CI2IN2
;      2 = 13.15V, use CI2IN3
;      3 = 21.0V,  use CI2IN1
;      4 = 25.1V,  use CI2IN0 (also for 26V)
;      5 = external supply (so shut switcher off)
; Trashes W,R0
;*==============================================================================*
set_vpp:	movwf	R0,0				;temp save input parameter

			clrf	CCPR1L,0			;set pulse width to 0 while we work

			movf	R0,F,0				;test for 0
			btfsc	STATUS,Z,0			;off?
			return						;y: done with pulse width = 0

			xorlw	0x05				;external supply?
			btfsc	STATUS,Z,0
			return						;y: done with pulse width = 0
			
;Create comparator input bits from R0
;  R0     C1CH1  C1CH0
; 0x01      1      0
; 0x02      1      1
; 0x03      0      1
; 0x04      0      0

			movlw	0x05
			incf	R0,F,0				;R0 is between 0x02 and 0x05
			btfsc	R0,0x02,0			;already okay for 0x02 and 0x03
			xorwf	R0,F,0				;clear bit 2, invert bit 0

;Set Vpp according to R0. (See page 267)
			movf	CM1CON0,W,0			;get all the other bits in this register
			andlw	((1<<C1CH0)+(1<<C1CH1)) ^ 0xFF	;strip comparator input (page 267)

			iorwf	R0,W,0				;put comparator bits in place
			movwf	CM1CON0,0			;actually select the input now

;Set the PWM pulse width to actually turn on the Vpp switcher
			movlw	VPP_PW
			movwf	CCPR1L,0

			return

;*==============================================================================*
; Subroutine to convert encoded time value to 10 uS units.
;  If msb in ET_PROGPULSE is set then the value is in 1 mS units, and must be
;  multiplied by 100. Also knock 30 uS off the result, to account for programming
;  loop overhead.
; On Entry:
;   W = value to decode
;      00h or 80h means it's a minimum amount of time
;      01h means special value
;      02h and 03h at this point not allowed (may be used for special values)
;      Otherwise: decode as above
; On Exit:
;   PRODH:PRODL = decoded value
;*==============================================================================*
decode_pulse:	movwf	PRODL,0
				clrf	PRODH,0				;high byte

				sublw	0x04				;special value below 3?
				bc		dp_done				;carry set if below 4

				movf	PRODL,W,0				
				bcf		PRODL,0x07,0		;clear the high bit, in case it's set
				movf	PRODL,F,0			;80h is the same as 0
				bz		dp_done

				andlw	0x80				;test bit 7 from input
				movlw	.100
				btfss	STATUS,Z,0			;time in 10 uS or 1 mS units?
				mulwf	PRODL,0				;adjust to 10 uS if 1 mS

				movlw	0x04				;(.25)adjust for 30 uS timer overhead
											;and that stall_r1r0 goes one extra time
				subwf	PRODL,F,0			;(.25)
				btfss	STATUS,C,0			;(.5)borrow?
				decf	PRODH,F,0			;PRODH:PRODL = program pulse time in 10 uS units

dp_done:		return

;*==============================================================================*
;Subroutine to control Vcc for this type of EPROM.
; Vdd is always on pin 24
; On Entry:
;   W = 0 to turn power off
;     = 1 to turn Vcc on for reading (always +5V)
;     = 2 to turn Vcc on for programming (according to EPROM type)
;   ETYPE has the current EPROM type
; On Exit:
;   R1 = 0 off, 1 for 5V, 2 for 6.2V. 3 for 12V
; Trashes W,R0,R3
;*==============================================================================*
vcc2pin:		movwf	R3,0				;save input

				movlw	(1<<P24_5Vn)+(1<<P24_6Vn)+(1<<P24_12Vn)	;(active-low bits)
				iorwf	LATB,F,0			;turn off all Vcc sources

				btfsc	R3,0x01,0			;R3=2, meaning on for programming?
				bra		vcc2p_onprog

				btfss	R3,0x00,0			;R3=0, meaning off?
				return						;y: done			

vcc2p_5V:		bcf		LATB,P24_5Vn		;n:Turn Vcc on for reading
				return				

;Set Vcc for programming according to the EPROM type
vcc2p_onprog:	movlw	ET_VBBVDD			;VBB & VDD voltage specs, Vcc programming voltage spec
				rcall	etype_getval		;returns programming Vcc specification in W bits <3:0>
				andlw	0x07				;W=0: 0V
											;W=1: +5V
											;W=2: +6.2V
											;W=3: +12V

				bz		vcc2p_off			;0v? it's already off

				addlw	0xFF				;decrement
				bz		vcc2p_5V			;make it 5V

				addlw	0xFF				;decrement
				bnz		vcc2p_not6

				bcf		LATB,P24_6Vn		;+6.2V setting
				return

vcc2p_not6:		bcf		LATB,P24_12Vn		;+12V setting
vcc2p_off:		return

;*==============================================================================*
; Subroutine to turn on/off Vbb and Vdd at the correct voltages for this type
; of EPROM.
; Vbb is always -5V on pin 21, if it exists
; Vdd is always +12V on pin 19, if it exists
; On Entry:
;   W = 0 to turn power off, R3 is not zero to turn power on.
;   ETYPE has the current EPROM type
; On Exit:
;   R1 = {0-NZ} for none, -5/+12
;   Vbb and Vdd for this type of EPROM are off or on as specified
; Trashes R0,R1,R3,EEADR,TBLPTR
;*==============================================================================*
vbbvdd2pins:	movwf	R3,0				;save input

				movlw	ET_VBBVDD			;VBB & VDD voltage specs, Vcc voltage spec
				rcall	etype_getval		;returns Vbb & Vdd specification in W bits <5:4>

				andlw	0x30				;just get & test bits 5 & 4
				movwf	R1,0				;R1=ET_VBBVDD value for thie EPROM

;Turn Vdd on or off as requested, if it exists
				movlw	((1<<P19_0)+(1<<P19_1)) ^ 0xFF
				andwf	LATA,W,0			;strip off pin 19 assignment

				tstfsz	R3,0				;on or off?
				iorlw	(1<<P19_1)			;select +12V on for pin 19

				btfsc	R1,0x05,0			;Is there a Vdd pin?
				movwf	LATA,0				;y: update pin 19

;Turn Vbb on or off as requested, if it exists
				movlw	((1<<P21_0)+(1<<P21_1)) ^ 0xFF
				andwf	LATE,W,0			;strip off pin 21 assignment

				tstfsz	R3,0				;on or off?
				iorlw	(1<<P21_1)			;select -5V on

				btfsc	R1,0x04,0			;Is there a Vb pin?
				movwf	LATE,0				;y: update pin 21 now

				return

;*==============================================================================*
; Subroutine to put the correct Vpp voltage to the correct EPROM pin
; On Entry:
;   W={0:3}={0V,reading state,inactive programming state,Vpp}
;   ETYPE is the current EPROM type
;   Vpp is already set to the correct voltage for this EPROM
;   EXT_VPP is set if this EPROM requires an external Vpp power supply
; On Exit:
;   R1 = pin number - 17, with 0 meaning none
; Trashes W,R2,EEADR,TBLPTR
; (16 uS from entry to pin off)
; (.5 uS from pin write through return)
;*==============================================================================*
;compute R2={0:3} for {0V,+5V,external,Vpp}
vpp2pin:	movwf	R2,0					;(.25)
			movf	R2,F,0					;(.25) test for 0
			bz		v2pins					;(.25)R2=0 means just make it 0V

			btfsc	R2,0x01,0				;(.25)one of the programming levels?
			bra		v2p_prog				;(.5)y: do programming levels

;Prepare to put Vpp at its read state (R2 = 1 here)
			btfss	FLAGS2,RDVPP,0			;is reading state high or low?
			clrf	R2,0					;if low, make R2 low
			bra		v2pins					;R2 = 0 for 0V, 1 for 5V

;We are programming. Should  Vpp become active or inactive?
v2p_prog:	btfss	R2,0x00,0				;(.25)inactive programming state?
			bra		v2p_poff				;(.5)y: turn it off

;Prepare to turn Vpp on, either for onboard Vpp or external Vpp. R2 = 3 here
			btfsc	FLAGS2,EXT_VPP,0		;external Vpp?
			decf	R2,F,0					;y: set up for external Vpp
			bra		v2pins					;R2 = 2 or 3, for external or onboard Vpp

;Prepare to put Vpp at its programming inactive state
v2p_poff:	clrf	R2,0					;(.25)Set up for 0V or +5 depending on PMODE0
			btfsc	FLAGS2,PMODE0,0			;(.5)
			incf	R2,F,0
											;R2 = 0 or 1, depending on PMODE0

;Figure out which pin is Vpp for this EPROM, and send the appropriate voltage there
;R2 = Vpp selection {0-3}={0V,5V,external Vpp,Vpp}
v2pins:		movlw	ET_PGMVPP							;(.25)specifies Vpp pin in bits <2:0>
			rcall	etype_getval						;(9.25 + .5)
			andlw	0x07								;(.25)just look at Vpp bits
			movwf	R1,0								;(.25) 0x00: none
														;      0x01: pin 18
														;      0x02: pin 19 (not allowed)
														;      0x03: pin 20
														;      0x04: pin 21

v2ptab:		movf	PCL,W,0								;(.25)force a load of the PCLATH register (page 62)
			rlncf	R1,W,0								;(.25)pin index (times 2 for jump table)
			addwf	PCL,F,0								;(.25)


			return										;No Vpp pin (EEPROM)
			bra		vpp_pin18							;(.25)Vpp on pin 18
			return										;not allowed: Vpp on pin 19
			bra		vpp_pin20							;Vpp on pin 20
v2ptend:												;Must be pin 21

vpp_pin21:	movf	LATE,W,0							;(.25)get other bits
			andlw	((1<<P21_0) + (1<<P21_1)) ^ 0FFh	;(.25)strip old value
			iorwf	R2,W,0								;(.25)new control bits in place
			movwf	LATE,0								;(.25)

			return										;(.5)

;Vpp in on pin 20
vpp_pin20:	movf	LATA,W,0							;get other port bits
			andlw	((1<<P20_0) + (1<<P20_1)) ^ 0FFh	;strip old value

			rrncf	R2,F,0								;put new bits in <7:6>
			rrncf	R2,F,0

			btfsc	LATA,0x07,0							;Was it at a high voltage?
			bra		vpp20q								;y: use hardware to yank it down

			iorwf	R2,W,0								;new control bits in place
			movwf	LATA,0

			return

;Pin 20 has a 0.047 uF cap on it, making discharge slow when changing from Vpp to +5V.
;If changing from either of the high voltages, uses the 7407 to yank it towards ground
;before setting it to 5V. The amount of time for this yank depends on Vpp.
vpp20q:		movwf	LATA,0							;Yank it to 0 first

			movlw	0x03							;(.25) (for 0.1 uF, use 0x06)
			btfss	CM1CON0,C1CH1,0					;(.25)this bit 0 for 21V and 25V
			movlw	0x04							;(.25) (for 0.1 uF, use 0x08)

			call	K_STALL1U						;delay as required

			movf	R2,W,0							;(.25)new control bits in place
			iorwf	LATA,F,0						;Vpp to +5V or ground
			return

;Vpp is on pin 18
vpp_pin18:	movf	LATA,W,0							;(.25)get other port bits
			andlw	((1<<P18_0) + (1<<P18_1)) ^ 0FFh	;(.25)strip old value

			rlncf	R2,F,0								;(.25)put new bits in <2:3>
			rlncf	R2,F,0								;(.25)

			iorwf	R2,W,0								;(.25)new control bits in place
			movwf	LATA,0								;(.25)

			return										;(.5)

;*************************************************************
; The above jump table must not span a 256-byte page boundary.
;*************************************************************
	if high(v2ptab)^ high(v2ptend)
	error "Martin sez: vpp2pin jump table spans a page boundary"
	endif

;*==============================================================================*
; Subroutine to send -OE signal to its correct pin
; On Entry:
;   W = 0 for -OE = inactive (5V)
;     = 1 for -OE = active(0V)
;     = 2 for -OE = programming-state
;   Note that -OE programming-state is +5V for most EPROMS, but +12V for some.
;   Hardware supports +12V OE signal only on pins 19 and 20. This is not
;   checked here.
;   Also, -OE may be the same pin as Vpp. In this case, set the pin to Vpp
;   if W=2.
;   ETYPE is the current EPROM type
; On Exit:
;   R1 = pin number - 17, with 0 meaning none
; Trashes W,R0,R3,EEADR,TBLPTR
; (14 uS)
; (1 us from pin-write through return, except if setting to +12V or Vpp)
;*==============================================================================*
oen2pin:	movwf	R3,0							;(.25)input in place for set_epin

			btg		R3,0x00,0						;(.25)-OE is an active-low pin
													;..so toggle the lsb of the input

			movlw	ET_OECS							;(.25)get -OE and CS pin specifications
			rcall	etype_getval					;(9.25+.5)returns -OE pin specification in R0<6:4>
			swapf	R0,W,0							;(.25)put the -OE pin number in W<2:0>

			btfss	R3,0x01,0						;(.25)R3<1> set if this is programming state
			bra		set_epin						;(3.5+.5)just set a normal (digital) -OE pin

;Set -OE to its programming level. There are 4 possibilities, depending on the EPROM:
; 1) -OE is also non-pulsed Vpp, and so gets set to the active Vpp voltage
; 2) -OE is also pulse Vpp, and so gets set to the inactive Vpp voltage
; 3) -OE is just a normal pin, and so gets set high (inactive)
; 4) -OE gets set to +12V
			andlw	0x07							;strip upper bits for pin number
			movwf	R1,0							;save pin number for return

			movlw	0x02							;(.25)programming-inactive for vpp2pin
			btfsc	FLAGS2,PMODE1,0					;(.25)0 means pulsed Vpp
			movlw	0x03							;(.25)programming-active for vpp2pin

			btfsc	SHARED,OEVPP					;(.5)is -OE pin also the Vpp pin?
			bra		vpp2pin							;y:set Vpp to its prog level again (R3=3)

			movf	R1,W,0							;(.25)Pin number again

			btfss	R0,0x07,0						;(.25)is -OE's programming-state +12V?		
			bra		set_epin						;(3.5+.5)n: just set the pin high

;Set -OE (Must be on pin 19 or 20) to +12V for active level during programming this type of EPROM
			xorlw	0x02							;pin 19 (encoded as 0x02)?
			bnz		oen2pin_20						;n: assume pin 20

;-OE is on pin 19, and wants to be +12V now
			movf	LATA,W,0
			andlw	((1<<P19_0)+(1<<P19_1)) ^ 0xFF	;strip off previous setting
			iorlw	(1<<P19_1)						;make it +12V
			movwf	LATA,0
			return

;-OE is on pin 20, and wants to be +12V now
oen2pin_20:	movf	LATA,W,0
			andlw	((1<<P20_0)+(1<<P20_1)) ^ 0xFF	;strip off previous setting
			iorlw	(1<<P20_1)						;make it +12V
			movwf	LATA,0
			return

;*==============================================================================*
; Subroutine to send PGM signal to its correct EPROM pin, with the correct
; polarity
; On Entry:
;   W = 0 if the pin is to be inactive
;     = 1 if the pin is to be active
; On Exit:
;   R1 = pin number - 17, with 0 meaning none
; Trashes W,R0,R3,EEADR,TBLPTR
; (15.5 uS through to pin write)
; (0.5 uS from pin write through return)
;*==============================================================================*
pgm2pin:	movwf	R3,0					;(.25)input in place for set_epin

			movlw	ET_PGMVPP				;(.25)PGM and VPP pin specifications
			rcall	etype_getval			;(9.75+.5)returns PGM pin specification in W & R0 bits <7:4>

;Invert W bit 0 if the PGM active-high bit is not set
			btfss	R0,0x07,0				;(.25)active-low pin?
			btg		R3,0x00,0				;(.25)active-low: invert bit					

			swapf	R0,W,0					;(.25)put the PGM pin-number bits in W<2:0>
			bra		set_epin				;(.5+3.5)pin number from W, return from there

;*==============================================================================*
; Subroutine to send -AS signal to its correct EPROM pin
; On Entry:
;   W = 0 if the pin is to be inactive (5V)
;     = 1 if the pin is to be active (0V)
; On Exit:
;   R1 = pin number - 17, with 0 meaning none
; Trashes W,R0,R3,EEADR,TBLPTR
; (11.75 uS min, if no -AS pin)
;*==============================================================================*
asn2pin:	movwf	R3,0					;(.25)input in place for set_epin

			movlw	ET_A11A12				;(.25)A11 and A12 pin specifications
			rcall	etype_getval			;(9.5+.5)returns PGM pin specification in W & R0 bits <7:4>

;A12 is really -AS if bit 3 is set
			clrf	R1,0					;(.25)in case this is A12
			btfss	R0,0x03,0				;(.25)-AS pin?
			return							;(.5)no: nothing to do
					
			btg		R3,0x00,0				;-AS is always active low, so invert

;Fall into  set_epin

;*==============================================================================*
; Subroutine to set a pin to a digital level: 0V or +5V
; On Entry:
;   W bits <2:0> identify the number of the pin to set:
;     0: none
;     1: pin 18
;     2: pin 19
;     3: pin 20
;     4: pin 21
;     5: pin 22
;     6: pin 23
;     7: illegal
;   R3<0> = 0 sets the pin to 0V, R3<0>=1 sets the pin to +5V
; This subroutine assumes a legal value for the given pin in W
; On Exit:
;   R1 = pin number - 17, with 0 meaning the pin does not exist
;   R3 has been rotated right 1 position
;   The correct output port bit has been set to the requested digital value
; Trashes W
; (3.5 uS) (.5 uS from pin-write through return)    
;*==============================================================================*
set_epin:		andlw	0x07				;(.25)just the pin assignment bits
				movwf	R1,0				;(.25)save pin number for return

;7-way jump based on R1, the pin number
epintab:		movf	PCL,W,0				;(.25)force a load of the PCLATH register (page 62)
				rlncf	R1,W,0				;(.25)command index (times 2 for jump table)
				addwf	PCL,F,0				;(.25)

				bra		epin_none			;if no pin defined for this signal
				bra		epin18				;(.25)
				bra		epin19
				bra		epin20
				bra		epin21
				bra		epin22
eptend:										;fall through to epin23

;Specified signal is on pin 23 (which has inverting hardware)
epin23:			rrcf	R3,F,0				;test
				bnc		epin23_low

				bcf		LATC,P23n,0			;bit is high (and inverted)
				return

epin23_low:		bsf		LATC,P23n,0			;bit is low (and inverted)
				return

;Specified signal is on pin 22 (which has inverting hardware)
epin22:			rrcf	R3,F,0				;test
				bnc		epin22_low

				bcf		LATB,P22n,0			;bit is high (and inverted)
				return

epin22_low:		bsf		LATB,P22n,0			;bit is low (and inverted)
				return
		
;Specified signal is on pin 21 (which is capable of other voltages too)
epin21:			movf	LATE,W,0
				andlw	((1<<P21_0)+(1<<P21_1)) ^ 0xFF	;strip off previous setting
				rrcf	R3,F,0							;test
				btfsc	STATUS,C,0
				iorlw	(1<<P21_0)						;y: make it 1
				movwf	LATE,0
				return

;Specified signal is on pin 20 (which is capable of other voltages too)
epin20:			movf	LATA,W,0
				andlw	((1<<P20_0)+(1<<P20_1)) ^ 0xFF	;strip off previous setting
				rrcf	R3,F,0							;test
				btfsc	STATUS,C,0
				iorlw	(1<<P20_0)						;y: make it 1
				movwf	LATA,0
				return

;Specified signal is on pin 19 (which is capable of other voltages too)
epin19:			movf	LATA,W,0
				andlw	((1<<P19_0)+(1<<P19_1)) ^ 0xFF	;strip off previous setting
				rrcf	R3,F,0							;test
				btfsc	STATUS,C,0
				iorlw	(1<<P19_0)						;y: make it 1
				movwf	LATA,0
				return

;Specified signal is on pin 18 (which is capable of other voltages too)
epin18:			movf	LATA,W,0						;(.25)
				andlw	((1<<P18_0)+(1<<P18_1)) ^ 0xFF	;(.25)strip off previous setting
				rrcf	R3,F,0							;(.25)test
				btfsc	STATUS,C,0						;(.25)
				iorlw	(1<<P18_0)						;(.25)y: make it 1
				movwf	LATA,0							;(.25)
				return									;(.5)

epin_none:		rrcf	R3,F,0
				return

;*************************************************************
; The above jump table must not span a 256-byte page boundary.
;*************************************************************
	if high(epintab)^ high(eptend)
	error "Martin sez: epin jump table spans a page boundary"
	endif

;*==============================================================================*
; Subroutine to send stuck-high signal to its correct EPROM pin
; On Entry:
;   W = 0 if the pin is to be low
;     = 1 if the pin is to be high
; On Exit:
;   R1 = pin number - 17, with 0 meaning none
; Trashes W,R0,R3,EEADR,TBLPTR
;*==============================================================================*
hi2pin:		movwf	R3,0					;save input

			movlw	ET_A11A12				;stuck high shares definition wih A11
			call	etype_getval			;result in W & R0

;does the stuck-high pin exist? Of not, done with R1=0
			clrf	R1,0					;indicate non-existance
			btfss	R0,0x07,0				;forced-high pin?
			return							;y:return with R1=0

;get the stuck-high pin number and set it according to R3
			swapf	R0,W,0					;<6:4> are the A11/stuck-high pin assignment
			bra		set_epin				;W=pin number, R3=state

;*==============================================================================*
; Subroutine to send CS signal to its correct EPROM pin, with the correct
; polarit
; Note that the CS pin may also be the Vpp pin or PGM pin, not dealt with here.
; On Entry:
;   W = 0 if the pin is to be inactive
;     = 1 if the pin is to be active
; On Exit:
;   R1 = pin number - 17, with 0 meaning none
; Trashes W,R0,R3,EEADR,TBLPTR
; (15.25 uS through to pin write)
; (0.5 uS from pin write through return)
;*==============================================================================*
cs2pin:		movwf	R3,0					;(.25)input in place for set_epin

			movlw	ET_OECS					;(.25)WE and -CS pin specifications
			rcall	etype_getval			;(9.75+.5)returns WE pin specification in W & R0 bits <3:0>

;Invert W bit 0 if the CS active-high bit is not set
			btfss	R0,0x03					;(.25)test active-high bit
			btg		R3,0x00,0				;(.25)active-low: invert bit	

			bra		set_epin				;(.5+3.5)pin number from W, return from there

;*==============================================================================*
;* Subroutine write an address to the EPROM pins
; On Entry:
;*  ADDRESSH:ADDRESSL = desired address
;   ETYPE = currently selected EPROM type
;   LATE.ADRCLK is either high or low, depending on Busy state
;   The data buffer is either directing data to the EPROM or
;   its output is disabled.
; On Exit:
;   Port D is tristated
;   The external data buffer is tristated
; Trashes W,R0,R1,R2,R3,FSR0,TBLPTRH:TBLTRL,REEADR
; (about 40.25 uS)
;*==============================================================================*
address2pins:	movff	ADDRESSH,R3			;(.25)high-order address bits first
											;(R3 gets rotated on each call to set_epin)
;write A8, which is always at LATC.P23n
				movlw	.23-.17				;(.25)specify pin 23 (always for A8)
				rcall	set_epin			;(3.5)

;Look up the pin positions of A9 & A10
				movlw	ET_A9A10			;(.25)
				rcall	etype_getval		;(9.5+.5)result in W & R0

;Put A9 signal on its correct pin for this EPROM
				swapf	R0,W,0				;(.25)<6:4> are the A9 pin assignment
				rcall	set_epin			;(3.5)(preserves R0, shifts R3 right)

;Put A10 signal on its correct pin for this EPROM.
				movf	R0,W,0				;(.25)<2:0> are the A10 pin number
				rcall	set_epin			;(3.5)(shifts R3 right)

;Get A11 & A12 positions. Note that A11 may be forced high by the EPROM definition.
;(This is to support EPROMs that have a second -CS pin.), and the A12 bits may
;instead describe a negative address strobe pin.
				movlw	ET_A11A12			;(.25)
				rcall	etype_getval		;(9.5+.5)result in W & R0

;Put A11 signal on its correct pin for this EPROM, unless it's the forced-high bit
				btfsc	R0,0x07,0			;(.5)forced-high pin?
				bsf		R3,0x00,0			;y: force it high

				swapf	R0,W,0				;(.25)<6:4> are the A11 pin assignment
				rcall	set_epin			;(3.5)(preserves R0, shifts R3 right)

;Put A12 signal on its correct pin for this EPROM unless it's the -AS bit
				movf	R0,W,0				;(.25)<2:0> are A12

				btfss	R0,0x03,0			;(.5)is this signal really tne -AS pin?
				rcall	set_epin			;(3.5)not -AS, so it is A12

;Write the low address byte into the external data latch. This is done last,
;because one or two of the high-address pins may have a 0.047 uF capacitor, and
;therefore be slower to transition.
				bsf		LATB,DBUFOEn		;(.25)disable data buffer output

				clrf	TRISD,0				;(.25)un-tristate port D
				movff	ADDRESSL,PORTD		;(.5)put low address on data bus

				btg		LATE,ADRCLK,0		;(.25) togle the clock one way
				btg		LATE,ADRCLK,0		;(.25) and then the other way

				setf	TRISD,0				;(.25)tristate on the data bus again

				return						;(.5)

;*==============================================================================*
; Subroutine to look up an EPROM type in the EPROM table, and get the value of
; a record item for that EPROM type. Note that the table of built-in EPROM types
; is in Flash memory, while the table of custom EPROM types are in EEPROM. The
; formats for these two tables are slightly different.
; On Entry:
;   W = desired record item
;   ETYPE (in EEPROM) = EPROM type
; On Exit:
;   W = R0 = specified value from EPROM table
;   If specified EPROM is in flash:
;      FLAGS2.CUST_EPROM is cleared
;      TBLPTRH:TBLPTRL = address of the desired item within the desired
;        EPROM record
;   If specified EPROM is in EEPROM (a custom EPROM):
;      FLAGS2.CUST_EPROM is set
;      EEADR = address of the desired item within the desired EPROM record
; Trashes R1
; (9.75 uS)
;*==============================================================================*
etype_getval:	rcall	etype_lookup			;(7.25+.5)

;Look up value in Flash table or EEPROM table, depending on CUST_EPROM
				btfsc	FLAGS2,CUST_EPROM,0		;(.5)custom EPROM (so the record is in EEPROM)?
				bra		egv_cust

;Flash table lookup
				tblrd*							;(.5)n: get flash table value
				movf	TABLAT,W,0				;(.25)

				movwf	R0,0					;(.25)into R0 for return
				return							;(.5)return with flash table value

;EEPROM table lookup
egv_cust:		call	K_RDEEPROM				;read value from EEPROM
				movwf	R0,0
				return

;*==============================================================================*
; Subroutine to look up an EPROM type in the EPROM table. Note that the table of
; built-in EPROM types is in Flash memory, while the table of custom EPROM types
; are in EEPROM.
; On Entry:
;   W = desired record item
;   ETYPE (in EEPROM) = EPROM type
; On Exit:
;   If specified EPROM is in flash:
;      FLAGS2.CUST_EPROM is cleared
;      TBLPTRH:TBLPTRL = address of the desired item within the desired EPROM record
;   If specified EPROM is in EEPROM (a custom EPROM):
;      FLAGS2.CUST_EPROM is set
;      W= EEADR = address of the desired item within the desired EPROM record
; Trashes W,R0,R1,PRODH:PRODL
; (7.25 uS)
;*==============================================================================*
etype_lookup:	movwf	R1,0				;(.25)remember record item
				movlw	ETYPE				;(.25)EEPROM address of variable
				call	K_RDEEPROM			;(1.5 +.5 )W = EPROM type

;Is this a custom EPROM, or one from the inbuilt table?
				movwf	R0,0				;(.25)temp save ETYPE
				movlw	FEPROM_COUNT		;(.25)
				subwf	R0,W,0				;(.25)W:= ETYPE - FEPROM_COUNT
											; carry cleared if  ETYPE < FEPROM_COUNT
				bc		cust_lookup			;(.25)

;Look up EPROM type in FLASH table. This will be a 16-bit FLASH address.
flookup:		movf	R0,W,0				;(.25)
				mullw	ET_RLENGTH			;(.25)
				movf	PRODL,W,0			;(.25)
				addlw	low(eprom_table)	;(.25)
				movwf	TBLPTRL,0			;(.25)
				movlw	high(eprom_table)	;(.25)
				addwfc	PRODH,W,0			;(.25)
				movwf	TBLPTRH,0			;(.25)TPLPTRH:TBLPTRL = record address

				movf	R1,W,0				;(.25)add in the item
				addwf	TBLPTRL,F,0			;(.25)
				btfsc	STATUS,C,0			;(.25)
				incf	TBLPTRH,F,0			;(.25)

				bcf		FLAGS2,CUST_EPROM,0	;(.25)EPROM is not custom
				return						;(.5)

;Look up EPROM in custom table in EEPROM. W = ETYPE - FEPROM_COUNT here. Note that
;the EEPROM is only 256 bytes long, so there will never be an address high byte.
cust_lookup:	mullw	ET_RLENGTH

				movf	PRODL,W,0			;offset to record
				addlw	CE_TABLE			;W = address of record in EEPROM
				addwf	R1,W,0				;W=address of item with record
				movwf	EEADR,0

				bsf		FLAGS2,CUST_EPROM,0	;This is a custom EPROM
				return

;*==============================================================================*
; Subroutine to print the programming voltage as seen at the pin, accounting
; for the diode-drop difference between Vpp at pin 18 versus Vpp at pins 19-21
; On Entry:
;   ETYPE is current EPROM type
;   R2 = pin number minus 17
;   R0 = ET_VPPSETUP for the current EPROM type
; Trashes W,R0,R1,R2, FSR0
;===============================================================================*
pvpp_voltage:	swapf	R0,W,0				;get the programming voltage bits
				andlw	0x07
				movwf	R1,0

				lfsr	0,fail_msg-sstrings	;flag bad table value

;If Vpp is on pin 18 then it will be one diode drop (about 0.85V) higher,
;requiring different messages
				decf	R2,W,0				;test for R2=1, meaning pin 18
				bnz		pvpp_notpin18

				dcfsnz	R1,F,0
				lfsr	0,v1340_msg-sstrings

				dcfsnz	R1,F,0
				lfsr	0,v1385_msg-sstrings

				dcfsnz	R1,F,0
				lfsr	0,v2170_msg-sstrings

				dcfsnz	R1,F,0
				lfsr	0,v2590_msg-sstrings

				decf	R1,F,0					;External supply?
				bnz		pvpp_printv

;External negative Vpp.
				lfsr	0,extneg_msg-sstrings	;"external supply"
				btfss	STATUS,C,0
				bra		cr_printf

;Note that an external supply is only allowed on pin 18.

pvpp_notpin18:	dcfsnz	R1,F,0
				lfsr	0,v1270_msg-sstrings

				dcfsnz	R1,F,0
				lfsr	0,v1315_msg-sstrings

				dcfsnz	R1,F,0
				lfsr	0,v21_msg-sstrings

				dcfsnz	R1,F,0
				lfsr	0,v2510_msg-sstrings

;Custom Vpp voltage?
pvpp_printv:	movlw	ET_SPECIAL
				rcall	etype_getval
				andlw	0x02				;test custom Vcc bit
				btfsc	STATUS,Z,0
				bra		printf				;n: print the programming voltage & ret

				movlw	ET_VPPMSG
				clrf	R2,0				;no formatting
				bra		eds_print			;print the adjustment message & return

;################################################################################
;#                       Custom EPROM Editor Subsystem                          #
;################################################################################

;*****------------------------------------------------------------------------*
;* ?A * Custom EPROM Editor Command: Display Pin Assignment Commands
;*****
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_ahelp:		movlw	low(eamenu_msg)
				movwf	TBLPTRL,0
				movlw	high(eamenu_msg)
				movwf	TBLPTRH,0
				goto	K_PRINTF				;return from there

;*****------------------------------------------------------------------------*
;* ? * Custom EPROM Editor Command: Re-display Eprom Editor Commands
;*****
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_eehelp:	movlw	low(eemenu_msg)			;Print the EPROM editor menu
				movwf	TBLPTRL,0
				movlw	high(eemenu_msg)
				movwf	TBLPTRH,0
				call	K_PRINTF

;Tell user to name the EPROM if it's not already named
				movlw	ET_NAME					;point EEADR to the name's place
				rcall	etype_getval			;look at 1st chr of name
				lfsr	0,gname_msg-sstrings	;"EPROM is unassigned until it is named (EN)"
				iorlw	0x00					;no name?
				btfsc	STATUS,Z,0
				call	cr_printf

				return

;******-------------------------------------------------------------------------*
;* QU * Custom EPROM Editor Command: Quit
;******
; On Entry:
;   ETYPE_SAVE = original EPROM type
;-------------------------------------------------------------------------------*
ecmd_quit:		rcall		restore_etype		;restore EPROM type
				rcall		etype_setup			;in case we changed the current type
				goto		main				;rude return

;******-------------------------------------------------------------------------*
;* EN * Custom EPROM Editor Command: Name EPROM
;******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_epname:	movlw	ET_NAME			;point EEADR to the name's place
				rcall	etype_lookup

				movlw	MAXNAME+1		;max allowed characters
				movwf	R3,0

epn_loop:			movlw	0x00
					decfsz	R3,F,0			;too many characters?
					call	K_GETCHR		;n: get another from the user
											;..into W & R0

					xorlw	' '				;terminator?
					btfss	STATUS,Z,0		;y: make it a null
					xorlw	' '
					movwf	R2,0			;put it back, space converted to null

					call	K_WREEPROM
					incf	EEADR,F,0

					movf	R2,F,0			;done?
					bnz		epn_loop	

				return

;******=========================================================================*
;* EE * Invoke Custom EPROM Editor
;******
; On Entry:
;	DFLT_FLAG set if no parameters came with this command
;   ADDRESSL = EPROM number
;===============================================================================*
;remember actual current EPROM type
do_ceedit:		rcall	save_etype				;save original ETYPE while we work
												;..and set EEADR to ETYPE

				btfsc	FLAGS1,DFLT_FLAG,0		;any user input?
				movwf	ADDRESSL,0				;n: current EPROM is the choice

;Validate that the requested EPROM type is a custom EPROM type
				lfsr	0,cantedit_msg-sstrings
				movlw	FEPROM_COUNT			;Trying to edit an inbuilt EPROM?
				subwf	ADDRESSL,W,0			;W:= input value - FEPROM_COUNT
												;carry cleared if input value < FEPROM_COUNT

				btfss	STATUS,C,0
				bra		cr_printf				;error if attempting to edit inbuilt EPROM

				sublw	CEPROM_COUNT-1			;too big?
				btfss	STATUS,C,0				;Carry set if ADDRESSL <= max allowed EPROM
				goto	cmd_error

;Set requested EPROM to be the current one for now
				movf	ADDRESSL,W,0			;EEADR is already set up
				call	K_WREEPROM				;requested EEPROM is current for now

;Display list of custom EPROM editor commands
				rcall	ecmd_eehelp

;---------------------------------------------------------------------------------
; Print the special EPROM Editor prompt on a new line and get a command
;---------------------------------------------------------------------------------
ee_main:		rcall	go_ee_main				;push return address onto the stack
				bra		ee_main

go_ee_main:		lfsr	0,eeprompt_msg-sstrings
				rcall	cr_printf
				movf	ADDRESSL,W,0			;put EPROM number in the prompt
				call	K_PRINTHEX2
				movlw	PROMPTCHR
				call	K_CONOUT

;Re-calculate the EPROM size every time we pass through this loop
;The size is determined by the highest (contiguous) assigned address pin.
				movlw	0x01
				movwf	R4,0					;assume 512 bytes for now

				movlw	ET_A9A10
				rcall	etype_getval			;result in W and R0
				andlw	0x70					;any A9 pin asigned?
				bz		size_done				;n: it's 512 bytes
				incf	R4,F,0					;y: it's at least 1K bytes

				movf	R0,W,0
				andlw	0x07					;any A10 pin assigned?
				bz		size_done				;n: it's 1K bytes
				incf	R4,F,0					;y: it's at least 2K bytes

				movlw	ET_A11A12
				rcall	etype_getval			;result in W & R0
				andlw	0x70					;any A11 pin asigned?
				bz		size_done				;n: it's 2K bytes
				incf	R4,F,0					;y: it's at least 4K bytes

				movf	R0,W,0
				andlw	0x07					;any A12 pin assigned?
				bz		size_done				;n: it's 4K bytes
				incf	R4,F,0					;y: it's 8K bytes

size_done:		movlw	ET_BYTES
				rcall	etype_getval			;result in W & R0, set EEADR to ET_BYTES

				movlw	0x80					;we only support 8 bits/byte now
				addwf	R4,W,0					;compute new reg value

				cpfseq	R0,0					;any changes to this byte?
				call	K_WREEPROM				;y: remember the new byte size in ET_BYTES

;Get a complete, CR-terminated line into the line buffer
				movlw	MAXLIN-1				;max input line size
				call	K_GETLIN				;get a line from the user into the LINEBUF
				bz		go_ee_main				;no input, ^c or ESC: start over

;Search EPROM Editor command table for match, get its command index into R4
				movlw	high(ee_cmds)
				movwf	TBLPTRH,0
				movlw	low(ee_cmds)
				movwf	TBLPTRL,0

				call	K_PARSE					;returns W=command index

;----------------------------------------------------------------------------------			
; Go execute an EPROM Editor command, if it exists
; W = command index, 0 means not found
; All of these command-processing routines return to ee_main when done
;----------------------------------------------------------------------------------			
jmptab2:	movff	PCL,R0				;force a load of the PCLATH register (page 62)
			addwf	PCL,F,0

			bra		ecmd_error			;command not found
			goto	cmd_tprog			;TPROG
			goto	cmd_tread			;TREAD
			bra		ecmd_deleprom		;DEP
			bra		ecmd_dispeprom		;ED
			bra		ecmd_epname			;EN
			bra		ecmd_quit			;QU
			bra		ecmd_eehelp			;?
			bra		ecmd_vbb			;VBB
			bra		ecmd_vdd			;VDD
			bra		ecmd_a9				;A9
			bra		ecmd_a10			;A10
			bra		ecmd_a11			;A11
			bra		ecmd_a12			;A12
			bra		ecmd_oen			;OEN
			bra		ecmd_cs				;CS
			bra		ecmd_csp			;CSP
			bra		ecmd_pgm			;PGM
			bra		ecmd_pgp			;PGP
			bra		ecmd_ppp			;PPP
			bra		ecmd_vcp			;VCP
			bra		ecmd_oev			;OEV
			bra		ecmd_vpp			;VPP
			bra		ecmd_vpr			;VPR
			bra		ecmd_pul			;PUL
			bra		ecmd_ptu			;PTU
			bra		ecmd_ppw			;PPW
			bra		ecmd_pps			;PPS
			bra		ecmd_pol			;POL
			bra		ecmd_spa			;SPA
			bra		ecmd_fp1			;FP1
			bra		ecmd_fn1			;FN1
			bra		ecmd_fp2			;FP2
			bra		ecmd_fn2			;FN2
			bra		ecmd_pmx			;PMX
			bra		ecmd_ff1			;FF1
			bra		ecmd_asn			;ASN
			bra		ecmd_hi				;HI
			bra		ecmd_bck			;BCK
			bra		ecmd_copy			;COP
			bra		ecmd_ahelp			;?A

jmpend2:	;fall into ecmd_phelp for ?P

;*****------------------------------------------------------------------------*
;* ?P * Custom EPROM Editor Command: Display Programming Parameter Commands
;*****
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_phelp:		movlw	low(epmenu_msg)
				movwf	TBLPTRL,0
				movlw	high(epmenu_msg)
				movwf	TBLPTRH,0
				goto	K_PRINTF				;return from there

;********************************************************************************
; The above jump table must not span a 256-byte page boundary.
;********************************************************************************
	if high(jmptab2)^ high(jmpend2)
	error "Martin sez: EPROM Editor command jump table spans a page boundary"
	endif

;*******------------------------------------------------------------------------*
;* DEP * Custom EPROM Editor Command: Delete EPROM
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_deleprom:	movlw	ET_NAME					;clear this to delete an EPROM
				rcall	etype_lookup			;set EEADR to ET_NAME

				lfsr	0,delce_msg-sstrings	;"Delete EPROM"
				rcall	cr_printf
				call	K_ASKYN
				bnz		edel_done				;did user say yes?

				movlw	ET_RLENGTH				;bytes per EPROM record
				movwf	R3,0

;Loop to clear out custom EPROM, starting at ET_NAME
edel_loop:			movlw	0x00
					call	K_WREEPROM
					incf	EEADR,F,0
					decf	R3,F,0
					bnz		edel_loop

edel_done:		return

;*******------------------------------------------------------------------------*
;* VBB * Custom EPROM Editor Command: Define Vbb pin
;*******
; VBB <0/1> 0 means none
;           1 means -5V on pin 21
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_vbb:		movlw	ET_VBBVDD
				rcall	etype_getval		;get Vcc, Vbb & Vdd setup
											;..and set ETYPE to ET_VBBVDD

				andlw	0xEF				;strip off old Vbb
				movwf	R5,0				;save for end

				movlw	0x01				;allow only allow 0-1
				rcall	ee_getdec			;result in W & R0, trash R1. Z set if 1
				bnz		evbb_none			;no Vbb pin?

;Vbb is always on pin 21. If any other signals are assigned to pin 21, remove them
				movlw	0x04				;pin 21 encoding
				movwf	R3,0
				rcall	dup_delete

				bsf		R5,0x04,0			;enable Vbb

evbb_none:		movf	R5,W,0
				goto	K_WREEPROM			;install new ET_VBBVDD value, return

;*******------------------------------------------------------------------------*
;* VDD * Custom EPROM Editor Command: Define Vdd pin
;*******
; VDD <0/1> 0 means none
;             1 means +12V on pin 19
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_vdd:		movlw	ET_VBBVDD
				rcall	etype_getval		;get Vcc, Vbb & Vdd setup
											;..and set EEADR to ET_VBBVDD
				andlw	0xDF				;strip off old Vdd
				movwf	R5,0				;save for end

				movlw	0x01				;allow only allow 0-1
				rcall	ee_getdec			;result in W & R0, trash R1. Z set if 1
				bnz		evdd_none			;no Vbb pin?

;Vbb is always on pin 19. If any other signals are assigned to pin 19, remove them
				movlw	0x02				;pin 21 encoding
				movwf	R3,0
				rcall	dup_delete

				bsf		R5,0x05,0			;enable Vdd

evdd_none:		movf	R5,W,0
				goto	K_WREEPROM			;install new ET_VBBVDD value, return

;******-------------------------------------------------------------------------*
;* A9 * Custom EPROM Editor Command: assign pin number to A9 signal
;******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; See comment at assignp for exit details
;-------------------------------------------------------------------------------*
ecmd_a9:	movlw	ET_A9A10
			rcall	etype_lookup		;point EEADR to the correct byte

			bra		assignp_hi

;*******------------------------------------------------------------------------*
;* A10 *  Custom EPROM Editor Command: assign pin number to A10 signal
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; Note that A10 shares its definition with the stuck-low signal - an EPROM
; with a stuck-low signal cannot also have an A10 pin and vice versa. Assigning
; a pin to be A10 will unassign any stuck-low signal.
; See comment at assignp for exit details
;-------------------------------------------------------------------------------*
ecmd_a10:	movlw	ET_A9A10
			rcall	etype_getval		;sets EEADR to ET_A9A10 too
			andlw	0xF7				;remove stuck-low indicator bit
			call	K_WREEPROM

			bra		assignp_lo			;creates mask=0xF8

;*******------------------------------------------------------------------------*
;* A11 *  Custom EPROM Editor Command: assign pin number to A11 signal
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; Note that A11 shares its definition with the stuck-high signal - an EPROM
; with a stuck-high signal cannot also have an A11 pin and vice versa. Assigning
; a pin to be A11 will unassign any stuck-high signal.
; See comment at assignp for exit details
;-------------------------------------------------------------------------------*
ecmd_a11:	movlw	ET_A11A12
			rcall	etype_getval		;sets EEADR to ET_A11A12 too

			andlw	0x7F				;remove stuck-high indicator bit
			call	K_WREEPROM

			bra		assignp_hi			;creates mask=0x8F

;*******------------------------------------------------------------------------*
;* A12 *  Custom EPROM Editor Command: assign pin number to A12 signal
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; Note that A12 shares its definition with the -AS signal - an EPROM with a -AS
; signal cannot also have an A12 pin and vice versa. Assigning a pin to be A12
; will unassign any -AS signal.
; See comment at assignp for exit details
;-------------------------------------------------------------------------------*
ecmd_a12:	movlw	ET_A11A12
			rcall	etype_getval		;sets EEADR to ET_A11A12 too

			andlw	0xF7				;remove -AS indicator bit
			call	K_WREEPROM

			bra		assignp_lo			;creates mask=0xF8

;*******------------------------------------------------------------------------*
;* ASN *  Custom EPROM Editor Command: assign pin number to -AS signal
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; Note that A12 shares its definition with the -AS signal - an EPROM with a -AS
; signal cannot also have an A12 pin and vice versa. Assigning a pin to be -AS
; will unassign any A12 signal.
; See comment at assignp for exit details
;-------------------------------------------------------------------------------*
ecmd_asn:	movlw	ET_A11A12			;-ASN shares pin definition with A12
			rcall	etype_getval		;sets EEADR to ET_A11A12 too

			iorlw	0x08				;set -AS indicator bit
			call	K_WREEPROM

			bra		assignp_lo			;creates mask=0xF8

;******-------------------------------------------------------------------------*
;* HI *  Custom EPROM Editor Command: Define stuck-high pin
;******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; Note that A11 shares its definition with the stuck-high signal - an EPROM
; with a stuck-high signal cannot also have an A11 pin and vice versa. Assigning
; a pin to be stuck-high will unassign any A11 signal.
; See comment at assignp for exit details
;-------------------------------------------------------------------------------*
ecmd_hi:	movlw	ET_A11A12			;stuck-high shares pin definition with A11
			rcall	etype_getval		;sets EEADR to ET_A11A12 too

			iorlw	0x80		 		;set stuck-high bit
			call	K_WREEPROM

;fall into assignp_hi

;-------------------------------------------------------------------------------*
; EPROM Editor Subroutine for EPROM to assign a pin number to an EPROM signal
; 1) Hunt for any other signals that have the same pin number as the one we
; are about to assign. 
; 2) If we find any, then sets them to 0, meaning that signal is now unassigned.
; 3) Install the new pin number into the signal specified on entry
; On Entry at assignp_hi or assignp_lo:
;   EEADR = address of entry
;   New pin number is still in the line buffer (to be retrieved by get_pin)
; On Entry at assignp:
;   EEADR = target signal's pin definition address
;   R3 = new pin number from user
;   R5 = pin mask
; On Exit (valid entry):
;   Any other signals with the provided pin number are now unassigned
;   The specified signal has the specified pin number
;   An invalid pin number prints the command error message and returns to the
;   EPROM Editor loop.
; Trashes W, R0-R6.
;-------------------------------------------------------------------------------*
assignp_hi:	movlw	0x8F				;high-nibble mask
			bra		assignp_hl

assignp_lo:	movlw	0xF8				;low-nibble mask

assignp_hl:	movwf	R5,0				;R5=pin mask

			rcall	get_pin				;get W=R3=pin number from user (trashes R0)

assignp:	movff	EEADR,R4			;R4=pin-pair address for the pin we are changing

;hunt for any pins that have the same pin number as this new one, and clear them
;   R3 = new pin number from user, in one nibble or the other
;   R4 = EEPROM address for the pin we will eventually write
;   R5 = pin mask, either 0x8F or 0xF8 (or 0F...)

			rcall	dup_delete

;install the new pin number in the correct place
;R3 = new pin number, replicated into both nibbles
;R4 = EEPROM address for the particular pin we are writing
;R5 = nibble mask for the particular pin we are writing
			comf	R5,W,0				;get new pin number in correct nibble
			andwf	R3,F,0				;..by stripping off the other nibble

			movf	R4,W,0				;restore pin-pair address
			call	K_RDEEPROM			;get 2 pin definitions
			andwf	R5,W,0				;strip old pin number for the one we are changing

			bra		binstall_r3			;Combine W and R3, install into
										;..EEPROM register, and return 

;*******------------------------------------------------------------------------*
;* OEN *  Custom EPROM Editor Command: assign pin number to -OE signal
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; Note: The -OE signal may be assigned to the same pin as the Vpp signal. In this
; case, it is not necessary to check for other signal-pin conflicts because they
; would have been checked when Vpp was assigned.
; Note also that the -OE signal may be programmed to be at +12V during
; programming, ony if it is assigned to pin 19 or 20 (hardware limitation).
; When -OE is assigned to a pin, this feature is cleared.
; See comment at assignp for exit details
;-------------------------------------------------------------------------------*
ecmd_oen:	rcall	get_pin				;get W=R3=new -OE pin number from user

;See if this new pin is also assigned to PGM, which is allowed.
			movlw	ET_PGMVPP			;Is this new -OE pin the same as PGM pin?
			rcall	etype_getval		;get OEN & CS pin assignment in W and R0
			movwf	R1,0				;temp save

			swapf	R0,W,0				;put PGM pins in <2:0>
			xorwf	R3,W,0				;-OE pin same as our new Vpp pin?
			andlw	0x07				;just the right pin bits
			bz		eoen_same			;y: no need to hunt for matches

;This new -OE pin number is not the same as the existing PGM pin number.
;Is it the same as  the VPP pin? This is also allowed.
			movf	R1,0				;VPP pin number in 2:0
			xorwf	R3,W,0				;VPP pin same as our new CS pin?
			andlw	0x07				;just the right pin bits
			bz		eoen_same			;y: no need to hunt for matches

;This new -OE pin number is not the same as the existing Vpp pin number either.
			movlw	ET_OECS
			rcall	etype_lookup		;set EEADR for -OE and CS pin assignment

			movlw	0x0F				;mask for -OE pin number (clears +12V bit too.)
			movwf	R5,0				;R5=pin mask

			bra		assignp				;check for duplicates, and assign

;The new -OE pin is the same as either PGM or Vpp, so no need to check for conflicts
eoen_same:	movlw	ET_OECS
			rcall	etype_getval		;get -OE and CS pin assignment
			andlw	0x0F				;strip old -OE assignmemt and +12V bit
			swapf	R3,F,0				;put new pin number in high nibble

			bra		binstall_r3			;Combine W and R3, install into
										;..EEPROM register, and return 

;******-------------------------------------------------------------------------*
;* CS *  Custom EPROM Editor Command: assign pin number to CS signal
;******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; See comment at assignp for exit details
;-------------------------------------------------------------------------------*
ecmd_cs:	rcall	get_pin				;get W=R3=new CS pin number from user

;See if this new pin is also assigned to PGM, which is allowed.
			movlw	ET_PGMVPP			;Is this new CS pin the same as PGM pin?
			rcall	etype_getval		;get OEN & CS pin assignment in W and R0
			movwf	R1,0				;temp save

			swapf	R0,W,0				;put PGM pins in <2:0>
			xorwf	R3,W,0				;CS pin same as our new Vpp pin?
			andlw	0x07				;just the right pin bits
			bz		ecs_same			;y: no need to hunt for matches

;This new CS pin number is not the same as the existing PGM pin number.
;Is it the same as  the VPP pin? This is also allowed.
			movf	R1,0				;VPP pin number in 2:0
			xorwf	R3,W,0				;VPP pin same as our new CS pin?
			andlw	0x07				;just the right pin bits
			bz		ecs_same			;y: no need to hunt for matches

;This new CS pin number is not the same as the existing Vpp pin number either.
			movlw	ET_OECS
			rcall	etype_lookup		;set EEADR for -OE and CS pin assignment

			movlw	0xF8				;low-nibble mask
			movwf	R5,0				;R5=pin mask

			bra		assignp				;check for duplicates, and assign

;The new CS pin is the same as either PGM or Vpp, so no need to check for conflicts
ecs_same:	movlw	ET_OECS
			rcall	etype_getval		;set EEADR for -OE and CS pin assignment
			andlw	0xF8				;remove old CS pin number

			bra		binstall_r3			;Combine W and R3, install into
										;..EEPROM register, and return 

;*******------------------------------------------------------------------------*
;* CSP * Custom EPROM Editor Command: Define CS signal polarity
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_csp:	movlw	ET_OECS
			rcall	etype_getval		;get -OE and CS pin assignment

			andlw	0xF7				;strip old CS polarity assignmemt
			movwf	R3,0				;temp save

			movlw	0x01				;allow only allow 0-1
			rcall	ee_getdec			;result in W & R0, trash R1, set Z if exactly max

			btfsc	STATUS,Z,0			;was value 0?
			bsf		R3,0x03,0			;n: set bit for positive polarity

			bra		install_r3			;install R3 into EEPROM register and return 

;*******------------------------------------------------------------------------*
;* PGM * Custom EPROM Editor Command: assign pin number to PGM signal
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; See comment at assignp for exit details
;------------------------------------------------------------------------------*
ecmd_pgm:	rcall	get_pin				;get W=R3=new PGM pin number from user

;See if this new pin is also assigned to OEN, which is allowed.
			movlw	ET_OECS				;Is this new PGM pin the same as OEN pin?
			rcall	etype_getval		;get OEN & CS pin assignment in W and R0
			movwf	R1,0				;temp save

			swapf	R0,W,0				;put OEN pins in <2:0>
			xorwf	R3,W,0				;OEN pin same as our new PGM pin?
			andlw	0x07				;just the right pin bits
			bz		epgm_same			;y: no need to hunt for matches

;This new PGM pin number is not the same as the existing OEN pin number.
;Is it the same as  the CS pin? This is also allowed.
			movf	R1,0				;CS pin number in 2:0
			xorwf	R3,W,0				;CS pin same as our new PGM pin?
			andlw	0x07				;just the right pin bits
			bz		epgm_same			;y: no need to hunt for matches

;This new PGM pin number is not the same as the existing -OE or CS pin number.
			movlw	ET_PGMVPP
			rcall	etype_lookup		;set EEADR for PGM & Vpp pin assignment
			movlw	0x8F				;mask for PGM pin number
			movwf	R5,0				;in place for assignp

			bra		assignp				;go install pin			

;This new PGM pin number matches the existing -OE or CS pin number, which is allowed.
epgm_same:	movlw	ET_PGMVPP
			rcall	etype_getval		;set EEADR for PGM & Vpp pin assignment
			andlw	0x8F				;remove old PGM pin number

			bra		binstall_r3			;Combine W and R3, install into
										;..EEPROM register, and return 
			
;*******------------------------------------------------------------------------*
;* PGP * Custom EPROM Editor Command: Define PGM signal polarity
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_pgp:	movlw	ET_PGMVPP
			rcall	etype_getval		;get PGM & Vpp pin assignment

			andlw	0x7F				;strip old Vpp pin polarity assignmemt
			movwf	R3,0				;temp save
			movlw	0x01
			rcall	ee_getdec			;result in W & R0, trash R1, Z set if exactly max

			btfsc	STATUS,Z,0			;was value 0?
			bsf		R3,0x07,0			;n: set bit for positive polarity

			bra		install_r3			;install R3 into EEPROM register and return 

;*******------------------------------------------------------------------------*
;* PPP * Custom EPROM Editor Command: assign pin number to Vpp signal
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; Note 1: Vpp may only be assigned to pins 18, 20, and 21. Attempting to
;   assign it to another pin will generate an error message instead.
; Note 2: The -OE signal or the CS signal may be assigned to the same pin as the
; Vpp signal. In these cases, it is not necessary to check for other signal-pin
; conflicts because they would have been checked when -OE was assigned.
; Note 3: the -OE signal may be programmed to be at +12V during programming,
;   but not if it is also the Vpp pin. If we are assigning Vpp to be on the same
;   pin as -OEN, then clear the flag that sets -OE to +12V during programming.
;   .
; When -OE is assigned to a pin, this feature is cleared.
;-------------------------------------------------------------------------------*
ecmd_ppp:	rcall	get_pin				;get W=R3=new Vpp pin number from user

;Vpp may not be on pins 19, 22, or 23. (R2 values 2, 5, 6) due to hardware limitations
			xorlw	0x02				;pin 19?
			bz		ppp_err
			movlw	0x05
			subwf	R3,W,0
			bc		ppp_err			

;See if this new pin is also assigned to -OE, which is allowed.
			movlw	ET_OECS				;Is this new Vpp pin the same as OEN pin?
			rcall	etype_getval		;get OEN & CS pin assignment in W and R0
										;..also sets EEADR to ET_OECS

			movwf	R1,0				;temp save

			swapf	R0,W,0				;put OEN pins in <2:0>
			xorwf	R3,W,0				;OEN pin same as our new Vpp pin?
			andlw	0x07				;just the right pin bits
			bz		evppoe_sam			;y: no need to hunt for matches

;This new Vpp pin number is not the same as the existing -OE pin number.
;Is it the same as  the CS pin? This is also allowed.
			movf	R1,0				;CS pin number in 2:0
			xorwf	R3,W,0				;CS pin same as our new Vpp pin?
			andlw	0x07				;just the right pin bits
			bz		evpp_same			;y: no need to hunt for matches

;This new Vpp pin number is not the same as the existing -OE or CS pin number.
			movlw	ET_PGMVPP
			rcall	etype_lookup		;set EEADR for PGM & Vpp pin assignment
			movlw	0xF8				;mask for Vpp pin number
			movwf	R5,0				;in place for assignp

			bra		assignp				;go install pin			

;This new Vpp pin number matches the existing -OE pin number, which is allowed.
evppoe_sam:	movf	R0,W,0				;-OE signal can't also be 12V during programming
			andlw	0x7F				;...so clear that flag
			btfsc	R0,0x07,0			;..unless it was already cleared
			call	K_WREEPROM			;EEADR = ET_OECS
			
;This new Vpp pin number matches the existing -OE  or CS pin number, which is allowed.
evpp_same:	movlw	ET_PGMVPP
			rcall	etype_getval		;set EEADR for PGM & Vpp pin assignment
			andlw	0xF8				;remove old Vpp pin number

			bra		binstall_r3			;Combine W and R3, install into
										;..EEPROM register, and return 

;The user is trying to put Vpp on a pin that can't be supported by the hardware
ppp_err:	lfsr	0,novpp_msg-sstrings
			bra		cr_hprintf

;*******------------------------------------------------------------------------*
;* VCP * Custom EPROM Editor Command: Define Vcc programming voltage
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_vcp:	movlw	ET_VBBVDD
			rcall	etype_getval		;get Vbb & Vdd & Vcc setup
			andlw	0xF8				;strip off old Vcc setup
			movwf	R3,0				;temp save

			movlw	0x03				;allow only allow 0-3
			rcall	ee_getdec			;result in W & R0, trash R1

			bra		binstall_r3			;Combine W and R3, install into
										;..EEPROM register, and return 

;*******------------------------------------------------------------------------*
;* OEV * Custom EPROM Editor Command: Define -OE programming voltage
;*******
; OE may be set to +12V during programming only if it is on pin 19 or 20
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; Note: the -OE signal may be programmed to be at +12V during programming,
;   but not if it is also the Vpp pin. If -OE is currently assigned to the same
;   pin as Vpp then exit with an error message.
;-------------------------------------------------------------------------------*
ecmd_oev:	movlw	ET_PGMVPP			;for testing if Vpp pin= -OE pin
			movwf	R4,0				;temp save

			movlw	ET_OECS
			rcall	etype_getval		;get -OE and CS pin assignment
										;..and set EEADR to ET_OECS

			andlw	0x7F				;strip old -OE voltage assignmemt
			movwf	R3,0				;temp save

			movlw	0x01				;allow only allow 0-1
			rcall	ee_getdec			;result in W & R0, trash R1, Z set if exactly max
			bnz		oev_ok				;always okay if turning feature off

			swapf	R3,W,0				;look at the -OE pin number
			xorwf	R4,W,0				;compare to Vpp pin number
			andlw	0x07				;just the pin bits
			bz		oev_vpperr			;y: can't also be +12V

;Is OE on pin 19 or 20 (encoded as 2 or 3)? if not, hardware can't set it to 12V
			swapf	R3,W,0
			andlw	0x07				;just the OEN pin number
			addlw	0xFE				;subtract 2
			bz		oev_ok
			addlw	0xFF				;subtract 1
			bnz		oev_pnerr			

;All ok: set bit for +12V -OE programming voltage
oev_ok:		bsf		R3,0x07,0
			movf	R3,W,0
			goto	K_WREEPROM			;put it back at ET_OECS, return

;The user has requested +12V OE on a pin the hardware can't support
oev_pnerr:	lfsr	0,oeverr_msg-sstrings
			goto		cr_printf

;The user is attempting to set -OE to +12V when it is also assigned to be Vpp
oev_vpperr:	lfsr	0,oevperr_msg-sstrings
			goto		cr_printf

;*******------------------------------------------------------------------------*
;* VPP * Custom EPROM Editor Command: Define Vpp programming voltage
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_vpp:	movlw	ET_VPPSETUP
			rcall	etype_getval		;get current Vpp setup

			andlw	0x8F				;strip off old Vpp setting
			movwf	R3,0				;temp save

			movlw	0x05				;allow only allow 0-5
			rcall	ee_getdec			;result in W & R0, trash R1

			swapf	R0,W,0				;value in place

			bra		binstall_r3			;Combine W and R3, install into
										;..EEPROM register, and return 

;*******------------------------------------------------------------------------*
;* VPR * Custom EPROM Editor Command: Define Vpp read voltage
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_vpr:	movlw	ET_VPPSETUP
			rcall	etype_getval		;get current Vpp etc. setup

			andlw	0xFB				;strip off old read-Vpp setting
			movwf	R3,0				;temp save

			movlw	0x01				;allow only allow 0-1
			rcall	ee_getdec			;result in W & R0, trash R1, Z set if exactly max

			btfsc	STATUS,Z,0			;was value 0?
			bsf		R3,0x02,0			;n: set bit for +5V read Vpp

			bra		install_r3			;install R3 into EEPROM register and return 

;*******------------------------------------------------------------------------*
;* BCK * Custom EPROM Editor Command: disable/enable blank check
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_bck:	movlw	ET_SPECIAL
			rcall	etype_getval		;get various programming details

			andlw	0xF7				;strip old CS blank-check requirement
			movwf	R3,0				;temp save

			movlw	0x01				;allow only allow 0-1
			rcall	ee_getdec			;result in W & R0, trash R1, set Z if exactly max

			btfss	STATUS,Z,0			;was value 1?
			bsf		R3,0x03,0			;n: set bit for no blank check

			bra		install_r3			;install R3 into EEPROM register and return 

;*******------------------------------------------------------------------------*
;* COP * Custom EPROM Editor Command: Copy EPROM
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; Trashes W,R0-R5,FSR0
;-------------------------------------------------------------------------------*
ecmd_copy:		bcf		KERN_FLAGS,ERR_FLAG,0	;no hex errors yet
				call	K_GETHEX4				;Get source etype in R2:R1
				btfsc	KERN_FLAGS,ERR_FLAG,0	;hex errors?
ec_bogus:		bra		ecmd_error

				movf	R2,F,0					;no high byte allowed
				bnz		ec_bogus

				movlw	ETYPE
				call	K_RDEEPROM
				movwf	R4,0					;remember destination EPROM type

				movf	R1,W,0					;source etype from user
				movwf	R5,0					;save source etype

				call	K_WREEPROM				;EEADR still points to ETYPE
				movlw	ET_NAME					;check for valid source
				call	etype_getval			;first chr of the name=0 means invalid entry
				iorlw	0x00
				bz		ecopy_error				;not a valid EPROM type

;Copy the requsted EPROM specs to the custom EPROM currently being edited
; R4 = destination EPROM
; R5 = source EPROM

				movlw	ET_BYTES				;first item to copy
				movwf	R3,0

ecopy_loop:			movlw	ETYPE				;point to source byte
					movwf	EEADR,0
					movf	R5,W,0
					call	K_WREEPROM

					movf	R3,W,0				;get this particular value
					rcall	etype_getval
					movwf	R2,0

					movlw	ETYPE				;point to dest byte
					movwf	EEADR,0
					movf	R4,W,0
					call	K_WREEPROM

					movf	R3,W,0				;this particular value
					rcall	etype_lookup
					movf	R2,W,0
					call	K_WREEPROM

					incf	R3,F,0
					movlw	ET_RLENGTH
					xorwf	R3,W,0
					bnz		ecopy_loop

;No manual voltage adjustment messages for custom EPROMS
				movlw	ET_SPECIAL
				rcall	etype_getval
				andlw	0xF8
				goto	K_WREEPROM			;return from there	

;Non-existent EPROM type
ecopy_error:	movlw	ETYPE				;restore EPROM type
				movwf	EEADR,0
				movf	R4,W,0
				call	K_WREEPROM

				lfsr	0,undef_msg-sstrings
				goto	cr_printf

;*******------------------------------------------------------------------------*
;* FF1 * Custom EPROM Editor Command: disable/enable EEPROM write FF (erase)
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_ff1:	movlw	ET_SMART1
			rcall	etype_getval		;get previous value

			andlw	0xF7				;strip old  spec
			movwf	R3,0				;temp save

			movlw	0x01				;allow only allow 0-1
			rcall	ee_getdec			;result in W & R0, trash R1, set Z if exactly max

			btfsc	STATUS,Z,0			;was value 1?
			bsf		R3,0x03,0			;y: set bit for EEPROM ersase

			bra		install_r3			;install R3 into EEPROM register and return 

;*******------------------------------------------------------------------------*
;* PUL * Custom EPROM Editor Command: Define programming pulse signal
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_pul:	movlw	ET_VPPSETUP
			rcall	etype_getval		;get current Vpp setup

			andlw	0xFC				;strip off old programing pulse setting
			movwf	R3,0				;temp save

			movlw	0x02				;allow only allow 0-2
			rcall	ee_getdec			;result in W & R0, trash R1

			bra		binstall_r3			;Combine W and R3, install into
										;..EEPROM register, and return 

;*******------------------------------------------------------------------------*
;* PTU * Custom EPROM Editor Command: Define programming pulse time units
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_ptu:	movlw	0x01				;allow only allow 0-1
			rcall	ee_getdec			;result in W & R0, trash R1
			movwf	R3,0

;Set the multiplier for the programming pulse
			movlw	ET_PROGPULSE
			rcall	etype_getval		;get current programming pulse
			andlw	0x7F				;remove old multiplier
			btfsc	R3,0x00,0			;multiply by 100?
			iorlw	0x80				;y: set the bit

			call	K_WREEPROM			;and put ET_PROGPULSE back

;Set the multiplier for the delay between pulses
			movlw	ET_PPDELAY
			rcall	etype_getval		;get current programming pulse
			andlw	0x7F				;remove old multip[lier
			btfsc	R3,0x00,0			;multiply by 100?
			iorlw	0x80				;y: set the bit

			goto	K_WREEPROM			;put ET_PPDELAY back and go to main

;*******------------------------------------------------------------------------*
;* PPW * Custom EPROM Editor Command: Define programming pulse width
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_ppw:	movlw	ET_PROGPULSE

			rcall	etype_getval		;get current separation pulse
			andlw	0x80				;save old multiplier
			movwf	R3,0

			movlw	0x7F				;allow only allow 0-127
			rcall	ee_getdec			;result in W & R0, trash R1

			bra		binstall_r3			;Combine W and R3, install into
										;..EEPROM register, and return 

;*******------------------------------------------------------------------------*
;* PPS * Custom EPROM Editor Command: Define programming pulse separation
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_pps:	movlw	ET_PPDELAY

			rcall	etype_getval		;get current separation pulse
			andlw	0x80				;save old multiplier
			movwf	R3,0

			movlw	0x7F				;allow only allow 0-127
			rcall	ee_getdec			;result in W & R0, trash R1

			movf	R3,F,0				;uS?
			bnz		pps_save			;n: all values allowed for mS
	
			sublw	0x04				;min uS value is 4
			movf	R0,W,0				;also tests for 0 without messing up C
			bz		pps_save			;0 is allowed
			btfsc	STATUS,C,0			;carry set if too small
			movlw	0x04				;just make it a bit bigger

pps_save:	bra		binstall_r3			;Combine W and R3, install into
										;..EEPROM register, and return 

;*******------------------------------------------------------------------------*
;* POL * Custom EPROM Editor Command: select EEPROM-style competion polling
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_pol:	movlw	ET_PPDELAY		;overwrites separation pulse
			rcall	etype_lookup
			movlw	0x01			;specifies polling
			goto	K_WREEPROM

;*******------------------------------------------------------------------------*
;* SPA * Custom EPROM Editor Command: Specify simple programming algorithm
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_spa:	movlw	ET_SMART1
			rcall	etype_lookup
			movlw	0x00				;clear this for simple algorithm
			goto	K_WREEPROM			;put it back and go to main

;*******------------------------------------------------------------------------*
;* FP1 * Custom EPROM Editor Command: Define fast-programming phase 1
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_fp1:	movlw	ET_SMART1
			bra		ecmd_fp12			;reuse code

;*******------------------------------------------------------------------------*
;* FP2 * Custom EPROM Editor Command: Define fast-programming phase 2
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_fp2:	movlw	ET_SMART2

;Fall into ecmd_fp12

;-------------------------------------------------------------------------------*
; Local subroutine to install new smart programming algorithm bit
; On Entry:
;   W=bit mask for algorithm bit
;-------------------------------------------------------------------------------*
ecmd_fp12:	rcall	etype_getval
			andlw	0x0F				;strip off old value
			movwf	R3,0				;temp save

			movlw	0x04				;allow only allow 0-4
			rcall	ee_getdec			;result in W & R0, trash R1, Z set if exactly max

			swapf	R0,W,0				;put result in bits <6:4>
			iorlw	0x80				;set msb so it's not SPA

			bra		binstall_r3			;Combine W and R3, install into
										;..EEPROM register, and return 

;*******------------------------------------------------------------------------*
;* FN1 * Custom EPROM Editor Command: Define n for fast-programming phase 1
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_fn1:	movlw	ET_SMART1
			bra		ecmd_fn12			;common code

;*******------------------------------------------------------------------------*
;* FN2 * Custom EPROM Editor Command: Define n for fast-programming phase 2
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_fn2:	movlw	ET_SMART2

;Fall into ecmd_fn12

;-------------------------------------------------------------------------------*
; Local subroutine to get and install a new n value for the smart algorithm
;-------------------------------------------------------------------------------*
ecmd_fn12:	rcall	etype_getval
			andlw	0xF8				;strip off old value
			movwf	R3,0				;temp save

			movlw	0x07				;allow only allow 0-7
			rcall	ee_getdec			;result in W & R0, trash R1, Z set if exactly max

;Fall into	binstall_r3

;-------------------------------------------------------------------------------*
; Subroutine to install R3 value into EEPROM
; On Entry:
;   EEADR =EEPROM address
;   W = value to install
;   R3 = original value, with appropriate bits cleared
;-------------------------------------------------------------------------------*
binstall_r3:	iorwf	R3,F,0				;combine with other bits

;Fall into install_r3

;-------------------------------------------------------------------------------*
; Subroutine to install R3 value into EEPROM
; On Entry:
;   EEADR =EEPROM address
;   R3 = value to install
;-------------------------------------------------------------------------------*
install_r3:		movf	R3,W,0
				goto	K_WREEPROM			;install new value, return

;*******------------------------------------------------------------------------*
;* PMX * Custom EPROM Editor Command: Define max 'P' for fast-programming
;*******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_pmx:	movlw	ET_PROGREPS
			rcall	etype_lookup

			movlw	0x0FF				;allow almost anything
			rcall	ee_getdec			;result in W & R0, trash R1
			goto	K_WREEPROM			;install new ET_PROGREPS value and return to main

;--------------------------------------------------------------------------------
; Subroutine to get and validate a decimal number
; On Entry:
;   FSR1 points to the next input character
;   W = max allowed value
; On Exit:
;   W=R0 = decimal value
;   Z set if value = max value
; Return to ecmd_error on bogus hex or value too large
; Trashes R1
;--------------------------------------------------------------------------------
ee_getdec:		movwf	R1,0
				call	K_GETDEC				;get W=decimal input, trash R0
				btfsc	KERN_FLAGS,ERR_FLAG,0	;any input errors?
				bra		ecmd_error

				movwf	R0,0					;for return
				subwf	R1,F,0					;beyond max value?
				btfsc	STATUS,C,0				;y:error
				return

;Fall into ecmd_error

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Common Routine
;   Bad command of some sort - print command error message and go to ee_main
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
ecmd_error:		call	cmd_error
				bra		ee_main					;rude return

;-------------------------------------------------------------------------------*
; Subroutine to delete duplicate pin assignments. Removes all instances of the
; specified pin number from all signal definitions.
; On Entry:
;   R3 = (new pin number-17) to hunt for and delete, in one nibble or the other     
; On Exit:
;   R3 has pin number replicated into both nibbles
; trashes W,R0,R1,R2,R6,EEADR,PRODH:PRODL
;-------------------------------------------------------------------------------*
dup_delete:	swapf	R3,W,0				;put new pin number in both nibbles
			iorwf	R3,F,0

			movlw	ET_A9A10			;first of four 2-pin definition bytes
			rcall	etype_lookup		;each byte define 2 pins

			movlw	0x04				;4 bytes of these signal definitions
			movwf	R6,0				;loop counter

dd_loop:		movf	EEADR,W,0
				call	K_RDEEPROM		;get 2 signal definitions
				movwf	R2,0			;save original version for compare at end
				movwf	R1,0			;this version may get modified

				xorwf	R3,W,0			;compare both nibbles to given pin number
				movwf	R0,0			;save compare
	
				andlw	0x07			;low nibble match?
				movlw	0xF8
				btfsc	STATUS,Z,0
				andwf	R1,F,0			;y: clear low nibble

				movf	R0,W,0			;result of 2-nibble compare again

				andlw	0x70			;high nibble match?
				movlw	0x8F
				btfsc	STATUS,Z,0
				andwf	R1,F,0			;y: clear high nibble

				movf	R1,W,0			;new value for this byte
				cpfseq	R2,0			;any changes to this byte?
				call	K_WREEPROM		;y: replace byte

				incf	EEADR,F,0		;next pin-definition word
				decf	R6,F,0			;loop counter
				bnz		dd_loop

;If the given pin is 19, then remove Vdd definition.
			movf	R3,W,0
			andlw	0x07				;pin definition
			xorlw	0x02				;pin 19?
			bnz		dd_not19

			movlw	ET_VBBVDD			;redefining pin 19, so no Vdd
			rcall	etype_getval
			andlw	0xDF				;remove Vdd definition
			call	K_WREEPROM
dd_not19:

;If the given pin is 21, then remove Vbb definition
			movf	R3,W,0
			andlw	0x07				;pin definition
			xorlw	0x04				;pin 21?
			bnz		dd_not21

			movlw	ET_VBBVDD			;redefining pin 21
			rcall	etype_getval
			andlw	0xEF				;remove Vbb definition
			call	K_WREEPROM

dd_not21:	return

;-------------------------------------------------------------------------------*
; Subroutine to get a pin number from the line buffer LINBUF
; legal values are 0 (meaning no pin) and 18-23
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
; On Exit:
;   R3=W=0-6, an encoded pin number
; Rude jump to ee_main on input error
; Trashes R0,PRODL
;-------------------------------------------------------------------------------*
get_pin:	call	K_GETDEC				;get PRODL=decimal input
			btfsc	KERN_FLAGS,ERR_FLAG,0	;any input errors?
			bra		ecmd_error

			movf	PRODL,W,0				;0, meaning no pin?
			bz		gp_done					;y: nothing more to do

			movlw	.18
			subwf	PRODL,F,0				;legal value should be above 17
			bnc		gp_error

			movlw	.5
			subwf	PRODL,W,0				;carry clear if PRODL >=6
			btfsc	STATUS,C,0

gp_error:	bra		ecmd_error

			incf	PRODL,W,0				;pins 18-23 encoded as 1-6

gp_done:	movwf	R3,0					;result in place for return
			return

;******-------------------------------------------------------------------------*
;* ED * Custom EPROM Editor Command: Display EPROM
;******
; On Entry:
;   FSR1 points to the rest of the command line from the user
;   ETYPE = the EPROM type currently being edited (one of the custom types)
;-------------------------------------------------------------------------------*
ecmd_dispeprom:

; Same as dispeprom

;*==============================================================================*
; Subroutine to Display EPROM Specifications (with pretty picture)
; (Placed at the end of code to optimize for rcall and bra instructions)
; On Entry:
;   ETYPE is the type of EPROM to display
; Trashes W,R0,R1,R2,R3,R4
;*==============================================================================*
dispeprom:

;-----------------------
; Print the EPROM's name
;-----------------------
				lfsr	0,type_msg-sstrings		;"Type "
				call	rep_etype2				;print EPROM type and name

;-------------------------------------------
; Print the EPROM's byte count and bits/byte
;-------------------------------------------
				lfsr	0,esize_msg-sstrings	;", size: "
				rcall	hprintf

				movlw	ET_BYTES
				rcall	etype_getval
				movwf	R3,0					;save upper nibble for byte width

				andlw	0x07					;just the byte count bits
				movwf	R2,0
				incf	R2,F,0					;loop will go at least once

				clrf	R1,0					;compute the number of bytes in R1:R0
				movlw	.128
				movwf	R0,0

de_bloop:			bcf		STATUS,C,0			;shift in a 0
					rlcf	R0,F,0
					rlcf	R1,F,0
					decf	R2,F,0
					bnz		de_bloop

				call	K_PRINTDEC				;Print R1:R0 in decimal

				lfsr	0,by_msg-sstrings		;" x "
				rcall	hprintf

				swapf	R3,W,0					;get the bits/byte portion
				andlw	0x0F
				movwf	R0,0					;set up for K_PRINTDEC
				clrf	R1,0
				call	K_PRINTDEC

;-----------------------------------------------------------------------------------
; Print a picture of the EPROM, with pins in the right place, with adjacent messages
;-----------------------------------------------------------------------------------
				lfsr	0,ep1_msg-sstrings		;beginning of picture
				rcall	cr_hprintf

;Programming Vcc goes adjacent to the Vcc pin
				movlw	ET_VBBVDD				;get the programming-Vcc bits
				rcall	etype_getval
				andlw	0x07

				lfsr	0,v0_msg-sstrings		;"0V"
				addlw	0xFF					;decrement
				btfsc	STATUS,Z,0
				lfsr	0,v5_msg-sstrings		;"5V"
				addlw	0xFF					;decrement
				btfsc	STATUS,Z,0
				lfsr	0,v6_msg-sstrings		;"6.2V"
				addlw	0xFF					;decrement
				btfsc	STATUS,Z,0
				lfsr	0,v12_msg-sstrings		;"12V"

;Does this EPROM require the (6.2V) power supply to be adjusted?
				movlw	ET_SPECIAL
				rcall	etype_getval
				andlw	0x01					;test custom Vcc bit
				bz		de_novccadj	

				movlw	ET_VCCMSG
				clrf	R2,0					;no formatting
				call	eds_print				;print the adjustment message from the data structure
				bra		de_vadone

de_novccadj:	rcall	hprintf
de_vadone:
;Figure out and print what's on pin 22
				lfsr	0,ep2_msg-sstrings		;2nd picture piece
				rcall	cr_hprintf

				movlw	.22-.17
				rcall	hunt_pin

;Figure out and print what's on pin 21
				lfsr	0,ep3_msg-sstrings		;3rd picture piece
				rcall	cr_hprintf

				movlw	.21-.17
				rcall	hunt_pin

;Figure out and print what's on pin 20
				lfsr	0,ep4_msg-sstrings		;4th picture piece
				rcall	cr_hprintf

				movlw	.20-.17
				rcall	hunt_pin

;Figure out and print what's on pin 19
				lfsr	0,ep5_msg-sstrings		;5th picture piece
				rcall	cr_hprintf

				movlw	.19-.17
				rcall	hunt_pin

;Figure out and print what's on pin 18
				lfsr	0,ep6_msg-sstrings		;6th picture piece
				rcall	cr_hprintf

				movlw	.18-.17
				rcall	hunt_pin

;Print the supported EPROMs list next to the EPROM picture. (Don't print the
;list if this is a custom EPROM.)
				lfsr	0,ep7_msg-sstrings		;7th picture piece
				rcall	cr_hprintf

				lfsr	0,supdev_msg-sstrings	;"   Supported devices:"
				btfss	FLAGS2,CUST_EPROM,0		;custom EPROM?
				rcall	hprintf					;n: print heading

				lfsr	0,ep8_msg-sstrings		;8th picture piece
				movlw	ET_PARTNO1
				rcall	print_2names

				lfsr	0,ep9_msg-sstrings		;9th picture piece
				movlw	ET_PARTNO2
				rcall	print_2names

				lfsr	0,ep10_msg-sstrings		;10th picture piece
				movlw	ET_PARTNO3
				rcall	print_2names

				lfsr	0,ep11_msg-sstrings		;11th picture piece
				movlw	ET_PARTNO4
				rcall	print_2names

				lfsr	0,ep12_msg-sstrings		;12th picture piece
				movlw	ET_PARTNO5
				rcall	print_2names

;-----------------------------------------------------------------------------
; Print additional details about Vpp, only if there is a Vpp pin on this EPROM
; If it's a pulsed Vpp, does it return to 0V or 5V?
;-----------------------------------------------------------------------------
				movlw	ET_VPPSETUP				;programming pulse type etc.
				rcall	etype_getval
				movwf	R2,0					;temp save

				movlw	ET_PGMVPP
				rcall	etype_getval
				andlw	0x07					;is there a Vpp pin?
				bz		de_noVpp

				lfsr	0,rvppat_msg-sstrings	;"Vpp level during read: "
				rcall	cr_hprintf

				lfsr	0,v0_msg-sstrings		;"0V"
				btfsc	R2,0x02,0
				lfsr	0,v5_msg-sstrings		;"5V"
				rcall	hprintf
de_noVpp:
;----------------------------------------------------------------------------
; Print details about digital programming pulse, if there is one
;----------------------------------------------------------------------------
				btfss	R2,0x01,0				;pulsed Vpp?
				bra		de_nopgm				;y: then there is no PGM pulse

				lfsr	0,propulse_msg-sstrings	;"programming pulse on "
				rcall	cr_hprintf

				lfsr	0,pgm_msg-sstrings+1	; (skipping slash)
				rcall	hprintf

				lfsr	0,pin_msg-sstrings		;" pin "
				rcall	hprintf
de_nopgm:

;----------------------------------------------------
; Announce any Vcc and/or Vpp adjustment requirements 
;----------------------------------------------------
				rcall	special_v

;----------------------------------------------------------------
; Announce if the chip is an EEPROM, and therefore no blank check
;----------------------------------------------------------------
				movlw	ET_SPECIAL
				rcall	etype_getval

				lfsr	0,eeprom_msg-sstrings	;"EEPROM: no blank check before programming"
				andlw	0x08					;EEPROM bit
				btfss	STATUS,Z,0
				rcall	cr_hprintf

;--------------------------------------------------
; Decode and print programming pulse width
;--------------------------------------------------
				lfsr	0,ppw_msg-sstrings			;"Programming pulse width: "
				rcall	cr_hprintf

				movlw	ET_PROGPULSE
				rcall	etype_getval

				rcall	pdecode_time				;decode and print pulse width time

;--------------------------------------------------
; Decode and print programming pulse delay
;--------------------------------------------------
				movlw	ET_PPDELAY
				rcall	etype_getval				;into W and R0
				movwf	R2,0						;temp save
				andlw	0x7F						;no delay?
				bz		de_nodelay					;..then no delay message

				decf	R2,W,0						;EEPROM-style polling?
				bnz		de_nopoll

				lfsr	0,poll_msg-sstrings			;"Poll data for write completion"
				rcall	cr_hprintf
				bra		de_nodelay				

de_nopoll:		lfsr	0,ppd_msg-sstrings			;", delay between pulses: "
				rcall	hprintf
				movf	R2,W,0
				rcall	pdecode_time				;decode and print pulse width time
de_nodelay:
;-----------------
; Smart algorithm?
;-----------------
				movlw	ET_SMART1
				rcall	etype_getval
				iorlw	0x00						;00 means no smart programming
				bz		de_dumb

;---------------------------------------------------------------------------
; Print smart programming specifics
; On Entry:
;    W<6:4> Phase 1 type
;        0: program until match, then program n times
;        1: program until match, then 1 pulse that is n*ET_PROGPULSE long
;        2: program until match (P), then nP times
;        3: program until match, then 1 pulse that is n*P*ET_PROGPULSE long
;        4: Program n times
;        5: Program once, pulse time = n*ET_PROGPULSE
;   W<3> = 1 to write FFh (EEPROM erase)
;   W<2:0> Phase n
;---------------------------------------------------------------------------
				bcf		FLAGS1,B_STATE,0			;will become set if this pass had a P
				movwf	R3,0						;remember smart specifics

				lfsr	0,smartrapid_msg-sstrings	;"Smart/Rapid algorithm:\r  Pass 1"
				rcall	de_smart					;describe pass 1

				movlw	ET_SMART2					;get pass 2 specs
				rcall	etype_getval
				movwf	R3,0						;for de_smart
				iorlw	0x00						;00 means no pass 2

				lfsr	0,pass2_msg-sstrings		;"  Pass 2"
				btfss	STATUS,Z,0
				rcall	de_smart					;y: describe pass 2

				btfss	FLAGS1,B_STATE,0			;Did either pass create a P value?
				return								;n: done

				lfsr	0,maxp_msg-sstrings			;"  Maximum P= "
				rcall	cr_hprintf					;y: announce
				bra		de_reps

;------------------------------
; Print programming repetitions
;------------------------------
de_dumb:		lfsr	0,ppr_msg-sstrings		;"Programming cycles: "
				rcall	cr_hprintf

de_reps:		movlw	ET_PROGREPS
				rcall	etype_getval			;result in W and R0
				clrf	R1,0					;set up for K_PRINTDEC
				goto	K_PRINTDEC				;print W in decimal, return from there

;---------------------------------------------------------------
; Local subroutine to print a piece of the supported EPROM list
; unless this is a custom EPROM
; On Entry:
;   lfsr0 = string pointer offset for piece of the EPROM picture
;   W = index to one of the supported EPROMs
;   ETYPE is current
; Trashes W,R0,R1,R2,R4
;---------------------------------------------------------------
print_2names:	movwf	R4,0					;save EPROM index
				rcall	cr_hprintf				;print EPROM picture piece
				lfsr	0,space7_msg-sstrings	;space over a bit from the EPROM picture
				rcall	hprintf

				btfsc	FLAGS2,CUST_EPROM,0		;custom EPROM?
				return							;y: no supported devices list

				rcall	print_1name				;print the 1st column supported EPROM

				movlw	.5 * 2					;print the second column - 5 names later
				addwf	R4,F,0

				movlw	ET_PARTNO10				;last name in the list?
				xorwf	R4,W,0
				bnz		print_1name				;n: nothing special to do

;The last name on the list may actually be a pointer to special programming instructions
				movlw	ET_SPECIAL				;the last 2 names overlap messages
				rcall	etype_getval
				andlw	0x7						;any special requirements?
				bz		print_1name				;n: carry on

				return							;y: don't print a "supported eprom"					

;--------------------------------------------------------------------
; Local subroutine to print one supported EPROM's manufacturer & name
; On Entry:
;   R4 = pointer to the manufacturer index
; Trashes W,R0,R1,R2 
;--------------------------------------------------------------------
print_1name:	movlw	.11					;print manufacturer names in 11-wide field
				movwf	R2,0
				movf	R4,W,0
				call	eds_print

				movlw	.12					;print part number in 12-wide field
				movwf	R2,0
				incf	R4,W,0
				goto	eds_print

;-----------------------------------------------------------------
; Local subroutine to decode and print a pulse width or delay time
; On Entry:
;   W = value to decode
; Trashes W,R0,R1,R2,R3,PRODH,PRODL
;-----------------------------------------------------------------
pdecode_time:	movwf	R3,0
				andlw	0x7F					;set Z if 0

				lfsr	0,ns500_msg-sstrings	;"500 nS"
				bz		de_pwdone

				movwf	R0,0					;set up for K_PRINTDEC
				clrf	R1,0
				call	K_PRINTDEC				;print W in decimal

				lfsr	0,us_msg-sstrings		;"0 uS" adds a zero too
				btfsc	R3,0x07,0
				lfsr	0,ms_msg-sstrings		;" mS"

de_pwdone:		bra		hprintf					;return from there

;-------------------------------------------------------------------------
; Local subroutine to decode and print one phase of the smart
; programming algorithm
; On Entry:
;   FSR0 = initial print string pointer
;   R3<6:4> Phase 1 type
;      0: program until match, then program n times
;      1: program until match, then 1 pulse that is n*ET_PROGPULSE long
;      2: program until match (P), then nP times
;      3: program until match, then 1 pulse that is n*P*ET_PROGPULSE long
;      4: Program n times
;      5: Program once, pulse time = n*ET_PROGPULSE
;   R3<3> = 1 forces data to FF (EEPROM erase)
;   R3<2:0> Phase n
; On Exit:
;   B_STATE set if this pass produced a P value
; Trashes W,R0,R1,R2,PRODH,PRODL
;------------------------------------------------------------------------
de_smart:		rcall	cr_hprintf
				lfsr	0,write_msg-sstrings	;": write "
				rcall	hprintf

				lfsr	0,ff_msg-sstrings		;"FFh to "
				btfsc	R3,0x03,0				;erase pass?
				rcall	hprintf

				lfsr	0,eachbyte_msg-sstrings	;"each byte "
				rcall	hprintf

				btfsc	R3,0x06,0				;program until match?
				bra		des1

;This is one of the P+ type algorithms
				bsf		FLAGS1,B_STATE,0		;remember this pass had a P

				lfsr	0,matchp_msg-sstrings	;"until it matches (P times)"
				rcall	hprintf

				movf	R3,W,0					;get n
				andlw	0x07
				btfsc	STATUS,Z,0
				return							;no n value: done

				lfsr	0,andthen_msg-sstrings	;", and then "
				rcall	hprintf

				lfsr	0,another_msg-sstrings	;"another "
				btfss	R3,0x04,0				;n/nP or fat pulse?
				rcall	hprintf

des1:			lfsr	0,withone_msg-sstrings	;"with one "
				btfsc	R3,0x04,0				;n/nP or fat pulse?
				rcall	hprintf

				movf	R3,W,0					;get n
				andlw	0x07

				movwf	R0,0					;set up for K_PRINTDEC
				clrf	R1,0
				call	K_PRINTDEC

				movlw	'P'
				btfsc	R3,0x05,0				;nP type programming?
				call	K_CONOUT

				lfsr	0,times_msg-sstrings	;" time(s)"
				btfss	R3,0x04,0				;n/nP or fat pulse?
				bra		hprintf					;print 'times', return from there

;This is one of the fat-pulse algorithms: n or nP times the programming pulse width
				lfsr	0,mult_msg-sstrings		;" * "
				rcall	hprintf

				movlw	ET_PROGPULSE			;get programming pulse width
				call	etype_getval
				rcall	pdecode_time			;decode and print it, with units

				lfsr	0,pulse_msg-sstrings	;" pulse"
				bra		hprintf
				 
;----------------------------------------------------------------
; Local subroutine to print the signal(s) assigned to a pin
; Note that a pin may have multiple signals assigned to it. If
; so, all are printed, separated by slashes.
; On Entry:
;   W = pin number -17, 0 means unassigned
;   ETYPE = the EPROM type
; Trashes W,R0,R1,R2,R3,R4,FSR0
;----------------------------------------------------------------
hunt_pin:		movwf	R4,0					;save pin number while we hunt
				bcf		FLAGS1,B_STATE			;this pin is not yet assigned

;A9 or A10?
				movlw	ET_A9A10
				call	etype_getval			;result in W and R0

				lfsr	0,A9_msg-sstrings		;"A9"
				swapf	R0,W,0
				andlw	0x07					;get the A9 pin definition
				xorwf	R4,W,0					;A9?

				btfsc	STATUS,Z,0
				bra		hunt_ppin				;print pin name

				lfsr	0,A10_msg-sstrings		;"A10"
				movf	R0,W,0
				andlw	0x07					;get the A10 pin definition
				xorwf	R4,W,0					;A10?

				btfsc	STATUS,Z,0
				bra		hunt_ppin				;print pin name

;A11 or A12?
				movlw	ET_A11A12
				call	etype_getval			;result in W and R0

				swapf	R0,W,0
				andlw	0x07					;get the A11 pin definition
				xorwf	R4,W,0					;A11?
				bnz		hunt_12

;It's A11. Is this forced high?
				lfsr	0,A11_msg-sstrings		;"A11"
				btfsc	R0,0x07,0
				lfsr	0,stuckhi_msg-sstrings	;"Stuck-High"
				bra		hunt_ppin				;print pin name

hunt_12:		movf	R0,W,0
				andlw	0x07					;get the A12 pin definition
				xorwf	R4,W,0					;A12?
				bnz		hunt_not12

;It's A12. is it really -Address Strobe?
				lfsr	0,A12_msg-sstrings		;"A12"
				btfsc	R0,0x03,0
				lfsr	0,as_msg-sstrings		;"-AS"
				bra		hunt_ppin				;print pin name
hunt_not12:

;-OE or CS?
				movlw	ET_OECS
				call	etype_getval			;result in W and R0
				movwf	R3,0					;temp save for +12V test

				swapf	R0,W,0
				andlw	0x07					;get the -OE bits
				xorwf	R4,W,0					;-OE?
				bnz		hunt_cs

;It's -OE.
				lfsr	0,oe_msg-sstrings		;"-OE"
				rcall	hunt_ppin				;print pin name, keep looking
			
;Is -OE pin at +12V during programming?
				lfsr	0,p12v_msg-sstrings		;" +12V during programming"
				btfsc	R3,0x07,0
				rcall	hprintf

hunt_cs:		movf	R3,W,0
				andlw	0x07					;get the CS bits (CS may not also be -OE)
				xorwf	R4,W,0					;CS?
				bnz		hunt_notcs

;It's CS. active-low?
				lfsr	0,cs_msg-sstrings		;"CS"
				btfss	R3,0x03,0
				lfsr	0,csn_msg-sstrings		;"-CS"
				rcall	hunt_ppin				;print pin name
hunt_notcs:

;PGM or Vpp?
				movlw	ET_PGMVPP
				call	etype_getval			;result in W and R0

				swapf	R0,W,0
				andlw	0x07					;get the PGM bits
				xorwf	R4,W,0					;PGM?
				bnz		hunt_vpp

;It's PGM. active-low?
				lfsr	0,pgmn_msg-sstrings		;"/-PGM"
				btfsc	R0,0x07,0
				lfsr	0,pgm_msg-sstrings		;"/PGM"

				rcall	hunt_sppin				;print pin name, with slash if needed

hunt_vpp:		movf	R0,W,0
				andlw	0x07					;get the Vpp bits
				xorwf	R4,W,0					;Vpp?
				bnz		hunt_notVpp

;It's Vpp. Print Vpp details.
;R4 = pin number - 17 here.
				lfsr	0,vpp_msg-sstrings		;"/Vpp: "
				rcall	hunt_sppin				;print pin name, with slash if needed

				movlw	ET_VPPSETUP
				call	etype_getval			;result in W and R0

				btfsc	R0,0x01,0				;pulsed Vpp?
				bra		hunt_pvpp

				movwf	R1,0					;temp save while we print
				lfsr	0,pvpp_msg-sstrings		;" pulsed from "
				rcall	hprintf

				lfsr	0,v0_msg-sstrings		;"0V"
				btfsc	R1,0x00,0
				lfsr	0,v5_msg-sstrings		;"5V"
				rcall	hprintf

				lfsr	0,to_msg-sstrings		;" to "
				rcall	hprintf

				movff	R1,R0					;voltage for pvpp_voltage

hunt_pvpp:		movff	R4,R2					;Pin number minus 17
				goto	pvpp_voltage			;return from there
hunt_notVpp:

;Vbb (on pin 21) (may not be shared with other signals)?
				movwf	R1,0
				movlw	.21-.17					;could this be Vbb (pin 21)?
				xorwf	R4,W,0
				bnz		hunt_notvbb

				movlw	ET_VBBVDD
				call	etype_getval
				andlw	0x10					;any Vbb pin on this EPROM?
				bz		hunt_notvbb

				lfsr	0,vbb5v_msg-sstrings	;"Vbb -5V"
				bra		hunt_ppin
hunt_notvbb:
;Vdd (on pin 19) (may not be shared with other signals)?
				movwf	R1,0
				movlw	.19-.17					;could this be Vdd (pin 19)?
				xorwf	R4,W,0
				bnz		hunt_notvdd

				movlw	ET_VBBVDD
				call	etype_getval
				andlw	0x20					;any Vdd pin on this EPROM?
				bz		hunt_notvdd

				lfsr	0,vdd12v_msg-sstrings	;"Vdd +12V"
				bra		hunt_ppin
hunt_notvdd:

;No signal assigned to this pin yet?
				lfsr	0,unassign_msg-sstrings	;"/unassigned pin (low)"

				btfsc	FLAGS1,B_STATE,0
				return

;Fall into hunt_ppin to print message

;----------------------------------------------------------------
; Local subroutine to print a signal name, with a leading slash
; if this pin has already been assigned to another signal
;On Entry:
;  LFSR0 = string pointer, with slash as leading chr
;  B_STATE set if a signal has already been assigned to this pin
;On Exit:
;  B_STATE is set
;   W,R0,TBLPTR,FSR0 trashed
;----------------------------------------------------------------
hunt_sppin:		btfss	FLAGS1,B_STATE,0		;does this pin already do something else?
				movf	POSTINC0,W,0			;n: bump FSR0 to skip over slash

hunt_ppin:		bsf		FLAGS1,B_STATE			;this pin is now assigned

;Fall into hprintf

;--------------------------------------------------------------------
;Local subroutines to print a string (saves a bunch of 2-word goto's)
;On Entry:
;  LFSR0 = string pointer
;On Exit:
;   W,R0,TBLPTR,FSR0 trashed
;--------------------------------------------------------------------
hprintf:		goto	printf

cr_hprintf:		goto	cr_printf

;*==============================================================================*
; Subroutine to print special Vcc and Vpp requirements
; (Placed at the end of code to optimize for rcall and bra instructions)
; On Entry:
;   ETYPE = current EPROM type
; On Exit:
;   R3 = ET_SPECIAL value
; Trashes W,R0,R1,R2
;*==============================================================================*
special_v:		movlw	ET_SPECIAL				;does this EPROM require an external power supply, etc.?
				call	etype_getval			;returns Vpp type in bits <3:0>
				movwf	R3,0					;Save for return

;Announce this EPROM's special programming requirements
				btfss	R3,0x00,0				;Requires programming Vcc adjustment?
				bra		no_adjvcc

				lfsr	0,adj_msg-sstrings		;"==>Manually Adjust "
				rcall	cr_hprintf

				lfsr	0,adjvcc_msg-sstrings	;"6.2V supply to "
				rcall	hprintf
				movlw	ET_VCCMSG
				clrf	R2,0					;no formatting at eds_print
				call	eds_print				;print the Vcc message from the data structure

no_adjvcc:
				btfss	R3,0x01,0				;Requires Vpp adjustment?
				bra		no_adjvpp

;Tell user which supply to adjust, and to what voltage

				lfsr	0,adj_msg-sstrings		;"==>Manually Adjust "
				rcall	cr_hprintf

				movlw	ET_VPPSETUP				;Which Vpp adjustment?
				call	etype_getval			;returns Vpp voltage in bits <6:4>
				swapf	R0,F,0
				movlw	0x07
				andwf	R0,F,0
				decf	R0,F,0					;0-3 are the possible supplies

				lfsr	0,v1270_msg-sstrings
				dcfsnz	R0,F,0
				lfsr	0,v1315_msg-sstrings
				dcfsnz	R0,F,0
				lfsr	0,v21_msg-sstrings
				dcfsnz	R0,F,0
				lfsr	0,v2510_msg-sstrings

				rcall	hprintf

				lfsr	0,vppto_msg-sstrings	;" Vpp supply to"
				bra		spec_eprint				;print the adjustment message from the data structure

no_adjvpp:
				btfss	R3,0x02,0				;Requires external (negative) Vpp power supply?
				return

				lfsr	0,req_msg-sstrings		;"Requires "

spec_eprint:	rcall	hprintf

				movlw	ET_VPPMSG				;ET_VPPMSG is used for both Vpp adjustment and external Vpp
				clrf	R2,0					;no formatting at eds_print
				goto	eds_print				;print the Vpp message from the data structure

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Reboot if this firmware does not match the Loader Kernel
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
wrong_programmer:	lfsr	0,wrongprog_msg-sstrings
					rcall	cr_hprintf

					movlw	.200			;stall for 50 chr times, to print message
					call	K_STALL250U
					reset					;let user type L to go to loader

;################################################################################
;#								Included tables									#
;################################################################################

;********************************************************************************
; Voltage Limits Table
; These are rough limits (15% below and above the expected value), intended only
; to detect gross errors and short circuits. They have been calculated to be 15%
; above and below each nominal voltage. (This leaves room for Vpp adjustments
; outside the nominal settings, for peculiar EPROMs.)
;********************************************************************************
LIMTABLE:	dw	0x694c		;Low limit, high limit for 12.75V supply
			dw	0x6F50		;Low limit, high limit for 13.15V supply
			dw	0xB482		;Low limit, high limit for 21V supply
			dw	0xD69A		;Low limit, high limit for 25V supply

;********************************************************************************
; User Commands Table
;********************************************************************************
	#include	Commands.inc

;********************************************************************************
; Strings accessed via FSR0
; Unlike TBLPTR, the FSRs can be loaded with a single instruction: LFSR. But
; the LFSR instruction takes a 12-bit literal, creating a 4K-byte limit.
;********************************************************************************
	#include	ShortStrings.inc

;********************************************************************************
; EPROM definition table
;********************************************************************************
	#include	EPROMTABLE.inc

;********************************************************************************
; EPROM name strings
;********************************************************************************
	#include	EPROMNames.inc

;********************************************************************************
;Long strings (mainly the help messages)
;********************************************************************************
	#include	HelpMessages.inc

	if	DEBUG
;******-------------------------------------------------------------------------*
;* PW * Adjust pulse width DEBUG command
;******
; On Entry:
;   R4 = second command character
;	DFLT_FLAG = 1 if the user did not provide an address offset value.
;   ADDRESSL = address offset
;*------------------------------------------------------------------------------*
cmd_pw:		movf	R4,W,0				;second character an W?
			xorlw	'W'					;anything else is an error
			btfss	STATUS,Z,0
			goto	cmd_error

;If no PW value from the user, then report the current setting, Otherwise
; set it according to the user input, then report
			movf	ADDRESSL,W,0		;user input, if any

			btfss	FLAGS1,DFLT_FLAG,0
			movwf	CCPR1L,0			;Set pulse width according to user input

			movlw	low(pw_msg)
			movwf	TBLPTRL,0
			movlw	high(pw_msg)
			movwf	TBLPTRH,0
			call	K_PRINTF			;print the message

			movf	CCPR1L,W,0			;print the current pulse width
			call	K_PRINTHEX2
			goto	main

pw_msg:		dw	"Pulse width: ",0

	endif


	end




			




