	title "EPROM Programmer Kernel Rev PIC18 1.00"
	subtitle "Copyright (C) 2015 Martin Eberhard"
	list b=4, c=132, n=80

;********************************************************************************
;* EPROM Programmer Loader Kernel												*
;* Written for both the ME2700 EPROM programmer									*
;* designed to work with the PIC18F44K20 or the PIC18F45K20						*
;*																				*
;*Revision    Date        Author         Comments								*
;*	1.00   	20SEP2015	M.Eberhard	Created										*
;*																				*
;* To Do (maybe)																*
;*   Use serial RAM to hold entire code image before writing to Flash			*
;********************************************************************************
; Major revision must change if the firmware is not fully compatable with		*
; previous revisions.															*
;********************************************************************************
REV_MAJOR		equ	0x01		;major revision number
REV_MINOR		equ	0x00		;minor revision number

TRUE	equ		0xff
FALSE	equ		0

	errorlevel 1		;eliminate all the 'not in bank 0' messages

;********************************************************************************
;*								GENERAL NOTES									*
;*																				*
;* This Loader Kernel allows EPROM programmer firmware to be loaded via the		*
;* serial port and written to the PIC's FLASH memory, without any development	*
;* hardware (such as a PICkit 3). It also provides some useful subroutines for	*
;* the EPROM programmer, reusing code needed for the loader. All of these		*
;* available subroutines can be accessed via a fixed-location jump table.		*
;*																				*
;* Firmware load is via Intel Hex file, as produced by the Microchip's MPASM	*
;* (version 8.83). Unfortunately, the RAM in the PIC microcontroller is too		*
;* small to hold the entire firmware, so downloaded firmware is written into	*
;* FLASH as it is received. Any Intel Hex errors, as well as read-back verify	*
;* errors, will be reported - but these errors will likely result in corrupted	*
;* firmware. However, this entire kernel is protected (by 'boot' protection in	*
;* configuration register CONFFIG2) and cannot be overwritten. So you can try	*
;* reloading the firmware if it fails to load the first time.					*
;*																				*
;* This program must be no more than 2048 bytes long - the amount of FLASH that	*
;* is protected by the 'boot' protection in config2. If the code is larger than	*
;* 2048 bytes, then an error will be generated when assembled.					* 																				*
;* The EPROM programmer firmware should load at address 0x800 (LOADED_CODE).	*
;* This program installs a dummy program there, which will be over-written when	*
;* programmer code is loaded via the serial port.								*
;*																				*
;* To invoke the loader, lean on the 'L' key (so that it auto-repeats), and		*
;* then turn on the EPROM programmer. You should see the Loader message instead	*
;* of the EPROM programmer's sign-on message.									*
;*																				*
;* To leave the loader and run the loaded firmware, type 3 or more ESCs at the	*
;* loader prompt. (ESCs will be ignored while a file is loading.)				*
;*																				*
;* Note that the PIC gets partially initialized by this loader. The programmer	*
;* firmware needs to initialize everything else.								*
;*																				*
;* This program assumes the PIC is clocked at 16 MHz, meaning a 4 MHz CPU       *
;* operating speed, or 0.25 uS instruction cycle time. The internal oscilator	*
;* has an accuracy of +/- 10%, which must be considered in all timing.			*
;*																				*
;* Page references in the comments refer to the appropriate pages in Microchip	*
;* document DS40001303H "PIC18F2XK20/4XK20 28/40/44-Pin Flash Microcontrollers	*
;* with XLP Technology"															*
;********************************************************************************
	page
;********************************************************************************
;* CALL Vectors for External Code Subroutine Access								*
;* Address	Vector		     Notes												*
;*  0000	RESET			Cold Reset (not meant to be called)					*
;*	0002	K_KERN_REV		get Loader Kernel firmware revision level in W		*
;*  0004	K_PROG_ID		returns W = 00 										*
;*  000A	K_CONIN			get one Rx Queue chr into W & R0. Z set if empty	*
;*							trashes FSR0										*
;*  000C	K_CONOUT		Send W to console (returns chr in R0, trashes FSR0)	*
;*	000E	K_PRINTF		print 0-terminated string at TBLPTRH:TBLPTRL		* 
;*							trashes W,R0,R1,R2,FSR0,TBLPTRL,TBLPTRH,TABLAT		*
;*  0010	K_EEPRINT		print 0-terminated string at EEADR from EEPROM.		*
;*							Trashes R0,FSR0,READR.								*
;*	0012	K_PRINTHEX1		print low nibble of W as 1 hex chr. Trashes R0,FSR0	*
;*	0014	K_PRINTHEX2		print W as 2 hex chrs. Trashes R0,R1,FSR0			*
;* 	0016	K_PRINTDEC		print R1:R0 in decimal, leading zeros suppressed.	*
;*							trashes W,R0,R1,PRODH,PRODL,FSR0,INP_FLAG			*
;*	0018	K_PRINTSN		print this unit's serial number. Trashes FSR0.		*
;*  001A	K_HEX2BIN		Convert hex value in R0 to binary, combine it with	*
;*							nibble already in R1. On return, R0=new binary		*
;*							nibble; R1=W=combined nibbles						*
;*	001C	K_GETLIN		Get null-terminated input line from user			*
;*	001E	K_PARSE			Parse table at TBLPTR looking for command at FSR1	*
;*	0020	K_GETCHR		Get next chr from LINEBUF at FSR1, Result in W		*
;*							and R0. FSR1 incremented.							*
;*	0022	K_GETUCHR		Get next chr from LINEBUF at FSR1, convert to		*
;*							uppercase. Result in W and R0. FSR1 incremented.	*
;*  0024    K_GETDEC		Get decimal value from LINBUF. Result in W & PRODL.	*
;*							trashes R0											*
;*  0026	K_GETHEX2		Get 2-digit hex chr with checksum					*
;*	0028	K_GETHEX4		Get 4-digit hex value from LINBUF at FSR1. Result	*
;*							in R2:R1, Trash W,R0								*
;*	002A	K_ASKYN			ask user Y/N. Z set if Y, clear if N. Trashes FSR0	*
;*	002C	K_CHKABORT		check for user abort. Z if no chr, C if abort,		*
;*							received chr in W and R0, if any.					*
;*	002E	K_RDSRAM		read SRAM byte at ADDRESSH:ADDRESSL, Result in W.	*
;*							Trashes R0.											*
;*	0030	K_WRSRAM		write W to SRAM at ADDRESSH:ADDRESSL.				*
;*							Trashes W ,R0, R1.									*
;*	0032	K_RDEEPROM		read EEPROM byte at address W, result in W			*
;*	0034	K_WREEPROM		write W to EEPROM at address EEADR					*
;* Reserved 2 slots for future commands here									*
;*	003A	K_STALL25M		stall for W * 25 mSec (trashes R0 & R1)				*
;*	003C	K_STALL250U		stall for W * 250 uSec (trashes R0)					*
;*	003E	K_STALL1U		stall for W + 1 uSec								*
;*																				*
;*  0800	LOADED_CODE		beginning of serial port-loaded programmer code		*
;********************************************************************************
;* Kernel Initialization														*
;*																				*
;*  # The PIC configuration registers are initialized. Note that this loader	*
;*    will not allow you to change the configuration registers.					*
;*  # The oscillator is initialized.											*
;*  # The Vpp switcher components are initialized, with the PWM off.			*
;*	# The SPI bus is initialized, and the external SPI SRAM is initialized.		*
;*  # Ports are initialized with the UART enabled.								*
;*	# The transmit and receive queues are initialized.							*
;*  # The transmit and receive interrupts are active.							*
;********************************************************************************
	page
;********************************************************************************
;* Intel Hex Format	for Firmware Download										*
;*																				*
;*  # Three record types are accepted; all other records are ignored.			*
;*     1) Type 00 records are data records. Data records are written to FLASH	*
;*        only if a Type 04 record has already been received, with extended		*
;*        address = 0000 (for program memory) If no	Type 04 record has been		*
;*        received yet, or if the last Type 04 record set the extended address	*
;*        to something other than 0000, then the data record will be ignored.   *
;*        This means (for example) that you	cannot write to the EEPROM or the	*
;*        Config registers of the PIC.											*
;*        																		*
;*     2) A Type 01 record (with 0 bytes of data) is an End-Of-File	record, and	*
;*        is required at the end of the file to force a write to FLASH of the	*
;*        last RAM buffer full of data.	NOTE: a data record with 0 bytes of		*
;*        data is NOT treated as an End-of-file record.							*
;*     3) Type 04 records set the extended address for the subsequent records.	*
;*        The "address" field of the Type 04 record (bytes 2 and 3) is ignored.	*
;*        The first 2 bytes of the "data" field (bytes 5 and 6) set the			*
;*        extended address. MPASM sets the extended address to 0000 for FLASH	*
;*        data, 00F0 for EEPROM, and ???? for the PIC Config registers.			*
;*  # The PIC's FLASH memory is organized in 32-byte "rows".					*
;*  # The PIC's erase block is 64 bytes long, or two rows long. For this 		*
;*    reason, Flash writing is done in pairs of rows.							*
;*  # This loader assumes that each Intel hex record fits completely within two	*
;*    32-byte FLASH rows, for a total of 64 bytes. However, one FLASH row pair	*
;*    may be (probably will be) made up of several hex records.					*
;*  # If the hex file has data for only part of a FLASH row-pair, then only the	*
;*    data supplied in the Hex file will be changed - the other bytes in that	*
;*   FLASH row-pair will remain unchaged, because this program first reads the  *
;*   old FLASH data from each row-pair into its RAM buffer, filling in the		*
;*    missing data for that row-pair.											*
;*  # After each row-pair is written, it is read back and verified.				*
;*  # The checksum is checked for all records, including ignored records.		*
;*  # The following conditions will generate an error message, and increment 	*
;*    error count that is reported after the end of the file:					*
;*     1) Checksum Error														*
;*     2) Bad Hex Error (something besides 0-9 or A-F when expecting hex)		*
;*     3) Verify Error (FLASH readback did not match the RAM buffer data)		*
;*  # A pair of FLASH writes occur when a new hex record addresses FLASH memory	*
;*    in a different FLASH row-pair than the previous record, or when a type 01	*
;*    record is received. This means that FLASH write verification occurs after	*
;*    the address and record-type fields of a hex data record have been			*
;*    received, and before the data bytes are received. And this means that if	*
;*    a verify fails, the verify error message will be printed in the middle of *
;*    a hex record, and will refer to the previous record(s).					*
;*  # All characters are ignored until a colon (marking the start of an Intel	*
;*    hex record) has been received. This means that comment lines in the Intel	*
;*    hex file will be ignored. This also means that any record where the		*
;*    initial colon has been corrupted will be ignored without being caught as	*
;*    an error.																	*
;*  # Receipt of a type 01 record (End-Of-File) causes the following actions:	*
;*     1) The total number of records loaded into the FLASH is printed. This	*
;*        can be checked manually, to make sure no records were dropped, and	*
;*        the load was in fact successful.										*
;*     2) The total number of errors detected is printed. Anything other than	*
;*        0000 means that the load was unsuccesful, and the loaded code should	*
;*        not be trusted.														*
;*     3) control returns to the loader, reprinting the loader's brief sign-	*
;*        on message. (To run the new code, hit the ESC key 3 times in a row)	*
;********************************************************************************
	page
;********************************************************************************
;* Include standard headers, programmer header, and macros						*
;********************************************************************************
	#include	P18F45K20.INC
	#include	ASCII.inc
	#include	18FKernelMemory.inc
	#include	18FKernel.inc

	page

;################################################################################
;# Overwrite the EEPROM data, so that it will appear uninitialized				#
;################################################################################
	org		EEPROM
	de		"Uninitialized EEPROM",0

;################################################################################
;# Dummy Firmware - gets overwritten by programmer firmware						#
;################################################################################
	org		LOADED_CODE

				lfsr	0,nocode_msg
				call	loader_printf
				goto	code_loader

nocode_msg:		dw	"\r\rNo programmer firmware loaded."
				dw	"\rSend ME2700.hex now.\r",0

	page
;################################################################################
;# Reset Vector - hardware-fixed location										#
;# Maximum of 1 word long - otherwise it will collide with K_KERN_REV.			#
;################################################################################
	org 	RESET_VECTOR				;routine must be exactly 2 words long
				bra		init

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Fixed-location subroutine to return the version for this loader.
; This subroutine can only be 1 word long here!
; On Exit:
;     W bits 7:4 = major revision number
;     W bits 3:0 = minor revision number
;*==============================================================================*
K_KERN_REV:		retlw	(REV_MAJOR << 4) | REV_MINOR

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Fixed-location subroutine to return the ID for this programmer.
; This subroutine can only be 1 word long here!
; On Exit:
;     W=0 for ME1702A, 1 for ME5204
;*==============================================================================*
K_KERN_ID:		retlw	0x00

;################################################################################
;# Low-priority Interrupt Vector - hardware-fixed location. There is room for	#
;# just one instruction word here.												#
;################################################################################
	org		INT_VECTOR
				bra		int_service

;################################################################################
;# Kernel call vectors (fixed locations for access by external code)			#
;# INT_VECTOR = 0x04. This table begins at 0x05, immediately following the		#
;# interrupt vector.															#
;################################################################################
	org		CALL_TABLE

K_CONIN:		bra		get_rxchr		;get one chr from Rx Q into W & R0. Z set if empty
K_CONOUT:		bra		printw			;send W to console
K_PRINTF:		bra		printf			;print 0-terminated string at TBLPTRH:TBLPTRL
K_EEPRINT:		bra		ee_print		;print 0=terminates string from EEPROM at EEADR
K_PRINTHEX1:	bra		print_hex1		;print low nibble of W as 1 hex chr
K_PRINTHEX2:	bra		print_hex2		;print W as 2 hex chrs
K_PRINTDEC:		bra		print_dec		;print R1:R0 in decimal, leading zeros suppressed
K_PRINTSN:		bra		print_sn		;print serial number
K_HEX2BIN:		bra		hex2bin			;convert hex value in R0 to binary, combine with R1
K_GETLIN:		bra		getlin			;Get a 0-terminated line from the user
K_PARSE:		bra		parse_cmd		;search cmd table at TBLPTR for cmd at FSR0, return W=cmd index
K_GETCHR:		bra		get_chr			;get next character at FSR1, result in W & R0
K_GETUCHR:		bra		get_uchr		;get next uppercase chr at FSR1, result in W & R0
K_GETDEC:		bra		get_dec			;Get 8-bit decimal value in W & PRODL. Trash R0
K_GETHEX2:		bra		get_lbhex2		;get 2-digit hex, result in W & R1, sum in CHECKSUM, Z set if CHECKSUM=0
K_GETHEX4:		bra		get_lbhex4		;Get 4-digit hex value into R2:R1. Trash W,R0
K_ASKYN:		bra		ask_yn			;Ask user Y/N. Z set if Y, cleared if N
K_CHKABORT:		bra		chk_abort		;Check for user abort. Z if no chr, C if abort, chr in W,R0
K_RDSRAM:		bra		rd_sram			;read SRAM byte at ADDRESSH:ADDRESSL, Result in W
K_WRSRAM:		bra		wr_sram			;write W to SRAM at ADDRESSH:ADDRESSL
K_RDEEPROM:		bra		rd_eepromw		;read EEPROM at address W, result in W
K_WREEPROM:		bra		wr_eeprom		;write W to EEPROM at address EEADR
				bra		init			;reserved for future command
				bra		init			;reserved for future command
K_STALL25M:		bra		stall_25ms		;stall for W * 25 mS
K_STALL250U:	bra		stall_250us		;stall for W * 250 uS

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Subroutine to stall for W + 1 uS (including the call)
;
; This routine is right at the end of the jump vector table to eliminate the
; overhead of an additional bra instruction, whose 0.50 uS time is significant.
; This routine assumes a 16 MHz oscillator frequency, resulting in a 0.25 uS
; instruction cycle time.
; On Exit:
;     W=0
;     Z set
;*==============================================================================*
K_STALL1U:								;Call Vector right here to make it fast
stall_1us:		addlw	0xff			;(.25)subtract 1 from W				
				btfss	STATUS,Z,0		;(.25/.5)This way to make loop exactly 1 uS
				bra		stall_1us		;(.50)

				nop						;(.25)
			return						;(.50)
	page
;################################################################################
;# Null-terminated strings used by kernel										#
;################################################################################
loader_msg:		dw	"\rME2700 Loader "
				dw	(REV_MAJOR + '0') + ('.' << 8)
				dw	(REV_MINOR + '0') + (CR << 8)
				dw	0
hexerror_msg:	dw	" ?Hex",0
cksumerror_msg:	dw	" ?Csm",0
verifyerr_msg:	dw	" ?Ver ",0			;this error will print in the middle of a record
reccount_msg:	dw	"\rLoaded Hex Records: ",0
errcount_msg:	dw	"\rErrors: ",0
eeinit_msg:		dw	"\rInitializing EEPROM",0
newsn_msg:		dw	"\rNew "			;fall into sn_msg
sn_msg:			dw	"ME2700 Serial Number: ",0
unchanged_msg:	dw	"unchanged",0
yn_msg:			dw	" (Y/N)? ",0
clrvars_msg:	dw	"\rClear all EEPROM variables",0
cleared_msg:	dw	"\rAll EEPROM variables cleared",0
run_msg:		dw	"\rRun code",0

	page
;################################################################################
;# Initialization Routine														#
;#   Initialize only the registers where the Reset value is not right for us.	#
;#   The following are initialized correctly by RESET							#
;#		ADCON0	= 0x00	(A/D converter is disabled. Page 255)					#
;#		ADCON1	= 0x00	(No A/D pims assigned. Page 256)						#
;#		BAUDCON	= 0x08	(16-bit baud rate generator. Page 233)					#
;#		CCP1CON	= 0x0C	(Single-ended PWM, active high for P1A. Page 165)		#
;#		CCP2CON	= 0x00	(Capture/Compare Module 2 disabled. Page 134)			#
;#		CM1CON0	= 0x9C	(Comparator 1 enabled and set up. Page 267)				#
;#		CM2CON0	= 0x00	(Cmparator module 2 disabled. Page 268)					#
;#		CM2CON1	= 0x00	(FVR is the reference for Comparater 1. Page 270)		#
;#		CCPR1L	= 0x00	(Duty cycle is 0. Page 141)								#
;#		CVRCON2	= 0x80	(Voltage reference enabled. Page 275)					#
;#		CVRCON	= 0x00	(Comparator voltage reference disabled. Page 274)		#
;#		ECCP1AS	= 0x10	(Shut down PWM when C1OUT is high. Page 171)			#
;#		HLVDCON	= 0x00	(High/low voltage detection disabled. Page 276)			#
;#		INTCON2	= 0xFF	(Weak pullups disabled. Page 103)						#
;#		INTCON3 = 0xC0	(external interrupts disabled. Page 104)				#
;#		OSCON	= 0x72	(Internal 16 MHz oscillator. Page 28)					#
;#		OSCTUNE	= 0x00	(no oscillator tuning)									#
;#		PIE1	= 0x20	(USART receive interrupt enabled. Page 107)				#
;#		PIE2	= 0x00	(a bunch of interrupts are disabled. Page 108)			#
;#		PIR1	= 0x00	(receive interrupt is cleared. Page 105)				#
;#		PIR2	= 0x00	(Flash write operation has not been started. Page 106)	#
;#		PSTRCON	= 0x01	(Steer PWM output to pin P1A. Page 175)					#
;#		PWM1CON	= 0x80	(auto-restart PWM when C1OUT goes low. Page 174)		#
;#		RCON	= 0x5C	(mainly, IPEN = 0. Page 49 & 111)						#
;#		RCSTA	= 0x90	(USART receive setup. Page 232)							#
;#		SLRCON	= 0x1F	(low slew rate on all ports. Page 130)					#
;#		SPBRG	= 0xAF	(9600 baud. Page 237)									#
;#		SPBRGH	= 0xBO	(9600 baud. Page 237)									#
;#		SSPCON1	= 0x20	(4 MHz SPI master enabled. Page 182)					#
;#		SSPSTAT	= 0x40	(SPI data changes on clock falling edge. Page 181)		#
;#		T0CON	= 0x00	(Timer 0 is off. Page 145)								#
;#		T1CON	= 0x00	(Timer 1 is off. Page 148)								#
;#		T2CON	= 0x04	(Timer 2 is on, no prescaler. Page 155)					#
;#		T3CON	= 0x00	(Timer 3 is off. Page 157)								#
;#		TBLPTRU = 0x00	(All code & tables are in the 1st 32K.)					#
;#		TRISD	= 0xFF	(Data port tristated)									#
;#		TXSTA	= 0x24	(USART transmit setup. Page 231)						#
;#		WDTCON	= 0x00	(Watchdog timer disabled. Page 291)						#
;################################################################################
init:

;*------------------------------*
; *Initialize the oscillator	*
;*------------------------------*
;internal oscillator block, 16 MHz (Page 28)
			movlw	(1 << SCS1) | (1 << IRCF0) | (1 << IRCF1) | (1 << IRCF2)
			movwf	OSCCON,0

;*------------------------------*
; *Initialize all I/O port pins	*
;*------------------------------*
			clrf	LATA,0
			movlw	TRISA_INIT
			movwf	TRISA,0
			movlw	ANSEL_INIT
			movwf	ANSEL,0

			setf	LATB,0
			movlw	TRISB_INIT
			movwf	TRISB,0
			movlw	ANSELH_INIT
			movwf	ANSELH,0

			movlw	(1 << SRAM_CEn)
			movwf	PORTC				;SRAM disabled
			movlw	TRISC_INIT
			movwf	TRISC,0

			clrf	LATD,0
										;TRISD is already 0xFF

			clrf	LATE,0
			bsf		LATE,ADRCLK,0		;Busy light on
			movlw	TRISE_INIT
			movwf	TRISE,0

;*------------------------------------------------------------------------------*
;* Set up comparator C1 and PWM1 as a voltage regulator for Vpp.				*
;* Comparator C1 compares one of the voltage-measurement resistor dividers to	*
;* the PIC's internal 1.2 volt inernal fixed voltage reference. C1OUT is high	*
;* whenever the output from the voltage measurement divider is higher than the	*
;* internal 1.2V reference. Capture/compare/PWM Module 1 is set up as an		*
;* "enhanced PWM" with a non-inverted output and is further set up to be		*
;* disabled whenever Comparator C1OUT is high. During this initialization, the	*
;* PWM pulse width is set to 0, so that Vpp is off.								*
;*------------------------------------------------------------------------------*

;Enable the fixed voltage reference.
;(CM2CON1 defaults to set the 1.2 volt FVR as the reference. See page 270.)

			movlw	(1 << FVREN)					;Enable FVR (page 275)
			movwf	CVRCON2,0

;Set up Timer 2 for the desired frequency (108 KHz) (See page 141)
;(This frequency experimentally determined)
			movlw	0x24							;timer value for 108 KHz
			movwf	PR2
		
			movlw	(1 << TMR2ON)					;turn timer 2 on, no prescale
			movwf	T2CON,0							;(page 155)

;Set the duty cycle to 0, to insure no output (page 141)
			clrf	CCPR1L,0

;Set up PWM1 as an enhanced PWM, with output steered to its P1A pin, and
;controlled by comparator output C1OUT
			movlw	(1 << CCP1M2) | (1 << CCP1M3)	;(page 161) Single-ended PWM mode,
			movwf	CCP1CON,0						;active high for P1A

			movlw	(1 << STRA)						;(page 175)
			movwf	PSTRCON,0						;Steer PWM output to pin P1A

			movlw	(1 << ECCPAS0)					;(page 171)
			movwf	ECCP1AS,0						;Shut down PWM and drive P1A to 0V
													;..when C1OUT is high

			movlw	(1<<PRSEN)						;(page 174)
			movwf	PWM1CON,0						;auto-restart PWM when C1OUT goes low

;Comparator 1 enabled, inverted, using internal reference for + input, no
;output pins (page 267) For now, also select channel 0 (the 12.5V divider)
;as the - input.

			movlw	(1 << C1R) | (1 << C1SP) | (1 << C1ON) | (1 << C1POL)
			movwf	CM1CON0,0

;*------------------------------------------------------*
;* Set up the RS-232 serial port and associated queues	*
;*------------------------------------------------------*
;Set up the EUSART for asynchronous 8-bit, specified baud, enabled
			movlw	(1 << BRG16)				;(page 233)
			movwf	BAUDCON,0

			movlw	low(BAUDRATE)				;(page 237)
			movwf	SPBRG,0
			movlw	high(BAUDRATE)
			movwf	SPBRGH,0

			movlw	(1 << BRGH) | (1 << TXEN)	;(page 231)
			movwf	TXSTA,0

			movlw	(1 << SPEN) | (1 << CREN) 	;(page 232)
			movwf	RCSTA,0

;initialize the receive queue
	if	low(RX_QUEUE)
			movlw	low(RX_QUEUE)
			movwf	RQ_IPTR,0
			movwf	RQ_OPTR,0
	else
			clrf	RQ_IPTR,0
			clrf	RQ_OPTR,0
	endif

;initialize the transmit queue
			movlw	low(TX_QUEUE)
			movwf	TQ_IPTR,0
			movwf	TQ_OPTR,0

;initialize interrupt flags: both queues are empty
			movlw	(1 << TQ_EMPTY)
			movwf	INT_FLAGS,0

			movf	RCREG,W,0			;chuck any junk in the UART receiver
			movf	RCREG,W,0			;it's a double-buffer

;Enable echo
			movlw	(1 << ECHO_FLAG)
			movwf	KERN_FLAGS,0

;Set up UART interrupts (Page 107)
			movlw	(1 << RCIE)			;enable USART Rx int
			movwf	PIE1,0				;note: Tx int gets enabled when enqueing data

;*------------------------------------------------------------------*
; Set up the Master Synchronous Serial Port for SPI communication	*
; with the external serial SRAM chip, and set up the SRAM chip		*
;*------------------------------------------------------------------*
			movlw	(1 << SSPEN)				;(page 182)
			movwf	SSPCON1,0					;enabled, SPI Master, 4 MHz operation
			movlw	(1<<CKE)
			movwf	SSPSTAT						;data changes on clock falling edge (page 181)

;Set the external Serial SRAM for byte mode
			bcf		LATC,SRAM_CEn,0				;enable SRAM

			movlw	SRAM_SWRITE					;SRAM write status command
			movwf	SSPBUF,0					;write command to SPI
			rcall	wait_spi					;wait for it to complete

			clrf	SSPBUF,0					;0 to status register means byte mode
			rcall	wait_spi					;wait for it to complete

			bsf		LATC,SRAM_CEn,0				;disable SRAM

;-----------------------------------*
; Initalize various PIC registers	*
;-----------------------------------*
			clrf	T0CON,0						;disable timer 0 (page 145)

;Turn on the BUSY LED, and clock 0 into the low-address latch
			bsf		LATC,ADRCLK,0

;Enable UART interrupts - we're ready to go (Page 102)
			movlw	(1 << GIE) | (1 << PEIE)
			movwf	INTCON,0

;################################################################################
;# Choose Setup, Loader, or Loaded Code											#
;# Test for user typing 'L's to invoke the loader. If not, go to loaded code.	#
;# auto-repeat is faster than 500 mS per character - we should see one within	#
;# 500 mS. 																		#
;################################################################################
;Check for Initialized string, and go set up the EEPROM if it's not there
			movlw	EE_INITLEN			;Initialized length 
			movwf	R1,0				;byte counter

			movlw	high(loader_msg)	;init string in FLASH
			movwf	TBLPTRH,0
			movlw	low(loader_msg)
			movwf	TBLPTRL,0

			movlw	EE_INIT				;init string address in EEPROM
			movwf	EEADR,0				;..where rd_eeprom wants the address

ckid_loop:		rcall	rd_eeprom		;get an EEPROM byte
				incf	EEADR,F,0		;bump pointer

				tblrd*+					;get string byte
				xorwf	TABLAT,W,0		;compare EEPROM to string
				btfss	STATUS,Z,0
				bra		new_eeprom		;mismatch: go setup EEPROM

				decfsz	R1,F,0			;test all bytes
				bra		ckid_loop

;Test for SETUP jumper, and go to set up the EEPROM if it's there
			btfsc	PORTE,SETUP,0
			bra		edit_eeprom

; The ID is good - assume the EEPROM is properly initialized

;Is the user trying to load code?
			movlw	.40					;40 * 25 mS = wait for 1000 mS
			rcall	stall_25ms

			rcall	get_rxchr			;get Rx chr. W will not equal 'L' if nothing typed
			xorlw	'L'					;L takes us to the loader
			btfss	STATUS,Z,0
			goto	LOADED_CODE			;no L typed: go to programming firmware

;fall into code_loader

	page
;################################################################################
;# Intel Hex Firmware Loader													#
;# Receive an entire file of Intel Hex records, and write them to FLASH memory	#
;# as appropriate.																#
;#																				#
;# Entry here is only from initialization (above), when the user has typed		#
;# several 'L's (that must be flushed), or from the dumme firmware code.		#
;################################################################################
; Print sign-on message, and initialize variables at beginning of file
code_loader:	rcall	start_loader			;clear counters, print sign-on message

;eat any following 'L's without echoing them
flush_l:			rcall	get_rxchr			;Z set if no chr. This does not echo
					bz		flush_l				;no characters

					xorlw	'L'
					bz		flush_l				;spin until non-'L' character

;echo the first non-'L' character, and begin looking for hex records
				rcall	printr0					;manually echo the first non-'L' chr

				bra		load_start				;we already have the first chr in R0

;--------------------------------------------------------------------------------------
; (1)Wait for colon to start an Intel Hex record. If the user types three sequential
; ESCs, *and* we are not in the middle of a load (i.e. FLASH_MEM is not set), then
; test_esc will go to main. Ignore everything else.
;--------------------------------------------------------------------------------------
wait_recstart:	rcall	test_esc				;check for 3 ESCs, and get chr into R0

load_start:		movf	R0,W,0					;beginning of hex record?
				xorlw	':'						;all records start with colon
				bnz		wait_recstart			;spin until we get a colon

				clrf	CHECKSUM,0				;initialize checksum
				bcf		KERN_FLAGS,ERR_FLAG,0	;good hex so far

;--------------------------------------------------------------------------------------
; (2)Get hex record byte count and save it in R2
;--------------------------------------------------------------------------------------
				rcall	get_rqhex2				;get byte count
				movwf	R2,0					;save record byte count

;--------------------------------------------------------------------------------------
; (3,4)Get high & low address bytes from hex record, save them in ADDRESSH, ADDRESSL
;--------------------------------------------------------------------------------------
				rcall	get_rqhex2				;get address high byte into W & R1
				movwf	ADDRESSH,0				;save high byte in ADDRESSH
				rcall	get_rqhex2				;get address low byte into W & R1
				movwf	ADDRESSL,0				;save low byte in ADDRESSL

;--------------------------------------------------------------------------------------
; (5)Get record-type byte and save it in REC_TYPE. Set msb of ADDRESSH if not type 0
; record. (msb of ADDRESSH set means this record has no FLASH data.) Also set msb
; of ADDRESSH if FLASH memory has not yet been selected by a type 4 record
;--------------------------------------------------------------------------------------
				rcall	get_rqhex2				;get Hex record type byte into W & R1
				movwf	REC_TYPE,0				;save record type in REC_TYPE

				movf	REC_TYPE,F,0			;test for type 0 record
				btfsc	STATUS,Z,0				;if no, remember no data should be loaded
				btfss	KERN_FLAGS,FLASH_MEM,0	;type 0. FLASH memory already selected?
				bsf		ADDRESSH,7,0			;set msb so data won't load

;--------------------------------------------------------------------------------------
; Is this in a different FLASH row pair as the previously-received record? Is this
; record not a data record at all? If so, we must first flush the RAM buffer by
; writing it to FLASH, before we can receive the data from this new record.
; If msb of ADDRESSH is set, then this record should not cause a write to FLASH.
; Reasons for not loading data (msb of ADDRESSH is set):
; 1) This is not a data record (not type 0)
; 2) FLASH memory has not yet been selected (haven't received type 4 record with 0000)
;--------------------------------------------------------------------------------------
				movf	ROWA_LOW,W,0		;compare record's address low byte
				xorwf	ADDRESSL,W,0		;low byte row number match?
				andlw	0xC0				;test for 64-byte row pair
				bnz		new_row

				movf	ROWA_HIGH,W,0		;high byte match?
				andlw	0x7f				;so that we trap msb set in ADDRESSH
				xorwf	ADDRESSH,W,0
				bz		same_row

;--------------------------------------------------------------------------------------
; This record is not in the same FLASH row pair as the previous record. Was the
; previous record one that should be written to FLASH? If so, we must flush the RAM
; buffer into FLASH before we can receive data from this next record.
;
;  FLASH_MEM set for program data, cleared otherwise (set by extended address record)
;  ADDRESSL = low byte of new record address
;  ADDRESSH = high byte of new record address - msb set means none
;  REC_TYPE = record type for new record
;  R2 = record byte count
;  RECOUNTH,RECOUNTL = total record count
;  ROWA_HIGH, ROWA_LOW = address of previous completed record 
;  ROWA_HIGH bit 7 set means the previous record was not FLASH data
;  FSR1H = high(RAM_BUF)
;--------------------------------------------------------------------------------------
new_row:		btfss	ROWA_HIGH,7,0	;was previous row a FLASH data record (msb cleared)?
				rcall	write_flash		;yes: write it to FLASH now

;--------------------------------------------------------------------------------------
; Now that the RAM buffer is available, deal with the new record that we have already
; begun to receive. We have already received the byte count, address, record type. 
;
; Set up to read hex record data into the RAM buffer
;
;  FLASH_MEM set for program data, cleared otherwise (extended address stuff)
;  ADDRESSL = low byte of new record address
;  ADDRESSH = high byte of new record address - msb set means none
;  REC_TYPE = record type for new record
;  R2 = record byte count
;  RECOUNTH,RECOUNTL = total record count
;  ROWA_HIGH, ROWA_LOW = address of previous completed record (msb set means none)
;  FSR1H = high(RAM_BUF)
;--------------------------------------------------------------------------------------
;compute and save new FLASH row-pair address in ROWA_HIGH, ROWA_LOW
				movff	ADDRESSH,ROWA_HIGH		;high address, including msb flag

				movf	ADDRESSL,W,0
				andlw	0xC0					;make it a 64-byte row-pair address
				movwf	ROWA_LOW,0				;...saved in PREVA

;--------------------------------------------------------------------------------------
; See if this data is actually for FLASH memory (msb of high address byte cleared).
; If so, read the existing FLASH data into the buffer before receiving the hex record
; data, so that any FLASH data in this row that is not part of the hex file will remain
; unchanged.
;
;  FLASH_MEM set for program data, cleared otherwise (extended address stuff)
;  ADDRESSL = low byte of new record address
;  ADDRESSH = high byte of new record address - msb set means none
;  REC_TYPE = record type for new record
;  R2 = record byte count
;  RECOUNTH,RECOUNTL = total record count
;  ROWA_HIGH, ROWA_LOW = ADDRESSH, ADDRESSL
;  FSR1H = high(RAM_BUF)
;--------------------------------------------------------------------------------------
				btfss	ROWA_HIGH,7,0			;is this a FLASH data record (msb cleared)?
				rcall	read_ver_flash			;yes: read the flash data into RAM

;--------------------------------------------------------------------------------------
; The FLASH row-pair is already set up, and FSR1H already points to the RAM buffer.
; Now set up FSR1L as RAM buffer pointer for this hex record. The buffer address for
; this hex record is the address within the FLASH row-pair - the low 6 bits of the
; record address.
;
;  FLASH_MEM set for program data, cleared otherwise (extended address stuff)
;  ADDRESSL = low byte of new record address
;  ADDRESSH = high byte of new record address - msb set means none
;  REC_TYPE = record type for new record
;  R2 = record byte count
;  RECOUNTH,RECOUNTL = total record count
;  ROWA_HIGH, ROWA_LOW = address of previous completed record (msb set means none)
;  FSR1H = high(RAM_BUF)
;--------------------------------------------------------------------------------------
same_row:		movf	R2,F,0					;0-data-byte record?
				bz		dr_ckcum				;yes: just check the checksum

				movf	ADDRESSL,W,0			;low byte of FLASH address		
				andlw	0x3F					;make it a RAM buffer address
				movwf	FSR1L,0 

;--------------------------------------------------------------------------------------
; Loop to get all R2 bytes of data in this record. We assume that all bytes
; within this record will fit within the same FLASH row-pair.
;--------------------------------------------------------------------------------------
dr_data_loop:		rcall	get_rqhex2			;get a hex data byte into W & R1
					movwf	POSTINC1			;write byte to buffer, bump pointer
					decfsz	R2,F,0				;bump & test loop pointer
					bra		dr_data_loop

;--------------------------------------------------------------------------------------
; Get and test the record checksum. Report checksum error if wrong.
;--------------------------------------------------------------------------------------
dr_ckcum:		rcall	get_rqhex2				;get checksum into Z
				lfsr	0,cksumerror_msg		;checksum error message
				btfss	STATUS,Z,0				;checksum should be zero
				rcall	report_error

;--------------------------------------------------------------------------------------
; Report any hex errors accumulated during this record
;--------------------------------------------------------------------------------------
				btfsc	KERN_FLAGS,ERR_FLAG,0
				rcall	hex_error

;--------------------------------------------------------------------------------------
; If this record has data to be loaded into FLASH, then bump RECOUNTH,RECOUNTL
;--------------------------------------------------------------------------------------
				btfsc	ADDRESSH,7,0			;flash data record to load?
				bra		not_written				;no: don't bump count

				infsnz	RECOUNTL,F,0			;bump FLASH data record count
				incf	RECOUNTH,F,0			;carry to high byte if low byte is 0
not_written:
;--------------------------------------------------------------------------------------
; Deal with the record type.
;--------------------------------------------------------------------------------------
;was this record type 1? Done loading hex file if so. 
				decf	REC_TYPE,W,0			;test for REC_TYPE = 1
				bz		load_done

;was this record type 4? if not, we are done with this record
				xorlw	0x03					;we already decremented REC_TYPE into W
				bnz		wait_recstart			;not type 4: done with this record

;This was a type 4 record. Set FLASH_MEM if record data = 0000 (FLASH memory space)

				bcf		KERN_FLAGS,FLASH_MEM,0

				clrf	FSR1L,0					;look at first data byte
				movf	POSTINC1,W,0			;and the second data byte
				iorwf	INDF1,W,0				;both must be zero to select FLASH memory
				btfsc	STATUS,Z,0
				bsf		KERN_FLAGS,FLASH_MEM,0

				bra		wait_recstart			;with REC_TYPE = 1								

;--------------------------------------------------------------------------------------
; The code load is complete. Print results, and start over
;--------------------------------------------------------------------------------------
load_done:		lfsr	0,reccount_msg		;Loaded recs:
				rcall	loader_printf

				movff	RECOUNTH,R1			;record count
				movff	RECOUNTL,R0
				rcall	print_dec

				lfsr	0,errcount_msg		;"errors:"
				rcall	loader_printf

				movff	ERRCNTH,R1			;error count
				movff	ERRCNTL,R0
				rcall	print_dec

				rcall	start_loader		;clear counters, reprint sign-on message
				bra 	wait_recstart		;back to the beginning

	page
;################################################################################
;# UART Interrrupt Handlers														#
;#   This services both the transmit and receive UART interrupts once.			#
;#   Note: interrupts cause the PIC to save W, BSR's, STATUS automatically in 	#
;#   shadow registers. (Page 112)												#
;#   FSR2 is reserved for use by these ISRs.									#
;################################################################################

;Service the UART receive interrupt, if needed. The receive interrupt
;is always enabled, though the general interrupt bit may be disabled.
int_service:	btfsc	PIR1,RCIF,0			;test for pending UART rx interrupt
				rcall	rxi_service

;Service the UART transmit interrupt, if needed
				btfsc	PIE1,TXIE,0			;is the transmit interrupt enabled?
				btfss	PIR1,TXIF,0			;y: test for pending UART tx interrupt
				retfie	1					;n: done. restire & return

;Fall into txi_service

;********************************************************************************
;* Transmit Interrupt Routine
;* Transmit UART character from transmit queue
;* If this empties the transmit queue, then turn TX interrupt off 
;* On Entry:
;*     The transmit queue is NOT empty
;*     Transmit is NOT blocked by XOFF
;*     The next chr to transmit is at TQ_OPTR
;* On Exit:
;*     if Tx queue is empty, then the transmit int is disabled via TXIE (in PIE1)
;*     and TQ_EMPTY is set
;********************************************************************************
txi_service:	movlw	high(TX_QUEUE)			; point FSR0 to transmit queue
				movwf	FSR2H,0
				movf	TQ_OPTR,W,0
				movwf	FSR2L,0

				movf	INDF2,W,0				;get queue chr

				movwf	TXREG,0					;transmit now

;circular-increment queue out-pointer, assuming aligned queue
				inc_tq	TQ_OPTR

;If out-pointer now equals in-pointer, then the queue has become empty.
;If so, turn off transmit interrupt
				movf	TQ_IPTR,W,0
				xorwf	TQ_OPTR,W,0
				bnz		int_return

				bsf		INT_FLAGS,TQ_EMPTY,0	;flag that the queue is empty
				rcall	mask_txint				;mask the transmit interrupt

;-------------------------------------------------------;
;Restore state, and return from the interrupt
;-------------------------------------------------------;
int_return:		retfie	1						;restore registers on the way out

;********************************************************************************
;* Receive Interrupt Subroutine
;*  Note: This is a subroutine that gets called only by the interrupt handler
;*  Note: we ignore framing and overrun errors.
;* On Entry:
;*     Character is waiting in the UART receiver
;*     RQ_FULL is set if the Rx queue is already full.
;*     The next available slot in the queue is at RQ_IPTR
;*     XOFF_STATE is set if transmission has already been stopped by a previously
;*      received XOFF character.
;*     PREV_CR is set if the previously received character was a CR
;*     PREV_LF is set if the previously received character was a LF
;*
;* On Exit:
;*     Chr received from the UART, parity stripped, nulls ignored
;*     Chr is put into rx queue, if there is room (RQ_FULL = 0).
;*     If the queue is already full (RQ_FULL = 1), then the chr is discarded.
;*     RQ_FULL gets set if the receive queue has just become full.
;*     A LF that immediately follows a CR gets ignored. Other LFs get translated
;*      into CRs.
;*     A CR that immediately follows a LF gets ignored.
;*     PREV_CR and PREV_LF are used to convert CR-LF, LF-CR, and LF into just CR 
;*     XOFF_STATE set & transmit interrupt disabled if TX should stop due to XOFF
;*     XOFF_STATE cleared if it had been set and we received any non-null chr
;********************************************************************************
;point FSR0 to next empty queue space
rxi_service:	movlw	high(RX_QUEUE)
				movwf	FSR2H,0
				movf	RQ_IPTR,W,0
				movwf	FSR2L,0

;Get received chr into W, strip parity, skip nulls
				movf	RCREG,W,0				;get received data from UART
				andlw	0x7f					;strip parity, test for null
				bz		rxi_done				;just ignore nulls

;see if transmit is blocked due to XOFF. If so, any received chr (except a null)
;will XON us, and we eat that character
				btfsc	INT_FLAGS,XOFF_STATE,0
				bra		rxi_do_xon

;put received chr in queue. If it shouldn't go in the queue, we just won't
;bump the queue pointer later.
				movwf	INDF2,0					;save received chr in queue

;If this is an XOFF character, then disable transmit without bumping RQ_IPTR
				xorlw	XOFF					;is this an xoff?
				bz		rxi_do_xoff				;if so, don't bump queue pointer

;Test for CR  which gets special treatment below
				xorlw	XOFF ^ CR				;Z set if chr = CR
				bz		rxi_cr					;deal with CR

;Test for LF  which gets special treatment below
				xorlw	CR ^ LF					;Did we just get a LF?
				bz		rxi_lf					;deal with LF

;Neither CR or LF -  cancel any CRLF sequence progress
				bcf		INT_FLAGS,PREV_CR,0
				bcf		INT_FLAGS,PREV_LF,0

;The character is already in place in the queue. Actually enqueue the character
;now by bumping the in-pointer - unless the queue is full
rxi_enqueue:	btfsc	INT_FLAGS,RQ_FULL,0		;full queue?
				return							;full: drop chr on floor

				inc_rq	RQ_IPTR					;circular-increment queue pointer

;Set RQ_FULL if we just became full
				movf	RQ_IPTR,W,0
				xorwf	RQ_OPTR,W,0				;does RQ_IPTR = RQ_OPTR?
				bnz		rxi_done				;n: the queue is not full

				bsf		INT_FLAGS,RQ_FULL,0		;y: note full queue

;Done with receive interrupt, so return
rxi_done:		return

;------------------------------------------------------------------------------
; Translate CR-LF into CR. This requires looking at the state from the previous
; chr, and remembering the state of this chr for next time.
;------------------------------------------------------------------------------
rxi_cr:			btfsc	INT_FLAGS,PREV_LF,0		;LF following a CR?
				bra		rxi_crlfdone			;yes: end of sequence

				bsf		INT_FLAGS,PREV_CR,0		;remember that we got a fresh CR
				bra		rxi_enqueue				;go bump queue pointer to enqueue

;------------------------------------------------------------------------------
; Translate LF-CR into CR. This requires looking at the state from the previous
; chr, and remembering the state of this chr for next time.
;------------------------------------------------------------------------------
rxi_lf:			btfsc	INT_FLAGS,PREV_CR,0		;LF following a CR?
				bra		rxi_crlfdone			;yes:deal with sequence

				movlw	CR						;translate LF into CR
				movwf	INDF2,0

				bsf		INT_FLAGS,PREV_LF,0		;remember that we got a LF
				bra		rxi_enqueue				;go bump queue pointer to enqueue

;------------------------------------------------------------------------------
; We've just completed a CR-LF or LF-CR sequence, deliberately not enqueing
; the second chr of one of these sequences. Now, reset state.
;------------------------------------------------------------------------------
rxi_crlfdone:	bcf		INT_FLAGS,PREV_CR,0		;end of sequence
				bcf		INT_FLAGS,PREV_LF,0
				return							;don't bump queue pointer
				
;------------------------------------------------------------------------------
; Disable transmit because of XOFF received
;------------------------------------------------------------------------------
rxi_do_xoff:	bsf		INT_FLAGS,XOFF_STATE,0	;set XOFF flag

;fall into mask_txint

;------------------------------------------------------------------------------
; Local subroutine to mask the transmit interrupt
;------------------------------------------------------------------------------
mask_txint:		bcf		PIE1,TXIE,0				;mask tx interrupt			
				return

;------------------------------------------------------------------------------
; Clear XOFF state, and re-enable transmit interrupt unless queue is empty
;------------------------------------------------------------------------------
rxi_do_xon:		bcf		INT_FLAGS,XOFF_STATE,0	;clear XOFF flag

;if the queue is not empty, return with transmit interrupt enabled
				btfss	INT_FLAGS,TQ_EMPTY,0
				bsf		PIE1,TXIE,0				;not empty: unmask tx interrupt

				return

	page
;################################################################################
;# Subroutines
;################################################################################

;*==============================================================================*
; Subroutines to report load errors
; On entry at report_error:
;     FSR0H:FSR0L = message address
; On Exit:
;     W = 0
;     R0, R1, FSR0, TBLPTRL, TBLPTRH, TABLAT trashed
;*==============================================================================*
hex_error:		lfsr	0,hexerror_msg		;hex character error

report_error:	incf	ERRCNTL,F,0
				btfsc	STATUS,Z,0
				incf	ERRCNTH,F,0

; fall into loader_printf, with message address in FSR0

;*==============================================================================*
;Subroutine to print string at FSR0
;*==============================================================================*

loader_printf:	movff	FSR0L,TBLPTRL
				movff	FSR0H,TBLPTRH

;Fall into printf

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Subroutine to print null-terminated strings
;
; String chrs are packed 2 to a 16-bit word, as the PIC assembler likes to do.
; The second (odd) byte may be a zero, if 2 strings were concactenated. The string
; is terminated by an even byte being 0. (A null odd byte will be ignored.)
; All CR's get translated to CR-LF by printw. 
; The receive interrupt is masked while the string is enqueued, so it goes in
; without echos interspersed. This makes error messages during hex loading
; look pretty. Note that long strings may cause us to miss rx characters.
; On Entry
;     TBLPTRH:TBLPTRL = address of string
; On Exit:
;     W = 0
;     R0 = last printed character, except CR, which will return LF
;     TBLPTRU = 0
;     TBLPTRH:TBLPTRL points to the next character after the terminating null
;     FSR0, TABLAT trashed
;     Receive interrupt (RCIE) is enabled
;*==============================================================================*
printf:		clrf	TBLPTRU,0				;there's nothing above 64K	
			bcf		PIE1,RCIE,0				;mask receive interrupts

;print loop - terminates on even-byte null (when the first byte of a word is 0)
printf_loop:	tblrd*+
				movf	TABLAT,W,0			;get string byte, test for 0
				bz		p_null

				rcall	printw				;print character from W
				bra	printf_loop

;check for an even-byte null, the string termination. Ignore null otherwise.
p_null:			btfss	TBLPTRL,0,0			;test TBLPTR bit 0 (has been post-incremented)
				bra		printf_loop			;clear means previous byte was odd

			bsf		PIE1,RCIE,0				;unmask receive interrupts
			return

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Subroutine to print R1:R0 as a a decimal value, with leading zeros suppressed
; On Entry:
;     byte to send is in W
; On Exit:
;     W,R0,R1,R2,PRODH:PRODL,FSR0,INP_FLAG trashed
;*==============================================================================*
print_dec:		bsf		KERN_FLAGS,INP_FLAG,0	;suppress leading zeros

				movff	R1,PRODH				;use PRODH & PRODL as temps
				movff	R0,PRODL

				movlw	high(.10000)			;sw\et up divisor
				movwf	R1,0
				movlw	low(.10000)
				movwf	R0,0
				rcall	pddigit

				movlw	high(.1000)
				movwf	R1,0
				movlw	low(.1000)
				movwf	R0,0
				rcall	pddigit

				clrf	R1,0
				movlw	.100
				movwf	R0,0
				rcall	pddigit

				movlw	.10
				movwf	R0,0
				rcall	pddigit

				movf	PRODL,W,0			;always print last digit
				bra		pascdig

;------------------------------------------------------------
; Local subroutine to calculate and print one decimal digit
; On Entry:
;   PRODH:PRODL = quotiant
;   R1:R0 = divisor
;   INP_FLAG set if leading zeros are to be suppressed
; On Exit:
;   PRODH:PRODL = remainder
;   INP_FLAG cleared if result is not 0
; Trashes R2
;------------------------------------------------------------
pddigit:	setf	R2,0					;start dividend as -1

pdigloop:		incf	R2,F,0				;compute dividend
				movf	R0,W,0
				subwf	PRODL,F,0
				movf	R1,W,0
				subwfb	PRODH,F,0
				bc		pdigloop

			movf	R0,W,0					;the above loop goes once too many times
			addwf	PRODL,F,0				;..so correct the remainder here
			movf	R1,W,0
			addwfc	PRODH,F,0

			movf	R2,W,0					;dividend
			btfss	STATUS,Z,0				;if not zero then end leading zero suppression
			bcf		KERN_FLAGS,INP_FLAG,0

			btfsc	KERN_FLAGS,INP_FLAG,0	;leading zero to suppress?
			return							;y: don't print it

pascdig:	addlw	'0'						;make it ASCII
			bra		printw					;print and return from there (trashes R0)

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Subroutine to print W as a 2-character hex value
; On Entry:
;     byte to send is in W
; On Exit:
;     W, R0, R1, FSR0 trashed
;*==============================================================================*
print_hex2:		movwf	R1,0					;temp save low nibble
				swapf	R1,W,0					;high byte in low nibble of W
				rcall	print_hex1				;send the high nibble

				movf	R1,W,0					;now the low nibble

;fall into print_hex1

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Subroutine to print one hex digit from the low nibble of W
; On Entry:
;     W low nibble = value to transmit
; On Exit:
;     W, FSR0 trashed
;     R0 = chr
;*==============================================================================*
;tricky binary to ASCII hex conversion
print_hex1:		andlw	0x0f					;W=just the low nibble
				sublw	0x09					;W=9-nib. is value greater than 9?
				btfsc	STATUS,C,0				;C set means <=9			
				addlw	'A'-'9'	- 1				;W=gap+9-nib
				sublw	'0' + 'A'-'9' + 0x08	;W='0'+gap+9(-gap)-9+nib
												;W='0'(+gap)+nib
;fall into printw to enqueue character in W

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Subroutine to print W
;    Carriage returns (CR) get translated into CR-LF
;    If W = XOFF or W = XON, then cancel any XOFF state on our transmitter side
; On Entry at printw:
;     W = chr to transmit
; On Entry at printr0 (not externally accessable):
;     R0 = chr to transmit
; On Exit:
;     W, FSR0 trashed
;     R0 = printed chr, except for CR - which returns with LF
;*==============================================================================*
printw:			movwf	R0,0						;save chr in R0

printr0:		movlw	high(TX_QUEUE)				;point FSR0 to transmit queue
				movwf	FSR0H,0

;See if tx queue is full, and stall if it is.
;The queue is full if TQ_IPTR = TQ_OPTR and the queue is not empty.			
pw_wait_txq:		movf	TQ_IPTR,W,0				;now check for full queue
					movwf	FSR0L,0					;and set up queue pointer low byte
					xorwf	TQ_OPTR,W,0				;Z set for empty or completely full
					bnz		pw_room					;no - then not full!

					btfss	INT_FLAGS,TQ_EMPTY,0	;in-pointer = out-pointer. Empty?
					bra		pw_wait_txq				;no: the queue is full
pw_room:
;put the character into the queue
				movf	R0,W,0						;recover chr to print
				movwf	INDF0,0						;put chr in the queue

;mask all interrupts while we mess with the queue pointer and flags
				bcf		INTCON,GIE,0

;if we are sending an XOFF or XON character, and if we are XOFF'd from transmitting,
;then undo XOFF state so that this XOFF or XON will actually get sent.
				xorlw	XOFF						;set Z if XOFF
				btfss	STATUS,Z,0
				xorlw	XOFF ^ XON					;set Z if XON
				btfsc	STATUS,Z,0
				bcf		INT_FLAGS,XOFF_STATE,0		;undo any transmit-side XOFF

;circular-increment the queue pointer
				inc_tq	TQ_IPTR

				bcf		INT_FLAGS,TQ_EMPTY,0		;tell txint the queue isn't empty

;Enable the transmit interrupt unless we are XOFF'd
				btfss	INT_FLAGS,XOFF_STATE,0		;don't enable ints if XOFF'd
				bsf		PIE1,TXIE,0					;enable transmit interrupt

;re-enable global interrupts
				bsf		INTCON,GIE,0

;If this chr was a CR, then send an LF too.
				movf	R0,W,0						;recover the printed character
				xorlw	CR							;was it a CR?
				btfss	STATUS,Z,0
				return								;n: don't need an LF

				movlw	LF			
				bra		printw						;y:go enqueue LF

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Subroutine to print null-terminated the serial number
; On Entry:
; trashes W,R0,FSR0,TBLPTRH,TBLPTRL
;*==============================================================================*
print_sn:	movlw	CR					;initial newline because of newsn_msg
			call	printw

			movlw	high(loader_msg)
			movwf	TBLPTRH

			lfsr	0,sn_msg			;"Serial Number: "
			rcall	loader_printf

			movlw	EE_SN				;serial number location
			movwf	EEADR,0

;fall into ee_print

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
;Print string from EEPROM
; On Entry:
;    EEADR = address of null-terminated string
;  trashes W,R0,FSR0,READR
;*==============================================================================*
ee_print:		rcall	rd_eeprom				;get a byte
				iorlw	0						;null termination?
				btfsc	STATUS,Z,0
			return								;y: done

				rcall	printw
				incf	EEADR,F,0
				bra		ee_print

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Subroutine to get one character from the Receive Queue
; (does not echo)
; On Exit:
;     Z set, W & R0 =0 if the queue is empty
;     otherwise, the character is in R0 and W
;     FSR0 trashed
;     GEI is set, enabling global interrupts
; (2.25 uS if no characters waiting)
;*==============================================================================*
get_rxchr:		movf	RQ_OPTR	,W,0		;(.25) check for empty queue
				movwf	FSR0L,0				;(.25) FSR0 will point to Rx queue
				xorwf	RQ_IPTR,W,0			;(.25) set Z if either full or empty

				btfss	INT_FLAGS,RQ_FULL,0	;(.25)queue is not empty if it is full
				bz		gr_done				;(.5)with Z set, W=0: nothing in the queue

;Finish pointing FSR0 at the next queue character
				movlw	high(RX_QUEUE)		;finish pointing FSR0 to receive queue 
				movwf	FSR0H,0

;circular-increment the Rx queue out-pointer, mask interrupts while we do
				bcf		INTCON,GIE,0		;mask all interrupts

				inc_rq	RQ_OPTR				;circular-increment out pointer (not FSR0...)
				bcf		INT_FLAGS,RQ_FULL,0	;queue is no longer full

;Get next chr from the Rx queue at FSR0
				movf	INDF0,W,0			;get the queue chr, clear Z flag
											;(since there are no nulls in the queue)

				bsf		INTCON,GIE,0		;unmask interrupts

gr_done:		movwf	R0,0				;(.25)return chr or 0 in R0 and W, Z cleared/set
				return						;(.5)

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Subroutine to get a line of input from the user, echoing as we go. ^C or ESC
; causes an immediate exit with ERR_FLAG set
; CR's are not echoed. BS and DEL allow line editing.
; On Entry:
;      W = max allowed characters
; On Exit:
;     0-terminated input line is at LINBUF.
;     W= 1st chr of line
;     FSR1 points to the beginning of LINBUF, unless control-C
;     CR_FLAG cleared, unless no chrs found
;     Z set if no bytes found or if control-C detected
;     ERR_FLAG set if control-C or ESC detected, cleared otherwise
;     trashes R0,R1 FSR0
;*==============================================================================*
getlin:		movwf		R1,0					;character count-down
			bcf			KERN_FLAGS,ERR_FLAG,0	;no control-C yet
			bcf			KERN_FLAGS,CR_FLAG,0	;no CR yet
			lfsr		1,LINBUF				;FSR1 points to the line buffer

getl1:			incf		R1,F,0				;room for terminating 0

getl_loop:			rcall		get_rxchr		;chr in W and R0
					bz			getl_loop		;wait for a character

				xorlw		CR					;end of input?
				bz			getl_done

				xorlw		CR ^ BS				;backspace?
				bz			getl_bs

				xorlw		BS ^ DEL			;delete?
				bz			getl_bs

				xorlw		DEL ^ CTRLC			;abort?
				bz			getl_abort

				xorlw		CTRLC ^ ESC			;other kind of abort?
				bz			getl_abort

				dcfsnz		R1,F,0				;room in buffer?
				bra			getl1				;n: chuck the character

				movf		R0,W,0				;recover character
				movwf		POSTINC1,0			;put chr in the line buffer, bump

				btfsc		KERN_FLAGS,ECHO_FLAG	;echo allowed?
				rcall		printw					;y:echo

				bra			getl_loop			;look for another chr

;backspace or delete encountered

getl_bs:		movf		FSR1L,W,0
				xorlw		low(LINBUF)			;zero bytes in queue?
				bz			getl_loop			;y: ignore bs

getl_dobs:		movlw		BS					;back up on screen
				rcall		printw
				movlw		' '
				rcall		printw
				movlw		BS
				call		printw

				incf		R1,F,0				;un-bump available byte counter
				decf		FSR1L,F,0			;dec pointer (buffer is all in same 256-byte page)
				bra			getl_loop

;Got a complete line. 			
getl_done:	clrf		INDF1,0					;Install terminating null
			lfsr		1,LINBUF				;FSR1 points to the line buffer again

			movf		INDF1,W,0				;test for 0 characters
			bnz			getl_ret
			bsf			KERN_FLAGS,CR_FLAG,0	;CR already detected

getl_ret:	return

getl_abort:	bsf			KERN_FLAGS,ERR_FLAG,0	;control-C or ESC detected
			return
	
;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; -->>NOTE: Entry is at ask_yn<<--
; Ask user yes or no
; On Entry:
;    W = max length of input line
; On Exit:
;    Z set if Y
;    FSR1, LINEBUF, R0 trashed
;*==============================================================================*
ask_again:	movlw	CR				;new line
			rcall	printw

ask_yn:		lfsr	0,yn_msg		;" (Y/N)?"
			rcall	loader_printf

			movlw	8				;allow more chrs than necessary
			rcall	getlin			;get user input
			rcall	get_uchr		;Get the first character from the line buffer
									;and convert to uppercase

			xorlw	'Y'				;yes respones?
			bz		got_yes			;y: return with Z flag set

			xorlw	'Y' ^ 'N'		;no response?
			bnz		ask_again		;n: it's garbage. ask again

			bcf		STATUS,Z		;clear Z: we got a no		

;Z flag set if 'y', cleared of 'n'
got_yes:	return

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Check for an abort (either Control-C or ESC) from the user
; On Exit:
;   Z set if no character found
;   Carry set if abort character found
;   W=R0=found character (if Z set)
; (4 uS if no chr waiting)
;*==============================================================================*
chk_abort:		rcall	get_rxchr			;(.5 + 2.25 if no chr)returns chr in R0 and W
				bcf		STATUS,C,0			;(.25)no abort found yet
				bz		ca_done				;(.5)Z flag set if no input

				xorlw	CTRLC				;abort?
				bz		ca_abort

				xorlw	CTRLC ^ ESC			;esc will abort too
				btfsc	STATUS,Z,0

ca_abort:		bsf		STATUS,C,0			;carry means abort found

				movf	R0,W,0				;recover typed character
				bcf		STATUS,Z,0			;indicate chr found
ca_done:		return						;(.5)

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Get one character from the Line Buffer, and convert it to uppercase,
; if necessary.
; On Entry:
;    FSR1 points to the next chr in the line buffer
; On Exit:
;    The next line buffer chr is in W & R0. Note that a CR is represented as 0.
;    FSR0 trashed
;    FSR1 incremented
;    CR_FLAG set if this chr is a CR
;*==============================================================================*
get_uchr:	rcall	get_chr				;get a chr from the line buffer into W & R0

;Convert any lowercase to uppercase
			sublw	'z'					;above lowercase z?
			bnc		case_ok				;yes: don't try to change case

			btfsc	R0,0x06,0			;if bit 6 is set, then clear bit 5
			bcf		R0,0x05,0			;that makes it uppercase

case_ok:	movf	R0,W,0				;get chr into W too
			return

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Get one character from the Line Buffer
; Note that the Line Buffer always terminates with a CR
; On Entry:
;    FSR1 points to the next chr in the line buffer
;    CR_FLAG is set if the CR for this line's already been found
; On Exit:
;    The next line buffer chr is in W & R0. Note that a CR is represented as 0.
;    FSR0 trashed
;    FSR1 incremented
;    CR_FLAG set if this chr is the end of the line (a null caused by a typed CR) 
;*==============================================================================*
get_chr:	movlw	0x00						;alread found this line's CR?
			btfsc	KERN_FLAGS,CR_FLAG,0
			bra		gchr_done

			movf	POSTINC1,W,0				;also test for 0, meaning CR
			btfsc	STATUS,Z,0
			bsf		KERN_FLAGS,CR_FLAG,0		;it's a CR

gchr_done:	movwf	R0,0
			return
	
;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Get a decimal number, less than 256
; On Entry:
;    FSR1 points to the next character of an input line. The decimal
;    number is terminated by either a null or a space.
;    CR_FLAG set if the CR for this line has already been found
; On Exit:
;   W = PRODL = input value in binary, defaults to 0
;   ERR_FLAG set if any sort of error
;   INP_FLAG set if value found
;    CR_FLAG set if the CR for this line has been found
; Trashes R0,PRODH
;*==============================================================================*
get_dec:	bcf		KERN_FLAGS,ERR_FLAG,0		;no errors yet
			bcf		KERN_FLAGS,INP_FLAG,0		;no input yet
			clrf	PRODL,0						;result so far

gd_loop:		rcall	get_chr					;get the next character into R0 and W
				btfsc	KERN_FLAGS,CR_FLAG,0	;end of input line?
				bra		gd_exit					;normal exit

				xorlw	' '						;end of parameter?
				bz	gd_exit						;normal exit

				bsf		KERN_FLAGS,INP_FLAG,0	;remember that input was found

				movlw	'0'						;de-ASCII
				subwf	R0,F,0

				movlw	.10						;previous digits * 10
				mulwf	PRODL,0

				subwf	R0,W,0					;Is new digit legal ASCII decimal?
				bc		gd_error

				movf	R0,W,0					;add in new digit
				addwf	PRODL,F,0
				bc		gd_error				;overflow?

				movf	PRODH,F,0				;overflow from multiply?
				bz		gd_loop					;n: get another digit

gd_error:	bsf		KERN_FLAGS,ERR_FLAG,0		;note error

gd_exit:	movf	PRODL,W,0					;result to W for return
			return

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Get a multi-character hex parameter value from the Line Buffer, skipping spaces
; and ignoring leading zeros. Returns default 0000 if there is no parameter.
; digits preceding the last 4 are ignored. This routine is not case-sensitive:
; lowercase is treated as uppercase.
; On Entry:
;    FSR1 points to the next chr in the line buffer
;    CR_FLAG set if the CR for this line has already been found
; On Exit:
;    FSR1 has been incremented as needed
;    R0 trashed
;    R1 = low byte of input value
;    R2 = high byte of input value
;    R1 & R2 = 0 if CR for this input has already been reached 
;    ERR_FLAG set if non-hex character found
;    CR_FLAG set if CR found
;    INP_FLAG set if a value is found - cleared if defaulting to 0
;*==============================================================================*
get_lbhex4:		bcf		KERN_FLAGS,INP_FLAG,0	;no characters yet
				bcf		KERN_FLAGS,ERR_FLAG,0	;no	hex errors yet
				clrf	R1,0					;value is 0 so far
				clrf	R2,0
				btfsc	KERN_FLAGS,CR_FLAG,0	;already found end of line?
				return							;return with R3:R1=0 if so

;skip over any leading spaces
gh4_skipspace:		rcall	get_uchr				;result in W & R0

					xorlw	' '						;ignore leading spaces
					bz		gh4_skipspace

;R0 = 1st new digit from LINBUF
;loop to get digits until space or CR. Throw away high digits (more than 4)
gh4_getdigit:		btfsc	KERN_FLAGS,CR_FLAG,0	;CR ends us
				return

					bsf		KERN_FLAGS,INP_FLAG,0	;remember that we found some user input

					movlw	0x0f
					andwf	R2,F,0					;chuck (clear) old high nibble of high byte
					movlw	0xf0
					andwf	R1,W,0					;peel off the high nibble of the low byte

					iorwf	R2,F,0					;combine the new highest remaining 2 nibbles
					swapf	R2,F,0					;put the 2 highest nibbles in the right order

					rcall	hex2bin					;convert new chr in R0 into low nibble of R1 & W
					bc		gh4_error				;C set means hex character error

					rcall	get_uchr				;get another chr - result in W & R0

					xorlw	' '						;trailing space ends us
					bnz		gh4_getdigit

				return

gh4_error:		bsf		KERN_FLAGS,ERR_FLAG,0		;indicate hex error
				return

			
;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Read SRAM
; On Entry:
;    ADDRESSH:ADDRESSL = SRAM address
; On Exit:
;    W=SRAM data
;    R0 trashed
;(about 15.25 uS)
;*==============================================================================*
rd_sram:	movlw	SRAM_DREAD			;(.25)data read command
			bra		rw_sram				;(.25) combined code

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Write SRAM
; On Entry:
;    ADDRESSH:ADDRESSL = SRAM address
;    W=SRAM data
; On Exit:
;    W, R0, R1 trashed
; About 15.25 uS
;*==============================================================================*
wr_sram:	movwf	R1,0					;(.25)save write data
			movlw	SRAM_DWRITE			;(.25)SRAM data write command

;Fall into rw_sram

;*==============================================================================*
;Subroutine to read or write to SRAM
; On Entry:
;    ADDRESSH:ADDRESSL = SRAM address
;    R1=SRAM write data for writes, dummy write data for reads
;    W=SRAM_DREAD or SRAM_DWRITE
; On Exit:
;    W = SPI data (SRAM data for reads, trash for writes)
;    R0,R1 trashed
; About 14.75 uS
;*==============================================================================*
rw_sram:	bcf		LATC,SRAM_CEn,0		;(.25)enable SRAM
			movff	SSPBUF,R0			;(.25)flush SPI interface, clear BF bit

			movwf	SSPBUF,0			;(.25)read or write command to SPI
			rcall	wait_spi			;(.5+2.75=3.25)

			movff	ADDRESSH,SSPBUF		;(.25)high address byte
			rcall	wait_spi			;(.5+2.75=3.25)

			movff	ADDRESSL,SSPBUF		;(.25)low address byte
			rcall	wait_spi			;(.5+2.75=3.25)

			movff	R1,SSPBUF			;(.25)write data (for writes)
										;..or dummy data (for reads)
			rcall	wait_spi			;(2.75)wait for write to complete

			bsf		LATC,SRAM_CEn,0		;(.25)disable SRAM
			return						;(.5)

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Read EEPROM (Page 95)
; On Entry at rd_epromw:
;    W = EEPROM address
; On Entry at rd_eprom:
;    EEADR = EEPROM address
; On Exit:
;    W=EEPROM data
;    Z set if data is 0
;    EEADR = specified address
; (1.75 uS)
;*==============================================================================*
rd_eepromw:	movwf	EEADR,0			;(.25)

rd_eeprom:	bcf		EECON1,EEPGD,0	;(.25)point to EEPROM
			bcf		EECON1,CFGS,0	;(.25)not config ROM
			bsf		EECON1,RD,0		;(.25)initiate read operation
			movf	EEDATA,W,0		;(.25)read EEPROM, test for 0

			return					;(.50)result in W

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Write EEPROM (Page 95)
; On Entry:
;    EEADR = EEPROM address
;    W=EEPROM data
; On Exit:
;    Interrupts are enabled (GIE is set)
;    W trashed
;*==============================================================================*
wr_eeprom:	bcf		PIR2,EEIF		;clear "write done" flag (page 95)

			movwf	EEDATA,0		;data to write

			bcf		INTCON,GIE,0	;disable interrupts

			movlw	(1 << WREN)		;Set up for EEPROM write
			movwf	EECON1,0

			movlw	0x55			;Write unlock sequence
			movwf	EECON2,0
			movlw	0xAA
			movwf	EECON2,0
			bsf		EECON1,WR,0		;write now

			bcf		EECON1,WREN,0	;disable writes

			bsf		INTCON,GIE,0	;enable interrupts

;wait for the write operation to complete  (per page 95)
wait_wre:		btfss	PIR2,EEIF,0
				bra		wait_wre

			return

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Stall for about W * 25 mS
; On Exit:
;      R0,R1,W cleared
;*==============================================================================*
stall_25ms:		movwf	R1,0
s25_loop:			movlw	.100
					rcall	stall_250us
					decfsz	R1,F,0
					bra		s25_loop
				return

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Stall for about 250 * W uS
; On Exit:
;      R0,R1,W cleared
;*==============================================================================*
stall_250us:	movwf	R0,0			;(0.25)

s250_loop:			movlw	.248		;(0.25) (250 inner loop)
					rcall	stall_1us	;(249.00)
					decfsz	R0,F,0		;(0.25)
					bra		s250_loop	;0.50)

				return					;(.5)

;*==============================================================================*
; Subroutine to start the loader
; On Exit:
;     ERRCNTH, ERRCNTL = 0000
;     RECOUNTH, RECOUNTL = 0000
;     FLASH_MEM = 0
;     msb of ROWA_HIGH = 1, indicating the previous row wasn't FLASH data
;     FSR1H = high(RAM_BUF)
;     R0 = last printed character of loader_msg
;     R1, FSR0, TBLPTRL, TBLPTRH, TABLAT trashed
;*==============================================================================*
start_loader:
;Print the serial number
				rcall	print_sn

				clrf	ERRCNTL,0			;no errors yet
				clrf	ERRCNTH,0

				clrf	RECOUNTL,0			;RECOUNTL,HIGH counts hex records
				clrf	RECOUNTH	,0			;...actually loaded into FLASH

				bcf		KERN_FLAGS,FLASH_MEM,0	;not in FLASH space yet

				bsf		ROWA_HIGH,7,0			;previous record wasn't a FLASH data record

				movlw	high(RAM_BUF)			;set up FSR1 as data pointer (never changes)
				movwf	FSR1H,0					;data gets stored in RAM buffer

				lfsr	0,loader_msg			; print sign-on message
				bra		loader_printf			;return from there

;==============================================================================
; Subroutine to wait for the SRAM to be ready for data
; On Exit:
;    W = any data from the SPI
;==============================================================================
wait_spi:
loop_spi:
			btfss	SSPSTAT,BF,0		;(.25)spin until BF bit gets set
			bra		loop_spi			;(.25)spin for about 2 uS)

			movf	SSPBUF,W,0			;(.25)flush/read SPI interface, clear BF bit
			return						;(.5)

;*==============================================================================*
; Subroutine to wait for and get get one character from the Receive Queue
; echo the character upon receipt (except nulls)
; On Exit:
;     The character is in R0. If chr was CR, then R0 = LF
;     FSR0, W trashed
;     GEI is set, enabling global interrupts
;*==============================================================================*
wait_rxchr:		rcall	get_rxchr			;get chr into W and R0
				bz		wait_rxchr

				bra		printr0				;echo and return (trashes W)

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Find a command in a command table
; On Entry:
;      FSR1 points to the next input character in the user input buffer
;      TBLPTRH:TBLPTRL points to the particular command table
;      Each command table entry comprises  a null terminated ASCII command
;      string (where the null must be on an even byte and odd-byte nulls are
;      ignored) followed by a 1-byte "command index"
; On Exit:
;      W = 0 if no match
;      W = command index from table if match
;      FSR1 points to the next input character after the command
; Trashes W,R0,R1,FSR2,TBLPTRH:TBLPTRL
;*==============================================================================*
parse_cmd:	movff	FSR1L,FSR2L				;save pointer so we can retry
			movff	FSR1H,FSR2H

nxt_cmd:		movff	FSR2L,FSR1L			;restore pointer to user input
				movff	FSR2H,FSR1H
				bcf		KERN_FLAGS,CR_FLAG,0
				clrf	R1,0				;match so far

				tblrd*						;end of table?
				movf	TABLAT,W,0
				btfsc	STATUS,Z,0			;y: return with W=0
				return

nxt_cchr:			tblrd*+					;Get command character
					rcall	get_uchr		;get an upper-case character from the user input
											;result in W and R0

					xorlw	' '				;convert space seperator to a null
					btfss	STATUS,Z,0
					xorlw	' '				;not space: put it back

					xorwf	TABLAT,W,0		;compare 1 chr
					iorwf	R1,F,0			;accumulate result
											;set Z flag if still matching

					tstfsz	TABLAT,0		;end of this command?
					bra		nxt_cchr		;n: test another character

				btfss	TBLPTRL,0x00,0		;skip over word-alignment null if necessary
				tblrd*+

				tblrd*+						;point TABLAT to command index

											;Z was set/cleared by iorwf above
				bnz		nxt_cmd				;unless all chrs matched, try the next command

			movf	TABLAT,W,0			;get command index
			return						;return with W=command index

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Get an exactly 2-character hex value from the Line Buffer. This is as fast as
; possible so we can keep up with hex record loading This routine *IS* cases
; sensitive - lowercase will cause an error. This routine also does not skip
; over white space.
;
; On Exit:
;     Result in W and R1
;     Result added to CHECKSUM
;     Z set if checksum = 0
;     ERR_FLAG set if any hex character errors. (Never cleared though)
;     R0, FSR0 trashed
;*==============================================================================*
get_lbhex2:		rcall	get_lbhex1				;get high nibble
				btfsc	STATUS,C,0				;C set means error
				bsf		KERN_FLAGS,ERR_FLAG,0

				rcall	get_lbhex1				;get low nibble
				btfsc	STATUS,C,0				;C cleared means success
				bsf		KERN_FLAGS,ERR_FLAG,0

				addwf	CHECKSUM,F,0			;add to checksum, set Z appropriately
				return

;*==============================================================================*
; Subroutine to get one character from the Rx queue, convert it to a binary
; nibble and test for good hex. This routine *IS* case-sensitive: A-F must be
; uppercase.
;
; Note: this is not an externally-acessible subroutine because the programmer
; firmware must load received data through a line buffer, so that line editing
; (at least deleting characters) can be supported.
; On Entry:
;     R1 = previous nibble(s)
; On Exit:
;     R0 = binary value of nibble
;     W = R1 = combined nibbles (upper nibble of R1 entry value is lost.)
;     C cleared if no error, set if bad hex character
;     FSR0 trashed
;*==============================================================================*
get_lbhex1:		rcall	get_chr			;get line buffer chr into R0, case sensitive

				bra		hex2bin

;*==============================================================================*
; Get an exactly 2-character hex value from the Rx queue. This is as fast as
; possible so we can keep up with hex record loading This routine *IS* cases
; sensitive - lowercase will cause an error. This routine also does not skip
; over white space.
;
; Note: this is not an externally-acessible subroutine because the programmer
; firmware must load received data through a line buffer, so that line editing
; (at least deleting characters) can be supported.
; On Exit:
;     Result in W and R1
;     Result added to CHECKSUM
;     Z set if checksum = 0
;     ERR_FLAG set if any hex character errors. (Never cleared though)
;     R0, FSR0 trashed
;*==============================================================================*
get_rqhex2:		rcall	get_rqhex1				;get high nibble
				btfsc	STATUS,C,0				;C set means error
				bsf		KERN_FLAGS,ERR_FLAG,0

				rcall	get_rqhex1				;get low nibble
				btfsc	STATUS,C,0				;C cleared means success
				bsf		KERN_FLAGS,ERR_FLAG,0

				addwf	CHECKSUM,F,0			;add to checksum, set Z appropriately
				return

;*==============================================================================*
; Subroutine to get one character from the Rx queue, convert it to a binary
; nibble and test for good hex. This routine *IS* case-sensitive: A-F must be
; uppercase.
;
; Note: this is not an externally-acessible subroutine because the programmer
; firmware must load received data through a line buffer, so that line editing
; (at least deleting characters) can be supported.
; On Entry:
;     R1 = previous nibble(s)
; On Exit:
;     R0 = binary value of nibble
;     W = R1 = combined nibbles (upper nibble of R1 entry value is lost.)
;     C cleared if no error, set if bad hex character
;     FSR0 trashed
;*==============================================================================*
get_rqhex1:		rcall	wait_rxchr			;get queue chr into R0, case sensitive

				;fall into hex2bin

;************************************===========================================*
;* Externally Accessible Subroutine *
;************************************
; Subroutine to convert hex chr in R0 to a binary, testing for a good
; hex digit. Combine result into previous digit in R1.
; This routine *IS* case-sensitive: A-F must be uppercase.
; On Entry: 
;     R0 is ASCII chr for this nibble
;     R1 = previous nibble(s)
; On Exit:
;     R0 = binary value of nibble
;     W = R1 = combined nibbles (upper nibble of R1 entry value is lost.)
;     C cleared if no error, set if bad hex character
;     both nibbles will be trashed if a bad hex chr is received.
; (5 uS, worst case)
;*==============================================================================*
hex2bin:		movlw	'0'				;(0.25)remove ASCII bias
				subwf	R0,F,0			;(0.25)result into R0

;test for chr between '0' and '9'
				movlw	0x0a			;(0.25)
				subwf	R0,W,0			;(0.25)
				bnc		good_nibble		;C cleared means '0'-'9'. binary value in R0

;simultaneously check for gap chr between '9' and 'A' and for chr above 'F'
				movlw	'A'-'0'			;(0.25)shift down to zero base
				subwf	R0,F,0			;(0.25)legal values are now 0x00 through 0x05
				movlw	0x06			;(0.25)
				subwf	R0,W,0			;(0.25)C set means value is 0x06 or more

				btfsc	STATUS,C,0		;(0.50)C set means gap chr or above F, an error
				return					;Bad hex chr: Return with carry set
				
				movlw	0x0a			;(0.25)convert 0x00 - 0x05 into 0x0A - 0x0F
				addwf	R0,F,0			;(0.25)binary value in R0 (leaves C cleared)

;install new R0 value as the low nibble of R1, shifting old value to the high nibble
good_nibble:	swapf	R1,W,0			;(0.25)push previous low nibble into high position
				andlw	0xf0			;(0.25)strip off any previous high nibble
				iorwf	R0,W,0			;(0.25)combine with new low nibble
				movwf	R1,0			;(0.25)result back into R1 for return

				return					;(0.50)Success: value in R0 and carry cleared		

;*==============================================================================*
; Subroutine to read and compare two rows of FLASH data into the RAM buffer
; This is a dual-purpose subroutine, to save code space:
;   1) Used to copy FLASH into the buffer - in which case, Z is ignored on return
;   2) Used to verify that FLASH matches the buffer, in which case, the buffer
;      gets over-written, precluding any possibility of retrying the write.
; On Entry:
;    ROWA_HIGH:ROWA_LOW contain the FLASH address
;    FSR1H contains the high byte of the RAM buffer address
; On Exit:
;    Data from FLASH has been written to the RAM buffer
;    Z setand R0=0 if the entire buffer matched the data before it was copied
;    R0, R1, FSR1L, TBLPTRL, TBLPTRH, TABLAT all trashed
;*==============================================================================*
read_ver_flash:	rcall	flash_setup		;set up TBLPTRL, TBLPTRH, FSR1L, CFGS

				clrf	R0,0				;no mismatches yet

				movlw	FLASH_ROW * FLASH_BLOCK	;byte count
				movwf	R1,0

rv_loop:			tblrd*+					;read flash data into TABLAT, bump

;compare the flash to the buffer, and make R0 non-zero if not the same
;then put the flash data in the buffer
					movf	TABLAT,W,0		;get flash data
					xorwf	INDF1,W,0		;compare, don't bump pointer yet
					iorwf	R0,F,0			;result accumulates in R0

					movf	TABLAT,W,0
					movwf	POSTINC1,0		;read into buffer, bump pointer

					decfsz	R1,F,0			;do all bytes in this block
					bra		rv_loop

				movf	R0,F,0				;set Z if the buffer matched the data
				return

;*==============================================================================*
; Subroutine to write buffer data to two sequential FLASH rows (Page 90 onward)
; On Entry:
;    Data is in RAM buffer
;    ROWA_HIGH, ROWA_LOW = FLASH address
;    FSR1H contains the high byte of the data buffer address
; On Exit:
;    Flash has been written
;    Error message printed and ERR_FLAG set if verify fails
;    W, R0, R1, FSR1L trashed
;*==============================================================================*
write_flash:

;--------------------------------------------------------------------------------------
; Writing to FLASH is slow, and interrupts must be masked during the process - so we
; must tell the sender to pause (via XOFF) while we write to FLASH. The sender may not
; respond to the XOFF immediately, so we will stall here until we don't see any more
; characters coming into the receive queue from the UART. Once the UART receiver is
; quiet, we mask all interrupts. Note that we won't actually pull any characters from
; the receive queue here: we just look at the queue pointer to see if it is changing.
;--------------------------------------------------------------------------------------
				movlw	XOFF				;pause receiver
				rcall	printw				;trashes W, R0, FSR0

;wait until we get no received characters for at least 3 character times = 3.1 mS
wait_xoff:			movf	RQ_IPTR	,W,0	;snapshot of Rx queue in-pointer
					movwf	R1,0			;temp storage

					movlw	.13				;13 X 250 uS = 3.25 mS stall
					rcall	stall_250us		;trashes R0

					movf	R1,W,0			;any new characters?
					xorwf	RQ_IPTR,W,0		;RX_IPTR changes with each received chr
					bnz		wait_xoff

;--------------------------------------------------------------------------------------
; First, erase the FLASH row pair at address ROWA_HIGH, ROWA_LOW. This takes about 2 mS
; (page 90)
;--------------------------------------------------------------------------------------
				rcall	flash_setup			;set up TBLPTR, FSR1L

				bcf		INTCON,GIE,0		;disable interrupts

; Point to flash memory, enable an erase function (page 94)
				movlw	(1 << EEPGD) | ( 1 << WREN) | (1 << FREE)
				movwf	EECON1,0

				movlw	0x55				;unlock sequence (page 91)
				movwf	EECON2,0
				movlw	0xAA
				movwf	EECON2,0
				bsf		EECON1,WR,0			;Set WR bit to begin erase

;--------------------------------------------------------------------------------------
; Next, write two FLASH rows with data from RAM_BUF. This takes about 2 mS each???
; TBLPTRH,TBLPTRL are already set up; interrupts are already masked
;--------------------------------------------------------------------------------------
				movlw	FLASH_BLOCK			;R1 counts rows within a block
				movwf	R1,0
				tblrd*-						;decrement TBLPTR, fro upcoming pre-increment

wrow_loop:			movlw	FLASH_ROW		;R0 counts bytes within a row
					movwf	R0,0

;Loop to fill the PIC's flash holding register. We use pre-increment here so that
;TBLPTR is still pointing to the correct row when we actually write to flash.
wbyte_loop:				movf	POSTINC1,W,0	;read a data byte
						movwf	TABLAT,0		;write it to the row buffer
						tblwt+*					;pre-increment, write to buffer
						decfsz	R0,F,0			;next byte
						bra		wbyte_loop

;Write the holding register to the flash data row pointed to by TBLPTR
					movlw	(1 << EEPGD) | ( 1 << WREN)	;enable Flash write
					movwf	EECON1,0

					movlw	0x55				;unlock sequence
					movwf	EECON2,0
					movlw	0xAA
					movwf	EECON2,0
					bsf		EECON1,WR,0			;Set WR bit to begin write

;do a secod row, if we haven't already	
				decfsz	R1,F,0					;done all rows in this erase block?
				bra		wrow_loop				;n: do another row

;Done: disable flash write
				bcf		EECON1,WREN,0			;disable further writes

;--------------------------------------------------------------------------------------
; Finally, verify written data by comparing FLASH to the RAM buffer
; (This trashes the RAM buffer data.) 
;--------------------------------------------------------------------------------------
				rcall	read_ver_flash		;verify the data, trash the buffer
											;returns with Z set if match

				bsf		INTCON,GIE,0		;re-enable interrupts here, in case we report error

				lfsr	0,verifyerr_msg
				btfss	STATUS,Z,0
				rcall	report_error		;FSR0 = verifyerr_msg

;--------------------------------------------------------------------------------------
; Critical timing is done. send XON to the sender, and return.
;--------------------------------------------------------------------------------------
				movlw	XON					;un-pause receiver
				bra		printw				;return from there

;*==============================================================================*
; Subroutine to set up FSRL and TBLPTR for reads and writes to FLASH
; On Entry:
;    ROWA_HIGH:ROWA_LOW = flash row address
; On Exit:
;    FSR1L = 0
;    TBLPTRU:TBLPTRH:TBLPTRL = ROWA_HIGH:ROWA_LOW
;*==============================================================================*
flash_setup:	clrf	FSR1L,0				;beginning of RAM buffer

				clrf	TBLPTRU,0			;high address bits are 0
				movff	ROWA_HIGH,TBLPTRH
				movff	ROWA_LOW,TBLPTRL
				return

;*==============================================================================*
; Subroutine to test for the user typing three sequential ESCAPEs to go to
; LOADED_CODE. If FLASH_MEM is set, meaning that we are actively loading a file,
; then ESCAPEs will be ignored.
; On Entry:
;     FLASH_MEM is set if we are actively loading data into FLASH.
; On Exit (not to LOADED_CODE):
;     R0 = chr received. If chr was CR, then R0 = LF
;     W, R1, FSR0 trashed
;*==============================================================================*
test_esc:	movlw	3							;loop counter: count ESCs
			movwf	R1,0

wait_escapes:	rcall	wait_rxchr				;get user input into R0

				btfsc	KERN_FLAGS,FLASH_MEM,0	;active write to flash memory?
			return								;yes: ESC sequences will be ignored

				movf	R0,W,0
				xorlw	ESC						;another ESC?
				btfss	STATUS,Z,0
			return								;any non-ESC end the hunt

				decfsz	R1,F,0
				bra		wait_escapes

			goto	LOADED_CODE					;complete ESC sequence: go to LOADED_CODE

;*==============================================================================*
; Subroutine to set up the EEPROM, assuming it is corrupt (probably has
; never been written)
; On Entry:
;    TBLPTRH = high(loader_msg)
;*==============================================================================*
new_eeprom:		lfsr	0,eeinit_msg		;"Initializing EEPROM"
				rcall	loader_printf

;Write the ID string to EEPROM
				movlw	EE_INITLEN			;Initialization string length 
				movwf	R1,0				;byte counter

				movlw	low(loader_msg)
				movwf	TBLPTRL,0

				movlw	EE_INIT				;init string address in EEPROM
				movwf	EEADR,0				;..where wr_eeprom wants the address

wrid_loop:			tblrd*+						;get string byte, bump pointer
					movf	TABLAT,W,0

					rcall	wr_eeprom			;write an EEPROM byte
					incf	EEADR,F,0			;bump pointer

					decfsz	R1,F,0				;test all bytes
					bra		wrid_loop

;initialize the serial number
get_snagain:	rcall	get_sn
				bz		get_snagain				;insist on a serial number

;initialize all EEPROM variables 

				bra		clr_eevars

;*==============================================================================*
; Subroutine to edit the EEPROM, assuming it is not corrupt
; On Entry:
;    TBLPTRH = high(loader_msg)
;*==============================================================================*
edit_eeprom:

;Print the existing serial number
			rcall	print_sn

;Ask user for serial number

			rcall get_sn
;If the user responded with a 0-character line then don't change the serial number.
			lfsr	0,unchanged_msg		;"unchanged"
			btfsc	STATUS,Z			;Z set means no new serial number
			rcall	loader_printf

;Ask user permission to initialize all the EEPROM variables, and do so
			lfsr	0,clrvars_msg	;"Clear EEPROM variables"
			rcall	loader_printf
			rcall	ask_yn
			bnz		ee_done				;'n'

;Subroutine end...
clr_eevars:

			movlw	EE_VARLEN			;total variables length 
			movwf	R1,0				;byte counter

			movlw	EE_VARBEG			;init string address in EEPROM
			movwf	EEADR,0				;..where wr_eeprom wants the address

cc_loop:		movlw	0x00				;clear it
				rcall	wr_eeprom			;write an EEPROM byte
				incf	EEADR,F,0			;bump pointer

				decfsz	R1,F,0				;clear all bytes
				bra		cc_loop

			lfsr	0,cleared_msg		;"EEPROM variables cleared"
			rcall	loader_printf

;Go to the firmware if the user says ok. (If the firmware has not been installed,
;then the dummy firmware will go to back the loader.)
ee_done:	lfsr	0,run_msg			;"Run code?"
			rcall	loader_printf

			rcall	ask_yn
			bnz		edit_eeprom

			goto	LOADED_CODE

;*==============================================================================*
; Subroutine to get the serial number from the user
; On Entry:
;    TBLPTRH = high(loader_msg)
; On Exit:
;    Z set if no serial number given
; trashes W, R0, R1, FSR0, FSR1
;*==============================================================================*
get_sn:		lfsr	0,newsn_msg		;"New Serial Number: "
			rcall	loader_printf

			movlw	EE_SNLEN		;max serial number length
			call	getlin			;get null-terminated serial number from user
			btfsc	STATUS,Z		;Z set if 0 chrs, otherwise FSR1 = LINBUF
			return					;with Z set

			movlw	EE_SN			;serial number location
			movwf	EEADR,0			;address for wr_eeprom

;loop to write new serial number (including its null termination to EEPROM.
;IND1 points to the serial number in the line buffer.
wrsn_loop:		movf	POSTINC1,W,0	;get a chr
				movwf	R1,0			;for test at end

				rcall	wr_eeprom		;write chr to EEPROM at FSR0L	
				incf	EEADR,F,0		;next location in EEPROM
				movf	R1,F,0			;was this the null termination?
				bnz		wrsn_loop		;n: keep going

sn_same:	bcf		STATUS,Z			;clear Z: got a s/n
			return
	page
;*==============================================================================*
; Print an error message in the listing if this is code too big
;*==============================================================================*
loader_end:

	if	loader_end > LOADED_CODE
	error	"Martin sez: Code is too large!"
	endif

	end
