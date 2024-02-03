;********************************************************************************
;* Kernel Header File															*
;*																				*
;* Page references in the comments refer to the appropriate pages in Microchip	*
;* document DS40001303H "PIC18F2XK20/4XK20 Data Sheet"							*
;********************************************************************************

;*----------------------------------------------*
;Configuration Register setup
;*----------------------------------------------*
	config	FOSC=INTIO67		;Internal oscillator block (Page 283)
	config	FCMEN=OFF			;Disable failsafe clock monitor
	config	IESO=OFF			;Disable oscillator switchover mode

	config	PWRT=ON				;Power-up timer enabled (Page 284)
	config	BOREN=OFF			;Brownout reset disabled
	config	BORV=30				;Brownout reset voltage=3.0V (don't care)

	config	WDTEN=OFF			;Watchdog timer disabled (Page 284)
	config	WDTPS=32768			;Watchdog timer postscale (don't care)

	config	CCP2MX=PORTC		;CCP2 output on port bit C1 (Page 285)
	config	PBADEN=ON			;Port B bits <4-0> inputs configured as analog
	config	LPT1OSC=ON			;Timer 1 configured for low-power operation
	config	HFOFST=OFF			;HFINTOSC output & ready status delayed until osc stable
	config	MCLRE=OFF			;MCLR pin disabled

	config	STVREN=OFF			;disable stack overflow reset (Page 285)
	config	LVP=OFF				;Single-supply programming disabled
	config	XINST=OFF			;Disable extended instruction set
	config	DEBUG=OFF			;Disable debugger

	config	CP0=OFF				;Disable code protection block 0 (Page 286)
	config	CP1=OFF				;Disable code protection block 1
	config	CP2=OFF				;Disable code protection block 2
	config	CP3=OFF				;Disable code protection block 3

	config	CPB=OFF				;Disable boot-block code protection (Page 286)
	config	CPD=OFF				;Disable EEPROM protection

	config	WRT0=OFF			;Disable write protection block 0 (Page 287)
	config	WRT1=OFF			;Disable write protection block 1
	config	WRT2=OFF			;Disable write protection block 2
	config	WRT3=OFF			;Disable write protection block 3

	config	WRTC=ON				;Enable configuration register write protect (Page 287)
	config	WRTB=ON				;Enable boot-block write protect
	config	WRTD=OFF			;Disable data EEPROM write protect

	config	EBTR0=OFF			;Disable block 0 table-read protection (Page 288)
	config	EBTR1=OFF			;Disable block 1 table-read protection
	config	EBTR2=OFF			;Disable block 2 table-read protection
	config	EBTR3=OFF			;Disable block 3 table-read protection

	config	EBTRB=OFF			;Disable boot-block table-read protection (Page 288)

;*------------------------------------------------------------------------*
; I/O Port A Initialization Values (Page 113)
;  Bit  Name		 	Dir			Function		Use
;	0	M25V		Analog Input	C12IN0- 	25-volt compare
;	1	M21V		Analog Input	C12IN1-		21-volt compare
;	2	P18_0		Output			RA2			Pin 18 Control:
;	3	P18_1		Output			RA3			00 = TTL low
;												01 = TTL high
;												10 = -40V (Interil option req'd)
;												11 = Vpp
;
;	4	P19_0		Output			RA4			Pin 19 Control:
;	5	P19_1		Output			RA5			00 = TTL low
;												01 = TTL high
;												10 = +12V
;												11 = N/A (TTL high)
;
;	6	P20_0		Output			RA6			Pin 20 Control:
;	7	P20_1		Output			RA7			00 = TTL 0
;												01 = TTL 1
;												10 = +12V
;												11 = Vpp
;*-----------------------------	-------------------------------------------*
M25V			equ		0
M21V			equ		1
P18_0			equ		2
P18_1			equ		3
P19_0			equ		4
P19_1			equ		5
P20_0			equ		6
P20_1			equ		7

TRISA_INIT		equ		(1 << M25V) | (1 << M21V)
ANSEL_INIT		equ		(1 << M25V) | (1 << M21V)

;*------------------------------------------------------------------------*
; I/O Port B Initialization Values (Page 116)
;  Bit  Name			 Dir		Function		Use
;	0	P2Vn		output			RB0			0 applies +5V to pin 24
;	1	M13_1V		Analog Input	C12IN3-		13.1-volt compare
;	2	P24_12Vn	Output			RB2			0 applies +12V to pin 24
;	3	M12_75V		Analog Input	C12IN2-		12.75-volt compare
;	4	P24_6Vn		Output			RB4			0 applies 6.2V to pin 24
;	5	A9n			Output			RB5			Inverted address bit 9
;	6	DBUFDIR		Output			RB6			Data buffer direction:
;												1 = Write to EPROM
;												0 = Read from EPROM
;	7	DBUFOEn		Output			RB7			0 enables  data buffer output	
;*------------------------------------------------------------------------*
P24_5Vn			equ		0
M13_1V			equ		1
P24_12Vn		equ		2
M12_75V			equ		3
P24_6Vn			equ		4
A9n				equ		5
DBUFDIR			equ		6
DBUFOEn			equ		7

TRISB_INIT		equ		(1 << M13_1V) | (1 << M12_75V)
ANSELH_INIT		equ		(1 << M13_1V) | (1 << M12_75V)

;*------------------------------------------------------------------------*
; I/O Port C Initialization Values (Page 119)
;  Bit  Name		 Dir		Function		Use
;
;	0	A8n			Output		RC0			Inverted address bit 8
;	1	SRAM_CEn	Output		RC1			Active-low SRAM chip enable
;	2	VPPULSE		Output		CCP1/P1A	Vpp Switcher pulse output
;	3	SCK			Output		SCK1		SPI Clock to SRAM
;	4	SI			Input		SDI1		SPI Data In from SRAM
;	5	SO			Output		SDO1		SPI Data Out to SRAM
;	6	TxD			Output		TX1			RS-232 transmit data
;	7	RxD			Input		RX1			RS-232 receive data
;*------------------------------------------------------------------------*
A8n				equ		0
SRAM_CEn		equ		1
VPULSE			equ		2

TRISC_INIT		equ		0x90

;*------------------------------------------------------------------------*
; I/O Port D Initialization Values (Page 122)
;  Bit  Name		 Dir		Function		Use
;	0	D0			In/out		RD0			Data bit 0
;	1	D1			In/out		RD1			Data bit 1
;	2	D2			In/out		RD2			Data bit 2
;	3	D3			In/out		RD3			Data bit 3
;	4	D4			In/out		RD4			Data bit 4
;	5	D5			In/out		RD5			Data bit 5
;	6	D6			In/out		RD6			Data bit 6
;	7	D7			In/out		RD7			Data bit 7
;*------------------------------------------------------------------------*
TRISD_INIT		equ		0xff			;initialize as inputs

;*------------------------------------------------------------------------*
; I/O Port E Initialization Values (Page 125)
;  Bit  Name		 Dir		Function		Use
;	0	P21_0		Output		RE0			Pin 21 Control:
;	1	P21_1		Output		RE1			00 = TTL 0
;											01 = TTL 1
;											10 = -5V
;											11 = Vpp
;
;	2	ADRCLK		Output		RE2			Positive edge clocks address low byte
;											Also light BUSY LED when high
;	3	SETUP		In			RE3			SETUP (jumper to +3.3 on ICP connector)
;	(Note: Pin RE3 gets high voltage during PIC programming)
;*------------------------------------------------------------------------*
P21_0			equ		0
P21_1			equ		1
ADRCLK			equ		2
SETUP			equ		3

TRISE_INIT		equ		(1 << SETUP)		;SETUP is an input

;*------------------------------------------------------------------------*
;Baud Rates (See page 237)
; Assuming 16 MHz oscillator, and BRG16=1
;*------------------------------------------------------------------------*
BAUD9600	equ		.416
BAUD19200	equ		.207
BAUD38400	equ		.103
BAUD57600	equ		.68
BAUDRATE	equ		BAUD9600

;*----------------------------------------------*
;* System Equates (Page ??)						*
;* and fixed-location firmware routines			*
;*----------------------------------------------*
RESET_VECTOR	equ	0x00						;defined by PIC hardware
INT_VECTOR		equ	0x08						;interrupt vector (priority disabled)
CALL_TABLE		equ	0x0A						;more routines here

EEPROM			equ	0xF00000					;address for initializing EEPROM

;The following registers are no longer used, once the code is running.

REC_TYPE	equ	R2+1		;Intel hex record type
ROWA_LOW	equ	R2+2		;low address byte of previous hex record
ROWA_HIGH	equ	R2+3		;high address byte of previous hex record


;*--------------------------------------------------------------*
;Flash memory constants
;*--------------------------------------------------------------*
FLASH_ROW		equ		.32		;bytes per row
FLASH_BLOCK		equ		2		;flash rows per erase block

;*--------------------------------------------------------------*
;* External Serial SRAM Equates for the Microchip 23K640		*
;*--------------------------------------------------------------*
SRAM_DREAD		equ		0x03		;read data
SRAM_DWRITE		equ		0x02		;write data
SRAM_SREAD		equ		0x05		;Status read
SRAM_SWRITE		equ		0x01		;Status write

;===================================================================
; The following 2 macros make assumptions about where RX_QUEUE and
; TX_QUEUE are placed in memory, and what their sizes are. If you
; change the location or size of these queues, then these macros
; must also be changed.
;===================================================================
;specialized macro to increment receive queue pointers
;these assume that the receive queue is located on a 256-byte
;boundary, and is 256 bytes long.
inc_rq	macro pointer
		incf	pointer,F,0		;linear increment the pointer
								;any carry gets chucked to wrap
		endm

;specialized macro to increment transmit queue pointers
;these assume that the transit queue is located on a 32-byte
;boundary, and is 16 bytes long
inc_tq	macro pointer
		incf	pointer,F,0		;linear increment the pointer
		bcf		pointer,0x4,0	;clear any overflow
		endm

		