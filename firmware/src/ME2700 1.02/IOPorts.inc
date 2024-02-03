;****************************************
;* Input/Putput Port Bit Definitions	*
;****************************************

;*------------------------------------------------------------------------*
; I/O Port A Initialization Values (Page 113)
;  Bit  Name		 	Dir			Function		Use
;	0	M25V		Analog Input	C12IN0- 	25-volt compare
;	1	M21V		Analog Input	C12IN1-		21-volt compare
;	2	P18_0		Output			RA2			Pin 18 Control:
;	3	P18_1		Output			RA3			00 = TTL low
;												01 = TTL high
;												10 = -40V (Intersil option req'd)
;												11 = Vpp
;
;	4	P19_0		Output			RA4			Pin 19 Control:
;	5	P19_1		Output			RA5			00 = TTL low
;												01 = +5V
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
;	0	P24_5Vn		output			RB0			0 applies +5V to pin 24
;	1	M13_1V		Analog Input	C12IN3-		13.1-volt compare
;	2	P24_12Vn	Output			RB2			0 applies +12V to pin 24
;	3	M12_75V		Analog Input	C12IN2-		12.75-volt compare
;	4	P24_6Vn		Output			RB4			0 applies 6.2V to pin 24
;	5	P22n		Output			RB5			Inverted EPROM pin 22
;	6	DBUFDIR		Output			RB6			Data buffer direction:
;												0 = Read from EPROM
;												1 = Write to EPROM
;	7	DBUFOEn		Output			RB7			0 enables  data buffer output	
;*------------------------------------------------------------------------*
P24_5Vn			equ		0
M13_1V			equ		1
P24_12Vn		equ		2
M12_75V			equ		3
P24_6Vn			equ		4
P22n			equ		5
DBUFDIR			equ		6
DBUFOEn			equ		7

TRISB_INIT		equ		(1 << M13_1V) | (1 << M12_75V)
ANSELH_INIT		equ		(1 << M13_1V) | (1 << M12_75V)

;*------------------------------------------------------------------------*
; I/O Port C Initialization Values (Page 119)
;  Bit  Name		 Dir		Function		Use
;
;	0	P23n		Output		RC0			Inverted EPROM pin 23
; 											(inverted EPROM address bit 8)
;	1	SRAM_CEn	Output		RC1			Active-low SRAM chip enable
;	2	VPPULSE		Output		CCP1/P1A	Vpp Switcher pulse output
;	3	SCK			Output		SCK1		SPI Clock to SRAM
;	4	SI			Input		SDI1		SPI Data In from SRAM
;	5	SO			Output		SDO1		SPI Data Out to SRAM
;	6	TxD			Output		TX1			RS-232 transmit data
;	7	RxD			Input		RX1			RS-232 receive data
;*------------------------------------------------------------------------*
P23n			equ		0
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

