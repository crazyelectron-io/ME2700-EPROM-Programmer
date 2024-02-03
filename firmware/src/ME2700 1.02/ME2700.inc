;****************************
; Various M2700 Constants	*
;****************************
; General constants
SRAM_SIZE		equ	0x2000			;8kB SRAM size
ME2700ID		equ	0				;expected ID from kernel

VPP_PW			equ	0x12			;pulse width for Vpp's PWM

PACCOUNT		equ .128			;pacifier character every 128 bytes written

MAXNAME			equ	.14				;max custom EPROM name character count

LISTROWS		equ	.16				;number of rows for the EL command

PROMPTCHR:		equ	'>'				;Prompt
;*------------------------------------------------------------------------*
;* RAM assignments
;*------------------------------------------------------------------------*
;General purpose registers (R0-R2 defined by Kernel):

R3				equ		R2+1
R4				equ		R3+1
R5				equ		R4+1
R6				equ		R5+1
R7				equ		R6+1
R8				equ		R7+1
R9				equ		R8+1

;Special function registers of our own

BUF_OFFSET		equ	R9+1			;buffer address offset
COUNTL			equ	BUF_OFFSET+1	;byte count low byte for commands & hex records
COUNTH			equ	COUNTL+1		;byte count high byte

ADDRESSL_SAVE	equ	COUNTH+1		;save for ADDRESSL
ADDRESSH_SAVE	equ	ADDRESSL_SAVE+1	;save for ADDRESSH
COUNTL_SAVE		equ	ADDRESSH_SAVE+1	;save for COUNT_LOW
COUNTH_SAVE		equ	COUNTL_SAVE+1	;save for COUNT_HIGH
ETYPE_SAVE		equ	COUNTH_SAVE+1	;save for ETYPE

SHARED			equ	ETYPE_SAVE+1	;shared pin flags
OEVPP				equ	0x00		;1 if -OE pin is also Vpp
CSVPP				equ	0x01		;1 if CS pin is also Vpp
OEPGM				equ	0x02		;1 if -OE pin is also PGM
CSPGM				equ	0x03		;1 if CS is also PGM

FLAGS1			equ	SHARED+1		;Flags and binary states
INTEL_STATE			equ	0x00		;Intel (rather than Motorola) record
A_STATE				equ	0x01		;general purpose flag
B_STATE				equ	0x02		;EB command in process (used for other things, too)
DFLT_FLAG			equ	0x03		;set if no first parameter was provided
FULL_FLAG			equ	0x04		;set if operation is for a complete EPROM
AO_MANUAL			equ	0x05		;0 means get address offset from 1st record
AO_INIT				equ	0x06		;1 when ADR_OFFSET is initialized
TEST				equ	0x07		; Test mode indicator

FLAGS2			equ	FLAGS1+1		;More flags. Bits marked (*) must remain where placed!
PMODE0				equ	0x00		;(*)00: pulse CS pin  01: pulse PGM pin
PMODE1				equ	0x01		;(*)10: pulse Vpp, return to 0V, 11: pulse Vpp, return to +5V
RDVPP				equ	0x02		;(*)Vpp level during read: 0=0V, 1=5V
FAST_PULSE			equ	0x03		;1 means <1 uS programming pulse
ADJ_VCC				equ	0x04		;(*)1 means Vcc requires manual adjustment
ADJ_VPP				equ	0x05		;(*)1 means Vpp requires manual adjustment
EXT_VPP				equ	0x06		;(*)1 means external Vpp required
CUST_EPROM			equ	0x07		;1 means current EPROM is custom

ADR_OFFSET		equ	FLAGS2+1		;hex record address high-byte offset
PPULSEL			equ	ADR_OFFSET+1	;(2 bytes)Programming pulse duration, in units of 10 uS
PPULSEH			equ	PPULSEL+1		;..not meaningful if FAST_PULSE=1
PPDELL			equ	PPULSEH+1		;(2 bytes)Delay between programming pulses, in units of 10 uS
PPDELH			equ	PPDELL+1		;..not meaningful if FAST_PULSE=1

PROGREPS		equ	PPDELH+1		;programming rpeitions (or rep limit for smart)
SMART1			equ	PROGREPS+1		;Smart programming mode, phase 1
SMART2			equ	SMART1+1		;Smart programming mode, phase 2
PACINDEX		equ	SMART2+1		;pacifier character counter

VSENSE_LOW		equ	PACINDEX+1		;lower Vpp voltage limit (0 means no limit)
VSENSE_HIGH		equ	VSENSE_LOW+1	;upper Vpp voltage limit (0xFF means no limit)


;*------------------------------------------------------------------------*
;* EEPROM assignments (retained when powered off)
;*------------------------------------------------------------------------*
ETYPE			equ		EE_VARBEG+0			;EPROM Type
PROGCNT			equ		ETYPE+1				;2-byte successful EPROM program count
FAILCNT			equ		PROGCNT+2			;2-byte failed program count
DINV_STATE		equ		FAILCNT+2			;NZ means EPROM data is inverted

CE_TABLE		equ		DINV_STATE+1		;custom EPROM definition table
CEPROM_COUNT	equ		4					;max supported custom EPROMs
CE_TABLEN		equ	CEPROM_COUNT*ET_RLENGTH	;table length
;*==============================================================================*
; Print an error message in the listing if EEPROM variables exceed the EEPROM
;*==============================================================================*
	if	CE_TABLE+CE_TABLEN > 256
	error	"Martin sez: EEPROM Overflow!"
	endif

