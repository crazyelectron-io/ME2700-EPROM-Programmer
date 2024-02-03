;* RAM Usage: RAM allocations are assigned to be compatible with the EPROM		*
;* programmer firmware.															*
;********************************************************************************

;Code Space assignments (from Kernel)
K_KERN_REV		equ	0x0002	;return kernal loader major rev and minor rev in high & low nibbles of W
K_PROG_ID		equ	0x0004	;return programmer ID in W. 00 for now.
K_CONIN			equ	0x000A	;get one chr from Rx Queue into W & R0. Z set if empty. Trashes FSR0.
K_CONOUT		equ	0x000C	;print W (returns chr in R0 unless chr was CR. Trashes FSR0.)
K_PRINTF		equ	0x000E	;print 0-terminated string at TBLPTRH:TBLPTRL. Trashes R0,FSR0,TBLPTR
K_EEPRINT		equ	0x0010	;print 0-terminated string from EEPROM at EEADR, Trashes R0,FSR0,READR
K_PRINTHEX1		equ	0x0012	;print low nibble of W as 1 hex chr. trashes R0,FSR0
K_PRINTHEX2		equ	0x0014	;print W as 2 hex chrs. Trashes R0,R1,FSR0
K_PRINTDEC		equ	0x0016	;print R1:R0 in decimal, leading zeros suppressed. Trash W,R0,R1,R2,FSR0,PRODH,PRODL
K_PRINTSN		equ	0x0018	;print this unit's serial number. Trashes FSR0.
K_HEX2BIN		equ	0x001A	;Convert hex value in R0 to binary, combine it with R1. Trashes FSR0.
K_GETLIN		equ	0x001C	;Get W-byte null-terminated input line from user. Returns FSR1= LINBUF
K_PARSE			equ	0x001E	;Search cmd table at TBLPTR for cmd at FSR0, return W=cmd index
K_GETCHR		equ	0x0020	;Get next LINBUF chr at FSR1. Result in W and R0
K_GETUCHR		equ	0x0022	;Get next LINBUF chr at FSR1 and convert to uppercase. Result in W & R0
K_GETDEC		equ	0x0024	;Get 8-bit decimal value from LINBUF at FSR1. Result in W & PRODL. Trash R0
K_GETHEX2		equ	0x0026	;Get 2-digit hex, result in W & R1, sum in CHECKSUM, Z set if CHECKSUM=0
K_GETHEX4		equ	0x0028	;Get 4-digit hex value from LINBUF at FSR1. Result in R2:R1. Trash W,R0
K_ASKYN			equ	0x002A	;ask user Y/N. Z set if Y, cleared if N. Trashes FSR0.
K_CHKABORT		equ	0x002C	;Check for user abort. Z if no chr, C if abort, chr in W and R0
K_RDSRAM		equ	0x002E	;read SRAM byte at ADDRESSH:ADDRESSL, Result in W, trash R0
K_WRSRAM		equ	0x0030	;write W to SRAM at ADDRESSH:ADDRESSL. Trashes W R0,R1
K_RDEEPROM		equ	0x0032	;read EEPROM byte at address W, result in W
K_WREEPROM		equ	0x0034	;write W to EEPROM at address EEADR
;2 slots reserved here
K_STALL25M		equ	0x003A	;stall for W * 25 mSec (trashes R0,R1
K_STALL250U		equ	0x003C	;stall for W * 250 uSec (trashes R0)
K_STALL1U		equ	0x003E	;stall for W + 1 uSec

LOADED_CODE		equ	0x800		;beginning of main programmer code

;*--------------------------------------------------------------*
;* Access RAM usage	(visible from every bank)					*
;* Those entries marked with (K) are fixed locations that are	*
;* accessible from the downloaded firmware.						*
;* Those marked with (I) are used by the interrupt service		*
;* routines.													*
;*--------------------------------------------------------------*
RQ_IPTR		equ	0x00		;(K)(I)points to first empty slot in receive queue
							;(Written only by Rx interrupt!)
RQ_OPTR		equ	0x01		;(K)(I)points to oldest chr in receive queue
TQ_IPTR		equ	0x02		;(K)(I)points to first empty slot in transmit queue
TQ_OPTR		equ	0x03		;(K)(I)points to oldest chr in transmit queue
							;(Written only by Tx interrupt!)
CHECKSUM	equ	0x04		;checksum for current hex record
ADDRESSL	equ	0x05		;address low byte
ADDRESSH	equ	0x06		;address high byte
RECOUNTL	equ	0x07		;low byte of count of hex records
RECOUNTH	equ	0x08		;high byte of count of hex records
ERRCNTL		equ	0x09		;16-bit error counter
ERRCNTH		equ	0x0A

INT_FLAGS	equ	0x0B		;(K)(I)flags used by int service routines and kernel routines
XOFF_STATE		equ	0x00		;(K)transmitter stopped due to XOFF state
TQ_EMPTY		equ	0x01		;(K)tx queue is empty
PREV_CR			equ	0x02		;(K)Previous received chr was a CR (used by rx int)
PREV_LF			equ	0x03		;(K)Previous received chr was a LF (used by rx int)
RQ_FULL			equ	0x04		;(K)Receive queue full

KERN_FLAGS	equ	0x0C		;(K)flags used by int service routines and kernel routines
ERR_FLAG		equ	0x00		;(K)generic error flag
FLASH_MEM		equ	0x01		;'FLASH memory has been selected' state bit (only during code load)
INP_FLAG		equ	0x01		;(K)input value found (never during code load)
ECHO_FLAG		equ	0x02		;(K)Echo enabled when set
CR_FLAG			equ	0x03		;(K)CR detected in LINBUF

R0				equ	0x0D		;general purpose registers
R1				equ	0x0E
R2				equ	0x0F		;Other header files assume that R2 is the last one

;*--------------------------------------------------------------*
;* Linear-memory buffers										*
;* Those entries marked with (K) are fixed locations that are	*
;* accessible by from the downloaded firmware.					*
;* The PIC18F44K20 has 768 bytes of SRAM. The first 96 bytes	*
;* map to the Access Bank.										*
;*--------------------------------------------------------------*
;Input line buffer
LINBUF			equ		.96					;just past the Access Bank
MAXLIN			equ		.80					;max input line size

;transmit queue (must be on (TQ_SIZE*2)-byte boundary)
TX_QUEUE		equ		0xE0				;(K) 16-byte transmit queue
TQ_SIZE			equ		0x10

;receive buffer (must be on RQ_SIZE-byte boundary)
RX_QUEUE		equ		0x100				;(K) 256-byte receive buffer
RQ_SIZE			equ		0x100

;General purpose RAM buffer, used during code load here 
RAM_BUF			equ		0x200			;(must be on 256-byte boundary)
RAMBUF_SIZ		equ		0x100			;256-byte RAM buffer

;*--------------------------------------------------------------*
;* Internal EEPROM allocation									*
;*--------------------------------------------------------------*
EE_INIT			equ		0x00					;initialized string
EE_INITLEN		equ		.20						;Room for loader_msg

EE_SN			equ		EE_INIT+EE_INITLEN		;Serial number
EE_SNLEN		equ		.8						;Length of serial number

EE_VARBEG		equ		EE_SN+EE_SNLEN			;beginning of variables
EE_VARLEN		equ		.256-EE_VARBEG			;room for variables



