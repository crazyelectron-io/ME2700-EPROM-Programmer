;********************************************************************************
; EPROM Configuration Table Data Structure
;
; For inbuilt EPROM definitions, ET_NAME and all the PARTNO's are compressed
; pointers to strings in Flash. For custom EPROMs (stored in the PIC's onboard
; EEPROM), ET_NAME is the null-terminated name string itself, and PARTNOs,
; custom Vcc messages, and custom Vpp messages are not supported.
; Numbers in <angle braces> are bit numbers.
;********************************************************************************

ET_NAME			equ		0					;Pointer to EPROM name string
											;(The high byte is reserved and should be 0FFh)
ET_PARTNO1		equ		ET_NAME+2			;1st byte is manufacturer string, 2nd byte is part number string
ET_PARTNO2		equ		ET_PARTNO1+2
ET_PARTNO3		equ		ET_PARTNO2+2
ET_PARTNO4		equ		ET_PARTNO3+2
ET_PARTNO5		equ		ET_PARTNO4+2
ET_PARTNO6		equ		ET_PARTNO5+2
ET_PARTNO7		equ		ET_PARTNO6+2
ET_PARTNO8		equ		ET_PARTNO7+2
ET_PARTNO9		equ		ET_PARTNO8+2
ET_PARTNO10		equ		ET_PARTNO9+2

										;The following 2 string pointers overlap ET_PARTNO10
ET_VCCMSG		equ		ET_PARTNO10		;Pointer to Vcc adjustment string
ET_VPPMSG		equ		ET_VCCMSG+1		;Pointer to Vpp adjustment message

ET_BYTES		equ		ET_VPPMSG+1			;<7:4> = bits/byte
											;<3> reserved
											;<2:0> = Bytes/EPROM
											;  0: 256
											;  1: 512
											;  2: 1024
											;  3: 2048
											;  4: 4096
											;  5: 8192

;For the following EPROM signal assignments, pin numbers are encoded as follows:
;			0x00: none
;			0x01: pin 18
;			0x02: pin 19
;			0x03: pin 20
;			0x04: pin 21
;			0x05: pin 22
;			0x06: pin 23 (which is actually not used here, since code always assumes pin 23 is A8)

ET_A9A10		equ		ET_BYTES+1			;<7> reserved
											;<6:4> = A9 pin
											;<3> reserved
											;<2:0> = A10 pin

ET_A11A12		equ		ET_A9A10+1			;<7> = 1 means force A11 high always
											;<6:4> = A11 pin
											;<3> = 1 means this is the -Address Strobe pin
											;<2:0> = A12 pin

ET_OECS			equ		ET_A11A12+1			;<7> = 1 means -OE=+12V while programming (pin 20 only)
											;<6:4> = -OE pin (always active low)
											;<3> = 1 means active-high CS pin
											;<2:0> = CS pin

ET_PGMVPP		equ		ET_OECS+1			;<7> = 1 means active-high PGM pin
											;<6:4> = PGM pin
											;<3> reserved
											;<2:0> = Vpp pin	

ET_VBBVDD		equ		ET_PGMVPP+1			;<7:6> Reserved
											;<5> = Vdd assignment (pin 19):
											;   0: none
											;   1: pin 19 = +12V Vdd
											;<4> = Vbb assignment (pin 21):
											;   0: none
											;   1: Pin 21 = -5V Vbb
											;<3> reserved
											;<2:0> = Vcc voltage during programming
											;	0: 0V
											;	1: +5V
											;	2: +6.25V
											;	3: +12V

ET_VPPSETUP		equ		ET_VBBVDD+1			;<7> reserved
											;<6:4> = Vpp programming voltage:
											;	0: Off
											;	1: 12.75V
											;	2: 13.15V
											;	3: 21V
											;	4: 25V or 26V
											;   5: use external negative supply
											;<3> reserved
											;<2> = Vpp level during reading
											;  0 = 0V
											;  1 = +5V
											;<1:0> = programming pulse type:
											;      00 = high-voltage pulse on Vpp pin, return to 0V
											;      01 - high-voltage pulse on Vpp pin, return to +5V
											;      10 = digital pulse on PGM pin

ET_PROGREPS		equ		ET_VPPSETUP+1		;number of programming repetitions
											;If smart programming, this is the limit for P.

ET_PROGPULSE	equ		ET_PROGREPS+1		;Programming pulse duration
											;<7> x 100 multiplier
											;<7:0> = 00 means 0.25 uS (PGM pulse only!)
											;<7> = 0, <6:0> = 01-03 reserved
											;                 04 to 7F: pulse in units of 10 uS
											;<7> = 1, <6:0> = pulse in units of mS

ET_PPDELAY		equ		ET_PROGPULSE+1		;duration of delay between programming pulses
											;<7> x 100 multiplier
											;<7:0> = 00 means no delay
											;<7:0> = 01 means EEPROM-style polling
											;<7> = 0, <6:0> = 02-03 reserved
											;                 04 to 7F: pulse in units of 10 uS
											;<7> = 1, <6:0> = pulse in units of mS


ET_SMART1		equ		ET_PPDELAY+1		;Phase 1 Algorithm
											;00 means no smart programming (just program the
											;EPROM ET_PROGREPS times)
											;80 means just program until match
											;<6:4> Phase 1 type
											;    0: program until match, then program n times
											;    1: program until match, then 1 pulse
											;      that is n*ET_PROGPULSE long
											;    2: program until match (P), then nP times
											;    3: program until match (P), then 1 pulse
											;      that is n*P*ET_PROGPULSE long
											;    4: Program n times
											;    5: Program once, pulse time = n*ET_PROGPULSE
											;<3> set means write FF (EEPROM erase)
											;<2:0> Phase n

ET_SMART2		equ		ET_SMART1+1			;Phase 2 Algorithm
											;00 means no phase 2
											;80 means just program until match
											;<6:4> Phase 2 type
											;    0: program until match, then program n times
											;    1: program until match, then 1 pulse
											;      that is n*ET_PROGPULSE long
											;    2: program until match (P), then nP times
											;    3: program until match (P), then 1 pulse
											;      that is n*P*ET_PROGPULSE long
											;    4: Program n times
											;    5: Program once, pulse time = n*ET_PROGPULSE
											;<3> set means write FF (EEPROM erase)
											;<2:0> Phase n

ET_SPECIAL		equ		ET_SMART2+1			;Special requirements
											;<7:4> reserved
											;<3> 1 = EEPROM: no blank check required
											;<2> 1 = External Vpp required
											;<1> 1 = Vpp requires adjustment
											;<0> 1 = programming Vcc requires adjustment

ET_RESERVED		equ		ET_SPECIAL+1		;reserved for future use

ET_RLENGTH		equ		ET_RESERVED+1		;record length (bytes/record)



