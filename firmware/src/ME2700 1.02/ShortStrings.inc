;################################################################################
;# Null-terminated strings														#
;# Strings are located at the beginnig of code, because the LFSR instruction 	#
;# demands that they all begin within the first 4K-bytes of program memory.		#
;# This is done so that printf can be called using FSR0, rather than TBLPTR.	#
;# Unlike TBLPTR, the FSRs can be loaded with a single instruction: LFSR. But	#
;# the LFSR instruction takes a 12-bit literal, creating this 4K-byte limit.	#
;################################################################################
sstrings:
cmdlst_msg:		dw	"Type ? for command list",0

prompt_msg:		dw	">",0

bad_input_msg:	dw	"Huh?",0
eeprompt_msg:	dw	"EE",0

recerror_msg:	dw	" ? Rec",0
hexerror_msg:	dw	" ? Hex",0
cksumerror_msg:	dw	" ? Csm",0
rcnterror_msg:	dw	" ? Cnt",0

boffset_msg		dw	"Buffer Address Offset: ",0

errorcount_msg:	dw	" (including EOF record), Bad "		;falls into reccount_msg
reccount_msg:	dw	"Records: ",0
loadcount_msg:	dw	" records loaded in buffer with "	;Fall into foffset_msg
foffset_msg:	dw	"File Address Offset: ",0
int_start_rec:	dw	":",0								;Intel record start
int_end_rec:	dw	":00000001FF\r",0					;Intel EOF record
mot_start_rec:	dw	"S1",0								;Motorola record start
mot_end_rec:	dw	"S9030000FC\r",0					;Motorola EOF record

loaderrev_msg:	dw	"ME Loader Kernel version ",0
fwrev_msg:		dw	"ME2700 firmware version "
		dw	('.' << 8) + (REV_MAJOR + '0')
		dw	(((REV_MINOR & 0x0F) + '0') << 8) + (((REV_MINOR & 0xf0) >> 4) + '0' ),0

buffer_msg:		dw	"Buffer ",0
csum_msg:		dw	"checksum: ",0
range_msg:		dw	"range ",0
echostate_msg:	dw	"Echo:",0
dinvert_msg:	dw	"EPROM data invert:",0
buffill_msg:	dw	"Buffer filled with ",0
auto_msg:		dw	"automatic",0

eprom_msg:		dw	"EPROM ",0
read_msg:		dw	"read into buffer. ",0
cmpgood_msg:	dw	"matches buffer",0
isblank_msg:	dw	"is blank",0
type_msg:		dw	"Type ",0

writefail_msg:	dw	"Write "			;fall into fail_msg
fail_msg:		dw	"Fail",0
success_msg:	dw	"Success",0

abort_msg:		dw	" Abort",0
pto_msg:		de	"\rAbort: EEPROM polling timeout",0
hvpanic_msg:	dw	"\rAbort: Vpp is too high!",0
lvpanic_msg:	dw	"\rAbort: Vpp is too low!",0

smartrapid_msg:	dw	"Fast algorithm:\r  Pass 1",0
pass2_msg:		dw	"  Pass 2",0
write_msg:		dw	": write ",0
ff_msg:			dw	"FFh to ",0
eachbyte_msg:	dw	"each byte ",0
times_msg:		dw	" time(s)",0
matchp_msg:		dw	"until it matches (P)",0
andthen_msg:	dw	",\r          and then ",0
another_msg:	dw	"another ",0
withone_msg:	dw	"with one ",0
mult_msg:		dw	" * ",0
pulse_msg:		dw	" pulse",0

programing_msg:	dw	"Programming  ",0
maxp_msg:		dw	"  Maximum P=",0

verifying_msg:	dw	"Verifying",0
elist_msg:		dw	"Supported Device Types:\r",0

mismatcha_msg:	dw	"Error at address ",0
mismatchb_msg:	dw	"   Buffer: ",0
mismatche_msg:	dw	"   EPROM"			;falls into colonspace_msg
colonspace_msg:	dw	": ",0

dataread_msg:	dw	"Data Read: ",0
etype_msg:		dw	"Current Device Type is ",0
undef_msg:		dw	"Error: Undefined Device Type",0
unassgn_msg:	dw	"Unassigned",0
testvpp_msg:	dw	" (Measure at TP3)",0
setvpp_msg:		dw	"Vpp set for ",0

vp18_msg:		dw	"Note: Vpp will be about 0.7V higher on pin 18",0

pwroff_msg:		dw	" powered off",0
rmode_msg:		dw	" read mode ",0
pmode_msg:		dw	" programming mode ",0

pinactive_msg:	dw	"in"					;fall into pactive_msg
pactive_msg:	dw	"active state",0

tvpp_msg:		dw	"Vpp"					;fall into pin_msg
pin_msg:		dw	" pin ",0

tvcc_msg:		dw	"Vcc",0
on_msg:			dw	" on",0
off_msg:		dw	" off",0
apl_msg:		dw	" at programming level",0
nonexist_msg:	dw	"does not exist",0
novbbvdd_msg:	dw	"Vbb and Vdd do not exist",0
vbb_msg:		dw	"-5V Vbb pin 21 and +12V Vdd",0

pulsing_msg:	dw	"Pulsing  ",0
reading_msg:	dw	"Reading  ",0

swoff_msg:		dw	"Switcher is off",0
adj_msg:		dw	"==> Manually adjust ",0
adjvcc_msg:		dw	"6.2V supply to ",0
vppto_msg:		dw	" Vpp supply to ",0
req_msg:		dw	"\r==> Requires ",0

readjvcc_msg:	dw	"==> Readjust 6.2V supply to 6.20V",0
readjvp_msg:	dw	"==> Readjust Vpp supply to nominal",0
removej_msg:	dw	"==> Remove Intersil jumper & "			;fall into extneg_msg
extneg_msg:		dw	"external supply",0


notblank_msg:	dw	"The EPROM programming range is not blank"	;fall into anyway_msg
anyway_msg:		dw	". Proceed anyway",0

ppw_msg:		dw	"Programming pulse width: ",0
ppd_msg:		dw	", delay between pulses: ",0
ns500_msg:		dw	"~350 nS",0
ms_msg:			dw	" mS",0
us_msg:			dw	"0 uS",0
ppr_msg:		dw	"Programming cycles: ",0
p12v_msg:		dw	" +12V during programming",0
rvppat_msg:		dw	"Vpp during read: ",0
pvpp_msg:		dw	"Pulsed from ",0
to_msg:			dw	" to ",0
esize_msg:		dw	", size: ",0
by_msg:			dw	" x ",0
eeprom_msg:		dw	"No blank check before programming",0
propulse_msg:	dw	"Programming pulse on ",0
poll_msg:		dw	"Poll data for write completion",0

sfail_msg:		dw	"Fast programming failed",0

ep_msg:			dw	" devices programmed ",0
epf_msg:		dw	"un"		;fall into eps_msg
eps_msg:		dw	"successfully",0

cantedit_msg:	dw	"Only Device Types "
 if (FEPROM_COUNT & 0x0F) < 0x0A
				dw	((FEPROM_COUNT>>4)+'0') + (((FEPROM_COUNT & 0x0F)+'0')<<8)
 else
				dw	((FEPROM_COUNT>>4)+'0') + (((FEPROM_COUNT & 0x0F)-0x0A + 'A')<<8)
 endif
				dw	" through "
 if ((FEPROM_COUNT+3) & 0x0F) < 0x0A
				dw	(((FEPROM_COUNT+3)>>4)+'0') + ((((FEPROM_COUNT+3) & 0x0F)+'0')<<8)
 else
				dw	(((FEPROM_COUNT+3)>>4)+'0') + ((((FEPROM_COUNT+3) & 0x0F)-0x0A + 'A')<<8)
 endif
				dw	" can be edited.",0

novpp_msg:		dw	"Error: Vpp may only be on pins 18, 20, or 21",0
oeverr_msg:		dw	"Error: -OE may be set to +12V on pins 19 and 20 only",0
oevperr_msg:	dw	"Error: -OE is currently also Vpp",0

v0_msg:			dw	"0V",0
v5_msg:			dw	"5V",0
v6_msg:			dw	"6.2V",0
v12_msg:		dw	"12V",0

v1270_msg:		dw	"12.7V",0		;These are for Vpp on pins 19-21
v1315_msg:		dw	"13.15V",0
v21_msg:		dw	"21V",0
v2510_msg:		dw	"25.2V",0

v1340_msg:		dw	"13.4V",0		;these are for Vpp on pin 18 only
v1385_msg:		dw	"13.85V",0
v2170_msg:		dw	"21.7V",0
v2590_msg:		dw	"25.9V",0

A9_msg:			dw	"A9",0
A10_msg:		dw	"A10",0
A11_msg:		dw	"A11",0
A12_msg:		dw	"A12",0
stuckhi_msg:	dw	"Stuck-High",0
as_msg:			dw	"-AS",0
oe_msg:			dw	"-OE",0
csn_msg:		dw	"-"				;fall into cs_msg
cs_msg:			dw	"CS",0
pgmn_msg:		dw	"/-PGM",0
pgm_msg:		dw	"/PGM",0
vpp_msg:		dw	"/Vpp ",0
vbb5v_msg:		dw	"Vbb -5V",0
vdd12v_msg:		dw	"Vdd +12V",0
unassign_msg:	dw	"/Stuck-Low",0

space7_msg:		dw	"     "			;fall into space2_msg
space2_msg:		dw	"  ",0

;                    123456789012345678901234567890123456789012345678901234567890
ep1_msg:		dw	"       -----v-----\r"
				dw	" A7  -| 1      24 |-  Vcc     Programming Vcc = ",0
ep2_msg:		dw	" A6  -| 2      23 |-  A8\r"
				dw	" A5  -| 3      22 |-  ",0
ep3_msg:		dw	" A4  -| 4      21 |-  ",0
ep4_msg:		dw	" A3  -| 5      20 |-  ",0
ep5_msg:		dw	" A2  -| 6      19 |-  ",0
ep6_msg:		dw	" A1  -| 7      18 |-  ",0
ep7_msg:		dw	" A0  -| 8      17 |-  D7",0
ep8_msg:		dw	" D0  -| 9      16 |-  D6",0
ep9_msg:		dw	" D1  -| 10     15 |-  D5",0
ep10_msg:		dw	" D2  -| 11     14 |-  D4",0
ep11_msg:		dw	"GND  -| 12     13 |-  D3",0
ep12_msg:		dw	"       -----------      ",0

supdev_msg:		dw	"      Supported Devices:",0

delce_msg:		dw	"Delete Device Type",0

gname_msg:		dw	"EPROM Device Type is unassigned until it is named. (EN command)",0

wrongkern_msg:	dw	"Wrong ME Loader Kernel revision for this firmware!",0
wrongprog_msg:	dw	"Wrong firmware for this programmer!\r",0

ready2prog_msg:	dw	"Make sure the correct Device Type is selected, and that the EPROM\r"
				dw	"is inserted correctly, with pin 1 closest to the socket handle.\r"
				dw	"Ready to program",0

 if DEBUG
shared_msg:		dw	"SHARED: ",0
 endif

;Longest message last
ss_almostend:

signon_msg:	dw	"\r*====================================*\r"
			dw	"*               ME2700               *\r"
			dw	"*====================================*\r"
			dw	"*       Orphan EPROM Programmer      *\r"
			dw	"*         By Martin Eberhard         *\r"
			dw	"*       Firmware Version  "
			dw	(REV_MAJOR + '0') + ('.' << 8)
			dw	(((REV_MINOR & 0xf0) >> 4) + '0' ) + (((REV_MINOR & 0x0F) + '0') << 8)
			dw	"       *\r"
			dw	"*====================================*\r\r",0

;*==============================================================================*
; Print an error message in the listing if this is section too big
;*==============================================================================*
	if	ss_almostend-sstrings > .4095
	error	"Martin sez: ShortStrings section is long!"
	endif

