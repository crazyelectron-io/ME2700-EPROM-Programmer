;********************************************************************
; Main Command Table												*
; Each entry is a null terminated ASCII string followed by a 1-byte	*
; command index. Index 0 is reserved for non-existant commands.		*
;********************************************************************
main_cmds:	dw	"EB",0x0200		;Blank-check EPROM
			dw	"ED",0x0400		;Display EPROM specs
			dw	"EE",0x0600		;edit custom EPROM
			dw	"DI",0x0800		;Set/clear data invert mode
			dw	"EL",0x0A00		;List supported EPROMs
			dw	"EP",0x0C00		;Program EPROM
			dw	"ER",0x0E00		;Read EPROM into buffer
			dw	"EC",0x1000		;Compare to buffer
			dw	"ET",0x1200		;Select EPROM type
			dw	"BAO",0x1400	;set buffer address offset
			dw	"FAO",0x1600	;set file address offset
			dw	"UI",0x1800		;upload buffer as Intel hex
			dw	"US",0x1A00		;upload buffer as Motorola S-records
			dw	"BD",0x1C00		;display buffer
			dw	"BE",0x1E00		;Edit buffer
			dw	"BF",0x2000		;Fill buffer
			dw	"DS",0x2200		;Display settings
			dw	"ECHO",0x2400		;Terminal echo on/off
			dw	"RESET",0x2600	;Reset
			dw	"AVPP",0x2800	;adjust Vpp
			dw	"TAS",0x2A00	;test -AS
			dw	"TVBD",0x2C00	;test Vbb & Vdd
			dw	"TVCC",0x2E00	;test Vcc
			dw	"TOE",0x3000	;test -OE pin
			dw	"TPGM",0x3200	;test PGM pin
			dw	"TPROG",0x3400	;test programming
			dw	"TREAD",0x3600	;test EPROM reading
			dw	"TCS",0x3800	;test CS pin
			dw	"TVPP",0x3A00	;Test Vpp pin
			dw	"THI",0x3C00	;Test stuck-high pin
			dw	"RD",0x3E00		;test data inputs
			dw	"WA",0x4000		;test address outputs
			dw	"WD",0x4200		;test data outputs
			dw	"?",0x4400		;help
			dw	"?B",0x4600		;help with buffer commands
			dw	"?N",0x4800		;General ME2700 notes
			dw	"?D",0x4A00		;help with diagnost commands
			dw	"?E",0x4C00		;help help with EPROM commands
			dw	"?F",0x4E00		;help help with file transfer commands
			dw	"?L",0x5000		;help with loader
			dw	"ES",0x5200		;Checksum EPROM

	if DEBUG
			dw	"PW",0x5400		;Pulse width adjust
	endif

			dw	0				;table end

;********************************************************************
; EPROM Editor Command Table										*
; Each entry is a null terminated ASCII string followed by a 1-byte	*
; command index. Index 0 is reserved for non-existant commands.		*
;********************************************************************
ee_cmds:	dw	"TPROG",0x0200	;test programming (requires goto)
			dw	"TREAD",0x0600	;test EPROM reading (requires goto)
			dw	"DELETE",0x0A00	;Delete EPROM
			dw	"ED",0x0C00		;Display EPROM
			dw	"EN",0x0E00		;EPROM name
			dw	"Q",0x1000		;Quit to main
			dw	"?",0x1200		;Help
			dw	"VBB",0x1400	;Vbb definition
			dw	"VDD",0x1600	;Vdd definition
			dw	"A9",0x1800		;A9 pin
			dw	"A10",0x1A00	;A10 pin
			dw	"A11",0x1C00	;A11 pin
			dw	"A12",0x1E00	;A12 pin
			dw	"OEN",0x2000	;-OE pin
			dw	"CS",0x2200		;CE pin
			dw	"CSP",0x2400	;CE polarity
			dw	"PGM",0x2600	;PGM pin
			dw	"PGP",0x2800	;PGM polarity
			dw	"PPP",0x2A00	;Vpp pin
			dw	"VCP",0x2C00	;VCC voltage during programming
			dw	"OEV",0x2E00	;Special OE voltage during programming
			dw	"VPP",0x3000	;Vpp voltage
			dw	"VPR",0x3200	;Vpp voltage during reading
			dw	"PUL",0x3400	;Programming pulse on pin
			dw	"PTU",0x3600	;Pulse time units
			dw	"PPW",0x3800	;Programming pulse width
			dw	"PPS",0x3A00	;Programming pulse separation
			dw	"POL",0x3C00	;EEPROM-style compltion polling
			dw	"SPA",0x3E00	;Simple Programming Algorithm
			dw	"FP1",0x4000	;Fast Pass 1 type
			dw	"FN1",0x4200	;N value for pass 1
			dw	"FP2",0x4400	;Fast Pass 2 type
			dw	"FN2",0x4600	;N value for pass 2
			dw	"PMX",0x4800	;Max P value or total count
			dw	"FF1",0x4A00	;first pass is EEPROM erase to FF
			dw	"ASN",0x4C00	;Define address strobe pin
			dw	"HI",0x4E00		;Define stuck-high pin
			dw	"BCK",0x5000	;Enable/disable blank check
			dw	"COPY",0x5200	;Copy EPROM specs
			dw	"?A",0x5400		;Pin Assignment Help
			dw	"?P",0x5600		;Programming parameter help
			dw	0				;end of table
