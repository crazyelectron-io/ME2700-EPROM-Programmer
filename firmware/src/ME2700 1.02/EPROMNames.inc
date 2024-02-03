;========================================================================
; Strings used to describe the supported EPROMs
; Each entry is a null-terminated string, where the null termination is
; on an even byte. The odd (high) byte of the termination word is the
; byte count of the message string.
;
; The minimum allowed string length here is 5 ASCII characters, because
; the addresses used to reference these strings are divided by 4, and
; the print routine hunts for the end of the prior string to find the
; exact beginning of the intended string.
;========================================================================

;Force this file to be on a quad-word boundary
es_here:	equ	$
			org		(es_here+7) & 0xFFF8

estrings:
				dw	0,0,0,0					;start string so alignment works

;-------------
;Manufacturers
;-------------
es_amd:			dw	"AMD  ",0x05<<8
es_atmel:		dw	"Atmel",0x05<<8
es_catalyst:	dw	"Catalyst",0x08<<8
es_elecarrays:	dw	"El. Arrays",0x0A<<8
es_eurotech:	dw	"Eurotech.",0x09<<8
es_exel:		dw	"Exel ",0x05<<8
es_fairchild:	dw	"Fairchild",0x09<<8
es_fujitsu:		dw	"Fujitsu",0x07<<8
es_gi:			dw	"GI   ",0x05<<8
es_harris:		dw	"Harris",0x06<<8
es_hitachi:		dw	"Hitachi",0x07<<8
es_intel:		dw	"Intel",0x05<<8
es_intersil:	dw	"Intersil",0x08<<8
es_microchip:	dw	"Microchip",0x09<<8
es_mitsubishi:	dw	"Mitsubishi",0x0A<<8
es_mme:			dw	"MME  ",0x05<<8
es_motorola:	dw	"Motorola",0x08<<8
es_national:	dw	"National",0x08<<8
es_nec:			dw	"NEC  ",0x05<<8
es_nte:			dw	"NTE  ",0x05<<8
es_onsemi:		dw	"On Semi",0x07<<8
es_oki:			dw	"OKI  ",0x05<<8
es_rockwell:	dw	"Rockwell",0x08<<8
es_samsung:		dw	"Samsung",0x07<<8
es_seeq:		dw	"Seeq ",0x05<<8
es_sgs:			dw	"SGS  ",0x05<<8
es_sharp:		dw	"Sharp",0x05<<8
es_signetics:	dw	"Signetics",0x09<<8
es_soviet:		dw	"Soviet",0x06<<8
es_st:			dw	"ST   ",0x05<<8
es_tesla:		dw	"Tesla",0x05<<8
es_ti:			dw	"TI   ",0x05<<8
es_toshiba:		dw	"Toshiba",0x07<<8
es_wsi:			dw	"WSI  ",0x05<<8
es_xicor:		dw	"Xicor",0x05<<8

;-----------
;EPROM Types
;-----------
es_2704:		dw	"2704 ",0x05<<8			;Type 0
es_2804a:		dw	"2804A",0x05<<8			;Type 1
es_28c04:		dw	"28C04",0x05<<8			;Type 2
es_im6654:		dw	"IM6654",0x06<<8		;Type 3
es_2708:		dw	"2708 ",0x05<<8			;Type 4
es_2758:		dw	"2758 ",0x05<<8			;Type 5
es_2716:		dw	"2716 ",0x05<<8			;Type 6
es_2716a:		dw	"2716A",0x05<<8			;Type 7
es_2716afast:	dw	"2716A-fast",0x0A<<8	;Type 8
es_2716b:		dw	"2716B",0x05<<8			;Type 9
es_27c16h:		dw	"27C16H",0x06<<8		;Type A
es_27c16b:		dw	"27c16B",0x06<<8		;Type B
es_tms2716:		dw	"TMS2716",0x07<<8		;Type C
es_57c191:		dw	"57C191",0x06<<8		;Type D
es_57c191c:		dw	"57C191C",0x07<<8		;Type E
es_2816a:		dw	"2816A",0x05<<8			;Type F
es_28c16:		dw	"28C16",0x05<<8			;Type 10
es_2816ai:		dw	"2816Ai",0x06<<8		;Type 11
es_2732:		dw	"2732 ",0x05<<8			;Type 12
es_2732a:		dw	"2732A",0x05<<8			;Type 13
es_2732afast:	dw	"2732A-fast",0x0A<<8	;Type 14
es_2732b:		dw	"2732B",0x05<<8			;Type 15
es_27c32h:		dw	"27C32H",0x06<<8		;Type 16
es_27c32b:		dw	"27C32B",0x06<<8		;Type 17
es_tms2532:		dw	"TMS2532",0x07<<8		;Type 18
es_tms2532a:	dw	"TMS2532A",0x08<<8		;Type 19
es_tms2732a:	dw	"TMS2732A",0x08<<8		;Type 1A
es_57c43:		dw	"57C43",0x05<<8			;Type 1B
es_57c43c:		dw	"57C43C",0x06<<8		;Type 1C
es_68764:		dw	"68764",0x05<<8			;Type 1D
es_lh5749:		dw	"LH5749",0x06<<8		;Type 1E
es_27hc641:		dw	"27HC641",0x07<<8		;Type 1F
es_27hc641s:	dw	"27HC641s",0x08<<8		;Type 20
es_27hc641r:	dw	"27HC641R",0x08<<8		;Type 21
es_57c49b:		dw	"57C49B",0X06<<8		;Type 22
es_57c49c:		dw	"57C49C",0X06<<8		;Type 23
es_52b13h:		dw	"52B13H",0x06<<8		;Type 24
es_im6658:		dw	"IM6658",0x06<<8		;Type 25
es_lh57191:		dw	"LH57191",0x07<<8		;Type 26
es_28c04n:		dw	"28C04n",0x06<<8		;Type 27

;---------------------------
;Manufacturers' Part Numbers
;---------------------------
;AMD
es_am2708:		dw	"AM2708",0x06<<8
es_am2716:		dw	"AM2716",0x06<<8
es_am2716b:		dw	"AM2716B",0x07<<8
es_am2732:		dw	"AM2732",0x06<<8
es_am2732a:		dw	"AM2732A",0x07<<8
es_am2732b:		dw	"AM2732B",0x07<<8

;Atmel
es_at28c04:		dw	"AT28C04",0x07<<8
es_at28c16:		dw	"AT28C16",0x07<<8
es_at28c16e:	dw	"AT28C16E",0x08<<8
es_at27hc641:	dw	"AT27HC641",0x09<<8
es_at27hc642:	dw	"AT27HC642",0x09<<8
es_at27hc641r:	dw	"AT27HC641R",0x0A<<8
es_at27hc642r:	dw	"AT27HC642R",0x0A<<8

;Catalyst = On Semiconductor
es_cat28c16a:	dw	"CAT28C16A",0x09<<8

;Electronic Arts
es_ea2708:		dw	"EA2708",0x06<<8

;Exel
es_xl2804a:		dw	"XL2804A",0x07<<8
es_xl2816a:		dw	"XL2816A",0x07<<8
es_xl28c16a:	dw	"XL28C16A",0x08<<8

;Eurotech
es_et2716q:		dw	"ET2716Q",0x07<<8
es_et2732q:		dw	"ET2732Q",0x07<<8

;Fairchild
es_f2708:		dw	"F2708",0x05<<8
es_nmc27c16b:	dw	"NMC27C16B",0x09<<8
es_nmc27c16h:	dw	"NMC27C16H",0x09<<8
es_f2732:		dw	"F2732",0x05<<8
es_nmc27c32b:	dw	"NMC27C32B",0x09<<8
es_nmc27c32h:	dw	"NMC27C32H",0x09<<8

;Fujutsu
es_mbm2716:		dw	"MBM2716",0x07<<8
es_mbm2732:		dw	"MBM2732",0x07<<8
es_mbm2732a:	dw	"MBM2732A",0x08<<8

;Harris
es_hm6758:		dw	"HM-6758",0x07<<8

;Hitachi
es_hn462532:	dw	"HN462532",0x08<<8

es_hn462716:	dw	"HN462716",0x08<<8
es_hn462732g:	dw	"HN462732G",0x09<<8
es_hn482732ag:	dw	"HN482732AG",0x0A<<8

;Microchip
es_28c04a:		dw	"28C04A",0x06<<8
es_28c16a:		dw	"28C16A",0x06<<8

;Mitsubishi
es_m5l2732k:	dw	"M5L2732K",0x08<<8

;MME
es_u555c:		dw	"U555C",0x05<<8
es_u2732:		dw	"U2732",0x05<<8

;Motorola
es_mcm2532:		dw	"MCM2532",0x07<<8
es_mcm2732		dw	"MCM2732",0x07<<8
es_mcm68764:	dw	"MCM68764",0x08<<8
es_mcm68766:	dw	"MCM68766",0x08<<8

;National Semiconductor
es_mm2704:		dw	"MM2704",0x06<<8
es_mm2708		dw	"MM2708",0x06<<8
es_mm2716e:		dw	"MM2716E",0x07<<8
es_mm2758A:		dw	"MM2758Q-A",0x09<<8

;NEC
es_upd2732		dw	"uPD2732",0x07<<8
es_upd2732a:	dw	"uPD2732A",0x08<<8
es_upd28c04:	dw	"uPD28C04",0x08<<8
;NTE
es_nte2708:		dw	"NTE2708",0x07<<8
es_nte2716:		dw	"NTE2716",0x07<<8

;Oki
es_msm2716as:	dw	"MSM2716AS",0x09<<8
es_msm2758:		dw	"MSM2758",0x07<<8

;Rockwell
es_r87C32:		dw	"R87C32",0x06<<8

;Samsung
es_km2816a:		dw	"KM2816A",0x07<<8

;Seeq
es_52b13:		dw	"52B13",0x05<<8
es_5516a:		dw	"5516A",0x05<<8

;Soviet
es_573RF2:		dw	"573RF2",0x06<<8

;ST
es_m2532:		dw	"M2532",0x05<<8
es_m2716:		dw	"M2716",0x05<<8
es_m2716af:		dw	"M2716a-Fast",0x0B<<8
es_m2732a:		dw	"M2732A",0x06<<8
es_m2732af:		dw	"M2732a-Fast",0x0B<<8

;Tesla
es_mhb8708c:	dw	"MHB8708C",0x08<<8

;Texas Instruments
es_tms2508:		dw	"TMS2508",0x07<<8
es_tms2516:		dw	"TMS2516",0x07<<8
es_tms2708:		dw	"TMS2708",0x07<<8
es_tms2758jl0:	dw	"TMS2758-JL0",0x0B<<8

;Toshiba
es_tmm2732d:	dw	"TMM2732D",0x08<<8

;Waferscale
es_ws57c191:	dw	"WS57C191",0x08<<8
es_ws57c191b:	dw	"WS57C191B",0x09<<8
es_ws57c191c:	dw	"WS57C191C",0x09<<8
es_ws57c291:	dw	"WS57C291",0x08<<8
es_ws57c291b:	dw	"WS57C291B",0x09<<8
es_ws57c291c:	dw	"WS57C291C",0x09<<8
es_ws57c43b:	dw	"WS57C43B",0x08<<8
es_ws57c43c:	dw	"WS57C43C",0x08<<8
es_ws57c49b:	dw	"WS57C49B",0x08<<8
es_ws57c49c:	dw	"WS57C49C",0x08<<8

;Xicor
es_x2804a:		dw	"X2804A",0x06<<8
es_x2816b:		dw	"X2816B",0x06<<8


;----------------
;Special voltages
;----------------
es_55v:			dw	"5.50V",0x05<<8
es_65v:			dw	"6.50V",0x05<<8

es_n30v:		dw	"external -31V supply & Intersil jumper",.38<<8

estrings_almostend:									;the last string may slop over

es_n40v:		dw	"external -41V supply & Intersil jumper",.38<<8

;*==============================================================================*
; Print an error message in the listing if this is section too big or
; if the last string is not properly double-word aligned
;*==============================================================================*
	if	estrings_almostend-estrings > .2047
	error	"estrings section is long!"
	endif
