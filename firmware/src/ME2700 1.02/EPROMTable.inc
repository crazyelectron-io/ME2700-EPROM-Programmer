;********************************************************************************
; EPROM Configuration Table. (See EPROMStructure file for explaination.)
; Note: you can't use the db directive here.
;********************************************************************************
eprom_table:

;+----+
;+ 00 +
;+---------------------------+
;+ Generic 2704 (3 supplies) +
;+---------------------------+
ep_2704:
	dw		(0xFF<<8)+(es_2704-estrings)/8							;(ET_NAME)

	dw		(((es_2704-estrings)/8)<<8)+(es_intel-estrings)/8		;(ET_PARTNO1)
	dw		(((es_mm2704-estrings)/8)<<8)+(es_national-estrings)/8	;(ET_PARTNO2)
	dw		(((es_2704-estrings)/8)<<8)+(es_signetics-estrings)/8	;(ET_PARTNO3)
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x0081				;(ET_BYTES)512 bytes, 8 bits/byte
								;(ET_A9A10)no A9 pin, no A10 pin

	dw		0xB000				;(ET_A11A12)no A11 pin, no A12 pin, no A11 pin
								;(ET_OECS)+12V -OE on pin 20, no CS pin

	dw		0x3101				;(ET_PGMVPP)no PGM pin, Vpp on pin 18
								;(ET_VBBVDD)Vbb=-5V & Vdd=12V, Vcc=5V

	dw		(.112<<8)+0x40		;(ET_VPPSETUP)Vpp = 26V, 0V during read, Pulsed Vpp, return to 0V
								;(ET_PROGREPS) 112 repetitions

	dw		(0x00<<8)+.90		;(ET_PROGPULSE) 900 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 01 +
;+--------------------------+
;+ 512x8 EEPROM, no polling +
;+--------------------------+
ep_2804a:
	dw		(0xFF<<8)+(es_2804a-estrings)/8							;(ET_NAME)

	dw		(((es_xl2804a-estrings)/8)<<8)+(es_exel-estrings)/8		;(ET_PARTNO1)
	dw		(((es_2804a-estrings)/8)<<8)+(es_seeq-estrings)/8		;(ET_PARTNO2)
	dw		(((es_x2804a-estrings)/8)<<8)+(es_xicor-estrings)/8		;(ET_PARTNO3)
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x0081				;(ET_BYTES)512 bytes, 8 bits/byte
								;(ET_A9A10)No A9, no A10

	dw		0x3100				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, active-low CS on pin 18

	dw		0x0140				;(ET_PGMVPP)active-low PGM on pin 21, no Vpp pin
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x06		;(ET_VPPSETUP)Vpp switcher off, 5V during read, pulsed PGM
								;(ET_PROGREPS) write once

	dw		((.12+80h)<<8)+0x00	;(ET_PROGPULSE) 250 nS programming pulse width
								;(ET_PPDELAY) 12 mS between writes

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0008				;(ET_SPECIAL) no blank check
								;(ET_RESERVED)

;+----+
;+ 02 +
;+--------------+
;+ 512x8 EEPROM +
;+--------------+
ep_28C04:
	dw		(0xFF<<8)+(es_28c04-estrings)/8							;(ET_NAME)

	dw		(((es_at28c04-estrings)/8)<<8)+(es_atmel-estrings)/8	;(ET_PARTNO1)
	dw		(((es_28c04-estrings)/8)<<8)+(es_gi-estrings)/8			;(ET_PARTNO2)
	dw		(((es_28c04a-estrings)/8)<<8)+(es_microchip-estrings)/8	;(ET_PARTNO3)
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x0081				;(ET_BYTES)512 bytes, 8 bits/byte
								;(ET_A9A10)No A9, no A10

	dw		0x3100				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, active-low CS on pin 18

	dw		0x0140				;(ET_PGMVPP)active-low PGM on pin 21, no Vpp pin
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x06		;(ET_VPPSETUP)Vpp = switcher off, 5V during read, pulsed PGM
								;(ET_PROGREPS) write once

	dw		0x0100				;(ET_PROGPULSE) 250 nS programming pulse width
								;(ET_PPDELAY) poll for completion between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0008				;(ET_SPECIAL) no blank check
								;(ET_RESERVED)

;+----+
;+ 03 +
;+--------------------------------+
;+ Intersil IM6654                +
;+ Requires external negative Vpp +
;+--------------------------------+
ep_IM6654:
	dw		(0xFF<<8)+(es_im6654-estrings)/8						;(ET_NAME)

	dw		(((es_im6654-estrings)/8)<<8)+(es_intersil-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF				;(ET_PARTNO2) null
	dw		0xFFFF				;(ET_PARTNO3) null
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null

	dw		(((es_n40v-estrings)/8)<<8)+0xFF	;(ET_VPPMSG)
												;(ET_VCCMSG) null

	dw		0x0081					;(ET_BYTES)512 bytes, 8 bits/byte
									;(ET_A9A10)no A9, no A10

	dw		0x40AB					;(ET_A11A12)Pin 19 stuck-high (+5V Vdd), -Address Strobe (-AS) on pin 20
									;(ET_OECS)-OE on pin 21, no CS pin

	dw		0x0101					;(ET_PGMVPP)No PGM pin, Vpp on pin 18
									;(ET_VBBVDD)no Vbb or Vdd (Vdd pin 19 is a stuck-high pin)

	dw		(.2<<8)+0x55			;(ET_VPPSETUP)External Vpp, 5V during read, pulsed Vpp, return to +5V
									;(ET_PROGREPS) max 2 passes for P

	dw		((.7+0x80)<<8)+.20+0x80	;(ET_PROGPULSE) 20 mS programming pulse width
									;(ET_PPDELAY) 7 mS delay between pulses

	dw		0x0024					;(ET_SMART1) P+4P smart programming
									;(ET_SMART1) no second pass

	dw		0x0004					;(ET_SPECIAL)requires external negative Vpp
									;(ET_RESERVED)

;+----+
;+ 04 +
;+---------------------------+
;+ Generic 2708 (3 supplies) +
;+---------------------------+
ep_2708:
	dw		(0xFF<<8)+(es_2708-estrings)/8								;(ET_NAME)

	dw		(((es_am2708-estrings)/8)<<8)+(es_amd-estrings)/8			;(ET_PARTNO1)
	dw		(((es_ea2708-estrings)/8)<<8)+(es_elecarrays-estrings)/8	;(ET_PARTNO2)
	dw		(((es_f2708-estrings)/8)<<8)+(es_fairchild-estrings)/8		;(ET_PARTNO3)
	dw		(((es_2708-estrings)/8)<<8)+(es_intel-estrings)/8			;(ET_PARTNO4)
	dw		(((es_u555c-estrings)/8)<<8)+(es_mme-estrings)/8			;(ET_PARTNO5)
	dw		(((es_mm2708-estrings)/8)<<8)+(es_national-estrings)/8		;(ET_PARTNO6)
	dw		(((es_nte2708-estrings)/8)<<8)+(es_nte-estrings)/8			;(ET_PARTNO7)
	dw		(((es_2708-estrings)/8)<<8)+(es_signetics-estrings)/8		;(ET_PARTNO8)
	dw		(((es_mhb8708c-estrings)/8)<<8)+(es_tesla-estrings)/8		;(ET_PARTNO9)
	dw		(((es_tms2708-estrings)/8)<<8)+(es_ti-estrings)/8			;(ET_PARTNO10)

	dw		0x5082				;(ET_BYTES)1024 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, no A10 pin

	dw		0xB000				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)+12V -OE on pin 20, no CS pin

	dw		0x3101				;(ET_PGMVPP)no PGM pin, Vpp on pin 18
								;(ET_VBBVDD)Vbb=-5V & Vdd=12V, Vcc = 5V

	dw		(.112<<8)+0x40		;(ET_VPPSETUP)Vpp = 26V, 0V during read, Pulsed Vpp, return to 0V
								;(ET_PROGREPS) 112 repetitions

	dw		(0x00<<8)+.90		;(ET_PROGPULSE) 900 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 05 +
;+---------------------------+
;+ Single-voltage 1Kx8 EPROM +
;+ (Made from half of a 2716)+
;+---------------------------+
ep_2758:
	dw		(0xFF<<8)+(es_2758-estrings)/8							;(ET_NAME)

	dw		(((es_hm6758-estrings)/8)<<8)+(es_harris-estrings)/8	;(ET_PARTNO1)
	dw		(((es_2758-estrings)/8)<<8)+(es_intel-estrings)/8		;(ET_PARTNO2)
	dw		(((es_mm2758A-estrings)/8)<<8)+(es_national-estrings)/8	;(ET_PARTNO3)
	dw		(((es_msm2758-estrings)/8)<<8)+(es_oki-estrings)/8		;(ET_PARTNO4)
	dw		(((es_tms2508-estrings)/8)<<8)+(es_ti-estrings)/8		;(ET_PARTNO5)
	dw		(((es_tms2758jl0-estrings)/8)<<8)+(es_ti-estrings)/8	;(ET_PARTNO6)
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x5082				;(ET_BYTES)1024 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, no A10 pin

	dw		0x3000				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0194				;(ET_PGMVPP)active-high PGM on pin 18, Vpp on pin 21
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x46		;(ET_VPPSETUP)Vpp = 26V, 5V during read, Pulsed PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.50+0x80	;(ET_PROGPULSE) 50 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 06 +
;+------------------------+
;+ Generic 2716 (Vpp=25V) +
;+------------------------+
ep_2716:
	dw		(0xFF<<8)+(es_2716-estrings)/8							;(ET_NAME)

;	dw		(((es_et2716q-estrings)/8)<<8)+(es_eurotech-estrings)/8	;(also)
;	dw		(((es_nte2716-estrings)/8)<<8)+(es_nte-estrings)/8		;(also)

	dw		(((es_am2716-estrings)/8)<<8)+(es_amd-estrings)/8		;(ET_PARTNO1)
	dw		(((es_mbm2716-estrings)/8)<<8)+(es_fujitsu-estrings)/8	;(ET_PARTNO2)
	dw		(((es_hn462716-estrings)/8)<<8)+(es_hitachi-estrings)/8	;(ET_PARTNO3)
	dw		(((es_2716-estrings)/8)<<8)+(es_intel-estrings)/8		;(ET_PARTNO4)
	dw		(((es_mm2716e-estrings)/8)<<8)+(es_national-estrings)/8	;(ET_PARTNO5)
	dw		(((es_msm2716as-estrings)/8)<<8)+(es_oki-estrings)/8	;(ET_PARTNO6)
	dw		(((es_m2716-estrings)/8)<<8)+(es_st-estrings)/8			;(ET_PARTNO7)
	dw		(((es_2716-estrings)/8)<<8)+(es_signetics-estrings)/8	;(ET_PARTNO8)
	dw		(((es_573RF2-estrings)/8)<<8)+(es_soviet-estrings)/8	;(ET_PARTNO9)	
	dw		(((es_tms2516-estrings)/8)<<8)+(es_ti-estrings)/8		;(ET_PARTNO10)

	dw		0x5283				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x3000				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0194				;(ET_PGMVPP)active-high PGM on pin 18, Vpp on pin 21
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x46		;(ET_VPPSETUP)Vpp = 25V, 5V during read, pulsed PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.50+0x80	;(ET_PROGPULSE) 50 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 07 +
;+-------------------+
;+ 2716 with Vpp=21V +
;+-------------------+
ep_2716A:
	dw		(0xFF<<8)+(es_2716a-estrings)/8			;(ET_NAME)

	dw		0xFFFF									;(ET_PARTNO1) null
	dw		0xFFFF									;(ET_PARTNO2) null
	dw		0xFFFF									;(ET_PARTNO3) null
	dw		0xFFFF									;(ET_PARTNO4) null
	dw		0xFFFF									;(ET_PARTNO5) null
	dw		0xFFFF									;(ET_PARTNO6) null
	dw		0xFFFF									;(ET_PARTNO7) null
	dw		0xFFFF									;(ET_PARTNO8) null
	dw		0xFFFF									;(ET_PARTNO9) null
	dw		0xFFFF									;(ET_PARTNO10) null

	dw		0x5283				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x3000				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0194				;(ET_PGMVPP)active-high PGM on pin 18, Vpp on pin 21
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x36		;(ET_VPPSETUP)Vpp = 21V, 5V during read, pulsed PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.50+0x80	;(ET_PROGPULSE) 50 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 08 +
;+-------------------------+
;+ 2716 with Vpp=21V, Fast +
;+-------------------------+
ep_2716Afast:
	dw		(0xFF<<8)+(es_2716afast-estrings)/8			;(ET_NAME)

	dw		(((es_m2716af-estrings)/8)<<8)+(es_st-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF												;(ET_PARTNO2) null
	dw		0xFFFF												;(ET_PARTNO3) null
	dw		0xFFFF												;(ET_PARTNO4) null
	dw		0xFFFF												;(ET_PARTNO5) null
	dw		0xFFFF												;(ET_PARTNO6) null
	dw		0xFFFF												;(ET_PARTNO7) null
	dw		0xFFFF												;(ET_PARTNO8) null
	dw		0xFFFF												;(ET_PARTNO9) null
	dw		0xFFFF												;(ET_PARTN10) null

	dw		0x5283				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x3000				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0294				;(ET_PGMVPP)active-high PGM on pin 18, Vpp on pin 21
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 6.0V

	dw		(.25<<8)+0x36		;(ET_VPPSETUP)Vpp = 21V, 5V during read, pulsed PGM
								;(ET_PROGREPS) max 25 tries

	dw		(0x00<<8)+.100		;(ET_PROGPULSE) 1 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0033				;(ET_SMART1) P+3P*ET_PROGPULSE smart programming
								;(ET_SMART2) no second pass

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 09 +
;+----------------------+
;+ 2716 with Vpp=12.75V +
;+----------------------+
ep_2716B:
	dw		(0xFF<<8)+(es_2716b-estrings)/8						;(ET_NAME)

	dw		(((es_am2716b-estrings)/8)<<8)+(es_amd-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF				;(ET_PARTNO2) null
	dw		0xFFFF				;(ET_PARTNO3) null
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x5283				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x3000				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0194				;(ET_PGMVPP)active-high PGM on pin 18, Vpp on pin 21
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.15<<8)+0x16		;(ET_VPPSETUP)Vpp = 12.75V, 5V during read, pulsed PGM
								;(ET_PROGREPS) max 15 reps

	dw		(0x00<<8)+.100		;(ET_PROGPULSE) 1 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0012				;(ET_SMART1) P+2*ET_PROGPULSE smart
								;(ET_SMART2) No second pass

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 0A +
;+-------------------+
;+ 2716H			 +
;+-------------------+
ep_27c16h:
	dw		(0xFF<<8)+(es_27c16h-estrings)/8							;(ET_NAME)

	dw		(((es_nmc27c16h-estrings)/8)<<8)+(es_fairchild-estrings)/8	;(ET_PARTNO1)
	dw		(((es_nmc27c16h-estrings)/8)<<8)+(es_national-estrings)/8	;(ET_PARTNO2)
	dw		0xFFFF				;(ET_PARTNO3) null
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x5283				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x3000				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0194				;(ET_PGMVPP)active-high PGM on pin 18, Vpp on pin 21
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x46		;(ET_VPPSETUP)Vpp = 25V, 5V during read, pulsed PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.10+0x80	;(ET_PROGPULSE) 10 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 0B +
;+-----------------------+
;+ 27C16 with Vpp=12.75V +
;+-----------------------+
ep_27C16B:
	dw	(	0xFF<<8)+(es_27c16b-estrings)/8								;(ET_NAME)

	dw		(((es_nmc27c16b-estrings)/8)<<8)+(es_fairchild-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF				;(ET_PARTNO2) null
	dw		0xFFFF				;(ET_PARTNO3) null
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x5283				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x3000				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0294				;(ET_PGMVPP)active-high PGM on pin 18, Vpp on pin 21
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 6.2V

	dw		(.20<<8)+0x16		;(ET_VPPSETUP)Vpp = 12.75V, 5V during read, pulsed PGM
								;(ET_PROGREPS) max 20 reps

	dw		(0x00<<8)+.10		;(ET_PROGPULSE) 100 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0080				;(ET_SMART1) P, n=0 (msb set so it is a P algorithm)
								;(ET_SMART2) No second pass

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 0C +
;+----------------------------------------+
;+ Texas Instruments 3-voltage 2Kx8 EPROM +
;+----------------------------------------+
ep_TMS2716:
	dw		(0xFF<<8)+(es_tms2716-estrings)/8					;(ET_NAME)

	dw		(((es_tms2716-estrings)/8)<<8)+(es_motorola-estrings)/8	;(ET_PARTNO1)
	dw		(((es_tms2716-estrings)/8)<<8)+(es_ti-estrings)/8		;(ET_PARTNO2)
	dw		0xFFFF				;(ET_PARTNO3) null
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x5383				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 20

	dw		0x0000				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)no -OE pin, No CS pin

	dw		0x3301				;(ET_PGMVPP)No PGM pin, Vpp on pin 18
								;(ET_VBBVDD)Vbb = -5V & Vdd = 12V, Vcc = 12V

	dw		(.112<<8)+0x40		;(ET_VPPSETUP)Vpp = 25V, 0V during read, pulsed Vpp, return to 0V
								;(ET_PROGREPS) 112 repetitions

	dw		(0x00<<8)+.90		;(ET_PROGPULSE) 90 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 0D +
;+--------------------------------------------------+
;+ WSI 2Kx8 EPROM, with Vcc=5.5V during programming +
;+--------------------------------------------------+
ep_57C191:
	dw		(0xFF<<8)+(es_57c191-estrings)/8						;(ET_NAME)

	dw		(((es_ws57c191-estrings)/8)<<8)+(es_wsi-estrings)/8		;(ET_PARTNO1)
	dw		(((es_ws57c191b-estrings)/8)<<8)+(es_wsi-estrings)/8	;(ET_PARTNO2)
	dw		(((es_ws57c291-estrings)/8)<<8)+(es_wsi-estrings)/8		;(ET_PARTNO3)
	dw		(((es_ws57c291b-estrings)/8)<<8)+(es_wsi-estrings)/8	;(ET_PARTNO4)
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null

	dw		(0xFF<<8)+(es_55v-estrings)/8		;(ET_VCCMSG)prog. Vcc needs adjustment
												;(ET_VPPMSG) null

	dw		0x5483				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 21

	dw		0x3A90				;(ET_A11A12)Force pin 18 high (CS3), no A12
								;(ET_OECS)-OE on pin 20, active-high CS on pin 19 (CS2)

	dw		0x0203				;(ET_PGMVPP)No PGM pin, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, use 6.2V supply for programming Vcc

	dw		(.01<<8)+0x21		;(ET_VPPSETUP)Vpp = 13.15V, 0V during read, pulsed Vpp, return to +5V
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.10+0x80	;(ET_PROGPULSE) 10 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0001				;(ET_SPECIAL) Programming Vcc requires adjustment
								;(ET_RESERVED)

;+----+
;+ 0E +
;+---------------------------------------------------+
;+ WSI 2Kx8 EPROM, with Vcc=6.2V during programming +
;+---------------------------------------------------+
ep_57C191C:
	dw		(0xFF<<8)+(es_57c191c-estrings)/8						;(ET_NAME)

	dw		(((es_ws57c191c-estrings)/8)<<8)+(es_wsi-estrings)/8	;(ET_PARTNO1)
	dw		(((es_ws57c291c-estrings)/8)<<8)+(es_wsi-estrings)/8	;(ET_PARTNO2)
	dw		0xFFFF				;(ET_PARTNO3) null
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTN10) null

	dw		0x5483				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 21

	dw		0x3A90				;(ET_A11A12)Force pin 18 high (CS3), no A12
								;(ET_OECS)-OE on pin 20, active-high CS on pin 19 (CS2)

	dw		0x0203				;(ET_PGMVPP)No PGM pin, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, use 6.2V supply for programming Vcc

	dw		(.25<<8)+0x11		;(ET_VPPSETUP)Vpp = 12.75V, 0V during read, pulsed Vpp, return to +5V
								;(ET_PROGREPS) max p = 25

	dw		(0x00<<8)+.15		;(ET_PROGPULSE) 150 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0001				;(ET_SMART1) P+1
								;(ET_SMART2) No second pass

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 0F +
;+-------------+
;+ 2Kx8 EEPROM +
;+-------------+
ep_2816a:
	dw		(0xFF<<8)+(es_2816a-estrings)/8							;(ET_NAME)

	dw		(((es_km2816a-estrings)/8)<<8)+(es_samsung-estrings)/8	;(ET_PARTNO1)
	dw		(((es_2816a-estrings)/8)<<8)+(es_seeq-estrings)/8		;(ET_PARTNO2)
	dw		(((es_5516a-estrings)/8)<<8)+(es_seeq-estrings)/8		;(ET_PARTNO3)
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x5283				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x3100				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, active-low CS on pin 18

	dw		0x0140				;(ET_PGMVPP)active-low PGM on pin 21, no Vpp pin
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x06		;(ET_VPPSETUP)Vpp switcher off, 5V during read, pulsed PGM
								;(ET_PROGREPS) write once

	dw		((.12+80h)<<8)+0x00	;(ET_PROGPULSE) 250 nS programming pulse width
								;(ET_PPDELAY) 12 mS between writes

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0008				;(ET_SPECIAL) no blank check
								;(ET_RESERVED)

;+----+
;+ 10 +
;+-------------+
;+ 2Kx8 EEPROM +
;+-------------+
ep_28C16:
	dw		(0xFF<<8)+(es_28c16-estrings)/8								;(ET_NAME)

	dw		(((es_at28c16-estrings)/8)<<8)+(es_atmel-estrings)/8		;(ET_PARTNO1)
	dw		(((es_at28c16e-estrings)/8)<<8)+(es_atmel-estrings)/8		;(ET_PARTNO2)
	dw		(((es_cat28c16a-estrings)/8)<<8)+(es_catalyst-estrings)/8	;(ET_PARTNO3)
	dw		(((es_xl2816a-estrings)/8)<<8)+(es_exel-estrings)/8			;(ET_PARTNO4)
	dw		(((es_xl28c16a-estrings)/8)<<8)+(es_exel-estrings)/8		;(ET_PARTNO5)
	dw		(((es_28c16a-estrings)/8)<<8)+(es_microchip-estrings)/8		;(ET_PARTNO6)
	dw		(((es_cat28c16a-estrings)/8)<<8)+(es_onsemi-estrings)/8		;(ET_PARTNO7)
	dw		(((es_x2816b-estrings)/8)<<8)+(es_xicor-estrings)/8		;(ET_PARTNO8)
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x5283				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x3100				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, active-low CS on pin 18

	dw		0x0140				;(ET_PGMVPP)active-low PGM on pin 21, no Vpp pin
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x06		;(ET_VPPSETUP)Vpp switcher off, 5V during read, pulsed PGM
								;(ET_PROGREPS) write once

	dw		0x0100				;(ET_PROGPULSE) 250 nS programming pulse width
								;(ET_PPDELAY) poll for completion between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0008				;(ET_SPECIAL) no blank check
								;(ET_RESERVED)

;+----+
;+ 11 +
;+-----------------------------------+
;+ 2Kx8 EEPROM   					 +
;+ Not self-timed, 12 mS write pulse +
;+-----------------------------------+
ep_2816ai:
	dw		(0xFF<<8)+(es_2816ai-estrings)/8					;(ET_NAME)

	dw		(((es_2816a-estrings)/8)<<8)+(es_intel-estrings)/8	;(ET_PARTNO1)
	dw		(((es_52b13-estrings)/8)<<8)+(es_seeq-estrings)/8	;(ET_PARTNO2)
	dw		0xFFFF				;(ET_PARTNO3) null
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x5283				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x3100				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, active-low CS on pin 18

	dw		0x0140				;(ET_PGMVPP)active-low PGM on pin 21, no Vpp pin
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x06		;(ET_VPPSETUP)Vpp switcher off, 5V during read, pulsed PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.12+80h	;(ET_PROGPULSE) 12 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x4149				;(ET_SMART1) pass 1 erases the EEPROM
								;(ET_SMART2) pass 2 writes it

	dw		0x0008				;(ET_SPECIAL) no blank check
								;(ET_RESERVED)

;+----+
;+ 12 +
;+------------------------+
;+ Generic 2732 (Vpp=25V) +
;+------------------------+

ep_2732:
	dw		(0xFF<<8)+(es_2732-estrings)/8								;(ET_NAME)

;	dw		(((es_et2732q-estrings)/8)<<8)+(es_eurotech-estrings)/8		;(also)

	dw		(((es_am2732-estrings)/8)<<8)+(es_amd-estrings)/8			;(ET_PARTNO1)
	dw		(((es_f2732-estrings)/8)<<8)+(es_fairchild-estrings)/8		;(ET_PARTNO2)
	dw		(((es_mbm2732-estrings)/8)<<8)+(es_fujitsu-estrings)/8		;(ET_PARTNO3)
	dw		(((es_hn462732g-estrings)/8)<<8)+(es_hitachi-estrings)/8	;(ET_PARTNO4)
	dw		(((es_2732-estrings)/8)<<8)+(es_intel-estrings)/8			;(ET_PARTNO5)
	dw		(((es_m5l2732k-estrings)/8)<<8)+(es_mitsubishi-estrings)/8	;(ET_PARTNO6)
	dw		(((es_u2732-estrings)/8)<<8)+(es_mme-estrings)/8			;(ET_PARTNO7)
	dw		(((es_mcm2732-estrings)/8)<<8)+(es_motorola-estrings)/8		;(ET_PARTNO8)
	dw		(((es_upd2732-estrings)/8)<<8)+(es_nec-estrings)/8			;(ET_PARTNO8)
	dw		(((es_tmm2732d-estrings)/8)<<8)+(es_toshiba-estrings)/8		;(ET_PARTNO10)

	dw		0x5284				;(ET_BYTES)4096 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x1340				;(ET_A11A12)A11 on pin 21, no A12 pin
								;(ET_OECS)-OE on pin 18, active-low -CS on pin 20

	dw		0x0113				;(ET_PGMVPP)acive low PGM on pin 18, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x42		;(ET_VPPSETUP)Vpp = 25V, 0V during read, Pulsed -PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.50+0x80	;(ET_PROGPULSE) 50 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 13 +
;+-------------------+
;+ 2732 with Vpp=21V +
;+-------------------+
ep_2732A:
	dw		(0xFF<<8)+(es_2732a-estrings)/8								;(ET_NAME)

	dw		(((es_am2732a-estrings)/8)<<8)+(es_amd-estrings)/8			;(ET_PARTNO1)
	dw		(((es_mbm2732a-estrings)/8)<<8)+(es_fujitsu-estrings)/8		;(ET_PARTNO2)
	dw		(((es_hn482732ag-estrings)/8)<<8)+(es_hitachi-estrings)/8	;(ET_PARTNO3)
	dw		(((es_2732a-estrings)/8)<<8)+(es_intel-estrings)/8			;(ET_PARTNO4)
	dw		(((es_upd2732a-estrings)/8)<<8)+(es_nec-estrings)/8			;(ET_PARTNO5)
	dw		(((es_r87C32-estrings)/8)<<8)+(es_rockwell-estrings)/8		;(ET_PARTNO6)
	dw		(((es_m2732a-estrings)/8)<<8)+(es_st-estrings)/8			;(ET_PARTNO7)
	dw		(((es_tms2732a-estrings)/8)<<8)+(es_ti-estrings)/8			;(ET_PARTNO8)
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x5284				;(ET_BYTES)4096 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x1340				;(ET_A11A12)A11 on pin 21, no A12 pin
								;(ET_OECS)-OE on pin 18, active-low -CS on pin 20

	dw		0x0113				;(ET_PGMVPP)acive low PGM on pin 18, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x32		;(ET_VPPSETUP)Vpp = 21V, 0V during read, Pulsed -PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.50+0x80	;(ET_PROGPULSE) 50 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 14 +
;+-------------------------+
;+ 2732 with Vpp=21V, FAST +
;+-------------------------+
ep_2732af:
	dw		(0xFF<<8)+(es_2732afast-estrings)/8					;(ET_NAME)

	dw		(((es_m2732af-estrings)/8)<<8)+(es_st-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF												;(ET_PARTNO2) null
	dw		0xFFFF												;(ET_PARTNO3) null
	dw		0xFFFF												;(ET_PARTNO4) null
	dw		0xFFFF												;(ET_PARTNO5) null
	dw		0xFFFF												;(ET_PARTNO6) null
	dw		0xFFFF												;(ET_PARTNO7) null
	dw		0xFFFF												;(ET_PARTNO8) null
	dw		0xFFFF												;(ET_PARTNO9) null
	dw		0xFFFF												;(ET_PARTN10) null

	dw		0x5284				;(ET_BYTES)4096 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x1340				;(ET_A11A12)A11 on pin 21, no A12 pin
								;(ET_OECS)-OE on pin 18, active-low -CS on pin 20

	dw		0x0213				;(ET_PGMVPP)acive low PGM on pin 18, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 6V

	dw		(.25<<8)+0x32		;(ET_VPPSETUP)Vpp = 21V, 0V during read, Pulsed -PGM
								;(ET_PROGREPS) max P=25

	dw		(0x00<<8)+.100		;(ET_PROGPULSE) 1000 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0033				;(ET_SMART1) P+3P*ET_PROGPULSE smart programming
								;(ET_SMART2) no second pass

	dw		0x0000				;(ET_SPECIAL) Nothing special

;+----+
;+ 15 +
;+----------------------+
;+ 2732 with Vpp=12.75V +
;+----------------------+
ep_2732B:
	dw		(0xFF<<8)+(es_2732b-estrings)/8						;(ET_NAME)

	dw		(((es_am2732b-estrings)/8)<<8)+(es_amd-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF												;(ET_PARTNO2) null
	dw		0xFFFF												;(ET_PARTNO3) null
	dw		0xFFFF												;(ET_PARTNO4) null
	dw		0xFFFF												;(ET_PARTNO5) null
	dw		0xFFFF												;(ET_PARTNO6) null
	dw		0xFFFF												;(ET_PARTNO7) null
	dw		0xFFFF												;(ET_PARTNO8) null
	dw		0xFFFF												;(ET_PARTNO9) null
	dw		0xFFFF												;(ET_PARTNO10) null

	dw		0x5284				;(ET_BYTES)4096 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x1340				;(ET_A11A12)A11 on pin 21, no A12 pin
								;(ET_OECS)-OE on pin 18, active-low -CS on pin 20

	dw		0x0113				;(ET_PGMVPP)acive low PGM on pin 18, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.15<<8)+0x12		;(ET_VPPSETUP)Vpp = 12.75V, 0V during read, Pulsed -PGM
								;(ET_PROGREPS) max 15 tries

	dw		(0x00<<8)+.100		;(ET_PROGPULSE) 1000 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0012				;(ET_SMART1) P+2*ET_PROGPULSE smart
								;(ET_SMART2) No second pass

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 16 +
;+----------------------+
;+ 2732H				+
;+----------------------+
ep_2732H:
	dw		(0xFF<<8)+(es_27c32h-estrings)/8							;(ET_NAME)

	dw		(((es_nmc27c32h-estrings)/8)<<8)+(es_fairchild-estrings)/8	;(ET_PARTNO1)
	dw		(((es_nmc27c32h-estrings)/8)<<8)+(es_national-estrings)/8	;(ET_PARTNO2)
	dw		0xFFFF														;(ET_PARTNO3) null
	dw		0xFFFF														;(ET_PARTNO4) null
	dw		0xFFFF														;(ET_PARTNO5) null
	dw		0xFFFF														;(ET_PARTNO6) null
	dw		0xFFFF														;(ET_PARTNO7) null
	dw		0xFFFF														;(ET_PARTNO8) null
	dw		0xFFFF														;(ET_PARTNO9) null
	dw		0xFFFF														;(ET_PARTNO10) null

	dw		0x5284				;(ET_BYTES)4096 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x1340				;(ET_A11A12)A11 on pin 21, no A12 pin
								;(ET_OECS)-OE on pin 18, active-low -CS on pin 20

	dw		0x0113				;(ET_PGMVPP)acive low PGM on pin 18, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x12		;(ET_VPPSETUP)Vpp = 12.75V, 0V during read, Pulsed -PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.10+0x80	;(ET_PROGPULSE) 10 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 17 +
;+-----------------------+
;+ 27C32 with Vpp=12.75V +
;+-----------------------+
ep_27C32B:
	dw		(0xFF<<8)+(es_27c32b-estrings)/8							;(ET_NAME)

	dw		(((es_nmc27c32b-estrings)/8)<<8)+(es_fairchild-estrings)/8	;(ET_PARTNO1)
	dw		(((es_nmc27c32b-estrings)/8)<<8)+(es_national-estrings)/8	;(ET_PARTNO2)
	dw		0xFFFF														;(ET_PARTNO3) null
	dw		0xFFFF														;(ET_PARTNO4) null
	dw		0xFFFF														;(ET_PARTNO5) null
	dw		0xFFFF														;(ET_PARTNO6) null
	dw		0xFFFF														;(ET_PARTNO7) null
	dw		0xFFFF														;(ET_PARTNO8) null
	dw		0xFFFF														;(ET_PARTNO9) null
	dw		0xFFFF														;(ET_PARTNO10) null

	dw		0x5284				;(ET_BYTES)4096 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x1340				;(ET_A11A12)A11 on pin 21, no A12 pin
								;(ET_OECS)-OE on pin 18, active-low -CS on pin 20

	dw		0x0213				;(ET_PGMVPP)acive low PGM on pin 18, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 6.2V

	dw		(.20<<8)+0x12		;(ET_VPPSETUP)Vpp = 12.75V, 0V during read, Pulsed -PGM
								;(ET_PROGREPS) max 20 tries

	dw		(0x00<<8)+.10		;(ET_PROGPULSE) 100 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0080				;(ET_SMART1) P, n=0 (msb set so it is a P algorithm)
								;(ET_SMART2) No second pass

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 18 +
;+-----------------------------------------------------+
;+ Texas Instruments 4Kx8 EPROM, active-low PGM signal +
;+ Vpp = 25V                                           +
;+-----------------------------------------------------+
ep_TMS2532:
	dw		(0xFF<<8)+(es_tms2532-estrings)/8						;(ET_NAME)

	dw		(((es_hn462532-estrings)/8)<<8)+(es_hitachi-estrings)/8	;(ET_PARTNO1)
	dw		(((es_mcm2532-estrings)/8)<<8)+(es_motorola-estrings)/8	;(ET_PARTNO2)
	dw		(((es_m2532-estrings)/8)<<8)+(es_sgs-estrings)/8		;(ET_PARTNO3)
	dw		(((es_tms2532-estrings)/8)<<8)+(es_ti-estrings)/8		;(ET_PARTNO4)
	dw		0xFFFF													;(ET_PARTNO5) null
	dw		0xFFFF													;(ET_PARTNO6) null
	dw		0xFFFF													;(ET_PARTNO7) null
	dw		0xFFFF													;(ET_PARTNO8) null
	dw		0xFFFF													;(ET_PARTNO9) null
	dw		0xFFFF													;(ET_PARTNO10) null

	dw		0x5284				;(ET_BYTES)4096 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x0310				;(ET_A11A12)A11 on pin 18, no A12 pin
								;(ET_OECS)no -OE pin, active-low -CS on pin 20

	dw		0x0134				;(ET_PGMVPP)Active low PGM on pin 20, Vpp on pin 21
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x46		;(ET_VPPSETUP)Vpp = 25V, high during read, pulsed -PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.50+0x80	;(ET_PROGPULSE) 50 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 19 +
;+-----------------------------------------------------+
;+ Texas Instruments 4Kx8 EPROM, active-low PGM signal +
;+ Vpp = 21V                                           +
;+-----------------------------------------------------+
ep_TMS2532A:
	dw		(0xFF<<8)+(es_tms2532a-estrings)/8					;(ET_NAME)

	dw		(((es_tms2532a-estrings)/8)<<8)+(es_ti-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF												;(ET_PARTNO2) null
	dw		0xFFFF												;(ET_PARTNO3) null
	dw		0xFFFF												;(ET_PARTNO4) null
	dw		0xFFFF												;(ET_PARTNO5) null
	dw		0xFFFF												;(ET_PARTNO6) null
	dw		0xFFFF												;(ET_PARTNO7) null
	dw		0xFFFF												;(ET_PARTNO8) null
	dw		0xFFFF												;(ET_PARTNO9) null
	dw		0xFFFF												;(ET_PARTNO10) null

	dw		0x5284				;(ET_BYTES)4096 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x0310				;(ET_A11A12)A11 on pin 18, no A12 pin
								;(ET_OECS)no -OE pin, active-low -CS on pin 20

	dw		0x0134				;(ET_PGMVPP)Active low PGM on pin 20, Vpp on pin 21
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x36		;(ET_VPPSETUP)Vpp = 21V, high during read, pulsed -PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.50+0x80	;(ET_PROGPULSE) 50 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 1A +
;+----------------------+
;+ TI 2732 with Vpp=21V +
;+----------------------+
ep_TMS2732A:
	dw		(0xFF<<8)+(es_tms2732a-estrings)/8					;(ET_NAME)

	dw		(((es_tms2732a-estrings)/8)<<8)+(es_ti-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF												;(ET_PARTNO2) null
	dw		0xFFFF												;(ET_PARTNO3) null
	dw		0xFFFF												;(ET_PARTNO4) null
	dw		0xFFFF												;(ET_PARTNO5) null
	dw		0xFFFF												;(ET_PARTNO6) null
	dw		0xFFFF												;(ET_PARTNO7) null
	dw		0xFFFF												;(ET_PARTNO8) null
	dw		0xFFFF												;(ET_PARTNO9) null
	dw		0xFFFF												;(ET_PARTNO10) null
	dw		0x5284				;(ET_BYTES)4096 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x1340				;(ET_A11A12)A11 on pin 21, no A12 pin
								;(ET_OECS)-OE on pin 18, active-low -CS on pin 20

	dw		0x0113				;(ET_PGMVPP)acive low PGM on pin 18, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x32		;(ET_VPPSETUP)Vpp = 21V, 0V during read, Pulsed -PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.10+0x80	;(ET_PROGPULSE) 10 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 1B +
;+----------------+
;+ WSI 4Kx8 EPROM +
;+----------------+
ep_57C43:
	dw		(0xFF<<8)+(es_57c43-estrings)/8						;(ET_NAME)

	dw		(((es_57c43-estrings)/8)<<8)+(es_wsi-estrings)/8	;(ET_PARTNO1)
	dw		(((es_ws57c43b-estrings)/8)<<8)+(es_wsi-estrings)/8	;(ET_PARTNO2)
	dw		0xFFFF												;(ET_PARTNO3) null
	dw		0xFFFF												;(ET_PARTNO4) null
	dw		0xFFFF												;(ET_PARTNO5) null
	dw		0xFFFF												;(ET_PARTNO6) null
	dw		0xFFFF												;(ET_PARTNO7) null
	dw		0xFFFF												;(ET_PARTNO8) null
	dw		0xFFFF												;(ET_PARTNO9) null

	dw		(0xFF<<8)+(es_55v-estrings)/8			;(ET_VCCMSG)prog. Vcc requires adjustment
													;(ET_VPPMSG) null

	dw		0x5484				;(ET_BYTES)4K bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 21

	dw		0x3920				;(ET_A11A12)A11 on pin 19, no A12
								;(ET_OECS) -OE on pin 20, active-high CS on pin 18 (CS2)

	dw		0x0203				;(ET_PGMVPP)No PGM pin, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, use 6.2V supply for programming Vcc

	dw		(.01<<8)+0x21		;(ET_VPPSETUP)Vpp = 13.15V, 0V during read, pulsed Vpp, return to +5V
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.10+0x80	;(ET_PROGPULSE) 10 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0001				;(ET_SPECIAL) Programming Vcc requires adjustment
								;(ET_RESERVED)

;+----+
;+ 1C +
;+------------------------------------------+
;+ WSI 4Kx8 EPROM                           +
;+ Note: The Smart algorith mere is a guess +
;+------------------------------------------+
ep_57C43C:
	dw		(0xFF<<8)+(es_57c43c-estrings)/8					;(ET_NAME)

	dw		(((es_ws57c43c-estrings)/8)<<8)+(es_wsi-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF												;(ET_PARTNO2) null
	dw		0xFFFF												;(ET_PARTNO3) null
	dw		0xFFFF												;(ET_PARTNO4) null
	dw		0xFFFF												;(ET_PARTNO5) null
	dw		0xFFFF												;(ET_PARTNO6) null
	dw		0xFFFF												;(ET_PARTNO7) null
	dw		0xFFFF												;(ET_PARTNO8) null
	dw		0xFFFF												;(ET_PARTNO9) null
	dw		0xFFFF												;(ET_PARTNO10) null

	dw		0x5484				;(ET_BYTES)4K bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 21

	dw		0x3920				;(ET_A11A12)A11 on pin 19, no A12
								;(ET_OECS) -OE on pin 20, active-high CS on pin 18 (CS2)

	dw		0x0203				;(ET_PGMVPP)No PGM pin, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, use 6.2V supply for programming Vcc

	dw		(.25<<8)+0x11		;(ET_VPPSETUP)Vpp = 12.75V, 0V during read, pulsed Vpp, return to +5V
								;(ET_PROGREPS) Max p = 25

	dw		(0x00<<8)+.15		;(ET_PROGPULSE) 150 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0001				;(ET_SMART1) P+1
								;(ET_SMART2) No second pass

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 1D +
;+-----------------------------------+
;+ Motorola 8Kx8 EPROM in 24-pin DIP +
;+ (also serves for the MCM68766)    +
;+-----------------------------------+
ep_MCM68764:
	dw		(0xFF<<8)+(es_68764-estrings)/8								;(ET_NAME)

	dw		(((es_mcm68764-estrings)/8)<<8)+(es_motorola-estrings)/8	;(ET_PARTNO1)
	dw		(((es_mcm68766-estrings)/8)<<8)+(es_motorola-estrings)/8	;(ET_PARTNO2)
	dw		0xFFFF														;(ET_PARTNO3) null
	dw		0xFFFF														;(ET_PARTNO4) null
	dw		0xFFFF														;(ET_PARTNO5) null
	dw		0xFFFF														;(ET_PARTNO6) null
	dw		0xFFFF														;(ET_PARTNO7) null
	dw		0xFFFF														;(ET_PARTNO8) null
	dw		0xFFFF														;(ET_PARTNO9) null
	dw		0xFFFF														;(ET_PARTN10) null

	dw		0x5285				;(ET_BYTES)8196 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x3014				;(ET_A11A12)A11 on pin 18, A12 on pin 21
								;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0203				;(ET_PGMVPP)no PGM pin, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 6V

	dw		(.20<<8)+0x41		;(ET_VPPSETUP)Vpp = 25V, 0V during read, pulsed Vpp, return to +5V
								;(ET_PROGREPS) max 20 passes

	dw		(0x00<<8)+.100		;(ET_PROGPULSE) 1 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0080				;(ET_SMART1) P, no n (bit 7 set to make it a smart algorithm.)
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 1E +
;+----------------------------+
;+ Sharp 8Kx8 in a 24-pin DIP +
;+----------------------------+
ep_LH5749:
	dw		(0xFF<<8)+(es_lh5749-estrings)/8						;(ET_NAME)

	dw		(((es_lh5749-estrings)/8)<<8)+(es_sharp-estrings)/8		;(ET_PARTNO1)
	dw		0xFFFF													;(ET_PARTNO2) null
	dw		0xFFFF													;(ET_PARTNO3) null
	dw		0xFFFF													;(ET_PARTNO4) null
	dw		0xFFFF													;(ET_PARTNO5) null
	dw		0xFFFF													;(ET_PARTNO6) null
	dw		0xFFFF													;(ET_PARTNO7) null
	dw		0xFFFF													;(ET_PARTNO8) null
	dw		0xFFFF													;(ET_PARTNO9) null
	dw		0xFFFF													;(ET_PARTN10) null

	dw		0x5485				;(ET_BYTES)8K bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 21

	dw		0x3021				;(ET_A11A12)A11 on pin 19, A12 on pin 18
								;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0203				;(ET_PGMVPP)No PGM pin, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, use 6.2V supply for programming Vcc

	dw		(.25<<8)+0x21		;(ET_VPPSETUP)Vpp = 13.1V, 0V during read, pulsed Vpp, return to +5V
								;(ET_PROGREPS) max 25 passes

	dw		(0x00<<8)+.100		;(ET_PROGPULSE) 1 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0033				;(ET_SMART1) P+ 3n*ET_PROGPULSE
								;(ET_SMART2)

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 1F +
;+----------------------------------------------+
;+ "Fast" algorithm								+
;+ 8Xx8 EPROM in 24-pin DIP						+
;+ Requires Vcc adjustment for programming		+
;+----------------------------------------------+
ep_27HC641:
	dw		(0xFF<<8)+(es_27hc641-estrings)/8							;(ET_NAME)

	dw		(((es_at27hc641-estrings)/8)<<8)+(es_atmel-estrings)/8		;(ET_PARTNO1)
	dw		(((es_at27hc642-estrings)/8)<<8)+(es_atmel-estrings)/8		;(ET_PARTNO2)
	dw		(((es_27hc641-estrings)/8)<<8)+(es_microchip-estrings)/8	;(ET_PARTNO3)
	dw		0xFFFF														;(ET_PARTNO4) null
	dw		0xFFFF														;(ET_PARTNO5) null
	dw		0xFFFF														;(ET_PARTNO6) null
	dw		0xFFFF														;(ET_PARTNO7) null
	dw		0xFFFF														;(ET_PARTNO8) null
	dw		0xFFFF														;(ET_PARTNO9) null
	dw		0xFFFF														;(ET_PARTN10) null

	dw		0x5485			;(ET_BYTES)8K bytes, 8 bits/byte
							;(ET_A9A10)A9 on pin 22, A10 on pin 21

	dw		0x3021			;(ET_A11A12)A11 on pin 19, A12 on pin 18
							;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0203			;(ET_PGMVPP)No PGM pin, Vpp on pin 20
							;(ET_VBBVDD)no Vbb or Vdd, use 6.2V supply for programming Vcc

	dw		(.25<<8)+0x11	;(ET_VPPSETUP)Vpp = 12.75V, 0V during read, pulsed Vpp, return to 5V
							;(ET_PROGREPS) max 25 tries

	dw		(0x00<<8)+.10	;(ET_PROGPULSE) 100 uS programming pulse width
							;(ET_PPDELAY) no delay between pulses

	dw		0x0033			;(ET_SMART1) P+3P*ET_PROGPULSE smart programming
							;(ET_SMART2) no second pass

	dw		0x0000			;(ET_SPECIAL) Nothing special
							;(ET_RESERVED)

;+----+
;+ 20 +
;+----------------------------------------------+
;+ 8Xx8 EPROM in 24-pin DIP						+
;+ Requires Vcc adjustment for programming		+
;+ Note: Cannot blank-check the Signetics part	+
;+       because erased bits are "undefined."	+
;+----------------------------------------------+
ep_27HC641s:
	dw		(0xFF<<8)+(es_27hc641s-estrings)/8							;(ET_NAME)

	dw		(((es_27hc641-estrings)/8)<<8)+(es_signetics-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF														;(ET_PARTNO2) null
	dw		0xFFFF														;(ET_PARTNO3) null
	dw		0xFFFF														;(ET_PARTNO4) null
	dw		0xFFFF														;(ET_PARTNO5) null
	dw		0xFFFF														;(ET_PARTNO6) null
	dw		0xFFFF														;(ET_PARTNO7) null
	dw		0xFFFF														;(ET_PARTNO8) null
	dw		0xFFFF														;(ET_PARTNO9) null
	dw		0xFFFF														;(ET_PARTN10) null

	dw		0x5485			;(ET_BYTES)8K bytes, 8 bits/byte
							;(ET_A9A10)A9 on pin 22, A10 on pin 21

	dw		0x3021			;(ET_A11A12)A11 on pin 19, A12 on pin 18
							;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0203			;(ET_PGMVPP)No PGM pin, Vpp on pin 20
							;(ET_VBBVDD)no Vbb or Vdd, use 6.2V supply for programming Vcc

	dw		(.25<<8)+0x11	;(ET_VPPSETUP)Vpp = 12.75V, 0V during read, pulsed Vpp, return to 5V
							;(ET_PROGREPS) max 25 tries

	dw		(0x00<<8)+.10	;(ET_PROGPULSE) 100 uS programming pulse width
							;(ET_PPDELAY) no delay between pulses

	dw		0x0033			;(ET_SMART1) P+3P*ET_PROGPULSE smart programming
							;(ET_SMART2) no second pass

	dw		0x0008			;(ET_SPECIAL) No blank-check (erased bits are undefined.)
							;(ET_RESERVED)

;+----+
;+ 21 +
;+-----------------------------------------+
;+ "Express" Algorithm                     +
;+ Atmel (etc.)8Xx8 EPROM in 24-pin DIP    +
;+ Requires Vcc adjustment for programming +
;+ Serves both the  AT27HC641R and the     +
;+ AT27HC642R                              +
;+-----------------------------------------+
ep_27HC641r:
	dw		(0xFF<<8)+(es_27hc641r-estrings)/8							;(ET_NAME)

	dw		(((es_at27hc641r-estrings)/8)<<8)+(es_atmel-estrings)/8		;(ET_PARTNO1)
	dw		(((es_at27hc642r-estrings)/8)<<8)+(es_atmel-estrings)/8		;(ET_PARTNO2)
	dw		(((es_27hc641-estrings)/8)<<8)+(es_microchip-estrings)/8	;(ET_PARTNO3)
	dw		0xFFFF														;(ET_PARTNO4) null
	dw		0xFFFF														;(ET_PARTNO5) null
	dw		0xFFFF														;(ET_PARTNO6) null
	dw		0xFFFF														;(ET_PARTNO7) null
	dw		0xFFFF														;(ET_PARTNO8) null
	dw		0xFFFF														;(ET_PARTNO9) null

	dw		(es_65v-estrings)/8						;(ET_VCCMSG)Prog. Vcc requires adjustment
													;(ET_VPPMSG) null

	dw		0x5485			;(ET_BYTES)8K bytes, 8 bits/byte
							;(ET_A9A10)A9 on pin 22, A10 on pin 21

	dw		0x3021			;(ET_A11A12)A11 on pin 19, A12 on pin 18
							;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0203			;(ET_PGMVPP)No PGM pin, Vpp on pin 20
							;(ET_VBBVDD)no Vbb or Vdd, use 6.2V supply for programming Vcc

	dw		(.10<<8)+0x21	;(ET_VPPSETUP)Vpp = 13.15V, 0V during read, pulsed Vpp, return to 5V
							;(ET_PROGREPS) max 10 tries

	dw		(0x00<<8)+.10	;(ET_PROGPULSE) 100 uS programming pulse width
							;(ET_PPDELAY) no delay between pulses

	dw		0x8041			;(ET_SMART1) one pass through
							;(ET_SMART2) P+0

	dw		0x0001			;(ET_SPECIAL) Programming Vcc requires adjustment
							;(ET_RESERVED)

;+----+
;+ 22 +
;+----------------+
;+ WSI 8Kx8 EPROM +
;+----------------+
ep_57C49b:
	dw		(0xFF<<8)+(es_57c49b-estrings)/8					;(ET_NAME)

	dw		(((es_ws57c49b-estrings)/8)<<8)+(es_wsi-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF												;(ET_PARTNO2) null
	dw		0xFFFF												;(ET_PARTNO3) null
	dw		0xFFFF												;(ET_PARTNO4) null
	dw		0xFFFF												;(ET_PARTNO5) null
	dw		0xFFFF												;(ET_PARTNO6) null
	dw		0xFFFF												;(ET_PARTNO7) null
	dw		0xFFFF												;(ET_PARTNO8) null
	dw		0xFFFF												;(ET_PARTNO9) null

	dw		(0xFF<<8)+(es_55v-estrings)/8			;(ET_VCCMSG)prog. Vcc requires adjustment
													;(ET_VPPMSG) null

	dw		0x5485				;(ET_BYTES)8K bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 21

	dw		0x3021				;(ET_A11A12)A11 on pin 19, A12 on pin 18
								;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0203				;(ET_PGMVPP)No PGM pin, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, use 6.2V supply for programming Vcc

	dw		(.01<<8)+0x21		;(ET_VPPSETUP)Vpp = 13.15V, 0V during read, pulsed Vpp, return to +5V
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.10+0x80	;(ET_PROGPULSE) 10 mS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0001				;(ET_SPECIAL) Programming Vcc requires adjustment
								;(ET_RESERVED)

;+----+
;+ 23 +
;+------------------------------------------+
;+ WSI 8Kx8 EPROM                           +
;+ Note: The Smart algorith mere is a guess +
;+------------------------------------------+
ep_57C49C:
	dw		(0xFF<<8)+(es_57c49c-estrings)/8					;(ET_NAME)

	dw		(((es_ws57c49c-estrings)/8)<<8)+(es_wsi-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF												;(ET_PARTNO2) null
	dw		0xFFFF												;(ET_PARTNO3) null
	dw		0xFFFF												;(ET_PARTNO4) null
	dw		0xFFFF												;(ET_PARTNO5) null
	dw		0xFFFF												;(ET_PARTNO6) null
	dw		0xFFFF												;(ET_PARTNO7) null
	dw		0xFFFF												;(ET_PARTNO8) null
	dw		0xFFFF												;(ET_PARTNO9) null
	dw		0xFFFF												;(ET_PARTNO10) null

	dw		0x5485			;(ET_BYTES)8K bytes, 8 bits/byte
							;(ET_A9A10)A9 on pin 22, A10 on pin 21

	dw		0x3021			;(ET_A11A12)A11 on pin 19, A12 on pin 18
							;(ET_OECS)-OE on pin 20, no CS pin

	dw		0x0203			;(ET_PGMVPP)No PGM pin, Vpp on pin 20
							;(ET_VBBVDD)no Vbb or Vdd, use 6.2V supply for programming Vcc

	dw		(.25<<8)+0x11	;(ET_VPPSETUP)Vpp = 12.75V, 0V during read, pulsed Vpp, return to +5V
							;(ET_PROGREPS) max p = 25

	dw		(0x00<<8)+.15	;(ET_PROGPULSE) 150 uS programming pulse width
							;(ET_PPDELAY) no delay between pulses

	dw		0x0001				;(ET_SMART1) P+1
								;(ET_SMART2) No second pass

	dw		0x0000			;(ET_SPECIAL) Nothing special
							;(ET_RESERVED)

;+----+
;+ 24 +
;+------------------------------------+
;+ 2Kx8 EEPROM   					  +
;+ Not self-timed, 1.2 mS write pulse +
;+------------------------------------+
ep_52b13h:
	dw		(0xFF<<8)+(es_52b13h-estrings)/8					;(ET_NAME)

	dw		(((es_52b13h-estrings)/8)<<8)+(es_seeq-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF				;(ET_PARTNO2) null
	dw		0xFFFF				;(ET_PARTNO3) null
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x5283				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 19

	dw		0x3100				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, active-low CS on pin 18

	dw		0x0140				;(ET_PGMVPP)active-low PGM on pin 21, no Vpp pin
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x06		;(ET_VPPSETUP)Vpp switcher off, 5V during read, pulsed PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.120		;(ET_PROGPULSE) 1200 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x4149				;(ET_SMART1) pass 1 erases the EEPROM
								;(ET_SMART2) pass 2 writes it

	dw		0x0008				;(ET_SPECIAL) no blank check
								;(ET_RESERVED)

;+----+
;+ 25 +
;+--------------------------------+
;+ Intersil IM6658                +
;+ Requires external negative Vpp +
;+--------------------------------+
ep_IM6658:
	dw		(0xFF<<8)+(es_im6658-estrings)/8						;(ET_NAME)

	dw		(((es_im6658-estrings)/8)<<8)+(es_intersil-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF				;(ET_PARTNO2) null
	dw		0xFFFF				;(ET_PARTNO3) null
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null

	dw		(((es_n30v-estrings)/8)<<8)+0xFF	;(ET_VPPMSG)
												;(ET_VCCMSG) null

	dw		0x5082					;(ET_BYTES)1024 bytes, 8 bits/byte
									;(ET_A9A10)A9 on pin 22, no A10 pin

	dw		0x400B					;(ET_A11A12) no A11, -Address Strobe (-AS) on pin 20
									;(ET_OECS)-OE on pin 21, no CS pin

	dw		0x0101					;(ET_PGMVPP)No PGM pin, Vpp on pin 18
									;(ET_VBBVDD)no Vbb or Vdd (Vdd pin 19 is a stuck-high pin)

	dw		(.2<<8)+0x55			;(ET_VPPSETUP)External Vpp, 5V during read, pulsed Vpp, return to +5V
									;(ET_PROGREPS) max 2 passes for P

	dw		((.7+0x80)<<8)+.20+0x80	;(ET_PROGPULSE) 20 mS programming pulse width
									;(ET_PPDELAY) 7 mS delay between pulses

	dw		0x0024					;(ET_SMART1) P+4P smart programming
									;(ET_SMART1) no second pass

	dw		0x0004					;(ET_SPECIAL)requires external negative Vpp
									;(ET_RESERVED)

;+----+
;+ 26 +
;+-----------------------------------------------------+
;+ Sharp 2Kx8 EPROM, with Vcc=6.2V during programming +
;+-----------------------------------------------------+
ep_lh57191:
	dw		(0xFF<<8)+(es_lh57191-estrings)/8						;(ET_NAME)

	dw		(((es_lh57191-estrings)/8)<<8)+(es_sharp-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF				;(ET_PARTNO2) null
	dw		0xFFFF				;(ET_PARTNO3) null
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTN10) null

	dw		0x5483				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)A9 on pin 22, A10 on pin 21

	dw		0x3A90				;(ET_A11A12)Force pin 18 high (CS3), no A12
								;(ET_OECS)-OE on pin 20, active-high CS on pin 19 (CS2)

	dw		0x0203				;(ET_PGMVPP)No PGM pin, Vpp on pin 20
								;(ET_VBBVDD)no Vbb or Vdd, use 6.2V supply for programming Vcc

	dw		(.25<<8)+0x21		;(ET_VPPSETUP)Vpp = 13.15V, 0V during read, pulsed Vpp, return to +5V
								;(ET_PROGREPS) max p = 25

	dw		(0x00<<8)+.100		;(ET_PROGPULSE) 1000 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0033				;(ET_SMART1) P+ 3*P*1000 uS pulse
								;(ET_SMART2) No second pass

	dw		0x0000				;(ET_SPECIAL) Nothing special
								;(ET_RESERVED)

;+----+
;+ 27 +
;+------------------------------------+
;+ 512x8 EEPROM   					  +
;+ Not self-timed, 1.2 mS write pulse +
;+------------------------------------+
ep_28c04n:
	dw		(0xFF<<8)+(es_28c04n-estrings)/8					;(ET_NAME)

	dw		(((es_upd28c04-estrings)/8)<<8)+(es_nec-estrings)/8	;(ET_PARTNO1)
	dw		0xFFFF				;(ET_PARTNO2) null
	dw		0xFFFF				;(ET_PARTNO3) null
	dw		0xFFFF				;(ET_PARTNO4) null
	dw		0xFFFF				;(ET_PARTNO5) null
	dw		0xFFFF				;(ET_PARTNO6) null
	dw		0xFFFF				;(ET_PARTNO7) null
	dw		0xFFFF				;(ET_PARTNO8) null
	dw		0xFFFF				;(ET_PARTNO9) null
	dw		0xFFFF				;(ET_PARTNO10) null

	dw		0x0083				;(ET_BYTES)2048 bytes, 8 bits/byte
								;(ET_A9A10)No A9 pin, No A10 pin

	dw		0x3100				;(ET_A11A12)no A11 pin, no A12 pin
								;(ET_OECS)-OE on pin 20, active-low CS on pin 18

	dw		0x0140				;(ET_PGMVPP)active-low PGM on pin 21, no Vpp pin
								;(ET_VBBVDD)no Vbb or Vdd, Vcc = 5V

	dw		(.01<<8)+0x06		;(ET_VPPSETUP)Vpp switcher off, 5V during read, pulsed PGM
								;(ET_PROGREPS) write once

	dw		(0x00<<8)+.120		;(ET_PROGPULSE) 1200 uS programming pulse width
								;(ET_PPDELAY) no delay between pulses

	dw		0x0000				;(ET_SMART1) No smart programming
								;(ET_SMART2)

	dw		0x0008				;(ET_SPECIAL) no blank check
								;(ET_RESERVED)

;How many EPROMs are in this table
FEPROM_COUNT	equ		($-eprom_table)/ET_RLENGTH

	dw	0				;end of table marker

