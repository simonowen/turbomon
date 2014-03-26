               *REPORT OFF
;
; U S E F U L   S A M   C O N S T A N T S
;

; General values
; --------------
            cr:EQU  13

; Port values
; -----------
      kempston:EQU  31
     disc1base:EQU  224
     disc2base:EQU  240
          pens:EQU  248
          clut:EQU  248
        status:EQU  249
   key_lines_3:EQU  249
      line_int:EQU  249
          lmpr:EQU  250
          hmpr:EQU  251
          vmpr:EQU  252
          midi:EQU  253
      keyboard:EQU  254
        border:EQU  254
    attributes:EQU  255
         sound:EQU  255

; Kempston joystick direction bits
; --------------------------------
kempston_right:EQU  %00000001
 kempston_left:EQU  %00000010
 kempston_down:EQU  %00000100
   kempston_up:EQU  %00001000
 kempston_fire:EQU  %00010000

; Memory positions/sizes
; ----------------------
     section_a:EQU  0
     section_b:EQU  16384
     section_c:EQU  32768
     section_d:EQU  49152
      block_ab:EQU  0
      block_cd:EQU  32768
 half_page_len:EQU  8192
   page_length:EQU  16384
  block_length:EQU  32768

; Screen sizes
; ------------
  mode1_length:EQU  6912
  mode2_length:EQU  14336
  mode3_length:EQU  24576
  mode4_length:EQU  24576
mode1_data_len:EQU  6144
mode1_attr_len:EQU  768

; LMPR bit values
; ---------------
  wprot_bitval:EQU  %10000000
   rom1_bitval:EQU  %01000000
   rom0_bitval:EQU  %00100000

; VMPR bit values
; ---------------
         mode1:EQU  %00000000
         mode2:EQU  %00100000
         mode3:EQU  %01000000
         mode4:EQU  %01100000

; BODRER bit values
; -----------------
scr_off_bitval:EQU  %10000000

; Useful absolute addresses (naughty?)
; -------------------------
; Keyword table in ROM 1
 keyword_table:EQU  63689
;
; Palette currently being used by BASIC
 basic_palette:EQU  21976
