; TurboMON v1.0
;
; Copyright 1994 Simon Owen
;
; Glue module for assembling with pyz80

; ROM source files
include "TM_1.asm"
include "TM_2.asm"
include "TM_3.asm"
include "TM_4.asm"
include "TM_5.asm"
include "TM_6.asm"
include "TM_7.asm"
include "TM_8.asm"
include "TM_9.asm"

	DUMP put_base+&6000-1536
MDAT "font.bin"
