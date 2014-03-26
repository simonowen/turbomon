;
; M I S C   R O U T I N E S
;
; Includes: Keyboard handling, Message printing,
;           Memory management, Port management
;           and Screen scrolling/clearing
;
; K E Y B O A R D   H A N D L I N G
;
; Scan keys and put key code in (IY+KEY) ; the key code
; is stored in (IY+LAST_KEY) whilst waiting for repeats.
; Different key tables are used depending on whether a
; 'shift' key is being used. The shift keys in decresing
; order of priority are: CTRL, EDIT, SYMBOL, SHIFT, NORM
;
 read_keyboard:LD   HL,key_line_buf
               LD   B,%11111110
         sc_lp:LD   C,key_lines_3
               IN   A,(C)
               CPL
               AND  %11100000
               LD   (HL),A
               LD   C,keyboard
               IN   A,(C)
               CPL
               AND  %00011111
               OR   (HL)
               LD   (HL),A
               INC  HL
               SCF
               RL   B
               JR   C,sc_lp
               LD   C,keyboard
               IN   A,(C)
               CPL
               AND  %00011111
               BIT  0,A
; CTRL
               RES  0,A
               LD   (HL),A
               LD   IX,key_line_buf
               LD   HL,ctrl
               JR   NZ,got_key_tab
               BIT  7,(IX+6)
; EDIT
               RES  7,(IX+6)
               LD   HL,edit
               JR   NZ,got_key_tab
               BIT  1,(IX+7)
; SYMBOL
               RES  1,(IX+7)
               LD   HL,sym
               JR   NZ,got_key_tab
               BIT  0,(IX+0)
; SHIFT
               RES  0,(IX+0)
               LD   HL,shift
               JR   NZ,got_key_tab
               LD   HL,norm
   got_key_tab:LD   B,9
        highlp:PUSH BC
               LD   B,8
        rot8lp:RL   (IX+0)
               JR   NC,nopress
               LD   A,(HL)
       nopress:INC  HL
               DJNZ rot8lp
               INC  IX
               POP  BC
               DJNZ highlp
               LD   B,A
               CP   (IY+lastkey)
               LD   (IY+lastkey),A
               JR   Z,keyrep
               LD   (IY+repdel),repdel_long
               JR   key_set
        keyrep:DEC  (IY+repdel)
               JR   NZ,no_key
               LD   (IY+repdel),repdel_short
       key_set:LD   A,B
               LD   (IY+key),A
               BIT  0,(IY+misc_flags)
               RET  Z
               CALL alpha
               LD   A,B
               RET  C
; Toggle upper/lower case
               XOR  32
               LD   (IY+key),A
               RET
        no_key:XOR  A
               LD   (IY+key),A
               RET
;
; ALPHA - Return NC if code in A is alphabetic
;
         alpha:CP   "A"
               RET  C
               CP   91
               CCF
               RET  NC
               CP   "a"
               RET  C
               CP   123
               CCF
               RET
;
  key_line_buf:DS   9
;
;
; Keyboard decoding tables
;
          norm:DB   203,202,201,"v","c","x","z",0
               DB   206,205,204,"g","f","d","s","a"
               DB   209,208,207,"t","r","e","w","q"
               DB   1,198,255,"5","4","3","2","1"
               DB   12,"+","-","6","7","8","9","0"
               DB   200,34,"=","y","u","i","o","p"
               DB   0,":",";","h","j","k","l",cr
               DB   253,".",",","b","n","m",0," "
               DB   0,0,0,9,8,10,11,0
;
         shift:DB   203,202,201,"V","C","X","Z",0
               DB   206,205,204,"G","F","D","S","A"
               DB   209,208,207,"T","R","E","W","Q"
               DB   1,198,255,"%","$","#","@","!"
               DB   12,"*","/","&","'","(",")","~"
               DB   200,"","_","Y","U","I","O","P"
               DB   0,":",";","H","J","K","L",cr
               DB   253,".",",","B","N","M",0," "-1
               DB   0,0,0,19,18,20,21,0
;
           sym:DB   "3","2","1",0,0,"?",0,0
               DB   "6","5","4","}","{",0,0,0
               DB   "9","8","7","]","[",0,">","<"
               DB   1,198,255,"%","$","#","@","!"
               DB   12,"*","/","&","'","(",")","~"
               DB   "0","","_",0,0,0,0,0
               DB   199,":",";","^",0,0,96,cr
               DB   253,".",",",0,0,0,0," "-1
               DB   0,0,0,19,18,20,21,0
;
          ctrl:DB   213,212,211,150,131,152,154,0
               DB   216,215,214,135,134,132,147,129
               DB   219,218,217,148,146,133,151,145
               DB   1,0,254,0,0,0,0,0
               DB   12,0,0,0,0,0,0,0
               DB   210,0,0,153,149,137,143,144
               DB   0,0,0,136,138,139,140,cr
               DB   252,0,0,130,142,141,0," "-1
               DB   0,0,0,19,18,20,21,0
;
          edit:DB   0,0,0,0,163,184,186,0
               DB   0,0,0,0,166,164,179,161
               DB   0,0,0,0,178,165,0,0
               DB   1,0,255,0,0,0,0,0
               DB   251,0,0,0,0,0,0,0
               DB   0,0,0,185,0,169,0,176
               DB   0,0,0,168,0,0,172,cr
               DB   253,0,0,162,0,173,0," "-1
               DB   0,0,0,0,0,0,0,0
;
;
; C H A R A C T E R
;
; The character in A is printed to the current position
; held in xpos/ypos and the new position is stored back
; there. Out of range characters are printed as '.'
; The real HMPR value is stored to prevent it being
; changed when GET_BYTE/PUT_BYTE is using it - it gives
; us a chance to ensure the mode 3 palette colours are
; correct. To allow different colours, the character dat
; is ANDed and then ORed with values depending on what
; the flags say the colours should be (inverse etc.)
;
     print_chr:PUSH BC
               PUSH DE
               PUSH HL
; Out of range chars give '.'
               LD   E,46
               CP   " "
               JR   C,pr_dot
               CP   128
               JR   NC,pr_dot
               LD   E,A
        pr_dot:LD   D,0
;
               LD   HL,(xpos)
               SLA  H
               RL   H
               RR   L
               LD   BC,16384
               ADD  HL,BC
               EX   DE,HL
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,HL
               LD   BC,monitor_font-512
               ADD  HL,BC
               LD   A,(IY+our_vmpr)
               INC  A
               AND  %00011111
               OR   (IY+mode3_bits)
               OUT  (hmpr),A
               BIT  1,(IY+misc_flags)
               LD   BC,65280+%10101010
               JR   NZ,masks_done
               BIT  4,(IY+misc_flags)
               JR   Z,norm_print_chr
               LD   BC,21760
    masks_done:LD   A,8
               EX   DE,HL
          prlp:EX   AF,AF'
               LD   A,(DE)
               AND  B
               OR   C
               LD   (HL),A
               INC  DE
               INC  L
               LD   A,(DE)
               AND  B
               OR   C
               LD   (HL),A
               INC  DE
               LD   A,L
               ADD  A,127
               LD   L,A
               LD   A,H
               ADC  A,0
               LD   H,A
               EX   AF,AF'
               DEC  A
               JR   NZ,prlp
               CALL pforw
               POP  HL
               POP  DE
               POP  BC
               RET
;
norm_print_chr:LD   B,4
    norm_pr_lp:LDI
               LDI
               DEC  DE
               DEC  E
               SET  7,E
               LDI
               LDI
               DEC  DE
               DEC  E
               RES  7,E
               INC  D
               DJNZ norm_pr_lp
               CALL pforw
               POP  HL
               POP  DE
               POP  BC
               RET

;
; Move to next:character position, maybe down a line too
;
         pforw:LD   A,(xpos)
               ADD  A,4
               LD   (xpos),A
               RET  NC
               XOR  A
               LD   (xpos),A
         pdown:LD   A,(ypos)
               INC  A
               LD   (ypos),A
               CP   23
               RET  NZ
               LD   A,22
               LD   (ypos),A
               RET
         pback:LD   A,(xpos)
               SUB  4
               LD   (xpos),A
               RET  NC
               LD   A,252
               LD   (xpos),A
           pup:LD   A,(ypos)
               SUB  4
               LD   (ypos),A
               RET  NZ
               LD   A,4
               LD   (ypos),A
               RET
;
;
; M E M O R Y   M A N A G E M E N T
;
; debugging nightmare ahoy...
;
; To keep the memory access times down, there are very
; few decisions made about how to get at the required
; bytes - only to work out which 16K block the byte lies
; in. Instead, the decisions are made when the paging
; ports are written to, and the address of the code for
; accessing the locations are patched into the get/put
; code. This is bad coding, but necessary for speed.
; A 32 bit address bus would be nice, to allow the memor
; to be paged in at all times... like PC (etc) Emulators
; When fetching more than one byte, the code has to test
; for blocks of bytes lying across 16K boundries - this
; takes a bit longer, but happens so infrequently that
; only speed critical loops across them will suffer.
;
; *** GET_BYTES now stored as RST 16 for speed reasons
;
   get_ram_a_b:SET  7,H
   get_ram_c_d:OUT  (hmpr),A
               XOR  A
   get_more_lp:LDI
               RET  PO
               CP   L
               JP   NZ,get_more_lp
               LD   A,H
               AND  %00111111
               JP   NZ,get_more_lp
               EX   AF,AF'
               INC  A
               LD   H,A
               JP   next_block
;
      get_rom1:LD   A,255
               OUT  (lmpr),A
more_rom1_loop:LDI
               JP   PO,rom1_done
               BIT  7,H
               JP   NZ,more_rom1_loop
   rom1_patch2:LD   A,255
               OUT  (lmpr),A
   patch_lmpr2:LD   A,255
     patch_a_2:JP   0
     rom1_done:LD   A,255
               OUT  (lmpr),A
               RET
;
      get_rom0:DI
   rom0_patch1:LD   A,255
               OUT  (hmpr),A
   rom0_patch2:LD   A,255
               JP   section_c+rom0_read
     rom0_read:OUT  (lmpr),A
               SET  7,D
more_rom0_loop:LDI
               JP   PO,section_c+rom0_done
               BIT  6,H
               JP   Z,section_c+more_rom0_loop
               RES  7,D
   rom0_patch3:LD   A,255
               OUT  (lmpr),A
               EI
   patch_lmpr3:LD   A,255
               JP   get_ram_a_b
     rom0_done:LD   A,255
               OUT  (lmpr),A
               EI
               RET
;
; The PUT_BYTE(S) code works on the same principal as th
; GET code. Writing to write protected areas is slightly
; faster than RAM areas since only the pointers are
; changed. Except for Speccy ROM, who does this anyway?
;
;
      put_byte:LD   C,1
     put_bytes:LD   B,0
     put_block:LD   A,D
               EX   AF,AF'
  p_next_block:BIT  7,D
               JR   Z,p_block_a_b
               BIT  6,D
  p_patch_hmpr:LD   A,255
p_patch_addr_c:JP   Z,put_ram_c_d
p_patch_addr_d:JP   0
   p_block_a_b:BIT  6,D
  p_patch_lmpr:LD   A,255
p_patch_addr_b:JP   NZ,put_ram_a_b
p_patch_addr_a:JP   0
;
   put_ram_a_b:SET  7,D
   put_ram_c_d:OUT  (hmpr),A
               XOR  A
   put_more_lp:LDI
               RET  PO
               CP   E
               JP   NZ,put_more_lp
               LD   A,D
               AND  %00111111
               JP   NZ,put_more_lp
               EX   AF,AF'
               INC  A
               LD   D,A
               JP   p_next_block
;
      put_rom1:XOR  A
   put_rom1_lp:INC  DE
               CPI
               RET  PO
               CP   D
               JP   NZ,put_rom1_lp
               JP   p_patch_lmpr
;
    put_prot_b:LD   A,128
  put_protb_lp:INC  DE
               CPI
               RET  PO
               CP   D
               JP   NZ,put_protb_lp
               JP   p_patch_hmpr
;
    put_prot_c:LD   A,192
  put_protc_lp:INC  DE
               CPI
               RET  PO
               CP   D
               JP   NZ,put_protc_lp
               OR   255
               JP   p_patch_hmpr
;
      put_rom0:LD   A,64
   put_rom0_lp:INC  DE
               CPI
               RET  PO
               CP   D
               JP   NZ,put_rom0_lp
               OR   255
               JP   p_patch_lmpr
;
;
; The following patching code looks at the values in the
; paging ports and interts the correct routine address i
; the GET/PUT code for each block. ie. if ROM0 is paged
; in then the handling routine for block A will deal wit
; the ROM otherwise it will deal with the RAM currently
; paged in there (LMPR). Writing to this block also has
; to handle the write protection that could be set.
;
; PATCH_MEM_INIT is only called when the monitor is
; entered. It patches the code where references to the
; monitors own page are used (when ROM0 is read, the
; monitor is temporarily paged in at block C). This code
; runs on into the normal patching code which sets thing
; up as normal.
;
patch_mem_init:IN   A,(lmpr)
               LD   B,A
               OR   %00100000
               LD   (rom0_patch3+1),A
               LD   (rom0_done+1),A
               AND  %11011111
               LD   (rom0_patch2+1),A
               OR   %01100000
               LD   (get_rom1+1),A
               LD   A,B
               LD   (rom1_patch2+1),A
               LD   (rom1_done+1),A
;
; PATCH_PORTS is called after changes to the paging
; port values and also when the extra mode 3 bits are
; changed in the HMPR or the border is changed. The
; latter is done so that the border colour is unaffected
; when the monitor own screen is viewed. This keeps the
; other screen completely seperate. :-)
;
   patch_ports:XOR  A
               LD   B,A
               BIT  2,(IY+misc_flags)
               JR   Z,normal_view
               LD   B,(IY+port_border)
               LD   A,(IY+port_hmpr)
               AND  %01100000
   normal_view:LD   C,A
               LD   (IY+mode3_bits),A
               LD   (IY+border_colour),B
               LD   A,(IY+port_hmpr)
               AND  %00011111
               OR   C
               LD   (patch_hmpr+1),A
               LD   (p_patch_hmpr+1),A
               IN   A,(lmpr)
               AND  %00011111
               OR   C
               LD   (rom0_patch1+1),A
; (original end to patch_HMPR and start of patch_LMPR)
               LD   A,(IY+port_lmpr)
               LD   B,A
               AND  %00011111
               OR   C
               LD   (p_patch_lmpr+1),A
               LD   (patch_lmpr+1),A
               LD   (patch_lmpr2+1),A
               LD   (patch_lmpr3+1),A
;
               IN   A,(lmpr)
               AND  %00011111
               LD   C,A
               LD   A,B
               AND  %00011111
               LD   B,A
;
               BIT  5,(IY+port_lmpr)
               LD   HL,get_rom0
               LD   DE,put_rom0
; Jump if ROM 0 is paged in
               JR   Z,change_sect_a
               LD   HL,get_ram_a_b
               BIT  7,(IY+port_lmpr)
; Jump if section A is write-protected
               JR   NZ,change_sect_a
               SUB  C
               LD   DE,put_ram_a_b
               JR   C,change_sect_a
               CP   monitor_pages
               JR   NC,change_sect_a
; Section A holds a TurboMON page so protect it
               LD   DE,put_rom0
 change_sect_a:LD   (patch_addr_a+1),HL
               LD   (patch_a_2+1),HL
               LD   (p_patch_addr_a+1),DE

               LD   A,B
               INC  A
               SUB  C
               LD   HL,put_ram_a_b
               JR   C,change_sect_b
               CP   monitor_pages
               JR   NC,change_sect_b
; Section B holds a TurboMON page so protect it
               LD   HL,put_prot_b
 change_sect_b:LD   (p_patch_addr_b+1),HL
;
               LD   A,(IY+port_hmpr)
               AND  %00011111
               LD   B,A
;
               SUB  C
               LD   HL,put_ram_c_d
               JR   C,change_sect_c
               CP   monitor_pages
               JR   NC,change_sect_c
; Section C holds a TurboMON page so protect it
               LD   HL,put_prot_c
 change_sect_c:LD   (p_patch_addr_c+1),HL
;
               LD   A,B
               INC  A
               BIT  6,(IY+port_lmpr)
               LD   HL,get_rom1
               LD   DE,put_rom1
; Jump if ROM 1 is paged in
               JR   NZ,change_sect_d
               LD   HL,get_ram_c_d
               LD   DE,put_ram_c_d
               SUB  C
               JR   C,change_sect_d
               CP   monitor_pages
               JR   NC,change_sect_d
; Section D holds a TurboMON page so protect it
               LD   DE,put_rom1
 change_sect_d:LD   (patch_addr_d+1),HL
               LD   (p_patch_addr_d+1),DE
               RET

;
; Code in block CD must take care fetching/putting bytes
; since it is paged out for the operation. We must have
; special calls that page hi memory code back in after
; it's done.
; If code in hi memory is called, we must make sure that
; it is paged in before hand.
; Code in section D cannot be self modifying when using
; get/put_bytes and must use this buffer instead.
   get_put_tmp:DS   20
;
;
get_bytes_safe:LD   (mini_stack),SP
               DI
               LD   SP,mini_stack
               RST  16
               POP  HL
               LD   SP,HL
               EI
               JP   page_in_screen
;
put_bytes_safe:LD   (mini_stack),SP
               DI
               LD   SP,mini_stack
               CALL put_bytes
               POP  HL
               LD   SP,HL
               EI
               JP   page_in_screen
;
put_block_safe:LD   (mini_stack),SP
               DI
               LD   SP,mini_stack
               CALL put_block
               POP  HL
               LD   SP,HL
               EI
               JP   page_in_screen

               DS   4
    mini_stack:DW   0

  input_number:CALL page_in_screen
               JP   do_input_num
;
  input_num_at:CALL page_in_screen
               JP   do_input_at
;
      txt_edit:CALL page_in_screen
               JP   do_txt_edit
;
      num_edit:CALL page_in_screen
               JP   do_num_edit
;
     mem_graph:CALL page_in_screen
               JP   do_mem_graph
;
    state_info:CALL page_in_screen
               JP   do_state_info
;
   fill_region:CALL page_in_screen
               JP   do_fill_region
;
    copy_block:CALL page_in_screen
               JP   do_copy_block
;
 search_memory:CALL page_in_screen
               JP   do_mem_search


;
;
; P O R T   H A N D L I N G
;
; The port handling code deals with any read of and
; writing to the ports as done by the IN/OUT opcodes.
; Some of the ports are act differently when read from
; than when written to, so different variables are used.
;
; PORTS:
;
;  224 to 231 Disc 1  R    All disc ports read as normal
;  224 to 231 Disc 1  W
;  232 to 247 Disc 2  R
;  232 to 247 Disc 2  W
;  248/#F8  PENS      R    Read as normal
;  248/#F8  PALETTE   W    Fully emulated
;  249/#F9  STATUS    R    Interrupt bits emulated but
;                          last until next EI.
;  249/#F9  LINE_INT  W    If < 192 interrupt will occur
;                          but not at specified line.
;  250/#FA  LMPR      R/W  Fully emulated
;  251/#FB  HMPR      R/W  Fully emulated except bit 7
;  252/#FC  VMPR      R/W  Fully emulated except midi
;                          sent and read as 0
;  253/#FD  MIDI      R/W  Input and output ignored
;  254/#FE  KEYBOARD       Read as normal
;  254/#FE  BORDER    W    Fully emulated except bit 6
;  255/#FF  SOUND     W    Written as normal
;  255/#FF  ATTR      R    Read as 0 if viewing panel
;                          screen, otherwise as normal.
;
;
; READ_PORT - Read from port held in BC and return the
;             byte in A. The code also handles the
;             execute condition set as "Stop if reading
;             from a certain port"
;
     read_port:LD   A,C
   input_patch:CP   0
; Jump if not reading from the execute condition port
               JR   NZ,no_input_scan
               BIT  3,(IY+cpu_flags)
; Jump if we're not interested in stopping
               JR   Z,no_input_scan
               BIT  0,(IY+cpu_flags)
; Jump if we're not executing properly
               JR   Z,no_input_scan
; Time to stop!
; Junk return to instruction execution
               POP  HL
; ... and fetch PC in HL
               POP  HL
; Jump to signal stop
               JP   execute_break
 no_input_scan:CP   keyboard
               JR   NZ,r_hmpr
               IN   A,(C)
               RET
        r_hmpr:CP   hmpr
               JR   NZ,r_lmpr
               LD   A,(IY+port_hmpr)
               RET
        r_lmpr:CP   lmpr
               JR   NZ,r_vmpr
               LD   A,(IY+port_lmpr)
               RET
        r_vmpr:CP   vmpr
               JR   NZ,r_status
               LD   A,(IY+port_vmpr)
               RET
      r_status:CP   status
               JR   NZ,r_kempston
               IN   A,(C)
               AND  %11100000
               LD   B,A
               LD   A,(IY+port_status)
               AND  %00011111
               OR   B
               RET
    r_kempston:CP   kempston
               JR   NZ,r_attr
               LD   A,239
               IN   A,(keyboard)
               LD   C,A
               XOR  A
               RR   C
               JR   C,kemp_not_fire
               OR   kempston_fire
 kemp_not_fire:RR   C
               JR   C,kemp_not_up
               OR   kempston_up
   kemp_not_up:RR   C
               JR   C,kemp_not_down
               OR   kempston_down
 kemp_not_down:RR   C
               JR   C,kemp_not_right
               OR   kempston_right
kemp_not_right:RR   C
               RET  C
               OR   kempston_left
               RET
        r_attr:CP   attributes
               JR   NZ,r_pens
               XOR  A
               BIT  2,(IY+misc_flags)
               RET  Z
               IN   A,(C)
               RET
        r_pens:CP   pens
               JR   NZ,r_midi
               IN   A,(C)
               BIT  0,B
               RET  NZ
               AND  %11111101
               RET
        r_midi:CP   midi
               JR   NZ,r_rest
               XOR  A
               RET
        r_rest:IN   A,(C)
               RET
;
; WRITE_PORT - Write the byte in A to port held in BC.
;              The ports may need patching and the scree
;              updating depending on which port is used.
;
    write_port:LD   E,A
               LD   A,C
  output_patch:CP   0
; Jump if not writing to the execute condition port
               JR   NZ,no_output_scan
               BIT  4,(IY+cpu_flags)
; Jump if we're not interested in stopping
               JR   Z,no_output_scan
               BIT  0,(IY+cpu_flags)
; Jump if we're not executing properly
               JR   Z,no_output_scan
; Time to stop!
; Junk the return to instruction execution
               POP  HL
; ... and fetch PC in HL
               POP  HL
; Jump to signal stop
               JP   execute_break
no_output_scan:CP   border
               JR   NZ,w_hmpr
               LD   A,E
  write_border:AND  %10111111
               LD   (IY+port_border),A
               BIT  2,(IY+misc_flags)
               RET  Z
               OUT  (border),A
               RET
        w_hmpr:CP   hmpr
               JR   NZ,w_lmpr
               RES  7,E
               LD   (IY+port_hmpr),E
               JP   redo_port_scr
        w_lmpr:CP   lmpr
               JR   NZ,w_vmpr
               LD   (IY+port_lmpr),E
               JP   patch_ports
        w_vmpr:CP   vmpr
               JR   NZ,w_palette
               RES  7,E
               LD   (IY+port_vmpr),E
               JP   update_screen
     w_palette:CP   clut
               JR   NZ,w_sound
               LD   HL,mon_palette
               LD   A,B
               AND  %00001111
               ADD  A,L
               LD   L,A
               JR   NC,no_pal_spill
               INC  H
  no_pal_spill:RES  7,E
               LD   (HL),E
               BIT  2,(IY+misc_flags)
               RET  Z
               OUT  (C),E
               RET
       w_sound:CP   sound
               JR   NZ,w_line_int
               OUT  (C),E
               RET
    w_line_int:CP   line_int
               JR   NZ,w_midi
               LD   (IY+port_line_int),E
               RET
        w_midi:CP   midi
               RET  Z
        w_rest:OUT  (C),E
               RET
;
;
; REDO_PORT_SCR - Update ports and screen stuff
;
 redo_port_scr:CALL patch_ports
;           ...run on into update_screen
;
; UPDATE_SCREEN - Show the relevant screen and palette
;
 update_screen:LD   A,(IY+port_vmpr)
               LD   HL,mon_palette+15
               BIT  2,(IY+misc_flags)
               JR   NZ,set_screen
               LD   A,(IY+our_vmpr)
               LD   HL,our_palette+15
               BIT  3,(IY+misc_flags)
               JR   Z,set_screen
               LD   HL,our_palette+19
    set_screen:OUT  (vmpr),A
               LD   A,(IY+border_colour)
               OUT  (border),A
               IN   A,(hmpr)
               AND  %00011111
               OR   (IY+mode3_bits)
               OUT  (hmpr),A
               LD   B,16
               LD   C,248
               OTDR
               RET

;
; M E S S A G E   P R I N T I N G
;
; PRINT_AT_POP - Print message at coords and message are
;                found after call instruction.
;
  print_at_pop:POP  HL
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               INC  HL
               LD   (xpos),DE
               CALL print_scr_msg
               JP   (HL)
;
; Print message at return address to the current pos.
 print_scr_pop:POP  HL
               CALL print_scr_msg
               JP   (HL)
;
; Print message at return address to buffer
 print_buf_pop:POP  HL
               CALL print_buf_msg
               JP   (HL)
;
; Print message HL points to to the screen OR X,Y in DE
  print_msg_at:LD   (xpos),DE
 print_scr_msg:LD   A,(HL)
               AND  127
               RST  8
               BIT  7,(HL)
               INC  HL
               JR   Z,print_scr_msg
               RET
;
; Print message HL points to to the buffer HL' points to
 print_buf_msg:LD   A,(HL)
               AND  127
               EXX
               LD   (HL),A
               INC  HL
               EXX
               BIT  7,(HL)
               INC  HL
               JR   Z,print_buf_msg
               RET
;
; Print the Ath item of the list HL points to to buffer
print_buf_item:CALL get_item
               JP   print_buf_msg
;
; Find Ath item in list HL points to. Bit 7 set = end
      get_item:AND  A
               RET  Z
               LD   B,A
     find_item:BIT  7,(HL)
               INC  HL
               JP   Z,find_item
               DJNZ find_item
               RET
;
; Print character in A to text buffer in HL'
   print_a_buf:EXX
               LD   (HL),A
               INC  HL
               EXX
               RET
;
; R E G I S T E R   P A N E L
;
; SHOW_REG_NAME / SHOW_REG_VALS
;
; Show the names of all the registers. This only needs
; doing once since it's never overwritten.
;
show_reg_names:DI
               SET  4,(IY+misc_flags)
               CALL print_at_pop
               DB   180,2,"B","C"+128
               CALL print_at_pop
               DB   220,2,"B","C","'"+128
               CALL print_at_pop
               DB   180,3,"D","E"+128
               CALL print_at_pop
               DB   220,3,"D","E","'"+128
               CALL print_at_pop
               DB   180,4,"H","L"+128
               CALL print_at_pop
               DB   220,4,"H","L","'"+128
               CALL print_at_pop
               DB   180,12,"I","X"+128
               CALL print_at_pop
               DB   224,12,"I","Y"+128
               CALL print_at_pop
               DB   180,13,"P","C"+128
               CALL print_at_pop
               DB   224,13,"S","P"+128
               CALL print_at_pop
               DB   180,6,"(","B","C",")"+128
               CALL print_at_pop
               DB   198,6,"(","D","E",")"+128
               CALL print_at_pop
               DB   216,6,"(","H","L",")"+128
               CALL print_at_pop
               DB   236,6,"(","H","L","'",")"+128
               CALL print_at_pop
               DB   180,9,"A","F"+128
               CALL print_at_pop
               DB   180,10,"A","F","'"+128
               CALL print_at_pop
               DB   208,15,"S","t","a","c","k",":"+128
               CALL print_at_pop
               DB   180,16,"A"+128
               CALL print_at_pop
               DB   180,17,"B"+128
               CALL print_at_pop
               DB   208,17,"C"+128
               CALL print_at_pop
               DB   180,18,"D"+128
               CALL print_at_pop
               DB   208,18,"E"+128
               CALL print_at_pop
               DB   180,19,"H"+128
               CALL print_at_pop
               DB   208,19,"L"+128
               CALL print_at_pop
               DB   180,21,"I"+128
               CALL print_at_pop
               DB   208,21,"R"+128
               CALL print_at_pop
               DB   180,22,"I","M"+128
               RES  4,(IY+misc_flags)
               EI
               RET
;
; Show the contents of all the registers (including flag
; and indirect pointer values)
;
 show_reg_vals:LD   HL,(reg_c)
               LD   DE,&02C0
               CALL print_word_at
               LD   HL,(reg_e)
               LD   DE,&03C0
               CALL print_word_at
               LD   HL,(reg_l)
               LD   DE,&04C0
               CALL print_word_at
               LD   HL,(reg_alt_c)
               LD   DE,&02EC
               CALL print_word_at
               LD   HL,(reg_alt_e)
               LD   DE,&03EC
               CALL print_word_at
               LD   HL,(reg_alt_l)
               LD   DE,&04EC
               CALL print_word_at
               LD   HL,(reg_ixl)
               LD   DE,&0CC0
               CALL print_word_at
               LD   HL,(reg_iyl)
               LD   DE,&0CEC
               CALL print_word_at
               LD   HL,(reg_pcl)
               LD   DE,&0DC0
               CALL print_word_at
               LD   HL,(reg_spl)
               LD   DE,&0DEC
               CALL print_word_at
               LD   HL,(reg_flags)
               LD   DE,&09C0
               CALL print_word_at
               LD   HL,(reg_alt_flags)
               LD   DE,&0AC0
               CALL print_word_at
               LD   A,(reg_a)
               LD   DE,&10BC
               CALL print_byte_at
               LD   A,(reg_b)
               LD   DE,&11BC
               CALL print_byte_at
               LD   A,(reg_c)
               LD   DE,&11D8
               CALL print_byte_at
               LD   A,(reg_d)
               LD   DE,&12BC
               CALL print_byte_at
               LD   A,(reg_e)
               LD   DE,&12D8
               CALL print_byte_at
               LD   A,(reg_h)
               LD   DE,&13BC
               CALL print_byte_at
               LD   A,(reg_l)
               LD   DE,&13D8
               CALL print_byte_at
               LD   A,(reg_int_vector)
               LD   DE,&15BC
               CALL print_byte_at
               LD   A,(reg_refresh)
               LD   DE,&15D8
               CALL print_byte_at
               LD   HL,&16C4
               LD   (xpos),HL
               LD   A,(reg_int_mode)
               ADD  A,48
               RST  8
               LD   HL,&16D0
               LD   (xpos),HL
               LD   A,(reg_iff1)
               RRCA
               RRCA
               ADD  A,"D"
               RST  8
               LD   A,"I"
               RST  8
               LD   A,(IY+int_delay)
               LD   B,A
               AND  A
               LD   A,"+"
               JR   Z,got_int_sym
               INC  B
               LD   A,"-"
               JR   Z,got_int_sym
               LD   A," "
   got_int_sym:RST  8
               LD   A," "
               RST  8
               LD   A,(IY+line_ints)
               AND  A
               LD   A," "
               JR   Z,no_line_ints
               LD   A,"L"
  no_line_ints:RST  8
               LD   HL,(reg_spl)
               LD   DE,work_space
               LD   BC,16
               PUSH DE
               RST  16
               LD   HL,(work_space)
               LD   (top_of_stack),HL
               POP  HL
               LD   DE,&0FEC
               LD   BC,print_word
               LD   A,8
               CALL print_list
               LD   HL,(reg_c)
               LD   DE,&07B6
               CALL print_indirect
               LD   HL,(reg_e)
               LD   DE,&07C8
               CALL print_indirect
               LD   HL,(reg_l)
               LD   DE,&07DA
               CALL print_indirect
               LD   HL,(reg_alt_l)
               LD   DE,&07F0
               CALL print_indirect
               LD   A,(reg_flags)
               LD   DE,&09E0
               CALL print_flags
               LD   A,(reg_alt_flags)
               LD   DE,&0AE0
               JP   print_flags
;
   print_flags:LD   B,A
               LD   (xpos),DE
               LD   A,"S"
               BIT  7,B
               JR   NZ,flag_neg
               OR   %00100000
      flag_neg:RST  8
               LD   A,"Z"
               BIT  6,B
               JR   NZ,flag_zero
               OR   %00100000
     flag_zero:RST  8
               LD   A,"-"
               BIT  5,B
               JR   Z,reset_5
               LD   A,"+"
       reset_5:RST  8
               LD   A,"H"
               BIT  4,B
               JR   NZ,flag_half
               OR   %00100000
     flag_half:RST  8
               LD   A,"-"
               BIT  3,B
               JR   Z,reset_3
               LD   A,"+"
       reset_3:RST  8
               LD   A,"E"
               BIT  2,B
               JR   NZ,flag__pe
               LD   A,"o"
      flag__pe:RST  8
               LD   A,"N"
               BIT  1,B
               JR   NZ,flag_n
               OR   %00100000
        flag_n:RST  8
               LD   A,"C"
               BIT  0,B
               JR   NZ,flag__c
               OR   %00100000
       flag__c:JP   pr_chr
;
print_indirect:PUSH DE
               LD   DE,work_space
               LD   BC,1
               RST  16
               POP  DE
               LD   A,(work_space)
               JP   print_byte_at
;
    print_list:LD   (list_printer+1),BC
               LD   B,A
       list_lp:LD   (xpos),DE
               PUSH DE
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               INC  HL
               PUSH HL
               EX   DE,HL
               PUSH BC
  list_printer:CALL 0
               POP  BC
               POP  HL
               POP  DE
               INC  D
               DJNZ list_lp
               RET
;
; Useful value to store rather than refetch from memory
  top_of_stack:DW   0
;
;
; N U M B E R   P R I N T I N G
;
; If bit 0 of (IY+number_base) is 0 we print in decimal
; otherwise we use hex.
;
; PRINT_WORD_AT - Print word in HL at coords in DE.
; PRINT_WORD    - Print word in HL at current position.
;
 print_word_at:LD   (xpos),DE
    print_word:BIT  0,(IY+number_base)
               JR   Z,decimal_word
print_hex_word:LD   A,"#"
               RST  8
               LD   A,H
               CALL dohex
               LD   A,D
               RST  8
               LD   A,E
               RST  8
               LD   A,L
               CALL dohex
               LD   A,D
               RST  8
               LD   A,E
               JP   pr_chr
;
  decimal_word:LD   DE,10000
               CALL nbt
               LD   DE,1000
               CALL nbt
         sdeci:LD   DE,100
               CALL nbt
     tens_byte:LD   DE,10
               CALL nbt
               LD   A,L
               ADD  A,"0"
               JP   pr_chr
           nbt:LD   A,47
          btlp:ADD  A,1
               SBC  HL,DE
               JR   NC,btlp
               ADD  HL,DE
               JP   pr_chr
;
; PRINT_BYTE_AT - Print the byte in A at coords in DE.
; PRINT_BYTE    - Print the byte in A at current positio
;
 print_byte_at:LD   (xpos),DE
    print_byte:BIT  0,(IY+number_base)
               LD   L,A
               LD   H,0
               JR   Z,sdeci
print_hex_byte:CALL dohex
               LD   A,"#"
               RST  8
               LD   A,D
               RST  8
               LD   A,E
               JP   pr_chr
;
         dohex:LD   B,A
               RRA
               RRA
               RRA
               RRA
               AND  15
               CP   10
               JR   C,nlet1
               ADD  A,7
         nlet1:ADD  A,"0"
               LD   D,A
               LD   A,B
               AND  15
               ADD  A,"0"
               CP   58
               LD   E,A
               RET  C
               ADD  A,7
               LD   E,A
               RET
;
; Print A in binary
  print_binary:LD   C,A
               LD   B,8
    bin_bit_lp:XOR  A
               RL   C
               ADC  A,48
               RST  8
               DJNZ bin_bit_lp
               RET
;
; Scroll main part of screen up/down (not panel)
;
   scroll_down:CALL page_in_screen
               LD   HL,section_b+3072
               LD   DE,section_b+2048
               LD   A,152
  scr_down_lp1:LD   BC,&04FF
  scr_down_lp2:DW   &A0ED,&A0ED,&A0ED,&A0ED,&A0ED,&A0ED
               DW   &A0ED,&A0ED,&A0ED,&A0ED,&A0ED,&A0ED
               DW   &A0ED,&A0ED,&A0ED,&A0ED,&A0ED,&A0ED
               DW   &A0ED,&A0ED,&A0ED,&A0ED
               DJNZ scr_down_lp2
               LD   BC,128-88
               ADD  HL,BC
               EX   DE,HL
               ADD  HL,BC
               EX   DE,HL
               DEC  A
               JR   NZ,scr_down_lp1
               RET
;
     scroll_up:CALL page_in_screen
               LD   HL,section_b+mode4_length-3200
               LD   DE,section_b+mode4_length-2176
               LD   A,152
    scr_up_lp1:LD   BC,&04FF
    scr_up_lp2:DW   &A0ED,&A0ED,&A0ED,&A0ED,&A0ED,&A0ED
               DW   &A0ED,&A0ED,&A0ED,&A0ED,&A0ED,&A0ED
               DW   &A0ED,&A0ED,&A0ED,&A0ED,&A0ED,&A0ED
               DW   &A0ED,&A0ED,&A0ED,&A0ED
               DJNZ scr_up_lp2
               LD   BC,0-128-88
               ADD  HL,BC
               EX   DE,HL
               ADD  HL,BC
               EX   DE,HL
               DEC  A
               JR   NZ,scr_up_lp1
               RET
;
; Clear main part of screen (not register panel)
clear_main_scr:CALL page_in_screen
               LD   HL,section_b+1024
               LD   A,184
               LD   B,L
  clear_scr_lp:LD   D,H
               LD   E,L
               INC  DE
               LD   C,89
               LD   (HL),B
               LDIR
               LD   DE,39
               ADD  HL,DE
               DEC  A
               JR   NZ,clear_scr_lp
               RET
;
; Page screen into section C (already in B)
page_in_screen:PUSH AF
               LD   A,(IY+our_vmpr)
               INC  A
               AND  %00011111
               OR   (IY+mode3_bits)
               OUT  (hmpr),A
               POP  AF
               RET

;
; Print the cursor used by number input and editing
;
   cursor_xpos:DB   0
   cursor_ypos:DB   0

  print_cursor:CALL page_in_screen
               LD   HL,(cursor_xpos)
               SLA  H
               RL   H
               RR   L
               LD   BC,16383
               ADD  HL,BC
               CALL mode4_pix_up
               LD   B,9
   cursor_loop:PUSH HL
               LD   A,(HL)
               XOR  %00000011
               LD   (HL),A
               INC  L
               LD   A,(HL)
               CPL
               LD   (HL),A
               INC  L
               LD   A,(HL)
               CPL
               LD   (HL),A
               INC  L
               LD   A,(HL)
               XOR  %11000000
               LD   (HL),A
               POP  HL
               CALL mode4_pix_down
               DJNZ cursor_loop
               RET

;
; Pixel up/down for different modes
;
mode4_pix_down:LD   A,L
               ADD  A,128
               LD   L,A
               RET  NC
               INC  H
               RET
;
  mode4_pix_up:LD   A,L
               SUB  128
               LD   L,A
               RET  NC
               DEC  H
               RET
