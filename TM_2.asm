;
; R E S T A R T   C O D E   +   V A R I A B L E S
;
      put_base:EQU  section_c
 monitor_pages:EQU  3
  monitor_font:EQU  section_d-1536
        pr_chr:EQU  8
     get_bytes:EQU  16
   single_step:EQU  32
     interrupt:EQU  56
           nmi:EQU  102
;
; Entry point from BASIC.
;
               ORG  0
               DUMP put_base+0
               autoexec

; End marker for SC_ASSEMBLER - two RST 56 instructions!
      relocate:DB   255,255
;
               LD   BC,configure-relocate
               ADD  HL,BC
               JP   (HL)

               DS   1

;
; Print the character in A
;
               JP   print_chr

; Save space by slotting the printing coords in here
          xpos:DB   0
          ypos:DB   0

               DS   3

;
; Decision making header for GET_BYTES. RST call is
; faster than a conventional CALL instruction.
;
               LD   A,H
               EX   AF,AF'
    next_block:BIT  7,H
               JR   Z,block_a_b
               BIT  6,H
    patch_hmpr:LD   A,255
               JP   Z,get_ram_c_d
  patch_addr_d:JP   0

;
; SINGLE_STEP - execute 1 instruction (PC in HL)
;               Nice fast RST call too :-)
;
               PUSH HL
               LD   DE,instr
               LD   BC,3
; Fetch...
               RST  16
;
               LD   A,(instr)
               LD   L,A
               LD   H,norm_div_2
               ADD  HL,HL
               LD   E,(HL)
               INC  L
; Decode...
               LD   H,(HL)
               LD   L,E
; Execute!
               JP   (HL)

;
; Drop out into ROM for return to BASIC.
;
      page_out:EI
; Return to BASIC
               OUT  (lmpr),A

               DS   1

;
; Maskable interrupt to read keys and trigger interrupts
; when executing. The keys are not read when executing.
;
               PUSH AF
               IN   A,(status)
               BIT  3,A
; Forget interrupt if this isn't a frame interrupt
               JR   NZ,forget_int
; Check to see if we want to stop executing
               CALL power_stop
               BIT  0,(IY+cpu_flags)
               JR   NZ,exec_check
    normal_int:PUSH BC
               PUSH DE
               PUSH HL
               PUSH IX
               EX   AF,AF'
               PUSH AF
               CALL read_keyboard
               CALL hot_keys
               POP  AF
               EX   AF,AF'
               POP  IX
               POP  HL
               POP  DE
               POP  BC
    forget_int:POP  AF
               EI
               RETI

               DS   4

;
; This pages in ROM 0 and drops into the NMI code
;
      nmi_exit:OUT  (lmpr),A

;
; Non-maskable interrupt - alternative execute break
; If pressed when ROM0 is paged in it will use the BASIC
; handler. The monitor will freeze if re-entered!
;
               CALL execute_break
               EI
               RET

;
; Rest of GET_BYTES code that couldn' fit in RST area
     block_a_b:BIT  6,H
    patch_lmpr:LD   A,255
               JP   NZ,get_ram_a_b
  patch_addr_a:JP   0
;
    power_stop:LD   A,255
               IN   A,(keyboard)
               RRA
               RET  C
               LD   A,251
               IN   A,(status)
               AND  %01000000
               RET  NZ
; Pressing CTRL-F8 to signal to stop
               JP   execute_break

;
;
; EXEC_CHECK - Check if it's time to generate another
;              interrupt ; if it is we set the EXEC_STOP
;              bit which defaults to cause an interrupt.
;
    exec_check:LD   A,(IY+int_count)
               INC  A
               CP   2
; Jump if no interrupts allowed
               JR   C,forget_int
               SUB  2
               LD   (IY+int_count),A
; No interrupt... yet
               JR   NZ,forget_int
               SET  1,(IY+cpu_flags)
               LD   A,(IY+int_delay)
               LD   (IY+int_count),A
               JR   forget_int

;
; Find the page we are running in and page us in at 0.
;
     configure:DI
               IN   A,(hmpr)
               AND  %00011111
               LD   B,A
               IN   A,(lmpr)
               LD   C,A
               LD   A,H
               SUB  192
               CCF
               LD   A,B
; Turn ROM0 off
               ADC  A,rom0_bitval
; Page us in at 0
               OUT  (lmpr),A
; Jump into version in page A
               JP   page_a
;
        page_a:LD   (storestack+1),SP
; Newstack is before the decode tables
               LD   SP,decode_tables
               LD   A,C
               PUSH AF
               IN   A,(vmpr)
               PUSH AF
               AND  %01111111
               LD   (basic_screen+1),A
               LD   A,B
               ADD  A,2
               LD   (h_page+1),A
               OUT  (hmpr),A
               DEC  A
               OR   %01000000
               LD   (panel_screen+1),A
; RUN MONITOR PROGRAM!
               CALL do_stuff
               LD   IX,page_out
;
  quit_prepare:DI
        h_page:LD   A,0
               OUT  (hmpr),A
               LD   HL,&FFFF
; Write the end of text marker for the SC_ASSEMBLER
               LD   (section_c),HL
               XOR  A
; Page in system vars
               OUT  (hmpr),A
; Use BASIC palette
               LD   HL,basic_palette+page_length+15
               LD   B,16
               LD   C,248
               OTDR
               POP  AF
               OUT  (vmpr),A
               POP  AF
    storestack:LD   SP,0
               JP   (IX)
;
; Prepare vars and then jump into main menu
      do_stuff:LD   IY,variables
               BIT  7,(IY+misc_flags)
               SET  7,(IY+misc_flags)
               JR   NZ,run_before
               CALL move_8k_code
               CALL prepare_basic
    run_before:BIT  7,(IY+misc_flags)
; Return if same version of TurboMON is running this!!!
               RET  Z
               RES  0,(IY+cpu_flags)
               RES  1,(IY+misc_flags)
               LD   HL,section_b
               LD   DE,section_b+1
               LD   BC,mode3_length-1
               LD   (HL),L
; CLS!
               LDIR
  panel_screen:LD   A,0
               OUT  (vmpr),A
               LD   BC,sound+256
               LD   A,28
               LD   (toggle_sound+1),A
               OUT  (C),A
               XOR  A
               LD   B,A
; Sound chip off
               OUT  (C),A
               RES  2,(IY+misc_flags)
               LD   (IY+refresh_flags),255
               IN   A,(vmpr)
               LD   (IY+our_vmpr),A
               SET  0,(IY+misc_flags)
               CALL patch_mem_init
               CALL update_screen
               CALL show_reg_names
               EI
               JP   main_menu
;
;
; Variables base (holding default values)
;
     variables:DB   &1F,&C0,31,1,94,0,0,0,0,0,0,0,0,0,0
               DB   0,0,0,0,0
; Entry point from BASIC.
; Above shared variables displacements from base
;
   port_status:EQU  0
 port_line_int:EQU  1
     port_lmpr:EQU  2
     port_hmpr:EQU  3
     port_vmpr:EQU  4
   port_border:EQU  5
     cpu_flags:EQU  6
    misc_flags:EQU  7
      our_vmpr:EQU  8
 border_colour:EQU  9
    mode3_bits:EQU  10
           key:EQU  11
       lastkey:EQU  12
        repdel:EQU  13
   number_base:EQU  14
; 50ths before next interrupt
     int_count:EQU  15
; Start value for INT_COUNT
     int_delay:EQU  16
; Line interrupts per frame int
    line_count:EQU  17
; Start value for LINE_COUNT
     line_ints:EQU  18
 refresh_flags:EQU  19
;
;
; Palette colours
;
; Default palette reset back to on request
normal_palette:DB   &00,&10,&20,&30,&40,&50,&60,&78
               DB   &00,&11,&22,&33,&44,&55,&66,&7F
;
; Palette of monitor screen
   mon_palette:DB   &00,&10,&20,&30,&40,&50,&60,&78
               DB   &00,&11,&22,&33,&44,&55,&66,&7F
;
; Palette panel screen  (normal and inverse)
   our_palette:DB   0,16,78,127
               DB   120,31,32,0
;
; Buffer just before the HL value to speed up multiple
; PUSH HLs by putting one pair less than we need.
   mult_hl_buf:DS   4

;
; Register store to hold current CPU status
;
         reg_l:DB   0
         reg_h:DB   0
         reg_e:DB   0
         reg_d:DB   0
         reg_c:DB   0
         reg_b:DB   0
     reg_flags:DB   0
         reg_a:DB   0
       reg_spl:DB   0
       reg_sph:DB   0
       reg_ixl:DB   0
       reg_ixh:DB   0
       reg_iyl:DB   0
       reg_iyh:DB   0
 reg_alt_flags:DB   0
     reg_alt_a:DB   0
     reg_alt_c:DB   0
     reg_alt_b:DB   0
     reg_alt_e:DB   0
     reg_alt_d:DB   0
     reg_alt_l:DB   0
     reg_alt_h:DB   0
       reg_pcl:DB   0
       reg_pch:DB   0
reg_int_vector:DB   0
   reg_refresh:DB   0
      reg_iff1:DB   4
      reg_iff2:DB   0
  reg_int_mode:DB   1
;
;
; Instruction fetched for execution
         instr:DS   4
;
;
; Misc variables that need to be accessed as more than
; one byte so must be stored seperately.
;
    temp_store:DW   0
;
  repdel_short:EQU  4
   repdel_long:EQU  35
;
;
; Bit positions:within MISC_FLAGS (and bit values)
;
      caps_bit:EQU  0
   caps_bitval:EQU  1
    invert_bit:EQU  1
 invert_bitval:EQU  2
    screen_bit:EQU  2
 screen_bitval:EQU  4
  inv_cols_bit:EQU  3
    inv_bitval:EQU  8
      pen2_bit:EQU  4
   release_bit:EQU  5
 allow_wildnum:EQU  6
  sound_bitval:EQU  64
run_before_bit:EQU  7
;
; Bit positions:within CPU_FLAGS
;
     executing:EQU  0
     exec_stop:EQU  1
     nmi_break:EQU  2
    input_scan:EQU  3
   output_scan:EQU  4
;
; Bit positions within REFRESH_FLAGS
;
     top_panel:EQU  0
         panel:EQU  1
   main_screen:EQU  2
