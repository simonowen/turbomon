;
; M A I N   M E N U   and execution handling
;
     dis_lines:EQU  20
  dis_buf_size:EQU  dis_lines+dis_lines
;
         ctrl_:EQU  64
         edit_:EQU  96
;
     main_menu:CALL update_info
               BIT  2,(IY+refresh_flags)
               RES  2,(IY+refresh_flags)
               CALL NZ,show_dis_page
               CALL prmenu
               RES  1,(IY+cpu_flags)
               LD   HL,main_menu
               PUSH HL
               BIT  5,(IY+misc_flags)
               RES  5,(IY+misc_flags)
               CALL NZ,let_go
     menu_loop:HALT
               BIT  2,(IY+cpu_flags)
               RES  2,(IY+cpu_flags)
               RET  NZ
               CALL page_in_screen
               LD   A,(IY+key)
               CALL alpha
               JR   C,got_key_code
; Change lower to upper case
               AND  %11011111
;
  got_key_code:CALL main_keys
               CALL common_keys
;
; Cursor_D:
               CP   10
               JP   Z,instr_down
; Cursor_U:
               CP   11
               JP   Z,instr_up
; B:
               CP   "B"
               JP   Z,exec_boundry
; C:
               CP   "C"
               JP   Z,copy_block
; D:
               CP   "D"
               JP   Z,do_dis_mem
; E:
               CP   "E"
               JP   Z,exec_n
; F:
               CP   "F"
               JP   Z,search_memory
; G:
               CP   "G"
               JP   Z,mem_graph
; N:
               CP   "N"
               JP   Z,num_edit
; R:
               CP   "R"
               JP   Z,exec_input
; S:
               CP   "S"
               JP   Z,state_info
; T:
               CP   "T"
               JP   Z,txt_edit
; U:
               CP   "U"
               JP   Z,exec_until
; W:
               CP   "W"
               JP   Z,exec_output
; X:
               CP   "X"
               JP   Z,exec_mem_watch
; EDIT-Z:
               CP   edit_+"Z"
               JP   Z,load_sna_regs
; F4:
               CP   204
               JP   Z,exec_complete
; F5:
               CP   205
               JP   Z,exec_till_tos
; F6:
               CP   206
               JP   Z,exec_to_here
; F7:
               CP   207
               JP   Z,do_single_step
; F8:
               CP   208
               JP   Z,execute_10
; F9:
               CP   209
               JP   Z,execute_100
; CTRL-F7:
               CP   217
               JP   Z,execute
; CTRL-F9:
               CP   219
               JP   Z,condit_execute
; EDIT-DELETE:
               CP   251
               JP   Z,delete_instr
; CTRL-E:
               CP   ctrl_+"E"
               JP   Z,do_evaluate
; CTRL-M:
               CP   ctrl_+"M"
               JP   Z,gen_int
; CTRL-N:
               CP   ctrl_+"N"
               JP   Z,generate_nmi
; CTRL-T:
               CP   ctrl_+"T"
               JP   Z,exec_trace
; CTRL-Q:
               CP   ctrl_+"Q"
               JP   Z,quit_monitor
; CTRL-Esc:
               CP   254
               JP   Z,nmi_quit
;
               JP   menu_loop

;
; Main keys to edit regs etc. Called by some options
;
     main_keys:POP  HL
; Cursor_L:
               CP   8
               JP   Z,dec_pc
; Cursor_R:
               CP   9
               JP   Z,inc_pc
; SHIFT-Cursor_U:
               CP   20
               JP   Z,inc_inc_sp
; SHIFT-Cursor_D:
               CP   21
               JP   Z,dec_dec_sp
; H:
               CP   "H"
               JP   Z,set_hmpr
; I:
               CP   "I"
               JP   Z,set_int_delay
; K:
               CP   "K"
               JP   Z,push_val
; L:
               CP   "L"
               JP   Z,set_lmpr
; M:
               CP   "M"
               JP   Z,set_scr_mode
; V:
               CP   "V"
               JP   Z,set_vmpr
; CTRL-A:
               CP   ctrl_+"A"
               JP   Z,menu_ex_af_af
; CTRL-D:
               CP   ctrl_+"D"
               JP   Z,menu_ex_de_hl
; CTRL-F:
               CP   ctrl_+"F"
               JP   Z,fill_region
; CTRL-I
               CP   ctrl_+"I"
               JP   Z,toggle_ints
; CTRL-L:
               CP   ctrl_+"L"
               JP   Z,set_line_ints
; CTRL-R:
               CP   ctrl_+"R"
               JP   Z,in_byte
; CTRL-W:
               CP   ctrl_+"W"
               JP   Z,out_byte
; CTRL-X:
               CP   ctrl_+"X"
               JP   Z,menu_exx
; CTRL-Z:
               CP   ctrl_+"Z"
               JP   Z,prepare_basic
; EDIT-A:
               CP   edit_+"A"
               JP   Z,edit_a
; EDIT-B:
               CP   edit_+"B"
               JP   Z,edit_b
; EDIT-C:
               CP   edit_+"C"
               JP   Z,edit_c
; EDIT-D:
               CP   edit_+"D"
               JP   Z,edit_d
; EDIT-E:
               CP   edit_+"E"
               JP   Z,edit_e
; EDIT-F:
               CP   edit_+"F"
               JP   Z,edit_f
; EDIT-H:
               CP   edit_+"H"
               JP   Z,edit_h
; EDIT-I:
               CP   edit_+"I"
               JP   Z,edit_i
; EDIT-L:
               CP   edit_+"L"
               JP   Z,edit_l
; EDIT-P:
               CP   edit_+"P"
               JP   Z,edit_pc
; EDIT-R:
               CP   edit_+"R"
               JP   Z,edit_r
; EDIT-S:
               CP   edit_+"S"
               JP   Z,edit_sp
; EDIT-X:
               CP   edit_+"X"
               JP   Z,edit_ix
; EDIT-Y:
               CP   edit_+"Y"
               JP   Z,edit_iy
; CTRL-F0:
               CP   210
               JP   Z,set_im_0
; CTRL-F1:
               CP   211
               JP   Z,set_im_1
; CTRL-F2:
               CP   212
               JP   Z,set_im_2
;
               JP   (HL)

;
  quit_monitor:POP  HL
               JP   let_go
;
      nmi_quit:POP  HL
               CALL let_go
               POP  HL
               LD   IX,quit_return
               JP   quit_prepare
   quit_return:LD   A,%00011111
               JP   nmi_exit


;
; Common keys used by most options
;
   common_keys:POP  HL
;
; P:
               CP   "P"
               JP   Z,set_palett_col
               PUSH HL
    txt_common:POP  HL
; CAPS:
               CP   1
               JP   Z,toggle_caps
; CTRL-B:
               CP   ctrl_+"B"
               JP   Z,toggle_base
; CTRL-P:
               CP   ctrl_+"P"
               JP   Z,reset_palette
; CTRL-S:
               CP   ctrl_+"S"
               JP   Z,toggle_sound
; CTRL-INV:
               CP   252
               JP   Z,sig_ref_all
; F0:
               CP   200
               JP   Z,toggle_rom0
; F1:
               CP   201
               JP   Z,toggle_rom1
; F2:
               CP   202
               JP   Z,toggle_wprot
;
               JP   (HL)

;
; Print lower menu
        prmenu:LD   DE,5888
               LD   HL,mendat
               LD   B,64
               JP   print_inv_safe
;
   update_info:CALL patch_ports
               CALL update_screen
               BIT  0,(IY+refresh_flags)
               RES  0,(IY+refresh_flags)
               CALL NZ,top_menu
               BIT  1,(IY+refresh_flags)
               RES  1,(IY+refresh_flags)
               CALL NZ,show_reg_vals
               RET
;
      top_menu:LD   A,(IY+port_lmpr)
               AND  %00011111
               CALL split_num
               LD   (top_txt+38),HL
               LD   A,(IY+port_hmpr)
               AND  %00011111
               CALL split_num
               LD   (top_txt+43),HL
               LD   A,(IY+port_vmpr)
               AND  %00011111
               CALL split_num
               LD   (top_txt+48),HL
               LD   A,(IY+port_vmpr)
               RLCA
               RLCA
               RLCA
               AND  %00000011
               ADD  A,49
               LD   (top_txt+53),A
               BIT  0,(IY+misc_flags)
               LD   HL,#4646
               JR   Z,caps_off
               LD   HL,#204E
      caps_off:LD   (top_txt+62),HL
               BIT  5,(IY+port_lmpr)
               LD   HL,rom0_mess
               JR   Z,rom0_is_on
               LD   HL,blanks
    rom0_is_on:LD   DE,top_txt+16
               LD   BC,4
               LDIR
               BIT  6,(IY+port_lmpr)
               LD   HL,rom1_mess
               JR   NZ,rom1_is_on
               LD   HL,blanks
    rom1_is_on:LD   DE,top_txt+22
               LD   BC,4
               LDIR
               BIT  7,(IY+port_lmpr)
               LD   HL,wprot_mess
               JR   NZ,wprot_is_on
               LD   HL,blanks
   wprot_is_on:LD   DE,top_txt+28
               LD   BC,5
               LDIR
               LD   HL,top_txt
               LD   DE,0
               LD   B,64
               JP   print_inv_safe
;
     split_num:LD   L,47
      split_lp:SUB  10
               INC  L
               JR   NC,split_lp
               ADD  A,58
               LD   H,A
               RET
;
;
;
       top_txt:DM   "TurboMON 1.0                     "
               DM   "   L:XX H:XX V:XX M:X  CAPS OFF"
        mendat:DM   "ctrl-Quit  Status Txt Num Dis Fin"
               DM   "d Graph Copy Eval Mode L/H/Vmpr"
     rom0_mess:DM   "ROM0"
     rom1_mess:DM   "ROM1"
    wprot_mess:DM   "WPROT"
        blanks:DM   "     "
;
;
        let_go:EX   AF,AF'
     let_go_lp:HALT
               LD   A,(IY+lastkey)
               AND  A
               JR   NZ,let_go_lp
               EX   AF,AF'
               HALT
               HALT
               RET
;
; Clear the input line
;
clear_inp_line:LD   B,64
               LD   HL,#1700
               LD   (xpos),HL
     cl_inp_lp:LD   A,32
               RST  8
               DJNZ cl_inp_lp
               RET
;
print_inv_safe:LD   A,B
               LD   BC,(xpos)
               PUSH BC
               BIT  1,(IY+misc_flags)
               SET  1,(IY+misc_flags)
               PUSH AF
               LD   (xpos),DE
               LD   B,A
 print_safe_lp:LD   A,(HL)
               RST  8
               INC  HL
               DJNZ print_safe_lp
               POP  AF
               POP  HL
               LD   (xpos),HL
               RET  NZ
               RES  1,(IY+misc_flags)
               RET
;
;
; Reset monitor palette to default values
;
 reset_palette:LD   HL,normal_palette
               LD   DE,mon_palette
               LD   BC,16
               LDIR
               SET  2,(IY+refresh_flags)
               RET
;
set_palett_col:LD   DE,set_palett_msg
               CALL input_number
               RET  NZ
               PUSH HL
               LD   DE,set_col_msg
               CALL input_number
               POP  DE
               RET  NZ
               SET  2,(IY+refresh_flags)
               LD   C,248
               LD   B,E
               LD   A,L
               JP   write_port
;
  get_xmpr_val:CALL page_in_screen
               LD   (set_xmpr_msg+4),A
               LD   DE,set_xmpr_msg
               CALL input_number
               POP  IX
               RET  NZ
               LD   A,L
               AND  %00011111
               LD   L,A
               JP   (IX)
;
      set_lmpr:LD   A,"L"
               CALL get_xmpr_val
               LD   A,(IY+port_lmpr)
               AND  %11100000
               OR   L
               LD   (IY+port_lmpr),A
               JR   sig_ref_topds2
;
      set_hmpr:LD   A,"H"
               CALL get_xmpr_val
               LD   A,(IY+port_hmpr)
               AND  %11100000
               OR   L
               LD   (IY+port_hmpr),A
sig_ref_topds2:SET  2,(IY+refresh_flags)
               JR   sig_ref_top2
;
      set_vmpr:LD   A,"V"
               CALL get_xmpr_val
               LD   A,(IY+port_vmpr)
               AND  %01100000
               OR   L
               LD   (IY+port_vmpr),A
               JR   sig_ref_topds2
;
  set_scr_mode:LD   DE,set_mode_msg
               CALL input_number
               RET  NZ
               LD   A,L
               DEC  A
               RRCA
               RRCA
               RRCA
               AND  %01100000
               LD   L,A
               LD   A,(IY+port_vmpr)
               AND  %00011111
               OR   L
               LD   (IY+port_vmpr),A
  sig_ref_top2:SET  0,(IY+refresh_flags)
               RET
;
      set_im_0:XOR  A
  set_int_mode:LD   (reg_int_mode),A
 sig_ref_panel:SET  1,(IY+refresh_flags)
               RET
      set_im_1:LD   A,1
               JR   set_int_mode
      set_im_2:LD   A,2
               JR   set_int_mode
;
   toggle_ints:LD   A,(reg_iff1)
               XOR  %00000100
               LD   (reg_iff1),A
               JR   sig_ref_panel
;
   toggle_base:LD   A,(IY+number_base)
               XOR  1
               LD   (IY+number_base),A
               SET  2,(IY+refresh_flags)
               JR   sig_ref_panel
;
  toggle_sound:LD   A,28
               XOR  28
               LD   (toggle_sound+1),A
               LD   E,A
               JR   Z,set_sound
               LD   A,1
     set_sound:LD   BC,511
               LD   A,28
               OUT  (C),A
               DEC  B
               OUT  (C),E
               RET
;
       in_byte:LD   DE,in_byte_msg
               CALL input_number
               RET  NZ
               LD   C,L
               LD   B,H
               PUSH HL
               CALL read_port
               PUSH AF
               PUSH AF
               CALL clear_txt_buf
               CALL print_buf_pop
               DM   "Value"
               DB   " "+128
               POP  AF
               CALL print_min_byte
               CALL print_buf_pop
               DB   " "," "+128
               POP  AF
               LD   E,A
               CALL bin_e_buf
               CALL print_buf_pop
               DM   "  read from port"
               DB   " "+128
               POP  HL
               CALL print_hl_min
               LD   A,"."
               CALL print_a_buf
               CALL print_buf_bot
               JP   wait_key_ret
;
      out_byte:LD   DE,out_byte_msg
               CALL input_number
               RET  NZ
               LD   C,L
               LD   B,H
               PUSH BC
               LD   DE,send_byte_msg
               CALL input_number
               POP  BC
               RET  NZ
               LD   A,L
               CALL write_port
               JR   sig_ref_3
;
   toggle_rom0:LD   A,(IY+port_lmpr)
               XOR  rom0_bitval
               LD   (IY+port_lmpr),A
               JR   sig_ref_3
;
   toggle_rom1:LD   A,(IY+port_lmpr)
               XOR  rom1_bitval
               LD   (IY+port_lmpr),A
     sig_ref_3:SET  2,(IY+refresh_flags)
               SET  1,(IY+refresh_flags)
               JR   sig_ref_top
;
  toggle_wprot:LD   A,(IY+port_lmpr)
               XOR  wprot_bitval
               LD   (IY+port_lmpr),A
               SET  2,(IY+refresh_flags)
   sig_ref_top:SET  0,(IY+refresh_flags)
               RET
;
; Hot keys work any-time except during execution
      hot_keys:LD   A,(IY+key)
               CP   203
               JR   Z,toggle_screen
               CP   213
               JR   Z,toggle_scr_on
               CP   253
               JR   Z,toggle_inv
               RET
;
 toggle_screen:LD   A,(IY+misc_flags)
               XOR  screen_bitval
               LD   (IY+misc_flags),A
               LD   (IY+key),0
               CALL patch_ports
               JP   update_screen
;
 toggle_scr_on:LD   A,(IY+port_border)
               XOR  scr_off_bitval
               LD   (IY+port_border),A
               CALL patch_ports
               SET  2,(IY+refresh_flags)
               JP   update_screen
;
   toggle_caps:LD   A,(IY+misc_flags)
               XOR  caps_bitval
               LD   (IY+misc_flags),A
               JR   sig_ref_top
;
    toggle_inv:LD   A,(IY+misc_flags)
               XOR  inv_bitval
               LD   (IY+misc_flags),A
               JP   update_screen

;
; E X E C U T I O N
;
; Signal the request to stop executing
 execute_break:SET  2,(IY+cpu_flags)
               SET  1,(IY+cpu_flags)
               RES  2,(IY+misc_flags)
               RET
;
  un_highlight:LD   HL,(reg_pcl)
               CALL disassemble
               PUSH AF
               LD   HL,(dis_coords)
               LD   (xpos),HL
               CALL print_txt_buf
               POP  AF
               RET
;
  highlight_pc:LD   DE,512
               LD   IX,dis_table
               LD   HL,(reg_pcl)
; Entries to search depends on lines on screen
               LD   B,dis_lines
  table_search:LD   A,L
               CP   (IX+0)
               JR   Z,low_match
     high_fail:INC  IX
               INC  IX
               INC  D
               DJNZ table_search
               SET  2,(IY+refresh_flags)
               RET
     low_match:LD   A,H
               CP   (IX+1)
               JR   NZ,high_fail
               LD   (xpos),DE
               LD   (dis_coords),DE
               SET  1,(IY+misc_flags)
               LD   HL,(reg_pcl)
               CALL disassemble
               CALL print_txt_buf
               RES  2,(IY+refresh_flags)
               RET
;
do_single_step:CALL un_highlight
               CALL exec_once
               JR   highlight_pc
;
     exec_once:LD   HL,(reg_pcl)
               CALL disassemble
               LD   HL,(reg_pcl)
               INC  HL
               JR   C,exec_prefix
               DEC  HL
; Only execute if not an unused index prefix
               CALL NC,single_step
   exec_prefix:LD   (reg_pcl),HL
   sig_ref_all:LD   (IY+refresh_flags),255
               RET
;
show_exe_messg:CALL clear_inp_line
               CALL print_at_pop
               DB   0,23
               DM   "Executing, press CTRL-F8 to stop."
               DB   " "+128
               SET  0,(IY+cpu_flags)
               RET

;
; Fast, unconditional execution
;
       execute:CALL show_exe_messg
               CALL un_highlight
               SET  2,(IY+misc_flags)
               CALL redo_port_scr
               LD   HL,(reg_pcl)
               CALL execute_loop
               JP   highlight_pc
;
  execute_loop:RST  32
               BIT  1,(IY+cpu_flags)
               JP   Z,execute_loop
               RES  1,(IY+cpu_flags)
               BIT  2,(IY+cpu_flags)
               LD   (reg_pcl),HL
               JR   NZ,stop_executing
               CALL generate_int
               JP   execute_loop
;
stop_executing:RES  0,(IY+cpu_flags)
               RES  2,(IY+misc_flags)
               RES  3,(IY+cpu_flags)
               RES  4,(IY+cpu_flags)
               LD   (IY+refresh_flags),255
               RET
;
    execute_10:LD   B,10
               JR   execute_n_simp
;
   execute_100:LD   B,100
execute_n_simp:PUSH BC
               CALL un_highlight
               POP  BC
               LD   HL,(reg_pcl)
   simp_exe_lp:PUSH BC
               RST  32
               POP  BC
               DJNZ simp_exe_lp
               LD   (reg_pcl),HL
               LD   (IY+refresh_flags),255
               JP   highlight_pc

;
; Execute until the currently set condition is met
;
condit_execute:CALL clear_inp_line
               CALL clear_txt_buf
               CALL print_buf_pop
               DM   "Executing until"
               DB   " "+128
  cond_printer:CALL until_printer
               EXX
               LD   (HL),"."+128
               CALL print_buf_bot
               CALL un_highlight
cond_init_rout:CALL dummy_ret
; IX holds the handler to check for the condition
  cond_handler:LD   IX,exec_until_chk
               LD   HL,(reg_pcl)
               RST  32
               SET  0,(IY+cpu_flags)
               CALL do_cond_check
               LD   (reg_pcl),HL
               CALL stop_executing
               JP   highlight_pc
;
   call_ind_ix:DB   #DD
   call_ind_hl:JP   (HL)
     dummy_ret:RET
;
 cond_exe_loop:RST  32
 do_cond_check:JP   (IX)
 cond_re_enter:BIT  1,(IY+cpu_flags)
               JP   Z,cond_exe_loop
               RES  1,(IY+cpu_flags)
               BIT  2,(IY+cpu_flags)
               LD   (reg_pcl),HL
               RET  NZ
               CALL generate_int
; Check if interrupt has caused condition
               JP   (IX)

;
; Set condition as: execute until PC = x
;
    exec_until:LD   DE,exec_until_msg
               CALL input_number
               RET  NZ
      until_in:LD   (until_addr+1),HL
               LD   HL,exec_until_chk
               LD   (cond_handler+2),HL
               LD   HL,dummy_ret
               LD   (cond_init_rout+1),HL
               LD   HL,until_printer
               LD   (cond_printer+1),HL
               JP   condit_execute
;
exec_until_chk:EX   DE,HL
    until_addr:LD   HL,0
               AND  A
               SBC  HL,DE
               EX   DE,HL
               JP   NZ,cond_re_enter
               RET
;
 until_printer:CALL print_buf_pop
               DM   "PC ="
               DB   " "+128
               LD   HL,(until_addr+1)
               CALL print_hl_min
               RET
;
; Execute Trace until PC = x
;
    exec_trace:LD   DE,exec_trace_msg
               CALL input_number
               RET  NZ
               LD   (tr_until_addr+1),HL
               LD   HL,trace_chk
               LD   (cond_handler+2),HL
               LD   HL,trace_init
               LD   (cond_init_rout+1),HL
               LD   HL,trace_printer
               LD   (cond_printer+1),HL
               JP   condit_execute
;
     trace_chk:EX   DE,HL
 tr_until_addr:LD   HL,0
               AND  A
               SBC  HL,DE
               EX   DE,HL
               JR   Z,trace_done
               LD   (trace_prev+1),HL
               JP   cond_re_enter
    trace_done:POP  IX
               CALL call_ind_ix
               CALL update_info
               RES  2,(IY+refresh_flags)
               CALL show_dis_page
               DI
               SET  4,(IY+misc_flags)
               CALL print_at_pop
               DB   0,23
               DM   "Trace:"
               DB   " "+128
               RES  4,(IY+misc_flags)
               EI
               CALL clear_txt_buf
               CALL print_buf_pop
               DM   "Previous instruction at address"
               DB   " "+128
    trace_prev:LD   HL,0
               CALL print_hl_min
               LD   A,"."
               CALL print_a_buf
               CALL print_txt_buf
  wait_key_ret:HALT
               LD   A,(IY+key)
               AND  A
               JR   Z,wait_key_ret
               SET  5,(IY+misc_flags)
               RET
;
    trace_init:LD   HL,(reg_pcl)
               LD   (trace_prev+1),HL
               RET
;
 trace_printer:CALL print_buf_pop
               DM   "PC ="
               DB   " "+128
               LD   HL,(tr_until_addr+1)
               CALL print_hl_min
               CALL print_buf_pop
               DM   " (with trace"
               DB   ")"+128
               RET

;
; Execute trace within boundry
;
  exec_boundry:LD   DE,bound_from_msg
               CALL input_number
               RET  NZ
               PUSH HL
               LD   DE,bound_to_msg
               CALL input_number
               POP  DE
               RET  NZ
               AND  A
               SBC  HL,DE
; Return if end < start
               RET  C
               INC  HL
               LD   A,H
               OR   L
; Execute normally if given [0 to 65535]
               JP   Z,execute
               LD   (boundry_length+1),HL
               DEC  HL
               ADD  HL,DE
               LD   (boundry_lower+1),DE
               LD   (boundry_upper+1),HL
;
               LD   HL,boundry_chk
               LD   (cond_handler+2),HL
               LD   HL,boundry_init
               LD   (cond_init_rout+1),HL
               LD   HL,bound_printer
               LD   (cond_printer+1),HL
               JP   condit_execute
;
  boundry_init:LD   HL,(reg_pcl)
               LD   (trace_prev+1),HL
               RET
;
 bound_printer:CALL print_buf_pop
               DM   "outside boundry "
               DB   "["+128
               LD   HL,(boundry_lower+1)
               CALL print_hl_min
               CALL print_buf_pop
               DM   " to"
               DB   " "+128
 boundry_upper:LD   HL,0
               CALL print_hl_min
               CALL print_buf_pop
               DB   "]","."+128
               RET
;
   boundry_chk:LD   E,L
               LD   D,H
               AND  A
 boundry_lower:LD   BC,0
               SBC  HL,BC
               JR   C,out_of_bounds
boundry_length:LD   BC,0
               SBC  HL,BC
               JR   NC,out_of_bounds
               EX   DE,HL
               LD   (trace_prev+1),HL
               JP   cond_re_enter
;
 out_of_bounds:EX   DE,HL
               JP   trace_done

;
; Execute until a given location changes in value
;
exec_mem_watch:LD   DE,exec_mem_msg
               CALL input_number
               RET  NZ
; Now we have to decide which page holds that location
               LD   A,(IY+port_lmpr)
               BIT  7,H
               SET  7,H
               JR   Z,watch_a_b
               LD   A,(IY+port_hmpr)
     watch_a_b:BIT  6,H
               JR   Z,watch_a_c
               RES  6,H
               INC  A
     watch_a_c:AND  %00011111
               LD   (watch_mem_chk+1),A
               LD   (watch_mem_addr+1),HL
               LD   HL,watch_mem_chk
               LD   (cond_handler+2),HL
               LD   HL,watch_mem_init
               LD   (cond_init_rout+1),HL
               LD   HL,mem_printer
               LD   (cond_printer+1),HL
               JP   condit_execute
;
watch_mem_init:LD   A,(watch_mem_chk+1)
               OUT  (hmpr),A
               LD   HL,(watch_mem_addr+1)
               LD   A,(HL)
               LD   (orig_watch_val+1),A
               RET
;
 watch_mem_chk:LD   A,0
               OUT  (hmpr),A
watch_mem_addr:LD   A,(0)
orig_watch_val:CP   0
               JP   Z,cond_re_enter
               POP  IX
               PUSH AF
               CALL call_ind_ix
               CALL update_info
               RES  2,(IY+refresh_flags)
               CALL show_dis_page
               DI
               SET  4,(IY+misc_flags)
               CALL print_at_pop
               DB   0,23
               DM   "Location:"
               DB   " "+128
               RES  4,(IY+misc_flags)
               EI
               CALL clear_txt_buf
               CALL print_buf_pop
               DM   "Changed from"
               DB   " "+128
               LD   A,(orig_watch_val+1)
               CALL print_min_byte
               CALL print_buf_pop
               DM   " to"
               DB   " "+128
               POP  AF
               CALL print_min_byte
               LD   A,"."
               CALL print_a_buf
               CALL print_txt_buf
               JP   wait_key_ret
;
   mem_printer:CALL print_buf_pop
               DM   "page"
               DB   " "+128
               LD   A,(watch_mem_chk+1)
               CALL print_min_byte
               CALL print_buf_pop
               DM   ", offset"
               DB   " "+128
               LD   HL,(watch_mem_addr+1)
               RES  7,H
               CALL print_hl_min
               CALL print_buf_pop
               DM   " changes"
               DB   "s"+128
               RET


;
; Execute until the current instruction is returned to
;
  exec_to_here:LD   HL,(reg_pcl)
               JP   until_in
;
; Execute until after the next instruction
;
 exec_complete:LD   HL,(reg_pcl)
               CALL disassemble
               JP   until_in

;
; Execute until the address on the top of the stack
;
 exec_till_tos:LD   HL,(top_of_stack)
               JP   until_in
;
; Execute a set number of instructions
;
        exec_n:LD   DE,exec_n_msg
               CALL input_number
               RET  NZ
               LD   A,H
               OR   L
               RET  Z
               LD   (exec_n_init+1),HL
               LD   HL,exec_n_chk
               LD   (cond_handler+2),HL
               LD   HL,exec_n_init
               LD   (cond_init_rout+1),HL
               LD   HL,exec_n_printer
               LD   (cond_printer+1),HL
               JP   condit_execute
;
    exec_n_chk:LD   DE,0
               DEC  DE
               LD   A,D
               OR   E
               LD   (exec_n_chk+1),DE
               JP   NZ,cond_re_enter
               RET
;
   exec_n_init:LD   HL,0
               LD   (exec_n_chk+1),HL
               RET
;
exec_n_printer:CALL print_buf_pop
               DM   "after"
               DB   " "+128
               LD   HL,(exec_n_init+1)
               CALL print_hl_min
               CALL print_buf_pop
               DM   " instruction"
               DB   "s"+128
               RET

;
; Set condition as: execute until port x is read from
;
    exec_input:LD   DE,input_port_msg
               CALL input_number
               RET  NZ
               LD   A,L
               LD   (input_patch+1),A
               LD   HL,input_init
               LD   (cond_init_rout+1),HL
               LD   HL,input_printer
               LD   (cond_printer+1),HL
               LD   HL,cond_re_enter
               LD   (cond_handler+2),HL
               JP   condit_execute
;
    input_init:SET  3,(IY+cpu_flags)
               RET
;
 input_printer:CALL print_buf_pop
               DM   "port"
               DB   " "+128
               LD   A,(input_patch+1)
               CALL print_min_byte
               CALL print_buf_pop
               DM   " is read fro"
               DB   "m"+128
               RET

;
; Set condition as: execute until port x is written to
;
   exec_output:LD   DE,outpt_port_msg
               CALL input_number
               RET  NZ
               LD   A,L
               LD   (output_patch+1),A
               LD   HL,output_init
               LD   (cond_init_rout+1),HL
               LD   HL,output_printer
               LD   (cond_printer+1),HL
               LD   HL,cond_re_enter
               LD   (cond_handler+2),HL
               JP   condit_execute
;
   output_init:SET  4,(IY+cpu_flags)
               RET
;
output_printer:CALL print_buf_pop
               DM   "port"
               DB   " "+128
               LD   A,(output_patch+1)
               CALL print_min_byte
               CALL print_buf_pop
               DM   " is written t"
               DB   "o"+128
               RET

;
 show_dis_page:LD   HL,(reg_pcl)
               LD   (dis_top),HL
               SET  1,(IY+misc_flags)
   show_dis_hl:LD   IX,dis_table
               LD   DE,512
               LD   (xpos),DE
               LD   (dis_coords),DE
               LD   B,dis_lines
   fill_dis_lp:LD   (IX+0),L
               LD   (IX+1),H
               INC  IX
               INC  IX
               PUSH BC
               PUSH IX
               CALL disassemble
               CALL print_txt_buf
               XOR  A
               LD   (xpos),A
               LD   A,(ypos)
               INC  A
               LD   (ypos),A
               POP  IX
               POP  BC
               DJNZ fill_dis_lp
               LD   (IX+0),L
               LD   (IX+1),H
               RET
;
     dis_table:DS   dis_buf_size+2
    dis_coords:DW   0
       dis_top:DW   0
;
 print_buf_bot:LD   DE,#1700
  print_buf_at:LD   (xpos),DE
 print_txt_buf:LD   B,45
               LD   DE,txt_buffer
        dis_lp:LD   A,(DE)
               RST  8
               INC  DE
               DJNZ dis_lp
               RES  1,(IY+misc_flags)
               EXX
               LD   HL,txt_buffer
               EXX
               RET
;
;
; Delay in 1/50:hs of a second between interrupts
 set_int_delay:LD   DE,int_delay_msg
               CALL input_number
               RET  NZ
               LD   A,H
               INC  H
               JR   Z,int_set
               LD   A,L
               AND  A
               JR   Z,int_set
               LD   A,L
               INC  L
               JR   NZ,int_set
               LD   A,254
       int_set:LD   (IY+int_count),A
               LD   (IY+int_delay),A
               LD   (IY+refresh_flags),%00000110
               RET
;
 set_line_ints:LD   DE,line_int_msg
               CALL input_number
               RET  NZ
               LD   (IY+line_count),L
               LD   (IY+line_ints),L
               LD   (IY+refresh_flags),%00000110
               RET
;
 load_sna_regs:CALL prepare_basic
               CALL reset_palette
;
               LD   A,3+wprot_bitval+rom0_bitval
               LD   (IY+port_lmpr),A
               LD   A,5
               LD   (IY+port_hmpr),A
               LD   A,4+mode1
               LD   (IY+port_vmpr),A
               CALL patch_ports
;
               LD   HL,section_b-27
               LD   DE,work_space
               LD   BC,27
               RST  16
               LD   A,(work_space)
               LD   (reg_int_vector),A
               LD   HL,(work_space+1)
               LD   (reg_alt_l),HL
               LD   HL,(work_space+3)
               LD   (reg_alt_e),HL
               LD   HL,(work_space+5)
               LD   (reg_alt_c),HL
               LD   HL,(work_space+7)
               LD   (reg_alt_flags),HL
               LD   HL,(work_space+9)
               LD   (reg_l),HL
               LD   HL,(work_space+11)
               LD   (reg_e),HL
               LD   HL,(work_space+13)
               LD   (reg_c),HL
               LD   HL,(work_space+15)
               LD   (reg_iyl),HL
               LD   HL,(work_space+17)
               LD   (reg_ixl),HL
               LD   HL,(work_space+19)
               LD   A,L
               AND  %00000100
               LD   (reg_iff1),A
               LD   A,H
               LD   (reg_refresh),A
               LD   HL,(work_space+21)
               LD   (reg_flags),HL
               LD   HL,(work_space+25)
               LD   A,L
               AND  %00000011
               LD   (reg_int_mode),A
               LD   A,H
               AND  %00000111
               LD   (IY+port_border),A
               LD   HL,(work_space+23)
               INC  HL
               INC  HL
               LD   (reg_spl),HL
               DEC  HL
               DEC  HL
               LD   DE,reg_pcl
               LD   BC,2
               RST  16
               JP   sig_ref_all

    work_space:DS   32

;
; Generate non-maskable interrupt
; Preserve interrupt status, disable interrupts and CALL
; NMI routine at 102
;
  generate_nmi:LD   A,(reg_iff1)
               LD   (reg_iff2),A
               XOR  A
               LD   (reg_iff1),A
               LD   HL,(reg_pcl)
               CALL push_hl
               LD   HL,nmi
               LD   (reg_pcl),HL
               LD   (IY+refresh_flags),255
               RET

;
; Generate maskable interrupt
; disable interrupts and call the interrupt handler for
; the current mode (using I for IM 2)
; Used as menu command AND by TurboMON interrupt gen.
;
       gen_int:LD   (IY+refresh_flags),255
  generate_int:LD   A,(reg_iff1)
               AND  %00000100
; Return if interrupts disabled
               RET  Z
               LD   HL,(reg_pcl)
               LD   DE,temp_store
; Fetch current instruction
               LD   BC,1
               RST  16
               DEC  (IY+line_count)
               JP   P,do_line_int
               LD   A,(IY+line_ints)
               LD   (IY+line_count),A
  do_frame_int:LD   A,(IY+port_status)
               OR   %00011111
; Signal FRAME INTerrupt
               AND  %11110111
               LD   (IY+port_status),A
               JR   do_interrupt
   do_line_int:LD   A,(IY+port_line_int)
               CP   192
; Line interrupts disabled so do a frame int instead
               JR   NC,do_frame_int
               LD   A,(IY+port_status)
               OR   %00011111
; Signal LINE INTerrupt
               AND  %11111110
               LD   (IY+port_status),A
  do_interrupt:XOR  A
; Disable monitor interrupts since interrupt has occurre
               LD   (reg_iff1),A
               LD   A,(reg_int_mode)
               CP   2
; IM 0 or IM 1 will call address 56
               LD   DE,56
               JR   NZ,push_int_addr
               LD   A,(reg_int_vector)
               LD   H,A
; Value read from bus taken as 255
               LD   L,255
               LD   DE,im2_addr+1
               LD   BC,2
               RST  16
      im2_addr:LD   DE,0
 push_int_addr:LD   A,(temp_store)
               CP   118
; If current instruction is HALT (waiting), free the CPU
               LD   HL,(reg_pcl)
               JR   NZ,not_halt
               INC  HL
      not_halt:LD   (reg_pcl),DE
       push_hl:LD   (int_addr),HL
               LD   HL,(reg_spl)
               DEC  HL
               DEC  HL
               LD   (reg_spl),HL
               LD   DE,int_addr
               EX   DE,HL
               LD   C,2
               CALL put_bytes
               LD   HL,(reg_pcl)
               RET
      int_addr:DW   0
;
; R E G I S T E R   E D I T I N G
;
  edit_reg_msg:DM   "Input new value for "
  edit_reg_lts:DB   " "," "
;
       edit_pc:LD   HL,#C350
               LD   DE,reg_pcl
               PUSH DE
               LD   (edit_reg_lts),HL
               LD   DE,edit_reg_msg
               CALL input_number
               POP  DE
               RET  NZ
               SET  2,(IY+refresh_flags)
               JR   edit_double_in
;
       edit_sp:LD   HL,#D053
               LD   DE,reg_spl
               JR   get_double_reg
;
       edit_ix:LD   HL,#D849
               LD   DE,reg_ixl
               JR   get_double_reg
;
       edit_iy:LD   HL,#D949
               LD   DE,reg_iyl
               JR   get_double_reg
;
get_double_reg:PUSH DE
               LD   (edit_reg_lts),HL
               LD   DE,edit_reg_msg
               CALL input_number
               POP  DE
edit_double_in:RET  NZ
               EX   DE,HL
               LD   (HL),E
               INC  HL
               LD   (HL),D
               JR   sig_ref_panel3
;
        edit_i:LD   A,"I"+128
               LD   HL,reg_int_vector
               JR   get_single_one
;
        edit_r:LD   A,"R"+128
               LD   HL,reg_refresh
               JR   get_single_one
;
        edit_a:LD   A,"A"+128
               LD   HL,reg_a
               JR   get_single_one
;
        edit_f:LD   A,"F"+128
               LD   HL,reg_flags
;
get_single_one:PUSH HL
               LD   (edit_reg_lts),A
               LD   DE,edit_reg_msg
               CALL input_number
               POP  DE
               RET  NZ
               EX   DE,HL
               LD   (HL),E
sig_ref_panel3:SET  1,(IY+refresh_flags)
               RET
;
        edit_b:LD   A,"B"+128
               LD   HL,reg_b
               LD   DE,reg_c
               JR   get_single_reg
;
        edit_c:LD   A,"C"+128
               LD   HL,reg_c
               JR   get_single_dup
;
        edit_d:LD   A,"D"+128
               LD   HL,reg_d
               LD   DE,reg_e
               JR   get_single_reg
;
        edit_e:LD   A,"E"+128
               LD   HL,reg_e
               JR   get_single_dup
;
        edit_h:LD   A,"H"+128
               LD   HL,reg_h
               LD   DE,reg_l
               JR   get_single_reg
;
        edit_l:LD   A,"L"+128
               LD   HL,reg_l
;
get_single_dup:LD   D,H
               LD   E,L
get_single_reg:PUSH DE
               PUSH HL
               LD   (edit_reg_lts),A
               LD   DE,edit_reg_msg
               CALL input_number
               EX   DE,HL
               POP  HL
               JR   NZ,pop_ret
               LD   (HL),E
               POP  HL
               LD   A,D
               AND  A
               JR   Z,sig_ref_panel3
               LD   (HL),E
               INC  HL
               LD   (HL),D
               JR   sig_ref_panel3
       pop_ret:POP  HL
               RET
;
;
        dec_pc:LD   HL,(reg_pcl)
               DEC  HL
               LD   (reg_pcl),HL
               JR   sig_ref_pandis
        inc_pc:LD   HL,(reg_pcl)
               INC  HL
               LD   (reg_pcl),HL
sig_ref_pandis:SET  2,(IY+refresh_flags)
               JR   sig_ref_panel4
;
    instr_down:CALL un_highlight
               LD   A,(ypos)
               DEC  A
               JR   look_up_in
;
      instr_up:CALL un_highlight
               LD   A,(ypos)
               SUB  3
               JR   C,instr_try_back
    look_up_in:CALL look_up_pc
               JR   new_pc_back
;
    look_up_pc:ADD  A,A
               LD   L,A
               LD   H,0
               LD   DE,dis_table
               ADD  HL,DE
               LD   E,(HL)
               INC  HL
               LD   H,(HL)
               LD   L,E
               RET
;
instr_try_back:LD   HL,(reg_pcl)
               CALL find_prev_inst
   new_pc_back:LD   (reg_pcl),HL
               SET  1,(IY+refresh_flags)
               JP   highlight_pc
;
find_prev_inst:LD   E,L
               LD   D,4
               LD   BC,0-4
               ADD  HL,BC
 instr_up_find:PUSH HL
               PUSH DE
               CALL disassemble
               POP  DE
               LD   A,E
               CP   L
               POP  HL
               RET  Z
               INC  HL
               DEC  D
               JR   NZ,instr_up_find
               DEC  HL
               RET
;
    dec_dec_sp:LD   HL,(reg_spl)
               DEC  HL
               DEC  HL
               LD   (reg_spl),HL
               JR   sig_ref_panel4
    inc_inc_sp:LD   HL,(reg_spl)
               INC  HL
               INC  HL
               LD   (reg_spl),HL
sig_ref_panel4:SET  1,(IY+refresh_flags)
               RET
;
      push_val:LD   DE,push_msg
               CALL input_number
               RET  NZ
               CALL push_hl
               SET  1,(IY+refresh_flags)
               RET
;
  delete_instr:LD   HL,(reg_pcl)
               CALL disassemble
               LD   DE,(reg_pcl)
               LD   HL,zeros
               CALL put_bytes
               LD   HL,(dis_top)
               CALL show_dis_hl
               JP   highlight_pc
         zeros:DB   0,0,0,0
;
; Prepare registers/paging to run a program
 prepare_basic:LD   HL,reg_l
               LD   B,reg_int_mode-reg_l
               LD   C,0
  clear_reg_lp:LD   (HL),C
               INC  HL
               DJNZ clear_reg_lp
               LD   A,31
               LD   (IY+port_lmpr),A
               LD   A,1
               LD   (IY+port_hmpr),A
  basic_screen:LD   A,0
               LD   (IY+port_vmpr),A
;
               LD   A,25
               LD   (IY+int_count),A
               LD   (IY+int_delay),A
               XOR  A
               LD   (IY+line_count),A
               LD   (IY+line_ints),A
;
               LD   (reg_int_vector),A
               LD   (reg_iff2),A
               INC  A
               LD   (reg_int_mode),A
               LD   A,4
               LD   (reg_iff1),A
;
               LD   A,128
               LD   (reg_pch),A
               LD   HL,17500
               LD   (reg_spl),HL
;
               LD   (IY+refresh_flags),255
               RET

;
; Evaluate numbers to various formats
;
   do_evaluate:LD   DE,eval_msg
               CALL input_number
               RET  NZ
               LD   (IY+refresh_flags),255
               PUSH HL
               CALL clear_main_scr
               DI
               CALL col_at_pop
               DB   48,3
               DM   "Word:    Low Byte:  High Byte"
               DB   ":"+128
               CALL col_at_pop
               DB   0,5
               DM   "Decimal"
               DB   ":"+128
               POP  HL
               PUSH HL
               LD   DE,decimal_word
               LD   BC,sdeci
               CALL print_eval
               CALL col_at_pop
               DB   0,7
               DM   "Hex"
               DB   ":"+128
               POP  HL
               PUSH HL
               LD   DE,print_hex_word
               LD   BC,print_hex_byte
               CALL print_eval
               CALL col_at_pop
               DB   0,9
               DM   "Binary"
               DB   ":"+128
               POP  HL
               PUSH HL
               LD   DE,dummy_ret
               LD   BC,print_8_bin
               CALL print_eval
               CALL col_at_pop
               DB   0,11
               DM   "ASCII"
               DB   ":"+128
               POP  HL
               PUSH HL
               LD   DE,dummy_ret
               LD   BC,print_ascii_1
               CALL print_eval
               CALL col_at_pop
               DB   0,13
               DM   "2's Comp."
               DB   ":"+128
               LD   HL,decimal_word
               LD   (twos_rout+1),HL
               POP  HL
               LD   DE,print_twos_2
               LD   BC,print_twos_1
               CALL print_eval
               EI
               CALL print_at_pop
               DB   0,23
               DM   "Press any key"
               DB   "."+128
               CALL let_go
               JP   wait_key_ret
;
    col_at_pop:SET  4,(IY+misc_flags)
               JP   print_at_pop
;
    print_eval:RES  4,(IY+misc_flags)
               LD   A,48
               LD   (xpos),A
               PUSH HL
               PUSH BC
               CALL call_ind_de
               LD   A,84
               LD   (xpos),A
               POP  DE
               POP  HL
               PUSH HL
               PUSH DE
               LD   H,0
               LD   A,L
               CALL call_ind_de
               LD   A,128
               LD   (xpos),A
               POP  DE
               POP  HL
               LD   L,H
               LD   H,0
               LD   A,L
               JP   call_ind_de
;
   call_ind_de:PUSH DE
               RET
;
   print_8_bin:LD   A,"%"
               RST  8
               LD   A,L
               JP   print_binary
;
 print_ascii_1:LD   A,L
               JP   pr_chr
;
  print_twos_1:LD   A,L
               RLA
               SBC  A,A
               LD   H,A
               LD   DE,sdeci
               LD   (twos_rout+1),DE
  print_twos_2:BIT  7,H
               LD   A,"+"
               JR   Z,twos_pos
               LD   A,"-"
               LD   DE,0
               EX   DE,HL
               AND  A
               SBC  HL,DE
      twos_pos:RST  8
     twos_rout:JP   decimal_word
;

    do_dis_mem:LD   DE,dis_addr_msg
               CALL input_number
               CP   128
               RET  Z
               CP   127
               JR   NZ,dis_back
               LD   HL,(reg_pcl)
;
      dis_back:CALL show_dis_hl
    dis_upd_lp:CALL clear_inp_line
               CALL update_info
               LD   HL,dis_upd_lp
               PUSH HL
  dis_key_loop:HALT
               BIT  2,(IY+refresh_flags)
               RES  2,(IY+refresh_flags)
               JR   Z,no_dis_refresh
               POP  HL
               LD   HL,(dis_table)
               JR   dis_back
;
no_dis_refresh:LD   A,(IY+key)
               AND  A
               JR   Z,dis_key_loop
;
               CP   10
               JR   Z,dis_down
;
               CP   11
               JP   Z,dis_up
;
               CP   8
               JP   Z,dis_page_up
               CP   21
               JP   Z,dis_page_up
;
               CP   9
               JP   Z,dis_page_down
               CP   20
               JP   Z,dis_page_down
;
               CP   255
               JR   Z,leave_dis
;
               LD   IX,dis_back
               CP   "A"
               JP   Z,inp_new_addr
;
               CP   "N"
               JP   Z,dis_switch_num
;
               CP   "T"
               JP   Z,dis_switch_txt
;
               CP   " "
               JR   Z,save_dis_addr
;
               CP   " "-1
               JR   Z,restore_dis_ad
;
               CALL common_keys
               CALL main_keys
;
               RET
;
 leave_txt_num:CALL print_cursor
     leave_dis:POP  HL
               LD   (IY+refresh_flags),255
               RET
;
 save_dis_addr:LD   HL,(dis_table)
               LD   (saved_dis_addr),HL
               CALL print_at_pop
               DB   0,23
               DM   "Address saved"
               DB   "."+128
               LD   B,50
    saved_wait:HALT
               DJNZ saved_wait
               RET
;
restore_dis_ad:LD   HL,(saved_dis_addr)
               POP  DE
               JP   dis_back
;
saved_dis_addr:DW   0
;
      dis_down:CALL scroll_down
               LD   HL,dis_table+2
               LD   DE,dis_table
               LD   BC,dis_buf_size-2
               LDIR
               LD   HL,(dis_table+dis_buf_size-4)
               CALL disassemble
               LD   (dis_table+dis_buf_size-2),HL
               CALL disassemble
               LD   DE,#1500
               JP   print_buf_at
;
        dis_up:LD   HL,dis_table+dis_buf_size-3
               LD   DE,dis_table+dis_buf_size-1
               LD   BC,dis_buf_size-2
               LDDR
               LD   HL,(dis_table)
               CALL find_prev_inst
               LD   (dis_table),HL
               CALL disassemble
               CALL scroll_up
               LD   DE,#0200
               JP   print_buf_at
;
dis_switch_num:LD   IX,num_x_default
               JR   dis_switch_in
dis_switch_txt:LD   IX,txt_x_default
 dis_switch_in:LD   HL,(dis_table)
               POP  DE
               JP   (IX)
;
 dis_page_down:LD   HL,(dis_table+dis_buf_size-2)
               CALL disassemble
               POP  DE
               JP   dis_back
;
   dis_page_up:LD   HL,(dis_table)
               LD   B,20
  page_up_find:PUSH BC
               CALL find_prev_inst
               POP  BC
               DJNZ page_up_find
               POP  DE
               JP   dis_back
;
  inp_new_addr:PUSH IX
               LD   DE,new_addr_msg
               CALL input_number
               POP  IX
               RET  NZ
               POP  DE
               JP   (IX)
