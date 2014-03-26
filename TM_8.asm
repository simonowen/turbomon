       end_16k:DS   0
  z_remain_16k:EQU  page_length-end_16k


;
; E X T R A   O P T I O N S
;
; Text editing, Number editing, Memory Graphs,
; Status Page, Area Fill, Block Copy, Memory Search
;

   graph_width:EQU  352
    graph_base:EQU  37760
;
; Code in high memory
               ORG  section_b+mode3_length
               DUMP section_d

    t_n_d_addr:DW   0

;
; Text Editing
;
   do_txt_edit:LD   DE,txt_addr_msg
               CALL input_number
               CP   128
               RET  Z
               CP   127
               LD   (txt_protect),A
               JR   NZ,txt_x_default
               LD   HL,(reg_pcl)
 txt_x_default:LD   DE,0-32
               ADD  HL,DE
               LD   (t_n_d_addr),HL
               CALL clear_inp_line
               LD   HL,&0320
               LD   (cursor_xpos),HL
               LD   A,255
               LD   (txt_protect),A
      txt_back:CALL txt_page
               CALL print_cursor
   txt_curs_lp:CALL cursor_info
               CALL update_info
               LD   HL,txt_curs_lp
               PUSH HL
  txt_key_loop:HALT
               BIT  2,(IY+refresh_flags)
               RES  2,(IY+refresh_flags)
               JR   Z,no_txt_refresh
               POP  HL
               LD   HL,(t_n_d_addr)
               LD   DE,0-640
               ADD  HL,DE
               LD   (t_n_d_addr),HL
               JR   txt_back
;
no_txt_refresh:LD   A,(IY+key)
               AND  A
               JR   Z,txt_key_loop
;
               CP   13
               JP   Z,togg_txt_prot
;
               CP   12
               JP   C,move_cursor
;
               CP   20
               JP   Z,txt_page_down
;
               CP   21
               JP   Z,txt_page_up
;
               CP   255
               JP   Z,leave_txt_num
;
               CALL txt_common
;
               LD   B,A
               LD   A,(txt_protect)
               AND  A
               LD   A,B
               JR   Z,no_txt_protect
;
               LD   IX,txt_x_default
               CP   "A"
               JP   Z,inp_new_addr
;
               CP   "D"
               JP   Z,txt_switch_dis
;
               CP   "N"
               JP   Z,txt_switch_num
;
               CP   "P"
               JP   Z,set_palett_col
;
               CALL main_keys
               RET
;
no_txt_protect:CP   32
               RET  C
               CP   128
               RET  NC
               LD   (get_put_tmp),A
               CALL get_cursor_add
     poke_byte:PUSH HL
               EX   DE,HL
               LD   HL,get_put_tmp
               LD   BC,1
               CALL put_bytes_safe
               POP  HL
               LD   DE,get_put_tmp
               LD   BC,1
               CALL get_bytes_safe
               LD   A,(get_put_tmp)
               LD   HL,(cursor_xpos)
               LD   (xpos),HL
               PUSH AF
               CALL print_cursor
               POP  AF
               RST  8
               CALL cursor_right
               CALL scroll_d_poss
               CALL print_cursor
               RET
;
txt_switch_num:LD   IX,num_x_default
               JR   txt_switch_in
txt_switch_dis:LD   IX,dis_back
 txt_switch_in:CALL print_cursor
               CALL get_cursor_add
               POP  DE
               JP   (IX)
;
 txt_page_down:LD   HL,(t_n_d_addr)
               LD   DE,0-160
               ADD  HL,DE
               LD   (t_n_d_addr),HL
               POP  HL
               JP   txt_back
;
   txt_page_up:LD   HL,(t_n_d_addr)
               LD   DE,0-1120
               ADD  HL,DE
               LD   (t_n_d_addr),HL
               POP  HL
               JP   txt_back
;
   move_cursor:PUSH AF
               CALL print_cursor
               POP  AF
               CP   8
               JR   NZ,curs_not_l
               CALL cursor_left
               JR   txt_curs_out
    curs_not_l:CP   9
               JR   NZ,curs_not_r
               CALL cursor_right
               JR   txt_curs_out
    curs_not_r:CP   10
               JR   NZ,curs_not_d
               CALL cursor_down
               JR   txt_curs_out
    curs_not_d:CP   11
               CALL Z,cursor_up
  txt_curs_out:CALL scroll_u_poss
               CALL scroll_d_poss
               CALL print_cursor
               RET
;
 scroll_d_poss:LD   A,(cursor_ypos)
               CP   &15
               RET  NZ
               CALL tline
               LD   A,&14
               LD   (cursor_ypos),A
               RET
;
 scroll_u_poss:LD   A,(cursor_ypos)
               CP   2
               RET  NZ
               LD   HL,(t_n_d_addr)
               LD   DE,0-32
               ADD  HL,DE
               PUSH HL
               LD   DE,0-640
               ADD  HL,DE
               LD   (t_n_d_addr),HL
               CALL tuline
               POP  HL
               LD   (t_n_d_addr),HL
               LD   A,3
               LD   (cursor_ypos),A
               RET
;
get_cursor_add:LD   A,(cursor_xpos)
               SUB  32
               SRL  A
               SRL  A
               LD   C,A
               LD   B,0
               LD   H,B
               LD   A,(cursor_ypos)
               SUB  3
               LD   L,A
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,BC
               LD   DE,(t_n_d_addr)
               ADD  HL,DE
               LD   DE,0-608
               ADD  HL,DE
               RET
;
 togg_txt_prot:LD   A,(txt_protect)
               CPL
               LD   (txt_protect),A
               CALL cursor_info
               LD   B,3
         prthl:HALT
               DJNZ prthl
               RET
;
  cursor_right:LD   A,(cursor_xpos)
               ADD  A,4
               CP   &A0
               LD   (cursor_xpos),A
               RET  NZ
               LD   A,&20
               LD   (cursor_xpos),A
   cursor_down:LD   A,(cursor_ypos)
               INC  A
               LD   (cursor_ypos),A
               CP   &18
               RET  NZ
               XOR  A
               LD   (cursor_ypos),A
               RET
;
   cursor_left:LD   A,(cursor_xpos)
               SUB  4
               CP   &1C
               LD   (cursor_xpos),A
               RET  NZ
               LD   A,&9C
               LD   (cursor_xpos),A
     cursor_up:LD   A,(cursor_ypos)
               SUB  1
               LD   (cursor_ypos),A
               RET  NC
               XOR  A
               LD   (cursor_ypos),A
               RET
;
      txt_page:CALL clear_txt_buf
               LD   HL,&0200
               LD   (xpos),HL
               LD   B,20
         tpglp:PUSH BC
               LD   HL,(t_n_d_addr)
               LD   B,0
               CALL print_hl
               LD   HL,(t_n_d_addr)
               LD   DE,txt_buffer+8
               LD   BC,32
               CALL get_bytes_safe
               LD   HL,(t_n_d_addr)
               LD   BC,32
               ADD  HL,BC
               LD   (t_n_d_addr),HL
               CALL print_txt_buf
               LD   HL,xpos
               LD   (HL),0
               INC  HL
               INC  (HL)
               POP  BC
               DJNZ tpglp
               RET
;
         tline:CALL scroll_down
               LD   HL,&1500
               LD   (xpos),HL
               CALL clear_txt_buf
               LD   B,1
               JR   tpglp
;
        tuline:CALL scroll_up
               LD   HL,&0200
               LD   (xpos),HL
               CALL clear_txt_buf
               LD   B,1
               JR   tpglp
;
   cursor_info:CALL clear_txt_buf
               CALL get_cursor_add
               PUSH HL
               LD   B,0
               CALL print_hl
               POP  HL
               LD   DE,get_put_tmp
               LD   BC,2
               CALL get_bytes_safe
               EXX
               LD   HL,txt_buffer+8
               EXX
               LD   A,(get_put_tmp)
               CALL print_a_byte
               EXX
               INC  HL
               INC  HL
               EXX
               LD   A,(get_put_tmp)
               LD   E,A
               CALL bin_e_buf
               EXX
               INC  HL
               INC  HL
               EXX
               LD   DE,cfini
               PUSH DE
               LD   A,(get_put_tmp)
               CP   32
               RET  C
               CP   128
               JP   C,print_a_buf
               CP   133
               RET  C
               CP   255
               JR   NZ,ntex
               LD   A,(get_put_tmp+1)
; Return to ignore out of range keywords
               CP   59
               RET  C
               CP   132
; Return to ignore out of range keywords
               RET  NC
               INC  A
          ntex:SUB  60
               EX   AF,AF'
               IN   A,(lmpr)
               OR   rom1_bitval
               OUT  (lmpr),A
               LD   HL,keyword_table
               EX   AF,AF'
               CALL print_buf_item
               IN   A,(lmpr)
               AND  &FF-rom1_bitval
               OUT  (lmpr),A
               RET
;
         cfini:LD   HL,5888
               LD   (xpos),HL
               LD   HL,txt_buffer+35
               EXX
               LD   A,(txt_protect)
               AND  A
               JR   NZ,tis_prot
               CALL print_buf_pop
               DM   "[OVERTYPE"
               DB   "]"+128
               JP   print_txt_buf
      tis_prot:CALL print_buf_pop
               DM   "[COMMAND]"
               DB   " "+128
               JP   print_txt_buf
;
  new_txt_addr:LD   DE,new_addr_msg
               CALL input_number
               RET  NZ
               POP  DE
               JP   txt_x_default
;
   txt_protect:DB   255

;
; Number Editing
;
; almost a carbon copy of Text Editing!! ;-)

   do_num_edit:LD   DE,num_addr_msg
               CALL input_number
               CP   128
               RET  Z
               CP   127
               JR   NZ,num_x_default
               LD   HL,(reg_pcl)
 num_x_default:LD   DE,0-9
               ADD  HL,DE
               LD   (t_n_d_addr),HL
               CALL clear_inp_line
               LD   HL,&031C
               LD   (cursor_xpos),HL
      num_back:CALL num_page
               CALL print_cursor
   num_curs_lp:CALL n_cursor_info
               CALL update_info
               LD   HL,num_curs_lp
               PUSH HL
  num_key_loop:HALT
               BIT  2,(IY+refresh_flags)
               RES  2,(IY+refresh_flags)
               JR   Z,no_num_refresh
               POP  HL
               LD   HL,(t_n_d_addr)
               LD   DE,0-180
               ADD  HL,DE
               LD   (t_n_d_addr),HL
               JR   num_back
;
no_num_refresh:LD   A,(IY+key)
               AND  A
               JR   Z,num_key_loop
;
               CP   13
               JP   Z,inp_num_bytes
;
               CP   12
               JP   C,move_num_curs
;
               CP   20
               JP   Z,num_page_down
;
               CP   21
               JP   Z,num_page_up
;
               LD   IX,num_x_default
               CP   "A"
               JP   Z,inp_new_addr
;
               CP   "D"
               JP   Z,num_switch_dis
;
               CP   "T"
               JP   Z,num_switch_txt
;
               CP   255
               JP   Z,leave_txt_num
;
               CALL main_keys
               CALL common_keys
               RET

 inp_num_bytes:LD   HL,(cursor_xpos)
               PUSH HL
               CALL let_go
               LD   HL,5888+104
               LD   DE,inp_bytes_msg
               CALL input_num_at
               JR   Z,ch_back
               POP  HL
               LD   (cursor_xpos),HL
               RET
       ch_back:LD   DE,&1700
               LD   (xpos),DE
               POP  DE
               PUSH HL
               PUSH HL
               LD   (cursor_xpos),DE
               PUSH AF
               CALL print_cursor
               POP  AF
               CALL n_get_curs_add
               POP  DE
               LD   A,E
               LD   (get_put_tmp),A
               LD   DE,get_put_tmp
               EX   DE,HL
               LD   BC,1
               CALL put_bytes_safe
   change_next:XOR  A
               LD   (xpos),A
               LD   C,A
               CALL n_line_start
               LD   A,(cursor_ypos)
               LD   (ypos),A
               LD   DE,(t_n_d_addr)
               PUSH DE
               LD   (t_n_d_addr),HL
               CALL clear_txt_buf
               LD   B,1
               CALL npglp
               POP  DE
               LD   (t_n_d_addr),DE
               CALL n_cursor_right
               CALL n_scrl_u_poss
               CALL n_scrl_d_poss
               CALL print_cursor
               CALL n_cursor_info
               POP  HL
               LD   A,H
               AND  A
               JR   Z,inp_num_bytes
               LD   L,A
               LD   H,0
               LD   DE,(cursor_xpos)
               PUSH DE
               JR   ch_back
;
num_switch_txt:LD   IX,txt_x_default
               JR   num_switch_in
num_switch_dis:LD   IX,dis_back
 num_switch_in:CALL print_cursor
               CALL n_get_curs_add
               POP  DE
               JP   (IX)
;
 num_page_down:LD   HL,(t_n_d_addr)
               LD   DE,0-45
               ADD  HL,DE
               LD   (t_n_d_addr),HL
               POP  HL
               JP   num_back
;
   num_page_up:LD   HL,(t_n_d_addr)
               LD   DE,0-315
               ADD  HL,DE
               LD   (t_n_d_addr),HL
               POP  HL
               JP   num_back
;
 move_num_curs:PUSH AF
               CALL print_cursor
               POP  AF
               CP   8
               JR   NZ,curs_n_l
               CALL n_cursor_left
               JR   num_curs_out
      curs_n_l:CP   9
               JR   NZ,curs_n_r
               CALL n_cursor_right
               JR   num_curs_out
      curs_n_r:CP   10
               JR   NZ,curs_n_d
               CALL n_cursor_down
               JR   num_curs_out
      curs_n_d:CP   11
               CALL Z,n_cursor_up
  num_curs_out:CALL n_scrl_u_poss
               CALL n_scrl_d_poss
               CALL print_cursor
               RET
;
 n_scrl_d_poss:LD   A,(cursor_ypos)
               CP   &15
               RET  NZ
               CALL nline
               LD   A,&14
               LD   (cursor_ypos),A
               RET
;
 n_scrl_u_poss:LD   A,(cursor_ypos)
               CP   2
               RET  NZ
               LD   HL,(t_n_d_addr)
               LD   DE,0-9
               ADD  HL,DE
               PUSH HL
               LD   DE,0-180
               ADD  HL,DE
               LD   (t_n_d_addr),HL
               CALL nuline
               POP  HL
               LD   (t_n_d_addr),HL
               LD   A,3
               LD   (cursor_ypos),A
               RET
;
n_get_curs_add:LD   A,(cursor_xpos)
               SUB  28
               SRL  A
               SRL  A
               SRL  A
               SRL  A
               LD   C,A
  n_line_start:LD   B,0
               LD   H,B
               LD   A,(cursor_ypos)
               SUB  3
               LD   L,A
               LD   E,L
               LD   D,H
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,DE
               ADD  HL,BC
               LD   DE,(t_n_d_addr)
               ADD  HL,DE
               LD   DE,0-171
               ADD  HL,DE
               RET
;
n_cursor_right:LD   A,(cursor_xpos)
               ADD  A,16
               CP   172
               LD   (cursor_xpos),A
               RET  NZ
               LD   A,28
               LD   (cursor_xpos),A
 n_cursor_down:LD   A,(cursor_ypos)
               INC  A
               LD   (cursor_ypos),A
               CP   24
               RET  NZ
               XOR  A
               LD   (cursor_ypos),A
               RET
;
 n_cursor_left:LD   A,(cursor_xpos)
               SUB  16
               CP   12
               LD   (cursor_xpos),A
               RET  NZ
               LD   A,156
               LD   (cursor_xpos),A
   n_cursor_up:LD   A,(cursor_ypos)
               SUB  1
               LD   (cursor_ypos),A
               RET  NC
               XOR  A
               LD   (cursor_ypos),A
               RET
;
      num_page:CALL clear_txt_buf
               LD   HL,&0200
               LD   (xpos),HL
               LD   B,20
         npglp:PUSH BC
               LD   HL,(t_n_d_addr)
               LD   B,0
               CALL print_hl
               EXX
               INC  HL
               INC  HL
               EXX
               LD   HL,(t_n_d_addr)
               LD   DE,get_put_tmp
               LD   BC,9
               CALL get_bytes_safe
               LD   B,9
               LD   HL,get_put_tmp
    pr_nums_lp:PUSH BC
               PUSH HL
               LD   B,0
               LD   A,(HL)
               CALL print_a_byte
               EXX
               INC  HL
               EXX
               POP  HL
               INC  HL
               POP  BC
               DJNZ pr_nums_lp
               LD   HL,(t_n_d_addr)
               LD   BC,9
               ADD  HL,BC
               LD   (t_n_d_addr),HL
               CALL print_txt_buf
               LD   HL,xpos
               LD   (HL),0
               INC  HL
               INC  (HL)
               POP  BC
               DJNZ npglp
               RET
;
         nline:CALL scroll_down
               LD   HL,&1500
               LD   (xpos),HL
               CALL clear_txt_buf
               LD   B,1
               JR   npglp
;
        nuline:CALL scroll_up
               LD   HL,&0200
               LD   (xpos),HL
               CALL clear_txt_buf
               LD   B,1
               JR   npglp
;
 n_cursor_info:CALL clear_txt_buf
               CALL n_get_curs_add
               PUSH HL
               LD   B,0
               CALL print_hl
               POP  HL
               LD   DE,get_put_tmp
               LD   BC,2
               CALL get_bytes_safe
               LD   HL,txt_buffer+8
               EXX
               LD   A,(get_put_tmp)
               CALL print_a_byte
               EXX
               INC  HL
               INC  HL
               EXX
               LD   A,(get_put_tmp)
               LD   E,A
               CALL bin_e_buf
               EXX
               INC  HL
               INC  HL
               EXX
               LD   A,(get_put_tmp)
               CP   32
               JR   C,n_info_line
               CP   128
               CALL C,print_a_buf
   n_info_line:LD   HL,5888
               LD   (xpos),HL
               JP   print_txt_buf

;
; Graph of memory pages
;
  do_mem_graph:LD   DE,graph_page_msg
               CALL input_number
               CP   128
               RET  Z
               SET  2,(IY+refresh_flags)
               SET  5,(IY+misc_flags)
               CP   127
               JP   Z,graph_64k
               LD   A,L
               AND  A
               JR   NZ,norm_graph
               DEC  H
               JP   Z,graph_256
               DEC  H
               JP   Z,graph_512
    norm_graph:IN   A,(lmpr)
               EX   AF,AF'
               LD   A,L
               AND  %00011111
               LD   (graphed_page+1),A
               OR   rom0_bitval
               DI
               OUT  (lmpr),A
               LD   IX,buffer2
               XOR  A
               LD   L,A
               LD   H,0
               LD   D,0
               EXX
               LD   L,A
               LD   H,A
  compile_page:LD   BC,&2F00
  compile_line:CP   (HL)
               JR   Z,nil_byte
               INC  C
      nil_byte:INC  L
               JR   Z,graph_hi
 not_done_line:DJNZ compile_line
               JP   mult_up
      graph_hi:INC  H
               BIT  6,H
               JP   Z,not_done_line
;
       mult_up:LD   A,C
               EXX
               LD   E,A
               ADD  HL,DE
               EXX
               ADD  A,A
               ADD  A,C
               SRL  C
               SRL  C
               ADD  A,C
               LD   (IX+0),A
               INC  IX
               XOR  A
               BIT  6,H
               JP   Z,compile_page
;
               LD   (IX+0),A
               LD   (IX+1),A
               LD   (IX+2),A
;
               EX   AF,AF'
               OUT  (lmpr),A
               EXX
               PUSH HL
               CALL clear_main_scr
               CALL clear_inp_line
               CALL clear_txt_buf
               CALL print_buf_pop
               DM   "Graph of page"
               DB   " "+128
  graphed_page:LD   A,0
               CALL print_min_byte
               CALL print_buf_pop
               DM   ", 1K units,"
               DB   " "+128
               POP  DE
               LD   HL,16384
               AND  A
               SBC  HL,DE
               CALL print_hl_min
               CALL print_buf_pop
               DM   " bytes free"
               DB   "."+128
               CALL print_buf_bot
               LD   HL,scale_pattern1
               CALL draw_scale
               JP   draw_graph
;
     graph_64k:CALL show_compiling
               LD   IX,buffer2
               LD   HL,0
        lp_64k:PUSH HL
               LD   DE,buffer
               LD   BC,187
               CALL get_bytes_safe
               LD   BC,&BB00
               XOR  A
               LD   HL,buffer
   compile_64l:CP   (HL)
               JR   Z,nil_64_byte
               INC  C
   nil_64_byte:INC  HL
               DJNZ compile_64l
               POP  HL
               LD   DE,187
               ADD  HL,DE
               LD   (IX+0),C
               INC  IX
               AND  A
               LD   DE,65450
               SBC  HL,DE
               EX   AF,AF'
               ADD  HL,DE
               EX   AF,AF'
               JP   NZ,lp_64k
; Correctly handle the overlap to section A
               LD   DE,buffer
               LD   BC,86
               CALL get_bytes_safe
               LD   BC,&5600
               XOR  A
               LD   HL,buffer
    correct_64:CP   (HL)
               JR   Z,nil_correct
               INC  C
   nil_correct:INC  HL
               DJNZ correct_64
               LD   (IX+0),C
               LD   (IX+1),A
;
               LD   HL,0
               LD   D,H
               EXX
               LD   HL,buffer2
               LD   DE,graph_width
 scale_down_lp:LD   A,(HL)
               EXX
               LD   E,A
               ADD  HL,DE
               EXX
               CP   1
               JR   Z,show_single
               SRL  A
               SRL  A
               SRL  A
               LD   B,A
               SRL  A
               ADD  A,B
               LD   B,A
               RRA
               RRA
               LD   A,(HL)
               SBC  A,B
   show_single:LD   (HL),A
               INC  HL
               DEC  DE
               LD   A,D
               OR   E
               JR   NZ,scale_down_lp
;
               EXX
               PUSH HL
               CALL clear_main_scr
               CALL clear_inp_line
               CALL clear_txt_buf
               CALL print_buf_pop
               DM   "Current 64K, 1K units,"
               DB   " "+128
               POP  DE
               LD   HL,0
               AND  A
               SBC  HL,DE
               CALL print_hl_min
               CALL print_buf_pop
               DM   " bytes free"
               DB   "."+128
               CALL print_buf_bot
               LD   HL,scale_pattern3
               CALL draw_scale
               JP   draw_graph
;
     graph_big:DI
               LD   (final_big_page+1),A
               LD   A,B
               LD   (big_per_block+1),A
               LD   A,C
               LD   (extra_divide),A
               LD   (across_big_lp+1),HL
               LD   (graph_stack),SP
               LD   SP,graph_stack
               IN   A,(lmpr)
               PUSH AF
               LD   IX,buffer2
               LD   A,rom0_bitval
   page_big_lp:PUSH AF
               OUT  (lmpr),A
               XOR  A
               LD   L,A
               LD   H,A
 big_per_block:LD   B,0
 inter_page_lp:PUSH BC
 across_big_lp:LD   BC,0
               LD   DE,0
   line_big_lp:XOR  A
               CP   (HL)
               JR   Z,nil_big
               BIT  6,H
               JR   NZ,done_big_line
               INC  DE
       nil_big:INC  HL
               DEC  BC
               LD   A,B
               OR   C
               JP   NZ,line_big_lp
 done_big_line:PUSH HL
               DEC  DE
               LD   A,D
               OR   E
               LD   A,1
               JR   Z,single_big
               INC  DE
  extra_divide:JR   skip_divide
               SRL  D
               RR   E
   skip_divide:SRL  D
               RR   E
               SRL  D
               RR   E
               LD   L,E
               LD   H,D
               SRL  D
               RR   E
               SRL  D
               RR   E
               SRL  D
               RR   E
               AND  A
               SBC  HL,DE
               SRL  D
               RR   E
               AND  A
               SBC  HL,DE
               LD   A,L
    single_big:LD   (IX+0),A
               INC  IX
               POP  HL
               POP  BC
               DJNZ inter_page_lp
               POP  AF
               INC  A
final_big_page:CP   0
               JR   NZ,page_big_lp
               POP  AF
               OUT  (lmpr),A
               LD   SP,(graph_stack)
               CALL clear_main_scr
               CALL clear_inp_line
               CALL clear_txt_buf
               RET

     graph_256:CALL show_compiling
               LD   A,rom0_bitval+16
               LD   B,22
               LD   C,24
               LD   HL,745
               CALL graph_big
               CALL print_buf_pop
               DM   "Pages 0 to 15 (256K), 16K units"
               DB   "."+128
               CALL print_buf_bot
               LD   HL,scale_pattern1
               CALL draw_scale
               JP   draw_graph

     graph_512:CALL show_compiling
               LD   A,rom0_bitval+32
               LD   B,11
               LD   C,62
               LD   HL,1490
               CALL graph_big
               CALL print_buf_pop
               DM   "Pages 0 to 31 (512K), 16K units"
               DB   "."+128
               CALL print_buf_bot
               LD   HL,scale_pattern2
               CALL draw_scale

    draw_graph:LD   HL,graph_base
               LD   IX,buffer2
               LD   DE,graph_width
               LD   C,%11000000
 out_draw_loop:PUSH HL
               LD   B,(IX+0)
               INC  IX
               INC  B
               DEC  B
               JR   Z,no_bits
  in_draw_loop:LD   A,C
               OR   (HL)
               LD   (HL),A
               CALL mode4_pix_up
               DJNZ in_draw_loop
       no_bits:POP  HL
               RRC  C
               RRC  C
               LD   A,L
               ADC  A,0
               LD   L,A
               DEC  DE
               LD   A,D
               OR   E
               JR   NZ,out_draw_loop
;
               EI
    graph_wait:HALT
               LD   A,(IY+key)
               AND  A
               JR   Z,graph_wait
               CP   "g"
               JP   Z,do_mem_graph
               CP   "G"
               JP   Z,do_mem_graph
               RET

    draw_scale:LD   (length_loop+1),HL
               LD   DE,graph_base+256
               LD   B,6
    thick_loop:PUSH BC
               PUSH DE
               LD   A,8
   length_loop:LD   HL,scale_pattern1
               LD   BC,11
               LDIR
               DEC  A
               JR   NZ,length_loop
               POP  DE
               LD   HL,128
               ADD  HL,DE
               EX   DE,HL
               POP  BC
               DJNZ thick_loop
               RET

show_compiling:CALL clear_inp_line
               CALL print_at_pop
               DB   0,23
               DM   "Compiling graph data..."
               DB   " "+128
               RET

               DS   20
   graph_stack:DW   0
scale_pattern1:DW   &AAAA,&AAAA,&ABAA,&BBBB,&BBBB,&00BB
scale_pattern2:DW   &AAAA,&BBAB,&BABB,&AAAA,&BBBB,&00BB
scale_pattern3:DW   &AEAA,&AAEE,&BABB,&EEAA,&ABEA,&00BB
     graph_buf:DS   graph_width
;
; Print extra system state information
;
 do_state_info:CALL let_go
               CALL clear_main_scr
               CALL print_at_pop
               DB   0,2
               DM   "Extra System State Information"
               DB   " "+128
               CALL print_at_pop
               DB   0,3
               DM   "------------------------------"
               DB   " "+128
               CALL print_at_pop
               DB   0,5
               DM   "PALETTE:"
               DB   " "+128
               CALL print_at_pop
               DB   0,6
               DM   "0 to 7 : "
               DB   " "+128
               CALL print_at_pop
               DB   0,7
               DM   "8 to 15: "
               DB   " "+128
               CALL print_at_pop
               DB   0,9
               DM   "INTERRUPTS:"
               DB   " "+128
;
               CALL print_at_pop
               DB   0,13
               DM   "Line interrupt port "
               DB   "("+128
               CALL print_at_pop
               DB   96,13
               DM   ") holds line"
               DB   " "+128
;
               CALL print_at_pop
               DB   0,15
               DM   "PORTS"
               DB   ":"+128
               CALL print_at_pop
               DB   0,16
               DM   "LMPR   (   ) holds       %"
               DB   " "+128
               CALL print_at_pop
               DB   0,17
               DM   "HMPR   (   ) holds       %"
               DB   " "+128
               CALL print_at_pop
               DB   0,18
               DM   "VMPR   (   ) holds       %"
               DB   " "+128
               CALL print_at_pop
               DB   0,19
               DM   "BORDER (   ) holds       %"
               DB   " "+128
;
 refresh_state:CALL clear_txt_buf
               LD   HL,&0A00
               LD   (xpos),HL
               LD   A,(IY+int_delay)
               CP   255
               LD   HL,no_int_msg
               JR   Z,got_f_int_msg
               AND  A
               LD   HL,halt_int_msg
               JR   Z,got_f_int_msg
               LD   HL,int_del_msg1
               CALL print_buf_msg
               LD   A,(IY+int_delay)
               CALL print_min_byte
               LD   HL,int_del_msg2
 got_f_int_msg:CALL print_buf_msg
               CALL print_txt_buf
;
               CALL clear_txt_buf
               LD   HL,&0B00
               LD   (xpos),HL
               LD   A,(IY+line_ints)
               AND  A
               LD   HL,no_l_int_msg
               JR   Z,got_l_int_msg
               LD   A,(IY+line_ints)
               CALL print_min_byte
               LD   HL,some_l_int_msg
 got_l_int_msg:CALL print_buf_msg
               CALL print_txt_buf
;
   done__ints2:LD   DE,&0628
               LD   HL,mon_palette
               CALL print_8_pots
               LD   DE,&0728
               LD   HL,mon_palette+8
               CALL print_8_pots
;
               LD   A,line_int
               LD   DE,&0D54
               CALL print_byte_at
               LD   A,(IY+port_line_int)
               LD   DE,&0D94
               CALL print_byte_at
               LD   A,"."
               RST  8

               LD   C,(IY+port_lmpr)
               LD   B,lmpr
               LD   D,16
               CALL print_port_val
               LD   C,(IY+port_hmpr)
               LD   B,hmpr
               LD   D,17
               CALL print_port_val
               LD   C,(IY+port_vmpr)
               LD   B,vmpr
               LD   D,18
               CALL print_port_val
               LD   C,(IY+port_border)
               LD   B,border
               LD   D,19
               CALL print_port_val
;
               CALL clear_txt_buf
               CALL print_buf_pop
               DM   "Execute until"
               DB   " "+128
               LD   HL,(cond_printer+1)
               CALL call_ind_hl
               LD   HL,&1500
               LD   (xpos),HL
               CALL print_txt_buf
;
  state_out_lp:CALL update_info
               CALL clear_inp_line
               LD   HL,state_out_lp
               PUSH HL
      state_lp:HALT
               BIT  2,(IY+refresh_flags)
               RES  2,(IY+refresh_flags)
               JR   Z,no_state_ref
               POP  HL
               JP   refresh_state
  no_state_ref:LD   A,(IY+key)
               AND  A
               JR   Z,state_lp
               CALL main_keys
               CALL common_keys
               INC  A
               JR   NZ,state_lp
               POP  HL
               SET  2,(IY+refresh_flags)
               RET
;
    no_int_msg:DM   "No interrupts occur at all."
               DB   " "+128
  halt_int_msg:DM   "Interrupts only occur at HALTs."
               DB   " "+128
  int_del_msg1:DM   "Interrupts occur every"
               DB   " "+128
  int_del_msg2:DM   " REAL interrupts"
               DB   "."+128
  no_l_int_msg:DM   "No line interrupts occur."
               DB   " "+128
some_l_int_msg:DM   " line interrupt(s) per frame "
               DM   "interrupt"
               DB   "."+128
;
; Print 8 bytes pointed to by HL
  print_8_pots:LD   (xpos),DE
               LD   B,8
 print_pots_lp:PUSH BC
               PUSH HL
               LD   A,(HL)
               CALL print_byte
               LD   A," "
               RST  8
               POP  HL
               POP  BC
               INC  HL
               DJNZ print_pots_lp
               RET
;
; Print port number (in A) and port contents (in C)
; to line B of the display (in set format)
print_port_val:LD   E,32
               LD   (xpos),DE
               LD   A,B
               PUSH BC
               CALL print_byte
               LD   A,76
               LD   (xpos),A
               POP  BC
               LD   A,C
               PUSH AF
               CALL print_byte
               LD   A,104
               LD   (xpos),A
               POP  AF
               CALL print_binary
               RET

;
; Fill memory area with a given byte
;
do_fill_region:LD   DE,fill_f_msg
               CALL input_number
               RET  NZ
               PUSH HL
               LD   DE,fill_l_msg
               CALL input_number
               POP  DE
               RET  NZ
               LD   A,H
               OR   L
               RET  Z
               LD   (IY+refresh_flags),255
               PUSH DE
               PUSH HL
               LD   DE,fill_w_msg
               CALL input_number
               POP  BC
               POP  DE
               RET  NZ
               LD   A,L
               LD   (fill_with_byte+1),A
               LD   A,C
               PUSH BC
               LD   HL,buffer
fill_with_byte:LD   BC,0
   fill_buffer:LD   (HL),C
               INC  HL
               DJNZ fill_buffer
               AND  A
               JR   Z,do_fill_hi
               LD   C,A
               LD   B,0
               PUSH BC
               PUSH DE
               LD   HL,buffer
               CALL put_bytes_safe
               POP  HL
               POP  BC
               ADD  HL,BC
               EX   DE,HL
    do_fill_hi:POP  BC
               INC  B
               DEC  B
               RET  Z
    fill_hi_lp:PUSH BC
               PUSH DE
               LD   HL,buffer
               LD   BC,256
               CALL put_block_safe
               POP  DE
               INC  D
               POP  BC
               DJNZ fill_hi_lp
               RET

;
; Copy memory blocks to another location
;
 do_copy_block:LD   DE,copy_f_msg
               CALL input_number
               RET  NZ
               LD   (copy_from_addr+1),HL
               LD   DE,copy_t_msg
               CALL input_number
               RET  NZ
               LD   (copy_to_addr+1),HL
               LD   DE,copy_l_msg
               CALL input_number
               RET  NZ
               LD   A,H
               OR   L
               RET  Z
               LD   C,L
               LD   B,H
copy_from_addr:LD   HL,0
  copy_to_addr:LD   DE,0
               SBC  HL,DE
               JR   C,copy_lddr
               ADD  HL,DE
  copy_ldir_hi:LD   A,B
               AND  A
               JR   Z,ldir_hi_done
    ldir_hi_lp:PUSH BC
               PUSH DE
               PUSH HL
               PUSH DE
               LD   BC,256
               LD   DE,buffer2
               CALL get_bytes_safe
               LD   HL,buffer2
               POP  DE
               LD   BC,256
               CALL put_block_safe
               POP  HL
               POP  DE
               POP  BC
               INC  H
               INC  D
               DJNZ ldir_hi_lp
  ldir_hi_done:LD   A,C
               AND  A
               RET  Z
     copy_rest:PUSH BC
               PUSH DE
               LD   DE,buffer2
               CALL get_bytes_safe
               LD   HL,buffer2
               POP  DE
               POP  BC
               JP   put_bytes_safe
;
     copy_lddr:ADD  HL,DE
               ADD  HL,BC
               EX   DE,HL
               ADD  HL,BC
               EX   DE,HL
  copy_lddr_hi:LD   A,B
               AND  A
               JR   Z,lddr_hi_done
    lddr_hi_lp:PUSH BC
               DEC  H
               DEC  D
               PUSH DE
               PUSH HL
               PUSH DE
               LD   DE,buffer2
               LD   BC,256
               CALL get_bytes_safe
               LD   HL,buffer2
               POP  DE
               LD   BC,256
               CALL put_block_safe
               POP  HL
               POP  DE
               POP  BC
               DJNZ lddr_hi_lp
  lddr_hi_done:LD   A,C
               AND  A
               RET  Z
               SBC  HL,BC
               EX   DE,HL
               AND  A
               SBC  HL,BC
               EX   DE,HL
               JR   copy_rest

;
; Search memory (a page or range) for numbers
;
 do_mem_search:LD   DE,search_rng_msg
               CALL input_number
               CP   128
               RET  Z
               LD   (IY+refresh_flags),255
               CP   127
               JP   Z,search_curr
               LD   A,H
               AND  A
               LD   A,1
               JR   Z,search_pages
               LD   L,0
               DEC  H
               LD   A,16
               JR   Z,search_pages
               LD   A,32
  search_pages:LD   (srch_page_init+2),A
               LD   A,L
               AND  %00011111
               LD   (last_page+1),A
               OR   %00100000
               LD   (srch_page_init+1),A
               CALL search_inputs
               CP   128
               RET  Z
               LD   IX,search_buffer
               CALL clear_inp_line
               LD   HL,&0400
               LD   (xpos),HL
               IN   A,(lmpr)
               LD   (monitor_lmpr),A
               DI
               LD   (search_stack+1),SP
               LD   SP,high_stack
srch_page_init:LD   BC,0
search_page_lp:LD   A,(srch_page_init+1)
               OUT  (lmpr),A
               LD   HL,0
               LD   BC,16384
               LD   (search_size+1),BC
 find_start_lp:LD   A,(IX+1)
   search_size:LD   BC,0
               CPIR
               JP   PO,searched_page
               LD   (search_size+1),BC
               PUSH IX
               PUSH HL
               INC  IX
               INC  IX
 search_blk_lp:LD   A,(IX+0)
               CP   123
               JR   Z,found_p_match
               AND  A
               INC  IX
               INC  IX
               INC  HL
               JR   Z,search_blk_lp
               LD   A,(IX-1)
               DEC  HL
               CP   (HL)
               INC  HL
               JR   Z,search_blk_lp
               POP  HL
               POP  IX
               JP   find_start_lp
 searched_page:LD   HL,srch_page_init+1
               INC  (HL)
               INC  HL
               DEC  (HL)
               JR   NZ,search_page_lp
               LD   A,(monitor_lmpr)
               OUT  (lmpr),A
  search_stack:LD   SP,0
               EI
   search_done:LD   DE,&1700
               LD   HL,search_end_msg
               CALL print_msg_at
               CALL let_go
               JP   wait_key_ret
;
  abort_search:LD   A,(monitor_lmpr)
               OUT  (lmpr),A
               LD   SP,(search_stack+1)
               EI
               RET
;
 found_p_match:LD   BC,(srch_page_init+1)
               CALL show_srch_page
               POP  HL
               PUSH HL
               PUSH BC
               DEC  HL
               PUSH HL
               DEC  HL
               DEC  HL
               DEC  HL
               LD   DE,buffer2
               LD   BC,10
               LDIR
               POP  HL
               POP  BC
               CALL show_found
               POP  HL
               POP  IX
               JP   find_start_lp

 found_c_match:POP  HL
               PUSH HL
               PUSH BC
               LD   DE,buffer+1
               AND  A
               SBC  HL,DE
               LD   DE,(searched_curr+1)
               ADD  HL,DE
               PUSH HL
               DEC  HL
               DEC  HL
               DEC  HL
               LD   DE,buffer+270
               LD   BC,10
               CALL get_bytes_safe
               LD   HL,buffer+270
               LD   DE,buffer2
               LD   BC,10
               LDIR
               POP  HL
               POP  BC
               CALL show_found
               POP  HL
               POP  IX
               JR   find_cur_start

   search_curr:CALL search_inputs
               CP   128
               RET  Z
               LD   A,255
               LD   (cur_page),A
               LD   (search_stack+1),SP
               LD   IX,search_buffer
               CALL clear_inp_line
               LD   HL,&0400
               LD   (xpos),HL
               IN   A,(lmpr)
               LD   (monitor_lmpr),A
               LD   HL,0
search_curr_lp:LD   (searched_curr+1),HL
               LD   BC,256
               LD   (curr_size+1),BC
               LD   C,15
               LD   DE,buffer
               PUSH DE
               CALL get_bytes_safe
               POP  HL
find_cur_start:LD   A,(IX+1)
     curr_size:LD   BC,0
               CPIR
               JP   PO,searched_curr
               LD   (curr_size+1),BC
               PUSH IX
               PUSH HL
               INC  IX
               INC  IX
search_cur_blk:LD   A,(IX+0)
               CP   123
               JP   Z,found_c_match
               AND  A
               INC  IX
               INC  IX
               INC  HL
               JR   Z,search_cur_blk
               LD   A,(IX-1)
               DEC  HL
               CP   (HL)
               INC  HL
               JR   Z,search_cur_blk
               POP  HL
               POP  IX
               JP   find_cur_start
 searched_curr:LD   HL,0
               INC  H
               JR   NZ,search_curr_lp
               JP   search_done

    show_found:IN   A,(lmpr)
               PUSH AF
               PUSH BC
               PUSH HL
               LD   A,(monitor_lmpr)
               OUT  (lmpr),A
               CALL check_page
               POP  HL
               CALL print_word
               LD   A," "
               RST  8
               LD   A," "
               RST  8
               LD   HL,buffer2
               SET  4,(IY+misc_flags)
               LD   B,3
               CALL byte_spaces
               RES  4,(IY+misc_flags)
               LD   A," "
               RST  8
               LD   B,6
               CALL byte_spaces
               LD   HL,ypos
               INC  (HL)
               POP  BC
               POP  AF
               OUT  (lmpr),A
               RET
;
   byte_spaces:PUSH BC
               PUSH HL
               LD   A,(HL)
               CALL print_byte
               LD   A," "
               RST  8
               POP  HL
               POP  BC
               INC  HL
               DJNZ byte_spaces
               RET
;
      cur_page:DB   0
show_srch_page:LD   A,C
               LD   (cur_page),A
     last_page:CP   0
               RET  Z
               LD   (last_page+1),A
               IN   A,(lmpr)
               PUSH AF
               LD   A,(monitor_lmpr)
               OUT  (lmpr),A
               CALL check_page
               CALL show_page_msg
               POP  AF
               OUT  (lmpr),A
               RET
;
 show_page_msg:LD   A,(ypos)
               CP   5
               RET  Z
               CALL print_scr_pop
               DM   "** Page"
               DB   " "+128
               LD   A,(cur_page)
               AND  %00011111
               LD   L,A
               LD   H,0
               CALL tens_byte
               CALL print_scr_pop
               DB   "*","*"+128
               LD   HL,ypos
               INC  (HL)
               RET
;
    check_page:LD   A,(ypos)
               CP   22
               JR   NZ,not_scr_bot
               CALL clear_inp_line
               CALL print_at_pop
               DB   0,23
               DM   "Press any key for more"
               DB   "."+128
               EI
     page_wait:HALT
               LD   A,(IY+key)
               AND  A
               JR   Z,page_wait
               INC  A
               JP   Z,abort_search
               DI
               LD   HL,section_b+4096
               LD   B,L
               LD   A,160
               CALL clear_scr_lp
               LD   A,4
               LD   (ypos),A
               XOR  A
               LD   (xpos),A
               LD   A,(cur_page)
               INC  A
               CALL NZ,show_page_msg
   not_scr_bot:XOR  A
               LD   (xpos),A
               RET
;
  monitor_lmpr:DB   0

 search_inputs:CALL clear_main_scr
               CALL clear_inp_line
               DI
               SET  4,(IY+misc_flags)
               CALL print_at_pop
               DB   0,2
               DM   "Find:"
               DB   " "+128
               RES  4,(IY+misc_flags)
               EI
               LD   IX,search_buffer
               LD   B,9
 search_inp_lp:PUSH BC
               LD   DE,search_inp_msg
               SET  6,(IY+misc_flags)
               LD   HL,(xpos)
               PUSH HL
               PUSH IX
               CALL input_number
               POP  IX
               POP  DE
               LD   (xpos),DE
               POP  BC
               CP   128
               RET  Z
               CP   127
               JR   NZ,inp_search_num
               LD   (IX+0),123
               LD   A,L
               CP   "*"
               LD   A,(IX-2)
               RET  NZ
               CP   128
               JR   Z,inp_search_skp
               CALL print_comma
               LD   A,"A"
               RST  8
               LD   A,"N"
               RST  8
               LD   A,"Y"
               RST  8
               LD   (IX+0),0
               JR   inp_search_nxt
inp_search_num:LD   (IX+0),255
               LD   (IX+1),L
               CALL print_comma
               INC  H
               DEC  H
               JR   NZ,inp_search_wrd
               LD   A,L
               PUSH BC
               CALL print_byte
               POP  BC
               JR   inp_search_nxt
inp_search_wrd:LD   (IX+2),255
               LD   (IX+3),H
               PUSH BC
               CALL print_word
               POP  BC
               DEC  B
               INC  IX
               INC  IX
inp_search_nxt:INC  IX
               INC  IX
inp_search_skp:DEC  B
               JP   P,search_inp_lp
               INC  B
               RET  Z
               LD   (IX-2),123
               RET
;
   print_comma:LD   A,(IX-2)
               CP   128
               RET  Z
               LD   A,&2C
               JP   pr_chr
;
               DW   &8080
 search_buffer:DS   22


;
; Input messages for the various options
;
  set_xmpr_msg:DM   "New XMPR page (0-31"
               DB   ")"+128
  set_mode_msg:DM   "New screen mode (1-4"
               DB   ")"+128
   in_byte_msg:DM   "Port to read fro"
               DB   "m"+128
  out_byte_msg:DM   "Port to send byte t"
               DB   "o"+128
 send_byte_msg:DM   "Byte to sen"
               DB   "d"+128
    exec_n_msg:DM   "Execute how many instruction"
               DB   "s"+128
exec_until_msg:DM   "Execute until PC reache"
               DB   "s"+128
exec_trace_msg:DM   "Trace until PC reache"
               DB   "s"+128
bound_from_msg:DM   "Lower boundry addres"
               DB   "s"+128
  bound_to_msg:DM   "Upper boundry addres"
               DB   "s"+128
input_port_msg:DM   "Watch reads from which por"
               DB   "t"+128
outpt_port_msg:DM   "Watch writes to which por"
               DB   "t"+128
 int_delay_msg:DM   "Interrupt delay (-1, 0 or 1-254"
               DB   ")"+128
  line_int_msg:DM   "Line interrupts per frame interrup"
               DB   "t"+128
  exec_mem_msg:DM   "Memory location to watc"
               DB   "h"+128
set_palett_msg:DM   "Palette position to chang"
               DB   "e"+128
   set_col_msg:DM   "New colour valu"
               DB   "e"+128
    fill_f_msg:DM   "Start of block to fil"
               DB   "l"+128
    fill_l_msg:DM   "Block lengt"
               DB   "h"+128
    fill_w_msg:DM   "Fill with byt"
               DB   "e"+128
    copy_f_msg:DM   "Copy block fro"
               DB   "m"+128
    copy_t_msg:DM   "Copy block t"
               DB   "o"+128
    copy_l_msg:DM   "Block lengt"
               DB   "h"+128
      push_msg:DM   "Value to PUSH onto stac"
               DB   "k"+128
search_rng_msg:DM   "Page/range to searc"
               DB   "h"+128
search_inp_msg:DM   "Input valu"
               DB   "e"+128
graph_page_msg:DM   "Input page/range to grap"
               DB   "h"+128
               DB   "."+128
      eval_msg:DM   "Input number/expressio"
               DB   "n"+128
     protected:DM   "PROTECT ON "
     writeable:DM   "PROTECT OFF"
  txt_addr_msg:DM   "Input Text Addres"
               DB   "s"+128
  new_addr_msg:DM   "Input new addres"
               DB   "s"+128
  num_addr_msg:DM   "Input Number addres"
               DB   "s"+128
 inp_bytes_msg:DM   "Valu"
               DB   "e"+128
  dis_addr_msg:DM   "Input Code addres"
               DB   "s"+128
search_end_msg:DM   "Search complete, press any key"
               DB   "."+128
