;
; D I S A S S E M B L E R
;
;
    opcode_pos:EQU  23
;
;
   disassemble:XOR  A
               LD   (using_ind_indx+1),A
               CALL do_disassemble
               PUSH AF
               PUSH BC
               PUSH DE
               PUSH HL
               BIT  1,(IY+misc_flags)
               JR   Z,leave_disassem
using_ind_indx:LD   A,0
               AND  A
               JR   Z,leave_disassem
               LD   HL,txt_buffer+40
               LD   A,(HL)
               CP   " "
               JR   NZ,leave_disassem
               LD   (HL),"["
               INC  HL
               EXX
               LD   HL,(saved_index)
               LD   DE,fetched_index+1
               LD   BC,1
               RST  16
 fetched_index:LD   A,0
               CALL print_min_byte
               EXX
               LD   (HL),"]"
               EXX
leave_disassem:POP  HL
               POP  DE
               POP  BC
               POP  AF
               RET
;
do_disassemble:LD   (addr),HL
               LD   DE,dis_buffer
               LD   BC,4
               RST  16
               CALL clear_txt_buf
               LD   HL,txt_buffer+opcode_pos
               LD   DE,pos_stack
               EXX
;:
               LD   IX,dis_buffer
               LD   A,(IX+0)
;:
               CP   #ED
               LD   C,1
               LD   HL,ed_string
               JR   Z,got_string_inc
               CP   #CB
               LD   HL,cb_string
               JR   Z,got_string_inc
               CP   #DD
               SET  2,C
               JR   Z,index_prefix
               CP   #FD
               LD   C,0
               LD   HL,normal_string
               JR   NZ,got_string
               LD   C,%00000111
  index_prefix:LD   HL,index_table
               INC  IX
               LD   A,(IX+0)
               LD   B,A
               AND  %00011111
               LD   E,A
               LD   D,0
               ADD  HL,DE
               LD   A,B
               RRA
               RRA
               AND  %00111000
               OR   %01000110
               LD   (self_mod+1),A
      self_mod:DB   203,0
               JR   Z,prefix_only
               LD   A,(IX+0)
               CP   #CB
               LD   HL,normal_string
               JR   NZ,got_string
               LD   HL,cb_string
               LD   A,(IX+2)
               JR   got_ddcb_fdcb
got_string_inc:INC  IX
    got_string:LD   A,(IX+0)
 got_ddcb_fdcb:LD   (opcode),A
               CALL parse_str
               CP   5
               JR   C,indx_noch
               SUB  4
               BIT  2,C
               JR   Z,indx_noch
               INC  A
     indx_noch:RR   C
               ADC  A,0
               PUSH AF
               LD   HL,txt_buffer+6
               EXX
               LD   HL,dis_buffer
    print_code:PUSH AF
               LD   A,(HL)
               PUSH HL
               LD   B,255
               CALL print_a_byte
               EXX
               INC  HL
               EXX
               POP  HL
               INC  HL
               POP  AF
               DEC  A
               JR   NZ,print_code
               LD   HL,txt_buffer
               EXX
               LD   B,255
               LD   HL,(addr)
               PUSH HL
               CALL print_hl
               POP  HL
               POP  AF
               LD   C,A
               LD   B,0
               ADD  HL,BC
               LD   DE,txt_buffer
               AND  A
               RET
   prefix_only:LD   HL,unused_string
               CALL got_string
               SCF
               RET
;
; Set bits indicate which instructions can have an index
; prefix - 1 bit per opcode.
   index_table:DB   #08,#8A,#0A,#8A,#3E,#BE,#3E,#08
               DB   #08,#8B,#0A,#4A,#3E,#3E,#3E,#08
               DB   #08,#08,#08,#08,#3E,#3E,#36,#08
               DB   #00,#83,#00,#00,#3C,#3C,#3C,#00

     parse_str:LD   A,(HL)
               INC  HL
               CP   128
               JR   NC,new_rot_mask
               CP   "["
               JP   Z,sect_start
               CP   "]"
               JR   Z,close_str
               CP   "a"
               JR   NC,sub_str
               CP   "^"
               JR   Z,parse_str
               CP   "A"
               JR   NC,print_char
               CP   "*"
               JR   Z,absolute_char
               CP   "!"
               JR   Z,find_sect_end
               CP   " "
               JR   Z,str_tab
               CP   "0"
               JR   NC,str_end
               CP   "%"
               JR   Z,str_function
    print_char:CALL print_a_buf
               JR   parse_str
;:
     close_str:EXX
               DEC  DE
               EXX
               JR   parse_str
;:
 absolute_char:LD   A,(HL)
               INC  HL
               JR   print_char
;:
 find_sect_end:CALL skip_section
               JR   close_str
  skip_section:LD   A,(HL)
               INC  HL
               CP   "["
               CALL Z,skip_section
               CP   "]"
               JR   NZ,skip_section
               XOR  A
               RET
;:
       str_end:SUB  48
               RET
;:
       str_tab:EXX
               LD   HL,txt_buffer+opcode_pos+5
               EXX
               JR   parse_str
;:
  new_rot_mask:LD   E,A
               AND  %00111000
               RRA
               RRA
               RRA
               LD   B,A
               LD   A,(opcode)
               JR   Z,no_rotates
      shift_op:RR   A
               DJNZ shift_op
    no_rotates:AND  E
               AND  %00000111
               CALL put_pos
               JR   parse_str
;:
  str_function:LD   A,(HL)
               INC  HL
               PUSH HL
               LD   DE,routine_tab
               CALL look_up
               CALL ind_hl
               POP  HL
               JP   parse_str
        ind_hl:JP   (HL)
;:
       sub_str:PUSH HL
               LD   DE,string_tab
               CALL look_up
;:             Recursively call...
               CALL parse_str
               POP  HL
               JP   parse_str
;:
       look_up:SUB  "a"
               ADD  A,A
               LD   L,A
               LD   H,0
               ADD  HL,DE
               LD   E,(HL)
               INC  HL
               LD   H,(HL)
               LD   L,E
               RET
;:
    sect_start:EXX
               LD   A,(DE)
               INC  DE
               EXX
               AND  A
               JP   Z,parse_str
               LD   E,A
  find_section:LD   A,(HL)
               INC  HL
               CP   "]"
               JP   Z,close_str
               CP   "["
               CALL Z,skip_section
               CP   "!"
               JR   Z,found_delim
               CP   "^"
               JR   NZ,find_section
   found_delim:DEC  E
               JR   NZ,find_section
               JP   parse_str
;:
; Routines a,b,: .. ,n
   routine_tab:DW   print_ins_word
               DW   binary_num
               DW   print_bit_pos
               DW   displacement
               DW   relative_addr
               DW   rst_addr
               DW   0
               DW   ind_hl_h
               DW   index_return
               DW   signal_ix
               DW   signal_iy
               DW   ind_hl_l
               DW   print_byte_2_4
               DW   print_byte_2_3
;:
    string_tab:DW   string_a,string_b,string_c,string_d
               DW   string_e,string_f,string_g,string_h
               DW   string_i,string_j,string_k,string_l,0
               DW   string_n,string_o,string_p,string_q
               DW   string_r,string_s,0,0
;:
      string_a:DB   159
               DM   "[ADD!ADC!SUB!SBC!AND!XOR!OR!"
               DM   "CP] [A,!A,!!A,]0"
      string_b:DM   "[IN!DE]C 0"
      string_c:DM   "[!^^%c,]0"
      string_d:DM   "[BC!DE!q!SP]0"
      string_e:DB   179
               DM   "["
               DB   159
               DM   "[RLC!RRC!RL* !RR* !SLA!SRA!SLL!SRL]
               DM   "!BIT!RES!SET]0"
      string_f:DB   159
      string_g:DM   "[NZ!Z!NC!C!PO!PE!P!M]0"
      string_h:DM   "%i[H0]q*h0"
      string_i:DM   "(q%i[)0]%d)0"
      string_j:DM   "LD 0"
      string_k:DB   163
               DM   "[BC!DE!q!AF]0"
      string_l:DM   "%i[L0]q*l0"
      string_n:DM   "%n0"
      string_o:DB   161
               DM   "([BC!DE])0"
      string_p:DM   "(%a)0"
      string_q:DM   "%i[HL!IX%j!IY%k]0"
      string_r:DM   "[B!C!D!E!H!L!i!A]0"
      string_s:DM   "[B!C!D!E!h!l!i!A]0"
;:
;: Simon, tidy these up!!
;:
 normal_string:DB   179
               DM   "["
               DB   135
               DM   "["
               DB   169
               DM   "["
               DB   161
               DM   "["
               DB   153
               DM   "[NOP!EX AF,AF']"
               DM   "!"
               DB   153
               DM   "[DJNZ!JR] %e2]!JR "
               DB   155
               DM   "g,%e2]"
               DM   "!"
               DB   153
               DM   "[j"
               DB   163
               DM   "d,%a3!ADD q,"
               DB   163
               DM   "d]"
               DM   "!"
               DM   "j"
               DB   169
               DM   "["
               DB   153
               DM   "[o,A!A,o"
               DM   "]!"
               DB   155
               DM   "[p,q!q,p!p,A!A,p]3]"
               DM   "!"
               DB   153
               DM   "b"
               DB   163
               DM   "d"
               DM   "!"
               DM   "^"
               DB   129
               DM   "b"
               DB   159
               DM   "s%h[!5]"
               DM   "!"
               DM   "j%h["
               DB   159
               DM   "s,n2]i,%m6"
               DM   "!"
               DB   169
               DM   "[R"
               DB   153
               DM   "[L!R]"
               DB   161
               DM   "[C]A!"
               DB   155
               DM   "[DAA!CPL!SCF!CCF]]"
               DM   "]1"
               DM   "!"
               DM   "%h[j%l["
               DB   159
               DM   "s!"
               DB   159
               DM   "r],%l["
               DB   135
               DM   "s1!"
               DB   135
               DM   "r]!%l[j"
               DB   159
               DM   "i,"
               DB   135
               DM   "r!HALT1]]5"
               DM   "!"
               DM   "a"
               DB   135
               DM   "s%l[1!5]"
               DM   "!"
               DB   135
               DM   "["
               DM   "RET f"
               DM   "!"
               DB   153
               DM   "[POP k!"
               DB   163
               DM   "[RET!EXX!JP (q)!jSP,q]]"
               DM   "!"
               DM   "JP f,%a3"
               DM   "!"
               DB   159
               DM   "[JP %a3!!OUT (n),A2!IN A,(n)2"
               DM   "!EX (SP),q!EX DE,HL!DI!EI]"
               DM   "!"
               DM   "CALL f,%a3"
               DM   "!"
               DB   153
               DM   "[PUSH k!CALL %a3]"
               DM   "!"
               DM   "a"
               DB   169
               DM   "[n2]"
               DB   155
               DM   "[!!!n"
               DM   "2]%b2"
               DM   "!"
               DM   "RST %f"
               DM   "]1"
               DM   "]URK*!0"
;:
;:
 unused_string:DM   "*[q* PREFIX*]0"
;:
;:
     ed_string:DB   179
               DM   "[!"
               DB   135
               DM   "["
               DM   "^"
               DB   129
               DM   "[IN!OUT] [%h["
               DB   159
               DM   "r!X],](C)[!,%h["
               DB   159
               DM   "r!*0]]"
               DM   "!"
               DB   153
               DM   "[SB!AD]C q,"
               DB   163
               DM   "d"
               DM   "!"
               DM   "j"
               DB   153
               DM   "[p,]"
               DB   163
               DM   "d"
               DB   153
               DM   "[!,p]3"
               DM   "!"
               DM   "NEG"
               DM   "!"
               DM   "RET"
               DB   153
               DM   "[N!I]"
               DM   "!"
               DM   "IM "
               DB   155
               DM   "[*0!*0**!*1!*2]"
               DM   "!"
               DB   169
               DM   "[j"
               DB   161
               DM   "[!A,]"
               DB   153
               DM   "[I!R]"
               DB   161
               DM   "[,A]!"
               DB   161
               DM   "[R"
               DB   153
               DM   "[R!L]D!NOP]]"
               DM   "]1"
               DM   "!"
               DB   169
               DM   "[!"
               DB   145
               DM   "["
               DB   131
               DM   "[LD!CP!IN!"
               DB   161
               DM   "[OUT"
               DB   153
               DM   "[I!D]1!OT]]"
               DB   153
               DM   "[I!D]"
               DB   161
               DM   "[!R]1]]"
               DM   "]NOP1"
;:
;:
     cb_string:DM   "%l[%i[e c"
               DB   135
               DM   "r1]"
               DB   179
               DM   "[!e %c,i3]j"
               DB   135
               DM   "r,e* ci3!e c"
               DB   135
               DM   "r%i[1]]3"
;:
;:
    binary_num:LD   E,(IX+1)
               LD   A,"%"
               CALL print_a_buf
     bin_e_buf:LD   B,8
      bin_loop:XOR  A
               RL   E
               ADC  A,"0"
               CALL print_a_buf
               DJNZ bin_loop
               RET
;:
  displacement:LD   A,(IX+1)
               AND  A
;:             Don't show displacement if zero
               JR   Z,show_ind_indx
               BIT  7,A
               JR   NZ,disp_negative
               LD   E,A
               LD   A,"+"
               JR   pr_disp
 disp_negative:NEG
               LD   E,A
               LD   A,"-"
       pr_disp:CALL print_a_buf
               LD   A,E
               CALL print_min_byte
; Signal we are using the indirect index
 show_ind_indx:LD   A,255
               LD   (using_ind_indx+1),A
               LD   HL,(saved_index)
               LD   A,(IX+1)
               LD   E,A
               RLA
               SBC  A,A
               LD   D,A
               ADD  HL,DE
               LD   (saved_index),HL
               RET
   saved_index:DW   0
;
     signal_ix:LD   HL,(reg_ixl)
               LD   (saved_index),HL
               RET
     signal_iy:LD   HL,(reg_iyl)
               LD   (saved_index),HL
               RET
;
 relative_addr:LD   HL,(addr)
               LD   A,(IX+1)
               LD   E,A
               RLA
               SBC  A,A
               LD   D,A
               ADD  HL,DE
               INC  HL
               INC  HL
               JP   print_hl_min
;:
  index_return:XOR  A
               BIT  2,C
               JR   Z,put_pos
               INC  A
               BIT  1,C
               JR   Z,put_pos
               INC  A
       put_pos:EXX
               LD   (DE),A
               EXX
               RET
;:
      ind_hl_l:LD   A,(opcode)
        ind_in:AND  %00000111
               CP   %00000110
               LD   A,0
               JR   NZ,put_pos
               INC  A
               JR   put_pos
;:
      ind_hl_h:LD   A,(opcode)
               RRA
               RRA
               RRA
               JR   ind_in
;:
print_byte_2_4:BIT  2,C
               JR   Z,print_byte_2_3
               INC  IX
print_byte_2_3:LD   A,(IX+1)
print_min_byte:LD   B,1
  print_a_byte:BIT  0,(IY+number_base)
               JR   Z,decimal_byte
               LD   E,A
print_hex_high:LD   A,"#"
               CALL print_a_buf
 print_hex_low:LD   A,E
               RRA
               RRA
               RRA
               RRA
               CALL print_hex_chr
               LD   A,E
 print_hex_chr:AND  %00001111
               ADD  A,48
               CP   58
               JP   C,print_a_buf
               ADD  A,7
               JP   print_a_buf
;:
  decimal_byte:LD   L,A
               LD   H,0
               JR   print_deci_8
;:
      rst_addr:LD   A,(IX+0)
               AND  %00111000
               JR   print_min_byte
;:
 print_bit_pos:LD   A,(opcode)
               RRA
               RRA
               RRA
               AND  %00000111
               ADD  A,48
               CALL print_a_buf
               RET
;:
print_ins_word:LD   L,(IX+1)
               LD   H,(IX+2)
  print_hl_min:LD   B,1
      print_hl:BIT  0,(IY+number_base)
               JR   Z,print_deci_16
               LD   E,H
               CALL print_hex_high
               LD   E,L
               JP   print_hex_low
 print_deci_16:LD   DE,10000
               CALL digit
               LD   DE,1000
               CALL digit
  print_deci_8:LD   DE,100
               CALL digit
               LD   DE,10
               CALL digit
               LD   A,L
               ADD  A,48
               CALL print_a_buf
               RET
         digit:XOR  A
      digit_lp:INC  A
               SBC  HL,DE
               JR   NC,digit_lp
               ADD  HL,DE
               CP   B
               RET  Z
               DEC  B
               ADD  A,47
               EXX
               LD   (HL),A
               INC  HL
               EXX
               RET
;
 clear_txt_buf:LD   HL,txt_buffer
               LD   BC,#2F20
  txt_clear_lp:LD   (HL),C
               INC  HL
               DJNZ txt_clear_lp
               EXX
               LD   HL,txt_buffer
               EXX
               RET
;
;:
    dis_buffer:DB   0,0,0,0
     pos_stack:DB   0,0,0,0,0,0,0,0,0,0
;:
          addr:DW   0
        opcode:DB   0
    txt_buffer:DS   50
