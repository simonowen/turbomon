;
; Z 8 0   E M U L A T I O N   C O D E
;

;
; Flag bit masks
        flag_z:EQU  %01000000
        flag_c:EQU  %00000001
       flag_pe:EQU  %00000100
        flag_m:EQU  %10000000

;
; RET CC
;
      i_ret_cc:LD   B,A
               RRA
               RRA
               AND  %00001100
               LD   (ret_flag_test+1),A
               LD   A,(reg_flags)
 ret_flag_test:JR   ret_flag_test
               BIT  6,A
               JR   ret_flag_done
               BIT  0,A
               JR   ret_flag_done
               BIT  2,A
               JR   ret_flag_done
               BIT  7,A
 ret_flag_done:JR   NZ,ret_flag_set
               BIT  3,B
               JP   NZ,len_1
         i_ret:LD   HL,(reg_spl)
               INC  HL
               INC  HL
               LD   (reg_spl),HL
               DEC  HL
               DEC  HL
               LD   DE,ret_cc_pc+1
               LD   BC,2
               RST  16
     ret_cc_pc:LD   HL,0
               POP  DE
               RET
  ret_flag_set:BIT  3,B
               JR   NZ,i_ret
               POP  HL
               INC  HL
               RET

;
; JP CC,nn  (fast)
;
     i_jp_z_nn:LD   A,(reg_flags)
               AND  flag_z
               JP   Z,len_3
               LD   HL,(instr+1)
               POP  DE
               RET
    i_jp_nc_nn:LD   A,(reg_flags)
               AND  flag_c
               JP   NZ,len_3
               LD   HL,(instr+1)
               POP  DE
               RET
     i_jp_c_nn:LD   A,(reg_flags)
               AND  flag_c
               JP   Z,len_3
               LD   HL,(instr+1)
               POP  DE
               RET
    i_jp_po_nn:LD   A,(reg_flags)
               AND  flag_pe
               JP   NZ,len_3
               LD   HL,(instr+1)
               POP  DE
               RET
    i_jp_pe_nn:LD   A,(reg_flags)
               AND  flag_pe
               JP   Z,len_3
               LD   HL,(instr+1)
               POP  DE
               RET
     i_jp_p_nn:LD   A,(reg_flags)
               AND  flag_m
               JP   NZ,len_3
               LD   HL,(instr+1)
               POP  DE
               RET
     i_jp_m_nn:LD   A,(reg_flags)
               AND  flag_m
               JP   Z,len_3
               LD   HL,(instr+1)
               POP  DE
               RET
    i_jp_nz_nn:LD   A,(reg_flags)
               AND  flag_z
               JP   NZ,len_3
       i_jp_nn:LD   HL,(instr+1)
               POP  DE
               RET
;
; CALL CC,nn
;
  i_call_cc_nn:LD   B,A
               RRA
               RRA
               AND  %00001100
               LD   (call_flag_test+1),A
               LD   A,(reg_flags)
call_flag_test:JR   call_flag_test
               BIT  6,A
               JR   call_flag_done
               BIT  0,A
               JR   call_flag_done
               BIT  2,A
               JR   call_flag_done
               BIT  7,A
call_flag_done:JR   NZ,call_flag_set
               BIT  3,B
               JP   NZ,len_3
     i_call_nn:POP  HL
               INC  HL
               INC  HL
               INC  HL
               LD   (temp_store),HL
       call_in:LD   HL,(reg_spl)
               DEC  HL
               DEC  HL
               LD   (reg_spl),HL
               LD   DE,temp_store
               EX   DE,HL
               LD   C,2
               CALL put_bytes
               LD   HL,(instr+1)
               RET
 call_flag_set:BIT  3,B
               JR   NZ,i_call_nn
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET

;
; RST p
;
       i_rst_p:POP  HL
               INC  HL
               LD   (temp_store),HL
               LD   HL,(reg_spl)
               DEC  HL
               DEC  HL
               LD   (reg_spl),HL
               LD   DE,temp_store
               EX   DE,HL
               LD   C,2
               CALL put_bytes
               LD   A,(instr)
               AND  %00111000
               LD   L,A
               LD   H,0
               RET
;
; JR CC,d  (fast)
;
; JR NZ,d  has been optimised so that if it is jumping
; to the previous instruction, which is a DEC r (but not
; DEC (HL)), the end of the loop is simulated and the
; PC is set to after the JR. Removes time delays :-)
;
      i_jr_z_e:LD   A,(reg_flags)
               AND  flag_z
               JP   NZ,i_jr_e
       jr_skip:POP  HL
               INC  HL
               INC  HL
               RET
     i_jr_nc_e:LD   A,(reg_flags)
               AND  flag_c
               JP   Z,i_jr_e
               JP   len_2
      i_jr_c_e:LD   A,(reg_flags)
               AND  flag_c
               JP   NZ,i_jr_e
               JP   len_2
     i_jr_nz_e:LD   A,(reg_flags)
               AND  flag_z
               JR   NZ,jr_skip
        i_jr_e:LD   A,(instr+1)
     jrnz_back:POP  HL
               INC  HL
               INC  HL
               LD   E,A
               RLA
               SBC  A,A
               LD   D,A
               ADD  HL,DE
               RET
;
; DJNZ e
; Using DJNZ as a delay is recognised and cut short if
; executing properly.  ie.  loop:DJNZ loop
; Executed as normal if single stepping.
;
      i_djnz_e:LD   A,(instr+1)
               ADD  A,2
               JR   NZ,norm_djnz
               BIT  0,(IY+cpu_flags)
               JR   Z,norm_djnz
               XOR  A
               LD   (reg_b),A
               POP  HL
               INC  HL
               INC  HL
               RET
;
     norm_djnz:LD   HL,reg_b
               DEC  (HL)
               JP   NZ,i_jr_e
               POP  HL
               INC  HL
               INC  HL
               RET
;
; LD SP,HL  (indexable)
;
    i_ld_sp_hl:LD   DE,reg_l
       ld_sp_i:EX   DE,HL
               LD   E,(HL)
               INC  L
               LD   D,(HL)
               LD   (reg_spl),DE
               POP  HL
               INC  HL
               RET
;
; LD A,(BC) and LD (BC),A
;
 i_ld_a_ind_bc:LD   HL,(reg_c)
               LD   DE,reg_a
               LD   BC,1
               RST  16
               POP  HL
               INC  HL
               RET
 i_ld_ind_bc_a:LD   HL,reg_a
               LD   DE,(reg_c)
               CALL put_byte
               POP  HL
               INC  HL
               RET
;
; LD A,(BC) and LD (BC),A
;
 i_ld_a_ind_de:LD   HL,(reg_e)
               LD   DE,reg_a
               LD   BC,1
               RST  16
               POP  HL
               INC  HL
               RET
 i_ld_ind_de_a:LD   HL,reg_a
               LD   DE,(reg_e)
               CALL put_byte
               POP  HL
               INC  HL
               RET
;
; POP rr  (indexable)
;
; POP HL is checked for trailing POP DE and then POP BC
;
      i_pop_hl:LD   A,(instr+1)
               CP   #D1
               JR   Z,pop_hl_de
   norm_pop_hl:LD   DE,reg_l
         pop_i:LD   HL,(reg_spl)
               INC  HL
               INC  HL
               LD   (reg_spl),HL
               DEC  HL
               DEC  HL
               LD   BC,2
               RST  16
               POP  HL
               INC  HL
               RET
     pop_hl_de:BIT  0,(IY+cpu_flags)
               JR   Z,norm_pop_hl
               LD   HL,(reg_spl)
               LD   C,L
               LD   B,H
               LD   A,(instr+2)
               CP   #C1
               JR   Z,pop_hl_de_bc
               LD   DE,4
               ADD  HL,DE
               LD   (reg_spl),HL
               LD   L,C
               LD   H,B
               LD   DE,reg_l
               LD   BC,4
               RST  16
               POP  HL
               INC  HL
               INC  HL
               RET
  pop_hl_de_bc:LD   DE,6
               ADD  HL,DE
               LD   (reg_spl),HL
               LD   L,C
               LD   H,B
               LD   DE,reg_l
               LD   BC,6
               RST  16
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
;
      i_pop_bc:LD   DE,reg_c
               LD   HL,(reg_spl)
               INC  HL
               INC  HL
               LD   (reg_spl),HL
               DEC  HL
               DEC  HL
               LD   BC,2
               RST  16
               LD   A,(instr+1)
               CP   #D1
               JR   Z,pop_bc_de
   norm_pop_bc:POP  HL
               INC  HL
               RET
     pop_bc_de:BIT  0,(IY+cpu_flags)
               JP   Z,norm_pop_bc
               LD   DE,reg_e
               LD   HL,(reg_spl)
               INC  HL
               INC  HL
               LD   (reg_spl),HL
               DEC  HL
               DEC  HL
               LD   BC,2
               RST  16
               LD   A,(instr+2)
               CP   #F1
               JR   Z,pop_bc_de_af
               POP  HL
               INC  HL
               INC  HL
               RET
  pop_bc_de_af:LD   DE,reg_flags
               LD   HL,(reg_spl)
               INC  HL
               INC  HL
               LD   (reg_spl),HL
               DEC  HL
               DEC  HL
               LD   BC,2
               RST  16
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
;
      i_pop_de:LD   DE,reg_e
               LD   HL,(reg_spl)
               INC  HL
               INC  HL
               LD   (reg_spl),HL
               DEC  HL
               DEC  HL
               LD   BC,2
               RST  16
               POP  HL
               INC  HL
               RET
;
      i_pop_af:LD   DE,reg_flags
               LD   HL,(reg_spl)
               INC  HL
               INC  HL
               LD   (reg_spl),HL
               DEC  HL
               DEC  HL
               LD   BC,2
               RST  16
               POP  HL
               INC  HL
               RET
;
; PUSH rr  (indexable) HL and DE reapeated up to thrice
;
     i_push_hl:LD   A,(instr+1)
               CP   #E5
               JR   Z,push_hl_hl
single_push_hl:LD   DE,reg_l
        push_i:LD   HL,(reg_spl)
               DEC  HL
               DEC  HL
               LD   (reg_spl),HL
               EX   DE,HL
               LD   C,2
               CALL put_bytes
               POP  HL
               INC  HL
               RET
;
    push_hl_hl:BIT  0,(IY+cpu_flags)
; Only 1 PUSH when not executing properly
               JP   Z,single_push_hl
               LD   HL,(reg_l)
               LD   (mult_hl_buf+2),HL
               LD   A,(instr+2)
               CP   #E5
               JR   Z,push_hl_hl_hl
               LD   HL,(reg_spl)
               LD   DE,0-4
               ADD  HL,DE
               LD   (reg_spl),HL
               LD   DE,mult_hl_buf+2
               EX   DE,HL
               LD   C,4
               CALL put_bytes
               POP  HL
               INC  HL
               INC  HL
               RET
 push_hl_hl_hl:LD   (mult_hl_buf),HL
               LD   HL,(reg_spl)
               LD   DE,0-6
               ADD  HL,DE
               LD   (reg_spl),HL
               LD   DE,mult_hl_buf
               EX   DE,HL
               LD   C,6
               CALL put_bytes
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET

     i_push_bc:LD   A,(instr+1)
               CP   #D5
               JR   Z,push_bc_de
  norm_push_bc:LD   DE,reg_c
               LD   HL,(reg_spl)
               DEC  HL
               DEC  HL
               LD   (reg_spl),HL
               EX   DE,HL
               LD   C,2
               CALL put_bytes
               POP  HL
               INC  HL
               RET
    push_bc_de:BIT  0,(IY+cpu_flags)
               JP   Z,norm_push_bc
               LD   HL,(reg_spl)
               LD   A,(instr+2)
               CP   #E5
               JR   Z,push_bc_de_hl
               LD   DE,0-4
               ADD  HL,DE
               LD   (reg_spl),HL
               LD   DE,reg_e
               EX   DE,HL
               LD   C,4
               CALL put_bytes
               POP  HL
               INC  HL
               INC  HL
               RET
 push_bc_de_hl:LD   DE,0-6
               ADD  HL,DE
               LD   (reg_spl),HL
               LD   DE,reg_l
               EX   DE,HL
               LD   C,6
               CALL put_bytes
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET

     i_push_de:LD   A,(instr+1)
               CP   #D5
               JR   Z,push_de_de
single_push_de:LD   DE,reg_e
               LD   HL,(reg_spl)
               DEC  HL
               DEC  HL
               LD   (reg_spl),HL
               EX   DE,HL
               LD   C,2
               CALL put_bytes
               POP  HL
               INC  HL
               RET
;
    push_de_de:BIT  0,(IY+cpu_flags)
; Only 1 PUSH when not executing properly
               JP   Z,single_push_de
               LD   HL,(reg_e)
               LD   (work_space),HL
               LD   (work_space+2),HL
               LD   DE,work_space
               LD   A,(instr+2)
               CP   #D5
               JR   Z,push_de_de_de
               LD   HL,(reg_spl)
               LD   BC,0-4
               ADD  HL,BC
               LD   (reg_spl),HL
               EX   DE,HL
               LD   C,4
               CALL put_bytes
               POP  HL
               INC  HL
               INC  HL
               RET
 push_de_de_de:LD   (work_space+4),HL
               LD   HL,(reg_spl)
               LD   BC,0-6
               ADD  HL,BC
               LD   (reg_spl),HL
               EX   DE,HL
               LD   C,6
               CALL put_bytes
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET

     i_push_af:LD   DE,reg_flags
               LD   HL,(reg_spl)
               DEC  HL
               DEC  HL
               LD   (reg_spl),HL
               EX   DE,HL
               LD   C,2
               CALL put_bytes
               LD   A,(instr+1)
               CP   #D5
               JR   Z,push_af_de
  norm_push_af:POP  HL
               INC  HL
               RET
    push_af_de:BIT  0,(IY+cpu_flags)
               JP   Z,norm_push_af
               LD   DE,reg_e
               LD   HL,(reg_spl)
               DEC  HL
               DEC  HL
               LD   (reg_spl),HL
               EX   DE,HL
               LD   C,2
               CALL put_bytes
               LD   A,(instr+2)
               CP   #C5
               JR   Z,push_af_de_bc
               POP  HL
               INC  HL
               INC  HL
               RET
 push_af_de_bc:LD   DE,reg_c
               LD   HL,(reg_spl)
               DEC  HL
               DEC  HL
               LD   (reg_spl),HL
               EX   DE,HL
               LD   C,2
               CALL put_bytes
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET

;
; LD A,(nn) and LD (nn),A
;
 i_ld_a_ind_nn:LD   HL,(instr+1)
               LD   DE,reg_a
               LD   BC,1
               RST  16
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
 i_ld_ind_nn_a:LD   HL,reg_a
               LD   DE,(instr+1)
               CALL put_byte
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
;
; LD dd,nn  (indexable)
;
    i_ld_bc_nn:LD   HL,(instr+1)
               LD   (reg_c),HL
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
    i_ld_sp_nn:LD   HL,(instr+1)
               LD   (reg_spl),HL
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
    i_ld_de_nn:LD   HL,(instr+1)
               LD   (reg_e),HL
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
    i_ld_hl_nn:LD   HL,(instr+1)
               LD   (reg_l),HL
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
       ld_i_nn:LD   HL,(instr+1)
               EX   DE,HL
               LD   (HL),E
               INC  L
               LD   (HL),D
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
;
; DEC rr  (indexable)
;
      i_dec_de:LD   HL,(reg_e)
               DEC  HL
               LD   (reg_e),HL
               POP  HL
               INC  HL
               RET
      i_dec_sp:LD   HL,(reg_spl)
               DEC  HL
               LD   (reg_spl),HL
               POP  HL
               INC  HL
               RET
      i_dec_bc:LD   HL,(reg_c)
               DEC  HL
               LD   (reg_c),HL
               POP  HL
               INC  HL
               RET
      i_dec_hl:LD   HL,(reg_l)
               DEC  HL
               LD   (reg_l),HL
               POP  HL
               INC  HL
               RET
        dec_ii:EX   DE,HL
               LD   E,(HL)
               INC  L
               LD   D,(HL)
               DEC  DE
               LD   (HL),D
               DEC  L
               LD   (HL),E
               POP  HL
               INC  HL
               RET

;
; INC rr  (indexable)
;
      i_inc_bc:LD   HL,reg_c
               INC  (HL)
               JR   Z,bc_high
               POP  HL
               INC  HL
               RET
       bc_high:INC  L
               INC  (HL)
               POP  HL
               INC  HL
               RET
      i_inc_sp:LD   HL,reg_spl
               INC  (HL)
               JR   Z,spl_high
               POP  HL
               INC  HL
               RET
      spl_high:INC  L
               INC  (HL)
               POP  HL
               INC  HL
               RET
      i_inc_de:LD   HL,reg_e
               INC  (HL)
               JR   Z,de_high
               POP  HL
               INC  HL
               RET
       de_high:INC  L
               INC  (HL)
               POP  HL
               INC  HL
               RET
      i_inc_hl:LD   HL,reg_l
               INC  (HL)
               JR   Z,hl_high
               POP  HL
               INC  HL
               RET
       hl_high:INC  L
               INC  (HL)
               POP  HL
               INC  HL
               RET
        inc_ii:EX   DE,HL
               INC  (HL)
               JR   Z,indx_high
               POP  HL
               INC  HL
               RET
     indx_high:INC  L
               INC  (HL)
               POP  HL
               INC  HL
               RET
;
; EX AF,AF'
;
 menu_ex_af_af:PUSH HL
               SET  1,(IY+refresh_flags)
    i_ex_af_af:LD   HL,(reg_flags)
               LD   DE,(reg_alt_flags)
               LD   (reg_alt_flags),HL
               LD   (reg_flags),DE
               POP  HL
               INC  HL
               RET

;
; EX DE,HL
;
 menu_ex_de_hl:PUSH HL
               SET  1,(IY+refresh_flags)
    i_ex_de_hl:LD   HL,(reg_l)
               LD   DE,(reg_e)
               LD   (reg_e),HL
               LD   (reg_l),DE
               POP  HL
               INC  HL
               RET

;
; EXX
;
      menu_exx:PUSH HL
               SET  1,(IY+refresh_flags)
         i_exx:LD   HL,(reg_c)
               LD   DE,(reg_alt_c)
               LD   (reg_alt_c),HL
               LD   (reg_c),DE
               LD   HL,(reg_e)
               LD   DE,(reg_alt_e)
               LD   (reg_alt_e),HL
               LD   (reg_e),DE
               LD   HL,(reg_l)
               LD   DE,(reg_alt_l)
               LD   (reg_alt_l),HL
               LD   (reg_l),DE
               POP  HL
               INC  HL
               RET

;
; DI and EI
; There is a special breakpoint at address 0 in ROM 0
; to stop execution before it starts the reset code.
;
          i_di:POP  HL
               LD   A,H
               OR   L
               JR   NZ,not_di_stop
               BIT  5,(IY+port_lmpr)
               JR   NZ,not_di_stop
               BIT  0,(IY+cpu_flags)
               JR   Z,not_di_stop
; At 0 in ROM 0 when executing properly so STOP!
               JP   execute_break
   not_di_stop:XOR  A
               LD   (reg_iff1),A
               INC  HL
               RET
;
; Since EI appears at the end of interrupt handlers we
; can use it as a convenient time to reset and interrupt
; signals in the status port - in reality they disappear
; after about 20 uS.
;
          i_ei:LD   A,%00000100
               LD   (reg_iff1),A
               LD   (IY+port_status),%00011111
               POP  HL
               INC  HL
               RET
;
; RETN
;
        i_retn:LD   A,(reg_iff2)
               LD   (reg_iff1),A
               JP   i_ret
;
;
; IN A,(n)
;
  i_in_a_ind_n:LD   A,(reg_a)
               LD   B,A
               LD   A,(instr+1)
               LD   C,A
               CALL read_port
               LD   (reg_a),A
               POP  HL
               INC  HL
               INC  HL
               RET
;
; OUT (n),A
;
 i_out_ind_n_a:POP  HL
               BIT  4,(IY+cpu_flags)
               JP   Z,no_bord_stop
               BIT  0,(IY+cpu_flags)
               JR   Z,no_bord_stop
               LD   A,(output_patch+1)
               LD   B,A
               LD   A,(instr+1)
               CP   B
               JP   Z,execute_break
  no_bord_stop:INC  HL
               INC  HL
               LD   A,(instr+1)
               LD   C,A
               CP   border
               LD   A,(reg_a)
               JP   Z,write_border
               LD   B,A
               PUSH HL
               CALL write_port
               POP  HL
               RET
;
; OUT (C),r (slowish), includes OUT (C),0 when r is (HL)
;
 i_out_ind_c_r:AND  %00111000
               RRA
               RRA
               LD   E,A
               XOR  A
               LD   D,A
               LD   (temp_store),A
               LD   HL,get_r_table
               ADD  HL,DE
               LD   A,(HL)
               INC  HL
               LD   H,(HL)
               LD   L,A
               LD   A,(HL)
               LD   BC,(reg_c)
               CALL write_port
               POP  HL
               INC  HL
               INC  HL
               RET
;
; IN r,(C)  (slow)
;
  i_in_r_ind_c:AND  %00111000
               RRA
               RRA
               LD   E,A
               LD   D,0
               LD   BC,(reg_c)
               CALL read_port
               OR   A
               PUSH AF
               LD   HL,get_r_table
               ADD  HL,DE
               LD   E,(HL)
               INC  HL
               LD   H,(HL)
               LD   L,E
               LD   (HL),A
               POP  BC
               RES  0,C
               LD   A,(reg_flags)
               AND  1
               OR   C
               LD   (reg_flags),A
               POP  HL
               INC  HL
               INC  HL
               RET
;
; IN X,(C)  [IN:(HL),(C)]  affects ports, junks value
;
  i_in_x_ind_c:LD   BC,(reg_c)
               CALL read_port
               OR   A
               PUSH AF
               POP  HL
               RES  0,L
               LD   A,(reg_flags)
               AND  1
               OR   L
               LD   (reg_flags),A
               POP  HL
               INC  HL
               INC  HL
               RET
;
; 1 byte instructions affecting only A and flags
;
    i_simple_1:LD   (simple_1_code),A
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
 simple_1_code:DB   0
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
;
; 2 byte instructions affecting only A and flags
;
    i_simple_2:LD   HL,(instr)
   simple_2_in:LD   (simple_2_code),HL
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
 simple_2_code:DW   0
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
;
; Arithmetic inctructions using (HL)
;
i_arith_ind_hl:LD   (arith_r_code),A
               LD   HL,(reg_l)
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   DE,temp_store
               JP   arith_exe
arith_ind_indx:LD   (arith_r_code),A
               EX   DE,HL
               LD   C,(HL)
               INC  L
               LD   B,(HL)
               POP  HL
               INC  HL
               PUSH HL
               LD   A,(instr+1)
               LD   L,A
               RLA
               SBC  A,A
               LD   H,A
               ADD  HL,BC
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   DE,temp_store
               JP   arith_exe
;
; Arithmetic instrs using single registers B,C,D,E,H,L,A
;
     i_arith_b:LD   DE,reg_b
               JP   arith_i
     i_arith_c:LD   DE,reg_c
               JP   arith_i
     i_arith_d:LD   DE,reg_d
               JP   arith_i
     i_arith_e:LD   DE,reg_e
               JP   arith_i
     i_arith_h:LD   DE,reg_h
               JP   arith_i
      arith_ih:INC  E
               JP   arith_i
     i_arith_l:LD   DE,reg_l
       arith_i:AND  %11111000
               OR   %00000110
               LD   (arith_r_code),A
     arith_exe:LD   HL,(reg_flags)
               PUSH HL
               EX   DE,HL
               POP  AF
  arith_r_code:DB   0
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
;
; OR r - checked for a trailing JR NZ and JP NZ
;
        i_or_b:LD   DE,reg_b
               JP   or_i
        i_or_c:LD   DE,reg_c
               JP   or_i
        i_or_d:LD   DE,reg_d
               JP   or_i
        i_or_e:LD   DE,reg_e
               JP   or_i
        i_or_h:LD   DE,reg_h
               JP   or_i
       i_or_ih:INC  E
               JP   or_i
        i_or_l:LD   DE,reg_l
          or_i:BIT  0,(IY+cpu_flags)
; Jump if we're not executing properly
               JP   Z,arith_i
               AND  %11111000
               OR   %00000110
               LD   (or_r_code),A
               LD   HL,(reg_flags)
               PUSH HL
               EX   DE,HL
               POP  AF
     or_r_code:DB   0
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
; Move past OR instruction
               INC  HL
               LD   A,(instr+1)
; Check for JR NZ,d
               CP   32
               RET  NZ
; Move past the JR instruction
               INC  HL
               INC  HL
               LD   A,(reg_flags)
               AND  flag_z
               RET  NZ
               LD   A,(instr+2)
               LD   E,A
               RLA
               SBC  A,A
               LD   D,A
               ADD  HL,DE
               RET

;
; EX (SP),HL  (indexable)
;
i_ex_ind_sp_hl:LD   DE,reg_l
   ex_ind_sp_i:EX   DE,HL
               LD   E,(HL)
               INC  L
               LD   D,(HL)
               DEC  L
               EX   DE,HL
               LD   (temp_store),HL
               LD   HL,(reg_spl)
               LD   BC,2
               PUSH HL
               RST  16
               POP  DE
               LD   HL,temp_store
               LD   C,2
               CALL put_bytes
               POP  HL
               INC  HL
               RET
;
; LD I,A  and  LD R,A
;
      i_ld_i_a:LD   A,(reg_a)
               LD   (reg_int_vector),A
               POP  HL
               INC  HL
               INC  HL
               RET
      i_ld_r_a:LD   A,(reg_a)
               LD   (reg_refresh),A
               POP  HL
               INC  HL
               INC  HL
               RET
;
; LD A,R  and  LD A,I
;
      i_ld_a_r:LD   A,(reg_refresh)
               LD   B,A
               LD   A,R
               LD   (reg_refresh),A
               LD   A,B
    ld_a_ir_in:LD   (reg_a),A
               OR   A
               PUSH AF
               POP  BC
               LD   A,(reg_flags)
               AND  1
               OR   C
               LD   C,A
               LD   A,(reg_iff1)
               OR   C
               LD   (reg_flags),A
               POP  HL
               INC  HL
               INC  HL
               RET
      i_ld_a_i:LD   A,(reg_int_vector)
               JP   ld_a_ir_in
;
; LD r,r'  (fast, indexable)
;
      i_ld_b_a:LD   A,(reg_a)
               LD   (reg_b),A
               POP  HL
               INC  HL
               RET
      i_ld_c_a:LD   A,(reg_a)
               LD   (reg_c),A
               POP  HL
               INC  HL
               RET
      i_ld_d_a:LD   A,(reg_a)
               LD   (reg_d),A
               POP  HL
               INC  HL
               RET
      i_ld_e_a:LD   A,(reg_a)
               LD   (reg_e),A
               POP  HL
               INC  HL
               RET
      i_ld_h_a:LD   A,(reg_a)
               LD   (reg_h),A
               POP  HL
               INC  HL
               RET
       ld_ih_a:INC  E
               LD   A,(reg_a)
               LD   (DE),A
               POP  HL
               INC  HL
               RET
      i_ld_l_a:LD   A,(reg_a)
               LD   (reg_l),A
               POP  HL
               INC  HL
               RET
       ld_il_a:LD   A,(reg_a)
               LD   (DE),A
               POP  HL
               INC  HL
               RET
;
      i_ld_a_b:LD   A,(reg_b)
               LD   (reg_a),A
               POP  HL
               INC  HL
               RET
      i_ld_c_b:LD   A,(reg_b)
               LD   (reg_c),A
               POP  HL
               INC  HL
               RET
      i_ld_d_b:LD   A,(reg_b)
               LD   (reg_d),A
               POP  HL
               INC  HL
               RET
      i_ld_e_b:LD   A,(reg_b)
               LD   (reg_e),A
               POP  HL
               INC  HL
               RET
      i_ld_h_b:LD   A,(reg_b)
               LD   (reg_h),A
               POP  HL
               INC  HL
               RET
       ld_ih_b:INC  E
               LD   A,(reg_b)
               LD   (DE),A
               POP  HL
               INC  HL
               RET
      i_ld_l_b:LD   A,(reg_b)
               LD   (reg_l),A
               POP  HL
               INC  HL
               RET
       ld_il_b:LD   A,(reg_b)
               LD   (DE),A
               POP  HL
               INC  HL
               RET
;
      i_ld_a_c:LD   A,(reg_c)
               LD   (reg_a),A
               POP  HL
               INC  HL
               RET
      i_ld_b_c:LD   A,(reg_c)
               LD   (reg_b),A
               POP  HL
               INC  HL
               RET
      i_ld_d_c:LD   A,(reg_c)
               LD   (reg_d),A
               POP  HL
               INC  HL
               RET
      i_ld_e_c:LD   A,(reg_c)
               LD   (reg_e),A
               POP  HL
               INC  HL
               RET
      i_ld_h_c:LD   A,(reg_c)
               LD   (reg_h),A
               POP  HL
               INC  HL
               RET
       ld_ih_c:INC  E
               LD   A,(reg_c)
               LD   (DE),A
               POP  HL
               INC  HL
               RET
      i_ld_l_c:LD   A,(reg_c)
               LD   (reg_l),A
               POP  HL
               INC  HL
               RET
       ld_il_c:LD   A,(reg_c)
               LD   (DE),A
               POP  HL
               INC  HL
               RET
;
      i_ld_a_d:LD   A,(reg_d)
               LD   (reg_a),A
               POP  HL
               INC  HL
               RET
      i_ld_b_d:LD   A,(reg_d)
               LD   (reg_b),A
               POP  HL
               INC  HL
               RET
      i_ld_c_d:LD   A,(reg_d)
               LD   (reg_c),A
               POP  HL
               INC  HL
               RET
      i_ld_e_d:LD   A,(reg_d)
               LD   (reg_e),A
               POP  HL
               INC  HL
               RET
      i_ld_h_d:LD   A,(reg_d)
               LD   (reg_h),A
               POP  HL
               INC  HL
               RET
       ld_ih_d:INC  E
               LD   A,(reg_d)
               LD   (DE),A
               POP  HL
               INC  HL
               RET
      i_ld_l_d:LD   A,(reg_d)
               LD   (reg_l),A
               POP  HL
               INC  HL
               RET
       ld_il_d:LD   A,(reg_d)
               LD   (DE),A
               POP  HL
               INC  HL
               RET
;
      i_ld_a_e:LD   A,(reg_e)
               LD   (reg_a),A
               POP  HL
               INC  HL
               RET
      i_ld_b_e:LD   A,(reg_e)
               LD   (reg_b),A
               POP  HL
               INC  HL
               RET
      i_ld_c_e:LD   A,(reg_e)
               LD   (reg_c),A
               POP  HL
               INC  HL
               RET
      i_ld_d_e:LD   A,(reg_e)
               LD   (reg_d),A
               POP  HL
               INC  HL
               RET
      i_ld_h_e:LD   A,(reg_e)
               LD   (reg_h),A
               POP  HL
               INC  HL
               RET
       ld_ih_e:INC  E
               LD   A,(reg_e)
               LD   (DE),A
               POP  HL
               INC  HL
               RET
      i_ld_l_e:LD   A,(reg_e)
               LD   (reg_l),A
               POP  HL
               INC  HL
               RET
       ld_il_e:LD   A,(reg_e)
               LD   (DE),A
               POP  HL
               INC  HL
               RET
;
      i_ld_a_h:LD   A,(reg_h)
               LD   (reg_a),A
               POP  HL
               INC  HL
               RET
       ld_a_ih:INC  E
               LD   A,(DE)
               LD   (reg_a),A
               POP  HL
               INC  HL
               RET
      i_ld_b_h:LD   A,(reg_h)
               LD   (reg_b),A
               POP  HL
               INC  HL
               RET
       ld_b_ih:INC  E
               LD   A,(DE)
               LD   (reg_b),A
               POP  HL
               INC  HL
               RET
      i_ld_c_h:LD   A,(reg_h)
               LD   (reg_c),A
               POP  HL
               INC  HL
               RET
       ld_c_ih:INC  E
               LD   A,(DE)
               LD   (reg_c),A
               POP  HL
               INC  HL
               RET
      i_ld_d_h:LD   A,(reg_h)
               LD   (reg_d),A
               POP  HL
               INC  HL
               RET
       ld_d_ih:INC  E
               LD   A,(DE)
               LD   (reg_d),A
               POP  HL
               INC  HL
               RET
      i_ld_e_h:LD   A,(reg_h)
               LD   (reg_e),A
               POP  HL
               INC  HL
               RET
       ld_e_ih:INC  E
               LD   A,(DE)
               LD   (reg_e),A
               POP  HL
               INC  HL
               RET
      i_ld_l_h:LD   A,(reg_h)
               LD   (reg_l),A
               POP  HL
               INC  HL
               RET
      ld_il_ih:INC  E
               LD   A,(DE)
               DEC  E
               LD   (DE),A
               POP  HL
               INC  HL
               RET
;
      i_ld_a_l:LD   A,(reg_l)
               LD   (reg_a),A
               POP  HL
               INC  HL
               RET
       ld_a_il:LD   A,(DE)
               LD   (reg_a),A
               POP  HL
               INC  HL
               RET
      i_ld_b_l:LD   A,(reg_l)
               LD   (reg_b),A
               POP  HL
               INC  HL
               RET
       ld_b_il:LD   A,(DE)
               LD   (reg_b),A
               POP  HL
               INC  HL
               RET
      i_ld_c_l:LD   A,(reg_l)
               LD   (reg_c),A
               POP  HL
               INC  HL
               RET
       ld_c_il:LD   A,(DE)
               LD   (reg_c),A
               POP  HL
               INC  HL
               RET
      i_ld_d_l:LD   A,(reg_l)
               LD   (reg_d),A
               POP  HL
               INC  HL
               RET
       ld_d_il:LD   A,(DE)
               LD   (reg_d),A
               POP  HL
               INC  HL
               RET
      i_ld_e_l:LD   A,(reg_l)
               LD   (reg_e),A
               POP  HL
               INC  HL
               RET
       ld_e_il:LD   A,(DE)
               LD   (reg_e),A
               POP  HL
               INC  HL
               RET
      i_ld_h_l:LD   A,(reg_l)
               LD   (reg_h),A
               POP  HL
               INC  HL
               RET
      ld_ih_il:LD   A,(DE)
               INC  E
               LD   (DE),A
               POP  HL
               INC  HL
               RET
;
; INC A - special case
;
       i_inc_a:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               INC  A
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
;
; INC r  (indexable)
;
       i_inc_b:LD   DE,reg_b
               JP   inc_i
       i_inc_c:LD   DE,reg_c
               JP   inc_i
       i_inc_d:LD   DE,reg_d
               JP   inc_i
       i_inc_e:LD   DE,reg_e
               JP   inc_i
       i_inc_h:LD   DE,reg_h
               JP   inc_i
        inc_ih:INC  E
               JP   inc_i
       i_inc_l:LD   DE,reg_l
         inc_i:LD   HL,(reg_flags)
               PUSH HL
               EX   DE,HL
               POP  AF
               INC  (HL)
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
;
; DEC A - special case
;
       i_dec_a:LD   HL,(reg_flags)
               PUSH HL
               LD   A,(instr+1)
               CP   32
               JR   NZ,norm_dec_a
               LD   A,(instr+2)
               CP   0-3
               JR   NZ,norm_dec_a
               POP  AF
               LD   A,1
               DEC  A
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
;
    norm_dec_a:POP  AF
               DEC  A
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
;
; DEC r  (indexable)
;
       i_dec_b:LD   DE,reg_b
               BIT  0,(IY+cpu_flags)
               JR   Z,dec_i
               POP  BC
               LD   HL,#03DB
               AND  A
               SBC  HL,BC
; Jump if not running the Speccy ROM BEEP routine
               PUSH BC
               JP   NZ,dec_i
               POP  HL
               LD   HL,#03DF
               RET
;
       i_dec_c:LD   DE,reg_c
               JP   dec_i
       i_dec_d:LD   DE,reg_d
               JP   dec_i
       i_dec_e:LD   DE,reg_e
               JP   dec_i
       i_dec_h:LD   DE,reg_h
               JP   dec_i
        dec_ih:INC  E
               JP   dec_i
       i_dec_l:LD   DE,reg_l
         dec_i:BIT  0,(IY+cpu_flags)
               JP   norm_dec_i
               LD   A,(instr+1)
; Check for trailing JR NZ,d
               CP   32
               JR   NZ,dec_i_jp_nz
               LD   A,(instr+2)
; Must be jumping back to the DEC instruction
               CP   0-3
               JR   NZ,norm_dec_i
;
               XOR  A
               LD   (DE),A
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   C,1
               DEC  C
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
; Skip the DEC and the JR NZ instructions
               INC  HL
               INC  HL
               INC  HL
               RET
;
; Check for trailing JP NZ,nn
   dec_i_jp_nz:CP   194
               JR   NZ,norm_dec_i
               POP  HL
               LD   A,(instr+1)
               CP   L
; If the low bytes match we'll execute the JP as well
               JR   NZ,norm_dec_i
               POP  HL
               JP   single_step
;
    norm_dec_i:LD   HL,(reg_flags)
               PUSH HL
               EX   DE,HL
               POP  AF
               DEC  (HL)
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
;
; LD r,n  (indexable)
;
      i_ld_a_n:LD   A,(instr+1)
               LD   (reg_a),A
               POP  HL
               INC  HL
               INC  HL
               RET
      i_ld_b_n:LD   A,(instr+1)
               LD   (reg_b),A
               POP  HL
               INC  HL
               INC  HL
               RET
      i_ld_c_n:LD   A,(instr+1)
               LD   (reg_c),A
               POP  HL
               INC  HL
               INC  HL
               RET
      i_ld_d_n:LD   A,(instr+1)
               LD   (reg_d),A
               POP  HL
               INC  HL
               INC  HL
               RET
      i_ld_e_n:LD   A,(instr+1)
               LD   (reg_e),A
               POP  HL
               INC  HL
               INC  HL
               RET
      i_ld_h_n:LD   A,(instr+1)
               LD   (reg_h),A
               POP  HL
               INC  HL
               INC  HL
               RET
       ld_ih_n:INC  E
               LD   A,(instr+1)
               LD   (DE),A
               POP  HL
               INC  HL
               INC  HL
               RET
      i_ld_l_n:LD   A,(instr+1)
               LD   (reg_l),A
               POP  HL
               INC  HL
               INC  HL
               RET
       ld_il_n:LD   A,(instr+1)
               LD   (DE),A
               POP  HL
               INC  HL
               INC  HL
               RET
;
; LD r,(HL)  (indexable)
;
 i_ld_a_ind_hl:LD   DE,reg_a
               LD   HL,(reg_l)
               LD   BC,1
               RST  16
               POP  HL
               INC  HL
               RET
 i_ld_b_ind_hl:LD   DE,reg_b
               LD   HL,(reg_l)
               LD   BC,1
               RST  16
               POP  HL
               INC  HL
               RET
 i_ld_c_ind_hl:LD   DE,reg_c
               LD   HL,(reg_l)
               LD   BC,1
               RST  16
               POP  HL
               INC  HL
               RET
 i_ld_d_ind_hl:LD   DE,reg_d
               LD   HL,(reg_l)
               LD   BC,1
               RST  16
               POP  HL
               INC  HL
               RET
 i_ld_e_ind_hl:LD   DE,reg_e
               LD   HL,(reg_l)
               LD   BC,1
               RST  16
               POP  HL
               INC  HL
               RET
 i_ld_h_ind_hl:LD   DE,reg_h
               LD   HL,(reg_l)
               LD   BC,1
               RST  16
               POP  HL
               INC  HL
               RET
 i_ld_l_ind_hl:LD   DE,reg_l
               LD   HL,(reg_l)
               LD   BC,1
               RST  16
               POP  HL
               INC  HL
               RET
;
 ld_b_ind_indx:LD   HL,reg_b
               JP   ld_r_ind_indx
 ld_c_ind_indx:LD   HL,reg_c
               JP   ld_r_ind_indx
 ld_d_ind_indx:LD   HL,reg_d
               JP   ld_r_ind_indx
 ld_e_ind_indx:LD   HL,reg_e
               JP   ld_r_ind_indx
 ld_h_ind_indx:LD   HL,reg_h
               JP   ld_r_ind_indx
 ld_l_ind_indx:LD   HL,reg_l
               JP   ld_r_ind_indx
 ld_a_ind_indx:LD   HL,reg_a
;
; LD r,(IX+d)
 ld_r_ind_indx:EX   DE,HL
               LD   C,(HL)
               INC  L
               LD   B,(HL)
               LD   A,(instr+1)
               LD   L,A
               RLA
               SBC  A,A
               LD   H,A
               ADD  HL,BC
               LD   BC,1
               RST  16
               POP  HL
               INC  HL
               INC  HL
               RET
;
; LD (HL),n
; Checks for the clearing loop used in Speccy ROM reset
; code - speeds things up nicely :-)
;
 i_ld_ind_hl_n:LD   A,(instr+1)
               CP   2
               JR   NZ,norm_ld_indhln
               BIT  0,(IY+cpu_flags)
; No short-circuit if nor executing properly
               JR   Z,norm_ld_indhln
               POP  DE
               LD   HL,#11DC
               SBC  HL,DE
               JR   NZ,norm_indhln_p
               LD   HL,(reg_l)
               DEC  HL
               LD   (reg_l),HL
               INC  HL
               LD   DE,instr+1
               EX   DE,HL
               CALL put_byte
               LD   A,(reg_h)
               LD   HL,(reg_flags)
               PUSH HL
               LD   H,A
               POP  AF
               CP   H
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               LD   HL,#11E0
               RET
;
 norm_indhln_p:PUSH DE
norm_ld_indhln:LD   HL,instr+1
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               INC  HL
               INC  HL
               RET
; LD (IX+d),n
 ld_ind_indx_n:POP  HL
               INC  HL
               PUSH HL
               LD   HL,instr+2
               JP   ld_ind_indx_r
;
; LD (HL),r  (indexable)
;
 i_ld_ind_hl_a:LD   HL,reg_a
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               INC  HL
               RET
 i_ld_ind_hl_b:LD   HL,reg_b
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               INC  HL
               RET
 i_ld_ind_hl_c:LD   HL,reg_c
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               INC  HL
               RET
 i_ld_ind_hl_d:LD   HL,reg_d
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               INC  HL
               RET
 i_ld_ind_hl_e:LD   HL,reg_e
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               INC  HL
               RET
 i_ld_ind_hl_h:LD   HL,reg_h
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               INC  HL
               RET
 i_ld_ind_hl_l:LD   HL,reg_l
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               INC  HL
               RET
;
 ld_ind_indx_b:LD   HL,reg_b
               JP   ld_ind_indx_r
 ld_ind_indx_c:LD   HL,reg_c
               JP   ld_ind_indx_r
 ld_ind_indx_d:LD   HL,reg_d
               JP   ld_ind_indx_r
 ld_ind_indx_e:LD   HL,reg_e
               JP   ld_ind_indx_r
 ld_ind_indx_h:LD   HL,reg_h
               JP   ld_ind_indx_r
 ld_ind_indx_l:LD   HL,reg_l
               JP   ld_ind_indx_r
 ld_ind_indx_a:LD   HL,reg_a
;
; LD (IX+d),r
 ld_ind_indx_r:EX   DE,HL
               LD   C,(HL)
               INC  L
               LD   B,(HL)
               LD   A,(instr+1)
               LD   L,A
               RLA
               SBC  A,A
               LD   H,A
               ADD  HL,BC
               EX   DE,HL
               CALL put_byte
               POP  HL
               INC  HL
               INC  HL
               RET
;
; LD HL,(nn)  and  LD (nn),HL
;
i_ld_hl_ind_nn:LD   DE,reg_l
   ld_i_ind_nn:LD   HL,(instr+1)
               LD   BC,2
               RST  16
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
i_ld_ind_nn_hl:LD   DE,reg_l
   ld_ind_nn_i:LD   HL,(instr+1)
               EX   DE,HL
               LD   C,2
               CALL put_bytes
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
;
; ADD HL,dd  (indexable)
;
   i_add_hl_bc:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               LD   DE,(reg_c)
               ADD  HL,DE
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
   i_add_hl_sp:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               LD   DE,(reg_spl)
               ADD  HL,DE
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
   i_add_hl_hl:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               ADD  HL,HL
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
       add_i_i:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               EX   DE,HL
               LD   E,(HL)
               INC  L
               LD   D,(HL)
               EX   DE,HL
               ADD  HL,HL
               EX   DE,HL
               LD   (HL),D
               DEC  L
               LD   (HL),E
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
   i_add_hl_de:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_e)
               LD   DE,(reg_l)
               ADD  HL,DE
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
      add_i_bc:LD   HL,(reg_flags)
               PUSH HL
               LD   HL,(reg_c)
               JP   add_i_dd
      add_i_sp:LD   HL,(reg_flags)
               PUSH HL
               LD   HL,(reg_spl)
               JP   add_i_dd
      add_i_de:LD   HL,(reg_flags)
               PUSH HL
               LD   HL,(reg_e)
      add_i_dd:POP  AF
               EX   DE,HL
               LD   C,(HL)
               INC  L
               LD   B,(HL)
               EX   DE,HL
               ADD  HL,BC
               PUSH AF
               EX   DE,HL
               LD   (HL),D
               DEC  L
               LD   (HL),E
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               RET
;
; DEC (HL)  checks for trailing JR Z,+3  (Speccy reset)
;
  i_dec_ind_hl:BIT  0,(IY+cpu_flags)
; No short-circuit if not executing properly
               JR   Z,norm_dec_indhl
               POP  DE
               LD   HL,#11E9
               AND  A
               SBC  HL,DE
; Jump if not a JR Z,+3
               JR   NZ,norm_decindhlp
               LD   HL,zeros
               LD   DE,(reg_l)
; LD (HL),0
               CALL put_byte
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               LD   DE,(reg_e)
; The next 4 instructions are REAL!
               AND  A
               SBC  HL,DE
               ADD  HL,DE
               INC  HL
               LD   (reg_l),HL
               LD   (reg_e),DE
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               LD   HL,#11E7
               RET
;
norm_decindhlp:PUSH DE
norm_dec_indhl:LD   HL,(reg_l)
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,temp_store
               DEC  (HL)
               PUSH AF
               POP  DE
               LD   (reg_flags),DE
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               INC  HL
               RET
;
; DEC (IX+d)
;
  dec_ind_indx:EX   DE,HL
               LD   C,(HL)
               INC  L
               LD   B,(HL)
               LD   A,(instr+1)
               LD   L,A
               RLA
               SBC  A,A
               LD   H,A
               ADD  HL,BC
               PUSH HL
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,temp_store
               DEC  (HL)
               POP  DE
               PUSH AF
               CALL put_byte
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
;
; INC (HL)  (indexable)
;
  i_inc_ind_hl:LD   HL,(reg_l)
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,temp_store
               INC  (HL)
               PUSH AF
               POP  DE
               LD   (reg_flags),DE
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               INC  HL
               RET
; INC (IX+d)
  inc_ind_indx:EX   DE,HL
               LD   C,(HL)
               INC  L
               LD   B,(HL)
               LD   A,(instr+1)
               LD   L,A
               RLA
               SBC  A,A
               LD   H,A
               ADD  HL,BC
               PUSH HL
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,temp_store
               INC  (HL)
               POP  DE
               PUSH AF
               CALL put_byte
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
;
; JP (HL)  (indexable)
;
   i_jp_ind_hl:LD   HL,(reg_l)
               POP  DE
               RET
      jp_ind_i:EX   DE,HL
               LD   E,(HL)
               INC  L
               LD   D,(HL)
               POP  HL
               EX   DE,HL
               RET
;
; NEG
;
         i_neg:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               NEG
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
;
; SBC HL,dd
;
   i_sbc_hl_sp:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               LD   DE,(reg_spl)
               SBC  HL,DE
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
   i_sbc_hl_bc:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               LD   DE,(reg_c)
               SBC  HL,DE
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
   i_sbc_hl_hl:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               SBC  HL,HL
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
   i_sbc_hl_de:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               LD   DE,(reg_e)
               SBC  HL,DE
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
;
; ADC HL,dd
;
   i_adc_hl_sp:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               LD   DE,(reg_spl)
               ADC  HL,DE
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
   i_adc_hl_bc:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               LD   DE,(reg_c)
               ADC  HL,DE
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
   i_adc_hl_hl:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               ADC  HL,HL
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
   i_adc_hl_de:LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               LD   DE,(reg_e)
               ADC  HL,DE
               LD   (reg_l),HL
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
;
;
; LD (nn),dd  and  LD dd,(nn)
;
i_ld_ind_nn_bc:POP  HL
               PUSH HL
               INC  HL
               INC  HL
               LD   DE,ld_bc_addr+1
               LD   BC,2
               RST  16
               LD   HL,reg_c
    ld_bc_addr:LD   DE,#FFFF
               LD   BC,2
               CALL put_bytes
               POP  HL
               LD   DE,4
               ADD  HL,DE
               RET
i_ld_bc_ind_nn:POP  HL
               PUSH HL
               INC  HL
               INC  HL
               LD   DE,ld_addr_bc+1
               LD   BC,2
               RST  16
    ld_addr_bc:LD   HL,#FFFF
               LD   DE,reg_c
               LD   BC,2
               RST  16
               POP  HL
               LD   DE,4
               ADD  HL,DE
               RET
i_ld_ind_nn_de:POP  HL
               PUSH HL
               INC  HL
               INC  HL
               LD   DE,ld_de_addr+1
               LD   BC,2
               RST  16
               LD   HL,reg_e
    ld_de_addr:LD   DE,#FFFF
               LD   BC,2
               CALL put_bytes
               POP  HL
               LD   DE,4
               ADD  HL,DE
               RET
i_ld_de_ind_nn:POP  HL
               PUSH HL
               INC  HL
               INC  HL
               LD   DE,ld_addr_de+1
               LD   BC,2
               RST  16
    ld_addr_de:LD   HL,#FFFF
               LD   DE,reg_e
               LD   BC,2
               RST  16
               POP  HL
               LD   DE,4
               ADD  HL,DE
               RET
;
 i_ld_ed_hl_nn:POP  HL
               PUSH HL
               INC  HL
               INC  HL
               LD   DE,ld_addr_hl+1
               LD   BC,2
               RST  16
    ld_addr_hl:LD   HL,#FFFF
               LD   DE,reg_l
               LD   BC,2
               RST  16
               POP  HL
               LD   DE,4
               ADD  HL,DE
               RET
 i_ld_nn_ed_hl:POP  HL
               PUSH HL
               INC  HL
               INC  HL
               LD   DE,ld_hl_addr+1
               LD   BC,2
               RST  16
               LD   HL,reg_l
    ld_hl_addr:LD   DE,#FFFF
               LD   BC,2
               CALL put_bytes
               POP  HL
               LD   DE,4
               ADD  HL,DE
               RET
;
i_ld_ind_nn_sp:POP  HL
               PUSH HL
               INC  HL
               INC  HL
               LD   DE,ld_sp_addr+1
               LD   BC,2
               RST  16
               LD   HL,reg_spl
    ld_sp_addr:LD   DE,#FFFF
               LD   BC,2
               CALL put_bytes
               POP  HL
               LD   DE,4
               ADD  HL,DE
               RET
i_ld_sp_ind_nn:POP  HL
               PUSH HL
               INC  HL
               INC  HL
               LD   DE,ld_addr_sp+1
               LD   BC,2
               RST  16
    ld_addr_sp:LD   HL,#FFFF
               LD   DE,reg_spl
               LD   BC,2
               RST  16
               POP  HL
               LD   DE,4
               ADD  HL,DE
               RET
;
; LDI - Checks for up to 3 trailing LDIs for speed-up
;
         i_ldi:LD   A,(instr+2)
               CP   #ED
               JP   NZ,single_ldi
               BIT  0,(IY+cpu_flags)
; Only single LDI allowed when not executing properly
               JP   Z,single_ldi
               LD   HL,(reg_e)
               LD   DE,(reg_l)
               SBC  HL,DE
               JR   C,multiple_ldi
               LD   BC,0-4
               ADD  HL,BC
               JR   NC,single_ldi
  multiple_ldi:POP  HL
               INC  HL
               INC  HL
               PUSH HL
               INC  HL
               LD   DE,ldi_buffer
               LD   BC,5
               RST  16
               POP  BC
               LD   A,(ldi_buffer)
               CP   #A0
               LD   A,1
               JR   NZ,got_ldi_times
               INC  BC
               INC  BC
               ADD  A,1
               LD   DE,#A0ED
               LD   HL,(ldi_buffer+1)
               SBC  HL,DE
               JR   NZ,got_ldi_times
               ADD  A,1
               INC  BC
               INC  BC
               LD   HL,(ldi_buffer+3)
               SBC  HL,DE
               JR   NZ,got_ldi_times
               INC  A
               INC  BC
               INC  BC
 got_ldi_times:PUSH BC
               LD   C,A
               LD   B,0
               PUSH BC
               LD   HL,(reg_l)
               LD   DE,ldi_buffer
               RST  16
               LD   HL,(reg_l)
               POP  BC
               ADD  HL,BC
               LD   (reg_l),HL
               LD   HL,ldi_buffer
               LD   DE,(reg_e)
               PUSH BC
               CALL put_bytes
               LD   HL,(reg_e)
               POP  DE
               ADD  HL,DE
               LD   (reg_e),HL
               LD   HL,(reg_flags)
               PUSH HL
               LD   HL,(reg_c)
               AND  A
               SBC  HL,DE
               LD   (reg_c),HL
               INC  HL
               LD   C,L
               LD   B,H
               LD   E,C
               LD   D,B
               POP  AF
               LDI
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               RET
;
    ldi_buffer:DS   5
;
    single_ldi:LD   HL,(reg_l)
               INC  HL
               LD   (reg_l),HL
               DEC  HL
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   HL,(reg_e)
               INC  HL
               LD   (reg_e),HL
               DEC  HL
               LD   DE,temp_store
               EX   DE,HL
               CALL put_byte
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   H,C
               LD   L,C
               LD   D,C
               LD   E,C
               LD   BC,(reg_c)
               LDI
               LD   (reg_c),BC
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET

;
; LDD - may be optimised
;
         i_ldd:LD   HL,(reg_l)
               DEC  HL
               LD   (reg_l),HL
               INC  HL
               LD   BC,1
               LD   DE,temp_store
               RST  16
               LD   HL,(reg_e)
               DEC  HL
               LD   (reg_e),HL
               INC  HL
               LD   DE,temp_store
               EX   DE,HL
               CALL put_byte
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   H,C
               LD   L,C
               LD   D,C
               LD   E,C
               LD   BC,(reg_c)
               LDD
               LD   (reg_c),BC
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
;
; LDIR
;      Non-overlapping blocks are faster then overlappin
;      ones. If using as block fill it is treated as a
;      special case. Needs a fix for BC=0
;
        i_ldir:LD   HL,(reg_l)
               LD   DE,(reg_e)
               LD   BC,(reg_c)
               LD   A,B
               OR   C
               JR   Z,ldir_length_0
               SBC  HL,DE
               JR   C,from_to
               SBC  HL,BC
               JP   ldir_decision
; Trying to move zero bytes is surely a bug!
 ldir_length_0:POP  HL
               JP   execute_break
       from_to:LD   A,H
               AND  L
               INC  A
               JR   Z,ldir_fill
               ADD  HL,BC
 ldir_decision:LD   HL,(reg_l)
; Jump if blocks overlap
               JP   C,ldir_lp
               INC  B
               DEC  B
; Jump if moving less than 256 bytes
               JR   Z,ldir_no_high
 ldir_block_lp:PUSH BC
               PUSH HL
               PUSH DE
               LD   DE,buffer
               LD   BC,256
               RST  16
               POP  DE
               PUSH DE
               LD   HL,buffer
               LD   B,1
               CALL put_block
               POP  DE
               INC  D
               POP  HL
               INC  H
               POP  BC
               DJNZ ldir_block_lp
  ldir_no_high:INC  C
               DEC  C
               JR   Z,ldir_out
               PUSH HL
               PUSH BC
               PUSH DE
               LD   DE,buffer
               RST  16
               POP  DE
               POP  BC
               PUSH BC
               PUSH DE
               LD   HL,buffer
               CALL put_bytes
               POP  HL
               POP  BC
               ADD  HL,BC
               EX   DE,HL
               POP  HL
               ADD  HL,BC
               LD   C,B
               JP   ldir_out
;
     ldir_fill:PUSH BC
               PUSH DE
               LD   HL,(reg_l)
               LD   DE,fill_byte+1
               LD   BC,1
               RST  16
               LD   HL,buffer
     fill_byte:LD   BC,0
  ldir_fill_lp:LD   (HL),C
               INC  HL
               DJNZ ldir_fill_lp
               POP  DE
               POP  BC
               INC  B
               DEC  B
               JR   Z,fill_rest
 ldir_fill_blk:PUSH BC
               PUSH DE
               LD   HL,buffer
               LD   BC,256
               CALL put_block
               POP  DE
               INC  D
               POP  BC
               DJNZ ldir_fill_blk
     fill_rest:INC  C
               DEC  C
               JR   Z,fill_done
               PUSH BC
               PUSH DE
               LD   HL,buffer
               CALL put_block
               POP  DE
               POP  BC
     fill_done:EX   DE,HL
               ADD  HL,BC
               LD   E,L
               LD   D,H
               DEC  HL
               LD   C,B
               JR   ldir_out
;
       ldir_lp:PUSH BC
               PUSH HL
               PUSH DE
               LD   DE,temp_store
               LD   BC,1
               RST  16
               POP  DE
               PUSH DE
               LD   HL,temp_store
               CALL put_byte
               POP  DE
               INC  DE
               POP  HL
               INC  HL
               POP  BC
               DEC  BC
               LD   A,B
               OR   C
               JP   NZ,ldir_lp
      ldir_out:LD   (reg_l),HL
               LD   (reg_e),DE
               LD   (reg_c),BC
               LD   A,(reg_flags)
               AND  %11101001
               LD   (reg_flags),A
               POP  HL
               INC  HL
               INC  HL
               RET

;
; LDDR - Needs speeding up like LDIR
;
        i_lddr:LD   BC,(reg_c)
               LD   HL,(reg_l)
               LD   DE,(reg_e)
       lddr_lp:PUSH BC
               PUSH HL
               PUSH DE
               LD   DE,temp_store
               LD   BC,1
               RST  16
               POP  DE
               PUSH DE
               LD   HL,temp_store
               CALL put_byte
               POP  DE
               DEC  DE
               POP  HL
               DEC  HL
               POP  BC
               DEC  BC
               LD   A,B
               OR   C
               JP   NZ,lddr_lp
               LD   (reg_l),HL
               LD   (reg_e),DE
               LD   (reg_c),BC
               LD   A,(reg_flags)
               AND  %11101001
               LD   (reg_flags),A
               POP  HL
               INC  HL
               INC  HL
               RET
;
; OTIR  and  OTDR
;
        i_otir:LD   A,35
               JP   otxr
        i_otdr:LD   A,43
          otxr:LD   (otxr_code),A
               LD   BC,(reg_c)
               LD   HL,(reg_l)
       otxr_lp:PUSH HL
               PUSH BC
               LD   DE,temp_store
               LD   BC,1
               RST  16
               POP  BC
               DEC  B
               POP  HL
               PUSH BC
               EXX
               POP  BC
               LD   A,(temp_store)
               CALL write_port
               EXX
     otxr_code:DB   0
               LD   A,B
               AND  A
               JR   NZ,otxr_lp
               LD   (reg_l),HL
               LD   (reg_c),BC
               LD   A,(reg_flags)
               OR   %01000010
               LD   (reg_flags),A
               POP  HL
               INC  HL
               INC  HL
               RET
;
; OUTD  and  OUTI
;
        i_outd:LD   HL,(reg_l)
               DEC  HL
               LD   (reg_l),HL
               INC  HL
               JR   outx
        i_outi:LD   HL,(reg_l)
               INC  HL
               LD   (reg_l),HL
               DEC  HL
          outx:LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   A,(temp_store)
               LD   BC,(reg_c)
               DEC  B
               LD   (reg_c),BC
               PUSH AF
               CALL write_port
               POP  AF
               LD   HL,reg_flags
;           Set N flag
               LD   BC,1
               SET  1,(HL)
;           Reset Z flag
               RES  6,(HL)
               JP   NZ,len_2
;           Z set since B=0 now
               SET  6,(HL)
               POP  HL
               INC  HL
               INC  HL
               RET
;
; CPDR  and  CPIR
;
        i_cpdr:LD   A,43
               JP   cpxr
        i_cpir:LD   A,35
          cpxr:LD   (cpxr_dir),A
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,(reg_l)
               LD   BC,(reg_c)
     cpxr_loop:LD   DE,temp_store
               PUSH HL
               PUSH BC
               PUSH AF
               LD   BC,1
               RST  16
               LD   HL,temp_store
               POP  AF
               POP  BC
               CPI
               POP  HL
      cpxr_dir:INC  HL
               JR   Z,cpxr_done
               JP   PE,cpxr_loop
     cpxr_done:LD   (reg_l),HL
               LD   (reg_c),BC
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET

;
; CPI and CPD
;
         i_cpi:LD   HL,(reg_l)
               INC  HL
               LD   (reg_l),HL
               DEC  HL
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   BC,(reg_c)
               LD   HL,temp_store
               CPI
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               LD   (reg_c),BC
               POP  HL
               INC  HL
               INC  HL
               RET
;
         i_cpd:LD   HL,(reg_l)
               DEC  HL
               LD   (reg_l),HL
               INC  HL
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   BC,(reg_c)
               LD   HL,temp_store
               CPD
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               LD   (reg_c),BC
               POP  HL
               INC  HL
               INC  HL
               RET

;
; INI and IND
;
         i_ind:LD   A,#AA
               JP   inx
         i_ini:LD   A,#A2
           inx:LD   (inx_code+1),A
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   BC,(reg_c)
               LD   HL,temp_store
      inx_code:INI
               PUSH AF
               LD   DE,temp_store
               AND  A
               SBC  HL,DE
               LD   DE,(reg_l)
               ADD  HL,DE
               LD   (reg_l),HL
               EX   (SP),HL
               LD   (reg_flags),HL
               LD   (reg_c),BC
               CALL read_port
               LD   (temp_store),A
               POP  DE
               LD   HL,temp_store
               LD   BC,1
               CALL put_bytes
               POP  HL
               INC  HL
               INC  HL
               RET

;
; HALT - needs special attention for different modes
;
        i_halt:LD   A,(reg_iff1)
               BIT  2,A
               POP  HL
; Stop executing if HALT with interrupts disabled
               JP   Z,execute_break
               INC  HL
               BIT  0,(IY+cpu_flags)
               RET  Z
               LD   A,(IY+int_delay)
               INC  A
; Skip halt if no interrupts are required
               RET  Z
               DEC  HL
               CP   2
; Leave PC at HALT until interrupts advance it
               RET  NC
               LD   (reg_pcl),HL
; We're at a HALT so we can generate an interrupt
               JP   generate_int
;
; RLD
;
         i_rld:LD   HL,(reg_l)
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,temp_store
               RLD
               PUSH AF
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
;
; RRD
;
         i_rrd:LD   HL,(reg_l)
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,temp_store
               RRD
               PUSH AF
               LD   DE,(reg_l)
               CALL put_byte
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
;
; IM 0 , IM 1 , IM 2
;
        i_im_0:XOR  A
               LD   (reg_int_mode),A
               POP  HL
               INC  HL
               INC  HL
               RET
        i_im_1:LD   A,1
               LD   (reg_int_mode),A
               POP  HL
               INC  HL
               INC  HL
               RET
        i_im_2:LD   A,2
               LD   (reg_int_mode),A
               POP  HL
               INC  HL
               INC  HL
               RET

        i_inir:LD   HL,(reg_c)
          inxr:LD   DE,(reg_l)
               ADD  HL,DE
               LD   (reg_l),HL
               LD   HL,0
               LD   (reg_c),HL
               JP   len_2
;
        i_indr:LD   HL,0
               LD   DE,(reg_c)
               AND  A
               SBC  HL,DE
               JR   inxr
