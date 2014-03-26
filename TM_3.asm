;
; I N S T R U C T I O N    D E C O D E    T A B L E S
;
; D E C O D E    C O D E   F O L L O W S    T A B L E S
;

 decode_tables:EQU  512
; norm_div_2 = decode_tables/512
    norm_div_2:EQU  1
      ed_div_2:EQU  norm_div_2+1
   index_div_2:EQU  ed_div_2+1
      cb_div_2:EQU  index_div_2+1
;
               ORG  decode_tables
               PUT  put_base+decode_tables
;
  normal_table:DW   len_1
;                   NOP
               DW   i_ld_bc_nn
               DW   i_ld_ind_bc_a
               DW   i_inc_bc
               DW   i_inc_b
               DW   i_dec_b
               DW   i_ld_b_n
               DW   i_simple_1
;                   RLCA
;   008:
               DW   i_ex_af_af
               DW   i_add_hl_bc
               DW   i_ld_a_ind_bc
               DW   i_dec_bc
               DW   i_inc_c
               DW   i_dec_c
               DW   i_ld_c_n
               DW   i_simple_1
;                   RRCA
;;  016:
               DW   i_djnz_e
               DW   i_ld_de_nn
               DW   i_ld_ind_de_a
               DW   i_inc_de
               DW   i_inc_d
               DW   i_dec_d
               DW   i_ld_d_n
               DW   i_simple_1
;                   RLA
;   024:
               DW   i_jr_e
               DW   i_add_hl_de
               DW   i_ld_a_ind_de
               DW   i_dec_de
               DW   i_inc_e
               DW   i_dec_e
               DW   i_ld_e_n
               DW   i_simple_1
;                   RRA
;;; 032:
               DW   i_jr_nz_e
               DW   i_ld_hl_nn
               DW   i_ld_ind_nn_hl
               DW   i_inc_hl
               DW   i_inc_h
               DW   i_dec_h
               DW   i_ld_h_n
               DW   i_simple_1
;                   DAA
;   040:
               DW   i_jr_z_e
               DW   i_add_hl_hl
               DW   i_ld_hl_ind_nn
               DW   i_dec_hl
               DW   i_inc_l
               DW   i_dec_l
               DW   i_ld_l_n
               DW   i_simple_1
;                   CPL
;;  048:
               DW   i_jr_nc_e
               DW   i_ld_sp_nn
               DW   i_ld_ind_nn_a
               DW   i_inc_sp
               DW   i_inc_ind_hl
               DW   i_dec_ind_hl
               DW   i_ld_ind_hl_n
               DW   i_simple_1
;                   SCF
;   056:
               DW   i_jr_c_e
               DW   i_add_hl_sp
               DW   i_ld_a_ind_nn
               DW   i_dec_sp
               DW   i_inc_a
               DW   i_dec_a
               DW   i_ld_a_n
               DW   i_simple_1
;                   CCF
;;; 064:
               DW   len_1
;                   LD B,B
               DW   i_ld_b_c
               DW   i_ld_b_d
               DW   i_ld_b_e
               DW   i_ld_b_h
               DW   i_ld_b_l
               DW   i_ld_b_ind_hl
               DW   i_ld_b_a
;   072:
               DW   i_ld_c_b
               DW   len_1
;                   LD C,C
               DW   i_ld_c_d
               DW   i_ld_c_e
               DW   i_ld_c_h
               DW   i_ld_c_l
               DW   i_ld_c_ind_hl
               DW   i_ld_c_a
;;  080:
               DW   i_ld_d_b
               DW   i_ld_d_c
               DW   len_1
;                   LD D,D
               DW   i_ld_d_e
               DW   i_ld_d_h
               DW   i_ld_d_l
               DW   i_ld_d_ind_hl
               DW   i_ld_d_a
;   088:
               DW   i_ld_e_b
               DW   i_ld_e_c
               DW   i_ld_e_d
               DW   len_1
;                   LD E,E
               DW   i_ld_e_h
               DW   i_ld_e_l
               DW   i_ld_e_ind_hl
               DW   i_ld_e_a
;;; 096:
               DW   i_ld_h_b
               DW   i_ld_h_c
               DW   i_ld_h_d
               DW   i_ld_h_e
               DW   len_1
;                   LD H,H
               DW   i_ld_h_l
               DW   i_ld_h_ind_hl
               DW   i_ld_h_a
;   104:
               DW   i_ld_l_b
               DW   i_ld_l_c
               DW   i_ld_l_d
               DW   i_ld_l_e
               DW   i_ld_l_h
               DW   len_1
;                   LD L,L
               DW   i_ld_l_ind_hl
               DW   i_ld_l_a
;;  112:
               DW   i_ld_ind_hl_b
               DW   i_ld_ind_hl_c
               DW   i_ld_ind_hl_d
               DW   i_ld_ind_hl_e
               DW   i_ld_ind_hl_h
               DW   i_ld_ind_hl_l
               DW   i_halt
               DW   i_ld_ind_hl_a
;   120:
               DW   i_ld_a_b
               DW   i_ld_a_c
               DW   i_ld_a_d
               DW   i_ld_a_e
               DW   i_ld_a_h
               DW   i_ld_a_l
               DW   i_ld_a_ind_hl
               DW   len_1
;                   LD A,A
;;; 128:
;                   ADD A,r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   i_arith_h
               DW   i_arith_l
               DW   i_arith_ind_hl
               DW   i_simple_1
;   136:
;                   ADC A,r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   i_arith_h
               DW   i_arith_l
               DW   i_arith_ind_hl
               DW   i_simple_1
;;  144:
;                   SUB r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   i_arith_h
               DW   i_arith_l
               DW   i_arith_ind_hl
               DW   i_simple_1
;   152:
;                   SBC A,r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   i_arith_h
               DW   i_arith_l
               DW   i_arith_ind_hl
               DW   i_simple_1
;;; 160:
;                   AND r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   i_arith_h
               DW   i_arith_l
               DW   i_arith_ind_hl
               DW   i_simple_1
;   168:
;                   XOR r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   i_arith_h
               DW   i_arith_l
               DW   i_arith_ind_hl
               DW   i_simple_1
;;  176:
;                   OR r
               DW   i_or_b
               DW   i_or_c
               DW   i_or_d
               DW   i_or_e
               DW   i_or_h
               DW   i_or_l
               DW   i_arith_ind_hl
               DW   i_simple_1
;   184:
;                   CP r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   i_arith_h
               DW   i_arith_l
               DW   i_arith_ind_hl
               DW   i_simple_1
;;; 192:
               DW   i_ret_cc
               DW   i_pop_bc
               DW   i_jp_nz_nn
               DW   i_jp_nn
               DW   i_call_cc_nn
               DW   i_push_bc
               DW   i_simple_2
;                   ADD A,n
               DW   i_rst_p
;   200:
               DW   i_ret_cc
               DW   i_ret
               DW   i_jp_z_nn
               DW   cb_prefix
               DW   i_call_cc_nn
               DW   i_call_nn
               DW   i_simple_2
;                   ADC A,n
               DW   i_rst_p
;;  208:
               DW   i_ret_cc
               DW   i_pop_de
               DW   i_jp_nc_nn
               DW   i_out_ind_n_a
               DW   i_call_cc_nn
               DW   i_push_de
               DW   i_simple_2
;                   SUB n
               DW   i_rst_p
;   216:
               DW   i_ret_cc
               DW   i_exx
               DW   i_jp_c_nn
               DW   i_in_a_ind_n
               DW   i_call_cc_nn
               DW   ix_prefix
               DW   i_simple_2
;                   SBC A,n
               DW   i_rst_p
;;; 224:
               DW   i_ret_cc
               DW   i_pop_hl
               DW   i_jp_po_nn
               DW   i_ex_ind_sp_hl
               DW   i_call_cc_nn
               DW   i_push_hl
               DW   i_simple_2
;                   AND n
               DW   i_rst_p
;   232:
               DW   i_ret_cc
               DW   i_jp_ind_hl
               DW   i_jp_pe_nn
               DW   i_ex_de_hl
               DW   i_call_cc_nn
               DW   ed_prefix
               DW   i_simple_2
;                   XOR n
               DW   i_rst_p
;;  240:
               DW   i_ret_cc
               DW   i_pop_af
               DW   i_jp_p_nn
               DW   i_di
               DW   i_call_cc_nn
               DW   i_push_af
               DW   i_simple_2
;                   OR n
               DW   i_rst_p
;   248:
               DW   i_ret_cc
               DW   i_ld_sp_hl
               DW   i_jp_m_nn
               DW   i_ei
               DW   i_call_cc_nn
               DW   iy_prefix
               DW   i_simple_2
;                   CP n
               DW   i_rst_p
;;; 256:
;:
; Decode table:for ED prefix
;:
; First 64 entries are NOPs (length 2)
;
      ed_table:DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;   ED 008:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;;  ED 016:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;   ED 024:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;;; ED 032:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;   ED 040:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;;  ED 048:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;   ED 056:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;;; ED 064:
               DW   i_in_r_ind_c
               DW   i_out_ind_c_r
               DW   i_sbc_hl_bc
               DW   i_ld_ind_nn_bc
               DW   i_neg
               DW   i_retn
               DW   i_im_0
               DW   i_ld_i_a
;   ED 072:
               DW   i_in_r_ind_c
               DW   i_out_ind_c_r
               DW   i_adc_hl_bc
               DW   i_ld_bc_ind_nn
               DW   i_neg
               DW   i_ret
               DW   i_im_0
               DW   i_ld_r_a
;;  ED 080:
               DW   i_in_r_ind_c
               DW   i_out_ind_c_r
               DW   i_sbc_hl_de
               DW   i_ld_ind_nn_de
               DW   i_neg
               DW   i_retn
               DW   i_im_1
               DW   i_ld_a_i
;   ED 088:
               DW   i_in_r_ind_c
               DW   i_out_ind_c_r
               DW   i_adc_hl_de
               DW   i_ld_de_ind_nn
               DW   i_neg
               DW   i_ret
               DW   i_im_2
               DW   i_ld_a_r
;;; ED 096:
               DW   i_in_r_ind_c
               DW   i_out_ind_c_r
               DW   i_sbc_hl_hl
               DW   i_ld_nn_ed_hl
               DW   i_neg
               DW   i_retn
               DW   i_im_0
               DW   i_rrd
;   ED 104:
               DW   i_in_r_ind_c
               DW   i_out_ind_c_r
               DW   i_adc_hl_hl
               DW   i_ld_ed_hl_nn
               DW   i_neg
               DW   i_ret
               DW   i_im_0
               DW   i_rld
;;  ED 112:
               DW   i_in_x_ind_c
               DW   i_out_ind_c_r
               DW   i_sbc_hl_sp
               DW   i_ld_ind_nn_sp
               DW   i_neg
               DW   i_retn
               DW   i_im_1
               DW   len_2
;   ED 120:
               DW   i_in_r_ind_c
               DW   i_out_ind_c_r
               DW   i_adc_hl_sp
               DW   i_ld_sp_ind_nn
               DW   i_neg
               DW   i_ret
               DW   i_im_2
               DW   len_2
;;; ED 128:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;   ED 136:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;;  ED 144:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;   ED 152:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;;; ED 160:
               DW   i_ldi
               DW   i_cpi
               DW   i_ini
               DW   i_outi
               DW   len_2,len_2,len_2,len_2
;   ED 168:
               DW   i_ldd
               DW   i_cpd
               DW   i_ind
               DW   i_outd
               DW   len_2,len_2,len_2,len_2
;;  ED 176:
               DW   i_ldir
               DW   i_cpir
               DW   i_inir
               DW   i_otir
               DW   len_2,len_2,len_2,len_2
;   ED 184:
               DW   i_lddr
               DW   i_cpdr
               DW   i_indr
               DW   i_otdr
               DW   len_2,len_2,len_2,len_2
;;; ED 192:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;   ED 200:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;;  ED 208:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;   ED 216:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;;; ED 224:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;   ED 232:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;;  ED 240:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;   ED 248:
               DW   len_2,len_2,len_2,len_2
               DW   len_2,len_2,len_2,len_2
;;; ED 256:
;
    indx_table:DW   len_1
;                   NOP
               DW   i_ld_bc_nn
               DW   i_ld_ind_bc_a
               DW   i_inc_bc
               DW   i_inc_b
               DW   i_dec_b
               DW   i_ld_b_n
               DW   i_simple_1
;                   RLCA
;   008:
               DW   i_ex_af_af
               DW   add_i_bc
               DW   i_ld_a_ind_bc
               DW   i_dec_bc
               DW   i_inc_c
               DW   i_dec_c
               DW   i_ld_c_n
               DW   i_simple_1
;                   RRCA
;;  016:
               DW   i_djnz_e
               DW   i_ld_de_nn
               DW   i_ld_ind_de_a
               DW   i_inc_de
               DW   i_inc_d
               DW   i_dec_d
               DW   i_ld_d_n
               DW   i_simple_1
;                   RLA
;   024:
               DW   i_jr_e
               DW   add_i_de
               DW   i_ld_a_ind_de
               DW   i_dec_de
               DW   i_inc_e
               DW   i_dec_e
               DW   i_ld_e_n
               DW   i_simple_1
;                   RRA
;;; 032:
               DW   i_jr_nz_e
               DW   ld_i_nn
               DW   ld_ind_nn_i
               DW   inc_ii
               DW   inc_ih
               DW   dec_ih
               DW   ld_ih_n
               DW   i_simple_1
;                   DAA
;   040:
               DW   i_jr_z_e
               DW   add_i_i
               DW   ld_i_ind_nn
               DW   dec_ii
               DW   inc_i
               DW   dec_i
               DW   ld_il_n
               DW   i_simple_1
;                   CPL
;;  048:
               DW   i_jr_nc_e
               DW   i_ld_sp_nn
               DW   i_ld_ind_nn_a
               DW   i_inc_sp
               DW   inc_ind_indx
               DW   dec_ind_indx
               DW   ld_ind_indx_n
               DW   i_simple_1
;                   SCF
;   056:
               DW   i_jr_c_e
               DW   add_i_sp
               DW   i_ld_a_ind_nn
               DW   i_dec_sp
               DW   i_inc_a
               DW   i_dec_a
               DW   i_ld_a_n
               DW   i_simple_1
;                   CCF
;;; 064:
               DW   len_1
;                   LD B,B
               DW   i_ld_b_c
               DW   i_ld_b_d
               DW   i_ld_b_e
               DW   ld_b_ih
               DW   ld_b_il
               DW   ld_b_ind_indx
               DW   i_ld_b_a
;   072:
               DW   i_ld_c_b
               DW   len_1
;                   LD C,C
               DW   i_ld_c_d
               DW   i_ld_c_e
               DW   ld_c_ih
               DW   ld_c_il
               DW   ld_c_ind_indx
               DW   i_ld_c_a
;;  080:
               DW   i_ld_d_b
               DW   i_ld_d_c
               DW   len_1
;                   LD D,D
               DW   i_ld_d_e
               DW   ld_d_ih
               DW   ld_d_il
               DW   ld_d_ind_indx
               DW   i_ld_d_a
;   088:
               DW   i_ld_e_b
               DW   i_ld_e_c
               DW   i_ld_e_d
               DW   len_1
;                   LD E,E
               DW   ld_e_ih
               DW   ld_e_il
               DW   ld_e_ind_indx
               DW   i_ld_e_a
;;; 096:
               DW   ld_ih_b
               DW   ld_ih_c
               DW   ld_ih_d
               DW   ld_ih_e
               DW   len_1
;                   LD H,H
               DW   ld_ih_il
               DW   ld_h_ind_indx
               DW   ld_ih_a
;   104:
               DW   ld_il_b
               DW   ld_il_c
               DW   ld_il_d
               DW   ld_il_e
               DW   ld_il_ih
               DW   len_1
;                   LD L,L
               DW   ld_l_ind_indx
               DW   ld_il_a
;;  112:
               DW   ld_ind_indx_b
               DW   ld_ind_indx_c
               DW   ld_ind_indx_d
               DW   ld_ind_indx_e
               DW   ld_ind_indx_h
               DW   ld_ind_indx_l
               DW   i_halt
               DW   ld_ind_indx_a
;   120:
               DW   i_ld_a_b
               DW   i_ld_a_c
               DW   i_ld_a_d
               DW   i_ld_a_e
               DW   ld_a_ih
               DW   ld_a_il
               DW   ld_a_ind_indx
               DW   len_1
;                   LD A,A
;;; 128:
;                   ADD A,r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   arith_ih
               DW   arith_i
               DW   arith_ind_indx
               DW   i_simple_1
;   136:
;                   ADC A,r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   arith_ih
               DW   arith_i
               DW   arith_ind_indx
               DW   i_simple_1
;;  144:
;                   SUB r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   arith_ih
               DW   arith_i
               DW   arith_ind_indx
               DW   i_simple_1
;   152:
;                   SBC A,r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   arith_ih
               DW   arith_i
               DW   arith_ind_indx
               DW   i_simple_1
;;; 160:
;                   AND r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   arith_ih
               DW   arith_i
               DW   arith_ind_indx
               DW   i_simple_1
;   168:
;                   XOR r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   arith_ih
               DW   arith_i
               DW   arith_ind_indx
               DW   i_simple_1
;;  176:
;                   OR r
               DW   i_or_b
               DW   i_or_c
               DW   i_or_d
               DW   i_or_e
               DW   i_or_ih
               DW   or_i
               DW   arith_ind_indx
               DW   i_simple_1
;   184:
;                   CP r
               DW   i_arith_b
               DW   i_arith_c
               DW   i_arith_d
               DW   i_arith_e
               DW   arith_ih
               DW   arith_i
               DW   arith_ind_indx
               DW   i_simple_1
;;; 192:
               DW   i_ret_cc
               DW   i_pop_bc
               DW   i_jp_nz_nn
               DW   i_jp_nn
               DW   i_call_cc_nn
               DW   i_push_bc
               DW   i_simple_2
;                   ADD A,n
               DW   i_rst_p
;   200:
               DW   i_ret_cc
               DW   i_ret
               DW   i_jp_z_nn
               DW   ddcb_prefix
               DW   i_call_cc_nn
               DW   i_call_nn
               DW   i_simple_2
;                   ADC A,n
               DW   i_rst_p
;;  208:
               DW   i_ret_cc
               DW   i_pop_de
               DW   i_jp_nc_nn
               DW   i_out_ind_n_a
               DW   i_call_cc_nn
               DW   i_push_de
               DW   i_simple_2
;                   SUB n
               DW   i_rst_p
;   216:
               DW   i_ret_cc
               DW   i_exx
               DW   i_jp_c_nn
               DW   i_in_a_ind_n
               DW   i_call_cc_nn
               DW   ix_prefix
               DW   i_simple_2
;                   SBC A,n
               DW   i_rst_p
;;; 224:
               DW   i_ret_cc
               DW   pop_i
               DW   i_jp_po_nn
               DW   ex_ind_sp_i
               DW   i_call_cc_nn
               DW   push_i
               DW   i_simple_2
;                   AND n
               DW   i_rst_p
;   232:
               DW   i_ret_cc
               DW   jp_ind_i
               DW   i_jp_pe_nn
               DW   i_ex_de_hl
               DW   i_call_cc_nn
               DW   ed_prefix
               DW   i_simple_2
;                   XOR n
               DW   i_rst_p
;;  240:
               DW   i_ret_cc
               DW   i_pop_af
               DW   i_jp_p_nn
               DW   i_di
               DW   i_call_cc_nn
               DW   i_push_af
               DW   i_simple_2
;                   OR n
               DW   i_rst_p
;   248:
               DW   i_ret_cc
               DW   ld_sp_i
               DW   i_jp_m_nn
               DW   i_ei
               DW   i_call_cc_nn
               DW   iy_prefix
               DW   i_simple_2
;                   CP n
               DW   i_rst_p
;;; 256:
;
; CB Decode table - 000 TO 255 All use same pattern of 8
;
      cb_table:DW   cb_b,cb_c,cb_d,cb_e,cb_h,cb_l
               DW   cb_ind_hl,cb_a
;:
;
; SINGLE step is now RST 32 ; (27-21)= 6 T-state saving
;
     ix_prefix:POP  HL
               INC  HL
               PUSH HL
               LD   DE,instr
               LD   BC,3
               RST  16
               LD   A,(instr)
               LD   L,A
               LD   H,index_div_2
               ADD  HL,HL
               LD   E,(HL)
               INC  L
               LD   H,(HL)
               LD   L,E
               LD   DE,reg_ixl
               JP   (HL)
;
     iy_prefix:POP  HL
               INC  HL
               PUSH HL
               LD   DE,instr
               LD   BC,3
               RST  16
               LD   A,(instr)
               LD   L,A
               LD   H,index_div_2
               ADD  HL,HL
               LD   E,(HL)
               INC  L
               LD   H,(HL)
               LD   L,E
               LD   DE,reg_iyl
               JP   (HL)
;
     cb_prefix:LD   A,(instr+1)
               LD   B,A
               AND  %00000111
               LD   L,A
               LD   H,cb_div_2
               ADD  HL,HL
               LD   E,(HL)
               INC  L
               LD   H,(HL)
               LD   L,E
               LD   A,B
               JP   (HL)
;
;                              Format: DDCBddop
   ddcb_prefix:LD   A,(instr+2)
               AND  %11111000
               OR   #06
               LD   (ddcb_code+1),A
               EX   DE,HL
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
     ddcb_code:DB   #CB,0
               POP  DE
               PUSH AF
               CALL put_byte
               POP  HL
               LD   (reg_flags),HL
               LD   A,(instr+2)
               AND  %00000111
               ADD  A,A
               LD   E,A
               LD   D,0
               LD   HL,get_r_table
               ADD  HL,DE
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               LD   A,(temp_store)
               LD   (DE),A
               POP  HL
               INC  HL
               INC  HL
               INC  HL
               RET
;
     cb_ind_hl:LD   HL,(reg_l)
               LD   DE,temp_store
               LD   BC,1
               RST  16
               LD   A,(instr+1)
               AND  %11111000
               OR   #06
               LD   (cb_ind_hl_code+1),A
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
               LD   HL,temp_store
cb_ind_hl_code:DB   #CB,0
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
          cb_a:LD   (cb_a_code+1),A
               LD   HL,(reg_flags)
               PUSH HL
               POP  AF
     cb_a_code:DB   #CB,0
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
;
          cb_b:LD   HL,reg_b
               JP   cb_r
          cb_c:LD   HL,reg_c
               JP   cb_r
          cb_d:LD   HL,reg_d
               JP   cb_r
          cb_e:LD   HL,reg_e
               JP   cb_r
          cb_h:LD   HL,reg_h
               JP   cb_r
          cb_l:LD   HL,reg_l
          cb_r:AND  %11111000
               OR   #06
               LD   (cb_r_code+1),A
               LD   DE,(reg_flags)
               PUSH DE
               POP  AF
     cb_r_code:DB   #CB,0
               PUSH AF
               POP  HL
               LD   (reg_flags),HL
               POP  HL
               INC  HL
               INC  HL
               RET
;
     ed_prefix:LD   A,(instr+1)
               LD   L,A
               LD   H,ed_div_2
               ADD  HL,HL
               LD   E,(HL)
               INC  L
               LD   H,(HL)
               LD   L,E
               JP   (HL)
;
;
; Instruction length adders, update Program Counter
;
         len_0:POP  HL
               RET
         len_1:POP  HL
               INC  L
               RET  NZ
               INC  H
               RET
         len_2:POP  HL
               INC  HL
               INC  L
               RET  NZ
               INC  H
               RET
         len_3:POP  HL
               INC  HL
               INC  HL
               INC  L
               RET  NZ
               INC  H
               RET
         len_4:POP  HL
               LD   DE,4
               ADD  HL,DE
               RET
;
; Look up table for registers held in 3 bits in the
; order: A,B,C,D,E,H,L,X,A  X=(HL)=temp_store
;
   get_r_table:DW   reg_b
               DW   reg_c
               DW   reg_d
               DW   reg_e
               DW   reg_h
               DW   reg_l
               DW   temp_store
               DW   reg_a

;
; Buffer used for various things
;
        buffer:DS   300
