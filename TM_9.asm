;
; I N P U T   N U M B E R
;
; Prompt for a number to be input with the message DE
; points to. The number is returned in HL with the Z fla
; set if a number was input. A is returned holding 128 i
; ESC was pressed and 127 if the input was blank (defaul
; settings are then possible by the calling routine).
; Uses a recursive decent parser - Compilers course was
; useful after all!
;
  input_buffer:DS   64
     input_ptr:DW   0
;
  do_input_num:CALL clear_inp_line
               SET  5,(IY+misc_flags)
               LD   HL,5888
   do_input_at:LD   (xpos),HL
               EX   DE,HL
               DI
               SET  4,(IY+misc_flags)
               CALL print_scr_msg
               CALL print_scr_pop
               DB   ":"," "+128
               RES  4,(IY+misc_flags)
               EI
               LD   HL,(cursor_xpos)
               PUSH HL
               LD   HL,(xpos)
               LD   (cursor_xpos),HL
               CALL print_cursor
               LD   HL,input_buffer
               CALL input_lp
               PUSH AF
               PUSH HL
               CALL print_cursor
               POP  HL
               POP  AF
               POP  DE
               LD   (cursor_xpos),DE
               RET
;
      input_lp:HALT
               LD   A,(IY+key)
               AND  A
               JR   Z,input_lp
               HALT
               HALT
               CP   255
               JP   Z,input_esc
               CP   13
               JR   Z,done_input
               CP   12
               JR   Z,input_delete
               CP   1
               JR   Z,input_caps
               LD   C,A
               LD   A,(xpos)
               CP   0-4-4
; Jump if line full
               JR   Z,input_lp
               LD   A,C
               CP   33
               JR   C,input_lp
               CP   128
               JR   NC,input_lp
               LD   (HL),A
               INC  HL
               PUSH HL
               PUSH AF
               CALL print_cursor
               LD   A,(cursor_xpos)
               ADD  A,4
               LD   (cursor_xpos),A
               POP  AF
               RST  8
               CALL print_cursor
               POP  HL
               JR   input_lp
;
  input_delete:EX   DE,HL
               LD   HL,input_buffer
               SBC  HL,DE
               EX   DE,HL
               JR   Z,input_lp
               PUSH HL
               CALL print_cursor
               CALL pback
               PUSH AF
               LD   A," "
               RST  8
               POP  AF
               LD   (xpos),A
               LD   A,(cursor_xpos)
               SUB  4
               LD   (cursor_xpos),A
               CALL print_cursor
               POP  HL
               DEC  HL
               JR   input_lp
;
     input_esc:LD   A,128
   input_empty:AND  A
               RET
;
    input_caps:LD   A,(IY+misc_flags)
               XOR  caps_bitval
               LD   (IY+misc_flags),A
               PUSH HL
               CALL top_menu
               POP  HL
               JP   input_lp
;
    done_input:EX   DE,HL
               LD   HL,input_buffer
               SBC  HL,DE
               EX   DE,HL
               LD   A,127
               JR   Z,input_empty
               BIT  6,(IY+misc_flags)
               RES  6,(IY+misc_flags)
               JR   Z,not_wildnum
               INC  DE
               LD   A,D
               OR   E
               JR   NZ,not_wildnum
               DEC  HL
               LD   A,(HL)
               INC  HL
               CP   "*"
               JR   NZ,not_wildnum
; We've got the wild-card number for searches
               LD   A,127
               LD   L,"*"
               AND  A
               RET
   not_wildnum:LD   (HL),cr
               LD   (input_ptr),HL
;
; Recursive evaluation time!
;
; Since the evaluation is very recursive we need a new
; BIG stack to stop input with lots of brackets from
; overwriting memory and crashing the program. The stack
; is below the font in high memory (with the screen), so
; the screen has to be paged in first.
;
               CALL page_in_screen
               LD   (warn_stack+1),SP
               LD   SP,high_stack
               LD   IX,input_buffer
               LD   A,(IX+0)
               CALL expression
               CP   cr
               JP   NZ,warn
               LD   SP,(warn_stack+1)
               RET
;
    expression:CALL simple
               CP   "&"
; Jump if arithmetic AND
               JR   Z,expr_op
               CP   "!"
; Jump if arithmetic OR
               JR   Z,expr_op
               CP   "@"
; Jump if arithmetic XOR
               JR   Z,expr_op
               CP   "?"
; Skip RET if MOD
               RET  NZ
       expr_op:PUSH HL
               PUSH AF
               INC  IX
               LD   A,(IX+0)
; Fetch 2nd operand
               CALL expression
               POP  AF
               POP  DE
               CP   "@"
               JR   Z,expr_xor
               CP   "!"
               JR   Z,expr_or
               CP   "?"
               JR   Z,expr_mod
               LD   A,D
               AND  H
               LD   H,A
               LD   A,E
               AND  L
               LD   L,A
               LD   A,(IX+0)
               RET
      expr_xor:LD   A,D
               XOR  H
               LD   H,A
               LD   A,E
               XOR  L
               LD   L,A
               LD   A,(IX+0)
               RET
       expr_or:LD   A,D
               OR   H
               LD   H,A
               LD   A,E
               OR   L
               LD   L,A
               LD   A,(IX+0)
               RET
      expr_mod:EX   DE,HL
               CALL divide_hl_de
               EX   DE,HL
               RET
;
        simple:CALL term_divide
               CP   "+"
               JR   Z,simple_op
               CP   "-"
               RET  NZ
     simple_op:PUSH HL
               CALL simple
               POP  DE
               ADD  HL,DE
               LD   A,(IX+0)
               RET
;
   term_divide:CALL term_multiply
               CP   "/"
               RET  NZ
               PUSH HL
               INC  IX
               LD   A,(IX+0)
               CALL term_divide
               POP  DE
               EX   DE,HL
  divide_hl_de:LD   A,D
               OR   E
; Don't allow division by zero
               JP   Z,warn
               LD   A,H
               LD   C,L
               LD   HL,0
               LD   B,16
        loop16:RL   C
               RLA
               ADC  HL,HL
               SBC  HL,DE
               JR   NC,subok
               ADD  HL,DE
         subok:CCF
               DJNZ loop16
               RL   C
               RLA
               EX   DE,HL
               LD   H,A
               LD   L,C
               LD   A,(IX+0)
               RET
;
 term_multiply:CALL factor
               CP   "*"
               RET  NZ
               PUSH HL
               INC  IX
               LD   A,(IX+0)
               CALL term_multiply
               POP  DE
multiply_hl_de:LD   C,H
               LD   A,L
               LD   B,16
               LD   HL,0
          mult:SRL  C
               RRA
               JR   NC,noadd
               ADD  HL,DE
         noadd:SLA  E
               RL   D
               DJNZ mult
               LD   A,(IX+0)
               RET
;
        factor:LD   E,0
     factor_lp:CP   "+"
               JR   Z,unary_sign
               CP   "-"
               JR   NZ,done_unary
               LD   A,E
; Toggle the sign for every minus seen
               CPL
               LD   E,A
    unary_sign:INC  IX
               LD   A,(IX+0)
               JR   factor_lp
    done_unary:LD   A,E
               AND  A
               PUSH AF
               LD   A,(IX+0)
               CP   "("
               JR   NZ,get_num
               INC  IX
               LD   A,(IX+0)
               CALL expression
               CP   ")"
               JP   NZ,warn
               INC  IX
  check_negate:POP  AF
               LD   A,(IX+0)
               RET  Z
               EX   DE,HL
               LD   HL,0
               SBC  HL,DE
               RET
       get_num:CALL number
               JR   check_negate
;
; Get number in any base or register value
        number:LD   HL,0
               CALL alpha
               JP   C,num_x_register
               LD   A,(IX+0)
               INC  IX
               CP   "B"
               JP   Z,num_reg_bc
               CP   "C"
               LD   DE,reg_c
               JR   Z,num_reg_r
               CP   "D"
               JP   Z,num_reg_de
               CP   "E"
               LD   DE,reg_e
               JR   Z,num_reg_r
               CP   "H"
               JP   Z,num_reg_hl
               CP   "L"
               LD   DE,reg_l
               JR   Z,num_reg_r
               CP   "A"
               LD   DE,reg_a
               JR   Z,num_reg_r
               CP   "F"
               LD   DE,reg_flags
               JR   Z,num_reg_r
               CP   "I"
               JR   Z,num_reg_index
               CP   "R"
               LD   DE,reg_refresh
               JR   Z,num_reg_r
               CP   "P"
               JR   Z,num_reg_pc
               CP   "S"
               JR   Z,num_reg_sp
               CP   "T"
               JR   Z,num_top_of_stk
               DEC  IX
               RET
;
     num_reg_r:LD   H,0
               LD   A,(DE)
               LD   L,A
               LD   A,(IX+0)
               RET
;
    num_reg_pc:DEC  IX
               LD   A,(IX+1)
               CP   "C"
               RET  NZ
               INC  IX
               LD   HL,(reg_pcl)
               JR   num_inc_fetch
;
    num_reg_sp:DEC  IX
               LD   A,(IX+1)
               CP   "P"
               RET  NZ
               INC  IX
               LD   HL,(reg_spl)
               JR   num_inc_fetch
;
 num_reg_index:LD   A,(IX+0)
               CP   "X"
               LD   HL,(reg_ixl)
               JR   Z,num_inc_fetch
               CP   "Y"
               LD   HL,(reg_iyl)
               JR   Z,num_inc_fetch
               LD   DE,reg_int_vector
               JR   num_reg_r
;
    num_reg_bc:LD   A,(IX+0)
               CP   "C"
               LD   DE,reg_b
               JR   NZ,num_reg_r
               LD   HL,(reg_c)
               JR   num_inc_fetch
;
    num_reg_de:LD   A,(IX+0)
               CP   "E"
               LD   DE,reg_d
               JR   NZ,num_reg_r
               LD   HL,(reg_e)
               JR   num_inc_fetch
;
    num_reg_hl:LD   A,(IX+0)
               CP   "L"
               LD   DE,reg_h
               JR   NZ,num_reg_r
               LD   HL,(reg_l)
 num_inc_fetch:INC  IX
               LD   A,(IX+0)
               RET
;
num_top_of_stk:DEC  IX
               LD   A,(IX+1)
               CP   "O"
               RET  NZ
               LD   A,(IX+2)
               CP   "S"
               RET  NZ
               INC  IX
               INC  IX
               LD   HL,(top_of_stack)
               JR   num_inc_fetch

num_x_register:CP   "["
               JR   NZ,num_x_peek
               INC  IX
               LD   A,(IX+0)
               CALL expression
               LD   DE,work_space
               LD   BC,1
               CALL get_bytes_safe
               LD   A,(IX+0)
               CP   "]"
               JP   NZ,warn
               LD   A,(work_space)
               LD   L,A
               LD   H,0
               JR   num_inc_fetch

    num_x_peek:CP   "{"
               JR   NZ,num_x_dpeek
               INC  IX
               LD   A,(IX+0)
               CALL expression
               LD   DE,work_space
               LD   BC,2
               CALL get_bytes_safe
               LD   A,(IX+0)
               CP   "}"
               JP   NZ,warn
               LD   HL,(work_space)
               JR   num_inc_fetch

   num_x_dpeek:CP   "%"
               JR   NZ,num_x_bin
               INC  IX
               LD   A,(IX+0)
; Start of binary number must only be 0 or 1
               CP   "0"
               JP   C,warn
               CP   "2"
               JP   NC,warn
     binary_lp:CP   49
               CCF
; Include new bit
               ADC  HL,HL
               INC  IX
               LD   A,(IX+0)
; Done number if digit out of range
               CP   "0"
               RET  C
               CP   "2"
               RET  NC
               JR   binary_lp
     num_x_bin:CP   34
; Jump if not ASCII
               JR   NZ,num_x_chr
               INC  IX
               LD   A,(IX+0)
               CP   cr
; Jump if nothing follows the quote
               JR   Z,warn
               LD   L,A
               JP   num_inc_fetch
     num_x_chr:CP   "#"
; Jump if not HEX
               JR   NZ,num_x_hex
               INC  IX
               LD   A,(IX+0)
               CALL is_hex_digit
               JR   C,warn
        hex_lp:LD   E,A
               LD   D,0
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,HL
; Include new digit
               ADD  HL,DE
               INC  IX
               LD   A,(IX+0)
               CALL is_hex_digit
               JR   NC,hex_lp
               LD   A,(IX+0)
               RET
  is_hex_digit:SUB  "0"
               RET  C
               CP   10
               CCF
               RET  NC
               SUB  7
               CP   10
               RET  C
               CP   16
               CCF
               RET  NC
               SUB  32
               CP   10
               RET  C
               CP   16
               CCF
               RET
     num_x_hex:CP   "0"
               JR   C,warn
               CP   58
               JR   NC,warn
       deci_lp:SUB  "0"
               ADD  HL,HL
               LD   E,L
               LD   D,H
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,DE
; Sum multiplied by 10 before including new digit
               LD   E,A
               LD   D,0
; Include new digit
               ADD  HL,DE
               INC  IX
               LD   A,(IX+0)
               CP   "0"
               RET  C
               CP   58
               JR   C,deci_lp
               RET
;
; WARN - Syntax error on number input - don't accept.
;
          warn:LD   BC,clut
               LD   A,34
               OUT  (C),A
               HALT
               HALT
               CALL update_screen
wait_no_return:HALT
               LD   A,(IY+lastkey)
               CP   13
               JR   Z,wait_no_return
               LD   HL,(input_ptr)
    warn_stack:LD   SP,0
; Jump back to alter invalid input
               JP   input_lp

;
; Buffer used by graph option, maybe others
;
       buffer2:DS   354
    high_stack:DS   0

   z_remain_8k:EQU  49152-high_stack-1538

;
; Code for moving the 8K code to just after the screen
;
    code_mover:DS   0

               ORG  code_mover-mode3_length
  move_8k_code:LD   HL,section_b
               LD   DE,section_b+mode3_length
               LD   BC,half_page_len
               LDIR
               RET
