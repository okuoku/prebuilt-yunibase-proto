#ifdef ___LINKER_INFO
; File: "28.c", produced by Gambit v4.9.4
(
409004
(C)
"srfi/28"
("srfi/28")
()
(("srfi/28"))
( #|*/"*/"symbols|#
"srfi/28"
"srfi/28#"
"srfi/28#format"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"srfi/28#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"srfi/28#format"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##display"
"##error"
"##fail-check-string"
"##get-output-string"
"##open-output-string"
"##write"
"##write-substring"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409004
#define ___MODULE_NAME "srfi/28"
#define ___LINKER_ID ___LNK_28_2e_o1
#define ___MH_PROC ___H_srfi_2f_28
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 3
#define ___GLOCOUNT 9
#define ___SUPCOUNT 2
#define ___SUBCOUNT 9
#define ___LBLCOUNT 24
#define ___OFDCOUNT 2
#define ___MODDESCR ___REF_SUB(6)
#include "gambit.h"

___NEED_SYM(___S_srfi_2f_28)
___NEED_SYM(___S_srfi_2f_28_23_)
___NEED_SYM(___S_srfi_2f_28_23_format)

___NEED_GLO(___G__23__23_display)
___NEED_GLO(___G__23__23_error)
___NEED_GLO(___G__23__23_fail_2d_check_2d_string)
___NEED_GLO(___G__23__23_get_2d_output_2d_string)
___NEED_GLO(___G__23__23_open_2d_output_2d_string)
___NEED_GLO(___G__23__23_write)
___NEED_GLO(___G__23__23_write_2d_substring)
___NEED_GLO(___G_srfi_2f_28_23_)
___NEED_GLO(___G_srfi_2f_28_23_format)

___BEGIN_SYM
___DEF_SYM(0,___S_srfi_2f_28,"srfi/28")
___DEF_SYM(1,___S_srfi_2f_28_23_,"srfi/28#")
___DEF_SYM(2,___S_srfi_2f_28_23_format,"srfi/28#format")
___END_SYM

#define ___SYM_srfi_2f_28 ___SYM(0,___S_srfi_2f_28)
#define ___SYM_srfi_2f_28_23_ ___SYM(1,___S_srfi_2f_28_23_)
#define ___SYM_srfi_2f_28_23_format ___SYM(2,___S_srfi_2f_28_23_format)

___BEGIN_GLO
___DEF_GLO(0,"srfi/28#")
___DEF_GLO(1,"srfi/28#format")
___DEF_GLO(2,"##display")
___DEF_GLO(3,"##error")
___DEF_GLO(4,"##fail-check-string")
___DEF_GLO(5,"##get-output-string")
___DEF_GLO(6,"##open-output-string")
___DEF_GLO(7,"##write")
___DEF_GLO(8,"##write-substring")
___END_GLO

#define ___GLO_srfi_2f_28_23_ ___GLO(0,___G_srfi_2f_28_23_)
#define ___PRM_srfi_2f_28_23_ ___PRM(0,___G_srfi_2f_28_23_)
#define ___GLO_srfi_2f_28_23_format ___GLO(1,___G_srfi_2f_28_23_format)
#define ___PRM_srfi_2f_28_23_format ___PRM(1,___G_srfi_2f_28_23_format)
#define ___GLO__23__23_display ___GLO(2,___G__23__23_display)
#define ___PRM__23__23_display ___PRM(2,___G__23__23_display)
#define ___GLO__23__23_error ___GLO(3,___G__23__23_error)
#define ___PRM__23__23_error ___PRM(3,___G__23__23_error)
#define ___GLO__23__23_fail_2d_check_2d_string ___GLO(4,___G__23__23_fail_2d_check_2d_string)
#define ___PRM__23__23_fail_2d_check_2d_string ___PRM(4,___G__23__23_fail_2d_check_2d_string)
#define ___GLO__23__23_get_2d_output_2d_string ___GLO(5,___G__23__23_get_2d_output_2d_string)
#define ___PRM__23__23_get_2d_output_2d_string ___PRM(5,___G__23__23_get_2d_output_2d_string)
#define ___GLO__23__23_open_2d_output_2d_string ___GLO(6,___G__23__23_open_2d_output_2d_string)
#define ___PRM__23__23_open_2d_output_2d_string ___PRM(6,___G__23__23_open_2d_output_2d_string)
#define ___GLO__23__23_write ___GLO(7,___G__23__23_write)
#define ___PRM__23__23_write ___PRM(7,___G__23__23_write)
#define ___GLO__23__23_write_2d_substring ___GLO(8,___G__23__23_write_2d_substring)
#define ___PRM__23__23_write_2d_substring ___PRM(8,___G__23__23_write_2d_substring)

___DEF_SUB_STR(___X0,28UL)
               ___STR8(85,110,114,101,99,111,103,110)
               ___STR8(105,122,101,100,32,101,115,99)
               ___STR8(97,112,101,32,115,101,113,117)
               ___STR4(101,110,99,101)
___DEF_SUB_STR(___X1,28UL)
               ___STR8(78,111,32,118,97,108,117,101)
               ___STR8(32,102,111,114,32,101,115,99)
               ___STR8(97,112,101,32,115,101,113,117)
               ___STR4(101,110,99,101)
___DEF_SUB_STR(___X2,1UL)
               ___STR1(126)
___DEF_SUB_STR(___X3,1UL)
               ___STR1(10)
___DEF_SUB_STR(___X4,28UL)
               ___STR8(78,111,32,118,97,108,117,101)
               ___STR8(32,102,111,114,32,101,115,99)
               ___STR8(97,112,101,32,115,101,113,117)
               ___STR4(101,110,99,101)
___DEF_SUB_STR(___X5,26UL)
               ___STR8(73,110,99,111,109,112,108,101)
               ___STR8(116,101,32,101,115,99,97,112)
               ___STR8(101,32,115,101,113,117,101,110)
               ___STR2(99,101)
___DEF_SUB_VEC(___X6,6UL)
               ___VEC1(___REF_SUB(7))
               ___VEC1(___REF_SUB(8))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X7,1UL)
               ___VEC1(___REF_SYM(0,___S_srfi_2f_28))
               ___VEC0
___DEF_SUB_VEC(___X8,0UL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
,___DEF_SUB(___X4)
,___DEF_SUB(___X5)
,___DEF_SUB(___X6)
,___DEF_SUB(___X7)
,___DEF_SUB(___X8)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_28_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_28_23_format)
___DEF_M_HLBL(___L1_srfi_2f_28_23_format)
___DEF_M_HLBL(___L2_srfi_2f_28_23_format)
___DEF_M_HLBL(___L3_srfi_2f_28_23_format)
___DEF_M_HLBL(___L4_srfi_2f_28_23_format)
___DEF_M_HLBL(___L5_srfi_2f_28_23_format)
___DEF_M_HLBL(___L6_srfi_2f_28_23_format)
___DEF_M_HLBL(___L7_srfi_2f_28_23_format)
___DEF_M_HLBL(___L8_srfi_2f_28_23_format)
___DEF_M_HLBL(___L9_srfi_2f_28_23_format)
___DEF_M_HLBL(___L10_srfi_2f_28_23_format)
___DEF_M_HLBL(___L11_srfi_2f_28_23_format)
___DEF_M_HLBL(___L12_srfi_2f_28_23_format)
___DEF_M_HLBL(___L13_srfi_2f_28_23_format)
___DEF_M_HLBL(___L14_srfi_2f_28_23_format)
___DEF_M_HLBL(___L15_srfi_2f_28_23_format)
___DEF_M_HLBL(___L16_srfi_2f_28_23_format)
___DEF_M_HLBL(___L17_srfi_2f_28_23_format)
___DEF_M_HLBL(___L18_srfi_2f_28_23_format)
___DEF_M_HLBL(___L19_srfi_2f_28_23_format)
___DEF_M_HLBL(___L20_srfi_2f_28_23_format)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_28_23_
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_28_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_28_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_srfi_2f_28_23_)
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_28_23_format
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_28_23_format)
___DEF_P_HLBL(___L1_srfi_2f_28_23_format)
___DEF_P_HLBL(___L2_srfi_2f_28_23_format)
___DEF_P_HLBL(___L3_srfi_2f_28_23_format)
___DEF_P_HLBL(___L4_srfi_2f_28_23_format)
___DEF_P_HLBL(___L5_srfi_2f_28_23_format)
___DEF_P_HLBL(___L6_srfi_2f_28_23_format)
___DEF_P_HLBL(___L7_srfi_2f_28_23_format)
___DEF_P_HLBL(___L8_srfi_2f_28_23_format)
___DEF_P_HLBL(___L9_srfi_2f_28_23_format)
___DEF_P_HLBL(___L10_srfi_2f_28_23_format)
___DEF_P_HLBL(___L11_srfi_2f_28_23_format)
___DEF_P_HLBL(___L12_srfi_2f_28_23_format)
___DEF_P_HLBL(___L13_srfi_2f_28_23_format)
___DEF_P_HLBL(___L14_srfi_2f_28_23_format)
___DEF_P_HLBL(___L15_srfi_2f_28_23_format)
___DEF_P_HLBL(___L16_srfi_2f_28_23_format)
___DEF_P_HLBL(___L17_srfi_2f_28_23_format)
___DEF_P_HLBL(___L18_srfi_2f_28_23_format)
___DEF_P_HLBL(___L19_srfi_2f_28_23_format)
___DEF_P_HLBL(___L20_srfi_2f_28_23_format)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_28_23_format)
   ___IF_NARGS_EQ(1,___SET_R2(___NUL))
   ___GET_REST(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_28_23_format)
   ___IF(___NOT(___STRINGP(___R1)))
   ___GOTO(___L35_srfi_2f_28_23_format)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_28_23_format)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),6,___G__23__23_open_2d_output_2d_string)
___DEF_SLBL(2,___L2_srfi_2f_28_23_format)
   ___SET_STK(-4,___STK(-7))
   ___SET_STK(-7,___STK(-6))
   ___SET_R3(___STK(-5))
   ___SET_R2(___FIX(0L))
   ___SET_R0(___STK(-4))
   ___ADJFP(-7)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_28_23_format)
   ___GOTO(___L21_srfi_2f_28_23_format)
___DEF_SLBL(4,___L4_srfi_2f_28_23_format)
   ___SET_R3(___CDR(___STK(-4)))
   ___SET_R2(___FIXADD(___STK(-3),___FIX(1L)))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-5))
   ___ADJFP(-7)
   ___POLL(5)
___DEF_SLBL(5,___L5_srfi_2f_28_23_format)
___DEF_GLBL(___L21_srfi_2f_28_23_format)
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R2)
   ___SET_R2(___R3)
   ___SET_R3(___STK(2))
   ___SET_R1(___STK(2))
   ___ADJFP(1)
   ___POLL(6)
___DEF_SLBL(6,___L6_srfi_2f_28_23_format)
___DEF_GLBL(___L22_srfi_2f_28_23_format)
   ___SET_R4(___STRINGLENGTH(___STK(-1)))
   ___IF(___NOT(___FIXLT(___R3,___R4)))
   ___GOTO(___L34_srfi_2f_28_23_format)
   ___END_IF
   ___SET_R4(___STRINGREF(___STK(-1),___R3))
   ___IF(___NOT(___CHAREQP(___R4,___CHR(126))))
   ___GOTO(___L33_srfi_2f_28_23_format)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_STK(3,___R3)
   ___SET_STK(7,___STK(-1))
   ___SET_R2(___R1)
   ___SET_R1(___STK(0))
   ___SET_R0(___LBL(9))
   ___ADJFP(7)
   ___POLL(7)
___DEF_SLBL(7,___L7_srfi_2f_28_23_format)
___DEF_GLBL(___L23_srfi_2f_28_23_format)
   ___IF(___NOT(___FIXLT(___R2,___R3)))
   ___GOTO(___L24_srfi_2f_28_23_format)
   ___END_IF
   ___SET_STK(1,___R3)
   ___SET_R3(___R1)
   ___SET_STK(2,___R2)
   ___SET_R2(___STK(1))
   ___SET_R1(___STK(2))
   ___ADJFP(2)
   ___POLL(8)
___DEF_SLBL(8,___L8_srfi_2f_28_23_format)
   ___ADJFP(-2)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),8,___G__23__23_write_2d_substring)
___DEF_GLBL(___L24_srfi_2f_28_23_format)
   ___SET_R1(___VOID)
   ___ADJFP(-1)
   ___JUMPRET(___R0)
___DEF_SLBL(9,___L9_srfi_2f_28_23_format)
   ___SET_R1(___FIXADD(___STK(-3),___FIX(1L)))
   ___SET_R2(___STRINGLENGTH(___STK(-7)))
   ___IF(___NOT(___FIXLT(___R1,___R2)))
   ___GOTO(___L32_srfi_2f_28_23_format)
   ___END_IF
   ___SET_R2(___STRINGREF(___STK(-7),___R1))
   ___BEGIN_SWITCH_CHAR(___R2)
   ___SWITCH_CHAR_CASE_GOTO(___CHR(97),___L30_srfi_2f_28_23_format)
   ___SWITCH_CHAR_CASE_GOTO(___CHR(115),___L25_srfi_2f_28_23_format)
   ___SWITCH_CHAR_CASE_GOTO(___CHR(37),___L29_srfi_2f_28_23_format)
   ___SWITCH_CHAR_CASE_GOTO(___CHR(126),___L28_srfi_2f_28_23_format)
   ___END_SWITCH_CHAR
   ___SET_R1(___SUB(0))
   ___SET_R0(___STK(-5))
   ___POLL(10)
___DEF_SLBL(10,___L10_srfi_2f_28_23_format)
   ___GOTO(___L26_srfi_2f_28_23_format)
___DEF_GLBL(___L25_srfi_2f_28_23_format)
   ___IF(___NOT(___NULLP(___STK(-4))))
   ___GOTO(___L27_srfi_2f_28_23_format)
   ___END_IF
   ___SET_R1(___SUB(1))
   ___SET_R0(___STK(-5))
   ___POLL(11)
___DEF_SLBL(11,___L11_srfi_2f_28_23_format)
___DEF_GLBL(___L26_srfi_2f_28_23_format)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),3,___G__23__23_error)
___DEF_GLBL(___L27_srfi_2f_28_23_format)
   ___SET_STK(-3,___R1)
   ___SET_R2(___STK(-6))
   ___SET_R1(___CAR(___STK(-4)))
   ___SET_R0(___LBL(4))
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23_write)
___DEF_GLBL(___L28_srfi_2f_28_23_format)
   ___SET_STK(-3,___R1)
   ___SET_R2(___STK(-6))
   ___SET_R1(___SUB(2))
   ___SET_R0(___LBL(12))
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23_display)
___DEF_SLBL(12,___L12_srfi_2f_28_23_format)
   ___SET_R3(___STK(-4))
   ___SET_R2(___FIXADD(___STK(-3),___FIX(1L)))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-5))
   ___ADJFP(-7)
   ___POLL(13)
___DEF_SLBL(13,___L13_srfi_2f_28_23_format)
   ___GOTO(___L21_srfi_2f_28_23_format)
___DEF_GLBL(___L29_srfi_2f_28_23_format)
   ___SET_STK(-3,___R1)
   ___SET_R2(___STK(-6))
   ___SET_R1(___SUB(3))
   ___SET_R0(___LBL(12))
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23_display)
___DEF_GLBL(___L30_srfi_2f_28_23_format)
   ___IF(___NOT(___NULLP(___STK(-4))))
   ___GOTO(___L31_srfi_2f_28_23_format)
   ___END_IF
   ___SET_R1(___SUB(4))
   ___SET_R0(___STK(-5))
   ___POLL(14)
___DEF_SLBL(14,___L14_srfi_2f_28_23_format)
   ___GOTO(___L26_srfi_2f_28_23_format)
___DEF_GLBL(___L31_srfi_2f_28_23_format)
   ___SET_STK(-3,___R1)
   ___SET_R2(___STK(-6))
   ___SET_R1(___CAR(___STK(-4)))
   ___SET_R0(___LBL(4))
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23_display)
___DEF_GLBL(___L32_srfi_2f_28_23_format)
   ___SET_R1(___SUB(5))
   ___SET_R0(___STK(-5))
   ___POLL(15)
___DEF_SLBL(15,___L15_srfi_2f_28_23_format)
   ___GOTO(___L26_srfi_2f_28_23_format)
___DEF_GLBL(___L33_srfi_2f_28_23_format)
   ___SET_R3(___FIXADD(___R3,___FIX(1L)))
   ___POLL(16)
___DEF_SLBL(16,___L16_srfi_2f_28_23_format)
   ___GOTO(___L22_srfi_2f_28_23_format)
___DEF_GLBL(___L34_srfi_2f_28_23_format)
   ___SET_STK(1,___R0)
   ___SET_STK(7,___STK(-1))
   ___SET_R2(___R1)
   ___SET_R1(___STK(0))
   ___SET_R0(___LBL(18))
   ___ADJFP(7)
   ___POLL(17)
___DEF_SLBL(17,___L17_srfi_2f_28_23_format)
   ___GOTO(___L23_srfi_2f_28_23_format)
___DEF_SLBL(18,___L18_srfi_2f_28_23_format)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-5))
   ___POLL(19)
___DEF_SLBL(19,___L19_srfi_2f_28_23_format)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),5,___G__23__23_get_2d_output_2d_string)
___DEF_GLBL(___L35_srfi_2f_28_23_format)
   ___SET_STK(1,___FIX(1L))
   ___SET_STK(2,___NUL)
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(2)
   ___POLL(20)
___DEF_SLBL(20,___L20_srfi_2f_28_23_format)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),4,___G__23__23_fail_2d_check_2d_string)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_srfi_2f_28_23_,___REF_SYM(1,___S_srfi_2f_28_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_srfi_2f_28_23_,0,-1)
,___DEF_LBL_INTRO(___H_srfi_2f_28_23_format,___REF_SYM(2,___S_srfi_2f_28_23_format),___REF_FAL,21,0)
,___DEF_LBL_PROC(___H_srfi_2f_28_23_format,2,-1)
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETN,5,2,0x1fL))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___OFD(___RETI,9,2,0x3f11fL))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,3,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETN,5,2,0x1fL))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETN,5,2,0x1fL))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___OFD(___RETI,9,2,0x3f106L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETN,5,2,0x6L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_srfi_2f_28_23_format,___IFD(___RETI,2,4,0x3f3L))
___END_LBL

___BEGIN_OFD
 ___DEF_OFD(___RETI,9,2)
               ___GCMAP1(0x3f11fL)
,___DEF_OFD(___RETI,9,2)
               ___GCMAP1(0x3f106L)
___END_OFD

___BEGIN_MOD_PRM
___DEF_MOD_PRM(0,___G_srfi_2f_28_23_,1)
___DEF_MOD_PRM(1,___G_srfi_2f_28_23_format,3)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(0,___G_srfi_2f_28_23_,1)
___DEF_MOD_GLO(1,___G_srfi_2f_28_23_format,3)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_srfi_2f_28,"srfi/28")
___DEF_MOD_SYM(1,___S_srfi_2f_28_23_,"srfi/28#")
___DEF_MOD_SYM(2,___S_srfi_2f_28_23_format,"srfi/28#format")
___END_MOD_SYM_KEY

#endif
