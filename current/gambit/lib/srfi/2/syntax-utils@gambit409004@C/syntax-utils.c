#ifdef ___LINKER_INFO
; File: "syntax-utils.c", produced by Gambit v4.9.4
(
409004
(C)
"srfi/2/syntax-utils"
("srfi/2/syntax-utils")
("gambit")
(("srfi/2/syntax-utils"))
( #|*/"*/"symbols|#
"gambit"
"srfi/2/syntax-utils"
"srfi/2/syntax-utils#"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"srfi/2/syntax-utils#"
"srfi/2/syntax-utils#syntax-lift"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"srfi/2/syntax-utils#syntax-cadr"
"srfi/2/syntax-utils#syntax-car"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##source-code"
"##source?"
"cadr"
"car"
"datum->syntax"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409004
#define ___MODULE_NAME "srfi/2/syntax-utils"
#define ___LINKER_ID ___LNK_syntax_2d_utils_2e_o1
#define ___MH_PROC ___H_srfi_2f_2_2f_syntax_2d_utils
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 3
#define ___GLOCOUNT 9
#define ___SUPCOUNT 4
#define ___SUBCOUNT 3
#define ___LBLCOUNT 14
#define ___MODDESCR ___REF_SUB(0)
#include "gambit.h"

___NEED_SYM(___S_gambit)
___NEED_SYM(___S_srfi_2f_2_2f_syntax_2d_utils)
___NEED_SYM(___S_srfi_2f_2_2f_syntax_2d_utils_23_)

___NEED_GLO(___G__23__23_source_2d_code)
___NEED_GLO(___G__23__23_source_3f_)
___NEED_GLO(___G_cadr)
___NEED_GLO(___G_car)
___NEED_GLO(___G_datum_2d__3e_syntax)
___NEED_GLO(___G_srfi_2f_2_2f_syntax_2d_utils_23_)
___NEED_GLO(___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_cadr)
___NEED_GLO(___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_car)
___NEED_GLO(___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)

___BEGIN_SYM
___DEF_SYM(0,___S_gambit,"gambit")
___DEF_SYM(1,___S_srfi_2f_2_2f_syntax_2d_utils,"srfi/2/syntax-utils")
___DEF_SYM(2,___S_srfi_2f_2_2f_syntax_2d_utils_23_,"srfi/2/syntax-utils#")
___END_SYM

#define ___SYM_gambit ___SYM(0,___S_gambit)
#define ___SYM_srfi_2f_2_2f_syntax_2d_utils ___SYM(1,___S_srfi_2f_2_2f_syntax_2d_utils)
#define ___SYM_srfi_2f_2_2f_syntax_2d_utils_23_ ___SYM(2,___S_srfi_2f_2_2f_syntax_2d_utils_23_)

___BEGIN_GLO
___DEF_GLO(0,"srfi/2/syntax-utils#")
___DEF_GLO(1,"srfi/2/syntax-utils#syntax-cadr")
___DEF_GLO(2,"srfi/2/syntax-utils#syntax-car")
___DEF_GLO(3,"srfi/2/syntax-utils#syntax-lift")
___DEF_GLO(4,"##source-code")
___DEF_GLO(5,"##source?")
___DEF_GLO(6,"cadr")
___DEF_GLO(7,"car")
___DEF_GLO(8,"datum->syntax")
___END_GLO

#define ___GLO_srfi_2f_2_2f_syntax_2d_utils_23_ ___GLO(0,___G_srfi_2f_2_2f_syntax_2d_utils_23_)
#define ___PRM_srfi_2f_2_2f_syntax_2d_utils_23_ ___PRM(0,___G_srfi_2f_2_2f_syntax_2d_utils_23_)
#define ___GLO_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_cadr ___GLO(1,___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_cadr)
#define ___PRM_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_cadr ___PRM(1,___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_cadr)
#define ___GLO_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_car ___GLO(2,___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_car)
#define ___PRM_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_car ___PRM(2,___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_car)
#define ___GLO_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift ___GLO(3,___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
#define ___PRM_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift ___PRM(3,___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
#define ___GLO__23__23_source_2d_code ___GLO(4,___G__23__23_source_2d_code)
#define ___PRM__23__23_source_2d_code ___PRM(4,___G__23__23_source_2d_code)
#define ___GLO__23__23_source_3f_ ___GLO(5,___G__23__23_source_3f_)
#define ___PRM__23__23_source_3f_ ___PRM(5,___G__23__23_source_3f_)
#define ___GLO_cadr ___GLO(6,___G_cadr)
#define ___PRM_cadr ___PRM(6,___G_cadr)
#define ___GLO_car ___GLO(7,___G_car)
#define ___PRM_car ___PRM(7,___G_car)
#define ___GLO_datum_2d__3e_syntax ___GLO(8,___G_datum_2d__3e_syntax)
#define ___PRM_datum_2d__3e_syntax ___PRM(8,___G_datum_2d__3e_syntax)

___DEF_SUB_VEC(___X0,6UL)
               ___VEC1(___REF_SUB(1))
               ___VEC1(___REF_SUB(2))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X1,1UL)
               ___VEC1(___REF_SYM(1,___S_srfi_2f_2_2f_syntax_2d_utils))
               ___VEC0
___DEF_SUB_VEC(___X2,1UL)
               ___VEC1(___REF_SYM(0,___S_gambit))
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_2_2f_syntax_2d_utils_23_)
___DEF_M_HLBL(___L1_srfi_2f_2_2f_syntax_2d_utils_23_)
___DEF_M_HLBL(___L2_srfi_2f_2_2f_syntax_2d_utils_23_)
___DEF_M_HLBL(___L3_srfi_2f_2_2f_syntax_2d_utils_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_M_HLBL(___L1_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_M_HLBL(___L2_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_M_HLBL(___L3_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_M_HLBL(___L4_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_M_HLBL(___L5_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_M_HLBL(___L6_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_M_HLBL(___L7_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_2_2f_syntax_2d_utils_23_
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_2_2f_syntax_2d_utils_23_)
___DEF_P_HLBL(___L1_srfi_2f_2_2f_syntax_2d_utils_23_)
___DEF_P_HLBL(___L2_srfi_2f_2_2f_syntax_2d_utils_23_)
___DEF_P_HLBL(___L3_srfi_2f_2_2f_syntax_2d_utils_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_2_2f_syntax_2d_utils_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_srfi_2f_2_2f_syntax_2d_utils_23_)
   ___SET_GLO(3,___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift,___PRC(6))
   ___SET_STK(1,___R0)
   ___SET_R1(___GLO_car)
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_2_2f_syntax_2d_utils_23_)
   ___SET_R0(___LBL(2))
   ___JUMPGLOSAFE(___SET_NARGS(1),3,___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_SLBL(2,___L2_srfi_2f_2_2f_syntax_2d_utils_23_)
   ___SET_GLO(2,___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_car,___R1)
   ___SET_R1(___GLO_cadr)
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(1),3,___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_SLBL(3,___L3_srfi_2f_2_2f_syntax_2d_utils_23_)
   ___SET_GLO(1,___G_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_cadr,___R1)
   ___SET_R1(___VOID)
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift
#undef ___PH_LBL0
#define ___PH_LBL0 6
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_P_HLBL(___L1_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_P_HLBL(___L2_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_P_HLBL(___L3_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_P_HLBL(___L4_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_P_HLBL(___L5_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_P_HLBL(___L6_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_P_HLBL(___L7_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___SET_STK(1,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(1),2)
   ___ADD_CLO_ELEM(0,___R1)
   ___END_SETUP_CLO(1)
   ___SET_R1(___STK(1))
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___JUMPRET(___R0)
___DEF_SLBL(2,___L2_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(2,1,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R4)
   ___ADJFP(8)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___GOTO(___L8_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
___DEF_SLBL(4,___L4_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___SET_STK(-5,___R1)
   ___SET_R0(___LBL(6))
   ___JUMPGLOSAFE(___SET_NARGS(1),5,___G__23__23_source_3f_)
___DEF_SLBL(5,___L5_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___SET_R0(___LBL(4))
   ___JUMPGENSAFE(___SET_NARGS(1),___CLO(___STK(-5),1))
___DEF_GLBL(___L8_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(1),4,___G__23__23_source_2d_code)
___DEF_SLBL(6,___L6_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L9_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___END_IF
   ___SET_R1(___STK(-5))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L9_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(7)
___DEF_SLBL(7,___L7_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),8,___G_datum_2d__3e_syntax)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_srfi_2f_2_2f_syntax_2d_utils_23_,___REF_SYM(2,___S_srfi_2f_2_2f_syntax_2d_utils_23_),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H_srfi_2f_2_2f_syntax_2d_utils_23_,0,-1)
,___DEF_LBL_RET(___H_srfi_2f_2_2f_syntax_2d_utils_23_,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_2_2f_syntax_2d_utils_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_srfi_2f_2_2f_syntax_2d_utils_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_INTRO(___H_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift,___REF_FAL,___REF_FAL,8,0)
,___DEF_LBL_PROC(___H_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift,1,1)
,___DEF_LBL_RET(___H_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift,___IFD(___RETI,8,8,0x3f00L))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(0,___G_srfi_2f_2_2f_syntax_2d_utils_23_,1)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(0,___G_srfi_2f_2_2f_syntax_2d_utils_23_,1)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_gambit,"gambit")
___DEF_MOD_SYM(1,___S_srfi_2f_2_2f_syntax_2d_utils,"srfi/2/syntax-utils")
___DEF_MOD_SYM(2,___S_srfi_2f_2_2f_syntax_2d_utils_23_,"srfi/2/syntax-utils#")
___END_MOD_SYM_KEY

#endif
