#ifdef ___LINKER_INFO
; File: "45.c", produced by Gambit v4.9.4
(
409004
(C)
"srfi/45"
("srfi/45")
()
(("srfi/45"))
( #|*/"*/"symbols|#
"srfi/45"
"srfi/45#"
"srfi/45#eager"
"srfi/45#force"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"srfi/45#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"srfi/45#eager"
"srfi/45#force"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##force"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409004
#define ___MODULE_NAME "srfi/45"
#define ___LINKER_ID ___LNK_45_2e_o1
#define ___MH_PROC ___H_srfi_2f_45
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 4
#define ___GLOCOUNT 4
#define ___SUPCOUNT 3
#define ___SUBCOUNT 3
#define ___LBLCOUNT 10
#define ___MODDESCR ___REF_SUB(0)
#include "gambit.h"

___NEED_SYM(___S_srfi_2f_45)
___NEED_SYM(___S_srfi_2f_45_23_)
___NEED_SYM(___S_srfi_2f_45_23_eager)
___NEED_SYM(___S_srfi_2f_45_23_force)

___NEED_GLO(___G__23__23_force)
___NEED_GLO(___G_srfi_2f_45_23_)
___NEED_GLO(___G_srfi_2f_45_23_eager)
___NEED_GLO(___G_srfi_2f_45_23_force)

___BEGIN_SYM
___DEF_SYM(0,___S_srfi_2f_45,"srfi/45")
___DEF_SYM(1,___S_srfi_2f_45_23_,"srfi/45#")
___DEF_SYM(2,___S_srfi_2f_45_23_eager,"srfi/45#eager")
___DEF_SYM(3,___S_srfi_2f_45_23_force,"srfi/45#force")
___END_SYM

#define ___SYM_srfi_2f_45 ___SYM(0,___S_srfi_2f_45)
#define ___SYM_srfi_2f_45_23_ ___SYM(1,___S_srfi_2f_45_23_)
#define ___SYM_srfi_2f_45_23_eager ___SYM(2,___S_srfi_2f_45_23_eager)
#define ___SYM_srfi_2f_45_23_force ___SYM(3,___S_srfi_2f_45_23_force)

___BEGIN_GLO
___DEF_GLO(0,"srfi/45#")
___DEF_GLO(1,"srfi/45#eager")
___DEF_GLO(2,"srfi/45#force")
___DEF_GLO(3,"##force")
___END_GLO

#define ___GLO_srfi_2f_45_23_ ___GLO(0,___G_srfi_2f_45_23_)
#define ___PRM_srfi_2f_45_23_ ___PRM(0,___G_srfi_2f_45_23_)
#define ___GLO_srfi_2f_45_23_eager ___GLO(1,___G_srfi_2f_45_23_eager)
#define ___PRM_srfi_2f_45_23_eager ___PRM(1,___G_srfi_2f_45_23_eager)
#define ___GLO_srfi_2f_45_23_force ___GLO(2,___G_srfi_2f_45_23_force)
#define ___PRM_srfi_2f_45_23_force ___PRM(2,___G_srfi_2f_45_23_force)
#define ___GLO__23__23_force ___GLO(3,___G__23__23_force)
#define ___PRM__23__23_force ___PRM(3,___G__23__23_force)

___DEF_SUB_VEC(___X0,6UL)
               ___VEC1(___REF_SUB(1))
               ___VEC1(___REF_SUB(2))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X1,1UL)
               ___VEC1(___REF_SYM(0,___S_srfi_2f_45))
               ___VEC0
___DEF_SUB_VEC(___X2,0UL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_HEAP ___W_R1 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_45_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_45_23_eager)
___DEF_M_HLBL(___L1_srfi_2f_45_23_eager)
___DEF_M_HLBL(___L2_srfi_2f_45_23_eager)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_45_23_force)
___DEF_M_HLBL(___L1_srfi_2f_45_23_force)
___DEF_M_HLBL(___L2_srfi_2f_45_23_force)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_45_23_
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
___DEF_P_HLBL(___L0_srfi_2f_45_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_45_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_srfi_2f_45_23_)
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_45_23_eager
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_45_23_eager)
___DEF_P_HLBL(___L1_srfi_2f_45_23_eager)
___DEF_P_HLBL(___L2_srfi_2f_45_23_eager)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_45_23_eager)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_45_23_eager)
   ___SET_STK(1,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(1),2)
   ___ADD_CLO_ELEM(0,___R1)
   ___END_SETUP_CLO(1)
   ___SET_R1(___MAKEDELAYPROMISE(___STK(1)))
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1_srfi_2f_45_23_eager)
   ___JUMPRET(___R0)
___DEF_SLBL(2,___L2_srfi_2f_45_23_eager)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(2,0,0,0)
   ___SET_R1(___CLO(___R4,1))
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_45_23_force
#undef ___PH_LBL0
#define ___PH_LBL0 7
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_45_23_force)
___DEF_P_HLBL(___L1_srfi_2f_45_23_force)
___DEF_P_HLBL(___L2_srfi_2f_45_23_force)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_45_23_force)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_45_23_force)
   ___IF(___EQP(___GLO__23__23_force,___PRM__23__23_force))
   ___GOTO(___L3_srfi_2f_45_23_force)
   ___END_IF
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_45_23_force)
   ___JUMPGLOSAFE(___SET_NARGS(1),3,___G__23__23_force)
___DEF_GLBL(___L3_srfi_2f_45_23_force)
   ___FORCE1(2,___R1)
___DEF_SLBL(2,___L2_srfi_2f_45_23_force)
   ___FORCE2
   ___SET_R1(___FORCE3)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_srfi_2f_45_23_,___REF_SYM(1,___S_srfi_2f_45_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_srfi_2f_45_23_,0,-1)
,___DEF_LBL_INTRO(___H_srfi_2f_45_23_eager,___REF_SYM(2,___S_srfi_2f_45_23_eager),___REF_FAL,3,0)
,___DEF_LBL_PROC(___H_srfi_2f_45_23_eager,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_45_23_eager,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H_srfi_2f_45_23_eager,0,1)
,___DEF_LBL_INTRO(___H_srfi_2f_45_23_force,___REF_SYM(3,___S_srfi_2f_45_23_force),___REF_FAL,3,0)
,___DEF_LBL_PROC(___H_srfi_2f_45_23_force,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_45_23_force,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_45_23_force,___IFD(___RETI,0,0,0x3fL))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(0,___G_srfi_2f_45_23_,1)
___DEF_MOD_PRM(1,___G_srfi_2f_45_23_eager,3)
___DEF_MOD_PRM(2,___G_srfi_2f_45_23_force,7)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(0,___G_srfi_2f_45_23_,1)
___DEF_MOD_GLO(1,___G_srfi_2f_45_23_eager,3)
___DEF_MOD_GLO(2,___G_srfi_2f_45_23_force,7)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_srfi_2f_45,"srfi/45")
___DEF_MOD_SYM(1,___S_srfi_2f_45_23_,"srfi/45#")
___DEF_MOD_SYM(2,___S_srfi_2f_45_23_eager,"srfi/45#eager")
___DEF_MOD_SYM(3,___S_srfi_2f_45_23_force,"srfi/45#force")
___END_MOD_SYM_KEY

#endif
