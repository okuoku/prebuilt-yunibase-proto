#ifdef ___LINKER_INFO
; File: "file.c", produced by Gambit v4.9.4
(
409004
(C)
"scheme/file"
("scheme/file")
()
(("scheme/file"))
( #|*/"*/"symbols|#
"scheme/file"
"scheme/file#"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"scheme/file#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409004
#define ___MODULE_NAME "scheme/file"
#define ___LINKER_ID ___LNK_file_2e_o1
#define ___MH_PROC ___H_scheme_2f_file
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 2
#define ___GLOCOUNT 1
#define ___SUPCOUNT 1
#define ___SUBCOUNT 3
#define ___LBLCOUNT 2
#define ___MODDESCR ___REF_SUB(0)
#include "gambit.h"

___NEED_SYM(___S_scheme_2f_file)
___NEED_SYM(___S_scheme_2f_file_23_)

___NEED_GLO(___G_scheme_2f_file_23_)

___BEGIN_SYM
___DEF_SYM(0,___S_scheme_2f_file,"scheme/file")
___DEF_SYM(1,___S_scheme_2f_file_23_,"scheme/file#")
___END_SYM

#define ___SYM_scheme_2f_file ___SYM(0,___S_scheme_2f_file)
#define ___SYM_scheme_2f_file_23_ ___SYM(1,___S_scheme_2f_file_23_)

___BEGIN_GLO
___DEF_GLO(0,"scheme/file#")
___END_GLO

#define ___GLO_scheme_2f_file_23_ ___GLO(0,___G_scheme_2f_file_23_)
#define ___PRM_scheme_2f_file_23_ ___PRM(0,___G_scheme_2f_file_23_)

___DEF_SUB_VEC(___X0,6UL)
               ___VEC1(___REF_SUB(1))
               ___VEC1(___REF_SUB(2))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X1,1UL)
               ___VEC1(___REF_SYM(0,___S_scheme_2f_file))
               ___VEC0
___DEF_SUB_VEC(___X2,0UL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_R0 ___D_R1
#undef ___MR_ALL
#define ___MR_ALL ___R_R0 ___R_R1
#undef ___MW_ALL
#define ___MW_ALL ___W_R1
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_file_23_)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_file_23_
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
___DEF_P_HLBL(___L0_scheme_2f_file_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_file_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_scheme_2f_file_23_)
   ___SET_R1(___FAL)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_scheme_2f_file_23_,___REF_SYM(1,___S_scheme_2f_file_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_scheme_2f_file_23_,0,-1)
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(0,___G_scheme_2f_file_23_,1)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(0,___G_scheme_2f_file_23_,1)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_scheme_2f_file,"scheme/file")
___DEF_MOD_SYM(1,___S_scheme_2f_file_23_,"scheme/file#")
___END_MOD_SYM_KEY

#endif
