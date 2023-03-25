#ifdef ___LINKER_INFO
; File: "unstable.c", produced by Gambit v4.9.4
(
409004
(C)
"gambit/unstable"
("gambit/unstable")
()
(("gambit/unstable"))
( #|*/"*/"symbols|#
"##make-inexact-real"
"exponent"
"gambit/unstable"
"gambit/unstable#"
"make-inexact-real"
"mantissa"
"negative?"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"##make-inexact-real"
"gambit/unstable#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"make-inexact-real"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##exact->inexact"
"##exact-int.*"
"##exact-int.expt"
"##fail-check-boolean"
"##fail-check-exact-integer"
"##fail-check-nonnegative-exact-integer"
"##negative?"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409004
#define ___MODULE_NAME "gambit/unstable"
#define ___LINKER_ID ___LNK_unstable_2e_o1
#define ___MH_PROC ___H_gambit_2f_unstable
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 7
#define ___GLOCOUNT 10
#define ___SUPCOUNT 3
#define ___CNSCOUNT 3
#define ___SUBCOUNT 4
#define ___LBLCOUNT 18
#define ___MODDESCR ___REF_SUB(1)
#include "gambit.h"

___NEED_SYM(___S__23__23_make_2d_inexact_2d_real)
___NEED_SYM(___S_exponent)
___NEED_SYM(___S_gambit_2f_unstable)
___NEED_SYM(___S_gambit_2f_unstable_23_)
___NEED_SYM(___S_make_2d_inexact_2d_real)
___NEED_SYM(___S_mantissa)
___NEED_SYM(___S_negative_3f_)

___NEED_GLO(___G__23__23_exact_2d__3e_inexact)
___NEED_GLO(___G__23__23_exact_2d_int_2e__2a_)
___NEED_GLO(___G__23__23_exact_2d_int_2e_expt)
___NEED_GLO(___G__23__23_fail_2d_check_2d_boolean)
___NEED_GLO(___G__23__23_fail_2d_check_2d_exact_2d_integer)
___NEED_GLO(___G__23__23_fail_2d_check_2d_nonnegative_2d_exact_2d_integer)
___NEED_GLO(___G__23__23_make_2d_inexact_2d_real)
___NEED_GLO(___G__23__23_negative_3f_)
___NEED_GLO(___G_gambit_2f_unstable_23_)
___NEED_GLO(___G_make_2d_inexact_2d_real)

___BEGIN_SYM
___DEF_SYM(0,___S__23__23_make_2d_inexact_2d_real,"##make-inexact-real")
___DEF_SYM(1,___S_exponent,"exponent")
___DEF_SYM(2,___S_gambit_2f_unstable,"gambit/unstable")
___DEF_SYM(3,___S_gambit_2f_unstable_23_,"gambit/unstable#")
___DEF_SYM(4,___S_make_2d_inexact_2d_real,"make-inexact-real")
___DEF_SYM(5,___S_mantissa,"mantissa")
___DEF_SYM(6,___S_negative_3f_,"negative?")
___END_SYM

#define ___SYM__23__23_make_2d_inexact_2d_real ___SYM(0,___S__23__23_make_2d_inexact_2d_real)
#define ___SYM_exponent ___SYM(1,___S_exponent)
#define ___SYM_gambit_2f_unstable ___SYM(2,___S_gambit_2f_unstable)
#define ___SYM_gambit_2f_unstable_23_ ___SYM(3,___S_gambit_2f_unstable_23_)
#define ___SYM_make_2d_inexact_2d_real ___SYM(4,___S_make_2d_inexact_2d_real)
#define ___SYM_mantissa ___SYM(5,___S_mantissa)
#define ___SYM_negative_3f_ ___SYM(6,___S_negative_3f_)

___BEGIN_GLO
___DEF_GLO(0,"##make-inexact-real")
___DEF_GLO(1,"gambit/unstable#")
___DEF_GLO(2,"make-inexact-real")
___DEF_GLO(3,"##exact->inexact")
___DEF_GLO(4,"##exact-int.*")
___DEF_GLO(5,"##exact-int.expt")
___DEF_GLO(6,"##fail-check-boolean")
___DEF_GLO(7,"##fail-check-exact-integer")
___DEF_GLO(8,"##fail-check-nonnegative-exact-integer")

___DEF_GLO(9,"##negative?")
___END_GLO

#define ___GLO__23__23_make_2d_inexact_2d_real ___GLO(0,___G__23__23_make_2d_inexact_2d_real)
#define ___PRM__23__23_make_2d_inexact_2d_real ___PRM(0,___G__23__23_make_2d_inexact_2d_real)
#define ___GLO_gambit_2f_unstable_23_ ___GLO(1,___G_gambit_2f_unstable_23_)
#define ___PRM_gambit_2f_unstable_23_ ___PRM(1,___G_gambit_2f_unstable_23_)
#define ___GLO_make_2d_inexact_2d_real ___GLO(2,___G_make_2d_inexact_2d_real)
#define ___PRM_make_2d_inexact_2d_real ___PRM(2,___G_make_2d_inexact_2d_real)
#define ___GLO__23__23_exact_2d__3e_inexact ___GLO(3,___G__23__23_exact_2d__3e_inexact)
#define ___PRM__23__23_exact_2d__3e_inexact ___PRM(3,___G__23__23_exact_2d__3e_inexact)
#define ___GLO__23__23_exact_2d_int_2e__2a_ ___GLO(4,___G__23__23_exact_2d_int_2e__2a_)
#define ___PRM__23__23_exact_2d_int_2e__2a_ ___PRM(4,___G__23__23_exact_2d_int_2e__2a_)
#define ___GLO__23__23_exact_2d_int_2e_expt ___GLO(5,___G__23__23_exact_2d_int_2e_expt)
#define ___PRM__23__23_exact_2d_int_2e_expt ___PRM(5,___G__23__23_exact_2d_int_2e_expt)
#define ___GLO__23__23_fail_2d_check_2d_boolean ___GLO(6,___G__23__23_fail_2d_check_2d_boolean)
#define ___PRM__23__23_fail_2d_check_2d_boolean ___PRM(6,___G__23__23_fail_2d_check_2d_boolean)
#define ___GLO__23__23_fail_2d_check_2d_exact_2d_integer ___GLO(7,___G__23__23_fail_2d_check_2d_exact_2d_integer)
#define ___PRM__23__23_fail_2d_check_2d_exact_2d_integer ___PRM(7,___G__23__23_fail_2d_check_2d_exact_2d_integer)
#define ___GLO__23__23_fail_2d_check_2d_nonnegative_2d_exact_2d_integer ___GLO(8,___G__23__23_fail_2d_check_2d_nonnegative_2d_exact_2d_integer)
#define ___PRM__23__23_fail_2d_check_2d_nonnegative_2d_exact_2d_integer ___PRM(8,___G__23__23_fail_2d_check_2d_nonnegative_2d_exact_2d_integer)
#define ___GLO__23__23_negative_3f_ ___GLO(9,___G__23__23_negative_3f_)
#define ___PRM__23__23_negative_3f_ ___PRM(9,___G__23__23_negative_3f_)

___BEGIN_CNS
 ___DEF_CNS(___REF_FIX(2),___REF_SYM(5,___S_mantissa))
,___DEF_CNS(___REF_FIX(3),___REF_SYM(1,___S_exponent))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(6,___S_negative_3f_))
___END_CNS

___DEF_SUB_F64VEC(___X0,23UL)
               ___F64VEC1(0x3ff00000L,0x0L)
               ___F64VEC1(0x40240000L,0x0L)
               ___F64VEC1(0x40590000L,0x0L)
               ___F64VEC1(0x408f4000L,0x0L)
               ___F64VEC1(0x40c38800L,0x0L)
               ___F64VEC1(0x40f86a00L,0x0L)
               ___F64VEC1(0x412e8480L,0x0L)
               ___F64VEC1(0x416312d0L,0x0L)
               ___F64VEC1(0x4197d784L,0x0L)
               ___F64VEC1(0x41cdcd65L,0x0L)
               ___F64VEC1(0x4202a05fL,0x20000000L)
               ___F64VEC1(0x42374876L,-0x18000000L)
               ___F64VEC1(0x426d1a94L,-0x5e000000L)
               ___F64VEC1(0x42a2309cL,-0x1ac00000L)
               ___F64VEC1(0x42d6bcc4L,0x1e900000L)
               ___F64VEC1(0x430c6bf5L,0x26340000L)
               ___F64VEC1(0x4341c379L,0x37e08000L)
               ___F64VEC1(0x43763457L,-0x7a276000L)
               ___F64VEC1(0x43abc16dL,0x674ec800L)
               ___F64VEC1(0x43e158e4L,0x60913d00L)
               ___F64VEC1(0x4415af1dL,0x78b58c40L)
               ___F64VEC1(0x444b1ae4L,-0x291d10b0L)
               ___F64VEC1(0x4480f0cfL,0x64dd592L)
               ___F64VEC0
___DEF_SUB_VEC(___X1,6UL)
               ___VEC1(___REF_SUB(2))
               ___VEC1(___REF_SUB(3))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X2,1UL)
               ___VEC1(___REF_SYM(2,___S_gambit_2f_unstable))
               ___VEC0
___DEF_SUB_VEC(___X3,0UL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4 ___D_F64(___F64V1) ___D_F64( \
___F64V2) ___D_F64(___F64V3)
#undef ___MR_ALL
#define ___MR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gambit_2f_unstable_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__23__23_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L1__23__23_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L2__23__23_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L3__23__23_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L4__23__23_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L5__23__23_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L6__23__23_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L7__23__23_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L8__23__23_make_2d_inexact_2d_real)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L1_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L2_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L3_make_2d_inexact_2d_real)
___DEF_M_HLBL(___L4_make_2d_inexact_2d_real)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_gambit_2f_unstable_23_
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
___DEF_P_HLBL(___L0_gambit_2f_unstable_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gambit_2f_unstable_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_gambit_2f_unstable_23_)
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__23__23_make_2d_inexact_2d_real
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4 ___D_F64(___F64V1) ___D_F64( \
___F64V2) ___D_F64(___F64V3)
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__23__23_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L1__23__23_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L2__23__23_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L3__23__23_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L4__23__23_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L5__23__23_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L6__23__23_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L7__23__23_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L8__23__23_make_2d_inexact_2d_real)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__23__23_make_2d_inexact_2d_real)
   ___IF_NARGS_EQ(2,___PUSH(___R1) ___SET_R1(___R2) ___SET_R2(___ABSENT) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___PUSH(___R1) ___SET_R1(___R2) ___SET_R2(___R3) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,2,2,0)
___DEF_GLBL(___L__23__23_make_2d_inexact_2d_real)
   ___IF(___NOT(___EQP(___R2,___ABSENT)))
   ___GOTO(___L9__23__23_make_2d_inexact_2d_real)
   ___END_IF
   ___SET_R2(___FIX(0L))
   ___IF(___EQP(___R3,___ABSENT))
   ___GOTO(___L10__23__23_make_2d_inexact_2d_real)
   ___END_IF
   ___GOTO(___L10__23__23_make_2d_inexact_2d_real)
___DEF_GLBL(___L9__23__23_make_2d_inexact_2d_real)
   ___IF(___NOT(___EQP(___R3,___ABSENT)))
   ___GOTO(___L10__23__23_make_2d_inexact_2d_real)
   ___END_IF
___DEF_GLBL(___L10__23__23_make_2d_inexact_2d_real)
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L15__23__23_make_2d_inexact_2d_real)
   ___END_IF
   ___IF(___NOT(___F64FROMFIXEXACTP(___R1)))
   ___GOTO(___L15__23__23_make_2d_inexact_2d_real)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___R2)))
   ___GOTO(___L15__23__23_make_2d_inexact_2d_real)
   ___END_IF
   ___SET_R3(___FIXNEG(___R2))
   ___IF(___NOT(___FIXLT(___R3,___FIX(23L))))
   ___GOTO(___L15__23__23_make_2d_inexact_2d_real)
   ___END_IF
   ___IF(___NOT(___FIXLT(___R2,___FIX(23L))))
   ___GOTO(___L15__23__23_make_2d_inexact_2d_real)
   ___END_IF
   ___IF(___NOT(___FIXLT(___R2,___FIX(0L))))
   ___GOTO(___L14__23__23_make_2d_inexact_2d_real)
   ___END_IF
   ___SET_R2(___FIXNEG(___R2))
   ___SET_F64(___F64V1,___F64VECTORREF(___SUB(0),___R2))
   ___SET_F64(___F64V2,___F64FROMFIX(___R1))
   ___SET_F64(___F64V3,___F64DIV(___F64V2,___F64V1))
   ___SET_R1(___F64BOX(___F64V3))
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1__23__23_make_2d_inexact_2d_real)
   ___IF(___NOT(___NOTFALSEP(___STK(0))))
   ___GOTO(___L13__23__23_make_2d_inexact_2d_real)
   ___END_IF
___DEF_GLBL(___L11__23__23_make_2d_inexact_2d_real)
   ___SET_F64(___F64V1,___F64UNBOX(___R1))
   ___SET_F64(___F64V2,___F64COPYSIGN(___F64V1,-1.))
   ___SET_R1(___F64BOX(___F64V2))
   ___ADJFP(-1)
   ___CHECK_HEAP(2,4096)
___DEF_SLBL(2,___L2__23__23_make_2d_inexact_2d_real)
   ___JUMPRET(___R0)
___DEF_SLBL(3,___L3__23__23_make_2d_inexact_2d_real)
___DEF_GLBL(___L12__23__23_make_2d_inexact_2d_real)
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___IF(___NOTFALSEP(___STK(0)))
   ___GOTO(___L11__23__23_make_2d_inexact_2d_real)
   ___END_IF
___DEF_GLBL(___L13__23__23_make_2d_inexact_2d_real)
   ___ADJFP(-1)
   ___JUMPRET(___R0)
___DEF_GLBL(___L14__23__23_make_2d_inexact_2d_real)
   ___SET_F64(___F64V1,___F64VECTORREF(___SUB(0),___R2))
   ___SET_F64(___F64V2,___F64FROMFIX(___R1))
   ___SET_F64(___F64V3,___F64MUL(___F64V2,___F64V1))
   ___SET_R1(___F64BOX(___F64V3))
   ___CHECK_HEAP(4,4096)
___DEF_SLBL(4,___L4__23__23_make_2d_inexact_2d_real)
   ___IF(___NOTFALSEP(___STK(0)))
   ___GOTO(___L11__23__23_make_2d_inexact_2d_real)
   ___END_IF
   ___GOTO(___L13__23__23_make_2d_inexact_2d_real)
___DEF_GLBL(___L15__23__23_make_2d_inexact_2d_real)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R1(___FIX(10L))
   ___ADJFP(7)
   ___POLL(5)
___DEF_SLBL(5,___L5__23__23_make_2d_inexact_2d_real)
   ___SET_R0(___LBL(6))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),5,___G__23__23_exact_2d_int_2e_expt)
___DEF_SLBL(6,___L6__23__23_make_2d_inexact_2d_real)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(7))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),4,___G__23__23_exact_2d_int_2e__2a_)
___DEF_SLBL(7,___L7__23__23_make_2d_inexact_2d_real)
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L16__23__23_make_2d_inexact_2d_real)
   ___END_IF
   ___SET_F64(___F64V1,___F64FROMFIX(___R1))
   ___SET_R1(___F64BOX(___F64V1))
   ___CHECK_HEAP(8,4096)
___DEF_SLBL(8,___L8__23__23_make_2d_inexact_2d_real)
   ___GOTO(___L12__23__23_make_2d_inexact_2d_real)
___DEF_GLBL(___L16__23__23_make_2d_inexact_2d_real)
   ___IF(___FLONUMP(___R1))
   ___GOTO(___L12__23__23_make_2d_inexact_2d_real)
   ___END_IF
   ___SET_R0(___LBL(3))
   ___JUMPPRM(___SET_NARGS(1),___PRM__23__23_exact_2d__3e_inexact)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_make_2d_inexact_2d_real
#undef ___PH_LBL0
#define ___PH_LBL0 13
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4 ___D_F64(___F64V1) ___D_F64(___F64V2) \
 ___D_F64(___F64V3)
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L1_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L2_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L3_make_2d_inexact_2d_real)
___DEF_P_HLBL(___L4_make_2d_inexact_2d_real)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_make_2d_inexact_2d_real)
   ___IF_NARGS_EQ(2,___PUSH(___R1) ___SET_R1(___R2) ___SET_R2(___ABSENT) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___PUSH(___R1) ___SET_R1(___R2) ___SET_R2(___R3) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,2,2,0)
___DEF_GLBL(___L_make_2d_inexact_2d_real)
   ___IF(___NOT(___BOOLEANP(___STK(0))))
   ___GOTO(___L17_make_2d_inexact_2d_real)
   ___END_IF
   ___IF(___FIXNUMP(___R1))
   ___GOTO(___L5_make_2d_inexact_2d_real)
   ___END_IF
   ___IF(___NOT(___BIGNUMP(___R1)))
   ___GOTO(___L6_make_2d_inexact_2d_real)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L7_make_2d_inexact_2d_real)
   ___END_IF
___DEF_GLBL(___L5_make_2d_inexact_2d_real)
   ___IF(___NOT(___FIXNEGATIVEP(___R1)))
   ___GOTO(___L8_make_2d_inexact_2d_real)
   ___END_IF
___DEF_GLBL(___L6_make_2d_inexact_2d_real)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___CNS(0))
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___LBL(0))
   ___ADJFP(2)
   ___POLL(1)
___DEF_SLBL(1,___L1_make_2d_inexact_2d_real)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),8,___G__23__23_fail_2d_check_2d_nonnegative_2d_exact_2d_integer)
___DEF_GLBL(___L7_make_2d_inexact_2d_real)
   ___IF(___NOT(___FLONUMP(___R1)))
   ___GOTO(___L16_make_2d_inexact_2d_real)
   ___END_IF
   ___SET_F64(___F64V1,___F64UNBOX(___R1))
   ___IF(___F64NEGATIVEP(___F64V1))
   ___GOTO(___L6_make_2d_inexact_2d_real)
   ___END_IF
___DEF_GLBL(___L8_make_2d_inexact_2d_real)
   ___IF(___NOT(___EQP(___R2,___ABSENT)))
   ___GOTO(___L13_make_2d_inexact_2d_real)
   ___END_IF
___DEF_GLBL(___L9_make_2d_inexact_2d_real)
   ___SET_R4(___FIX(0L))
   ___IF(___NOT(___FIXNUMP(___R4)))
   ___GOTO(___L14_make_2d_inexact_2d_real)
   ___END_IF
___DEF_GLBL(___L10_make_2d_inexact_2d_real)
   ___IF(___NOT(___EQP(___R3,___ABSENT)))
   ___GOTO(___L12_make_2d_inexact_2d_real)
   ___END_IF
   ___SET_R2(___FAL)
___DEF_GLBL(___L11_make_2d_inexact_2d_real)
   ___SET_R3(___R2)
   ___SET_R2(___R4)
   ___SET_NARGS(4) ___JUMPINT(___NOTHING,___PRC(3),___L0__23__23_make_2d_inexact_2d_real)
___DEF_GLBL(___L12_make_2d_inexact_2d_real)
   ___SET_R2(___R3)
   ___GOTO(___L11_make_2d_inexact_2d_real)
___DEF_SLBL(2,___L2_make_2d_inexact_2d_real)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L15_make_2d_inexact_2d_real)
   ___END_IF
   ___SET_R3(___STK(-3))
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___IF(___EQP(___R2,___ABSENT))
   ___GOTO(___L9_make_2d_inexact_2d_real)
   ___END_IF
___DEF_GLBL(___L13_make_2d_inexact_2d_real)
   ___SET_R4(___R2)
   ___IF(___FIXNUMP(___R4))
   ___GOTO(___L10_make_2d_inexact_2d_real)
   ___END_IF
___DEF_GLBL(___L14_make_2d_inexact_2d_real)
   ___IF(___BIGNUMP(___R4))
   ___GOTO(___L10_make_2d_inexact_2d_real)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___CNS(1))
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___LBL(0))
   ___ADJFP(2)
   ___POLL(3)
___DEF_SLBL(3,___L3_make_2d_inexact_2d_real)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),7,___G__23__23_fail_2d_check_2d_exact_2d_integer)
___DEF_GLBL(___L15_make_2d_inexact_2d_real)
   ___SET_R3(___STK(-3))
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___GOTO(___L6_make_2d_inexact_2d_real)
___DEF_GLBL(___L16_make_2d_inexact_2d_real)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R0(___LBL(2))
   ___ADJFP(7)
   ___JUMPPRM(___SET_NARGS(1),___PRM__23__23_negative_3f_)
___DEF_GLBL(___L17_make_2d_inexact_2d_real)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___CNS(2))
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___LBL(0))
   ___ADJFP(2)
   ___POLL(4)
___DEF_SLBL(4,___L4_make_2d_inexact_2d_real)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),6,___G__23__23_fail_2d_check_2d_boolean)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_gambit_2f_unstable_23_,___REF_SYM(3,___S_gambit_2f_unstable_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_gambit_2f_unstable_23_,0,-1)
,___DEF_LBL_INTRO(___H__23__23_make_2d_inexact_2d_real,___REF_SYM(0,___S__23__23_make_2d_inexact_2d_real),___REF_FAL,9,0)
,___DEF_LBL_PROC(___H__23__23_make_2d_inexact_2d_real,4,-1)
,___DEF_LBL_RET(___H__23__23_make_2d_inexact_2d_real,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H__23__23_make_2d_inexact_2d_real,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H__23__23_make_2d_inexact_2d_real,___IFD(___RETN,5,1,0x3L))
,___DEF_LBL_RET(___H__23__23_make_2d_inexact_2d_real,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H__23__23_make_2d_inexact_2d_real,___IFD(___RETI,8,1,0x3f07L))
,___DEF_LBL_RET(___H__23__23_make_2d_inexact_2d_real,___IFD(___RETN,5,1,0x7L))
,___DEF_LBL_RET(___H__23__23_make_2d_inexact_2d_real,___IFD(___RETN,5,1,0x3L))
,___DEF_LBL_RET(___H__23__23_make_2d_inexact_2d_real,___IFD(___RETI,8,1,0x3f03L))
,___DEF_LBL_INTRO(___H_make_2d_inexact_2d_real,___REF_SYM(4,___S_make_2d_inexact_2d_real),___REF_FAL,5,0)
,___DEF_LBL_PROC(___H_make_2d_inexact_2d_real,4,-1)
,___DEF_LBL_RET(___H_make_2d_inexact_2d_real,___IFD(___RETI,3,4,0x3f7L))
,___DEF_LBL_RET(___H_make_2d_inexact_2d_real,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H_make_2d_inexact_2d_real,___IFD(___RETI,3,4,0x3f7L))
,___DEF_LBL_RET(___H_make_2d_inexact_2d_real,___IFD(___RETI,3,4,0x3f7L))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(1,___G_gambit_2f_unstable_23_,1)
___DEF_MOD_PRM(0,___G__23__23_make_2d_inexact_2d_real,3)
___DEF_MOD_PRM(2,___G_make_2d_inexact_2d_real,13)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(1,___G_gambit_2f_unstable_23_,1)
___DEF_MOD_GLO(0,___G__23__23_make_2d_inexact_2d_real,3)
___DEF_MOD_GLO(2,___G_make_2d_inexact_2d_real,13)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S__23__23_make_2d_inexact_2d_real,"##make-inexact-real")
___DEF_MOD_SYM(1,___S_exponent,"exponent")
___DEF_MOD_SYM(2,___S_gambit_2f_unstable,"gambit/unstable")
___DEF_MOD_SYM(3,___S_gambit_2f_unstable_23_,"gambit/unstable#")
___DEF_MOD_SYM(4,___S_make_2d_inexact_2d_real,"make-inexact-real")
___DEF_MOD_SYM(5,___S_mantissa,"mantissa")
___DEF_MOD_SYM(6,___S_negative_3f_,"negative?")
___END_MOD_SYM_KEY

#endif
