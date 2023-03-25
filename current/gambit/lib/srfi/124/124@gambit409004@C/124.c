#ifdef ___LINKER_INFO
; File: "124.c", produced by Gambit v4.9.4
(
409004
(C)
"srfi/124"
("srfi/124")
()
(("srfi/124"))
( #|*/"*/"symbols|#
"##fail-check-ephemeron"
"##type-2-E3B20910-F869-4C15-B427-454DC8334518"
"##type-5"
"datum"
"ephemeron"
"fields"
"flags"
"id"
"name"
"srfi/124"
"srfi/124#"
"srfi/124#ephemeron-broken?"
"srfi/124#ephemeron-datum"
"srfi/124#ephemeron-key"
"srfi/124#ephemeron?"
"srfi/124#make-ephemeron"
"srfi/124#make-ephemeron-finalizer"
"srfi/124#reference-barrier"
"super"
"type"
"will"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"srfi/124#"
"srfi/124#make-ephemeron-finalizer"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"##fail-check-ephemeron"
"srfi/124#ephemeron-broken?"
"srfi/124#ephemeron-datum"
"srfi/124#ephemeron-key"
"srfi/124#ephemeron?"
"srfi/124#make-ephemeron"
"srfi/124#reference-barrier"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##raise-type-exception"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409004
#define ___MODULE_NAME "srfi/124"
#define ___LINKER_ID ___LNK_124_2e_o1
#define ___MH_PROC ___H_srfi_2f_124
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 21
#define ___GLOCOUNT 10
#define ___SUPCOUNT 9
#define ___CNSCOUNT 3
#define ___SUBCOUNT 7
#define ___LBLCOUNT 34
#define ___MODDESCR ___REF_SUB(4)
#include "gambit.h"

___NEED_SYM(___S__23__23_fail_2d_check_2d_ephemeron)
___NEED_SYM(___S__23__23_type_2d_2_2d_E3B20910_2d_F869_2d_4C15_2d_B427_2d_454DC8334518)
___NEED_SYM(___S__23__23_type_2d_5)
___NEED_SYM(___S_datum)
___NEED_SYM(___S_ephemeron)
___NEED_SYM(___S_fields)
___NEED_SYM(___S_flags)
___NEED_SYM(___S_id)
___NEED_SYM(___S_name)
___NEED_SYM(___S_srfi_2f_124)
___NEED_SYM(___S_srfi_2f_124_23_)
___NEED_SYM(___S_srfi_2f_124_23_ephemeron_2d_broken_3f_)
___NEED_SYM(___S_srfi_2f_124_23_ephemeron_2d_datum)
___NEED_SYM(___S_srfi_2f_124_23_ephemeron_2d_key)
___NEED_SYM(___S_srfi_2f_124_23_ephemeron_3f_)
___NEED_SYM(___S_srfi_2f_124_23_make_2d_ephemeron)
___NEED_SYM(___S_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
___NEED_SYM(___S_srfi_2f_124_23_reference_2d_barrier)
___NEED_SYM(___S_super)
___NEED_SYM(___S_type)
___NEED_SYM(___S_will)

___NEED_GLO(___G__23__23_fail_2d_check_2d_ephemeron)
___NEED_GLO(___G__23__23_raise_2d_type_2d_exception)
___NEED_GLO(___G_srfi_2f_124_23_)
___NEED_GLO(___G_srfi_2f_124_23_ephemeron_2d_broken_3f_)
___NEED_GLO(___G_srfi_2f_124_23_ephemeron_2d_datum)
___NEED_GLO(___G_srfi_2f_124_23_ephemeron_2d_key)
___NEED_GLO(___G_srfi_2f_124_23_ephemeron_3f_)
___NEED_GLO(___G_srfi_2f_124_23_make_2d_ephemeron)
___NEED_GLO(___G_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
___NEED_GLO(___G_srfi_2f_124_23_reference_2d_barrier)

___BEGIN_SYM
___DEF_SYM(0,___S__23__23_fail_2d_check_2d_ephemeron,"##fail-check-ephemeron")
___DEF_SYM(1,___S__23__23_type_2d_2_2d_E3B20910_2d_F869_2d_4C15_2d_B427_2d_454DC8334518,"##type-2-E3B20910-F869-4C15-B427-454DC8334518")

___DEF_SYM(2,___S__23__23_type_2d_5,"##type-5")
___DEF_SYM(3,___S_datum,"datum")
___DEF_SYM(4,___S_ephemeron,"ephemeron")
___DEF_SYM(5,___S_fields,"fields")
___DEF_SYM(6,___S_flags,"flags")
___DEF_SYM(7,___S_id,"id")
___DEF_SYM(8,___S_name,"name")
___DEF_SYM(9,___S_srfi_2f_124,"srfi/124")
___DEF_SYM(10,___S_srfi_2f_124_23_,"srfi/124#")
___DEF_SYM(11,___S_srfi_2f_124_23_ephemeron_2d_broken_3f_,"srfi/124#ephemeron-broken?")
___DEF_SYM(12,___S_srfi_2f_124_23_ephemeron_2d_datum,"srfi/124#ephemeron-datum")
___DEF_SYM(13,___S_srfi_2f_124_23_ephemeron_2d_key,"srfi/124#ephemeron-key")
___DEF_SYM(14,___S_srfi_2f_124_23_ephemeron_3f_,"srfi/124#ephemeron?")
___DEF_SYM(15,___S_srfi_2f_124_23_make_2d_ephemeron,"srfi/124#make-ephemeron")
___DEF_SYM(16,___S_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer,"srfi/124#make-ephemeron-finalizer")

___DEF_SYM(17,___S_srfi_2f_124_23_reference_2d_barrier,"srfi/124#reference-barrier")
___DEF_SYM(18,___S_super,"super")
___DEF_SYM(19,___S_type,"type")
___DEF_SYM(20,___S_will,"will")
___END_SYM

#define ___SYM__23__23_fail_2d_check_2d_ephemeron ___SYM(0,___S__23__23_fail_2d_check_2d_ephemeron)
#define ___SYM__23__23_type_2d_2_2d_E3B20910_2d_F869_2d_4C15_2d_B427_2d_454DC8334518 ___SYM(1,___S__23__23_type_2d_2_2d_E3B20910_2d_F869_2d_4C15_2d_B427_2d_454DC8334518)
#define ___SYM__23__23_type_2d_5 ___SYM(2,___S__23__23_type_2d_5)
#define ___SYM_datum ___SYM(3,___S_datum)
#define ___SYM_ephemeron ___SYM(4,___S_ephemeron)
#define ___SYM_fields ___SYM(5,___S_fields)
#define ___SYM_flags ___SYM(6,___S_flags)
#define ___SYM_id ___SYM(7,___S_id)
#define ___SYM_name ___SYM(8,___S_name)
#define ___SYM_srfi_2f_124 ___SYM(9,___S_srfi_2f_124)
#define ___SYM_srfi_2f_124_23_ ___SYM(10,___S_srfi_2f_124_23_)
#define ___SYM_srfi_2f_124_23_ephemeron_2d_broken_3f_ ___SYM(11,___S_srfi_2f_124_23_ephemeron_2d_broken_3f_)
#define ___SYM_srfi_2f_124_23_ephemeron_2d_datum ___SYM(12,___S_srfi_2f_124_23_ephemeron_2d_datum)
#define ___SYM_srfi_2f_124_23_ephemeron_2d_key ___SYM(13,___S_srfi_2f_124_23_ephemeron_2d_key)
#define ___SYM_srfi_2f_124_23_ephemeron_3f_ ___SYM(14,___S_srfi_2f_124_23_ephemeron_3f_)
#define ___SYM_srfi_2f_124_23_make_2d_ephemeron ___SYM(15,___S_srfi_2f_124_23_make_2d_ephemeron)
#define ___SYM_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer ___SYM(16,___S_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
#define ___SYM_srfi_2f_124_23_reference_2d_barrier ___SYM(17,___S_srfi_2f_124_23_reference_2d_barrier)
#define ___SYM_super ___SYM(18,___S_super)
#define ___SYM_type ___SYM(19,___S_type)
#define ___SYM_will ___SYM(20,___S_will)

___BEGIN_GLO
___DEF_GLO(0,"##fail-check-ephemeron")
___DEF_GLO(1,"srfi/124#")
___DEF_GLO(2,"srfi/124#ephemeron-broken?")
___DEF_GLO(3,"srfi/124#ephemeron-datum")
___DEF_GLO(4,"srfi/124#ephemeron-key")
___DEF_GLO(5,"srfi/124#ephemeron?")
___DEF_GLO(6,"srfi/124#make-ephemeron")
___DEF_GLO(7,"srfi/124#make-ephemeron-finalizer")

___DEF_GLO(8,"srfi/124#reference-barrier")
___DEF_GLO(9,"##raise-type-exception")
___END_GLO

#define ___GLO__23__23_fail_2d_check_2d_ephemeron ___GLO(0,___G__23__23_fail_2d_check_2d_ephemeron)
#define ___PRM__23__23_fail_2d_check_2d_ephemeron ___PRM(0,___G__23__23_fail_2d_check_2d_ephemeron)
#define ___GLO_srfi_2f_124_23_ ___GLO(1,___G_srfi_2f_124_23_)
#define ___PRM_srfi_2f_124_23_ ___PRM(1,___G_srfi_2f_124_23_)
#define ___GLO_srfi_2f_124_23_ephemeron_2d_broken_3f_ ___GLO(2,___G_srfi_2f_124_23_ephemeron_2d_broken_3f_)
#define ___PRM_srfi_2f_124_23_ephemeron_2d_broken_3f_ ___PRM(2,___G_srfi_2f_124_23_ephemeron_2d_broken_3f_)
#define ___GLO_srfi_2f_124_23_ephemeron_2d_datum ___GLO(3,___G_srfi_2f_124_23_ephemeron_2d_datum)
#define ___PRM_srfi_2f_124_23_ephemeron_2d_datum ___PRM(3,___G_srfi_2f_124_23_ephemeron_2d_datum)
#define ___GLO_srfi_2f_124_23_ephemeron_2d_key ___GLO(4,___G_srfi_2f_124_23_ephemeron_2d_key)
#define ___PRM_srfi_2f_124_23_ephemeron_2d_key ___PRM(4,___G_srfi_2f_124_23_ephemeron_2d_key)
#define ___GLO_srfi_2f_124_23_ephemeron_3f_ ___GLO(5,___G_srfi_2f_124_23_ephemeron_3f_)
#define ___PRM_srfi_2f_124_23_ephemeron_3f_ ___PRM(5,___G_srfi_2f_124_23_ephemeron_3f_)
#define ___GLO_srfi_2f_124_23_make_2d_ephemeron ___GLO(6,___G_srfi_2f_124_23_make_2d_ephemeron)
#define ___PRM_srfi_2f_124_23_make_2d_ephemeron ___PRM(6,___G_srfi_2f_124_23_make_2d_ephemeron)
#define ___GLO_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer ___GLO(7,___G_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
#define ___PRM_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer ___PRM(7,___G_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
#define ___GLO_srfi_2f_124_23_reference_2d_barrier ___GLO(8,___G_srfi_2f_124_23_reference_2d_barrier)
#define ___PRM_srfi_2f_124_23_reference_2d_barrier ___PRM(8,___G_srfi_2f_124_23_reference_2d_barrier)
#define ___GLO__23__23_raise_2d_type_2d_exception ___GLO(9,___G__23__23_raise_2d_type_2d_exception)
#define ___PRM__23__23_raise_2d_type_2d_exception ___PRM(9,___G__23__23_raise_2d_type_2d_exception)

___BEGIN_CNS
 ___DEF_CNS(___REF_FIX(1),___REF_SYM(4,___S_ephemeron))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(4,___S_ephemeron))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(4,___S_ephemeron))
___END_CNS

___DEF_SUB_STRUCTURE(___X0,6UL)
               ___VEC1(___REF_SUB(1))
               ___VEC1(___REF_SYM(1,___S__23__23_type_2d_2_2d_E3B20910_2d_F869_2d_4C15_2d_B427_2d_454DC8334518))
               ___VEC1(___REF_SYM(4,___S_ephemeron))
               ___VEC1(___REF_FIX(29))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(3))
               ___VEC0
___DEF_SUB_STRUCTURE(___X1,6UL)
               ___VEC1(___REF_SUB(1))
               ___VEC1(___REF_SYM(2,___S__23__23_type_2d_5))
               ___VEC1(___REF_SYM(19,___S_type))
               ___VEC1(___REF_FIX(8))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(2))
               ___VEC0
___DEF_SUB_VEC(___X2,15UL)
               ___VEC1(___REF_SYM(7,___S_id))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(8,___S_name))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(6,___S_flags))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(18,___S_super))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(5,___S_fields))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X3,6UL)
               ___VEC1(___REF_SYM(20,___S_will))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(3,___S_datum))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X4,6UL)
               ___VEC1(___REF_SUB(5))
               ___VEC1(___REF_SUB(6))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X5,1UL)
               ___VEC1(___REF_SYM(9,___S_srfi_2f_124))
               ___VEC0
___DEF_SUB_VEC(___X6,0UL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
,___DEF_SUB(___X4)
,___DEF_SUB(___X5)
,___DEF_SUB(___X6)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_124_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__23__23_fail_2d_check_2d_ephemeron)
___DEF_M_HLBL(___L1__23__23_fail_2d_check_2d_ephemeron)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_124_23_ephemeron_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
___DEF_M_HLBL(___L1_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
___DEF_M_HLBL(___L2_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_124_23_make_2d_ephemeron)
___DEF_M_HLBL(___L1_srfi_2f_124_23_make_2d_ephemeron)
___DEF_M_HLBL(___L2_srfi_2f_124_23_make_2d_ephemeron)
___DEF_M_HLBL(___L3_srfi_2f_124_23_make_2d_ephemeron)
___DEF_M_HLBL(___L4_srfi_2f_124_23_make_2d_ephemeron)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_124_23_ephemeron_2d_broken_3f_)
___DEF_M_HLBL(___L1_srfi_2f_124_23_ephemeron_2d_broken_3f_)
___DEF_M_HLBL(___L2_srfi_2f_124_23_ephemeron_2d_broken_3f_)
___DEF_M_HLBL(___L3_srfi_2f_124_23_ephemeron_2d_broken_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_124_23_ephemeron_2d_key)
___DEF_M_HLBL(___L1_srfi_2f_124_23_ephemeron_2d_key)
___DEF_M_HLBL(___L2_srfi_2f_124_23_ephemeron_2d_key)
___DEF_M_HLBL(___L3_srfi_2f_124_23_ephemeron_2d_key)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_124_23_ephemeron_2d_datum)
___DEF_M_HLBL(___L1_srfi_2f_124_23_ephemeron_2d_datum)
___DEF_M_HLBL(___L2_srfi_2f_124_23_ephemeron_2d_datum)
___DEF_M_HLBL(___L3_srfi_2f_124_23_ephemeron_2d_datum)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_124_23_reference_2d_barrier)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_124_23_
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
___DEF_P_HLBL(___L0_srfi_2f_124_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_124_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_srfi_2f_124_23_)
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__23__23_fail_2d_check_2d_ephemeron
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__23__23_fail_2d_check_2d_ephemeron)
___DEF_P_HLBL(___L1__23__23_fail_2d_check_2d_ephemeron)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__23__23_fail_2d_check_2d_ephemeron)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(0,2,0,0)
___DEF_GLBL(___L__23__23_fail_2d_check_2d_ephemeron)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__23__23_fail_2d_check_2d_ephemeron)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),9,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_124_23_ephemeron_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 6
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_124_23_ephemeron_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_124_23_ephemeron_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_124_23_ephemeron_3f_)
   ___SET_R1(___BOOLEAN(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_2_2d_E3B20910_2d_F869_2d_4C15_2d_B427_2d_454DC8334518)))
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer
#undef ___PH_LBL0
#define ___PH_LBL0 8
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_R1 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
___DEF_P_HLBL(___L1_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
___DEF_P_HLBL(___L2_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
   ___SET_STK(1,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(1),2)
   ___ADD_CLO_ELEM(0,___R1)
   ___END_SETUP_CLO(1)
   ___SET_R1(___STK(1))
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
   ___JUMPRET(___R0)
___DEF_SLBL(2,___L2_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(2,1,0,0)
   ___SET_R3(___CLO(___R4,1))
   ___UNCHECKEDSTRUCTURESET(___R3,___FAL,___FIX(1L),___SUB(0),___FAL)
   ___SET_R4(___CLO(___R4,1))
   ___UNCHECKEDSTRUCTURESET(___R4,___FAL,___FIX(2L),___SUB(0),___FAL) ___SET_R1(___R4)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_124_23_make_2d_ephemeron
#undef ___PH_LBL0
#define ___PH_LBL0 12
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_124_23_make_2d_ephemeron)
___DEF_P_HLBL(___L1_srfi_2f_124_23_make_2d_ephemeron)
___DEF_P_HLBL(___L2_srfi_2f_124_23_make_2d_ephemeron)
___DEF_P_HLBL(___L3_srfi_2f_124_23_make_2d_ephemeron)
___DEF_P_HLBL(___L4_srfi_2f_124_23_make_2d_ephemeron)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_124_23_make_2d_ephemeron)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_srfi_2f_124_23_make_2d_ephemeron)
   ___BEGIN_ALLOC_STRUCTURE(3UL)
   ___ADD_STRUCTURE_ELEM(0,___SUB(0))
   ___ADD_STRUCTURE_ELEM(1,___FAL)
   ___ADD_STRUCTURE_ELEM(2,___R2)
   ___END_ALLOC_STRUCTURE(3)
   ___SET_R2(___GET_STRUCTURE(3))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R1(___R2)
   ___ADJFP(8)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1_srfi_2f_124_23_make_2d_ephemeron)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_124_23_make_2d_ephemeron)
   ___SET_R0(___LBL(3))
   ___JUMPINT(___SET_NARGS(1),___PRC(8),___L_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer)
___DEF_SLBL(3,___L3_srfi_2f_124_23_make_2d_ephemeron)
   ___SET_R1(___MAKEWILL(___STK(-6),___R1))
   ___UNCHECKEDSTRUCTURESET(___STK(-5),___R1,___FIX(1L),___SUB(0),___FAL)
   ___SET_R1(___STK(-5))
   ___ADJFP(-7)
   ___CHECK_HEAP(4,4096)
___DEF_SLBL(4,___L4_srfi_2f_124_23_make_2d_ephemeron)
   ___ADJFP(-1)
   ___JUMPRET(___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_124_23_ephemeron_2d_broken_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 18
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_124_23_ephemeron_2d_broken_3f_)
___DEF_P_HLBL(___L1_srfi_2f_124_23_ephemeron_2d_broken_3f_)
___DEF_P_HLBL(___L2_srfi_2f_124_23_ephemeron_2d_broken_3f_)
___DEF_P_HLBL(___L3_srfi_2f_124_23_ephemeron_2d_broken_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_124_23_ephemeron_2d_broken_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_124_23_ephemeron_2d_broken_3f_)
   ___IF(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_2_2d_E3B20910_2d_F869_2d_4C15_2d_B427_2d_454DC8334518))
   ___GOTO(___L4_srfi_2f_124_23_ephemeron_2d_broken_3f_)
   ___END_IF
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___CNS(0))
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_124_23_ephemeron_2d_broken_3f_)
   ___SET_NARGS(3) ___GOTO(___L2_srfi_2f_124_23_ephemeron_2d_broken_3f_)
___DEF_SLBL(2,___L2_srfi_2f_124_23_ephemeron_2d_broken_3f_)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(2,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_124_23_ephemeron_2d_broken_3f_)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),9,___G__23__23_raise_2d_type_2d_exception)
___DEF_GLBL(___L4_srfi_2f_124_23_ephemeron_2d_broken_3f_)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___SUB(0),___FAL))
   ___SET_R1(___BOOLEAN(___FALSEP(___R1)))
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_124_23_ephemeron_2d_key
#undef ___PH_LBL0
#define ___PH_LBL0 23
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_124_23_ephemeron_2d_key)
___DEF_P_HLBL(___L1_srfi_2f_124_23_ephemeron_2d_key)
___DEF_P_HLBL(___L2_srfi_2f_124_23_ephemeron_2d_key)
___DEF_P_HLBL(___L3_srfi_2f_124_23_ephemeron_2d_key)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_124_23_ephemeron_2d_key)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_124_23_ephemeron_2d_key)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_2_2d_E3B20910_2d_F869_2d_4C15_2d_B427_2d_454DC8334518)))
   ___GOTO(___L5_srfi_2f_124_23_ephemeron_2d_key)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___SUB(0),___FAL))
   ___SET_STK(1,___R1)
   ___ADJFP(1)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L4_srfi_2f_124_23_ephemeron_2d_key)
   ___END_IF
   ___SET_R1(___WILLTESTATOR(___STK(0)))
   ___ADJFP(-1)
   ___JUMPRET(___R0)
___DEF_GLBL(___L4_srfi_2f_124_23_ephemeron_2d_key)
   ___ADJFP(-1)
   ___JUMPRET(___R0)
___DEF_GLBL(___L5_srfi_2f_124_23_ephemeron_2d_key)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___CNS(1))
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_124_23_ephemeron_2d_key)
   ___SET_NARGS(3) ___GOTO(___L2_srfi_2f_124_23_ephemeron_2d_key)
___DEF_SLBL(2,___L2_srfi_2f_124_23_ephemeron_2d_key)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(2,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_124_23_ephemeron_2d_key)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),9,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_124_23_ephemeron_2d_datum
#undef ___PH_LBL0
#define ___PH_LBL0 28
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_124_23_ephemeron_2d_datum)
___DEF_P_HLBL(___L1_srfi_2f_124_23_ephemeron_2d_datum)
___DEF_P_HLBL(___L2_srfi_2f_124_23_ephemeron_2d_datum)
___DEF_P_HLBL(___L3_srfi_2f_124_23_ephemeron_2d_datum)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_124_23_ephemeron_2d_datum)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_124_23_ephemeron_2d_datum)
   ___IF(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_2_2d_E3B20910_2d_F869_2d_4C15_2d_B427_2d_454DC8334518))
   ___GOTO(___L4_srfi_2f_124_23_ephemeron_2d_datum)
   ___END_IF
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___CNS(2))
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_124_23_ephemeron_2d_datum)
   ___SET_NARGS(3) ___GOTO(___L2_srfi_2f_124_23_ephemeron_2d_datum)
___DEF_SLBL(2,___L2_srfi_2f_124_23_ephemeron_2d_datum)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(2,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_124_23_ephemeron_2d_datum)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),9,___G__23__23_raise_2d_type_2d_exception)
___DEF_GLBL(___L4_srfi_2f_124_23_ephemeron_2d_datum)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(2L),___SUB(0),___FAL))
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_124_23_reference_2d_barrier
#undef ___PH_LBL0
#define ___PH_LBL0 33
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_124_23_reference_2d_barrier)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_124_23_reference_2d_barrier)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_124_23_reference_2d_barrier)
   ___SET_R1(___R1)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_srfi_2f_124_23_,___REF_SYM(10,___S_srfi_2f_124_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_srfi_2f_124_23_,0,-1)
,___DEF_LBL_INTRO(___H__23__23_fail_2d_check_2d_ephemeron,___REF_SYM(0,___S__23__23_fail_2d_check_2d_ephemeron),___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__23__23_fail_2d_check_2d_ephemeron,3,-1)
,___DEF_LBL_RET(___H__23__23_fail_2d_check_2d_ephemeron,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_124_23_ephemeron_3f_,___REF_SYM(14,___S_srfi_2f_124_23_ephemeron_3f_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_srfi_2f_124_23_ephemeron_3f_,1,-1)
,___DEF_LBL_INTRO(___H_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer,___REF_SYM(16,___S_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer),___REF_FAL,3,0)
,___DEF_LBL_PROC(___H_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer,1,1)
,___DEF_LBL_INTRO(___H_srfi_2f_124_23_make_2d_ephemeron,___REF_SYM(15,___S_srfi_2f_124_23_make_2d_ephemeron),___REF_FAL,5,0)
,___DEF_LBL_PROC(___H_srfi_2f_124_23_make_2d_ephemeron,2,-1)
,___DEF_LBL_RET(___H_srfi_2f_124_23_make_2d_ephemeron,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_srfi_2f_124_23_make_2d_ephemeron,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_srfi_2f_124_23_make_2d_ephemeron,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_srfi_2f_124_23_make_2d_ephemeron,___IFD(___RETI,1,0,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_124_23_ephemeron_2d_broken_3f_,___REF_SYM(11,___S_srfi_2f_124_23_ephemeron_2d_broken_3f_),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H_srfi_2f_124_23_ephemeron_2d_broken_3f_,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_124_23_ephemeron_2d_broken_3f_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H_srfi_2f_124_23_ephemeron_2d_broken_3f_,3,-1)
,___DEF_LBL_RET(___H_srfi_2f_124_23_ephemeron_2d_broken_3f_,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_124_23_ephemeron_2d_key,___REF_SYM(13,___S_srfi_2f_124_23_ephemeron_2d_key),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H_srfi_2f_124_23_ephemeron_2d_key,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_124_23_ephemeron_2d_key,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H_srfi_2f_124_23_ephemeron_2d_key,3,-1)
,___DEF_LBL_RET(___H_srfi_2f_124_23_ephemeron_2d_key,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_124_23_ephemeron_2d_datum,___REF_SYM(12,___S_srfi_2f_124_23_ephemeron_2d_datum),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H_srfi_2f_124_23_ephemeron_2d_datum,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_124_23_ephemeron_2d_datum,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H_srfi_2f_124_23_ephemeron_2d_datum,3,-1)
,___DEF_LBL_RET(___H_srfi_2f_124_23_ephemeron_2d_datum,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_124_23_reference_2d_barrier,___REF_SYM(17,___S_srfi_2f_124_23_reference_2d_barrier),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_srfi_2f_124_23_reference_2d_barrier,1,-1)
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(1,___G_srfi_2f_124_23_,1)
___DEF_MOD_PRM(0,___G__23__23_fail_2d_check_2d_ephemeron,3)
___DEF_MOD_PRM(5,___G_srfi_2f_124_23_ephemeron_3f_,6)
___DEF_MOD_PRM(7,___G_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer,8)
___DEF_MOD_PRM(6,___G_srfi_2f_124_23_make_2d_ephemeron,12)
___DEF_MOD_PRM(2,___G_srfi_2f_124_23_ephemeron_2d_broken_3f_,18)
___DEF_MOD_PRM(4,___G_srfi_2f_124_23_ephemeron_2d_key,23)
___DEF_MOD_PRM(3,___G_srfi_2f_124_23_ephemeron_2d_datum,28)
___DEF_MOD_PRM(8,___G_srfi_2f_124_23_reference_2d_barrier,33)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(1,___G_srfi_2f_124_23_,1)
___DEF_MOD_GLO(0,___G__23__23_fail_2d_check_2d_ephemeron,3)
___DEF_MOD_GLO(5,___G_srfi_2f_124_23_ephemeron_3f_,6)
___DEF_MOD_GLO(7,___G_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer,8)
___DEF_MOD_GLO(6,___G_srfi_2f_124_23_make_2d_ephemeron,12)
___DEF_MOD_GLO(2,___G_srfi_2f_124_23_ephemeron_2d_broken_3f_,18)
___DEF_MOD_GLO(4,___G_srfi_2f_124_23_ephemeron_2d_key,23)
___DEF_MOD_GLO(3,___G_srfi_2f_124_23_ephemeron_2d_datum,28)
___DEF_MOD_GLO(8,___G_srfi_2f_124_23_reference_2d_barrier,33)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S__23__23_fail_2d_check_2d_ephemeron,"##fail-check-ephemeron")
___DEF_MOD_SYM(1,___S__23__23_type_2d_2_2d_E3B20910_2d_F869_2d_4C15_2d_B427_2d_454DC8334518,"##type-2-E3B20910-F869-4C15-B427-454DC8334518")

___DEF_MOD_SYM(2,___S__23__23_type_2d_5,"##type-5")
___DEF_MOD_SYM(3,___S_datum,"datum")
___DEF_MOD_SYM(4,___S_ephemeron,"ephemeron")
___DEF_MOD_SYM(5,___S_fields,"fields")
___DEF_MOD_SYM(6,___S_flags,"flags")
___DEF_MOD_SYM(7,___S_id,"id")
___DEF_MOD_SYM(8,___S_name,"name")
___DEF_MOD_SYM(9,___S_srfi_2f_124,"srfi/124")
___DEF_MOD_SYM(10,___S_srfi_2f_124_23_,"srfi/124#")
___DEF_MOD_SYM(11,___S_srfi_2f_124_23_ephemeron_2d_broken_3f_,"srfi/124#ephemeron-broken?")
___DEF_MOD_SYM(12,___S_srfi_2f_124_23_ephemeron_2d_datum,"srfi/124#ephemeron-datum")
___DEF_MOD_SYM(13,___S_srfi_2f_124_23_ephemeron_2d_key,"srfi/124#ephemeron-key")
___DEF_MOD_SYM(14,___S_srfi_2f_124_23_ephemeron_3f_,"srfi/124#ephemeron?")
___DEF_MOD_SYM(15,___S_srfi_2f_124_23_make_2d_ephemeron,"srfi/124#make-ephemeron")
___DEF_MOD_SYM(16,___S_srfi_2f_124_23_make_2d_ephemeron_2d_finalizer,"srfi/124#make-ephemeron-finalizer")

___DEF_MOD_SYM(17,___S_srfi_2f_124_23_reference_2d_barrier,"srfi/124#reference-barrier")
___DEF_MOD_SYM(18,___S_super,"super")
___DEF_MOD_SYM(19,___S_type,"type")
___DEF_MOD_SYM(20,___S_will,"will")
___END_MOD_SYM_KEY

#endif
