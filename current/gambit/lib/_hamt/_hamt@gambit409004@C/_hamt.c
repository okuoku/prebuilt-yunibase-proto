#ifdef ___LINKER_INFO
; File: "_hamt.c", produced by Gambit v4.9.4
(
409004
(C)
"_hamt"
("_hamt")
()
(("_hamt"))
( #|*/"*/"symbols|#
"##type-4-A8E85030-3E96-4AD5-B257-73B99DCBD137"
"##type-5"
"_hamt"
"_hamt#"
"_hamt#fail-check-hamt"
"_hamt#hamt*->list"
"_hamt#hamt*-alist-ref"
"_hamt#hamt*-alist-remove"
"_hamt#hamt*-fold"
"_hamt#hamt*-for-each"
"_hamt#hamt*-ref"
"_hamt#hamt*-remove"
"_hamt#hamt*-search"
"_hamt#hamt*-set"
"_hamt#hamt*<-reverse-list"
"_hamt#hamt->list"
"_hamt#hamt-empty?"
"_hamt#hamt-fold"
"_hamt#hamt-for-each"
"_hamt#hamt-has-key?"
"_hamt#hamt-has-value?"
"_hamt#hamt-keys"
"_hamt#hamt-length"
"_hamt#hamt-map"
"_hamt#hamt-merge"
"_hamt#hamt-merge-aux"
"_hamt#hamt-ref"
"_hamt#hamt-search"
"_hamt#hamt-set"
"_hamt#hamt-values"
"_hamt#hamt?"
"_hamt#hash-procedure->hash"
"_hamt#list->hamt"
"_hamt#make-hamt"
"_hamt#make-hamt*"
"_hamt#test-procedure->hash"
"_hamt#test-procedure->test"
"fields"
"flags"
"hamt"
"hash"
"id"
"length"
"name"
"super"
"test"
"tree"
"type"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
"hash"
"test"
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"_hamt#"
"_hamt#hamt*->list"
"_hamt#hamt*-alist-ref"
"_hamt#hamt*-alist-remove"
"_hamt#hamt*-fold"
"_hamt#hamt*-for-each"
"_hamt#hamt*-ref"
"_hamt#hamt*-remove"
"_hamt#hamt*-search"
"_hamt#hamt*-set"
"_hamt#hamt*<-reverse-list"
"_hamt#hamt-merge-aux"
"_hamt#test-procedure->hash"
"_hamt#test-procedure->test"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"_hamt#fail-check-hamt"
"_hamt#hamt->list"
"_hamt#hamt-empty?"
"_hamt#hamt-fold"
"_hamt#hamt-for-each"
"_hamt#hamt-has-key?"
"_hamt#hamt-has-value?"
"_hamt#hamt-keys"
"_hamt#hamt-length"
"_hamt#hamt-map"
"_hamt#hamt-merge"
"_hamt#hamt-ref"
"_hamt#hamt-search"
"_hamt#hamt-set"
"_hamt#hamt-values"
"_hamt#hamt?"
"_hamt#hash-procedure->hash"
"_hamt#list->hamt"
"_hamt#make-hamt"
"_hamt#make-hamt*"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##eq?"
"##eq?-hash"
"##equal?"
"##equal?-hash"
"##eqv?"
"##eqv?-hash"
"##fail-check-pair-list"
"##fail-check-procedure"
"##fail-check-proper-list"
"##generic-hash"
"##raise-type-exception"
"##raise-unbound-key-exception"
"##string-ci=?"
"##string-ci=?-hash"
"##string=?"
"##string=?-hash"
"eq?"
"eq?-hash"
"equal?"
"equal?-hash"
"eqv?"
"eqv?-hash"
"string-ci=?"
"string-ci=?-hash"
"string=?"
"string=?-hash"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409004
#define ___MODULE_NAME "_hamt"
#define ___LINKER_ID ___LNK___hamt_2e_o1
#define ___MH_PROC ___H___hamt
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 48
#define ___KEYCOUNT 2
#define ___GLOCOUNT 60
#define ___SUPCOUNT 34
#define ___SUBCOUNT 10
#define ___LBLCOUNT 322
#define ___OFDCOUNT 14
#define ___MODDESCR ___REF_SUB(7)
#include "gambit.h"

___NEED_SYM(___S__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)
___NEED_SYM(___S__23__23_type_2d_5)
___NEED_SYM(___S___hamt)
___NEED_SYM(___S___hamt_23_)
___NEED_SYM(___S___hamt_23_fail_2d_check_2d_hamt)
___NEED_SYM(___S___hamt_23_hamt_2a__2d__3e_list)
___NEED_SYM(___S___hamt_23_hamt_2a__2d_alist_2d_ref)
___NEED_SYM(___S___hamt_23_hamt_2a__2d_alist_2d_remove)
___NEED_SYM(___S___hamt_23_hamt_2a__2d_fold)
___NEED_SYM(___S___hamt_23_hamt_2a__2d_for_2d_each)
___NEED_SYM(___S___hamt_23_hamt_2a__2d_ref)
___NEED_SYM(___S___hamt_23_hamt_2a__2d_remove)
___NEED_SYM(___S___hamt_23_hamt_2a__2d_search)
___NEED_SYM(___S___hamt_23_hamt_2a__2d_set)
___NEED_SYM(___S___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___NEED_SYM(___S___hamt_23_hamt_2d__3e_list)
___NEED_SYM(___S___hamt_23_hamt_2d_empty_3f_)
___NEED_SYM(___S___hamt_23_hamt_2d_fold)
___NEED_SYM(___S___hamt_23_hamt_2d_for_2d_each)
___NEED_SYM(___S___hamt_23_hamt_2d_has_2d_key_3f_)
___NEED_SYM(___S___hamt_23_hamt_2d_has_2d_value_3f_)
___NEED_SYM(___S___hamt_23_hamt_2d_keys)
___NEED_SYM(___S___hamt_23_hamt_2d_length)
___NEED_SYM(___S___hamt_23_hamt_2d_map)
___NEED_SYM(___S___hamt_23_hamt_2d_merge)
___NEED_SYM(___S___hamt_23_hamt_2d_merge_2d_aux)
___NEED_SYM(___S___hamt_23_hamt_2d_ref)
___NEED_SYM(___S___hamt_23_hamt_2d_search)
___NEED_SYM(___S___hamt_23_hamt_2d_set)
___NEED_SYM(___S___hamt_23_hamt_2d_values)
___NEED_SYM(___S___hamt_23_hamt_3f_)
___NEED_SYM(___S___hamt_23_hash_2d_procedure_2d__3e_hash)
___NEED_SYM(___S___hamt_23_list_2d__3e_hamt)
___NEED_SYM(___S___hamt_23_make_2d_hamt)
___NEED_SYM(___S___hamt_23_make_2d_hamt_2a_)
___NEED_SYM(___S___hamt_23_test_2d_procedure_2d__3e_hash)
___NEED_SYM(___S___hamt_23_test_2d_procedure_2d__3e_test)
___NEED_SYM(___S_fields)
___NEED_SYM(___S_flags)
___NEED_SYM(___S_hamt)
___NEED_SYM(___S_hash)
___NEED_SYM(___S_id)
___NEED_SYM(___S_length)
___NEED_SYM(___S_name)
___NEED_SYM(___S_super)
___NEED_SYM(___S_test)
___NEED_SYM(___S_tree)
___NEED_SYM(___S_type)

___NEED_KEY(___K_hash)
___NEED_KEY(___K_test)

___NEED_GLO(___G__23__23_eq_3f_)
___NEED_GLO(___G__23__23_eq_3f__2d_hash)
___NEED_GLO(___G__23__23_equal_3f_)
___NEED_GLO(___G__23__23_equal_3f__2d_hash)
___NEED_GLO(___G__23__23_eqv_3f_)
___NEED_GLO(___G__23__23_eqv_3f__2d_hash)
___NEED_GLO(___G__23__23_fail_2d_check_2d_pair_2d_list)
___NEED_GLO(___G__23__23_fail_2d_check_2d_procedure)
___NEED_GLO(___G__23__23_fail_2d_check_2d_proper_2d_list)
___NEED_GLO(___G__23__23_generic_2d_hash)
___NEED_GLO(___G__23__23_raise_2d_type_2d_exception)
___NEED_GLO(___G__23__23_raise_2d_unbound_2d_key_2d_exception)
___NEED_GLO(___G__23__23_string_2d_ci_3d__3f_)
___NEED_GLO(___G__23__23_string_2d_ci_3d__3f__2d_hash)
___NEED_GLO(___G__23__23_string_3d__3f_)
___NEED_GLO(___G__23__23_string_3d__3f__2d_hash)
___NEED_GLO(___G___hamt_23_)
___NEED_GLO(___G___hamt_23_fail_2d_check_2d_hamt)
___NEED_GLO(___G___hamt_23_hamt_2a__2d__3e_list)
___NEED_GLO(___G___hamt_23_hamt_2a__2d_alist_2d_ref)
___NEED_GLO(___G___hamt_23_hamt_2a__2d_alist_2d_remove)
___NEED_GLO(___G___hamt_23_hamt_2a__2d_fold)
___NEED_GLO(___G___hamt_23_hamt_2a__2d_for_2d_each)
___NEED_GLO(___G___hamt_23_hamt_2a__2d_ref)
___NEED_GLO(___G___hamt_23_hamt_2a__2d_remove)
___NEED_GLO(___G___hamt_23_hamt_2a__2d_search)
___NEED_GLO(___G___hamt_23_hamt_2a__2d_set)
___NEED_GLO(___G___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___NEED_GLO(___G___hamt_23_hamt_2d__3e_list)
___NEED_GLO(___G___hamt_23_hamt_2d_empty_3f_)
___NEED_GLO(___G___hamt_23_hamt_2d_fold)
___NEED_GLO(___G___hamt_23_hamt_2d_for_2d_each)
___NEED_GLO(___G___hamt_23_hamt_2d_has_2d_key_3f_)
___NEED_GLO(___G___hamt_23_hamt_2d_has_2d_value_3f_)
___NEED_GLO(___G___hamt_23_hamt_2d_keys)
___NEED_GLO(___G___hamt_23_hamt_2d_length)
___NEED_GLO(___G___hamt_23_hamt_2d_map)
___NEED_GLO(___G___hamt_23_hamt_2d_merge)
___NEED_GLO(___G___hamt_23_hamt_2d_merge_2d_aux)
___NEED_GLO(___G___hamt_23_hamt_2d_ref)
___NEED_GLO(___G___hamt_23_hamt_2d_search)
___NEED_GLO(___G___hamt_23_hamt_2d_set)
___NEED_GLO(___G___hamt_23_hamt_2d_values)
___NEED_GLO(___G___hamt_23_hamt_3f_)
___NEED_GLO(___G___hamt_23_hash_2d_procedure_2d__3e_hash)
___NEED_GLO(___G___hamt_23_list_2d__3e_hamt)
___NEED_GLO(___G___hamt_23_make_2d_hamt)
___NEED_GLO(___G___hamt_23_make_2d_hamt_2a_)
___NEED_GLO(___G___hamt_23_test_2d_procedure_2d__3e_hash)
___NEED_GLO(___G___hamt_23_test_2d_procedure_2d__3e_test)
___NEED_GLO(___G_eq_3f_)
___NEED_GLO(___G_eq_3f__2d_hash)
___NEED_GLO(___G_equal_3f_)
___NEED_GLO(___G_equal_3f__2d_hash)
___NEED_GLO(___G_eqv_3f_)
___NEED_GLO(___G_eqv_3f__2d_hash)
___NEED_GLO(___G_string_2d_ci_3d__3f_)
___NEED_GLO(___G_string_2d_ci_3d__3f__2d_hash)
___NEED_GLO(___G_string_3d__3f_)
___NEED_GLO(___G_string_3d__3f__2d_hash)

___BEGIN_SYM
___DEF_SYM(0,___S__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137,"##type-4-A8E85030-3E96-4AD5-B257-73B99DCBD137")

___DEF_SYM(1,___S__23__23_type_2d_5,"##type-5")
___DEF_SYM(2,___S___hamt,"_hamt")
___DEF_SYM(3,___S___hamt_23_,"_hamt#")
___DEF_SYM(4,___S___hamt_23_fail_2d_check_2d_hamt,"_hamt#fail-check-hamt")
___DEF_SYM(5,___S___hamt_23_hamt_2a__2d__3e_list,"_hamt#hamt*->list")
___DEF_SYM(6,___S___hamt_23_hamt_2a__2d_alist_2d_ref,"_hamt#hamt*-alist-ref")
___DEF_SYM(7,___S___hamt_23_hamt_2a__2d_alist_2d_remove,"_hamt#hamt*-alist-remove")
___DEF_SYM(8,___S___hamt_23_hamt_2a__2d_fold,"_hamt#hamt*-fold")
___DEF_SYM(9,___S___hamt_23_hamt_2a__2d_for_2d_each,"_hamt#hamt*-for-each")
___DEF_SYM(10,___S___hamt_23_hamt_2a__2d_ref,"_hamt#hamt*-ref")
___DEF_SYM(11,___S___hamt_23_hamt_2a__2d_remove,"_hamt#hamt*-remove")
___DEF_SYM(12,___S___hamt_23_hamt_2a__2d_search,"_hamt#hamt*-search")
___DEF_SYM(13,___S___hamt_23_hamt_2a__2d_set,"_hamt#hamt*-set")
___DEF_SYM(14,___S___hamt_23_hamt_2a__3c__2d_reverse_2d_list,"_hamt#hamt*<-reverse-list")
___DEF_SYM(15,___S___hamt_23_hamt_2d__3e_list,"_hamt#hamt->list")
___DEF_SYM(16,___S___hamt_23_hamt_2d_empty_3f_,"_hamt#hamt-empty?")
___DEF_SYM(17,___S___hamt_23_hamt_2d_fold,"_hamt#hamt-fold")
___DEF_SYM(18,___S___hamt_23_hamt_2d_for_2d_each,"_hamt#hamt-for-each")
___DEF_SYM(19,___S___hamt_23_hamt_2d_has_2d_key_3f_,"_hamt#hamt-has-key?")
___DEF_SYM(20,___S___hamt_23_hamt_2d_has_2d_value_3f_,"_hamt#hamt-has-value?")
___DEF_SYM(21,___S___hamt_23_hamt_2d_keys,"_hamt#hamt-keys")
___DEF_SYM(22,___S___hamt_23_hamt_2d_length,"_hamt#hamt-length")
___DEF_SYM(23,___S___hamt_23_hamt_2d_map,"_hamt#hamt-map")
___DEF_SYM(24,___S___hamt_23_hamt_2d_merge,"_hamt#hamt-merge")
___DEF_SYM(25,___S___hamt_23_hamt_2d_merge_2d_aux,"_hamt#hamt-merge-aux")
___DEF_SYM(26,___S___hamt_23_hamt_2d_ref,"_hamt#hamt-ref")
___DEF_SYM(27,___S___hamt_23_hamt_2d_search,"_hamt#hamt-search")
___DEF_SYM(28,___S___hamt_23_hamt_2d_set,"_hamt#hamt-set")
___DEF_SYM(29,___S___hamt_23_hamt_2d_values,"_hamt#hamt-values")
___DEF_SYM(30,___S___hamt_23_hamt_3f_,"_hamt#hamt?")
___DEF_SYM(31,___S___hamt_23_hash_2d_procedure_2d__3e_hash,"_hamt#hash-procedure->hash")
___DEF_SYM(32,___S___hamt_23_list_2d__3e_hamt,"_hamt#list->hamt")
___DEF_SYM(33,___S___hamt_23_make_2d_hamt,"_hamt#make-hamt")
___DEF_SYM(34,___S___hamt_23_make_2d_hamt_2a_,"_hamt#make-hamt*")
___DEF_SYM(35,___S___hamt_23_test_2d_procedure_2d__3e_hash,"_hamt#test-procedure->hash")
___DEF_SYM(36,___S___hamt_23_test_2d_procedure_2d__3e_test,"_hamt#test-procedure->test")
___DEF_SYM(37,___S_fields,"fields")
___DEF_SYM(38,___S_flags,"flags")
___DEF_SYM(39,___S_hamt,"hamt")
___DEF_SYM(40,___S_hash,"hash")
___DEF_SYM(41,___S_id,"id")
___DEF_SYM(42,___S_length,"length")
___DEF_SYM(43,___S_name,"name")
___DEF_SYM(44,___S_super,"super")
___DEF_SYM(45,___S_test,"test")
___DEF_SYM(46,___S_tree,"tree")
___DEF_SYM(47,___S_type,"type")
___END_SYM

#define ___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137 ___SYM(0,___S__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)
#define ___SYM__23__23_type_2d_5 ___SYM(1,___S__23__23_type_2d_5)
#define ___SYM___hamt ___SYM(2,___S___hamt)
#define ___SYM___hamt_23_ ___SYM(3,___S___hamt_23_)
#define ___SYM___hamt_23_fail_2d_check_2d_hamt ___SYM(4,___S___hamt_23_fail_2d_check_2d_hamt)
#define ___SYM___hamt_23_hamt_2a__2d__3e_list ___SYM(5,___S___hamt_23_hamt_2a__2d__3e_list)
#define ___SYM___hamt_23_hamt_2a__2d_alist_2d_ref ___SYM(6,___S___hamt_23_hamt_2a__2d_alist_2d_ref)
#define ___SYM___hamt_23_hamt_2a__2d_alist_2d_remove ___SYM(7,___S___hamt_23_hamt_2a__2d_alist_2d_remove)
#define ___SYM___hamt_23_hamt_2a__2d_fold ___SYM(8,___S___hamt_23_hamt_2a__2d_fold)
#define ___SYM___hamt_23_hamt_2a__2d_for_2d_each ___SYM(9,___S___hamt_23_hamt_2a__2d_for_2d_each)
#define ___SYM___hamt_23_hamt_2a__2d_ref ___SYM(10,___S___hamt_23_hamt_2a__2d_ref)
#define ___SYM___hamt_23_hamt_2a__2d_remove ___SYM(11,___S___hamt_23_hamt_2a__2d_remove)
#define ___SYM___hamt_23_hamt_2a__2d_search ___SYM(12,___S___hamt_23_hamt_2a__2d_search)
#define ___SYM___hamt_23_hamt_2a__2d_set ___SYM(13,___S___hamt_23_hamt_2a__2d_set)
#define ___SYM___hamt_23_hamt_2a__3c__2d_reverse_2d_list ___SYM(14,___S___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
#define ___SYM___hamt_23_hamt_2d__3e_list ___SYM(15,___S___hamt_23_hamt_2d__3e_list)
#define ___SYM___hamt_23_hamt_2d_empty_3f_ ___SYM(16,___S___hamt_23_hamt_2d_empty_3f_)
#define ___SYM___hamt_23_hamt_2d_fold ___SYM(17,___S___hamt_23_hamt_2d_fold)
#define ___SYM___hamt_23_hamt_2d_for_2d_each ___SYM(18,___S___hamt_23_hamt_2d_for_2d_each)
#define ___SYM___hamt_23_hamt_2d_has_2d_key_3f_ ___SYM(19,___S___hamt_23_hamt_2d_has_2d_key_3f_)
#define ___SYM___hamt_23_hamt_2d_has_2d_value_3f_ ___SYM(20,___S___hamt_23_hamt_2d_has_2d_value_3f_)
#define ___SYM___hamt_23_hamt_2d_keys ___SYM(21,___S___hamt_23_hamt_2d_keys)
#define ___SYM___hamt_23_hamt_2d_length ___SYM(22,___S___hamt_23_hamt_2d_length)
#define ___SYM___hamt_23_hamt_2d_map ___SYM(23,___S___hamt_23_hamt_2d_map)
#define ___SYM___hamt_23_hamt_2d_merge ___SYM(24,___S___hamt_23_hamt_2d_merge)
#define ___SYM___hamt_23_hamt_2d_merge_2d_aux ___SYM(25,___S___hamt_23_hamt_2d_merge_2d_aux)
#define ___SYM___hamt_23_hamt_2d_ref ___SYM(26,___S___hamt_23_hamt_2d_ref)
#define ___SYM___hamt_23_hamt_2d_search ___SYM(27,___S___hamt_23_hamt_2d_search)
#define ___SYM___hamt_23_hamt_2d_set ___SYM(28,___S___hamt_23_hamt_2d_set)
#define ___SYM___hamt_23_hamt_2d_values ___SYM(29,___S___hamt_23_hamt_2d_values)
#define ___SYM___hamt_23_hamt_3f_ ___SYM(30,___S___hamt_23_hamt_3f_)
#define ___SYM___hamt_23_hash_2d_procedure_2d__3e_hash ___SYM(31,___S___hamt_23_hash_2d_procedure_2d__3e_hash)
#define ___SYM___hamt_23_list_2d__3e_hamt ___SYM(32,___S___hamt_23_list_2d__3e_hamt)
#define ___SYM___hamt_23_make_2d_hamt ___SYM(33,___S___hamt_23_make_2d_hamt)
#define ___SYM___hamt_23_make_2d_hamt_2a_ ___SYM(34,___S___hamt_23_make_2d_hamt_2a_)
#define ___SYM___hamt_23_test_2d_procedure_2d__3e_hash ___SYM(35,___S___hamt_23_test_2d_procedure_2d__3e_hash)
#define ___SYM___hamt_23_test_2d_procedure_2d__3e_test ___SYM(36,___S___hamt_23_test_2d_procedure_2d__3e_test)
#define ___SYM_fields ___SYM(37,___S_fields)
#define ___SYM_flags ___SYM(38,___S_flags)
#define ___SYM_hamt ___SYM(39,___S_hamt)
#define ___SYM_hash ___SYM(40,___S_hash)
#define ___SYM_id ___SYM(41,___S_id)
#define ___SYM_length ___SYM(42,___S_length)
#define ___SYM_name ___SYM(43,___S_name)
#define ___SYM_super ___SYM(44,___S_super)
#define ___SYM_test ___SYM(45,___S_test)
#define ___SYM_tree ___SYM(46,___S_tree)
#define ___SYM_type ___SYM(47,___S_type)

___BEGIN_KEY
___DEF_KEY(0,___K_hash,"hash")
___DEF_KEY(1,___K_test,"test")
___END_KEY

#define ___KEY_hash ___KEY(0,___K_hash)
#define ___KEY_test ___KEY(1,___K_test)

___BEGIN_GLO
___DEF_GLO(0,"_hamt#")
___DEF_GLO(1,"_hamt#fail-check-hamt")
___DEF_GLO(2,"_hamt#hamt*->list")
___DEF_GLO(3,"_hamt#hamt*-alist-ref")
___DEF_GLO(4,"_hamt#hamt*-alist-remove")
___DEF_GLO(5,"_hamt#hamt*-fold")
___DEF_GLO(6,"_hamt#hamt*-for-each")
___DEF_GLO(7,"_hamt#hamt*-ref")
___DEF_GLO(8,"_hamt#hamt*-remove")
___DEF_GLO(9,"_hamt#hamt*-search")
___DEF_GLO(10,"_hamt#hamt*-set")
___DEF_GLO(11,"_hamt#hamt*<-reverse-list")
___DEF_GLO(12,"_hamt#hamt->list")
___DEF_GLO(13,"_hamt#hamt-empty?")
___DEF_GLO(14,"_hamt#hamt-fold")
___DEF_GLO(15,"_hamt#hamt-for-each")
___DEF_GLO(16,"_hamt#hamt-has-key?")
___DEF_GLO(17,"_hamt#hamt-has-value?")
___DEF_GLO(18,"_hamt#hamt-keys")
___DEF_GLO(19,"_hamt#hamt-length")
___DEF_GLO(20,"_hamt#hamt-map")
___DEF_GLO(21,"_hamt#hamt-merge")
___DEF_GLO(22,"_hamt#hamt-merge-aux")
___DEF_GLO(23,"_hamt#hamt-ref")
___DEF_GLO(24,"_hamt#hamt-search")
___DEF_GLO(25,"_hamt#hamt-set")
___DEF_GLO(26,"_hamt#hamt-values")
___DEF_GLO(27,"_hamt#hamt?")
___DEF_GLO(28,"_hamt#hash-procedure->hash")
___DEF_GLO(29,"_hamt#list->hamt")
___DEF_GLO(30,"_hamt#make-hamt")
___DEF_GLO(31,"_hamt#make-hamt*")
___DEF_GLO(32,"_hamt#test-procedure->hash")
___DEF_GLO(33,"_hamt#test-procedure->test")
___DEF_GLO(34,"##eq?")
___DEF_GLO(35,"##eq?-hash")
___DEF_GLO(36,"##equal?")
___DEF_GLO(37,"##equal?-hash")
___DEF_GLO(38,"##eqv?")
___DEF_GLO(39,"##eqv?-hash")
___DEF_GLO(40,"##fail-check-pair-list")
___DEF_GLO(41,"##fail-check-procedure")
___DEF_GLO(42,"##fail-check-proper-list")
___DEF_GLO(43,"##generic-hash")
___DEF_GLO(44,"##raise-type-exception")
___DEF_GLO(45,"##raise-unbound-key-exception")
___DEF_GLO(46,"##string-ci=?")
___DEF_GLO(47,"##string-ci=?-hash")
___DEF_GLO(48,"##string=?")
___DEF_GLO(49,"##string=?-hash")
___DEF_GLO(50,"eq?")
___DEF_GLO(51,"eq?-hash")
___DEF_GLO(52,"equal?")
___DEF_GLO(53,"equal?-hash")
___DEF_GLO(54,"eqv?")
___DEF_GLO(55,"eqv?-hash")
___DEF_GLO(56,"string-ci=?")
___DEF_GLO(57,"string-ci=?-hash")
___DEF_GLO(58,"string=?")
___DEF_GLO(59,"string=?-hash")
___END_GLO

#define ___GLO___hamt_23_ ___GLO(0,___G___hamt_23_)
#define ___PRM___hamt_23_ ___PRM(0,___G___hamt_23_)
#define ___GLO___hamt_23_fail_2d_check_2d_hamt ___GLO(1,___G___hamt_23_fail_2d_check_2d_hamt)
#define ___PRM___hamt_23_fail_2d_check_2d_hamt ___PRM(1,___G___hamt_23_fail_2d_check_2d_hamt)
#define ___GLO___hamt_23_hamt_2a__2d__3e_list ___GLO(2,___G___hamt_23_hamt_2a__2d__3e_list)
#define ___PRM___hamt_23_hamt_2a__2d__3e_list ___PRM(2,___G___hamt_23_hamt_2a__2d__3e_list)
#define ___GLO___hamt_23_hamt_2a__2d_alist_2d_ref ___GLO(3,___G___hamt_23_hamt_2a__2d_alist_2d_ref)
#define ___PRM___hamt_23_hamt_2a__2d_alist_2d_ref ___PRM(3,___G___hamt_23_hamt_2a__2d_alist_2d_ref)
#define ___GLO___hamt_23_hamt_2a__2d_alist_2d_remove ___GLO(4,___G___hamt_23_hamt_2a__2d_alist_2d_remove)
#define ___PRM___hamt_23_hamt_2a__2d_alist_2d_remove ___PRM(4,___G___hamt_23_hamt_2a__2d_alist_2d_remove)
#define ___GLO___hamt_23_hamt_2a__2d_fold ___GLO(5,___G___hamt_23_hamt_2a__2d_fold)
#define ___PRM___hamt_23_hamt_2a__2d_fold ___PRM(5,___G___hamt_23_hamt_2a__2d_fold)
#define ___GLO___hamt_23_hamt_2a__2d_for_2d_each ___GLO(6,___G___hamt_23_hamt_2a__2d_for_2d_each)
#define ___PRM___hamt_23_hamt_2a__2d_for_2d_each ___PRM(6,___G___hamt_23_hamt_2a__2d_for_2d_each)
#define ___GLO___hamt_23_hamt_2a__2d_ref ___GLO(7,___G___hamt_23_hamt_2a__2d_ref)
#define ___PRM___hamt_23_hamt_2a__2d_ref ___PRM(7,___G___hamt_23_hamt_2a__2d_ref)
#define ___GLO___hamt_23_hamt_2a__2d_remove ___GLO(8,___G___hamt_23_hamt_2a__2d_remove)
#define ___PRM___hamt_23_hamt_2a__2d_remove ___PRM(8,___G___hamt_23_hamt_2a__2d_remove)
#define ___GLO___hamt_23_hamt_2a__2d_search ___GLO(9,___G___hamt_23_hamt_2a__2d_search)
#define ___PRM___hamt_23_hamt_2a__2d_search ___PRM(9,___G___hamt_23_hamt_2a__2d_search)
#define ___GLO___hamt_23_hamt_2a__2d_set ___GLO(10,___G___hamt_23_hamt_2a__2d_set)
#define ___PRM___hamt_23_hamt_2a__2d_set ___PRM(10,___G___hamt_23_hamt_2a__2d_set)
#define ___GLO___hamt_23_hamt_2a__3c__2d_reverse_2d_list ___GLO(11,___G___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
#define ___PRM___hamt_23_hamt_2a__3c__2d_reverse_2d_list ___PRM(11,___G___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
#define ___GLO___hamt_23_hamt_2d__3e_list ___GLO(12,___G___hamt_23_hamt_2d__3e_list)
#define ___PRM___hamt_23_hamt_2d__3e_list ___PRM(12,___G___hamt_23_hamt_2d__3e_list)
#define ___GLO___hamt_23_hamt_2d_empty_3f_ ___GLO(13,___G___hamt_23_hamt_2d_empty_3f_)
#define ___PRM___hamt_23_hamt_2d_empty_3f_ ___PRM(13,___G___hamt_23_hamt_2d_empty_3f_)
#define ___GLO___hamt_23_hamt_2d_fold ___GLO(14,___G___hamt_23_hamt_2d_fold)
#define ___PRM___hamt_23_hamt_2d_fold ___PRM(14,___G___hamt_23_hamt_2d_fold)
#define ___GLO___hamt_23_hamt_2d_for_2d_each ___GLO(15,___G___hamt_23_hamt_2d_for_2d_each)
#define ___PRM___hamt_23_hamt_2d_for_2d_each ___PRM(15,___G___hamt_23_hamt_2d_for_2d_each)
#define ___GLO___hamt_23_hamt_2d_has_2d_key_3f_ ___GLO(16,___G___hamt_23_hamt_2d_has_2d_key_3f_)
#define ___PRM___hamt_23_hamt_2d_has_2d_key_3f_ ___PRM(16,___G___hamt_23_hamt_2d_has_2d_key_3f_)
#define ___GLO___hamt_23_hamt_2d_has_2d_value_3f_ ___GLO(17,___G___hamt_23_hamt_2d_has_2d_value_3f_)
#define ___PRM___hamt_23_hamt_2d_has_2d_value_3f_ ___PRM(17,___G___hamt_23_hamt_2d_has_2d_value_3f_)
#define ___GLO___hamt_23_hamt_2d_keys ___GLO(18,___G___hamt_23_hamt_2d_keys)
#define ___PRM___hamt_23_hamt_2d_keys ___PRM(18,___G___hamt_23_hamt_2d_keys)
#define ___GLO___hamt_23_hamt_2d_length ___GLO(19,___G___hamt_23_hamt_2d_length)
#define ___PRM___hamt_23_hamt_2d_length ___PRM(19,___G___hamt_23_hamt_2d_length)
#define ___GLO___hamt_23_hamt_2d_map ___GLO(20,___G___hamt_23_hamt_2d_map)
#define ___PRM___hamt_23_hamt_2d_map ___PRM(20,___G___hamt_23_hamt_2d_map)
#define ___GLO___hamt_23_hamt_2d_merge ___GLO(21,___G___hamt_23_hamt_2d_merge)
#define ___PRM___hamt_23_hamt_2d_merge ___PRM(21,___G___hamt_23_hamt_2d_merge)
#define ___GLO___hamt_23_hamt_2d_merge_2d_aux ___GLO(22,___G___hamt_23_hamt_2d_merge_2d_aux)
#define ___PRM___hamt_23_hamt_2d_merge_2d_aux ___PRM(22,___G___hamt_23_hamt_2d_merge_2d_aux)
#define ___GLO___hamt_23_hamt_2d_ref ___GLO(23,___G___hamt_23_hamt_2d_ref)
#define ___PRM___hamt_23_hamt_2d_ref ___PRM(23,___G___hamt_23_hamt_2d_ref)
#define ___GLO___hamt_23_hamt_2d_search ___GLO(24,___G___hamt_23_hamt_2d_search)
#define ___PRM___hamt_23_hamt_2d_search ___PRM(24,___G___hamt_23_hamt_2d_search)
#define ___GLO___hamt_23_hamt_2d_set ___GLO(25,___G___hamt_23_hamt_2d_set)
#define ___PRM___hamt_23_hamt_2d_set ___PRM(25,___G___hamt_23_hamt_2d_set)
#define ___GLO___hamt_23_hamt_2d_values ___GLO(26,___G___hamt_23_hamt_2d_values)
#define ___PRM___hamt_23_hamt_2d_values ___PRM(26,___G___hamt_23_hamt_2d_values)
#define ___GLO___hamt_23_hamt_3f_ ___GLO(27,___G___hamt_23_hamt_3f_)
#define ___PRM___hamt_23_hamt_3f_ ___PRM(27,___G___hamt_23_hamt_3f_)
#define ___GLO___hamt_23_hash_2d_procedure_2d__3e_hash ___GLO(28,___G___hamt_23_hash_2d_procedure_2d__3e_hash)
#define ___PRM___hamt_23_hash_2d_procedure_2d__3e_hash ___PRM(28,___G___hamt_23_hash_2d_procedure_2d__3e_hash)
#define ___GLO___hamt_23_list_2d__3e_hamt ___GLO(29,___G___hamt_23_list_2d__3e_hamt)
#define ___PRM___hamt_23_list_2d__3e_hamt ___PRM(29,___G___hamt_23_list_2d__3e_hamt)
#define ___GLO___hamt_23_make_2d_hamt ___GLO(30,___G___hamt_23_make_2d_hamt)
#define ___PRM___hamt_23_make_2d_hamt ___PRM(30,___G___hamt_23_make_2d_hamt)
#define ___GLO___hamt_23_make_2d_hamt_2a_ ___GLO(31,___G___hamt_23_make_2d_hamt_2a_)
#define ___PRM___hamt_23_make_2d_hamt_2a_ ___PRM(31,___G___hamt_23_make_2d_hamt_2a_)
#define ___GLO___hamt_23_test_2d_procedure_2d__3e_hash ___GLO(32,___G___hamt_23_test_2d_procedure_2d__3e_hash)
#define ___PRM___hamt_23_test_2d_procedure_2d__3e_hash ___PRM(32,___G___hamt_23_test_2d_procedure_2d__3e_hash)
#define ___GLO___hamt_23_test_2d_procedure_2d__3e_test ___GLO(33,___G___hamt_23_test_2d_procedure_2d__3e_test)
#define ___PRM___hamt_23_test_2d_procedure_2d__3e_test ___PRM(33,___G___hamt_23_test_2d_procedure_2d__3e_test)
#define ___GLO__23__23_eq_3f_ ___GLO(34,___G__23__23_eq_3f_)
#define ___PRM__23__23_eq_3f_ ___PRM(34,___G__23__23_eq_3f_)
#define ___GLO__23__23_eq_3f__2d_hash ___GLO(35,___G__23__23_eq_3f__2d_hash)
#define ___PRM__23__23_eq_3f__2d_hash ___PRM(35,___G__23__23_eq_3f__2d_hash)
#define ___GLO__23__23_equal_3f_ ___GLO(36,___G__23__23_equal_3f_)
#define ___PRM__23__23_equal_3f_ ___PRM(36,___G__23__23_equal_3f_)
#define ___GLO__23__23_equal_3f__2d_hash ___GLO(37,___G__23__23_equal_3f__2d_hash)
#define ___PRM__23__23_equal_3f__2d_hash ___PRM(37,___G__23__23_equal_3f__2d_hash)
#define ___GLO__23__23_eqv_3f_ ___GLO(38,___G__23__23_eqv_3f_)
#define ___PRM__23__23_eqv_3f_ ___PRM(38,___G__23__23_eqv_3f_)
#define ___GLO__23__23_eqv_3f__2d_hash ___GLO(39,___G__23__23_eqv_3f__2d_hash)
#define ___PRM__23__23_eqv_3f__2d_hash ___PRM(39,___G__23__23_eqv_3f__2d_hash)
#define ___GLO__23__23_fail_2d_check_2d_pair_2d_list ___GLO(40,___G__23__23_fail_2d_check_2d_pair_2d_list)
#define ___PRM__23__23_fail_2d_check_2d_pair_2d_list ___PRM(40,___G__23__23_fail_2d_check_2d_pair_2d_list)
#define ___GLO__23__23_fail_2d_check_2d_procedure ___GLO(41,___G__23__23_fail_2d_check_2d_procedure)
#define ___PRM__23__23_fail_2d_check_2d_procedure ___PRM(41,___G__23__23_fail_2d_check_2d_procedure)
#define ___GLO__23__23_fail_2d_check_2d_proper_2d_list ___GLO(42,___G__23__23_fail_2d_check_2d_proper_2d_list)
#define ___PRM__23__23_fail_2d_check_2d_proper_2d_list ___PRM(42,___G__23__23_fail_2d_check_2d_proper_2d_list)
#define ___GLO__23__23_generic_2d_hash ___GLO(43,___G__23__23_generic_2d_hash)
#define ___PRM__23__23_generic_2d_hash ___PRM(43,___G__23__23_generic_2d_hash)
#define ___GLO__23__23_raise_2d_type_2d_exception ___GLO(44,___G__23__23_raise_2d_type_2d_exception)
#define ___PRM__23__23_raise_2d_type_2d_exception ___PRM(44,___G__23__23_raise_2d_type_2d_exception)
#define ___GLO__23__23_raise_2d_unbound_2d_key_2d_exception ___GLO(45,___G__23__23_raise_2d_unbound_2d_key_2d_exception)
#define ___PRM__23__23_raise_2d_unbound_2d_key_2d_exception ___PRM(45,___G__23__23_raise_2d_unbound_2d_key_2d_exception)
#define ___GLO__23__23_string_2d_ci_3d__3f_ ___GLO(46,___G__23__23_string_2d_ci_3d__3f_)
#define ___PRM__23__23_string_2d_ci_3d__3f_ ___PRM(46,___G__23__23_string_2d_ci_3d__3f_)
#define ___GLO__23__23_string_2d_ci_3d__3f__2d_hash ___GLO(47,___G__23__23_string_2d_ci_3d__3f__2d_hash)
#define ___PRM__23__23_string_2d_ci_3d__3f__2d_hash ___PRM(47,___G__23__23_string_2d_ci_3d__3f__2d_hash)
#define ___GLO__23__23_string_3d__3f_ ___GLO(48,___G__23__23_string_3d__3f_)
#define ___PRM__23__23_string_3d__3f_ ___PRM(48,___G__23__23_string_3d__3f_)
#define ___GLO__23__23_string_3d__3f__2d_hash ___GLO(49,___G__23__23_string_3d__3f__2d_hash)
#define ___PRM__23__23_string_3d__3f__2d_hash ___PRM(49,___G__23__23_string_3d__3f__2d_hash)
#define ___GLO_eq_3f_ ___GLO(50,___G_eq_3f_)
#define ___PRM_eq_3f_ ___PRM(50,___G_eq_3f_)
#define ___GLO_eq_3f__2d_hash ___GLO(51,___G_eq_3f__2d_hash)
#define ___PRM_eq_3f__2d_hash ___PRM(51,___G_eq_3f__2d_hash)
#define ___GLO_equal_3f_ ___GLO(52,___G_equal_3f_)
#define ___PRM_equal_3f_ ___PRM(52,___G_equal_3f_)
#define ___GLO_equal_3f__2d_hash ___GLO(53,___G_equal_3f__2d_hash)
#define ___PRM_equal_3f__2d_hash ___PRM(53,___G_equal_3f__2d_hash)
#define ___GLO_eqv_3f_ ___GLO(54,___G_eqv_3f_)
#define ___PRM_eqv_3f_ ___PRM(54,___G_eqv_3f_)
#define ___GLO_eqv_3f__2d_hash ___GLO(55,___G_eqv_3f__2d_hash)
#define ___PRM_eqv_3f__2d_hash ___PRM(55,___G_eqv_3f__2d_hash)
#define ___GLO_string_2d_ci_3d__3f_ ___GLO(56,___G_string_2d_ci_3d__3f_)
#define ___PRM_string_2d_ci_3d__3f_ ___PRM(56,___G_string_2d_ci_3d__3f_)
#define ___GLO_string_2d_ci_3d__3f__2d_hash ___GLO(57,___G_string_2d_ci_3d__3f__2d_hash)
#define ___PRM_string_2d_ci_3d__3f__2d_hash ___PRM(57,___G_string_2d_ci_3d__3f__2d_hash)
#define ___GLO_string_3d__3f_ ___GLO(58,___G_string_3d__3f_)
#define ___PRM_string_3d__3f_ ___PRM(58,___G_string_3d__3f_)
#define ___GLO_string_3d__3f__2d_hash ___GLO(59,___G_string_3d__3f__2d_hash)
#define ___PRM_string_3d__3f__2d_hash ___PRM(59,___G_string_3d__3f__2d_hash)

___DEF_SUB_STRUCTURE(___X0,6UL)
               ___VEC1(___REF_SUB(1))
               ___VEC1(___REF_SYM(0,___S__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137))
               ___VEC1(___REF_SYM(39,___S_hamt))
               ___VEC1(___REF_FIX(28))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(3))
               ___VEC0
___DEF_SUB_STRUCTURE(___X1,6UL)
               ___VEC1(___REF_SUB(1))
               ___VEC1(___REF_SYM(1,___S__23__23_type_2d_5))
               ___VEC1(___REF_SYM(47,___S_type))
               ___VEC1(___REF_FIX(8))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(2))
               ___VEC0
___DEF_SUB_VEC(___X2,15UL)
               ___VEC1(___REF_SYM(41,___S_id))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(43,___S_name))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(38,___S_flags))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(44,___S_super))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(37,___S_fields))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X3,12UL)
               ___VEC1(___REF_SYM(45,___S_test))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(40,___S_hash))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(46,___S_tree))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(42,___S_length))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X4,1UL)
               ___VEC1(___REF_FIX(0))
               ___VEC0
___DEF_SUB_VEC(___X5,4UL)
               ___VEC1(___REF_KEY(1,___K_test))
               ___VEC1(___REF_ABSENT)
               ___VEC1(___REF_KEY(0,___K_hash))
               ___VEC1(___REF_ABSENT)
               ___VEC0
___DEF_SUB_VEC(___X6,4UL)
               ___VEC1(___REF_KEY(1,___K_test))
               ___VEC1(___REF_ABSENT)
               ___VEC1(___REF_KEY(0,___K_hash))
               ___VEC1(___REF_ABSENT)
               ___VEC0
___DEF_SUB_VEC(___X7,6UL)
               ___VEC1(___REF_SUB(8))
               ___VEC1(___REF_SUB(9))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X8,1UL)
               ___VEC1(___REF_SYM(2,___S___hamt))
               ___VEC0
___DEF_SUB_VEC(___X9,0UL)
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
,___DEF_SUB(___X9)
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
___DEF_M_HLBL(___L0___hamt_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_fail_2d_check_2d_hamt)
___DEF_M_HLBL(___L1___hamt_23_fail_2d_check_2d_hamt)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_make_2d_hamt_2a_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2a__2d_ref)
___DEF_M_HLBL(___L1___hamt_23_hamt_2a__2d_ref)
___DEF_M_HLBL(___L2___hamt_23_hamt_2a__2d_ref)
___DEF_M_HLBL(___L3___hamt_23_hamt_2a__2d_ref)
___DEF_M_HLBL(___L4___hamt_23_hamt_2a__2d_ref)
___DEF_M_HLBL(___L5___hamt_23_hamt_2a__2d_ref)
___DEF_M_HLBL(___L6___hamt_23_hamt_2a__2d_ref)
___DEF_M_HLBL(___L7___hamt_23_hamt_2a__2d_ref)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L1___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L2___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L3___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L4___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L5___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L6___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L7___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L8___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L9___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L10___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L11___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L12___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L13___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L14___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L15___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L16___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L17___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L18___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L19___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L20___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L21___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L22___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L23___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L24___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L25___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L26___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L27___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L28___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL(___L29___hamt_23_hamt_2a__2d_set)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L1___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L2___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L3___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L4___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L5___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L6___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L7___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L8___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L9___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L10___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L11___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L12___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL(___L13___hamt_23_hamt_2a__2d_remove)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L1___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L2___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L3___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L4___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L5___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L6___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L7___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L8___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L9___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L10___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L11___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L12___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL(___L13___hamt_23_hamt_2a__2d_search)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L1___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L2___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L3___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L4___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L5___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L6___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L7___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L8___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L9___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L10___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L11___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L12___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L13___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L14___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L15___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL(___L16___hamt_23_hamt_2a__2d_fold)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L1___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L2___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L3___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L4___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L5___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L6___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L7___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L8___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L9___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L10___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L11___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L12___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L13___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL(___L14___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L1___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L2___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L3___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L4___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L5___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L6___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L7___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L8___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L9___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L10___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L11___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L12___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L13___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L14___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL(___L15___hamt_23_hamt_2a__2d__3e_list)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_M_HLBL(___L1___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_M_HLBL(___L2___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_M_HLBL(___L3___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_M_HLBL(___L4___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_M_HLBL(___L5___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_M_HLBL(___L6___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_M_HLBL(___L1___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_M_HLBL(___L2___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_M_HLBL(___L3___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_M_HLBL(___L4___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_M_HLBL(___L1___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_M_HLBL(___L2___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_M_HLBL(___L3___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_M_HLBL(___L4___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_M_HLBL(___L5___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_empty_3f_)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_empty_3f_)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_empty_3f_)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_empty_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L1___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L2___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L3___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L4___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L5___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L6___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L7___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L8___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L9___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L10___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L11___hamt_23_make_2d_hamt)
___DEF_M_HLBL(___L12___hamt_23_make_2d_hamt)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_length)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_length)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_length)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_length)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_ref)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_ref)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_ref)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_ref)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_ref)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_ref)
___DEF_M_HLBL(___L6___hamt_23_hamt_2d_ref)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_set)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_set)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_set)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_set)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_set)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_set)
___DEF_M_HLBL(___L6___hamt_23_hamt_2d_set)
___DEF_M_HLBL(___L7___hamt_23_hamt_2d_set)
___DEF_M_HLBL(___L8___hamt_23_hamt_2d_set)
___DEF_M_HLBL(___L9___hamt_23_hamt_2d_set)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_merge)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_merge)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_merge)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_merge)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_merge)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_merge)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L6___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L7___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L8___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L9___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L10___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L11___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L12___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL(___L13___hamt_23_hamt_2d_merge_2d_aux)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_search)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_search)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_search)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_search)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_search)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_search)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_fold)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_fold)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_fold)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_fold)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_fold)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_fold)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_for_2d_each)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_for_2d_each)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_for_2d_each)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_for_2d_each)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_for_2d_each)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_for_2d_each)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_map)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_map)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_map)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_map)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_map)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_map)
___DEF_M_HLBL(___L6___hamt_23_hamt_2d_map)
___DEF_M_HLBL(___L7___hamt_23_hamt_2d_map)
___DEF_M_HLBL(___L8___hamt_23_hamt_2d_map)
___DEF_M_HLBL(___L9___hamt_23_hamt_2d_map)
___DEF_M_HLBL(___L10___hamt_23_hamt_2d_map)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d__3e_list)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d__3e_list)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d__3e_list)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d__3e_list)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d__3e_list)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L1___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L2___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L3___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L4___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L5___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L6___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L7___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L8___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L9___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L10___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L11___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L12___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L13___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L14___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L15___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L16___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L17___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L18___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L19___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L20___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L21___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL(___L22___hamt_23_list_2d__3e_hamt)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_keys)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_keys)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_keys)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_keys)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_keys)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_keys)
___DEF_M_HLBL(___L6___hamt_23_hamt_2d_keys)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_values)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_values)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_values)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_values)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_values)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_values)
___DEF_M_HLBL(___L6___hamt_23_hamt_2d_values)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L1___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L2___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L3___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L4___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L5___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L6___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L7___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L8___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L9___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L10___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L11___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL(___L12___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_test_2d_procedure_2d__3e_test)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_test_2d_procedure_2d__3e_hash)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___hamt_23_hash_2d_procedure_2d__3e_hash)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_
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
___DEF_P_HLBL(___L0___hamt_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L___hamt_23_)
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_fail_2d_check_2d_hamt
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
___DEF_P_HLBL(___L0___hamt_23_fail_2d_check_2d_hamt)
___DEF_P_HLBL(___L1___hamt_23_fail_2d_check_2d_hamt)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_fail_2d_check_2d_hamt)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(0,2,0,0)
___DEF_GLBL(___L___hamt_23_fail_2d_check_2d_hamt)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_fail_2d_check_2d_hamt)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_make_2d_hamt_2a_
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
___DEF_P_HLBL(___L0___hamt_23_make_2d_hamt_2a_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_make_2d_hamt_2a_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L___hamt_23_make_2d_hamt_2a_)
   ___SET_R1(___SUB(4))
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2a__2d_ref
#undef ___PH_LBL0
#define ___PH_LBL0 8
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2a__2d_ref)
___DEF_P_HLBL(___L1___hamt_23_hamt_2a__2d_ref)
___DEF_P_HLBL(___L2___hamt_23_hamt_2a__2d_ref)
___DEF_P_HLBL(___L3___hamt_23_hamt_2a__2d_ref)
___DEF_P_HLBL(___L4___hamt_23_hamt_2a__2d_ref)
___DEF_P_HLBL(___L5___hamt_23_hamt_2a__2d_ref)
___DEF_P_HLBL(___L6___hamt_23_hamt_2a__2d_ref)
___DEF_P_HLBL(___L7___hamt_23_hamt_2a__2d_ref)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2a__2d_ref)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,3,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2a__2d_ref)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R1(___R2)
   ___SET_R2(___UNCHECKEDSTRUCTUREREF(___R3,___FIX(2L),___SUB(0),___FAL))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2a__2d_ref)
   ___SET_R0(___LBL(2))
   ___JUMPGENNOTSAFE(___SET_NARGS(1),___R2)
___DEF_SLBL(2,___L2___hamt_23_hamt_2a__2d_ref)
   ___SET_R1(___FIXAND(___FIX(268435455L),___R1))
   ___SET_R1(___FIXIOR(___FIX(268435456L),___R1))
   ___SET_STK(-3,___STK(-7))
   ___SET_STK(-7,___STK(-6))
   ___SET_R3(___STK(-4))
   ___SET_R2(___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-3))
   ___ADJFP(-7)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2a__2d_ref)
   ___GOTO(___L9___hamt_23_hamt_2a__2d_ref)
___DEF_GLBL(___L8___hamt_23_hamt_2a__2d_ref)
   ___SET_R4(___VECTORREF(___STK(-2),___STK(0)))
   ___IF(___FIXEQ(___FIX(1L),___R2))
   ___GOTO(___L10___hamt_23_hamt_2a__2d_ref)
   ___END_IF
   ___IF(___PAIRP(___R4))
   ___GOTO(___L11___hamt_23_hamt_2a__2d_ref)
   ___END_IF
   ___SET_STK(-2,___R4)
   ___ADJFP(-2)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2a__2d_ref)
___DEF_GLBL(___L9___hamt_23_hamt_2a__2d_ref)
   ___SET_R4(___FIXAND(___R2,___FIX(15L)))
   ___SET_R2(___FIXASHR(___R2,___FIX(4L)))
   ___SET_STK(1,___VECTORREF(___STK(0),___FIX(0L)))
   ___SET_STK(2,___FIXASHL(___FIX(1L),___R4))
   ___SET_STK(2,___FIXSUB(___STK(2),___FIX(1L)))
   ___SET_STK(2,___FIXAND(___STK(1),___STK(2)))
   ___SET_STK(2,___FIXBITCOUNT(___STK(2)))
   ___SET_STK(2,___FIXADD(___STK(2),___FIX(1L)))
   ___SET_R4(___FIXASHR(___STK(1),___R4))
   ___ADJFP(2)
   ___IF(___FIXODDP(___R4))
   ___GOTO(___L8___hamt_23_hamt_2a__2d_ref)
   ___END_IF
   ___SET_R1(___FAL)
   ___ADJFP(-3)
   ___JUMPRET(___R0)
___DEF_GLBL(___L10___hamt_23_hamt_2a__2d_ref)
   ___SET_R2(___R1)
   ___SET_R1(___R4)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2a__2d_ref)
   ___ADJFP(-3)
   ___JUMPINT(___SET_NARGS(3),___PRC(137),___L___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_GLBL(___L11___hamt_23_hamt_2a__2d_ref)
   ___SET_R2(___CAR(___R4))
   ___SET_STK(-2,___R0)
   ___SET_STK(-1,___R4)
   ___SET_STK(0,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(0))
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R3,___FIX(1L),___SUB(0),___FAL))
   ___ADJFP(5)
   ___POLL(6)
___DEF_SLBL(6,___L6___hamt_23_hamt_2a__2d_ref)
   ___SET_R0(___LBL(7))
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___R3)
___DEF_SLBL(7,___L7___hamt_23_hamt_2a__2d_ref)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L12___hamt_23_hamt_2a__2d_ref)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L12___hamt_23_hamt_2a__2d_ref)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2a__2d_set
#undef ___PH_LBL0
#define ___PH_LBL0 17
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L1___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L2___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L3___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L4___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L5___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L6___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L7___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L8___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L9___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L10___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L11___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L12___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L13___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L14___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L15___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L16___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L17___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L18___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L19___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L20___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L21___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L22___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L23___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L24___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L25___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L26___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L27___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L28___hamt_23_hamt_2a__2d_set)
___DEF_P_HLBL(___L29___hamt_23_hamt_2a__2d_set)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2a__2d_set)
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,4,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2a__2d_set)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R2(___UNCHECKEDSTRUCTUREREF(___R3,___FIX(2L),___SUB(0),___FAL))
   ___ADJFP(7)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2a__2d_set)
   ___SET_R0(___LBL(2))
   ___JUMPGENNOTSAFE(___SET_NARGS(1),___R2)
___DEF_SLBL(2,___L2___hamt_23_hamt_2a__2d_set)
   ___SET_R1(___FIXAND(___FIX(268435455L),___R1))
   ___SET_R1(___FIXIOR(___FIX(268435456L),___R1))
   ___SET_R2(___CONS(___STK(-5),___STK(-4)))
   ___SET_R3(___STK(-3))
   ___SET_STK(-5,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___CHECK_HEAP(3,4096)
___DEF_SLBL(3,___L3___hamt_23_hamt_2a__2d_set)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2a__2d_set)
   ___GOTO(___L31___hamt_23_hamt_2a__2d_set)
___DEF_GLBL(___L30___hamt_23_hamt_2a__2d_set)
   ___SET_R4(___VECTORREF(___STK(-3),___STK(-1)))
   ___IF(___FIXEQ(___FIX(1L),___R2))
   ___GOTO(___L33___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___IF(___PAIRP(___R4))
   ___GOTO(___L35___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_STK(-2,___R0)
   ___SET_STK(0,___R4)
   ___SET_STK(5,___R4)
   ___SET_R0(___LBL(28))
   ___ADJFP(5)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2a__2d_set)
___DEF_GLBL(___L31___hamt_23_hamt_2a__2d_set)
   ___SET_R4(___FIXAND(___R2,___FIX(15L)))
   ___SET_R2(___FIXASHR(___R2,___FIX(4L)))
   ___SET_STK(1,___VECTORREF(___STK(0),___FIX(0L)))
   ___SET_STK(2,___FIXASHL(___FIX(1L),___R4))
   ___SET_STK(2,___FIXSUB(___STK(2),___FIX(1L)))
   ___SET_STK(2,___FIXAND(___STK(1),___STK(2)))
   ___SET_STK(2,___FIXBITCOUNT(___STK(2)))
   ___SET_STK(2,___FIXADD(___STK(2),___FIX(1L)))
   ___SET_STK(3,___FIXASHR(___STK(1),___R4))
   ___ADJFP(3)
   ___IF(___FIXODDP(___STK(0)))
   ___GOTO(___L30___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_STK(0,___UNCHECKEDSTRUCTUREREF(___R3,___FIX(4L),___SUB(0),___FAL))
   ___SET_STK(0,___FIXADD(___STK(0),___FIX(1L)))
   ___UNCHECKEDSTRUCTURESET(___R3,___STK(0),___FIX(4L),___SUB(0),___FAL)
   ___IF(___NOT(___FIXEQ(___FIX(1L),___R2)))
   ___GOTO(___L32___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_R1(___CONS(___R1,___NUL))
   ___CHECK_HEAP(6,4096)
___DEF_SLBL(6,___L6___hamt_23_hamt_2a__2d_set)
___DEF_GLBL(___L32___hamt_23_hamt_2a__2d_set)
   ___VECTORINSERTSMALL3(___STK(-3),___STK(-1),___R1)
   ___SET_R1(___GET_SMALL_ALLOC)
   ___SET_R2(___FIXASHL(___FIX(1L),___R4))
   ___SET_R2(___FIXIOR(___STK(-2),___R2))
   ___VECTORSET(___R1,___FIX(0L),___R2)
   ___ADJFP(-4)
   ___CHECK_HEAP(7,4096)
___DEF_SLBL(7,___L7___hamt_23_hamt_2a__2d_set)
   ___JUMPRET(___R0)
___DEF_GLBL(___L33___hamt_23_hamt_2a__2d_set)
   ___SET_STK(-2,___R0)
   ___SET_STK(0,___R1)
   ___SET_STK(1,___R3)
   ___SET_STK(2,___R4)
   ___SET_R2(___CAR(___R1))
   ___SET_R1(___R4)
   ___ADJFP(8)
   ___POLL(8)
___DEF_SLBL(8,___L8___hamt_23_hamt_2a__2d_set)
   ___SET_R0(___LBL(9))
   ___JUMPINT(___SET_NARGS(3),___PRC(143),___L___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_SLBL(9,___L9___hamt_23_hamt_2a__2d_set)
   ___IF(___NOT(___EQP(___STK(-6),___R1)))
   ___GOTO(___L34___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_R2(___UNCHECKEDSTRUCTUREREF(___STK(-7),___FIX(4L),___SUB(0),___FAL))
   ___SET_R2(___FIXADD(___R2,___FIX(1L)))
   ___UNCHECKEDSTRUCTURESET(___STK(-7),___R2,___FIX(4L),___SUB(0),___FAL)
___DEF_GLBL(___L34___hamt_23_hamt_2a__2d_set)
   ___SET_R1(___CONS(___STK(-8),___R1))
   ___VECTORUPDATESMALL3(___STK(-11),___STK(-9),___R1)
   ___SET_R1(___GET_SMALL_ALLOC)
   ___ADJFP(-10)
   ___CHECK_HEAP(10,4096)
___DEF_SLBL(10,___L10___hamt_23_hamt_2a__2d_set)
   ___ADJFP(-2)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L35___hamt_23_hamt_2a__2d_set)
   ___SET_STK(-2,___CAR(___R4))
   ___SET_STK(0,___CAR(___R1))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_STK(5,___R4)
   ___SET_R2(___STK(0))
   ___SET_R1(___STK(-2))
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R3,___FIX(1L),___SUB(0),___FAL))
   ___ADJFP(8)
   ___POLL(11)
___DEF_SLBL(11,___L11___hamt_23_hamt_2a__2d_set)
   ___SET_R0(___LBL(12))
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___R3)
___DEF_SLBL(12,___L12___hamt_23_hamt_2a__2d_set)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L37___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_R1(___CDR(___STK(-6)))
   ___SET_R2(___CDR(___STK(-3)))
   ___IF(___NOT(___EQP(___R2,___R1)))
   ___GOTO(___L36___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_R1(___STK(-11))
   ___ADJFP(-12)
   ___JUMPRET(___STK(5))
___DEF_GLBL(___L36___hamt_23_hamt_2a__2d_set)
   ___VECTORUPDATESMALL3(___STK(-11),___STK(-9),___STK(-6))
   ___SET_R1(___GET_SMALL_ALLOC)
   ___ADJFP(-7)
   ___CHECK_HEAP(13,4096)
___DEF_SLBL(13,___L13___hamt_23_hamt_2a__2d_set)
   ___ADJFP(-5)
   ___JUMPRET(___STK(5))
___DEF_GLBL(___L37___hamt_23_hamt_2a__2d_set)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___STK(-4),___FIX(4L),___SUB(0),___FAL))
   ___SET_R1(___FIXADD(___R1,___FIX(1L)))
   ___UNCHECKEDSTRUCTURESET(___STK(-4),___R1,___FIX(4L),___SUB(0),___FAL)
   ___SET_R1(___STK(-10))
   ___SET_R2(___UNCHECKEDSTRUCTUREREF(___STK(-4),___FIX(2L),___SUB(0),___FAL))
   ___SET_R0(___LBL(14))
   ___JUMPGENNOTSAFE(___SET_NARGS(1),___R2)
___DEF_SLBL(14,___L14___hamt_23_hamt_2a__2d_set)
   ___SET_R1(___FIXAND(___FIX(268435455L),___R1))
   ___SET_R1(___FIXIOR(___FIX(268435456L),___R1))
   ___SET_R2(___FIXAND(___FIX(268435456L),___STK(-5)))
   ___IF(___NOT(___FIXEQ(___FIX(0L),___R2)))
   ___GOTO(___L41___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_R1(___FIXASHR(___R1,___FIX(4L)))
   ___SET_R2(___FIXASHR(___FIX(268435456L),___FIX(4L)))
   ___SET_R3(___FIXAND(___R2,___STK(-5)))
   ___IF(___NOT(___FIXEQ(___FIX(0L),___R3)))
   ___GOTO(___L41___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_R2(___FIXASHR(___R2,___FIX(4L)))
   ___SET_R1(___FIXASHR(___R1,___FIX(4L)))
   ___SET_R3(___FIXAND(___R2,___STK(-5)))
   ___IF(___NOT(___FIXEQ(___FIX(0L),___R3)))
   ___GOTO(___L41___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_R1(___FIXASHR(___R1,___FIX(4L)))
   ___SET_R2(___FIXASHR(___R2,___FIX(4L)))
   ___SET_R3(___FIXAND(___R2,___STK(-5)))
   ___IF(___NOT(___FIXEQ(___FIX(0L),___R3)))
   ___GOTO(___L41___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_R3(___FIXASHR(___R1,___FIX(4L)))
   ___SET_R2(___FIXASHR(___R2,___FIX(4L)))
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(16))
   ___GOTO(___L39___hamt_23_hamt_2a__2d_set)
___DEF_GLBL(___L38___hamt_23_hamt_2a__2d_set)
   ___SET_R2(___FIXASHR(___R2,___FIX(4L)))
   ___SET_R3(___FIXASHR(___R3,___FIX(4L)))
   ___SET_R4(___FIXAND(___R2,___R1))
   ___IF(___NOT(___FIXEQ(___FIX(0L),___R4)))
   ___GOTO(___L40___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_R3(___FIXASHR(___R3,___FIX(4L)))
   ___SET_R2(___FIXASHR(___R2,___FIX(4L)))
   ___POLL(15)
___DEF_SLBL(15,___L15___hamt_23_hamt_2a__2d_set)
___DEF_GLBL(___L39___hamt_23_hamt_2a__2d_set)
   ___SET_R4(___FIXAND(___R2,___R1))
   ___IF(___FIXEQ(___FIX(0L),___R4))
   ___GOTO(___L38___hamt_23_hamt_2a__2d_set)
   ___END_IF
___DEF_GLBL(___L40___hamt_23_hamt_2a__2d_set)
   ___SET_R1(___R3)
   ___JUMPRET(___R0)
___DEF_SLBL(16,___L16___hamt_23_hamt_2a__2d_set)
___DEF_GLBL(___L41___hamt_23_hamt_2a__2d_set)
   ___SET_STK(-10,___STK(-3))
   ___SET_STK(-3,___STK(-6))
   ___SET_R3(___R1)
   ___SET_R2(___STK(-10))
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(26))
   ___ADJFP(-3)
   ___GOTO(___L43___hamt_23_hamt_2a__2d_set)
___DEF_GLBL(___L42___hamt_23_hamt_2a__2d_set)
   ___IF(___NOT(___FIXEQ(___R4,___STK(0))))
   ___GOTO(___L46___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_STK(0,___R0)
   ___SET_STK(1,___R4)
   ___SET_STK(7,___STK(-1))
   ___SET_R3(___FIXASHR(___R3,___FIX(4L)))
   ___SET_R0(___LBL(24))
   ___ADJFP(7)
   ___POLL(17)
___DEF_SLBL(17,___L17___hamt_23_hamt_2a__2d_set)
___DEF_GLBL(___L43___hamt_23_hamt_2a__2d_set)
   ___SET_R4(___FIXAND(___R1,___FIX(15L)))
   ___SET_STK(1,___FIXAND(___R3,___FIX(15L)))
   ___SET_R1(___FIXASHR(___R1,___FIX(4L)))
   ___ADJFP(1)
   ___IF(___NOT(___FIXEQ(___FIX(1L),___R1)))
   ___GOTO(___L42___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___IF(___FIXEQ(___R4,___STK(0)))
   ___GOTO(___L45___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_R1(___CONS(___STK(-1),___NUL))
   ___SET_R2(___CONS(___R2,___NUL))
   ___SET_R3(___FIXASHL(___FIX(1L),___STK(0)))
   ___SET_STK(-1,___FIXASHL(___FIX(1L),___R4))
   ___SET_R3(___FIXIOR(___STK(-1),___R3))
   ___CHECK_HEAP(18,4096)
___DEF_SLBL(18,___L18___hamt_23_hamt_2a__2d_set)
   ___IF(___NOT(___FIXLT(___R4,___STK(0))))
   ___GOTO(___L44___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___BEGIN_ALLOC_VECTOR(3UL)
   ___ADD_VECTOR_ELEM(0,___R3)
   ___ADD_VECTOR_ELEM(1,___R1)
   ___ADD_VECTOR_ELEM(2,___R2)
   ___END_ALLOC_VECTOR(3)
   ___SET_R1(___GET_VECTOR(3))
   ___ADJFP(-2)
   ___CHECK_HEAP(19,4096)
___DEF_SLBL(19,___L19___hamt_23_hamt_2a__2d_set)
   ___JUMPRET(___R0)
___DEF_GLBL(___L44___hamt_23_hamt_2a__2d_set)
   ___BEGIN_ALLOC_VECTOR(3UL)
   ___ADD_VECTOR_ELEM(0,___R3)
   ___ADD_VECTOR_ELEM(1,___R2)
   ___ADD_VECTOR_ELEM(2,___R1)
   ___END_ALLOC_VECTOR(3)
   ___SET_R1(___GET_VECTOR(3))
   ___ADJFP(-2)
   ___CHECK_HEAP(20,4096)
___DEF_SLBL(20,___L20___hamt_23_hamt_2a__2d_set)
   ___JUMPRET(___R0)
___DEF_GLBL(___L45___hamt_23_hamt_2a__2d_set)
   ___BEGIN_ALLOC_LIST(2UL,___R2)
   ___ADD_LIST_ELEM(1,___STK(-1))
   ___END_ALLOC_LIST(2)
   ___SET_R1(___GET_LIST(2))
   ___SET_R2(___FIXASHL(___FIX(1L),___R4))
   ___BEGIN_ALLOC_VECTOR(2UL)
   ___ADD_VECTOR_ELEM(0,___R2)
   ___ADD_VECTOR_ELEM(1,___R1)
   ___END_ALLOC_VECTOR(2)
   ___SET_R1(___GET_VECTOR(2))
   ___ADJFP(-2)
   ___CHECK_HEAP(21,4096)
___DEF_SLBL(21,___L21___hamt_23_hamt_2a__2d_set)
   ___JUMPRET(___R0)
___DEF_GLBL(___L46___hamt_23_hamt_2a__2d_set)
   ___SET_R1(___FIXASHL(___FIX(1L),___STK(0)))
   ___SET_R3(___FIXASHL(___FIX(1L),___R4))
   ___SET_R1(___FIXIOR(___R3,___R1))
   ___IF(___NOT(___FIXLT(___R4,___STK(0))))
   ___GOTO(___L47___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___BEGIN_ALLOC_VECTOR(3UL)
   ___ADD_VECTOR_ELEM(0,___R1)
   ___ADD_VECTOR_ELEM(1,___STK(-1))
   ___ADD_VECTOR_ELEM(2,___R2)
   ___END_ALLOC_VECTOR(3)
   ___SET_R1(___GET_VECTOR(3))
   ___ADJFP(-2)
   ___CHECK_HEAP(22,4096)
___DEF_SLBL(22,___L22___hamt_23_hamt_2a__2d_set)
   ___JUMPRET(___R0)
___DEF_GLBL(___L47___hamt_23_hamt_2a__2d_set)
   ___BEGIN_ALLOC_VECTOR(3UL)
   ___ADD_VECTOR_ELEM(0,___R1)
   ___ADD_VECTOR_ELEM(1,___R2)
   ___ADD_VECTOR_ELEM(2,___STK(-1))
   ___END_ALLOC_VECTOR(3)
   ___SET_R1(___GET_VECTOR(3))
   ___ADJFP(-2)
   ___CHECK_HEAP(23,4096)
___DEF_SLBL(23,___L23___hamt_23_hamt_2a__2d_set)
   ___JUMPRET(___R0)
___DEF_SLBL(24,___L24___hamt_23_hamt_2a__2d_set)
   ___SET_R2(___FIXASHL(___FIX(1L),___STK(-5)))
   ___BEGIN_ALLOC_VECTOR(2UL)
   ___ADD_VECTOR_ELEM(0,___R2)
   ___ADD_VECTOR_ELEM(1,___R1)
   ___END_ALLOC_VECTOR(2)
   ___SET_R1(___GET_VECTOR(2))
   ___ADJFP(-6)
   ___CHECK_HEAP(25,4096)
___DEF_SLBL(25,___L25___hamt_23_hamt_2a__2d_set)
   ___ADJFP(-2)
   ___JUMPRET(___STK(2))
___DEF_SLBL(26,___L26___hamt_23_hamt_2a__2d_set)
   ___VECTORUPDATESMALL3(___STK(-7),___STK(-5),___R1)
   ___SET_R1(___GET_SMALL_ALLOC)
   ___ADJFP(-3)
   ___CHECK_HEAP(27,4096)
___DEF_SLBL(27,___L27___hamt_23_hamt_2a__2d_set)
   ___ADJFP(-5)
   ___JUMPRET(___STK(5))
___DEF_SLBL(28,___L28___hamt_23_hamt_2a__2d_set)
   ___IF(___NOT(___EQP(___R1,___STK(-4))))
   ___GOTO(___L48___hamt_23_hamt_2a__2d_set)
   ___END_IF
   ___SET_R1(___STK(-7))
   ___ADJFP(-8)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L48___hamt_23_hamt_2a__2d_set)
   ___VECTORUPDATESMALL3(___STK(-7),___STK(-5),___R1)
   ___SET_R1(___GET_SMALL_ALLOC)
   ___ADJFP(-6)
   ___CHECK_HEAP(29,4096)
___DEF_SLBL(29,___L29___hamt_23_hamt_2a__2d_set)
   ___ADJFP(-2)
   ___JUMPRET(___STK(2))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2a__2d_remove
#undef ___PH_LBL0
#define ___PH_LBL0 48
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L1___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L2___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L3___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L4___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L5___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L6___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L7___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L8___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L9___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L10___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L11___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L12___hamt_23_hamt_2a__2d_remove)
___DEF_P_HLBL(___L13___hamt_23_hamt_2a__2d_remove)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2a__2d_remove)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,3,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2a__2d_remove)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R1(___R2)
   ___SET_R2(___UNCHECKEDSTRUCTUREREF(___R3,___FIX(2L),___SUB(0),___FAL))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2a__2d_remove)
   ___SET_R0(___LBL(2))
   ___JUMPGENNOTSAFE(___SET_NARGS(1),___R2)
___DEF_SLBL(2,___L2___hamt_23_hamt_2a__2d_remove)
   ___SET_R1(___FIXAND(___FIX(268435455L),___R1))
   ___SET_R1(___FIXIOR(___FIX(268435456L),___R1))
   ___SET_STK(-3,___STK(-7))
   ___SET_STK(-7,___STK(-6))
   ___SET_R3(___STK(-4))
   ___SET_R2(___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-3))
   ___ADJFP(-7)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2a__2d_remove)
   ___GOTO(___L15___hamt_23_hamt_2a__2d_remove)
___DEF_GLBL(___L14___hamt_23_hamt_2a__2d_remove)
   ___SET_STK(0,___VECTORREF(___STK(-4),___STK(-1)))
   ___IF(___FIXEQ(___FIX(1L),___STK(-3)))
   ___GOTO(___L16___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___IF(___PAIRP(___STK(0)))
   ___GOTO(___L22___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___SET_STK(-2,___R0)
   ___SET_STK(1,___R2)
   ___SET_STK(8,___STK(0))
   ___SET_R2(___STK(-3))
   ___SET_R0(___LBL(12))
   ___ADJFP(8)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2a__2d_remove)
___DEF_GLBL(___L15___hamt_23_hamt_2a__2d_remove)
   ___SET_R4(___FIXAND(___R2,___FIX(15L)))
   ___SET_STK(1,___FIXASHR(___R2,___FIX(4L)))
   ___SET_STK(2,___VECTORREF(___STK(0),___FIX(0L)))
   ___SET_STK(3,___FIXASHL(___FIX(1L),___R4))
   ___SET_STK(3,___FIXSUB(___STK(3),___FIX(1L)))
   ___SET_STK(3,___FIXAND(___STK(2),___STK(3)))
   ___SET_STK(3,___FIXBITCOUNT(___STK(3)))
   ___SET_STK(3,___FIXADD(___STK(3),___FIX(1L)))
   ___SET_STK(4,___FIXASHR(___STK(2),___R4))
   ___ADJFP(4)
   ___IF(___FIXODDP(___STK(0)))
   ___GOTO(___L14___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___SET_R1(___STK(-4))
   ___ADJFP(-5)
   ___JUMPRET(___R0)
___DEF_GLBL(___L16___hamt_23_hamt_2a__2d_remove)
   ___SET_STK(-3,___R0)
   ___SET_STK(1,___R4)
   ___SET_R2(___R1)
   ___SET_R1(___STK(0))
   ___ADJFP(7)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2a__2d_remove)
   ___SET_R0(___LBL(6))
   ___JUMPINT(___SET_NARGS(3),___PRC(143),___L___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_SLBL(6,___L6___hamt_23_hamt_2a__2d_remove)
   ___IF(___EQP(___R1,___STK(-7)))
   ___GOTO(___L21___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L18___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___SET_R2(___VECTORLENGTH(___STK(-11)))
   ___IF(___FIXGE(___R2,___FIX(3L)))
   ___GOTO(___L17___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___SET_R2(___CDR(___R1))
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L19___hamt_23_hamt_2a__2d_remove)
   ___END_IF
___DEF_GLBL(___L17___hamt_23_hamt_2a__2d_remove)
   ___VECTORUPDATESMALL3(___STK(-11),___STK(-8),___R1)
   ___SET_R1(___GET_SMALL_ALLOC)
   ___ADJFP(-10)
   ___CHECK_HEAP(7,4096)
___DEF_SLBL(7,___L7___hamt_23_hamt_2a__2d_remove)
   ___ADJFP(-2)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L18___hamt_23_hamt_2a__2d_remove)
   ___SET_R1(___VECTORLENGTH(___STK(-11)))
   ___IF(___NOT(___FIXEQ(___R1,___FIX(3L))))
   ___GOTO(___L20___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___SET_R1(___FIXXOR(___STK(-8),___FIX(3L)))
   ___SET_R1(___VECTORREF(___STK(-11),___R1))
   ___SET_R2(___CDR(___R1))
   ___IF(___PAIRP(___R2))
   ___GOTO(___L20___hamt_23_hamt_2a__2d_remove)
   ___END_IF
___DEF_GLBL(___L19___hamt_23_hamt_2a__2d_remove)
   ___SET_R1(___CAR(___R1))
   ___ADJFP(-12)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L20___hamt_23_hamt_2a__2d_remove)
   ___VECTORDELETESMALL2(___STK(-11),___STK(-8))
   ___SET_R1(___GET_SMALL_ALLOC)
   ___SET_R2(___FIXASHL(___FIX(1L),___STK(-6)))
   ___SET_R2(___FIXNOT(___R2))
   ___SET_R2(___FIXAND(___STK(-9),___R2))
   ___VECTORSET(___R1,___FIX(0L),___R2)
   ___ADJFP(-10)
   ___CHECK_HEAP(8,4096)
___DEF_SLBL(8,___L8___hamt_23_hamt_2a__2d_remove)
   ___ADJFP(-2)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L21___hamt_23_hamt_2a__2d_remove)
   ___SET_R1(___STK(-11))
   ___ADJFP(-12)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L22___hamt_23_hamt_2a__2d_remove)
   ___SET_STK(-3,___CAR(___STK(0)))
   ___SET_STK(0,___R0)
   ___SET_STK(1,___R2)
   ___SET_STK(2,___R4)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-3))
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R3,___FIX(1L),___SUB(0),___FAL))
   ___ADJFP(7)
   ___POLL(9)
___DEF_SLBL(9,___L9___hamt_23_hamt_2a__2d_remove)
   ___SET_R0(___LBL(10))
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___R3)
___DEF_SLBL(10,___L10___hamt_23_hamt_2a__2d_remove)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L24___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___IF(___FIXGE(___STK(-6),___FIX(268435456L)))
   ___GOTO(___L23___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___SET_R1(___VECTORLENGTH(___STK(-11)))
   ___IF(___FIXGE(___R1,___FIX(4L)))
   ___GOTO(___L23___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___SET_R1(___FIXXOR(___STK(-8),___FIX(3L)))
   ___SET_R1(___VECTORREF(___STK(-11),___R1))
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L23___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___ADJFP(-12)
   ___JUMPRET(___STK(5))
___DEF_GLBL(___L23___hamt_23_hamt_2a__2d_remove)
   ___VECTORDELETESMALL2(___STK(-11),___STK(-8))
   ___SET_R1(___GET_SMALL_ALLOC)
   ___SET_R2(___FIXASHL(___FIX(1L),___STK(-5)))
   ___SET_R2(___FIXNOT(___R2))
   ___SET_R2(___FIXAND(___STK(-9),___R2))
   ___VECTORSET(___R1,___FIX(0L),___R2)
   ___ADJFP(-7)
   ___CHECK_HEAP(11,4096)
___DEF_SLBL(11,___L11___hamt_23_hamt_2a__2d_remove)
   ___ADJFP(-5)
   ___JUMPRET(___STK(5))
___DEF_GLBL(___L24___hamt_23_hamt_2a__2d_remove)
   ___SET_R1(___STK(-11))
   ___ADJFP(-12)
   ___JUMPRET(___STK(5))
___DEF_SLBL(12,___L12___hamt_23_hamt_2a__2d_remove)
   ___IF(___EQP(___R1,___STK(-7)))
   ___GOTO(___L27___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L25___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___IF(___FIXGE(___STK(-6),___FIX(268435456L)))
   ___GOTO(___L25___hamt_23_hamt_2a__2d_remove)
   ___END_IF
   ___SET_R2(___VECTORLENGTH(___STK(-11)))
   ___IF(___NOT(___FIXGE(___R2,___FIX(3L))))
   ___GOTO(___L26___hamt_23_hamt_2a__2d_remove)
   ___END_IF
___DEF_GLBL(___L25___hamt_23_hamt_2a__2d_remove)
   ___VECTORUPDATESMALL3(___STK(-11),___STK(-8),___R1)
   ___SET_R1(___GET_SMALL_ALLOC)
   ___ADJFP(-9)
   ___CHECK_HEAP(13,4096)
___DEF_SLBL(13,___L13___hamt_23_hamt_2a__2d_remove)
   ___ADJFP(-3)
   ___JUMPRET(___STK(3))
___DEF_GLBL(___L26___hamt_23_hamt_2a__2d_remove)
   ___ADJFP(-12)
   ___JUMPRET(___STK(3))
___DEF_GLBL(___L27___hamt_23_hamt_2a__2d_remove)
   ___SET_R1(___STK(-11))
   ___ADJFP(-12)
   ___JUMPRET(___STK(3))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2a__2d_search
#undef ___PH_LBL0
#define ___PH_LBL0 63
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L1___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L2___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L3___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L4___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L5___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L6___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L7___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L8___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L9___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L10___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L11___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L12___hamt_23_hamt_2a__2d_search)
___DEF_P_HLBL(___L13___hamt_23_hamt_2a__2d_search)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2a__2d_search)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2a__2d_search)
   ___SET_STK(1,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(1))
   ___SET_R3(___FIX(0L))
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2a__2d_search)
   ___GOTO(___L15___hamt_23_hamt_2a__2d_search)
___DEF_SLBL(2,___L2___hamt_23_hamt_2a__2d_search)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L22___hamt_23_hamt_2a__2d_search)
   ___END_IF
   ___SET_R3(___STK(-3))
   ___SET_R2(___FIXADD(___STK(-4),___FIX(1L)))
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2a__2d_search)
___DEF_GLBL(___L14___hamt_23_hamt_2a__2d_search)
   ___SET_R4(___VECTORLENGTH(___R1))
   ___IF(___NOT(___FIXLT(___R2,___R4)))
   ___GOTO(___L26___hamt_23_hamt_2a__2d_search)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R2(___VECTORREF(___R1,___R2))
   ___SET_R1(___STK(0))
   ___SET_R0(___LBL(11))
   ___ADJFP(7)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2a__2d_search)
___DEF_GLBL(___L15___hamt_23_hamt_2a__2d_search)
   ___IF(___NOT(___FIXEQ(___R3,___FIX(7L))))
   ___GOTO(___L20___hamt_23_hamt_2a__2d_search)
   ___END_IF
___DEF_GLBL(___L16___hamt_23_hamt_2a__2d_search)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2a__2d_search)
___DEF_GLBL(___L17___hamt_23_hamt_2a__2d_search)
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L21___hamt_23_hamt_2a__2d_search)
   ___END_IF
   ___SET_R3(___CAR(___R2))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R2(___CDR(___R3))
   ___SET_R1(___CAR(___R3))
   ___ADJFP(8)
   ___POLL(6)
___DEF_SLBL(6,___L6___hamt_23_hamt_2a__2d_search)
   ___SET_R0(___LBL(7))
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___STK(-6))
___DEF_SLBL(7,___L7___hamt_23_hamt_2a__2d_search)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L18___hamt_23_hamt_2a__2d_search)
   ___END_IF
   ___SET_R2(___CDR(___STK(-5)))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(8)
___DEF_SLBL(8,___L8___hamt_23_hamt_2a__2d_search)
   ___GOTO(___L17___hamt_23_hamt_2a__2d_search)
___DEF_SLBL(9,___L9___hamt_23_hamt_2a__2d_search)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L19___hamt_23_hamt_2a__2d_search)
   ___END_IF
___DEF_GLBL(___L18___hamt_23_hamt_2a__2d_search)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L19___hamt_23_hamt_2a__2d_search)
   ___SET_STK(-3,___STK(-7))
   ___SET_STK(-7,___STK(-6))
   ___SET_R3(___STK(-4))
   ___SET_R1(___STK(-5))
   ___SET_R2(___FIX(2L))
   ___SET_R0(___STK(-3))
   ___ADJFP(-7)
   ___POLL(10)
___DEF_SLBL(10,___L10___hamt_23_hamt_2a__2d_search)
   ___GOTO(___L14___hamt_23_hamt_2a__2d_search)
___DEF_SLBL(11,___L11___hamt_23_hamt_2a__2d_search)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L22___hamt_23_hamt_2a__2d_search)
   ___END_IF
   ___SET_R1(___FIXADD(___STK(-4),___FIX(1L)))
   ___SET_R2(___VECTORLENGTH(___STK(-5)))
   ___IF(___NOT(___FIXLT(___R1,___R2)))
   ___GOTO(___L23___hamt_23_hamt_2a__2d_search)
   ___END_IF
   ___SET_STK(-4,___R1)
   ___SET_R2(___VECTORREF(___STK(-5),___R1))
   ___SET_R3(___STK(-3))
   ___SET_R1(___STK(-7))
   ___SET_R0(___LBL(2))
   ___IF(___FIXEQ(___R3,___FIX(7L)))
   ___GOTO(___L16___hamt_23_hamt_2a__2d_search)
   ___END_IF
___DEF_GLBL(___L20___hamt_23_hamt_2a__2d_search)
   ___IF(___PAIRP(___R2))
   ___GOTO(___L24___hamt_23_hamt_2a__2d_search)
   ___END_IF
   ___SET_R3(___FIXADD(___R3,___FIX(1L)))
   ___SET_R4(___VECTORLENGTH(___R2))
   ___IF(___FIXLT(___FIX(1L),___R4))
   ___GOTO(___L25___hamt_23_hamt_2a__2d_search)
   ___END_IF
___DEF_GLBL(___L21___hamt_23_hamt_2a__2d_search)
   ___SET_R1(___FAL)
   ___JUMPRET(___R0)
___DEF_GLBL(___L22___hamt_23_hamt_2a__2d_search)
   ___ADJFP(-8)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L23___hamt_23_hamt_2a__2d_search)
   ___SET_R1(___FAL)
   ___ADJFP(-8)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L24___hamt_23_hamt_2a__2d_search)
   ___SET_R3(___CDR(___R2))
   ___SET_STK(1,___R2)
   ___SET_R2(___R3)
   ___SET_R3(___CAR(___STK(1)))
   ___SET_STK(1,___R1)
   ___SET_R1(___R3)
   ___ADJFP(1)
   ___POLL(12)
___DEF_SLBL(12,___L12___hamt_23_hamt_2a__2d_search)
   ___ADJFP(-1)
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___STK(1))
___DEF_GLBL(___L25___hamt_23_hamt_2a__2d_search)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R2(___VECTORREF(___R2,___FIX(1L)))
   ___SET_R0(___LBL(9))
   ___ADJFP(8)
   ___POLL(13)
___DEF_SLBL(13,___L13___hamt_23_hamt_2a__2d_search)
   ___GOTO(___L15___hamt_23_hamt_2a__2d_search)
___DEF_GLBL(___L26___hamt_23_hamt_2a__2d_search)
   ___SET_R1(___FAL)
   ___ADJFP(-1)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2a__2d_fold
#undef ___PH_LBL0
#define ___PH_LBL0 78
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L1___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L2___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L3___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L4___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L5___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L6___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L7___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L8___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L9___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L10___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L11___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L12___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L13___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L14___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L15___hamt_23_hamt_2a__2d_fold)
___DEF_P_HLBL(___L16___hamt_23_hamt_2a__2d_fold)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2a__2d_fold)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,3,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2a__2d_fold)
   ___SET_STK(1,___R2)
   ___SET_R2(___R3)
   ___SET_R3(___FIX(0L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2a__2d_fold)
   ___GOTO(___L18___hamt_23_hamt_2a__2d_fold)
___DEF_SLBL(2,___L2___hamt_23_hamt_2a__2d_fold)
   ___SET_R3(___STK(-3))
   ___SET_R2(___FIXADD(___STK(-4),___FIX(1L)))
   ___SET_R0(___STK(-5))
   ___ADJFP(-6)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2a__2d_fold)
___DEF_GLBL(___L17___hamt_23_hamt_2a__2d_fold)
   ___SET_R4(___VECTORLENGTH(___STK(0)))
   ___IF(___NOT(___FIXLT(___R2,___R4)))
   ___GOTO(___L28___hamt_23_hamt_2a__2d_fold)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_STK(3,___R3)
   ___SET_STK(7,___STK(-1))
   ___SET_R2(___VECTORREF(___STK(0),___R2))
   ___SET_STK(4,___R1)
   ___SET_R1(___R2)
   ___SET_R2(___STK(4))
   ___SET_R0(___LBL(16))
   ___ADJFP(7)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2a__2d_fold)
___DEF_GLBL(___L18___hamt_23_hamt_2a__2d_fold)
   ___IF(___NOT(___FIXEQ(___R3,___FIX(7L))))
   ___GOTO(___L23___hamt_23_hamt_2a__2d_fold)
   ___END_IF
___DEF_GLBL(___L19___hamt_23_hamt_2a__2d_fold)
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(0))
   ___ADJFP(-1)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2a__2d_fold)
___DEF_GLBL(___L20___hamt_23_hamt_2a__2d_fold)
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L22___hamt_23_hamt_2a__2d_fold)
   ___END_IF
   ___SET_R4(___CAR(___R2))
   ___SET_R2(___CDR(___R2))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R1(___R3)
   ___SET_R3(___CDR(___R4))
   ___SET_R2(___CAR(___R4))
   ___ADJFP(8)
   ___POLL(6)
___DEF_SLBL(6,___L6___hamt_23_hamt_2a__2d_fold)
   ___SET_R0(___LBL(7))
   ___JUMPGENNOTSAFE(___SET_NARGS(3),___STK(-6))
___DEF_SLBL(7,___L7___hamt_23_hamt_2a__2d_fold)
   ___IF(___NOT(___PAIRP(___STK(-5))))
   ___GOTO(___L21___hamt_23_hamt_2a__2d_fold)
   ___END_IF
   ___SET_R2(___CAR(___STK(-5)))
   ___SET_R3(___CDR(___STK(-5)))
   ___SET_STK(-5,___R3)
   ___SET_R3(___CDR(___R2))
   ___SET_R2(___CAR(___R2))
   ___SET_R0(___LBL(8))
   ___JUMPGENNOTSAFE(___SET_NARGS(3),___STK(-6))
___DEF_SLBL(8,___L8___hamt_23_hamt_2a__2d_fold)
   ___IF(___NOT(___PAIRP(___STK(-5))))
   ___GOTO(___L21___hamt_23_hamt_2a__2d_fold)
   ___END_IF
   ___SET_R2(___CAR(___STK(-5)))
   ___SET_R3(___CDR(___R2))
   ___SET_R2(___CAR(___R2))
   ___SET_R0(___LBL(9))
   ___JUMPGENNOTSAFE(___SET_NARGS(3),___STK(-6))
___DEF_SLBL(9,___L9___hamt_23_hamt_2a__2d_fold)
   ___SET_R3(___R1)
   ___SET_R2(___CDR(___STK(-5)))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(10)
___DEF_SLBL(10,___L10___hamt_23_hamt_2a__2d_fold)
   ___GOTO(___L20___hamt_23_hamt_2a__2d_fold)
___DEF_GLBL(___L21___hamt_23_hamt_2a__2d_fold)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L22___hamt_23_hamt_2a__2d_fold)
   ___SET_R1(___R3)
   ___JUMPRET(___R0)
___DEF_SLBL(11,___L11___hamt_23_hamt_2a__2d_fold)
   ___SET_R2(___FIXADD(___STK(-4),___FIX(1L)))
   ___SET_R3(___VECTORLENGTH(___STK(-6)))
   ___IF(___NOT(___FIXLT(___R2,___R3)))
   ___GOTO(___L26___hamt_23_hamt_2a__2d_fold)
   ___END_IF
   ___SET_STK(-4,___R2)
   ___SET_STK(1,___STK(-7))
   ___SET_R2(___VECTORREF(___STK(-6),___R2))
   ___SET_STK(-2,___R1)
   ___SET_R1(___R2)
   ___SET_R3(___STK(-3))
   ___SET_R2(___STK(-2))
   ___SET_R0(___LBL(2))
   ___ADJFP(1)
   ___IF(___FIXEQ(___R3,___FIX(7L)))
   ___GOTO(___L19___hamt_23_hamt_2a__2d_fold)
   ___END_IF
___DEF_GLBL(___L23___hamt_23_hamt_2a__2d_fold)
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L24___hamt_23_hamt_2a__2d_fold)
   ___END_IF
   ___SET_STK(1,___R1)
   ___SET_R1(___R2)
   ___SET_R3(___CDR(___STK(1)))
   ___SET_R2(___CAR(___STK(1)))
   ___ADJFP(1)
   ___POLL(12)
___DEF_SLBL(12,___L12___hamt_23_hamt_2a__2d_fold)
   ___ADJFP(-2)
   ___JUMPGENNOTSAFE(___SET_NARGS(3),___STK(1))
___DEF_GLBL(___L24___hamt_23_hamt_2a__2d_fold)
   ___SET_R3(___FIXADD(___R3,___FIX(1L)))
   ___SET_R4(___VECTORLENGTH(___R1))
   ___IF(___NOT(___FIXLT(___FIX(1L),___R4)))
   ___GOTO(___L25___hamt_23_hamt_2a__2d_fold)
   ___END_IF
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R0)
   ___SET_STK(3,___R3)
   ___SET_STK(8,___STK(0))
   ___SET_R1(___VECTORREF(___R1,___FIX(1L)))
   ___SET_R0(___LBL(14))
   ___ADJFP(8)
   ___POLL(13)
___DEF_SLBL(13,___L13___hamt_23_hamt_2a__2d_fold)
   ___GOTO(___L18___hamt_23_hamt_2a__2d_fold)
___DEF_SLBL(14,___L14___hamt_23_hamt_2a__2d_fold)
   ___SET_R3(___STK(-4))
   ___SET_R2(___FIX(2L))
   ___SET_R0(___STK(-5))
   ___ADJFP(-6)
   ___POLL(15)
___DEF_SLBL(15,___L15___hamt_23_hamt_2a__2d_fold)
   ___GOTO(___L17___hamt_23_hamt_2a__2d_fold)
___DEF_GLBL(___L25___hamt_23_hamt_2a__2d_fold)
   ___SET_R1(___R2)
   ___ADJFP(-1)
   ___JUMPRET(___R0)
___DEF_SLBL(16,___L16___hamt_23_hamt_2a__2d_fold)
   ___SET_R2(___FIXADD(___STK(-4),___FIX(1L)))
   ___SET_R3(___VECTORLENGTH(___STK(-6)))
   ___IF(___FIXLT(___R2,___R3))
   ___GOTO(___L27___hamt_23_hamt_2a__2d_fold)
   ___END_IF
___DEF_GLBL(___L26___hamt_23_hamt_2a__2d_fold)
   ___ADJFP(-8)
   ___JUMPRET(___STK(3))
___DEF_GLBL(___L27___hamt_23_hamt_2a__2d_fold)
   ___SET_STK(-4,___R2)
   ___SET_STK(1,___STK(-7))
   ___SET_R2(___VECTORREF(___STK(-6),___R2))
   ___SET_STK(-2,___R1)
   ___SET_R1(___R2)
   ___SET_R3(___STK(-3))
   ___SET_R2(___STK(-2))
   ___SET_R0(___LBL(11))
   ___ADJFP(1)
   ___IF(___FIXEQ(___R3,___FIX(7L)))
   ___GOTO(___L19___hamt_23_hamt_2a__2d_fold)
   ___END_IF
   ___GOTO(___L23___hamt_23_hamt_2a__2d_fold)
___DEF_GLBL(___L28___hamt_23_hamt_2a__2d_fold)
   ___ADJFP(-2)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2a__2d_for_2d_each
#undef ___PH_LBL0
#define ___PH_LBL0 96
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L1___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L2___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L3___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L4___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L5___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L6___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L7___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L8___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L9___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L10___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L11___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L12___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L13___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_P_HLBL(___L14___hamt_23_hamt_2a__2d_for_2d_each)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2a__2d_for_2d_each)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(2))
   ___SET_R3(___FIX(0L))
   ___SET_R0(___LBL(14))
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2a__2d_for_2d_each)
   ___GOTO(___L16___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_SLBL(2,___L2___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_R3(___STK(-3))
   ___SET_R2(___FIXADD(___STK(-4),___FIX(1L)))
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_GLBL(___L15___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_R4(___VECTORLENGTH(___R1))
   ___IF(___NOT(___FIXLT(___R2,___R4)))
   ___GOTO(___L24___hamt_23_hamt_2a__2d_for_2d_each)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R2(___VECTORREF(___R1,___R2))
   ___SET_R1(___STK(0))
   ___SET_R0(___LBL(9))
   ___ADJFP(7)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_GLBL(___L16___hamt_23_hamt_2a__2d_for_2d_each)
   ___IF(___NOT(___FIXEQ(___R3,___FIX(7L))))
   ___GOTO(___L19___hamt_23_hamt_2a__2d_for_2d_each)
   ___END_IF
___DEF_GLBL(___L17___hamt_23_hamt_2a__2d_for_2d_each)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_GLBL(___L18___hamt_23_hamt_2a__2d_for_2d_each)
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L20___hamt_23_hamt_2a__2d_for_2d_each)
   ___END_IF
   ___SET_R3(___CAR(___R2))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R2(___CDR(___R3))
   ___SET_R1(___CAR(___R3))
   ___ADJFP(8)
   ___POLL(6)
___DEF_SLBL(6,___L6___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_R0(___LBL(7))
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___STK(-6))
___DEF_SLBL(7,___L7___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_R2(___CDR(___STK(-5)))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(8)
___DEF_SLBL(8,___L8___hamt_23_hamt_2a__2d_for_2d_each)
   ___GOTO(___L18___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_SLBL(9,___L9___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_R1(___FIXADD(___STK(-4),___FIX(1L)))
   ___SET_R2(___VECTORLENGTH(___STK(-5)))
   ___IF(___NOT(___FIXLT(___R1,___R2)))
   ___GOTO(___L21___hamt_23_hamt_2a__2d_for_2d_each)
   ___END_IF
   ___SET_STK(-4,___R1)
   ___SET_R2(___VECTORREF(___STK(-5),___R1))
   ___SET_R3(___STK(-3))
   ___SET_R1(___STK(-7))
   ___SET_R0(___LBL(2))
   ___IF(___FIXEQ(___R3,___FIX(7L)))
   ___GOTO(___L17___hamt_23_hamt_2a__2d_for_2d_each)
   ___END_IF
___DEF_GLBL(___L19___hamt_23_hamt_2a__2d_for_2d_each)
   ___IF(___PAIRP(___R2))
   ___GOTO(___L22___hamt_23_hamt_2a__2d_for_2d_each)
   ___END_IF
   ___SET_R3(___FIXADD(___R3,___FIX(1L)))
   ___SET_R4(___VECTORLENGTH(___R2))
   ___IF(___FIXLT(___FIX(1L),___R4))
   ___GOTO(___L23___hamt_23_hamt_2a__2d_for_2d_each)
   ___END_IF
___DEF_GLBL(___L20___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_R1(___FAL)
   ___JUMPRET(___R0)
___DEF_GLBL(___L21___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_R1(___FAL)
   ___ADJFP(-8)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L22___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_R3(___CDR(___R2))
   ___SET_STK(1,___R2)
   ___SET_R2(___R3)
   ___SET_R3(___CAR(___STK(1)))
   ___SET_STK(1,___R1)
   ___SET_R1(___R3)
   ___ADJFP(1)
   ___POLL(10)
___DEF_SLBL(10,___L10___hamt_23_hamt_2a__2d_for_2d_each)
   ___ADJFP(-1)
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___STK(1))
___DEF_GLBL(___L23___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R2(___VECTORREF(___R2,___FIX(1L)))
   ___SET_R0(___LBL(12))
   ___ADJFP(8)
   ___POLL(11)
___DEF_SLBL(11,___L11___hamt_23_hamt_2a__2d_for_2d_each)
   ___GOTO(___L16___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_SLBL(12,___L12___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_STK(-3,___STK(-7))
   ___SET_STK(-7,___STK(-6))
   ___SET_R3(___STK(-4))
   ___SET_R1(___STK(-5))
   ___SET_R2(___FIX(2L))
   ___SET_R0(___STK(-3))
   ___ADJFP(-7)
   ___POLL(13)
___DEF_SLBL(13,___L13___hamt_23_hamt_2a__2d_for_2d_each)
   ___GOTO(___L15___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_GLBL(___L24___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_R1(___FAL)
   ___ADJFP(-1)
   ___JUMPRET(___R0)
___DEF_SLBL(14,___L14___hamt_23_hamt_2a__2d_for_2d_each)
   ___SET_R1(___VOID)
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2a__2d__3e_list
#undef ___PH_LBL0
#define ___PH_LBL0 112
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L1___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L2___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L3___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L4___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L5___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L6___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L7___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L8___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L9___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L10___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L11___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L12___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L13___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L14___hamt_23_hamt_2a__2d__3e_list)
___DEF_P_HLBL(___L15___hamt_23_hamt_2a__2d__3e_list)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2a__2d__3e_list)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2a__2d__3e_list)
   ___SET_R3(___NUL)
   ___SET_R2(___FIX(0L))
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2a__2d__3e_list)
   ___GOTO(___L17___hamt_23_hamt_2a__2d__3e_list)
___DEF_SLBL(2,___L2___hamt_23_hamt_2a__2d__3e_list)
   ___SET_R3(___R1)
   ___SET_R2(___STK(-5))
   ___SET_R1(___FIXSUB(___STK(-3),___FIX(1L)))
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2a__2d__3e_list)
___DEF_GLBL(___L16___hamt_23_hamt_2a__2d__3e_list)
   ___IF(___NOT(___FIXGT(___R1,___FIX(0L))))
   ___GOTO(___L26___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
   ___SET_R4(___FIXSUB(___R1,___FIX(1L)))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_STK(3,___R4)
   ___SET_R1(___VECTORREF(___STK(0),___R1))
   ___SET_R0(___LBL(14))
   ___ADJFP(7)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2a__2d__3e_list)
___DEF_GLBL(___L17___hamt_23_hamt_2a__2d__3e_list)
   ___IF(___NOT(___FIXEQ(___R2,___FIX(7L))))
   ___GOTO(___L21___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
___DEF_GLBL(___L18___hamt_23_hamt_2a__2d__3e_list)
   ___SET_R2(___R3)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2a__2d__3e_list)
___DEF_GLBL(___L19___hamt_23_hamt_2a__2d__3e_list)
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L20___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
   ___SET_R3(___CAR(___R1))
   ___SET_R1(___CDR(___R1))
   ___SET_R2(___CONS(___R3,___R2))
   ___CHECK_HEAP(6,4096)
___DEF_SLBL(6,___L6___hamt_23_hamt_2a__2d__3e_list)
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L20___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
   ___SET_R3(___CAR(___R1))
   ___SET_R2(___CONS(___R3,___R2))
   ___SET_R1(___CDR(___R1))
   ___CHECK_HEAP(7,4096)
___DEF_SLBL(7,___L7___hamt_23_hamt_2a__2d__3e_list)
   ___POLL(8)
___DEF_SLBL(8,___L8___hamt_23_hamt_2a__2d__3e_list)
   ___GOTO(___L19___hamt_23_hamt_2a__2d__3e_list)
___DEF_GLBL(___L20___hamt_23_hamt_2a__2d__3e_list)
   ___SET_R1(___R2)
   ___JUMPRET(___R0)
___DEF_SLBL(9,___L9___hamt_23_hamt_2a__2d__3e_list)
   ___IF(___NOT(___FIXGT(___STK(-3),___FIX(0L))))
   ___GOTO(___L22___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
   ___SET_R2(___VECTORREF(___STK(-7),___STK(-3)))
   ___SET_STK(-4,___R1)
   ___SET_R1(___R2)
   ___SET_R3(___STK(-4))
   ___SET_R2(___STK(-5))
   ___SET_R0(___LBL(2))
   ___IF(___FIXEQ(___R2,___FIX(7L)))
   ___GOTO(___L18___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
___DEF_GLBL(___L21___hamt_23_hamt_2a__2d__3e_list)
   ___IF(___PAIRP(___R1))
   ___GOTO(___L25___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
   ___SET_R4(___VECTORLENGTH(___R1))
   ___SET_R4(___FIXSUB(___R4,___FIX(1L)))
   ___SET_R2(___FIXADD(___R2,___FIX(1L)))
   ___IF(___NOT(___FIXGT(___R4,___FIX(0L))))
   ___GOTO(___L24___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
   ___SET_STK(1,___FIXSUB(___R4,___FIX(1L)))
   ___SET_STK(2,___R0)
   ___SET_STK(3,___R1)
   ___SET_STK(4,___R2)
   ___SET_R1(___VECTORREF(___R1,___R4))
   ___SET_R0(___LBL(11))
   ___ADJFP(8)
   ___POLL(10)
___DEF_SLBL(10,___L10___hamt_23_hamt_2a__2d__3e_list)
   ___GOTO(___L17___hamt_23_hamt_2a__2d__3e_list)
___DEF_SLBL(11,___L11___hamt_23_hamt_2a__2d__3e_list)
   ___IF(___NOT(___FIXGT(___STK(-7),___FIX(0L))))
   ___GOTO(___L22___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
   ___SET_STK(-3,___STK(-7))
   ___SET_STK(-7,___STK(-5))
   ___SET_R2(___VECTORREF(___STK(-5),___STK(-3)))
   ___SET_STK(-5,___R1)
   ___SET_R1(___R2)
   ___SET_R3(___STK(-5))
   ___SET_R2(___STK(-4))
   ___SET_R0(___LBL(12))
   ___IF(___FIXEQ(___R2,___FIX(7L)))
   ___GOTO(___L18___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
   ___GOTO(___L21___hamt_23_hamt_2a__2d__3e_list)
___DEF_SLBL(12,___L12___hamt_23_hamt_2a__2d__3e_list)
   ___SET_R3(___R1)
   ___SET_R2(___STK(-4))
   ___SET_R1(___FIXSUB(___STK(-3),___FIX(1L)))
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___POLL(13)
___DEF_SLBL(13,___L13___hamt_23_hamt_2a__2d__3e_list)
   ___GOTO(___L16___hamt_23_hamt_2a__2d__3e_list)
___DEF_SLBL(14,___L14___hamt_23_hamt_2a__2d__3e_list)
   ___IF(___FIXGT(___STK(-4),___FIX(0L)))
   ___GOTO(___L23___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
___DEF_GLBL(___L22___hamt_23_hamt_2a__2d__3e_list)
   ___ADJFP(-8)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L23___hamt_23_hamt_2a__2d__3e_list)
   ___SET_R2(___FIXSUB(___STK(-4),___FIX(1L)))
   ___SET_STK(-3,___R2)
   ___SET_R2(___VECTORREF(___STK(-7),___STK(-4)))
   ___SET_STK(-4,___R1)
   ___SET_R1(___R2)
   ___SET_R3(___STK(-4))
   ___SET_R2(___STK(-5))
   ___SET_R0(___LBL(9))
   ___IF(___FIXEQ(___R2,___FIX(7L)))
   ___GOTO(___L18___hamt_23_hamt_2a__2d__3e_list)
   ___END_IF
   ___GOTO(___L21___hamt_23_hamt_2a__2d__3e_list)
___DEF_GLBL(___L24___hamt_23_hamt_2a__2d__3e_list)
   ___SET_R1(___R3)
   ___JUMPRET(___R0)
___DEF_GLBL(___L25___hamt_23_hamt_2a__2d__3e_list)
   ___SET_R1(___CONS(___R1,___R3))
   ___CHECK_HEAP(15,4096)
___DEF_SLBL(15,___L15___hamt_23_hamt_2a__2d__3e_list)
   ___JUMPRET(___R0)
___DEF_GLBL(___L26___hamt_23_hamt_2a__2d__3e_list)
   ___SET_R1(___R3)
   ___ADJFP(-1)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2a__3c__2d_reverse_2d_list
#undef ___PH_LBL0
#define ___PH_LBL0 129
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_P_HLBL(___L1___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_P_HLBL(___L2___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_P_HLBL(___L3___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_P_HLBL(___L4___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_P_HLBL(___L5___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_P_HLBL(___L6___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___SET_STK(1,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(1))
   ___SET_R3(___SUB(4))
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___GOTO(___L7___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_SLBL(2,___L2___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___SET_R3(___R1)
   ___SET_R2(___CDR(___STK(-5)))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_GLBL(___L7___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L9___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___END_IF
   ___SET_R4(___CDR(___R2))
   ___SET_R2(___CAR(___R2))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R4)
   ___SET_STK(9,___R3)
   ___SET_R3(___R1)
   ___SET_R1(___CDR(___R2))
   ___SET_STK(4,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___CAR(___STK(4)))
   ___ADJFP(9)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___SET_R0(___LBL(5))
   ___JUMPINT(___SET_NARGS(4),___PRC(17),___L___hamt_23_hamt_2a__2d_set)
___DEF_SLBL(5,___L5___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___IF(___NOT(___PAIRP(___STK(-5))))
   ___GOTO(___L8___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___END_IF
   ___SET_R2(___CDR(___STK(-5)))
   ___SET_R3(___CAR(___STK(-5)))
   ___SET_STK(-5,___R2)
   ___SET_STK(1,___R1)
   ___SET_STK(-4,___R3)
   ___SET_R3(___STK(-6))
   ___SET_R2(___CDR(___STK(-4)))
   ___SET_R1(___CAR(___STK(-4)))
   ___SET_R0(___LBL(6))
   ___ADJFP(1)
   ___JUMPINT(___SET_NARGS(4),___PRC(17),___L___hamt_23_hamt_2a__2d_set)
___DEF_SLBL(6,___L6___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___IF(___NOT(___PAIRP(___STK(-5))))
   ___GOTO(___L8___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___END_IF
   ___SET_R2(___CAR(___STK(-5)))
   ___SET_STK(1,___R1)
   ___SET_R3(___STK(-6))
   ___SET_R1(___CDR(___R2))
   ___SET_STK(-4,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___CAR(___STK(-4)))
   ___SET_R0(___LBL(2))
   ___ADJFP(1)
   ___JUMPINT(___SET_NARGS(4),___PRC(17),___L___hamt_23_hamt_2a__2d_set)
___DEF_GLBL(___L8___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L9___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
   ___SET_R1(___R3)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2a__2d_alist_2d_ref
#undef ___PH_LBL0
#define ___PH_LBL0 137
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_P_HLBL(___L1___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_P_HLBL(___L2___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_P_HLBL(___L3___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_P_HLBL(___L4___hamt_23_hamt_2a__2d_alist_2d_ref)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2a__2d_alist_2d_ref)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,3,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2a__2d_alist_2d_ref)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2a__2d_alist_2d_ref)
   ___GOTO(___L5___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_SLBL(2,___L2___hamt_23_hamt_2a__2d_alist_2d_ref)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L7___hamt_23_hamt_2a__2d_alist_2d_ref)
   ___END_IF
   ___SET_R3(___STK(-7))
   ___SET_R2(___STK(-8))
   ___SET_R1(___CDR(___STK(-9)))
   ___SET_R0(___STK(-10))
   ___ADJFP(-12)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2a__2d_alist_2d_ref)
___DEF_GLBL(___L5___hamt_23_hamt_2a__2d_alist_2d_ref)
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L6___hamt_23_hamt_2a__2d_alist_2d_ref)
   ___END_IF
   ___SET_R4(___CAR(___R1))
   ___SET_STK(1,___CAR(___R4))
   ___SET_STK(2,___R0)
   ___SET_STK(3,___R1)
   ___SET_STK(4,___R2)
   ___SET_STK(5,___R3)
   ___SET_STK(6,___R4)
   ___SET_R1(___STK(1))
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R3,___FIX(1L),___SUB(0),___FAL))
   ___ADJFP(12)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2a__2d_alist_2d_ref)
   ___SET_R0(___LBL(2))
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___R3)
___DEF_GLBL(___L6___hamt_23_hamt_2a__2d_alist_2d_ref)
   ___SET_R1(___FAL)
   ___JUMPRET(___R0)
___DEF_GLBL(___L7___hamt_23_hamt_2a__2d_alist_2d_ref)
   ___SET_R1(___STK(-6))
   ___ADJFP(-12)
   ___JUMPRET(___STK(2))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2a__2d_alist_2d_remove
#undef ___PH_LBL0
#define ___PH_LBL0 143
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_P_HLBL(___L1___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_P_HLBL(___L2___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_P_HLBL(___L3___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_P_HLBL(___L4___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_P_HLBL(___L5___hamt_23_hamt_2a__2d_alist_2d_remove)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,3,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___IF(___PAIRP(___R1))
   ___GOTO(___L6___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___END_IF
   ___GOTO(___L9___hamt_23_hamt_2a__2d_alist_2d_remove)
___DEF_SLBL(2,___L2___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L7___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___END_IF
   ___SET_R1(___CDR(___STK(-9)))
   ___SET_STK(-11,___R1)
   ___SET_R3(___STK(-7))
   ___SET_R2(___STK(-8))
   ___SET_R0(___LBL(4))
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L9___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___END_IF
___DEF_GLBL(___L6___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___SET_R4(___CAR(___R1))
   ___SET_STK(1,___CAR(___R4))
   ___SET_STK(2,___R0)
   ___SET_STK(3,___R1)
   ___SET_STK(4,___R2)
   ___SET_STK(5,___R3)
   ___SET_STK(6,___R4)
   ___SET_R1(___STK(1))
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R3,___FIX(1L),___SUB(0),___FAL))
   ___ADJFP(12)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___SET_R0(___LBL(2))
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___R3)
___DEF_GLBL(___L7___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___SET_R1(___CDR(___STK(-9)))
   ___ADJFP(-12)
   ___JUMPRET(___STK(2))
___DEF_SLBL(4,___L4___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___IF(___NOT(___EQP(___STK(-11),___R1)))
   ___GOTO(___L8___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___END_IF
   ___SET_R1(___STK(-9))
   ___ADJFP(-12)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L8___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___SET_R1(___CONS(___STK(-6),___R1))
   ___ADJFP(-10)
   ___CHECK_HEAP(5,4096)
___DEF_SLBL(5,___L5___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___ADJFP(-2)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L9___hamt_23_hamt_2a__2d_alist_2d_remove)
   ___SET_R1(___NUL)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_empty_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 150
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_empty_3f_)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_empty_3f_)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_empty_3f_)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_empty_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_empty_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_empty_3f_)
   ___IF(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137))
   ___GOTO(___L4___hamt_23_hamt_2d_empty_3f_)
   ___END_IF
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___FIX(1L))
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_empty_3f_)
   ___SET_NARGS(3) ___GOTO(___L2___hamt_23_hamt_2d_empty_3f_)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_empty_3f_)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(2,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_empty_3f_)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___DEF_GLBL(___L4___hamt_23_hamt_2d_empty_3f_)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(4L),___SUB(0),___FAL))
   ___SET_R1(___BOOLEAN(___FIXZEROP(___R1)))
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 155
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___hamt_23_hamt_3f_)
   ___SET_R1(___BOOLEAN(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_make_2d_hamt
#undef ___PH_LBL0
#define ___PH_LBL0 157
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L1___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L2___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L3___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L4___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L5___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L6___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L7___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L8___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L9___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L10___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L11___hamt_23_make_2d_hamt)
___DEF_P_HLBL(___L12___hamt_23_make_2d_hamt)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_make_2d_hamt)
   ___IF_NARGS_EQ(0,___SET_R1(___ABSENT) ___SET_R2(___ABSENT))
   ___GET_KEY(0,0,0,2,___SUB(5))
___DEF_GLBL(___L___hamt_23_make_2d_hamt)
   ___IF(___NOT(___EQP(___R1,___ABSENT)))
   ___GOTO(___L16___hamt_23_make_2d_hamt)
   ___END_IF
   ___SET_STK(1,___R1)
   ___SET_R1(___R2)
   ___SET_R3(___FIX(0L))
   ___SET_R2(___PRM__23__23_equal_3f_)
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_make_2d_hamt)
   ___GOTO(___L13___hamt_23_make_2d_hamt)
___DEF_SLBL(2,___L2___hamt_23_make_2d_hamt)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-5))
   ___SET_R3(___FIX(2L))
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_make_2d_hamt)
___DEF_GLBL(___L13___hamt_23_make_2d_hamt)
   ___IF(___NOT(___EQP(___R1,___ABSENT)))
   ___GOTO(___L14___hamt_23_make_2d_hamt)
   ___END_IF
   ___SET_STK(0,___R0)
   ___SET_STK(1,___R2)
   ___SET_R1(___R2)
   ___ADJFP(7)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_make_2d_hamt)
   ___SET_R0(___LBL(5))
   ___JUMPINT(___SET_NARGS(1),___PRC(319),___L___hamt_23_test_2d_procedure_2d__3e_hash)
___DEF_SLBL(5,___L5___hamt_23_make_2d_hamt)
   ___BEGIN_ALLOC_STRUCTURE(5UL)
   ___ADD_STRUCTURE_ELEM(0,___SUB(0))
   ___ADD_STRUCTURE_ELEM(1,___STK(-6))
   ___ADD_STRUCTURE_ELEM(2,___R1)
   ___ADD_STRUCTURE_ELEM(3,___SUB(4))
   ___ADD_STRUCTURE_ELEM(4,___FIX(0L))
   ___END_ALLOC_STRUCTURE(5)
   ___SET_R1(___GET_STRUCTURE(5))
   ___ADJFP(-7)
   ___CHECK_HEAP(6,4096)
___DEF_SLBL(6,___L6___hamt_23_make_2d_hamt)
   ___ADJFP(-1)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L14___hamt_23_make_2d_hamt)
   ___SET_R3(___FIXADD(___R3,___FIX(2L)))
   ___IF(___PROCEDUREP(___R1))
   ___GOTO(___L15___hamt_23_make_2d_hamt)
   ___END_IF
   ___BEGIN_ALLOC_LIST(5UL,___R1)
   ___ADD_LIST_ELEM(1,___KEY_hash)
   ___ADD_LIST_ELEM(2,___STK(0))
   ___ADD_LIST_ELEM(3,___KEY_test)
   ___ADD_LIST_ELEM(4,___LBL(0))
   ___END_ALLOC_LIST(5)
   ___SET_R2(___GET_LIST(5))
   ___SET_R1(___R3)
   ___CHECK_HEAP(7,4096)
___DEF_SLBL(7,___L7___hamt_23_make_2d_hamt)
   ___POLL(8)
___DEF_SLBL(8,___L8___hamt_23_make_2d_hamt)
   ___ADJFP(-1)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),41,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L15___hamt_23_make_2d_hamt)
   ___BEGIN_ALLOC_STRUCTURE(5UL)
   ___ADD_STRUCTURE_ELEM(0,___SUB(0))
   ___ADD_STRUCTURE_ELEM(1,___R2)
   ___ADD_STRUCTURE_ELEM(2,___R1)
   ___ADD_STRUCTURE_ELEM(3,___SUB(4))
   ___ADD_STRUCTURE_ELEM(4,___FIX(0L))
   ___END_ALLOC_STRUCTURE(5)
   ___SET_R1(___GET_STRUCTURE(5))
   ___ADJFP(-1)
   ___CHECK_HEAP(9,4096)
___DEF_SLBL(9,___L9___hamt_23_make_2d_hamt)
   ___JUMPRET(___R0)
___DEF_GLBL(___L16___hamt_23_make_2d_hamt)
   ___IF(___NOT(___PROCEDUREP(___R1)))
   ___GOTO(___L17___hamt_23_make_2d_hamt)
   ___END_IF
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R0)
   ___SET_STK(3,___R2)
   ___ADJFP(8)
   ___POLL(10)
___DEF_SLBL(10,___L10___hamt_23_make_2d_hamt)
   ___SET_R0(___LBL(2))
   ___JUMPINT(___SET_NARGS(1),___PRC(317),___L___hamt_23_test_2d_procedure_2d__3e_test)
___DEF_GLBL(___L17___hamt_23_make_2d_hamt)
   ___BEGIN_ALLOC_LIST(5UL,___R2)
   ___ADD_LIST_ELEM(1,___KEY_hash)
   ___ADD_LIST_ELEM(2,___R1)
   ___ADD_LIST_ELEM(3,___KEY_test)
   ___ADD_LIST_ELEM(4,___LBL(0))
   ___END_ALLOC_LIST(5)
   ___SET_R2(___GET_LIST(5))
   ___SET_R1(___FIX(2L))
   ___CHECK_HEAP(11,4096)
___DEF_SLBL(11,___L11___hamt_23_make_2d_hamt)
   ___POLL(12)
___DEF_SLBL(12,___L12___hamt_23_make_2d_hamt)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),41,___G__23__23_fail_2d_check_2d_procedure)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_length
#undef ___PH_LBL0
#define ___PH_LBL0 171
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_length)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_length)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_length)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_length)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_length)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_length)
   ___IF(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137))
   ___GOTO(___L4___hamt_23_hamt_2d_length)
   ___END_IF
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___FIX(1L))
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_length)
   ___SET_NARGS(3) ___GOTO(___L2___hamt_23_hamt_2d_length)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_length)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(2,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_length)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___DEF_GLBL(___L4___hamt_23_hamt_2d_length)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(4L),___SUB(0),___FAL))
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_ref
#undef ___PH_LBL0
#define ___PH_LBL0 176
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_ref)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_ref)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_ref)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_ref)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_ref)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_ref)
___DEF_P_HLBL(___L6___hamt_23_hamt_2d_ref)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_ref)
   ___IF_NARGS_EQ(2,___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,2,1,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_ref)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L9___hamt_23_hamt_2d_ref)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R3(___R1)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_ref)
   ___SET_R0(___LBL(2))
   ___JUMPINT(___SET_NARGS(3),___PRC(8),___L___hamt_23_hamt_2a__2d_ref)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_ref)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L8___hamt_23_hamt_2d_ref)
   ___END_IF
   ___IF(___NOT(___EQP(___STK(-4),___ABSENT)))
   ___GOTO(___L7___hamt_23_hamt_2d_ref)
   ___END_IF
   ___SET_R3(___STK(-5))
   ___SET_R2(___STK(-6))
   ___SET_R1(___LBL(0))
   ___SET_R0(___STK(-7))
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_ref)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),45,___G__23__23_raise_2d_unbound_2d_key_2d_exception)
___DEF_GLBL(___L7___hamt_23_hamt_2d_ref)
   ___SET_R1(___STK(-4))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L8___hamt_23_hamt_2d_ref)
   ___SET_R1(___CDR(___R1))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L9___hamt_23_hamt_2d_ref)
   ___SET_STK(1,___FIX(1L))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_ref)
   ___SET_NARGS(5) ___GOTO(___L5___hamt_23_hamt_2d_ref)
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_ref)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(5,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(6)
___DEF_SLBL(6,___L6___hamt_23_hamt_2d_ref)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_set
#undef ___PH_LBL0
#define ___PH_LBL0 184
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_set)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_set)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_set)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_set)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_set)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_set)
___DEF_P_HLBL(___L6___hamt_23_hamt_2d_set)
___DEF_P_HLBL(___L7___hamt_23_hamt_2d_set)
___DEF_P_HLBL(___L8___hamt_23_hamt_2d_set)
___DEF_P_HLBL(___L9___hamt_23_hamt_2d_set)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_set)
   ___IF_NARGS_EQ(2,___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,2,1,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_set)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L12___hamt_23_hamt_2d_set)
   ___END_IF
   ___IF(___NOT(___EQP(___R3,___ABSENT)))
   ___GOTO(___L11___hamt_23_hamt_2d_set)
   ___END_IF
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R3)
   ___SET_R3(___R1)
   ___SET_R1(___STK(3))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_set)
   ___SET_R0(___LBL(2))
   ___JUMPINT(___SET_NARGS(3),___PRC(48),___L___hamt_23_hamt_2a__2d_remove)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_set)
   ___IF(___NOT(___EQP(___STK(-5),___R1)))
   ___GOTO(___L10___hamt_23_hamt_2d_set)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L10___hamt_23_hamt_2d_set)
   ___SET_R2(___UNCHECKEDSTRUCTUREREF(___STK(-6),___FIX(4L),___SUB(0),___FAL))
   ___SET_R2(___FIXSUB(___R2,___FIX(1L)))
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___STK(-6),___FIX(2L),___SUB(0),___FAL))
   ___SET_R4(___UNCHECKEDSTRUCTUREREF(___STK(-6),___FIX(1L),___SUB(0),___FAL))
   ___BEGIN_ALLOC_STRUCTURE(5UL)
   ___ADD_STRUCTURE_ELEM(0,___SUB(0))
   ___ADD_STRUCTURE_ELEM(1,___R4)
   ___ADD_STRUCTURE_ELEM(2,___R3)
   ___ADD_STRUCTURE_ELEM(3,___R1)
   ___ADD_STRUCTURE_ELEM(4,___R2)
   ___END_ALLOC_STRUCTURE(5)
   ___SET_R1(___GET_STRUCTURE(5))
   ___ADJFP(-7)
   ___CHECK_HEAP(3,4096)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_set)
   ___ADJFP(-1)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L11___hamt_23_hamt_2d_set)
   ___SET_R4(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(4L),___SUB(0),___FAL))
   ___SET_STK(1,___UNCHECKEDSTRUCTUREREF(___R1,___FIX(2L),___SUB(0),___FAL))
   ___SET_STK(2,___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___SUB(0),___FAL))
   ___BEGIN_ALLOC_STRUCTURE(5UL)
   ___ADD_STRUCTURE_ELEM(0,___SUB(0))
   ___ADD_STRUCTURE_ELEM(1,___STK(2))
   ___ADD_STRUCTURE_ELEM(2,___STK(1))
   ___ADD_STRUCTURE_ELEM(3,___FAL)
   ___ADD_STRUCTURE_ELEM(4,___R4)
   ___END_ALLOC_STRUCTURE(5)
   ___SET_R4(___GET_STRUCTURE(5))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R4)
   ___SET_STK(9,___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___SET_STK(3,___R3)
   ___SET_R3(___R4)
   ___SET_STK(4,___R2)
   ___SET_R2(___STK(3))
   ___SET_R1(___STK(4))
   ___ADJFP(9)
   ___CHECK_HEAP(4,4096)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_set)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_set)
   ___SET_R0(___LBL(6))
   ___JUMPINT(___SET_NARGS(4),___PRC(17),___L___hamt_23_hamt_2a__2d_set)
___DEF_SLBL(6,___L6___hamt_23_hamt_2d_set)
   ___UNCHECKEDSTRUCTURESET(___STK(-6),___R1,___FIX(3L),___SUB(0),___FAL)
   ___SET_R1(___STK(-6))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L12___hamt_23_hamt_2d_set)
   ___SET_STK(1,___FIX(1L))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(7)
___DEF_SLBL(7,___L7___hamt_23_hamt_2d_set)
   ___SET_NARGS(5) ___GOTO(___L8___hamt_23_hamt_2d_set)
___DEF_SLBL(8,___L8___hamt_23_hamt_2d_set)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(8,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(9)
___DEF_SLBL(9,___L9___hamt_23_hamt_2d_set)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_merge
#undef ___PH_LBL0
#define ___PH_LBL0 195
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_merge)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_merge)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_merge)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_merge)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_merge)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_merge)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_merge)
   ___IF_NARGS_EQ(2,___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,2,1,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_merge)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L7___hamt_23_hamt_2d_merge)
   ___END_IF
   ___IF(___NOT(___STRUCTUREDIOP(___R2,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L6___hamt_23_hamt_2d_merge)
   ___END_IF
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_merge)
   ___SET_NARGS(3) ___JUMPINT(___NOTHING,___PRC(202),___L0___hamt_23_hamt_2d_merge_2d_aux)
___DEF_GLBL(___L6___hamt_23_hamt_2d_merge)
   ___SET_STK(1,___FIX(2L))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(2)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_merge)
   ___GOTO(___L8___hamt_23_hamt_2d_merge)
___DEF_GLBL(___L7___hamt_23_hamt_2d_merge)
   ___SET_STK(1,___FIX(1L))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_merge)
___DEF_GLBL(___L8___hamt_23_hamt_2d_merge)
   ___SET_NARGS(5) ___GOTO(___L4___hamt_23_hamt_2d_merge)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_merge)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(4,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_merge)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_merge_2d_aux
#undef ___PH_LBL0
#define ___PH_LBL0 202
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L6___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L7___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L8___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L9___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L10___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L11___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L12___hamt_23_hamt_2d_merge_2d_aux)
___DEF_P_HLBL(___L13___hamt_23_hamt_2d_merge_2d_aux)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_merge_2d_aux)
   ___IF_NARGS_EQ(2,___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,2,1,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_merge_2d_aux)
   ___IF(___EQP(___R3,___ABSENT))
   ___GOTO(___L14___hamt_23_hamt_2d_merge_2d_aux)
   ___END_IF
   ___IF(___NOT(___EQP(___R3,___FAL)))
   ___GOTO(___L19___hamt_23_hamt_2d_merge_2d_aux)
   ___END_IF
___DEF_GLBL(___L14___hamt_23_hamt_2d_merge_2d_aux)
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R2,___FIX(1L),___SUB(0),___FAL))
   ___SET_R4(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___SUB(0),___FAL))
   ___IF(___NOT(___EQP(___R4,___R3)))
   ___GOTO(___L17___hamt_23_hamt_2d_merge_2d_aux)
   ___END_IF
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R2,___FIX(2L),___SUB(0),___FAL))
   ___SET_R4(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(2L),___SUB(0),___FAL))
   ___IF(___NOT(___EQP(___R4,___R3)))
   ___GOTO(___L17___hamt_23_hamt_2d_merge_2d_aux)
   ___END_IF
   ___SET_STK(1,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(1))
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_merge_2d_aux)
___DEF_GLBL(___L15___hamt_23_hamt_2d_merge_2d_aux)
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(4L),___SUB(0),___FAL))
   ___SET_R4(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(2L),___SUB(0),___FAL))
   ___SET_STK(1,___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___SUB(0),___FAL))
   ___BEGIN_ALLOC_STRUCTURE(5UL)
   ___ADD_STRUCTURE_ELEM(0,___SUB(0))
   ___ADD_STRUCTURE_ELEM(1,___STK(1))
   ___ADD_STRUCTURE_ELEM(2,___R4)
   ___ADD_STRUCTURE_ELEM(3,___FAL)
   ___ADD_STRUCTURE_ELEM(4,___R3)
   ___END_ALLOC_STRUCTURE(5)
   ___SET_R3(___GET_STRUCTURE(5))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R3)
   ___SET_STK(3,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(3),5)
   ___ADD_CLO_ELEM(0,___R3)
   ___END_SETUP_CLO(1)
   ___SET_STK(4,___R2)
   ___SET_R2(___STK(3))
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___STK(4),___FIX(3L),___SUB(0),___FAL))
   ___ADJFP(8)
   ___CHECK_HEAP(2,4096)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_merge_2d_aux)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_merge_2d_aux)
___DEF_GLBL(___L16___hamt_23_hamt_2d_merge_2d_aux)
   ___SET_R0(___LBL(4))
   ___JUMPINT(___SET_NARGS(3),___PRC(78),___L___hamt_23_hamt_2a__2d_fold)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_merge_2d_aux)
   ___UNCHECKEDSTRUCTURESET(___STK(-6),___R1,___FIX(3L),___SUB(0),___FAL)
   ___SET_R1(___STK(-6))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_merge_2d_aux)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(5,3,0,0)
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R3)
   ___SET_R3(___CLO(___R4,1))
   ___SET_STK(3,___R2)
   ___SET_R2(___STK(2))
   ___SET_R1(___STK(3))
   ___ADJFP(3)
   ___POLL(6)
___DEF_SLBL(6,___L6___hamt_23_hamt_2d_merge_2d_aux)
   ___ADJFP(-2)
   ___JUMPINT(___SET_NARGS(4),___PRC(17),___L___hamt_23_hamt_2a__2d_set)
___DEF_GLBL(___L17___hamt_23_hamt_2d_merge_2d_aux)
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(4L),___SUB(0),___FAL))
   ___SET_R4(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(2L),___SUB(0),___FAL))
   ___SET_STK(1,___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___SUB(0),___FAL))
   ___BEGIN_ALLOC_STRUCTURE(5UL)
   ___ADD_STRUCTURE_ELEM(0,___SUB(0))
   ___ADD_STRUCTURE_ELEM(1,___STK(1))
   ___ADD_STRUCTURE_ELEM(2,___R4)
   ___ADD_STRUCTURE_ELEM(3,___FAL)
   ___ADD_STRUCTURE_ELEM(4,___R3)
   ___END_ALLOC_STRUCTURE(5)
   ___SET_R3(___GET_STRUCTURE(5))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R3)
   ___SET_STK(3,___ALLOC_CLO(2UL))
   ___BEGIN_SETUP_CLO(2,___STK(3),9)
   ___ADD_CLO_ELEM(0,___R1)
   ___ADD_CLO_ELEM(1,___R3)
   ___END_SETUP_CLO(2)
   ___SET_STK(4,___R2)
   ___SET_R2(___STK(3))
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___STK(4),___FIX(3L),___SUB(0),___FAL))
   ___ADJFP(8)
   ___CHECK_HEAP(7,4096)
___DEF_SLBL(7,___L7___hamt_23_hamt_2d_merge_2d_aux)
   ___POLL(8)
___DEF_SLBL(8,___L8___hamt_23_hamt_2d_merge_2d_aux)
   ___GOTO(___L16___hamt_23_hamt_2d_merge_2d_aux)
___DEF_SLBL(9,___L9___hamt_23_hamt_2d_merge_2d_aux)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(9,3,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_STK(5,___R4)
   ___SET_R3(___CLO(___R4,1))
   ___ADJFP(8)
   ___POLL(10)
___DEF_SLBL(10,___L10___hamt_23_hamt_2d_merge_2d_aux)
   ___SET_R0(___LBL(11))
   ___JUMPINT(___SET_NARGS(3),___PRC(8),___L___hamt_23_hamt_2a__2d_ref)
___DEF_SLBL(11,___L11___hamt_23_hamt_2d_merge_2d_aux)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L18___hamt_23_hamt_2d_merge_2d_aux)
   ___END_IF
   ___SET_STK(-2,___STK(-7))
   ___SET_STK(-7,___STK(-6))
   ___SET_R3(___CLO(___STK(-3),2))
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-2))
   ___POLL(12)
___DEF_SLBL(12,___L12___hamt_23_hamt_2d_merge_2d_aux)
   ___ADJFP(-7)
   ___JUMPINT(___SET_NARGS(4),___PRC(17),___L___hamt_23_hamt_2a__2d_set)
___DEF_GLBL(___L18___hamt_23_hamt_2d_merge_2d_aux)
   ___SET_R1(___STK(-6))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L19___hamt_23_hamt_2d_merge_2d_aux)
   ___POLL(13)
___DEF_SLBL(13,___L13___hamt_23_hamt_2d_merge_2d_aux)
   ___GOTO(___L15___hamt_23_hamt_2d_merge_2d_aux)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_search
#undef ___PH_LBL0
#define ___PH_LBL0 217
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_search)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_search)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_search)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_search)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_search)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_search)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_search)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_search)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L7___hamt_23_hamt_2d_search)
   ___END_IF
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L6___hamt_23_hamt_2d_search)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_search)
   ___JUMPINT(___SET_NARGS(2),___PRC(63),___L___hamt_23_hamt_2a__2d_search)
___DEF_GLBL(___L6___hamt_23_hamt_2d_search)
   ___SET_STK(1,___FIX(2L))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(2)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_search)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),41,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L7___hamt_23_hamt_2d_search)
   ___SET_STK(1,___FIX(1L))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_search)
   ___SET_NARGS(4) ___GOTO(___L4___hamt_23_hamt_2d_search)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_search)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(4,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_search)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_fold
#undef ___PH_LBL0
#define ___PH_LBL0 224
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_fold)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_fold)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_fold)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_fold)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_fold)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_fold)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_fold)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,3,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_fold)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L7___hamt_23_hamt_2d_fold)
   ___END_IF
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L6___hamt_23_hamt_2d_fold)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_fold)
   ___JUMPINT(___SET_NARGS(3),___PRC(78),___L___hamt_23_hamt_2a__2d_fold)
___DEF_GLBL(___L6___hamt_23_hamt_2d_fold)
   ___SET_STK(1,___FIX(2L))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(2)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_fold)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),41,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L7___hamt_23_hamt_2d_fold)
   ___SET_STK(1,___FIX(1L))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_fold)
   ___SET_NARGS(5) ___GOTO(___L4___hamt_23_hamt_2d_fold)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_fold)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(4,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_fold)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_for_2d_each
#undef ___PH_LBL0
#define ___PH_LBL0 231
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_for_2d_each)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_for_2d_each)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_for_2d_each)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_for_2d_each)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_for_2d_each)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_for_2d_each)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_for_2d_each)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_for_2d_each)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L7___hamt_23_hamt_2d_for_2d_each)
   ___END_IF
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L6___hamt_23_hamt_2d_for_2d_each)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_for_2d_each)
   ___JUMPINT(___SET_NARGS(2),___PRC(96),___L___hamt_23_hamt_2a__2d_for_2d_each)
___DEF_GLBL(___L6___hamt_23_hamt_2d_for_2d_each)
   ___SET_STK(1,___FIX(2L))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(2)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_for_2d_each)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),41,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L7___hamt_23_hamt_2d_for_2d_each)
   ___SET_STK(1,___FIX(1L))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_for_2d_each)
   ___SET_NARGS(4) ___GOTO(___L4___hamt_23_hamt_2d_for_2d_each)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_for_2d_each)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(4,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_for_2d_each)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_map
#undef ___PH_LBL0
#define ___PH_LBL0 238
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_map)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_map)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_map)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_map)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_map)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_map)
___DEF_P_HLBL(___L6___hamt_23_hamt_2d_map)
___DEF_P_HLBL(___L7___hamt_23_hamt_2d_map)
___DEF_P_HLBL(___L8___hamt_23_hamt_2d_map)
___DEF_P_HLBL(___L9___hamt_23_hamt_2d_map)
___DEF_P_HLBL(___L10___hamt_23_hamt_2d_map)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_map)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_map)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L12___hamt_23_hamt_2d_map)
   ___END_IF
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L11___hamt_23_hamt_2d_map)
   ___END_IF
   ___SET_STK(1,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(1),3)
   ___ADD_CLO_ELEM(0,___R2)
   ___END_SETUP_CLO(1)
   ___SET_R2(___STK(1))
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___SET_R3(___NUL)
   ___ADJFP(1)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_map)
   ___POLL(2)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_map)
   ___ADJFP(-1)
   ___JUMPINT(___SET_NARGS(3),___PRC(78),___L___hamt_23_hamt_2a__2d_fold)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_map)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(3,3,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R2(___R3)
   ___SET_R1(___STK(3))
   ___ADJFP(8)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_map)
   ___SET_R0(___LBL(5))
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___CLO(___R4,1))
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_map)
   ___SET_R1(___CONS(___R1,___STK(-6)))
   ___ADJFP(-7)
   ___CHECK_HEAP(6,4096)
___DEF_SLBL(6,___L6___hamt_23_hamt_2d_map)
   ___ADJFP(-1)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L11___hamt_23_hamt_2d_map)
   ___SET_STK(1,___FIX(2L))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(7)
___DEF_SLBL(7,___L7___hamt_23_hamt_2d_map)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),41,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L12___hamt_23_hamt_2d_map)
   ___SET_STK(1,___FIX(1L))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(8)
___DEF_SLBL(8,___L8___hamt_23_hamt_2d_map)
   ___SET_NARGS(4) ___GOTO(___L9___hamt_23_hamt_2d_map)
___DEF_SLBL(9,___L9___hamt_23_hamt_2d_map)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(9,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(10)
___DEF_SLBL(10,___L10___hamt_23_hamt_2d_map)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d__3e_list
#undef ___PH_LBL0
#define ___PH_LBL0 250
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d__3e_list)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d__3e_list)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d__3e_list)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d__3e_list)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d__3e_list)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d__3e_list)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2d__3e_list)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L5___hamt_23_hamt_2d__3e_list)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d__3e_list)
   ___JUMPINT(___SET_NARGS(1),___PRC(112),___L___hamt_23_hamt_2a__2d__3e_list)
___DEF_GLBL(___L5___hamt_23_hamt_2d__3e_list)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___FIX(1L))
   ___POLL(2)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d__3e_list)
   ___SET_NARGS(3) ___GOTO(___L3___hamt_23_hamt_2d__3e_list)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d__3e_list)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(3,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d__3e_list)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_list_2d__3e_hamt
#undef ___PH_LBL0
#define ___PH_LBL0 256
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L1___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L2___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L3___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L4___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L5___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L6___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L7___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L8___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L9___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L10___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L11___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L12___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L13___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L14___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L15___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L16___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L17___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L18___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L19___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L20___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L21___hamt_23_list_2d__3e_hamt)
___DEF_P_HLBL(___L22___hamt_23_list_2d__3e_hamt)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_list_2d__3e_hamt)
   ___IF_NARGS_EQ(1,___SET_R2(___ABSENT) ___SET_R3(___ABSENT))
   ___GET_KEY(0,1,0,2,___SUB(6))
___DEF_GLBL(___L___hamt_23_list_2d__3e_hamt)
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___R3)
   ___SET_R3(___NUL)
   ___ADJFP(2)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_list_2d__3e_hamt)
   ___GOTO(___L24___hamt_23_list_2d__3e_hamt)
___DEF_GLBL(___L23___hamt_23_list_2d__3e_hamt)
   ___SET_R4(___CAR(___R2))
   ___IF(___NOT(___PAIRP(___R4)))
   ___GOTO(___L32___hamt_23_list_2d__3e_hamt)
   ___END_IF
   ___SET_R3(___CONS(___R4,___R3))
   ___SET_R2(___CDR(___R2))
   ___CHECK_HEAP(2,4096)
___DEF_SLBL(2,___L2___hamt_23_list_2d__3e_hamt)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_list_2d__3e_hamt)
___DEF_GLBL(___L24___hamt_23_list_2d__3e_hamt)
   ___IF(___PAIRP(___R2))
   ___GOTO(___L23___hamt_23_list_2d__3e_hamt)
   ___END_IF
   ___IF(___NOT(___NULLP(___R2)))
   ___GOTO(___L31___hamt_23_list_2d__3e_hamt)
   ___END_IF
   ___IF(___NOT(___EQP(___STK(0),___ABSENT)))
   ___GOTO(___L29___hamt_23_list_2d__3e_hamt)
   ___END_IF
   ___SET_STK(1,___R1)
   ___SET_R1(___R3)
   ___SET_R3(___FIX(1L))
   ___SET_R2(___PRM__23__23_equal_3f_)
   ___ADJFP(1)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_list_2d__3e_hamt)
___DEF_GLBL(___L25___hamt_23_list_2d__3e_hamt)
   ___IF(___NOT(___EQP(___STK(0),___ABSENT)))
   ___GOTO(___L26___hamt_23_list_2d__3e_hamt)
   ___END_IF
   ___SET_STK(-2,___R0)
   ___SET_STK(-1,___R1)
   ___SET_STK(0,___R2)
   ___SET_R1(___R2)
   ___ADJFP(5)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_list_2d__3e_hamt)
   ___SET_R0(___LBL(6))
   ___JUMPINT(___SET_NARGS(1),___PRC(319),___L___hamt_23_test_2d_procedure_2d__3e_hash)
___DEF_SLBL(6,___L6___hamt_23_list_2d__3e_hamt)
   ___SET_R3(___R1)
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(7)
___DEF_SLBL(7,___L7___hamt_23_list_2d__3e_hamt)
   ___GOTO(___L27___hamt_23_list_2d__3e_hamt)
___DEF_GLBL(___L26___hamt_23_list_2d__3e_hamt)
   ___SET_R3(___FIXADD(___R3,___FIX(2L)))
   ___IF(___NOT(___PROCEDUREP(___STK(0))))
   ___GOTO(___L28___hamt_23_list_2d__3e_hamt)
   ___END_IF
   ___SET_R3(___STK(0))
   ___ADJFP(-3)
   ___POLL(8)
___DEF_SLBL(8,___L8___hamt_23_list_2d__3e_hamt)
___DEF_GLBL(___L27___hamt_23_list_2d__3e_hamt)
   ___BEGIN_ALLOC_STRUCTURE(5UL)
   ___ADD_STRUCTURE_ELEM(0,___SUB(0))
   ___ADD_STRUCTURE_ELEM(1,___R2)
   ___ADD_STRUCTURE_ELEM(2,___R3)
   ___ADD_STRUCTURE_ELEM(3,___SUB(4))
   ___ADD_STRUCTURE_ELEM(4,___FIX(0L))
   ___END_ALLOC_STRUCTURE(5)
   ___SET_R2(___GET_STRUCTURE(5))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___ADJFP(8)
   ___CHECK_HEAP(9,4096)
___DEF_SLBL(9,___L9___hamt_23_list_2d__3e_hamt)
   ___POLL(10)
___DEF_SLBL(10,___L10___hamt_23_list_2d__3e_hamt)
   ___SET_R0(___LBL(11))
   ___JUMPINT(___SET_NARGS(2),___PRC(129),___L___hamt_23_hamt_2a__3c__2d_reverse_2d_list)
___DEF_SLBL(11,___L11___hamt_23_list_2d__3e_hamt)
   ___UNCHECKEDSTRUCTURESET(___STK(-6),___R1,___FIX(3L),___SUB(0),___FAL)
   ___SET_R1(___STK(-6))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L28___hamt_23_list_2d__3e_hamt)
   ___BEGIN_ALLOC_LIST(5UL,___STK(0))
   ___ADD_LIST_ELEM(1,___KEY_hash)
   ___ADD_LIST_ELEM(2,___STK(-1))
   ___ADD_LIST_ELEM(3,___KEY_test)
   ___ADD_LIST_ELEM(4,___LBL(0))
   ___END_ALLOC_LIST(5)
   ___SET_R2(___GET_LIST(5))
   ___SET_STK(-1,___R3)
   ___SET_R3(___STK(-2))
   ___SET_R1(___STK(-1))
   ___CHECK_HEAP(12,4096)
___DEF_SLBL(12,___L12___hamt_23_list_2d__3e_hamt)
   ___POLL(13)
___DEF_SLBL(13,___L13___hamt_23_list_2d__3e_hamt)
   ___ADJFP(-3)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),41,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L29___hamt_23_list_2d__3e_hamt)
   ___IF(___NOT(___PROCEDUREP(___STK(0))))
   ___GOTO(___L30___hamt_23_list_2d__3e_hamt)
   ___END_IF
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R0)
   ___SET_STK(3,___R3)
   ___SET_R1(___STK(0))
   ___ADJFP(6)
   ___POLL(14)
___DEF_SLBL(14,___L14___hamt_23_list_2d__3e_hamt)
   ___SET_R0(___LBL(15))
   ___JUMPINT(___SET_NARGS(1),___PRC(317),___L___hamt_23_test_2d_procedure_2d__3e_test)
___DEF_SLBL(15,___L15___hamt_23_list_2d__3e_hamt)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-3))
   ___SET_R3(___FIX(3L))
   ___SET_R0(___STK(-4))
   ___ADJFP(-5)
   ___POLL(16)
___DEF_SLBL(16,___L16___hamt_23_list_2d__3e_hamt)
   ___GOTO(___L25___hamt_23_list_2d__3e_hamt)
___DEF_GLBL(___L30___hamt_23_list_2d__3e_hamt)
   ___BEGIN_ALLOC_LIST(5UL,___R1)
   ___ADD_LIST_ELEM(1,___KEY_hash)
   ___ADD_LIST_ELEM(2,___STK(0))
   ___ADD_LIST_ELEM(3,___KEY_test)
   ___ADD_LIST_ELEM(4,___LBL(0))
   ___END_ALLOC_LIST(5)
   ___SET_R2(___GET_LIST(5))
   ___SET_R3(___STK(-1))
   ___SET_R1(___FIX(3L))
   ___CHECK_HEAP(17,4096)
___DEF_SLBL(17,___L17___hamt_23_list_2d__3e_hamt)
   ___POLL(18)
___DEF_SLBL(18,___L18___hamt_23_list_2d__3e_hamt)
   ___ADJFP(-2)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),41,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L31___hamt_23_list_2d__3e_hamt)
   ___BEGIN_ALLOC_LIST(5UL,___R1)
   ___ADD_LIST_ELEM(1,___KEY_hash)
   ___ADD_LIST_ELEM(2,___STK(0))
   ___ADD_LIST_ELEM(3,___KEY_test)
   ___ADD_LIST_ELEM(4,___LBL(0))
   ___END_ALLOC_LIST(5)
   ___SET_R2(___GET_LIST(5))
   ___SET_R3(___STK(-1))
   ___SET_R1(___FIX(1L))
   ___CHECK_HEAP(19,4096)
___DEF_SLBL(19,___L19___hamt_23_list_2d__3e_hamt)
   ___POLL(20)
___DEF_SLBL(20,___L20___hamt_23_list_2d__3e_hamt)
   ___ADJFP(-2)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),42,___G__23__23_fail_2d_check_2d_proper_2d_list)
___DEF_GLBL(___L32___hamt_23_list_2d__3e_hamt)
   ___BEGIN_ALLOC_LIST(5UL,___R1)
   ___ADD_LIST_ELEM(1,___KEY_hash)
   ___ADD_LIST_ELEM(2,___STK(0))
   ___ADD_LIST_ELEM(3,___KEY_test)
   ___ADD_LIST_ELEM(4,___LBL(0))
   ___END_ALLOC_LIST(5)
   ___SET_R2(___GET_LIST(5))
   ___SET_R3(___STK(-1))
   ___SET_R1(___FIX(1L))
   ___CHECK_HEAP(21,4096)
___DEF_SLBL(21,___L21___hamt_23_list_2d__3e_hamt)
   ___POLL(22)
___DEF_SLBL(22,___L22___hamt_23_list_2d__3e_hamt)
   ___ADJFP(-2)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),40,___G__23__23_fail_2d_check_2d_pair_2d_list)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_keys
#undef ___PH_LBL0
#define ___PH_LBL0 280
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_keys)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_keys)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_keys)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_keys)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_keys)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_keys)
___DEF_P_HLBL(___L6___hamt_23_hamt_2d_keys)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_keys)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_keys)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L7___hamt_23_hamt_2d_keys)
   ___END_IF
   ___SET_R2(___LBL(2))
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___SET_R3(___NUL)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_keys)
   ___JUMPINT(___SET_NARGS(3),___PRC(78),___L___hamt_23_hamt_2a__2d_fold)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_keys)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(2,3,0,0)
   ___SET_R1(___CONS(___R2,___R1))
   ___CHECK_HEAP(3,4096)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_keys)
   ___JUMPRET(___R0)
___DEF_GLBL(___L7___hamt_23_hamt_2d_keys)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___FIX(1L))
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_keys)
   ___SET_NARGS(3) ___GOTO(___L5___hamt_23_hamt_2d_keys)
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_keys)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(5,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(6)
___DEF_SLBL(6,___L6___hamt_23_hamt_2d_keys)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_values
#undef ___PH_LBL0
#define ___PH_LBL0 288
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_values)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_values)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_values)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_values)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_values)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_values)
___DEF_P_HLBL(___L6___hamt_23_hamt_2d_values)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_values)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_values)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L7___hamt_23_hamt_2d_values)
   ___END_IF
   ___SET_R2(___LBL(2))
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___SET_R3(___NUL)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_values)
   ___JUMPINT(___SET_NARGS(3),___PRC(78),___L___hamt_23_hamt_2a__2d_fold)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_values)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(2,3,0,0)
   ___SET_R1(___CONS(___R3,___R1))
   ___CHECK_HEAP(3,4096)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_values)
   ___JUMPRET(___R0)
___DEF_GLBL(___L7___hamt_23_hamt_2d_values)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___FIX(1L))
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_values)
   ___SET_NARGS(3) ___GOTO(___L5___hamt_23_hamt_2d_values)
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_values)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(5,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(6)
___DEF_SLBL(6,___L6___hamt_23_hamt_2d_values)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_has_2d_key_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 296
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_has_2d_key_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_has_2d_key_3f_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_has_2d_key_3f_)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L7___hamt_23_hamt_2d_has_2d_key_3f_)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_R3(___R1)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_has_2d_key_3f_)
   ___SET_R0(___LBL(2))
   ___JUMPINT(___SET_NARGS(3),___PRC(8),___L___hamt_23_hamt_2a__2d_ref)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_has_2d_key_3f_)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L6___hamt_23_hamt_2d_has_2d_key_3f_)
   ___END_IF
   ___SET_R1(___TRU)
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L6___hamt_23_hamt_2d_has_2d_key_3f_)
   ___SET_R1(___FAL)
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L7___hamt_23_hamt_2d_has_2d_key_3f_)
   ___SET_STK(1,___FIX(1L))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_has_2d_key_3f_)
   ___SET_NARGS(4) ___GOTO(___L4___hamt_23_hamt_2d_has_2d_key_3f_)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_has_2d_key_3f_)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(4,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_has_2d_key_3f_)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hamt_2d_has_2d_value_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 303
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L1___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L2___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L3___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L4___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L5___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L6___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L7___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L8___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L9___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L10___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L11___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_P_HLBL(___L12___hamt_23_hamt_2d_has_2d_value_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hamt_2d_has_2d_value_3f_)
   ___IF_NARGS_EQ(2,___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,2,1,0)
___DEF_GLBL(___L___hamt_23_hamt_2d_has_2d_value_3f_)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137)))
   ___GOTO(___L16___hamt_23_hamt_2d_has_2d_value_3f_)
   ___END_IF
   ___IF(___NOT(___EQP(___R3,___ABSENT)))
   ___GOTO(___L13___hamt_23_hamt_2d_has_2d_value_3f_)
   ___END_IF
   ___SET_STK(1,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(1),8)
   ___ADD_CLO_ELEM(0,___R2)
   ___END_SETUP_CLO(1)
   ___SET_R2(___STK(1))
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___ADJFP(1)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1___hamt_23_hamt_2d_has_2d_value_3f_)
   ___POLL(2)
___DEF_SLBL(2,___L2___hamt_23_hamt_2d_has_2d_value_3f_)
   ___GOTO(___L14___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_GLBL(___L13___hamt_23_hamt_2d_has_2d_value_3f_)
   ___IF(___NOT(___PROCEDUREP(___R3)))
   ___GOTO(___L15___hamt_23_hamt_2d_has_2d_value_3f_)
   ___END_IF
   ___SET_STK(1,___ALLOC_CLO(2UL))
   ___BEGIN_SETUP_CLO(2,___STK(1),6)
   ___ADD_CLO_ELEM(0,___R3)
   ___ADD_CLO_ELEM(1,___R2)
   ___END_SETUP_CLO(2)
   ___SET_R2(___STK(1))
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___ADJFP(1)
   ___CHECK_HEAP(3,4096)
___DEF_SLBL(3,___L3___hamt_23_hamt_2d_has_2d_value_3f_)
   ___POLL(4)
___DEF_SLBL(4,___L4___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_GLBL(___L14___hamt_23_hamt_2d_has_2d_value_3f_)
   ___ADJFP(-1)
   ___JUMPINT(___SET_NARGS(2),___PRC(63),___L___hamt_23_hamt_2a__2d_search)
___DEF_GLBL(___L15___hamt_23_hamt_2d_has_2d_value_3f_)
   ___SET_STK(1,___FIX(3L))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(5)
___DEF_SLBL(5,___L5___hamt_23_hamt_2d_has_2d_value_3f_)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),41,___G__23__23_fail_2d_check_2d_procedure)
___DEF_SLBL(6,___L6___hamt_23_hamt_2d_has_2d_value_3f_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(6,2,0,0)
   ___SET_R1(___CLO(___R4,2))
   ___POLL(7)
___DEF_SLBL(7,___L7___hamt_23_hamt_2d_has_2d_value_3f_)
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___CLO(___R4,1))
___DEF_SLBL(8,___L8___hamt_23_hamt_2d_has_2d_value_3f_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(8,2,0,0)
   ___SET_R1(___CLO(___R4,1))
   ___POLL(9)
___DEF_SLBL(9,___L9___hamt_23_hamt_2d_has_2d_value_3f_)
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23_equal_3f_)
___DEF_GLBL(___L16___hamt_23_hamt_2d_has_2d_value_3f_)
   ___SET_STK(1,___FIX(1L))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(10)
___DEF_SLBL(10,___L10___hamt_23_hamt_2d_has_2d_value_3f_)
   ___SET_NARGS(5) ___GOTO(___L11___hamt_23_hamt_2d_has_2d_value_3f_)
___DEF_SLBL(11,___L11___hamt_23_hamt_2d_has_2d_value_3f_)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(11,2,0,0)
   ___SET_STK(1,___R1)
   ___SET_R1(___SUB(0))
   ___ADJFP(1)
   ___POLL(12)
___DEF_SLBL(12,___L12___hamt_23_hamt_2d_has_2d_value_3f_)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),44,___G__23__23_raise_2d_type_2d_exception)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_test_2d_procedure_2d__3e_test
#undef ___PH_LBL0
#define ___PH_LBL0 317
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_test_2d_procedure_2d__3e_test)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_test_2d_procedure_2d__3e_test)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___hamt_23_test_2d_procedure_2d__3e_test)
   ___IF(___EQP(___R1,___PRM__23__23_eq_3f_))
   ___GOTO(___L4___hamt_23_test_2d_procedure_2d__3e_test)
   ___END_IF
   ___IF(___EQP(___R1,___PRM_eq_3f_))
   ___GOTO(___L4___hamt_23_test_2d_procedure_2d__3e_test)
   ___END_IF
   ___IF(___EQP(___R1,___PRM__23__23_eqv_3f_))
   ___GOTO(___L3___hamt_23_test_2d_procedure_2d__3e_test)
   ___END_IF
   ___IF(___EQP(___R1,___PRM_eqv_3f_))
   ___GOTO(___L3___hamt_23_test_2d_procedure_2d__3e_test)
   ___END_IF
   ___IF(___EQP(___R1,___PRM__23__23_equal_3f_))
   ___GOTO(___L1___hamt_23_test_2d_procedure_2d__3e_test)
   ___END_IF
   ___IF(___NOT(___EQP(___R1,___PRM_equal_3f_)))
   ___GOTO(___L2___hamt_23_test_2d_procedure_2d__3e_test)
   ___END_IF
___DEF_GLBL(___L1___hamt_23_test_2d_procedure_2d__3e_test)
   ___SET_R1(___PRM__23__23_equal_3f_)
   ___JUMPRET(___R0)
___DEF_GLBL(___L2___hamt_23_test_2d_procedure_2d__3e_test)
   ___JUMPRET(___R0)
___DEF_GLBL(___L3___hamt_23_test_2d_procedure_2d__3e_test)
   ___SET_R1(___PRM__23__23_eqv_3f_)
   ___JUMPRET(___R0)
___DEF_GLBL(___L4___hamt_23_test_2d_procedure_2d__3e_test)
   ___SET_R1(___PRM__23__23_eq_3f_)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_test_2d_procedure_2d__3e_hash
#undef ___PH_LBL0
#define ___PH_LBL0 319
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_test_2d_procedure_2d__3e_hash)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_test_2d_procedure_2d__3e_hash)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___hamt_23_test_2d_procedure_2d__3e_hash)
   ___IF(___EQP(___R1,___PRM__23__23_eq_3f_))
   ___GOTO(___L7___hamt_23_test_2d_procedure_2d__3e_hash)
   ___END_IF
   ___IF(___EQP(___R1,___PRM_eq_3f_))
   ___GOTO(___L7___hamt_23_test_2d_procedure_2d__3e_hash)
   ___END_IF
   ___IF(___EQP(___R1,___PRM__23__23_eqv_3f_))
   ___GOTO(___L6___hamt_23_test_2d_procedure_2d__3e_hash)
   ___END_IF
   ___IF(___EQP(___R1,___PRM_eqv_3f_))
   ___GOTO(___L6___hamt_23_test_2d_procedure_2d__3e_hash)
   ___END_IF
   ___IF(___EQP(___R1,___PRM__23__23_equal_3f_))
   ___GOTO(___L5___hamt_23_test_2d_procedure_2d__3e_hash)
   ___END_IF
   ___IF(___EQP(___R1,___PRM_equal_3f_))
   ___GOTO(___L5___hamt_23_test_2d_procedure_2d__3e_hash)
   ___END_IF
   ___IF(___EQP(___R1,___PRM__23__23_string_3d__3f_))
   ___GOTO(___L4___hamt_23_test_2d_procedure_2d__3e_hash)
   ___END_IF
   ___IF(___EQP(___R1,___PRM_string_3d__3f_))
   ___GOTO(___L3___hamt_23_test_2d_procedure_2d__3e_hash)
   ___END_IF
   ___IF(___EQP(___R1,___PRM__23__23_string_2d_ci_3d__3f_))
   ___GOTO(___L2___hamt_23_test_2d_procedure_2d__3e_hash)
   ___END_IF
   ___IF(___NOT(___EQP(___R1,___PRM_string_2d_ci_3d__3f_)))
   ___GOTO(___L1___hamt_23_test_2d_procedure_2d__3e_hash)
   ___END_IF
   ___SET_R1(___GLO_string_2d_ci_3d__3f__2d_hash)
   ___JUMPRET(___R0)
___DEF_GLBL(___L1___hamt_23_test_2d_procedure_2d__3e_hash)
   ___SET_R1(___GLO__23__23_generic_2d_hash)
   ___JUMPRET(___R0)
___DEF_GLBL(___L2___hamt_23_test_2d_procedure_2d__3e_hash)
   ___SET_R1(___GLO__23__23_string_2d_ci_3d__3f__2d_hash)
   ___JUMPRET(___R0)
___DEF_GLBL(___L3___hamt_23_test_2d_procedure_2d__3e_hash)
   ___SET_R1(___GLO_string_3d__3f__2d_hash)
   ___JUMPRET(___R0)
___DEF_GLBL(___L4___hamt_23_test_2d_procedure_2d__3e_hash)
   ___SET_R1(___GLO__23__23_string_3d__3f__2d_hash)
   ___JUMPRET(___R0)
___DEF_GLBL(___L5___hamt_23_test_2d_procedure_2d__3e_hash)
   ___SET_R1(___GLO__23__23_equal_3f__2d_hash)
   ___JUMPRET(___R0)
___DEF_GLBL(___L6___hamt_23_test_2d_procedure_2d__3e_hash)
   ___SET_R1(___GLO__23__23_eqv_3f__2d_hash)
   ___JUMPRET(___R0)
___DEF_GLBL(___L7___hamt_23_test_2d_procedure_2d__3e_hash)
   ___SET_R1(___GLO__23__23_eq_3f__2d_hash)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___hamt_23_hash_2d_procedure_2d__3e_hash
#undef ___PH_LBL0
#define ___PH_LBL0 321
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___hamt_23_hash_2d_procedure_2d__3e_hash)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___hamt_23_hash_2d_procedure_2d__3e_hash)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___hamt_23_hash_2d_procedure_2d__3e_hash)
   ___IF(___EQP(___R1,___GLO_eq_3f__2d_hash))
   ___GOTO(___L3___hamt_23_hash_2d_procedure_2d__3e_hash)
   ___END_IF
   ___IF(___EQP(___R1,___GLO_eqv_3f__2d_hash))
   ___GOTO(___L2___hamt_23_hash_2d_procedure_2d__3e_hash)
   ___END_IF
   ___IF(___NOT(___EQP(___R1,___GLO_equal_3f__2d_hash)))
   ___GOTO(___L1___hamt_23_hash_2d_procedure_2d__3e_hash)
   ___END_IF
   ___SET_R1(___GLO__23__23_equal_3f__2d_hash)
   ___JUMPRET(___R0)
___DEF_GLBL(___L1___hamt_23_hash_2d_procedure_2d__3e_hash)
   ___JUMPRET(___R0)
___DEF_GLBL(___L2___hamt_23_hash_2d_procedure_2d__3e_hash)
   ___SET_R1(___GLO__23__23_eqv_3f__2d_hash)
   ___JUMPRET(___R0)
___DEF_GLBL(___L3___hamt_23_hash_2d_procedure_2d__3e_hash)
   ___SET_R1(___GLO__23__23_eq_3f__2d_hash)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H___hamt_23_,___REF_SYM(3,___S___hamt_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H___hamt_23_,0,-1)
,___DEF_LBL_INTRO(___H___hamt_23_fail_2d_check_2d_hamt,___REF_SYM(4,___S___hamt_23_fail_2d_check_2d_hamt),___REF_FAL,2,0)
,___DEF_LBL_PROC(___H___hamt_23_fail_2d_check_2d_hamt,3,-1)
,___DEF_LBL_RET(___H___hamt_23_fail_2d_check_2d_hamt,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_make_2d_hamt_2a_,___REF_SYM(34,___S___hamt_23_make_2d_hamt_2a_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H___hamt_23_make_2d_hamt_2a_,0,-1)
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2a__2d_ref,___REF_SYM(10,___S___hamt_23_hamt_2a__2d_ref),___REF_FAL,8,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2a__2d_ref,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_ref,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_ref,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_ref,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_ref,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_ref,___IFD(___RETI,3,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_ref,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_ref,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2a__2d_set,___REF_SYM(13,___S___hamt_23_hamt_2a__2d_set),___REF_FAL,30,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2a__2d_set,4,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,8,1,0x3f1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___OFD(___RETI,9,1,0x3f10fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,4,4,0x3f7L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___OFD(___RETI,12,1,0x3f03fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETN,9,1,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,2,1,0x3f2L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___OFD(___RETI,12,4,0x3f1f7L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETN,9,4,0x1f7L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,5,4,0x3f10L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETN,9,4,0x175L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETN,9,4,0x175L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___OFD(___RETI,9,1,0x3f106L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,2,4,0x3f2L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETN,5,1,0x6L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,2,1,0x3f2L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETN,5,4,0x15L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,5,4,0x3f10L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETN,5,1,0xfL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_set,___IFD(___RETI,2,1,0x3f2L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2a__2d_remove,___REF_SYM(11,___S___hamt_23_hamt_2a__2d_remove),___REF_FAL,14,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2a__2d_remove,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___OFD(___RETI,13,2,0x3f103dL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___OFD(___RETI,12,1,0x3f03fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___IFD(___RETN,9,1,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___IFD(___RETI,2,1,0x3f2L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___IFD(___RETI,2,1,0x3f2L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___OFD(___RETI,12,4,0x3f07dL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___IFD(___RETN,9,4,0x7dL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___IFD(___RETI,5,4,0x3f10L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___IFD(___RETN,9,2,0x3dL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_remove,___IFD(___RETI,3,2,0x3f4L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2a__2d_search,___REF_SYM(12,___S___hamt_23_hamt_2a__2d_search),___REF_FAL,14,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2a__2d_search,2,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETI,8,1,0x3f1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_search,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2a__2d_fold,___REF_SYM(8,___S___hamt_23_hamt_2a__2d_fold),___REF_FAL,17,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2a__2d_fold,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETN,5,2,0x1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___OFD(___RETI,9,2,0x3f11fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETN,5,2,0x1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___OFD(___RETI,9,2,0x3f10fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETN,5,2,0xfL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_fold,___IFD(___RETN,5,2,0x1fL))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2a__2d_for_2d_each,___REF_SYM(9,___S___hamt_23_hamt_2a__2d_for_2d_each),___REF_FAL,15,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2a__2d_for_2d_each,2,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETI,8,1,0x3f1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_for_2d_each,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2a__2d__3e_list,___REF_SYM(5,___S___hamt_23_hamt_2a__2d__3e_list),___REF_FAL,16,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2a__2d__3e_list,1,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETN,5,1,0x17L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETI,8,1,0x3f0fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETN,5,1,0x17L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETI,8,1,0x3f0fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETN,5,1,0xfL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETN,5,1,0x1bL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETN,5,1,0xfL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d__3e_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2a__3c__2d_reverse_2d_list,___REF_SYM(14,___S___hamt_23_hamt_2a__3c__2d_reverse_2d_list),___REF_FAL,7,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2a__3c__2d_reverse_2d_list,2,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__3c__2d_reverse_2d_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__3c__2d_reverse_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__3c__2d_reverse_2d_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__3c__2d_reverse_2d_list,___OFD(___RETI,9,0,0x3f107L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__3c__2d_reverse_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__3c__2d_reverse_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2a__2d_alist_2d_ref,___REF_SYM(6,___S___hamt_23_hamt_2a__2d_alist_2d_ref),___REF_FAL,5,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2a__2d_alist_2d_ref,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_alist_2d_ref,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_alist_2d_ref,___IFD(___RETN,9,1,0x3eL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_alist_2d_ref,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_alist_2d_ref,___OFD(___RETI,12,1,0x3f03eL))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2a__2d_alist_2d_remove,___REF_SYM(7,___S___hamt_23_hamt_2a__2d_alist_2d_remove),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2a__2d_alist_2d_remove,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_alist_2d_remove,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_alist_2d_remove,___IFD(___RETN,9,1,0x3eL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_alist_2d_remove,___OFD(___RETI,12,1,0x3f03eL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_alist_2d_remove,___IFD(___RETN,9,1,0x27L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2a__2d_alist_2d_remove,___IFD(___RETI,2,1,0x3f2L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_empty_3f_,___REF_SYM(16,___S___hamt_23_hamt_2d_empty_3f_),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_empty_3f_,1,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_empty_3f_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_empty_3f_,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_empty_3f_,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_3f_,___REF_SYM(30,___S___hamt_23_hamt_3f_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_3f_,1,-1)
,___DEF_LBL_INTRO(___H___hamt_23_make_2d_hamt,___REF_SYM(33,___S___hamt_23_make_2d_hamt),___REF_FAL,13,0)
,___DEF_LBL_PROC(___H___hamt_23_make_2d_hamt,2,-1)
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETN,5,1,0x7L))
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETI,1,0,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETI,8,1,0x3f07L))
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_make_2d_hamt,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_length,___REF_SYM(22,___S___hamt_23_hamt_2d_length),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_length,1,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_length,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_length,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_length,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_ref,___REF_SYM(26,___S___hamt_23_hamt_2d_ref),___REF_FAL,7,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_ref,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_ref,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_ref,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_ref,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_ref,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_ref,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_ref,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_set,___REF_SYM(28,___S___hamt_23_hamt_2d_set),___REF_FAL,10,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_set,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_set,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_set,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_set,___IFD(___RETI,1,0,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_set,___OFD(___RETI,9,0,0x3f103L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_set,___OFD(___RETI,9,0,0x3f103L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_set,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_set,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_set,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_set,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_merge,___REF_SYM(24,___S___hamt_23_hamt_2d_merge),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_merge,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_merge,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_merge_2d_aux,___REF_SYM(25,___S___hamt_23_hamt_2d_merge_2d_aux),___REF_FAL,14,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_merge_2d_aux,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge_2d_aux,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge_2d_aux,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge_2d_aux,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge_2d_aux,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_merge_2d_aux,3,1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge_2d_aux,___IFD(___RETI,3,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge_2d_aux,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge_2d_aux,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_merge_2d_aux,3,2)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge_2d_aux,___IFD(___RETI,8,0,0x3f1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge_2d_aux,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge_2d_aux,___IFD(___RETI,8,8,0x3f01L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_merge_2d_aux,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_search,___REF_SYM(27,___S___hamt_23_hamt_2d_search),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_search,2,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_search,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_search,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_search,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_search,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_search,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_fold,___REF_SYM(17,___S___hamt_23_hamt_2d_fold),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_fold,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_fold,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_fold,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_fold,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_fold,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_fold,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_for_2d_each,___REF_SYM(18,___S___hamt_23_hamt_2d_for_2d_each),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_for_2d_each,2,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_for_2d_each,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_for_2d_each,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_for_2d_each,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_for_2d_each,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_for_2d_each,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_map,___REF_SYM(23,___S___hamt_23_hamt_2d_map),___REF_FAL,11,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_map,2,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_map,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_map,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_map,3,1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_map,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_map,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_map,___IFD(___RETI,1,0,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_map,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_map,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_map,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_map,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d__3e_list,___REF_SYM(15,___S___hamt_23_hamt_2d__3e_list),___REF_FAL,5,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d__3e_list,1,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d__3e_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d__3e_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d__3e_list,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d__3e_list,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_list_2d__3e_hamt,___REF_SYM(32,___S___hamt_23_list_2d__3e_hamt),___REF_FAL,23,0)
,___DEF_LBL_PROC(___H___hamt_23_list_2d__3e_hamt,3,-1)
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,3,4,0x3f7L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,3,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,3,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,8,3,0x3f1fL))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETN,5,3,0x1fL))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,3,4,0x3f7L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,2,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,2,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,2,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,2,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,2,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_list_2d__3e_hamt,___IFD(___RETI,2,4,0x3f0L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_keys,___REF_SYM(21,___S___hamt_23_hamt_2d_keys),___REF_FAL,7,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_keys,1,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_keys,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_keys,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_keys,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_keys,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_keys,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_keys,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_values,___REF_SYM(29,___S___hamt_23_hamt_2d_values),___REF_FAL,7,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_values,1,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_values,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_values,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_values,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_values,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_values,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_values,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_has_2d_key_3f_,___REF_SYM(19,___S___hamt_23_hamt_2d_has_2d_key_3f_),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_has_2d_key_3f_,2,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_key_3f_,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_key_3f_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_key_3f_,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_has_2d_key_3f_,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_key_3f_,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_hamt_2d_has_2d_value_3f_,___REF_SYM(20,___S___hamt_23_hamt_2d_has_2d_value_3f_),___REF_FAL,13,0)
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_has_2d_value_3f_,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_value_3f_,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_value_3f_,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_value_3f_,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_value_3f_,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_value_3f_,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_has_2d_value_3f_,2,2)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_value_3f_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_has_2d_value_3f_,2,1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_value_3f_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_value_3f_,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_PROC(___H___hamt_23_hamt_2d_has_2d_value_3f_,3,-1)
,___DEF_LBL_RET(___H___hamt_23_hamt_2d_has_2d_value_3f_,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H___hamt_23_test_2d_procedure_2d__3e_test,___REF_SYM(36,___S___hamt_23_test_2d_procedure_2d__3e_test),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H___hamt_23_test_2d_procedure_2d__3e_test,1,-1)
,___DEF_LBL_INTRO(___H___hamt_23_test_2d_procedure_2d__3e_hash,___REF_SYM(35,___S___hamt_23_test_2d_procedure_2d__3e_hash),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H___hamt_23_test_2d_procedure_2d__3e_hash,1,-1)
,___DEF_LBL_INTRO(___H___hamt_23_hash_2d_procedure_2d__3e_hash,___REF_SYM(31,___S___hamt_23_hash_2d_procedure_2d__3e_hash),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H___hamt_23_hash_2d_procedure_2d__3e_hash,1,-1)
___END_LBL

___BEGIN_OFD
 ___DEF_OFD(___RETI,9,1)
               ___GCMAP1(0x3f10fL)
,___DEF_OFD(___RETI,12,1)
               ___GCMAP1(0x3f03fL)
,___DEF_OFD(___RETI,12,4)
               ___GCMAP1(0x3f1f7L)
,___DEF_OFD(___RETI,9,1)
               ___GCMAP1(0x3f106L)
,___DEF_OFD(___RETI,13,2)
               ___GCMAP1(0x3f103dL)
,___DEF_OFD(___RETI,12,1)
               ___GCMAP1(0x3f03fL)
,___DEF_OFD(___RETI,12,4)
               ___GCMAP1(0x3f07dL)
,___DEF_OFD(___RETI,9,2)
               ___GCMAP1(0x3f11fL)
,___DEF_OFD(___RETI,9,2)
               ___GCMAP1(0x3f10fL)
,___DEF_OFD(___RETI,9,0)
               ___GCMAP1(0x3f107L)
,___DEF_OFD(___RETI,12,1)
               ___GCMAP1(0x3f03eL)
,___DEF_OFD(___RETI,12,1)
               ___GCMAP1(0x3f03eL)
,___DEF_OFD(___RETI,9,0)
               ___GCMAP1(0x3f103L)
,___DEF_OFD(___RETI,9,0)
               ___GCMAP1(0x3f103L)
___END_OFD

___BEGIN_MOD_PRM
___DEF_MOD_PRM(0,___G___hamt_23_,1)
___DEF_MOD_PRM(1,___G___hamt_23_fail_2d_check_2d_hamt,3)
___DEF_MOD_PRM(31,___G___hamt_23_make_2d_hamt_2a_,6)
___DEF_MOD_PRM(7,___G___hamt_23_hamt_2a__2d_ref,8)
___DEF_MOD_PRM(10,___G___hamt_23_hamt_2a__2d_set,17)
___DEF_MOD_PRM(8,___G___hamt_23_hamt_2a__2d_remove,48)
___DEF_MOD_PRM(9,___G___hamt_23_hamt_2a__2d_search,63)
___DEF_MOD_PRM(5,___G___hamt_23_hamt_2a__2d_fold,78)
___DEF_MOD_PRM(6,___G___hamt_23_hamt_2a__2d_for_2d_each,96)
___DEF_MOD_PRM(2,___G___hamt_23_hamt_2a__2d__3e_list,112)
___DEF_MOD_PRM(11,___G___hamt_23_hamt_2a__3c__2d_reverse_2d_list,129)
___DEF_MOD_PRM(3,___G___hamt_23_hamt_2a__2d_alist_2d_ref,137)
___DEF_MOD_PRM(4,___G___hamt_23_hamt_2a__2d_alist_2d_remove,143)
___DEF_MOD_PRM(13,___G___hamt_23_hamt_2d_empty_3f_,150)
___DEF_MOD_PRM(27,___G___hamt_23_hamt_3f_,155)
___DEF_MOD_PRM(30,___G___hamt_23_make_2d_hamt,157)
___DEF_MOD_PRM(19,___G___hamt_23_hamt_2d_length,171)
___DEF_MOD_PRM(23,___G___hamt_23_hamt_2d_ref,176)
___DEF_MOD_PRM(25,___G___hamt_23_hamt_2d_set,184)
___DEF_MOD_PRM(21,___G___hamt_23_hamt_2d_merge,195)
___DEF_MOD_PRM(22,___G___hamt_23_hamt_2d_merge_2d_aux,202)
___DEF_MOD_PRM(24,___G___hamt_23_hamt_2d_search,217)
___DEF_MOD_PRM(14,___G___hamt_23_hamt_2d_fold,224)
___DEF_MOD_PRM(15,___G___hamt_23_hamt_2d_for_2d_each,231)
___DEF_MOD_PRM(20,___G___hamt_23_hamt_2d_map,238)
___DEF_MOD_PRM(12,___G___hamt_23_hamt_2d__3e_list,250)
___DEF_MOD_PRM(29,___G___hamt_23_list_2d__3e_hamt,256)
___DEF_MOD_PRM(18,___G___hamt_23_hamt_2d_keys,280)
___DEF_MOD_PRM(26,___G___hamt_23_hamt_2d_values,288)
___DEF_MOD_PRM(16,___G___hamt_23_hamt_2d_has_2d_key_3f_,296)
___DEF_MOD_PRM(17,___G___hamt_23_hamt_2d_has_2d_value_3f_,303)
___DEF_MOD_PRM(33,___G___hamt_23_test_2d_procedure_2d__3e_test,317)
___DEF_MOD_PRM(32,___G___hamt_23_test_2d_procedure_2d__3e_hash,319)
___DEF_MOD_PRM(28,___G___hamt_23_hash_2d_procedure_2d__3e_hash,321)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(0,___G___hamt_23_,1)
___DEF_MOD_GLO(1,___G___hamt_23_fail_2d_check_2d_hamt,3)
___DEF_MOD_GLO(31,___G___hamt_23_make_2d_hamt_2a_,6)
___DEF_MOD_GLO(7,___G___hamt_23_hamt_2a__2d_ref,8)
___DEF_MOD_GLO(10,___G___hamt_23_hamt_2a__2d_set,17)
___DEF_MOD_GLO(8,___G___hamt_23_hamt_2a__2d_remove,48)
___DEF_MOD_GLO(9,___G___hamt_23_hamt_2a__2d_search,63)
___DEF_MOD_GLO(5,___G___hamt_23_hamt_2a__2d_fold,78)
___DEF_MOD_GLO(6,___G___hamt_23_hamt_2a__2d_for_2d_each,96)
___DEF_MOD_GLO(2,___G___hamt_23_hamt_2a__2d__3e_list,112)
___DEF_MOD_GLO(11,___G___hamt_23_hamt_2a__3c__2d_reverse_2d_list,129)
___DEF_MOD_GLO(3,___G___hamt_23_hamt_2a__2d_alist_2d_ref,137)
___DEF_MOD_GLO(4,___G___hamt_23_hamt_2a__2d_alist_2d_remove,143)
___DEF_MOD_GLO(13,___G___hamt_23_hamt_2d_empty_3f_,150)
___DEF_MOD_GLO(27,___G___hamt_23_hamt_3f_,155)
___DEF_MOD_GLO(30,___G___hamt_23_make_2d_hamt,157)
___DEF_MOD_GLO(19,___G___hamt_23_hamt_2d_length,171)
___DEF_MOD_GLO(23,___G___hamt_23_hamt_2d_ref,176)
___DEF_MOD_GLO(25,___G___hamt_23_hamt_2d_set,184)
___DEF_MOD_GLO(21,___G___hamt_23_hamt_2d_merge,195)
___DEF_MOD_GLO(22,___G___hamt_23_hamt_2d_merge_2d_aux,202)
___DEF_MOD_GLO(24,___G___hamt_23_hamt_2d_search,217)
___DEF_MOD_GLO(14,___G___hamt_23_hamt_2d_fold,224)
___DEF_MOD_GLO(15,___G___hamt_23_hamt_2d_for_2d_each,231)
___DEF_MOD_GLO(20,___G___hamt_23_hamt_2d_map,238)
___DEF_MOD_GLO(12,___G___hamt_23_hamt_2d__3e_list,250)
___DEF_MOD_GLO(29,___G___hamt_23_list_2d__3e_hamt,256)
___DEF_MOD_GLO(18,___G___hamt_23_hamt_2d_keys,280)
___DEF_MOD_GLO(26,___G___hamt_23_hamt_2d_values,288)
___DEF_MOD_GLO(16,___G___hamt_23_hamt_2d_has_2d_key_3f_,296)
___DEF_MOD_GLO(17,___G___hamt_23_hamt_2d_has_2d_value_3f_,303)
___DEF_MOD_GLO(33,___G___hamt_23_test_2d_procedure_2d__3e_test,317)
___DEF_MOD_GLO(32,___G___hamt_23_test_2d_procedure_2d__3e_hash,319)
___DEF_MOD_GLO(28,___G___hamt_23_hash_2d_procedure_2d__3e_hash,321)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S__23__23_type_2d_4_2d_A8E85030_2d_3E96_2d_4AD5_2d_B257_2d_73B99DCBD137,"##type-4-A8E85030-3E96-4AD5-B257-73B99DCBD137")

___DEF_MOD_SYM(1,___S__23__23_type_2d_5,"##type-5")
___DEF_MOD_SYM(2,___S___hamt,"_hamt")
___DEF_MOD_SYM(3,___S___hamt_23_,"_hamt#")
___DEF_MOD_SYM(4,___S___hamt_23_fail_2d_check_2d_hamt,"_hamt#fail-check-hamt")
___DEF_MOD_SYM(5,___S___hamt_23_hamt_2a__2d__3e_list,"_hamt#hamt*->list")
___DEF_MOD_SYM(6,___S___hamt_23_hamt_2a__2d_alist_2d_ref,"_hamt#hamt*-alist-ref")
___DEF_MOD_SYM(7,___S___hamt_23_hamt_2a__2d_alist_2d_remove,"_hamt#hamt*-alist-remove")
___DEF_MOD_SYM(8,___S___hamt_23_hamt_2a__2d_fold,"_hamt#hamt*-fold")
___DEF_MOD_SYM(9,___S___hamt_23_hamt_2a__2d_for_2d_each,"_hamt#hamt*-for-each")
___DEF_MOD_SYM(10,___S___hamt_23_hamt_2a__2d_ref,"_hamt#hamt*-ref")
___DEF_MOD_SYM(11,___S___hamt_23_hamt_2a__2d_remove,"_hamt#hamt*-remove")
___DEF_MOD_SYM(12,___S___hamt_23_hamt_2a__2d_search,"_hamt#hamt*-search")
___DEF_MOD_SYM(13,___S___hamt_23_hamt_2a__2d_set,"_hamt#hamt*-set")
___DEF_MOD_SYM(14,___S___hamt_23_hamt_2a__3c__2d_reverse_2d_list,"_hamt#hamt*<-reverse-list")
___DEF_MOD_SYM(15,___S___hamt_23_hamt_2d__3e_list,"_hamt#hamt->list")
___DEF_MOD_SYM(16,___S___hamt_23_hamt_2d_empty_3f_,"_hamt#hamt-empty?")
___DEF_MOD_SYM(17,___S___hamt_23_hamt_2d_fold,"_hamt#hamt-fold")
___DEF_MOD_SYM(18,___S___hamt_23_hamt_2d_for_2d_each,"_hamt#hamt-for-each")
___DEF_MOD_SYM(19,___S___hamt_23_hamt_2d_has_2d_key_3f_,"_hamt#hamt-has-key?")
___DEF_MOD_SYM(20,___S___hamt_23_hamt_2d_has_2d_value_3f_,"_hamt#hamt-has-value?")
___DEF_MOD_SYM(21,___S___hamt_23_hamt_2d_keys,"_hamt#hamt-keys")
___DEF_MOD_SYM(22,___S___hamt_23_hamt_2d_length,"_hamt#hamt-length")
___DEF_MOD_SYM(23,___S___hamt_23_hamt_2d_map,"_hamt#hamt-map")
___DEF_MOD_SYM(24,___S___hamt_23_hamt_2d_merge,"_hamt#hamt-merge")
___DEF_MOD_SYM(25,___S___hamt_23_hamt_2d_merge_2d_aux,"_hamt#hamt-merge-aux")
___DEF_MOD_SYM(26,___S___hamt_23_hamt_2d_ref,"_hamt#hamt-ref")
___DEF_MOD_SYM(27,___S___hamt_23_hamt_2d_search,"_hamt#hamt-search")
___DEF_MOD_SYM(28,___S___hamt_23_hamt_2d_set,"_hamt#hamt-set")
___DEF_MOD_SYM(29,___S___hamt_23_hamt_2d_values,"_hamt#hamt-values")
___DEF_MOD_SYM(30,___S___hamt_23_hamt_3f_,"_hamt#hamt?")
___DEF_MOD_SYM(31,___S___hamt_23_hash_2d_procedure_2d__3e_hash,"_hamt#hash-procedure->hash")
___DEF_MOD_SYM(32,___S___hamt_23_list_2d__3e_hamt,"_hamt#list->hamt")
___DEF_MOD_SYM(33,___S___hamt_23_make_2d_hamt,"_hamt#make-hamt")
___DEF_MOD_SYM(34,___S___hamt_23_make_2d_hamt_2a_,"_hamt#make-hamt*")
___DEF_MOD_SYM(35,___S___hamt_23_test_2d_procedure_2d__3e_hash,"_hamt#test-procedure->hash")
___DEF_MOD_SYM(36,___S___hamt_23_test_2d_procedure_2d__3e_test,"_hamt#test-procedure->test")
___DEF_MOD_SYM(37,___S_fields,"fields")
___DEF_MOD_SYM(38,___S_flags,"flags")
___DEF_MOD_SYM(39,___S_hamt,"hamt")
___DEF_MOD_SYM(40,___S_hash,"hash")
___DEF_MOD_SYM(41,___S_id,"id")
___DEF_MOD_SYM(42,___S_length,"length")
___DEF_MOD_SYM(43,___S_name,"name")
___DEF_MOD_SYM(44,___S_super,"super")
___DEF_MOD_SYM(45,___S_test,"test")
___DEF_MOD_SYM(46,___S_tree,"tree")
___DEF_MOD_SYM(47,___S_type,"type")
___DEF_MOD_KEY(0,___K_hash,"hash")
___DEF_MOD_KEY(1,___K_test,"test")
___END_MOD_SYM_KEY

#endif
