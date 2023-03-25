#ifdef ___LINKER_INFO
; File: "69.c", produced by Gambit v4.9.4
(
409004
(C)
"srfi/69"
("srfi/69")
()
(("srfi/69"))
( #|*/"*/"symbols|#
"##type-5"
"##type-6-F3F63A41-2974-4D41-8B24-1744E866741D"
"bound"
"f"
"fields"
"flags"
"func"
"gcht"
"hash"
"ht"
"ht1"
"ht2"
"id"
"init"
"loads"
"name"
"proc"
"srfi/69"
"srfi/69#"
"srfi/69#alist->hash-table"
"srfi/69#hash"
"srfi/69#hash-by-identity"
"srfi/69#hash-table->alist"
"srfi/69#hash-table-copy"
"srfi/69#hash-table-delete!"
"srfi/69#hash-table-equivalence-function"
"srfi/69#hash-table-exists?"
"srfi/69#hash-table-fold"
"srfi/69#hash-table-hash-function"
"srfi/69#hash-table-keys"
"srfi/69#hash-table-merge!"
"srfi/69#hash-table-ref"
"srfi/69#hash-table-ref/default"
"srfi/69#hash-table-set!"
"srfi/69#hash-table-size"
"srfi/69#hash-table-update!"
"srfi/69#hash-table-update!/default"
"srfi/69#hash-table-values"
"srfi/69#hash-table-walk"
"srfi/69#hash-table?"
"srfi/69#make-hash-table"
"srfi/69#string-ci-hash"
"srfi/69#string-hash"
"string"
"super"
"table"
"test"
"type"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
"hash"
"test"
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"srfi/69#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"srfi/69#alist->hash-table"
"srfi/69#hash"
"srfi/69#hash-by-identity"
"srfi/69#hash-table->alist"
"srfi/69#hash-table-copy"
"srfi/69#hash-table-delete!"
"srfi/69#hash-table-equivalence-function"
"srfi/69#hash-table-exists?"
"srfi/69#hash-table-fold"
"srfi/69#hash-table-hash-function"
"srfi/69#hash-table-keys"
"srfi/69#hash-table-merge!"
"srfi/69#hash-table-ref"
"srfi/69#hash-table-ref/default"
"srfi/69#hash-table-set!"
"srfi/69#hash-table-size"
"srfi/69#hash-table-update!"
"srfi/69#hash-table-update!/default"
"srfi/69#hash-table-values"
"srfi/69#hash-table-walk"
"srfi/69#hash-table?"
"srfi/69#make-hash-table"
"srfi/69#string-ci-hash"
"srfi/69#string-hash"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##eq?"
"##eq?-hash"
"##equal?-hash"
"##fail-check-exact-integer"
"##fail-check-procedure"
"##fail-check-string"
"##fail-check-table"
"##fold"
"##list->table"
"##make-table"
"##modulo"
"##raise-range-exception"
"##raise-unbound-key-exception"
"##string-ci=?-hash"
"##string=?-hash"
"##table->list"
"##table-copy"
"##table-for-each"
"##table-length"
"##table-merge!"
"##table-ref"
"##table-set!"
"##table?"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409004
#define ___MODULE_NAME "srfi/69"
#define ___LINKER_ID ___LNK_69_2e_o1
#define ___MH_PROC ___H_srfi_2f_69
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 48
#define ___KEYCOUNT 2
#define ___GLOCOUNT 48
#define ___SUPCOUNT 25
#define ___CNSCOUNT 36
#define ___SUBCOUNT 7
#define ___LBLCOUNT 152
#define ___MODDESCR ___REF_SUB(4)
#include "gambit.h"

___NEED_SYM(___S__23__23_type_2d_5)
___NEED_SYM(___S__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)
___NEED_SYM(___S_bound)
___NEED_SYM(___S_f)
___NEED_SYM(___S_fields)
___NEED_SYM(___S_flags)
___NEED_SYM(___S_func)
___NEED_SYM(___S_gcht)
___NEED_SYM(___S_hash)
___NEED_SYM(___S_ht)
___NEED_SYM(___S_ht1)
___NEED_SYM(___S_ht2)
___NEED_SYM(___S_id)
___NEED_SYM(___S_init)
___NEED_SYM(___S_loads)
___NEED_SYM(___S_name)
___NEED_SYM(___S_proc)
___NEED_SYM(___S_srfi_2f_69)
___NEED_SYM(___S_srfi_2f_69_23_)
___NEED_SYM(___S_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___NEED_SYM(___S_srfi_2f_69_23_hash)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_by_2d_identity)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_copy)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_fold)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_keys)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_ref)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_set_21_)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_size)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_values)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_2d_walk)
___NEED_SYM(___S_srfi_2f_69_23_hash_2d_table_3f_)
___NEED_SYM(___S_srfi_2f_69_23_make_2d_hash_2d_table)
___NEED_SYM(___S_srfi_2f_69_23_string_2d_ci_2d_hash)
___NEED_SYM(___S_srfi_2f_69_23_string_2d_hash)
___NEED_SYM(___S_string)
___NEED_SYM(___S_super)
___NEED_SYM(___S_table)
___NEED_SYM(___S_test)
___NEED_SYM(___S_type)

___NEED_KEY(___K_hash)
___NEED_KEY(___K_test)

___NEED_GLO(___G__23__23_eq_3f_)
___NEED_GLO(___G__23__23_eq_3f__2d_hash)
___NEED_GLO(___G__23__23_equal_3f__2d_hash)
___NEED_GLO(___G__23__23_fail_2d_check_2d_exact_2d_integer)
___NEED_GLO(___G__23__23_fail_2d_check_2d_procedure)
___NEED_GLO(___G__23__23_fail_2d_check_2d_string)
___NEED_GLO(___G__23__23_fail_2d_check_2d_table)
___NEED_GLO(___G__23__23_fold)
___NEED_GLO(___G__23__23_list_2d__3e_table)
___NEED_GLO(___G__23__23_make_2d_table)
___NEED_GLO(___G__23__23_modulo)
___NEED_GLO(___G__23__23_raise_2d_range_2d_exception)
___NEED_GLO(___G__23__23_raise_2d_unbound_2d_key_2d_exception)
___NEED_GLO(___G__23__23_string_2d_ci_3d__3f__2d_hash)
___NEED_GLO(___G__23__23_string_3d__3f__2d_hash)
___NEED_GLO(___G__23__23_table_2d__3e_list)
___NEED_GLO(___G__23__23_table_2d_copy)
___NEED_GLO(___G__23__23_table_2d_for_2d_each)
___NEED_GLO(___G__23__23_table_2d_length)
___NEED_GLO(___G__23__23_table_2d_merge_21_)
___NEED_GLO(___G__23__23_table_2d_ref)
___NEED_GLO(___G__23__23_table_2d_set_21_)
___NEED_GLO(___G__23__23_table_3f_)
___NEED_GLO(___G_srfi_2f_69_23_)
___NEED_GLO(___G_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___NEED_GLO(___G_srfi_2f_69_23_hash)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_by_2d_identity)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_copy)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_fold)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_keys)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_ref)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_set_21_)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_size)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_values)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_2d_walk)
___NEED_GLO(___G_srfi_2f_69_23_hash_2d_table_3f_)
___NEED_GLO(___G_srfi_2f_69_23_make_2d_hash_2d_table)
___NEED_GLO(___G_srfi_2f_69_23_string_2d_ci_2d_hash)
___NEED_GLO(___G_srfi_2f_69_23_string_2d_hash)

___BEGIN_SYM
___DEF_SYM(0,___S__23__23_type_2d_5,"##type-5")
___DEF_SYM(1,___S__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D,"##type-6-F3F63A41-2974-4D41-8B24-1744E866741D")

___DEF_SYM(2,___S_bound,"bound")
___DEF_SYM(3,___S_f,"f")
___DEF_SYM(4,___S_fields,"fields")
___DEF_SYM(5,___S_flags,"flags")
___DEF_SYM(6,___S_func,"func")
___DEF_SYM(7,___S_gcht,"gcht")
___DEF_SYM(8,___S_hash,"hash")
___DEF_SYM(9,___S_ht,"ht")
___DEF_SYM(10,___S_ht1,"ht1")
___DEF_SYM(11,___S_ht2,"ht2")
___DEF_SYM(12,___S_id,"id")
___DEF_SYM(13,___S_init,"init")
___DEF_SYM(14,___S_loads,"loads")
___DEF_SYM(15,___S_name,"name")
___DEF_SYM(16,___S_proc,"proc")
___DEF_SYM(17,___S_srfi_2f_69,"srfi/69")
___DEF_SYM(18,___S_srfi_2f_69_23_,"srfi/69#")
___DEF_SYM(19,___S_srfi_2f_69_23_alist_2d__3e_hash_2d_table,"srfi/69#alist->hash-table")
___DEF_SYM(20,___S_srfi_2f_69_23_hash,"srfi/69#hash")
___DEF_SYM(21,___S_srfi_2f_69_23_hash_2d_by_2d_identity,"srfi/69#hash-by-identity")
___DEF_SYM(22,___S_srfi_2f_69_23_hash_2d_table_2d__3e_alist,"srfi/69#hash-table->alist")
___DEF_SYM(23,___S_srfi_2f_69_23_hash_2d_table_2d_copy,"srfi/69#hash-table-copy")
___DEF_SYM(24,___S_srfi_2f_69_23_hash_2d_table_2d_delete_21_,"srfi/69#hash-table-delete!")
___DEF_SYM(25,___S_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function,"srfi/69#hash-table-equivalence-function")

___DEF_SYM(26,___S_srfi_2f_69_23_hash_2d_table_2d_exists_3f_,"srfi/69#hash-table-exists?")
___DEF_SYM(27,___S_srfi_2f_69_23_hash_2d_table_2d_fold,"srfi/69#hash-table-fold")
___DEF_SYM(28,___S_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function,"srfi/69#hash-table-hash-function")

___DEF_SYM(29,___S_srfi_2f_69_23_hash_2d_table_2d_keys,"srfi/69#hash-table-keys")
___DEF_SYM(30,___S_srfi_2f_69_23_hash_2d_table_2d_merge_21_,"srfi/69#hash-table-merge!")
___DEF_SYM(31,___S_srfi_2f_69_23_hash_2d_table_2d_ref,"srfi/69#hash-table-ref")
___DEF_SYM(32,___S_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default,"srfi/69#hash-table-ref/default")

___DEF_SYM(33,___S_srfi_2f_69_23_hash_2d_table_2d_set_21_,"srfi/69#hash-table-set!")
___DEF_SYM(34,___S_srfi_2f_69_23_hash_2d_table_2d_size,"srfi/69#hash-table-size")
___DEF_SYM(35,___S_srfi_2f_69_23_hash_2d_table_2d_update_21_,"srfi/69#hash-table-update!")
___DEF_SYM(36,___S_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,"srfi/69#hash-table-update!/default")

___DEF_SYM(37,___S_srfi_2f_69_23_hash_2d_table_2d_values,"srfi/69#hash-table-values")
___DEF_SYM(38,___S_srfi_2f_69_23_hash_2d_table_2d_walk,"srfi/69#hash-table-walk")
___DEF_SYM(39,___S_srfi_2f_69_23_hash_2d_table_3f_,"srfi/69#hash-table?")
___DEF_SYM(40,___S_srfi_2f_69_23_make_2d_hash_2d_table,"srfi/69#make-hash-table")
___DEF_SYM(41,___S_srfi_2f_69_23_string_2d_ci_2d_hash,"srfi/69#string-ci-hash")
___DEF_SYM(42,___S_srfi_2f_69_23_string_2d_hash,"srfi/69#string-hash")
___DEF_SYM(43,___S_string,"string")
___DEF_SYM(44,___S_super,"super")
___DEF_SYM(45,___S_table,"table")
___DEF_SYM(46,___S_test,"test")
___DEF_SYM(47,___S_type,"type")
___END_SYM

#define ___SYM__23__23_type_2d_5 ___SYM(0,___S__23__23_type_2d_5)
#define ___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D ___SYM(1,___S__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)
#define ___SYM_bound ___SYM(2,___S_bound)
#define ___SYM_f ___SYM(3,___S_f)
#define ___SYM_fields ___SYM(4,___S_fields)
#define ___SYM_flags ___SYM(5,___S_flags)
#define ___SYM_func ___SYM(6,___S_func)
#define ___SYM_gcht ___SYM(7,___S_gcht)
#define ___SYM_hash ___SYM(8,___S_hash)
#define ___SYM_ht ___SYM(9,___S_ht)
#define ___SYM_ht1 ___SYM(10,___S_ht1)
#define ___SYM_ht2 ___SYM(11,___S_ht2)
#define ___SYM_id ___SYM(12,___S_id)
#define ___SYM_init ___SYM(13,___S_init)
#define ___SYM_loads ___SYM(14,___S_loads)
#define ___SYM_name ___SYM(15,___S_name)
#define ___SYM_proc ___SYM(16,___S_proc)
#define ___SYM_srfi_2f_69 ___SYM(17,___S_srfi_2f_69)
#define ___SYM_srfi_2f_69_23_ ___SYM(18,___S_srfi_2f_69_23_)
#define ___SYM_srfi_2f_69_23_alist_2d__3e_hash_2d_table ___SYM(19,___S_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
#define ___SYM_srfi_2f_69_23_hash ___SYM(20,___S_srfi_2f_69_23_hash)
#define ___SYM_srfi_2f_69_23_hash_2d_by_2d_identity ___SYM(21,___S_srfi_2f_69_23_hash_2d_by_2d_identity)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d__3e_alist ___SYM(22,___S_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_copy ___SYM(23,___S_srfi_2f_69_23_hash_2d_table_2d_copy)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_delete_21_ ___SYM(24,___S_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function ___SYM(25,___S_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_exists_3f_ ___SYM(26,___S_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_fold ___SYM(27,___S_srfi_2f_69_23_hash_2d_table_2d_fold)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function ___SYM(28,___S_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_keys ___SYM(29,___S_srfi_2f_69_23_hash_2d_table_2d_keys)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_merge_21_ ___SYM(30,___S_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_ref ___SYM(31,___S_srfi_2f_69_23_hash_2d_table_2d_ref)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default ___SYM(32,___S_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_set_21_ ___SYM(33,___S_srfi_2f_69_23_hash_2d_table_2d_set_21_)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_size ___SYM(34,___S_srfi_2f_69_23_hash_2d_table_2d_size)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_update_21_ ___SYM(35,___S_srfi_2f_69_23_hash_2d_table_2d_update_21_)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default ___SYM(36,___S_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_values ___SYM(37,___S_srfi_2f_69_23_hash_2d_table_2d_values)
#define ___SYM_srfi_2f_69_23_hash_2d_table_2d_walk ___SYM(38,___S_srfi_2f_69_23_hash_2d_table_2d_walk)
#define ___SYM_srfi_2f_69_23_hash_2d_table_3f_ ___SYM(39,___S_srfi_2f_69_23_hash_2d_table_3f_)
#define ___SYM_srfi_2f_69_23_make_2d_hash_2d_table ___SYM(40,___S_srfi_2f_69_23_make_2d_hash_2d_table)
#define ___SYM_srfi_2f_69_23_string_2d_ci_2d_hash ___SYM(41,___S_srfi_2f_69_23_string_2d_ci_2d_hash)
#define ___SYM_srfi_2f_69_23_string_2d_hash ___SYM(42,___S_srfi_2f_69_23_string_2d_hash)
#define ___SYM_string ___SYM(43,___S_string)
#define ___SYM_super ___SYM(44,___S_super)
#define ___SYM_table ___SYM(45,___S_table)
#define ___SYM_test ___SYM(46,___S_test)
#define ___SYM_type ___SYM(47,___S_type)

___BEGIN_KEY
___DEF_KEY(0,___K_hash,"hash")
___DEF_KEY(1,___K_test,"test")
___END_KEY

#define ___KEY_hash ___KEY(0,___K_hash)
#define ___KEY_test ___KEY(1,___K_test)

___BEGIN_GLO
___DEF_GLO(0,"srfi/69#")
___DEF_GLO(1,"srfi/69#alist->hash-table")
___DEF_GLO(2,"srfi/69#hash")
___DEF_GLO(3,"srfi/69#hash-by-identity")
___DEF_GLO(4,"srfi/69#hash-table->alist")
___DEF_GLO(5,"srfi/69#hash-table-copy")
___DEF_GLO(6,"srfi/69#hash-table-delete!")
___DEF_GLO(7,"srfi/69#hash-table-equivalence-function")

___DEF_GLO(8,"srfi/69#hash-table-exists?")
___DEF_GLO(9,"srfi/69#hash-table-fold")
___DEF_GLO(10,"srfi/69#hash-table-hash-function")
___DEF_GLO(11,"srfi/69#hash-table-keys")
___DEF_GLO(12,"srfi/69#hash-table-merge!")
___DEF_GLO(13,"srfi/69#hash-table-ref")
___DEF_GLO(14,"srfi/69#hash-table-ref/default")
___DEF_GLO(15,"srfi/69#hash-table-set!")
___DEF_GLO(16,"srfi/69#hash-table-size")
___DEF_GLO(17,"srfi/69#hash-table-update!")
___DEF_GLO(18,"srfi/69#hash-table-update!/default")

___DEF_GLO(19,"srfi/69#hash-table-values")
___DEF_GLO(20,"srfi/69#hash-table-walk")
___DEF_GLO(21,"srfi/69#hash-table?")
___DEF_GLO(22,"srfi/69#make-hash-table")
___DEF_GLO(23,"srfi/69#string-ci-hash")
___DEF_GLO(24,"srfi/69#string-hash")
___DEF_GLO(25,"##eq?")
___DEF_GLO(26,"##eq?-hash")
___DEF_GLO(27,"##equal?-hash")
___DEF_GLO(28,"##fail-check-exact-integer")
___DEF_GLO(29,"##fail-check-procedure")
___DEF_GLO(30,"##fail-check-string")
___DEF_GLO(31,"##fail-check-table")
___DEF_GLO(32,"##fold")
___DEF_GLO(33,"##list->table")
___DEF_GLO(34,"##make-table")
___DEF_GLO(35,"##modulo")
___DEF_GLO(36,"##raise-range-exception")
___DEF_GLO(37,"##raise-unbound-key-exception")
___DEF_GLO(38,"##string-ci=?-hash")
___DEF_GLO(39,"##string=?-hash")
___DEF_GLO(40,"##table->list")
___DEF_GLO(41,"##table-copy")
___DEF_GLO(42,"##table-for-each")
___DEF_GLO(43,"##table-length")
___DEF_GLO(44,"##table-merge!")
___DEF_GLO(45,"##table-ref")
___DEF_GLO(46,"##table-set!")
___DEF_GLO(47,"##table?")
___END_GLO

#define ___GLO_srfi_2f_69_23_ ___GLO(0,___G_srfi_2f_69_23_)
#define ___PRM_srfi_2f_69_23_ ___PRM(0,___G_srfi_2f_69_23_)
#define ___GLO_srfi_2f_69_23_alist_2d__3e_hash_2d_table ___GLO(1,___G_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
#define ___PRM_srfi_2f_69_23_alist_2d__3e_hash_2d_table ___PRM(1,___G_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
#define ___GLO_srfi_2f_69_23_hash ___GLO(2,___G_srfi_2f_69_23_hash)
#define ___PRM_srfi_2f_69_23_hash ___PRM(2,___G_srfi_2f_69_23_hash)
#define ___GLO_srfi_2f_69_23_hash_2d_by_2d_identity ___GLO(3,___G_srfi_2f_69_23_hash_2d_by_2d_identity)
#define ___PRM_srfi_2f_69_23_hash_2d_by_2d_identity ___PRM(3,___G_srfi_2f_69_23_hash_2d_by_2d_identity)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d__3e_alist ___GLO(4,___G_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d__3e_alist ___PRM(4,___G_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_copy ___GLO(5,___G_srfi_2f_69_23_hash_2d_table_2d_copy)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_copy ___PRM(5,___G_srfi_2f_69_23_hash_2d_table_2d_copy)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_delete_21_ ___GLO(6,___G_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_delete_21_ ___PRM(6,___G_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function ___GLO(7,___G_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function ___PRM(7,___G_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_exists_3f_ ___GLO(8,___G_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_exists_3f_ ___PRM(8,___G_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_fold ___GLO(9,___G_srfi_2f_69_23_hash_2d_table_2d_fold)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_fold ___PRM(9,___G_srfi_2f_69_23_hash_2d_table_2d_fold)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function ___GLO(10,___G_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function ___PRM(10,___G_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_keys ___GLO(11,___G_srfi_2f_69_23_hash_2d_table_2d_keys)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_keys ___PRM(11,___G_srfi_2f_69_23_hash_2d_table_2d_keys)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_merge_21_ ___GLO(12,___G_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_merge_21_ ___PRM(12,___G_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_ref ___GLO(13,___G_srfi_2f_69_23_hash_2d_table_2d_ref)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_ref ___PRM(13,___G_srfi_2f_69_23_hash_2d_table_2d_ref)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default ___GLO(14,___G_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default ___PRM(14,___G_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_set_21_ ___GLO(15,___G_srfi_2f_69_23_hash_2d_table_2d_set_21_)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_set_21_ ___PRM(15,___G_srfi_2f_69_23_hash_2d_table_2d_set_21_)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_size ___GLO(16,___G_srfi_2f_69_23_hash_2d_table_2d_size)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_size ___PRM(16,___G_srfi_2f_69_23_hash_2d_table_2d_size)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_update_21_ ___GLO(17,___G_srfi_2f_69_23_hash_2d_table_2d_update_21_)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_update_21_ ___PRM(17,___G_srfi_2f_69_23_hash_2d_table_2d_update_21_)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default ___GLO(18,___G_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default ___PRM(18,___G_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_values ___GLO(19,___G_srfi_2f_69_23_hash_2d_table_2d_values)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_values ___PRM(19,___G_srfi_2f_69_23_hash_2d_table_2d_values)
#define ___GLO_srfi_2f_69_23_hash_2d_table_2d_walk ___GLO(20,___G_srfi_2f_69_23_hash_2d_table_2d_walk)
#define ___PRM_srfi_2f_69_23_hash_2d_table_2d_walk ___PRM(20,___G_srfi_2f_69_23_hash_2d_table_2d_walk)
#define ___GLO_srfi_2f_69_23_hash_2d_table_3f_ ___GLO(21,___G_srfi_2f_69_23_hash_2d_table_3f_)
#define ___PRM_srfi_2f_69_23_hash_2d_table_3f_ ___PRM(21,___G_srfi_2f_69_23_hash_2d_table_3f_)
#define ___GLO_srfi_2f_69_23_make_2d_hash_2d_table ___GLO(22,___G_srfi_2f_69_23_make_2d_hash_2d_table)
#define ___PRM_srfi_2f_69_23_make_2d_hash_2d_table ___PRM(22,___G_srfi_2f_69_23_make_2d_hash_2d_table)
#define ___GLO_srfi_2f_69_23_string_2d_ci_2d_hash ___GLO(23,___G_srfi_2f_69_23_string_2d_ci_2d_hash)
#define ___PRM_srfi_2f_69_23_string_2d_ci_2d_hash ___PRM(23,___G_srfi_2f_69_23_string_2d_ci_2d_hash)
#define ___GLO_srfi_2f_69_23_string_2d_hash ___GLO(24,___G_srfi_2f_69_23_string_2d_hash)
#define ___PRM_srfi_2f_69_23_string_2d_hash ___PRM(24,___G_srfi_2f_69_23_string_2d_hash)
#define ___GLO__23__23_eq_3f_ ___GLO(25,___G__23__23_eq_3f_)
#define ___PRM__23__23_eq_3f_ ___PRM(25,___G__23__23_eq_3f_)
#define ___GLO__23__23_eq_3f__2d_hash ___GLO(26,___G__23__23_eq_3f__2d_hash)
#define ___PRM__23__23_eq_3f__2d_hash ___PRM(26,___G__23__23_eq_3f__2d_hash)
#define ___GLO__23__23_equal_3f__2d_hash ___GLO(27,___G__23__23_equal_3f__2d_hash)
#define ___PRM__23__23_equal_3f__2d_hash ___PRM(27,___G__23__23_equal_3f__2d_hash)
#define ___GLO__23__23_fail_2d_check_2d_exact_2d_integer ___GLO(28,___G__23__23_fail_2d_check_2d_exact_2d_integer)
#define ___PRM__23__23_fail_2d_check_2d_exact_2d_integer ___PRM(28,___G__23__23_fail_2d_check_2d_exact_2d_integer)
#define ___GLO__23__23_fail_2d_check_2d_procedure ___GLO(29,___G__23__23_fail_2d_check_2d_procedure)
#define ___PRM__23__23_fail_2d_check_2d_procedure ___PRM(29,___G__23__23_fail_2d_check_2d_procedure)
#define ___GLO__23__23_fail_2d_check_2d_string ___GLO(30,___G__23__23_fail_2d_check_2d_string)
#define ___PRM__23__23_fail_2d_check_2d_string ___PRM(30,___G__23__23_fail_2d_check_2d_string)
#define ___GLO__23__23_fail_2d_check_2d_table ___GLO(31,___G__23__23_fail_2d_check_2d_table)
#define ___PRM__23__23_fail_2d_check_2d_table ___PRM(31,___G__23__23_fail_2d_check_2d_table)
#define ___GLO__23__23_fold ___GLO(32,___G__23__23_fold)
#define ___PRM__23__23_fold ___PRM(32,___G__23__23_fold)
#define ___GLO__23__23_list_2d__3e_table ___GLO(33,___G__23__23_list_2d__3e_table)
#define ___PRM__23__23_list_2d__3e_table ___PRM(33,___G__23__23_list_2d__3e_table)
#define ___GLO__23__23_make_2d_table ___GLO(34,___G__23__23_make_2d_table)
#define ___PRM__23__23_make_2d_table ___PRM(34,___G__23__23_make_2d_table)
#define ___GLO__23__23_modulo ___GLO(35,___G__23__23_modulo)
#define ___PRM__23__23_modulo ___PRM(35,___G__23__23_modulo)
#define ___GLO__23__23_raise_2d_range_2d_exception ___GLO(36,___G__23__23_raise_2d_range_2d_exception)
#define ___PRM__23__23_raise_2d_range_2d_exception ___PRM(36,___G__23__23_raise_2d_range_2d_exception)
#define ___GLO__23__23_raise_2d_unbound_2d_key_2d_exception ___GLO(37,___G__23__23_raise_2d_unbound_2d_key_2d_exception)
#define ___PRM__23__23_raise_2d_unbound_2d_key_2d_exception ___PRM(37,___G__23__23_raise_2d_unbound_2d_key_2d_exception)
#define ___GLO__23__23_string_2d_ci_3d__3f__2d_hash ___GLO(38,___G__23__23_string_2d_ci_3d__3f__2d_hash)
#define ___PRM__23__23_string_2d_ci_3d__3f__2d_hash ___PRM(38,___G__23__23_string_2d_ci_3d__3f__2d_hash)
#define ___GLO__23__23_string_3d__3f__2d_hash ___GLO(39,___G__23__23_string_3d__3f__2d_hash)
#define ___PRM__23__23_string_3d__3f__2d_hash ___PRM(39,___G__23__23_string_3d__3f__2d_hash)
#define ___GLO__23__23_table_2d__3e_list ___GLO(40,___G__23__23_table_2d__3e_list)
#define ___PRM__23__23_table_2d__3e_list ___PRM(40,___G__23__23_table_2d__3e_list)
#define ___GLO__23__23_table_2d_copy ___GLO(41,___G__23__23_table_2d_copy)
#define ___PRM__23__23_table_2d_copy ___PRM(41,___G__23__23_table_2d_copy)
#define ___GLO__23__23_table_2d_for_2d_each ___GLO(42,___G__23__23_table_2d_for_2d_each)
#define ___PRM__23__23_table_2d_for_2d_each ___PRM(42,___G__23__23_table_2d_for_2d_each)
#define ___GLO__23__23_table_2d_length ___GLO(43,___G__23__23_table_2d_length)
#define ___PRM__23__23_table_2d_length ___PRM(43,___G__23__23_table_2d_length)
#define ___GLO__23__23_table_2d_merge_21_ ___GLO(44,___G__23__23_table_2d_merge_21_)
#define ___PRM__23__23_table_2d_merge_21_ ___PRM(44,___G__23__23_table_2d_merge_21_)
#define ___GLO__23__23_table_2d_ref ___GLO(45,___G__23__23_table_2d_ref)
#define ___PRM__23__23_table_2d_ref ___PRM(45,___G__23__23_table_2d_ref)
#define ___GLO__23__23_table_2d_set_21_ ___GLO(46,___G__23__23_table_2d_set_21_)
#define ___PRM__23__23_table_2d_set_21_ ___PRM(46,___G__23__23_table_2d_set_21_)
#define ___GLO__23__23_table_3f_ ___GLO(47,___G__23__23_table_3f_)
#define ___PRM__23__23_table_3f_ ___PRM(47,___G__23__23_table_3f_)

___BEGIN_CNS
 ___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(3),___REF_SYM(6,___S_func))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(3),___REF_SYM(6,___S_func))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(16,___S_proc))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(3,___S_f))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(9,___S_ht))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(11,___S_ht2))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(10,___S_ht1))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(43,___S_string))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
,___DEF_CNS(___REF_FIX(1),___REF_SYM(43,___S_string))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
,___DEF_CNS(___REF_FIX(2),___REF_SYM(2,___S_bound))
___END_CNS

___DEF_SUB_STRUCTURE(___X0,6UL)
               ___VEC1(___REF_SUB(1))
               ___VEC1(___REF_SYM(1,___S__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D))
               ___VEC1(___REF_SYM(45,___S_table))
               ___VEC1(___REF_FIX(29))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(3))
               ___VEC0
___DEF_SUB_STRUCTURE(___X1,6UL)
               ___VEC1(___REF_SUB(1))
               ___VEC1(___REF_SYM(0,___S__23__23_type_2d_5))
               ___VEC1(___REF_SYM(47,___S_type))
               ___VEC1(___REF_FIX(8))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(2))
               ___VEC0
___DEF_SUB_VEC(___X2,15UL)
               ___VEC1(___REF_SYM(12,___S_id))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(15,___S_name))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(5,___S_flags))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(44,___S_super))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(4,___S_fields))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X3,18UL)
               ___VEC1(___REF_SYM(5,___S_flags))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(46,___S_test))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(8,___S_hash))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(14,___S_loads))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(7,___S_gcht))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(13,___S_init))
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
               ___VEC1(___REF_SYM(17,___S_srfi_2f_69))
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
___DEF_M_HLBL(___L0_srfi_2f_69_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_3f_)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_make_2d_hash_2d_table)
___DEF_M_HLBL(___L1_srfi_2f_69_23_make_2d_hash_2d_table)
___DEF_M_HLBL(___L2_srfi_2f_69_23_make_2d_hash_2d_table)
___DEF_M_HLBL(___L3_srfi_2f_69_23_make_2d_hash_2d_table)
___DEF_M_HLBL(___L4_srfi_2f_69_23_make_2d_hash_2d_table)
___DEF_M_HLBL(___L5_srfi_2f_69_23_make_2d_hash_2d_table)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___DEF_M_HLBL(___L1_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___DEF_M_HLBL(___L2_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___DEF_M_HLBL(___L3_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___DEF_M_HLBL(___L4_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___DEF_M_HLBL(___L5_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_M_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_M_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_M_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_M_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_set_21_)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_set_21_)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_set_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
___DEF_M_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_M_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_M_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_M_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_M_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_M_HLBL(___L7_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_M_HLBL(___L8_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_M_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_M_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_M_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_M_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_size)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_size)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_size)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_M_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_M_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_M_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_M_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_M_HLBL(___L7_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_M_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_M_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_M_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_M_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_M_HLBL(___L7_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_walk)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_walk)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_walk)
___DEF_M_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_walk)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_M_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_M_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_M_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_M_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_M_HLBL(___L7_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_M_HLBL(___L8_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_copy)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_copy)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_copy)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___DEF_M_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash)
___DEF_M_HLBL(___L3_srfi_2f_69_23_hash)
___DEF_M_HLBL(___L4_srfi_2f_69_23_hash)
___DEF_M_HLBL(___L5_srfi_2f_69_23_hash)
___DEF_M_HLBL(___L6_srfi_2f_69_23_hash)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_string_2d_hash)
___DEF_M_HLBL(___L1_srfi_2f_69_23_string_2d_hash)
___DEF_M_HLBL(___L2_srfi_2f_69_23_string_2d_hash)
___DEF_M_HLBL(___L3_srfi_2f_69_23_string_2d_hash)
___DEF_M_HLBL(___L4_srfi_2f_69_23_string_2d_hash)
___DEF_M_HLBL(___L5_srfi_2f_69_23_string_2d_hash)
___DEF_M_HLBL(___L6_srfi_2f_69_23_string_2d_hash)
___DEF_M_HLBL(___L7_srfi_2f_69_23_string_2d_hash)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_M_HLBL(___L1_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_M_HLBL(___L2_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_M_HLBL(___L3_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_M_HLBL(___L4_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_M_HLBL(___L5_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_M_HLBL(___L6_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_M_HLBL(___L7_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_M_HLBL(___L1_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_M_HLBL(___L2_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_M_HLBL(___L3_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_M_HLBL(___L4_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_M_HLBL(___L5_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_M_HLBL(___L6_srfi_2f_69_23_hash_2d_by_2d_identity)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_
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
___DEF_P_HLBL(___L0_srfi_2f_69_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_)
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_3f_)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_3f_)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_3f_)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),47,___G__23__23_table_3f_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_make_2d_hash_2d_table
#undef ___PH_LBL0
#define ___PH_LBL0 6
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_make_2d_hash_2d_table)
___DEF_P_HLBL(___L1_srfi_2f_69_23_make_2d_hash_2d_table)
___DEF_P_HLBL(___L2_srfi_2f_69_23_make_2d_hash_2d_table)
___DEF_P_HLBL(___L3_srfi_2f_69_23_make_2d_hash_2d_table)
___DEF_P_HLBL(___L4_srfi_2f_69_23_make_2d_hash_2d_table)
___DEF_P_HLBL(___L5_srfi_2f_69_23_make_2d_hash_2d_table)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_make_2d_hash_2d_table)
   ___IF_NARGS_EQ(0,___SET_R1(___ABSENT) ___SET_R2(___ABSENT))
   ___IF_NARGS_EQ(1,___SET_R2(___ABSENT))
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,0,2,0)
___DEF_GLBL(___L_srfi_2f_69_23_make_2d_hash_2d_table)
   ___IF(___NOT(___EQP(___R1,___ABSENT)))
   ___GOTO(___L6_srfi_2f_69_23_make_2d_hash_2d_table)
   ___END_IF
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_make_2d_hash_2d_table)
   ___JUMPGLONOTSAFE(___SET_NARGS(0),34,___G__23__23_make_2d_table)
___DEF_GLBL(___L6_srfi_2f_69_23_make_2d_hash_2d_table)
   ___IF(___NOT(___PROCEDUREP(___R1)))
   ___GOTO(___L9_srfi_2f_69_23_make_2d_hash_2d_table)
   ___END_IF
   ___IF(___NOT(___EQP(___R2,___ABSENT)))
   ___GOTO(___L7_srfi_2f_69_23_make_2d_hash_2d_table)
   ___END_IF
   ___SET_R2(___R1)
   ___SET_R1(___KEY_test)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_make_2d_hash_2d_table)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),34,___G__23__23_make_2d_table)
___DEF_GLBL(___L7_srfi_2f_69_23_make_2d_hash_2d_table)
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L8_srfi_2f_69_23_make_2d_hash_2d_table)
   ___END_IF
   ___SET_STK(1,___KEY_test)
   ___SET_R3(___R2)
   ___SET_R2(___KEY_hash)
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_69_23_make_2d_hash_2d_table)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),34,___G__23__23_make_2d_table)
___DEF_GLBL(___L8_srfi_2f_69_23_make_2d_hash_2d_table)
   ___SET_STK(1,___FIX(2L))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(4)
___DEF_SLBL(4,___L4_srfi_2f_69_23_make_2d_hash_2d_table)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),29,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L9_srfi_2f_69_23_make_2d_hash_2d_table)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___FIX(1L))
   ___POLL(5)
___DEF_SLBL(5,___L5_srfi_2f_69_23_make_2d_hash_2d_table)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),29,___G__23__23_fail_2d_check_2d_procedure)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_alist_2d__3e_hash_2d_table
#undef ___PH_LBL0
#define ___PH_LBL0 13
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___DEF_P_HLBL(___L1_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___DEF_P_HLBL(___L2_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___DEF_P_HLBL(___L3_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___DEF_P_HLBL(___L4_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___DEF_P_HLBL(___L5_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___IF_NARGS_EQ(1,___SET_R2(___ABSENT) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(2,___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,1,2,0)
___DEF_GLBL(___L_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___IF(___NOT(___EQP(___R2,___ABSENT)))
   ___GOTO(___L6_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___END_IF
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),33,___G__23__23_list_2d__3e_table)
___DEF_GLBL(___L6_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L9_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___END_IF
   ___IF(___NOT(___EQP(___R3,___ABSENT)))
   ___GOTO(___L7_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___END_IF
   ___SET_R3(___R2)
   ___SET_R2(___KEY_test)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),33,___G__23__23_list_2d__3e_table)
___DEF_GLBL(___L7_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___IF(___NOT(___PROCEDUREP(___R3)))
   ___GOTO(___L8_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___END_IF
   ___SET_STK(1,___R1)
   ___SET_STK(2,___KEY_test)
   ___SET_R1(___R2)
   ___SET_R2(___KEY_hash)
   ___ADJFP(2)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),33,___G__23__23_list_2d__3e_table)
___DEF_GLBL(___L8_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___SET_STK(1,___FIX(3L))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(4)
___DEF_SLBL(4,___L4_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),29,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L9_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___SET_STK(1,___FIX(2L))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(5)
___DEF_SLBL(5,___L5_srfi_2f_69_23_alist_2d__3e_hash_2d_table)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),29,___G__23__23_fail_2d_check_2d_procedure)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function
#undef ___PH_LBL0
#define ___PH_LBL0 20
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L3_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(2L),___SUB(0),___FAL))
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L2_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
   ___END_IF
   ___JUMPRET(___R0)
___DEF_GLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
   ___SET_R1(___PRM__23__23_eq_3f_)
   ___JUMPRET(___R0)
___DEF_GLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___CNS(0))
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function
#undef ___PH_LBL0
#define ___PH_LBL0 23
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L3_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(0),___FAL))
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L2_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
   ___END_IF
   ___JUMPRET(___R0)
___DEF_GLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
   ___SET_R1(___GLO__23__23_eq_3f__2d_hash)
   ___JUMPRET(___R0)
___DEF_GLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___CNS(1))
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_ref
#undef ___PH_LBL0
#define ___PH_LBL0 26
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_P_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_P_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_P_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_ref)
___DEF_P_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_ref)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___IF_NARGS_EQ(2,___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,2,1,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L10_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R3(___DELETED)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(3),45,___G__23__23_table_2d_ref)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___IF(___NOT(___EQP(___R1,___DELETED)))
   ___GOTO(___L9_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___END_IF
   ___IF(___NOT(___EQP(___STK(-4),___ABSENT)))
   ___GOTO(___L7_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___END_IF
   ___SET_STK(-3,___STK(-7))
   ___SET_STK(-7,___LBL(0))
   ___SET_R3(___STK(-4))
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-3))
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___ADJFP(-7)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),37,___G__23__23_raise_2d_unbound_2d_key_2d_exception)
___DEF_GLBL(___L7_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___IF(___NOT(___PROCEDUREP(___STK(-4))))
   ___GOTO(___L8_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___END_IF
   ___SET_R0(___STK(-7))
   ___POLL(4)
___DEF_SLBL(4,___L4_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___ADJFP(-8)
   ___JUMPGENNOTSAFE(___SET_NARGS(0),___STK(4))
___DEF_GLBL(___L8_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___SET_STK(-3,___STK(-7))
   ___SET_STK(-7,___FIX(3L))
   ___SET_STK(-2,___STK(-6))
   ___SET_STK(-6,___LBL(0))
   ___SET_R3(___STK(-4))
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-2))
   ___SET_R0(___STK(-3))
   ___POLL(5)
___DEF_SLBL(5,___L5_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___ADJFP(-6)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),29,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L9_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L10_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___SET_STK(1,___CNS(2))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(6)
___DEF_SLBL(6,___L6_srfi_2f_69_23_hash_2d_table_2d_ref)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default
#undef ___PH_LBL0
#define ___PH_LBL0 34
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,3,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L3_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
   ___END_IF
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),45,___G__23__23_table_2d_ref)
___DEF_GLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
   ___SET_STK(1,___CNS(3))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_set_21_
#undef ___PH_LBL0
#define ___PH_LBL0 38
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_set_21_)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_set_21_)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_set_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_set_21_)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,3,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_set_21_)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L3_srfi_2f_69_23_hash_2d_table_2d_set_21_)
   ___END_IF
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_set_21_)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),46,___G__23__23_table_2d_set_21_)
___DEF_GLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_set_21_)
   ___SET_STK(1,___CNS(4))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_set_21_)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_delete_21_
#undef ___PH_LBL0
#define ___PH_LBL0 42
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L3_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
   ___END_IF
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),46,___G__23__23_table_2d_set_21_)
___DEF_GLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
   ___SET_STK(1,___CNS(5))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_delete_21_)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_exists_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 46
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
___DEF_P_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L4_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_R3(___DELETED)
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(3),45,___G__23__23_table_2d_ref)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
   ___SET_R1(___BOOLEAN(___EQP(___R1,___DELETED)))
   ___SET_R1(___BOOLEAN(___FALSEP(___R1)))
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
   ___SET_STK(1,___CNS(6))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_69_23_hash_2d_table_2d_exists_3f_)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_update_21_
#undef ___PH_LBL0
#define ___PH_LBL0 51
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_P_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_P_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_P_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_P_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_P_HLBL(___L7_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___DEF_P_HLBL(___L8_srfi_2f_69_23_hash_2d_table_2d_update_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___IF_NARGS_EQ(3,___PUSH(___R1) ___SET_R1(___R2) ___SET_R2(___R3) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,3,1,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___IF(___NOT(___STRUCTUREDIOP(___STK(0),___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L12_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___END_IF
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L11_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R2(___R1)
   ___SET_R1(___STK(0))
   ___SET_R3(___DELETED)
   ___ADJFP(7)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(3),45,___G__23__23_table_2d_ref)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___IF(___NOT(___EQP(___R1,___DELETED)))
   ___GOTO(___L10_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___END_IF
   ___IF(___NOT(___EQP(___STK(-3),___ABSENT)))
   ___GOTO(___L9_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___END_IF
   ___SET_STK(-2,___STK(-7))
   ___SET_STK(-7,___LBL(0))
   ___SET_STK(-1,___STK(-6))
   ___SET_STK(-6,___STK(-2))
   ___SET_R3(___STK(-3))
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-1))
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___ADJFP(-6)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),37,___G__23__23_raise_2d_unbound_2d_key_2d_exception)
___DEF_GLBL(___L9_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___SET_R0(___LBL(4))
   ___JUMPGENNOTSAFE(___SET_NARGS(0),___STK(-3))
___DEF_SLBL(4,___L4_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___SET_R0(___LBL(5))
   ___JUMPGENNOTSAFE(___SET_NARGS(1),___STK(-4))
___DEF_SLBL(5,___L5_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___SET_R3(___R1)
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-7))
   ___SET_R0(___STK(-6))
   ___POLL(6)
___DEF_SLBL(6,___L6_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),46,___G__23__23_table_2d_set_21_)
___DEF_GLBL(___L10_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___SET_R0(___LBL(5))
   ___JUMPGENNOTSAFE(___SET_NARGS(1),___STK(-4))
___DEF_GLBL(___L11_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___CNS(7))
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___LBL(0))
   ___ADJFP(2)
   ___POLL(7)
___DEF_SLBL(7,___L7_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),29,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L12_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___CNS(8))
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___LBL(0))
   ___ADJFP(2)
   ___POLL(8)
___DEF_SLBL(8,___L8_srfi_2f_69_23_hash_2d_table_2d_update_21_)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default
#undef ___PH_LBL0
#define ___PH_LBL0 61
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_P_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_P_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_P_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___DEF_P_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,4,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___IF(___NOT(___STRUCTUREDIOP(___STK(0),___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L8_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___END_IF
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L7_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(0))
   ___ADJFP(7)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(3),45,___G__23__23_table_2d_ref)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___SET_R0(___LBL(3))
   ___JUMPGENNOTSAFE(___SET_NARGS(1),___STK(-4))
___DEF_SLBL(3,___L3_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___SET_R3(___R1)
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-7))
   ___SET_R0(___STK(-6))
   ___POLL(4)
___DEF_SLBL(4,___L4_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),46,___G__23__23_table_2d_set_21_)
___DEF_GLBL(___L7_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___CNS(9))
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___LBL(0))
   ___ADJFP(2)
   ___POLL(5)
___DEF_SLBL(5,___L5_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),29,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L8_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___CNS(10))
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___LBL(0))
   ___ADJFP(2)
   ___POLL(6)
___DEF_SLBL(6,___L6_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_size
#undef ___PH_LBL0
#define ___PH_LBL0 69
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_size)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_size)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_size)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_size)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_size)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L3_srfi_2f_69_23_hash_2d_table_2d_size)
   ___END_IF
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_size)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),43,___G__23__23_table_2d_length)
___DEF_GLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_size)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___CNS(11))
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_size)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_keys
#undef ___PH_LBL0
#define ___PH_LBL0 73
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_P_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_P_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_P_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_P_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_P_HLBL(___L7_srfi_2f_69_23_hash_2d_table_2d_keys)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L10_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___END_IF
   ___SET_STK(1,___R0)
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),40,___G__23__23_table_2d__3e_list)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___SET_R0(___STK(-3))
   ___ADJFP(-4)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___GOTO(___L9_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_GLBL(___L8_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___SET_R2(___CAR(___R1))
   ___SET_R2(___CAR(___R2))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_R1(___CDR(___R1))
   ___SET_R0(___LBL(5))
   ___ADJFP(8)
   ___POLL(4)
___DEF_SLBL(4,___L4_srfi_2f_69_23_hash_2d_table_2d_keys)
___DEF_GLBL(___L9_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___IF(___PAIRP(___R1))
   ___GOTO(___L8_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___END_IF
   ___SET_R1(___NUL)
   ___JUMPRET(___R0)
___DEF_SLBL(5,___L5_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___SET_R1(___CONS(___STK(-6),___R1))
   ___ADJFP(-7)
   ___CHECK_HEAP(6,4096)
___DEF_SLBL(6,___L6_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___ADJFP(-1)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L10_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___CNS(12))
   ___POLL(7)
___DEF_SLBL(7,___L7_srfi_2f_69_23_hash_2d_table_2d_keys)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_values
#undef ___PH_LBL0
#define ___PH_LBL0 82
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_P_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_P_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_P_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_P_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_P_HLBL(___L7_srfi_2f_69_23_hash_2d_table_2d_values)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_values)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_values)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L10_srfi_2f_69_23_hash_2d_table_2d_values)
   ___END_IF
   ___SET_STK(1,___R0)
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_values)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),40,___G__23__23_table_2d__3e_list)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_values)
   ___SET_R0(___STK(-3))
   ___ADJFP(-4)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_69_23_hash_2d_table_2d_values)
   ___GOTO(___L9_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_GLBL(___L8_srfi_2f_69_23_hash_2d_table_2d_values)
   ___SET_R2(___CAR(___R1))
   ___SET_R2(___CDR(___R2))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_R1(___CDR(___R1))
   ___SET_R0(___LBL(5))
   ___ADJFP(8)
   ___POLL(4)
___DEF_SLBL(4,___L4_srfi_2f_69_23_hash_2d_table_2d_values)
___DEF_GLBL(___L9_srfi_2f_69_23_hash_2d_table_2d_values)
   ___IF(___PAIRP(___R1))
   ___GOTO(___L8_srfi_2f_69_23_hash_2d_table_2d_values)
   ___END_IF
   ___SET_R1(___NUL)
   ___JUMPRET(___R0)
___DEF_SLBL(5,___L5_srfi_2f_69_23_hash_2d_table_2d_values)
   ___SET_R1(___CONS(___STK(-6),___R1))
   ___ADJFP(-7)
   ___CHECK_HEAP(6,4096)
___DEF_SLBL(6,___L6_srfi_2f_69_23_hash_2d_table_2d_values)
   ___ADJFP(-1)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L10_srfi_2f_69_23_hash_2d_table_2d_values)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___CNS(13))
   ___POLL(7)
___DEF_SLBL(7,___L7_srfi_2f_69_23_hash_2d_table_2d_values)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_walk
#undef ___PH_LBL0
#define ___PH_LBL0 91
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_walk)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_walk)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_walk)
___DEF_P_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_walk)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_walk)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_walk)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L5_srfi_2f_69_23_hash_2d_table_2d_walk)
   ___END_IF
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L4_srfi_2f_69_23_hash_2d_table_2d_walk)
   ___END_IF
   ___SET_STK(1,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(1))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_walk)
   ___ADJFP(-1)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),42,___G__23__23_table_2d_for_2d_each)
___DEF_GLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_walk)
   ___SET_STK(1,___CNS(14))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_walk)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),29,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_walk)
   ___SET_STK(1,___CNS(15))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_69_23_hash_2d_table_2d_walk)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_fold
#undef ___PH_LBL0
#define ___PH_LBL0 96
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_P_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_P_HLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_P_HLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_P_HLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_P_HLBL(___L7_srfi_2f_69_23_hash_2d_table_2d_fold)
___DEF_P_HLBL(___L8_srfi_2f_69_23_hash_2d_table_2d_fold)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,3,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L10_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___END_IF
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L9_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___END_IF
   ___SET_STK(1,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(1),5)
   ___ADD_CLO_ELEM(0,___R2)
   ___END_SETUP_CLO(1)
   ___SET_STK(2,___R0)
   ___SET_STK(3,___R3)
   ___ADJFP(8)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),40,___G__23__23_table_2d__3e_list)
___DEF_SLBL(3,___L3_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___SET_R3(___R1)
   ___SET_R2(___STK(-5))
   ___SET_R0(___STK(-6))
   ___SET_R1(___STK(-7))
   ___POLL(4)
___DEF_SLBL(4,___L4_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___ADJFP(-8)
   ___JUMPPRM(___SET_NARGS(3),___PRM__23__23_fold)
___DEF_SLBL(5,___L5_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(5,2,0,0)
   ___SET_R3(___R2)
   ___SET_R2(___CDR(___R1))
   ___SET_R1(___CAR(___R1))
   ___POLL(6)
___DEF_SLBL(6,___L6_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___JUMPGENNOTSAFE(___SET_NARGS(3),___CLO(___R4,1))
___DEF_GLBL(___L9_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___SET_STK(1,___CNS(16))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(7)
___DEF_SLBL(7,___L7_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),29,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L10_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___SET_STK(1,___CNS(17))
   ___SET_STK(2,___LBL(0))
   ___ADJFP(2)
   ___POLL(8)
___DEF_SLBL(8,___L8_srfi_2f_69_23_hash_2d_table_2d_fold)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d__3e_alist
#undef ___PH_LBL0
#define ___PH_LBL0 106
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L3_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
   ___END_IF
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),40,___G__23__23_table_2d__3e_list)
___DEF_GLBL(___L3_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___CNS(18))
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d__3e_alist)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_copy
#undef ___PH_LBL0
#define ___PH_LBL0 110
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_copy)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_copy)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_copy)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_copy)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_copy)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L3_srfi_2f_69_23_hash_2d_table_2d_copy)
   ___END_IF
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_copy)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),41,___G__23__23_table_2d_copy)
___DEF_GLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_copy)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___CNS(19))
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_copy)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_table_2d_merge_21_
#undef ___PH_LBL0
#define ___PH_LBL0 114
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___DEF_P_HLBL(___L3_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L5_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
   ___END_IF
   ___IF(___NOT(___STRUCTUREDIOP(___R2,___SYM__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D)))
   ___GOTO(___L4_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
   ___END_IF
   ___SET_R3(___FAL)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),44,___G__23__23_table_2d_merge_21_)
___DEF_GLBL(___L4_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
   ___SET_STK(1,___CNS(20))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
   ___GOTO(___L6_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___DEF_GLBL(___L5_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
   ___SET_STK(1,___CNS(21))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(3)
___DEF_SLBL(3,___L3_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
___DEF_GLBL(___L6_srfi_2f_69_23_hash_2d_table_2d_merge_21_)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),31,___G__23__23_fail_2d_check_2d_table)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash
#undef ___PH_LBL0
#define ___PH_LBL0 119
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash)
___DEF_P_HLBL(___L3_srfi_2f_69_23_hash)
___DEF_P_HLBL(___L4_srfi_2f_69_23_hash)
___DEF_P_HLBL(___L5_srfi_2f_69_23_hash)
___DEF_P_HLBL(___L6_srfi_2f_69_23_hash)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash)
   ___IF_NARGS_EQ(1,___SET_R2(___ABSENT))
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,1,1,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash)
   ___IF(___NOT(___EQP(___R2,___ABSENT)))
   ___GOTO(___L7_srfi_2f_69_23_hash)
   ___END_IF
   ___SET_R3(___FIX(536870911L))
   ___IF(___FIXNUMP(___R3))
   ___GOTO(___L8_srfi_2f_69_23_hash)
   ___END_IF
   ___GOTO(___L13_srfi_2f_69_23_hash)
___DEF_GLBL(___L7_srfi_2f_69_23_hash)
   ___SET_R3(___R2)
   ___IF(___NOT(___FIXNUMP(___R3)))
   ___GOTO(___L13_srfi_2f_69_23_hash)
   ___END_IF
___DEF_GLBL(___L8_srfi_2f_69_23_hash)
   ___IF(___NOT(___FIXNEGATIVEP(___R3)))
   ___GOTO(___L10_srfi_2f_69_23_hash)
   ___END_IF
   ___SET_STK(1,___CNS(22))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash)
___DEF_GLBL(___L9_srfi_2f_69_23_hash)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),36,___G__23__23_raise_2d_range_2d_exception)
___DEF_GLBL(___L10_srfi_2f_69_23_hash)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R3)
   ___ADJFP(8)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),27,___G__23__23_equal_3f__2d_hash)
___DEF_SLBL(3,___L3_srfi_2f_69_23_hash)
   ___IF(___NOT(___FIXNUMP(___STK(-6))))
   ___GOTO(___L11_srfi_2f_69_23_hash)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L11_srfi_2f_69_23_hash)
   ___END_IF
   ___IF(___NOT(___EQP(___STK(-6),___FIX(0L))))
   ___GOTO(___L12_srfi_2f_69_23_hash)
   ___END_IF
___DEF_GLBL(___L11_srfi_2f_69_23_hash)
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(4)
___DEF_SLBL(4,___L4_srfi_2f_69_23_hash)
   ___ADJFP(-8)
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23_modulo)
___DEF_GLBL(___L12_srfi_2f_69_23_hash)
   ___SET_R1(___FIXMOD(___R1,___STK(-6)))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L13_srfi_2f_69_23_hash)
   ___IF(___NOT(___BIGNUMP(___R3)))
   ___GOTO(___L14_srfi_2f_69_23_hash)
   ___END_IF
   ___SET_STK(1,___CNS(23))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(5)
___DEF_SLBL(5,___L5_srfi_2f_69_23_hash)
   ___GOTO(___L9_srfi_2f_69_23_hash)
___DEF_GLBL(___L14_srfi_2f_69_23_hash)
   ___SET_STK(1,___CNS(24))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(6)
___DEF_SLBL(6,___L6_srfi_2f_69_23_hash)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),28,___G__23__23_fail_2d_check_2d_exact_2d_integer)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_string_2d_hash
#undef ___PH_LBL0
#define ___PH_LBL0 127
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_string_2d_hash)
___DEF_P_HLBL(___L1_srfi_2f_69_23_string_2d_hash)
___DEF_P_HLBL(___L2_srfi_2f_69_23_string_2d_hash)
___DEF_P_HLBL(___L3_srfi_2f_69_23_string_2d_hash)
___DEF_P_HLBL(___L4_srfi_2f_69_23_string_2d_hash)
___DEF_P_HLBL(___L5_srfi_2f_69_23_string_2d_hash)
___DEF_P_HLBL(___L6_srfi_2f_69_23_string_2d_hash)
___DEF_P_HLBL(___L7_srfi_2f_69_23_string_2d_hash)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_string_2d_hash)
   ___IF_NARGS_EQ(1,___SET_R2(___ABSENT))
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,1,1,0)
___DEF_GLBL(___L_srfi_2f_69_23_string_2d_hash)
   ___IF(___NOT(___STRINGP(___R1)))
   ___GOTO(___L16_srfi_2f_69_23_string_2d_hash)
   ___END_IF
   ___IF(___NOT(___EQP(___R2,___ABSENT)))
   ___GOTO(___L8_srfi_2f_69_23_string_2d_hash)
   ___END_IF
   ___SET_R3(___FIX(536870911L))
   ___IF(___FIXNUMP(___R3))
   ___GOTO(___L9_srfi_2f_69_23_string_2d_hash)
   ___END_IF
   ___GOTO(___L14_srfi_2f_69_23_string_2d_hash)
___DEF_GLBL(___L8_srfi_2f_69_23_string_2d_hash)
   ___SET_R3(___R2)
   ___IF(___NOT(___FIXNUMP(___R3)))
   ___GOTO(___L14_srfi_2f_69_23_string_2d_hash)
   ___END_IF
___DEF_GLBL(___L9_srfi_2f_69_23_string_2d_hash)
   ___IF(___NOT(___FIXNEGATIVEP(___R3)))
   ___GOTO(___L11_srfi_2f_69_23_string_2d_hash)
   ___END_IF
   ___SET_STK(1,___CNS(25))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_string_2d_hash)
___DEF_GLBL(___L10_srfi_2f_69_23_string_2d_hash)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),36,___G__23__23_raise_2d_range_2d_exception)
___DEF_GLBL(___L11_srfi_2f_69_23_string_2d_hash)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R3)
   ___ADJFP(8)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_string_2d_hash)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),39,___G__23__23_string_3d__3f__2d_hash)
___DEF_SLBL(3,___L3_srfi_2f_69_23_string_2d_hash)
   ___IF(___NOT(___FIXNUMP(___STK(-6))))
   ___GOTO(___L12_srfi_2f_69_23_string_2d_hash)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L12_srfi_2f_69_23_string_2d_hash)
   ___END_IF
   ___IF(___NOT(___EQP(___STK(-6),___FIX(0L))))
   ___GOTO(___L13_srfi_2f_69_23_string_2d_hash)
   ___END_IF
___DEF_GLBL(___L12_srfi_2f_69_23_string_2d_hash)
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(4)
___DEF_SLBL(4,___L4_srfi_2f_69_23_string_2d_hash)
   ___ADJFP(-8)
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23_modulo)
___DEF_GLBL(___L13_srfi_2f_69_23_string_2d_hash)
   ___SET_R1(___FIXMOD(___R1,___STK(-6)))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L14_srfi_2f_69_23_string_2d_hash)
   ___IF(___NOT(___BIGNUMP(___R3)))
   ___GOTO(___L15_srfi_2f_69_23_string_2d_hash)
   ___END_IF
   ___SET_STK(1,___CNS(26))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(5)
___DEF_SLBL(5,___L5_srfi_2f_69_23_string_2d_hash)
   ___GOTO(___L10_srfi_2f_69_23_string_2d_hash)
___DEF_GLBL(___L15_srfi_2f_69_23_string_2d_hash)
   ___SET_STK(1,___CNS(27))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(6)
___DEF_SLBL(6,___L6_srfi_2f_69_23_string_2d_hash)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),28,___G__23__23_fail_2d_check_2d_exact_2d_integer)
___DEF_GLBL(___L16_srfi_2f_69_23_string_2d_hash)
   ___SET_STK(1,___CNS(28))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(7)
___DEF_SLBL(7,___L7_srfi_2f_69_23_string_2d_hash)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),30,___G__23__23_fail_2d_check_2d_string)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_string_2d_ci_2d_hash
#undef ___PH_LBL0
#define ___PH_LBL0 136
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_P_HLBL(___L1_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_P_HLBL(___L2_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_P_HLBL(___L3_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_P_HLBL(___L4_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_P_HLBL(___L5_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_P_HLBL(___L6_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_P_HLBL(___L7_srfi_2f_69_23_string_2d_ci_2d_hash)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___IF_NARGS_EQ(1,___SET_R2(___ABSENT))
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,1,1,0)
___DEF_GLBL(___L_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___IF(___NOT(___STRINGP(___R1)))
   ___GOTO(___L16_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___END_IF
   ___IF(___NOT(___EQP(___R2,___ABSENT)))
   ___GOTO(___L8_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___END_IF
   ___SET_R3(___FIX(536870911L))
   ___IF(___FIXNUMP(___R3))
   ___GOTO(___L9_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___END_IF
   ___GOTO(___L14_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_GLBL(___L8_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___SET_R3(___R2)
   ___IF(___NOT(___FIXNUMP(___R3)))
   ___GOTO(___L14_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___END_IF
___DEF_GLBL(___L9_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___IF(___NOT(___FIXNEGATIVEP(___R3)))
   ___GOTO(___L11_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___END_IF
   ___SET_STK(1,___CNS(29))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_GLBL(___L10_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),36,___G__23__23_raise_2d_range_2d_exception)
___DEF_GLBL(___L11_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R3)
   ___ADJFP(8)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),38,___G__23__23_string_2d_ci_3d__3f__2d_hash)
___DEF_SLBL(3,___L3_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___IF(___NOT(___FIXNUMP(___STK(-6))))
   ___GOTO(___L12_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L12_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___END_IF
   ___IF(___NOT(___EQP(___STK(-6),___FIX(0L))))
   ___GOTO(___L13_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___END_IF
___DEF_GLBL(___L12_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(4)
___DEF_SLBL(4,___L4_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___ADJFP(-8)
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23_modulo)
___DEF_GLBL(___L13_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___SET_R1(___FIXMOD(___R1,___STK(-6)))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L14_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___IF(___NOT(___BIGNUMP(___R3)))
   ___GOTO(___L15_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___END_IF
   ___SET_STK(1,___CNS(30))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(5)
___DEF_SLBL(5,___L5_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___GOTO(___L10_srfi_2f_69_23_string_2d_ci_2d_hash)
___DEF_GLBL(___L15_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___SET_STK(1,___CNS(31))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(6)
___DEF_SLBL(6,___L6_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),28,___G__23__23_fail_2d_check_2d_exact_2d_integer)
___DEF_GLBL(___L16_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___SET_STK(1,___CNS(32))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(7)
___DEF_SLBL(7,___L7_srfi_2f_69_23_string_2d_ci_2d_hash)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),30,___G__23__23_fail_2d_check_2d_string)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_srfi_2f_69_23_hash_2d_by_2d_identity
#undef ___PH_LBL0
#define ___PH_LBL0 145
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_P_HLBL(___L1_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_P_HLBL(___L2_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_P_HLBL(___L3_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_P_HLBL(___L4_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_P_HLBL(___L5_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_P_HLBL(___L6_srfi_2f_69_23_hash_2d_by_2d_identity)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___IF_NARGS_EQ(1,___SET_R2(___ABSENT))
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,1,1,0)
___DEF_GLBL(___L_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___IF(___NOT(___EQP(___R2,___ABSENT)))
   ___GOTO(___L7_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___END_IF
   ___SET_R3(___FIX(536870911L))
   ___IF(___FIXNUMP(___R3))
   ___GOTO(___L8_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___END_IF
   ___GOTO(___L13_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_GLBL(___L7_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___SET_R3(___R2)
   ___IF(___NOT(___FIXNUMP(___R3)))
   ___GOTO(___L13_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___END_IF
___DEF_GLBL(___L8_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___IF(___NOT(___FIXNEGATIVEP(___R3)))
   ___GOTO(___L10_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___END_IF
   ___SET_STK(1,___CNS(33))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_GLBL(___L9_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),36,___G__23__23_raise_2d_range_2d_exception)
___DEF_GLBL(___L10_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R3)
   ___ADJFP(8)
   ___POLL(2)
___DEF_SLBL(2,___L2_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),26,___G__23__23_eq_3f__2d_hash)
___DEF_SLBL(3,___L3_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___IF(___NOT(___FIXNUMP(___STK(-6))))
   ___GOTO(___L11_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L11_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___END_IF
   ___IF(___NOT(___EQP(___STK(-6),___FIX(0L))))
   ___GOTO(___L12_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___END_IF
___DEF_GLBL(___L11_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(4)
___DEF_SLBL(4,___L4_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___ADJFP(-8)
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23_modulo)
___DEF_GLBL(___L12_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___SET_R1(___FIXMOD(___R1,___STK(-6)))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L13_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___IF(___NOT(___BIGNUMP(___R3)))
   ___GOTO(___L14_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___END_IF
   ___SET_STK(1,___CNS(34))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(5)
___DEF_SLBL(5,___L5_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___GOTO(___L9_srfi_2f_69_23_hash_2d_by_2d_identity)
___DEF_GLBL(___L14_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___SET_STK(1,___CNS(35))
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___LBL(0))
   ___ADJFP(1)
   ___POLL(6)
___DEF_SLBL(6,___L6_srfi_2f_69_23_hash_2d_by_2d_identity)
   ___JUMPGLONOTSAFE(___SET_NARGS(4),28,___G__23__23_fail_2d_check_2d_exact_2d_integer)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_srfi_2f_69_23_,___REF_SYM(18,___S_srfi_2f_69_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_,0,-1)
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_3f_,___REF_SYM(39,___S_srfi_2f_69_23_hash_2d_table_3f_),___REF_FAL,2,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_3f_,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_3f_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_make_2d_hash_2d_table,___REF_SYM(40,___S_srfi_2f_69_23_make_2d_hash_2d_table),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_make_2d_hash_2d_table,2,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_make_2d_hash_2d_table,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_make_2d_hash_2d_table,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_make_2d_hash_2d_table,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_make_2d_hash_2d_table,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_make_2d_hash_2d_table,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_alist_2d__3e_hash_2d_table,___REF_SYM(19,___S_srfi_2f_69_23_alist_2d__3e_hash_2d_table),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_alist_2d__3e_hash_2d_table,3,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_alist_2d__3e_hash_2d_table,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_alist_2d__3e_hash_2d_table,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_alist_2d__3e_hash_2d_table,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_alist_2d__3e_hash_2d_table,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_alist_2d__3e_hash_2d_table,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function,___REF_SYM(25,___S_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function),___REF_FAL,2,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function,___REF_SYM(28,___S_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function),___REF_FAL,2,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_ref,___REF_SYM(31,___S_srfi_2f_69_23_hash_2d_table_2d_ref),___REF_FAL,7,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_ref,3,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_ref,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_ref,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_ref,___IFD(___RETI,8,8,0x3f01L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_ref,___IFD(___RETI,8,8,0x3f08L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_ref,___IFD(___RETI,8,8,0x3f03L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_ref,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default,___REF_SYM(32,___S_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default),___REF_FAL,3,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default,3,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_set_21_,___REF_SYM(33,___S_srfi_2f_69_23_hash_2d_table_2d_set_21_),___REF_FAL,3,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_set_21_,3,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_set_21_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_set_21_,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_delete_21_,___REF_SYM(24,___S_srfi_2f_69_23_hash_2d_table_2d_delete_21_),___REF_FAL,3,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_delete_21_,2,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_delete_21_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_delete_21_,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_exists_3f_,___REF_SYM(26,___S_srfi_2f_69_23_hash_2d_table_2d_exists_3f_),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_exists_3f_,2,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_exists_3f_,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_exists_3f_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_exists_3f_,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_update_21_,___REF_SYM(35,___S_srfi_2f_69_23_hash_2d_table_2d_update_21_),___REF_FAL,9,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_update_21_,4,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21_,___IFD(___RETI,8,1,0x3f1fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21_,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21_,___IFD(___RETI,8,8,0x3f03L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21_,___IFD(___RETN,5,1,0xfL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21_,___IFD(___RETN,5,1,0x7L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21_,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21_,___IFD(___RETI,3,4,0x3f7L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21_,___IFD(___RETI,3,4,0x3f7L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,___REF_SYM(36,___S_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default),___REF_FAL,7,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,4,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,___IFD(___RETI,8,1,0x3f0fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,___IFD(___RETN,5,1,0xfL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,___IFD(___RETN,5,1,0x7L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,___IFD(___RETI,3,4,0x3f7L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,___IFD(___RETI,3,4,0x3f7L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_size,___REF_SYM(34,___S_srfi_2f_69_23_hash_2d_table_2d_size),___REF_FAL,3,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_size,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_size,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_size,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_keys,___REF_SYM(29,___S_srfi_2f_69_23_hash_2d_table_2d_keys),___REF_FAL,8,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_keys,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_keys,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_keys,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_keys,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_keys,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_keys,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_keys,___IFD(___RETI,1,0,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_keys,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_values,___REF_SYM(37,___S_srfi_2f_69_23_hash_2d_table_2d_values),___REF_FAL,8,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_values,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_values,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_values,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_values,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_values,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_values,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_values,___IFD(___RETI,1,0,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_values,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_walk,___REF_SYM(38,___S_srfi_2f_69_23_hash_2d_table_2d_walk),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_walk,2,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_walk,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_walk,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_walk,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_fold,___REF_SYM(27,___S_srfi_2f_69_23_hash_2d_table_2d_fold),___REF_FAL,9,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_fold,3,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_fold,___IFD(___RETI,8,1,0x3f07L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_fold,___IFD(___RETI,8,1,0x3f07L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_fold,___IFD(___RETN,5,1,0x7L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_fold,___IFD(___RETI,8,8,0x3f01L))
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_fold,2,1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_fold,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_fold,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_fold,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d__3e_alist,___REF_SYM(22,___S_srfi_2f_69_23_hash_2d_table_2d__3e_alist),___REF_FAL,3,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d__3e_alist,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d__3e_alist,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d__3e_alist,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_copy,___REF_SYM(23,___S_srfi_2f_69_23_hash_2d_table_2d_copy),___REF_FAL,3,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_copy,1,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_copy,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_copy,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_table_2d_merge_21_,___REF_SYM(30,___S_srfi_2f_69_23_hash_2d_table_2d_merge_21_),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_table_2d_merge_21_,2,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_merge_21_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_merge_21_,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_table_2d_merge_21_,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash,___REF_SYM(20,___S_srfi_2f_69_23_hash),___REF_FAL,7,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash,2,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_string_2d_hash,___REF_SYM(42,___S_srfi_2f_69_23_string_2d_hash),___REF_FAL,8,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_string_2d_hash,2,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_hash,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_hash,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_hash,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_hash,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_hash,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_hash,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_hash,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_string_2d_ci_2d_hash,___REF_SYM(41,___S_srfi_2f_69_23_string_2d_ci_2d_hash),___REF_FAL,8,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_string_2d_ci_2d_hash,2,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_ci_2d_hash,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_ci_2d_hash,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_ci_2d_hash,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_ci_2d_hash,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_ci_2d_hash,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_ci_2d_hash,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_string_2d_ci_2d_hash,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H_srfi_2f_69_23_hash_2d_by_2d_identity,___REF_SYM(21,___S_srfi_2f_69_23_hash_2d_by_2d_identity),___REF_FAL,7,0)
,___DEF_LBL_PROC(___H_srfi_2f_69_23_hash_2d_by_2d_identity,2,-1)
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_by_2d_identity,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_by_2d_identity,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_by_2d_identity,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_by_2d_identity,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_by_2d_identity,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H_srfi_2f_69_23_hash_2d_by_2d_identity,___IFD(___RETI,1,4,0x3f1L))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(0,___G_srfi_2f_69_23_,1)
___DEF_MOD_PRM(21,___G_srfi_2f_69_23_hash_2d_table_3f_,3)
___DEF_MOD_PRM(22,___G_srfi_2f_69_23_make_2d_hash_2d_table,6)
___DEF_MOD_PRM(1,___G_srfi_2f_69_23_alist_2d__3e_hash_2d_table,13)
___DEF_MOD_PRM(7,___G_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function,20)
___DEF_MOD_PRM(10,___G_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function,23)
___DEF_MOD_PRM(13,___G_srfi_2f_69_23_hash_2d_table_2d_ref,26)
___DEF_MOD_PRM(14,___G_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default,34)
___DEF_MOD_PRM(15,___G_srfi_2f_69_23_hash_2d_table_2d_set_21_,38)
___DEF_MOD_PRM(6,___G_srfi_2f_69_23_hash_2d_table_2d_delete_21_,42)
___DEF_MOD_PRM(8,___G_srfi_2f_69_23_hash_2d_table_2d_exists_3f_,46)
___DEF_MOD_PRM(17,___G_srfi_2f_69_23_hash_2d_table_2d_update_21_,51)
___DEF_MOD_PRM(18,___G_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,61)
___DEF_MOD_PRM(16,___G_srfi_2f_69_23_hash_2d_table_2d_size,69)
___DEF_MOD_PRM(11,___G_srfi_2f_69_23_hash_2d_table_2d_keys,73)
___DEF_MOD_PRM(19,___G_srfi_2f_69_23_hash_2d_table_2d_values,82)
___DEF_MOD_PRM(20,___G_srfi_2f_69_23_hash_2d_table_2d_walk,91)
___DEF_MOD_PRM(9,___G_srfi_2f_69_23_hash_2d_table_2d_fold,96)
___DEF_MOD_PRM(4,___G_srfi_2f_69_23_hash_2d_table_2d__3e_alist,106)
___DEF_MOD_PRM(5,___G_srfi_2f_69_23_hash_2d_table_2d_copy,110)
___DEF_MOD_PRM(12,___G_srfi_2f_69_23_hash_2d_table_2d_merge_21_,114)
___DEF_MOD_PRM(2,___G_srfi_2f_69_23_hash,119)
___DEF_MOD_PRM(24,___G_srfi_2f_69_23_string_2d_hash,127)
___DEF_MOD_PRM(23,___G_srfi_2f_69_23_string_2d_ci_2d_hash,136)
___DEF_MOD_PRM(3,___G_srfi_2f_69_23_hash_2d_by_2d_identity,145)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(0,___G_srfi_2f_69_23_,1)
___DEF_MOD_GLO(21,___G_srfi_2f_69_23_hash_2d_table_3f_,3)
___DEF_MOD_GLO(22,___G_srfi_2f_69_23_make_2d_hash_2d_table,6)
___DEF_MOD_GLO(1,___G_srfi_2f_69_23_alist_2d__3e_hash_2d_table,13)
___DEF_MOD_GLO(7,___G_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function,20)
___DEF_MOD_GLO(10,___G_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function,23)
___DEF_MOD_GLO(13,___G_srfi_2f_69_23_hash_2d_table_2d_ref,26)
___DEF_MOD_GLO(14,___G_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default,34)
___DEF_MOD_GLO(15,___G_srfi_2f_69_23_hash_2d_table_2d_set_21_,38)
___DEF_MOD_GLO(6,___G_srfi_2f_69_23_hash_2d_table_2d_delete_21_,42)
___DEF_MOD_GLO(8,___G_srfi_2f_69_23_hash_2d_table_2d_exists_3f_,46)
___DEF_MOD_GLO(17,___G_srfi_2f_69_23_hash_2d_table_2d_update_21_,51)
___DEF_MOD_GLO(18,___G_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,61)
___DEF_MOD_GLO(16,___G_srfi_2f_69_23_hash_2d_table_2d_size,69)
___DEF_MOD_GLO(11,___G_srfi_2f_69_23_hash_2d_table_2d_keys,73)
___DEF_MOD_GLO(19,___G_srfi_2f_69_23_hash_2d_table_2d_values,82)
___DEF_MOD_GLO(20,___G_srfi_2f_69_23_hash_2d_table_2d_walk,91)
___DEF_MOD_GLO(9,___G_srfi_2f_69_23_hash_2d_table_2d_fold,96)
___DEF_MOD_GLO(4,___G_srfi_2f_69_23_hash_2d_table_2d__3e_alist,106)
___DEF_MOD_GLO(5,___G_srfi_2f_69_23_hash_2d_table_2d_copy,110)
___DEF_MOD_GLO(12,___G_srfi_2f_69_23_hash_2d_table_2d_merge_21_,114)
___DEF_MOD_GLO(2,___G_srfi_2f_69_23_hash,119)
___DEF_MOD_GLO(24,___G_srfi_2f_69_23_string_2d_hash,127)
___DEF_MOD_GLO(23,___G_srfi_2f_69_23_string_2d_ci_2d_hash,136)
___DEF_MOD_GLO(3,___G_srfi_2f_69_23_hash_2d_by_2d_identity,145)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S__23__23_type_2d_5,"##type-5")
___DEF_MOD_SYM(1,___S__23__23_type_2d_6_2d_F3F63A41_2d_2974_2d_4D41_2d_8B24_2d_1744E866741D,"##type-6-F3F63A41-2974-4D41-8B24-1744E866741D")

___DEF_MOD_SYM(2,___S_bound,"bound")
___DEF_MOD_SYM(3,___S_f,"f")
___DEF_MOD_SYM(4,___S_fields,"fields")
___DEF_MOD_SYM(5,___S_flags,"flags")
___DEF_MOD_SYM(6,___S_func,"func")
___DEF_MOD_SYM(7,___S_gcht,"gcht")
___DEF_MOD_SYM(8,___S_hash,"hash")
___DEF_MOD_SYM(9,___S_ht,"ht")
___DEF_MOD_SYM(10,___S_ht1,"ht1")
___DEF_MOD_SYM(11,___S_ht2,"ht2")
___DEF_MOD_SYM(12,___S_id,"id")
___DEF_MOD_SYM(13,___S_init,"init")
___DEF_MOD_SYM(14,___S_loads,"loads")
___DEF_MOD_SYM(15,___S_name,"name")
___DEF_MOD_SYM(16,___S_proc,"proc")
___DEF_MOD_SYM(17,___S_srfi_2f_69,"srfi/69")
___DEF_MOD_SYM(18,___S_srfi_2f_69_23_,"srfi/69#")
___DEF_MOD_SYM(19,___S_srfi_2f_69_23_alist_2d__3e_hash_2d_table,"srfi/69#alist->hash-table")
___DEF_MOD_SYM(20,___S_srfi_2f_69_23_hash,"srfi/69#hash")
___DEF_MOD_SYM(21,___S_srfi_2f_69_23_hash_2d_by_2d_identity,"srfi/69#hash-by-identity")
___DEF_MOD_SYM(22,___S_srfi_2f_69_23_hash_2d_table_2d__3e_alist,"srfi/69#hash-table->alist")
___DEF_MOD_SYM(23,___S_srfi_2f_69_23_hash_2d_table_2d_copy,"srfi/69#hash-table-copy")
___DEF_MOD_SYM(24,___S_srfi_2f_69_23_hash_2d_table_2d_delete_21_,"srfi/69#hash-table-delete!")
___DEF_MOD_SYM(25,___S_srfi_2f_69_23_hash_2d_table_2d_equivalence_2d_function,"srfi/69#hash-table-equivalence-function")

___DEF_MOD_SYM(26,___S_srfi_2f_69_23_hash_2d_table_2d_exists_3f_,"srfi/69#hash-table-exists?")
___DEF_MOD_SYM(27,___S_srfi_2f_69_23_hash_2d_table_2d_fold,"srfi/69#hash-table-fold")
___DEF_MOD_SYM(28,___S_srfi_2f_69_23_hash_2d_table_2d_hash_2d_function,"srfi/69#hash-table-hash-function")

___DEF_MOD_SYM(29,___S_srfi_2f_69_23_hash_2d_table_2d_keys,"srfi/69#hash-table-keys")
___DEF_MOD_SYM(30,___S_srfi_2f_69_23_hash_2d_table_2d_merge_21_,"srfi/69#hash-table-merge!")
___DEF_MOD_SYM(31,___S_srfi_2f_69_23_hash_2d_table_2d_ref,"srfi/69#hash-table-ref")
___DEF_MOD_SYM(32,___S_srfi_2f_69_23_hash_2d_table_2d_ref_2f_default,"srfi/69#hash-table-ref/default")

___DEF_MOD_SYM(33,___S_srfi_2f_69_23_hash_2d_table_2d_set_21_,"srfi/69#hash-table-set!")
___DEF_MOD_SYM(34,___S_srfi_2f_69_23_hash_2d_table_2d_size,"srfi/69#hash-table-size")
___DEF_MOD_SYM(35,___S_srfi_2f_69_23_hash_2d_table_2d_update_21_,"srfi/69#hash-table-update!")
___DEF_MOD_SYM(36,___S_srfi_2f_69_23_hash_2d_table_2d_update_21__2f_default,"srfi/69#hash-table-update!/default")

___DEF_MOD_SYM(37,___S_srfi_2f_69_23_hash_2d_table_2d_values,"srfi/69#hash-table-values")
___DEF_MOD_SYM(38,___S_srfi_2f_69_23_hash_2d_table_2d_walk,"srfi/69#hash-table-walk")
___DEF_MOD_SYM(39,___S_srfi_2f_69_23_hash_2d_table_3f_,"srfi/69#hash-table?")
___DEF_MOD_SYM(40,___S_srfi_2f_69_23_make_2d_hash_2d_table,"srfi/69#make-hash-table")
___DEF_MOD_SYM(41,___S_srfi_2f_69_23_string_2d_ci_2d_hash,"srfi/69#string-ci-hash")
___DEF_MOD_SYM(42,___S_srfi_2f_69_23_string_2d_hash,"srfi/69#string-hash")
___DEF_MOD_SYM(43,___S_string,"string")
___DEF_MOD_SYM(44,___S_super,"super")
___DEF_MOD_SYM(45,___S_table,"table")
___DEF_MOD_SYM(46,___S_test,"test")
___DEF_MOD_SYM(47,___S_type,"type")
___DEF_MOD_KEY(0,___K_hash,"hash")
___DEF_MOD_KEY(1,___K_test,"test")
___END_MOD_SYM_KEY

#endif
