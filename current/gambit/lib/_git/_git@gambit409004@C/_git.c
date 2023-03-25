#ifdef ___LINKER_INFO
; File: "_git.c", produced by Gambit v4.9.4
(
409004
(C)
"_git"
("_git")
("_git" "_tar")
(("_git"))
( #|*/"*/"symbols|#
"##type-1-AF9B3B94-EE56-4D95-A323-AEE3C97E70FC"
"##type-5"
"_git"
"_git#"
"_git#git-archive"
"_git#git-clone"
"_git#git-command"
"_git#git-command-aux"
"_git#git-pull"
"_git#git-repository-open"
"_git#git-status"
"_git#git-tag"
"_tar"
"directory"
"fields"
"flags"
"git-repository"
"id"
"name"
"path"
"super"
"type"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
"arguments"
"directory"
"environment"
"path"
"pseudo-terminal"
"stderr-redirection"
"stdin-redirection"
"stdout-redirection"
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"_git#"
"_git#git-command-aux"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"_git#git-archive"
"_git#git-clone"
"_git#git-command"
"_git#git-pull"
"_git#git-repository-open"
"_git#git-status"
"_git#git-tag"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##="
"##call-with-input-process"
"##fail-check-procedure"
"##fail-check-string"
"##fail-check-string-list"
"##file-exists?"
"##file-info"
"##file-info-type"
"##newline1"
"##os-environ"
"##path-expand"
"##process-status"
"##read-line"
"##repl"
"##reverse"
"##tty-mode-reset"
"##write-string"
"_git#d"
"_tar#tar-unpack-port"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409004
#define ___MODULE_NAME "_git"
#define ___LINKER_ID ___LNK___git_2e_o1
#define ___MH_PROC ___H___git
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 22
#define ___KEYCOUNT 8
#define ___GLOCOUNT 28
#define ___SUPCOUNT 9
#define ___SUBCOUNT 16
#define ___LBLCOUNT 107
#define ___MODDESCR ___REF_SUB(13)
#include "gambit.h"

___NEED_SYM(___S__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC)
___NEED_SYM(___S__23__23_type_2d_5)
___NEED_SYM(___S___git)
___NEED_SYM(___S___git_23_)
___NEED_SYM(___S___git_23_git_2d_archive)
___NEED_SYM(___S___git_23_git_2d_clone)
___NEED_SYM(___S___git_23_git_2d_command)
___NEED_SYM(___S___git_23_git_2d_command_2d_aux)
___NEED_SYM(___S___git_23_git_2d_pull)
___NEED_SYM(___S___git_23_git_2d_repository_2d_open)
___NEED_SYM(___S___git_23_git_2d_status)
___NEED_SYM(___S___git_23_git_2d_tag)
___NEED_SYM(___S___tar)
___NEED_SYM(___S_directory)
___NEED_SYM(___S_fields)
___NEED_SYM(___S_flags)
___NEED_SYM(___S_git_2d_repository)
___NEED_SYM(___S_id)
___NEED_SYM(___S_name)
___NEED_SYM(___S_path)
___NEED_SYM(___S_super)
___NEED_SYM(___S_type)

___NEED_KEY(___K_arguments)
___NEED_KEY(___K_directory)
___NEED_KEY(___K_environment)
___NEED_KEY(___K_path)
___NEED_KEY(___K_pseudo_2d_terminal)
___NEED_KEY(___K_stderr_2d_redirection)
___NEED_KEY(___K_stdin_2d_redirection)
___NEED_KEY(___K_stdout_2d_redirection)

___NEED_GLO(___G__23__23__3d_)
___NEED_GLO(___G__23__23_call_2d_with_2d_input_2d_process)
___NEED_GLO(___G__23__23_fail_2d_check_2d_procedure)
___NEED_GLO(___G__23__23_fail_2d_check_2d_string)
___NEED_GLO(___G__23__23_fail_2d_check_2d_string_2d_list)
___NEED_GLO(___G__23__23_file_2d_exists_3f_)
___NEED_GLO(___G__23__23_file_2d_info)
___NEED_GLO(___G__23__23_file_2d_info_2d_type)
___NEED_GLO(___G__23__23_newline1)
___NEED_GLO(___G__23__23_os_2d_environ)
___NEED_GLO(___G__23__23_path_2d_expand)
___NEED_GLO(___G__23__23_process_2d_status)
___NEED_GLO(___G__23__23_read_2d_line)
___NEED_GLO(___G__23__23_repl)
___NEED_GLO(___G__23__23_reverse)
___NEED_GLO(___G__23__23_tty_2d_mode_2d_reset)
___NEED_GLO(___G__23__23_write_2d_string)
___NEED_GLO(___G___git_23_)
___NEED_GLO(___G___git_23_d)
___NEED_GLO(___G___git_23_git_2d_archive)
___NEED_GLO(___G___git_23_git_2d_clone)
___NEED_GLO(___G___git_23_git_2d_command)
___NEED_GLO(___G___git_23_git_2d_command_2d_aux)
___NEED_GLO(___G___git_23_git_2d_pull)
___NEED_GLO(___G___git_23_git_2d_repository_2d_open)
___NEED_GLO(___G___git_23_git_2d_status)
___NEED_GLO(___G___git_23_git_2d_tag)
___NEED_GLO(___G___tar_23_tar_2d_unpack_2d_port)

___BEGIN_SYM
___DEF_SYM(0,___S__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC,"##type-1-AF9B3B94-EE56-4D95-A323-AEE3C97E70FC")

___DEF_SYM(1,___S__23__23_type_2d_5,"##type-5")
___DEF_SYM(2,___S___git,"_git")
___DEF_SYM(3,___S___git_23_,"_git#")
___DEF_SYM(4,___S___git_23_git_2d_archive,"_git#git-archive")
___DEF_SYM(5,___S___git_23_git_2d_clone,"_git#git-clone")
___DEF_SYM(6,___S___git_23_git_2d_command,"_git#git-command")
___DEF_SYM(7,___S___git_23_git_2d_command_2d_aux,"_git#git-command-aux")
___DEF_SYM(8,___S___git_23_git_2d_pull,"_git#git-pull")
___DEF_SYM(9,___S___git_23_git_2d_repository_2d_open,"_git#git-repository-open")
___DEF_SYM(10,___S___git_23_git_2d_status,"_git#git-status")
___DEF_SYM(11,___S___git_23_git_2d_tag,"_git#git-tag")
___DEF_SYM(12,___S___tar,"_tar")
___DEF_SYM(13,___S_directory,"directory")
___DEF_SYM(14,___S_fields,"fields")
___DEF_SYM(15,___S_flags,"flags")
___DEF_SYM(16,___S_git_2d_repository,"git-repository")
___DEF_SYM(17,___S_id,"id")
___DEF_SYM(18,___S_name,"name")
___DEF_SYM(19,___S_path,"path")
___DEF_SYM(20,___S_super,"super")
___DEF_SYM(21,___S_type,"type")
___END_SYM

#define ___SYM__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC ___SYM(0,___S__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC)
#define ___SYM__23__23_type_2d_5 ___SYM(1,___S__23__23_type_2d_5)
#define ___SYM___git ___SYM(2,___S___git)
#define ___SYM___git_23_ ___SYM(3,___S___git_23_)
#define ___SYM___git_23_git_2d_archive ___SYM(4,___S___git_23_git_2d_archive)
#define ___SYM___git_23_git_2d_clone ___SYM(5,___S___git_23_git_2d_clone)
#define ___SYM___git_23_git_2d_command ___SYM(6,___S___git_23_git_2d_command)
#define ___SYM___git_23_git_2d_command_2d_aux ___SYM(7,___S___git_23_git_2d_command_2d_aux)
#define ___SYM___git_23_git_2d_pull ___SYM(8,___S___git_23_git_2d_pull)
#define ___SYM___git_23_git_2d_repository_2d_open ___SYM(9,___S___git_23_git_2d_repository_2d_open)
#define ___SYM___git_23_git_2d_status ___SYM(10,___S___git_23_git_2d_status)
#define ___SYM___git_23_git_2d_tag ___SYM(11,___S___git_23_git_2d_tag)
#define ___SYM___tar ___SYM(12,___S___tar)
#define ___SYM_directory ___SYM(13,___S_directory)
#define ___SYM_fields ___SYM(14,___S_fields)
#define ___SYM_flags ___SYM(15,___S_flags)
#define ___SYM_git_2d_repository ___SYM(16,___S_git_2d_repository)
#define ___SYM_id ___SYM(17,___S_id)
#define ___SYM_name ___SYM(18,___S_name)
#define ___SYM_path ___SYM(19,___S_path)
#define ___SYM_super ___SYM(20,___S_super)
#define ___SYM_type ___SYM(21,___S_type)

___BEGIN_KEY
___DEF_KEY(0,___K_arguments,"arguments")
___DEF_KEY(1,___K_directory,"directory")
___DEF_KEY(2,___K_environment,"environment")
___DEF_KEY(3,___K_path,"path")
___DEF_KEY(4,___K_pseudo_2d_terminal,"pseudo-terminal")
___DEF_KEY(5,___K_stderr_2d_redirection,"stderr-redirection")
___DEF_KEY(6,___K_stdin_2d_redirection,"stdin-redirection")
___DEF_KEY(7,___K_stdout_2d_redirection,"stdout-redirection")
___END_KEY

#define ___KEY_arguments ___KEY(0,___K_arguments)
#define ___KEY_directory ___KEY(1,___K_directory)
#define ___KEY_environment ___KEY(2,___K_environment)
#define ___KEY_path ___KEY(3,___K_path)
#define ___KEY_pseudo_2d_terminal ___KEY(4,___K_pseudo_2d_terminal)
#define ___KEY_stderr_2d_redirection ___KEY(5,___K_stderr_2d_redirection)
#define ___KEY_stdin_2d_redirection ___KEY(6,___K_stdin_2d_redirection)
#define ___KEY_stdout_2d_redirection ___KEY(7,___K_stdout_2d_redirection)

___BEGIN_GLO
___DEF_GLO(0,"_git#")
___DEF_GLO(1,"_git#git-archive")
___DEF_GLO(2,"_git#git-clone")
___DEF_GLO(3,"_git#git-command")
___DEF_GLO(4,"_git#git-command-aux")
___DEF_GLO(5,"_git#git-pull")
___DEF_GLO(6,"_git#git-repository-open")
___DEF_GLO(7,"_git#git-status")
___DEF_GLO(8,"_git#git-tag")
___DEF_GLO(9,"##=")
___DEF_GLO(10,"##call-with-input-process")
___DEF_GLO(11,"##fail-check-procedure")
___DEF_GLO(12,"##fail-check-string")
___DEF_GLO(13,"##fail-check-string-list")
___DEF_GLO(14,"##file-exists?")
___DEF_GLO(15,"##file-info")
___DEF_GLO(16,"##file-info-type")
___DEF_GLO(17,"##newline1")
___DEF_GLO(18,"##os-environ")
___DEF_GLO(19,"##path-expand")
___DEF_GLO(20,"##process-status")
___DEF_GLO(21,"##read-line")
___DEF_GLO(22,"##repl")
___DEF_GLO(23,"##reverse")
___DEF_GLO(24,"##tty-mode-reset")
___DEF_GLO(25,"##write-string")
___DEF_GLO(26,"_git#d")
___DEF_GLO(27,"_tar#tar-unpack-port")
___END_GLO

#define ___GLO___git_23_ ___GLO(0,___G___git_23_)
#define ___PRM___git_23_ ___PRM(0,___G___git_23_)
#define ___GLO___git_23_git_2d_archive ___GLO(1,___G___git_23_git_2d_archive)
#define ___PRM___git_23_git_2d_archive ___PRM(1,___G___git_23_git_2d_archive)
#define ___GLO___git_23_git_2d_clone ___GLO(2,___G___git_23_git_2d_clone)
#define ___PRM___git_23_git_2d_clone ___PRM(2,___G___git_23_git_2d_clone)
#define ___GLO___git_23_git_2d_command ___GLO(3,___G___git_23_git_2d_command)
#define ___PRM___git_23_git_2d_command ___PRM(3,___G___git_23_git_2d_command)
#define ___GLO___git_23_git_2d_command_2d_aux ___GLO(4,___G___git_23_git_2d_command_2d_aux)
#define ___PRM___git_23_git_2d_command_2d_aux ___PRM(4,___G___git_23_git_2d_command_2d_aux)
#define ___GLO___git_23_git_2d_pull ___GLO(5,___G___git_23_git_2d_pull)
#define ___PRM___git_23_git_2d_pull ___PRM(5,___G___git_23_git_2d_pull)
#define ___GLO___git_23_git_2d_repository_2d_open ___GLO(6,___G___git_23_git_2d_repository_2d_open)
#define ___PRM___git_23_git_2d_repository_2d_open ___PRM(6,___G___git_23_git_2d_repository_2d_open)
#define ___GLO___git_23_git_2d_status ___GLO(7,___G___git_23_git_2d_status)
#define ___PRM___git_23_git_2d_status ___PRM(7,___G___git_23_git_2d_status)
#define ___GLO___git_23_git_2d_tag ___GLO(8,___G___git_23_git_2d_tag)
#define ___PRM___git_23_git_2d_tag ___PRM(8,___G___git_23_git_2d_tag)
#define ___GLO__23__23__3d_ ___GLO(9,___G__23__23__3d_)
#define ___PRM__23__23__3d_ ___PRM(9,___G__23__23__3d_)
#define ___GLO__23__23_call_2d_with_2d_input_2d_process ___GLO(10,___G__23__23_call_2d_with_2d_input_2d_process)
#define ___PRM__23__23_call_2d_with_2d_input_2d_process ___PRM(10,___G__23__23_call_2d_with_2d_input_2d_process)
#define ___GLO__23__23_fail_2d_check_2d_procedure ___GLO(11,___G__23__23_fail_2d_check_2d_procedure)
#define ___PRM__23__23_fail_2d_check_2d_procedure ___PRM(11,___G__23__23_fail_2d_check_2d_procedure)
#define ___GLO__23__23_fail_2d_check_2d_string ___GLO(12,___G__23__23_fail_2d_check_2d_string)
#define ___PRM__23__23_fail_2d_check_2d_string ___PRM(12,___G__23__23_fail_2d_check_2d_string)
#define ___GLO__23__23_fail_2d_check_2d_string_2d_list ___GLO(13,___G__23__23_fail_2d_check_2d_string_2d_list)
#define ___PRM__23__23_fail_2d_check_2d_string_2d_list ___PRM(13,___G__23__23_fail_2d_check_2d_string_2d_list)
#define ___GLO__23__23_file_2d_exists_3f_ ___GLO(14,___G__23__23_file_2d_exists_3f_)
#define ___PRM__23__23_file_2d_exists_3f_ ___PRM(14,___G__23__23_file_2d_exists_3f_)
#define ___GLO__23__23_file_2d_info ___GLO(15,___G__23__23_file_2d_info)
#define ___PRM__23__23_file_2d_info ___PRM(15,___G__23__23_file_2d_info)
#define ___GLO__23__23_file_2d_info_2d_type ___GLO(16,___G__23__23_file_2d_info_2d_type)
#define ___PRM__23__23_file_2d_info_2d_type ___PRM(16,___G__23__23_file_2d_info_2d_type)
#define ___GLO__23__23_newline1 ___GLO(17,___G__23__23_newline1)
#define ___PRM__23__23_newline1 ___PRM(17,___G__23__23_newline1)
#define ___GLO__23__23_os_2d_environ ___GLO(18,___G__23__23_os_2d_environ)
#define ___PRM__23__23_os_2d_environ ___PRM(18,___G__23__23_os_2d_environ)
#define ___GLO__23__23_path_2d_expand ___GLO(19,___G__23__23_path_2d_expand)
#define ___PRM__23__23_path_2d_expand ___PRM(19,___G__23__23_path_2d_expand)
#define ___GLO__23__23_process_2d_status ___GLO(20,___G__23__23_process_2d_status)
#define ___PRM__23__23_process_2d_status ___PRM(20,___G__23__23_process_2d_status)
#define ___GLO__23__23_read_2d_line ___GLO(21,___G__23__23_read_2d_line)
#define ___PRM__23__23_read_2d_line ___PRM(21,___G__23__23_read_2d_line)
#define ___GLO__23__23_repl ___GLO(22,___G__23__23_repl)
#define ___PRM__23__23_repl ___PRM(22,___G__23__23_repl)
#define ___GLO__23__23_reverse ___GLO(23,___G__23__23_reverse)
#define ___PRM__23__23_reverse ___PRM(23,___G__23__23_reverse)
#define ___GLO__23__23_tty_2d_mode_2d_reset ___GLO(24,___G__23__23_tty_2d_mode_2d_reset)
#define ___PRM__23__23_tty_2d_mode_2d_reset ___PRM(24,___G__23__23_tty_2d_mode_2d_reset)
#define ___GLO__23__23_write_2d_string ___GLO(25,___G__23__23_write_2d_string)
#define ___PRM__23__23_write_2d_string ___PRM(25,___G__23__23_write_2d_string)
#define ___GLO___git_23_d ___GLO(26,___G___git_23_d)
#define ___PRM___git_23_d ___PRM(26,___G___git_23_d)
#define ___GLO___tar_23_tar_2d_unpack_2d_port ___GLO(27,___G___tar_23_tar_2d_unpack_2d_port)
#define ___PRM___tar_23_tar_2d_unpack_2d_port ___PRM(27,___G___tar_23_tar_2d_unpack_2d_port)

___DEF_SUB_STR(___X0,4UL)
               ___STR4(46,103,105,116)
___DEF_SUB_STRUCTURE(___X1,6UL)
               ___VEC1(___REF_SUB(2))
               ___VEC1(___REF_SYM(0,___S__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC))
               ___VEC1(___REF_SYM(16,___S_git_2d_repository))
               ___VEC1(___REF_FIX(29))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(4))
               ___VEC0
___DEF_SUB_STRUCTURE(___X2,6UL)
               ___VEC1(___REF_SUB(2))
               ___VEC1(___REF_SYM(1,___S__23__23_type_2d_5))
               ___VEC1(___REF_SYM(21,___S_type))
               ___VEC1(___REF_FIX(8))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(3))
               ___VEC0
___DEF_SUB_VEC(___X3,15UL)
               ___VEC1(___REF_SYM(17,___S_id))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(18,___S_name))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(15,___S_flags))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(20,___S_super))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(14,___S_fields))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X4,3UL)
               ___VEC1(___REF_SYM(19,___S_path))
               ___VEC1(___REF_FIX(18))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_STR(___X5,21UL)
               ___STR8(71,73,84,95,84,69,82,77)
               ___STR8(73,78,65,76,95,80,82,79)
               ___STR5(77,80,84,61,48)
___DEF_SUB_STR(___X6,3UL)
               ___STR3(103,105,116)
___DEF_SUB_STR(___X7,7UL)
               ___STR7(97,114,99,104,105,118,101)
___DEF_SUB_STR(___X8,5UL)
               ___STR5(99,108,111,110,101)
___DEF_SUB_STR(___X9,4UL)
               ___STR4(112,117,108,108)
___DEF_SUB_STR(___X10,6UL)
               ___STR6(111,114,105,103,105,110)
___DEF_SUB_STR(___X11,6UL)
               ___STR6(115,116,97,116,117,115)
___DEF_SUB_STR(___X12,3UL)
               ___STR3(116,97,103)
___DEF_SUB_VEC(___X13,6UL)
               ___VEC1(___REF_SUB(14))
               ___VEC1(___REF_SUB(15))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X14,1UL)
               ___VEC1(___REF_SYM(2,___S___git))
               ___VEC0
___DEF_SUB_VEC(___X15,2UL)
               ___VEC1(___REF_SYM(2,___S___git))
               ___VEC1(___REF_SYM(12,___S___tar))
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
,___DEF_SUB(___X10)
,___DEF_SUB(___X11)
,___DEF_SUB(___X12)
,___DEF_SUB(___X13)
,___DEF_SUB(___X14)
,___DEF_SUB(___X15)
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
___DEF_M_HLBL(___L0___git_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL(___L1___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL(___L2___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL(___L3___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL(___L4___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL(___L5___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL(___L6___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL(___L7___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL(___L8___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL(___L9___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL(___L10___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL(___L11___git_23_git_2d_repository_2d_open)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___git_23_git_2d_command_2d_aux)
___DEF_M_HLBL(___L1___git_23_git_2d_command_2d_aux)
___DEF_M_HLBL(___L2___git_23_git_2d_command_2d_aux)
___DEF_M_HLBL(___L3___git_23_git_2d_command_2d_aux)
___DEF_M_HLBL(___L4___git_23_git_2d_command_2d_aux)
___DEF_M_HLBL(___L5___git_23_git_2d_command_2d_aux)
___DEF_M_HLBL(___L6___git_23_git_2d_command_2d_aux)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___git_23_git_2d_command)
___DEF_M_HLBL(___L1___git_23_git_2d_command)
___DEF_M_HLBL(___L2___git_23_git_2d_command)
___DEF_M_HLBL(___L3___git_23_git_2d_command)
___DEF_M_HLBL(___L4___git_23_git_2d_command)
___DEF_M_HLBL(___L5___git_23_git_2d_command)
___DEF_M_HLBL(___L6___git_23_git_2d_command)
___DEF_M_HLBL(___L7___git_23_git_2d_command)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___git_23_git_2d_archive)
___DEF_M_HLBL(___L1___git_23_git_2d_archive)
___DEF_M_HLBL(___L2___git_23_git_2d_archive)
___DEF_M_HLBL(___L3___git_23_git_2d_archive)
___DEF_M_HLBL(___L4___git_23_git_2d_archive)
___DEF_M_HLBL(___L5___git_23_git_2d_archive)
___DEF_M_HLBL(___L6___git_23_git_2d_archive)
___DEF_M_HLBL(___L7___git_23_git_2d_archive)
___DEF_M_HLBL(___L8___git_23_git_2d_archive)
___DEF_M_HLBL(___L9___git_23_git_2d_archive)
___DEF_M_HLBL(___L10___git_23_git_2d_archive)
___DEF_M_HLBL(___L11___git_23_git_2d_archive)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___git_23_git_2d_clone)
___DEF_M_HLBL(___L1___git_23_git_2d_clone)
___DEF_M_HLBL(___L2___git_23_git_2d_clone)
___DEF_M_HLBL(___L3___git_23_git_2d_clone)
___DEF_M_HLBL(___L4___git_23_git_2d_clone)
___DEF_M_HLBL(___L5___git_23_git_2d_clone)
___DEF_M_HLBL(___L6___git_23_git_2d_clone)
___DEF_M_HLBL(___L7___git_23_git_2d_clone)
___DEF_M_HLBL(___L8___git_23_git_2d_clone)
___DEF_M_HLBL(___L9___git_23_git_2d_clone)
___DEF_M_HLBL(___L10___git_23_git_2d_clone)
___DEF_M_HLBL(___L11___git_23_git_2d_clone)
___DEF_M_HLBL(___L12___git_23_git_2d_clone)
___DEF_M_HLBL(___L13___git_23_git_2d_clone)
___DEF_M_HLBL(___L14___git_23_git_2d_clone)
___DEF_M_HLBL(___L15___git_23_git_2d_clone)
___DEF_M_HLBL(___L16___git_23_git_2d_clone)
___DEF_M_HLBL(___L17___git_23_git_2d_clone)
___DEF_M_HLBL(___L18___git_23_git_2d_clone)
___DEF_M_HLBL(___L19___git_23_git_2d_clone)
___DEF_M_HLBL(___L20___git_23_git_2d_clone)
___DEF_M_HLBL(___L21___git_23_git_2d_clone)
___DEF_M_HLBL(___L22___git_23_git_2d_clone)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___git_23_git_2d_pull)
___DEF_M_HLBL(___L1___git_23_git_2d_pull)
___DEF_M_HLBL(___L2___git_23_git_2d_pull)
___DEF_M_HLBL(___L3___git_23_git_2d_pull)
___DEF_M_HLBL(___L4___git_23_git_2d_pull)
___DEF_M_HLBL(___L5___git_23_git_2d_pull)
___DEF_M_HLBL(___L6___git_23_git_2d_pull)
___DEF_M_HLBL(___L7___git_23_git_2d_pull)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___git_23_git_2d_status)
___DEF_M_HLBL(___L1___git_23_git_2d_status)
___DEF_M_HLBL(___L2___git_23_git_2d_status)
___DEF_M_HLBL(___L3___git_23_git_2d_status)
___DEF_M_HLBL(___L4___git_23_git_2d_status)
___DEF_M_HLBL(___L5___git_23_git_2d_status)
___DEF_M_HLBL(___L6___git_23_git_2d_status)
___DEF_M_HLBL(___L7___git_23_git_2d_status)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0___git_23_git_2d_tag)
___DEF_M_HLBL(___L1___git_23_git_2d_tag)
___DEF_M_HLBL(___L2___git_23_git_2d_tag)
___DEF_M_HLBL(___L3___git_23_git_2d_tag)
___DEF_M_HLBL(___L4___git_23_git_2d_tag)
___DEF_M_HLBL(___L5___git_23_git_2d_tag)
___DEF_M_HLBL(___L6___git_23_git_2d_tag)
___DEF_M_HLBL(___L7___git_23_git_2d_tag)
___DEF_M_HLBL(___L8___git_23_git_2d_tag)
___DEF_M_HLBL(___L9___git_23_git_2d_tag)
___DEF_M_HLBL(___L10___git_23_git_2d_tag)
___DEF_M_HLBL(___L11___git_23_git_2d_tag)
___DEF_M_HLBL(___L12___git_23_git_2d_tag)
___DEF_M_HLBL(___L13___git_23_git_2d_tag)
___DEF_M_HLBL(___L14___git_23_git_2d_tag)
___DEF_M_HLBL(___L15___git_23_git_2d_tag)
___DEF_M_HLBL(___L16___git_23_git_2d_tag)
___DEF_M_HLBL(___L17___git_23_git_2d_tag)
___DEF_M_HLBL(___L18___git_23_git_2d_tag)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H___git_23_
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
___DEF_P_HLBL(___L0___git_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___git_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L___git_23_)
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___git_23_git_2d_repository_2d_open
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___git_23_git_2d_repository_2d_open)
___DEF_P_HLBL(___L1___git_23_git_2d_repository_2d_open)
___DEF_P_HLBL(___L2___git_23_git_2d_repository_2d_open)
___DEF_P_HLBL(___L3___git_23_git_2d_repository_2d_open)
___DEF_P_HLBL(___L4___git_23_git_2d_repository_2d_open)
___DEF_P_HLBL(___L5___git_23_git_2d_repository_2d_open)
___DEF_P_HLBL(___L6___git_23_git_2d_repository_2d_open)
___DEF_P_HLBL(___L7___git_23_git_2d_repository_2d_open)
___DEF_P_HLBL(___L8___git_23_git_2d_repository_2d_open)
___DEF_P_HLBL(___L9___git_23_git_2d_repository_2d_open)
___DEF_P_HLBL(___L10___git_23_git_2d_repository_2d_open)
___DEF_P_HLBL(___L11___git_23_git_2d_repository_2d_open)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___git_23_git_2d_repository_2d_open)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___git_23_git_2d_repository_2d_open)
   ___IF(___NOT(___STRINGP(___R1)))
   ___GOTO(___L16___git_23_git_2d_repository_2d_open)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1___git_23_git_2d_repository_2d_open)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),14,___G__23__23_file_2d_exists_3f_)
___DEF_SLBL(2,___L2___git_23_git_2d_repository_2d_open)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L13___git_23_git_2d_repository_2d_open)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),15,___G__23__23_file_2d_info)
___DEF_SLBL(3,___L3___git_23_git_2d_repository_2d_open)
   ___SET_R0(___LBL(4))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),16,___G__23__23_file_2d_info_2d_type)
___DEF_SLBL(4,___L4___git_23_git_2d_repository_2d_open)
   ___IF(___NOT(___EQP(___SYM_directory,___R1)))
   ___GOTO(___L15___git_23_git_2d_repository_2d_open)
   ___END_IF
   ___SET_R2(___STK(-6))
   ___SET_R1(___SUB(0))
   ___SET_R0(___LBL(5))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),19,___G__23__23_path_2d_expand)
___DEF_SLBL(5,___L5___git_23_git_2d_repository_2d_open)
   ___SET_STK(-5,___R1)
   ___SET_R0(___LBL(6))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),14,___G__23__23_file_2d_exists_3f_)
___DEF_SLBL(6,___L6___git_23_git_2d_repository_2d_open)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L14___git_23_git_2d_repository_2d_open)
   ___END_IF
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L12___git_23_git_2d_repository_2d_open)
   ___END_IF
   ___GOTO(___L13___git_23_git_2d_repository_2d_open)
___DEF_SLBL(7,___L7___git_23_git_2d_repository_2d_open)
   ___SET_R1(___BOOLEAN(___EQP(___SYM_directory,___R1)))
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L13___git_23_git_2d_repository_2d_open)
   ___END_IF
___DEF_GLBL(___L12___git_23_git_2d_repository_2d_open)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(8))
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),19,___G__23__23_path_2d_expand)
___DEF_SLBL(8,___L8___git_23_git_2d_repository_2d_open)
   ___BEGIN_ALLOC_STRUCTURE(2UL)
   ___ADD_STRUCTURE_ELEM(0,___SUB(1))
   ___ADD_STRUCTURE_ELEM(1,___R1)
   ___END_ALLOC_STRUCTURE(2)
   ___SET_R1(___GET_STRUCTURE(2))
   ___ADJFP(-3)
   ___CHECK_HEAP(9,4096)
___DEF_SLBL(9,___L9___git_23_git_2d_repository_2d_open)
   ___ADJFP(-1)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L13___git_23_git_2d_repository_2d_open)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L14___git_23_git_2d_repository_2d_open)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(10))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),15,___G__23__23_file_2d_info)
___DEF_SLBL(10,___L10___git_23_git_2d_repository_2d_open)
   ___SET_R0(___LBL(7))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),16,___G__23__23_file_2d_info_2d_type)
___DEF_GLBL(___L15___git_23_git_2d_repository_2d_open)
   ___SET_R1(___FAL)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L16___git_23_git_2d_repository_2d_open)
   ___SET_R3(___R1)
   ___SET_R2(___LBL(0))
   ___SET_R1(___FIX(1L))
   ___POLL(11)
___DEF_SLBL(11,___L11___git_23_git_2d_repository_2d_open)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),12,___G__23__23_fail_2d_check_2d_string)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___git_23_git_2d_command_2d_aux
#undef ___PH_LBL0
#define ___PH_LBL0 16
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___git_23_git_2d_command_2d_aux)
___DEF_P_HLBL(___L1___git_23_git_2d_command_2d_aux)
___DEF_P_HLBL(___L2___git_23_git_2d_command_2d_aux)
___DEF_P_HLBL(___L3___git_23_git_2d_command_2d_aux)
___DEF_P_HLBL(___L4___git_23_git_2d_command_2d_aux)
___DEF_P_HLBL(___L5___git_23_git_2d_command_2d_aux)
___DEF_P_HLBL(___L6___git_23_git_2d_command_2d_aux)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___git_23_git_2d_command_2d_aux)
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,4,0,0)
___DEF_GLBL(___L___git_23_git_2d_command_2d_aux)
   ___IF(___NOT(___STRUCTUREDIOP(___R2,___SYM__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC)))
   ___GOTO(___L7___git_23_git_2d_command_2d_aux)
   ___END_IF
   ___SET_R2(___UNCHECKEDSTRUCTUREREF(___R2,___FIX(1L),___SUB(1),___FAL))
   ___SET_STK(1,___R1)
   ___SET_R1(___R2)
   ___ADJFP(1)
   ___GOTO(___L8___git_23_git_2d_command_2d_aux)
___DEF_GLBL(___L7___git_23_git_2d_command_2d_aux)
   ___SET_STK(1,___R1)
   ___SET_R1(___FAL)
   ___ADJFP(1)
___DEF_GLBL(___L8___git_23_git_2d_command_2d_aux)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R3)
   ___ADJFP(6)
   ___POLL(1)
___DEF_SLBL(1,___L1___git_23_git_2d_command_2d_aux)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),24,___G__23__23_tty_2d_mode_2d_reset)
___DEF_SLBL(2,___L2___git_23_git_2d_command_2d_aux)
   ___IF(___NOT(___NOTFALSEP(___STK(-3))))
   ___GOTO(___L10___git_23_git_2d_command_2d_aux)
   ___END_IF
   ___SET_R1(___FAL)
   ___GOTO(___L9___git_23_git_2d_command_2d_aux)
___DEF_SLBL(3,___L3___git_23_git_2d_command_2d_aux)
   ___SET_R1(___CONS(___SUB(5),___R1))
   ___CHECK_HEAP(4,4096)
___DEF_SLBL(4,___L4___git_23_git_2d_command_2d_aux)
___DEF_GLBL(___L9___git_23_git_2d_command_2d_aux)
   ___SET_R2(___BOOLEAN(___FALSEP(___STK(-3))))
   ___SET_R3(___BOOLEAN(___FALSEP(___STK(-3))))
   ___BEGIN_ALLOC_LIST(16UL,___R1)
   ___ADD_LIST_ELEM(1,___KEY_environment)
   ___ADD_LIST_ELEM(2,___STK(-3))
   ___ADD_LIST_ELEM(3,___KEY_pseudo_2d_terminal)
   ___ADD_LIST_ELEM(4,___R2)
   ___ADD_LIST_ELEM(5,___KEY_stderr_2d_redirection)
   ___ADD_LIST_ELEM(6,___R3)
   ___ADD_LIST_ELEM(7,___KEY_stdout_2d_redirection)
   ___ADD_LIST_ELEM(8,___FAL)
   ___ADD_LIST_ELEM(9,___KEY_stdin_2d_redirection)
   ___ADD_LIST_ELEM(10,___STK(-4))
   ___ADD_LIST_ELEM(11,___KEY_directory)
   ___ADD_LIST_ELEM(12,___STK(-7))
   ___ADD_LIST_ELEM(13,___KEY_arguments)
   ___ADD_LIST_ELEM(14,___SUB(6))
   ___ADD_LIST_ELEM(15,___KEY_path)
   ___END_ALLOC_LIST(16)
   ___SET_R1(___GET_LIST(16))
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-5))
   ___CHECK_HEAP(5,4096)
___DEF_SLBL(5,___L5___git_23_git_2d_command_2d_aux)
   ___POLL(6)
___DEF_SLBL(6,___L6___git_23_git_2d_command_2d_aux)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),10,___G__23__23_call_2d_with_2d_input_2d_process)
___DEF_GLBL(___L10___git_23_git_2d_command_2d_aux)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),18,___G__23__23_os_2d_environ)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___git_23_git_2d_command
#undef ___PH_LBL0
#define ___PH_LBL0 24
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___git_23_git_2d_command)
___DEF_P_HLBL(___L1___git_23_git_2d_command)
___DEF_P_HLBL(___L2___git_23_git_2d_command)
___DEF_P_HLBL(___L3___git_23_git_2d_command)
___DEF_P_HLBL(___L4___git_23_git_2d_command)
___DEF_P_HLBL(___L5___git_23_git_2d_command)
___DEF_P_HLBL(___L6___git_23_git_2d_command)
___DEF_P_HLBL(___L7___git_23_git_2d_command)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___git_23_git_2d_command)
   ___IF_NARGS_EQ(2,___PUSH(___R1) ___SET_R1(___R2) ___SET_R2(___ABSENT) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___PUSH(___R1) ___SET_R1(___R2) ___SET_R2(___R3) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,2,2,0)
___DEF_GLBL(___L___git_23_git_2d_command)
   ___IF(___NOT(___EQP(___R3,___ABSENT)))
   ___GOTO(___L8___git_23_git_2d_command)
   ___END_IF
   ___SET_R4(___FAL)
   ___GOTO(___L9___git_23_git_2d_command)
___DEF_GLBL(___L8___git_23_git_2d_command)
   ___SET_R4(___R3)
___DEF_GLBL(___L9___git_23_git_2d_command)
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R2)
   ___SET_STK(3,___R3)
   ___SET_R2(___STK(0))
   ___SET_R1(___R4)
   ___SET_R3(___NUL)
   ___ADJFP(3)
   ___POLL(1)
___DEF_SLBL(1,___L1___git_23_git_2d_command)
___DEF_GLBL(___L10___git_23_git_2d_command)
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L12___git_23_git_2d_command)
   ___END_IF
   ___SET_R4(___CAR(___R2))
   ___IF(___NOT(___STRINGP(___R4)))
   ___GOTO(___L11___git_23_git_2d_command)
   ___END_IF
   ___SET_R2(___CDR(___R2))
   ___SET_R3(___CONS(___R4,___R3))
   ___CHECK_HEAP(2,4096)
___DEF_SLBL(2,___L2___git_23_git_2d_command)
   ___POLL(3)
___DEF_SLBL(3,___L3___git_23_git_2d_command)
   ___GOTO(___L10___git_23_git_2d_command)
___DEF_GLBL(___L11___git_23_git_2d_command)
   ___SET_STK(-1,___STK(-3))
   ___SET_STK(-3,___FIX(1L))
   ___SET_STK(1,___STK(-2))
   ___SET_STK(-2,___LBL(0))
   ___SET_R3(___STK(0))
   ___SET_R1(___STK(1))
   ___SET_R2(___GLO___git_23_d)
   ___ADJFP(1)
   ___POLL(4)
___DEF_SLBL(4,___L4___git_23_git_2d_command)
   ___ADJFP(-2)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),13,___G__23__23_fail_2d_check_2d_string_2d_list)
___DEF_GLBL(___L12___git_23_git_2d_command)
   ___IF(___PROCEDUREP(___STK(-2)))
   ___GOTO(___L13___git_23_git_2d_command)
   ___END_IF
   ___SET_STK(1,___STK(-3))
   ___SET_STK(-3,___FIX(2L))
   ___SET_STK(2,___STK(-2))
   ___SET_STK(-2,___LBL(0))
   ___SET_STK(3,___STK(-1))
   ___SET_STK(-1,___STK(1))
   ___SET_R3(___STK(0))
   ___SET_R2(___STK(3))
   ___SET_R1(___STK(2))
   ___ADJFP(3)
   ___POLL(5)
___DEF_SLBL(5,___L5___git_23_git_2d_command)
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),11,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L13___git_23_git_2d_command)
   ___SET_STK(-3,___R0)
   ___SET_STK(0,___R1)
   ___SET_R1(___R3)
   ___SET_R0(___LBL(6))
   ___ADJFP(4)
   ___JUMPPRM(___SET_NARGS(1),___PRM__23__23_reverse)
___DEF_SLBL(6,___L6___git_23_git_2d_command)
   ___SET_STK(-3,___STK(-7))
   ___SET_STK(-7,___R1)
   ___SET_R3(___STK(-4))
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-3))
   ___POLL(7)
___DEF_SLBL(7,___L7___git_23_git_2d_command)
   ___ADJFP(-7)
   ___JUMPINT(___SET_NARGS(4),___PRC(16),___L___git_23_git_2d_command_2d_aux)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___git_23_git_2d_archive
#undef ___PH_LBL0
#define ___PH_LBL0 33
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___git_23_git_2d_archive)
___DEF_P_HLBL(___L1___git_23_git_2d_archive)
___DEF_P_HLBL(___L2___git_23_git_2d_archive)
___DEF_P_HLBL(___L3___git_23_git_2d_archive)
___DEF_P_HLBL(___L4___git_23_git_2d_archive)
___DEF_P_HLBL(___L5___git_23_git_2d_archive)
___DEF_P_HLBL(___L6___git_23_git_2d_archive)
___DEF_P_HLBL(___L7___git_23_git_2d_archive)
___DEF_P_HLBL(___L8___git_23_git_2d_archive)
___DEF_P_HLBL(___L9___git_23_git_2d_archive)
___DEF_P_HLBL(___L10___git_23_git_2d_archive)
___DEF_P_HLBL(___L11___git_23_git_2d_archive)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___git_23_git_2d_archive)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L___git_23_git_2d_archive)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC)))
   ___GOTO(___L17___git_23_git_2d_archive)
   ___END_IF
   ___BEGIN_ALLOC_LIST(2UL,___R2)
   ___ADD_LIST_ELEM(1,___SUB(7))
   ___END_ALLOC_LIST(2)
   ___SET_R2(___GET_LIST(2))
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1___git_23_git_2d_archive)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC)))
   ___GOTO(___L12___git_23_git_2d_archive)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___SUB(1),___FAL))
   ___GOTO(___L13___git_23_git_2d_archive)
___DEF_GLBL(___L12___git_23_git_2d_archive)
   ___SET_R1(___FAL)
___DEF_GLBL(___L13___git_23_git_2d_archive)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___ADJFP(8)
   ___POLL(2)
___DEF_SLBL(2,___L2___git_23_git_2d_archive)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),24,___G__23__23_tty_2d_mode_2d_reset)
___DEF_SLBL(3,___L3___git_23_git_2d_archive)
   ___SET_R0(___LBL(4))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),18,___G__23__23_os_2d_environ)
___DEF_SLBL(4,___L4___git_23_git_2d_archive)
   ___SET_R1(___CONS(___SUB(5),___R1))
   ___BEGIN_ALLOC_LIST(16UL,___R1)
   ___ADD_LIST_ELEM(1,___KEY_environment)
   ___ADD_LIST_ELEM(2,___FAL)
   ___ADD_LIST_ELEM(3,___KEY_pseudo_2d_terminal)
   ___ADD_LIST_ELEM(4,___TRU)
   ___ADD_LIST_ELEM(5,___KEY_stderr_2d_redirection)
   ___ADD_LIST_ELEM(6,___TRU)
   ___ADD_LIST_ELEM(7,___KEY_stdout_2d_redirection)
   ___ADD_LIST_ELEM(8,___FAL)
   ___ADD_LIST_ELEM(9,___KEY_stdin_2d_redirection)
   ___ADD_LIST_ELEM(10,___STK(-6))
   ___ADD_LIST_ELEM(11,___KEY_directory)
   ___ADD_LIST_ELEM(12,___STK(-5))
   ___ADD_LIST_ELEM(13,___KEY_arguments)
   ___ADD_LIST_ELEM(14,___SUB(6))
   ___ADD_LIST_ELEM(15,___KEY_path)
   ___END_ALLOC_LIST(16)
   ___SET_R1(___GET_LIST(16))
   ___SET_R2(___LBL(7))
   ___SET_R0(___STK(-7))
   ___CHECK_HEAP(5,4096)
___DEF_SLBL(5,___L5___git_23_git_2d_archive)
   ___POLL(6)
___DEF_SLBL(6,___L6___git_23_git_2d_archive)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),10,___G__23__23_call_2d_with_2d_input_2d_process)
___DEF_SLBL(7,___L7___git_23_git_2d_archive)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(7,1,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___ADJFP(8)
   ___POLL(8)
___DEF_SLBL(8,___L8___git_23_git_2d_archive)
   ___SET_R0(___LBL(9))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),20,___G__23__23_process_2d_status)
___DEF_SLBL(9,___L9___git_23_git_2d_archive)
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L16___git_23_git_2d_archive)
   ___END_IF
   ___SET_R1(___BOOLEAN(___FIXEQ(___R1,___FIX(0L))))
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L14___git_23_git_2d_archive)
   ___END_IF
   ___GOTO(___L15___git_23_git_2d_archive)
___DEF_SLBL(10,___L10___git_23_git_2d_archive)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L15___git_23_git_2d_archive)
   ___END_IF
___DEF_GLBL(___L14___git_23_git_2d_archive)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(11)
___DEF_SLBL(11,___L11___git_23_git_2d_archive)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),27,___G___tar_23_tar_2d_unpack_2d_port)
___DEF_GLBL(___L15___git_23_git_2d_archive)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L16___git_23_git_2d_archive)
   ___SET_R2(___FIX(0L))
   ___SET_R0(___LBL(10))
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23__3d_)
___DEF_GLBL(___L17___git_23_git_2d_archive)
   ___SET_R1(___FAL)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___git_23_git_2d_clone
#undef ___PH_LBL0
#define ___PH_LBL0 46
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___git_23_git_2d_clone)
___DEF_P_HLBL(___L1___git_23_git_2d_clone)
___DEF_P_HLBL(___L2___git_23_git_2d_clone)
___DEF_P_HLBL(___L3___git_23_git_2d_clone)
___DEF_P_HLBL(___L4___git_23_git_2d_clone)
___DEF_P_HLBL(___L5___git_23_git_2d_clone)
___DEF_P_HLBL(___L6___git_23_git_2d_clone)
___DEF_P_HLBL(___L7___git_23_git_2d_clone)
___DEF_P_HLBL(___L8___git_23_git_2d_clone)
___DEF_P_HLBL(___L9___git_23_git_2d_clone)
___DEF_P_HLBL(___L10___git_23_git_2d_clone)
___DEF_P_HLBL(___L11___git_23_git_2d_clone)
___DEF_P_HLBL(___L12___git_23_git_2d_clone)
___DEF_P_HLBL(___L13___git_23_git_2d_clone)
___DEF_P_HLBL(___L14___git_23_git_2d_clone)
___DEF_P_HLBL(___L15___git_23_git_2d_clone)
___DEF_P_HLBL(___L16___git_23_git_2d_clone)
___DEF_P_HLBL(___L17___git_23_git_2d_clone)
___DEF_P_HLBL(___L18___git_23_git_2d_clone)
___DEF_P_HLBL(___L19___git_23_git_2d_clone)
___DEF_P_HLBL(___L20___git_23_git_2d_clone)
___DEF_P_HLBL(___L21___git_23_git_2d_clone)
___DEF_P_HLBL(___L22___git_23_git_2d_clone)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___git_23_git_2d_clone)
   ___IF_NARGS_EQ(2,___PUSH(___R1) ___SET_R1(___R2) ___SET_R2(___ABSENT) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___PUSH(___R1) ___SET_R1(___R2) ___SET_R2(___R3) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,2,2,0)
___DEF_GLBL(___L___git_23_git_2d_clone)
   ___IF(___NOT(___EQP(___R3,___ABSENT)))
   ___GOTO(___L23___git_23_git_2d_clone)
   ___END_IF
   ___SET_R4(___FAL)
   ___IF(___EQP(___R2,___ABSENT))
   ___GOTO(___L25___git_23_git_2d_clone)
   ___END_IF
   ___GOTO(___L24___git_23_git_2d_clone)
___DEF_GLBL(___L23___git_23_git_2d_clone)
   ___SET_R4(___R3)
   ___IF(___EQP(___R2,___ABSENT))
   ___GOTO(___L25___git_23_git_2d_clone)
   ___END_IF
___DEF_GLBL(___L24___git_23_git_2d_clone)
   ___IF(___NOT(___EQP(___R2,___FAL)))
   ___GOTO(___L30___git_23_git_2d_clone)
   ___END_IF
___DEF_GLBL(___L25___git_23_git_2d_clone)
   ___SET_STK(1,___ALLOC_CLO(2UL))
   ___BEGIN_SETUP_CLO(2,___STK(1),9)
   ___ADD_CLO_ELEM(0,___R1)
   ___ADD_CLO_ELEM(1,___R4)
   ___END_SETUP_CLO(2)
   ___SET_STK(2,___R2)
   ___SET_R2(___STK(1))
   ___SET_STK(1,___STK(2))
   ___ADJFP(1)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1___git_23_git_2d_clone)
   ___IF(___NOT(___STRINGP(___STK(-1))))
   ___GOTO(___L31___git_23_git_2d_clone)
   ___END_IF
___DEF_GLBL(___L26___git_23_git_2d_clone)
   ___IF(___NOT(___STRINGP(___R1)))
   ___GOTO(___L29___git_23_git_2d_clone)
   ___END_IF
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L28___git_23_git_2d_clone)
   ___END_IF
   ___SET_STK(0,___R0)
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R2)
   ___SET_STK(3,___R4)
   ___ADJFP(6)
   ___POLL(2)
___DEF_SLBL(2,___L2___git_23_git_2d_clone)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),14,___G__23__23_file_2d_exists_3f_)
___DEF_SLBL(3,___L3___git_23_git_2d_clone)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L27___git_23_git_2d_clone)
   ___END_IF
   ___BEGIN_ALLOC_LIST(3UL,___STK(-5))
   ___ADD_LIST_ELEM(1,___STK(-7))
   ___ADD_LIST_ELEM(2,___SUB(8))
   ___END_ALLOC_LIST(3)
   ___SET_STK(-7,___GET_LIST(3))
   ___SET_R3(___STK(-3))
   ___SET_R1(___STK(-4))
   ___SET_R2(___FAL)
   ___SET_R0(___STK(-6))
   ___CHECK_HEAP(4,4096)
___DEF_SLBL(4,___L4___git_23_git_2d_clone)
   ___POLL(5)
___DEF_SLBL(5,___L5___git_23_git_2d_clone)
   ___ADJFP(-7)
   ___JUMPINT(___SET_NARGS(4),___PRC(16),___L___git_23_git_2d_command_2d_aux)
___DEF_GLBL(___L27___git_23_git_2d_clone)
   ___SET_R1(___FAL)
   ___ADJFP(-8)
   ___JUMPRET(___STK(2))
___DEF_GLBL(___L28___git_23_git_2d_clone)
   ___SET_STK(1,___STK(-1))
   ___SET_STK(-1,___FIX(3L))
   ___SET_STK(2,___STK(0))
   ___SET_STK(0,___LBL(0))
   ___SET_R2(___STK(2))
   ___ADJFP(2)
   ___POLL(6)
___DEF_SLBL(6,___L6___git_23_git_2d_clone)
   ___ADJFP(-1)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),11,___G__23__23_fail_2d_check_2d_procedure)
___DEF_GLBL(___L29___git_23_git_2d_clone)
   ___SET_STK(1,___STK(-1))
   ___SET_STK(-1,___FIX(2L))
   ___SET_STK(2,___STK(0))
   ___SET_STK(0,___LBL(0))
   ___SET_R2(___STK(2))
   ___ADJFP(2)
   ___POLL(7)
___DEF_SLBL(7,___L7___git_23_git_2d_clone)
   ___GOTO(___L32___git_23_git_2d_clone)
___DEF_GLBL(___L30___git_23_git_2d_clone)
   ___SET_STK(1,___R2)
   ___ADJFP(1)
   ___IF(___STRINGP(___STK(-1)))
   ___GOTO(___L26___git_23_git_2d_clone)
   ___END_IF
___DEF_GLBL(___L31___git_23_git_2d_clone)
   ___SET_STK(1,___STK(-1))
   ___SET_STK(-1,___FIX(1L))
   ___SET_STK(2,___STK(0))
   ___SET_STK(0,___LBL(0))
   ___SET_R2(___STK(2))
   ___ADJFP(2)
   ___POLL(8)
___DEF_SLBL(8,___L8___git_23_git_2d_clone)
___DEF_GLBL(___L32___git_23_git_2d_clone)
   ___ADJFP(-1)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),12,___G__23__23_fail_2d_check_2d_string)
___DEF_SLBL(9,___L9___git_23_git_2d_clone)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(9,1,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R4)
   ___ADJFP(8)
   ___POLL(10)
___DEF_SLBL(10,___L10___git_23_git_2d_clone)
   ___SET_R0(___LBL(11))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),20,___G__23__23_process_2d_status)
___DEF_SLBL(11,___L11___git_23_git_2d_clone)
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L37___git_23_git_2d_clone)
   ___END_IF
   ___IF(___FIXEQ(___R1,___FIX(0L)))
   ___GOTO(___L36___git_23_git_2d_clone)
   ___END_IF
   ___GOTO(___L33___git_23_git_2d_clone)
___DEF_SLBL(12,___L12___git_23_git_2d_clone)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L36___git_23_git_2d_clone)
   ___END_IF
___DEF_GLBL(___L33___git_23_git_2d_clone)
   ___IF(___NOT(___NOTFALSEP(___CLO(___STK(-5),2))))
   ___GOTO(___L35___git_23_git_2d_clone)
   ___END_IF
   ___ADJFP(-4)
___DEF_GLBL(___L34___git_23_git_2d_clone)
   ___SET_R1(___FAL)
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L35___git_23_git_2d_clone)
   ___SET_STK(-5,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(-5),15)
   ___ADD_CLO_ELEM(0,___STK(-6))
   ___END_SETUP_CLO(1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(14))
   ___ADJFP(-4)
   ___CHECK_HEAP(13,4096)
___DEF_SLBL(13,___L13___git_23_git_2d_clone)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),22,___G__23__23_repl)
___DEF_SLBL(14,___L14___git_23_git_2d_clone)
   ___GOTO(___L34___git_23_git_2d_clone)
___DEF_SLBL(15,___L15___git_23_git_2d_clone)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(15,2,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_STK(3,___R4)
   ___SET_R1(___R2)
   ___SET_R0(___LBL(16))
   ___ADJFP(8)
   ___JUMPPRM(___SET_NARGS(1),___PRM__23__23_newline1)
___DEF_SLBL(16,___L16___git_23_git_2d_clone)
   ___SET_R1(___CLO(___STK(-5),1))
   ___SET_R2(___FAL)
   ___POLL(17)
___DEF_SLBL(17,___L17___git_23_git_2d_clone)
   ___SET_R0(___LBL(18))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),21,___G__23__23_read_2d_line)
___DEF_SLBL(18,___L18___git_23_git_2d_clone)
   ___SET_R2(___STK(-6))
   ___SET_R0(___LBL(19))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),25,___G__23__23_write_2d_string)
___DEF_SLBL(19,___L19___git_23_git_2d_clone)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(20))
   ___ADJFP(-4)
   ___JUMPPRM(___SET_NARGS(1),___PRM__23__23_newline1)
___DEF_SLBL(20,___L20___git_23_git_2d_clone)
   ___SET_R1(___TRU)
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L36___git_23_git_2d_clone)
   ___SET_R1(___CLO(___STK(-5),1))
   ___SET_R0(___LBL(21))
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),19,___G__23__23_path_2d_expand)
___DEF_SLBL(21,___L21___git_23_git_2d_clone)
   ___BEGIN_ALLOC_STRUCTURE(2UL)
   ___ADD_STRUCTURE_ELEM(0,___SUB(1))
   ___ADD_STRUCTURE_ELEM(1,___R1)
   ___END_ALLOC_STRUCTURE(2)
   ___SET_R1(___GET_STRUCTURE(2))
   ___ADJFP(-3)
   ___CHECK_HEAP(22,4096)
___DEF_SLBL(22,___L22___git_23_git_2d_clone)
   ___ADJFP(-1)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L37___git_23_git_2d_clone)
   ___SET_R2(___FIX(0L))
   ___SET_R0(___LBL(12))
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23__3d_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___git_23_git_2d_pull
#undef ___PH_LBL0
#define ___PH_LBL0 70
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___git_23_git_2d_pull)
___DEF_P_HLBL(___L1___git_23_git_2d_pull)
___DEF_P_HLBL(___L2___git_23_git_2d_pull)
___DEF_P_HLBL(___L3___git_23_git_2d_pull)
___DEF_P_HLBL(___L4___git_23_git_2d_pull)
___DEF_P_HLBL(___L5___git_23_git_2d_pull)
___DEF_P_HLBL(___L6___git_23_git_2d_pull)
___DEF_P_HLBL(___L7___git_23_git_2d_pull)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___git_23_git_2d_pull)
   ___IF_NARGS_EQ(1,___SET_R2(___ABSENT) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(2,___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,1,2,0)
___DEF_GLBL(___L___git_23_git_2d_pull)
   ___IF(___NOT(___EQP(___R2,___ABSENT)))
   ___GOTO(___L8___git_23_git_2d_pull)
   ___END_IF
   ___SET_R4(___LBL(4))
   ___IF(___EQP(___R3,___ABSENT))
   ___GOTO(___L9___git_23_git_2d_pull)
   ___END_IF
   ___GOTO(___L12___git_23_git_2d_pull)
___DEF_GLBL(___L8___git_23_git_2d_pull)
   ___SET_R4(___R2)
   ___IF(___NOT(___EQP(___R3,___ABSENT)))
   ___GOTO(___L12___git_23_git_2d_pull)
   ___END_IF
___DEF_GLBL(___L9___git_23_git_2d_pull)
   ___SET_STK(1,___R1)
   ___SET_R1(___FAL)
   ___ADJFP(1)
   ___IF(___NOT(___PROCEDUREP(___R4)))
   ___GOTO(___L13___git_23_git_2d_pull)
   ___END_IF
___DEF_GLBL(___L10___git_23_git_2d_pull)
   ___IF(___NOT(___STRUCTUREDIOP(___STK(0),___SYM__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC)))
   ___GOTO(___L11___git_23_git_2d_pull)
   ___END_IF
   ___BEGIN_ALLOC_LIST(2UL,___SUB(10))
   ___ADD_LIST_ELEM(1,___SUB(9))
   ___END_ALLOC_LIST(2)
   ___SET_R2(___GET_LIST(2))
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R2)
   ___SET_R3(___R1)
   ___SET_R2(___STK(1))
   ___SET_R1(___R4)
   ___ADJFP(1)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1___git_23_git_2d_pull)
   ___POLL(2)
___DEF_SLBL(2,___L2___git_23_git_2d_pull)
   ___ADJFP(-1)
   ___JUMPINT(___SET_NARGS(4),___PRC(16),___L___git_23_git_2d_command_2d_aux)
___DEF_GLBL(___L11___git_23_git_2d_pull)
   ___SET_R1(___FAL)
   ___ADJFP(-1)
   ___JUMPRET(___R0)
___DEF_GLBL(___L12___git_23_git_2d_pull)
   ___SET_STK(1,___R1)
   ___SET_R1(___R3)
   ___ADJFP(1)
   ___IF(___PROCEDUREP(___R4))
   ___GOTO(___L10___git_23_git_2d_pull)
   ___END_IF
___DEF_GLBL(___L13___git_23_git_2d_pull)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___FIX(2L))
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___LBL(0))
   ___SET_R1(___STK(2))
   ___ADJFP(2)
   ___POLL(3)
___DEF_SLBL(3,___L3___git_23_git_2d_pull)
   ___ADJFP(-1)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),11,___G__23__23_fail_2d_check_2d_procedure)
___DEF_SLBL(4,___L4___git_23_git_2d_pull)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(4,1,0,0)
   ___SET_STK(1,___R0)
   ___ADJFP(4)
   ___POLL(5)
___DEF_SLBL(5,___L5___git_23_git_2d_pull)
   ___SET_R0(___LBL(6))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),20,___G__23__23_process_2d_status)
___DEF_SLBL(6,___L6___git_23_git_2d_pull)
   ___IF(___FIXNUMP(___R1))
   ___GOTO(___L14___git_23_git_2d_pull)
   ___END_IF
   ___SET_R2(___FIX(0L))
   ___SET_R0(___STK(-3))
   ___POLL(7)
___DEF_SLBL(7,___L7___git_23_git_2d_pull)
   ___ADJFP(-4)
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23__3d_)
___DEF_GLBL(___L14___git_23_git_2d_pull)
   ___SET_R1(___BOOLEAN(___FIXEQ(___R1,___FIX(0L))))
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___git_23_git_2d_status
#undef ___PH_LBL0
#define ___PH_LBL0 79
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___git_23_git_2d_status)
___DEF_P_HLBL(___L1___git_23_git_2d_status)
___DEF_P_HLBL(___L2___git_23_git_2d_status)
___DEF_P_HLBL(___L3___git_23_git_2d_status)
___DEF_P_HLBL(___L4___git_23_git_2d_status)
___DEF_P_HLBL(___L5___git_23_git_2d_status)
___DEF_P_HLBL(___L6___git_23_git_2d_status)
___DEF_P_HLBL(___L7___git_23_git_2d_status)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___git_23_git_2d_status)
   ___IF_NARGS_EQ(1,___SET_R2(___ABSENT) ___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(2,___SET_R3(___ABSENT))
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,1,2,0)
___DEF_GLBL(___L___git_23_git_2d_status)
   ___IF(___NOT(___EQP(___R2,___ABSENT)))
   ___GOTO(___L8___git_23_git_2d_status)
   ___END_IF
   ___SET_R4(___LBL(4))
   ___IF(___EQP(___R3,___ABSENT))
   ___GOTO(___L9___git_23_git_2d_status)
   ___END_IF
   ___GOTO(___L12___git_23_git_2d_status)
___DEF_GLBL(___L8___git_23_git_2d_status)
   ___SET_R4(___R2)
   ___IF(___NOT(___EQP(___R3,___ABSENT)))
   ___GOTO(___L12___git_23_git_2d_status)
   ___END_IF
___DEF_GLBL(___L9___git_23_git_2d_status)
   ___SET_STK(1,___R1)
   ___SET_R1(___FAL)
   ___ADJFP(1)
   ___IF(___NOT(___PROCEDUREP(___R4)))
   ___GOTO(___L13___git_23_git_2d_status)
   ___END_IF
___DEF_GLBL(___L10___git_23_git_2d_status)
   ___IF(___NOT(___STRUCTUREDIOP(___STK(0),___SYM__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC)))
   ___GOTO(___L11___git_23_git_2d_status)
   ___END_IF
   ___SET_R2(___CONS(___SUB(11),___NUL))
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R2)
   ___SET_R3(___R1)
   ___SET_R2(___STK(1))
   ___SET_R1(___R4)
   ___ADJFP(1)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1___git_23_git_2d_status)
   ___POLL(2)
___DEF_SLBL(2,___L2___git_23_git_2d_status)
   ___ADJFP(-1)
   ___JUMPINT(___SET_NARGS(4),___PRC(16),___L___git_23_git_2d_command_2d_aux)
___DEF_GLBL(___L11___git_23_git_2d_status)
   ___SET_R1(___FAL)
   ___ADJFP(-1)
   ___JUMPRET(___R0)
___DEF_GLBL(___L12___git_23_git_2d_status)
   ___SET_STK(1,___R1)
   ___SET_R1(___R3)
   ___ADJFP(1)
   ___IF(___PROCEDUREP(___R4))
   ___GOTO(___L10___git_23_git_2d_status)
   ___END_IF
___DEF_GLBL(___L13___git_23_git_2d_status)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___FIX(2L))
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___LBL(0))
   ___SET_R1(___STK(2))
   ___ADJFP(2)
   ___POLL(3)
___DEF_SLBL(3,___L3___git_23_git_2d_status)
   ___ADJFP(-1)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),11,___G__23__23_fail_2d_check_2d_procedure)
___DEF_SLBL(4,___L4___git_23_git_2d_status)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(4,1,0,0)
   ___SET_STK(1,___R0)
   ___ADJFP(4)
   ___POLL(5)
___DEF_SLBL(5,___L5___git_23_git_2d_status)
   ___SET_R0(___LBL(6))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),20,___G__23__23_process_2d_status)
___DEF_SLBL(6,___L6___git_23_git_2d_status)
   ___IF(___FIXNUMP(___R1))
   ___GOTO(___L14___git_23_git_2d_status)
   ___END_IF
   ___SET_R2(___FIX(0L))
   ___SET_R0(___STK(-3))
   ___POLL(7)
___DEF_SLBL(7,___L7___git_23_git_2d_status)
   ___ADJFP(-4)
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23__3d_)
___DEF_GLBL(___L14___git_23_git_2d_status)
   ___SET_R1(___BOOLEAN(___FIXEQ(___R1,___FIX(0L))))
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H___git_23_git_2d_tag
#undef ___PH_LBL0
#define ___PH_LBL0 88
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0___git_23_git_2d_tag)
___DEF_P_HLBL(___L1___git_23_git_2d_tag)
___DEF_P_HLBL(___L2___git_23_git_2d_tag)
___DEF_P_HLBL(___L3___git_23_git_2d_tag)
___DEF_P_HLBL(___L4___git_23_git_2d_tag)
___DEF_P_HLBL(___L5___git_23_git_2d_tag)
___DEF_P_HLBL(___L6___git_23_git_2d_tag)
___DEF_P_HLBL(___L7___git_23_git_2d_tag)
___DEF_P_HLBL(___L8___git_23_git_2d_tag)
___DEF_P_HLBL(___L9___git_23_git_2d_tag)
___DEF_P_HLBL(___L10___git_23_git_2d_tag)
___DEF_P_HLBL(___L11___git_23_git_2d_tag)
___DEF_P_HLBL(___L12___git_23_git_2d_tag)
___DEF_P_HLBL(___L13___git_23_git_2d_tag)
___DEF_P_HLBL(___L14___git_23_git_2d_tag)
___DEF_P_HLBL(___L15___git_23_git_2d_tag)
___DEF_P_HLBL(___L16___git_23_git_2d_tag)
___DEF_P_HLBL(___L17___git_23_git_2d_tag)
___DEF_P_HLBL(___L18___git_23_git_2d_tag)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0___git_23_git_2d_tag)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L___git_23_git_2d_tag)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC)))
   ___GOTO(___L23___git_23_git_2d_tag)
   ___END_IF
   ___SET_R2(___CONS(___SUB(12),___NUL))
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1___git_23_git_2d_tag)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC)))
   ___GOTO(___L19___git_23_git_2d_tag)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___SUB(1),___FAL))
   ___GOTO(___L20___git_23_git_2d_tag)
___DEF_GLBL(___L19___git_23_git_2d_tag)
   ___SET_R1(___FAL)
___DEF_GLBL(___L20___git_23_git_2d_tag)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___ADJFP(8)
   ___POLL(2)
___DEF_SLBL(2,___L2___git_23_git_2d_tag)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),24,___G__23__23_tty_2d_mode_2d_reset)
___DEF_SLBL(3,___L3___git_23_git_2d_tag)
   ___SET_R0(___LBL(4))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),18,___G__23__23_os_2d_environ)
___DEF_SLBL(4,___L4___git_23_git_2d_tag)
   ___SET_R1(___CONS(___SUB(5),___R1))
   ___BEGIN_ALLOC_LIST(16UL,___R1)
   ___ADD_LIST_ELEM(1,___KEY_environment)
   ___ADD_LIST_ELEM(2,___FAL)
   ___ADD_LIST_ELEM(3,___KEY_pseudo_2d_terminal)
   ___ADD_LIST_ELEM(4,___TRU)
   ___ADD_LIST_ELEM(5,___KEY_stderr_2d_redirection)
   ___ADD_LIST_ELEM(6,___TRU)
   ___ADD_LIST_ELEM(7,___KEY_stdout_2d_redirection)
   ___ADD_LIST_ELEM(8,___FAL)
   ___ADD_LIST_ELEM(9,___KEY_stdin_2d_redirection)
   ___ADD_LIST_ELEM(10,___STK(-6))
   ___ADD_LIST_ELEM(11,___KEY_directory)
   ___ADD_LIST_ELEM(12,___STK(-5))
   ___ADD_LIST_ELEM(13,___KEY_arguments)
   ___ADD_LIST_ELEM(14,___SUB(6))
   ___ADD_LIST_ELEM(15,___KEY_path)
   ___END_ALLOC_LIST(16)
   ___SET_R1(___GET_LIST(16))
   ___SET_R2(___LBL(7))
   ___SET_R0(___STK(-7))
   ___CHECK_HEAP(5,4096)
___DEF_SLBL(5,___L5___git_23_git_2d_tag)
   ___POLL(6)
___DEF_SLBL(6,___L6___git_23_git_2d_tag)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),10,___G__23__23_call_2d_with_2d_input_2d_process)
___DEF_SLBL(7,___L7___git_23_git_2d_tag)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(7,1,0,0)
   ___SET_R2(___NUL)
   ___POLL(8)
___DEF_SLBL(8,___L8___git_23_git_2d_tag)
   ___GOTO(___L21___git_23_git_2d_tag)
___DEF_SLBL(9,___L9___git_23_git_2d_tag)
   ___IF(___EOFP(___R1))
   ___GOTO(___L22___git_23_git_2d_tag)
   ___END_IF
   ___SET_R2(___CONS(___R1,___STK(-5)))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___CHECK_HEAP(10,4096)
___DEF_SLBL(10,___L10___git_23_git_2d_tag)
   ___POLL(11)
___DEF_SLBL(11,___L11___git_23_git_2d_tag)
___DEF_GLBL(___L21___git_23_git_2d_tag)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___ADJFP(8)
   ___POLL(12)
___DEF_SLBL(12,___L12___git_23_git_2d_tag)
   ___SET_R0(___LBL(13))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),21,___G__23__23_read_2d_line)
___DEF_SLBL(13,___L13___git_23_git_2d_tag)
   ___IF(___EOFP(___R1))
   ___GOTO(___L22___git_23_git_2d_tag)
   ___END_IF
   ___SET_R1(___CONS(___R1,___STK(-5)))
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(15))
   ___CHECK_HEAP(14,4096)
___DEF_SLBL(14,___L14___git_23_git_2d_tag)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),21,___G__23__23_read_2d_line)
___DEF_SLBL(15,___L15___git_23_git_2d_tag)
   ___IF(___EOFP(___R1))
   ___GOTO(___L22___git_23_git_2d_tag)
   ___END_IF
   ___SET_R1(___CONS(___R1,___STK(-5)))
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(17))
   ___CHECK_HEAP(16,4096)
___DEF_SLBL(16,___L16___git_23_git_2d_tag)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),21,___G__23__23_read_2d_line)
___DEF_SLBL(17,___L17___git_23_git_2d_tag)
   ___IF(___EOFP(___R1))
   ___GOTO(___L22___git_23_git_2d_tag)
   ___END_IF
   ___SET_R1(___CONS(___R1,___STK(-5)))
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(9))
   ___CHECK_HEAP(18,4096)
___DEF_SLBL(18,___L18___git_23_git_2d_tag)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),21,___G__23__23_read_2d_line)
___DEF_GLBL(___L22___git_23_git_2d_tag)
   ___SET_R1(___STK(-5))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L23___git_23_git_2d_tag)
   ___SET_R1(___FAL)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H___git_23_,___REF_SYM(3,___S___git_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H___git_23_,0,-1)
,___DEF_LBL_INTRO(___H___git_23_git_2d_repository_2d_open,___REF_SYM(9,___S___git_23_git_2d_repository_2d_open),___REF_FAL,12,0)
,___DEF_LBL_PROC(___H___git_23_git_2d_repository_2d_open,1,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_repository_2d_open,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H___git_23_git_2d_repository_2d_open,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___git_23_git_2d_repository_2d_open,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___git_23_git_2d_repository_2d_open,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___git_23_git_2d_repository_2d_open,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___git_23_git_2d_repository_2d_open,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_repository_2d_open,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___git_23_git_2d_repository_2d_open,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H___git_23_git_2d_repository_2d_open,___IFD(___RETI,1,0,0x3f1L))
,___DEF_LBL_RET(___H___git_23_git_2d_repository_2d_open,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___git_23_git_2d_repository_2d_open,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H___git_23_git_2d_command_2d_aux,___REF_SYM(7,___S___git_23_git_2d_command_2d_aux),___REF_FAL,7,0)
,___DEF_LBL_PROC(___H___git_23_git_2d_command_2d_aux,4,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_command_2d_aux,___IFD(___RETI,8,2,0x3f1fL))
,___DEF_LBL_RET(___H___git_23_git_2d_command_2d_aux,___IFD(___RETN,5,2,0x1fL))
,___DEF_LBL_RET(___H___git_23_git_2d_command_2d_aux,___IFD(___RETN,5,2,0x1fL))
,___DEF_LBL_RET(___H___git_23_git_2d_command_2d_aux,___IFD(___RETI,8,2,0x3f1fL))
,___DEF_LBL_RET(___H___git_23_git_2d_command_2d_aux,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H___git_23_git_2d_command_2d_aux,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_INTRO(___H___git_23_git_2d_command,___REF_SYM(6,___S___git_23_git_2d_command),___REF_FAL,8,0)
,___DEF_LBL_PROC(___H___git_23_git_2d_command,4,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_command,___IFD(___RETI,4,4,0x3ffL))
,___DEF_LBL_RET(___H___git_23_git_2d_command,___IFD(___RETI,4,4,0x3ffL))
,___DEF_LBL_RET(___H___git_23_git_2d_command,___IFD(___RETI,4,4,0x3ffL))
,___DEF_LBL_RET(___H___git_23_git_2d_command,___IFD(___RETI,5,8,0x3f07L))
,___DEF_LBL_RET(___H___git_23_git_2d_command,___IFD(___RETI,7,8,0x3f07L))
,___DEF_LBL_RET(___H___git_23_git_2d_command,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H___git_23_git_2d_command,___IFD(___RETI,8,8,0x3f01L))
,___DEF_LBL_INTRO(___H___git_23_git_2d_archive,___REF_SYM(4,___S___git_23_git_2d_archive),___REF_FAL,12,0)
,___DEF_LBL_PROC(___H___git_23_git_2d_archive,2,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_archive,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___git_23_git_2d_archive,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H___git_23_git_2d_archive,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_archive,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_archive,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H___git_23_git_2d_archive,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_PROC(___H___git_23_git_2d_archive,1,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_archive,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H___git_23_git_2d_archive,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___git_23_git_2d_archive,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___git_23_git_2d_archive,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_INTRO(___H___git_23_git_2d_clone,___REF_SYM(5,___S___git_23_git_2d_clone),___REF_FAL,23,0)
,___DEF_LBL_PROC(___H___git_23_git_2d_clone,4,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETI,2,4,0x3f7L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETI,8,1,0x3f1fL))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETI,8,8,0x3f01L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETI,8,8,0x3f01L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETI,4,4,0x3f7L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETI,4,4,0x3f7L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETI,4,4,0x3f7L))
,___DEF_LBL_PROC(___H___git_23_git_2d_clone,1,2)
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_PROC(___H___git_23_git_2d_clone,2,1)
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H___git_23_git_2d_clone,___IFD(___RETI,1,0,0x3f1L))
,___DEF_LBL_INTRO(___H___git_23_git_2d_pull,___REF_SYM(8,___S___git_23_git_2d_pull),___REF_FAL,8,0)
,___DEF_LBL_PROC(___H___git_23_git_2d_pull,3,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_pull,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_RET(___H___git_23_git_2d_pull,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_RET(___H___git_23_git_2d_pull,___IFD(___RETI,3,4,0x3f3L))
,___DEF_LBL_PROC(___H___git_23_git_2d_pull,1,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_pull,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H___git_23_git_2d_pull,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H___git_23_git_2d_pull,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H___git_23_git_2d_status,___REF_SYM(10,___S___git_23_git_2d_status),___REF_FAL,8,0)
,___DEF_LBL_PROC(___H___git_23_git_2d_status,3,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_status,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_RET(___H___git_23_git_2d_status,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_RET(___H___git_23_git_2d_status,___IFD(___RETI,3,4,0x3f3L))
,___DEF_LBL_PROC(___H___git_23_git_2d_status,1,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_status,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H___git_23_git_2d_status,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H___git_23_git_2d_status,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H___git_23_git_2d_tag,___REF_SYM(11,___S___git_23_git_2d_tag),___REF_FAL,19,0)
,___DEF_LBL_PROC(___H___git_23_git_2d_tag,1,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_PROC(___H___git_23_git_2d_tag,1,-1)
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H___git_23_git_2d_tag,___IFD(___RETI,8,0,0x3f07L))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(0,___G___git_23_,1)
___DEF_MOD_PRM(6,___G___git_23_git_2d_repository_2d_open,3)
___DEF_MOD_PRM(4,___G___git_23_git_2d_command_2d_aux,16)
___DEF_MOD_PRM(3,___G___git_23_git_2d_command,24)
___DEF_MOD_PRM(1,___G___git_23_git_2d_archive,33)
___DEF_MOD_PRM(2,___G___git_23_git_2d_clone,46)
___DEF_MOD_PRM(5,___G___git_23_git_2d_pull,70)
___DEF_MOD_PRM(7,___G___git_23_git_2d_status,79)
___DEF_MOD_PRM(8,___G___git_23_git_2d_tag,88)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(0,___G___git_23_,1)
___DEF_MOD_GLO(6,___G___git_23_git_2d_repository_2d_open,3)
___DEF_MOD_GLO(4,___G___git_23_git_2d_command_2d_aux,16)
___DEF_MOD_GLO(3,___G___git_23_git_2d_command,24)
___DEF_MOD_GLO(1,___G___git_23_git_2d_archive,33)
___DEF_MOD_GLO(2,___G___git_23_git_2d_clone,46)
___DEF_MOD_GLO(5,___G___git_23_git_2d_pull,70)
___DEF_MOD_GLO(7,___G___git_23_git_2d_status,79)
___DEF_MOD_GLO(8,___G___git_23_git_2d_tag,88)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S__23__23_type_2d_1_2d_AF9B3B94_2d_EE56_2d_4D95_2d_A323_2d_AEE3C97E70FC,"##type-1-AF9B3B94-EE56-4D95-A323-AEE3C97E70FC")

___DEF_MOD_SYM(1,___S__23__23_type_2d_5,"##type-5")
___DEF_MOD_SYM(2,___S___git,"_git")
___DEF_MOD_SYM(3,___S___git_23_,"_git#")
___DEF_MOD_SYM(4,___S___git_23_git_2d_archive,"_git#git-archive")
___DEF_MOD_SYM(5,___S___git_23_git_2d_clone,"_git#git-clone")
___DEF_MOD_SYM(6,___S___git_23_git_2d_command,"_git#git-command")
___DEF_MOD_SYM(7,___S___git_23_git_2d_command_2d_aux,"_git#git-command-aux")
___DEF_MOD_SYM(8,___S___git_23_git_2d_pull,"_git#git-pull")
___DEF_MOD_SYM(9,___S___git_23_git_2d_repository_2d_open,"_git#git-repository-open")
___DEF_MOD_SYM(10,___S___git_23_git_2d_status,"_git#git-status")
___DEF_MOD_SYM(11,___S___git_23_git_2d_tag,"_git#git-tag")
___DEF_MOD_SYM(12,___S___tar,"_tar")
___DEF_MOD_SYM(13,___S_directory,"directory")
___DEF_MOD_SYM(14,___S_fields,"fields")
___DEF_MOD_SYM(15,___S_flags,"flags")
___DEF_MOD_SYM(16,___S_git_2d_repository,"git-repository")
___DEF_MOD_SYM(17,___S_id,"id")
___DEF_MOD_SYM(18,___S_name,"name")
___DEF_MOD_SYM(19,___S_path,"path")
___DEF_MOD_SYM(20,___S_super,"super")
___DEF_MOD_SYM(21,___S_type,"type")
___DEF_MOD_KEY(0,___K_arguments,"arguments")
___DEF_MOD_KEY(1,___K_directory,"directory")
___DEF_MOD_KEY(2,___K_environment,"environment")
___DEF_MOD_KEY(3,___K_path,"path")
___DEF_MOD_KEY(4,___K_pseudo_2d_terminal,"pseudo-terminal")
___DEF_MOD_KEY(5,___K_stderr_2d_redirection,"stderr-redirection")
___DEF_MOD_KEY(6,___K_stdin_2d_redirection,"stdin-redirection")
___DEF_MOD_KEY(7,___K_stdout_2d_redirection,"stdout-redirection")
___END_MOD_SYM_KEY

#endif
