#!/usr/local/bin/ruby -w

require 'minitest/autorun'
require 'ruby2smalltalk'
require 'pt_testcase'

class SmalltalkTestCase < ParseTreeTestCase
  i_suck_and_my_tests_are_order_dependent!

  testcase_order << "RubyToSmalltalk"

  def self.skip *names
    names.each do |name|
      add_tests(name, "RubyToSmalltalk" => :skip)
    end
  end

  skip("alias",
       "argspush",
       "attrasgn",
       "attrset",
       "back_ref",
       "begin",
       "bmethod",
       "break",
       "break_arg",
       "case",
       "cdecl",
       "colon2",
       "colon3",
       "cvar",
       "cvasgn",
       "cvdecl",
       "defs",
       "dmethod",
       "dot2",
       "dot3",
       "dregx",
       "dregx_once",
       "dstr",
       "dsym",
       "dxstr",
       "ensure",
       "fbody",
       "flip2",
       "flip3",
       "for",
       "gasgn",
       "global",
       "gvar",
       "hash",
       "iasgn",
       "ivar",
       "masgn",
       "match",
       "match2",
       "match3",
       "module",
       "next",
       "nth_ref",
       "op_asgn1",
       "op_asgn2",
       "op_asgn_and",
       "op_asgn_or",
       "postexe",
       "redo",
       "rescue_exceptions",
       "retry",
       "sclass",
       "splat",
       "svalue",
       "to_ary",
       "undef",
       "valias",
       "xstr",
       "alias_ugh",
       "argscat_inside",
       "argscat_svalue",
       "array_pct_W",
       "array_pct_W_dstr",
       "array_pct_w",
       "array_pct_w_dstr",
       "attrasgn_index_equals",
       "attrasgn_index_equals_space",
       "begin_def",
       "begin_rescue_ensure",
       "begin_rescue_ensure_all_empty",
       "begin_rescue_twice",
       "begin_rescue_twice_mri_verbose_flag",
       "block_attrasgn",
       "block_lasgn",
       "block_mystery_block",
       "block_pass_args_and_splat",
       "block_pass_call_0",
       "block_pass_call_1",
       "block_pass_call_n",
       "block_pass_fcall_0",
       "block_pass_fcall_1",
       "block_pass_fcall_n",
       "block_pass_omgwtf",
       "block_pass_splat",
       "block_pass_thingy",
       "block_stmt_after",
       "block_stmt_after_mri_verbose_flag",
       "block_stmt_before",
       "block_stmt_before_mri_verbose_flag",
       "block_stmt_both",
       "block_stmt_both_mri_verbose_flag",
       "bmethod_noargs",
       "bmethod_splat",
       "call_arglist_hash",
       "call_arglist_norm_hash",
       "call_command",
       "call_expr",
       "call_index",
       "call_index_no_args",
       "call_index_space",
       "call_no_space_symbol",
       "call_unary_neg",
       "case_nested",
       "case_nested_inner_no_expr",
       "case_no_expr",
       "case_splat",
       "class_plain",
       "class_scoped",
       "class_scoped3",
       "class_super_array",
       "class_super_expr",
       "class_super_object",
       "constX",
       "constY",
       "constZ",
       "cvasgn_cls_method",
       "dasgn_0",
       "dasgn_1",
       "dasgn_2",
       "dasgn_curr",
       "dasgn_icky",
       "dasgn_mixed",
       "defn_args_block",
       "defn_args_mand",
       "defn_args_mand_block",
       "defn_args_mand_opt",
       "defn_args_mand_opt_block",
       "defn_args_mand_opt_splat",
       "defn_args_mand_opt_splat_block",
       "defn_args_mand_opt_splat_no_name",
       "defn_args_mand_splat",
       "defn_args_mand_splat_block",
       "defn_args_mand_splat_no_name",
       "defn_args_none",
       "defn_args_opt",
       "defn_args_opt_block",
       "defn_args_opt_splat",
       "defn_args_opt_splat_block",
       "defn_args_opt_splat_no_name",
       "defn_args_splat",
       "defn_args_splat_no_name",
       "defn_rescue",
       "defn_rescue_mri_verbose_flag",
       "defn_something_eh",
       "defn_splat_no_name",
       "defs_empty",
       "defs_empty_args",
       "defs_expr_wtf",
       "dregx_interp",
       "dregx_interp_empty",
       "dregx_n",
       "dregx_once_n_interp",
       "dstr_2",
       "dstr_3",
       "dstr_concat",
       "dstr_gross",
       "dstr_heredoc_expand",
       "dstr_heredoc_windoze_sucks",
       "dstr_heredoc_yet_again",
       "dstr_nest",
       "dstr_str_lit_start",
       "dstr_the_revenge",
       "fcall_arglist",
       "fcall_arglist_hash",
       "fcall_arglist_norm_hash",
       "fcall_block",
       "fcall_index_space",
       "fcall_inside_parens",
       "fcall_keyword",
       "flip2_method",
       "for_no_body",
       "gvar_underscore",
       "gvar_underscore_blah",
       "hash_rescue",
       "if_block_condition",
       "if_lasgn_short",
       "if_nested",
       "if_post",
       "if_pre",
       "iter_call_arglist_space",
       "iter_dasgn_curr_dasgn_madness",
       "iter_downto",
       "iter_each_lvar",
       "iter_each_nested",
       "iter_loop_empty",
       "iter_masgn_2",
       "iter_masgn_args_splat",
       "iter_masgn_args_splat_no_name",
       "iter_masgn_splat",
       "iter_masgn_splat_no_name",
       "iter_shadowed_var",
       "iter_upto",
       "iter_while",
       "lambda_args_anon_star",
       "lambda_args_anon_star_block",
       "lambda_args_block",
       "lambda_args_norm_anon_star",
       "lambda_args_norm_anon_star_block",
       "lambda_args_norm_block",
       "lambda_args_norm_comma",
       "lambda_args_norm_comma2",
       "lambda_args_norm_star",
       "lambda_args_norm_star_block",
       "lambda_args_star",
       "lambda_args_star_block",
       "lit_long_negative",
       "lit_regexp_i_wwtt",
       "lit_regexp_n",
       "lit_regexp_once",
       "lit_sym_splat",
       "lvar_def_boundary",
       "masgn_argscat",
       "masgn_attrasgn",
       "masgn_attrasgn_array_rhs",
       "masgn_attrasgn_idx",
       "masgn_cdecl",
       "masgn_iasgn",
       "masgn_masgn",
       "masgn_splat_lhs",
       "masgn_splat_no_name_to_ary",
       "masgn_splat_no_name_trailing",
       "masgn_splat_rhs_1",
       "masgn_splat_rhs_n",
       "masgn_splat_to_ary",
       "masgn_splat_to_ary2",
       "module2",
       "module_scoped",
       "module_scoped3",
       "next_arg",
       "op_asgn1_ivar",
       "op_asgn2_self",
       "op_asgn_and_ivar2",
       "op_asgn_or_block",
       "op_asgn_or_ivar",
       "op_asgn_or_ivar2",
       "or_big",
       "or_big2",
       "parse_floats_as_args",
       "proc_args_0",
       "proc_args_1",
       "proc_args_2",
       "proc_args_no",
       "rescue",
       "rescue_block_body",
       "rescue_block_body_3",
       "rescue_block_body_ivar",
       "rescue_block_nada",
       "rescue_iasgn_var_empty",
       "rescue_lasgn",
       "rescue_lasgn_var",
       "rescue_lasgn_var_empty",
       "return_0",
       "return_1",
       "return_1_splatted",
       "return_n",
       "sclass_multiple",
       "sclass_trailing_class",
       "splat_array",
       "splat_break",
       "splat_break_array",
       "splat_fcall",
       "splat_fcall_array",
       "splat_lasgn",
       "splat_lasgn_array",
       "splat_lit_1",
       "splat_lit_n",
       "splat_next",
       "splat_next_array",
       "splat_return",
       "splat_return_array",
       "splat_super",
       "splat_super_array",
       "splat_yield",
       "splat_yield_array",
       "str",
       "str_concat_newline",
       "str_concat_space",
       "str_heredoc",
       "str_heredoc_call",
       "str_heredoc_double",
       "str_heredoc_empty",
       "str_heredoc_indent",
       "str_interp_file",
       "structure_extra_block_for_dvar_scoping",
       "structure_remove_begin_1",
       "structure_remove_begin_2",
       "super_0",
       "super_1",
       "super_1_array",
       "super_block_pass",
       "super_block_splat",
       "super_n",
       "ternary_nil_no_space",
       "ternary_symbol_no_spaces",
       "undef_2",
       "undef_3",
       "undef_block_1",
       "undef_block_2",
       "undef_block_3",
       "undef_block_3_post",
       "undef_block_wtf",
       "unless_post",
       "unless_pre",
       "until_pre_mod",
       "while_post2",
       "while_pre_mod",
       "while_pre_nil",
       "yield_0",
       "yield_1",
       "yield_array_0",
       "yield_array_1",
       "yield_array_n",
       "yield_n")

end

class TestRubyToSmalltalk < SmalltalkTestCase

  def setup
    super
    @processor = RubyToSmalltalk.new
  end

  add_test("and",
           "self a & self b")

  add_test("array",
           "{ 1. #b. 'c' }")

  add_test("call",
           "self method")

  add_test("call_arglist",
           "self o puts: { 42 }")

  add_test("class",
           "Array subclass: #X
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'ruby2smalltalk'!

!X methodsFor: 'translated'!
blah

  self puts: { 'hello' }! !")

  add_test("class_sans",
           "Object subclass: #X
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'ruby2smalltalk'!

!X methodsFor: 'translated'!
blah

  self puts: { 'hello' }! !")

  # add_test("conditional1",
  #          "[ 42 == 0 ] ifTrue: [ ^ 1 ]")
  #
  # add_test("conditional2",
  #          "[ 42 == 0 ] ifFalse: [ ^ 2 ]")
  #
  # add_test("conditional3",
  #          "[ 42 == 0 ] ifTrue: [ ^ 3 ] ifFalse: [ ^ 4 ]")
  #
  # add_test("conditional4",
  #          "[ 42 == 0 ] ifTrue: [ ^ 2 ] ifFalse: [ [ 42 < 0 ] ifTrue: [ ^ 3 ] ifFalse: [ ^ 4 ] ]")
  #
  # add_test("conditional5",
  #          "[ true ] ifFalse: [ [ false ] ifTrue: [ ^ ] ]")

  add_test("const",
           "X")

  unsupported_tests "defined"

#   add_test("defn_args",
# "x: args
#
#   | a b c d |
#
#   a := args removeFirst.
#   b := args removeFirst.
#   d := args removeLast.
#   c := args.
#   [ b isNil ] ifTrue: [ b := 42 ].
#
#   self p: { a. b. c. d }")
#
#   add_test("defn_empty",
#            "empty\n\n  nil")

  # add_test("defn_is_something",
  #          "is_something\n\n  nil")

  add_test("defn_or",
           "| o\n\n  nil")

  add_test("defn_zarray",
           "zarray\n\n  a := {}.\n  ^ a")

  # "Ruby"         => "def self.x(y)\n  (y + 1)\nend",

  add_test("false",
           "false")

  # add_test("fcall",
  #          "self p: { 4 }")

#   add_test("iteration1",
#            "self loop: [\n]")
#
#   add_test("iteration2", # FIX
#            "array := { 1. 2. 3 }.\narray each: [ :x |\n  self puts: { x to_s }\n]")
#
#   add_test("iteration3",
#            "1 upto: { 3 } do: [ :n |\n  self puts: { n to_s }\n]") # FIX
#
#   add_test("iteration4",
#            "3 downto: { 1 } do: [ :n |\n  self puts: { n to_s }\n]") # FIX
#
#   add_test("iteration5",
#            "argl := 10.\n[ argl >= 1 ] whileTrue: [ self puts: { 'hello' }.\nargl := argl - 1 ]")
#
#   add_test("iteration6",
#            "array1 := { 1. 2. 3 }.
# array2 := { 4. 5. 6. 7 }.
# array1 each: [ :x |
#   array2 each: [ :y |
#     self puts: { x to_s }.
#     self puts: { y to_s }
#   ]
# ]")

  add_test("lasgn_array",
           "var := { 'foo'. 'bar' }")

  add_test("lasgn_call",
           "c := 2 + 3")

  add_test("lit_bool_false",
           "false")

  add_test("lit_bool_true",
           "true")

  add_test("lit_float",
           "1.1")

  add_test("lit_long",
           "1")

  unsupported_tests "lit_range2", "lit_range3"
  add_test("lit_regexp", "'x'") # FIX
  add_test("lit_sym", "#x")
  add_test("or",
           "self a | self b")

  # add_test("super",
  #          "x\n\n  super x: { 4 }")
  # 
  # add_test("super_multi",
  #          "x\n\n  super x: { 4. 2. 1 }")

  add_test("true",
           "true")

  add_test("until_post",
           "[ 1 + 1. false ] whileFalse")
  add_test("until_pre",
           "[ false ] whileFalse: [ 1 + 1 ]")
  add_test("vcall",
           "self method")
  add_test("while_post",
           "[ 1 + 1. false ] whileTrue")
  add_test("while_pre",
           "[ false ] whileTrue: [ 1 + 1 ]")

  add_test("zarray",
           "a := {}")

  add_test("zsuper",
           "x\n\n  super x")

end
