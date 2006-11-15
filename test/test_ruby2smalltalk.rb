#!/usr/local/bin/ruby -w

require 'test/unit' if $0 == __FILE__
require 'ruby2smalltalk'
require 'pt_testcase'

ParseTreeTestCase.testcase_order << "RubyToSmalltalk"
class TestRubyToSmalltalk < ParseTreeTestCase
  def setup
    super
    @processor = RubyToSmalltalk.new
  end

  add_test("alias", :flunk)

  add_test("and",
           "self a & self b")

  add_test("argscat", :flunk)
  add_test("argspush", :flunk)

  add_test("array",
           "{ 1. #b. 'c' }")

  add_test("attrasgn", :flunk)
  add_test("attrset", :flunk)
  add_test("back_ref", :flunk)
  add_test("begin", :flunk)
  add_test("block_pass", :flunk)
  add_test("bmethod", :flunk)
  add_test("break", :flunk)
  add_test("break_arg", :flunk)

  add_test("call",
           "self method")

  add_test("call_arglist",
           "self puts: { 42 }")

  add_test("case", :flunk)
  add_test("cdecl", :flunk)

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

  add_test("colon2", :flunk)
  add_test("colon3", :flunk)

  add_test("conditional1",
           "[ 42 == 0 ] ifTrue: [ ^ 1 ]")

  add_test("conditional2",
           "[ 42 == 0 ] ifFalse: [ ^ 2 ]")

  add_test("conditional3",
           "[ 42 == 0 ] ifTrue: [ ^ 3 ] ifFalse: [ ^ 4 ]")

  add_test("conditional4",
           "[ 42 == 0 ] ifTrue: [ ^ 2 ] ifFalse: [ [ 42 < 0 ] ifTrue: [ ^ 3 ] ifFalse: [ ^ 4 ] ]")

  add_test("conditional5",
           "[ true ] ifFalse: [ [ false ] ifTrue: [ ^ ] ]")

  add_test("const",
           "X")
  add_test("cvar", :flunk)
  add_test("cvasgn", :flunk)
  add_test("cvdecl", :flunk)
  add_test("dasgn", :flunk)

  unsupported_tests "defined"

  add_test("defn_args",
"x: args

  | a b c d |

  a := args removeFirst.
  b := args removeFirst.
  d := args removeLast.
  c := args.
  [ b isNil ] ifTrue: [ b := 42 ].

  self p: { a. b. c. d }")

  add_test("defn_empty",
           "empty\n\n  nil")

  add_test("defn_is_something",
           "is_something\n\n  nil")

  add_test("defn_or",
           "| o\n\n  nil")

  add_test("defn_zarray",
           "zarray\n\n  a := {}.\n  ^ a")

  add_test("defs",
           "x: y\n\n  y + 1")

  add_test("dmethod", :flunk)
  add_test("dot2", :flunk)
  add_test("dot3", :flunk)
  add_test("dregx", :flunk)
  add_test("dregx_once", :flunk)
  add_test("dstr", :flunk)
  add_test("dsym", :flunk)
  add_test("dxstr", :flunk)
  add_test("ensure", :flunk)

  add_test("false",
           "false")

  add_test("fbody", :flunk)
  add_test("fcall",
           "self p: { 4 }")
  add_test("flip2", :flunk)
  add_test("flip3", :flunk)
  add_test("for", :flunk)
  add_test("gasgn", :flunk)
  add_test("global", :flunk)
  add_test("gvar", :flunk)
  add_test("hash", :flunk)
  add_test("iasgn", :flunk)

  add_test("iteration1",
           "self loop: [\n]")

  add_test("iteration2", # FIX
           "array := { 1. 2. 3 }.\narray each: [ :x |\n  self puts: { x to_s }\n]")

  add_test("iteration3",
           "1 upto: { 3 } do: [ :n |\n  self puts: { n to_s }\n]") # FIX

  add_test("iteration4",
           "3 downto: { 1 } do: [ :n |\n  self puts: { n to_s }\n]") # FIX

  add_test("iteration5",
           "argl := 10.\n[ argl >= 1 ] whileTrue: [ self puts: { 'hello' }.\nargl := argl - 1 ]")

  add_test("iteration6",
           "array1 := { 1. 2. 3 }.
array2 := { 4. 5. 6. 7 }.
array1 each: [ :x |
  array2 each: [ :y |
    self puts: { x to_s }.
    self puts: { y to_s }
  ]
]")

  add_test("ivar", :flunk)

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
  # add_test("lit_range2", :flunk)
  # add_test("lit_range3", :flunk)
  add_test("lit_regexp", "'x'") # FIX
  add_test("lit_str", "'x'")
  add_test("lit_sym", "#x")
  add_test("masgn", :flunk)
  add_test("match", :flunk)
  add_test("match2", :flunk)
  add_test("match3", :flunk)
  add_test("module", :flunk)
  add_test("next", :flunk)
  add_test("not", :flunk)
  add_test("nth_ref", :flunk)
  add_test("op_asgn1", :flunk)
  add_test("op_asgn2", :flunk)
  add_test("op_asgn_and", :flunk)
  add_test("op_asgn_or", :flunk)
  add_test("or",
           "a | b")
  add_test("postexe", :flunk)
  add_test("redo", :flunk)
  add_test("rescue_block", :flunk)
  add_test("rescue_exceptions", :flunk)
  add_test("retry", :flunk)
  add_test("sclass", :flunk)
  add_test("splat", :flunk)

  add_test("super",
           "x\n\n  super x: { 4 }")

  add_test("super_multi",
           "x\n\n  super x: { 4. 2. 1 }")

  add_test("svalue", :flunk)
  add_test("to_ary", :flunk)

  add_test("true",
           "true")

  add_test("undef", :flunk)
  add_test("undef_multi", :flunk)
  add_test("until_post", 
           "[ 1 + 1. false ] whileFalse")
  add_test("until_pre", 
           "[ false ] whileFalse: [ 1 + 1 ]")
  add_test("valias", :flunk)
  add_test("vcall",
           "self method")
  add_test("while_post", 
           "[ 1 + 1. false ] whileTrue")
  add_test("while_pre", 
           "[ false ] whileTrue: [ 1 + 1 ]")
  add_test("xstr", :flunk)
  add_test("yield", :flunk)
  add_test("yield_arg", :flunk)
  add_test("yield_args", :flunk)

  add_test("zarray",
           "a := {}")

  add_test("zsuper",
           "x\n\n  super x")
end
