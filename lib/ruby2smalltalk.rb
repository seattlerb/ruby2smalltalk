# -*- ruby -*-

require 'rubygems'
require 'sexp_processor'
require 'ruby_parser'

class RubyToSmalltalk < SexpProcessor
  VERSION = '1.0.0'
  BINARY_MSGS = [ :==, :<, :>, :<=, :>=, :+, :-, :*, :/, :| ]
  KEYWORD_MSGS = [ :puts, :upto, :downto, :p ]

  def self.translate(src, f = "(string)", t = 10) # parens for emacs *sigh*
    self.new.process(RubyParser.new.process(src, f, t))
  end

  def initialize
    super
    self.auto_shift_type = true
    self.strict = true
    self.expected = String
    self.unsupported << :defined
    @indent = "  "
  end

  def process_and(exp)
    "#{process(exp.shift)} & #{process(exp.shift)}"
  end

  def process_arglist(exp)
    r = []
    r << process(exp.shift) until exp.empty?
    "{ #{r.join(". ")} }"
  end

  def process_args(exp)
    args = []
    args << exp.shift.to_s until exp.empty?
    args.join(", ")
  end

  def process_array(exp)
    return "{}" if exp.empty?
    r = []
    r << process(exp.shift) until exp.empty?
    "{ #{r.join('. ')} }"
  end

  def process_block(exp)
    r = []
    r << process(exp.shift) until exp.empty?
    r.join(".\n")
  end

  def process_block_arg(exp)
    exp.shift.to_s
  end

  def process_call(exp)
    recv = exp.shift
    mesg = exp.shift
    args = exp.dup
    arity = args.size

    exp.clear

    lhs = process(recv)
    msg = mesg

    lhs ||= "self"

    rhs = process_arglist args

    KEYWORD_MSGS << msg # TODO: why?

    case arity
    when 0 # unary
      "#{lhs} #{msg}"
    else
      case msg
      when *BINARY_MSGS then
        rhs = $1 if rhs =~ /^\{ (.*) \}$/
        lhs = "( #{lhs} )" if lhs =~ / / # HACK - look at sexp
        rhs = "( #{rhs} )" if rhs =~ / / # HACK - look at sexp
        "#{lhs} #{msg} #{rhs}"
      when *KEYWORD_MSGS then
        case msg
        when :upto, :downto then # HACK
          "#{lhs} #{msg}: #{rhs} do"
        else
          "#{lhs} #{msg}: #{rhs}"
        end
      else
        raise "message not handled: #{msg}"
      end
    end
  end

  def process_class(exp)
    new_exp = Sexp.from_array exp
    exp.clear
    exp = new_exp

    name = exp.shift
    supr = process(exp.shift) || "Object"

    exp = exp.scope
    exp = exp.block if exp.block
    exp.shift # :scope or :block

    methods = []
    methods << process(exp.shift) until exp.empty?

    "#{supr} subclass: ##{name}
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'ruby2smalltalk'!\n\n" + methods.map { |method|
      "!#{name} methodsFor: 'translated'!\n#{method}! !"
    }.join("\n\n")
  end

  def process_const(exp)
    exp.shift.to_s
  end

  def process_dasgn_curr(exp)
    exp.shift.to_s
  end

  def process_defn(exp)
    name = exp.shift
    args = exp.shift
    body = exp.dup

    exp.clear

    @method_name = name # for zsuper rewriters and such

    opt_args = args.block(true)
    block_arg = process(body.block_arg(true))

    body = process_block body
    arity = args.size - 1

    name = case name.to_s
           when /\?$/ then
             "is_#{name.to_s[0..-2]}".intern
           else
             name
           end

      case arity
      when 0 then
        "#{name}\n\n#{indent(body)}"
      when 1 then # binary or keyword?
        if BINARY_MSGS.include? name then
          "#{name} #{process(args)}\n\n#{indent(body)}"
        else
          arg = process(args)
          <<-"ST80".gsub(/^            /, "").chomp
            #{name}: args

              | #{arg} |
              ( args class == Array ) ifTrue: [ #{arg} := args at: 1. ] ifFalse: [ #{arg} := args ].

            #{indent(body)}
          ST80
        end
      else
        r = "#{name}: args\n\n"
        r << "  | "
        args.shift # :args
        r << args.map { |arg2|
          case arg2.to_s
          when /^\*/ then
            arg2.to_s[1..-1]
          else
            arg2.to_s
          end
        }.join(" ") + (block_arg ? " #{block_arg}" : "")

        r << " |\n\n"

        args.each do |arg2|
          case arg2.to_s
          when /^\*/ then
            r << indent("#{block_arg} := args removeLast.\n") if block_arg
            r << indent("#{arg2.to_s[1..-1]} := args.\n")
          else
            r << indent("#{arg2} := args removeFirst.\n")
          end
        end

        if opt_args then
          opt_args.each_with_index do |subexp, i|
            next if i == 0
            var = subexp[1]
            opt_args[i] = s(:if, s(:call, s(:lvar, var), :isNil), subexp)
          end
          r << indent(process(opt_args))
        end

        r << ".\n\n"
        r << "#{indent(body)}"
        r
      end
  end

  def process_defs(exp)
    # HACK - figger out the class chunk thingy
    exp.shift # HACK target of class
    process_defn(exp)
  end

  def process_dvar(exp)
    exp.shift.to_s
  end

#   def process_global(exp)
#     abort exp.inspect
#   end

  def process_false(exp)
    "false"
  end

  def process_fcall(exp)
    exp.unshift [:self]
    process_call(exp)
  end

  def process_iasgn(exp)
    var = exp.shift
    val = process exp.shift

    "self #{var}: #{val}"
  end

  def process_if(exp)
    c = process(exp.shift)
    t = process(exp.shift)
    f = process(exp.shift)

    result = []
    result << "( #{c} )"
    result << " ifTrue: [ #{t} ]" if t
    result << " ifFalse: [ #{f} ]" if f
    result.join
  end

  def process_iter(exp)
    iter = process exp.shift
    args = process exp.shift
    body = process exp.shift

    r = []
    r << "#{iter}: ["
    r << " #{args.map { |a| ":#{a}" } } |" if args
    r << "\n"
    r << "#{indent(body)}\n" if body
    r << "]"

    r.join
  end

#   def process_ivar(exp)
#     abort exp.inspect
#   end

  def process_lasgn(exp)
    lhs = exp.shift
    rhs = process(exp.shift)
    "#{lhs} := #{rhs}"
  end

  def process_lit(exp)
    val = exp.shift
    case val
    when Numeric then
      val.to_s
    when Regexp then
      regexp = val.to_s.split(/:/, 2).last[0..-2]
      "'#{regexp}'"
    when Symbol then
      "##{val}"
    when Range then
      raise UnsupportedNodeError, "Currently we don't do ranges"
    else
      raise "no: #{val.inspect}/#{val.class}"
    end
  end

  def process_lvar(exp)
    "#{exp.shift}"
  end

  def process_masgn(exp)
    lhs = exp.shift
    rhs = exp.shift

    assert_type lhs, :array
    lhs.shift
    lhs = lhs.map { |l| process(l) }

    unless rhs.nil? then
      # HACK - but seems to work (see to_ary test)      assert_type rhs, :array
      rhs.shift
      rhs = rhs.map { |r| process(r) }
      lhs.zip(rhs).map do |l,r|
        "#{l} := #{r}."
      end.join("\n")
    else
      return lhs.join(", ")
    end
  end

  def process_nil(exp)
    "nil"
  end

  def process_not(exp)
    "#{process(exp.shift)} not"
  end

  # b[2] &&= 11
  # b[3] += 12

  def process_op_asgn1(exp)
    # b[1] ||= 10 <=> b at: 1 put: ((b at: 1) rubyFalse or: [ 11 ])
    lhs = process exp.shift # b
    rhs = process exp.shift # 1
    op  = exp.shift         # ||
    val = process exp.shift # 10

    case op
    when :"||" then
      op, val = "rubyFalse or:", "[ #{val} ]"
    when :"&&" then
      op, val = "rubyFalse and:", "[ #{val} ]"
    else
      raise "unsupported op #{op}"
    end

    "#{lhs} at: #{rhs} put: ((#{lhs} at: #{rhs}) rubyFalse #{op} #{val})"
  end

  def process_op_asgn2(exp)
    # c.var ||= 20 <=> c.var: c.var rubyFalse or: [ 20 ]

    "#{lhs} at: #{rhs} put: ((#{lhs} at: #{rhs}) rubyFalse #{op} #{val})"
  end

#   def process_op_asgn_and(exp)
#     abort exp.inspect
#   end

#   def process_op_asgn_or(exp)
#     abort exp.inspect
#   end

  def process_or(exp)
    "#{process(exp.shift)} | #{process(exp.shift)}"
  end

  def process_return(exp)
    "^" + (exp.empty? ? "" : " #{process(exp.shift)}")
  end

  def process_scope(exp)
    process exp.shift
  end

  def process_self(exp)
    "self"
  end

  def process_str(exp)
    str = exp.shift.inspect[1..-2]
    "'#{str}'"
  end

  def process_super(exp)
    args = exp.shift
    args.shift # :array
    "super #{@method_name}: #{process_arglist args}"
  end

  def process_true(exp)
    "true"
  end

  def process_until(exp)
    cond = process(exp.shift)
    body = process(exp.shift)
    head_controlled = exp.empty? ? false : exp.shift

    if head_controlled then
      "[ #{cond} ] whileFalse: [ #{body} ]"
    else
      "[ #{body}. #{cond} ] whileFalse"
    end
  end

  def process_while(exp)
    cond = process(exp.shift)
    body = process(exp.shift)
    head_controlled = exp.empty? ? false : exp.shift

    if head_controlled then
      "[ #{cond} ] whileTrue: [ #{body} ]"
    else
      "[ #{body}. #{cond} ] whileTrue"
    end
  end

  def process_vcall(exp)
    # TODO: args? should be a keyword expression I guess
    "self #{exp.shift}"
  end

  def process_zsuper(exp)
    "super #{@method_name}"
  end

  ############################################################

  def indent(s)
    s.lines.map{|line| @indent + line}.join
  end
end
