require 'test/unit'
require 'inliner'

class TestInliner < Test::Unit::TestCase

  class InlineTargetOrig
    def caller
      v1 = 2 + 3
      x = callee(v1)
      x + 2
    end

    def callee(v)
      v + 5
    end

    def caller_blockarg
      block = proc do |x| x + 1 end
      callee_blockarg(&block)
    end

    def callee_blockarg(&block)
      1.times(&block)
    end

    def caller_nasty
      block = proc do |x| x + 1 end
      callee_nasty(1, 2, *[3, 4], &block)
    end

    def callee_nasty(a, b = nil, *c, &block)
      1.times(&block)
    end

    def caller_rescue
      callee_rescue
    end

    def callee_rescue
    rescue ArgumentError
    end

    def caller_starargs
      callee_starargs 1, 2
    end

    def callee_starargs(*args)
      args
    end

    def caller_varargs
      callee_varargs 1
    end

    def callee_varargs(a, b = 2)
      a + b
    end

    # unsupported/error

    def caller_block
      callee_block 5 do |x| x + 1 end
    end

    def callee_block(v)
      v = v * 2
      w = yield v
      w * 2
    end

    def callee_return
      return 5
    end
  end

  def setup
    super

    @processor = Inliner.new

    self.class.send :remove_const, :InlineTarget if
      self.class.constants.include? 'InlineTarget'
    self.class.const_set :InlineTarget, InlineTargetOrig.dup
  end

  def test_extract_expression
    args, block_arg, defaults, body_expr =
      @processor.extract_expression(InlineTarget, :callee)

    assert_equal s(:array, s(:lasgn, :inline_callee_v)), args

    expected = s(:block,
                 s(:call,
                   s(:lvar, :inline_callee_v),
                   :+,
                   s(:array, s(:lit, 5))))

    assert_equal s(), defaults

    assert_equal expected, body_expr
  end

  def test_extract_expression_block
    e = assert_raise Inliner::Error do
      @processor.extract_expression(InlineTarget, :callee_block)
    end

    assert_equal "yield gets slower", e.message
  end

  def test_extract_expression_blockarg
    args, block_arg, defaults, body_expr =
      @processor.extract_expression(InlineTarget, :callee_blockarg)

    assert_equal s(:array), args

    assert_equal s(:lasgn, :inline_callee_blockarg_block), block_arg

    expected = s(:block,
                 s(:block_pass,
                   s(:lvar, :inline_callee_blockarg_block),
                   s(:call, s(:lit, 1), :times)))

    assert_equal s(), defaults

    assert_equal expected, body_expr
  end

  def test_extract_expression_nasty
    args, block_arg, defaults, body_expr =
      @processor.extract_expression(InlineTarget, :callee_nasty)

    expected_args = s(:array,
                      s(:lasgn, :inline_callee_nasty_a),
                      s(:lasgn, :inline_callee_nasty_b),
                      s(:lasgn, :"*inline_callee_nasty_c"))
    assert_equal expected_args, args

    assert_equal s(:lasgn, :inline_callee_nasty_block), block_arg

    assert_equal s(:block, s(:lasgn, :b, s(:nil))), defaults

    expected = s(:block,
                s(:block_pass,
                 s(:lvar, :inline_callee_nasty_block),
                 s(:call, s(:lit, 1), :times)))

    assert_equal expected, body_expr
  end

  def test_extract_expression_rescue
    args, block_arg, defaults, body_expr =
      @processor.extract_expression(InlineTarget, :callee_rescue)

    assert_equal s(:array), args

    assert_equal s(), defaults

    expected = s(:begin,
                 s(:rescue,
                   s(:resbody,
                     s(:array,
                       s(:const, :ArgumentError)))))

    assert_equal expected, body_expr
  end

  def test_extract_expression_return
    e = assert_raise Inliner::Error do
      @processor.extract_expression(InlineTarget, :callee_return)
    end

    assert_equal "return unsupported", e.message
  end

  def test_extract_expression_starargs
    args, block_arg, defaults, body_expr =
      @processor.extract_expression(InlineTarget, :callee_starargs)

    expected_args = s(:array,
                      s(:lasgn, :"*inline_callee_starargs_args"))
    assert_equal expected_args, args

    assert_equal s(), defaults

    expected = s(:block,
                 s(:lvar, :inline_callee_starargs_args))

    assert_equal expected, body_expr
  end

  def test_extract_expression_varargs
    args, block_arg, defaults, body_expr =
      @processor.extract_expression(InlineTarget, :callee_varargs)

    expected_args = s(:array,
                      s(:lasgn, :inline_callee_varargs_a),
                      s(:lasgn, :inline_callee_varargs_b))
    assert_equal expected_args, args

    assert_equal s(:block, s(:lasgn, :b, s(:lit, 2))), defaults

    expected = s(:block,
                 s(:call,
                   s(:lvar, :inline_callee_varargs_a),
                   :+,
                   s(:array, s(:lvar, :inline_callee_varargs_b))))

    assert_equal expected, body_expr
  end

  def test_inline_into
    expected = [:defn, :caller,
      [:scope,
        [:block,
          [:args],
          [:lasgn, :v1, [:call, [:lit, 2], :+, [:array, [:lit, 3]]]],
          [:lasgn, :x,
            [:block,
              [:lasgn, :inline_callee_v, [:lvar, :v1]],
              [:call, [:lvar, :inline_callee_v], :+, [:array, [:lit, 5]]]]],
          [:call, [:lvar, :x], :+, [:array, [:lit, 2]]]]]] 

    @processor.inline_into InlineTarget, :caller, :callee

    it = InlineTarget.new
    assert_equal expected, it.method(:caller).to_sexp
  end

  def test_inline_into_blockarg
    expected = [:defn, :caller_blockarg,
      [:scope,
        [:block,
          [:args],
            [:lasgn, :block,
              [:iter,
                [:fcall, :proc],
                [:dasgn_curr, :x],
                [:call, [:dvar, :x], :+, [:array, [:lit, 1]]]]],
            [:block,
              [:lasgn, :inline_callee_blockarg_block, [:lvar, :block]],
              [:block_pass,
                [:lvar, :inline_callee_blockarg_block,],
                [:call, [:lit, 1], :times]]]]]]

    @processor.inline_into InlineTarget, :caller_blockarg, :callee_blockarg

    it = InlineTarget.new

    assert_equal expected, it.method(:caller_blockarg).to_sexp
  end

  def test_inline_into_error
    expected = [:defn, :caller_block,
      [:scope,
        [:block,
          [:args],
          [:iter,
            [:fcall, :callee_block, [:array, [:lit, 5]]],
            [:dasgn_curr, :x],
            [:call, [:dvar, :x], :+, [:array, [:lit, 1]]]]]]]

    @processor.inline_into InlineTarget, :caller_block, :callee_block

    it = InlineTarget.new
    assert_equal expected, it.method(:caller_block).to_sexp
  end

  def test_inline_into_nasty
    expected = [:defn, :caller_nasty,
      [:scope,
        [:block,
          [:args],
          [:lasgn, :block,
            [:iter,
              [:fcall, :proc],
              [:dasgn_curr, :x],
              [:call, [:dvar, :x], :+, [:array, [:lit, 1]]]]],
          [:block,
            [:masgn,
              [:array,
                [:lasgn, :inline_callee_nasty_a],
                [:lasgn, :inline_callee_nasty_b]],
              [:lasgn, :inline_callee_nasty_c],
              [:argscat,
                [:array, [:lit, 1], [:lit, 2]],
                [:array, [:lit, 3], [:lit, 4]]]],
            [:lasgn, :inline_callee_nasty_block, [:lvar, :block]],
            [:block_pass,
              [:lvar, :inline_callee_nasty_block],
              [:call, [:lit, 1], :times]]]]]]


    @processor.inline_into InlineTarget, :caller_nasty, :callee_nasty

    it = InlineTarget.new

    assert_equal expected, it.method(:caller_nasty).to_sexp
  end

  def test_inline_into_starargs
    expected = [:defn, :caller_starargs,
      [:scope,
        [:block,
          [:args],
          [:block,
            [:masgn,
              [:lasgn, :inline_callee_starargs_args],
              [:array, [:lit, 1], [:lit, 2]]],
            [:lvar, :inline_callee_starargs_args]]]]]

    @processor.inline_into InlineTarget, :caller_starargs, :callee_starargs

    it = InlineTarget.new
    assert_equal expected, it.method(:caller_starargs).to_sexp
  end

  def test_inline_into_varargs
    expected = [:defn, :caller_varargs,
      [:scope,
        [:block,
          [:args],
          [:block,
            [:masgn,
              [:array,
                [:lasgn, :inline_callee_varargs_a],
                [:lasgn, :inline_callee_varargs_b]],
              [:array, [:lit, 1], [:lit, 2]]],
            [:call,
              [:lvar, :inline_callee_varargs_a],
              :+,
              [:array, [:lvar, :inline_callee_varargs_b]]]]]]]

    @processor.inline_into InlineTarget, :caller_varargs, :callee_varargs

    it = InlineTarget.new
    assert_equal expected, it.method(:caller_varargs).to_sexp
  end

  def test_rename
    @processor.defn_name = :foo
    assert_equal :inline_foo_arg, @processor.rename(:arg),
                 "rename #{@processor.defn_name}"

    @processor.defn_name = :"foo?"
    assert_equal :inline_foo_eh_arg, @processor.rename(:arg),
                 "rename #{@processor.defn_name}"
  end

  def test_replace_block_pass
    sexp = Sexp.for InlineTarget, :caller_blockarg

    @processor.replace_block_pass sexp, :callee_blockarg

    expected = s(:defn, :caller_blockarg,
                 s(:scope,
                   s(:block,
                     s(:args),
                     s(:lasgn, :block,
                       s(:iter,
                         s(:fcall, :proc),
                         s(:dasgn_curr, :x),
                         s(:call, s(:dvar, :x), :+, s(:array, s(:lit, 1))))),
                     s(:fcall, :callee_blockarg,
                       s(:block_arg, s(:lvar, :block))))))

    assert_equal expected, sexp
  end

  def test_replace_block_pass_nasty
    sexp = Sexp.for InlineTarget, :caller_nasty

    @processor.replace_block_pass sexp, :callee_nasty

    expected = s(:defn, :caller_nasty,
                 s(:scope,
                   s(:block,
                     s(:args),
                     s(:lasgn, :block,
                       s(:iter,
                         s(:fcall, :proc),
                         s(:dasgn_curr, :x),
                         s(:call, s(:dvar, :x), :+, s(:array, s(:lit, 1))))),
                     s(:fcall, :callee_nasty,
                       s(:argscat,
                         s(:array, s(:lit, 1), s(:lit, 2)),
                         s(:array, s(:lit, 3), s(:lit, 4))),
                       s(:block_arg, s(:lvar, :block))))))

    assert_equal expected, sexp
  end

  def test_replace_fcalls
    vars = s(:array, s(:lasgn, :inline_callee_v))
    body_exp = s(:block,
                 s(:call,
                   s(:lvar, :inline_callee_v),
                   :+,
                   s(:array, s(:lit, 5))))

    kaller_sexp = Sexp.for InlineTarget, :caller
    @processor.replace_fcalls kaller_sexp, :callee, vars, nil, s(), body_exp

    expected = s(:defn, :caller,
                 s(:scope,
                   s(:block, s(:args),
                     s(:lasgn, :v1,
                       s(:call, s(:lit, 2), :+, s(:array, s(:lit, 3)))),
                     s(:lasgn, :x,
                       s(:block,
                         s(:masgn,
                           s(:array, s(:lasgn, :inline_callee_v)),
                           s(:array, s(:lvar, :v1))),
                         nil,
                         s(:block,
                           s(:call,
                             s(:lvar, :inline_callee_v),
                             :+,
                             s(:array, s(:lit, 5)))))),
                     s(:call,
                       s(:lvar, :x),
                       :+,
                       s(:array, s(:lit, 2))))))

    assert_equal expected, kaller_sexp
  end

  def test_replace_fcalls_blockarg
    vars = s(:array)
    defaults = s()
    block_var = s(:lasgn, :inline_callee_blockarg_block)
    body_exp = s(:block,
                 s(:block_pass,
                   s(:lvar, :inline_callee_blockarg_block),
                   s(:call, s(:lit, 1), :times)))

    kaller_sexp = Sexp.for InlineTarget, :caller_blockarg

    @processor.replace_block_pass kaller_sexp, :callee_blockarg # HACK lazy

    @processor.replace_fcalls kaller_sexp, :callee_blockarg,
                              vars, block_var, defaults, body_exp

    expected = s(:defn, :caller_blockarg,
                 s(:scope,
                   s(:block,
                     s(:args),
                     s(:lasgn, :block,
                       s(:iter,
                         s(:fcall, :proc),
                         s(:dasgn_curr, :x),
                         s(:call,
                           s(:dvar, :x),
                           :+,
                           s(:array, s(:lit, 1))))),
                     s(:block,
                       nil,
                       s(:lasgn, :inline_callee_blockarg_block,
                         s(:lvar, :block)),
                       s(:block,
                         s(:block_pass,
                           s(:lvar, :inline_callee_blockarg_block),
                           s(:call, s(:lit, 1), :times)))))))

    assert_equal expected, kaller_sexp
  end

  def test_replace_fcalls_nasty
    vars = s(:array,
             s(:lasgn, :inline_callee_nasty_a),
             s(:lasgn, :inline_callee_nasty_b),
             s(:lasgn, :"*inline_callee_nasty_c"))
    defaults = s(:block, s(:lasgn, :b, s(:nil)))
    block_var = s(:lasgn, :inline_callee_nasty_block)
    body_exp = s(:block,
                 s(:block_pass,
                   s(:lvar, :inline_callee_nasty_block),
                   s(:call, s(:lit, 1), :times)))

    kaller_sexp = Sexp.for InlineTarget, :caller_nasty

    @processor.replace_block_pass kaller_sexp, :callee_nasty

    @processor.replace_fcalls kaller_sexp, :callee_nasty,
                              vars, block_var, defaults, body_exp

    expected = s(:defn, :caller_nasty,
                 s(:scope,
                   s(:block,
                     s(:args),
                     s(:lasgn, :block,
                       s(:iter,
                         s(:fcall, :proc),
                         s(:dasgn_curr, :x),
                         s(:call, s(:dvar, :x), :+, s(:array, s(:lit, 1))))),
                     s(:block,
                       s(:masgn,
                         s(:array,
                           s(:lasgn, :inline_callee_nasty_a),
                           s(:lasgn, :inline_callee_nasty_b)),
                         s(:lasgn, :inline_callee_nasty_c),
                         s(:argscat,
                           s(:array, s(:lit, 1), s(:lit, 2)),
                           s(:array, s(:lit, 3), s(:lit, 4)))),
                       s(:lasgn, :inline_callee_nasty_block, s(:lvar, :block)),
                       s(:block,
                         s(:block_pass,
                           s(:lvar, :inline_callee_nasty_block),
                           s(:call, s(:lit, 1), :times)))))))

    assert_equal expected, kaller_sexp
  end

  def test_replace_fcalls_starargs
    vars = s(:array,
             s(:lasgn, :"*inline_callee_starargs_args"))
    defaults = s()
    body_exp = s(:block,
                 s(:lvar, :inline_callee_starargs_args))

    kaller_sexp = Sexp.for InlineTarget, :caller_starargs

    @processor.replace_fcalls kaller_sexp, :callee_starargs,
                              vars, nil, defaults, body_exp

    expected = s(:defn, :caller_starargs,
                 s(:scope,
                   s(:block, s(:args),
                     s(:block,
                      s(:masgn,
                       s(:array), s(:lasgn, :inline_callee_starargs_args),
                       s(:array, s(:lit, 1), s(:lit, 2))),
                      nil,
                      s(:block, s(:lvar, :inline_callee_starargs_args))))))

    assert_equal expected, kaller_sexp
  end

  def test_replace_fcalls_varargs
    vars = s(:array,
             s(:lasgn, :inline_callee_varargs_a),
             s(:lasgn, :inline_callee_varargs_b))
    defaults = s(:block, s(:lasgn, :b, s(:lit, 2)))
    body_exp = s(:block,
                 s(:call,
                   s(:lvar, :inline_callee_varargs_a),
                   :+,
                   s(:array, s(:lvar, :inline_callee_varargs_b))))

    kaller_sexp = Sexp.for InlineTarget, :caller_varargs
    @processor.replace_fcalls kaller_sexp, :callee_varargs,
                              vars, nil, defaults, body_exp

    expected = s(:defn, :caller_varargs,
                 s(:scope,
                   s(:block, s(:args),
                     s(:block,
                      s(:masgn,
                       s(:array,
                        s(:lasgn, :inline_callee_varargs_a),
                        s(:lasgn, :inline_callee_varargs_b)),
                       s(:array, s(:lit, 1), s(:lit, 2))),
                       nil,
                       s(:block,
                         s(:call,
                           s(:lvar, :inline_callee_varargs_a),
                           :+,
                           s(:array, s(:lvar, :inline_callee_varargs_b))))))))

    assert_equal expected, kaller_sexp
  end

  def test_rewrite_defn_dasgn
    inn = s(:defn, :thing,
            s(:scope,
              s(:block, s(:args),
                s(:dasgn, :var, s(:lit, 5)))))
    out = s(:defn, :thing, s(:args),
            s(:scope,
              s(:block,
                s(:dasgn, :inline_thing_var, s(:lit, 5)))))

    assert_equal out, @processor.rewrite(inn)
  end

  def test_rewrite_defn_dasgn_curr
    inn = s(:defn, :thing,
            s(:scope,
              s(:block, s(:args),
                s(:dasgn_curr, :var))))
    out = s(:defn, :thing, s(:args),
            s(:scope,
              s(:block,
                s(:dasgn_curr, :inline_thing_var))))

    assert_equal out, @processor.rewrite(inn)
  end

  def test_rewrite_defn_dvar
    inn = s(:defn, :thing,
            s(:scope,
              s(:block, s(:args),
                s(:dvar, :var))))
    out = s(:defn, :thing, s(:args),
            s(:scope,
              s(:block,
                s(:dvar, :inline_thing_var))))

    assert_equal out, @processor.rewrite(inn)
  end

  def test_rewrite_defn_lasgn
    inn = s(:defn, :thing,
            s(:scope,
              s(:block, s(:args),
                s(:lasgn, :var, s(:lit, 5)))))
    out = s(:defn, :thing, s(:args),
            s(:scope,
              s(:block,
                s(:lasgn, :inline_thing_var, s(:lit, 5)))))

    assert_equal out, @processor.rewrite(inn)
  end

  def test_rewrite_defn_masgn_lasgn
    inn = s(:defn, :thing,
            s(:scope,
              s(:block, s(:args),
                s(:masgn,
                  s(:array,
                    s(:lasgn, :a), s(:lasgn, :b)),
                  s(:array,
                    s(:lit, 1), s(:lit, 2))))))
    out = s(:defn, :thing, s(:args),
            s(:scope,
              s(:block,
                s(:masgn,
                  s(:array,
                    s(:lasgn, :inline_thing_a), s(:lasgn, :inline_thing_b)),
                  s(:array,
                    s(:lit, 1), s(:lit, 2))))))

    assert_equal out, @processor.rewrite(inn)
  end

  def test_rewrite_defn_lvar
    inn = s(:defn, :thing,
            s(:scope,
              s(:block, s(:args),
                s(:lvar, :var))))
    out = s(:defn, :thing, s(:args),
            s(:scope,
              s(:block,
                s(:lvar, :inline_thing_var))))

    assert_equal out, @processor.rewrite(inn)
  end

end
