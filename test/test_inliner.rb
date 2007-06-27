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

    def caller_varargs
      callee_varargs 1
    end

    def callee_varargs(a, b = 2)
      a + b
    end

    def caller_starargs
      callee_starargs 1, 2
    end

    def callee_starargs(*args)
      args
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
    args, defaults, body_expr =
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

  def test_extract_expression_varargs
    args, defaults, body_expr =
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

  def test_extract_expression_starargs
    args, defaults, body_expr =
      @processor.extract_expression(InlineTarget, :callee_starargs)

    expected_args = s(:array,
                      s(:lasgn, :"*inline_callee_starargs_args"))
    assert_equal expected_args, args

    assert_equal s(), defaults

    expected = s(:block,
                 s(:lvar, :inline_callee_starargs_args))

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

  def test_replace_fcalls
    vars = s(:array, s(:lasgn, :inline_callee_v))
    body_exp = s(:block,
                 s(:call,
                   s(:lvar, :inline_callee_v),
                   :+,
                   s(:array, s(:lit, 5))))

    inlined_sexp = @processor.replace_fcalls(InlineTarget, :caller, :callee,
                                             vars, s(), body_exp)

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
                         s(:block,
                           s(:call,
                             s(:lvar, :inline_callee_v),
                             :+,
                             s(:array, s(:lit, 5)))))),
                     s(:call,
                       s(:lvar, :x),
                       :+,
                       s(:array, s(:lit, 2))))))

    assert_equal expected, inlined_sexp
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

    inlined_sexp = @processor.replace_fcalls(InlineTarget, :caller_varargs,
                                             :callee_varargs,
                                             vars, defaults, body_exp)

    expected = s(:defn, :caller_varargs,
                 s(:scope,
                   s(:block, s(:args),
                     s(:block,
                      s(:masgn,
                       s(:array,
                        s(:lasgn, :inline_callee_varargs_a),
                        s(:lasgn, :inline_callee_varargs_b)),
                       s(:array, s(:lit, 1), s(:lit, 2))),
                       s(:block,
                         s(:call,
                           s(:lvar, :inline_callee_varargs_a),
                           :+,
                           s(:array, s(:lvar, :inline_callee_varargs_b))))))))

    assert_equal expected, inlined_sexp
  end

  def test_replace_fcalls_starargs
    vars = s(:array,
             s(:lasgn, :"*inline_callee_starargs_args"))
    defaults = s()
    body_exp = s(:block,
                 s(:lvar, :inline_callee_starargs_args))

    inlined_sexp = @processor.replace_fcalls(InlineTarget, :caller_starargs,
                                             :callee_starargs,
                                             vars, defaults, body_exp)

    expected = s(:defn, :caller_starargs,
                 s(:scope,
                   s(:block, s(:args),
                     s(:block,
                      s(:masgn,
                       s(:array, s(:lasgn, :"*inline_callee_starargs_args")),
                       s(:array, s(:lit, 1), s(:lit, 2))),
                      s(:block, s(:lvar, :inline_callee_starargs_args))))))

    assert_equal expected, inlined_sexp
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
