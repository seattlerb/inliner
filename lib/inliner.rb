require 'rubygems'
require 'ruby2ruby'

class Inliner < RubyToRuby

  ##
  # The version of Inliner you are using.

  VERSION = '1.0.0'

  attr_accessor :defn_name

  @@signature = :fuck
  @@threshold = 500
  @@sacred = {
    Sexp => true,
  }
  @@skip = Hash.new(false)
  # { klass => { [kaller, kallee] => count } }
  @@data = Hash.new { |h,klass| h[klass] = Hash.new(0) }

  def self.data
    @@data
  end

  def self.inline_into(signature)
    klass, kaller, kallee = *signature

    unless @@sacred.include? klass then
      $stderr.puts "*** Inliner threshold tripped for #{klass} #{kallee} -> #{kaller}"

      begin
        new.inline_into klass, kaller, kallee
      rescue Exception => e
        $stderr.puts "Failed to inline #{klass} #{kallee} -> #{kaller}"
        $stderr.puts "Exception = #{e.class}, message = #{e.message}"
      end
    end

    @@skip[signature] = true
  end

  def self.signature
    @@signature
  end

  def self.start_inlining
    add_event_hook
  end

  def self.stop_inlining
    remove_event_hook
    if $DEBUG then
      $stderr.puts @@skip.inspect
      $stderr.puts @@data.inspect
    end
  end

  def initialize
    super
    @renames = {}
    @defn_name = nil
  end

  def extract_expression(klass, kallee)
    kallee_sexp = Sexp.for klass, kallee

    kallee_args = nil
    kallee_sexp.each_of_type(:args) { |a| kallee_args = a }

    defaults = Sexp === kallee_args.last ? kallee_args.pop : s()

    @defn_name = kallee
    args = kallee_args.sexp_body.map { |arg| s(:lasgn, rename(arg)) }

    if args.last.last.to_s =~ /\*/ then
      star_arg = args.last.last
      star_arg = "*#{star_arg.to_s.sub '*', ''}"
      args.last[-1] = star_arg.intern
    end

    args = s(:array, *args)
    @defn_name = nil

    body_defn = rewrite kallee_sexp
    body_expr = body_defn.scope.block

    return args, defaults, body_expr
  end

  def inline_into(klass, kaller, kallee)
    args, defaults, inlined_body = extract_expression klass, kallee

    inlined_sexp = replace_fcalls klass, kaller, kallee,
                                  args, defaults, inlined_body

    kaller_ruby = RubyToRuby.new.process inlined_sexp

    klass.class_eval kaller_ruby
  end

  def replace_fcalls(klass, kaller, kallee, vars, defaults, inlined_body)
    kaller_sexp = Sexp.for klass, kaller

    kaller_sexp.each_of_type(:fcall) do |fcall|
      next unless fcall[1] == kallee

      fcall_args = fcall[2]

      if vars.length > fcall_args.length then
        defaults = defaults.dup

        until vars.length == fcall_args.length
          fcall_args << defaults.pop.last
        end
      end

      callee_vars = vars.length - 1

      fcall_args = fcall[2]

      fcall_vars = fcall_args.length - 1

      var_masgn = s(:masgn, vars, fcall[2])

      inlined = s(:block, var_masgn, inlined_body)

      fcall.replace inlined
    end

    kaller_sexp
  end

  def rewrite_defn(exp)
    old_defn_name = @defn_name
    @defn_name = exp[1]
    exp = super

    body = exp.last

    body.each_of_type(:dasgn) { |e| e.replace rewrite_dasgn(e) }
    body.each_of_type(:dasgn_curr) { |e| e.replace rewrite_dasgn_curr(e) }
    body.each_of_type(:dvar) { |e| e.replace rewrite_dvar(e) }
    body.each_of_type(:lasgn) { |e| e.replace rewrite_lasgn(e) }
    body.each_of_type(:lvar) { |e| e.replace rewrite_lvar(e) }

    exp
  ensure
    @defn_name = old_defn_name
  end

  def rewrite_dasgn(exp)
    exp.shift # :dasgn
    name = rename exp.shift
    val = exp.shift

    return s(:dasgn, name, val)
  end

  def rewrite_dasgn_curr(exp)
    exp.shift # :dasgn_curr
    name = rename exp.shift

    return s(:dasgn_curr, name)
  end

  def rewrite_dvar(exp)
    exp.shift # :dvar
    name = rename exp.shift

    return s(:dvar, name)
  end

  def rewrite_lasgn(exp)
    exp.shift # :lasgn
    name = rename exp.shift
    val = exp.shift

    return s(:lasgn, name, val)
  end

  def rewrite_lvar(exp)
    exp.shift # :lvar
    name = rename exp.shift

    return s(:lvar, name)
  end

  def rename(var)
    @renames[var] ||= "inline_#{@defn_name}_#{var}".intern
  end

  ############################################################
  # Inlined Methods:

  inline(:C) do |builder|

    builder.add_type_converter("rb_event_t", '', '')
    builder.add_type_converter("ID", '', '')

    builder.include '"ruby.h"'
    builder.include '"node.h"'
    builder.include '"env.h"'

    builder.prefix "static VALUE inliner_klass = Qnil;
static VALUE data = Qnil;
static VALUE skip = Qnil;
static unsigned long threshold = 0;"

    builder.c_raw_singleton <<-'EOF'
    static void
    inline_event_hook(rb_event_t event, NODE *node,
                    VALUE self, ID mid, VALUE klass) {

      static int inlining = 0;

      if (NIL_P(inliner_klass))
        inliner_klass = rb_path2class("Inliner");
      if (NIL_P(data))
        data = rb_cv_get(inliner_klass, "@@data");
      if (NIL_P(skip))
        skip = rb_cv_get(inliner_klass, "@@skip");
      if (threshold == 0)
        threshold = NUM2ULONG(rb_cv_get(inliner_klass, "@@threshold"));
      if (inlining) return;
      inlining++;

      switch (event) {
      case RUBY_EVENT_CALL:
        {
          VALUE signature, kall_pair, klasses_counts;
          struct FRAME *frame = ruby_frame->prev;

          if (frame->last_func == ID_ALLOCATOR)
            frame = frame->prev;

          if (frame->self != self)
            goto finish;

          if (!frame->last_func)
            goto finish;

          kall_pair = rb_ary_new2(2);
          rb_ary_store(kall_pair, 0, ID2SYM(frame->last_func));
          rb_ary_store(kall_pair, 1, ID2SYM(mid));

          signature = rb_ary_new2(3);
          rb_ary_store(signature, 0, klass);
          rb_ary_store(signature, 1, ID2SYM(frame->last_func));
          rb_ary_store(signature, 2, ID2SYM(mid));

          rb_cv_set(inliner_klass, "@@signature", signature);
          klasses_counts = rb_hash_aref(data, klass);
          unsigned long count = NUM2ULONG(rb_hash_aref(klasses_counts, kall_pair)) + 1;

          if (count > threshold) {
            if (! RTEST(rb_hash_aref(skip, signature))) {
              rb_funcall(inliner_klass, rb_intern("inline_into"), 1, signature);
            }
          }

          rb_hash_aset(klasses_counts, kall_pair, ULONG2NUM(count));
        }
        break;
      }
      finish:
      inlining--;
    }
    EOF

    builder.c_singleton <<-'EOF'
      void add_event_hook() {
        rb_add_event_hook(inline_event_hook, RUBY_EVENT_CALL);
      }
    EOF

    builder.c_singleton <<-'EOF'
      void remove_event_hook() {
        rb_remove_event_hook(inline_event_hook);
      }
    EOF
  end
end
