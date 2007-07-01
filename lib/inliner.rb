require 'rubygems'
require 'ruby2ruby'

class Inliner < RubyToRuby

  class Error < RuntimeError; end

  ##
  # The version of Inliner you are using.

  VERSION = '1.0.0'

  attr_accessor :defn_name

  @@signature = :bogus
  @@threshold = 500
  @@sacred = {
    Sexp => true,
  }
  @@skip = Hash.new(false)
  # { klass => { [kaller, kallee] => count } }
  @@data = Hash.new { |h,klass| h[klass] = Hash.new(0) }
  @@debug = false

  def self.data
    @@data
  end

  def self.debug() @@debug || $DEBUG end
  def self.debug=(v) @@debug = v end

  def self.inline_during(wait, duration)
    Thread.start do
      sleep wait
      start_inlining
      sleep duration
      stop_inlining
    end
  end

  def self.inline_into(signature)
    return if @@skip.include? signature

    klass, kaller, kallee = *signature

    return if @@sacred.include? klass

    log "Inliner threshold tripped for #{klass} #{kallee} -> #{kaller}"

    begin
      new.inline_into klass, kaller, kallee
    rescue Exception => e
      log "Failed to inline #{klass} #{kallee} -> #{kaller}"
      log "Exception = #{e.class}, message = #{e.message}"
    end

    @@skip[signature] = true
  end

  def self.log(message)
    $stderr.puts message if debug
  end

  def self.sacred
    @@sacred
  end

  def self.signature
    @@signature
  end

  def self.start_inlining
    log "starting inlining"
    add_event_hook
  end

  def self.stop_inlining
    remove_event_hook
    log "stopped inlining"
    log @@skip.inspect
    log @@data.inspect
  end

  def initialize
    super
    @renames = {}
    @defn_name = nil
  end

  def extract_expression(klass, kallee)
    kallee_sexp = Sexp.for klass, kallee

    # HACK Sexp#=~ sucks
    kallee_sexp.each_of_type(:yield) { raise Error, "yield gets slower" }
    kallee_sexp.each_of_type(:return) { raise Error, "return unsupported" }

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

    block_arg = nil
    kallee_sexp.each_of_type(:block_arg) do |ba| 
      block_arg = s(:lasgn, rename(ba.last))
    end

    args = s(:array, *args)
    @defn_name = nil

    body_defn = rewrite kallee_sexp
    body_expr = body_defn.scope.block

    return args, block_arg, defaults, body_expr
  end

  def inline_into(klass, kaller, kallee)
    vars, block_var, defaults, inlined_body = extract_expression klass, kallee

    kaller_sexp = Sexp.for klass, kaller

    replace_block_pass kaller_sexp, kallee

    replace_fcalls kaller_sexp, kallee, vars, block_var, defaults, inlined_body

    kaller_ruby = RubyToRuby.new.process kaller_sexp

    klass.class_eval kaller_ruby

  rescue SyntaxError, Inliner::Error => e
    self.class.log "Error inlining #{klass} #{kallee} into #{kaller}:\n\t"
    self.class.log e.message
    self.class.log "###\n#{kaller_ruby}\n###" if SyntaxError === e
  end

  def replace_block_pass(kaller_sexp, kallee)
    kaller_sexp.each_of_type(:block_pass) do |block_pass|
      fcall = block_pass.fcall
      next unless fcall[1] == kallee

      block_var = s(:block_arg, block_pass[1])

      fcall << block_var

      block_pass.replace fcall
    end
  end

  def replace_fcalls(kaller_sexp, kallee,
                     vars, block_var, defaults, inlined_body)
    kaller_sexp.each_of_type(:fcall) do |fcall|
      next unless fcall[1] == kallee

      fcall_args = fcall[2] || []

      case fcall_args.first 
      when :array then
        if vars.length > fcall_args.length then
          defaults = defaults.dup

          fcall_args << defaults.pop.last until vars.length == fcall_args.length
        end
      when :argscat, :block_arg then
        # ignore
      else
        raise Error, "unknown fcall argument node #{fcall[2].first}"
      end

      block_arg = fcall.block_arg(true)
      block_arg = block_arg.last if block_arg

      var_masgn = nil
      if vars.length > 1 then
        var_masgn = s(:masgn)

        if vars.length == 1 then
          var_masgn << vars
        else
          star_var = vars.pop if vars.last.to_s =~ /\*/
          var_masgn << vars
          if star_var then
            star_var[-1] = star_var.last.to_s.sub('*', '').intern
            var_masgn << star_var
          end
        end

        var_masgn << fcall_args
      end

      block_asgn = block_var << block_arg if block_var and block_arg

      inlined = s(:block, var_masgn, block_asgn, inlined_body)

      fcall.replace inlined
    end
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

          if (T_ICLASS == rb_type(klass))
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
