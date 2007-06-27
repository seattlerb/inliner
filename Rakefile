# -*- ruby -*-

require 'rubygems'
require 'hoe'
require './lib/inliner.rb'

Hoe.new('Inliner', Inliner::VERSION) do |p|
  p.rubyforge_name = 'Inliner'
  p.author = 'Eric Hodel'
  p.email = 'drbrain@segment7.net'
  # p.summary = 'FIX'
  # p.description = p.paragraphs_of('README.txt', 2..5).join("\n\n")
  # p.url = p.paragraphs_of('README.txt', 0).first.split(/\n/)[1..-1]
  p.changes = p.paragraphs_of('History.txt', 0..1).join("\n\n")

  p.extra_deps << 'Ruby2Ruby'
  p.extra_deps << 'RubyInline'
end

# vim: syntax=Ruby
