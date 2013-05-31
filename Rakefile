# -*- ruby -*-

require "rubygems"
require "hoe"

%w(sexp_processor ruby_parser RubyInline ZenTest).each do |project|
  Hoe.add_include_dirs "../../#{project}/dev/lib"
end

Hoe.plugin :seattlerb

Hoe.spec "ruby2smalltalk" do
  developer "Ryan Davis", "ryand-ruby@zenspider.com"

  dependency "ruby_parser", "~> 3.0"
end

# vim: syntax=Ruby
