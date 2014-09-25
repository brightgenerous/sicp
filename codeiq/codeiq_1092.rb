#! /usr/bin/ruby

# https://codeiq.jp/ace/matthew/q1092


require 'prime'

print Prime.each(nil).lazy
  .each_cons(3)
  .select { |a| a.member? a.first + 6 }
  .map { |a| [a.first, a.first + 6]  }
  .take_while { |ns| ns.last < 501 }
  .to_a.map { |ns| "(#{ns[0]},#{ns[1]})" }.join(',')

