#! /usr/bin/ruby

# https://codeiq.jp/ace/sinohara/q1068

require 'prime'

p Prime.each(10000).lazy
  .select { |n| ('000' + n.to_s).chars.last(4).uniq.size == 4}
  .group_by { |n| n.to_s.chars.sort.join }
  .max_by { |k, v| v.size }

