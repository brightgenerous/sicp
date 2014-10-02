#! /usr/bin/ruby

#
# Ruby
#
# https://codeiq.jp/ace/thisweek_masuipeo/q1099


def split_merge? n, t = 2 * (n - 1)
  ary = tmp = (1..(n * 2)).to_a
  t.times do
    tmp = tmp.each_slice(n).to_a.transpose.flatten
  end
  tmp == ary
end


p (1..100).count{ |i| split_merge? i }

