#! /usr/bin/ruby

# https://codeiq.jp/ace/thisweek_masuipeo/q1062

def comb total
  (0..((total / 2.to_f + 0.5))).inject(0) { |r, idx|
    s = total - idx + 1
    r + ((s - idx + 1)..s).inject(1, &:*) / (1..idx).inject(1, &:*)
  }
end

(1..30).each { |i|
  p "#{i} => #{comb i}"
}

