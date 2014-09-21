#! /usr/bin/ruby

# https://codeiq.jp/ace/code_teller/q1076

def to_index idx
  i, s = lower idx
  _idx = idx - s
  c = _idx / i
  a = _idx % i
  val = ("9" * (i - 1)).to_i + c + 1
  val.to_s.chars[a]
end


def lower idx
  size_upper = 1
  (1..Float::INFINITY).each { |i|
    _size_upper = size_upper + 9 * 10 ** (i - 1) * i
    if idx <= _size_upper
      return i, size_upper
    end
    size_upper = _size_upper
  }
end


data = %w[
186
2886
38886
579334
62346205
787925698
1234567890
3333333333
].map { |s| s.to_i }.each { |i|
  p to_index i + 4
}

