#! /usr/bin/ruby

# https://codeiq.jp/ace/joboffer_apli/q1078

class Primes
  include Enumerable

  def initialize max
    @max = max
  end

  def each
    nums = Array.new @max, true
    (2..@max).each do |val|
      if nums[val]
        yield val
        (2..(@max / val)).each do |idx|
          nums[val * idx] = false
        end
      end
    end
  end
end


datas = %w[
  2
  5
  10
  19
  54
  224
  312
  616
  888
  977
].map(&:to_i)
datas.each do |i|
  #p "#{i} : #{Primes.new(i).to_a.size}"
  p Primes.new(i).to_a.size
end

