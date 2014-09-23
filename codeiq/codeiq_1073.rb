#! /usr/bin/ruby

row = 2
col = 2
x_pow = 10 ** row.to_s.size

      # block => 1: o , 2: \ , 3: / , 4: x
count = (1..4).to_a.repeated_permutation(row * col)
  .count { |blocks|
    blocks.each.with_index.flat_map { |block, i|
      x = i % col
      y = i / col
      points = [
        (x + 1) * x_pow + y, (x + 1) * x_pow + (y + 1),
        x * x_pow + (y + 1), (x + 1) * x_pow + (y + 1)
      ]
      if x == 0
        points.push x * x_pow + y, (x + 1) * x_pow + y
      end
      if y == 0
        points.push x * x_pow + y, x * x_pow + (y + 1)
      end
      if [2, 4].include? block
        points.push x * x_pow + y, (x + 1) * x_pow + y + 1
      end
      if [3, 4].include? block
        points.push (x + 1) * x_pow + y, x * x_pow + y + 1
      end
      points
    }.group_by { |p| p }
      .select { |p, ps| ps.size.odd? }
      .size < 3
  }

p count

