#! /usr/bin/ruby

# https://codeiq.jp/ace/joboffer_apli/q1075

def data
  [
    [  0,  7, 12,  8, 11,  7],
    [  3,  0, 10,  7, 13,  2],
    [  4,  8,  0,  9, 12,  3],
    [  6,  6,  9,  0, 10,  7],
    [  7,  7, 11, 10,  0,  5],
    [  9,  7,  8,  9, 10,  0]
  ]
end


_data = data
row_with_indexes = _data.zip(0..Float::INFINITY)
elems = [[[0], 0, "0"]]
(row_with_indexes.size - 1).times do
  elems = elems.flat_map do |elem|
    last = elem[0].last
    indexes = (0..._data[last].size).to_a - elem[0]
    indexes.map do |index|
      value = _data[last][index]
      [elem[0] + [index], elem[1] + value, elem[2] + "+#{value}"]
    end
  end
end
short = elems.min_by { |elem| elem[1] }


# to_house_index
short[0] = short[0].map { |index| index + 1 }
p "Routes = #{short[0]}"
p "Cost   = #{short[1]}"
p "Costs  = #{short[2]}"

