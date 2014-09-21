#! /usr/bin/ruby


def hit_blow ans, prd
  ans_chars = ans.to_s.chars
  prd_chars = prd.to_s.chars
  ans_counts = Hash[ans_chars.group_by { |k| k }.map { |k, v| [k, v.size] }]
  prd_counts = Hash[prd_chars.group_by { |k| k }.map { |k, v| [k, v.size] }]
  blow = ans_counts.map { |k, v|
    if prd_counts.include? k
      [v, prd_counts[k]].min
    else
      0
    end
  }.inject(0) { |r, v| r + v }
  [[
    ans_chars.zip(prd_chars).count { |cs| cs[0] == cs[1] },
    blow
  ]].map { |hb| hb[1] = hb[1] - hb[0]; hb }.first
end


data = %w[
2527 0794
0001 3220
2664 7963
5910 4794
4183 2695
5773 3297
2417 2174
1529 6475
7797 5917
2407 1023
6860 2460
8289 6285
5670 8997
7800 8700
6543 3456
3510 1099
1529 0529
4201 4201
9144 8994
5528 6918
]

(0...(data.size / 2)).each do |idx|
  ans = data[idx * 2]
  prd = data[idx * 2 + 1]
  ap = hit_blow ans, prd
  #p "ans = #{ans}, prd = #{prd}"
  p "#{ap[0]}H#{ap[1]}B"
end

