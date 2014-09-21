#! /usr/bin/ruby

# https://codeiq.jp/ace/joboffer_apli/q1081

datas = %w[
  steal
  stela
  telas
  teals
  elast
  least
  laste
  astel
]


matrixes = datas.first.chars.permutation.map { |heads|
  [heads, []]
}


size = datas.first.size
(0...size).each { |idx|
  matrixes = matrixes.flat_map { |matrix|
    heads = matrix[0]
    head = heads[idx]
    words = matrix[1]
    (datas - words).select { |word| word.start_with? head }
      .map { |word| [heads, words + [word]] }
      .select { |matrix|
        matrix[1].map { |word| word.chars }.transpose.all? { |chars|
          datas.any? { |word| word.start_with? chars.join }
        }
      }
  }
}


# ["s", "t", "e", "l", "a"]
# ["stela", "telas", "elast", "laste", "astel"]
# ["t", "e", "l", "a", "s"]
# ["telas", "elast", "laste", "astel", "stela"]
# ["e", "l", "a", "s", "t"]
# ["elast", "laste", "astel", "stela", "telas"]
# ["a", "s", "t", "e", "l"]
# ["astel", "stela", "telas", "elast", "laste"]
# ["l", "a", "s", "t", "e"]
# ["laste", "astel", "stela", "telas", "elast"]
matrixes.each { |matrix|
  p matrix[0]
  p matrix[1]
}

