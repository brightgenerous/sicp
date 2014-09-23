#! /usr/bin/ruby

# https://codeiq.jp/ace/code_teller/q1093

Point = Struct.new(:x, :y)
Element = Struct.new(:points, :denies)


class Array
  def subset min = 0, max = length
    (min..max).inject([]) { |ret, n|
      ret.push(*combination(n))
    }
  end
end


def first_datas width, height
  denies = []
  (0...height).flat_map { |h|
    (0...width).map { |w|
      points = [Point.new(w, h)]
      data = Element.new points, denies
      denies = denies + points
      data
    }
  }
end


def allow_points width, height, data
  (0...height).flat_map { |h|
    (0...width).map { |w|
      point = Point.new w, h
      if data.points.include? point or data.denies.include? point
        nil
      else
        arounds = [[w, h - 1], [w - 1, h], [w + 1, h], [w, h + 1]]
          .select { |w_h| 0 <= w_h[0] and w_h[0] < width and 0 <= w_h[1] and w_h[1] < height }
          .map { |w_h| Point.new *w_h }
        if arounds.any? { |a| data.points.include? a }
          point
        else
          nil
        end
      end
    }.reject { |p| p.nil? }
  }
end


def datas width, height, count
  queue = first_datas width, height
  finishes = queue.select { |data| data.points.size == count }
  until queue.empty?
    data = queue.shift
    allow_ps = allow_points width, height, data
    allow_pss = allow_ps.subset 1, count - data.points.size
    fill_pss, not_fill_pss = *allow_pss
      .partition { |ps| data.points.size + ps.size == count }
    fill_pss.each { |ps|
      finishes << Element.new(data.points + ps, data.denies + (allow_ps - ps))
    }
    not_fill_pss.each { |ps|
      queue << Element.new(data.points + ps, data.denies + (allow_ps - ps))
    }
  end
  finishes
end


samples = [
  # [2, 3, 2], [2, 3, 3],
  [5, 5, 2], [3, 4, 3], [3, 4, 4], [6, 7, 5]
]

samples.each do |sample|
  width, height, count = *sample

  # print "(width, height, count) = (#{width}, #{height}, #{count})\n"
  ds = datas(width, height, count)

  if false
    ds.each { |e|
     p e.points
    }
  end

  print "#{ds.size}\n"
end

