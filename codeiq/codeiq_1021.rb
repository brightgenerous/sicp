#! /usr/bin/ruby

#
# Ruby
#
# https://codeiq.jp/ace/ozy_ozuzu/q1021

Point = Struct.new(:x, :y)
Element = Struct.new(:points)


def is_ok? points
  queue = [Element.new(points)]
  until queue.empty?
    elem = queue.shift
    points = elem.points
    points.each { |p|
      others = points - [p]
      neighbors = others.select { |o|
        (o.x == p.x and (o.y - p.y).abs == 1) or (o.y == p.y and (o.x - p.x).abs == 1)
      }
      return true if others.size == 1 and not neighbors.empty?
      neighbors.each { |neighbor|
        queue << Element.new(others - [neighbor])
      }
    }
  end
  false
end


def points
  lines = []
  if false
    until (line = STDIN.gets).nil?
      lines << line
    end
  else
    lines << "O-O"
    lines << "O-O"
    lines << "OO-"
  end
  lines.zip(0..Float::INFINITY).flat_map { |line, y|
    line.chars.zip(0..Float::INFINITY).map { |c, x|
      Point.new(x, y) if c == "O"
    }.compact
  }
end


if is_ok? points
  print "yes"
else
  print "no"
end
print "\n"

