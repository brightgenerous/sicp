#! /usr/bin/ruby

# https://codeiq.jp/ace/ozy_ozuzu/q1094


class Array
  def subset min = 0, max = length
    (min..max).inject([]) { |ret, n|
      ret.push(*combination(n))
    }
  end
end


Point = Struct.new(:x, :y) do
  def self.make x, y
    @cache ||= {}
    points = @cache[x]
    unless points
      points = {}
      @cache[x] = points
    end
    point = points[y]
    unless point
      point = Point.new x, y
      points[y] = point
    end
    point
  end
end

Group = Struct.new(:points) do
  def corner_points point
    points.select { |p|
      (p.x - point.x).abs == 1 and
      (p.y - point.y).abs == 1
    }
  end
  def add point
    Group.new(points + [point])
  end
  def merge group, force: false
    Group.new(points + group.points)
  end
  def family? group
    group.points.any? { |point| points.member? point }
  end
  def side_points col, row
    points.select { |point|
      point.x == 0 or
      point.x == col - 1 or
      point.y == 0 or
      point.y == row - 1
    }
  end
end


#
# type make_matrixes : [matrix]
#   type matrix : [rows, groups]
#   type rows   : [row]
#   type row    : [Int]
#   type groups : [Group]
def make_matrixes col, row

  if row == 1 or col == 1
    return [ # matrixes
        [ # matrix
          Array.new(row, []), # rows
          [] # groups
        ]
      ]
  end

  #
  # type make_row_patterns : [[Int]]
  #   when col = 3
  #     => [[], [0], [1], [2], [0, 2]]
  make_row_patterns = -> {
    patterns = (0...col).to_a.subset(0, (col / 2.0).ceil)
      .select { |nums|
        nums.each_cons(2).none? { |ns| ns[0] + 1 == ns[1] }
      }
  }

  #
  # type make_row_patterns_hash : { [Int] => [Int] }
  #   when col = 3
  #     => {
  #          []     => [[], [0], [1], [2], [0, 2]],
  #          [0]    => [[], [1], [2]],
  #          [1]    => [[], [0], [2], [0, 2]],
  #          [2]    => [[], [0], [1]],
  #          [0, 2] => [[], [1]]
  #        }
  #
  make_row_patterns_hash = -> {
    ps = make_row_patterns.call
    ps.each_with_object({}) { |nums, h|
      h[nums] = ps.select { |ns|
        ns.size + nums.size < col and
        ns - nums == ns
      }
    }
  }

  #
  # type patterns : { [Int] => [Int] }
  #
  patterns = make_row_patterns_hash.call

  #
  # type matrixes : [matrix]
  #   type matrix : [rows, groups]
  #   type rows   : [row]
  #   type row    : [Int]
  #   type groups : [Group]
  #
  matrixes = patterns.keys.map { |row|
    [[row], row.map { |x| Group.new [Point.make(x, 0)] }]
  }

  #
  # loop : apply flat_map
  (1...row).each { |y|
    matrixes = matrixes.flat_map { |matrix|
      rows = matrix.first
      groups = matrix[1]
      patterns[rows.last].map { |pattern|
        #
        # merge pattern with groups
        #
        #   what a pain logic!!
        #
        new_groups = []
        catch(:pattern) do
          old_groups = groups.clone
          pattern.lazy.map { |x| Point.make x, y }
            .each { |point|

              # new_groups.map.size === 0, 1
              ngs = new_groups.lazy.map { |group|
                [group, group.corner_points(point)]
              }.select { |group_corners|
                group_corners[1].size > 0
              }.map { |g_c|

                # if group has (two) corner points,
                #   when the group surround.
                if g_c[1].size > 1
                  new_groups = nil
                  throw :pattern
                end

                g_c[0]
              }.to_a
              new_groups = new_groups - ngs

              # old_groups.map.size === 0, 1, 2
              ogs = old_groups.lazy.map { |group|
                [group, group.corner_points(point)]
              }.select { |group_corners|
                group_corners[1].size > 0
              }.map { |g_c|

                # if group has (two) corner points,
                #   when the group surround.
                if g_c[1].size > 1
                  new_groups = nil
                  throw :pattern
                end

                g_c[0]
              }.to_a
              old_groups = old_groups - ogs

              # new_group
              new_group = if ngs.empty? and ogs.empty?
                  Group.new [point]
                else
                  n_g = (ngs + ogs)
                    .inject { |g_s, g| g_s.merge g }
                    .add(point)

                  # if group has (two) side points,
                  #   when the group surround.
                  if n_g.side_points(col, row).size > 1
                    new_groups = nil
                    throw :pattern
                  end

                  n_g
                end
              new_groups << new_group
            }
        end # end of catch(:pattern)

        if new_groups
          [rows + [pattern], new_groups]
        else
          nil
        end
      }.compact
    }
  }
  matrixes
end


dump = ->(matrixes) {
  print "dump do #--\n"
  matrixes.each { |matrix|
    rows = matrix.first
    groups = matrix[1]
    print "#{rows}\n"
  }
  print "end #dump --\n\n"
}


COL = 6
ROW = 5

start_time = Time.now
matrixes = make_matrixes COL, ROW
elapse = Time.now - start_time

#dump.call matrixes

# size   => 149283
# elapse => 43.68081 seconds.
print "size   => #{matrixes.size}\n"
print "elapse => #{elapse} seconds.\n"

