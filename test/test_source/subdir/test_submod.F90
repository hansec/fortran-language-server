module points
  type :: point
     real :: x, y
  end type point

  interface
     module function point_dist(a, b) result(distance)
       type(point), intent(in) :: a, b
       real :: distance
     end function point_dist

     module logical function is_point_equal_a(a, b)
      type(point), intent(in) :: a, b
    end function is_point_equal_a
  end interface
contains
  logical function is_point_equal(a, b)
    type(point), intent(in) :: a, b
    is_point_equal = merge(.true., .false., a%x == b%x .and. a%y == b%y)
  end function is_point_equal
end module points
#define __PARENT_MOD__ points
submodule (__PARENT_MOD__) points_a
contains
  module function point_dist
    type(point) :: c
    distance = sqrt((a%x - b%x)**2 + (a%y - b%y)**2)
  end function point_dist

  module procedure is_point_equal_a
    type(point) :: c
    is_point_equal = merge(.true., .false., a%x == b%x .and. a%y == b%y)
  end procedure is_point_equal_a
end submodule points_a
