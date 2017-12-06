PROGRAM test_program
!
USE test_free, ONLY: vector, scaled_vector, module_variable
IMPLICIT NONE
!
CHARACTER(LEN=*) :: test_str1 = "i2.2,':',i2.2", test_str2 = 'i2.2,":",i2.2'
INTEGER(4) :: n
REAL(8) :: x,y
COMPLEX(8) :: xc,yc
TYPE(vector) :: loc_vector
TYPE(scaled_vector) :: stretch_vector
!
y = myfun(n,x)
CALL glob_sub(n,xc,yc)
!
CALL loc_vector%create(n)
x = loc_vector%norm()
!
CALL stretch_vector%create(n)
CALL stretch_vector%set_scale(loc_vector%norm(self))
x = stretch_vector%norm()
y = stretch_vector%scale%val
END PROGRAM test_program
