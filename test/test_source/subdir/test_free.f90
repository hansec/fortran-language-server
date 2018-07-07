MODULE test_free
USE, INTRINSIC :: iso_fortran_env, ONLY: error_unit
IMPLICIT NONE
!
TYPE :: scale_type
  REAL(8) :: val = 1.d0
END TYPE scale_type
!
TYPE :: vector
  INTEGER(4) :: n
  REAL(8), POINTER, DIMENSION(:) :: v => NULL()
CONTAINS
  PROCEDURE :: create => vector_create
  PROCEDURE :: norm => vector_norm
END TYPE vector
!
TYPE, EXTENDS(vector) :: scaled_vector
  TYPE(scale_type) :: scale
CONTAINS
  PROCEDURE :: set_scale => scaled_vector_set
  PROCEDURE :: norm => scaled_vector_norm
END TYPE scaled_vector
!
INTERFACE
  SUBROUTINE fort_wrap(a,b)
  INTEGER(4), INTENT(in) :: a
  REAL(8), INTENT(out) :: b
  END SUBROUTINE fort_wrap
END INTERFACE
!
LOGICAL :: module_variable
CONTAINS
!
SUBROUTINE vector_create(self, n)
CLASS(vector), INTENT(inout) :: self
INTEGER(4), INTENT(in) :: n
self%n=n
ALLOCATE(self%v(n))
self%v=0.d0
END SUBROUTINE vector_create
!
FUNCTION vector_norm(self) RESULT(norm)
CLASS(vector), INTENT(in) :: self
REAL(8) :: norm
norm = SQRT(DOT_PRODUCT(self%v,self%v))
END FUNCTION vector_norm
!
SUBROUTINE scaled_vector_set(self, scale)
CLASS(vector), INTENT(inout) :: self
REAL(8), INTENT(in) :: scale
self%scale%val = scale
END SUBROUTINE scaled_vector_set
!
FUNCTION scaled_vector_norm(self) RESULT(norm)
CLASS(scaled_vector), INTENT(in) :: self
REAL(8) :: norm
norm = self%scale%val*SQRT(DOT_PRODUCT(self%v,self%v))
END FUNCTION scaled_vector_norm
!
FUNCTION unscaled_norm(self)
CLASS(scaled_vector), INTENT(in) :: self
REAL(8) :: unscaled_norm
unscaled_norm = SQRT(DOT_PRODUCT(self%v,self%v))
END FUNCTION unscaled_norm
!
SUBROUTINE test_sig_sub(arg1,arg2,opt1,opt2,opt3)
INTEGER, INTENT(in) :: arg1,arg2
INTEGER, OPTIONAL, INTENT(in) :: opt1,opt2,opt3
END SUBROUTINE test_sig_sub
END MODULE test_free
