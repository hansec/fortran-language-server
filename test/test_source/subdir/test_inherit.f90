MODULE test_inherit
USE :: test_free, ONLY: scaled_vector
IMPLICIT NONE
!
TYPE, EXTENDS(scaled_vector) :: myvec
  REAL(8) :: x
END TYPE myvec
CONTAINS
SUBROUTINE inherit_completion(self)
TYPE(myvec), INTENT(INOUT) :: self
self%scale%val
END SUBROUTINE inherit_completion
END MODULE test_inherit
