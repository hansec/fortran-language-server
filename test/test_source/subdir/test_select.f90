MODULE test_select
IMPLICIT NONE
!
TYPE :: parent
  INTEGER(4) :: n
END TYPE parent
!
TYPE, EXTENDS(parent) :: child1
  REAL(8) :: a
END TYPE child1
!
TYPE, EXTENDS(parent) :: child2
  COMPLEX(8) :: a
END TYPE child2
CONTAINS
!
SUBROUTINE test_select_sub(self)
CLASS(parent), INTENT(inout) :: self
! Select statement with binding
SELECT TYPE(this=>self)
TYPE IS(child1)
  this%a
CLASS IS(child2)
  this%a
CLASS DEFAULT
  this%n
END SELECT
! Select statement without binding
SELECT TYPE(self)
TYPE IS(child1)
  self%a
END SELECT
END SUBROUTINE test_select_sub
END MODULE test_select
