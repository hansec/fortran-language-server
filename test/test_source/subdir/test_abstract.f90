MODULE test_abstract
ABSTRACT INTERFACE
  SUBROUTINE abs_interface(a,b)
  INTEGER(4), DIMENSION(3,6), INTENT(in) :: a
  REAL(8), INTENT(out) :: b(4)
  END SUBROUTINE abs_interface
END INTERFACE
PROCEDURE(abs_interface) :: test
END MODULE test_abstract
