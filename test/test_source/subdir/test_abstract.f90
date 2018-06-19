MODULE test_abstract
ABSTRACT INTERFACE
  SUBROUTINE abs_interface(a,b)
  INTEGER(4), INTENT(in) :: a
  REAL(8), INTENT(out) :: b
  END SUBROUTINE abs_interface
END INTERFACE
PROCEDURE(abs_interface) :: test
END MODULE test_abstract
