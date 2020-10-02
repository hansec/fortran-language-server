MODULE test_generic
TYPE :: test_gen_type
CONTAINS
  GENERIC :: my_gen => gen1,gen2
  GENERIC :: ASSIGNMENT(=) => assign1, assign2
  GENERIC :: OPERATOR(+) => plusop1, plusop2
  GENERIC, PRIVATE :: my_gen2 => gen3, gen4
END TYPE test_gen_type
CONTAINS
!
SUBROUTINE gen1(self,a,b)
CLASS(test_gen_type) :: self
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: b
CALL self%
END SUBROUTINE gen1
!
SUBROUTINE gen2(self,a,b,c)
CLASS(test_gen_type) :: self
REAL(8), INTENT(IN) :: a,c
REAL(8), INTENT(OUT) :: b
END SUBROUTINE gen2
!
SUBROUTINE assign1(outvar,invar)
REAL(8) :: outvar
CLASS(test_gen_type) :: invar
END SUBROUTINE assign1
!
SUBROUTINE assign2(outvar,invar)
LOGICAL :: outvar
CLASS(test_gen_type) :: invar
END SUBROUTINE assign2
!
REAL(8) FUNCTION plusop1(var1,var2)
REAL(8) :: var1
CLASS(test_gen_type) :: var2
END FUNCTION plusop1
!
LOGICAL FUNCTION plusop2(var1,var2)
LOGICAL :: var1
CLASS(test_gen_type) :: var2
END FUNCTION plusop2
!
SUBROUTINE gen3(self,a,b)
CLASS(test_gen_type) :: self
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: b
CALL self%
END SUBROUTINE gen3
!
SUBROUTINE gen4(self,a,b,c)
CLASS(test_gen_type) :: self
REAL(8), INTENT(IN) :: a,c
REAL(8), INTENT(OUT) :: b
END SUBROUTINE gen4
END MODULE test_generic
