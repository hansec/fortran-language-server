SUBROUTINE block_sub()
INTEGER :: res0,i,j
res0 = 0
add1 : BLOCK
  INTEGER :: res1
  res1 = res0 + 1
  BLOCK
    INTEGER :: res2,blockVar
    res2 = res1 + 1
    blockVar = res0 + 1
  END BLOCK
END BLOCK add1
!
outer: DO i=1,10
  DO j=1,i
    res0=res0+1
  END DO
END DO outer
!
IF(res0>10)THEN
  i=res0
END IF
END SUBROUTINE block_sub
