SUBROUTINE block_sub()
INTEGER :: res0
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
END SUBROUTINE block_sub
