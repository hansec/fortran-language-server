      double precision function myfun(n,xval)
      integer i,n
c     **********
      double precision xval
      integer ieq1(2), ieq2(2)
      double precision req(2)
      character*(LEN=200) bob
      character dave*(20)
      equivalence (req(1),ieq1(1))
      equivalence (req(2),ieq2(1))
c
      data req(1) /1.0000000d-16/
      data req(2) /1.0000000d-308/
c
      myfun = xval
      bob(1:20) = dave
      do 10 i = 1, n
   10    myfun = myfun + xval
      return
c
      end
c
      subroutine glob_sub(n,xval,yval)
      integer i,n
c     **********
      double complex xval,yval
c
      yval = xval
      do 20 i = 1, n
         yval = yval + xval
   20    continue
      return
c
      end
