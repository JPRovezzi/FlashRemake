        !COMPILER-GENERATED INTERFACE MODULE: Wed Oct 28 17:10:29 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GMIX__genmod
          INTERFACE 
            SUBROUTINE GMIX(NARG,NDIF,FUN,GRAD,XMAT,YVAL)
              INTEGER(KIND=4) :: NARG
              INTEGER(KIND=4) :: NDIF
              REAL(KIND=8) :: FUN
              REAL(KIND=8) :: GRAD(30)
              REAL(KIND=8) :: XMAT(30,30)
              REAL(KIND=8) :: YVAL(30)
            END SUBROUTINE GMIX
          END INTERFACE 
        END MODULE GMIX__genmod
