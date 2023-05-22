        !COMPILER-GENERATED INTERFACE MODULE: Wed Oct 28 17:10:29 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MARQ__genmod
          INTERFACE 
            SUBROUTINE MARQ(FUNC,N,M,X,XLAMB,FAC,EPSG,MAXF)
              EXTERNAL FUNC
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: X(2)
              REAL(KIND=8) :: XLAMB
              REAL(KIND=8) :: FAC
              REAL(KIND=8) :: EPSG
              INTEGER(KIND=4) :: MAXF
            END SUBROUTINE MARQ
          END INTERFACE 
        END MODULE MARQ__genmod
