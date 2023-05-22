        !COMPILER-GENERATED INTERFACE MODULE: Wed Oct 28 17:10:29 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GCON__genmod
          INTERFACE 
            SUBROUTINE GCON(NK,X,ACT,DACT,ICVEX)
              INTEGER(KIND=4) :: NK
              REAL(KIND=8) :: X(3)
              REAL(KIND=8) :: ACT(3)
              REAL(KIND=8) :: DACT(10,10)
              INTEGER(KIND=4) :: ICVEX
            END SUBROUTINE GCON
          END INTERFACE 
        END MODULE GCON__genmod
