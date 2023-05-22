        !COMPILER-GENERATED INTERFACE MODULE: Wed Oct 28 17:10:29 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE STABIL__genmod
          INTERFACE 
            SUBROUTINE STABIL(N,NDIF,FUN,GRAD,XMAT,Y)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: NDIF
              REAL(KIND=8) :: FUN
              REAL(KIND=8) :: GRAD(30)
              REAL(KIND=8) :: XMAT(30,30)
              REAL(KIND=8) :: Y(30)
            END SUBROUTINE STABIL
          END INTERFACE 
        END MODULE STABIL__genmod
