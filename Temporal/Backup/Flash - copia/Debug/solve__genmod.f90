        !COMPILER-GENERATED INTERFACE MODULE: Wed Oct 28 17:10:29 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SOLVE__genmod
          INTERFACE 
            SUBROUTINE SOLVE(Y,DY,NOLD,NEW,NITER,N,NT)
              REAL(KIND=8) :: Y(4)
              REAL(KIND=8) :: DY(4)
              INTEGER(KIND=4) :: NOLD
              INTEGER(KIND=4) :: NEW
              INTEGER(KIND=4) :: NITER
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: NT
            END SUBROUTINE SOLVE
          END INTERFACE 
        END MODULE SOLVE__genmod
