        !COMPILER-GENERATED INTERFACE MODULE: Wed Oct 28 17:10:29 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LUBKSB__genmod
          INTERFACE 
            SUBROUTINE LUBKSB(A,N,NP,INDX,B)
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(NP,NP)
              INTEGER(KIND=4) :: INDX(N)
              REAL(KIND=8) :: B(N)
            END SUBROUTINE LUBKSB
          END INTERFACE 
        END MODULE LUBKSB__genmod
