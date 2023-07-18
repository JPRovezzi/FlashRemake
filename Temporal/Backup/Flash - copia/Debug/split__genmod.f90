        !COMPILER-GENERATED INTERFACE MODULE: Wed Oct 28 17:10:29 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SPLIT__genmod
          INTERFACE 
            SUBROUTINE SPLIT(ND,N,IDI,BETA,DEL,G,W)
              INTEGER(KIND=4) :: ND
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: IDI
              REAL(KIND=8) :: BETA
              REAL(KIND=8) :: DEL
              REAL(KIND=8) :: G(ND,ND)
              REAL(KIND=8) :: W(ND,5)
            END SUBROUTINE SPLIT
          END INTERFACE 
        END MODULE SPLIT__genmod
