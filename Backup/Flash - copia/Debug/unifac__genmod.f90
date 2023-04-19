        !COMPILER-GENERATED INTERFACE MODULE: Wed Oct 28 17:10:29 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UNIFAC__genmod
          INTERFACE 
            SUBROUTINE UNIFAC(NDIF,X,ACT,DACT,PACT)
              COMMON/NGA/ NGA,MASS
                INTEGER(KIND=4) :: NGA
                INTEGER(KIND=4) :: MASS(12)
              INTEGER(KIND=4) :: NDIF
              REAL(KIND=8) :: X(10)
              REAL(KIND=8) :: ACT(10)
              REAL(KIND=8) :: DACT(10,10)
              REAL(KIND=8) :: PACT(2,2)
            END SUBROUTINE UNIFAC
          END INTERFACE 
        END MODULE UNIFAC__genmod
