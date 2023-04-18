        !COMPILER-GENERATED INTERFACE MODULE: Thu Jul 05 18:29:55 2018
        MODULE UNIFA2__genmod
          INTERFACE 
            SUBROUTINE UNIFA2(NDIF,X,ACT,DACT,PACT)
              COMMON/NGA/ NGA,MASS
                INTEGER(KIND=4) :: NGA
                INTEGER(KIND=4) :: MASS(12)
              INTEGER(KIND=4) :: NDIF
              REAL(KIND=8) :: X(10)
              REAL(KIND=8) :: ACT(10)
              REAL(KIND=8) :: DACT(10,10)
              REAL(KIND=8) :: PACT(2,2)
            END SUBROUTINE UNIFA2
          END INTERFACE 
        END MODULE UNIFA2__genmod
