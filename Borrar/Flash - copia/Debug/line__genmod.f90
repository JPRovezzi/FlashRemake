        !COMPILER-GENERATED INTERFACE MODULE: Wed Oct 28 17:10:29 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LINE__genmod
          INTERFACE 
            SUBROUTINE LINE(ND,N,XLAM,GD,G,W)
              INTEGER(KIND=4) :: ND
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: XLAM
              REAL(KIND=8) :: GD(ND)
              REAL(KIND=8) :: G(ND,ND)
              REAL(KIND=8) :: W(ND,5)
            END SUBROUTINE LINE
          END INTERFACE 
        END MODULE LINE__genmod
