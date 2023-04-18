        !COMPILER-GENERATED INTERFACE MODULE: Wed Oct 28 17:10:29 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TMSSJ__genmod
          INTERFACE 
            SUBROUTINE TMSSJ(ND,N,IPR,NMAX,XLAM,GLIM,F,X,GD,G,W,IFUNC)
              INTEGER(KIND=4) :: ND
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: IPR
              INTEGER(KIND=4) :: NMAX
              REAL(KIND=8) :: XLAM
              REAL(KIND=8) :: GLIM
              REAL(KIND=8) :: F
              REAL(KIND=8) :: X(ND)
              REAL(KIND=8) :: GD(ND)
              REAL(KIND=8) :: G(ND,ND)
              REAL(KIND=8) :: W(ND,5)
              INTEGER(KIND=4) :: IFUNC
            END SUBROUTINE TMSSJ
          END INTERFACE 
        END MODULE TMSSJ__genmod
