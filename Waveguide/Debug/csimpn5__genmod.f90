        !COMPILER-GENERATED INTERFACE MODULE: Mon Mar 27 13:36:13 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CSIMPN5__genmod
          INTERFACE 
            SUBROUTINE CSIMPN5(CF,A,B,EPS,N,SB,R)
              INTEGER(KIND=4) :: N
              EXTERNAL CF
              COMPLEX(KIND=8) :: A
              COMPLEX(KIND=8) :: B
              REAL(KIND=8) :: EPS
              COMPLEX(KIND=8) :: SB(N)
              COMPLEX(KIND=8) :: R(N)
            END SUBROUTINE CSIMPN5
          END INTERFACE 
        END MODULE CSIMPN5__genmod
