!        SUBROUTINE CSIMPN(CF,A,B,EPS,N,R)
!        IMPLICIT REAL*8(A-H,O-Z)
!        COMPLEX*16 A,B,R(N),S(2000),S1(2000),S2(2000),
!     *  S3(2000),H,H1,H3,SB(2000),RI
!        COMPLEX*16 U
!        COMMON/CH/H/B/SB,IB/INF/IY,IV
!     *  /EPS/EP1
!        H=(B-A)*0.5D0
!        NH=1
!        EP1=EPS
!        IF(IB)2,2,3
! 2      DO 11 I=1,N
! 11     S1(I)=SB(I)
!        GO TO 12
! 3      CALL CF(A,S1,N)
! 12     CALL CF(B,SB,N)
!        IF(IV.LT.0) GO TO 13
!        IF(IY.GT.0) GO TO 13
!        B=A+H
! 14     IY=1
!        CALL CF(B,SB,N)
!        H=H*0.5D0
!        B=B+H*SIGN(1,IY)       
!        AH=ABS(H)
!        IF(AH.GT.EPS) GO TO 14
!        H=(B-A)*0.5D0
!        IY=-1
!        CALL CF(B,SB,N)
! 13     CALL CF(A+H,S3,N)
!        H3=H/3.0D0
!        DO 4 I=1,N
!        S1(I)=(S1(I)+SB(I))*H3
!        S2(I)=0.0D0
!        S3(I)=4.0D0*H3*S3(I)
! 4      R(I)=S1(I)+S3(I)
! 1      H1=H
!        H=0.5D0*H
!        H3=4.0D0*H/3.0D0
!        NH=2*NH
!        DO 5 I=1,N
!        S1(I)=0.50D0*S1(I)
!        S2(I)=0.50D0*S2(I)+0.25D0*S3(I)
! 5      S3(I)=0.0D0
!        U=A+H
!        DO 6 K=1,NH
!        CALL CF(U,S,N)
!        DO 7 I=1,N
! 7      S3(I)=S3(I)+S(I)*H3
! 6      U=U+H1
!        PM=0.0D0
!        DO 8 I=1,N      
!        RI=S1(I)+S2(I)+S3(I)
!        AH=ABS(RI)
!        IF(AH.LT.1D-9) GO TO 8
!        T=ABS((RI-R(I))/RI)
!        IF(T.LE.PM) GO TO 8
!        PM=T
!!	print*,A
!!	print*,PM
!!	pause 1 
! 8      R(I)=RI
!        IF(PM.LE.EPS) RETURN
!        AH=ABS(H)
!        IF(AH.GT.EPS) GO TO 1
!        RETURN
!        END
