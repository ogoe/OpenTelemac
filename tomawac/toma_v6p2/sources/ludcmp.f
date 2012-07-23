!                       ***************** 
                        SUBROUTINE LUDCMP 
!                       ***************** 
     &( A    , N     , NP    , INDX  , D     ) 
! 
!*********************************************************************** 
! TOMAWAC   V6P2                                   25/06/2012 
!*********************************************************************** 
! 
!Brief    LINEAR EQUATION SOLUTION - LU DECOMPOSITION 
!+ 
!+            TAKEN FROM NUMERICAL RECEIPES IN FORTRAN 77 
!+            USED IN THE FREE-MESH METHOD (DIFFRACTION) 
!+            FOR THE CALCULATION OF INVERSE MATRIX NXN 
! 
!history  E. KRIEZI (LNH) 
!+        04/12/2006 
!+        V5P5 
!+ 
! 
! 
!history  G.MATTAROLO (EDF - LNHE) 
!+        23/06/2012 
!+        V6P2 
!+        Modification for V6P2 
!  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!| A              |<--| LU-DECOMPOSED MATRIX 
!| D              |<--| INDICATES EVEN OR ODD N. OF RAW INTERCHANGES 
!| INDX           |<--| PERMUTATION VECTOR RETURNED BY LUDCMP.f 
!| N              |-->| MATRIX DIMENSION 
!| NP             |-->| MATRIX PHYSICAL DIMENSION 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      IMPLICIT NONE 
! 
!.....VARIABLES IN ARGUMENT 
!     """"""""""""""""""""" 
      INTEGER N,NP,INDX(N) 
      DOUBLE PRECISION D, A(NP,NP) 
 
!.....LOCAL VARIABLES 
!     """"""""""""""" 
      INTEGER, PARAMETER:: NMAX=1000 
      INTEGER I, IMAX, J, K 
      DOUBLE PRECISION, PARAMETER:: TINY=1.0E-20 
      DOUBLE PRECISION AAMAX, DUM, SUM, VV(NMAX) 
!      PARAMETER (NMAX=1000,TINY=1.0E-20) 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      D=1. 
      DO 12 I=1,N 
        AAMAX=0. 
        DO 11 J=1,N 
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J)) 
 
 
11      CONTINUE 
        IF (AAMAX.EQ.0.) PAUSE 'singular matrix in ludcmp' 
        VV(I)=1./AAMAX 
         
12    CONTINUE 
      DO 19 J=1,N 
        DO 14 I=1,J-1 
          SUM=A(I,J) 
          DO 13 K=1,I-1 
            SUM=SUM-A(I,K)*A(K,J) 
13        CONTINUE 
          A(I,J)=SUM 
14      CONTINUE 
        AAMAX=0. 
        DO 16 I=J,N 
          SUM=A(I,J) 
          DO 15 K=1,J-1 
            SUM=SUM-A(I,K)*A(K,J) 
15        CONTINUE 
          A(I,J)=SUM 
          DUM=VV(I)*ABS(SUM) 
          IF (DUM.GE.AAMAX) THEN 
            IMAX=I 
            AAMAX=DUM 
          ENDIF 
16      CONTINUE 
        IF (J.NE.IMAX)THEN 
          DO 17 K=1,N 
            DUM=A(IMAX,K) 
            A(IMAX,K)=A(J,K) 
            A(J,K)=DUM 
17        CONTINUE 
          D=-D 
          VV(IMAX)=VV(J) 
        ENDIF 
        INDX(J)=IMAX 
        IF(A(J,J).EQ.0.)A(J,J)=TINY 
        IF(J.NE.N)THEN 
          DUM=1./A(J,J) 
          DO 18 I=J+1,N 
            A(I,J)=A(I,J)*DUM 
18        CONTINUE 
        ENDIF 
19    CONTINUE 
      RETURN 
      END 
