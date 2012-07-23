!                       ***************** 
                        SUBROUTINE LUBKSB 
!                       ***************** 
     &( A    , N     , NP    , INDX  , B     ) 
! 
!*********************************************************************** 
! TOMAWAC   V6P2                                   25/06/2012 
!*********************************************************************** 
! 
!Brief    LINEAR EQUATION SOLUTION - BACKSUBSTITUTION 
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
!| A              |-->| LU-DECOMPOSED MATRIX 
!| B              |<--| SOLUTION VECTOR 
!| INDX           |<->| PERMUTATION VECTOR RETURNED BY LUDCMP.f 
!| N              |-->| MATRIX DIMENSION 
!| NP             |-->| MATRIX PHYSICAL DIMENSION 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      IMPLICIT NONE 
! 
!.....VARIABLES IN ARGUMENT 
!     """"""""""""""""""""" 
      INTEGER N,NP,INDX(N) 
      DOUBLE PRECISION  A(NP,NP),B(NP) 
! 
!.....LOCAL VARIABLES 
!     """"""""""""""""" 
      INTEGER I,II,J,LL 
      DOUBLE PRECISION SUM 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      II=0 
      DO 12 I=1,N 
        LL=INDX(I) 
        SUM=B(LL) 
        B(LL)=B(I) 
        IF (II.NE.0)THEN 
          DO 11 J=II,I-1 
            SUM=SUM-A(I,J)*B(J) 
11        CONTINUE 
        ELSE IF (SUM.NE.0.) THEN 
          II=I 
        ENDIF 
        B(I)=SUM 
12    CONTINUE 
      DO 14 I=N,1,-1 
        SUM=B(I) 
        DO 13 J=I+1,N 
          SUM=SUM-A(I,J)*B(J) 
13      CONTINUE 
        B(I)=SUM/A(I,I)  
14    CONTINUE 
      RETURN 
      END 
