!                    *****************
                     SUBROUTINE SD_NNS
!                    *****************
!
     &(N,R,C,IL,JL,L,D,IU,JU,U,Z,B,TMP)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    NUMERIC SOLUTION OF A SPARSE NONSYMMETRICAL SYSTEM
!+                OF LINEAR EQUATIONS GIVEN LDU-FACTORISATION
!+               (UNCOMPRESSED POINTER STORAGE).
!code
!+       INPUT VARIABLES:   N, R,C, IL,JL,L, D, IU,JU,U, B
!+       OUTPUT VARIABLES:  Z
!+
!+       PARAMETERS USED INTERNALLY:
!+ FIA   \ TMP   - HOLDS NEW RIGHT-HAND SIDE B' FOR SOLUTION OF THE
!+       \           EQUATION UX = B'.
!+       \           SIZE = N.
!
!note     IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!
!         DON'T HESITATE TO CHANGE IN/OUTPUT VARIABLES COMMENTS 
!         FOR CLARITY
!
!history  E. RAZAFINDRAKOTO (LNH)
!+        18/02/08
!+        V5P9
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| B              |-->| RIGHT-HAND SIDE B ; 
!| C              |-->| ORDERING OF THE COLUMNS OF MATRIX
!| D              |-->| DIAGONAL FACTORIZED OF MATRIX
!| IL, JL         |-->| STRUCTURE OF LOWER FACTORISED TRIANGULAR MATRIX
!| IU, JU         |-->| STRUCTURE OF UPPER FACTORISED TRIANGULAR MATRIX
!| L              |-->| LOWER FACTORIZED TRIANGULAR MATRIX
!| N              |-->| RANK OF MATRIX
!| R              |-->| ORDERING OF THE ROWS OF MATRIX
!| TMP            |-->| REAL ONE-DIMENSIONAL WORK ARRAY
!| U              |-->| UPPER FACTORIZED TRIANGULAR MATRIX
!| Z              |<--| SOLUTION X
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,R(*),C(*),IL(*),JL(*),IU(*),JU(*)
      DOUBLE PRECISION L(*),D(*),U(*),Z(*),B(*),TMP(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,JMIN,JMAX
      DOUBLE PRECISION BSUM
!
!-----------------------------------------------------------------------
!
!  ******  SOLVES LDY = B  BY FORWARD SUBSTITUTION  ********************
!
        DO 2 K=1,N
          BSUM = B(R(K))
          JMIN = IL(K)
          JMAX = IL(K+1) - 1
          IF (JMIN.GT.JMAX)  GO TO 2
          DO 1 J=JMIN,JMAX
            BSUM = BSUM - L(J) * TMP(JL(J))
1         CONTINUE
2         TMP(K) = BSUM * D(K)
!
!  ******  SOLVES  UX = Y  BY BACK SUBSTITUTION  ***********************
!
      K = N
      DO 5 I=1,N
        BSUM = TMP(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF (JMIN.GT.JMAX)  GO TO 4
        DO 3 J=JMIN,JMAX
          BSUM = BSUM - U(J) * TMP(JU(J))
3       CONTINUE
   4    TMP(K) = BSUM
        Z(C(K)) = BSUM
        K = K-1
5     CONTINUE
!
!------------------------------------------------------------------------
!
      RETURN
      END
