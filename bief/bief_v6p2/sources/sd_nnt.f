!                    *****************
                     SUBROUTINE SD_NNT
!                    *****************
!
     &(N,R,C,IL,JL,L,D,IU,JU,U,Z,B,TMP)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    NUMERIC SOLUTION OF THE TRANSPOSE OF A SPARSE
!+                NONSYMMETRICAL SYSTEM OF LINEAR EQUATIONS GIVEN
!+                LDU-FACTORISATION (UNCOMPRESSED POINTER STORAGE).
!code
!+       INPUT VARIABLES:   N, R,C, IL,JL,L, D, IU,JU,U, B
!+       OUTPUT VARIABLES:  Z
!+
!+       PARAMETERS USED INTERNALLY:
!+ FIA   \ TMP   - HOLDS NEW RIGHT-HAND SIDE B' FOR SOLUTION OF THE
!+       \           EQUATION LX = B'.
!+       \           SIZE = N.
!
!note     IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
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
      INTEGER R(*),C(*),IL(*),JL(*),IU(*),JU(*),N
      DOUBLE PRECISION L(*),D(*),U(*),Z(*),B(*),TMP(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,JMIN,JMAX
      DOUBLE PRECISION TMPK
!
!-----------------------------------------------------------------------
!
!  ******  SOLVES UT Y = B  BY FORWARD SUBSTITUTION  *******************
!
      DO K=1,N
        TMP(K) = B(C(K))
      ENDDO
!
      DO 3 K=1,N
        TMPK = - TMP(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF (JMIN.GT.JMAX)  GO TO 3
        DO 2 J=JMIN,JMAX
          TMP(JU(J)) = TMP(JU(J)) + U(J) * TMPK
2       CONTINUE
3     CONTINUE
!
!  ******  SOLVES D LT X = Y  BY BACK SUBSTITUTION  ********************
!
      K = N
      DO I=1,N
        TMPK = - TMP(K) * D(K)
        JMIN = IL(K)
        JMAX = IL(K+1) - 1
        IF(JMIN.GT.JMAX) GO TO 5
        DO 4 J=JMIN,JMAX
          TMP(JL(J)) = TMP(JL(J)) + L(J) * TMPK
4       CONTINUE
5       Z(R(K)) = - TMPK
        K = K-1
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
