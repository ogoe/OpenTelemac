!                    *****************
                     SUBROUTINE SD_NNF
!                    *****************
!
     &(N,R,C,IC,IA,JA,A,Z,B,IL,JL,L,LMAX,D,IU,JU,U,UMAX,ROW,TMP,FLAG)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    NUMERIC LDU-FACTORISATION OF SPARSE NONSYMMETRICAL
!+                MATRIX AND SOLUTION OF SYSTEM OF LINEAR EQUATIONS
!+              (UNCOMPRESSED POINTER STORAGE).
!code
!+       INPUT VARIABLES:   N, R,C,IC, IA,JA,A, B, IL,JL,LMAX, IU,JU,UMAX
!+       OUTPUT VARIABLES:  Z, L,D,U, FLAG
!+
!+       PARAMETERS USED INTERNALLY:
!+ FIA   \ ROW   - HOLDS INTERMEDIATE VALUES IN CALCULATION OF L, D, U.
!+       \           SIZE = N.
!+ FIA   \ TMP   - HOLDS NEW RIGHT-HAND SIDE B' FOR SOLUTION OF THE
!+       \           EQUATION  UX = B'.
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
!| A              |-->| NONZERO ENTRIES OF THE COEFFICIENT MATRIX M, 
!|                |   | STORED BY ROWS
!| B              |-->| RIGHT-HAND SIDE B ; 
!| C              |-->| ORDERING OF THE COLUMNS OF MATRIX
!| D              |<--| DIAGONAL FACTORIZED OF MATRIX
!| FLAG           |<--| INDICATOR ERROR : 
!|                |   |= 4*N + 1:INSUFFICIENT STORAGE FOR L
!|                |   |= 7*N + 1:INSUFFICIENT STORAGE FOR U
!| IA, JA         |-->| STRUCTURE OF A NONSYMMETRICAL MATRIX
!| IA, JA         |-->| STRUCTURE OF A NONSYMMETRICAL MATRIX
!| IC             |-->| INVERSE OF THE ORDERING OF THE COLUMNS OF MATRIX
!| IL, JL         |-->| STRUCTURE OF LOWER FACTORISED TRIANGULAR MATRIX
!| IU, JU         |-->| STRUCTURE OF UPPER FACTORISED TRIANGULAR MATRIX
!| L              |<--| LOWER FACTORIZED TRIANGULAR MATRIX
!| LMAX           |-->| PREVISIONAL MAXIMUM DIMENSION OF JL
!| N              |-->| RANK OF MATRIX
!| R              |-->| ORDERING OF THE ROWS OF MATRIX
!| ROW            |---| REAL ONE-DIMENSIONAL WORK ARRAY 
!| TMP            |---| REAL ONE-DIMENSIONAL WORK ARRAY
!| U              |<--| UPPER FACTORIZED TRIANGULAR MATRIX
!| UMAX           |-->| PREVISIONAL MAXIMUM DIMENSION OF JU
!| Z              |<--| SOLUTION X
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER R(*),C(*),IC(*),IA(*),JA(*),N
      INTEGER IL(*),JL(*),LMAX,IU(*),JU(*),UMAX,FLAG
      DOUBLE PRECISION A(*),Z(*),B(*),L(*),D(*),U(*),ROW(*),TMP(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LI,I,J,K,IMIN,IMAX,JMIN,JMAX
      DOUBLE PRECISION BSUM,DK
!
!-----------------------------------------------------------------------
!
!     CHECKS STORAGE
!
      IF(IL(N+1)-1.GT.LMAX) THEN
!       ERROR: INSUFFICIENT STORAGE FOR L
        FLAG = 4*N + 1
        RETURN
      ENDIF
      IF(IU(N+1)-1.GT.UMAX) THEN
!       ERROR: INSUFFICIENT STORAGE FOR U
        FLAG = 7*N + 1
        RETURN
      ENDIF
!
!     FOR EACH ROW
!
      DO 10 K=1,N
!
!       SETS THE INITIAL STRUCTURE OF ROW
!
        JMIN = IL(K)
        JMAX = IL(K+1) - 1
        IF(JMAX.GE.JMIN) THEN
!         IF L(K,M) .NE. 0, ROW(M)=0
          DO J=JMIN,JMAX
            ROW(JL(J)) = 0
          ENDDO
        ENDIF
        ROW(K) = 0
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF(JMAX.GE.JMIN) THEN
!         IF U(K,M) .NE. 0, ROW(M)=0
          DO J=JMIN,JMAX
            ROW(JU(J)) = 0
          ENDDO
        ENDIF
        JMIN = IA(R(K))
        JMAX = IA(R(K)+1) - 1
!       SETS ROW TO KTH ROW OF REORDERED A
        DO J=JMIN,JMAX
          ROW(IC(JA(J))) = A(J)
        ENDDO
!       INITIALISES BSUM
        BSUM = B(R(K))
!
!       ASSIGNS THE KTH ROW OF L AND ADJUSTS ROW, BSUM
        IMIN = IL(K)
        IMAX = IL(K+1) - 1
        IF(IMAX.GT.IMIN) THEN
          DO I=IMIN,IMAX
            LI = - ROW(JL(I))
!           IF L IS NOT REQUIRED, THEN COMMENT OUT THE FOLLOWING LINE
            L(I) = - LI
            BSUM = BSUM + LI * TMP(JL(I))
            JMIN = IU(JL(I))
            JMAX = IU(JL(I)+1) - 1
            IF(JMAX.GT.JMIN) THEN
              DO J=JMIN,JMAX
                ROW(JU(J)) = ROW(JU(J)) + LI * U(J)
              ENDDO
            ENDIF
          ENDDO
        ENDIF
!
!       ASSIGNS DIAGONAL D AND KTH ROW OF U, SETS TMP(K)
!
        IF(ROW(K).EQ.0) THEN
!         ERROR:  ZERO PIVOT
          FLAG = 8*N + K
          RETURN
        ENDIF
        DK = 1 / ROW(K)
        D(K) = DK
        TMP(K) = BSUM * DK
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF(JMAX.GE.JMIN) THEN
          DO J=JMIN,JMAX
            U(J) = ROW(JU(J)) * DK
          ENDDO
        ENDIF
10    CONTINUE
!
!     SOLVES  UX = TMP  BY BACK SUBSTITUTION
!
      K = N
      DO I=1,N
        BSUM = TMP(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF(JMAX.GE.JMIN) THEN
          DO J=JMIN,JMAX
            BSUM = BSUM - U(J) * TMP(JU(J))
          ENDDO
        ENDIF
        TMP(K)  = BSUM
        Z(C(K)) = BSUM
        K = K-1
      ENDDO
!
      FLAG = 0
!
!-----------------------------------------------------------------------
!
      RETURN
      END
