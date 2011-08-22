!                    *****************
                     SUBROUTINE SD_NSF
!                    *****************
!
     &(N,R,IC,IA,JA,IL,JL,JLMAX,IU,JU,JUMAX,Q,IM,FLAG)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SYMBOLIC LDU-FACTORISATION OF A NONSYMMETRICAL
!+                SPARSE MATRIX (UNCOMPRESSED POINTER STORAGE).
!code
!+       INPUT VARIABLES:   N, R,IC, IA,JA, JLMAX, JUMAX.
!+       OUTPUT VARIABLES:  IL,JL, IU,JU, FLAG.
!+
!+       PARAMETERS USED INTERNALLY:
!+ NIA   \ Q     - SUPPOSE M' IS THE RESULT OF REORDERING M;  IF
!+       \           PROCESSING OF THE KTH ROW OF M' (HENCE THE KTH ROWS
!+       \           OF L AND U) IS BEING DONE, THEN Q(J) IS INITIALLY
!+       \           NONZERO IF M'(K,J) IS NONZERO;  SINCE VALUES NEED
!+       \           NOT BE STORED, EACH ENTRY POINTS TO THE NEXT
!+       \           NONZERO;  FOR EXAMPLE, IF  N=9  AND THE 5TH ROW OF
!+       \           M' IS
!+       \                   0 X X 0 X 0 0 X 0,
!+       \           THEN Q WILL INITIALLY BE
!+       \                   A 3 5 A 8 A A 10 A 2        (A - ARBITRARY);
!+       \           Q(N+1) POINTS TO THE FIRST NONZERO IN THE ROW AND
!+       \           THE LAST NONZERO POINTS TO  N+1;  AS THE ALGORITHM
!+       \           PROCEEDS, OTHER ELEMENTS OF Q ARE INSERTED IN THE
!+       \           LIST BECAUSE OF FILLIN.
!+       \           SIZE = N+1.
!+ NIA   \ IM    - AT EACH STEP IN THE FACTORIZATION, IM(I) IS THE LAST
!+       \           ELEMENT IN THE ITH ROW OF U WHICH NEEDS TO BE
!+       \           CONSIDERED IN COMPUTING FILLIN.
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
!| FLAG           |<--| INDICATOR ERROR : 
!|                |   |= N + R(K):  NULL ROW IN A
!|                |   |= 2*N + R(K):DUPLICATE ENTRY IN A
!|                |   |= 3*N + K:INSUFFICIENT STORAGE FOR JL
!|                |   |= 6*N + K:INSUFFICIENT STORAGE FOR JU
!|                |   |= 5*N + K: ZERO PIVOT
!| IA, JA         |-->| STRUCTURE OF A NONSYMMETRICAL MATRIX
!| IC             |-->| INVERSE OF THE ORDERING OF THE COLUMNS OF MATRIX
!| IL, JL         |<--| STRUCTURE OF LOWER FACTORISED TRIANGULAR MATRIX
!| IU, JU         |<--| STRUCTURE OF UPPER FACTORISED TRIANGULAR MATRIX
!| IM             |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;SIZE = N
!| JLMAX          |-->| PREVISIONAL MAXIMUM DIMENSION OF JL
!| JUMAX          |-->| PREVISIONAL MAXIMUM DIMENSION OF JU
!| N              |-->| RANK OF MATRIX
!| Q              |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;SIZE = N+1
!| R              |-->| ORDERING OF THE ROWS OF MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N
      INTEGER R(*),IC(*),IA(*),JA(*),IL(*),JL(*)
      INTEGER IU(*),JU(*),Q(*),IM(*),FLAG,QM,VJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     JLPTR - POINTS TO THE LAST POSITION USED IN  JL
!     JUPTR - POINTS TO THE LAST POSITION USED IN  JU
!
      INTEGER JLPTR,JUPTR,JLMAX,JUMAX,I,J,K,M,JMIN,JMAX
!
!-----------------------------------------------------------------------
!
!     INITIALISES POINTERS
!
      JLPTR = 0
      IL(1) = 1
      JUPTR = 0
      IU(1) = 1
!
!     FOR EACH ROW OF L AND U
!
      DO 10 K=1,N
!       SETS Q TO THE REORDERED ROW OF A
        Q(N+1) = N+1
        JMIN = IA(R(K))
        JMAX = IA(R(K)+1) - 1
        IF(JMIN.GT.JMAX) GO TO 101
        DO J=JMIN,JMAX
          VJ = IC(JA(J))
          QM = N+1
1         CONTINUE
          M = QM
          QM = Q(M)
          IF(QM.LT.VJ) GO TO 1
          IF(QM.EQ.VJ) GO TO 102
          Q(M) = VJ
          Q(VJ) = QM
        ENDDO
!
!       FOR EACH ENTRY IN THE LOWER TRIANGLE
!
        I = N+1
3       CONTINUE
        I = Q(I)
        IF(I.GE.K) GO TO 7
!       L(K,I) WILL BE NONZERO, SO ADDS IT TO JL
        JLPTR = JLPTR+1
        IF(JLPTR.GT.JLMAX) GO TO 103
        JL(JLPTR) = I
        QM = I
!       INSPECTS ITH ROW FOR FILLING, ADJUSTS IM IF POSSIBLE
        JMIN = IU(I)
        JMAX = IM(I)
        IF(JMIN.GT.JMAX)  GO TO 3
        DO 5 J=JMIN,JMAX
          VJ = JU(J)
          IF (VJ.EQ.K)  IM(I) = J
4         CONTINUE
          M = QM
          QM = Q(M)
          IF (QM.LT.VJ)  GO TO 4
          IF (QM.EQ.VJ)  GO TO 5
          Q(M) = VJ
          Q(VJ) = QM
          QM = VJ
5       CONTINUE
        GO TO 3
!
!       CHECKS FOR 0 PIVOT
!
7       CONTINUE
        IF(I.NE.K) GO TO 105
!       REMAINING ELEMENTS OF Q DEFINE STRUCTURE OF U(K, )
8       CONTINUE
        I = Q(I)
        IF(I.GT.N) GO TO 9
        JUPTR = JUPTR+1
        IF (JUPTR.GT.JUMAX)  GO TO 106
        JU(JUPTR) = I
        GO TO 8
!       GETS READY FOR NEXT ROW
9       CONTINUE
        IM(K) = JUPTR
        IL(K+1) = JLPTR+1
        IU(K+1) = JUPTR+1
10    CONTINUE
!
        FLAG = 0
        RETURN
!
! ** ERROR:  NULL ROW IN A
 101    FLAG = N + R(K)
        RETURN
! ** ERROR:  DUPLICATE ENTRY IN A
 102    FLAG = 2*N + R(K)
        RETURN
! ** ERROR:  INSUFFICIENT STORAGE FOR JL
 103    FLAG = 3*N + K
        RETURN
! ** ERROR:  0 PIVOT
 105    FLAG = 5*N + K
        RETURN
! ** ERROR:  INSUFFICIENT STORAGE FOR JU
 106    FLAG = 6*N + K
!
!-----------------------------------------------------------------------
!
      RETURN
      END
