!                    *****************
                     SUBROUTINE CNTPRE
!                    *****************
!
     &(DAM,NPOIN,IPRECO,IPREC2)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INHIBITS THE DIAGONAL PRECONDITIONING IF ONE OF THE
!+                ELEMENTS OF DAM IS NEGATIVE OR ZERO.
!code
!+  --------------------------------------------------------------------
!+ |  VALUE OF IPREC2   I                  MEANING
!+ |    OR IPRECO       I
!+  --------------------------------------------------------------------
!+ |       1            I  NOTHING.
!+ |       2            I  DIAGONAL PRECONDITIONING USING THE MATRIX
!+ |                    I  DIAGONAL.
!+ |       3            I  DIAGONAL PRECONDITIONING USING THE CONDENSED
!+ |                    I  MATRIX.
!+ |       5            I  OTHER (NOT DEFINED)
!+ |                    I
!+ |       7            I  CROUT PRECONDITIONING BY ELEMENT
!+ |                    I  (NOT CODED IN)
!+  --------------------------------------------------------------------
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        02/06/1999
!+        V5P1
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
!| DAM            |-->| DIAGONALE OF THE MATRIX
!| IPREC2         |<--| PRECONDITIONNING USED
!| IPRECO         |-->| PRECONDITIONNING REQUIRED BY USER
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NPOIN,IPRECO,IPREC2,I
!
      DOUBLE PRECISION DAM(NPOIN)
!
!-----------------------------------------------------------------------
!
      IF (IPRECO.EQ.0) IPRECO = 1
      IPREC2 = IPRECO
!
      IF (MOD(IPRECO,2).EQ.0.OR.MOD(IPRECO,3).EQ.0) THEN
!
         DO 10 I=1,NPOIN
            IF (DAM(I).LE.0.D0) THEN
20             CONTINUE
               IF(MOD(IPREC2,2).EQ.0) THEN
                 IPREC2 = IPREC2/2
                 GO TO 20
               ENDIF
21             CONTINUE
               IF(MOD(IPREC2,3).EQ.0) THEN
                 IPREC2 = IPREC2/3
                 GO TO 21
               ENDIF
               IF (LNG.EQ.1) WRITE(LU,100)
               IF (LNG.EQ.2) WRITE(LU,101)
100     FORMAT(1X,'CNTPRE (ARTEMIS) : PRECONDITIONNEMENT DIAGONAL NON AP
     &PLIQUE (UN ELEMENT DIAGONAL DE LA MATRICE EST NEGATIF OU NUL)')
101     FORMAT(1X,'CNTPRE (ARTEMIS) : DIAGONAL SCALING NOT APPLIED (ONE
     &COEFFICIENT OF THE MATRIX DIAGONAL IS NEGATIVE OR ZERO)')
               GOTO 30
            ENDIF
10       CONTINUE
!
      ELSEIF (MOD(IPRECO,5).EQ.0) THEN
!
         DO 40 I=1,NPOIN
            IF (ABS(DAM(I)).LE.1.D-6) THEN
50             CONTINUE
               IF(MOD(IPREC2,5).EQ.0) THEN
                 IPREC2 = IPREC2/5
                 GO TO 50
               ENDIF
               IF (LNG.EQ.1) WRITE(LU,200)
               IF (LNG.EQ.2) WRITE(LU,201)
200     FORMAT(1X,'CNTPRE (ARTEMIS) : PRECONDITIONNEMENT DIAGONAL NON AP
     &PLIQUE (UN ELEMENT DIAGONAL DE LA MATRICE EST NUL)')
201     FORMAT(1X,'CNTPRE (ARTEMIS) : DIAGONAL SCALING NOT APPLIED (ONE
     &COEFFICIENT OF THE MATRIX DIAGONAL IS ZERO)')
               GOTO 30
            ENDIF
40       CONTINUE
!
      ENDIF
!
30    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
