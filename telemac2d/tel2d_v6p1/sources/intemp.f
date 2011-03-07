!                    *****************
                     SUBROUTINE INTEMP
!                    *****************
!
     &(W,FLUX,AIRS,DT,NPOIN,ZF,CF,EPS,DDMIN,KFROT,SMH)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INTEGRATES IN TIME.
!
!note     DDMIN NOT USED (JMH)
!note     PORTABILITY: CRAY
!
!history  N. GOUTAL
!+        24/11/1997
!+
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
!| AIRS           |-->| TABLEAU DES AIRES DES CELLULES
!| CF             |---|
!| DDMIN          |---|
!| DT             |-->| PAS DE TEMPS.
!| EPS            |---|
!| FLUX           |-->| FLUX DE ROE
!| KFROT          |---|
!| NPOIN          |---|
!| SMH            |---|
!| W              |---|
!| ZF             |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,KFROT
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FLUX(NPOIN,3),DT,EPS,DDMIN
      DOUBLE PRECISION, INTENT(IN)    :: AIRS(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN),SMH(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION DELTA,KF,ST2D,ALPHAF
!
!-----------------------------------------------------------------------
!
!------
! 1. COMPUTES W AT TIME N+1
!------
!
      DO 10 I = 1 , NPOIN
         W(1,I) = W(1,I) - DT *( FLUX (I,1)-SMH(I)) / AIRS(I)
         W(2,I) = W(2,I) - DT * FLUX (I,2) / AIRS(I)
         W(3,I) = W(3,I) - DT * FLUX (I,3) / AIRS(I)
10    CONTINUE
!
!     TAKES FRICTION INTO ACCOUNT (NO FRICTION RITTER)
!     *****************************
      IF (KFROT.NE.0) THEN
!
         DO 20 I = 1,NPOIN
!
! FH-FRDATA
!            IF (W(1,I).GT.EPS/10.D0) THEN
            IF ((W(1,I).GT.EPS/10.D0).AND.(CF(I).GT.1.D-12)) THEN
! FH-FRDATA
               ST2D = CF(I)
               KF = 9.81D0*DT*DSQRT(W(2,I)**2+W(3,I)**2)/
     &              (ST2D*ST2D*W(1,I)**(7.D0/3.D0))
               IF (KF.GT.1.D-6) THEN
                  DELTA = (1.D0+4.D0*KF)
                  ALPHAF = (-1.D0+SQRT(DELTA))/(2*KF)
               ELSE
                  ALPHAF = 1.D0 - KF
               ENDIF
                  W(2,I) = ALPHAF * W(2,I)
                  W(3,I) = ALPHAF * W(3,I)
            ENDIF
!
20       CONTINUE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END