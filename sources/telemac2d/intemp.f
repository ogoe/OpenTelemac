!                      *****************
                       SUBROUTINE INTEMP
!                      *****************
!
     &(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,ZF,CF,EPS,KFROT,SMH,
     & HN,QU,QV,LT,GAMMA)
!
!***********************************************************************
! TELEMAC2D   V6P1                                         03/15/2011
!***********************************************************************
!
!brief      TIME INTEGRATION WITH NEWMARK SCHEME:
!           U_(N+1)=U_N + DT*( (1-GAMMA)ACC_N +GAMMA*ACC_(N+1))
!       ACC: IS THE ACCELERATION (FLUX BALANCE FOR FV)
!       FOR GAMMA=0.5 THE SCHEME IS SECOND ORDER ACCURATE
!       FOR GAMMA=1.0 THE SCHEME IS EULER EXPLICIT (FIRST ORDER)
!+
!history  N. GOUTAL
!+        24/11/1997
!+
!+
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  R. ATA (EDF-LNHE)
!+        03/15/2011
!+        V6P1
!+    CHANGE EXPLICIT EULER BY NEWMARK SCHEME
!+     GAMMA FIXES THE SCHEME ACCURACY (SEE BELOW)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  W             |<--|  (H,HU,HV)
!|  FLUX          |-->|  FLUX AT TN+1
!|  FLUX_OLD      |-->|  FLUX AT TN
!|  AIRS          |-->|  CELL AREAS
!|  DT            |-->|  TIME STEP
!|  NPOIN         |-->|  TOTAL NUMBER OF NODES
!|  ZF            |-->|  BATHYMETRIES
!|  CF            |-->|  FRICTION COEFFICIENTS
!|  EPS           |-->|  TOLERANCE FOR WATER DEPTH
!|  KFROT         |-->|  LOGICAL! FRICTION OF NO FRICTION
!|  SMH           |-->|  MASS SOURCE
!|  HN,QU;QV      |-->|  H, HU AND HV AT TN
!|  LT            |-->|  CURRENT TIME ITERATION
!|  GAMMA         |-->|  NEWMARK PARAMETER (SEE BELOW)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,KFROT,LT
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FLUX(NPOIN,3),DT,EPS
      DOUBLE PRECISION, INTENT(IN)    :: AIRS(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FLUX_OLD(NPOIN,3)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN),SMH(NPOIN),GAMMA
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN),QU(NPOIN),QV(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION DELTA,KF,ST2D,ALPHAF,UNMGAMMA,FACT
!
!=======================
!---- NEWMARK SCHEME
!=======================
!
      UNMGAMMA = 1.D0-GAMMA
!
!     - FOR GAMMA=0.5, THIS CHOICE GIVES ORDER 2 ACCURACY AND
!     THE SCHEME IS UNCONDITIALLY STABLE
!     - FOR USER WHO PREFERS (EULER) EXPLICIT SCHEME,
!     YOU HAVE TO PUT GAMMA=1
!
      DO I=1,NPOIN
        FACT=DT/AIRS(I)
!---    FIRST TIME STEP
        IF(LT.EQ.1)THEN
          W(1,I) = HN(I) - FACT * ( FLUX(I,1)-SMH(I) )
          W(2,I) = QU(I) - FACT * FLUX (I,2)
          W(3,I) = QV(I) - FACT * FLUX (I,3)
        ELSE
          W(1,I) = HN(I) - FACT * (UNMGAMMA*FLUX_OLD(I,1) +
     &                             GAMMA*FLUX(I,1)-SMH(I))
          W(2,I) = QU(I) - FACT * (UNMGAMMA*FLUX_OLD(I,2) +
     &                             GAMMA*FLUX(I,2))
          W(3,I) = QV(I) - FACT * (UNMGAMMA*FLUX_OLD(I,3) +
     &                             GAMMA*FLUX(I,3))
        ENDIF
      ENDDO
!
!     TO TAKE INTO ACCOUNT FRICTION
!     *****************************
      IF (KFROT.NE.0) THEN
!
        DO I = 1,NPOIN
!
          IF((W(1,I).GT.EPS/10.D0).AND.(CF(I).GT.1.D-12)) THEN
            ST2D = CF(I)
            KF = 9.81D0*DT*SQRT(W(2,I)**2+W(3,I)**2)/
     &           (ST2D*ST2D*W(1,I)**(7.D0/3.D0))
            IF(KF.GT.1.D-6) THEN
              DELTA = (1.D0+4.D0*KF)
              ALPHAF = (-1.D0+SQRT(DELTA))/(2*KF)
            ELSE
              ALPHAF = 1.D0 - KF
            ENDIF
            W(2,I) = ALPHAF * W(2,I)
            W(3,I) = ALPHAF * W(3,I)
          ENDIF
!
        ENDDO ! I
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
