!                       ****************
                        SUBROUTINE MAJZZ  
!                       ****************
!  
     &(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,ZF,CF,EPS,KFROT,SMH,
     & HN,QU,QV,LT,GAMMA,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU)
!
!***********************************************************************
! TELEMAC2D   V6P1                                         03/15/2011
!***********************************************************************
!
!brief      TIME INTEGRATION WITH NEWMARK SCHEME:
!+          U_(N+1)=U_N + DT*( (1-GAMMA)ACC_N +GAMMA*ACC_(N+1))
!+      ACC: IS THE ACCELERATION (FLUX BALANCE FOR FV)
!+      FOR GAMMA=0.5 THE SCHEME IS SECOND ORDER ACCURATE
!+      FOR GAMMA=1.0 THE SCHEME IS EULER EXPLICIT (FIRST ORDER)
!
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
!|  NPTFR         |-->|  TOTAL NUMNER OF BOUNDARY NODES
!|  NBOR          |-->|  GLOBAL INDEX OF BOUNDARY NODES
!|  LIMPRO        |-->|  BC TYPE
!|  XNEBOR,YNEBOR |-->|  X AND Y COMPONENT OF THE OUTWARD UNIT NORMAL
!|  KNEU          |-->|  CONVENTION FOR NEUMANN POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,KFROT,LT,NPTFR,KNEU
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FLUX(NPOIN,3),DT,EPS
      DOUBLE PRECISION, INTENT(IN)    :: FLUX_OLD(NPOIN,3),GAMMA
      DOUBLE PRECISION, INTENT(IN)    :: AIRS(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN),SMH(NPOIN) 
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN),QU(NPOIN),QV(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
      INTEGER I 
! 
      DOUBLE PRECISION FACT,UNMGAMMA
      DOUBLE PRECISION, PARAMETER :: G = 9.81D0

!=======================
!---- NEWMARK SCHEME
!=======================
! 
      UNMGAMMA = 1.0D0-GAMMA
!  - FOR GAMMA=0.5, THIS CHOICE GIVES ORDER 2 ACCURACY AND
!     THE SCHEME IS UNCONDITIALLY STABLE
!  - FOR USER WHO PREFERS (EULER) EXPLICIT SCHEME,
!     YOU HAVE TO PUT GAMMA=1  
      DO I=1,NPOIN
        FACT=DT/AIRS(I)
        !--- FIRST TIME STEP
        IF (LT.EQ.1)THEN
           W(1,I) = HN(I) + FACT * ( FLUX(I,1)+SMH(I) ) 
           W(2,I) = QU(I) + FACT * FLUX (I,2)
           W(3,I) = QV(I) + FACT * FLUX (I,3)
        ELSE
           W(1,I) = HN(I) + FACT * (UNMGAMMA*FLUX_OLD(I,1) + 
     &                             GAMMA*FLUX(I,1)+SMH(I))
           W(2,I) = QU(I) + FACT * (UNMGAMMA*FLUX_OLD(I,2) + 
     &                             GAMMA*FLUX(I,2))
           W(3,I) = QV(I) + FACT * (UNMGAMMA*FLUX_OLD(I,3) + 
     &                             GAMMA*FLUX(I,3))
        ENDIF
      ENDDO

100   CONTINUE
!   PROJECTION ON THE SLIPPING BOUNDARY CONDITIONS
!   **********************************************
      CALL CDLPROJ(NPOIN,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,W)
!
!      DO I =1,NPOIN
!        IF(W(1,I).LE.1.D-12) W(1,I)=0.D0
!        IF(ABS(W(2,I)).LE.1.D-12) W(2,I)=0.D0
!        IF(ABS(W(3,I)).LE.1.D-12) W(3,I)=0.D0
!      ENDDO
!     SEMI IMPLICIT FRICTION INTRODUCTION
!     ***********************************
      IF (KFROT.NE.0) CALL FRICTION(NPOIN,G,DT,W,HN,QU,QV,CF)
!
      RETURN
      END
