!                       ****************
                        SUBROUTINE MAJZZ
!                       ****************
!
     &  (W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,CF,KFROT,SMH,
     &   HN,QU,QV,LT,GAMMA,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,G,
     &   RAIN,PLUIE,FU,FV)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief      TIME INTEGRATION WITH NEWMARK SCHEME:
!+           U_(N+1)=U_N + DT*( (1-GAMMA)ACC_N +GAMMA*ACC_(N+1))
!+       ACC: IS THE ACCELERATION (FLUX BALANCE FOR FV)
!+       FOR GAMMA=0.5 THE SCHEME IS SECOND ORDER ACCURATE
!+       FOR GAMMA=1.0 THE SCHEME IS EULER EXPLICIT (FIRST ORDER)
!
!
!history  R. ATA (EDF-LNHE)
!+        03/15/2011
!+        V6P1
!+    CHANGE EXPLICIT EULER BY NEWMARK SCHEME
!+    GAMMA FIXES THE SCHEME ACCURACY (SEE BELOW)
!
!history  R. ATA (EDF-LNHE)
!+        01/07/2013
!+        V6P3
!+    cleaning
!+
!history  R. ATA (EDF-LNHE)
!+        01/25/2013
!+        V7p0
!+    add normal projection of flux for liquid
!+    boundaries
!+    warning: its impact on mass balance is not taken into account
!+             to be considered for next release
!
!history  R. ATA
!+        25/12/2016
!+        V7P2
!+    include rain and evaporation 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  AIRS          |-->|  CELL AREAS
!|  CF            |-->|  FRICTION COEFFICIENTS
!|  DT            |-->|  TIME STEP
!|  FLUX          |-->|  FLUX AT TN+1
!|  FLUX_OLD      |-->|  FLUX AT TN
!|  FU,FV         |-->|  SOURCE TERMS FOR MOMENTUM (ON X AND Y)
!|  G             |-->|  GRAVITY
!|  GAMMA         |-->|  NEWMARK PARAMETER (SEE BELOW)
!|  HN,QU;QV      |-->|  H, HU AND HV AT TN
!|  KFROT         |-->|  LOGICAL! FRICTION OR NO FRICTION
!|  KNEU          |-->|  CONVENTION FOR NEUMANN POINT
!|  LIMPRO        |-->|  BC TYPE
!|  LT            |-->|  CURRENT TIME ITERATION
!|  NBOR          |-->|  GLOBAL INDEX OF BOUNDARY NODES
!|  NPOIN         |-->|  TOTAL NUMBER OF NODES
!|  NPTFR         |-->|  TOTAL NUMNER OF BOUNDARY NODES
!|  PLUIE         |-->|  RAIN
!|  RAIN          |-->|  IF YES TAKE RAIN INTO ACCOUNT
!|  SMH           |-->|  MASS SOURCE
!|  XNEBOR,YNEBOR |-->|  X AND Y COMPONENT OF THE OUTWARD UNIT NORMAL
!|  W             |<--|  (H,HU,HV)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_MAJZZ => MAJZZ
      USE DECLARATIONS_TELEMAC2D,ONLY:OPTVF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,KFROT,LT,NPTFR,KNEU
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      LOGICAL, INTENT(IN)             :: RAIN
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN),FLUX(NPOIN,3)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(IN)    :: FLUX_OLD(NPOIN,3),GAMMA
      DOUBLE PRECISION, INTENT(IN)    :: AIRS(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN),FU(NPOIN),FV(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN),QU(NPOIN),QV(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: G,PLUIE(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: SMH(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K
      DOUBLE PRECISION FACT,UNMGAMMA,UGN
!
!
!=======================
!---- TEST FOR DT
!=======================
!
      IF(DT.LE.0.D0) THEN
        WRITE(LU,*)'*********************************************'
        WRITE(LU,*)'          WARNING: TIME STEP =0'
        WRITE(LU,*)'          IN MAJZZ SUBROUTINE...'
        WRITE(LU,*)'*********************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!      PROJECTION ON THE NORMAL TO ELIMINATE THE TANGENT COMPONENT
!      ONLY FOR LIQUID BOUNDARY
!
      DO K=1,NPTFR
        I=NBOR(K)
        IF(LIMPRO(K,1).NE.KNEU) THEN
          !NORMALIZED NORMAL
          UGN =XNEBOR(K)*FLUX(I,2)+YNEBOR(K)*FLUX(I,3) ! TO RETRIEVE NORMAL COMPONENET OF UG
!         VGN =  0.D0  ! PUT TANGENTIAL COMPENENT =0
!         INVERSE ROTATION
          FLUX(I,2) = XNEBOR(K)*UGN
          FLUX(I,3) = YNEBOR(K)*UGN
        ENDIF
      ENDDO
!
!++++++++++++++++++++++++++++++++++++
! TIME INTEGRATION
!++++++++++++++++++++++++++++++++++++
!
      IF(GAMMA.EQ.1.D0) THEN
!
!==========================
!---- EULER EXPLICIT SCHEME
!==========================
!
        DO I=1,NPOIN
          FACT=DT/AIRS(I)
          W(1,I) = HN(I) + FACT*(FLUX(I,1)+SMH(I))
          IF(RAIN)W(1,I)=W(1,I)+DT*PLUIE(I)
          W(2,I) = QU(I) + FACT*FLUX(I,2) 
          W(3,I) = QV(I) + FACT*FLUX(I,3)
        ENDDO
!
      ELSEIF(GAMMA.GE.0.D0.AND.GAMMA.LT.1.D0) THEN
!
!==========================
!---- NEWMARK SCHEME
!==========================
!
        UNMGAMMA = 1.D0-GAMMA
!
!       - FOR GAMMA=0.5, THIS CHOICE GIVES ORDER 2 ACCURACY AND
!         THE SCHEME IS UNCONDITIALLY STABLE
!       - FOR USER WHO PREFERS (EULER) EXPLICIT SCHEME,
!         YOU HAVE TO PUT GAMMA=1
        DO I=1,NPOIN
          FACT=DT/AIRS(I)
          !--- FIRST TIME STEP
          IF(LT.EQ.1)THEN
            W(1,I) = HN(I) + FACT*(FLUX(I,1)+SMH(I) )
            IF(RAIN)W(1,I)=W(1,I)+DT*PLUIE(I)
            W(2,I) = QU(I) + FACT* FLUX(I,2) 
            W(3,I) = QV(I) + FACT* FLUX(I,3)
          ELSE
            W(1,I) = HN(I) + FACT*(UNMGAMMA*FLUX_OLD(I,1) +
     &                             GAMMA*FLUX(I,1)+SMH(I))
            IF(RAIN)W(1,I)=W(1,I)+DT*PLUIE(I)
            W(2,I) = QU(I) + FACT*(UNMGAMMA*FLUX_OLD(I,2) +
     &                       GAMMA*FLUX(I,2)) 
            W(3,I) = QV(I) + FACT*(UNMGAMMA*FLUX_OLD(I,3) +
     &                       GAMMA*FLUX(I,3)) 
          ENDIF
        ENDDO
!
      ELSE
        IF(LNG.EQ.1) THEN
           WRITE(LU,*) 'MAJZZ: ERREUR: COEFFICIENT DE NEWMARK DOIT ...'
           WRITE(LU,*) '... ETRE ENTRE 0 ET 1: ',GAMMA
        ELSEIF(LNG.EQ.2) THEN
           WRITE(LU,*) 'MAJZZ: ERROR:NEWMARK COEFFICIENT MUST...'
           WRITE(LU,*) '... BE BETWEEN 0 AND 1: ',GAMMA
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     PROJECTION ON THE SLIPPING BOUNDARY CONDITIONS
!     **********************************************
!
      CALL CDLPROJ(NPOIN,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,W)
!
      DO I =1,NPOIN
!        IF(W(1,I).LE.1.D-12) W(1,I)=0.D0
        IF(ABS(W(2,I)).LE.1.D-12) W(2,I)=0.D0
        IF(ABS(W(3,I)).LE.1.D-12) W(3,I)=0.D0
      ENDDO
!
!     SEMI IMPLICIT FRICTION INTRODUCTION
!     ***********************************
!     NOW CHANGED IN SOURCE_MOMENT ==> TO CHANGE LATER: MAKE IT UNIFORM
      IF(OPTVF.EQ.1.OR.OPTVF.EQ.2)THEN
        IF(KFROT.NE.0) CALL FRICTION(NPOIN,G,DT,W,HN,QU,QV,CF)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
