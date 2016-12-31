!                    *******************
                     SUBROUTINE FLUX_TCH
!                    *******************
!
     &(X,Y,NS,NSEG,NELEM,NUBO,G,W,ZF,VNOCL,
     & ELTSEG,CE,IFABOR)
!
!***********************************************************************
! TELEMAC-2D VERSION 6.2                                     03/15/2011
!***********************************************************************
!
!brief  COMPUTES TCHAMEN FLUX AT THE INERNAL INTERFACES
!       REF.:"MODELING OF WETTING-DRYING TRANSITIONS IN FREE SURFACE FLOWS
!             OVER COMPLEX TOPOGRAPHIES" CMAME 199(2010) PP 2281-2304
!
!history  R. ATA (EDF-LNHE)
!+        06/01/2012
!+        V6P1
!+
!history  R. ATA (EDF-LNHE)
!+        28/08/2012
!+        V6P2
!+     cleaning up unused variables
!+
!history  R. ATA (EDF-LNHE)
!+
!+        07/01/2013
!+        V6P3
!+      adaptation with the new data structure (common with FEM)
!
!history  R. ATA (EDF-LNHE)
!+
!+        30/03/2013
!+        V6P3
!+      PARALLELIZATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  NS            |-->|  TOTAL NUMBER OF NODES
!|  NSEG          |-->|  TOTAL NUMBER OF EDGES
!|  NUBO          |-->|  GLOBAL NUMBERS (INDEX) OF EDGE EXTREMITIES
!|  G             |-->|  GRAVITY CONSTANT
!|  W             |-->|  (H,HU,HV)
!|  ZF            |-->|  BATHYMETRIES
!|  VNOCL         |-->|  OUTWARD UNIT NORMAL (XNN,YNN, SEGMENT LENGTH)
!|  CE            |<--|  FLUX INCREMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FLUX_TCH => FLUX_TCH
      USE BIEF_DEF, ONLY:NCSIZE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,NSEG,NELEM
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NS),VNOCL(3,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: G,W(3,NS)
      DOUBLE PRECISION, INTENT(IN)    :: X(NS),Y(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3)
      INTEGER, INTENT(IN)             :: IFABOR(NELEM,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSG,NUBO1,NUBO2,IVAR,I,IDRY
      INTEGER IEL,IER
!
      DOUBLE PRECISION ZF1,ZF2,XNN,YNN,RNN
      DOUBLE PRECISION V21,V22,V31,V32,DEMI
      DOUBLE PRECISION H1,H2,EPS,FLXI(3),FLXJ(3)
      DOUBLE PRECISION ETA1,ETA2,PROD_SCAL
      LOGICAL,ALLOCATABLE ::   YESNO(:)
!
!-----------------------------------------------------------------------
!
      EPS=1.E-6
      DEMI = 0.5D0
      ALLOCATE(YESNO(NSEG),STAT=IER)
      IF(IER.NE.0)THEN
        IF(LNG.EQ.1)WRITE(LU,*)'FLUX_TCH: ERREUR D''ALLOCATION'
        IF(LNG.EQ.2)WRITE(LU,*)'FLUX_TCH: ALLOCATION ERROR '
        CALL PLANTE(1)
        STOP
      ENDIF
!
! INITIALIZATION OF CE
!
      DO I=1,3
        DO IVAR=1,NS
          CE(IVAR,I) = 0.D0
        ENDDO
      ENDDO
! INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
!
!-----------------------------------------------------------------------
!
!     LOOP OVER GLOBAL LIST OF EDGES
!
      DO IEL=1, NELEM
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!           INDICATOR FOR DRY CELLS
            IDRY=0
!           INITIALIZATION
            FLXI(1)=0.D0
            FLXI(2)=0.D0
            FLXI(3)=0.D0
            FLXJ(1)=0.D0
            FLXJ(2)=0.D0
            FLXJ(3)=0.D0
            PROD_SCAL=0.D0
!           RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!            WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
            PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &                  (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
            IF(PROD_SCAL.LT.0.D0)THEN
              NUBO1 = NUBO(2,NSG)
              NUBO2 = NUBO(1,NSG)
            ENDIF
!           THEIR BATHYMETRIES
            ZF1 = ZF(NUBO1)
            ZF2 = ZF(NUBO2)
!
!           NORMAL COORDINATES NX, NY AND SEGMENT LENGTH
!
            XNN = VNOCL(1,NSG)
            YNN = VNOCL(2,NSG)
            RNN = VNOCL(3,NSG)
!
!           WATER DEPTH
!
            H1 = W(1,NUBO1)
            H2 = W(1,NUBO2)
!
!           UNKNOWN SET (V1,V2,V3)=(eta,U,V) FOR EACH NODE
!
            ETA1 = W(1,NUBO1)+ZF1
            ETA2 = W(1,NUBO2)+ZF2
!
            IF(H1.GT.EPS) THEN
              V21 = W(2,NUBO1)/H1
              V31 = W(3,NUBO1)/H1
            ELSE
              V21=0.D0
              V31=0.D0
              IDRY=IDRY+1
            ENDIF
!
            IF(H2.GT.EPS)THEN
              V22 = W(2,NUBO2)/H2
              V32 = W(3,NUBO2)/H2
            ELSE
              V22=0.D0
              V32=0.D0
              IDRY=IDRY+1
            ENDIF
!
!           LOCAL FLUX COMPUTATION
!
!           AT LEAST ONE WET CELL
!
            IF(IDRY.LT.2)THEN
              CALL FLU_TCHAMEN(H1,H2,ETA1,ETA2,V21,V22,
     &                         V31,V32,XNN,YNN,FLXI,FLXJ,G)
!
!             FOR PARALLELISM
!
              IF(NCSIZE.GT.1)THEN
                IF(IFABOR(IEL,I).EQ.-2)THEN !THIS IS INTERFACE EDGE
                 ! DEMI=DEMI*SIGN(1.0D0,PROD_SCAL)
                  FLXI(1)= DEMI*FLXI(1)
                  FLXI(2)= DEMI*FLXI(2)
                  FLXI(3)= DEMI*FLXI(3)
                  FLXJ(1)= DEMI*FLXJ(1)
                  FLXJ(2)= DEMI*FLXJ(2)
                  FLXJ(3)= DEMI*FLXJ(3)
                ENDIF
              ENDIF
!
!             FLUX INCREMENT
!
              CE(NUBO1,1) = CE(NUBO1,1) - RNN*FLXI(1)
              CE(NUBO1,2) = CE(NUBO1,2) - RNN*FLXI(2)
              CE(NUBO1,3) = CE(NUBO1,3) - RNN*FLXI(3)
!
              CE(NUBO2,1) = CE(NUBO2,1) + RNN*FLXJ(1)
              CE(NUBO2,2) = CE(NUBO2,2) + RNN*FLXJ(2)
              CE(NUBO2,3) = CE(NUBO2,3) + RNN*FLXJ(3)
            ENDIF
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
!
      DEALLOCATE(YESNO)
!-----------------------------------------------------------------------
!
      RETURN
      END
