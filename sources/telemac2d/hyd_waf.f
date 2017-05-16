!                       *******************
                        SUBROUTINE HYD_WAF
!                       *******************

     &(NS,NSEG,NELEM,NUBO,G,W,ZF,VNOCL,DT,DTHAUT,
     & X,Y,CE,ELTSEG,NEISEG,HROPT)
!
!***********************************************************************
! TELEMAC 2D VERSION V6P3                                     15/01/2013
!***********************************************************************
!
!brief
!
!     FUNCTION  : COMPUTES ALL THE FLUXES FOR INTERNAL INTERFACES USING
!                 WAF FLUX.
!
!history  RIADH ATA (EDF R&D-LNHE)
!+        07/15/2012
!+        V6P2
!+
!
!history  RIADH ATA (EDF R&D-LNHE)
!+        01/15/2013
!+        V6P
!+ Adaptation to the new data structure (common with FE)
!+ FIRST STEPS FOR PARALLELIZATION
!
!history  R. ATA (EDF-LNHE)
!+        26/05/2016
!+        V7P2
!+      New option for hydrostatic reconstruction
!+      introduced by Noelle et al.
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! |  CE            |<-->|  FLUX  INCREMENTS AT INTERNAL FACES          |                          |
! |  DT            | -->|  TIME STEP                                   |
! |  ELTSEG        | -->|  SEGMENT NUMBERS PER ELEMENT                 |
! |  G             | -->|  GRAVITY                                     |
! |  HROPT         | -->|  OPTION FOR HYDROSTATIC RECONSTRUCTION:      |
! !                |    |   1: AUDUSSE, 2: NOELLE
! |  NELEM         | -->|  NUMBER OF TOTAL ELEMENTS                    |
! |  NS            | -->|  NUMBER OF TOTAL MESH NODES                  |
! |  NSEG          | -->|  NUMBER OF TOTAL MESH EDGES                  |
! |  NUBO          ! -->!  GLOBAL NUMBER OF EDGE EXTREMITIES           |
! |  NEISEG        | -->| LEFT & RIGHT NEIGHBOUR SEGMENTS OF A SEGMENT |
! !  VNOCL         | -->|  OUTWARD UNIT NORMALS                        |
! !                |    |   (2 FIRST COMPONENTS) AND                   |
! !                |    |   SEGMENT LENGTH  (THIRD COMPONENT)          |
! |  W             | -->|  W(1,IS) = H,  W(2,IS)=U  ,W(3,IS)=V         |
! |  X,Y           | -->|  X AND Y COORDINATES                         |
! |  ZF            | -->|  BATHYMETRIES                                |
! !________________|____|______________________________________________|
!  MODE: -->( UNCHANGEABLE INPUT ),<--(OUTPUT),<-->(CHANGEABLE INPUT)
!-----------------------------------------------------------------------
!     - CALLINF SUBROUTINE(S) : RESOLU
!
!***********************************************************************
!
      USE INTERFACE_TELEMAC2D, EX_HYD_WAF => HYD_WAF
      USE BIEF_DEF,ONLY: NCSIZE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,NSEG,NELEM,HROPT
      INTEGER, INTENT(IN) :: NUBO(2,NSEG),NEISEG(2,NSEG)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NS),VNOCL(3,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: X(NS),Y(NS)
      DOUBLE PRECISION, INTENT(IN)    :: G,W(3,NS)
      DOUBLE PRECISION, INTENT(IN)    :: DTHAUT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3),DT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER    :: NSG,NUBO1,NUBO2,IVAR,I,IDRY,IEL
      INTEGER    :: NUBOL,NUBOR,SEG1,SEG2,IER
!
      DOUBLE PRECISION :: H1,H2,EPS,FLX(4)
      DOUBLE PRECISION :: ZF1,ZF2,XNN,YNN,RNN
      DOUBLE PRECISION :: V21,V22,V31,V32,DX,SL1,SL2
      DOUBLE PRECISION :: HIJ,HJI,DSINT
      DOUBLE PRECISION :: HGZI,HGZJ,HDXZ1,HDYZ1,HDXZ2,HDYZ2
      DOUBLE PRECISION :: HL_UP,HR_UP
      DOUBLE PRECISION :: VL_UP,VR_UP
      DOUBLE PRECISION :: PROD_SCAL,DZIJ,DZJI,ZINT,DZINT
      DOUBLE PRECISION :: PSIL_UP,PSIR_UP
      LOGICAL, ALLOCATABLE  :: YESNO(:)
!  XI = X/T, AT THE (X,T) DIAGRAMM, WE COMPUETE THE FLUX AT XI = 0
      DOUBLE PRECISION,PARAMETER :: XI=0.0D0
! PSI1 AND PSI2 ARE TRACER
! NOT ADAPTED YET
      DOUBLE PRECISION           :: PSI1,PSI2
!
!**************************************************************
      EPS=1.E-6
! NO TRACER UNTIL NOW ...
      PSI1 = 0.0D0
      PSI2 = 0.0D0
      ALLOCATE(YESNO(NSEG),STAT=IER)
      IF(IER.NE.0)THEN
        IF(LNG.EQ.1)WRITE(LU,*)'FLUX_TCH: ERREUR D''ALLOCATION'
        IF(LNG.EQ.2)WRITE(LU,*)'FLUX_TCH: ALLOCATION ERROR '
        CALL PLANTE(1)
        STOP
      ENDIF
!**************************************************************
! INITIALIZATION OF CE
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
!     LOOP OVER GLOBAL LIST OF EDGES
!    *******************************
!
      DO IEL=1,NELEM
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!           INDICATOR FOR DRY CELLS
            IDRY=0
!           INITIALIZATION
            FLX(1) = 0.0D0
            FLX(2) = 0.0D0
            FLX(3) = 0.0D0
            FLX(4) = 0.0D0
!
!           RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!           WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
            PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &                  (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
            IF(PROD_SCAL.LT.0.D0)THEN
              NUBO1 = NUBO(2,NSG)
              NUBO2 = NUBO(1,NSG)
            ENDIF
!           THEIR BATHYMETRIES
            ZF1   =    ZF(NUBO1)
            ZF2   =    ZF(NUBO2)
!           MEAN DISTANCE BETWEEN THEM (FOR CFL)
            DX    = 0.5D0*(DTHAUT(NUBO1)+DTHAUT(NUBO2))
!           NORMAL COORDINATES NX, NY AND SEGMENT LENGTH
            XNN       = VNOCL(1,NSG)
            YNN       = VNOCL(2,NSG)
            RNN       = VNOCL(3,NSG)
!
!           WATER DEPTH
!
            H1=W(1,NUBO1)
            H2=W(1,NUBO2)
!******************************************************
!           HYDROSTATIC RECONSTRUCTION
!           HYDROSTATIC RECONSTRUCTION OF AUDUSSE
            IF(HROPT.EQ.1)THEN
              DZIJ = MAX(0.D0,ZF2-ZF1)
              HIJ  = MAX(0.D0,H1- DZIJ)
!
              DZJI = MAX(0.D0,ZF1-ZF2)
              HJI  = MAX(0.D0,H2- DZJI)
            ELSEIF(HROPT.EQ.2)THEN
!           HYDROSTATIC RECONSTRUCTION OF NOELLE ET AL.
              SL1   = H1+ZF1
              SL2   = H2+ZF2
              DZINT = MAX(ZF2,ZF1)
              DSINT = MIN(SL2,SL1)
              ZINT  = MIN(DZINT,DSINT)
!
              HIJ = MIN(SL1-ZINT,H1)
              HJI = MIN(SL2-ZINT,H2)
            ELSE
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'HYD_HLLC: OPTION DE RECONSTRUCTION'
                WRITE(LU,*) '          HYDROSTATIQUE NON IMPLEMENTEE'
              ELSE
                WRITE(LU,*) 'HYD_HLLC: OPTION OF HYDROSTATIC RECONS-'
                WRITE(LU,*) '          TRUCTION NOT IMPLEMENTED YET'
              ENDIF
              CALL PLANTE(1)
              STOP
            ENDIF
!******************************************************
!
!           VELOCITY COMPONENTS
!
            IF(H1.GT.EPS)THEN
              V21 = W(2,NUBO1)/H1
              V31 = W(3,NUBO1)/H1
            ELSE
              V21=0.0D0
              V31=0.0D0
              IDRY=IDRY+1
            ENDIF
!
            IF(H2.GT.EPS)THEN
              V22 = W(2,NUBO2)/H2
              V32 = W(3,NUBO2)/H2
            ELSE
              V22=0.0D0
              V32=0.0D0
              IDRY=IDRY+1
            ENDIF
!
!           SEGMENT NEIGHBORS (FOR LIMITER)
!
            SEG1 = NEISEG(1,NSG)
            SEG2 = NEISEG(2,NSG)
!             VERIFY THAT WE HAVE THE GOOD NEIGHBORS
            IF((SEG1.LE.0.OR.SEG2.LE.0).AND.NCSIZE.LE.1)THEN
              WRITE(LU,*)'PROBLEM TO FIND SEGMENT NEIGHBORS'
              WRITE(LU,*)'WE ARE IN HYD_WAF.F'
              WRITE(LU,*)'SEGMENT OF INTEREST  :',NSG
              WRITE(LU,*)'NEIGHBORS ARE  :',SEG1,SEG2
              CALL PLANTE(1)
              STOP
            ENDIF
!
            NUBOL=0
            NUBOR=0
!
            IF(NUBO(1,SEG1).EQ.NUBO1) THEN
              NUBOL = NUBO(2,SEG1)
            ELSE
              NUBOL = NUBO(1,SEG1)
            ENDIF
            IF(NUBO(1,SEG2).EQ.NUBO2) THEN
              NUBOR = NUBO(2,SEG2)
            ELSE
              NUBOR = NUBO(1,SEG2)
            ENDIF
!             VERIFY THAT WE HAVE THE GOOD NEIGHBORS
            IF(NUBOL.LE.0.OR.NUBOR.LE.0) THEN
              WRITE(LU,*)'PROBLEM TO FIND NEIGHBOR'
              WRITE(LU,*)'WE ARE IN HYD_WAF.F'
              WRITE(LU,*)'NODES ARE  :',NUBO1,NUBO2
              WRITE(LU,*)'NEIGHBORS ARE  :', NUBOR,NUBOL
              CALL PLANTE(1)
              STOP
            ENDIF
!
!           WATER DEPTH, VELOCITY, TRACER OF NEIGHBORS
!
            HL_UP   = W(1,NUBOL)
            HR_UP   = W(1,NUBOR)
            IF(HL_UP.GT.EPS)THEN
              VL_UP =  W(3,NUBOL)/HL_UP
            ELSE
              VL_UP = 0.0D0
            ENDIF
            IF(HR_UP.GT.EPS)THEN
              VR_UP = W(3,NUBOR)/HR_UP
            ELSE
              VR_UP = 0.0D0
            ENDIF
            PSIL_UP = PSI1
            PSIR_UP = PSI2
!
!           LOCAL FLUX COMPUTATION
!
!           AT LEAST ONE WET CELL
            IF(IDRY.LT.2)THEN
!           CALL FLUX_WAF(XI,H1,H2,V21,V22,V31,V32,PSI1,PSI2,
              CALL FLUX_WAF(XI,HIJ,HJI,V21,V22,V31,V32,PSI1,PSI2,
     &                      HL_UP,HR_UP,VL_UP,VR_UP,PSIL_UP,PSIR_UP,
     &                      XNN,YNN,DT,DX,FLX)
!
!
!*********************************************************
!       GEOMETRIC SOURCE TERMS:HYDROSTATIC RECONSTRUCTION
!*********************************************************
!
!             HYDROSTATIC RECONSTRUCTION OF AUDUSSE
              IF(HROPT.EQ.1)THEN
                HGZI = 0.5D0*RNN*(HIJ+H1)*(HIJ-H1)
                HGZJ = 0.5D0*RNN*(HJI+H2)*(HJI-H2)
!             HYDROSTATIC RECONSTRUCTION OF NOELLE ET AL.
              ELSEIF(HROPT.EQ.2)THEN
                HGZI = -0.5D0*RNN*(HIJ+H1)*(ZINT-ZF1)
                HGZJ =  0.5D0*RNN*(HJI+H2)*(ZF2-ZINT)
              ENDIF
!
              HDXZ1 = G*XNN*HGZI
              HDYZ1 = G*YNN*HGZI
!
              HDXZ2 = G*XNN*HGZJ
              HDYZ2 = G*YNN*HGZJ
!
!             FLUX INCREMENT
!
              CE(NUBO1,1) = CE(NUBO1,1) - RNN*FLX(1)
              CE(NUBO1,2) = CE(NUBO1,2) - RNN*FLX(2) + HDXZ1
              CE(NUBO1,3) = CE(NUBO1,3) - RNN*FLX(3) + HDYZ1
!
              CE(NUBO2,1) = CE(NUBO2,1) + RNN*FLX(1)
              CE(NUBO2,2) = CE(NUBO2,2) + RNN*FLX(2) - HDXZ2
              CE(NUBO2,3) = CE(NUBO2,3) + RNN*FLX(3) - HDYZ2
            ENDIF
!
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
!
      DEALLOCATE(YESNO)
!-----------------------------------------------------------------------
      RETURN
      END
