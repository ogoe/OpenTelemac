!                       *******************
                        SUBROUTINE HYD_HLLC
!                       *******************

     &(NS,NELEM,NSEG,NUBO,G,W,ZF,VNOCL,X,Y,ELTSEG,CE,IFABOR,HROPT)
!
!***********************************************************************
! TELEMAC 2D                                                 VERSION 7.2
!***********************************************************************
!
!brief
!+     FUNCTION  : COMPUTE ALL THE FLUXES FOR INTERNAL INTERFACES USING
!+                 HLLC FLUX.
!+
!history  RIADH ATA (EDF R&D-LNHE)
!+        07/15/2012
!+        V6P2
!+
!+
!history  R. ATA (EDF-LNHE)
!+        01/07/2013
!+        V6P3
!+      adaptation with the new data structure (common with FEM)
!+      PARALLELIZATION
!+
!history  R. ATA (EDF-LNHE)
!+        18/01/2015
!+        V7P0
!+      optimization (change place of Hydro Reconst)
!+
!history  R. ATA (EDF-LNHE)
!+        18/05/2016
!+        V7P2
!+      New option for hydrostatic reconstruction
!+      introduced by Noelle et al.
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! |  CE            |<-->|  FLUX  INCREMENTS AT INTERNAL FACES          |                          |
! |  ELTSEG        | -->|  SEGMENT NUMBERS PER ELEMENT                 |
! |  G             | -->|  GRAVITY                                     |
! |  HROPT         | -->|  OPTION FOR HYDROSTATIC RECONSTRUCTION:      |
! !                |    |   1: AUDUSSE, 2: NOELLE                      |
! |  NELEM         | -->|  NUMBER OF TOTAL ELEMENTS                    |
! |  NS            | -->|  NUMBER OF TOTAL MESH NODES                  |
! |  NSEG          | -->|  NUMBER OF TOTAL MESH EDGES                  |
! |  NUBO          ! -->!  GLOBAL NUMBER OF EDGE EXTREMITIES           |
! !  VNOCL         | -->|  OUTWARD UNIT NORMALS                        |
! !                |    |   (2 FIRST COMPONENTS) AND                   |
! !                |    |   SEGMENT LENGTH  (THIRD COMPONENT)          |
! |  W             | -->|  W(1,IS) = H,  W(2,IS)=U  ,W(3,IS)=V         |
! |  X,Y           | -->|  X AND Y COORDINATES                         |
! |  ZF            | -->|  BATHYMETRIES                                |
! !________________|____|______________________________________________|
!
!  MODE: -->( UNCHANGEABLE INPUT ),<--(OUTPUT),<-->(CHANGEABLE INPUT)
!-----------------------------------------------------------------------
!     - CALLING SUBROUTINE(S) : RESOLU
!
!***********************************************************************
!
      USE BIEF_DEF
      USE INTERFACE_TELEMAC2D, EX_HYD_HLLC => HYD_HLLC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,NSEG,NELEM,HROPT
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NS),VNOCL(3,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: X(NS),Y(NS)
      DOUBLE PRECISION, INTENT(IN)    :: G,W(3,NS)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3)
      INTEGER, INTENT(IN)             :: IFABOR(NELEM,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSG,NUBO1,NUBO2,IVAR,IS,IDRY,I,IEL
!
      DOUBLE PRECISION ZF1,ZF2,XNN,YNN,RNN
      DOUBLE PRECISION V21,V22,V31,V32,SL1,SL2
      DOUBLE PRECISION HIJ,PROD_SCAL,DSJI,DSINT
      DOUBLE PRECISION HJI,DZIJ,DZJI,DSIJ,ZINT,DZINT
      DOUBLE PRECISION HGZI,HGZJ,HDXZ1,HDYZ1,HDXZ2,HDYZ2
!
      DOUBLE PRECISION H1,H2,EPS,FLX(4),DEMI
      LOGICAL          ROT
!     XI = X/T, AT THE (X,T) DIAGRAM, WE COMPUTE THE FLUX AT XI = 0
      DOUBLE PRECISION,PARAMETER :: XI=0.D0
!     PSI1 AND PSI2 ARE TRACER DENSITIES, MAKE ADAPTATION FOR TELEMAC
!     NOT ADAPTED YET
      DOUBLE PRECISION           :: PSI1,PSI2
      LOGICAL   YESNO(NSEG)
!
!-----------------------------------------------------------------------
!
      EPS=1.E-6
      DEMI=0.5D0
      ROT =.TRUE.
!     NO TRACER UNTIL NOW ...
      PSI1 = 0.D0
      PSI2 = 0.D0
!
!-----------------------------------------------------------------------
!
! INITIALIZATION OF CE
!
      DO IS=1,3
        DO IVAR=1,NS
          CE(IVAR,IS) = 0.D0
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
      DO IEL=1,NELEM
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!    INDICATOR FOR DRY CELLS
            IDRY=0
!    INITIALIZATION
            FLX(1) = 0.D0
            FLX(2) = 0.D0
            FLX(3) = 0.D0
            FLX(4) = 0.D0
!    RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!     WITH RESPECT TO THE NORMAL
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
!    NORMAL COORDINATES NX, NY AND SEGMENT LENGTH
            XNN = VNOCL(1,NSG)
            YNN = VNOCL(2,NSG)
            RNN = VNOCL(3,NSG)
!    WATER DEPTH
            H1=W(1,NUBO1)
            H2=W(1,NUBO2)
!
!    VELOCITY COMPONENTS
!
            IF(H1.GT.EPS)THEN
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
              V22=0.0D0
              V32=0.0D0
              IDRY=IDRY+1
            ENDIF
!
!       LOCAL FLUX COMPUTATION
!       AT LEAST ONE WET CELL
!
            IF(IDRY.LT.2)THEN
!
!             HYDROSTATIC RECONSTRUCTION OF AUDUSSE
              IF(HROPT.EQ.1)THEN
                DZIJ = MAX(0.D0,ZF2-ZF1 )
                HIJ  = MAX(0.D0,H1 -DZIJ)
!
                DZJI = MAX(0.D0,ZF1-ZF2 )
                HJI  = MAX(0.D0,H2 -DZJI)
              ELSEIF(HROPT.EQ.2)THEN
!             HYDROSTATIC RECONSTRUCTION OF NOELLE ET AL.
                SL1   = H1+ZF1
                SL2   = H2+ZF2
                DZINT = MAX(ZF2,ZF1)
                DSINT = MIN(SL2,SL1)
                ZINT  = MIN(DZINT,DSINT)
!
                HIJ   = MIN(SL1-ZINT,H1)
                HJI   = MIN(SL2-ZINT,H2)
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
!*****************************************************
              CALL FLUX_HLLC(XI,HIJ,HJI,V21,V22,V31,V32,
     &                       PSI1,PSI2,XNN,YNN,ROT,FLX)
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
!FOR PARALLELISM
!
              IF(NCSIZE.GT.1)THEN
                IF(IFABOR(IEL,I).EQ.-2)THEN !THIS IS INTERFACE EDGE
!               DEMI=DEMI*SIGN(1.0D0,PROD_SCAL)
                  FLX(1)= DEMI*FLX(1)
                  FLX(2)= DEMI*FLX(2)
                  FLX(3)= DEMI*FLX(3)
!                 FLX(4)= DEMI*FLX(4)
                  HDXZ1 = DEMI*HDXZ1
                  HDYZ1 = DEMI*HDYZ1
                  HDXZ2 = DEMI*HDXZ2
                  HDYZ2 = DEMI*HDYZ2
                ENDIF
              ENDIF
!
!***********************************************************
!
!       FLUX INCREMENT
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
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
