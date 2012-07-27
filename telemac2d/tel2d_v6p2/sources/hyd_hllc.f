!                       *******************
                        SUBROUTINE HYD_HLLC
!                       *******************

     &(NS,NSEG,NUBO,G,X,Y,W,ZF,VNOCL,CE,AIRS)
!
!***********************************************************************
! TELEMAC 2D VERSION 6.2                                         R. ATA
!
!***********************************************************************
!
!brief 
! 
!     FUNCTION  : COMPUTE ALL THE FLUXES FOR INTERNAL INTERFACES USING
!                 HLLC FLUX.
!
!history  RIADH ATA (EDF R&D-LNHE)
!+        07/15/2012
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! |  NS            | -->|  NUMBER OF TOTAL MESH NODES                  |
! |  NSEG          | -->|  NUMBER OF TOTAL MESH EDGES                  |
! !  NUBO          ! -->!  GLOBAL NUMBER OF EDGE EXTREMITIES           |
! |  G             | -->|  GRAVITY                                     |
! |  X,Y           | -->|  COORDINATES OF NODES                        |
! |  W             | -->|  W(1,IS) = H,  W(2,IS)=U  ,W(3,IS)=V         |
! |  ZF            | -->|  BATHYMETRIES                                |
! |  VNOCL         | -->|  OUTWARD UNIT NORMALS                        |
! |                |    |   (2 FIRST COMPONENTS) AND                   |
! |                |    |   SEGMENT LENGTH  (THIRD COMPONENT)          |
! |  CE            |<-->|  FLUX  INCREMENTS AT INTERNAL FACES          |
! |                |    |                                              |
! |  AIRS          | -->|  AREA OF CELLS                               |
! !________________|____|______________________________________________!
!  MODE: -->( UNCHANGEABLE INPUT ),<--(OUTPUT),<-->(CHANGEABLE INPUT)   
!-----------------------------------------------------------------------
!     - CALLING SUBROUTINE(S) : RESOLU                             
!
!***********************************************************************
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!     
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,NSEG
      INTEGER, INTENT(IN) :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: X(NS),Y(NS)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NS),VNOCL(3,NSEG),AIRS(*)
      DOUBLE PRECISION, INTENT(IN)    :: G,W(3,NS)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSG,NUBO1,NUBO2,J,IVAR,IS,K,IDRY   
!
      DOUBLE PRECISION VNX,VNY,VNL,ZF1,ZF2,XNN,YNN,RNN
      DOUBLE PRECISION V21,V22,V31,V32,U10
      DOUBLE PRECISION HI,HI0,HIJ,CIJ,V210,V220
      DOUBLE PRECISION HJ,HJ0,HJI,CJI,DZIJ,DZJI,Z_STAR_IJ
      DOUBLE PRECISION HGZI,HGZJ,HDXZ1,HDYZ1,HDXZ2,HDYZ2
      DOUBLE PRECISION ZR,ETA1,ETA2
!
      DOUBLE PRECISION H1,H2,EPS,FLX(4)
      LOGICAL          ROT
!     XI = X/T, AT THE (X,T) DIAGRAM, WE COMPUTE THE FLUX AT XI = 0
      DOUBLE PRECISION,PARAMETER :: XI=0.D0
!     PSI1 AND PSI2 ARE TRACER DENSITIES, MAKE ADAPTATION FOR TELEMAC
!     NOT ADAPTED YET
      DOUBLE PRECISION           :: PSI1,PSI2
!
!-----------------------------------------------------------------------
!
      EPS=1.E-6
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
!
!-----------------------------------------------------------------------
!
!     LOOP OVER GLOBAL LIST OF EDGES
!
      DO NSG=1,NSEG 
!       INDICATOR FOR DRY CELLS
        IDRY=0
!       INITIALIZATION
        FLX(1) = 0.D0
        FLX(2) = 0.D0
        FLX(3) = 0.D0
        FLX(4) = 0.D0
!       RETRIEVE NODES OF THE EDGE 
        NUBO1 = NUBO(1,NSG)
        NUBO2 = NUBO(2,NSG)
!       THEIR BATHYMETRIES
        ZF1 = ZF(NUBO1)
        ZF2 = ZF(NUBO2)
!       NORMAL COORDINATES NX, NY AND SEGMENT LENGTH
        XNN = VNOCL(1,NSG)
        YNN = VNOCL(2,NSG)
        RNN = VNOCL(3,NSG) 
!
! WATER DEPTH
!
        H1=W(1,NUBO1)
        H2=W(1,NUBO2)
!
!*****************************************************
!    HYDROSTATIC RECONSTRUCTION
!
        DZIJ = MAX(0.D0,ZF2-ZF1)
        HIJ  = MAX(0.D0,H1- DZIJ)
!*****************************************************
!    HYDROSTATIC RECONSTRUCTION
!
        DZJI = MAX(0.D0,ZF1-ZF2)
        HJI  = MAX(0.D0,H2- DZJI)
!*****************************************************
!
! VELOCITY COMPONENTS 
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
          CALL FLUX_HLLC(XI,HIJ,HJI,V21,V22,V31,V32,PSI1,PSI2,
     &                   XNN,YNN,ROT,FLX)
        ENDIF
!
!*********************************************************
!       GEOMETRIC SOURCE TERMS:HYDROSTATIC RECONSTRUCTION
!*********************************************************
!
        HGZI =0.5D0*RNN*(HIJ+H1)*(HIJ-H1)
        HGZJ =0.5D0*RNN*(HJI+H2)*(HJI-H2)
!
        HDXZ1  = G*XNN*HGZI
        HDYZ1  = G*YNN*HGZI
! 
        HDXZ2  = G*XNN*HGZJ
        HDYZ2  = G*YNN*HGZJ
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
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
