!                      *******************
                        SUBROUTINE FLUXZZ
!                      *******************

     &(X,Y,NS,NSEG,NELEM,NUBO,G,W,ZF,VNOCL,
     & ELTSEG,CE,IFABOR)
!
!***********************************************************************
! TELEMAC-2D VERSION 6.3                                     01/07/2013
!***********************************************************************
!
!brief  COMPUTES ZOKAGOA/TCHAMEN FLUX AT THE INERNAL INTERFACES
!       REF.:"MODELING OF WETTING-DRYING TRANSITIONS IN FREE SURFACE FLOWS
!             OVER COMPLEX TOPOGRAPHIES" CMAME 199(2010) PP 2281-2304
!
!history  R. ATA (EDF-LNHE)
!+
!+        V6P1
!+
!history  R. ATA (EDF-LNHE)   01/07/2013
!+
!+       V6P3
!+ adaptation with the new data structure (common with FEM)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! |  CE            |<-->|  FLUX  INCREMENTS AT INTERNAL FACES          |                          |
! |  ELTSEG        | -->|  SEGMENT NUMBERS PER ELEMENT                 |
! |  G             | -->|  GRAVITY                                     |
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
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_FLUXZZ => FLUXZZ
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
      INTEGER IELEM,IER
!
      DOUBLE PRECISION ZF1,ZF2,XNN,YNN,RNN
      DOUBLE PRECISION V21,V22,V31,V32,PROD_SCAL
      DOUBLE PRECISION H1,H2,EPS,FLXI(3),FLXJ(3)
      DOUBLE PRECISION ETA1,ETA2
      DOUBLE PRECISION DEMI
      LOGICAL, ALLOCATABLE :: YESNO(:)
!-----------------------------------------------------------------------
!**************************************************************
      EPS=1.E-3
      DEMI = 0.5D0
      ALLOCATE(YESNO(NSEG),STAT=IER)
      IF(IER.NE.0)THEN
        IF(LNG.EQ.1)WRITE(LU,*)'FLUXZZ: ERREUR D''ALLOCATION'
        IF(LNG.EQ.2)WRITE(LU,*)'FLUXZZ: ALLOCATION ERROR '
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
!
!-----------------------------------------------------------------------
!     LOOP OVER GLOBAL LIST OF EDGES
!    ********************************
!
      DO IELEM=1, NELEM
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IELEM,I)))THEN
            NSG = ELTSEG(IELEM,I)
! INDICATOR FOR DRY CELLS
            IDRY= 0
! INITIALIZATION
            FLXI(1)= 0.0D0
            FLXI(2)= 0.0D0
            FLXI(3)= 0.0D0
            FLXJ(1)= 0.0D0
            FLXJ(2)= 0.0D0
            FLXJ(3)= 0.0D0
! RECUPERATE NODES OF THE EDGE WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
            PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &                  (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
            IF(PROD_SCAL.LT.0.D0)THEN
              NUBO1 = NUBO(2,NSG)
              NUBO2 = NUBO(1,NSG)
            ENDIF
!
! THEIR BATHYMETRIES
!
            ZF1   =  ZF(NUBO1)
            ZF2   =  ZF(NUBO2)
!
! NORMAL COORDINATES NX, NY AND SEGMENT LENGTH
!
            XNN   = VNOCL(1,NSG)
            YNN   = VNOCL(2,NSG)
            RNN   = VNOCL(3,NSG)
!
! WATER DEPTH
!
            H1    = W(1,NUBO1)
            H2    = W(1,NUBO2)
!
! UNKNOWN SET (V1,V2,V3)=(eta,U,V) FOR EACH NODE
!
            ETA1  = W(1,NUBO1)+ZF1
            ETA2  = W(1,NUBO2)+ZF2

            IF(H1.GT.EPS)THEN
              V21 = W(2,NUBO1)/H1
              V31 = W(3,NUBO1)/H1
            ELSE
              V21 = 0.0D0
              V31 = 0.0D0
              IDRY= IDRY+1
            ENDIF

            IF(H2.GT.EPS)THEN
              V22 = W(2,NUBO2)/H2
              V32 = W(3,NUBO2)/H2
            ELSE
              V22 = 0.0D0
              V32 = 0.0D0
              IDRY= IDRY+1
            ENDIF
!
! LOCAL FLUX COMPUTATION
!
!        AT LEAST ONE WET CELL
            IF(IDRY.LT.2)THEN
              CALL FLU_ZOKAGOA(H1,H2,ZF1,ZF2,ETA1,ETA2,V21,V22,
     &                         V31,V32,XNN,YNN,FLXI,FLXJ,G)
!
!FOR PARALLELISM
!
              IF(NCSIZE.GT.1)THEN
                IF(IFABOR(IELEM,I).EQ.-2)THEN !THIS IS INTERFACE EDGE
                  FLXI(1)= DEMI*FLXI(1)
                  FLXI(2)= DEMI*FLXI(2)
                  FLXI(3)= DEMI*FLXI(3)
                  FLXJ(1)= DEMI*FLXJ(1)
                  FLXJ(2)= DEMI*FLXJ(2)
                  FLXJ(3)= DEMI*FLXJ(3)
                ENDIF
              ENDIF
!
! FLUX INCREMENT
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
!
      ENDDO
!
      DEALLOCATE(YESNO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
