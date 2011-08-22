!                    *******************
                     SUBROUTINE FLUX_TCH
!                    *******************
!
     &(NS,NSEG,NUBO,G,X,Y,W,ZF,VNOCL,CE,AIRS)
!
!***********************************************************************
! TELEMAC-2D VERSION 6.1                                     03/15/2011
!***********************************************************************
!
!brief  COMPUTES TCHAMEN FLUX AT THE INERNAL INTERFACES 
!       REF.:"MODELING OF WETTING-DRYING TRANSITIONS IN FREE SURFACE FLOWS 
!             OVER COMPLEX TOPOGRAPHIES" CMAME 199(2010) PP 2281-2304 
!
!history  R. ATA (EDF-LNHE)
!+
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AIRS           |-->| AIRES DES CELLULES
!| CE             |<->| FLUX  INCREMENTES DES FLUX
!|                |   | AUX INTERFACES INTERNES
!| G              |-->| CONSTANTE DE GRAVITE
!| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
!| NSEG           |-->| NOMBRE D'ARETES DU MAILLAGE
!| NUBO           |-->| NUMEROS GLOBAUX DES EXTREMITES DES ARETES
!| VNOCL          |-->| NORMALE A l'INTERFACE
!|                |   | (2 PREMIERES COMPOSANTES) ET
!|                |   | LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
!| W              |-->| W(1,IS) = H,  W(2,IS)=U  ,W(3,IS)=V
!| X,Y            |-->| COORDONNEES DES NOEUDS DU MAILLAGE
!| ZF             |-->| COTES DU FOND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
!***********************************************************************
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      INTEGER NSG,NUBO1,NUBO2,J,IVAR,IS,K,IDRY
!
      DOUBLE PRECISION VNX,VNY,VNL,ZF1,ZF2,XNN,YNN,RNN
      DOUBLE PRECISION V21,V22,V31,V32
      DOUBLE PRECISION HI,HI0,HIJ,CIJ,V210,V220
      DOUBLE PRECISION HJ,HJ0,HJI,CJI,DZIJ,DZJI
      DOUBLE PRECISION FLU11,FLUIJ_2,FLUIJ_3,FLU12,FLU22
      DOUBLE PRECISION FLUIJ_20
      DOUBLE PRECISION SIGMAX,UNORM
!
      DOUBLE PRECISION FLUIJ_1,H1,H2,EPS,FLX(3)
      DOUBLE PRECISION ETA1,ETA2,U_IJ,D_IJ,C_IJ,C_I,C_J
!-----------------------------------------------------------------------
!**************************************************************
! VOIR SON EFFET
         EPS=1.E-6
!**************************************************************
! INITIALISATION DE CE
       DO IS=1,3
         DO IVAR=1,NS
           CE(IVAR,IS) = 0.D0
         ENDDO
       ENDDO
!
!
!-----------------------------------------------------------------------
!     LOOP ON GLOBAL LIST OF EDGES
!    ******************************
!
      DO 5500 NSG=1,NSEG
! INDICATOR FOR DRY CELLS
         IDRY=0
! INITIALIZATION
         FLX(1)=0.0D0
         FLX(2)=0.0D0
         FLX(3)=0.0D0
! RECUPERATE NODES OF THE EDGE
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
! THEIR BATHYMETRIES
         ZF1   =    ZF(NUBO1)
         ZF2   =    ZF(NUBO2)
! NORMAL COORDINATES NX, NY AND SEGMENT LENGTH
         XNN       = VNOCL(1,NSG)
         YNN       = VNOCL(2,NSG)
         RNN       = VNOCL(3,NSG)
!
! WATER DEPTH
!
         H1=W(1,NUBO1)
         H2=W(1,NUBO2)
!
! UNKNOWN SET (V1,V2,V3)=(eta,U,V) FOR EACH NODE
!
         ETA1     = W(1,NUBO1)+ZF1
         ETA2     = W(1,NUBO2)+ZF2
         IF(H1.GT.EPS)THEN
            V21 = W(2,NUBO1)/H1
            V31 = W(3,NUBO1)/H1
         ELSE
            V21=0.0D0
            V31=0.0D0
            IDRY=IDRY+1
         ENDIF
         IF(H2.GT.EPS)THEN
            V22 = W(2,NUBO2)/H2
            V32 = W(3,NUBO2)/H2
         ELSE
            V22=0.0D0
            V32=0.0D0
            IDRY=IDRY+1
         ENDIF
!
! LOCAL FLUX COMPUTATION
!
!       AT LEAST ONE WET CELL
         IF(IDRY.LT.2)THEN
            CALL FLU_TCHAMEN(H1,H2,ETA1,ETA2,V21,V22,
     &                     V31,V32,XNN,YNN,FLX,G,EPS)
         ELSE
            FLX(1)=0.0D0
            FLX(2)=0.0D0
            FLX(3)=0.0D0
         ENDIF
!
! FLUX INCREMENT
!
         CE(NUBO1,1) = CE(NUBO1,1) - RNN*FLX(1)
         CE(NUBO1,2) = CE(NUBO1,2) - RNN*FLX(2)
         CE(NUBO1,3) = CE(NUBO1,3) - RNN*FLX(3)
!
         CE(NUBO2,1) = CE(NUBO2,1) + RNN*FLX(1)
         CE(NUBO2,2) = CE(NUBO2,2) + RNN*FLX(2)
         CE(NUBO2,3) = CE(NUBO2,3) + RNN*FLX(3)
!
5500   CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
