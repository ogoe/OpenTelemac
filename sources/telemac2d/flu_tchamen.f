!                       **********************
                        SUBROUTINE FLU_TCHAMEN
!                       **********************
!
     &(H1,H2,ETA1,ETA2,U1,U2,V1,V2,XNN,YNN,FLXI,FLXJ,G)
!
!***********************************************************************
! TELEMAC 2D VERSION 6.2                                     03/15/2011
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
!|  H1,H2         |-->|  LEFT AND RIGHT WATER DEPTHS
!|  ETA1,ETA2     |-->|  LEFT AND RIGHT FREE SURFACES
!|  U1,U2         |-->|  LEFT AND RIGHT VELOCITY X-COMPONENTS
!|  V1,V2         |-->|  LEFT AND RIGHT VELOCITY Y-COMPONENTS
!|  XNN,YNN       |-->|  X AND Y COMPONENTS OF THE OUTWARD UNIT NORMAL
!|  FLXI,FLXJ     |<--|  RIGHT AND LEFT CONTRIBUTIONS TO THE FLUX
!|  G             |-->|  GRAVITY CONSTANT
!|  EPS           |-->|  TOLERANCE FOR WATER DEPTH DIVISION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: G,H1,H2,ETA1,ETA2,U1,U2
      DOUBLE PRECISION, INTENT(IN)    :: V1,V2,XNN,YNN
      DOUBLE PRECISION, INTENT(INOUT) :: FLXI(3),FLXJ(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IVAR
!
      DOUBLE PRECISION FLUIJ_20
!
      INTEGER CHOICE_D
      DOUBLE PRECISION GSUR2,DIJS2
      DOUBLE PRECISION ALPHA,FLUIJ_1
      DOUBLE PRECISION UI,UJ,VI,VJ
      DOUBLE PRECISION U_IJ,D_IJ,C_IJ,C_I,C_J,UI0,UJ0
      DOUBLE PRECISION FLUIJ_2I,FLUIJ_2J
      DOUBLE PRECISION FLUIJ_3,FLUIJ_3I,FLUIJ_3J
!
!-----------------------------------------------------------------------
!
      ALPHA=1.D0
      CHOICE_D=2
      GSUR2=G/2.D0
!
!-----------------------------------------------------------------------
!
!     INITIALIZATION OF FLXI AND FLXJ
!
      DO IVAR=1,3
        FLXI(IVAR) = 0.D0
        FLXJ(IVAR) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     VELOCITIES
!
      UI=U1
      VI=V1
      UJ=U2
      VJ=V2
!
! ROTATION
!
      UI0 = UI
      UI  = XNN*UI0+YNN*VI
      VI  =-YNN*UI0+XNN*VI
!
      UJ0 = UJ
      UJ  = XNN*UJ0+YNN*VJ
      VJ  =-YNN*UJ0+XNN*VJ
!
! WET/DRY TREATMENT
!
!     CALL WETDRY(ETA1,ZF1,H1,UI,VI,ETA2,ZF2,H2,UJ,VJ,EPS)
!
!
!     LET'S COMPUTE D_IJ
!
      IF(CHOICE_D.EQ.1) THEN
!
!       ZOKAGOA'S CHOICE
!
        U_IJ=0.5D0*(UI+UJ)
        C_IJ=SQRT(GSUR2*(H1+H2))
        D_IJ=ALPHA*MAX(ABS(U_IJ-C_IJ),MAX(ABS(U_IJ),ABS(U_IJ+C_IJ)))
!
      ELSEIF(CHOICE_D.EQ.2) THEN
!
!       TORO'S CHOICE
!
        C_I=SQRT(G*H1)
        C_J=SQRT(G*H2)
        D_IJ=MAX(ABS(UI)+C_I,ABS(UJ)+C_J)
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,4010) CHOICE_D
        IF(LNG.EQ.2) WRITE(LU,4020) CHOICE_D
4010    FORMAT(1X,'FLU_TCH : ERREUR DANS LE CHOIX DE L''UPWIND : ',1I6)
4020    FORMAT(1X,'FLU_TCH: ERROR IN THE UPWIND CHOICE: ',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
! CENTERED FLUX COMPUTATION
!
! TCHAMEN FLUX
!
      FLUIJ_1=0.5D0*(H1*UI+H2*UJ)
      FLUIJ_2I=0.5D0*(H1*(UI*UI)+H2*(UJ*UJ)+G*H1*(ETA1+ETA2))
      FLUIJ_2J=FLUIJ_2I+GSUR2*(ETA1+ETA2)*(H2-H1)
      FLUIJ_3=0.5D0*(H1*UI*VI+H2*UJ*VJ)
!
! UPWINDING
!
      DIJS2=0.5D0*D_IJ
      FLUIJ_1=FLUIJ_1 -    DIJS2*(ETA2-ETA1)
      FLUIJ_2I = FLUIJ_2I- DIJS2*(H2*UJ-H1*UI)
      FLUIJ_2J = FLUIJ_2J- DIJS2*(H2*UJ-H1*UI)
      FLUIJ_3=FLUIJ_3 -    DIJS2*(H2*VJ-H1*VI)
!
! INVERSE ROTATION
!
      FLUIJ_20  = FLUIJ_2I
      FLUIJ_3I  = FLUIJ_3
      FLUIJ_2I  = XNN*FLUIJ_20-YNN*FLUIJ_3I
      FLUIJ_3I  = YNN*FLUIJ_20+XNN*FLUIJ_3I
!
      FLUIJ_20  = FLUIJ_2J
      FLUIJ_3J  = FLUIJ_3
      FLUIJ_2J  = XNN*FLUIJ_20-YNN*FLUIJ_3J
      FLUIJ_3J  = YNN*FLUIJ_20+XNN*FLUIJ_3J
!
! FINAL FLUX
!
      FLXI(1) =  FLUIJ_1
      FLXI(2) =  FLUIJ_2I
      FLXI(3) =  FLUIJ_3I
!
      FLXJ(1) =  FLUIJ_1
      FLXJ(2) =  FLUIJ_2J
      FLXJ(3) =  FLUIJ_3J
!
!-----------------------------------------------------------------------
!
      RETURN
      END
