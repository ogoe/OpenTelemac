!                        ********************
                         SUBROUTINE FLUX_HLLC
!                        ********************
     &(XI,H1,H2,U1,U2,V1,V2,PSI1,PSI2,
     & XNN,YNN,ROT,HLLCFLX)
!
!***********************************************************************
! TELEMAC 2D VERSION 7.0                                         R. ATA
!***********************************************************************
!BRIEF
!
!     FUNCTION  : SUBROUTINE COMPUTES HLLC FLUX: THREE HYDRODYNAMICAL
!                 COMPENENTS + TRACER TRANSPORT
!      SEE TORO: SHOCK CAPTURING METHODS FOR FREE
!            SURFACE FLOWS (WILEY 2005)
!
!HISTORY  RIADH ATA (EDF R&D-LNHE)
!+        07/15/2012
!+        V6P2
!+
!
!HISTORY  RIADH ATA (EDF R&D-LNHE)
!+        03/20/2013
!+        V6P3
!+  OPTIMIZATION OF THE CODE
!+  AVOID DIVISION BY 0
!
!HISTORY  RIADH ATA (EDF R&D-LNHE)
!+        10/6/2013
!+        V6P3
!+  BUG FIXED IN COMPUTING U*
!+  THANKS TO L. STADLER (BAW)
!
!history  RIADH ATA & S. PAVAN (EDF R&D-LNHE)
!+        10/04/2014
!+        V7P0
!+  IMPROVEMENT OF S* COMPUTATION FOR DRY CASES (GOTO ADDED)
!+  USE OF ANALYTICAL FORMULA FOR THESE CASES
!+  ADD TEST TO CHECK DIVISION BY ZERO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! |  FLX           | <--|  FLUX COMPONENTS AT THE INTERFACE            |
! |  H1,H2         | -->|  LEFT AND RIGHT WATER DEPTHS                 |
! |  PSI1,PSI2     | -->|  LEFT AND RIGHT TRACER DENSITIES             |
! |  ROT           | -->|  EXECUTE FINAL ROTATION OR NO                |
! |  U1,U2         | -->|  LEFT AND RIGHT VELOCITY X-COMPONENTS        |
! |  V1,V2         | -->|  LEFT AND RIGHT VELOCITY Y-COMPONENTS        |
! |  XNN,YNN       | -->|  X AND Y COMPONENT OF THE OUTWARD NORMAL     |
! ______________________________________________________________________
!
!  MODE: -->(UNCHANGEABLE INPUT),<--(OUTPUT),<-->(CHANGEABLE INPUT)
!-----------------------------------------------------------------------
!  CALLING SUBROUTINE FLUX_WAF OR FLUX_HLLC OR FLUXZZ
!
!***********************************************************************
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: H1,H2,U1,U2,PSI1,PSI2
      DOUBLE PRECISION, INTENT(IN)    :: XI,V1,V2,XNN,YNN
      LOGICAL, INTENT(IN)             :: ROT
      DOUBLE PRECISION, INTENT(INOUT) :: HLLCFLX(4)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                         :: I,SPY
      DOUBLE PRECISION, PARAMETER     :: G = 9.81D0
      DOUBLE PRECISION                :: HL,UL,VL,PSI_L
      DOUBLE PRECISION                :: HR,UR,VR,PSI_R
      DOUBLE PRECISION                :: AL,AR,HSTAR,USTAR
      DOUBLE PRECISION                :: PQL,PQR,SL,SR
      DOUBLE PRECISION                :: QSTARL(4),QSTARR(4)
      DOUBLE PRECISION                :: QL(4),QR(4),FL(4),FR(4)
      DOUBLE PRECISION                :: FSTARL(4),FSTARR(4)
!
      DOUBLE PRECISION                :: GSUR2,EPS,DENOM
      DOUBLE PRECISION                :: FLU2X,FLU2Y
      DOUBLE PRECISION                :: U0,POND,SSTAR
      DOUBLE PRECISION                :: FLX(4)
!
!-----------------------------------------------------------------------
!
      EPS   = 1.E-6
      GSUR2 = G/2.0D0
      SPY   = 0
      PQL   = 0.0D0
      PQR   = 0.0D0
      USTAR = 0.0D0
      HSTAR = 0.0D0
      AL    = 0.0D0
      AR    = 0.0D0
!***********************************************************************
! INITIALIZATION OF FLX AND HLLCFLX
      DO I=1,4
        FLX(I)     = 0.0D0
        HLLCFLX(I) = 0.0D0
      ENDDO
!
!-----------------------------------------------------------------------
! DEPTHS, VELOCITIES, TRACERS
      HL    = H1
      UL    = U1
      VL    = V1
      PSI_L = PSI1
!
      HR    = H2
      UR    = U2
      VR    = V2
      PSI_R = PSI2
!
! ROTATION
!
      U0 = UL
      UL  = XNN*U0+YNN*VL
      VL  =-YNN*U0+XNN*VL
!
      U0 = UR
      UR  = XNN*U0+YNN*VR
      VR  =-YNN*U0+XNN*VR
!
! CASE WITH DRY LEFT AND RIGHT
      IF(HL.LT.EPS.AND.HR.LT.EPS)GOTO 20
!
! CELERITIES
!
      AL = SQRT(G*HL)
      AR = SQRT(G*HR)
! STAR VARIABLES
      HSTAR = 0.5D0*(HL+HR)-0.25D0*(UR-UL)*(HL+HR)/(AL+AR)
!RA BUG FIXED WHEN COMPUTING U STAR
!       USTAR = 0.5D0*(UL+UR)-0.25D0*(HR-HL)*(AL+AR)/(HL+HR)
      USTAR = 0.5D0*(UL+UR)-       (HR-HL)*(AL+AR)/(HL+HR)
! COMPUTE PQL AND PQR:
! IT WILL DEPEND IF WE ARE IN PRESENCE OF SHOCK OR RAREFACTION WAVE
      IF(HSTAR.LT.HL)THEN
!       RAREFACTION
        PQL = 1.0D0
      ELSE
!       SHOCK
        IF(HL.GT.EPS)THEN
          PQL = SQRT(0.5D0*(HSTAR+HL)*HSTAR/HL**2)
        ELSE
          PQL = 0.0D0
        ENDIF
      ENDIF
      IF(HSTAR.LT.HR)THEN
!       RAREFACTION
        PQR = 1.0D0
      ELSE
!       SHOCK
        IF(HR.GT.EPS)THEN
          PQR = SQRT(0.5D0*(HSTAR+HR)*HSTAR/HR**2)
        ELSE
          PQR = 0.0D0
        ENDIF
      ENDIF
!
20    CONTINUE
!
! COMPUTE SL, SR AND SSTAR  (WE CONSIDER DRY CASES)
      IF(HL.GT.EPS)THEN
        SL = UL-AL*PQL
      ELSE
        SL = UR - 2.0D0*AR
        SR = UR + AR
! RA+SP: USE OF ANALYTICAL FORMULA FOR SSTAR
!       SSTAR = SL
        GOTO 35
      ENDIF
!
      IF(HR.GT.EPS)THEN
        SR = UR + AR*PQR
      ELSE
        SL = UL - AL
        SR = UL + 2.0D0*AL
! RA+SP: USE OF ANALYTICAL FORMULA FOR SSTAR
!       SSTAR = SR
        GOTO 35
      ENDIF
!RA      SSTAR = USTAR
35    CONTINUE
      DENOM = HR*(UR-SR)-HL*(UL-SL)
      IF(ABS(DENOM).LT.EPS)THEN
        SSTAR = USTAR
      ELSE
        SSTAR = (SL*HR*(UR-SR)-SR*HL*(UL-SL))/DENOM
      ENDIF
!END RA
!
! COMPUTE QL AND QR
      QL(1)     = HL
      QL(2)     = HL*UL
      QL(3)     = HL*VL
      QL(4)     = HL*PSI_L
!
      QR(1)     = HR
      QR(2)     = HR*UR
      QR(3)     = HR*VR
      QR(4)     = HR*PSI_R

! COMPUTE QSTARL AND QSTARR
      IF(ABS(SL-SSTAR).GT.EPS)THEN
        POND = HL*(SL-UL)/(SL-SSTAR)
      ELSE
        POND = 0.0D0
      ENDIF
      QSTARL(1) = POND
      QSTARL(2) = POND*SSTAR
      QSTARL(3) = POND*VL
      QSTARL(4) = POND*PSI_L
!
      IF(ABS(SR-SSTAR).GT.EPS)THEN
        POND = HR*(SR-UR)/(SR-SSTAR)
      ELSE
        POND = 0.0D0
      ENDIF
      QSTARR(1) = POND
      QSTARR(2) = POND*SSTAR
      QSTARR(3) = POND*VR
      QSTARR(4) = POND*PSI_R
!
! COMPUTE FL AND FR
!
      FL(1)     = HL*UL
      FL(2)     = HL*UL**2 +GSUR2*HL**2
      FL(3)     = HL*UL*VL
      FL(4)     = HL*UL*PSI_L
!
      FR(1)     = HR*UR
      FR(2)     = HR*UR**2 +GSUR2*HR**2
      FR(3)     = HR*UR*VR
      FR(4)     = HR*UR*PSI_R
!
! COMPUTE FSTARL SFTARR
      FSTARL(1) = FL(1) + SL*(QSTARL(1)-QL(1))
      FSTARL(2) = FL(2) + SL*(QSTARL(2)-QL(2))
      FSTARL(3) = FL(3) + SL*(QSTARL(3)-QL(3))
      FSTARL(4) = FL(4) + SL*(QSTARL(4)-QL(4))
!
      FSTARR(1) = FR(1) + SR*(QSTARR(1)-QR(1))
      FSTARR(2) = FR(2) + SR*(QSTARR(2)-QR(2))
      FSTARR(3) = FR(3) + SR*(QSTARR(3)-QR(3))
      FSTARR(4) = FR(4) + SR*(QSTARR(4)-QR(4))
! AND FINALLY THE HLLC FLUX (BEFORE ROTATION)
      IF(XI.LT.SL)THEN
        FLX(1) = FL(1)
        FLX(2) = FL(2)
        FLX(3) = FL(3)
        FLX(4) = FL(4)
        SPY = 1
      ELSEIF(XI.LT.SSTAR.AND.XI.GT.SL) THEN
        FLX(1) = FSTARL(1)
        FLX(2) = FSTARL(2)
        FLX(3) = FSTARL(3)
        FLX(4) = FSTARL(4)
        SPY = 1
      ELSEIF(XI.GT.SSTAR.AND.XI.LT.SR) THEN
        FLX(1) = FSTARR(1)
        FLX(2) = FSTARR(2)
        FLX(3) = FSTARR(3)
        FLX(4) = FSTARR(4)
        SPY = 1
      ELSE
        FLX(1) = FR(1)
        FLX(2) = FR(2)
        FLX(3) = FR(3)
        FLX(4) = FR(4)
        SPY = 1
      ENDIF
      IF(SPY.EQ.0)THEN
        WRITE(LU,*)'ERROR IN HLLC FLUX ESTIMATION (FLUX_HLLC.F)'
        CALL PLANTE(1)
        STOP
      ENDIF
!
! INVERSE ROTATION AND FINAL FLUX
!
      IF(ROT)THEN
        FLU2X  = XNN*FLX(2) - YNN*FLX(3)
        FLU2Y  = YNN*FLX(2) + XNN*FLX(3)
!
        HLLCFLX(1) = FLX(1)
        HLLCFLX(2) = FLU2X
        HLLCFLX(3) = FLU2Y
        HLLCFLX(4) = FLX(4)
      ELSE
! IN THIS CASE, NO ROTATION
!
! FINAL FLUX
!
        HLLCFLX(1) = FLX(1)
        HLLCFLX(2) = FLX(2)
        HLLCFLX(3) = FLX(3)
        HLLCFLX(4) = FLX(4)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
