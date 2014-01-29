!                       *******************
                        SUBROUTINE FLUX_WAF
!                       *******************

     &(XI,H1,H2,U1,U2,V1,V2,PSI1,PSI2,
     & HL_UP,HR_UP,VL_UP,VR_UP,PSIL_UP,PSIR_UP,
     & XNN,YNN,DT,DX,WAFFLX)
!
!***********************************************************************
! TELEMAC 2D VERSION 6.3                                         R. ATA
!
!***********************************************************************
!brief 
! 
!     FUNCTION  : SUBROUTINE COMPUTES WAF FLUX: THREE HYDRODYNAMICAL
!                 COMPENENTS + TRACER TRANSPORT
!      SEE TORO: SHOCK CAPTURING METHODS FOR FREE 
!            SURFACE FLOWS (WILEY 2005)
!
!history  RIADH ATA (EDF R&D-LNHE)
!+        07/15/2012
!+        V6P2
!+
!history  RIADH ATA (EDF R&D-LNHE)
!+        03/20/2013
!+        V6P3
!+  OPTIMIZATION OF THE CODE
!+  AVOID DIVISION BY 0
!
!history  RIADH ATA (EDF R&D-LNHE)
!+        12/11/2013
!+        V6P3
!+  BUG FIXED IN COMPUTING U*, THANKS TO L. STADLER (BAW)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! |  DT            | -->|  TIME STEP                                   |
! |  DX            | -->|  CHAACTERISTIC LENGTH (FOR COURANT NUMBER)   |
! |  H1,H2         | -->|  LEFT AND RIGHT WATER DEPTHS                 |
! |  HL_UP,HR_UP   | -->|  LEFT AND RIGHT NEGHBORS WATER DEPTHS        |
! |  PSI1,PSI2     | -->|  LEFT AND RIGHT TRACER DENSITIES             |
! |  PSIL_UP,PSIR_UP| -->|  LEFT AND RIGHT NEIGHBORS TRACER DENSITIES  |
! |  ROT           | -->|  EXECUTE FINAL ROTATION OR NO                |
! |  U1,U2         | -->|  LEFT AND RIGHT VELOCITY X-COMPONENTS        |
! |  V1,V2         | -->|  LEFT AND RIGHT VELOCITY Y-COMPONENTS        |
! |  VL_UP,VR_UP   | -->|  LEFT AND RIGHT NEIGHBOR VELOCITY COMPONENTS |
! |  XI            | -->|  LOCATION OF THE INTERFACE IN RIEMANN DIAGRAM|
! |  XNN,YNN       | -->|  X AND Y COMPONENT OF THE OUTWARD NORMAL     |
! |  WAFFLX        | <--|  FLUX COMPONENTS AT THE INTERFACE            |
! ______________________________________________________________________
!
!  MODE: -->(UNCHANGEABLE INPUT),<--(OUTPUT),<-->(CHANGEABLE INPUT)   
!-----------------------------------------------------------------------
!  CALLING SUBROUTINE FLUX_WAF OR FLUX_HLLC OR FLUXZZ 
! 
!***********************************************************************
!
      USE BIEF
      USE INTERFACE_TELEMAC2D,ONLY: LIMITER,FLUX_HLLC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!     
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: H1,H2,U1,U2,PSI1,PSI2
      DOUBLE PRECISION, INTENT(IN)    :: XI,V1,V2,XNN,YNN,DT
      DOUBLE PRECISION, INTENT(IN)    :: HL_UP,HR_UP,VL_UP,VR_UP
      DOUBLE PRECISION, INTENT(IN)    :: PSIL_UP,PSIR_UP
      DOUBLE PRECISION, INTENT(IN)    :: DX
      DOUBLE PRECISION, INTENT(INOUT) :: WAFFLX(4)
!     
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                         :: I,ILIM
      LOGICAL                         :: ROT,TVD
!
      DOUBLE PRECISION, PARAMETER     :: G = 9.81D0
      DOUBLE PRECISION                :: HL,UL,VL,PSI_L
      DOUBLE PRECISION                :: HR,UR,VR,PSI_R
      DOUBLE PRECISION                :: AL,AR,HSTAR,USTAR
      DOUBLE PRECISION                :: pQL,pQR,SL,SR
      DOUBLE PRECISION                :: FL(4),FR(4)
!
      DOUBLE PRECISION                :: GSUR2,EPS,DTDX
      DOUBLE PRECISION                :: cL,cR,cSTAR,wL,wR
      DOUBLE PRECISION                :: wLR,wLSTAR,wRSTAR
      DOUBLE PRECISION                :: FLU2X,FLU2Y
      DOUBLE PRECISION                :: U0,SSTAR
      DOUBLE PRECISION                :: FLX(4),HLLCFLX(4)
!
      DOUBLE PRECISION                :: LIM_RL,LIM_RR,LIM_RSTAR
      DOUBLE PRECISION                :: rL,rR,rSTAR,DELTA
!
      INTRINSIC SIGN
!
!-----------------------------------------------------------------------
!
      EPS   = 1.E-6
      GSUR2 = G/2.0D0
      ROT   = .FALSE.
      TVD   = .TRUE.
      ILIM  = 4
      pQL   = 0.D0
      pQR   = 0.D0
      USTAR = 0.D0
      HSTAR = 0.D0
      AL    = 0.D0
      AR    = 0.D0
!
!***********************************************************************
!     INITIALIZATION OF FLX, HLLCFLX AND WAFFLX
      DO I=1,4
        FLX(I)     = 0.D0
        HLLCFLX(I) = 0.D0
        WAFFLX(I)  = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!     DEPTHS, VELOCITIES, TRACERS
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
! LET'S START BY COMPUT GNIHLLC FLUX (WITHOUT INVERSE ROTATION IN THE END)
!
      CALL FLUX_HLLC(XI,HL,HR,UL,UR,VL,VR,PSI_L,PSI_R,
     &               XNN,YNN,ROT,HLLCFLX)
!
! ROTATION
!
      U0  = UL
      UL  = XNN*U0+YNN*VL
      VL  =-YNN*U0+XNN*VL
!
      U0  = UR
      UR  = XNN*U0+YNN*VR
      VR  =-YNN*U0+XNN*VR
!
! CASE WITH DRY LEFT AND RIGHT  
      IF(HL.LT.EPS.AND.HR.LT.EPS) GOTO 20 
!
! CELERITIES
!
      AL = SQRT(G*HL)
      AR = SQRT(G*HR)
!
! STAR VARIABLES
!
      HSTAR = 0.5D0*(HL+HR)-0.25D0*(UR-UL)*(HL+HR)/(AL+AR)
!RA BUG FIXED WHEN COMPUTING U STAR, THANKS TO L.STADLER (BAW) 
!     USTAR = 0.5D0*(UL+UR)-0.25D0*(HR-HL)*(AL+AR)/(HL+HR)
      USTAR = 0.5D0*(UL+UR)-       (HR-HL)*(AL+AR)/(HL+HR)

! COMPUTE pQL AND pQR:
! IT DEPENDS IF WE ARE IN PRESENCE OF SHOCK OR RAREFACTION WAVE 
      IF(HSTAR.LT.HL)THEN
!        RAREFACTION
         pQL = 1.0D0
      ELSE
!        SHOCK 
         IF(HL.GT.EPS)THEN
            pQL = SQRT(0.5D0*(HSTAR+HL)*HSTAR/HL**2)
         ELSE
            pQL = 0.0D0
         ENDIF
      ENDIF
      IF(HSTAR.LT.HR)THEN
!        RAREFACTION
         pQR = 1.0D0
      ELSE
!        SHOCK
         IF(HR.GT.EPS)THEN
            pQR = SQRT(0.5D0*(HSTAR+HR)*HSTAR/HR**2)
         ELSE
            pQR = 0.0D0
         ENDIF
      ENDIF
!
20    CONTINUE
!
! FL AND FR
!
      FL(1)   = HL*UL
      FL(2)   = HL*UL**2 +GSUR2*HL**2
      FL(3)   = HL*UL*VL
      FL(4)   = HL*UL*PSI_L 
!
      FR(1)   = HR*UR
      FR(2)   = HR*UR**2 +GSUR2*HR**2
      FR(3)   = HR*UR*VR
      FR(4)   = HR*UR*PSI_R 
!
!     SL, SR AND SSTAR  (WE CONSIDER DRY CASES)
!
      IF(HL.GT.EPS) THEN
        SL    = UL-AL*pQL
      ELSE
        SL    = UR - 2.D0*AR
        SR    = UR + AR
        SSTAR = SL
      ENDIF
!
      IF(HR.GT.EPS)THEN
        SR    = UR + AR*pQR
      ELSE
        SL    = UL - AL
        SR    = UL + 2.D0*AL
        SSTAR = SR
        GOTO 35
      ENDIF
      SSTAR   = USTAR

35    CONTINUE
!
! WEIGHTING COEFFICIENTS wL,wLR, wR wLSTAR AND wRSTAR
!
!     COURANT NUMBERS FOR ALL WAVES
      DTDX  = DT/DX 
      cL    = SL*DTDX
      cR    = SR*DTDX
      cSTAR = SSTAR*DTDX
!
!===================================================
!   NON TVD WAF SCHEME
!===================================================
!
      IF(.NOT.TVD) THEN
!
!     COEFFICIENTS
      wL     = 0.5D0*(1.D0 + cL)
      wR     = 0.5D0*(1.D0 - cR)
      wLR    = 0.5D0*(cR - cL)
      wLSTAR = 0.5D0*(1.D0 + cSTAR)
      wRSTAR = 0.5D0*(1.D0 - cSTAR)
!
!     FINAL FLUX (BEFORE ROTATION)
!
      FLX(1) = wL*FL(1) + wLR*HLLCFLX(1) + wR*FR(1)
      FLX(2) = wL*FL(2) + wLR*HLLCFLX(2) + wR*FR(2)
      FLX(3) = (wLSTAR*VL + wRSTAR*VR)*FLX(1)
      FLX(4) = (wLSTAR*PSI_L + wRSTAR*PSI_R)*FLX(1)
!
!===================================================
!    TVD WAF SCHEME
!===================================================
!
      ELSE
!
!     LIMITERS
!     PREPARE rK BEFORE CALLING LIMITER
!     COMPUTE ALL rK (SEE LOUKILI ET AL. PAGE 4)
!       rL
        IF(SL.GT.0.D0)THEN
          DELTA = HL-HL_UP
        ELSE
          DELTA = HR_UP-HR
        ENDIF
        rL = DELTA/(HR-HL + EPS)
!       rR
        IF(SR.GT.0.0D0)THEN
          DELTA = HL-HL_UP
        ELSE
          DELTA = HR_UP-HR
        ENDIF
        rR = DELTA/(HR-HL + EPS)
!       r*
        IF(SSTAR.GT.0.D0)THEN
          DELTA = VL-VL_UP
        ELSE
          DELTA = VR_UP-VR
        ENDIF
        rSTAR = DELTA/(VR-VL+EPS)
! 
        LIM_RL    = LIMITER(ILIM,rL,cL)
        LIM_RR    = LIMITER(ILIM,rR,cR)
        LIM_RSTAR = LIMITER(ILIM,rSTAR,cSTAR)
!
!   TVD COEFFICIENTS
!
      wL     = 0.5D0*(1.D0 + SIGN(1.D0,cL)*LIM_RL) !DSIGN(A,B)=|A|*SIGN(B)
      wR     = 0.5D0*(1.D0 - SIGN(1.D0,cR)*LIM_RR)
      wLR    = 0.5D0*(SIGN(1.D0,cR)*LIM_RR - SIGN(1.D0,cL)*LIM_RL)
      wLSTAR = 0.5D0*(1.D0 + SIGN(1.D0,cSTAR)*LIM_RSTAR)
      wRSTAR = 0.5D0*(1.D0 - SIGN(1.D0,cSTAR)*LIM_RSTAR)
!
! FINAL FLUX (BEFORE ROTATION)
!
      FLX(1) = wL*FL(1) + wLR*HLLCFLX(1) + wR*FR(1)
      FLX(2) = wL*FL(2) + wLR*HLLCFLX(2) + wR*FR(2)
      FLX(3) = (wLSTAR*VL    + wRSTAR*VR   )*FLX(1)
      FLX(4) = (wLSTAR*PSI_L + wRSTAR*PSI_R)*FLX(1)
!
      ENDIF
!
!
! INVERSE ROTATION
!
      FLU2X  = XNN*FLX(2) - YNN*FLX(3) 
      FLU2Y  = YNN*FLX(2) + XNN*FLX(3)
!
! FINAL WAF FLUX 
!
      WAFFLX(1) = FLX(1)
      WAFFLX(2) = FLU2X 
      WAFFLX(3) = FLU2Y
      WAFFLX(4) = FLX(4) 
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
