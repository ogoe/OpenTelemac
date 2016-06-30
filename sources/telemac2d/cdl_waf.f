!                       ******************
                        SUBROUTINE CDL_WAF
!                       ******************
!
     &(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,KDDL,
     & W,CE,FLUENT,FLUSORT,FLBOR,DTHAUT,DT,EPS,WINF)
!
!***********************************************************************
!  TELEMAC 2D VERSION 6.3                                         R. ATA
!-----------------------------------------------------------------------
!                 WAF SCHEME (OPTVF =6)
!
!     COMPUTATION OF THE CONVECTIVE FLUXES AT BOUNDARIES
!
!
!     W(1,IS) = H,  W(2,IS)=QU  ,W(3,IS)=QV
!
!history  R. ATA (EDF-LNHE) 07/15/2012
!+
!+        V6P2
!+
!history  R. ATA (EDF-LNHE) 01/07/2013
!+
!+        V6P3
!+ reimplement strong imposition for wall condition
!+ clean unused variables
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  NS            |-->|  TOTAL NUMNER OF NODES
!|  NPTFR         |-->|  TOTAL NUMBER OF BOUNDARY NODES
!|  NBOR          |-->|  GLOBAL NUMBERS OF BOUNDARY POINTS
!|  LIMPRO        |-->|  TYPES OF BOUNDARY CONDITION
!|  XNEBOR,YNEBOR |-->|  UNIT OUTWARD NORMAL COMPONENTS AT BOUNDARY POINTS
!|  KDIR          |-->|  CONVENTION FOR DIRICHLET POINTS
!|  KNEU          |-->|  CONVENTION FOR NEUMANN POINTS
!|  W             |-->|  UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!|  CE            |<->|  FLUX
!|  FLUENT,FLUSORT|<--|  IN AND OUT MASS FLUX
!|  FLBOR         |<--|  IN AND OUT WATER MASS FLUX
!|  DTHAUT        |-->|  CHARACTERISTIC LENGTH (DX) FOR CFL
!|  DT            |<->|  TIME STEP
!|  EPS           |-->|  TOLERANCE FOR WATER DEPTH DIVISION
!|  WINF          |-->|  PRESCRIBED BOUNDARY CONDITIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!-----------------------------------------------------------------------
!
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
      INTEGER, INTENT(IN)             :: NS,NPTFR,KDIR,KNEU,KDDL
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: W(3,NS),DTHAUT(NS),EPS
      DOUBLE PRECISION, INTENT(INOUT) :: DT,WINF(3,NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3),FLUENT,FLUSORT
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,IDRY
!
      DOUBLE PRECISION VNX,VNY,XNN,YNN,VNL
      DOUBLE PRECISION :: FLX(4),H1,U10,U1,V1
      DOUBLE PRECISION :: H2,U2,V2,DX
      DOUBLE PRECISION :: INFLOW,OUTFLOW
!     DOUBLE PRECISION :: HL_UP,HR_UP,VL_UP,VR_UP,PSIL_UP,PSIR_UP
!     HL_UP AND HR_UP NOT USED BUT COULD BE USED IF WE CHANGE ALGORITHM
!     FOR LIMITER
      DOUBLE PRECISION,PARAMETER ::XI=0.D0
!     TO CORRECT WHEN CONSIDERING TRACER
      DOUBLE PRECISION,PARAMETER ::PSI1=0.D0,PSI2=0.D0
!
!-----------------------------------------------------------------------
!
!     LOOP OVER BOUNDARY NODES
!
      DO K=1,NPTFR
!
        IS=NBOR(K)
!
!       INITIALIZATION
!
        FLUENT  = 0.D0
        FLUSORT = 0.0D0
        INFLOW  = 0.0D0
        OUTFLOW = 0.0D0
        FLX(1)  = 0.0D0
        FLX(2)  = 0.0D0
        FLX(3)  = 0.0D0
        FLX(4)  = 0.0D0
! INDICATOR FOR DRY CELLS
        IDRY=0
! NORMALIZED NORMAL
        XNN=XNEBOR(K)
        YNN=YNEBOR(K)
! NON NORMALIZED NORMAL
        VNX=XNEBOR(K+NPTFR)
        VNY=YNEBOR(K+NPTFR)
        VNL=SQRT(VNX**2+VNY**2)
!
        H1 = W(1,IS)
        IF(H1.GT.EPS)THEN
          U1   = W(2,IS)/H1
          V1   = W(3,IS)/H1
        ELSE
          U1   = 0.0D0
          V1   = 0.0D0
          IDRY=IDRY+1
        ENDIF
! MEAN DISTANCE (FOR CFL)
        DX    = DTHAUT(IS)
!**************************************************
!         SOLID WALL
!**************************************************
!===============================
!    SLIPPING CONDITION
!===============================
!
        IF(LIMPRO(K,1).EQ.KNEU) THEN
! FIRST METHOD: STRONG IMPOSITION
!********************************
! DEFINITION OF THE GHOST STATE Ue
          H2=H1
!         ROTATION
          U10 = U1
          U1  = XNN*U10+YNN*V1
          V1  =-YNN*U10+XNN*V1
! PUT NORMAL COMPONENT = 0
          U1 =  0.D0
          U2 =  U1
          V2 =  V1
! INVERSE ROTATION
          U10 = U1
          U1  = -YNN*V1
          V1  =  XNN*V1
!
          U2  = -YNN*V2
          V2  =  XNN*V2
! SECOND METHOD: WEAK IMPOSITION
!********************************
! !DEFINITION OF THE GHOST STATE Ue
!             H2 = H1
! ! INNER PRODUCT 2V.n
!            U10 = 2.D0*(U1*XNN + V1*YNN)
! ! WEAK IMPOSITION: PUT Ve = V1-2(V1.n)n
!            U2 = U1 - U10*XNN
!            V2 = V1 - U10*YNN
!
! ! HERE WE MAKE THE DECISION TO USE HL_UP=HL, HR_UP=HR ..
! ! WHICH HAS A CONSEQUENCE THAT LIMITER =0 (A=1)
!          CALL FLUX_WAF(XI,H1,H2,U1,U2,V1,V2,PSI1,PSI2,
!      &                 H1,H2,V1,V2,PSI1,PSI2,
!      &                 XNN,YNN,DT,DX,FLX)
!
!**************************************************
!         LIQUID BOUDARIES
!**************************************************
        ELSEIF(LIMPRO(K,1).EQ.KDIR.OR.LIMPRO(K,1).EQ.KDDL)THEN
!===============================
!    SI H EST IMPOSEE
!===============================
!
          IF(LIMPRO(K,1).EQ.KDIR) THEN
!
            H2 = WINF(1,K)
!
            IF(H2 .GT.EPS)THEN
              U2 = WINF(2,K) / H2
              V2 = WINF(3,K) / H2
            ELSE
              U2 = 0.0D0
              V2 = 0.0D0
              IDRY = IDRY + 1
            ENDIF
!
            IF(IDRY.LT.2)THEN
!           AT LEAST ONE WET CELL
              CALL FLUX_WAF(XI,H1,H2,U1,U2,V1,V2,PSI1,PSI2,
     &                      H1,H2,V1,V2,PSI1,PSI2,
     &                      XNN,YNN,DT,DX,FLX)
            ENDIF
            OUTFLOW    = FLX(1)*VNL
            FLUSORT    = FLUSORT + OUTFLOW
            FLBOR%R(K) = OUTFLOW
!
!         LIMPRO(K,1).NE.KDIR
          ELSE
            H2 = H1
            U2 = U1
            V2 = V1
!
            H1 = WINF(1,K)
            IF(H1.GT.EPS)THEN
              U1 = WINF(2,K) / H1
              V1 = WINF(3,K) / H1
            ELSE
              U1 = 0.0D0
              V1 = 0.0D0
              IDRY = IDRY + 1
            ENDIF
!
            IF(IDRY.LT.2)THEN
!           AT LEAST ONE WET CELL
              CALL FLUX_WAF(XI,H2,H1,U2,U1,V2,V1,PSI2,PSI1,
     &                      H1,H2,V1,V2,PSI1,PSI2,
     &                      XNN,YNN,DT,DX,FLX)
            ENDIF
            INFLOW     = FLX(1)*VNL
            FLUENT     = FLUENT + INFLOW
            FLBOR%R(K) = INFLOW
!
          ENDIF
        ENDIF
!
        CE(IS,1)  = CE(IS,1) - VNL*FLX(1)
        CE(IS,2)  = CE(IS,2) - VNL*FLX(2)
        CE(IS,3)  = CE(IS,3) - VNL*FLX(3)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

