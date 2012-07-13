!                       ********************                             
                        SUBROUTINE CDL_HLLC
!                       ********************                             
!                                                                       
     &(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,KDDL,G,
     & HBOR,UBOR,VBOR,W,CE,FLUENT,FLUSORT,
     & FLBOR,DTHAUT,DT,CFL,EPS,
     & ZF,WINF)
!                                                                       
!
!***********************************************************************
! TELEMAC 2D VERSION 6.2                                     01/15/2012
!***********************************************************************
!
!brief  COMPUTATION OF THE CONVECTIVE FLUXES AT BOUNDARIES FOR HLLC FLUX
!
!    UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!
!history  R. ATA (EDF-LNHE)
!+
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  NS            |-->|  TOTAL NUMBER OF NODES
!|  NPTFR         |-->|  TOTAL NUMBER OF BOUNDARY NODES
!|  NBOR          |-->|  GLOBAL NUMBERS OF BOUNDARY POINTS
!|  LIMPRO        |-->|  TYPES OF BOUNDARY CONDITION
!|  XNEBOR,YNEBOR |-->|  UNIT OUTWARD NORMAL COMPONENTS AT BOUNDARY POINTS
!|  KDIR          |-->|  CONVENTION FOR DIRICHLET POINTS
!|  KNEU          |-->|  CONVENTION FOR NEUMANN POINTS
!|  G             |-->|  GRAVITY CONSTANT
!|  HBOR          |-->|  IMPOSED VALUES FOR H
!|  UBOR          |-->|  IMPOSED VALUES FOR U
!|  VBOR          |-->|  IMPOSED VALUES FOR V
!|  W             |-->|  UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!|  CE            |<->|  FLUX 
!|  FLUENT,FLUSORT|<--|  IN AND OUT MASS FLUX
!|  FLBOR         |<--|  IN AND OUT WATER MASS FLUX
!|  DTHAUT        |-->|  CHARACTERISTIC LENGTH (DX) FOR CFL
!|  DT            |<->|  TIME STEP
!|  CFL           |-->|  CFL NUMBER
!|  EPS           |-->|  TOLERANCE FOR WATER DEPTH DIVISION 
!|  ZF            |-->|  BATHYMETRY
!|  WINF          |-->|  PRESCRIBED BOUNDARY CONDITIONS 
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
      INTEGER, INTENT(IN)             :: NS,NPTFR,KDIR,KNEU,KDDL
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR),W(3,NS),DTHAUT(*)
      DOUBLE PRECISION, INTENT(IN)    :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFL,EPS,ZF(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DT,WINF(3,NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3),FLUENT,FLUSORT
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,NIT,IDRY    
!    
      DOUBLE PRECISION VNX,VNY,XNN,YNN,VNL
      DOUBLE PRECISION :: FLX(4),H1,U10,U1,V1,FLUIJ_20
      DOUBLE PRECISION :: H2,U2,V2
      DOUBLE PRECISION :: INFLOW,OUTFLOW
      DOUBLE PRECISION,PARAMETER ::XI=0.0D0
! TO CORRECT WHEN CONSIDERING TRACERS
      DOUBLE PRECISION,PARAMETER ::PSI1=0.0D0,PSI2=0.0D0
      LOGICAL                    ::ROT
!
      ROT = .TRUE.
!
! LOOP OVER BOUNDARY NODES
      DO K=1,NPTFR
       IS=NBOR(K)
!
! INITIALIZATION
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
!   NORMALIZED NORMAL    
       XNN=XNEBOR(K)
       YNN=YNEBOR(K)
!   NON NORMALIZED NORMAL
       VNX=XNEBOR(K+NPTFR)
       VNY=YNEBOR(K+NPTFR)
       VNL=SQRT(VNX**2+VNY**2)
!
       H1   = W(1,IS)

       IF(H1.GT.EPS)THEN
          U1   = W(2,IS)/H1
          V1   = W(3,IS)/H1
       ELSE
          U1   = 0.0D0
          V1   = 0.0D0
          IDRY=IDRY+1
       ENDIF
!**************************************************
!         WALL BOUNDARY
!**************************************************
!===============================
!    SLIPPING CONDITION
!===============================
!
       IF(LIMPRO(K,1).EQ.KNEU) THEN 

! FIRST METHOD: STRONG IMPOSITION
!********************************
! DEFINITION OF THE GHOST STATE Ue
!         H2=H1
!        ROTATION 
!         U10 = U1
!         U1  = XNN*U10+YNN*V1
!         V1  =-YNN*U10+XNN*V1
!! PUT NORMAL COMPONENT = 0        
!         U1 =  0.0D0
!         U2 =  U1
!         V2 =  V1
!! INVERSE ROTATION
!         U10 = U1
!         U1  = -YNN*V1
!         V1  =  XNN*V1
!!         
!         U2  = -YNN*V2
!         V2  =  XNN*V2
! SECOND METHOD: WEAK IMPOSITION
!********************************
!DEFINITION OF THE GHOST STATE Ue
            H2 = H1
! INNER PRODUCT 2V.n
           U10 = 2.D0*(U1*XNN + V1*YNN)
! WEAK IMPOSITION: PUT Ve = V1-2(V1.n)n
           U2 = U1 - U10*XNN
           V2 = V1 - U10*YNN

         CALL FLUX_HLLC(XI,H1,H2,U1,U2,V1,V2,PSI1,PSI2,
     *                 XNN,YNN,ROT,FLX)
!
!**************************************************
!         LIQUID BOUNDARIES
!**************************************************
       ELSEIF((LIMPRO(K,1).EQ.KDIR).OR.(LIMPRO(K,1).EQ.KDDL))THEN 
!===============================
!    IF H IS IMPOSED
!===============================
!
        IF(LIMPRO(K,1).EQ.KDIR) THEN
!
          H2 = WINF(1,K)

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
!         AT LEAST ONE WET CELL
            CALL FLUX_HLLC(XI,H1,H2,U1,U2,V1,V2,PSI1,PSI2,
     *                    XNN,YNN,ROT,FLX)
          ENDIF 
          OUTFLOW    = FLX(1)*VNL
          FLUSORT    = FLUSORT + OUTFLOW
          FLBOR%R(K) = OUTFLOW

!       LIMPRO(K,1).NE.KDIR    
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
!         AT LEAST ONE WET CELL
            CALL FLUX_HLLC(XI,H2,H1,U2,U1,V2,V1,PSI2,PSI1,
     *                    XNN,YNN,ROT,FLX)
          ENDIF 
          INFLOW     = FLX(1)*VNL
          FLUENT     = FLUENT + INFLOW
          FLBOR%R(K) = INFLOW 

      ENDIF
      ENDIF
!
!
100    CONTINUE

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
!**********************************************************************
