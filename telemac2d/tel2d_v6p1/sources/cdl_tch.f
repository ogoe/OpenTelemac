!                       ******************
                        SUBROUTINE CDL_TCH
!                       ******************
!
     &(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,KDDL,G,
     & HBOR,UBOR,VBOR,W,CE,FLUENT,FLUSORT,
     & FLBOR,DTHAUT,DT,CFL,EPS,
     & ZF,WINF)
!
!***********************************************************************
! TELEMAC 2D VERSION 6.1                                     03/15/2011
!***********************************************************************
!
!brief  COMPUTATION OF THE CONVECTIVE FLUXES AT BOUNDARIES FOR TCHAMEN FLUX
!
!    UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!
!history  R. ATA (EDF-LNHE)
!+
!+        V6P1
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
      DOUBLE PRECISION ::  FLXI(3),FLXJ(3),H1,U10,U1,V1,ETA1,FLUIJ_20
      DOUBLE PRECISION :: H2,ETA2,U2,V2
      DOUBLE PRECISION :: INFLOW,OUTFLOW
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
       FLXI(1)  = 0.0D0
       FLXI(2)  = 0.0D0
       FLXI(3)  = 0.0D0
       FLXJ(1)  = 0.0D0
       FLXJ(2)  = 0.0D0
       FLXJ(3)  = 0.0D0
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
       ETA1=H1+ZF(IS)
       IF(H1.GT.EPS)THEN
          U1   = W(2,IS)/H1
          V1   = W(3,IS)/H1
       ELSE
          U1   = 0.0D0
          V1   = 0.0D0
          IDRY=IDRY+1
       ENDIF
!************************
!    SOLID WALL
!************************
!
!    PERFECT SLIPPIN CONDITION
!===============================
!
       IF(LIMPRO(K,1).EQ.KNEU) THEN 
!
! DEFINITION OF THE GOST STATE Ue
         H2=H1
         ETA2=ETA1
!        ROTATION 
         U10 = U1
         U1  = XNN*U10+YNN*V1
         V1  =-YNN*U10+XNN*V1
! SET NORMAL COMPONENT = 0        
         U1 =  0.0D0
         U2 =  U1
         V2 =  V1
! INVERSE ROTATION
         U10 = U1
         U1  = -YNN*V1
         V1  =  XNN*V1
!
         U2  = -YNN*V2
         V2  =  XNN*V2

         CALL FLU_TCHAMEN(H2,H1,ETA2,ETA1,U2,U1,
     &                       V2,V1,XNN,YNN,FLXI,FLXJ,G)

!**************************************************
!        LIQUID BOUNDARY
!**************************************************
       ELSEIF((LIMPRO(K,1).EQ.KDIR).OR.(LIMPRO(K,1).EQ.KDDL))THEN 
!
!    IMPOSED H 
!===============================
!
        IF(LIMPRO(K,1).EQ.KDIR) THEN
!
          H2 = WINF(1,K)
          ETA2 = H2 + ZF(IS)
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
            CALL FLU_TCHAMEN(H1,H2,ETA1,ETA2,U1,U2,
     &                       V1,V2,XNN,YNN,FLXI,FLXJ,G)
          ENDIF 
          OUTFLOW    = FLXI(1)*VNL
          FLUSORT    = FLUSORT + OUTFLOW
          FLBOR%R(K) = OUTFLOW

!       LIMPRO(K,1).NE.KDIR    
        ELSE 

          H2 = H1
          U2 = U1
          V2 = V1
          ETA2=ETA1
!
          H1 = WINF(1,K)
          ETA1=H1+ZF(IS)
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
            CALL FLU_TCHAMEN(H2,H1,ETA2,ETA1,U2,U1,
     &                      V2,V1,XNN,YNN,FLXI,FLXJ,G)
          ENDIF 
          INFLOW     = FLXI(1)*VNL
          FLUENT     = FLUENT + INFLOW
          FLBOR%R(K) = INFLOW  

      ENDIF
      ENDIF
!
!
100    CONTINUE

       CE(IS,1)  = CE(IS,1) - VNL*FLXI(1)
       CE(IS,2)  = CE(IS,2) - VNL*FLXI(2)
       CE(IS,3)  = CE(IS,3) - VNL*FLXI(3)
!
       ENDDO
!
!-----------------------------------------------------------------------
!
       RETURN
       END
