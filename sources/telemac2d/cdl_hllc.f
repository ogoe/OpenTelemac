!                       ********************
                        SUBROUTINE CDL_HLLC
!                       ********************
!
     &(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,
     & W,CE,FLUENT,FLUSORT,FLBOR,EPS,WINF,
     & G,HBOR,UBOR,VBOR,MESH)
!
!***********************************************************************
! TELEMAC 2D VERSION 7.1
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
!history  R. ATA (EDF-LNHE) 07/15/2013
!+
!+        V6P3
!+ reimplement strong imposition
!+ cleaning
!
!history  R. ATA (EDF-LNHE) 06/05/2014
!+
!+        V7P0
!+ completely re-written to exactly impose boundary
!+ conditions and especially flowrates
!
!history  R. ATA (EDF-LNHE) 10/01/2015
!+
!+        V7P0
!+ add free condition and fix parallel issues
!+ move projection on liquid boundaries to majzz
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  NS            |-->|  TOTAL NUMBER OF NODES
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
!|  EPS           |-->|  TOLERANCE FOR WATER DEPTH DIVISION
!|  HBOR,UBOR,VBOR|-->|  PRESCRIBED H, U AND V GIVEN BY BORD
!|  WINF          |-->|  PRESCRIBED BOUNDARY CONDITIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC,ONLY: KDIR,KENT,KDDL,KNEU,KENTU
      USE DECLARATIONS_TELEMAC2D,ONLY:NUMLIQ,LIUBOR,ENTET
      USE INTERFACE_TELEMAC2D, EX_CDL_HLLC => CDL_HLLC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,NPTFR
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: W(3,NS),EPS,G
      DOUBLE PRECISION, INTENT(INOUT) :: WINF(3,NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3),FLUENT,FLUSORT
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,IDRY
!
      DOUBLE PRECISION :: VNX,VNY,XNN,YNN,VNL(NPTFR)
      DOUBLE PRECISION :: UNN,VNN,LAMBDA1,LAMBDA2
      DOUBLE PRECISION :: FLX(NPTFR,4),H1,U10,U1,V1
      DOUBLE PRECISION :: H2,U2,V2,UGN,AL,C1
      DOUBLE PRECISION :: HG,UG,VG,CG,UGTEMP
      DOUBLE PRECISION :: INFLOW,OUTFLOW,REGIME,FOURG
      DOUBLE PRECISION,PARAMETER ::XI=0.0D0
!     TO CORRECT WHEN CONSIDERING TRACERS
      DOUBLE PRECISION,PARAMETER ::PSI1=0.0D0,PSI2=0.0D0
      LOGICAL                    ::ROT,DEJA
!
      DEJA =.FALSE.
      ROT = .TRUE.
      FOURG = 4.D0*G
!
!     LOOP OVER BOUNDARY NODES
      IF(NPTFR.GT.0)THEN    ! FOR PARALLEL CASES
      DO K=1,NPTFR
        IS=NBOR(K)
!
!     INITIALIZATION
        FLBOR%R(K) = 0.D0
        FLUENT     = 0.D0
        FLUSORT    = 0.D0
        INFLOW     = 0.D0
        OUTFLOW    = 0.D0
        FLX(K,1)     = 0.D0
        FLX(K,2)     = 0.D0
        FLX(K,3)     = 0.D0
        FLX(K,4)     = 0.D0
!     INDICATOR FOR DRY CELLS
        IDRY=0
!     NORMALIZED NORMAL
        XNN=XNEBOR(K)
        YNN=YNEBOR(K)
!     NON NORMALIZED NORMAL
        VNX=XNEBOR(K+NPTFR)
        VNY=YNEBOR(K+NPTFR)
!
        VNL(K)=SQRT(VNX**2+VNY**2)
!
        H1 = W(1,IS)
        IF(H1.GT.EPS)THEN
          U1 = W(2,IS)/H1
          V1 = W(3,IS)/H1
        ELSE
          U1 = 0.0D0
          V1 = 0.0D0
          IDRY=IDRY+1
        ENDIF
!**************************************************
!     WALL BOUNDARY
!**************************************************
!===============================
!     SLIPPING CONDITION
!===============================
!
        IF(LIMPRO(K,1).EQ.KNEU) THEN

! FIRST METHOD: STRONG IMPOSITION
!********************************
!    CE.n = 0  is done in cdlproj
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
!DEFINITION OF THE GHOST STATE Ue
!           H2 = H1
! INNER PRODUCT 2V.n
!           U10 = 2.D0*(U1*XNN + V1*YNN)
! WEAK IMPOSITION: PUT Ve = V1-2(V1.n)n
!           U2 = U1 - U10*XNN
!           V2 = V1 - U10*YNN
!
          CALL FLUX_HLLC(XI,H1,H2,U1,U2,V1,V2,PSI1,PSI2,
     &                   XNN,YNN,ROT,FLX(K,:))
!          GOTO 100
!
!**************************************************
!         LIQUID BOUNDARIES
!**************************************************
        ELSEIF(LIMPRO(K,1).EQ.KDIR.OR.LIMPRO(K,1).EQ.KDDL)THEN

!         PREPARE COMPUTATION OF RIEMANN INVARIANTS
          IF(H1.LT.EPS)THEN
            UNN = 0.D0
            VNN = 0.D0
          ELSE
            UNN =  XNN*U1 + YNN*V1
            VNN = -YNN*U1 + XNN*V1
          ENDIF
!===============================
!         IF H IS IMPOSED
!===============================
!
          IF(LIMPRO(K,1).EQ.KDIR) THEN
!
            HG = HBOR(K) ! THIS IS HG (GHOST STATE)
            CG = SQRT(G*HG)
            C1 = SQRT(G*H1)
            LAMBDA1 = UNN + C1 ! WE USE REAL H (H1) TO ASSESS THE REGIME
            LAMBDA2 = UNN - C1
            REGIME  = LAMBDA1*LAMBDA2
!
            IF(REGIME.LT.0.D0.OR.UNN.LE.0.D0) THEN ! SUBCRITICAL REGIME OR ENTRY
              IF(HG.LT.EPS)THEN
                UG = 0.D0 !  UG (GHOST)
                VG = 0.D0 !  VG (GHOST)
                IDRY = IDRY + 1
              ELSE
                IF(REGIME.LT.0.D0) THEN !SUBCRITICAL
                  UG = UNN +2.D0*(C1-CG)
                  VG = VNN
!                 INVERSE ROTATION
                  UGTEMP = UG
                  UG = XNN*UGTEMP - YNN*VG
                  VG = YNN*UGTEMP + XNN*VG
                ELSE                            ! SUPERCRITICAL
                  IF(LIUBOR%I(K).EQ.KENTU.OR.LIUBOR%I(K).EQ.KENT) THEN ! IMPOSED INFLOW
                                                                   ! OR IMPOSED VELOCITY
                    UG = UBOR(K)  ! FORCING H TO HG AND U TO UG (SUPERCRITICAL)
                    VG = VBOR(K)
                  ELSE ! DATA MISSING
                    UG = 0.D0 !THIS IS LAKE AT REST (HYPOTHESIS)
                    VG = 0.D0
                    IF(.NOT.DEJA.AND.ENTET)THEN
                      IF(LNG.EQ.1) WRITE(LU,30) NUMLIQ%I(K)
                      IF(LNG.EQ.2) WRITE(LU,31) NUMLIQ%I(K)
                      DEJA=.TRUE.
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ELSE !THIS IS A SUPERCRITICAL OUTFLOW (NO NEED FOR GIVEN H)
              IF(.NOT.DEJA.AND.ENTET)THEN
                IF(LNG.EQ.1) WRITE(LU,60) NUMLIQ%I(K),ABS(UNN)/
     &                                    MAX(EPS,C1)
                IF(LNG.EQ.2) WRITE(LU,61) NUMLIQ%I(K),ABS(UNN)/
     &                                    MAX(EPS,C1)
                DEJA=.TRUE.
!               NO CONTRIBUTION
              ENDIF
!              GOTO 100
            ENDIF
!
          GOTO 90 ! TO COMPUTE THE FLUX
!==================================
!      IF GIVEN VELOCITY OR DISCHARGE
!==================================
!
          ELSEIF(LIMPRO(K,2).EQ.KDIR)THEN     !   (LIUBOR%I(K).EQ.KENTU)THEN
!
            UG = UBOR(K) ! GIVEN BY BORD
            VG = VBOR(K) ! GIVEN BY BORD
!
            UGN =  XNN*UG + YNN*VG ! TO RETRIEVE NORMAL COMPONENT OF UG
!           VGN = -YNN*UG + XNN*VG ! AND SO ON
!
            HG = (UNN + 2.D0*SQRT(G*H1)-UGN)**2/FOURG
!
            IF(HG.LE.EPS)THEN
              IDRY = IDRY + 1
            ENDIF
!
            GOTO 90 ! TO COMPUTE THE FLUX
!=========================================================
!     IF GIVEN DISCHARGE :CONSIDERED IN THE PREVIOUS CASES
!     THANKS TO SUBROUTINE DEBIMP WHICH TRANSFORMS FLOWRATES
!     TO H (AT TN, WHICH CAN BE IMPROVED) AND VELOCITY
!==========================================================
!         ELSE IF(LIUBOR%I(K).EQ.KENT)THEN
!
!===============================
!     FREE CONDITION
!===============================
          ELSE
!         NOTHING TO DO
            HG = 0.D0
            UG = 0.D0
            VG = 0.D0
            IDRY = IDRY+1
          ENDIF
!
90        CONTINUE
!         COMPUTE THE FLUX
          IF(IDRY.LT.2)THEN
!         AT LEAST ONE WET CELL
            CALL FLUX_HLLC(XI,H1,HG,U1,UG,V1,VG,PSI1,PSI2,
     &                     XNN,YNN,ROT,FLX(K,:))
          ENDIF
!
        ENDIF
      ENDDO
      ENDIF
!
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM_BORD(FLX(1,1),1,MESH)
        CALL PARCOM_BORD(FLX(1,2),1,MESH)
        CALL PARCOM_BORD(FLX(1,3),1,MESH)
!       FOR TRACER UNCOMMENT WHEN IMPLEMENTED
!       CALL PARCOM_BORD(FLX(1,4),1,MESH)
      ENDIF

      IF(NPTFR.GT.0)THEN
      DO K=1,NPTFR
        IS=NBOR(K)
!       FINAL BALANCE
        IF(NCSIZE.GT.1)THEN
          OUTFLOW  = FLX(K,1)*VNL(K)*MESH%IFAC%I(IS)
        ELSE
          OUTFLOW  = FLX(K,1)*VNL(K)
        ENDIF
        IF(FLX(K,1).LE.0.D0)THEN ! INLET
          FLUENT = FLUENT + OUTFLOW
        ELSE                    ! OUTLET
          FLUSORT = FLUSORT + OUTFLOW
        ENDIF
        FLBOR%R(K) = OUTFLOW
!
!100     CONTINUE
!
        CE(IS,1)  = CE(IS,1) - VNL(K)*FLX(K,1)
        CE(IS,2)  = CE(IS,2) - VNL(K)*FLX(K,2)
        CE(IS,3)  = CE(IS,3) - VNL(K)*FLX(K,3)
      ENDDO
!
      ENDIF ! PARALLEL CASES
!
30    FORMAT(1X,'CDL_HLLC: ATTENTION SUR LA FRONTIERE ',1I6,/,1X,
     & '          ENTREE TORRENTIELLE        ',/,1X,
     & '          ET DEBIT NON FOURNI',/,1X)
31    FORMAT(1X,'CDL_HLLC: WARNING, LIQUID BOUNDARY ',1I6,/,1X,
     & '          SUPERCRITICAL INLET        ',/,1X,
     & '          AND NO DISCHARGE PROVIDED',/,1X)
!
60    FORMAT(1X,'CDL_HLLC: ATTENTION SUR LA FRONTIERE ',1I6,/,1X,
     & '          SORTIE TORRENTIELLE ET DONC       ',/,1X,
     & '          CONDITION AUX LIMITES (PEUT-ETRE) NON VERIFIEE',/,1X,
     & '          NOMBRE DE FROUDE A LA FRONTIERE: ',G16.7)
61    FORMAT(1X,'CDL_HLLC: WARNING, LIQUID BOUNDARY ',1I6,/,1X,
     & '          SUPERCRITICAL OUTLET        ',/,1X,
     & '          DESIRED BOUNDARY CONDITION MAY BE UNSATISFIED',/,1X,
     & '          FROUDE AT BOUNDARY IS: ',G16.7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
