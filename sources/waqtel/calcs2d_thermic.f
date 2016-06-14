!                    ***************************
                      SUBROUTINE CALCS2D_THERMIC
!                    ***************************
     & (NPOIN,TN,TEXP,HPROP,PATMOS,IND_T,LISTIN)
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                        21/09/2014
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR  WAQ THERMIC PROCESS
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DT             |-->| TIME STEP
!| DIMM           |-->| 2D OR 3D
!| HPROP          |-->| PROPAGATION DEPTH
!| IND_T          |-->| INDEX OF THE TEMPERATURE IN THE TRACER TABLE
!| LISTIN         |-->| LOGICAL FOR LISTING
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NPOIN          |-->| NUMBER OF NODES IN THE MESH
!| NTRAC          |-->| NUMBER OF TRACERS
!| PATMOS         |-->| ATMOSPHERIC PRESSURE
!| TETAT          |-->| COEFFICIENT OF IMPLICITATION FOR TRACERS.
!| TEXP           |-->| EXPLICIT SOURCE TERM.
!| TIMP           |-->| IMPLICIT SOURCE TERM.
!| TN             |-->| TRACERS AT TIME N
!| TSCE           |-->| PRESCRIBED VALUES OF TRACERS AT POINT SOURCES
!| TSCEXP         |<--| EXPLICIT SOURCE TERM OF POINT SOURCES
!|                |   | IN TRACER EQUATION, EQUAL TO:
!|                |   | TSCE - ( 1 - TETAT ) TN
!| VOLU2D         |-->| BASES AREA (NON ASSEMBLED)
!| YASMI          |<--| IF YES, THERE ARE IMPLICIT SOURCE TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!***********************************************************************
!
      USE BIEF_DEF
      USE DECLARATIONS_WAQTEL,ONLY:COEF_K,EMA,CFAER,PVAP,RAY3,
     &                             TAIR,NEBU,NWIND,BOLTZ,CP_EAU,CP_AIR,
     &                             EMI_EAU,EMA,RO0
!      USE EXCHANGE_WITH_ATMOSPHERE
      USE INTERFACE_WAQTEL, EX_CALCS2D_THERMIC => CALCS2D_THERMIC
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN
      TYPE(BIEF_OBJ), INTENT(IN)      :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TEXP
      TYPE(BIEF_OBJ), INTENT(IN)      :: HPROP
      TYPE(BIEF_OBJ), INTENT(IN)      :: PATMOS
      INTEGER,        INTENT(IN)      :: IND_T
      LOGICAL,        INTENT(IN)      :: LISTIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
!
!  LOCAL VARIABLES
!
      INTEGER                     :: I 
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION            :: CE,CV,RE,L_VAP
      DOUBLE PRECISION            :: RAJ,RA,PATMC
      DOUBLE PRECISION            :: TEMPER,HA_SAT
      DOUBLE PRECISION            :: ROA,HA,P_VAP_SAT
      DOUBLE PRECISION   :: CONSTCE,CONSTCV,CONSTRA,CONSTSS,CONSTRE   
!
      INTRINSIC MAX
!
! ----------------------------------------------------------------
!
!     SOME OPTIMIZATION 
      CONSTRE = EMI_EAU*BOLTZ
      CONSTCV = CP_AIR*(CFAER(1)+CFAER(2)*NWIND)
      CONSTCE = CFAER(1)+CFAER(2)*NWIND
      CONSTRA = EMA*BOLTZ *(TAIR+273.15D0)**4 * 
     &          (1.D0+COEF_K*(NEBU/8.D0)**2)
!
!     MAJORATED RADIATION
!
      RAJ      = 1.8D0*CONSTRA
      CONSTSS = 1.D0/(RO0*CP_EAU)
!
!     LOOP OVER ALL MESH POINTS
!
      DO I=1,NPOIN
        TEMPER = TN%ADR(IND_T)%P%R(I)
!       AIR DENSITY
        ROA = 100.D0*PATMOS%R(I)/((TAIR+273.15D0)*287.D0) 
!       AIR SPECIFIC MOISTURE 
        PATMC=PATMOS%R(I)-0.378D0*P_VAP_SAT
        HA  = 0.622D0*PVAP/(MAX(PATMC,EPS))
!       RADIATION ON WATER SURFACE     
        RE = CONSTRE*(TEMPER+273.15D0)**4
!       ADVECTIVE HEAT FLUX 
        CV = ROA*CONSTCV*(TEMPER-TAIR)    ! WIND IS CONSIDERED CONST IN SPACE !
!       VAPOR LATENT HEAT
        L_VAP = 2500900.D0 - 2365.D0*TEMPER
!       PRESSURE OF EVAPORATION
        P_VAP_SAT = 6.11D0*EXP(17.27D0*TEMPER /(TEMPER+237.3D0))
!       AIR MOISTURE AT SATURATION
        IF(ABS(PATMC).GT.EPS)THEN
          HA_SAT = 0.622D0*P_VAP_SAT/PATMC
        ELSE
          HA_SAT = 0.D0
        ENDIF
!       EVAPORATION HEAT FLUX
        CE = ROA*L_VAP*CONSTCE*(HA_SAT-HA)
!       ATMOSPHERIC RADIATION
        IF(HA_SAT.LT.HA)THEN
          RA = RAJ 
        ELSE
          RA = CONSTRA
        ENDIF
!       READY TO INTRODUCE SOURCE TERM
        TEXP%ADR(IND_T)%P%R(I) = CONSTSS*(RAY3+RA-RE-CV-CE)/
     &                           MAX(HPROP%R(I),EPS)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
!
      END SUBROUTINE
