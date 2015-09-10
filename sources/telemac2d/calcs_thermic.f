!                     ************************
                      SUBROUTINE CALCS_THERMIC
!                     ************************
     &(NPOIN,TN,TEXP)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR  WAQ THERMIC PROCESS
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+      CREATION
!
!history  J-M HERVOUET
!+        07/09/2015
!+        V7P1
!+      Checking that the air pressure has been given
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! ! NBTRA     ! E  ! M  ! NOMBRE DE TRACEURS                           !
! !  Q        ! TR ! D  ! DEBIT                                        !
! !  A        ! TR ! D  ! SECTION MOUILLEE                             !
! !  Z        ! TR ! D  ! HAUTEUR D EAU                                !
! !  RH       ! TR ! D  ! RAYON HYDRAULIQUE                            !
! !  ST       ! TR ! D  ! STRICKLER                                    !
! !  IM       ! E  ! M  ! NOMBRE DE SECTIONS DE CALCUL                 !
! !  C        ! TR ! D  ! CONCENTRATIONS                               !
! !  SVA      ! TR ! D  ! TERMES SOURCES VOLUMIQUE AJOUTES             !
! !  SSA      ! TR ! D  ! TERME SOURCE SURFACIQUE  AJOUTES             !
! !  T        !  R ! D  ! TEMPS                                        !
! !  DT       !  R ! D  ! PAS DE TEMPS                                 !
!  RESULTATS------------------------------------------------------------
! !  RNUV     ! TR ! D  ! TERMES SOURCES VOLUMIQUES IMPLICITES         !
! !  RNUS     ! TR ! D  ! TERME SOURCE SURFACIQUE IMPLICITES           !
! !  SV       ! TR ! D  ! TERMES SOURCES EXPLICITES                   !
! !          !
! !___________!____!____!______________________________________________!
!                               COMMON
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  NMSCAL   ! E  ! M  ! NOMBRE MAXIMUM DE SECTIONS DE CALCUL         !
! !  NMTRA    ! E  ! M  ! NOMBRE MAXIMUM DE TRACEURS                   !
! !___________!____!____!______________________________________________!
!                          VARIABLES INTERNES
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !   RO      ! R  !    ! MASSE VOLUMIQUE DE L'EAU                     !
! !   CPE     ! R  !    ! CHALEUR SPECIFIQUE DE L'EAU                  !
! !   CP_AIR  ! R  !    ! CHALEUR SPECIFIQUE DE L'AIR                  !
! !   CFAER(1)! R  !    ! COEF.A DE LA FORMULE D'AERATION A+UB         !
! !   CFAER(2)! R  !    ! COEF.B DE LA FORMULE D'AERATION A+UB         !
! !   COEF_K  ! R  !    ! COEF. REPR. DE LA COUVERTURE NUAGEUSE        !
! !   EMA     ! R  !    ! COEF. DE CALAGE DU RAYONNT ATMOSPH.          !
! !   EMI_EAU ! R  !    ! COEF. DE CALAGE DU RAYONNT DU PLAN D'EAU     !
! !           !    !    !                                              !
! !  IF1      ! TR ! D  ! INDIC DE LECTURE DU FICHIER DES PARAMETRES   !
! !___________!____!____!______________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
      USE BIEF_DEF
      USE DECLARATIONS_WAQTEL,ONLY:COEF_K,EMA,CFAER,PVAP,RAY3,
     &                             TAIR,NEBU,NWIND,BOLTZ,CP_EAU,CP_AIR,
     &                             EMI_EAU,EMA,ROO
!     USE EXCHANGE_WITH_ATMOSPHERE
      USE DECLARATIONS_TELEMAC2D,ONLY: HPROP,PATMOS,IND_T,LISTIN,ATMOS
      USE INTERFACE_TELEMAC2D, EX_CALCS_THERMIC => CALCS_THERMIC
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TN,TEXP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  LOCAL VARIABLES
!
      INTEGER                     :: I
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION            :: CE,CV,RE,L_VAP
      DOUBLE PRECISION            :: PATM,RAJ,RA
      DOUBLE PRECISION            :: TEMPER,HA_SAT
      DOUBLE PRECISION            :: ROA,HA,P_VAP_SAT
      DOUBLE PRECISION   :: CONSTCE,CONSTCV,CONSTRA,CONSTSS,CONSTRE
!
!     THE FOLLOWING CONSTANTS ARE THOSE OF EXCHANGE_WITH_ATM MODULE
!     TO REMOVE LATER CP_EAU,CP_AIR,BOLTZ,ROO,EMI_EAU
!
      INTRINSIC MAX
!
! ----------------------------------------------------------------
!
!     HERE WE NEED ONLY TAIR, PVAP, NWIND, NEBU, RAY3, PATM
!     HERE PATM IS CONSTANT, WHILE FOR TELEMAC PATMOS VARIES IN SPACE
!
      IF(ATMOS) THEN
!       Note JMH: this will not work in parallel if the pressure is
!                 variable in space...
        PATM=PATMOS%R(1)
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'CALCS_THERMIC : LE MOT-CLE'
          WRITE(LU,*) '                PRESSION ATMOSPHERIQUE'
          WRITE(LU,*) '                DOIT ETRE A OUI'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CALCS_THERMIC : KEYWORD'
          WRITE(LU,*) '                AIR PRESSURE'
          WRITE(LU,*) '                MUST BE YES'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     AIR DENSITY
      ROA = 100.D0*PATM/((TAIR+273.15D0)*287.D0)
!     AIR SPECIFIC MOISTURE ? CAN BE READ DIRECTLY FROM METEO FILE ?
      HA  = 0.622D0*PVAP/(MAX(PATM-0.378D0*PVAP,EPS))
!
!     SOME OPTIMIZATION
      CONSTRE = EMI_EAU*BOLTZ
      CONSTCV = ROA*CP_AIR*(CFAER(1)+CFAER(2)*NWIND)
      CONSTCE = ROA*(CFAER(1)+CFAER(2)*NWIND)
      CONSTRA = EMA*BOLTZ *(TAIR+273.15D0)**4 *
     &          (1.D0+COEF_K*(NEBU/8.D0)**2)
!     MAJORATED RADIATION
      RAJ     = 1.8D0*CONSTRA
      CONSTSS = 1.D0/(ROO*CP_EAU)
!     LOOP OVER ALL MESH POINTS
      DO I=1,NPOIN
        TEMPER = TN%ADR(IND_T)%P%R(I)
!       RADIATION ON WATER SURFACE
        RE = CONSTRE*(TEMPER+273.15D0)**4
!       ADVECTIVE HEAT FLUX
        CV = CONSTCV*(TEMPER-TAIR)    ! WIND IS CONSIDERED CONST IN SPACE !
!       VAPOR LATENT HEAT
        L_VAP = 2500900.D0 - 2365.D0*TEMPER
!       PRESSURE OF EVAPORATION
        P_VAP_SAT = 6.11D0*EXP(17.27D0*TEMPER /(TEMPER+237.3D0))
!       AIR MOISTURE AT SATURATION
        IF(ABS(PATM-0.378D0*P_VAP_SAT).GT.EPS)THEN
          HA_SAT = 0.622D0*P_VAP_SAT/(PATM-0.378D0*P_VAP_SAT )
        ELSE
          HA_SAT = 0.D0
        ENDIF
!       EVAPORATION HEAT FLUX
        CE = L_VAP*CONSTCE*(HA_SAT-HA)
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

      END SUBROUTINE
