!                    **********************
                      SUBROUTINE CALCS_O2
!                    **********************
     & (NPOIN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,NTRAC,GRAV,HPROP,UN,VN,ZF)
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                        21/09/2014
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR O2 WAQ PROCESS
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION
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
! !  SV       ! TR ! D  ! TERMES SOURCES  EXPLICITES                   !
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
! !   U       ! TR ! D  ! VITESSE DE L'EAU                             !
! !   J       ! TR ! D  ! PENTE DE LA LIGNE DE CHARGE                  !
! !   K1      ! R  !    ! CONST DE CINET. DE DEGRAD. DE LA CHARGE ORG. !
! !   K44      ! R  !    ! CONST DE CINET. DE NITRIFICATION             !
! !  DEMBEN     ! R  !    ! DEMANDE BENTHIQUE                            !
! !   RESP    ! R  !    ! RESPIRATION VEGETALE                         !
! !   PHOTO   ! R  !    ! PHOTOSYNTHESE                                !
! !   K2      ! R  !    ! COEFFICIENT DE REAERATION                    !
! !   FORMK2  ! E  !    ! FORMULE DE CALCUL DE K2                      !
! !   O2SATU     ! R  !    ! CONC DE SATURATION EN OXYGENE DE L'EAU (CS)      !
! !   FORMCS  ! E  !    ! FORMULE DE CALCUL DE CS                      !
! !   CORECT  ! R  !    ! COEF DE CORRECTION DES FORMULES AVEC TEMP    !
! !   WATTEMP ! R  !    ! TEMPERATURE  DE L'EAU                                !
! !   RS      ! R  !    ! COEFFICIENT DE REAERATION AUX SEUILS         !
! !   FORMRS  ! E  !    ! FORMULE DE CALCUL DE RS                      !
! !   ARS     ! R  !    ! COEFFICIENT A DES FORMULES DE CALCUL DE R    !
! !   BRS     ! R  !    ! COEFFICIENT B DES FORMULES DE CALCUL DE R    !
! !___________!____!____!______________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY: FORMCS,K2
      USE INTERFACE_WAQTEL, EX_CALCS_O2 => CALCS_O2

      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER          , INTENT(IN   ) :: FORMK2,NPOIN,NTRAC
!      LOGICAL          , INTENT(IN   ) :: YATEMP  ! IF TEMPERATURE IS VARIABLE
      DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP,GRAV
      DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44
!
      DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,UN,VN,ZF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP
!
!     LOCAL VARIABLES
      INTEGER                     :: I,RANKTR1,RANKTR2
      INTEGER         , PARAMETER :: ADDTR = 3
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION, PARAMETER :: CORR1=1.065D0
      DOUBLE PRECISION, PARAMETER :: CORR2=1.0241D0
      DOUBLE PRECISION            :: UNORM,BENCORR,PJ,SECTODAY
      DOUBLE PRECISION            :: CORR1T,CORR2T,POWER
      INTRINSIC MAX,SQRT
!
!     PRELIMINARY COMPUTATIONS
!
      POWER   = WATTEMP-20.D0
!     HERE TEMPERATURE IS FIXED WHICH IS NOT PHYSICAL
!     TO INVESTIGATE IF NECESSARY TO USE T VARIABLE !
      CORR1T  = CORR1**POWER
      CORR2T  = CORR2**POWER
      BENCORR = DEMBEN*CORR1T
      SECTODAY=1.D0/86400.D0
!
!     COMPUTE CS (O2SATU)
!
      CALL SATUR_O2(O2SATU,FORMCS,WATTEMP,EPS)
!
!     COMPUTE K2
!
      CALL REAER(FORMK2,K2,K22,NPOIN,UN,VN,HPROP,EPS)
!
!     COMPUTE RS (DONE IN DIFSOU)
!
!----------------------------------------------------------------------
!     -II- LET'S NOW COMPUTE SOURCE TERMS
!
      RANKTR1 = NTRAC-ADDTR+1
      RANKTR2 = RANKTR1+1
!     RANKTR3 =NTRAC
      DO I=1,NPOIN
!       FIRST TRACER O2 (RANK NTRAC-ADDTR+1) (EXPLICIT ?)
        TEXP%ADR(RANKTR1)%P%R(I) =
     &   K2%R(I) * CORR2T * (O2SATU-TN%ADR(RANKTR1)%P%R(I)) -
     &   K1 * TN%ADR(RANKTR2)%P%R(I) -
     &   K44 * TN%ADR(NTRAC)%P%R(I)  +
     &   PHOTO - RESP - BENCORR/MAX(EPS,HPROP%R(I))
!
!       SECOND TRACER [L] ORGANIC LOAD
!
        TEXP%ADR(RANKTR2)%P%R(I) =
     &      -K1*TN%ADR(RANKTR2)%P%R(I)
!       THIRD TRACER [NH4]
        TEXP%ADR(NTRAC)%P%R(I) =
     &      -K44*TN%ADR(NTRAC)%P%R(I)
!
      ENDDO
!     CONVERT DAYS TO SECONDS
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR1)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR2)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(NTRAC  )%P,C=SECTODAY)
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
