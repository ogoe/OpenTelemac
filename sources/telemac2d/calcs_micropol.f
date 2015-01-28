!                    ***************************
                      SUBROUTINE CALCS_MICROPOL
!                    **************************
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                        21/09/2014
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR MICROPOL WAQ PROCESS
!          WAQ PROCESS OF CODE_TRACER (MASCARET SYSTEM)
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
! !  J        ! TR ! D  ! PENTE DE LA LIGNE DE CHARGE                  !
! !  TAUB     ! TR ! D  ! TENSION DE CISAILLEMENT AU FOND              !
! !  ERO      ! R  !    ! TAUX D EROSION                               !
! !  TAUS     ! R  !    ! CONTRAINTE CRITIQUE DE REMISE EN SUSPENSION  !
! !  TAUR     ! R  !    ! CONTRAINTE CRITIQUE DE SEDIMENTATION         !
! !  VITCHU   ! R  !    ! VITESSE DE CHUTE DES MES                     !
! !  LAMBD    ! R  !    ! CONSTANTE DE DESINTEGRATION EXPONENTIELLE    !
! !  KD       ! R  !    ! COEFFICIENT DE DISTRIBUTION                  !
! !  KDESORP  ! R  !    ! CONSTANTE CINETIQUE DE DESORPTION            !
! !           !    !    !                                              !
! !  IF1      ! TR ! D  ! INDIC DE LECTURE DU FICHIER DES PARAMETRES   !
! !___________!____!____!______________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!***********************************************************************
!
      USE BIEF_DEF
      USE DECLARATIONS_WAQTEL,ONLY:ERO,TAUR,TAUS,VITCHU,CDISTRIB,KDESORP
!      USE EXCHANGE_WITH_ATMOSPHERE
      USE DECLARATIONS_TELEMAC2D,ONLY: HPROP,LISTIN
!      USE INTERFACE_TELEMAC2D, EX_CALCS_MICROPOL => CALCS_MICROPOL

      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!
!
      
      IF(LNG.EQ.1)THEN
        WRITE(LU,*) 'MICROPOL: NOT IMPLEMENTED YET'
      ELSE
        WRITE(LU,*) 'MICROPOL: NOT IMPLEMENTED YET'
      ENDIF
      CALL PLANTE(1)
      STOP
      

!
!
!-----------------------------------------------------------------------
!
      RETURN
      END 
