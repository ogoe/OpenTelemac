!                    **********************
                      SUBROUTINE CALCS_O2
!                    **********************
     & (NPOIN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,NTRAC)
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
      USE DECLARATIONS_TELEMAC2D,ONLY: GRAV,HPROP,UN,VN,ZF
      USE INTERFACE_TELEMAC2D, EX_CALCS_O2 => CALCS_O2

      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER          , INTENT(IN   ) :: FORMK2,NPOIN,NTRAC
!      LOGICAL          , INTENT(IN   ) :: YATEMP  ! IF TEMPERATURE IS VARIABLE
      DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP
      DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44
!
      DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP
!
!     LOCAL VARIABLES
      INTEGER                     :: I,RANKTR1,RANKTR2
      INTEGER         , PARAMETER :: ADDTR = 3
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION, PARAMETER :: CORR1=1.065D0
      DOUBLE PRECISION, PARAMETER :: CORR2=1.0241D0
      DOUBLE PRECISION            :: UNORM,BENCORR,PJ,DAYTOSEC
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
      DAYTOSEC=1.D0/86400.D0
!      DEUXG   = 2.D0*GRAV
!
!   IF CS (O2SATU) IS CHANGING
!
      IF(FORMCS.NE.0)THEN
        IF(FORMCS.EQ.1)THEN
          O2SATU = 14.652D0 - 0.41022D0 * WATTEMP
     &         + 0.007991D0 * WATTEMP**2
     &         - 7.7774D-5 * WATTEMP**3
        ELSEIF(FORMCS.EQ.2)THEN
          IF(ABS(31.6D0+WATTEMP).GT.EPS)THEN
            O2SATU = 468.D0/(31.6D0+WATTEMP)
          ELSE
            O2SATU = 468.D0/EPS
          ENDIF
        ELSE
          IF(LNG.EQ.1)THEN
            WRITE(LU,100)FORMCS
          ELSE
            WRITE(LU,101)FORMCS
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!     COMPUTE K2
!
      IF(FORMK2.EQ.0)THEN ! ==> CONSTANT K2
        CALL OS('X=C     ',K2,K2,K2,K22)
      ELSE  ! ==> VARIABLE K2
        DO I =1,NPOIN
!
          UNORM = SQRT(UN%R(I)**2+VN%R(I)**2) !  GENERALIZATION OF U IN 1D
!         HEAD OR WATER DEPTH TO REPLACE RH ?
          PJ = HPROP%R(I)  !ZF%R(I)+HPROP%R(I)+UNORM**2/DEUXG
!         FORMULA OF THE TENESSEE VALLEY AUTHORITY
          IF( FORMK2.EQ.1 ) THEN
            K2%R(I) = 5.23D0*UNORM* MAX(HPROP%R(I),EPS)**(-1.67D0)
!         FORMULA OF OWENS ET AL.
          ELSEIF(FORMK2.EQ.2)THEN
            K2%R(I) = 5.33D0*(UNORM**0.67D0)*
     &              MAX(HPROP%R(I),EPS)**(-1.85D0)
!         FORMULA OF CHURCHILL ET AL.
          ELSEIF(FORMK2.EQ.3)THEN
            K2%R(I) = 0.746D0 * (UNORM**2.695D0) /
     &      (MAX(HPROP%R(I),EPS)**(-3.085D0) *
     &       MAX(ABS(PJ),EPS)**0.823D0) ! VERIFY THE FORMULA, SOME DOUBT ?
!         FORMULA OF O CONNOR & DOBBINS'
          ELSEIF(FORMK2.EQ.4)THEN
            K2%R(I) = (3.90D0 * UNORM**0.5D0 ) /
     &               MAX(HPROP%R(I),EPS)**(1.5D0)
!         FORMULA OF ?? INVISIBLE MAN :) : IT SEEMS TO BE A COMBINATION OF THE 3 LAST FORMULA ?!
          ELSEIF( FORMK2.EQ.5 ) THEN
            IF( HPROP%R(I).LE.0.6D0 ) THEN
              K2%R(I) = 5.33D0 * (UNORM**0.67D0) *
     &                MAX(HPROP%R(I),EPS)**(-1.85D0)
            ELSEIF (HPROP%R(I).LT.(12.D0*UNORM-6.6D0)) THEN
              K2%R(I) =  0.746D0*(UNORM**2.695D0)/
     &                (MAX(HPROP%R(I),EPS)**(-3.085D0) *
     &                 MAX(ABS(PJ),EPS)**0.823D0)
            ELSE
              K2%R(I) = 3.90D0 * (UNORM**0.5D0)/
     &                MAX(HPROP%R(I),EPS)**1.5D0
            ENDIF
          ELSE
            IF(LNG.EQ.1)THEN
              WRITE(LU,110)FORMK2
            ELSE
              WRITE(LU,111)FORMK2
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDIF
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
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR1)%P,C=DAYTOSEC)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR2)%P,C=DAYTOSEC)
      CALL OS('X=CX    ',X=TEXP%ADR(NTRAC  )%P,C=DAYTOSEC)
!
!
!     ERROR MESSAGES
!
100   FORMAT(1X,'FORMULE DE CS:',I3,/,1X, 'NON PROGRAMMEE')
101   FORMAT(1X,'CS FORMULA :',I3,/,1X, 'NOT AVAILABLE')
!
110   FORMAT(1X,'FORMULE DE K2:',I3,/,1X, 'NON PROGRAMMEE')
111   FORMAT(1X,'K2 FORMULA :',I3,/,1X, 'NOT AVAILABLE')
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
