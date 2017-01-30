!                       ******************
                        SUBROUTINE SPELIM
!                       ******************
!
     &( ENER  , OM00 , UV2D , VV2D , TETA , FREMAX, FETCH , SIGMAA ,
     &  SIGMAB , ALPHIL , GAMMA , FPIC  , HM0 , TETA1 , SPRED1, TETA2 ,
     &  SPRED2, XLAMDA, NPLAN , TYPSPE , FRABL, F0 , SPEULI , RESU ,
     &  FRA , DEPTH )
!
!**********************************************************************
!  COWADIS - V1.0    M. MARCOS               (EDF/DER/LNH)  -   6/03/97
!**********************************************************************
!
!  FONCTION : CALCUL DU SPECTRE DE VARIANCE EN UN POINT DONNE
!  **********
!
!  ARGUMENTS :
!  ***********
!  +-------------+----+--------------------------------------------+
!  ! NOM         !MODE! SIGNIFICATION - OBSERVATIONS               !
!  +-------------+----+--------------------------------------------+
!  ! ENER        !<-- ! ENERGIE INTEGREE SUR LES FREQUENCES        !
!  ! OM00        !<-- ! ENERGIE INTEGREE SUR LES FREQUENCES        !
!  ! UV2D        ! -->! COMPOSANTE OUEST-EST DU VENT  AU POINT2D   !
!  ! VV2D        ! -->! COMPOSANTE SUD-NORD  DU VENT  AU POINT2D   !
!  ! TETA(-)     ! -->! VECTEUR DES DIRECTIONS DE DISCRETISATION   !
!  ! FREMAX      ! -->! VALEUR MAXIMUM DE LA FREQUENCE DE PIC      !
!  ! FETCH       ! -->! FETCH MOYEN                                !
!  ! SIGMAA      ! -->! VALEUR DE SIGMA JONSWAP POUR F < FP        !
!  ! SIGMAB      ! -->! VALEUR DE SIGMA JONSWAP POUR F > FP        !
!  ! GAMMA       ! -->! FACTEUR DE FORME DE PIC JONSWAP            !
!  ! ALPHIL      ! -->! CONSTANTE DE PHILLIPS (ALPHA)              !
!  ! FPIC        ! -->! FREQUENCE DE PIC JONSWAP                   !
!  ! HM0         ! -->! HAUTEUR SIGNIFICATIVE JONSWAP              !
!  ! TETA1       ! -->! DIRECTION PRINCIPALE 1 POUR FRA            !
!  ! SPRED1      ! -->! ETALEMENT DIRECTIONNEL 1 POUR FRA          !
!  ! TETA2       ! -->! DIRECTION PRINCIPALE 2 POUR FRA            !
!  ! SPRED2      ! -->! ETALEMENT DIRECTIONNEL 2 POUR FRA          !
!  ! XLAMDA      ! -->! FACTEUR DE PONDERATION POUR LA FRA         !
!  ! NPLAN       ! -->! NOMBRE DE DIRECTIONS DE DISCRETISATION     !
!  ! TYPSPE      ! -->! INDICATEUR DU TYPE DU SPECTRE              !
!  ! F0          ! -->! FREQUENCE MOYENNE INITIALE                 !
!  ! SPEULI      ! -->! LOGIQUE INDIQUANT LA MODIF DU SPECTRE PAR  !
!  !             !    ! L'UTILISATEUR                              !
!  ! RESU        !<-->! TABLEAU DE TRAVAIL                         !
!  ! FRA         !<-->! TABLEAU DE TRAVAIL                         !
!  +-------------+----+--------------------------------------------+
!  ! MODE   (-> : NON-MODIFIE)  (<-> : MODIFIE)  (<- : INITIALISE) !
!  +---------------------------------------------------------------+
!
!  APPELS :    - PROGRAMME(S) APPELANT  :  CDICOW, LIMCOW
!  ********    - PROGRAMME(S) APPELE(S) :  SPEINI
!
!  REMARQUES :
!  ***********
!  -
!
!**********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
      INTEGER NF
!
      PARAMETER (NF = 25 )
!
!.....VARIABLES TRANSMISES
!     """"""""""""""""""""
      INTEGER  NPLAN , NPOIN2 , TYPSPE , IP2D, FRABL
      DOUBLE PRECISION ENER(NPLAN) , OM00(NPLAN)
      DOUBLE PRECISION GRAVIT, FREMAX, FETCH , SIGMAA, SIGMAB, GAMMA
      DOUBLE PRECISION FPIC  , HM0   , ALPHIL, TETA1 , SPRED1, TETA2
      DOUBLE PRECISION SPRED2, XLAMDA, E2FMIN
      DOUBLE PRECISION TETA(NPLAN),SPEC(NF),RESU(NPLAN,NF),FRA(NPLAN)
      DOUBLE PRECISION UV2D(1) , VV2D(1) , DEPTH(1), F0
      LOGICAL SPEULI
!
!.....VARIABLES LOCALES
!     """""""""""""""""
      INTEGER IFREQ,IPLAN
      DOUBLE PRECISION DFREQ(NF) , FREQ(NF) , DEUPI, DTETA
      DOUBLE PRECISION, ALLOCATABLE :: TRAV(:)
!
!
      DEUPI = 2.D0*3.141592654D0
!
!     ===========================================================
!     CALCUL DES GRANDEURS FREQUENTIELLES
!     ===========================================================
      DO IFREQ = 1,NF
        FREQ(IFREQ) = FPIC*1.1D0**(IFREQ-12)
      ENDDO
!
      DFREQ(1)=5.D-2*FREQ(1)
      DFREQ(NF)=5.D-2*FREQ(NF-1)
      DO IFREQ = 2,NF-1
        DFREQ(IFREQ) = 5.D-2*(FREQ(IFREQ)+FREQ(IFREQ-1))
      ENDDO
!
!     ===========================================================
!     APPEL A SPEINI
!     ===========================================================
      GRAVIT = 9.81D0
      E2FMIN = 1.D-30
!
      CALL SPEINI
     &( RESU , SPEC , FRA , UV2D , VV2D , FREQ , TETA , GRAVIT,
     &  FREMAX , FETCH , SIGMAA, SIGMAB, GAMMA , FPIC  , HM0 , ALPHIL,
     &  TETA1 , SPRED1, TETA2 , SPRED2, XLAMDA, 1, NPLAN , NF    ,
     &  TYPSPE, E2FMIN, DEPTH , FRABL )
!
!     ===========================================================
!     ZONE UTILISATEUR - ON PEUT Y MODIFIER RESU
!     ===========================================================
      IF (SPEULI) THEN
!
!   EXEMPLE DE MODIFICATION DE FRA - A MODIFIER SUIVANT VOTRE CAS
!   EXAMPLE OF MODIFICATION OF FRA - TO BE MODIFIED DEPENDING
!     ON YOUR CASE
!
        ALLOCATE(TRAV(1:NF))
        DO IFREQ=1,NF
          IF (FREQ(IFREQ).LT.FPIC) THEN
            TRAV(IFREQ)=0.4538D0*(FREQ(IFREQ)/FPIC)**(-2.03D0)
          ELSE
            TRAV(IFREQ)=0.4538D0*(FREQ(IFREQ)/FPIC)**(1.04D0)
          ENDIF
        ENDDO
!
        DO IPLAN=1,NPLAN
          DTETA=TETA(IPLAN)-TETA1
          IF ((TETA(IPLAN)-TETA1).GT.DEUPI/2) THEN
            DTETA=DEUPI-DTETA
          ENDIF
          DO IFREQ=1,NF
            FRA(IPLAN)=1.D0/SQRT(DEUPI)*TRAV(IFREQ)*
     &                    EXP(-DTETA**2/(2.D0*TRAV(IFREQ)**2))
            RESU(IPLAN,IFREQ)= SPEC(IFREQ)*FRA(IPLAN)
          ENDDO
        ENDDO
        DEALLOCATE(TRAV)

      ENDIF
!
!     ===========================================================
!     INTEGRATION SUR LES FREQUENCES
!     ===========================================================
      DO IPLAN = 1,NPLAN
        ENER(IPLAN) = 0.D0
        OM00(IPLAN) = 0.D0
        DO IFREQ = 1,NF
          ENER(IPLAN) = ENER(IPLAN) +
     &              RESU(IPLAN,IFREQ)*DFREQ(IFREQ)
          OM00(IPLAN) = OM00(IPLAN) +
     &                 RESU(IPLAN,IFREQ)*DFREQ(IFREQ)*FREQ(IFREQ)
!    &                 RESU(IPLAN,IFREQ)*DFREQ(IFREQ)/FREQ(IFREQ)
        ENDDO
        IF (ENER(IPLAN).GT.1.D-10) THEN
          OM00(IPLAN) = DEUPI * OM00(IPLAN) / ENER(IPLAN)
!         OM00(IPLAN) = DEUPI / OM00(IPLAN) * ENER(IPLAN)
        ELSE
          OM00(IPLAN) = DEUPI * F0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

