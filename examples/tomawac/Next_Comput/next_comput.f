


!======================================================================C
!  Fichier FORTRAN de cas-test de validation de TOMAWAC V5P6.          C
!                                                                      C
!  Nom du cas : PROPAGATION SUR COURANT TOURBILLONNAIRE.               C
!                                                                      C
!  Ce fichier contient les subroutines suivantes :                     C
!   ANACOS : specification d'un champ de courant analytique.           C
!   TOM_CORFON : la cote du fond est fixee a -200 m.                   C
!   CORRXY : dilatation du maillage lu sur le fichier GEO.             C
!   SPELIM :                                                           C
!   LIMWAC : affectation du spectre de houle aux limites.              C
!                                                                      C
!   Faire une recherche de CMB pour trouver les parties modifiees.     C
!                                                                      C
!                          Michel BENOIT (EDF R&D LNHE)   22/12/2005   C
!======================================================================C
!
!                       *****************
                        SUBROUTINE ANACOS
!                       *****************
!
     &( UC    , VC    , X     , Y     , NPOIN2 )
!
!***********************************************************************
!  TOMAWAC VERSION 5.2    07/06/01
!***********************************************************************
!
!     FONCTION  : PERMET LA SPECIFICATION D'UN COURANT ANALYTIQUE
!
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !    UC,VC       !<-- ! COMPOSANTES DU CHAMP DE COURANT              !
! !    X,Y         ! -->! COORDONNEES DES POINTS DU MAILLAGE 2D        !
! !    NPOIN2      ! -->! NOMBRE DE POINTS 2D                          !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : CONDIW
!
!  SOUS-PROGRAMME APPELE : NEANT
!
!***********************************************************************
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!.....VARIABLES TRANSMISES
!     """"""""""""""""""""
      INTEGER  NPOIN2
      DOUBLE PRECISION X (NPOIN2) , Y (NPOIN2)
      DOUBLE PRECISION UC(NPOIN2) , VC(NPOIN2)
!
!MB--------------------------------------Modif debut
!
!.....VARIABLES LOCALES
!     """""""""""""""""
      INTEGER  II
      DOUBLE PRECISION R0, RR, B, R1, UM, UU1, UR
!
!
      R0=10.D3
      B=0.3
      R1=R0/2.D0*(1+SQRT(1-2*B**2))
      UM=1.D0
      UU1=UM*EXP(-((R1-R0)/B/R0)**2)
!
      DO II=1,NPOIN2
        RR=SQRT(X(II)**2+Y(II)**2)
        IF (RR.LE.R1) THEN
          UR=UU1*RR/R1
        ELSE
          UR=UM*EXP(-((RR-R0)/(B*R0))**2)
        ENDIF
        IF (RR.EQ.0.D0) THEN
          UC(II)=0.D0
          VC(II)=0.D0
        ELSE
          UC(II)=UR*Y(II)/RR
          VC(II)=-UR*X(II)/RR
        ENDIF
      ENDDO
!MB--------------------------------------Modif fin
!
      RETURN
      END
!                       *****************
                        SUBROUTINE TOM_CORFON
!                       *****************
!
!***********************************************************************
! PROGICIEL : COWADIS           26/07/99           F.MARCOS
!***********************************************************************
!
!  USER SUBROUTINE TOM_CORFON
!
!  FONCTION  : MODIFICATION DE LA TOPOGRAPHIE
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
!
      USE DECLARATIONS_TOMAWAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!MB--------------------------------------Modif debut
      INTEGER IP
!
      DO IP=1,NPOIN2
        ZF(IP)=-200.D0
      ENDDO
!MB--------------------------------------Modif fin
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CORRXY
!                       *****************
     & (X,Y,NPOIN)
!
!***********************************************************************
! PROGICIEL : BIEF 5.0          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORRXY
!
!  FUNCTION  : MODIFICATION OF THE COORDINATES OF THE POINTS IN THE MESH
!
!              LINES WITH AN INITIAL CEX ARE AN EXAMPLE
!              WITH TELEMAC-2D
!
!              THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
!              THE CALLING PROGRAM AND THE NEEDED MODIFICATION
!              BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
!              ALL THE DATA STRUCTURE OF THIS CODE IS
!              AVAILABLE
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED IN THE EXAMPLE
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |    X,Y         | -->|  COORDONNEES DU MAILLAGE .                   |
! |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
! APPELE PAR : INBIEF
!
! SOUS-PROGRAMME APPELE : NEANT
!
!***********************************************************************
!
      USE BIEF, EX_CORRXY => CORRXY
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
!
!MB--------------------------------------Modif debut
      INTEGER IP
!
      DO IP=1,NPOIN
        X(IP)=(X(IP)-10.D0)*4.D3
        Y(IP)=(Y(IP)-0.5D0)*4.D3
      ENDDO
!MB--------------------------------------Modif fin
!
      RETURN
      END
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
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
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
!                       *****************
                        SUBROUTINE LIMWAC
!                       *****************
!
     &(F     , FBOR  , LIFBOR, NPTFR , NPLAN , NF    ,  TETA , FREQ  ,
     & NPOIN2, NBOR  , AT    , LT    , DDC   , LIMSPE, FPMAXL, FETCHL,
     & SIGMAL, SIGMBL, GAMMAL, FPICL , HM0L  , APHILL, TETA1L, SPRE1L,
     & TETA2L, SPRE2L, XLAMDL, X ,Y  , KENT  , KSORT , NFO1  , NBI1  ,
     & BINBI1, UV    , VV    , SPEULI, VENT  , VENSTA, GRAVIT, DEUPI ,
     & PRIVE , NPRIV , SPEC  , FRA   , DEPTH , FRABL ,BOUNDARY_COLOUR)
!
!***********************************************************************
! TOMAWAC   V1.0            01/02/95        F. MARCOS  (LNH) 30 87 72 66
!***********************************************************************
!
!      FONCTION:
!      =========
!
!    CONDITIONS AUX LIMITES
!
!    ATTENTION
!    PAR DEFAUT, ON DUPLIQUE SUR L'ENSEMBLE DES DIRECTIONS ET DES
!    FREQUENCES LA CONDITION A LA LIMITE DONNEE DANS LE FICHIER DYNAM
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !    F           ! -->!  DENSITE SPECTRALE                           !
! !    FBOR        !<-->!  DENSITE SPECTRALE AU BORD                   !
! !    LIFBOR      ! -->!  TYPE DE CONDITION LIMITE SUR F              !
! !    NPTFR       ! -->!  NOMBRE DE POINTS FRONTIERE 2D               !
! !    NPLAN       ! -->!  NOMBRE DE DIRECTIONS                        !
! !    NF          ! -->!  NOMBRE DE FREQUENCES                        !
! !    TETA        ! -->! DIRECTIONS DE PROPAGATION                    !
! !    FREQ        ! -->! FREQUENCES DISCRETISEES                      !
! !    NPOIN2      ! -->!  NOMBRE DE POINTS 2D                         !
! !    NBOR        ! -->!  NUMEROTATION DES POINTS DE BORD 2D          !
! !    AT          ! -->!  TEMPS                                       !
! !    LT          ! -->!  NUMERO DU PAS DE TEMPS                      !
! !    DDC         ! -->!  DATE DU DEBUT DU CALCUL                     !
! !    X           ! -->!  ABSCISSES DES POINTS 2D                     !
! !    Y           ! -->!  ORDONNEES DES POINTS 2D                     !
! !    KENT        ! -->!  C.L. INDIQUANT UNE FRONTIERE MARITIME       !
! !    KSORT       ! -->!  C.L. INDIQUANT UNE FRONTIERE SOLIDE         !
! !    NFO1        ! -->!  NUMERO DU FICHIER FORMATE UTILISATEUR       !
! !    NBI1        ! -->!  NUMERO DU FICHIER BINAIRE UTILISATEUR       !
! !    BINBI1      ! -->!  BINAIRE DU FICHIER BINAIRE UTILISATEUR      !
! !    PRIVE       ! -->!  TABLEAU DE L'UTILISATEUR                    !
! !    NPRIV       ! -->!  DIMENSION DU TABLEAU PRIVE                  !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : WAC
!
!***********************************************************************
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NPLAN,NF,NPOIN2,NPTFR,LT,NPRIV
!
      INTEGER, INTENT(IN) :: BOUNDARY_COLOUR(NPTFR)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF),X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION FBOR(NPTFR,NPLAN,NF),TETA(NPLAN),FREQ(NF)
      DOUBLE PRECISION UV(NPOIN2),VV(NPOIN2), SPEC(NF), FRA(NPLAN)
      DOUBLE PRECISION PRIVE(NPOIN2,NPRIV),DDC, DEPTH(NPOIN2)
      DOUBLE PRECISION HM0L,FPICL,GAMMAL,SIGMAL,SIGMBL,APHILL,FETCHL
      DOUBLE PRECISION FPMAXL,TETA1L,SPRE1L,TETA2L,SPRE2L,XLAMDL
      DOUBLE PRECISION GRAVIT,DEUPI,E2FMIN
!
      DOUBLE PRECISION AT
!
      LOGICAL SPEULI, VENT, VENSTA
!
      INTEGER NBOR(NPTFR),LIFBOR(NPTFR),NFO1,NBI1,IFREQ
      INTEGER KENT,KSORT,IFF,IPLAN,IPTFR,LIMSPE,FRABL
!
      DOUBLE PRECISION, ALLOCATABLE :: TRAV(:)
      DOUBLE PRECISION, ALLOCATABLE :: UV2D(:),VV2D(:),PROF(:)
!
      DOUBLE PRECISION S00, DTETA
      DOUBLE PRECISION HM0   , AL    , FP    , GAMMA , SIGMAA, SIGMAB
      DOUBLE PRECISION DEGRAD, FPMIN
      LOGICAL FLAG
!
      CHARACTER*3 BINBI1
!
      SAVE UV2D,VV2D,PROF
!
!***********************************************************************
!
!   MODIFICATION EVENTUELLE DU TYPE DE CONDITION A LA LIMITE
!
!   A REMPLIR PAR L'UTILISATEUR
!
!   LIFBOR(IPTFR)=KENT OU KSORT
!
      IF (LIMSPE.EQ.0 .AND. .NOT.SPEULI) RETURN
      FLAG=.FALSE.
      IF (VENT .AND. (LIMSPE.EQ.1 .OR. LIMSPE.EQ.2 .OR. LIMSPE.EQ.3
     & .OR. LIMSPE.EQ.5)) FLAG=.TRUE.
!
!     AU PREMIER PASSAGE, ON ALLOUE DE LA MEMOIRE AUX TABLEAUX UTILES
!     ---------------------------------------------------------------
      IF (LT.LT.1) THEN
        IF (FLAG) THEN
          ALLOCATE(UV2D(1:NPTFR),VV2D(1:NPTFR))
        ENDIF
        IF (LIMSPE.EQ.7 .OR. SPEULI) THEN
          ALLOCATE(PROF(1:NPTFR))
        ENDIF
      ENDIF
!
!     AU PREMIER PASSAGE (ET EVENTUELLEMENT AUX AUTRES SI LE VENT EST
!     INSTATIONNAIRE ET QUE LE SPECTRE A LA LIMITE EN DEPEND),
!     ON CALCULE LE SPECTRE AUX LIMITES
!     ----------------------------------------------------------------
      IF (LT.LT.1) THEN
      DEUPI=2.D0*3.141592654D0
      DEGRAD=DEUPI/360.D0
      GRAVIT=9.81D0
      E2FMIN=1.D-30
      FPMIN =1.D-10
!
      HM0    = 1
      FP     = 0.1D0
      GAMMA  = 1.5D0
      SIGMAA = 0.07D0
      SIGMAB = 0.09D0
      AL=0.0624D0/(0.230D0+0.0336D0*GAMMA-0.185D0/(1.9D0+GAMMA))
     &  *(DEUPI*FP)**4*HM0*HM0/GRAVIT**2
!
      CALL SPEJON
     &( SPEC , FREQ  , NF , AL , FP    , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
!
!     ===========================================================
!     ZONE UTILISATEUR - ON PEUT Y MODIFIER RESU
!     ===========================================================
        IF (SPEULI) THEN
!
!        EXEMPLE DE MODIFICATION DE FRA - A MODIFIER SUIVANT VOTRE CAS
!        EXAMPLE OF MODIFICATION OF FRA - TO BE MODIFIED DEPENDING
!        ON YOUR CASE
        ALLOCATE(TRAV(1:NF))
        S00=0.4538D0
        DO IFREQ=1,NF
          IF (FREQ(IFREQ).LT.FPICL) THEN
            TRAV(IFREQ)=S00*(FREQ(IFREQ)/FPICL)**(-2.03D0)
          ELSE
            TRAV(IFREQ)=S00*(FREQ(IFREQ)/FPICL)**(1.04D0)
          ENDIF
        ENDDO
!
        DO IPTFR=1,NPTFR
          IF (LIFBOR(IPTFR).EQ.KENT) THEN
            DO IPLAN=1,NPLAN
              DTETA=TETA(IPLAN)-TETA1L
              IF ((TETA(IPLAN)-TETA1L).GT.DEUPI/2) THEN
                DTETA=DEUPI-DTETA
              ENDIF
              DO IFREQ=1,NF
                FRA(IPLAN)=1.D0/SQRT(DEUPI)*TRAV(IFREQ)*
     &          EXP(-DTETA**2/(2.D0*TRAV(IFREQ)**2))
                F(NBOR(IPTFR),IPLAN,IFREQ)=SPEC(IFREQ)*FRA(IPLAN)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
!
        DEALLOCATE(TRAV)
!
        ENDIF
!
!     ===========================================================
!     FIN DE LA ZONE UTILISATEUR
!     ===========================================================
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
