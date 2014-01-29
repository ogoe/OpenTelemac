


C======================================================================C
C  Fichier FORTRAN de cas-test de validation de TOMAWAC V5P6.          C
C                                                                      C
C  Nom du cas : PROPAGATION SUR COURANT TOURBILLONNAIRE.               C
C                                                                      C
C  Ce fichier contient les subroutines suivantes :                     C
C   ANACOS : specification d'un champ de courant analytique.           C
C   TOM_CORFON : la cote du fond est fixee a -200 m.                       C
C   CORRXY : dilatation du maillage lu sur le fichier GEO.             C
C   SPELIM :                                                           C
C   LIMWAC : affectation du spectre de houle aux limites.              C
C                                                                      C
C   Faire une recherche de CMB pour trouver les parties modifiees.     C
C                                                                      C
C                          Michel BENOIT (EDF R&D LNHE)   22/12/2005   C
C======================================================================C
C
C                       *****************
                        SUBROUTINE ANACOS
C                       *****************
C
     *( UC    , VC    , X     , Y     , NPOIN2 ) 
C
C***********************************************************************
C  TOMAWAC VERSION 5.2    07/06/01       
C***********************************************************************
C
C     FONCTION  : PERMET LA SPECIFICATION D'UN COURANT ANALYTIQUE
C                 
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !    UC,VC       !<-- ! COMPOSANTES DU CHAMP DE COURANT              !
C !    X,Y         ! -->! COORDONNEES DES POINTS DU MAILLAGE 2D        !
C !    NPOIN2      ! -->! NOMBRE DE POINTS 2D                          !
C !________________!____!______________________________________________!
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : CONDIW
C
C  SOUS-PROGRAMME APPELE : NEANT
C
C***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
C.....VARIABLES TRANSMISES
C     """"""""""""""""""""
      INTEGER  NPOIN2
      DOUBLE PRECISION X (NPOIN2) , Y (NPOIN2)
      DOUBLE PRECISION UC(NPOIN2) , VC(NPOIN2)
C
CMB--------------------------------------Modif debut
C
C.....VARIABLES LOCALES
C     """""""""""""""""
      INTEGER  II
      DOUBLE PRECISION R0, RR, B, R1, UM, UU1, UR
C
C
      R0=10.D3
      B=0.3
      R1=R0/2.D0*(1+SQRT(1-2*B**2))
      UM=1.D0
      UU1=UM*EXP(-((R1-R0)/B/R0)**2)
C
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
CMB--------------------------------------Modif fin
C
      RETURN
      END
C                       *****************
                        SUBROUTINE TOM_CORFON
C                       *****************
C
C***********************************************************************
C PROGICIEL : COWADIS           26/07/99           F.MARCOS
C***********************************************************************
C
C  USER SUBROUTINE TOM_CORFON
C
C  FONCTION  : MODIFICATION DE LA TOPOGRAPHIE
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
      USE BIEF
C
      USE DECLARATIONS_TOMAWAC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
CMB--------------------------------------Modif debut
      INTEGER IP
C
      DO IP=1,NPOIN2
        ZF(IP)=-200.D0
      ENDDO
CMB--------------------------------------Modif fin
C
      RETURN
      END                  
C                       *****************
                        SUBROUTINE CORRXY
C                       *****************
     * (X,Y,NPOIN)
C
C***********************************************************************
C PROGICIEL : BIEF 5.0          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORRXY
C
C  FUNCTION  : MODIFICATION OF THE COORDINATES OF THE POINTS IN THE MESH
C
C              LINES WITH AN INITIAL CEX ARE AN EXAMPLE
C              WITH TELEMAC-2D
C
C              THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
C              THE CALLING PROGRAM AND THE NEEDED MODIFICATION
C              BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
C              ALL THE DATA STRUCTURE OF THIS CODE IS
C              AVAILABLE
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED IN THE EXAMPLE 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |    X,Y         | -->|  COORDONNEES DU MAILLAGE .                   |
C |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
C APPELE PAR : INBIEF
C
C SOUS-PROGRAMME APPELE : NEANT
C
C***********************************************************************
C
      USE BIEF, EX_CORRXY => CORRXY
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
C
CMB--------------------------------------Modif debut
      INTEGER IP
C
      DO IP=1,NPOIN
        X(IP)=(X(IP)-10.D0)*4.D3
        Y(IP)=(Y(IP)-0.5D0)*4.D3
      ENDDO
CMB--------------------------------------Modif fin
C
      RETURN
      END 
C                       ******************
                        SUBROUTINE SPELIM
C                       ******************
C
     *( ENER  , OM00 , UV2D , VV2D , TETA , FREMAX, FETCH , SIGMAA ,
     *  SIGMAB , ALPHIL , GAMMA , FPIC  , HM0 , TETA1 , SPRED1, TETA2 ,
     *  SPRED2, XLAMDA, NPLAN , TYPSPE , FRABL, F0 , SPEULI , RESU ,
     *  FRA , DEPTH )
C
C**********************************************************************
C  COWADIS - V1.0    M. MARCOS               (EDF/DER/LNH)  -   6/03/97
C**********************************************************************
C
C  FONCTION : CALCUL DU SPECTRE DE VARIANCE EN UN POINT DONNE
C  **********
C
C  ARGUMENTS :
C  ***********
C  +-------------+----+--------------------------------------------+
C  ! NOM         !MODE! SIGNIFICATION - OBSERVATIONS               !
C  +-------------+----+--------------------------------------------+
C  ! ENER        !<-- ! ENERGIE INTEGREE SUR LES FREQUENCES        !
C  ! OM00        !<-- ! ENERGIE INTEGREE SUR LES FREQUENCES        !
C  ! UV2D        ! -->! COMPOSANTE OUEST-EST DU VENT  AU POINT2D   !
C  ! VV2D        ! -->! COMPOSANTE SUD-NORD  DU VENT  AU POINT2D   !
C  ! TETA(-)     ! -->! VECTEUR DES DIRECTIONS DE DISCRETISATION   !
C  ! FREMAX      ! -->! VALEUR MAXIMUM DE LA FREQUENCE DE PIC      !
C  ! FETCH       ! -->! FETCH MOYEN                                !
C  ! SIGMAA      ! -->! VALEUR DE SIGMA JONSWAP POUR F < FP        !
C  ! SIGMAB      ! -->! VALEUR DE SIGMA JONSWAP POUR F > FP        !
C  ! GAMMA       ! -->! FACTEUR DE FORME DE PIC JONSWAP            !
C  ! ALPHIL      ! -->! CONSTANTE DE PHILLIPS (ALPHA)              !
C  ! FPIC        ! -->! FREQUENCE DE PIC JONSWAP                   !
C  ! HM0         ! -->! HAUTEUR SIGNIFICATIVE JONSWAP              !
C  ! TETA1       ! -->! DIRECTION PRINCIPALE 1 POUR FRA            !
C  ! SPRED1      ! -->! ETALEMENT DIRECTIONNEL 1 POUR FRA          !
C  ! TETA2       ! -->! DIRECTION PRINCIPALE 2 POUR FRA            !
C  ! SPRED2      ! -->! ETALEMENT DIRECTIONNEL 2 POUR FRA          !
C  ! XLAMDA      ! -->! FACTEUR DE PONDERATION POUR LA FRA         !
C  ! NPLAN       ! -->! NOMBRE DE DIRECTIONS DE DISCRETISATION     !
C  ! TYPSPE      ! -->! INDICATEUR DU TYPE DU SPECTRE              !
C  ! F0          ! -->! FREQUENCE MOYENNE INITIALE                 !
C  ! SPEULI      ! -->! LOGIQUE INDIQUANT LA MODIF DU SPECTRE PAR  !
C  !             !    ! L'UTILISATEUR                              !
C  ! RESU        !<-->! TABLEAU DE TRAVAIL                         !
C  ! FRA         !<-->! TABLEAU DE TRAVAIL                         !
C  +-------------+----+--------------------------------------------+
C  ! MODE   (-> : NON-MODIFIE)  (<-> : MODIFIE)  (<- : INITIALISE) !
C  +---------------------------------------------------------------+
C
C  APPELS :    - PROGRAMME(S) APPELANT  :  CDICOW, LIMCOW
C  ********    - PROGRAMME(S) APPELE(S) :  SPEINI      
C
C  REMARQUES :
C  ***********
C  - 
C
C**********************************************************************
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NF
C
      PARAMETER (NF = 25 )
C
C.....VARIABLES TRANSMISES
C     """"""""""""""""""""
      INTEGER  NPLAN , NPOIN2 , TYPSPE , IP2D, FRABL
      DOUBLE PRECISION ENER(NPLAN) , OM00(NPLAN)
      DOUBLE PRECISION GRAVIT, FREMAX, FETCH , SIGMAA, SIGMAB, GAMMA
      DOUBLE PRECISION FPIC  , HM0   , ALPHIL, TETA1 , SPRED1, TETA2
      DOUBLE PRECISION SPRED2, XLAMDA, E2FMIN
      DOUBLE PRECISION TETA(NPLAN),SPEC(NF),RESU(NPLAN,NF),FRA(NPLAN)
      DOUBLE PRECISION UV2D(1) , VV2D(1) , DEPTH(1), F0
      LOGICAL SPEULI
C
C.....VARIABLES LOCALES 
C     """""""""""""""""
      INTEGER IFREQ,IPLAN
      DOUBLE PRECISION DFREQ(NF) , FREQ(NF) , DEUPI, DTETA
      DOUBLE PRECISION, ALLOCATABLE :: TRAV(:)
C
C
      DEUPI = 2.D0*3.141592654D0
C
C     ===========================================================
C     CALCUL DES GRANDEURS FREQUENTIELLES
C     ===========================================================
      DO IFREQ = 1,NF
         FREQ(IFREQ) = FPIC*1.1D0**(IFREQ-12)
      ENDDO
C
      DFREQ(1)=5.D-2*FREQ(1)
      DFREQ(NF)=5.D-2*FREQ(NF-1)
      DO IFREQ = 2,NF-1
          DFREQ(IFREQ) = 5.D-2*(FREQ(IFREQ)+FREQ(IFREQ-1))
      ENDDO
C
C     ===========================================================
C     APPEL A SPEINI
C     ===========================================================
      GRAVIT = 9.81D0
      E2FMIN = 1.D-30
C
      CALL SPEINI
     *( RESU , SPEC , FRA , UV2D , VV2D , FREQ , TETA , GRAVIT,
     *  FREMAX , FETCH , SIGMAA, SIGMAB, GAMMA , FPIC  , HM0 , ALPHIL,
     *  TETA1 , SPRED1, TETA2 , SPRED2, XLAMDA, 1, NPLAN , NF    ,
     *  TYPSPE, E2FMIN, DEPTH , FRABL )
C
C     ===========================================================
C     ZONE UTILISATEUR - ON PEUT Y MODIFIER RESU
C     ===========================================================
      IF (SPEULI) THEN
C
C   EXEMPLE DE MODIFICATION DE FRA - A MODIFIER SUIVANT VOTRE CAS
C   EXAMPLE OF MODIFICATION OF FRA - TO BE MODIFIED DEPENDING 
C     ON YOUR CASE
C
        ALLOCATE(TRAV(1:NF))
        DO IFREQ=1,NF
             IF (FREQ(IFREQ).LT.FPIC) THEN
              TRAV(IFREQ)=0.4538D0*(FREQ(IFREQ)/FPIC)**(-2.03D0)
           ELSE
               TRAV(IFREQ)=0.4538D0*(FREQ(IFREQ)/FPIC)**(1.04D0)
           ENDIF
        ENDDO
C
        DO IPLAN=1,NPLAN
             DTETA=TETA(IPLAN)-TETA1
           IF ((TETA(IPLAN)-TETA1).GT.DEUPI/2) THEN
               DTETA=DEUPI-DTETA
            ENDIF
           DO IFREQ=1,NF
               FRA(IPLAN)=1.D0/SQRT(DEUPI)*TRAV(IFREQ)*
     *                       EXP(-DTETA**2/(2.D0*TRAV(IFREQ)**2))
               RESU(IPLAN,IFREQ)= SPEC(IFREQ)*FRA(IPLAN)
           ENDDO
        ENDDO
        DEALLOCATE(TRAV)

      ENDIF
C
C     ===========================================================
C     INTEGRATION SUR LES FREQUENCES
C     ===========================================================
      DO IPLAN = 1,NPLAN
         ENER(IPLAN) = 0.D0
         OM00(IPLAN) = 0.D0
         DO IFREQ = 1,NF
            ENER(IPLAN) = ENER(IPLAN) + 
     *                RESU(IPLAN,IFREQ)*DFREQ(IFREQ)
            OM00(IPLAN) = OM00(IPLAN) + 
     *                   RESU(IPLAN,IFREQ)*DFREQ(IFREQ)*FREQ(IFREQ)
C    *                   RESU(IPLAN,IFREQ)*DFREQ(IFREQ)/FREQ(IFREQ)
         ENDDO
         IF (ENER(IPLAN).GT.1.D-10) THEN
              OM00(IPLAN) = DEUPI * OM00(IPLAN) / ENER(IPLAN)
C             OM00(IPLAN) = DEUPI / OM00(IPLAN) * ENER(IPLAN)
         ELSE
             OM00(IPLAN) = DEUPI * F0
         ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C                       *****************
                        SUBROUTINE LIMWAC
C                       *****************
C
     *(F     , FBOR  , LIFBOR, NPTFR , NPLAN , NF    ,  TETA , FREQ  ,
     * NPOIN2, NBOR  , AT    , LT    , DDC   , LIMSPE, FPMAXL, FETCHL,
     * SIGMAL, SIGMBL, GAMMAL, FPICL , HM0L  , APHILL, TETA1L, SPRE1L,
     * TETA2L, SPRE2L, XLAMDL, X ,Y  , KENT  , KSORT , NFO1  , NBI1  ,
     * BINBI1, UV    , VV    , SPEULI, VENT  , VENSTA, GRAVIT, DEUPI , 
     * PRIVE , NPRIV , SPEC  , FRA   , DEPTH , FRABL ,BOUNDARY_COLOUR)
C
C***********************************************************************
C TOMAWAC   V1.0            01/02/95        F. MARCOS  (LNH) 30 87 72 66
C***********************************************************************
C
C      FONCTION:
C      =========
C
C    CONDITIONS AUX LIMITES
C
C    ATTENTION
C    PAR DEFAUT, ON DUPLIQUE SUR L'ENSEMBLE DES DIRECTIONS ET DES
C    FREQUENCES LA CONDITION A LA LIMITE DONNEE DANS LE FICHIER DYNAM
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !    F           ! -->!  DENSITE SPECTRALE                           !
C !    FBOR        !<-->!  DENSITE SPECTRALE AU BORD                   !
C !    LIFBOR      ! -->!  TYPE DE CONDITION LIMITE SUR F              !
C !    NPTFR       ! -->!  NOMBRE DE POINTS FRONTIERE 2D               !
C !    NPLAN       ! -->!  NOMBRE DE DIRECTIONS                        !
C !    NF          ! -->!  NOMBRE DE FREQUENCES                        !
C !    TETA        ! -->! DIRECTIONS DE PROPAGATION                    !
C !    FREQ        ! -->! FREQUENCES DISCRETISEES                      !
C !    NPOIN2      ! -->!  NOMBRE DE POINTS 2D                         !
C !    NBOR        ! -->!  NUMEROTATION DES POINTS DE BORD 2D          !
C !    AT          ! -->!  TEMPS                                       !
C !    LT          ! -->!  NUMERO DU PAS DE TEMPS                      !
C !    DDC         ! -->!  DATE DU DEBUT DU CALCUL                     !
C !    X           ! -->!  ABSCISSES DES POINTS 2D                     !
C !    Y           ! -->!  ORDONNEES DES POINTS 2D                     !
C !    KENT        ! -->!  C.L. INDIQUANT UNE FRONTIERE MARITIME       !
C !    KSORT       ! -->!  C.L. INDIQUANT UNE FRONTIERE SOLIDE         !
C !    NFO1        ! -->!  NUMERO DU FICHIER FORMATE UTILISATEUR       !
C !    NBI1        ! -->!  NUMERO DU FICHIER BINAIRE UTILISATEUR       !
C !    BINBI1      ! -->!  BINAIRE DU FICHIER BINAIRE UTILISATEUR      !
C !    PRIVE       ! -->!  TABLEAU DE L'UTILISATEUR                    !
C !    NPRIV       ! -->!  DIMENSION DU TABLEAU PRIVE                  !
C !________________!____!______________________________________________!
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C SOUS-PROGRAMME APPELE PAR : WAC
C
C***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NPLAN,NF,NPOIN2,NPTFR,LT,NPRIV
C
      INTEGER, INTENT(IN) :: BOUNDARY_COLOUR(NPTFR)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF),X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION FBOR(NPTFR,NPLAN,NF),TETA(NPLAN),FREQ(NF)
      DOUBLE PRECISION UV(NPOIN2),VV(NPOIN2), SPEC(NF), FRA(NPLAN)
      DOUBLE PRECISION PRIVE(NPOIN2,NPRIV),DDC, DEPTH(NPOIN2)
      DOUBLE PRECISION HM0L,FPICL,GAMMAL,SIGMAL,SIGMBL,APHILL,FETCHL
      DOUBLE PRECISION FPMAXL,TETA1L,SPRE1L,TETA2L,SPRE2L,XLAMDL
      DOUBLE PRECISION GRAVIT,DEUPI,E2FMIN
C
      DOUBLE PRECISION AT
C
      LOGICAL SPEULI, VENT, VENSTA
C
      INTEGER NBOR(NPTFR),LIFBOR(NPTFR),NFO1,NBI1,IFREQ
      INTEGER KENT,KSORT,IFF,IPLAN,IPTFR,LIMSPE,FRABL
C
      DOUBLE PRECISION, ALLOCATABLE :: TRAV(:)
      DOUBLE PRECISION, ALLOCATABLE :: UV2D(:),VV2D(:),PROF(:)
C
      DOUBLE PRECISION S00, DTETA
      DOUBLE PRECISION HM0   , AL    , FP    , GAMMA , SIGMAA, SIGMAB
      DOUBLE PRECISION DEGRAD, FPMIN
      LOGICAL FLAG
C
      CHARACTER*3 BINBI1
C
      SAVE UV2D,VV2D,PROF
C
C***********************************************************************
C
C   MODIFICATION EVENTUELLE DU TYPE DE CONDITION A LA LIMITE
C
C   A REMPLIR PAR L'UTILISATEUR
C
C   LIFBOR(IPTFR)=KENT OU KSORT
C
      IF (LIMSPE.EQ.0 .AND. .NOT.SPEULI) RETURN
      FLAG=.FALSE.
      IF (VENT .AND. (LIMSPE.EQ.1 .OR. LIMSPE.EQ.2 .OR. LIMSPE.EQ.3
     * .OR. LIMSPE.EQ.5)) FLAG=.TRUE.
C
C     AU PREMIER PASSAGE, ON ALLOUE DE LA MEMOIRE AUX TABLEAUX UTILES
C     ---------------------------------------------------------------
      IF (LT.LT.1) THEN
        IF (FLAG) THEN
          ALLOCATE(UV2D(1:NPTFR),VV2D(1:NPTFR))
        ENDIF
        IF (LIMSPE.EQ.7 .OR. SPEULI) THEN
          ALLOCATE(PROF(1:NPTFR))
        ENDIF
      ENDIF
C
C     AU PREMIER PASSAGE (ET EVENTUELLEMENT AUX AUTRES SI LE VENT EST 
C     INSTATIONNAIRE ET QUE LE SPECTRE A LA LIMITE EN DEPEND),
C     ON CALCULE LE SPECTRE AUX LIMITES
C     ----------------------------------------------------------------
      IF (LT.LT.1) THEN
      DEUPI=2.D0*3.141592654D0
      DEGRAD=DEUPI/360.D0
      GRAVIT=9.81D0
      E2FMIN=1.D-30
      FPMIN =1.D-10
C
      HM0    = 1
      FP     = 0.1D0
      GAMMA  = 1.5D0
      SIGMAA = 0.07D0
      SIGMAB = 0.09D0
      AL=0.0624D0/(0.230D0+0.0336D0*GAMMA-0.185D0/(1.9D0+GAMMA))
     &  *(DEUPI*FP)**4*HM0*HM0/GRAVIT**2
C
      CALL SPEJON
     &( SPEC , FREQ  , NF , AL , FP    , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
C
C     ===========================================================
C     ZONE UTILISATEUR - ON PEUT Y MODIFIER RESU
C     ===========================================================
        IF (SPEULI) THEN
C
C        EXEMPLE DE MODIFICATION DE FRA - A MODIFIER SUIVANT VOTRE CAS
C        EXAMPLE OF MODIFICATION OF FRA - TO BE MODIFIED DEPENDING 
C        ON YOUR CASE
        ALLOCATE(TRAV(1:NF))
        S00=0.4538D0
        DO IFREQ=1,NF
          IF (FREQ(IFREQ).LT.FPICL) THEN
            TRAV(IFREQ)=S00*(FREQ(IFREQ)/FPICL)**(-2.03D0)
          ELSE
            TRAV(IFREQ)=S00*(FREQ(IFREQ)/FPICL)**(1.04D0)
          ENDIF
        ENDDO
C
        DO IPTFR=1,NPTFR
          IF (LIFBOR(IPTFR).EQ.KENT) THEN
             DO IPLAN=1,NPLAN
               DTETA=TETA(IPLAN)-TETA1L
               IF ((TETA(IPLAN)-TETA1L).GT.DEUPI/2) THEN
                 DTETA=DEUPI-DTETA
               ENDIF
             DO IFREQ=1,NF
                 FRA(IPLAN)=1.D0/SQRT(DEUPI)*TRAV(IFREQ)*
     &           EXP(-DTETA**2/(2.D0*TRAV(IFREQ)**2))
                 F(NBOR(IPTFR),IPLAN,IFREQ)=SPEC(IFREQ)*FRA(IPLAN)
             ENDDO
           ENDDO
         ENDIF
        ENDDO
C
        DEALLOCATE(TRAV)
C
        ENDIF
C
C     ===========================================================
C     FIN DE LA ZONE UTILISATEUR 
C     ===========================================================
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
