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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
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

