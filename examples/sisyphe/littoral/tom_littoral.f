!
C                       *****************
                        SUBROUTINE LIMWAC
C                       *****************
C
     *(F     , FBOR  , LIFBOR, NPTFR , NPLAN , NF    ,  TETA , FREQ  ,
     * NPOIN2, NBOR  , AT    , LT    , DDC   , LIMSPE, FPMAXL, FETCHL,
     * SIGMAL, SIGMBL, GAMMAL, FPICL , HM0L  , APHILL, TETA1L, SPRE1L,
     * TETA2L, SPRE2L, XLAMDL, X ,Y  , KENT  , KSORT , NFO1  , NBI1  ,
     * BINBI1, UV    , VV    , SPEULI, VENT  , VENSTA, GRAVIT, DEUPI , 
     * PRIVE , NPRIV , SPEC  , FRA   , DEPTH , FRABL , BOUNDARY_COLOUR)
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
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH, NCSIZE

C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NPLAN,NF,NPOIN2,NPTFR,LT,NPRIV
C
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
      INTEGER BOUNDARY_COLOUR(NPTFR)
      INTEGER NBOR(NPTFR),LIFBOR(NPTFR),NFO1,NBI1,NPB
      INTEGER KENT,KSORT,IFF,IPLAN,IPTFR,LIMSPE,FRABL
!GM
!      INTEGER IP, IMIL, IDRO, IGAU
      INTEGER IP, IMIL(40), IDRO(40), IGAU(40)
!GM Fin
C
      DOUBLE PRECISION, ALLOCATABLE :: TRAV(:)
      DOUBLE PRECISION, ALLOCATABLE :: UV2D(:),VV2D(:),PROF(:)
      DOUBLE PRECISION, ALLOCATABLE :: FB_CTE(:,:)
      LOGICAL FLAG
C
      CHARACTER*3 BINBI1
C
!GM
      DOUBLE PRECISION DUMMY(40,NPLAN,NF), P_DMAX, P_DMIN
      EXTERNAL P_DMAX, P_DMIN
!GM Fin
      SAVE UV2D,VV2D,PROF,FB_CTE
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
      NPB=1
      FLAG=.FALSE.
      IF(VENT .AND. (LIMSPE.EQ.1 .OR. LIMSPE.EQ.2 .OR. LIMSPE.EQ.3
     * .OR. LIMSPE.EQ.5)) FLAG=.TRUE.
C
C     AU PREMIER PASSAGE, ON ALLOUE DE LA MEMOIRE AUX TABLEAUX UTILES
C     ---------------------------------------------------------------
      IF (LT.LT.1) THEN
        IF (FLAG) THEN
           ALLOCATE(UV2D(1:NPTFR),VV2D(1:NPTFR))
           NPB=NPTFR
        ENDIF
        IF (LIMSPE.EQ.7 .OR. SPEULI) THEN
           ALLOCATE(PROF(1:NPTFR))
           NPB=NPTFR
        ENDIF
        IF (NPB.EQ.1) THEN
           ALLOCATE(FB_CTE(1:NPLAN,1:NF))
        ENDIF
      ENDIF
      IF (.NOT.ALLOCATED(UV2D)) ALLOCATE(UV2D(NPTFR))
      IF (.NOT.ALLOCATED(VV2D)) ALLOCATE(VV2D(NPTFR))
      IF (.NOT.ALLOCATED(PROF)) ALLOCATE(PROF(NPTFR))
      IF (.NOT.ALLOCATED(FB_CTE)) ALLOCATE(FB_CTE(1:NPLAN,1:NF))
C
C     AU PREMIER PASSAGE (ET EVENTUELLEMENT AUX AUTRES SI LE VENT EST 
C     INSTATIONNAIRE ET QUE LE SPECTRE A LA LIMITE EN DEPEND),
C     ON CALCULE LE SPECTRE AUX LIMITES
C     ----------------------------------------------------------------
      IF (LT.LT.1 .OR. (.NOT.VENSTA.AND.FLAG) .OR. SPEULI) THEN
        IF (FLAG) THEN

          DO IPTFR=1,NPTFR
            UV2D(IPTFR)=UV(NBOR(IPTFR))
            VV2D(IPTFR)=VV(NBOR(IPTFR))
          ENDDO
        ENDIF
        IF(LIMSPE.EQ.7 .OR. SPEULI) THEN
          DO IPTFR=1,NPTFR
            PROF(IPTFR)=DEPTH(NBOR(IPTFR))
          ENDDO
        ENDIF
C
C       APPEL A SPEINI
C     ----------------------------------------------------------------
        E2FMIN = 1.D-30
C
        IF (NPB.EQ.NPTFR) THEN
          CALL SPEINI
     *( FBOR  , SPEC  , FRA    , UV2D  , VV2D  , FREQ ,
     *  TETA  , GRAVIT, FPMAXL , FETCHL, SIGMAL, SIGMBL, GAMMAL, FPICL,
     *  HM0L  , APHILL, TETA1L , SPRE1L, TETA2L, SPRE2L, XLAMDL,
     *  NPTFR , NPLAN , NF     , LIMSPE, E2FMIN, PROF  , FRABL )
        ELSE
          CALL SPEINI
     *( FB_CTE, SPEC  , FRA    , UV2D  , VV2D  , FREQ ,
     *  TETA  , GRAVIT, FPMAXL , FETCHL, SIGMAL, SIGMBL, GAMMAL, FPICL,
     *  HM0L  , APHILL, TETA1L , SPRE1L, TETA2L, SPRE2L, XLAMDL,
     *  NPB   , NPLAN , NF     , LIMSPE, E2FMIN, PROF  , FRABL )
        ENDIF
C
C     ===========================================================
C     ZONE UTILISATEUR - ON PEUT Y MODIFIER RESU
C     ===========================================================
        IF (SPEULI) THEN
C
C        EXEMPLE DE MODIFICATION DE FRA - A MODIFIER SUIVANT VOTRE CAS
C        EXAMPLE OF MODIFICATION OF FRA - TO BE MODIFIED DEPENDING 
C        ON YOUR CASE
C        ALLOCATE(TRAV(1:NF))
C
C        DO IFREQ=1,NF

C             IF (FREQ(IFF).LT.FPIC) THEN
C              TRAV(IFF)=0.4538D0*(FREQ(IFF)/FPIC)**(-2.03D0)
C           ELSE
C              TRAV(IFF)=0.4538D0*(FREQ(IFF)/FPIC)**(1.04D0)
C           ENDIF
C        ENDDO
C
C        DO IPLAN=1,NPLAN
C             DTETA=TETA(IPLAN)-TETA1
C           IF ((TETA(IPLAN)-TETA1).GT.DEUPI/2) THEN
C              DTETA=DEUPI-DTETA
C           ENDIF
C           DO IFF=1,NF
C              FRA(IPLAN)=1.D0/SQRT(DEUPI)*TRAV(IFF)*
C     *                       EXP(-DTETA**2/(2.D0*TRAV(IFF)**2))
C              DO IPTFR=1,NPTFR
C                FBOR(IPTFR,IPLAN,IFF)= SPEC(IFF)*FRA(IPLAN)
C              ENDDO
C           ENDDO
C        ENDDO
C        DEALLOCATE(TRAV)
C
C        PARTIE A SUPPRIMER SI ON FAIT DES MODIFICATIONS
C        LINES TO ERASE IF YOU DO MODIFICATIONS 
C
        IF (LNG.EQ.1) THEN
          WRITE(LU,*)'*****  ERREUR LIMWAC  ******'
          WRITE(LU,*)
     *      ' VOUS NE MODIFIEZ PAS LE SPECTRE AUX LIMITES ALORS QUE'
          WRITE(LU,*)' VOUS EN DEMANDEZ LA POSSIBILITE'
        ELSE
          WRITE(LU,*)'*****  ERROR LIMWAC  ******'
          WRITE(LU,*)
     *      ' YOU DID NOT MODIFY THE BOUNDARY SPECTRUM WHEREAS '
          WRITE(LU,*)' YOU ASK FOR THAT '
        ENDIF
        STOP
      ENDIF
C
C     ===========================================================
C     FIN DE LA ZONE UTILISATEUR 
C     ===========================================================
      ENDIF
C
C
C     -----------------------------------------------------------------
C     DUPLICATION SUR TOUTES LES DIRECTIONS ET TOUTES LES FREQUENCES
C     DE LA C.L. DE DYNAM SI ON EST EN CONDITION DE FRONT. LIQUIDE
C     -----------------------------------------------------------------
      IF (FLAG .OR. LIMSPE.EQ.7 .OR. SPEULI) THEN
        DO IPTFR=1,NPTFR
          IF (LIFBOR(IPTFR).EQ.KENT) THEN
            DO IFF=1,NF
              DO IPLAN=1,NPLAN
                F(NBOR(IPTFR),IPLAN,IFF)=FBOR(IPTFR,IPLAN,IFF)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ELSE
        DO IPTFR=1,NPTFR
          IF (LIFBOR(IPTFR).EQ.KENT) THEN
            DO IFF=1,NF
              DO IPLAN=1,NPLAN
                F(NBOR(IPTFR),IPLAN,IFF)=FB_CTE(IPLAN,IFF)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C     MODIFICATION M. BENOIT (12/03/2002) POUR METTRE SUR LES LIMITES
C     LATERALES LE SPECTRE CALCULE SUR L'AXE DU DOMAINE 
C     (ATTENTION : CECI N'EST VALABLE QUE POUR LE MAILLAGE "COURANT
C      LITTORAL" ; LES NUMEROS DE POINTS SONT CODES EN DUR)
C-----------------------------------------------------------------------
      DO IP=1,40
        IMIL(IP)=1117+IP-1
        IF (IMIL(IP).EQ.1156) IMIL(IP)=116
        IGAU(IP)=180-IP+1
        IDRO(IP)= 52+IP-1
      ENDDO
!
      IF (NCSIZE.GT.1) THEN
!        WRITE(*,*)'APPEL A BORD_WAC'
        DO IP=1,40
           CALL BORD_WAC(F,NPLAN,NF,NPOIN2,IP)
        ENDDO
!        WRITE(*,*)'RETOUR BORD_WAC'
      ENDIF
!
      IF(NCSIZE.LE.1) THEN
       DO IP=1,40
         DO IFF=1,NF
           DO IPLAN = 1,NPLAN
             F(IGAU(IP),IPLAN,IFF) = F(IMIL(IP),IPLAN,IFF)
             F(IDRO(IP),IPLAN,IFF) = F(IMIL(IP),IPLAN,IFF)
           ENDDO
         ENDDO
       ENDDO
      ENDIF
C
!      WRITE(*,*)'FIN PROGRAMME'
      RETURN
      END
C#######################################################################
C
C                  *******************
                   SUBROUTINE BORD_WAC
C                  *******************
C
     *(F     , NPLAN , NF , NPOIN2, IP)
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !    F           ! <->!  DENSITE SPECTRALE                           !
C !    NPLAN       ! -->!  NOMBRE DE DIRECTIONS                        !
C !    NF          ! -->!  NOMBRE DE FREQUENCES                        !
C !    NPOIN2      ! -->!  NOMBRE DE POINTS 2D                         !
C !________________!____!______________________________________________!
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C SOUS-PROGRAMME APPELE PAR : LIMWAC
C
C***********************************************************************
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH, NCSIZE
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NPLAN,NF,NPOIN2,NPTFR,LT,NPRIV
C
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF),TOTO(NPOIN2,NPLAN,NF)
C
      INTEGER IFF,IPLAN
      INTEGER IP, IMIL, IDRO, IGAU
      DOUBLE PRECISION DUMMY(NPLAN,NF)
      DOUBLE PRECISION P_DMAX, P_DMIN
      EXTERNAL P_DMAX, P_DMIN
C
C***********************************************************************
      IMIL=1117+IP-1
      IF (IMIL.EQ.1156) IMIL=116
      IGAU=180-IP+1
      IDRO= 52+IP-1
C
      IMIL=MESH%KNOGL%I(IMIL)
      IF(IMIL.EQ.0) THEN
        DO IFF=1,NF
          DO IPLAN = 1,NPLAN
            DUMMY(IPLAN,IFF)=0.
          ENDDO
        ENDDO
      ELSE
        DO IFF=1,NF
          DO IPLAN = 1,NPLAN
            DUMMY(IPLAN,IFF)=F(IMIL,IPLAN,IFF)
          ENDDO
        ENDDO
      ENDIF
C
      IGAU=MESH%KNOGL%I(IGAU)
      IDRO=MESH%KNOGL%I(IDRO)
      IF(IGAU.EQ.0) THEN
        DO IFF=1,NF
          DO IPLAN = 1,NPLAN
           TOTO(IGAU,IPLAN,IFF) = P_DMAX(DUMMY(IPLAN,IFF))+
     &                            P_DMIN(DUMMY(IPLAN,IFF))
          ENDDO
        ENDDO
      ELSE
        DO IFF=1,NF
          DO IPLAN = 1,NPLAN
           F(IGAU,IPLAN,IFF) = P_DMAX(DUMMY(IPLAN,IFF))+
     &                            P_DMIN(DUMMY(IPLAN,IFF))
          ENDDO
        ENDDO
      ENDIF
      IF(IDRO.EQ.0) THEN
        DO IFF=1,NF
          DO IPLAN = 1,NPLAN
           TOTO(IGAU,IPLAN,IFF) = P_DMAX(DUMMY(IPLAN,IFF))+
     &                            P_DMIN(DUMMY(IPLAN,IFF))
          ENDDO
        ENDDO
      ELSE
        DO IFF=1,NF
          DO IPLAN = 1,NPLAN
           F(IDRO,IPLAN,IFF) = P_DMAX(DUMMY(IPLAN,IFF))+
     &                            P_DMIN(DUMMY(IPLAN,IFF))
          ENDDO
        ENDDO
      ENDIF
C
!      IF(IP.GT.38) WRITE(*,*) 'OK !', IP
      RETURN
      END
C
C#######################################################################
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
C                 (! STATIONNAIRE !)
C
C     FUNCTION  : SPECIFICATION OF AN ANALYTICAL CURRENT 
C                 (! STATIONNARY !)
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
C.....VARIABLES LOCALES
C     """""""""""""""""
      INTEGER  IP
      DOUBLE PRECISION UCONST, VCONST
C
C
      UCONST=1.0D0
      VCONST=1.0D0
C
      DO 100 IP=1,NPOIN2
        UC(IP)=UCONST
        VC(IP)=VCONST
  100 CONTINUE
C
      RETURN
      END
C
