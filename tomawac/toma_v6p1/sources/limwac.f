!                    *****************
                     SUBROUTINE LIMWAC
!                    *****************
!
     &(F     , FBOR  , LIFBOR, NPTFR , NPLAN , NF    ,  TETA , FREQ  ,
     & NPOIN2, NBOR  , AT    , LT    , DDC   , LIMSPE, FPMAXL, FETCHL,
     & SIGMAL, SIGMBL, GAMMAL, FPICL , HM0L  , APHILL, TETA1L, SPRE1L,
     & TETA2L, SPRE2L, XLAMDL, X ,Y  , KENT  , KSORT , NFO1  , NBI1  ,
     & BINBI1, UV    , VV    , SPEULI, VENT  , VENSTA, GRAVIT, DEUPI ,
     & PRIVE , NPRIV , SPEC  , FRA   , DEPTH , FRABL ,BOUNDARY_COLOUR)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    BOUNDARY CONDITIONS.
!
!warning  BY DEFAULT, THE BOUNDARY CONDITIONS SPECIFIED IN THE FILE
!+            DYNAM ARE DUPLICATED ON ALL THE DIRECTIONS AND FREQUENCIES
!
!history  F. MARCOS (LNH)
!+        01/02/95
!+        V1P0
!+   
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into 
!+   English comments 
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and 
!+   cross-referencing of the FORTRAN sources 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| APHILL         |---| 
!| AT             |-->| TEMPS
!| BINBI1         |-->| BINAIRE DU FICHIER BINAIRE UTILISATEUR
!| DDC            |-->| DATE DU DEBUT DU CALCUL
!| DEPTH          |---| 
!| DEUPI          |---| 
!| F              |-->| DENSITE SPECTRALE
!| FBOR           |<->| DENSITE SPECTRALE AU BORD
!| FETCHL         |---| 
!| FPICL          |---| 
!| FPMAXL         |---| 
!| FRA            |---| 
!| FRABL          |---| 
!| FREQ           |-->| FREQUENCES DISCRETISEES
!| GAMMAL         |---| 
!| GRAVIT         |---| 
!| HM0L           |---| 
!| KENT           |-->| C.L. INDIQUANT UNE FRONTIERE MARITIME
!| KSORT          |-->| C.L. INDIQUANT UNE FRONTIERE SOLIDE
!| LIFBOR         |-->| TYPE DE CONDITION LIMITE SUR F
!| LIMSPE         |---| 
!| LT             |-->| NUMERO DU PAS DE TEMPS
!| NBI1           |-->| NUMERO DU FICHIER BINAIRE UTILISATEUR
!| NBOR           |-->| NUMEROTATION DES POINTS DE BORD 2D
!| NF             |-->| NOMBRE DE FREQUENCES
!| NFO1           |-->| NUMERO DU FICHIER FORMATE UTILISATEUR
!| NPLAN          |-->| NOMBRE DE DIRECTIONS
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| NPRIV          |-->| DIMENSION DU TABLEAU PRIVE
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE 2D
!| PRIVE          |-->| TABLEAU DE L'UTILISATEUR
!| SIGMAL         |---| 
!| SIGMBL         |---| 
!| SPEC           |---| 
!| SPEULI         |---| 
!| SPRE1L         |---| 
!| SPRE2L         |---| 
!| TETA           |-->| DIRECTIONS DE PROPAGATION
!| TETA1L         |---| 
!| TETA2L         |---| 
!| UV             |---| 
!| VENSTA         |---| 
!| VENT           |---| 
!| VV             |---| 
!| X              |-->| ABSCISSES DES POINTS 2D
!| XLAMDL         |---| 
!| Y              |-->| ORDONNEES DES POINTS 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_LIMWAC => LIMWAC
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NPLAN,NF,NPOIN2,NPTFR,LT,NPRIV
      INTEGER, INTENT(IN) :: BOUNDARY_COLOUR(NPTFR)
!
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
      INTEGER NBOR(NPTFR),LIFBOR(NPTFR),NFO1,NBI1,NPB
      INTEGER KENT,KSORT,IFF,IPLAN,IPTFR,LIMSPE,FRABL
!
      DOUBLE PRECISION, ALLOCATABLE :: TRAV(:)
      DOUBLE PRECISION, ALLOCATABLE :: UV2D(:),VV2D(:),PROF(:)
      DOUBLE PRECISION, ALLOCATABLE :: FB_CTE(:,:)
      LOGICAL FLAG
!
      CHARACTER*3 BINBI1
!
      SAVE NPB,UV2D,VV2D,PROF,FB_CTE
!
!***********************************************************************
!
!   MODIFIES THE TYPE OF BOUNDARY CONDITION (OPTIONAL)
!
!   TO BE CODED BU THE USER
!
!   LIFBOR(IPTFR)=KENT OR KSORT
!
      IF (LIMSPE.EQ.0 .AND. .NOT.SPEULI) RETURN
!
      FLAG=.FALSE.
      IF (VENT .AND. (LIMSPE.EQ.1 .OR. LIMSPE.EQ.2 .OR. LIMSPE.EQ.3
     & .OR. LIMSPE.EQ.5)) FLAG=.TRUE.
!
!     THE FIRST TIME, ALLOCATES MEMORY FOR THE USEFUL ARRAYS
!     ---------------------------------------------------------------
      IF (LT.LT.1) THEN
        NPB=1
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
!
!     THE FIRST TIME (AND POSSIBLY SUBSEQUENTLY IF THE WIND IS NOT
!     STATIONARY AND IF THE BOUNDARY SPECTRUM DEPENDS ON IT),
!     COMPUTES THE BOUNDARY SPECTRUM
!     ----------------------------------------------------------------
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
!
!       CALLS SPEINI
!     ----------------------------------------------------------------
        E2FMIN = 1.D-30
!
        IF (NPB.EQ.NPTFR) THEN
          CALL SPEINI
     &( FBOR  , SPEC  , FRA    , UV2D  , VV2D  , FREQ ,
     &  TETA  , GRAVIT, FPMAXL , FETCHL, SIGMAL, SIGMBL, GAMMAL, FPICL,
     &  HM0L  , APHILL, TETA1L , SPRE1L, TETA2L, SPRE2L, XLAMDL,
     &  NPTFR , NPLAN , NF     , LIMSPE, E2FMIN, PROF  , FRABL )
        ELSE
          CALL SPEINI
     &( FB_CTE, SPEC  , FRA    , UV2D  , VV2D  , FREQ ,
     &  TETA  , GRAVIT, FPMAXL , FETCHL, SIGMAL, SIGMBL, GAMMAL, FPICL,
     &  HM0L  , APHILL, TETA1L , SPRE1L, TETA2L, SPRE2L, XLAMDL,
     &  NPB   , NPLAN , NF     , LIMSPE, E2FMIN, PROF  , FRABL )
	ENDIF
!
!     ===========================================================
!     TO BE MODIFIED BY USER - RESU CAN BE CHANGED
!     ===========================================================
        IF (SPEULI) THEN
!
!        EXEMPLE DE MODIFICATION DE FRA - A MODIFIER SUIVANT VOTRE CAS
!        EXAMPLE OF MODIFICATION OF FRA - TO BE MODIFIED DEPENDING
!        ON YOUR CASE
!        ALLOCATE(TRAV(1:NF))
!
!        DO IFREQ=1,NF
!             IF (FREQ(IFF).LT.FPIC) THEN
!              TRAV(IFF)=0.4538D0*(FREQ(IFF)/FPIC)**(-2.03D0)
!           ELSE
!              TRAV(IFF)=0.4538D0*(FREQ(IFF)/FPIC)**(1.04D0)
!           ENDIF
!        ENDDO
!
!        DO IPLAN=1,NPLAN
!             DTETA=TETA(IPLAN)-TETA1
!           IF ((TETA(IPLAN)-TETA1).GT.DEUPI/2) THEN
!              DTETA=DEUPI-DTETA
!           ENDIF
!           DO IFF=1,NF
!              FRA(IPLAN)=1.D0/SQRT(DEUPI)*TRAV(IFF)*
!     *                       EXP(-DTETA**2/(2.D0*TRAV(IFF)**2))
!              DO IPTFR=1,NPTFR
!                FBOR(IPTFR,IPLAN,IFF)= SPEC(IFF)*FRA(IPLAN)
!              ENDDO
!           ENDDO
!        ENDDO
!        DEALLOCATE(TRAV)
!
!        PARTIE A SUPPRIMER SI ON FAIT DES MODIFICATIONS
!        DELETE THESE LINES IF MODIFICATIONS HAVE BEEN IMPLEMENTED
!
        IF (LNG.EQ.1) THEN
          WRITE(LU,*)'*****  ERREUR LIMWAC  ******'
          WRITE(LU,*)
     &      ' VOUS NE MODIFIEZ PAS LE SPECTRE AUX LIMITES ALORS QUE'
          WRITE(LU,*)' VOUS EN DEMANDEZ LA POSSIBILITE'
        ELSE
          WRITE(LU,*)'*****  ERROR LIMWAC  ******'
          WRITE(LU,*)
     &      ' YOU DID NOT MODIFY THE BOUNDARY SPECTRUM WHEREAS '
          WRITE(LU,*)' YOU ASK FOR THAT '
        ENDIF
        STOP
      ENDIF
!
!     ===========================================================
!     END OF USER MODIFICATIONS
!     ===========================================================
      ENDIF
!
!
!     -----------------------------------------------------------------
!     DUPLICATES THE BOUNDARY CONDITION FROM DYNAM ON ALL THE
!     DIRECTIONS AND FREQUENCIES, IF LIQUID BOUNDARY
!     -----------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
      RETURN
      END