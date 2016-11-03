!                       *****************
                        SUBROUTINE LECSEL
!                       *****************
!
     &(XINIT,YINIT,IKINIT,NPINIT,NEINIT,X,Y,IKLE,IKLES,W,TITRE,TEXTE,
     & NVARIN,NVAR2,STD,NCOLOR,FUSION,NGEO,NFO1,IPOBO,IPARAM,DATE,
     & TIME)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2           11/02/93    J.M. JANIN
!***********************************************************************
!
!   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
!               MAILLAGE DANS LE FICHIER D'ENTREE SELAFIN
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | NPOIN1         |<-- | NOMBRE REEL DE POINTS DU MAILLAGE
! |                |    | (NPOIN REPRESENTE L'INDICE MAX DES NOEUDS CAR
! |                |    | SUPERTAB LAISSE DES TROUS DANS LA NUMEROTATION
! | TYPELE         |<-- | TYPE D'ELEMENTS
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        |<-- | TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       |<-- | NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       |<-- | NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |  FICH:         |    |
! |    NRES        |--> | NUMERO DU CANAL DU FICHIER DE SERAFIN
! |    NGEO       |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
! |    NLIM      |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
! |    NFO1      |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : HOMERE
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,MESH,NDP,NPOIN,NELMAX,NPMAX
      USE INTERFACE_STBTEL, EX_LECSEL => LECSEL
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(INOUT) :: XINIT(*), YINIT(*), X(*), Y(*)
      REAL, INTENT(INOUT) :: W(*)
      INTEGER, INTENT(IN) :: NGEO , NFO1
      INTEGER, INTENT(INOUT) :: IPARAM(10),DATE(3),TIME(3)
      INTEGER, INTENT(INOUT) :: NEINIT , NPINIT
      INTEGER, INTENT(INOUT) :: NVARIN , NVAR2
      INTEGER, INTENT(INOUT) :: IKINIT(NELEM,NDP)
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,NDP),IKLES(NDP,NELEM)
      INTEGER, INTENT(INOUT) :: NCOLOR(*),IPOBO(*)
      LOGICAL, INTENT(IN) :: FUSION
      CHARACTER(LEN=72), INTENT(INOUT) :: TITRE
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(26)
      CHARACTER(LEN=3), INTENT(IN) ::  STD
!
      INTEGER NPOIN1 , NELEM1 , NPOIN2 , NELEM2
      INTEGER IBID(1), I , IELEM , IB(10) , ISTAT
      CHARACTER(LEN=72) CBID
      DOUBLE PRECISION XBID(1)
!
!
!=======================================================================
! LECTURE SEQUENTIELLE DU PREMIER FICHIER
!=======================================================================
!
      REWIND NGEO
!
      CALL LIT(XBID,W,IBID,TITRE,72,'CH',NGEO,STD,ISTAT)
      CALL LIT(XBID,W,IB  ,CBID , 2,'I ',NGEO,STD,ISTAT)
      NVARIN = IB(1) + IB(2)
      DO I=1,NVARIN
        CALL LIT(XBID,W,IBID,TEXTE(I),32,'CH',NGEO,STD,ISTAT)
      ENDDO
!     LECTURE ET STOCKAGE DU TABLEAU IPARAM
      CALL LIT(XBID,W,IPARAM  ,CBID,10,'I ',NGEO,STD,ISTAT)
!     LECTURE ET STOCKAGE DE LA DATE
      IF(IPARAM(10).EQ.1) THEN
        CALL LIT(XBID,W,IB,CBID,6,'I ',NGEO,STD,ISTAT)
        DATE(1)=IB(1)
        DATE(2)=IB(2)
        DATE(3)=IB(3)
        TIME(1)=IB(4)
        TIME(2)=IB(5)
        TIME(3)=IB(6)
      ENDIF
      CALL LIT(XBID,W,IB  ,CBID, 4,'I ',NGEO,STD,ISTAT)
!
      NELEM1 = IB(1)
      NPOIN1 = IB(2)
!
      CALL LIT(XBID,W,IKLES,CBID,NELEM1*NDP,'I ',NGEO,STD,ISTAT)
!     LECTURE ET STOCKAGE DES IPOBO (KNOLG EN PARALLELISME)
      CALL LIT(XBID,W,IPOBO,CBID,NPOIN1,'I ',NGEO,STD,ISTAT)
!     LECTURE DES COORDONNEES
      CALL LIT(X   ,W,IBID,CBID,NPOIN1,'R4',NGEO,STD,ISTAT)
      CALL LIT(Y   ,W,IBID,CBID,NPOIN1,'R4',NGEO,STD,ISTAT)
!
!=======================================================================
! LECTURE SEQUENTIELLE DU SECOND FICHIER EN CAS DE FUSION
!=======================================================================
!
      IF (FUSION) THEN
!
        REWIND NFO1
!
        CALL LIT(XBID,W,IBID,CBID,72,'CH',NFO1,STD,ISTAT)
        CALL LIT(XBID,W,IB  ,CBID, 2,'I ',NFO1,STD,ISTAT)
        NVAR2 = IB(1) + IB(2)
        DO I=1,NVAR2
          CALL LIT(XBID,W,IBID,CBID,32,'CH',NFO1,STD,ISTAT)
        ENDDO
        CALL LIT(XBID,W,IB  ,CBID,10,'I ',NFO1,STD,ISTAT)
        CALL LIT(XBID,W,IB  ,CBID, 4,'I ',NFO1,STD,ISTAT)
!
        NELEM2 = IB(1)
        NPOIN2 = IB(2)
!
        CALL LIT(XBID,W,IKLES(1,NELEM1+1),CBID,NELEM2*NDP,'I ',
     &           NFO1,STD,ISTAT)
        CALL LIT(XBID,W,IB,CBID, 2,'I ',NFO1,STD,ISTAT)
        CALL LIT(X(NPOIN1+1),W,IBID,CBID,NPOIN2,'R4',NFO1,STD,ISTAT)
        CALL LIT(Y(NPOIN1+1),W,IBID,CBID,NPOIN2,'R4',NFO1,STD,ISTAT)
!
      ENDIF
!
!=======================================================================
! AFFECTATION DES VALEURS LUES AUX VARIABLES CONCERNEES
!=======================================================================
!
      NEINIT = NELEM
      NPINIT = NPOIN
!
!       INVERSION DE IKLES EN IKLE.
!
      DO I = 1,NDP
        DO IELEM = 1,NELEM1
          IKLE  (IELEM,I) = IKLES(I,IELEM)
          IKINIT(IELEM,I) = IKLES(I,IELEM)
        ENDDO
        IF (FUSION) THEN
           DO IELEM = NELEM1+1,NELEM
             IKLE  (IELEM,I) = IKLES(I,IELEM) + NPOIN1
             IKINIT(IELEM,I) = IKLES(I,IELEM) + NPOIN1
           ENDDO
        ENDIF
      ENDDO
!
      DO I = 1,NPOIN
        XINIT(I) = X(I)
        YINIT(I) = Y(I)
        NCOLOR(I) = 11
      ENDDO
!
!=======================================================================
!
      RETURN
      END
