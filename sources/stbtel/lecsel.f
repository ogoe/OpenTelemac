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
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION XBID(1) , XINIT(*) , YINIT(*) , X(*) , Y(*)
!
      REAL W(*)
!
      INTEGER NGEO , NFO1,IPARAM(10),DATE(3),TIME(3)
      INTEGER MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
      INTEGER NEINIT , NPINIT, NPOIN1 , NELEM1 , NPOIN2 , NELEM2
      INTEGER IBID(1) , NVARIN , NVAR2 , I , IELEM , IB(10) , ISTAT
      INTEGER IKINIT(NELEM,NDP),IKLE(NELMAX,NDP),IKLES(NDP,NELEM)
      INTEGER NCOLOR(*),IPOBO(*)
!
      LOGICAL FUSION
!
      CHARACTER*72 CBID,TITRE
      CHARACTER*32 TEXTE(26)
      CHARACTER*3  STD
!
! COMMON
!
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
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
      DO 10 I=1,NVARIN
         CALL LIT(XBID,W,IBID,TEXTE(I),32,'CH',NGEO,STD,ISTAT)
10    CONTINUE
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
         DO 20 I=1,NVAR2
            CALL LIT(XBID,W,IBID,CBID,32,'CH',NFO1,STD,ISTAT)
20       CONTINUE
         CALL LIT(XBID,W,IB  ,CBID,10,'I ',NFO1,STD,ISTAT)
         CALL LIT(XBID,W,IB  ,CBID, 4,'I ',NFO1,STD,ISTAT)
!
         NELEM2 = IB(1)
         NPOIN2 = IB(2)
!
         CALL LIT(XBID,W,IKLES(1,NELEM1+1),CBID,NELEM2*NDP,'I ',
     &            NFO1,STD,ISTAT)
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
      DO 13 I = 1,NDP
        DO 11 IELEM = 1,NELEM1
          IKLE  (IELEM,I) = IKLES(I,IELEM)
          IKINIT(IELEM,I) = IKLES(I,IELEM)
11      CONTINUE
        IF (FUSION) THEN
           DO 12 IELEM = NELEM1+1,NELEM
             IKLE  (IELEM,I) = IKLES(I,IELEM) + NPOIN1
             IKINIT(IELEM,I) = IKLES(I,IELEM) + NPOIN1
12         CONTINUE
        ENDIF
13    CONTINUE
!
      DO 14 I = 1,NPOIN
         XINIT(I) = X(I)
         YINIT(I) = Y(I)
         NCOLOR(I) = 11
14    CONTINUE
!
!=======================================================================
!
      RETURN
      END