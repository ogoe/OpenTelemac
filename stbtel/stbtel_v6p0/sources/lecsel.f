C                       *****************
                        SUBROUTINE LECSEL
C                       *****************
C
     *(XINIT,YINIT,IKINIT,NPINIT,NEINIT,X,Y,IKLE,IKLES,W,TITRE,TEXTE,
     * NVARIN,NVAR2,STD,NCOLOR,FUSION,NGEO,NFO1,IPOBO,IPARAM,DATE,
     * TIME)
C
C***********************************************************************
C PROGICIEL : STBTEL  V5.2           11/02/93    J.M. JANIN
C***********************************************************************
C
C   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
C               MAILLAGE DANS LE FICHIER D'ENTREE SELAFIN
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C | NPOIN1         |<-- | NOMBRE REEL DE POINTS DU MAILLAGE
C |                |    | (NPOIN REPRESENTE L'INDICE MAX DES NOEUDS CAR
C |                |    | SUPERTAB LAISSE DES TROUS DANS LA NUMEROTATION
C | TYPELE         |<-- | TYPE D'ELEMENTS
C |________________|____|______________________________________________
C | COMMON:        |    |
C |  GEO:          |    |
C |    MESH        |<-- | TYPE DES ELEMENTS DU MAILLAGE
C |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
C |    NPOIN       |<-- | NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C |    NELEM       |<-- | NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
C |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
C |                |    | (NPMAX = NPOIN + 0.1*NELEM)
C |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
C |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
C |  FICH:         |    |
C |    NRES        |--> | NUMERO DU CANAL DU FICHIER DE SERAFIN
C |    NGEO       |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
C |    NLIM      |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
C |    NFO1      |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C APPELE PAR : HOMERE
C APPEL DE : -
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION XBID(1) , XINIT(*) , YINIT(*) , X(*) , Y(*)
C
      REAL W(*)
C
      INTEGER NGEO , NFO1,IPARAM(10),DATE(3),TIME(3)
      INTEGER MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
      INTEGER NEINIT , NPINIT, NPOIN1 , NELEM1 , NPOIN2 , NELEM2
      INTEGER IBID(1) , NVARIN , NVAR2 , I , IELEM , IB(10) , ISTAT
      INTEGER IKINIT(NELEM,NDP),IKLE(NELMAX,NDP),IKLES(NDP,NELEM)
      INTEGER NCOLOR(*),IPOBO(*)
C
      LOGICAL FUSION
C
      CHARACTER*72 CBID,TITRE
      CHARACTER*32 TEXTE(26)
      CHARACTER*3  STD
C
C COMMON
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C
C=======================================================================
C LECTURE SEQUENTIELLE DU PREMIER FICHIER
C=======================================================================
C
      REWIND NGEO
C
      CALL LIT(XBID,W,IBID,TITRE,72,'CH',NGEO,STD,ISTAT)
      CALL LIT(XBID,W,IB  ,CBID , 2,'I ',NGEO,STD,ISTAT)
      NVARIN = IB(1) + IB(2)
      DO 10 I=1,NVARIN
         CALL LIT(XBID,W,IBID,TEXTE(I),32,'CH',NGEO,STD,ISTAT)
10    CONTINUE
C     LECTURE ET STOCKAGE DU TABLEAU IPARAM
      CALL LIT(XBID,W,IPARAM  ,CBID,10,'I ',NGEO,STD,ISTAT)
C     LECTURE ET STOCKAGE DE LA DATE
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
C
      NELEM1 = IB(1)
      NPOIN1 = IB(2)
C
      CALL LIT(XBID,W,IKLES,CBID,NELEM1*NDP,'I ',NGEO,STD,ISTAT)
C     LECTURE ET STOCKAGE DES IPOBO (KNOLG EN PARALLELISME)
      CALL LIT(XBID,W,IPOBO,CBID,NPOIN1,'I ',NGEO,STD,ISTAT)
C     LECTURE DES COORDONNEES
      CALL LIT(X   ,W,IBID,CBID,NPOIN1,'R4',NGEO,STD,ISTAT)
      CALL LIT(Y   ,W,IBID,CBID,NPOIN1,'R4',NGEO,STD,ISTAT)
C
C=======================================================================
C LECTURE SEQUENTIELLE DU SECOND FICHIER EN CAS DE FUSION
C=======================================================================
C
      IF (FUSION) THEN
C
         REWIND NFO1
C
         CALL LIT(XBID,W,IBID,CBID,72,'CH',NFO1,STD,ISTAT)
         CALL LIT(XBID,W,IB  ,CBID, 2,'I ',NFO1,STD,ISTAT)
         NVAR2 = IB(1) + IB(2)
         DO 20 I=1,NVAR2
            CALL LIT(XBID,W,IBID,CBID,32,'CH',NFO1,STD,ISTAT)
20       CONTINUE
         CALL LIT(XBID,W,IB  ,CBID,10,'I ',NFO1,STD,ISTAT)
         CALL LIT(XBID,W,IB  ,CBID, 4,'I ',NFO1,STD,ISTAT)
C
         NELEM2 = IB(1)
         NPOIN2 = IB(2)
C
         CALL LIT(XBID,W,IKLES(1,NELEM1+1),CBID,NELEM2*NDP,'I ',
     *            NFO1,STD,ISTAT)
         CALL LIT(XBID,W,IB,CBID, 2,'I ',NFO1,STD,ISTAT)
         CALL LIT(X(NPOIN1+1),W,IBID,CBID,NPOIN2,'R4',NFO1,STD,ISTAT)
         CALL LIT(Y(NPOIN1+1),W,IBID,CBID,NPOIN2,'R4',NFO1,STD,ISTAT)
C
      ENDIF
C
C=======================================================================
C AFFECTATION DES VALEURS LUES AUX VARIABLES CONCERNEES
C=======================================================================
C
      NEINIT = NELEM
      NPINIT = NPOIN
C
C       INVERSION DE IKLES EN IKLE.
C
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
C
      DO 14 I = 1,NPOIN
         XINIT(I) = X(I)
         YINIT(I) = Y(I)
         NCOLOR(I) = 11
14    CONTINUE
C
C=======================================================================
C
      RETURN
      END
