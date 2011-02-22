C                       *****************
                        SUBROUTINE INISEL
C                       *****************
C
     *(NPOIN1,TYPELE,STD,NSFOND,FUSION,IHAUT,NGEO,NFO1)
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
C | IHAUT          |<-- | NUMERO DE LA VARIABLE HAUTEUR D'EAU 
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
C |    NGEO        |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
C |    NLIM        |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
C |    NFO1        |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
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
      DOUBLE PRECISION XBID(1)
C
      REAL WBID(1)
C
      INTEGER NGEO , NFO1
      INTEGER MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
      INTEGER IBID(1) , NVAR , I , IB(10) , ISTAT , NPOIN1 , NSFOND
      INTEGER IHAUT
C
      LOGICAL FUSION
C
      CHARACTER*72 CBID
      CHARACTER*32 NOMVAR
      CHARACTER*11 TYPELE
      CHARACTER*3  STD
C
C COMMON
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C
C=======================================================================
C INITIALISATION
C=======================================================================
C
      REWIND NGEO
C
C=======================================================================
C LECTURE SEQUENTIELLE DU FICHIER ET RECHERCHE ENREGISTREMENT 5
C=======================================================================
C
      CALL LIT(XBID,WBID,IBID,CBID,72,'CH',NGEO,STD,ISTAT)
      CALL LIT(XBID,WBID,IB  ,CBID, 2,'I ',NGEO,STD,ISTAT)
      NVAR = IB(1) + IB(2)
      IHAUT = 0
      DO 10 I=1,NVAR
         CALL LIT(XBID,WBID,IBID,NOMVAR,32,'CH',NGEO,STD,ISTAT)
         IF( NOMVAR(1:16) .EQ. 'FOND            ' .OR.
     *       NOMVAR(1:16) .EQ. 'BOTTOM          ' ) NSFOND = I
         IF( NOMVAR(1:16) .EQ. 'HAUTEUR D''EAU   ' .OR.
     *       NOMVAR(1:16) .EQ. 'WATER DEPTH     ' ) IHAUT = I
10    CONTINUE
      CALL LIT(XBID,WBID,IB  ,CBID,10,'I ',NGEO,STD,ISTAT)
C   
C     CORRECTION JMH 07/03/2001
C
C     CAS OU IL Y A UNE DATE DANS LE FICHIER
      IF(IB(10).EQ.1) THEN
        CALL LIT(XBID,WBID,IB  ,CBID,6,'I ',NGEO,STD,ISTAT)        
      ENDIF
C   
C     FIN CORRECTION JMH 07/03/2001
C
      CALL LIT(XBID,WBID,IB  ,CBID, 4,'I ',NGEO,STD,ISTAT)
C
C=======================================================================
C AFFECTATION DES VALEURS LUES AUX VARIABLES CONCERNEES
C=======================================================================
C
      NELEM = IB(1)
      NPOIN = IB(2)
      NDP   = IB(3)
      NPOIN1= NPOIN
C
C=======================================================================
C LECTURE SEQUENTIELLE DU SECOND FICHIER EN CAS DE FUSION
C=======================================================================
C
      IF (FUSION) THEN
C
         REWIND NFO1
C
         CALL LIT(XBID,WBID,IBID,CBID,72,'CH',NFO1,STD,ISTAT)
         CALL LIT(XBID,WBID,IB  ,CBID, 2,'I ',NFO1,STD,ISTAT)
         NVAR = IB(1) + IB(2)
         DO 20 I=1,NVAR
            CALL LIT(XBID,WBID,IBID,NOMVAR,32,'CH',NFO1,STD,ISTAT)
20       CONTINUE
         CALL LIT(XBID,WBID,IB  ,CBID,10,'I ',NFO1,STD,ISTAT)
         CALL LIT(XBID,WBID,IB  ,CBID, 4,'I ',NFO1,STD,ISTAT)
C
         NELEM = NELEM + IB(1)
         NPOIN = NPOIN + IB(2)
C
         IF (NDP.NE.IB(3)) THEN
            IF (LNG.EQ.1) WRITE(LU,130)
            IF (LNG.EQ.2) WRITE(LU,3130)
 130        FORMAT(' INISEL : TYPES DE MAILLAGE HETEROGENES')
 3130       FORMAT(' INISEL : TYPES OF MESH INHOMOGENEOUS')
            STOP
         ENDIF
C
      ENDIF
C
C=======================================================================
C MISE DES VALEURS DE MESH AU STANDARD TELEMAC
C=======================================================================
C
      IF (NDP.EQ.4) THEN
        MESH = 2
        TYPELE = 'QUADRANGLES'
      ELSEIF (NDP.EQ.3) THEN
        MESH = 3
        TYPELE = 'TRIANGLES  '
      ELSE
        IF (LNG.EQ.1) WRITE(LU,140) MESH
        IF (LNG.EQ.2) WRITE(LU,3140) MESH
 140    FORMAT(' INISEL : TYPE DE MAILLAGE NON PREVU DANS TELEMAC,
     *           MESH = ',I4)
 3140   FORMAT(' INISEL : TYPE OF MESH NOT AVAILABLE IN TELEMAC,
     *           MESH = ',I4)
        STOP
      ENDIF
C
      RETURN
      END
