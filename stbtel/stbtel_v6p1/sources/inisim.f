C                       *****************
                        SUBROUTINE INISIM
C                       *****************
C
     *(NPOIN1,TYPELE,INOP5,NGEO)
C
C***********************************************************************
C PROGICIEL : STBTEL  V5.2            25/02/92    J.C. GALLAND
C***********************************************************************
C
C   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
C               MAILLAGE, AINSI QUE LA LONGUEUR DU TABLEAU NOP5 DANS LE
C               FICHIER GEOMETRIE DE SIMAIL
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C | NPOIN1         |<-- | NOMBRE REEL DE POINTS DU MAILLAGE
C | TYPELE         |<-- | TYPE D'ELEMENTS
C | IA             |--> | TABLEAU DE TRAVAIL POUR LA LECTURE DE LA SD
C | INOP5          |<-- | DIMENSION DU TABLEAU NOP5 (CONTENANT LES IKLE)
C |                |    | DU FICHIER GEOMETRIE DE SIMAIL
C |________________|____|______________________________________________
C | COMMON:        |    |
C |  GEO:          |    |
C |    MESH        |<-- | TYPE DES ELEMENTS DU MAILLAGE
C |    NDP         |<-- | NOMBRE DE NOEUDS PAR ELEMENTS
C |    NPOIN       |<-- | NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C |    NELEM       |<-- | NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
C |    NPMAX       |<-- | DIMENSION EFFECTIVE DES TABLEAUX X ET Y
C |                |    | (NPMAX = NPOIN + 0.1*NELEM)
C |    NELMAX      |<-- | DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
C |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
C |  FICH:         |    |
C |    NRES        |--> | NUMERO DU CANAL DU FICHIER DE SERAFIN
C |    NGEO        |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
C |    NDYNAM      |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
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
      INTEGER NGEO
      INTEGER NPOIN1 , NELEM , NPMAX , NELMAX , MESH
      INTEGER NPOIN , IA(32) , INOP5
      INTEGER LONG , I , J , NTASD , MESHT , MESHQ , NDP
      CHARACTER*11 TYPELE
C
C COMMON
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C INITIALISATION
C=======================================================================
C
      REWIND NGEO
      NPOIN  = 0
      NPOIN1 = 0
      NELEM  = 0
C
C=======================================================================
C LECTURE PARTIELLE DU 1ER ENREGISTREMENT DE LA SD
C=======================================================================
C
      READ(NGEO,ERR=110,END=120) LONG,(IA(I),I=1,MIN(LONG,32))
C
C=======================================================================
C LECTURE PARTIELLE DU TABLEAU NOP0
C RECHERCHE DU NOMBRE DE TABLEAUX ASSOCIES, NTASD
C=======================================================================
C
      READ(NGEO,ERR=110,END=120) LONG,(IA(I),I=1,32)
      NTASD = IA(32)
C
C=======================================================================
C LECTURE DU TABLEAU NOP1 ET DES TABLEAUX ASSOCIES
C=======================================================================
C
      IF (NTASD.GT.0) THEN
        READ(NGEO,ERR=110,END=120) LONG,(IA(I),I=1,MIN(LONG,32))
        DO 10 I=1,NTASD
          READ(NGEO,ERR=110,END=120) LONG,(IA(J),J=1,MIN(LONG,32))
10      CONTINUE
      ENDIF
C
C=======================================================================
C LECTURE DU TABLEAU NOP2
C LECTURE DU NOMBRE DE POINTS, DU NOMBRE D'ELEMENTS, DU TYPE D'ELEMENT
C ET DE LA LONGUEUR DU TABLEAU NOP5 (TABLEAU DES IKLE)
C AFFECTATION DES VALEURS LUES AUX VARIABLES CONCERNEES
C=======================================================================
C
      READ(NGEO,ERR=110,END=120) LONG,(IA(I),I=1,MIN(LONG,32))
      NPOIN1 = IA(15)
      NELEM  = IA(5)
      MESHT  = IA(8)
      MESHQ  = IA(9)
      INOP5  = IA(26)
C
      NPOIN = NPOIN1
C
C=======================================================================
C MISE DES VALEURS DE MESH AU STANDARD TELEMAC
C=======================================================================
C
      IF (MESHQ.NE.0) THEN
        MESH = 2
        NDP  = 4
        TYPELE = 'QUADRANGLES'
      ELSEIF (MESHT.NE.0) THEN
        MESH = 3
        NDP  = 3
        TYPELE = 'TRIANGLES  '
      ELSE
        IF (LNG.EQ.1) WRITE(LU,100)
        IF (LNG.EQ.2) WRITE(LU,3100)
        STOP
      ENDIF
C
      GOTO 20
C
 110  IF (LNG.EQ.1) WRITE(LU,1100)
      IF (LNG.EQ.2) WRITE(LU,4100)
 120  IF (LNG.EQ.1) WRITE(LU,1200)
      IF (LNG.EQ.2) WRITE(LU,4200)
C
20    CONTINUE
C
C=======================================================================
C IMPRESSION DES RESULTATS
C=======================================================================
C
 100  FORMAT(/,'*******************************************************'
     *      ,/,'INISIM : TELEMAC NE TRAITE PAS LES MAILLAGES COMPORTANT'
     *      ,/,'         A LA FOIS DES TRIANGLES ET DES QUADRANGLES',
     *      /,'*******************************************************')
 3100 FORMAT(/,'*************************************************'
     *      ,/,'INISIM : TELEMAC DOESN''T WORK WITH MESHES MIXING '
     *      ,/,'         TRIANGLES AND QUADRILATERALS',
     *      /,'**************************************************')
 1100 FORMAT(//,'**********************************************',/,
     *          'INISIM : ERREUR A LA LECTURE DU FICHIER SIMAIL',/,
     *          '**********************************************',//)
 4100 FORMAT(//,'*************************************',/,
     *          'INISIM : ERROR IN READING FILE SIMAIL',/,
     *          '*************************************',//)
 1200 FORMAT(//,'*****************************************',/,
     *          'INISIM : FIN PREMATUREE DU FICHIER SIMAIL',/,
     *          '*****************************************',//)
 4200 FORMAT(//,'*************************************************',/,
     *          'INISIM : ATTEMPT TO READ AFTER END OF FILE SIMAIL',/,
     *          '*************************************************',//)
C
      RETURN
      END
