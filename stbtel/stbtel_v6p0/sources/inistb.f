C                       *****************
                        SUBROUTINE INISTB
C                       *****************
C
     *(NPOIN1,TYPELE,MAILLE,PRECIS,NGEO,NSEC2,NSEC11,NSEC12)
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2            09/08/89    J.C. GALLAND
C***********************************************************************
C
C   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
C               MAILLAGE DANS LE FICHIER UNIVERSEL DE SUPERTAB
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
C | MAILLE         |<-- | NOM DU MAILLEUR
C | PRECIS         |<-- | FORMAT DE LECTURE DES COORDONNEES DES NOEUDS
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
C |  SECT:         |    |
C |    NSEC11      |--> | INDICATEUR DU SECTEUR CONTENANT LES NOEUDS
C |                |    | (LECTURE EN SIMPLE PRECISION)
C |    NSEC12      |--> | INDICATEUR DU SECTEUR CONTENANT LES NOEUDS
C |                |    | (LECTURE EN DOUBLE PRECISION)
C |    NSEC2       |--> | INDICATEUR DU SECTEUR CONTENANT LES ELEMENTS
C |    NSEC3       |--> | INDICATEUR DU SECTEUR CONTENANT LE TITRE
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
      DOUBLE PRECISION X1
C
      INTEGER NPOIN1 , NPOIN2 
      INTEGER NPOIN , NELEM , NPMAX , NELMAX , MESH , NDP
      INTEGER NSEC11 , NSEC12 , NSEC2 , N1
      INTEGER NSEC , NGEO , N2
      INTEGER INDI11 , INDI12 , INDIC2
C
      CHARACTER*2  MOINS1
      CHARACTER*4  BLANC*4
      CHARACTER*11 TYPELE
      CHARACTER*9  MAILLE
      CHARACTER*6  PRECIS
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
      NPOIN1  = 0
      NPOIN2  = 0
      NELEM   = 0
      INDI11  = 0
      INDI12  = 0
      INDIC2  = 0
C
C=======================================================================
C LECTURE SEQUENTIELLE DU FICHIER ET RECHERCHE DES INDICATEURS
C NSEC1 ET NSEC2
C=======================================================================
C
 10   READ(NGEO,1000,ERR=110,END=120) BLANC,MOINS1
      IF (MOINS1.NE.'-1'.OR.BLANC.NE.'    ') GOTO 10
 1000 FORMAT(A4,A2)
C
 20   READ(NGEO,2000,ERR=110,END=120) NSEC
      IF (NSEC.EQ.-1) THEN
         GOTO 20
C
C=======================================================================
C NOMBRE DE POINTS
C=======================================================================
C
C LECTURE EN SIMPLE PRECISION
C
      ELSE IF (NSEC.EQ.NSEC11) THEN
         INDI11 = 1
C
 30      CONTINUE
         READ(NGEO,2000,ERR=110,END=120) NSEC
C
         IF (NSEC.NE.-1) THEN
            NPOIN1 = NPOIN1+1
            NPOIN2 = MAX0(NSEC,NPOIN1)
            GOTO 30
         ELSE
            GOTO 50
         ENDIF
C
C LECTURE EN DOUBLE PRECISION
C
      ELSE IF (NSEC.EQ.NSEC12) THEN
         INDI12 = 1
C
 31      CONTINUE
         READ(NGEO,2000,ERR=110,END=120) NSEC
C
         IF (NSEC.NE.-1) THEN
            NPOIN1 = NPOIN1+1
            NPOIN2 = MAX0(NSEC,NPOIN1)
         ELSE
            GOTO 50
         ENDIF
C
         READ(NGEO,4000,ERR=110,END=120) X1
C
         GOTO 31
C
C=======================================================================
C NOMBRE ET TYPE D'ELEMENTS
C=======================================================================
C
      ELSE IF (NSEC.EQ.NSEC2) THEN
         INDIC2 = 1
         IF (MAILLE.EQ.'SUPERTAB4') THEN
C  LECTURE AU FORMAT SUPERTAB VERSION 4
            READ(NGEO,3000,ERR=110,END=120) N1,N2,MESH
         ELSE
C  LECTURE AU FORMAT SUPERTAB VERSION 6
            READ(NGEO,3000,ERR=110,END=120) N1,MESH,N2
         ENDIF
         NELEM = 1
 40      READ(NGEO,2000,ERR=110,END=120) NSEC
         IF (NSEC.NE.-1) THEN
            NELEM = NELEM+1
            GOTO 40
         ELSE
            GOTO 50
         ENDIF
      ENDIF
C
 50   IF ((INDI11.EQ.1.OR.INDI12.EQ.1).AND.INDIC2.EQ.1) THEN
         GOTO 60
      ELSE
         GOTO 10
      ENDIF
C
 110  IF (LNG.EQ.1) WRITE(LU,1100)
      IF (LNG.EQ.2) WRITE(LU,4100)
      STOP
C
 120  CONTINUE
      IF ((INDI11.NE.1.AND.INDI12.NE.1).AND.(LNG.EQ.1)) WRITE(LU,1200)
      IF ((INDI11.NE.1.AND.INDI12.NE.1).AND.(LNG.EQ.2)) WRITE(LU,4200)
      IF ((INDI12.NE.1).AND.(LNG.EQ.1)) WRITE(LU,1300)
      IF ((INDI12.NE.1).AND.(LNG.EQ.2)) WRITE(LU,4300)
      STOP
C
 60   CONTINUE
C
C=======================================================================
C AFFECTATION DES VALEURS LUES AUX VARIABLES CONCERNEES
C=======================================================================
C
      IF (INDI11.EQ.1) PRECIS='SIMPLE'
      IF (INDI12.EQ.1) PRECIS='DOUBLE'
C
      NELEM =NELEM / 2
C
      NPOIN = NPOIN2
C
C=======================================================================
C MISE DES VALEURS DE MESH AU STANDARD TELEMAC
C=======================================================================
C
      IF (MESH.EQ.94) THEN
        MESH = 2
        NDP  = 4
        TYPELE = 'QUADRANGLES'
      ELSEIF (MESH.EQ.91) THEN
        MESH = 3
        NDP  = 3
        TYPELE = 'TRIANGLES  '
      ELSE
        IF (LNG.EQ.1) WRITE(LU,140) MESH
        IF (LNG.EQ.2) WRITE(LU,3140) MESH
 140    FORMAT(' INISTB : TYPE DE MAILLAGE NON PREVU DANS TELEMAC,
     *           MESH = ',I4)
 3140   FORMAT(' INISTB : TYPE OF MESH NOT AVAILABLE IN TELEMAC,
     *           MESH = ',I4)
      ENDIF
C
C=======================================================================
C IMPRESSION DES RESULTATS
C=======================================================================
C
 1100 FORMAT(//,'*************************************************',/,
     *          'ERREUR A LA LECTURE DU FICHIER UNIVERSEL (INISTB)',/,
     *          '*************************************************')
 4100 FORMAT(//,'****************************************',/,
     *          'ERROR IN READING UNIVERSAL FILE (INISTB)',/,
     *          '****************************************')
 1200 FORMAT(//,'*************************************************',/,
     *          'FIN DU FICHIER UNIVERSEL : PAS DE NOEUDS (INISTB)',/,
     *          '*************************************************')
 4200 FORMAT(//,'********************************************',/,
     *          'END OF THE UNIVERSAL FILE : NO NODE (INISTB)',/,
     *          '********************************************')
 1300 FORMAT(//,'**************************************************',/,
     *          'FIN DU FICHIER UNIVERSEL : PAS D''ELEMENTS (INISTB)',/,
     *          '**************************************************')
 4300 FORMAT(//,'***********************************************',/,
     *          'END OF THE UNIVERSAL FILE : NO ELEMENT (INISTB)',/,
     *          '***********************************************')
 2000 FORMAT(I10)
 3000 FORMAT(3I10)
 4000 FORMAT(D25.16)
C
      RETURN
      END
