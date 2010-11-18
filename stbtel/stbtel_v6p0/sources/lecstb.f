C                       *****************
                        SUBROUTINE LECSTB
C                       *****************
C
     *( X , Y ,IKLE , NCOLOR , TITRE , NPOIN1 ,
     *  NGEO , NSEC2,NSEC3,NSEC11,NSEC12)
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2         09/08/89    J-C GALLAND  (LNH)
C***********************************************************************
C
C     FONCTION  :  LECTURE DU FICHIER DE LA GEOMETRIE CREE PAR SUPERTAB
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   X,Y          |<-- | COORDONNEES DU MAILLAGE .
C |   IKLE         |<-- | LISTE DES POINTS DE CHAQUE ELEMENT
C |   NCOLOR       |<-- | TABLEAU DES COULEURS DES POINTS DU MAILLAGE
C |   TITRE        |<-- | TITRE DU MAILLAGE
C |   TRAV1,2      |<-->| TABLEAUX DE TRAVAIL
C |   NPOIN1       | -->| NOMBRE TOTAL DE POINTS
C |________________|____|______________________________________________
C | COMMON:        |    |
C |  GEO:          |    |
C |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
C |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
C |    NPOIN       | -->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C |    NELEM       | -->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
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
C----------------------------------------------------------------------
C APPELE PAR : STBTEL
C APPEL DE : -
C***********************************************************************
C
C    LISTE DES ENREGISTREMENTS DU FICHIER GEOMETRIQUE:
C             (DOCUMENTION: NOTICE SUPERTAB)
C
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER INDIC3 , NGEO , NPOIN , NSEC3 , NPOIN1 , N1 , N2 ,NCOLOI
      INTEGER NELEM , MESH , NDP , NELMAX , NPMAX
      INTEGER IKLE(NELMAX,4) , NCOLOR(*)
      INTEGER NSEC11 , NSEC12 , NSEC2 , NSEC
      INTEGER INDIC1 , INDIC2 , I
C
      DOUBLE PRECISION X(*) , Y(*) , X2 , Y2
      REAL X1 , Y1
C
      CHARACTER*2  MOINS1
      CHARACTER*80 TITRE
      CHARACTER*4  BLANC
C
      INTRINSIC DBLE
C
C COMMON
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C   INITIALISATION
C=======================================================================
C
      INDIC1 = 0
      INDIC2 = 0
      INDIC3 = 0
      REWIND NGEO
C
      DO 5 I=1,NPOIN
         X(I) = 9999999.D0
         Y(I) = 9999999.D0
         NCOLOR(I) = 99999
 5    CONTINUE
C
C=======================================================================
C LECTURE SEQUENTIELLE DU FICHIER ET RECHERCHE DES INDICATEURS
C NSEC1 , NSEC2 ET NSEC3
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
C LECTURE DU TITRE DU MAILLAGE
C=======================================================================
C
      ELSE IF (NSEC.EQ.NSEC3) THEN
         INDIC3 = 1
         READ(NGEO,25,ERR=110,END=120) TITRE
 25      FORMAT(A80)
C
C=======================================================================
C LECTURE DES COORDONNEES ET DE LA COULEUR DES POINTS
C=======================================================================
C
C LECTURE EN SIMPLE PRECISION
C
      ELSE IF (NSEC.EQ.NSEC11) THEN
         INDIC1 = 1
C
         DO 30 I=1,NPOIN1
            READ(NGEO,35,ERR=110,END=120) NSEC,N1,N2,NCOLOI,X1,Y1
C
C PASSAGE EN DOUBLE PRECISION
C
            X(NSEC) = DBLE(X1)
            Y(NSEC) = DBLE(Y1)
            NCOLOR(NSEC) = NCOLOI
 30         CONTINUE
C
 35         FORMAT(4I10,2E13.5)
C
         GOTO 50
C
C LECTURE EN DOUBLE PRECISION
C
      ELSE IF (NSEC.EQ.NSEC12) THEN
         INDIC1 = 1
C
         DO 31 I=1,NPOIN1
            READ(NGEO,36,ERR=110,END=120) NSEC,N1,N2,NCOLOI
            READ(NGEO,37,ERR=110,END=120) X2,Y2
            X(NSEC) = X2
            Y(NSEC) = Y2
            NCOLOR(NSEC) = NCOLOI
 31      CONTINUE
C
 36         FORMAT(4I10)
 37         FORMAT(2D25.16)
C
         GOTO 50
C
C=======================================================================
C LECTURE DE IKLE
C=======================================================================
C
      ELSE IF (NSEC.EQ.NSEC2) THEN
         INDIC2 = 1
         DO 40 I=1,NELEM
            IF (MESH.EQ.2) THEN
               READ(NGEO,2000,ERR=110,END=120) NSEC
               READ(NGEO,4000,ERR=110,END=120) IKLE(I,1),IKLE(I,2),
     *                                          IKLE(I,3),IKLE(I,4)
            ELSE IF (MESH.EQ.3) THEN
               READ(NGEO,2000,ERR=110,END=120) NSEC
               READ(NGEO,4000,ERR=110,END=120) IKLE(I,1),IKLE(I,2),
     *                                          IKLE(I,3)
            ELSE
               IF (LNG.EQ.1) WRITE(LU,1400) MESH
               IF (LNG.EQ.2) WRITE(LU,4400) MESH
 1400          FORMAT(2X,'TYPE DE MAILLAGE NON PREVU : MESH = ',I3)
 4400          FORMAT(2X,'TYPE OF MESH NOT AVAILABLE : MESH = ',I3)
               STOP
            ENDIF
 40      CONTINUE
         GOTO 50
C
      ENDIF
C
 50   IF (INDIC1.EQ.1.AND.INDIC2.EQ.1.AND.INDIC3.EQ.1) THEN
         GOTO 60
      ELSE
         GOTO 10
      ENDIF
C
 110  IF (LNG.EQ.1) WRITE(LU,1100)
      IF (LNG.EQ.2) WRITE(LU,4100)
      STOP
 120  IF (LNG.EQ.1) WRITE(LU,1200)
      IF (LNG.EQ.2) WRITE(LU,4200)
      STOP
C
 60   CONTINUE
C
 2000 FORMAT(I10)
 4000 FORMAT(4I10)
 1100 FORMAT(/,'*************************************************',/,
     *         'ERREUR A LA LECTURE DU FICHIER UNIVERSEL (LECSTB)',/,
     *         '*************************************************')
 4100 FORMAT(/,'****************************************',/,
     *         'ERROR IN READING UNIVERSAL FILE (LECSTB)',/,
     *         '****************************************')
 1200 FORMAT(/,'******************************************',/,
     *         'FIN DU FICHIER UNIVERSEL : ERREUR (LECSTB)',/,
     *         '******************************************')
 4200 FORMAT(/,'******************************************',/,
     *         'END OF THE UNIVERSAL FILE : ERROR (LECSTB)',/,
     *         '******************************************')
C
      RETURN
      END
