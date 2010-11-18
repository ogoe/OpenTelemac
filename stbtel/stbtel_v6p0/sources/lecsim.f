C                       *****************
                        SUBROUTINE LECSIM
C                       *****************
C
     *( X , Y , IKLE , NCOLOR , TITRE , NOP5 , NGEO )
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2         25/02/92    J-C GALLAND  (LNH)
C***********************************************************************
C
C     FONCTION  :  LECTURE DU FICHIER DE LA GEOMETRIE CREE PAR SIMAIL
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   X,Y          |<-- | COORDONNEES DU MAILLAGE .
C |   X1,Y1        |<-- | COORDONNEES DU MAILLAGE LUES EN SIMPLE
C |                |    | PRECISION DANS LE FICHIER SIMAIL
C |   IKLE         |<-- | LISTE DES POINTS DE CHAQUE ELEMENT
C |   NCOLOR       |<-- | TABLEAU DES COULEURS DES POINTS DU MAILLAGE
C |   TITRE        |<-- | TITRE DU MAILLAGE
C |   NOP5         | -->| TABLEAU DE TRAVAIL POUR LA LECTURE DE LA SD
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
C |    NRES        | -->| NUMERO DU CANAL DU FICHIER DE SERAFIN
C |    NGEO       | -->| NUMERO DU CANAL DU FICHIER MAILLEUR
C |    NLIM      | -->| NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
C |    NFO1      |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C----------------------------------------------------------------------
C APPELE PAR : STBTEL
C APPEL DE : -
C***********************************************************************
C
C    LISTE DES ENREGISTREMENTS DU FICHIER GEOMETRIQUE:
C             (DOCUMENTION: NOTICE SIMAIL)
C
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NGEO , NPOIN , ERR
      INTEGER NELEM , MESH , NDP , NELMAX , NPMAX
      INTEGER IKLE(NELMAX,4) , NCOLOR(*)
      INTEGER I,J,K
      INTEGER NOP5(*)
      INTEGER LONG , NTASD
      INTEGER NCGE , NMAE , NDSDE , NNO , NCOPNP ,NPO
      INTEGER INING , NBEGM , INDIC
C
      DOUBLE PRECISION X(*) , Y(*)
C
      REAL, DIMENSION(:), ALLOCATABLE :: X1,Y1
C
      CHARACTER*80 TITRE
C
      INTRINSIC DBLE
C
C COMMON
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C-----------------------------------------------------------------------
C
      ALLOCATE(X1(NPOIN),STAT=ERR)
      ALLOCATE(Y1(NPOIN),STAT=ERR)
C
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,7000) ERR
        IF(LNG.EQ.2) WRITE(LU,8000) ERR
7000    FORMAT(1X,'LECSIM : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     *            'CODE D''ERREUR : ',1I6)
8000    FORMAT(1X,'LECSIM: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     *            'ERROR CODE: ',1I6)
      ENDIF
C
C=======================================================================
C   INITIALISATION
C=======================================================================
C
      REWIND NGEO
C
      DO 5 I=1,NPOIN
         X(I) = 9999999.D0
         Y(I) = 9999999.D0
         NCOLOR(I) = 99999
 5    CONTINUE
C
C=======================================================================
C LECTURE SEQUENTIELLE DU FICHIER (1ER ENRGISTREMENT DE LA SD)
C POUR LES LECTURES BIDON, ON UTILISE NOP5
C=======================================================================
C
      READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
C
C=======================================================================
C LECTURE SEQUENTIELLE DU FICHIER (TABLEAU NOP0) . LECTURE DU TITRE
C POUR LES LECTURES BIDON, ON UTILISE NOP5
C=======================================================================
C
      READ(NGEO,ERR=110,END=120) LONG,TITRE,(NOP5(I),I=1,11),NTASD
C
C=======================================================================
C LECTURE SEQUENTIELLE DU FICHIER (TABLEAUX NOP1 ET ASSOCIES)
C POUR LES LECTURES BIDON, ON UTILISE NOP5
C=======================================================================
C
      IF (NTASD.GT.0) THEN
         READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
         DO 10 I=1,NTASD
            READ(NGEO,ERR=110,END=120) LONG,(NOP5(J),J=1,LONG)
 10      CONTINUE
      ENDIF
C
C=======================================================================
C LECTURE SEQUENTIELLE DU FICHIER (TABLEAU NOP2)
C POUR LES LECTURES BIDON, ON UTILISE NOP5
C=======================================================================
C
      READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
      NCOPNP = NOP5(4)
      NBEGM  = NOP5(25)
C
C=======================================================================
C LECTURE SEQUENTIELLE DU FICHIER (TABLEAU NOP3)
C POUR LES LECTURES BIDON, ON UTILISE NOP5
C=======================================================================
C
      IF (NBEGM.NE.0) THEN
         READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
      ENDIF
C
C=======================================================================
C LECTURE SEQUENTIELLE DES COORDONNEES DES NOEUDS (TABLEAU NOP4)
C=======================================================================
C
      READ(NGEO,ERR=110,END=120) LONG,(X1(I),Y1(I),I=1,NPOIN)
      DO 20 I=1,NPOIN
         X(I) = DBLE(X1(I))
         Y(I) = DBLE(Y1(I))
 20   CONTINUE
C
C=======================================================================
C LECTURE SEQUENTIELLE DES IKLE (TABLEAU NOP5)
C POUR LES LECTURE BIDON, ON UTILISE NOP5 ET LONG
C=======================================================================
C
      INDIC = 0
      READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
      DO 30 I=1,NELEM
         INDIC = INDIC +1
         NCGE  = NOP5(INDIC)
         INDIC = INDIC +1
         NMAE  = NOP5(INDIC)
         INDIC = INDIC +1
         NDSDE = NOP5(INDIC)
         INDIC = INDIC +1
         NNO   = NOP5(INDIC)
C NNO : NOMBRE DE NOEUDS PAR ELEMENT
         IF ( (NNO.EQ.4.AND.MESH.NE.2) .OR. (NNO.EQ.3.AND.MESH.NE.3) )
     *   THEN
            IF (LNG.EQ.1) WRITE(LU,1000)
            IF (LNG.EQ.2) WRITE(LU,4000)
            STOP
         ENDIF
         DO 40 K=1,NNO
            INDIC = INDIC +1
            IKLE(I,K) = NOP5(INDIC)
 40      CONTINUE
         IF (NCOPNP.EQ.1) GOTO 50
         INDIC = INDIC +1
         NPO = NOP5(INDIC)
         DO 60 K=1,NPO
            INDIC = INDIC +1
 60      CONTINUE
 50      CONTINUE
C  NMAE :
         IF (NMAE.EQ.0) GOTO 30
         INDIC = INDIC +1
         INING = NOP5(INDIC)
         DO 70 K=2,NMAE
            IF (INING.EQ.3) THEN
               INDIC = INDIC +1
               NCOLOR(IKLE(I,K-1)) = NOP5(INDIC)
            ELSE IF(INING.EQ.2) THEN
               INDIC = INDIC +1
               IF (K.GT.NNO+1) NCOLOR(IKLE(I,K-(NNO+1))) = NOP5(INDIC)
            ELSE IF(INING.EQ.1) THEN
               INDIC = INDIC +1
               IF (K.GT.2*NNO+1)
     *         NCOLOR(IKLE(I,K-(2*NNO+1))) = NOP5(INDIC)
            ENDIF
 70      CONTINUE
 30   CONTINUE
C
      GOTO 80
C
 110  IF (LNG.EQ.1) WRITE(LU,1100)
      IF (LNG.EQ.2) WRITE(LU,4100)
 120  IF (LNG.EQ.1) WRITE(LU,1200)
      IF (LNG.EQ.2) WRITE(LU,4200)
C
 80   CONTINUE
C
C=======================================================================
C
      DEALLOCATE (X1)
      DEALLOCATE (Y1)
C
C=======================================================================
C
1000  FORMAT(//,'*********************************************',/,
     *          'LECSIM : IL N''Y A PAS DE LIEN ENTRE LE NOMBRE ',/,
     *          'DE POINTS PAR ELEMENT ET LE TYPE DES ELEMENTS ',/,
     *          '**********************************************',//)
4000  FORMAT(//,'*********************************************',/,
     *          'LECSIM : THERE IS NO LINK BETWEEN THE NUMBER  ',/,
     *          'OF POINTS BY ELEMENT AND THE TYPE OF ELEMENTS ',/,
     *          '**********************************************',//)
1100  FORMAT(//,'**********************************************',/,
     *          'LECSIM : ERREUR A LA LECTURE DU FICHIER SIMAIL',/,
     *          '**********************************************',//)
4100  FORMAT(//,'**********************************************',/,
     *          'LECSIM : ERROR IN READING FILE SIMAIL         ',/,
     *          '**********************************************',//)
1200  FORMAT(//,'*****************************************',/,
     *          'LECSIM : FIN PREMATUREE DU FICHIER SIMAIL',/,
     *          '*****************************************',//)
4200  FORMAT(//,'*************************************************',/,
     *          'LECSIM : ATTEMPT TO READ AFTER END OF FILE SIMAIL ',/,
     *          '*************************************************',//)
C
      RETURN
      END
