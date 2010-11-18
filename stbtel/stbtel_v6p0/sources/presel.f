C                       *****************
                        SUBROUTINE PRESEL
C                       *****************
C
     *(IKLE,TRAV1,NELEM,NELMAX,NDP,TEXTE,NBFOND,SORLEO,COLOR,
     * NSFOND,NVARIN,NVAROU,MAILLE)
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2    07/12/88    J-M HERVOUET (LNH) 30 87 80 18
C                            19/02/93    J-M JANIN    (LNH) 30 87 72 84
C                                        A   WATRIN
C***********************************************************************
C
C  FONCTION  :  PREPARATION DE DONNEES AVANT L'APPEL DE FMTSEL
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C APPELE PAR : PREDON
C APPEL DE : -
C
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NDP,NELEM,NELMAX,NBFOND,NSFOND,NVARIN,NVAROU
      INTEGER TRAV1(NELEM,NDP),IKLE(NELMAX,NDP),I,IDP,IELEM
C
      CHARACTER*32 TEXTE(26)
      CHARACTER*9 MAILLE
C
      LOGICAL SORLEO(26),COLOR
C
C-----------------------------------------------------------------------
C
C  IKLE EST REFAIT EN FONCTION DU NOMBRE DEFINITIF D'ELEMENTS
C  LE RESULTAT EST MIS DANS TRAV1.
C
      DO 10 IELEM = 1 , NELEM
         DO 20 IDP = 1 , NDP
           TRAV1(IELEM,IDP) = IKLE(IELEM,IDP)
20       CONTINUE
10    CONTINUE
C
C-----------------------------------------------------------------------
C
C  NOMS DES VARIABLES QUI SERONT DANS LE FICHIER DE GEOMETRIE
C  TABLEAUX INDIQUANT SI ELLES SERONT ECRITES.
C
      DO 30 I = 1 , 26
         SORLEO(I) = .FALSE.
30    CONTINUE
C
      NVAROU = NVARIN
      IF (NVAROU.GT.0) THEN
         DO 35 I = 1 , NVAROU
            SORLEO(I) = .TRUE.
35       CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C
C  RAJOUT DU FOND PUIS DE LA COULEUR DES NOEUDS PUIS D'UNE VARIABLE
C  BIDON SI NECESSAIRE DANS LES VARIABLES DE SORTIE
C
      IF (NBFOND.GT.0.AND.NSFOND.EQ.0.AND.NVAROU.LT.26) THEN
         NVAROU = NVAROU + 1
         SORLEO(NVAROU) = .TRUE.
         IF (LNG.EQ.1) TEXTE(NVAROU)='FOND                            '
         IF (LNG.EQ.2) TEXTE(NVAROU)='BOTTOM                          '
         NSFOND = NVAROU
      ELSEIF (NBFOND.EQ.0) THEN
         NSFOND = 0
      ENDIF
C
      IF (COLOR) THEN
         IF (NVAROU.LT.26) THEN
            NVAROU = NVAROU + 1
            SORLEO(NVAROU) = .TRUE.
            TEXTE(NVAROU) = 'COULEUR                         '
         ELSE
            COLOR = .FALSE.
         ENDIF
      ENDIF
C
      IF(NVAROU.EQ.0) THEN
         SORLEO(1) = .TRUE.
        IF(MAILLE.NE.'ADCIRC') THEN
          TEXTE(1) = 'MAILLAGE                        '
        ELSE
          IF (LNG.EQ.1) TEXTE(1)='FOND                            '
          IF (LNG.EQ.2) TEXTE(1)='BOTTOM                          '
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
