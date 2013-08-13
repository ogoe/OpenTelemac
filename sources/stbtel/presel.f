!                       *****************
                        SUBROUTINE PRESEL
!                       *****************
!
     &(IKLE,TRAV1,NELEM,NELMAX,NDP,TEXTE,NBFOND,SORLEO,COLOR,
     & NSFOND,NVARIN,NVAROU,MAILLE)
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2    07/12/88    J-M HERVOUET (LNH) 30 87 80 18
!                            19/02/93    J-M JANIN    (LNH) 30 87 72 84
!                                        A   WATRIN
!***********************************************************************
!
!  FONCTION  :  PREPARATION DE DONNEES AVANT L'APPEL DE FMTSEL
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! APPELE PAR : PREDON
! APPEL DE : -
!
!***********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NDP,NELEM,NELMAX,NBFOND,NSFOND,NVARIN,NVAROU
      INTEGER TRAV1(NELEM,NDP),IKLE(NELMAX,NDP),I,IDP,IELEM
!
      CHARACTER*32 TEXTE(26)
      CHARACTER*9 MAILLE
!
      LOGICAL SORLEO(26),COLOR
!
!-----------------------------------------------------------------------
!
!  IKLE EST REFAIT EN FONCTION DU NOMBRE DEFINITIF D'ELEMENTS
!  LE RESULTAT EST MIS DANS TRAV1.
!
      DO 10 IELEM = 1 , NELEM
         DO 20 IDP = 1 , NDP
           TRAV1(IELEM,IDP) = IKLE(IELEM,IDP)
20       CONTINUE
10    CONTINUE
!
!-----------------------------------------------------------------------
!
!  NOMS DES VARIABLES QUI SERONT DANS LE FICHIER DE GEOMETRIE
!  TABLEAUX INDIQUANT SI ELLES SERONT ECRITES.
!
      DO 30 I = 1 , 26
         SORLEO(I) = .FALSE.
30    CONTINUE
!
      NVAROU = NVARIN
      IF (NVAROU.GT.0) THEN
         DO 35 I = 1 , NVAROU
            SORLEO(I) = .TRUE.
35       CONTINUE
      ENDIF
!
!-----------------------------------------------------------------------
!
!  RAJOUT DU FOND PUIS DE LA COULEUR DES NOEUDS PUIS D'UNE VARIABLE
!  BIDON SI NECESSAIRE DANS LES VARIABLES DE SORTIE
!
      IF (NBFOND.GT.0.AND.NSFOND.EQ.0.AND.NVAROU.LT.26) THEN
         NVAROU = NVAROU + 1
         SORLEO(NVAROU) = .TRUE.
         IF (LNG.EQ.1) TEXTE(NVAROU)='FOND                            '
         IF (LNG.EQ.2) TEXTE(NVAROU)='BOTTOM                          '
         NSFOND = NVAROU
      ELSEIF (NBFOND.EQ.0) THEN
         NSFOND = 0
      ENDIF
!
      IF (COLOR) THEN
         IF (NVAROU.LT.26) THEN
            NVAROU = NVAROU + 1
            SORLEO(NVAROU) = .TRUE.
            TEXTE(NVAROU) = 'COULEUR                         '
         ELSE
            COLOR = .FALSE.
         ENDIF
      ENDIF
!
      IF(NVAROU.EQ.0) THEN
         SORLEO(1) = .TRUE.
        IF(MAILLE.NE.'ADCIRC') THEN
          TEXTE(1) = 'MAILLAGE                        '
        ELSE
          IF (LNG.EQ.1) TEXTE(1)='FOND                            '
          IF (LNG.EQ.2) TEXTE(1)='BOTTOM                          '
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END