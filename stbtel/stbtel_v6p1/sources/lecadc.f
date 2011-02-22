C                       *****************
                        SUBROUTINE LECADC
C                       *****************
C
     *( X , Y , ZF , IKLE , NGEO )
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2         13/08/01    J-M HERVOUET  (LNH)
C
C***********************************************************************
C
C     FONCTION  :  LECTURE DU FICHIER DE LA GEOMETRIE CREE PAR ADCIRC
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
C |   TITRE        |<-- | TITRE DU MAILLAGE
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
      INTEGER NGEO , NPOIN , IBID
      INTEGER NELEM , MESH , NDP , NELMAX , NPMAX
      INTEGER IKLE(NELMAX,4)
      INTEGER I,J
C
      DOUBLE PRECISION X(*) , Y(*),ZF(*)
C
C COMMON
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C   INITIALISATION
C=======================================================================
C
C     REWIND NGEO
C
C
C=======================================================================
C LECTURE SEQUENTIELLE DES COORDONNEES
C=======================================================================
C
      DO I=1,NPOIN
        READ(NGEO,*) J,X(I),Y(I),ZF(I)
        IF(I.NE.J) THEN
          WRITE(LU,*) 'ERROR IN THE LIST OF COORDINATES LINE ',I
          STOP
        ENDIF
      ENDDO
C
C=======================================================================
C LECTURE SEQUENTIELLE DU TABLEAU IKLE
C=======================================================================
C
      DO I=1,NELEM
        READ(NGEO,*) J,IBID,IKLE(I,1),IKLE(I,2),IKLE(I,3)
      ENDDO
C
C=======================================================================
C
      RETURN
      END
