C                       *****************
                        SUBROUTINE SHUFLE
C                       *****************
C
     *(IKLE,X)
C
C***********************************************************************
C PROGICIEL : STBTEL  V5.2       19/02/93  J-M JANIN   (LNH) 30 87 72 84
C***********************************************************************
C
C FONCTION : CHANGEMENT DE LA NUMEROTATION DES ELEMENTS
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |  IKLE          |<-->|NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT  |
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
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C APPELE PAR : STBTEL
C APPEL DE : ECHELE
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER MESH , NDP , NELEM , NPMAX , NPOIN , NELMAX
      INTEGER IKLE(NELMAX,4) , I
C
      INTEGER IELEM , I1 , I2 , I3 , I4
C
      DOUBLE PRECISION X(*) , XA
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C
      DO 10 I = 1 , (NELEM-4)/2 , 2
        CALL ECHELE (IKLE,I,NELEM-I+1)
10    CONTINUE
C
C=======================================================================
C
      IF(NDP.EQ.4) THEN
C
        DO 20 IELEM = 1 , NELEM
C
          I1 = IKLE(IELEM,1)
          I2 = IKLE(IELEM,2)
          I3 = IKLE(IELEM,3)
          I4 = IKLE(IELEM,4)
          XA = X(I1)
          IF(XA.LT.X(I2)) THEN
            XA = X(I2)
            IKLE(IELEM,1) = I2
            IKLE(IELEM,2) = I3
            IKLE(IELEM,3) = I4
            IKLE(IELEM,4) = I1
          ENDIF
          IF(XA.LT.X(I3)) THEN
            XA = X(I3)
            IKLE(IELEM,1) = I3
            IKLE(IELEM,2) = I4
            IKLE(IELEM,3) = I1
            IKLE(IELEM,4) = I2
          ENDIF
          IF(XA.LT.X(I4)) THEN
            IKLE(IELEM,1) = I4
            IKLE(IELEM,2) = I1
            IKLE(IELEM,3) = I2
            IKLE(IELEM,4) = I3
          ENDIF
C
20      CONTINUE
C
      ELSEIF(NDP.EQ.3) THEN
C
        DO 30 IELEM = 1 , NELEM
C
          I1 = IKLE(IELEM,1)
          I2 = IKLE(IELEM,2)
          I3 = IKLE(IELEM,3)
          XA = X(I1)
          IF(XA.LT.X(I2)) THEN
            XA = X(I2)
            IKLE(IELEM,1) = I2
            IKLE(IELEM,2) = I3
            IKLE(IELEM,3) = I1
          ENDIF
          IF(XA.LT.X(I3)) THEN
            IKLE(IELEM,1) = I3
            IKLE(IELEM,2) = I1
            IKLE(IELEM,3) = I2
          ENDIF
C
30      CONTINUE
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,*) 'MAILLAGE INCONNU DANS SHUFLE'
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN MESH IN SHUFLE'
        STOP
C
      ENDIF
C
      RETURN
      END
