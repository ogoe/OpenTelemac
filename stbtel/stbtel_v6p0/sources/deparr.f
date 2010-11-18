C                       *****************
                        SUBROUTINE DEPARR
C                       *****************
C
     *(IKLE,NDEPAR,LGVEC)
C
C***********************************************************************
C PROGICIEL: STBTEL V5.2          28/08/89  J-M HERVOUET (LNH) 3071 8018
C***********************************************************************
C
C FONCTION : DETECTION DES DEPENDANCES ARRIERES
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |  IKLE          | -->|NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT  |
C |  NDEPAR        | -->|NOMBRE DE DEPENDANCES ARRIERES                |
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
C APPEL DE : -
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER LGVEC , MESH , NDP , NDEPAR , IELEM , NELEM , K
      INTEGER NPMAX , NPOIN , NELMAX
      INTEGER I1 , I2 , I3 , J1 , J2 , J3 , IEL1
      INTEGER IKLE(NELMAX,4)
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C
      NDEPAR = 0
      DO 20 IELEM = 1,NELEM
         I1 = IKLE(IELEM,1)
         I2 = IKLE(IELEM,2)
         I3 = IKLE(IELEM,3)
         DO 30 K = 2,LGVEC
            IEL1 = MOD(NELEM+IELEM-K,NELEM) + 1
            J1 = IKLE(IEL1,1)
            J2 = IKLE(IEL1,2)
            J3 = IKLE(IEL1,3)
            IF (I1.EQ.J1.OR.I1.EQ.J2.OR.I1.EQ.J3.OR.
     *          I2.EQ.J1.OR.I2.EQ.J2.OR.I2.EQ.J3.OR.
     *          I3.EQ.J1.OR.I3.EQ.J2.OR.I3.EQ.J3) NDEPAR = NDEPAR + 1
30       CONTINUE
20    CONTINUE
C
C=======================================================================
C
      RETURN
      END
