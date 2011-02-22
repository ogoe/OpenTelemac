C                       *****************
                        SUBROUTINE ECHELE
C                       *****************
C
     *(IKLE, IEL1 , IEL2 )
C
C***********************************************************************
C PROGICIEL: STBTEL V5.2          28/08/89  J-M HERVOUET (LNH) 3071 8018
C***********************************************************************
C
C FONCTION : ECHANGE DES NUMEROS DE 2 ELEMENTS
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |  IKLE          |<-->|VECTEUR ASSEMBLE                              |
C |  IEL1, IEL2    | -->|NUMEROS DES NOEUDS A PERMUTER                 |
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
C APPELE PAR : SHUFLE
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER MESH , NDP , I , IEL1 , IEL2 , NPMAX , NELEM
      INTEGER NPOIN , NELMAX
      INTEGER IKLE(NELMAX,4) , STO(4)
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C
      DO 10 I = 1 , NDP
        STO(I) = IKLE(IEL1,I)
        IKLE(IEL1,I) = IKLE(IEL2,I)
        IKLE(IEL2,I) = STO(I)
 10   CONTINUE
C
C=======================================================================
C
      RETURN
      END
