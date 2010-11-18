C                       *****************
                        SUBROUTINE CORDEP
C                       *****************
C
     *(IKLE,LGVEC)
C
C***********************************************************************
C PROGICIEL: STBTEL V5.2          28/08/89  J-M HERVOUET (LNH) 3071 8018
C***********************************************************************
C
C FONCTION : CORRECTION DES DEPENDANCES ARRIERES
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |  IKLE          |<-->| NUMERO GLOBAUX DES NOEUDS DE CHAQUE ELEMENT  |
C |________________|____|______________________________________________
C | COMMON:        |    |
C |  GEO:          |    |
C |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
C |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
C |    NPOIN       | -->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C |    NELEM       | -->| NOMBRE TOTAL D'ELEMTS DU MAILLAGE
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
      INTEGER IELEM , NELEM , IEL1 , IEL2
      INTEGER I1 , I2 , I3 , J1 , J2 , J3
      INTEGER LGVEC , MESH , NDP , K
      INTEGER NPMAX , NPOIN , NELMAX
      INTEGER IKLE(NELMAX,4)
C
      LOGICAL DEP
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C
      DO 20 IELEM = 1,NELEM
         IEL2 = IELEM
25       CONTINUE
         DEP = .FALSE.
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
     *          I3.EQ.J1.OR.I3.EQ.J2.OR.I3.EQ.J3) DEP=.TRUE.
30       CONTINUE
         IF (DEP) THEN
            IEL2 = MOD(IEL2,NELEM) + 1
            IF (IEL2.EQ.IELEM) GOTO 40
            CALL ECHELE(IKLE,IELEM,IEL2)
            GOTO 25
         ENDIF
20    CONTINUE
C
C=======================================================================
C
40    CONTINUE
C
      RETURN
      END
