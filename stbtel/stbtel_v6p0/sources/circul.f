C                       *****************
                        SUBROUTINE CIRCUL
C                       *****************
C
     *(IKLE,ITEST1 ,IELEM,I1,I2,I3,X,Y)
C
C***********************************************************************
C  STBTEL VERSION 5.2   16/08/89    J.C. GALLAND   (LNH)
C***********************************************************************
C
C    FONCTION : CALCUL DE L'AIRE FORMEE PAR LES TROIS POINTS I1,I2,I3
C               ET PERMUTATION DES POINTS I2 ET I3 LORSQU'ELLE EST
C               NEGATIVE.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    IKLE        |<-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT |
C |    ITEST1      | -->| COMPTEUR                                     |
C |    IELEM       | -->| NUMERO DE L'ELEMENT COURANT                  |
C |    I1,I2,I3    | -->| PERMUTATION DES IKLE                         |
C |    X,Y         | -->| COORDONNEES DES POINTS DU MAILLAGE           |
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
C  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C APPELE PAR : LECSTB
C APPEL DE :
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER IELEM , NELEM , NPOIN , MESH , NDP , NELMAX , NPMAX
      INTEGER IKLE(NELMAX,4) , I1 , I2 , I3 , ITEST1 , I
C
      DOUBLE PRECISION X2 , X3 , Y2 , Y3 , X(*) , Y(*)
      DOUBLE PRECISION AIRE
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
         X2 = X(IKLE(IELEM,I2))-X(IKLE(IELEM,I1))
         X3 = X(IKLE(IELEM,I3))-X(IKLE(IELEM,I1))
C
         Y2 = Y(IKLE(IELEM,I2))-Y(IKLE(IELEM,I1))
         Y3 = Y(IKLE(IELEM,I3))-Y(IKLE(IELEM,I1))
C
         AIRE = X2*Y3 - X3*Y2
C
         IF (AIRE.LT.0.D0) THEN
            ITEST1 = ITEST1 + 1
            I = IKLE(IELEM,I2)
C
            IKLE(IELEM,I2) = IKLE(IELEM,I3)
            IKLE(IELEM,I3) = I
         ENDIF
C
         RETURN
         END
