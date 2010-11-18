C                       *****************
                        SUBROUTINE DECOUP
C                       *****************
C
     *(ISURC,X,Y,IKLE,NCOLOR,IFABOR, NELEM2,NPOIN2,COLOR)
C
C***********************************************************************
C PROGICIEL: STBTEL V5.2        19/04/91  J-C GALLAND  (LNH)
C                               19/02/93  J-M JANIN    (LNH)
C***********************************************************************
C
C FONCTION : DECOUPAGE DES TRIANGLES SURCONTRAINTS :
C            ILS SONT COUPES EN TROIS PAR AJOUT D'UN POINT A
C            LEUR BARYCENTRE
C
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   ISURC        | -->| NUMERO DE L'ELEMENT SURCONTRAINT A TRAITER
C |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
C |   IKLE         |<-->| LISTE DES POINTS DE CHAQUE ELEMENT
C |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS
C |   IFABOR       | -->| TABLEAU DES VOISINS DES ELEMENTS
C |   NELEM2       |<-->| NOUVEAU NOMBRE D'ELEMENTS APRES DECOUP
C |   NPOIN2       |<-->| NOUVEAU NOMBRE DE POINTS APRES DECOUP
C |    COLOR       |<-->| STOCKAGE COULEURS DES NOEUDS SUR FICHIER GEO
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
C |                |    |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C APPELE PAR : SURCON
C APPEL DE : -
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION X(*) , Y(*)
C
      INTEGER NELEM , NELEM2 , MESH , NDP , NPOIN , NPOIN2 , NPMAX
      INTEGER NELMAX , KELEM
      INTEGER ISURC , IFAC , ICOLOR , I , I1 , I2 , I3
      INTEGER IKLE(NELMAX,4) , NCOLOR(*)
      INTEGER IFABOR(NELMAX,*)
C
      LOGICAL COLOR
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C CALCUL DES COORDONNEES DU NOUVEAU NOEUD 4 (DE NUMERO NPOIN2)
C=======================================================================
C
      NPOIN2 = NPOIN2 + 1
      I1 = IKLE(ISURC,1)
      I2 = IKLE(ISURC,2)
      I3 = IKLE(ISURC,3)
C
      X(NPOIN2) = (X(I1) + X(I2) + X(I3))/3.D0
      Y(NPOIN2) = (Y(I1) + Y(I2) + Y(I3))/3.D0
C
C=======================================================================
C DEFINITION DE LA COULEUR DU NOEUD CREE (C'EST CELLE DU NOEUD NON POINT
C DE BORD DE L'ELEMENT VOISIN)
C=======================================================================
C
      IF (COLOR) THEN
         DO 10 IFAC=1,3
            IF(IFABOR(ISURC,IFAC).GT.0) ICOLOR = IFABOR(ISURC,IFAC)
 10      CONTINUE
C
         DO 20 I=1,3
            IF(IKLE(ICOLOR,I).NE.I1.AND.IKLE(ICOLOR,I).NE.I2.AND.
     *         IKLE(ICOLOR,I).NE.I3)
     *         NCOLOR(NPOIN2) = NCOLOR(IKLE(ICOLOR,I))
 20      CONTINUE
       ENDIF
C
C=======================================================================
C CALCUL DES NOUVEAUX IKLE : L'ELEMENT (1,2,4) CONSERVE LE NUMERO ISURC
C                            L'ELEMENT (2,3,4) PREND LE NUMERO NELEM2+1
C                            L'ELEMENT (3,1,4) PREND LE NUMERO NELEM2+2
C=======================================================================
C
      IKLE(ISURC,3) = NPOIN2
C
      NELEM2 = NELEM2 + 1
      IKLE(NELEM2,1) = I2
      IKLE(NELEM2,2) = I3
      IKLE(NELEM2,3) = NPOIN2
C
      KELEM = IFABOR(ISURC,2)
      IFABOR(NELEM2,1) = KELEM
      IFABOR(NELEM2,2) = NELEM2+1
      IFABOR(NELEM2,3) = ISURC
      IF (KELEM.GT.0) THEN
         IF (IFABOR(KELEM,1).EQ.ISURC) IFABOR(KELEM,1) = NELEM2
         IF (IFABOR(KELEM,2).EQ.ISURC) IFABOR(KELEM,2) = NELEM2
         IF (IFABOR(KELEM,3).EQ.ISURC) IFABOR(KELEM,3) = NELEM2
      ENDIF
      IFABOR(ISURC,2) = NELEM2
C
      NELEM2 = NELEM2 + 1
      IKLE(NELEM2,1) = I3
      IKLE(NELEM2,2) = I1
      IKLE(NELEM2,3) = NPOIN2
C
      KELEM = IFABOR(ISURC,3)
      IFABOR(NELEM2,1) = IFABOR(ISURC,3)
      IFABOR(NELEM2,2) = ISURC
      IFABOR(NELEM2,3) = NELEM2-1
      IF (KELEM.GT.0) THEN
         IF (IFABOR(KELEM,1).EQ.ISURC) IFABOR(KELEM,1) = NELEM2
         IF (IFABOR(KELEM,2).EQ.ISURC) IFABOR(KELEM,2) = NELEM2
         IF (IFABOR(KELEM,3).EQ.ISURC) IFABOR(KELEM,3) = NELEM2
      ENDIF
      IFABOR(ISURC,3) = NELEM2
C
C=======================================================================
C
      RETURN
      END
