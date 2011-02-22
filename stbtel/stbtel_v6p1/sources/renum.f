C                       ****************
                        SUBROUTINE RENUM
C                       ****************
C
     *(X,Y,W,IKLE,NBOR,TRAV1,TRAV2,TAB,NCOLOR,COLOR,NPTFR)
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2                   19/04/91  J-C GALLAND  (LNH)
C                                           19/02/93  J-M JANIN    (LNH)
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
C |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
C |   IKLE         |<-->| LISTE DES POINTS DE CHAQUE ELEMENT
C |   TRAV1,2      |<-->| TABLEAUX DE TRAVAIL
C |   TAB          |<-->| TABLEAU DE TRAVAIL
C |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS
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
      DOUBLE PRECISION X(*) , Y(*) , W(*)
C
      INTEGER MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX , NPTFR
      INTEGER TAB(*) , IPOIN , IELEM , IPTFR , I1 , I2 , TABMAX
      INTEGER TRAV1(*) , TRAV2(*) , IKLE(NELMAX,3) , NCOLOR(*) , NBOR(*)
C
      LOGICAL COLOR
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C CALCUL DU NOMBRE DE POINTS ET ELEMENTS VOISINS
C=======================================================================
C
      DO 100 IPOIN = 1,NPOIN
         TRAV1(IPOIN) = 0
100   CONTINUE
C
      DO 110 IELEM = 1,NELEM
         TRAV1(IKLE(IELEM,1)) = TRAV1(IKLE(IELEM,1)) + 2
         TRAV1(IKLE(IELEM,2)) = TRAV1(IKLE(IELEM,2)) + 2
         TRAV1(IKLE(IELEM,3)) = TRAV1(IKLE(IELEM,3)) + 2
110   CONTINUE
C
      DO 112 IPTFR = 1,NPTFR
         TRAV1(NBOR(IPTFR)) = TRAV1(NBOR(IPTFR)) + 1
112   CONTINUE
C
C=======================================================================
C RENUMEROTATIONS DES POINTS SUIVANT ORDRE CROISSANT DE VOISINS
C=======================================================================
C
      TABMAX = 0
C
      DO 120 IPOIN = 1,NPOIN
C
         I1 = TRAV1(IPOIN)
C
         IF (I1.GT.TABMAX) THEN
            DO 130 I2 = TABMAX+1,I1
               TAB(I2) = IPOIN - 1
130         CONTINUE
            TABMAX = I1
         ELSEIF (I1.LT.TABMAX) THEN
            DO 140 I2 = TABMAX,I1+1,-1
               TAB(I2) = TAB(I2) + 1
               TRAV2(TAB(I2)) = TRAV2(TAB(I2-1)+1)
140         CONTINUE
         ENDIF
C
         TAB(I1) = TAB(I1) + 1
         TRAV2(TAB(I1)) = IPOIN
C
120   CONTINUE
C
      DO 145 I1 = 1,TABMAX
         PRINT*,'TAB(',I1,')=',TAB(I1)
145   CONTINUE
C
C=======================================================================
C MODIFICATIONS CORRESPONDANTES DANS LES DIFFERENTES VARIABLES
C=======================================================================
C
      DO 150 IPOIN = 1,NPOIN
         TRAV1(TRAV2(IPOIN)) = IPOIN
150   CONTINUE
C
      DO 160 IELEM = 1,NELEM
         IKLE(IELEM,1) = TRAV1(IKLE(IELEM,1))
         IKLE(IELEM,2) = TRAV1(IKLE(IELEM,2))
         IKLE(IELEM,3) = TRAV1(IKLE(IELEM,3))
160   CONTINUE
C
      DO 165 IPTFR = 1,NPTFR
         NBOR(IPTFR) = TRAV1(NBOR(IPTFR))
         NBOR(NPTFR+IPTFR) = TRAV1(NBOR(NPTFR+IPTFR))
165   CONTINUE
C
      DO 200 IPOIN = 1,NPOIN
         W(IPOIN) = X(TRAV2(IPOIN))
200   CONTINUE
      DO 205 IPOIN = 1,NPOIN
         X(IPOIN) = W(IPOIN)
205   CONTINUE
C
      DO 210 IPOIN = 1,NPOIN
         W(IPOIN) = Y(TRAV2(IPOIN))
210   CONTINUE
      DO 215 IPOIN = 1,NPOIN
         Y(IPOIN) = W(IPOIN)
215   CONTINUE
C
      IF (COLOR) THEN
C
         DO 230 IPOIN = 1,NPOIN
            TRAV1(IPOIN) = NCOLOR(TRAV2(IPOIN))
230      CONTINUE
         DO 235 IPOIN = 1,NPOIN
            NCOLOR(IPOIN) = TRAV1(IPOIN)
235      CONTINUE
C
      ENDIF
C
C=======================================================================
C
      RETURN
      END
