C                       *****************
                        SUBROUTINE VERIFI
C                       *****************
C
     *( X , Y ,IKLE , NCOLOR , TRAV1 , EPSI )
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2         09/08/89    J-C GALLAND  (LNH)
C***********************************************************************
C
C     FONCTION  :  ELIMINATION DES TROUS DANS LA NUMEROTATION DES NOEUDS
C                  ET RE-ORIENTATION DES ELEMENTS DU MAILLAGE
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
C |   IKLE         |<-->| NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
C |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS DU MAILLAGE
C |   TRAV1,2      |<-->| TABLEAUX DE TRAVAIL
C |   EPSI         | -->| DISTANCE MINIMALE ENTRE 2 NOEUDS DU MAILLAGE
C |________________|____|______________________________________________
C | COMMON:        |    |
C |  GEO:          |    |
C |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
C |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
C |    NPOIN       |<-->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C |    NELEM       |<-->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
C |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
C |                |    | (NPMAX = NPOIN + 0.1*NELEM)
C |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
C |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C----------------------------------------------------------------------
C APPELE PAR : STBTEL
C APPEL DE : REMAIL, CIRCUL
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER MESH , NDP , NELEM , NPOIN , NELMAX , NPMAX
      INTEGER IKLE(NELMAX,4) , NCOLOR(*)
      INTEGER ITEST , ITEST1 , IELEM
      INTEGER TRAV1(*)
C
      DOUBLE PRECISION X(*) , Y(*) , EPSI
C
C COMMON
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C ON VERIFIE QUE TOUS LES POINTS SONT DISTINCTS
C=======================================================================
C
      CALL REMAIL (IKLE,NCOLOR,TRAV1,X,Y,EPSI)
C
C=======================================================================
C ON VERIFIE QUE TOUS LES ELEMENTS SONT CORRECTEMENT ORIENTES
C=======================================================================
C
      ITEST = 0
C
C CAS DES QUADRANGLES
C
      IF (MESH.EQ.2) THEN
C
         DO 70 IELEM=1,NELEM
C
            ITEST1 = 0
            CALL CIRCUL (IKLE,ITEST1,IELEM,1,2,3,X,Y)
            CALL CIRCUL (IKLE,ITEST1,IELEM,2,3,4,X,Y)
            CALL CIRCUL (IKLE,ITEST1,IELEM,3,4,1,X,Y)
            CALL CIRCUL (IKLE,ITEST1,IELEM,4,1,2,X,Y)
            IF (ITEST1.GT.0) ITEST = ITEST + 1
C
 70      CONTINUE
C
C CAS DES TRIANGLES
C
      ELSE IF (MESH.EQ.3) THEN
C
         DO 80 IELEM=1,NELEM
C
            ITEST1 = 0
            CALL CIRCUL (IKLE,ITEST1,IELEM,1,2,3,X,Y)
            IF (ITEST1.GT.0) ITEST = ITEST + 1
C
 80      CONTINUE
C
      ELSE
         IF (LNG.EQ.1) WRITE(LU,90) MESH
         IF (LNG.EQ.2) WRITE(LU,3090) MESH
 90      FORMAT(/,' LECSTB TYPE DE MAILLAGE NON PREVU , MESH = ',I4)
 3090    FORMAT(/,' LECSTB TYPE OF MESH NOT AVAILABLE , MESH = ',I4)
      ENDIF
C
      IF (LNG.EQ.1) WRITE(LU,100) ITEST
      IF (LNG.EQ.2) WRITE(LU,3100) ITEST
 100  FORMAT(1X,'NOMBRE D''ELEMENTS MAL ORIENTES : ',I5)
 3100 FORMAT(1X,'NUMBER OF ELEMENTS BADLY ORIENTED : ',I5)
C
      RETURN
      END
