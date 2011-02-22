C                       *****************
                        SUBROUTINE DIVISE
C                       *****************
C
     *(X,Y,IKLE,NCOLOR,NPOIN,NELEM,NELMAX,NSOM2,SOM2,INDICP,INDICE)
C
C***********************************************************************
C PROGICIEL : STBTEL  V5.2                 J-M JANIN   (LNH) 30 87 72 84
C ORIGINE   : TELEMAC
C***********************************************************************
C
C     FONCTION  :  DIVISION PAR 4 DE TOUTES LES MAILLES
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
C |   IKLE         |<-->| NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
C |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS DU MAILLAGE
C |   NPOIN        |<-->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C |   NELEM        |<-->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
C |   NELMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C----------------------------------------------------------------------
C APPELE PAR : STBTEL
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN , NELEM , NELMAX , NSOM2 , IELEM , IPOIN , ISOM
      INTEGER IKLE(NELMAX,*) , NCOLOR(*) , INDICP(*) , INDICE(*)
      INTEGER NO1 , NO2 , NO3 , NP1 , NP2 , NP3 , NE1 , NE2 , NE3
C
      DOUBLE PRECISION X(*) , Y(*) , SOM2(10,2) , DX , DY
C
C=======================================================================
C      RECHERCHE DES ELEMENTS A DIVISER PAR 4 OU PAR 2
C=======================================================================
C
      DO 1 IPOIN = 1,NPOIN
         INDICP(IPOIN) = 1
1     CONTINUE
C
      IF (NSOM2.GE.3) THEN
C
         DO 5 ISOM = 1,NSOM2
C
            DX = SOM2(ISOM+1,1) - SOM2(ISOM,1)
            DY = SOM2(ISOM+1,2) - SOM2(ISOM,2)
C
            DO 10 IPOIN = 1,NPOIN
               IF (DX*(Y(IPOIN)-SOM2(ISOM,2)).LT.
     *             DY*(X(IPOIN)-SOM2(ISOM,1))) INDICP(IPOIN) = 0
10          CONTINUE
C
5        CONTINUE
C
      ENDIF
C
      DO 20 IELEM = 1,NELEM
         INDICE(IELEM) = INDICP(IKLE(IELEM,1))
     *               + 2*INDICP(IKLE(IELEM,2))
     *               + 4*INDICP(IKLE(IELEM,3))
20    CONTINUE
C
C=======================================================================
C      DIVISION DES ELEMENTS PAR 4 OU PAR 2
C=======================================================================
C
      IPOIN = 1
C
      DO 30 IELEM = 1,NELEM
C
         IF (INDICE(IELEM).EQ.7) THEN
C
            NO1 = IKLE(IELEM,1)
            NO2 = IKLE(IELEM,2)
            NO3 = IKLE(IELEM,3)
C
            NP1 = NPOIN + IPOIN
            NP2 = NP1   + 1
            NP3 = NP2   + 1
C
            NE1 = NELEM + IPOIN
            NE2 = NE1   + 1
            NE3 = NE2   + 1
C
            IPOIN = IPOIN + 3
C
            X(NP1) = 0.5D0 * ( X(NO1) + X(NO2) )
            X(NP2) = 0.5D0 * ( X(NO2) + X(NO3) )
            X(NP3) = 0.5D0 * ( X(NO3) + X(NO1) )
C
            Y(NP1) = 0.5D0 * ( Y(NO1) + Y(NO2) )
            Y(NP2) = 0.5D0 * ( Y(NO2) + Y(NO3) )
            Y(NP3) = 0.5D0 * ( Y(NO3) + Y(NO1) )
C
            NCOLOR(NP1) = NCOLOR(NO1)
            NCOLOR(NP2) = NCOLOR(NO2)
            NCOLOR(NP3) = NCOLOR(NO3)
C
            IKLE(IELEM,2) = NP1
            IKLE(IELEM,3) = NP3
C
            IKLE(  NE1,1) = NP1
            IKLE(  NE1,2) = NO2
            IKLE(  NE1,3) = NP2
C
            IKLE(  NE2,1) = NP3
            IKLE(  NE2,2) = NP2
            IKLE(  NE2,3) = NO3
C
            IKLE(  NE3,1) = NP2
            IKLE(  NE3,2) = NP3
            IKLE(  NE3,3) = NP1
C
         ELSEIF (INDICE(IELEM).EQ.3.OR.
     *           INDICE(IELEM).EQ.5.OR.
     *           INDICE(IELEM).EQ.6) THEN
C
            IF (INDICE(IELEM).EQ.3) THEN
               NO1 = IKLE(IELEM,1)
               NO2 = IKLE(IELEM,2)
               NO3 = IKLE(IELEM,3)
            ELSEIF (INDICE(IELEM).EQ.5) THEN
               NO1 = IKLE(IELEM,3)
               NO2 = IKLE(IELEM,1)
               NO3 = IKLE(IELEM,2)
            ELSE
               NO1 = IKLE(IELEM,2)
               NO2 = IKLE(IELEM,3)
               NO3 = IKLE(IELEM,1)
            ENDIF
C
            NP1 = NPOIN + IPOIN
C
            NE1 = NELEM + IPOIN
C
            IPOIN = IPOIN + 1
C
            X(NP1) = 0.5D0 * ( X(NO1) + X(NO2) )
C
            Y(NP1) = 0.5D0 * ( Y(NO1) + Y(NO2) )
C
            NCOLOR(NP1) = NCOLOR(NO1)
C
            IKLE(IELEM,1) = NO1
            IKLE(IELEM,2) = NP1
            IKLE(IELEM,3) = NO3
C
            IKLE(  NE1,1) = NO2
            IKLE(  NE1,2) = NO3
            IKLE(  NE1,3) = NP1
C
         ENDIF
C
30    CONTINUE
C
      NPOIN = NPOIN + IPOIN - 1
      NELEM = NELEM + IPOIN - 1
C
C=======================================================================
C  SORTIE LISTING
C=======================================================================
C
      IF (LNG.EQ.1) WRITE(LU,40) NPOIN,NELEM
      IF (LNG.EQ.2) WRITE(LU,50) NPOIN,NELEM
40    FORMAT(//,1X,'DIVISION PAR 4 DES ELEMENTS',
     *        /,1X,'---------------------------',/,
     *        /,1X,'NOUVEAU NOMBRE DE POINTS   :',I9,
     *        /,1X,'NOUVEAU NOMBRE D''ELEMENTS  :',I9)
50    FORMAT(//,1X,'CUTTING ELEMENTS BY 4',
     *        /,1X,'---------------------',/,
     *        /,1X,'NEW NUMBER OF POINTS   : ',I9,
     *        /,1X,'NEW NUMBER OF ELEMENTS : ',I9)
C
      RETURN
      END
