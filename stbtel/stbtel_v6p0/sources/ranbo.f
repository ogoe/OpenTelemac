C                       ****************
                        SUBROUTINE RANBO
C                       ****************
C
     *(NBOR,KP1BOR,IFABOR,IKLE,NCOLOR,TRAV1,NPTFR,X,Y,NCOLFR)
C
C***********************************************************************
C  PROGICIEL : STBTEL V5.2   10/02/93    J.M. JANIN   (LNH)
C***********************************************************************
C
C    FONCTION : CONSTRUCTION DE LA TABLE DES ARETES DE BORD
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    NBOR        |<-- | TABLEAU DES POINTS DE BORD                   |
C |    IFABOR      | -->| TABLEAU DES VOISINS DES FACES.               |
C |    IKLE        | -->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT |
C |    NCOLOR      | -->| TABLEAU DES COULEURS DES POINTS              |
C |    NCOLFR      |<-- | TABLEAU DES COULEURS DES POINTS DE BORD      |
C |    TRAV1       |<-->| TABLEAU DE TRAVAIL                           |
C |    NPTFR       |<-- | NOMBRE DE POINTS DE BORD
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
C APPELE PAR : STBTEL
C APPEL DE : -
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER MESH , NDP , NELEM , IILE , NILE , I
      INTEGER NELMAX , NPOIN , NPMAX
      INTEGER IFABOR(NELMAX,*) , IKLE(NELMAX,4) , NCOLOR(*) , NCOLFR(*)
      INTEGER TRAV1(NPOIN,2)
      INTEGER ISUIV , IELEM , IFACE , NPTFR , NOEUD1 , NOEUD2
      INTEGER SOMSUI(4) , IERROR , I1 , I2 , NBOR(*) , KP1BOR(*)
C
      DOUBLE PRECISION X(NPOIN) , Y(NPOIN) , SOM1 , SOM2 , Y2 , EPSILO
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
      DATA SOMSUI / 2 , 3 , 4 , 0 /
      DATA EPSILO / 1.D-6 /
C
C=======================================================================
C INITIALISATION
C=======================================================================
C
      SOMSUI(NDP) = 1
      IF (MESH.NE.2.AND.MESH.NE.3) THEN
         IF (LNG.EQ.1) WRITE(LU,1000) MESH
         IF (LNG.EQ.2) WRITE(LU,4000) MESH
1000     FORMAT(/,1X,'RANBO : CAS DE MAILLAGE NON PREVU, MESH = ',I4,/)
4000     FORMAT(/,1X,'RANBO : MESH NOT ALLOWED , MESH = ',I4,/)
         STOP
      ENDIF
C
C=======================================================================
C RECHERCHE DES ARETES DE BORD,NUMEROTEES DE 1 A NPTFR
C=======================================================================
C
      NPTFR = 0
      DO 10 IELEM=1,NELEM
         DO 20 IFACE=1,NDP
            IF (IFABOR(IELEM,IFACE).LE.0) THEN
               NPTFR = NPTFR + 1
               TRAV1(NPTFR,1) = IKLE(IELEM,       IFACE )
               TRAV1(NPTFR,2) = IKLE(IELEM,SOMSUI(IFACE))
            ENDIF
20       CONTINUE
10    CONTINUE
C
C=======================================================================
C ON VERIFIE QUE CHAQUE POINT N'APPARAIT QUE DEUX FOIS
C ( UNE FOIS COMME NOEUD 1 , UNE FOIS COMME NOEUD 2 )
C=======================================================================
C
      IERROR = 0
      DO 50 I=1,NPTFR
         I1 = 1
         I2 = 1
         DO 60 ISUIV=1,NPTFR
            IF (TRAV1(I,1).EQ.TRAV1(ISUIV,2)) I1 = I1 + 1
            IF (TRAV1(I,2).EQ.TRAV1(ISUIV,1)) I2 = I2 + 1
60       CONTINUE
         IF (I1.NE.2) THEN
            IERROR = IERROR + 1
            IF (LNG.EQ.1) WRITE(LU,1010) X(TRAV1(I,1)),Y(TRAV1(I,1)),I1
            IF (LNG.EQ.2) WRITE(LU,1020) X(TRAV1(I,1)),Y(TRAV1(I,1)),I1
         ENDIF
         IF (I2.NE.2) THEN
            IERROR = IERROR + 1
            IF (LNG.EQ.1) WRITE(LU,1010) X(TRAV1(I,2)),Y(TRAV1(I,2)),I2
            IF (LNG.EQ.2) WRITE(LU,1020) X(TRAV1(I,2)),Y(TRAV1(I,2)),I2
         ENDIF
50    CONTINUE
C
1010  FORMAT(1X,'ERREUR SUR LE POINT DE BORD :',/,
     *       1X,'X=',F13.3,'  Y=',F13.3,/,
     *       1X,'IL EST CONTENU DANS',I2,' ARETE(S) DE BORD')
1020  FORMAT(1X,'ERROR ON BOUNDARY NODE :',/,
     *       1X,'X=',F13.3,'  Y=',F13.3,/,
     *       1X,'IT BELONGS TO',I2,' BOUNDARY SEGMENT(S)')
C
      IF (IERROR.GT.0) STOP
C
C=======================================================================
C RANGEMENT DES ARETES DE BORD BOUT A BOUT.
C ON COMMENCE ARBITRAIREMENT PAR LE POINT LE PLUS SUD-OUEST
C ( PUIS LE PLUS SUD SI CONFLIT ) AFIN D'ETRE SUR DE COMMENCER
C SUR LE CONTOUR ET NON SUR UNE ILE |||
C=======================================================================
C
      SOM2 = X(1) + Y(1)
      Y2   = Y(1)
C
      DO 80 I=1,NPTFR
C
         SOM1 = X(TRAV1(I,1)) + Y(TRAV1(I,1))
         IF (ABS(SOM1-SOM2).LE.ABS(EPSILO*SOM1)) THEN
            IF (Y(TRAV1(I,1)).LE.Y2) THEN
               Y2    = Y(TRAV1(I,1))
               SOM2  = SOM1
               ISUIV = I
            ENDIF
         ELSEIF (SOM1.LE.SOM2) THEN
            Y2    = Y(TRAV1(I,1))
            SOM2  = SOM1
            ISUIV = I
         ENDIF
C
80    CONTINUE
C
      NOEUD1 = TRAV1(ISUIV,1)
      NOEUD2 = TRAV1(ISUIV,2)
      TRAV1(ISUIV,1) = TRAV1(1,1)
      TRAV1(ISUIV,2) = TRAV1(1,2)
      TRAV1(1,1) = NOEUD1
      TRAV1(1,2) = NOEUD2
C
      IILE = 0
      NILE = 1
C
      DO 70 I=2,NPTFR
C
C=======================================================================
C RECHERCHE DE L'ARETE DONT LE PREMIER NOEUD EST IDENTIQUE AU SECOND
C DE L'ARETE PRECEDENTE
C=======================================================================
C
         DO 90 ISUIV=I,NPTFR
C
            IF (TRAV1(ISUIV,1).EQ.TRAV1(I-1,2)) THEN
C
C=======================================================================
C PERMUTATION DES ARETES DE NUMEROS I+1 ET ISUIV
C=======================================================================
C
               NOEUD1 = TRAV1(ISUIV,1)
               NOEUD2 = TRAV1(ISUIV,2)
               TRAV1(ISUIV,1) = TRAV1(I,1)
               TRAV1(ISUIV,2) = TRAV1(I,2)
               TRAV1(I,1) = NOEUD1
               TRAV1(I,2) = NOEUD2
               KP1BOR(I+NPTFR) = I-1
               KP1BOR(I-1) = I
               GOTO 70
C
            ENDIF
C
90       CONTINUE
C
C=======================================================================
C SI ON NE TROUVE PAS DE POINT SUIVANT : ON VERIFIE QUE LE DERNIER POINT
C TROUVE EST IDENTIQUE AU PREMIER , DANS CE CAS ON EST EN PRESENCE D'UNE
C ILE ET ON ITERE LE PROCESSUS GLOBAL
C=======================================================================
C
         IF (TRAV1(NILE,1).NE.TRAV1(I-1,2)) THEN
C
C=======================================================================
C SINON IL Y A ERREUR
C=======================================================================
C
            IF (LNG.EQ.1) WRITE(LU,1500) TRAV1(I-1,2)
            IF (LNG.EQ.2) WRITE(LU,4500) TRAV1(I-1,2)
1500        FORMAT(1X,'ERREUR LORS DU RANGEMENT DES ARETES DE BORD',/,
     *             1X,'POUR LE NOEUD ',I5)
4500        FORMAT(1X,'ERROR IN STORING THE EDGE SEGMENTS',/,
     *             1X,'FOR THE NODE ',I5)
            STOP
         ENDIF
C
         KP1BOR(NILE+NPTFR) = I-1
         KP1BOR(I-1) = NILE
         IILE = IILE+1
         NILE = I
C
70    CONTINUE
C
C=======================================================================
C ON VERIFIE QUE LA DERNIERE ILE EST FERMEE
C=======================================================================
C
      IF (TRAV1(NILE,1).NE.TRAV1(NPTFR,2)) THEN
         IF (LNG.EQ.1) WRITE(LU,2000) TRAV1(NILE,1),TRAV1(NPTFR,2)
         IF (LNG.EQ.2) WRITE(LU,5000) TRAV1(NILE,1),TRAV1(NPTFR,2)
2000     FORMAT(1X,'ERREUR, LE CONTOUR N''EST PAS FERME :',/,
     *          1X,'PREMIER POINT :',I5,2X,'DERNIER POINT : ',I5)
5000     FORMAT(1X,'ERROR, THE BOUNDARY IS NOT CLOSED :',/,
     *          1X,'FIRST POINT :',I5,2X,'LAST POINT : ',I5)
         STOP
      ENDIF
C
      KP1BOR(NILE+NPTFR) = NPTFR
      KP1BOR(NPTFR) = NILE
C
      IF (LNG.EQ.1) WRITE(LU,2500) NPTFR
      IF (LNG.EQ.2) WRITE(LU,5500) NPTFR
      IF (LNG.EQ.1) WRITE(LU,2600) IILE
      IF (LNG.EQ.2) WRITE(LU,5600) IILE
 2500 FORMAT(1X,'NOMBRE DE POINTS FRONTIERE     : ',I5)
 5500 FORMAT(1X,'NUMBER OF BOUNDARY POINTS      : ',I5)
 2600 FORMAT(1X,'NOMBRE D''ILE(S)                : ',I5)
 5600 FORMAT(1X,'NUMBER OF ISLANDS              : ',I5)
C
C=======================================================================
C REMPLISSAGE DU TABLEAU NBOR ET STOCKAGE DE LA COULEUR DES POINTS DE
C BORD DANS LE TABLEAU NCOLFR
C=======================================================================
C
      DO 110 I=1,NPTFR
         NBOR(I      ) = TRAV1(I,1)
         NBOR(I+NPTFR) = TRAV1(I,2)
         NCOLFR(I) = NCOLOR(TRAV1(I,1))
110   CONTINUE
C
      RETURN
      END
