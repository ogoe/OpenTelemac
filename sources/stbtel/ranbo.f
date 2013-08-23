!                       ****************
                        SUBROUTINE RANBO
!                       ****************
!
     &(NBOR,KP1BOR,IFABOR,IKLE,NCOLOR,TRAV1,NPTFR,X,Y,NCOLFR)
!
!***********************************************************************
!  PROGICIEL : STBTEL V5.2   10/02/93    J.M. JANIN   (LNH)
!***********************************************************************
!
!    FONCTION : CONSTRUCTION DE LA TABLE DES ARETES DE BORD
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    NBOR        |<-- | TABLEAU DES POINTS DE BORD                   |
! |    IFABOR      | -->| TABLEAU DES VOISINS DES FACES.               |
! |    IKLE        | -->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT |
! |    NCOLOR      | -->| TABLEAU DES COULEURS DES POINTS              |
! |    NCOLFR      |<-- | TABLEAU DES COULEURS DES POINTS DE BORD      |
! |    TRAV1       |<-->| TABLEAU DE TRAVAIL                           |
! |    NPTFR       |<-- | NOMBRE DE POINTS DE BORD
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       | -->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       | -->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |________________|____|______________________________________________|
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : STBTEL
! APPEL DE : -
!***********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER MESH , NDP , NELEM , IILE , NILE , I
      INTEGER NELMAX , NPOIN , NPMAX
      INTEGER IFABOR(NELMAX,*) , IKLE(NELMAX,4) , NCOLOR(*) , NCOLFR(*)
      INTEGER TRAV1(NPOIN,2)
      INTEGER ISUIV , IELEM , IFACE , NPTFR , NOEUD1 , NOEUD2
      INTEGER SOMSUI(4) , IERROR , I1 , I2 , NBOR(*) , KP1BOR(*)
!
      DOUBLE PRECISION X(NPOIN) , Y(NPOIN) , SOM1 , SOM2 , Y2 , EPSILO
!
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
!
      DATA SOMSUI / 2 , 3 , 4 , 0 /
      DATA EPSILO / 1.D-6 /
!
!=======================================================================
! INITIALISATION
!=======================================================================
!
      SOMSUI(NDP) = 1
      IF (MESH.NE.2.AND.MESH.NE.3) THEN
         IF (LNG.EQ.1) WRITE(LU,1000) MESH
         IF (LNG.EQ.2) WRITE(LU,4000) MESH
1000     FORMAT(/,1X,'RANBO : CAS DE MAILLAGE NON PREVU, MESH = ',I4,/)
4000     FORMAT(/,1X,'RANBO : MESH NOT ALLOWED , MESH = ',I4,/)
         STOP
      ENDIF
!
!=======================================================================
! RECHERCHE DES ARETES DE BORD,NUMEROTEES DE 1 A NPTFR
!=======================================================================
!
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
!
!=======================================================================
! ON VERIFIE QUE CHAQUE POINT N'APPARAIT QUE DEUX FOIS
! ( UNE FOIS COMME NOEUD 1 , UNE FOIS COMME NOEUD 2 )
!=======================================================================
!
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
!
1010  FORMAT(1X,'ERREUR SUR LE POINT DE BORD :',/,
     &       1X,'X=',F13.3,'  Y=',F13.3,/,
     &       1X,'IL EST CONTENU DANS',I2,' ARETE(S) DE BORD')
1020  FORMAT(1X,'ERROR ON BOUNDARY NODE :',/,
     &       1X,'X=',F13.3,'  Y=',F13.3,/,
     &       1X,'IT BELONGS TO',I2,' BOUNDARY SEGMENT(S)')
!
      IF (IERROR.GT.0) STOP
!
!=======================================================================
! RANGEMENT DES ARETES DE BORD BOUT A BOUT.
! ON COMMENCE ARBITRAIREMENT PAR LE POINT LE PLUS SUD-OUEST
! ( PUIS LE PLUS SUD SI CONFLIT ) AFIN D'ETRE SUR DE COMMENCER
! SUR LE CONTOUR ET NON SUR UNE ILE |||
!=======================================================================
!
      SOM2 = X(1) + Y(1)
      Y2   = Y(1)
!
      DO 80 I=1,NPTFR
!
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
!
80    CONTINUE
!
      NOEUD1 = TRAV1(ISUIV,1)
      NOEUD2 = TRAV1(ISUIV,2)
      TRAV1(ISUIV,1) = TRAV1(1,1)
      TRAV1(ISUIV,2) = TRAV1(1,2)
      TRAV1(1,1) = NOEUD1
      TRAV1(1,2) = NOEUD2
!
      IILE = 0
      NILE = 1
!
      DO 70 I=2,NPTFR
!
!=======================================================================
! RECHERCHE DE L'ARETE DONT LE PREMIER NOEUD EST IDENTIQUE AU SECOND
! DE L'ARETE PRECEDENTE
!=======================================================================
!
         DO 90 ISUIV=I,NPTFR
!
            IF (TRAV1(ISUIV,1).EQ.TRAV1(I-1,2)) THEN
!
!=======================================================================
! PERMUTATION DES ARETES DE NUMEROS I+1 ET ISUIV
!=======================================================================
!
               NOEUD1 = TRAV1(ISUIV,1)
               NOEUD2 = TRAV1(ISUIV,2)
               TRAV1(ISUIV,1) = TRAV1(I,1)
               TRAV1(ISUIV,2) = TRAV1(I,2)
               TRAV1(I,1) = NOEUD1
               TRAV1(I,2) = NOEUD2
               KP1BOR(I+NPTFR) = I-1
               KP1BOR(I-1) = I
               GOTO 70
!
            ENDIF
!
90       CONTINUE
!
!=======================================================================
! SI ON NE TROUVE PAS DE POINT SUIVANT : ON VERIFIE QUE LE DERNIER POINT
! TROUVE EST IDENTIQUE AU PREMIER , DANS CE CAS ON EST EN PRESENCE D'UNE
! ILE ET ON ITERE LE PROCESSUS GLOBAL
!=======================================================================
!
         IF (TRAV1(NILE,1).NE.TRAV1(I-1,2)) THEN
!
!=======================================================================
! SINON IL Y A ERREUR
!=======================================================================
!
            IF (LNG.EQ.1) WRITE(LU,1500) TRAV1(I-1,2)
            IF (LNG.EQ.2) WRITE(LU,4500) TRAV1(I-1,2)
1500        FORMAT(1X,'ERREUR LORS DU RANGEMENT DES ARETES DE BORD',/,
     &             1X,'POUR LE NOEUD ',I5)
4500        FORMAT(1X,'ERROR IN STORING THE EDGE SEGMENTS',/,
     &             1X,'FOR THE NODE ',I5)
            STOP
         ENDIF
!
         KP1BOR(NILE+NPTFR) = I-1
         KP1BOR(I-1) = NILE
         IILE = IILE+1
         NILE = I
!
70    CONTINUE
!
!=======================================================================
! ON VERIFIE QUE LA DERNIERE ILE EST FERMEE
!=======================================================================
!
      IF (TRAV1(NILE,1).NE.TRAV1(NPTFR,2)) THEN
         IF (LNG.EQ.1) WRITE(LU,2000) TRAV1(NILE,1),TRAV1(NPTFR,2)
         IF (LNG.EQ.2) WRITE(LU,5000) TRAV1(NILE,1),TRAV1(NPTFR,2)
2000     FORMAT(1X,'ERREUR, LE CONTOUR N''EST PAS FERME :',/,
     &          1X,'PREMIER POINT :',I5,2X,'DERNIER POINT : ',I5)
5000     FORMAT(1X,'ERROR, THE BOUNDARY IS NOT CLOSED :',/,
     &          1X,'FIRST POINT :',I5,2X,'LAST POINT : ',I5)
         STOP
      ENDIF
!
      KP1BOR(NILE+NPTFR) = NPTFR
      KP1BOR(NPTFR) = NILE
!
      IF (LNG.EQ.1) WRITE(LU,2500) NPTFR
      IF (LNG.EQ.2) WRITE(LU,5500) NPTFR
      IF (LNG.EQ.1) WRITE(LU,2600) IILE
      IF (LNG.EQ.2) WRITE(LU,5600) IILE
 2500 FORMAT(1X,'NOMBRE DE POINTS FRONTIERE     : ',I5)
 5500 FORMAT(1X,'NUMBER OF BOUNDARY POINTS      : ',I5)
 2600 FORMAT(1X,'NOMBRE D''ILE(S)                : ',I5)
 5600 FORMAT(1X,'NUMBER OF ISLANDS              : ',I5)
!
!=======================================================================
! REMPLISSAGE DU TABLEAU NBOR ET STOCKAGE DE LA COULEUR DES POINTS DE
! BORD DANS LE TABLEAU NCOLFR
!=======================================================================
!
      DO 110 I=1,NPTFR
         NBOR(I      ) = TRAV1(I,1)
         NBOR(I+NPTFR) = TRAV1(I,2)
         NCOLFR(I) = NCOLOR(TRAV1(I,1))
110   CONTINUE
!
      RETURN
      END