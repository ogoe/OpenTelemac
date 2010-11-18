C                       *****************
                        SUBROUTINE VERIFS
C                       *****************
C
     *(IFABOR,IKLE,TRAV1,NPTFR,NUMPB,NBPB)
C
C***********************************************************************
C  PROGICIEL : STBTEL V5.2        10/02/93    J.M. JANIN   (LNH)
C                                 25/02/99    P. LANG      (SOGREAH)
C***********************************************************************
C
C    FONCTION : REPERAGE DES POINTS APPARTENANT A PLUS DE TROIS
C               SEGMENTS FRONTIERES APRES ELIMINATION DES ELEMENTS SECS
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    NBOR        |<-- | TABLEAU DES POINTS DE BORD                   |
C |    IFABOR      | -->| TABLEAU DES VOISINS DES FACES.               |
C |    IKLE        | -->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT |
C |    TRAV1       |<-->| TABLEAU DE TRAVAIL                           |
C |    NPTFR       |<-- | NOMBRE DE POINTS DE BORD                     |
C |    X,Y         |--> | COORDONNEES DES POINTS DU MAILLAGE           |
C |    NUMPB       |<-- | NUMEROS DES POINTS POSANT PROBLEME           |
C |    NBPB        |<-- | NOMBRE DE POINTS POSANT PROBLEME             |
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
      INTEGER MESH , NDP , NELEM , I, J
      INTEGER NELMAX , NPOIN , NPMAX
      INTEGER IFABOR(NELMAX,*) , IKLE(NELMAX,4)
      INTEGER TRAV1(NPOIN,2)
      INTEGER ISUIV , IELEM , IFACE , NPTFR
      INTEGER SOMSUI(4) , I1 , I2 
      INTEGER NUMPB(100), NBPB
C
      LOGICAL EXIST
C 
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
      DATA SOMSUI / 2 , 3 , 4 , 0 /
C
C=======================================================================
C INITIALISATION
C=======================================================================
C
      IF (LNG.EQ.1) WRITE(LU,1010) 
      IF (LNG.EQ.2) WRITE(LU,1020)
      NBPB = 0
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
      DO 50 I=1,NPTFR
        I1 = 1
        I2 = 1
        DO 60 ISUIV=1,NPTFR
          IF (TRAV1(I,1).EQ.TRAV1(ISUIV,2)) I1 = I1 + 1
          IF (TRAV1(I,2).EQ.TRAV1(ISUIV,1)) I2 = I2 + 1
60      CONTINUE
        IF (I1.NE.2) THEN
          IF (NBPB.EQ.0) THEN
            NBPB = 1
            NUMPB(NBPB) = TRAV1(I,1)
           ELSE
            EXIST = .FALSE.
            DO 100  J=1,NBPB
              IF (NUMPB(J).EQ.TRAV1(I,1)) EXIST = .TRUE.
 100        CONTINUE
            IF (.NOT.EXIST) THEN
              NBPB = NBPB + 1
              IF (NBPB.GT.100) THEN
                IF (LNG.EQ.1) WRITE(LU,9000) 
                IF (LNG.EQ.2) WRITE(LU,9001)
                STOP
              ENDIF
              NUMPB(NBPB) = TRAV1(I,1)
            ENDIF
          ENDIF
        ENDIF
        IF (I2.NE.2) THEN
          IF (NBPB.EQ.0) THEN
            NBPB = 1
            NUMPB(NBPB) = TRAV1(I,2)
           ELSE
            EXIST = .FALSE.
            DO 101 J=1,NBPB
              IF (NUMPB(J).EQ.TRAV1(I,2)) EXIST = .TRUE.
 101        CONTINUE
            IF (.NOT.EXIST) THEN
              NBPB = NBPB + 1
              IF (NBPB.GT.100) THEN
                IF (LNG.EQ.1) WRITE(LU,9000) 
                IF (LNG.EQ.2) WRITE(LU,9001)
                STOP
              ENDIF
              NUMPB(NBPB) = TRAV1(I,2)
            ENDIF
          ENDIF
        ENDIF
50    CONTINUE
C
      RETURN
C
C -------------------------FORMATS------------------------------------------
 1010 FORMAT (//,1X,'RECHERCHE DES ILES CONNECTEES',/,
     +          1X,'-----------------------------')
 1020 FORMAT (//,1X,'SEARCHING ABOUT CONNECTED ISLANDS',/,
     +          1X,'---------------------------------')
 9000 FORMAT (1X,'******************************************',/,
     +        1X,'ERREUR - ROUTINE VERIFS',/,
     +        1X,'NB DE POINT DE CONNECTION SUPERIEUR A 100',/,
     +        1X,'******************************************')
 9001 FORMAT (1X,'*****************************************',/,
     +        1X,'ERROR - ROUTINE VERIFS',/,
     +        1X,'NB OF CONNECTION POINTS GREATHER THAN 100',/,
     +        1X,'*****************************************')
    
      END
