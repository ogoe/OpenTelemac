!                       *****************
                        SUBROUTINE VERIFS
!                       *****************
!
     &(IFABOR,IKLE,TRAV1,NPTFR,NUMPB,NBPB)
!
!***********************************************************************
!  PROGICIEL : STBTEL V5.2        10/02/93    J.M. JANIN   (LNH)
!                                 25/02/99    P. LANG      (SOGREAH)
!***********************************************************************
!
!    FONCTION : REPERAGE DES POINTS APPARTENANT A PLUS DE TROIS
!               SEGMENTS FRONTIERES APRES ELIMINATION DES ELEMENTS SECS
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    NBOR        |<-- | TABLEAU DES POINTS DE BORD                   |
! |    IFABOR      | -->| TABLEAU DES VOISINS DES FACES.               |
! |    IKLE        | -->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT |
! |    TRAV1       |<-->| TABLEAU DE TRAVAIL                           |
! |    NPTFR       |<-- | NOMBRE DE POINTS DE BORD                     |
! |    X,Y         |--> | COORDONNEES DES POINTS DU MAILLAGE           |
! |    NUMPB       |<-- | NUMEROS DES POINTS POSANT PROBLEME           |
! |    NBPB        |<-- | NOMBRE DE POINTS POSANT PROBLEME             |
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
      INTEGER MESH , NDP , NELEM , I, J
      INTEGER NELMAX , NPOIN , NPMAX
      INTEGER IFABOR(NELMAX,*) , IKLE(NELMAX,4)
      INTEGER TRAV1(NPOIN,2)
      INTEGER ISUIV , IELEM , IFACE , NPTFR
      INTEGER SOMSUI(4) , I1 , I2
      INTEGER NUMPB(100), NBPB
!
      LOGICAL EXIST
!
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
!
      DATA SOMSUI / 2 , 3 , 4 , 0 /
!
!=======================================================================
! INITIALISATION
!=======================================================================
!
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
!
      RETURN
!
! -------------------------FORMATS------------------------------------------
 1010 FORMAT (//,1X,'RECHERCHE DES ILES CONNECTEES',/,
     &          1X,'-----------------------------')
 1020 FORMAT (//,1X,'SEARCHING ABOUT CONNECTED ISLANDS',/,
     &          1X,'---------------------------------')
 9000 FORMAT (1X,'******************************************',/,
     &        1X,'ERREUR - ROUTINE VERIFS',/,
     &        1X,'NB DE POINT DE CONNECTION SUPERIEUR A 100',/,
     &        1X,'******************************************')
 9001 FORMAT (1X,'*****************************************',/,
     &        1X,'ERROR - ROUTINE VERIFS',/,
     &        1X,'NB OF CONNECTION POINTS GREATHER THAN 100',/,
     &        1X,'*****************************************')
!
      END