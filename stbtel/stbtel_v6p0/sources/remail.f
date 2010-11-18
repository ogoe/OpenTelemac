C                       *****************
                        SUBROUTINE REMAIL
C                       *****************
C
     *(IKLE,NCOLOR,NEW,X,Y,EPSI)
C
C***********************************************************************
C  PROGICIEL : STBTEL V5.2  17/08/89   J.M. JANIN    (LNH)
C
C***********************************************************************
C
C    FONCTION : ELIMINATION DES POINTS COINCIDENTS ET DES TROUS DU
C               MAILLAGE , RECONSTRUCTION DES TABLEAUX IKLE ET NCOLOR
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    IKLE        |<-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT |
C |    NCOLOR      |<-->| TABLEAU DES COULEURS DES POINTS              |
C |    PTELI       |<-->| TABLEAU DE TRAVAIL ENTIER.                   |
C |    NEW         |<-->| TABLEAU DE TRAVAIL ENTIER.                   |
C |    X,Y         |<-->| COORDONNEES DES POINTS                       |
C |    EPSI        | -->| DISTANCE MINIMALE ENTRE 2 NOEUDS DU MAILLAGE |
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
C APPEL DE : -
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER MESH , NDP , I , NPOIN , J , NELEM , NPTELI , NELELI
      INTEGER NELMAX , NPMAX , I1, I2, I3, I4, J1, J2, J3, J4
      INTEGER IKLE(NELMAX,4) , NEW(*) , NCOLOR(*)
C
      DOUBLE PRECISION X(*) , Y(*) , EPSI
C
      LOGICAL PTPRO , PTELI , ELELI
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C RECHERCHE DES POINTS N'APPARTENANT A AUCUN ELEMENT
C=======================================================================
C
      DO 10 I=1,NPOIN
         NEW(I) = 0
10    CONTINUE
C
      DO 20 I=1,NELEM
         NEW(IKLE(I,1)) = IKLE(I,1)
         NEW(IKLE(I,2)) = IKLE(I,2)
         NEW(IKLE(I,3)) = IKLE(I,3)
         IF(NDP.EQ.4) NEW(IKLE(I,4)) = IKLE(I,4)
20    CONTINUE
C
C=======================================================================
C RECHERCHE DES POINTS TROP PROCHES
C=======================================================================
C
      EPSI   = EPSI * EPSI
      PTPRO  = .FALSE.
      PTELI  = .FALSE.
      NPTELI = 0
C
      DO 30 I=1,NPOIN-1
         IF(NEW(I).EQ.I) THEN
            DO 40 J=I+1,NPOIN
               IF((X(I)-X(J))**2+(Y(I)-Y(J))**2.LT.EPSI
     *            .AND.NEW(J).EQ.J) THEN
                  PTPRO  = .TRUE.
                  NEW(J) = I
               ENDIF
40          CONTINUE
         ELSE
            PTELI = .TRUE.
         ENDIF
30    CONTINUE
C
C=======================================================================
C SEUL LE DERNIER POINT EST A ELIMINER
C=======================================================================
C
      IF(.NOT.PTELI.AND.NEW(NPOIN).NE.NPOIN) NPTELI = 1
C
C=======================================================================
C MODIFICATION DES IKLE SI DETECTION DE POINTS TROP PROCHES
C=======================================================================
C
      IF(PTPRO) THEN
         DO 50 I=1,NELEM
            IKLE(I,1) = NEW(IKLE(I,1))
            IKLE(I,2) = NEW(IKLE(I,2))
            IKLE(I,3) = NEW(IKLE(I,3))
            IF(NDP.EQ.4) IKLE(I,4) = NEW(IKLE(I,4))
50       CONTINUE
      ENDIF
C
C=======================================================================
C REMPLISSAGE DES TROUS LAISSES PAR L'ELIMINATION DE POINTS
C=======================================================================
C
      IF(PTELI) THEN
         DO 60 I=1,NPOIN
            IF(NEW(I).EQ.I) THEN
               NEW(I) = I - NPTELI
               X(I-NPTELI) = X(I)
               Y(I-NPTELI) = Y(I)
               NCOLOR(I-NPTELI) = NCOLOR(I)
            ELSE
               NPTELI = NPTELI + 1
            ENDIF
60       CONTINUE
C
C=======================================================================
C MODIFICATION DES IKLE DUE AU REMPLISSAGE DES TROUS
C=======================================================================
C
         DO 70 I=1,NELEM
            IKLE(I,1) = NEW(IKLE(I,1))
            IKLE(I,2) = NEW(IKLE(I,2))
            IKLE(I,3) = NEW(IKLE(I,3))
            IF(NDP.EQ.4) IKLE(I,4) = NEW(IKLE(I,4))
70       CONTINUE
      ENDIF
C
      NPOIN = NPOIN - NPTELI
C
C=======================================================================
C RECHERCHE ET ELIMINATION DES ELEMENTS DEGENERES
C RECHERCHE ET ELIMINATION DES ELEMENTS SUPERPOSES
C=======================================================================
C
      ELELI  = .FALSE.
      NELELI = 0
C
      IF (NDP.EQ.3) THEN
C
         DO 75 I=1,NELEM
            I1 = IKLE(I,1)
            I2 = IKLE(I,2)
            I3 = IKLE(I,3)
            NEW(I) = 0
            IF (I1.EQ.I2.OR.I1.EQ.I3.OR.I2.EQ.I3) NEW(I) = 1
75       CONTINUE
C
         DO 80 I=1,NELEM-1
            IF (NEW(I).EQ.0) THEN
               I1 = IKLE(I,1)
               I2 = IKLE(I,2)
               I3 = IKLE(I,3)
               DO 90 J=I+1,NELEM
                  IF (NEW(J).EQ.0) THEN
                     J1 = IKLE(J,1)
                     J2 = IKLE(J,2)
                     J3 = IKLE(J,3)
                     IF ((I1.EQ.J1.OR.I1.EQ.J2.OR.I1.EQ.J3).AND.
     *                   (I2.EQ.J1.OR.I2.EQ.J2.OR.I2.EQ.J3).AND.
     *                   (I3.EQ.J1.OR.I3.EQ.J2.OR.I3.EQ.J3)) NEW(J) = 1
                  ENDIF
90             CONTINUE
            ELSE
               ELELI = .TRUE.
            ENDIF
80       CONTINUE
C
      ELSE
C
         DO 95 I=1,NELEM
            I1 = IKLE(I,1)
            I2 = IKLE(I,2)
            I3 = IKLE(I,3)
            I4 = IKLE(I,4)
            NEW(I) = 0
            IF (I1.EQ.I2.OR.I1.EQ.I3.OR.I1.EQ.I4.OR.
     *          I2.EQ.I3.OR.I2.EQ.I4.OR.I3.EQ.I4) NEW(I) = 1
95       CONTINUE
C
         DO 100 I=1,NELEM-1
            IF (NEW(I).EQ.0) THEN
               I1 = IKLE(I,1)
               I2 = IKLE(I,2)
               I3 = IKLE(I,3)
               I4 = IKLE(I,4)
               DO 110 J=I+1,NELEM
                  IF (NEW(J).EQ.0) THEN
                     J1 = IKLE(J,1)
                     J2 = IKLE(J,2)
                     J3 = IKLE(J,3)
                     J4 = IKLE(J,4)
             IF((I1.EQ.J1.OR.I1.EQ.J2.OR.I1.EQ.J3.OR.I1.EQ.J4).AND.
     *          (I2.EQ.J1.OR.I2.EQ.J2.OR.I2.EQ.J3.OR.I2.EQ.J4).AND.
     *          (I3.EQ.J1.OR.I3.EQ.J2.OR.I3.EQ.J3.OR.I3.EQ.J4).AND.
     *          (I4.EQ.J1.OR.I4.EQ.J2.OR.I4.EQ.J3.OR.I4.EQ.J4)) NEW(J)=1
                  ENDIF
110            CONTINUE
            ELSE
               ELELI = .TRUE.
            ENDIF
100      CONTINUE
C
      ENDIF
C
C=======================================================================
C SEUL LE DERNIER ELEMENT EST A ELIMINER
C=======================================================================
C
      IF(.NOT.ELELI.AND.NEW(NELEM).EQ.1) NELELI = 1
C
C=======================================================================
C REMPLISSAGE DES TROUS LAISSES PAR L'ELIMINATION D'ELEMENTS
C=======================================================================
C
      IF(ELELI) THEN
         DO 120 I=1,NELEM
            IF(NEW(I).EQ.0) THEN
               IKLE(I-NELELI,1) = IKLE(I,1)
               IKLE(I-NELELI,2) = IKLE(I,2)
               IKLE(I-NELELI,3) = IKLE(I,3)
               IF(NDP.EQ.4) IKLE(I-NELELI,4) = IKLE(I,4)
            ELSE
               NELELI = NELELI + 1
            ENDIF
120      CONTINUE
      ENDIF
C
      NELEM = NELEM - NELELI
C
C=======================================================================
C  SORTIE LISTING
C=======================================================================
C
      IF (LNG.EQ.1) WRITE(LU,130) NPTELI,NELELI,NPOIN,NELEM
      IF (LNG.EQ.2) WRITE(LU,3130) NPTELI,NELELI,NPOIN,NELEM
 130  FORMAT(//,1X,'MISE AU STANDARD TELEMAC',
     *        /,1X,'------------------------',/,
     *        /,1X,'RENUMEROTATION EFFECTUEE :',
     *        /,6X,I6,' POINTS ELIMINES',
     *        /,6X,I6,' ELEMENTS ELIMINES',
     *        /,1X,'NOUVEAU NOMBRE DE POINTS   :',I6,
     *        /,1X,'NOUVEAU NOMBRE D''ELEMENTS  :',I6)
 3130 FORMAT(//,1X,'SETTING TELEMAC STANDARD',
     *        /,1X,'------------------------',/,
     *        /,1X,'RENUMBERING DONE :',
     *        /,6X,I6,' POINTS CANCELLED',
     *        /,6X,I6,' ELEMENTS CANCELLED',
     *        /,1X,'NEW NUMBER OF POINTS   : ',I6,
     *        /,1X,'NEW NUMBER OF ELEMENTS : ',I6)
C
       RETURN
       END
