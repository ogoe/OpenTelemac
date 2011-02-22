C                       *****************
                        SUBROUTINE PROJEC
C                       *****************
C
     *(X , Y , ZF , XRELV , YRELV , ZRELV , NBAT ,
     * NBOR , NPTFR , NFOND , NBFOND , FOND , DM ,
     * FONTRI , CORTRI , MAILLE,NGEO,KP1BOR)
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2         24/04/91    J-C GALLAND  (LNH)
C                                 09/11/94    P LANG / TRIGRID (LHF)
C                               07/96    P CHAILLET / FASTTABS (LHF)
C***********************************************************************
C
C FONCTION : INTERPOLATION DES FONDS SUR LE MAILLAGE
C
C----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |    X,Y         | -->|  COORDONNEES DES POINTS DU MAILLAGE
C |    ZF          |<-- |  COTES DU FOND
C |    XRELV,YRELV | -->|  COORDONNEES DES POINTS DE BATHY
C |    ZRELV       | -->|  COTES DES POINTS DE BATHY
C |    NBAT        | -->|  NOMBRE DE POINTS DE BATHY
C |    NBOR        | -->|  NUMEROTATION DES ELEMENTS DE BORD
C |    NPTFR       | -->|  NOMBRE DE POINTS FRONTIERE
C |    NFOND       | -->|  CANAUX DES FICHIERS DES FONDS
C |    NBFOND      | -->|  NOMBRE DE FICHIERS FONDS DONNES PAR
C |                |    |  L'UTILISATEUR (5 MAXI)
C |    FOND        | -->|  NOM DES FICHIERS DES FONDS
C |    DM          | -->|  DISTANCE MINIMALE A LA FRONTIERE
C |                |    |  POUR L'INTERPOLATION DES FONDS
C |    FONTRI      | -->|  INDICATEUR DE LECTURE DES FONDS DANS TRIGRID
C |    CORTRI      | -->|  CORRECTION DES FONDS POUR TRIGRID
C |    MAILLE      | -->| NOM DU MAILLEUR UTILISE
C |                |    |
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
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C----------------------------------------------------------------------
C
C APPELE PAR : STBTEL
C APPEL DE : LECFON, FASP
C
C**********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I , NPT , NPOIN , NPMAX , NPTFR , IVOIS , NBAT
      INTEGER NELMAX , NELEM , MESH , NDP
      INTEGER NFOND(*) , NBOR(NPTFR,2) , NP(5) , NBFOND
      INTEGER NGEO, KP1BOR(NPTFR)
C
      DOUBLE PRECISION DIST , DIST2
      DOUBLE PRECISION XRELV(*) , YRELV(*) , ZRELV(*)
      DOUBLE PRECISION X(*) , Y(*) , ZF(*) , DM
      DOUBLE PRECISION CORTRI
C
      CHARACTER*72 FOND(NBFOND)
C
C Ajout PCt - 11/07/96
      CHARACTER*9  MAILLE
C
      LOGICAL FONTRI
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C  LECTURE DES FICHIERS DES FONDS
C=======================================================================
C
      CALL LECFON (XRELV,YRELV,ZRELV,NBAT,NFOND,NBFOND,NP,NPT,
     *             FONTRI,CORTRI,MAILLE,NGEO)
C
      IF (.NOT.FONTRI) THEN
        IF (NBFOND.NE.0.AND.LNG.EQ.1) WRITE(LU,1000)
        IF (NBFOND.NE.0.AND.LNG.EQ.2) WRITE(LU,4000)
C
        DO 20 I = 1,NBFOND
           IF (LNG.EQ.1) WRITE(LU,1100) I,FOND(I),I,NP(I)
           IF (LNG.EQ.2) WRITE(LU,4100) I,FOND(I),I,NP(I)
 20     CONTINUE
      ENDIF
C
C=======================================================================
C  DETERMINATION DE LA COTE DU FOND AU POINT I PAR INTERPOLATION
C  SUR LES POINTS NON EXTERIEURS AU DOMAINE
C=======================================================================
C
      CALL FASP (X,Y,ZF,NPOIN,XRELV,YRELV,ZRELV,NPT,NBOR,KP1BOR,
     *           NPTFR,DM)
C
C=======================================================================
C  CERTAINS POINTS N'ONT PU ETRE TRAITES PAR FASP FAUTE DE DONNEES
C  LEUR PROFONDEUR A ETE MISE A -1.E6
C  ON AFFECTE A CES POINTS LA PROFONDEUR DE LEUR PLUS PROCHE VOISIN.
C=======================================================================
C
      DO 60 I=1,NPOIN
        IF(ZF(I).LT.-0.9D6) THEN
           DIST = 1.D12
           DO 70 IVOIS = 1 , NPOIN
              DIST2 = ( X(I)-X(IVOIS) )**2 + ( Y(I)-Y(IVOIS) )**2
              IF(DIST2.LT.DIST.AND.ZF(IVOIS).GT.-0.9D6) THEN
                 DIST = DIST2
                 ZF(I) = ZF(IVOIS)
              ENDIF
70         CONTINUE
           IF (LNG.EQ.1) WRITE(LU,1200) I,X(I),Y(I),ZF(I)
           IF (LNG.EQ.2) WRITE(LU,4200) I,X(I),Y(I),ZF(I)
        ENDIF
60    CONTINUE
C
C-----------------------------------------------------------------------
C
 1000 FORMAT(//,1X,'INTERPOLATION DES FONDS A PARTIR DE :',/,
     *          1X,'-------------------------------------',/)
 4000 FORMAT(//,1X,'INTERPOLATION OF BOTTOM TOPOGRAPHY FROM :',/,
     *          1X,'-----------------------------------------',/)
 1100 FORMAT(1X,'FOND ',I1,' : ',A72,/,
     *       1X,'NOMBRE DE POINTS LUS SUR LE FICHIER FOND ',I1,' : ',
     *       I6,/)
 4100 FORMAT(1X,'BOTTOM ',I1,' : ',A72,/,
     *       1X,'NUMBER OF POINTS READ IN THE BOTTOM TOPOGRAPHY FILE ',
     *       I1,' : ',I6,/)
 1200 FORMAT('POINT : ',I5,' X = ',F10.1,' Y = ',F10.1,
     *       '  PAS DE DONNEES , ZF : ',F8.2)
 4200 FORMAT('POINT : ',I5,' X = ',F10.1,' Y = ',F10.1,
     *       '  NO DATA , ZF : ',F8.2)
C
      RETURN
      END
