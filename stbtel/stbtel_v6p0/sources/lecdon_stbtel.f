C                       ************************ 
                        SUBROUTINE LECDON_STBTEL 
C                       ************************ 
C 
C*********************************************************************** 
C PROGICIEL : STBTEL V5.2     24/10/90    J-M HERVOUET (LNH) 30 71 80 18 
C                             09/11/94    P LANG / LHF 
C                                08/96    P CHAILLET/ LHF  
C                                01/99    A CABAL/ P LANG SOGREAH 
C*********************************************************************** 
C 
C FONCTION : LECTURE DU FICHIER CAS PAR APPEL DU LOGICIEL DAMOCLES. 
C 
C---------------------------------------------------------------------- 
C                             ARGUMENTS 
C .________________.____.______________________________________________ 
C |      NOM       |MODE|                   ROLE 
C |________________|____|______________________________________________ 
C |    NCLE       | -->| NUMERO D'UNITE LOGIQUE DES MOTS-CLES DE REF. 
C |    NCAS        | -->| NUMERO D'UNITE LOGIQUE DU FICHIER CAS. 
C |    STD         |<-- | STANDARD  DE BINAIRE 
C |    DECTRI      |<-- | DECOUPAGE DES TRIANGLES SURCONTRAINTS 
C |    FOND        |<-- | TABLEAU DES NOMS DES FICHIERS DE BATHYMETRIE 
C |    EPSI        |<-- | DISTANCE MINIMALE ENTRE 2 NOEUDS DU MAILLAGE 
C |                |    | L'INTERPOLATION DES FONDS 
C |    COLOR       |<-- | ECRITURE DE LA COULEUR DES NOEUDS 
C |    NBAT        |<-- | NOMBRE DE POINTS DE BATHYMETRIE 
C |    ELIDEP      |<-- | ELIMINATION DES DEPENDANCES ARRIERES 
C |    NBFOND      |<-- | NOMBRE DE FICHIERS BATHY 
C |    MAILLE      |<-- | MAILLEUR UTILISE : 
C |                |    |   SUPERTAB VERSION 6 : SUPERTAB6 (DEFAUT) 
C |                |    |   SUPERTAB VERSION 4 : SUPERTAB4 
C |                |    |   SIMAIL 
C |    DM          |<-- | DISTANCE MNIMALE A LA FRONTIERE POUR 
C |                |    | L'INTERPOLATION DES FONDS 
C |    FONTRI      |<-- | INDICATEUR DE LECTURE DES FONDS DANS TRIGRID 
C |    CORTRI      |<-- | CORRECTION DES FONDS DE TRIGRID 
C |    OPTASS      |    | 
C |    ADDFAS      |<-- | CONDITION LIMITE DANS FICHIER ADDITIONNEL 
C |    ELISEC      |<-- | INDIC ELIMINATION DES ELEMENTS SECS 
C |    ELPSEC      |<-- | INDIC ELIM ELEMENTS PARTIELLEMENT SECS
C |    SEUSEC      |<-- | VALEUR POUR LA DEFINITION SECHERESSE 
C |    STOTOT      |<-- | INDIC RECUP TOTALITE DES PAS DE TEMPS           
C |________________|____|______________________________________________ 
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
C---------------------------------------------------------------------- 
C 
C APPELE PAR : STBTEL 
C APPEL DE : DAMOCL 
C 
C********************************************************************** 
C
      USE DECLARATIONS_TELEMAC 
      USE DECLARATIONS_STBTEL 
C 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
C 
      INTEGER NMAX 
      PARAMETER(NMAX=200) 
C 
C AJOUTE POUR EDAMOX: 
C 
      INTEGER          TROUVE(4,NMAX)  
      INTEGER          ADRES(4,NMAX) , DIMENS(4,NMAX) , MOTINT(NMAX) 
      INTEGER          NLNG  
      CHARACTER*144    MOTCAR(NMAX) 
      CHARACTER*72     MOTCLE(4,NMAX,2) 
      DOUBLE PRECISION MOTREA(NMAX) 
      LOGICAL          DOC 
      LOGICAL          MOTLOG(NMAX) 
C 
C FIN DES VARIABLES AJOUTEES POUR EDAMOX: 
C 
      INTEGER I                
C
C----------------------------------------------------------------------- 
C 
      DOC = .FALSE. 
      NLNG=2 
C
      CALL DAMOCLES( ADRES  , DIMENS , NMAX   , DOC    , LNG , LU , 
     *               MOTINT , MOTREA , MOTLOG , MOTCAR , 
     *               MOTCLE , TROUVE , NCLE  , NCAS   , .FALSE. )  
C
C    AFFECTATION DES PARAMETRES SOUS LEUR NOM EN FORTRAN 
C 
C----------------------------------------------------------------------- 
C MOTS CLE DE TYPE ENTIER 
C----------------------------------------------------------------------- 
C 
      NBAT      = MOTINT  (ADRES(1,1)) 
      LGVEC     = MOTINT  (ADRES(1,2)) 
C>>>> 
      NSOM      = MIN(MOTINT  (ADRES(1,3)),9) 
      NSOM2     = MIN(MOTINT  (ADRES(1,4)),9) 
C<<<< 
C 
C----------------------------------------------------------------------- 
C MOTS CLE DE TYPE REEL 
C----------------------------------------------------------------------- 
C 
      EPSI      = MOTREA  (ADRES(2,1)) 
      DM        = MOTREA  (ADRES(2,2)) 
      CORTRI    = MOTREA  (ADRES(2,3)) 
C>>>> 
      IF (NSOM.GE.3) THEN 
         DO 2 I=1,NSOM 
            SOM(I,1) = MOTREA  (ADRES(2,4)+I-1) 
            SOM(I,2) = MOTREA  (ADRES(2,5)+I-1) 
2        CONTINUE 
         SOM(NSOM+1,1) = SOM(1,1) 
         SOM(NSOM+1,2) = SOM(1,2) 
      ENDIF 
C 
      IF (NSOM2.GE.3) THEN 
         DO 3 I=1,NSOM2 
            SOM2(I,1) = MOTREA  (ADRES(2,6)+I-1) 
            SOM2(I,2) = MOTREA  (ADRES(2,7)+I-1) 
3        CONTINUE 
         SOM2(NSOM2+1,1) = SOM2(1,1) 
         SOM2(NSOM2+1,2) = SOM2(1,2) 
      ENDIF 
C<<<< 
      SEUSEC =  MOTREA  (ADRES(2,8))
C 
C----------------------------------------------------------------------- 
C MOTS CLE DE TYPE LOGIQUE 
C----------------------------------------------------------------------- 
C 
      DECTRI    = MOTLOG  (ADRES(3,1)) 
      COLOR     = MOTLOG  (ADRES(3,2)) 
      ELIDEP    = MOTLOG  (ADRES(3,3)) 
      DIV4      = MOTLOG  (ADRES(3,4)) 
      FONTRI    = MOTLOG  (ADRES(3,5)) 
      OPTASS    = MOTLOG  (ADRES(3,6)) 
C 
      ADDFAS    = MOTLOG  (ADRES(3,7)) 
      PROJEX    = MOTLOG  (ADRES(3,8)) 
C 
      IF (NSOM2.GE.3) DIV4 = .TRUE. 
C 
      ELISEC = MOTLOG  (ADRES(3,9)) 
      ELPSEC = MOTLOG  (ADRES(3,10))
      STOTOT = MOTLOG  (ADRES(3,11))
C 
C----------------------------------------------------------------------- 
C MOTS CLE DE TYPE CARACTERE 
C----------------------------------------------------------------------- 
C 
      NBFOND=0 
      DO 5 I=1,DIMENS(4,8) 
         IF (MOTCAR ( ADRES(4,8) + I-1).NE.' ') THEN 
            NBFOND = NBFOND + 1
            IF(I.EQ.1) THEN
              FOND(NBFOND) = MOTCAR ( ADRES(4,8) + I-1)
              NOMFON = MOTCAR ( ADRES(4,8) + I-1)
            ELSEIF(I.EQ.2) THEN
              FOND(NBFOND) = MOTCAR ( ADRES(4,8) + I-1)
              NOMFO2 = MOTCAR ( ADRES(4,8) + I-1)
            ELSEIF(I.EQ.2) THEN
              FOND(NBFOND) = MOTCAR ( ADRES(4,8) + I-1)
              NOMIMP = MOTCAR ( ADRES(4,8) + I-1)   
            ELSEIF(I.EQ.2) THEN
              FOND(NBFOND) = MOTCAR ( ADRES(4,8) + I-1)
              NOMSOU = MOTCAR ( ADRES(4,8) + I-1)   
            ELSEIF(I.EQ.2) THEN
              FOND(NBFOND) = MOTCAR ( ADRES(4,8) + I-1)
              NOMFRC = MOTCAR ( ADRES(4,8) + I-1)   
            ENDIF             
         ENDIF 
5     CONTINUE 
C
      NOMGEO = MOTCAR( ADRES(4, 5) )
      NOMFOR = MOTCAR( ADRES(4, 3) )
      NOMCAS = MOTCAR( ADRES(4, 4) )
      NOMLIM = MOTCAR( ADRES(4, 7) )
      NOMRES = MOTCAR( ADRES(4, 6) ) 
      NOMFO1 = MOTCAR( ADRES(4,15) ) 
C
      STD       = MOTCAR ( ADRES(4,11))(1:3) 
      MAILLE    = MOTCAR ( ADRES(4,14))(1:9) 
C 
      FUSION = .FALSE. 
      IF (MOTCAR(ADRES(4,15)).NE.' '.AND.MAILLE.EQ.'SELAFIN') 
     *   FUSION = .TRUE. 
C 
C----------------------------------------------------------------------- 
C VERIFICATION DES VALEURS LUES 
C----------------------------------------------------------------------- 
C 
      IF (FONTRI) NBFOND = 1 
      IF (NBFOND.GT.5) THEN 
         IF (LNG.EQ.1) WRITE(LU,1000)  
         IF (LNG.EQ.2) WRITE(LU,4000)  
         STOP 
      ENDIF 
C 
      IF (STD.NE.'IBM'.AND.STD.NE.'I3E'.AND.STD.NE.'STD') THEN 
         IF (LNG.EQ.1) WRITE(LU,1100) STD 
         IF (LNG.EQ.2) WRITE(LU,4100) STD 
         STOP 
      ENDIF 
C 
      IF (MAILLE.NE.'SUPERTAB4'.AND.MAILLE.NE.'SUPERTAB6'.AND. 
     *    MAILLE.NE.'SIMAIL'   .AND.MAILLE.NE.'SELAFIN'  .AND. 
     *    MAILLE.NE.'TRIGRID'  .AND.MAILLE.NE.'MASTER2'  .AND. 
     *    MAILLE.NE.'FASTTABS' .AND.MAILLE.NE.'ADCIRC'   ) THEN 
         IF (LNG.EQ.1) WRITE(LU,1200) MAILLE 
         IF (LNG.EQ.2) WRITE(LU,4200) MAILLE 
         STOP 
      ENDIF 
C
      IF (MAILLE.EQ.'SUPERTAB4') THEN  
C INDICATEUR DE DEBUT DE LA LISTE DES POINTS DU MAILLAGE 
         NSEC11 = 15 
         NSEC12 = 0  
C INDICATEUR DE DEBUT DE LA LISTE DES IKLE 
         NSEC2  = 71  
C INDICATEUR DE POSITION DU TITRE 
         NSEC3  = 151  
      ELSEIF (MAILLE.EQ.'SUPERTAB6') THEN 
C INDICATEUR DE DEBUT DE LA LISTE DES POINTS DU MAILLAGE  
C LECTURE EN SIMPLE PRECISION 
         NSEC11 = 15 
C LECTURE EN DOUBLE PRECISION 
         NSEC12 = 781  
C INDICATEUR DE DEBUT DE LA LISTE DES IKLE 
         NSEC2  = 780  
C INDICATEUR DE POSITION DU TITRE 
         NSEC3  = 151  
      ELSEIF (MAILLE.EQ.'MASTER2') THEN  
C INDICATEUR DE DEBUT DE LA LISTE DES POINTS DU MAILLAGE 
         NSEC11 = 0 
         NSEC12 = 2411  
C INDICATEUR DE DEBUT DE LA LISTE DES IKLE 
         NSEC2  = 2412  
C INDICATEUR DE POSITION DU TITRE 
         NSEC3  = 151  
      ENDIF 
C
C----------------------------------------------------------------------- 
C
      IF (ELISEC) THEN
        IF (MAILLE.NE.'SELAFIN') THEN
          IF (LNG.EQ.1) WRITE(LU,1300)  
          IF (LNG.EQ.2) WRITE(LU,4300)
          STOP
        ENDIF
        IF (NBFOND.GT.0) THEN
          IF (LNG.EQ.1) WRITE(LU,1301)  
          IF (LNG.EQ.2) WRITE(LU,4301)  
          STOP
        ENDIF
        IF (DIV4) THEN
          IF (LNG.EQ.1) WRITE(LU,1302)  
          IF (LNG.EQ.2) WRITE(LU,4302)  
          STOP
        ENDIF
        DIV4      = .FALSE.  
        FONTRI    = .FALSE.  
        OPTASS    = .FALSE. 
        ADDFAS    = .FALSE.
        PROJEX    = .FALSE.  
      ENDIF
C 
C----------------------------------------------------------------------- 
C 
1000  FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . LE NOMBRE DE FICHIERS DES FONDS EST',/, 
     *          1X,'         LIMITE A 5 |',/, 
     *          1X,'         (MOT-CLE : FICHIERS DES FONDS)',/, 
     *          1X,'||||||||||||||||||||||||||||||||||||||||||||',//) 
4000  FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . THE NUMBER OF BOTTOM TOPOGRAPHY FILES',/, 
     *          1X,'         IS LIMITED TO 5 |',/, 
     *          1X,'         (KEYWORD : BOTTOM TOPOGRAPHY FILE)',/, 
     *          1X,'||||||||||||||||||||||||||||||||||||||||||||||',//) 
C 
1100  FORMAT(//,1X,'|||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . STANDARD DE BINAIRE INCONNU : ',A3,/, 
     *          1X,'         (MOT-CLE : STANDARD DE BINAIRE)',/, 
     *          1X,'|||||||||||||||||||||||||||||||||||||||||||',//) 
4100  FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . UNKNOWN BINARY FILE STANDARD : ',A3,/, 
     *          1X,'         (KEYWORD : BINARY FILE STANDARD)',/, 
     *          1X,'||||||||||||||||||||||||||||||||||||||||||||',//) 
C 
1200  FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . TYPE DE MAILLAGE INCONNU : ',A9,/, 
     *          1X,'         (MOT-CLE : MAILLEUR)',/, 
     *          1X,'||||||||||||||||||||||||||||||||||||||||||||||',//) 
4200  FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . UNKNOWN TYPE OF MESH GENERATOR : ',A9,/, 
     *          1X,'         (KEYWORD : MESH GENERATOR)',/, 
     *          1X,'||||||||||||||||||||||||||||||||||||||||||||||',//) 
 1300 FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . L''ELIMINATION DES ELEMENTS SECS',/,
     *          1X,'N''EST POSSIBLE QU''AVEC UN FICHIER SELAFIN.',/,
     *          1X,'||||||||||||||||||||||||||||||||||||||||||||',//) 
 4300 FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . THE DRY ELEMENTS ELIMINATION IS ONLY',/,
     *          1X,'AVAILABLE WHEN USING SELAFIN FILE.',/,
     *          1X,'||||||||||||||||||||||||||||||||||||||||||||',//) 
 1301 FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . INTERPOLATION DE BATHYMETRIE IMPOSSIBLE',/,
     *          1X,'LORS DU TRAITEMENT DES ELEMENTS SECS.',/,
     *          1X,'||||||||||||||||||||||||||||||||||||||||||||',//) 
 4301 FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . BATHYMETRY INTERPOLATION IMPOSSIBLE',/,
     *          1X,'WHEN USING DRY ELEMENTS ELIMINATION.',/,
     *          1X,'||||||||||||||||||||||||||||||||||||||||||||',//) 
 1302 FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . DECOUPAGE DES ELEMENTS IMPOSSIBLE',/,
     *          1X,'LORS DU TRAITEMENT DES ELEMENTS SECS.',/,
     *          1X,'||||||||||||||||||||||||||||||||||||||||||||',//) 
 4302 FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/, 
     *          1X,'LECDON . TRIANGLE CUTTING IMPOSSIBLE',/,
     *          1X,'WHEN USING DRY ELEMENTS ELIMINATION.',/,
     *          1X,'||||||||||||||||||||||||||||||||||||||||||||',//) 
 
C----------------------------------------------------------------------- 
C 
      RETURN 
      END 
