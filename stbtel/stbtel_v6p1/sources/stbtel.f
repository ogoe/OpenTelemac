C                       ***************** 
                        SUBROUTINE STBTEL 
C                       ***************** 
C  
     *( NPOIN1 , TYPELE , NFOND , PRECIS , NSFOND , TITRE)  
C 
C*********************************************************************** 
C PROGICIEL : STBTEL  6.0           09/08/89    J.C. GALLAND 
C                                    19/02/93    J.M. JANIN 
C                                    09/11/94    P. LANG / LHF (TRIGRID) 
C                                  07/96    P. CHAILLET / LHF (FASTTABS) 
C                                  09/98    A. CABAL / P. LANG SOGREAH 
C*********************************************************************** 
C 
C     FONCTION  : PROGRAMME PRINCIPAL 
C 
C----------------------------------------------------------------------- 
C                             ARGUMENTS 
C .________________.____.______________________________________________ 
C |      NOM       |MODE|                   ROLE 
C |________________|____|______________________________________________ 
C | X,Y            |<-- | COORDONNEES DES POINTS DU MAILLAGE 
C | ZF             |<-- | COTES DU FOND 
C | XR,YR          |<-- | COORDONNEES DES POINTS DE BATHY 
C | ZR             |<-- | COTES DES POINTS DE BATHY 
C | NBAT           | -->| NOMBRE DE POINTS DE BATHY 
C | IKLE           |<-- | NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT 
C | IFABOR         |<-- | NUMERO DE L'ELEMENT VOISIN DE CHAQUE FACE 
C | NBOR           |<-- | NUMEROTATION DES ELEMENTS DE BORD 
C | TRAV1,2        |<-->| TABLEAUX DE TRAVAIL 
C | NCOLOR         |<-- | TABLEAU DES COULEURS DES NOEUDS 
C | NCOLFR         |<-- | TABLEAU DES COULEURS DES NOEUDS FRONTIERES  
C | NOP5           | -->| TABLEAU DE TRAVAIL POUR LA LECTURE DU FICHIER 
C |                |    | GEOMETRIE DE SIMAIL 
C | NPOIN1         | -->| NOMBRE REEL DE POINTS DU MAILLAGE 
C |                |    | (NPOIN REPRESENTE L'INDICE MAX DES NOEUDS CAR 
C |                |    | SUPERTAB LAISSE DES TROUS DANS LA NUMEROTATION 
C | TYPELE         | -->| TYPE DES ELEMENTS 
C | STD            | -->| STANDARD DE BINAIRE 
C | DECTRI         | -->| DECOUPAGE OU NON DES TRIANGLES SURCONTRAINTS 
C | FOND           | -->| TABLEAU DES NOMS DES FICHIERS BATHY 
C | NFOND          | -->| TABLEAU DES CANAUX DES FICHIERS BATHY 
C | EPSI           | -->| DISTANCE MINIMALE ENTRE 2 POINTS POUR DEFINIR 
C |                |    | LES POINTS DE MAILLAGE CONFONDUS 
C | COLOR          |<-- | COULEUR DES NOEUDS 
C | ELIDEP         | -->| LOGIQUE POUR L'ELIMINATION DES MOTS-CLES 
C | NBFOND         | -->| NOMBRE DE FICHIERS DE BATHY 
C | MAILLE         | -->| NOM DU MAILLEUR UTILISE 
C | DM             | -->| DISTANCE MINIMALE A LA FRONTIERE 
C |                |    | POUR LA PROJECTION DES FONDS 
C | PRECIS         | -->| FORMAT DE LECTURE DES COORDONNEES DES NOEUDS 
C | FONTRI         | -->| INDICATEUR DE LECTURE DES FONDS DANS NGEO 
C | CORTRI         | -->| CORRECTION DES FONDS POUR TRIGRID 
C | TFAST1,2       | -->| TABLEAUX DE TRAVAIL (FASTTABS) 
C | ADDFAS         | -->| INDICATEUR UTILISATION DES C.L. (FASTTABS) 
C | VAR            | -->| TABLEAU DOUBLE PREC. SERVANT A LIRE LES RESULTATS     
C | ELISEC         | -->| INDICATEUR ELIMINATION DES ELEMENTS SECS  
C | ELPSEC         | -->| INDICATEUR ELIM DES ELEMENTS PARTIELLEMENT SECS
C | SEUSEC         | -->| VALEUR POUR LA DEFINITION SECHERESSE              
C | ISDRY          | -->| TABLEAU D'INDICATEURS HAUTEUR NULLE 
C | IHAUT          | -->| INDICE DE LA HAUTEUR_D_EAU DANS LA LISTE DES VARIABLES 
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
C |  FICH:         |    | 
C |    NRES        |--> | NUMERO DU CANAL DU FICHIER DE SERAFIN 
C |    NGEO       |--> | NUMERO DU CANAL DU FICHIER MAILLEUR 
C |    NLIM      |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC 
C |    NFO1      |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID 
C |  SECT:         |    | 
C |    NSEC11      |--> | INDICATEUR DU SECTEUR CONTENANT LES NOEUDS 
C |                |--> | (LECTURE EN SIMPLE PRECISION) 
C |    NSEC12      |--> | INDICATEUR DU SECTEUR CONTENANT LES NOEUDS 
C |                |--> | (LECTURE EN DOUBLE PRECISION) 
C |    NSEC2       |--> | INDICATEUR DU SECTEUR CONTENANT LES ELEMENTS 
C |    NSEC3       |--> | INDICATEUR DU SECTEUR CONTENANT LE TITRE 
C |________________|____|______________________________________________ 
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
C----------------------------------------------------------------------- 
C APPELE PAR : HOMERE 
C APPEL DE : LECSIM, LECSTB, IMPRIM , VERIFI, VOISIN, RANBO, SURCON, 
C            SHUFLE, CORDEP, DEPARR, PROJEC, PRESEL, FMTSEL, ECRSEL, 
C            DYNAMI 
C*********************************************************************** 
C
C     USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_STBTEL
C 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
C 
      INTEGER NPOIN1 , NPOIN , NELEM , IELM 
      INTEGER NELMAX , MESH , NPTFR , NITER 
      INTEGER NDEPAR , NPMAX , NDP 
C
C     TABLEAU BIDON UTILISE PAR VOISIN SEULEMENT EN PARALLELISME
      INTEGER NACHB(1)
C  
      INTEGER NFOND(5)
      INTEGER STAND , NSFOND  
      INTEGER NVAR , NVARCL   
      INTEGER NPINIT , NEINIT  
      INTEGER NUMPB(100), NBPB, I,IPARAM(10)
      DATA IPARAM/0,0,0,0,0,0,0,0,0,0/
C
      REAL, DIMENSION(:), ALLOCATABLE :: W
      DOUBLE PRECISION,DIMENSION(:)  ,ALLOCATABLE :: WORK,X,Y,ZF
      DOUBLE PRECISION,DIMENSION(:)  ,ALLOCATABLE :: XR,YR,ZR
      DOUBLE PRECISION,DIMENSION(:)  ,ALLOCATABLE :: XINIT,YINIT
      DOUBLE PRECISION,DIMENSION(:)  ,ALLOCATABLE :: VAINIT,VAR
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE :: SHP
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: TRAV1,TRAV2,TRAV3 
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLE,IFABOR,IKINIT
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR,KP1BOR,LIUBOR
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: LIVBOR,LITBOR,LIHBOR 
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: ELT,NCOLOR,NCOLFR,NOP5
      INTEGER, DIMENSION(:),ALLOCATABLE :: TFAST1,TFAST2,ISDRY,IPOBO      
C 
      CHARACTER*80 TITRE 
      CHARACTER*11 TYPELE 
C
      CHARACTER*6  PRECIS 
      CHARACTER*32 TEXTE(26) , VARCLA(1) 
C 
      LOGICAL SORLEO(26) 
      LOGICAL SUIT , ECRI , DEBU , LISTIN   
C
      INTEGER DATE(3) , TIME(3) 
      DOUBLE PRECISION TPSFIN(1) 
      INTEGER NVARIN , NVAROU , NVAR2 ,ERR
      INTEGER NSOR , MXPTVS , MXELVS 
C 
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX 
C
C     ALLOCATION DYNAMIQUE DES TABLEAUX REELS
C
      ALLOCATE(W(NPOIN)       ,STAT=ERR)
      ALLOCATE(WORK(NPOIN)    ,STAT=ERR)
      ALLOCATE(X(NPMAX)       ,STAT=ERR)
      ALLOCATE(Y(NPMAX)       ,STAT=ERR)
      ALLOCATE(ZF(NPMAX)      ,STAT=ERR)
      ALLOCATE(XR(NBAT)       ,STAT=ERR)
      ALLOCATE(YR(NBAT)       ,STAT=ERR)
      ALLOCATE(ZR(NBAT)       ,STAT=ERR)
      ALLOCATE(XINIT(NPOIN)   ,STAT=ERR)
      ALLOCATE(YINIT(NPOIN)   ,STAT=ERR)
      ALLOCATE(VAINIT(NPOIN)  ,STAT=ERR)
      ALLOCATE(VAR(NPMAX)     ,STAT=ERR)
      ALLOCATE(SHP(NPMAX,3)   ,STAT=ERR)
      ALLOCATE(NOP5(INOP5)    ,STAT=ERR)
C
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,7000) ERR
        IF(LNG.EQ.2) WRITE(LU,8000) ERR
7000    FORMAT(1X,'STBTEL : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     *            'CODE D''ERREUR : ',1I6)
8000    FORMAT(1X,'STBTEL: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     *            'ERROR CODE: ',1I6)
        STOP
      ENDIF
C
C     ALLOCATION DYNAMIQUE DES TABLEAUX ENTIERS
C
      ALLOCATE(TRAV1(4*NELMAX)  ,STAT=ERR)
      ALLOCATE(TRAV2(4*NELMAX)  ,STAT=ERR)
      ALLOCATE(TRAV3(NPMAX)     ,STAT=ERR)
      ALLOCATE(NCOLOR(NPMAX)    ,STAT=ERR)
      ALLOCATE(IKLE(NELMAX,4)   ,STAT=ERR)
      ALLOCATE(IKINIT(NELEM,3)  ,STAT=ERR)
      ALLOCATE(IFABOR(NELMAX,4) ,STAT=ERR)
      ALLOCATE(ELT(NPMAX)       ,STAT=ERR)
      ALLOCATE(TFAST1(NPMAX)    ,STAT=ERR)
      ALLOCATE(TFAST2(NPMAX)    ,STAT=ERR)
      ALLOCATE(ISDRY(NPMAX)     ,STAT=ERR)
C     NPTFR REMPLACE PAR NPMAX (VALEUR PAR EXCES)
      ALLOCATE(NBOR(NPMAX)      ,STAT=ERR)
      ALLOCATE(KP1BOR(NPMAX)    ,STAT=ERR)
      ALLOCATE(LIUBOR(NPMAX)    ,STAT=ERR)
      ALLOCATE(LIVBOR(NPMAX)    ,STAT=ERR)
      ALLOCATE(LITBOR(NPMAX)    ,STAT=ERR)
      ALLOCATE(LIHBOR(NPMAX)    ,STAT=ERR)
      ALLOCATE(NCOLFR(NPMAX)    ,STAT=ERR)
C
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,7000) ERR
        IF(LNG.EQ.2) WRITE(LU,8000) ERR
        STOP
      ENDIF
C 
C======================================================================= 
C LECTURE DES COORDONNEES ET DE LA COULEUR DES POINTS , DES IKLE ET DU 
C TITRE DU MAILLAGE 
C======================================================================= 
C 
      NVARIN = 0 
C 
      IF (MAILLE.EQ.'SELAFIN') THEN
         ALLOCATE(IPOBO(NPOIN)     ,STAT=ERR)
         IF(ERR.NE.0) THEN
           IF(LNG.EQ.1) WRITE(LU,7000) ERR
           IF(LNG.EQ.2) WRITE(LU,8000) ERR
           STOP
         ENDIF 
         CALL LECSEL (XINIT,YINIT,IKINIT,NPINIT,NEINIT,X,Y,IKLE,TRAV1, 
     *                W,TITRE,TEXTE,NVARIN,NVAR2,STD,NCOLOR,FUSION,
     *                NGEO,NFO1,IPOBO,IPARAM,DATE,TIME)
      ELSEIF (MAILLE.EQ.'ADCIRC') THEN 
         CALL LECADC (X,Y,ZF,IKLE,NGEO)
         NSFOND=1
      ELSEIF (MAILLE.EQ.'SIMAIL') THEN 
         CALL LECSIM (X,Y,IKLE,NCOLOR,TITRE,NOP5,NGEO) 
      ELSEIF (MAILLE.EQ.'TRIGRID') THEN 
         CALL LECTRI (X,Y,IKLE,NCOLOR,NGEO,NFO1) 
         TITRE = 'MAILLAGE TRIGRID' 
      ELSEIF (MAILLE.EQ.'FASTTABS') THEN 
         CALL LECFAS (X,Y,IKLE, NCOLOR, TFAST1, TFAST2, ADDFAS,
     *                NGEO , NFO1) 
         TITRE = 'MAILLAGE FASTTABS' 
      ELSE 
         CALL LECSTB (X,Y,IKLE,NCOLOR,TITRE,NPOIN1,
     *                NGEO,NSEC2,NSEC3,NSEC11,NSEC12) 
      ENDIF 
C 
C======================================================================= 
C EXTRACTION D'UN MAILLAGE 
C======================================================================= 
C 
      IF(MESH.EQ.3.AND.NSOM.GE.3) 
     *   CALL EXTRAC (X,Y,SOM,IKLE,TRAV1,NELEM,NELMAX,NPOIN,NSOM,PROJEX) 
C 
C======================================================================= 
C IMPRESSION DES DONNEES GEOMETRIQUES 
C======================================================================= 
C 
      CALL IMPRIM (NPOIN1,NPOIN,TYPELE,NELEM,TITRE,MAILLE,PRECIS) 
C 
C======================================================================= 
C DIVISION PAR 4 DE TOUTE OU PARTIE DES MAILLES 
C======================================================================= 
C 
      IF(MESH.EQ.3.AND.DIV4) THEN 
         CALL DIVISE (X,Y,IKLE,NCOLOR,NPOIN,NELEM,NELMAX,NSOM2,SOM2, 
     *                TRAV1,TRAV2) 
      ELSE 
         IF (DIV4.AND.LNG.EQ.1) WRITE(LU,901) 
         IF (DIV4.AND.LNG.EQ.2) WRITE(LU,3901) 
      ENDIF 
 
C 
C======================================================================= 
C OPTION ELIMINATION DES ELEMENTS SECS OU PARTIELLEMENT SECS 
C======================================================================= 
C 
      IF (ELISEC) THEN
        IF (MESH.EQ.3) THEN 
          IF (LNG.EQ.1) WRITE(LU,3006) 
          IF (LNG.EQ.2) WRITE(LU,3007)
          CALL ELMSEC ( ELPSEC, SEUSEC, TPSFIN, X, Y, IKLE, 
     *    NCOLOR, ISDRY, IHAUT, NVARIN, VAR, W , TRAV2, STD ,NGEO)
C
C APRES ELIMINATION, ON RECHERCHE LES POINTS FRONTIERES POSANT PROBLEME
C
          CALL VERIFI (X,Y,IKLE,NCOLOR,TRAV1,EPSI)  
          IELM = 11 
          CALL VOISIN_STBTEL(IFABOR,NELEM,NELMAX,IELM,IKLE,NPOIN, 
     *                       NACHB,NBOR,NPTFR,TRAV1,TRAV2) 
          CALL VERIFS (IFABOR,IKLE,TRAV1,NPTFR,NUMPB,NBPB)
          IF (NBPB.GT.0) THEN
            DO 50 I=1,NBPB
              IF (LNG.EQ.1) WRITE(LU,3000) NUMPB(I)
              IF (LNG.EQ.2) WRITE(LU,3001) NUMPB(I)
 50         CONTINUE
            CALL ELMPB (NBPB, NUMPB, X,Y,IKLE,NCOLOR,ISDRY,TRAV2) 
           ELSE
            IF (LNG.EQ.1) WRITE(LU,3008) 
            IF (LNG.EQ.2) WRITE(LU,3009) 
          ENDIF           
        ELSE
          IF (LNG.EQ.1) WRITE(LU,2002) 
          IF (LNG.EQ.2) WRITE(LU,4002) 
        ENDIF 
      ENDIF 
C 
C======================================================================= 
C MISE AU FORMAT TELEMAC DU MAILLAGE 
C======================================================================= 
C 
      CALL VERIFI(X,Y,IKLE,NCOLOR,TRAV1,EPSI) 
C 
C======================================================================= 
C CONSTRUCTION DU TABLEAU IFABOR 
C======================================================================= 
C 
      IELM = 21 
      IF (MESH.EQ.3) IELM = 11 
C 
      CALL VOISIN_STBTEL(IFABOR,NELEM,NELMAX,IELM,IKLE,NPOIN, 
     *                   NACHB,NBOR,NPTFR,TRAV1,TRAV2)  
C 
C======================================================================= 
C CONSTRUCTION DE LA TABLE DES POINTS DE BORD 
C    (RANGES DANS L'ORDRE TRIGONOMETRIQUE POUR LE CONTOUR 
C     ET L'ORDRE INVERSE POUR LES ILES) 
C======================================================================= 
C 
      CALL RANBO (NBOR,KP1BOR,IFABOR,IKLE,NCOLOR,TRAV1,NPTFR,X,Y,NCOLFR) 
C 
C======================================================================= 
C ELIMINATION DES TRIANGLES SURCONTRAINTS 
C======================================================================= 
C 
      IF(MESH.EQ.3.AND.DECTRI) THEN 
C 
         CALL SURCON (X,Y,IKLE,TRAV1,NBOR,NPTFR,NCOLOR,IFABOR,COLOR)
C 
      ELSE 
         IF (DECTRI.AND.LNG.EQ.1) WRITE(LU,900) 
         IF (DECTRI.AND.LNG.EQ.2) WRITE(LU,3900) 
      ENDIF 
C 
C======================================================================= 
C RENUMEROTATION DES NOEUDS POUR OPTIMISATION D'ASSEMBLAGE 
C======================================================================= 
C 
      IF(OPTASS) THEN
        CALL RENUM 
     *  (X,Y,WORK,IKLE,NBOR,TRAV1,TRAV2,TRAV3,NCOLOR,COLOR,NPTFR) 
      ENDIF
C 
C======================================================================= 
C RENUMEROTATION DES ELEMENTS POUR EVITER LES DEPENDENCES ARRIERES 
C======================================================================= 
C 
      IF (ELIDEP) THEN 
C 
         IF (LNG.EQ.1) WRITE(LU,3010) 
         IF (LNG.EQ.2) WRITE(LU,3011)
         CALL SHUFLE (IKLE,X) 
C 
         NITER = 0 
C 
10       CONTINUE 
C 
        CALL CORDEP (IKLE,LGVEC) 
C 
C======================================================================= 
C VERIFICATION DES DEPENDANCES ARRIERES 
C======================================================================= 
C 
         CALL DEPARR (IKLE,NDEPAR,LGVEC) 
         IF(NDEPAR.NE.0) THEN 
            NITER = NITER + 1 
            IF (NITER.GT.50) THEN 
               IF (LNG.EQ.1) WRITE(LU,1000) 
               IF (LNG.EQ.2) WRITE(LU,4000) 
               STOP 
            ENDIF 
            GOTO 10 
         ENDIF 
C 
         IF (LNG.EQ.1) WRITE(LU,1100) NITER 
         IF (LNG.EQ.2) WRITE(LU,4100) NITER 
C 
      ENDIF 
C 
C======================================================================= 
C PROJECTION DES FONDS SUR LE MAILLAGE 
C======================================================================= 
C 
      IF(NBFOND.NE.0) THEN 
        CALL PROJEC (X,Y,ZF,XR,YR,ZR,NBAT,NBOR,NPTFR,NFOND,NBFOND, 
     *               FOND,DM,FONTRI,CORTRI,MAILLE,NGEO,KP1BOR) 
      ENDIF 
C 
C======================================================================= 
C CONSTRUCTION DU FICHIER DE GEOMETRIE AU FORMAT SELAFIN : 
C======================================================================= 
C 
      IF (LNG.EQ.1) WRITE(LU,3002) 
      IF (LNG.EQ.2) WRITE(LU,3003)
      STAND = 3 
      NVARCL= 0 
      DEBU  = .FALSE. 
      SUIT  = .FALSE. 
      ECRI  = .TRUE. 
      LISTIN= .TRUE. 
C 
      NSOR = 26
C     SI LA DATE MANQUE
      IF(IPARAM(10).EQ.0) THEN
        DATE(1) = 0 
        DATE(2) = 0 
        DATE(3) = 0 
        TIME(1) = 0 
        TIME(2) = 0 
        TIME(3) = 0
      ENDIF 
C 
      CALL PRESEL(IKLE,TRAV1,NELEM,NELMAX,NDP,TEXTE,NBFOND,SORLEO, 
     *            COLOR,NSFOND,NVARIN,NVAROU,MAILLE) 
C 
C  ATTENTION DANS L'APPEL A FM3SEL, LE VRAI IKLE EST TRAV1 
C  ET IKLE EST EMPLOYE COMME TABLEAU DE TRAVAIL. 
C 
      CALL FM3SEL(X,Y,NPOIN,NBOR,NRES,STD,NVAR,TEXTE,TEXTE, 
     *            VARCLA,NVARCL,TITRE,SORLEO,NSOR,W,TRAV1,IKLE, 
     *            TRAV2,NELEM,NPTFR,NDP,MXPTVS,MXELVS,DATE,TIME, 
     *            DEBU,SUIT,ECRI,LISTIN,IPARAM,IPOBO) 
C 
C  INTERPOLATION DES VARIABLES DU FICHIER D'ENTREE 
C 
      IF (MAILLE.EQ.'SELAFIN') CALL INTERP 
     *   (XINIT,YINIT,IKINIT,NPINIT,NEINIT,X,Y,NPOIN,NPMAX,SHP,ELT) 
C 
      IF (ELISEC) THEN
C       ECRITURE DES VARIABLES DE SORTIE AU FORMAT RESULTAT TELEMAC-2D 
C 
        CALL ECRRES (VAINIT,IKINIT,NPINIT,NEINIT,SHP,ELT,NPOIN,NPOIN1, 
     *             NPMAX,W,X,ZF,NSFOND,NCOLOR,COLOR,VAR,NVARIN,NVAROU, 
     *             STD, NDP, TRAV1, STOTOT, TPSFIN,NGEO,NRES) 
      ELSE
C 
C       ECRITURE DES VARIABLES DE SORTIE AU FORMAT SELAFIN 
C 
        CALL ECRSEL(VAINIT,IKINIT,NPINIT,NEINIT,SHP,ELT,NPOIN,NPOIN1, 
     *             NPMAX,W,X,ZF,NSFOND,NCOLOR,COLOR,VAR,NVARIN,NVAROU, 
     *             NVAR2,STD,FUSION,NRES,NGEO,NFO1,MAILLE) 
      ENDIF
C 
C======================================================================= 
C CONSTRUCTION DU FICHIER DYNAM DE TELEMAC 
C======================================================================= 
C 
       IF (LNG.EQ.1) WRITE(LU,3004) 
       IF (LNG.EQ.2) WRITE(LU,3005)
       CALL DYNAMI (NPTFR,NBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR, 
     *             NCOLFR,MAILLE,NLIM) 
C 
  900 FORMAT(//,'********************************************',/, 
     *          'L''ELIMINATION DES ELEMENTS SURCONTRAINTS EST',/, 
     *          'PREVU UNIQUEMENT DANS LE CAS DES TRIANGLES',/, 
     *          '********************************************',/) 
 3900 FORMAT(//,'********************************************',/, 
     *          'OVERSTRESSED ELEMENTS ARE CANCELLED ONLY IN',/, 
     *          'THE CASE OF TRIANGLES                     ',/, 
     *          '********************************************',/) 
  901 FORMAT(//,'********************************************',/, 
     *          'LA DIVISION PAR 4 DE TOUTES LES MAILLES EST',/, 
     *          'PREVU UNIQUEMENT DANS LE CAS DES TRIANGLES',/, 
     *          '********************************************',/) 
 3901 FORMAT(//,'********************************************',/, 
     *          'ELEMENTS CAN BE CUT IN FOUR ONLY IN',/, 
     *          'THE CASE OF TRIANGLES                     ',/, 
     *          '********************************************',/) 
 1000 FORMAT(//,'***********************************************',/, 
     *          'ECHEC DANS L''ELIMINATION DES DEPENDANCES     ',/, 
     *          'ARRIERES (NOMBRE DE TENTATIVES : 50)           ',/, 
     *          'IL DOIT Y AVOIR TROP PEU DE NOEUDS DE MAILLAGE ',/, 
     *          '***********************************************') 
 4000 FORMAT(//,'***********************************************',/, 
     *          'FAILURE IN CANCELLING BACKWARD DEPENDENCIES    ',/, 
     *          '         (NUMBER OF ATTEMPTS : 50)             ',/, 
     *          'THERE MUST BE TOO FEW NODES IN THE MESH        ',/, 
     *          '***********************************************') 
 1100 FORMAT(1X,'ELIMINATION DES DEPENDANCES ARRIERES APRES ',I2, 
     *          ' TENTATIVE(S)') 
 4100 FORMAT(1X,'BACKWARD DEPENDENCIES ARE CANCELLED AFTER ',I2, 
     *          ' ATTEMPTS') 
 2001 FORMAT(//,1X,'ELIMINATION DES ELEMENTS SECS DU MAILLAGE',
     *        /,1X,'-----------------------------------------')
 4001 FORMAT(//,1X,'MESH DRY ELEMENT SUPPRESSION',
     *        /,1X,'----------------------------')
  
 2002 FORMAT(//,'***********************************************',/, 
     *          'ELIMINATION DES ELEMENTS SECS DU MAILLAGE ',/, 
     *          'NON IMPLANTEE SUR MAILLAGE NON TRIANGULAIRE. ',/, 
     *          '***********************************************') 
 4002 FORMAT(//,'***********************************************',/,  
     *          'MESH DRY ELEMENT SUPPRESION NOT AVAILABLE FOR ', 
     *          'NON TRIANGULAR MESH.',/, 
     *          '***********************************************') 
C
 3000 FORMAT(1X,'LE POINT NUMERO ',I6,' EST A ELIMINER') 
 3001 FORMAT(1X,'THE POINT NUMBER ',I6,' HAS TO BE REMOVED') 
 3002 FORMAT(//,1X,'GENERATION DU FICHIER DE GEOMETRIE',/,
     *         1X,'----------------------------------')
 3003 FORMAT(//,1X,'GENERATING GEOMETRY FILE',/,
     *         1X,'------------------------')
 3004 FORMAT(//,1X,'TRAITEMENT DES CONDITIONS AUX LIMITES',/,
     *         1X,'-------------------------------------')
 3005 FORMAT(//,1X,'TREATMENT OF BOUNDARY CONDITIONS',/,
     *         1X,'--------------------------------')
 3006 FORMAT(//,1X,'ELIMINATION DES ELEMENTS SECS DU MAILLAGE',
     *        /,1X,'-----------------------------------------',/)
 3007 FORMAT(//,1X,'MESH DRY ELEMENT SUPPRESSION',
     *        /,1X,'----------------------------',/)
 3008 FORMAT(/,1X,'AUCUNE ILE CONNECTEE')
 3009 FORMAT(/,1X,'NO CONNECTED ISLAND')
 3010 FORMAT(//,1X,'ELIMINATION DES DEPENDANCES ARRIERES',
     *        /,1X,'------------------------------------',/)
 3011 FORMAT(//,1X,'ELIMINATION OK BACKWARDS DEPENDENCIES',
     *        /,1X,'------------------------------------',/)
C
      DEALLOCATE(W)
      DEALLOCATE(WORK)
      DEALLOCATE(TRAV1)
      DEALLOCATE(TRAV2)
      DEALLOCATE(TRAV3)
C
C-----------------------------------------------------------------------
C
      RETURN 
      END
