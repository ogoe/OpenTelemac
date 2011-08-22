!                    **************
                     SUBROUTINE WAC
!                    **************
!
     &(PART, U_TEL, V_TEL, H_TEL, FX_WAC, FY_WAC, UV_WAC, VV_WAC,
     & CODE, T_TEL, DT_TEL,NIT_TEL,PERCOU_WAC)
!
!***********************************************************************
! TOMAWAC   V6P1                                   29/06/2011
!***********************************************************************
!
!brief    MAIN SUBROUTINE OF TOMAWAC
!+               SOLVES THE EQUATION FOR THE
!+               DIRECTIONAL WAVE SPECTRUM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CODE           |-->| CALLING PROGRAM (IF COUPLING)
!| DT_TEL         |-->| TELEMAC MODEL TIME STEP
!| FX_WAC         |<--| DRIVING FORCE ALONG X PASSED TO TELEMAC
!| FY_WAC         |<--| DRIVING FORCE ALONG Y PASSED TO TELEMAC
!| H_TEL          |-->| TELEMAC MODEL WATER DEPTH
!| NIT_TEL        |-->| NUMBER OF TELEMAC TIME STEPS
!| PART           |-->| -1: NO COUPLING
!|                |   |  0: COUPLING WITH TELEMAC (INITIALISATION) 
!|                |   |  1: COUPLING WITH TELEMAC (LOOP OVER TIME STEPS) 
!| PERCOU_WAC     |   | VARIABLE CURRENTLY NOT USED
!| T_TEL          |-->| COMPUTATION TIME OF TELEMAC MODEL
!| U_TEL          |-->| CURRENT VELOCITY ALONG X IN TELEMAC MODEL
!| V_TEL          |-->| CURRENT VELOCITY ALONG Y IN TELEMAC MODEL
!| UV_WAC         |<--| WIND VELOCITY ALONG X IN TOMAWAC MODEL
!| VV_WAC         |<--| WIND VELOCITY ALONG Y IN TOMAWAC MODEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_WAC => WAC
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!COUPLAGE Telemac-Tomawac : variables de la liste d'arguments en appel
      INTEGER,           INTENT(IN)      :: PART,NIT_TEL,PERCOU_WAC
      CHARACTER(LEN=24), INTENT(IN)      :: CODE
      TYPE(BIEF_OBJ),    INTENT(IN)      :: U_TEL,V_TEL,H_TEL
      TYPE(BIEF_OBJ),    INTENT(INOUT)   :: FX_WAC,FY_WAC
      TYPE(BIEF_OBJ),    INTENT(INOUT)   :: UV_WAC,VV_WAC
      DOUBLE PRECISION,  INTENT(IN)      :: DT_TEL,T_TEL
!Variables rajoutees pour le couplage et declarees localement
      DOUBLE PRECISION    :: DT_MIN, DT_MAX
      INTEGER          DUMMY, LT_WAC
!Fin COUPLAGE
!
!     VARIABLES DECLAREES LOCALEMENT DANS LA PROCEDURE.
!
      INTEGER NVARCL,LT,LT1,NRK,NPV,NPC,NPM
      INTEGER NOLEO(99)
      LOGICAL ISLEO(99)
      INTEGER DATE(3),TIME(3),NPTL,IP
!
!     TV1 TEMPS CORRESPONDANT AU VENT 1
!     TV2 TEMPS CORRESPONDANT AU VENT 2
!
      DOUBLE PRECISION LAMBD0,C,Z(1),DEUPI,DTSI
      DOUBLE PRECISION AT    ,TV1,TV2,TC1,TC2,TM1,TM2
      DOUBLE PRECISION VITVEN, VITMIN
      INTEGER  ADC , MDC , JDC , HDC, I1, I2, NVHMA,NVCOU
      INTEGER NBD
      LOGICAL IMPRES, DEBRES
!
      INTEGER, ALLOCATABLE :: QINDI(:)
!V6P1 Variable declaree localement pour les termes source
      INTEGER K
!Fin V6P1
      LOGICAL DEJA
      DATA DEJA/.FALSE./
!
!     SAVING LOCAL VARIABLES FROM ONE CALL TO THE OTHER
!     VERY IMPORTANT WITH CODE COUPLING
!
      SAVE
!
!     QINDI ALLOCATED ONLY AT FIRST CALL
!
      IF(.NOT.DEJA) THEN
        ALLOCATE(QINDI(NPLAN))
        DEJA=.TRUE.
      ENDIF
!
!=====C
!  1  C INITIALISATIONS DES VARIABLES LOCALES
!=====C======================================
!COUPLAGE : verification des conditions pour le couplage
!           TELEMAC-TOMAWAC
      IF(PART.GE.0) THEN
        IF(MAREE.OR.COUSTA.OR.DONTEL) THEN
           IF(LNG.EQ.1) THEN
             WRITE(LU,*) ''
             WRITE(LU,*) '***************************************'
             WRITE(LU,*) ' ATTENTION : COUPLAGE TELEMAC-TOMAWAC :'
             WRITE(LU,*) ' AUCUN FICHIER DE COURANT/MAREE NE DOIT'
             WRITE(LU,*) ' ETRE UTILISE EN ENTREE.               '
             WRITE(LU,*) '           ARRET DU PROGRAMME          '
             WRITE(LU,*) '***************************************'
           ELSE
             WRITE(LU,*) ''
             WRITE(LU,*) '***************************************'
             WRITE(LU,*) ' ATTENTION : COUPLING TELEMAC-TOMAWAC :'
             WRITE(LU,*) ' CURRENT/WATER LEVEL FILE CANNOT BE    '
             WRITE(LU,*) ' USED AS INPUT FILE.                   '
             WRITE(LU,*) '         END OF THE COMPUTATION        '
             WRITE(LU,*) '***************************************'
           ENDIF
           CALL PLANTE(1)
           STOP
        ENDIF
!
        DT_MAX=MAX(DT,DT_TEL)
        DT_MIN=MIN(DT,DT_TEL)
        IF(ABS(NINT(DT_MAX/DT_MIN)-DT_MAX/DT_MIN).GT.1.D-6) THEN
           IF(LNG.EQ.1) THEN
             WRITE(LU,*) ''
             WRITE(LU,*) '***************************************'
             WRITE(LU,*) ' ATTENTION : COUPLAGE TELEMAC-TOMAWAC :'
             WRITE(LU,*) ' LES DEUX PAS DE TEMPS UTILISES NE SONT'
             WRITE(LU,*) ' PAS UN MULTIPLE DE L''AUTRE.          '
             WRITE(LU,*) '           ARRET DU PROGRAMME          '
             WRITE(LU,*) '***************************************'
           ELSE
             WRITE(LU,*) ''
             WRITE(LU,*) '***************************************'
             WRITE(LU,*) ' ATTENTION : COUPLING TELEMAC-TOMAWAC :'
             WRITE(LU,*) ' THE CHOSEN TIME STEPS ARE NOT MULTIPLE'
             WRITE(LU,*) ' OF EACH OTHER.                        '
             WRITE(LU,*) '         END OF THE COMPUTATION        '
             WRITE(LU,*) '***************************************'
           ENDIF
           CALL PLANTE(1)
           STOP
        ENDIF
      ENDIF
!
!.....1.2 CONSTANTES PHYSIQUES.
!     """""""""""""""""""""""""
      GRAVIT=9.806D0
      DEUPI =2.D0*3.14159265358979D0
!
!.....1.3 PARAMETRES GENERAUX DU CALCUL.
!     """""""""""""""""""""""""""""""""
      NVARCL=0
      NPTL  =NPOIN3*NF
!
!.....1.4 INITIALISATION DES TABLEAUX DATE ET TIME
!     """"""""""""""""""""""""""""""""""""""""""""
      ADC=INT(DDC*1.D-8)
      MDC=INT(DDC*1.D-6)
      JDC=INT(DDC*1.D-4)
      HDC=INT(DDC*1.D-2)
      DATE(1)=ADC
      DATE(2)=MDC-100*ADC
      DATE(3)=JDC-100*MDC
      TIME(1)=HDC-100*JDC
      TIME(2)=INT(DDC-100.D0*HDC)
      TIME(3)=0
!
!=====C
!  2  C LECTURE DES CONDITIONS LIMITES ET INDICES DES POINTS FRONTIERES
!=====C================================================================
!
!COUPLAGE TELEMAC-TOMAWAC : initialisation
      IF(PART.LE.0) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECLIM'
      CALL LECLIM_TOMAWAC
     &(SLIFBR%I,SFBOR%R,NPTFR,NBOR,STDGEO,WAC_FILES(WACCLI)%LU,
!BD_INCKA modif //
!    *  MESH%ISEG%I,MESH%XSEG%R,MESH%YSEG%R,MESH%NACHB%I )
     &  MESH%ISEG%I,MESH%XSEG%R,MESH%YSEG%R,MESH%NACHB%I ,MESH,
     &  BOUNDARY_COLOUR%I)
      IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE LECLIM'
!BD_INCKA fin modif //
!
!-----------------------------------------------------------------------
!
! COMPLEMENT DE LA STRUCTURE DE DONNEES POUR BIEF
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INBIEF'
      CALL INBIEF(SLIFBR%I,KLOG,SITR31,SITR32,SITR33,
     &            LVMAC,IELM2,LAMBD0,SPHE,MESH,ST1,ST2,1,1,EQUA)
      IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE INBIEF'
!
!-----------------------------------------------------------------------
!
! LECTURE DE LA COTE DU FOND (ZF) SUR LE FICHIER DE GEOMETRIE
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FONSTR'
      CALL FONSTR (ST1,SZF,ST2,ST3,WAC_FILES(WACGEO)%LU,
     &             WAC_FILES(WACFON)%LU,WAC_FILES(WACFON)%NAME,MESH,
     &             1.D0,.TRUE.)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FONSTR'
!
! CORRECTION EVENTUELLE DES VALEURS DU FOND (OU CALCUL DU FOND SI CELA
! N'A PAS ETE FAIT DANS FONSTR)
! EN STANDARD, CORFON NE FAIT RIEN (ATTENTION, ALLER CHERCHER LE CORFON
! DE TOMAWAC).
! DANS LE CAS DE COUPLAGE AVEC TELEMAC, ON LIT LE FOND A PARTIR DU
! MODELE TELEMAC ET CORFON N EST PAS UTILISE
      IF(PART.LT.0)THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORFON'
        CALL CORFON
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORFON'
      ENDIF
!
!.....CALCUL DE LA PROFONDEUR D'EAU (TABLEAU DEPTH)
!
      DO IP=1,NPOIN2
        DEPTH(IP)=ZREPOS-ZF(IP)
        IF(DEPTH(IP).LT.PROMIN) DEPTH(IP)=0.9D0*PROMIN
      ENDDO
!
!-----------------------------------------------------------------------
!
! PREPARATION DES SORTIES GRAPHIQUES
!
        ! CREATION DU JEU DE DONNEES POUR UN FORMAT DE FICHIER
        ! FORMAT_RES.
        ! LE JEU DE DONNEES EST CREE DANS LE FICHIER NRES, ET EST
        ! DEFINIT PAR UN TITRE ET LES VARIABLES A ECRIRE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CREATE_DATASET'
        CALL CREATE_DATASET(WAC_FILES(WACRES)%FMT, ! FORMAT FICHIER RESULTAT
     &                      WAC_FILES(WACRES)%LU,  ! LU FICHIER RESULTAT
     &                      TITCAS,     ! TITRE DE L'ETUDE
     &                      MAXVAR,     ! MAX VARIABLES SORTIE
     &                      TEXTE,      ! NOMS VARIABLES SORTIE
     &                      SORLEO)     ! SORTIE OU PAS DES VARIABLES
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CREATE_DATASET'
        ! ECRITURE DU MAILLAGE DANS LE FICHIER SORTIE :
        ! SI ON EST ON PARALLEL, FAUT L'INDIQUER VIA NCSIZE ET NPTIR.
        ! LES AUTRES INFORMATIONS SONT DANS MESH.
        ! EN PLUS : DATE/TEMPS DE DEPART ET LES COORDONNEES DE
        ! L'ORIGINE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WRITE_MESH'
        CALL WRITE_MESH(WAC_FILES(WACRES)%FMT, ! FORMAT FICHIER RESULTAT
     &                  WAC_FILES(WACRES)%LU,  ! LU FICHIER RESULTAT
     &                  MESH,          ! DESCRIPTEUR MAILLAGE
     &                  1,             ! NOMBRE DE PLAN /NA/
     &                  DATE,          ! DATE DEBUT
     &                  TIME,          ! HEURE DEBUT
     &                  I_ORIG,J_ORIG) ! COORDONNEES DE L'ORIGINE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WRITE_MESH'
!
!-----------------------------------------------------------------------
!
! CONDITIONS INITIALES
!
!
!
!=====C INITIALISATION DES VECTEURS DE DISCRETISATION, DU COURANT,
!  2  C DU VENT ET DU SPECTRE DE VARIANCE.
!=====C===========================================================
      LT=0
      DTSI=DT/NSITS
!
      IF (SUIT) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECSUI'
!COUPLAGE TELEMAC-TOMAWAC : modification de la liste
!       d'arguments d'appel pour prise en compte du courant
        CALL LECSUI
     &( SF%R    , NPLAN   , NF      , STETA%R, SFR%R  ,
     &  NELEM2  , NPOIN2  , AT      , SUC%R  , SVC%R  ,
     &  SUC1%R  , SVC1%R  , SUC2%R  , SVC2%R , SUV%R  ,
     &  SVV%R   , SUV1%R  , SVV1%R  , SUV2%R , SVV2%R ,
!     *  VENT    , TV1     , TV2     , COUSTA ,
     &  VENT    , TV1     , TV2     , COUSTA.OR.PART.EQ.0 ,
     &  WAC_FILES(WACPRE)%LU ,
     &  BINPRE  , SDEPTH%R, TC1 , TC2 , ZM1 , ZM2 ,
!     *  SDZHDT%R, TM1     , TM2     , MAREE )
     &  SDZHDT%R, TM1     , TM2     , MAREE.OR.PART.EQ.0 )
!Fin COUPLAGE
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LECSUI'
      ELSE
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CONDIW'
! COUPLAGE TELEMAC-TOMAWAC : si PART=0, courants et hauteur d'eau
!  passes par TELEMAC
        CALL CONDIW
     &( AT, LT , DEUPI , TC1 , TC2 , NPC, TV1, TV2, NPV, TM1, TM2, NPM,
     &  NVHMA  , NVCOU , PART , U_TEL, V_TEL, H_TEL )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CONDIW'
!
        IF(PART.EQ.0) THEN
          DO IP=1,NPOIN2
            SDZHDT%R(IP)=0.D0
!           IF(DEPTH(IP).LT.PROMIN) DEPTH(IP)=0.9D0*PROMIN
            IF(DEPTH(IP).LT.PROMIN) DEPTH(IP)=PROMIN
          ENDDO
        ENDIF
!Fin COUPLAGE
      ENDIF
!
!
!
!=====C
!  3  C UTILISATION (EVENTUELLE) DE LA VARIABLE TELEMAC.
!=====C=================================================
!COUPLAGE
!      IF (DONTEL) THEN
      IF (DONTEL.AND.PART.LT.0) THEN
!Fin COUPLAGE
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VARTEL'
        CALL VARTEL
     &( STRA31%R, MESH%X%R, MESH%Y%R, SDEPTH%R,
     &  SUC%R   , SVC%R   , ZREPOS     , STRA32%R,
     &  SF%R    , NPLAN      , NF         , NPOIN2     )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VARTEL'
      ENDIF
!
      DO IP=1,NPOIN2
         IF(DEPTH(IP).LE.0.D0) THEN
           IF(LNG.EQ.1) THEN
             WRITE(LU,*) ''
             WRITE(LU,*) '*************************'
             WRITE(LU,*) ' ! PROFONDEUR NEGATIVE ! '
             WRITE(LU,*) '   ARRET DU PROGRAMME    '
             WRITE(LU,*) '*************************'
             CALL PLANTE(0)
           ELSE
             WRITE(LU,*) ''
             WRITE(LU,*) '**************************'
             WRITE(LU,*) ' ! NEGATIVE WATER DEPTH ! '
             WRITE(LU,*) '   END OF THE COMPUTATION '
             WRITE(LU,*) '**************************'
             CALL PLANTE(0)
           ENDIF
         ENDIF
      ENDDO
!
!
!
!=====C
!  4  C CALCULS PREPARATOIRES POUR INTERACTIONS NON-LINEAIRES.
!=====C=======================================================
!.....DIA method (Hasselmann et al., 1985)
      IF(STRIF.EQ.1) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRENL1'
        CALL PRENL1( IANGNL, COEFNL, NPLAN , NF , RAISF , XLAMD )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRENL1'
!.....MDIA method (Tolman, 2004)
      ELSEIF (STRIF.EQ.2) THEN
!.....Setting parametres for MDIA
        XLAMDI(1)=0.075D0
        XMUMDI(1)=0.023D0
        XLAMDI(2)=0.219D0
        XMUMDI(2)=0.127D0
        XLAMDI(3)=0.299D0
        XMUMDI(3)=0.184D0
        XLAMDI(4)=0.394D0
        XMUMDI(4)=0.135D0
!        
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRENL2'
        DO K=1,MDIA
          CALL PRENL2
     *( IANMDI(1,1,K) , COEMDI(1,K), NPLAN , NF , RAISF ,
     *  XLAMDI(K), XMUMDI(K))
        ENDDO
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRENL2'
!        
!.....GQM method (Lavrenov, 2001)   
      ELSEIF(STRIF.EQ.3) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRENL3'
        CALL PRENL3
     *( NF    , NPLAN , RAISF , TAILF , FREQ  , TB_SCA, LBUF  , DIMBUF,
     *  F_POIN, F_COEF, T_POIN, F_PROJ, IQ_OM1, NQ_TE1, NQ_OM2, NF1   ,
     *  NT1   , K_IF1 , K_IF2 , K_IF3 , TB_V14, TB_V24, TB_V34, K_1P  , 
     *  K_1M  , K_1P2P, K_1P3M, K_1P2M, K_1P3P, K_1M2P, K_1M3M, K_1M2M,
     *  K_1M3P, TB_TPM, TB_TMP, TB_FAC, SEUIL1, SEUIL2, ELIM  , NCONF ,
     *  NCONFM, IDCONF)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRENL3'
      ENDIF       
!
      IF (STRIA.EQ.2) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREQT2'
        CALL PREQT2(STETA%R,NPLAN,BDISPB,BDSSPB,NBD,QINDI)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREQT2'
      ENDIF
!
!=====C INITIALISATION DE LA CONTRAINTE DE HOULE, PUIS CALCUL DES
!  5  C VITESSE DE FROTTEMENT U*, RUGOSITE Z0 ET DIRECTION INITIALES.
!=====C==============================================================
!
!.....5.1 INITIALISATION DE LA CONTRAINTE DE HOULE INITIALE.
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""
      CALL OV ( 'X=C     ' ,STRA41%R, STRA32%R , STRA33%R,
     &                      0.D0 , NPOIN2 )
!
!.....5.2 CALCUL DE U* ET Z0 SELON LA METHODE CONSIDEREE.
!     """""""""""""""""""""""""""""""""""""""""""""""""""
      IF (VENT) THEN
        IF (SVENT.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR1'
          CALL USTAR1
     &( STRA42%R , STRA44%R , STRA41%R , SUV%R , SVV%R ,
     &  CDRAG    , ALPHA    , XKAPPA   , ZVENT , GRAVIT,
     &  NPOIN2   )
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR1'
!V6P1 termes source
        ELSEIF (SVENT.GE.2) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR2'
          CALL USTAR2(STRA42%R,SUV%R,SVV%R,NPOIN2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR2'
        ELSEIF ((SVENT.EQ.0).AND.(LVENT.EQ.1)) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR2'
          CALL USTAR2(STRA42%R,SUV%R,SVV%R,NPOIN2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR2' 
        ELSEIF ((SVENT.EQ.0).AND.(LVENT.EQ.0).AND.(SMOUT.EQ.2)) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR2'
          CALL USTAR2(STRA42%R,SUV%R,SVV%R,NPOIN2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR2'    
!V6P1 Fin
        ELSE
          IF (LNG.EQ.1) THEN
            WRITE(LU,*)
     &      'PB DANS WAC : VENT PRESENT, MAIS SVENT NON CORRECT'
          ELSE
            WRITE(LU,*)
     &      'PB IN WAC : WIND PRESENT, BUT SVENT NOT CORRECT'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!.....5.3 CALCUL DE LA DIRECTION DU VENT
!     """"""""""""""""""""""""""""""""""
      VITMIN=1.D-3
      IF (VENT) THEN
        DO IP=1,NPOIN2
          VITVEN=SQRT(SUV%R(IP)**2+SVV%R(IP)**2)
          IF (VITVEN.GT.VITMIN) THEN
            STRA43%R(IP)=ATAN2(SUV%R(IP),SVV%R(IP))
          ELSE
            STRA43%R(IP)=0.D0
          ENDIF
        ENDDO
      ENDIF
!
!
!
!=====C
!  6  C INITIALISATION DE CERTAINS TABLEAUX UTILES.
!=====C============================================
!COUPLAGE TELEMAC-TOMAWAC si PART=0
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INITAB'
      CALL INITAB( SIBOR%I, MESH%IFABOR%I, NELEM2, PART)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INITAB'
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE IMPR'
      CALL IMPR(LISPRD,LT,AT,LT,3)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE IMPR'
!
!
!=====C
!  7  C AFFECTATION DES CONDITIONS AUX LIMITES A L'INSTANT INITIAL.
!=====C============================================================
      I1=NF+1
      I2=NF+NPLAN
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LIMWAC'
      CALL LIMWAC
     &(SF%R    , SFBOR%R , SLIFBR%I , NPTFR  , NPLAN , NF    ,
     & STETA%R , SFR%R   , NPOIN2   , NBOR   , AT    , LT    ,
     & DDC     , LIMSPE  , FPMAXL   , FETCHL , SIGMAL, SIGMBL,
     & GAMMAL  , FPICL   , HM0L     , APHILL , TETA1L, SPRE1L,
     & TETA2L  , SPRE2L  , XLAMDL   , MESH%X%R   ,
     & MESH%Y%R, KENT    , KSORT    , WAC_FILES(WACFO1)%LU  ,
     & WAC_FILES(WACBI1)%LU         , BINBI1 ,
     & SUV%R   , SVV%R   , SPEULI   , VENT  , VENSTA, GRAVIT ,
     & DEUPI   , SPRIVE%R, NPRIV    , STRA31%R , STRA32%R,
     & DEPTH   , FRABL   , BOUNDARY_COLOUR%I)
       IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LIMWAC'
!
!
!
!=====C CALCUL DES NOMBRES D'ONDE (XK), DE LA VITESSE DE GROUPE (CG) ET
!  8  C DU FACTEUR DE PASSAGE (B) EN SPECTRE DE VARIANCE EN (FR,TETA).
!=====C=================================================================
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INIPHY'
      CALL INIPHY
     &( SXK%R   , SCG%R , SB%R , SDEPTH%R , SFR%R ,
     &  SCOSF%R , NPOIN2   , NF      , PROINF      , SPHE     )
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INIPHY'
!
!=====C
!  8b C MISE A ZERO DU SPECTRE SUR LES POINTS OU PROF < PROMIN
!=====C=======================================================
!
      IF(.NOT.PROINF) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRETE'
        CALL ECRETE(SF%R,SDEPTH%R,NPOIN2,NPLAN,NF,PROMIN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRETE'
      ENDIF
!
!
!=====C
!  9  C SORTIES GRAPHIQUES (EVENTUELLES) A L'ETAT INITIAL.
!=====C===================================================
!
!.....9.1 CHOIX DES POINTS DE SORTIE DU SPECTRE DIRECTIONNEL.
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRELEO_MPI'
      CALL PRELEO_MPI
     &(XLEO,YLEO,NPLEO,MESH%X%R,MESH%Y%R,MESH%IKLE%I,MESH%SURDET%R,
     & NPOIN2,NELEM2,NOLEO,ISLEO)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRELEO_MPI'
!
!.....9.2 TEST POUR SAVOIR SI ON IMPRIME OU PAS.
!     """"""""""""""""""""""""""""""""""""""""""
      IMPRES=.FALSE.
      DEBRES=.FALSE.
      IF (LT.EQ.GRADEB) THEN
        IMPRES=.TRUE.
        DEBRES=.TRUE.
      ENDIF
!
      IF (IMPRES) THEN
!
!.....9.3 IMPRESSION (EVENTUELLE) DES VARIABLES SUR LE MAILLAGE 2D.
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DUMP2D'
        CALL DUMP2D(LT,DEUPI,SF%R,NPTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DUMP2D'
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_DESIMP'
        CALL BIEF_DESIMP(WAC_FILES(WACRES)%FMT,VARSOR,
     &            HIST,0,NPOIN2,WAC_FILES(WACRES)%LU,'STD',AT,
     &            LT,GRAPRD,GRAPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,GRADEB,GRADEB)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_DESIMP'
!
!.....9.4 IMPRESSION (EVENTUELLE) DES SPECTRES DIRECTIONNELS.
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRSPE'
        CALL ECRSPE
     &( SF%R    , STRA01%R , STETA%R, NPLAN ,
     &  SFR%R   , NF  , NF   , NPOIN2      , AT ,LT ,
     &  STRA01%R, SITR01%I, NOLEO , NPLEO , WAC_FILES(WACLEO)%LU ,
     &  WAC_FILES(WACLEO)%FMT, DEBRES , TITCAS , DATE , TIME ,
     &  ISLEO , MESH%KNOLG%I )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRSPE'
!
      ENDIF
!
!
!=====C
!  10 C PREPARATION DE LA PROPAGATION (REMONTEE DES CARACTERISTIQUES).
!=====C===============================================================
      IF (PROP) THEN
        CALL IMPR(LISPRD,LT,AT,LT,1)
        CALL IMPR(LISPRD,LT,AT,LT,2)
        NRK=3
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREPRO 1'
!COUPLAGE TELEMAC-TOMAWAC : modification de la liste
!       d'arguments d'appel
        CALL PREPRO
     & ( SCX%R    , SCY%R     , SCT%R    , SCF%R   , DT    ,
     &   NRK      , MESH%X%R  , MESH%Y%R , STETA%R ,
     &   SCOSTE%R , SSINTE%R  , SFR%R    , MESH%IKLE%I     ,
     &   SIBOR%I  , SETAP1%I  , STRA01%R , SSHP1%R ,
     &   SSHP2%R  , SSHP3%R   , SSHZ%R   , SSHF%R  ,
     &   SELT%I   , SETA%I    , SFRE%I   , SDEPTH%R,
     &   SDZHDT%R , SDZX%R    , SDZY%R   , SUC%R   ,
     &   SVC%R    , SDUX%R    , SDUY%R   , SDVX%R  ,
     &   SDVY%R   , SXK%R     , SCG%R    , SCOSF%R ,
     &   STGF%R   , SITR01%I  , NPOIN3   , NPOIN2  , NELEM2,
!     *   NPLAN    , NF        , MESH%SURDET%R, COURAN,
     &   NPLAN    , NF    , MESH%SURDET%R, COURAN.OR.PART.EQ.0,
     &   SPHE     , PROINF   , PROMIN,MESH)
!Fin COUPLAGE
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREPRO 1'
!
      ENDIF
!
!COUPLAGE : end cycle IF(PART.LE.0) pour couplage avec TELEMAC
      ENDIF
!
!=====C
!  11 C BOUCLE EN TEMPS PRINCIPALE.
!=====C============================
!
!COUPLAGE TELEMAC-TOMAWAC
      IF(PART.NE.0) THEN
!
!Preparation au couplage : on calcule le nombre de cycle
! de la boucle en temps, selon la duree du pas de temps dans
! Telemac (DT_TEL) et dans Tomawac (DT).
! On assigne a DUMMY la valeur originaire du nombre de pas
! de temps specifie dans le fichier .cas de Tomawac.
      IF(PART.EQ.1) THEN
        AT=T_TEL
        DUMMY=NIT
        IF(DT.GE.DT_TEL) NIT=1
        IF(DT.LT.DT_TEL) NIT=NINT(DT_TEL/DT)
      ENDIF
!Fin COUPLAGE
!
!COUPLAGE TELEMAC-TOMAWAC : LT est defini comme le numero de pas
!         de temps effectifs de TOMAWAC. La variable LT_WAC
!         compte les pas de temps de chaque boucle, meme
!         quand TOMAWAC est appele par TELEMAC
!      DO 10 LT=1,NIT
      DO 10 LT_WAC=1,NIT
!
!.....11.1 AFFECTATION DE LA DATE DE FIN DU PAS DE TEMPS COURANT.
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      AT=AT+DT
!Calcul de LT
      LT=NINT(AT/DT)
!Fin COUPLAGE
!
      CALL IMPR(LISPRD,LT,AT,LT,3)
!
!.....11.2 AFFECTATION DES CONDITIONS AUX LIMITES.
!     """"""""""""""""""""""""""""""""""""""""""""
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LIMWAC'
      CALL LIMWAC
     &(SF%R    , SFBOR%R , SLIFBR%I , NPTFR  , NPLAN , NF    ,
     & STETA%R , SFR%R   , NPOIN2      , NBOR   , AT    , LT    ,
     & DDC     , LIMSPE  , FPMAXL      , FETCHL , SIGMAL, SIGMBL,
     & GAMMAL     , FPICL      , HM0L  , APHILL , TETA1L, SPRE1L,
     & TETA2L     , SPRE2L     , XLAMDL, MESH%X%R   ,
     & MESH%Y%R   , KENT       , KSORT , WAC_FILES(WACFO1)%LU ,
     & WAC_FILES(WACBI1)%LU    , BINBI1,
     & SUV%R   , SVV%R   , SPEULI      , VENT  , VENSTA, GRAVIT ,
     & DEUPI      , SPRIVE%R, NPRIV      , STRA31%R , STRA32%R,
     & DEPTH      , FRABL   ,BOUNDARY_COLOUR%I)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LIMWAC'
!
!.....11.2b MISE A ZERO DU SPECTRE SUR LES POINTS OU PROF < PROMIN
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF (.NOT.PROINF) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRETE'
        CALL ECRETE( SF%R    , SDEPTH%R, NPOIN2, NPLAN , NF , PROMIN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRETE'
      ENDIF
!
      IF (MAREE) THEN
       LT1=MAX((LT/LAM)*LAM,2)
       IF (LT.EQ.LT1) THEN
        DO IP=1,NPOIN2
          DEPTH(IP)=ZREPOS-ZF(IP)
        ENDDO
       ENDIF
      ENDIF
!
!......11.3 A JOUR DE LA BATHY ET DES COURANTS
!      """"""""""""""""""""""""""""""""""""""""
!COUPLAGE TELEMAC-TOMAWAC : en cas de couplage, actualisation
!         des courants et de l'hauteur d'eau (et de leurs gradients)
!         tous les DT_TEL
      IF((MAREE.AND.LT.EQ.LT1).OR.
     &   (PART.EQ.1.AND.LT_WAC.EQ.1)) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORMAR'       
        CALL CORMAR
     &( AT    , LT    , TC1   , TC2   , TV1   , TV2   , TM1   , TM2   ,
     &  NPC   , NPM   , NVHMA , NVCOU , PART, U_TEL, V_TEL , H_TEL  )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORMAR'
        DO IP=1,NPOIN2
          IF (DEPTH(IP).LT.PROMIN) DEPTH(IP)=0.9D0*PROMIN
        ENDDO
!
!......11.3.1 PREPARATION DE LA PROPAGATION (REMONTEE DES CARACTERISTIQUES).
!      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INIPHY'
        CALL INIPHY
     & ( SXK%R  , SCG%R , SB%R , SDEPTH%R , SFR%R  ,
     &   SCOSF%R, NPOIN2   , NF      , PROINF      , SPHE      )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INIPHY'
!
        IF (PROP) THEN
         CALL IMPR(LISPRD,LT,AT,LT,1)
         CALL IMPR(LISPRD,LT,AT,LT,2)
         NRK=3
         IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREPRO 2'
!COUPLAGE TELEMAC-TOMAWAC : modification de la liste
!       d'arguments d'appel
         CALL PREPRO
     & ( SCX%R    , SCY%R     , SCT%R    , SCF%R   , DT    ,
     &   NRK      , MESH%X%R  , MESH%Y%R , STETA%R ,
     &   SCOSTE%R , SSINTE%R  , SFR%R    , MESH%IKLE%I     ,
     &   SIBOR%I  , SETAP1%I  , STRA01%R , SSHP1%R ,
     &   SSHP2%R  , SSHP3%R   , SSHZ%R   , SSHF%R  ,
     &   SELT%I   , SETA%I    , SFRE%I   , SDEPTH%R,
     &   SDZHDT%R , SDZX%R    , SDZY%R   , SUC%R   ,
     &   SVC%R    , SDUX%R    , SDUY%R   , SDVX%R  ,
     &   SDVY%R   , SXK%R     , SCG%R    , SCOSF%R ,
     &   STGF%R   , SITR01%I  , NPOIN3   , NPOIN2  , NELEM2,
!     &   NPLAN    , NF        , MESH%SURDET%R, COURAN,
     &   NPLAN    , NF    , MESH%SURDET%R, COURAN.OR.PART.EQ.1,
     &   SPHE     , PROINF    , PROMIN   , MESH)
!Fin COUPLAGE
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREPRO 2'
        ENDIF
      ENDIF
!Fin cycle IF((MAREE.AND.LT.EQ.LT1).OR.(PART.EQ.1.AND.LT_WAC.EQ.1))
!
!.....11.3 PROPAGATION (INTERPOLATION AU PIED DES CARACTERISTIQUES).
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF (PROP) THEN
        CALL IMPR(LISPRD,LT,AT,LT,5)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROPA'
!COUPLAGE TELEMAC-TOMAWAC : modification de la liste
!       d'arguments d'appel
        CALL PROPA
     &( SF%R       , SB%R    , SSHP1%R, SSHP2%R, SSHP3%R,
     &  SSHZ%R     , SSHF%R  , SELT%I , SETA%I , SFRE%I ,
     &  MESH%IKLE%I, SETAP1%I, NPOIN3    , NPOIN2    , NELEM2,
!     *  NPLAN         , NF   , COURAN    ,STRA01%R   , STRA02%R )
     &  NPLAN , NF , COURAN.OR.PART.EQ.1 ,STRA01%R   , STRA02%R )
!Fin COUPLAGE
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROPA'
       ENDIF
!
!.....11.4 INTEGRATION DES TERMES SOURCES.
!     """"""""""""""""""""""""""""""""""""
      IF (TSOU) THEN
        CALL IMPR(LISPRD,LT,AT,NSITS,4)
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SEMIMP'
        CALL SEMIMP(SF%R, SXK%R, SFR%R, SDFR%R, SDEPTH%R, SUV%R, SVV%R ,
     &  MESH%X%R, MESH%Y%R, WAC_FILES(WACVEB)%LU, WAC_FILES(WACVEF)%LU ,
     &  NBOR, NPTFR, DDC,TV1,TV2,NPV, SXRELV%R, SYRELV%R, SUV1%R,SVV1%R,
     &  SUV2%R,SVV2%R,STETA%R,SSINTE%R,SCOSTE%R,INDIV,TAILF,RAISF      ,
     &  GRAVIT,CFROT1,CMOUT1,CMOUT2,CMOUT3,CMOUT4,CMOUT5,CMOUT6,AT,DTSI,
     &  ROAIR,ROEAU,XKAPPA,BETAM,DECAL,CDRAG,ALPHA,ZVENT,NF,NPLAN      ,
     &  NPOIN2,IANGNL,COEFNL,F1,NSITS,SMOUT,SFROT,SVENT,LVENT,STRIF    ,
     &  VENT,VENSTA,VX_CTE,VY_CTE,SBREK,ALFABJ,GAMBJ1,GAMBJ2,IQBBJ     ,
     &  IHMBJ,IFRBJ,BORETG,GAMATG,IWHTG,IFRTG,ALFARO,GAMARO,GAM2RO     ,
     &  IDISRO,IEXPRO,IFRRO,BETAIH,EM2SIH,IFRIH,COEFHS,XDTBRK,NDTBRK   ,
     &  STRIA,ALFLTA,RFMLTA,KSPB,BDISPB,BDSSPB,PROINF,DF_LIM,LIMIT     , 
     &  CIMPLI,COEFWD,COEFWE,COEFWF,COEFWH,WAC_FILES(WACVEB)%NAME      ,
     &  WAC_FILES(WACVEF)%NAME,BINVEN,NBD,QINDI,STRA41%R,STRA42%R      ,
     &  STRA43%R,STRA44%R,STSTOT%R,STSDER%R,STOLD%R,STNEW%R,STRA31%R   ,
     &  STRA32%R,STRA33%R,STRA34%R,STRA35%R,STRA36%R,STRA37%R,STRA38%R ,
     &  STRA39%R,ST1%R,ST2%R,ST3%R,ST4%R,STRA01%R,SBETA%R,NQ_TE1,NQ_OM2,
     &  NF1,NF2,NT1,NCONF,NCONFM,SEUIL,LBUF,DIMBUF,F_POIN, T_POIN,
     &  F_COEF,F_PROJ,TB_SCA,K_IF1,K_1P,K_1M,K_IF2,K_IF3,K_1P2P,K_1P2M,
     &  K_1P3P,K_1P3M,K_1M2P,K_1M2M,K_1M3P,K_1M3M,IDCONF,TB_V14,TB_V24,
     &  TB_V34, TB_TPM, TB_TMP ,TB_FAC, MDIA  , IANMDI, COEMDI)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SEMIMP'
      ENDIF
!
!.....11.5 PASSAGE (EVENTUEL) EN FREQUENCE ABSOLUE.
!     """""""""""""""""""""""""""""""""""""""""""""
!COUPLAGE TELEMAC-TOMAWAC : prise en compte du courant
!      IF (COURAN) THEN
      IF (COURAN.OR.PART.EQ.1) THEN
!Fin COUPLAGE
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TRANSF'
        CALL TRANSF
     &( STRA02%R      , SF%R  , SFR%R , SDFR%R, SCOSTE%R      ,
     &  SSINTE%R      , SUC%R , SVC%R , SXK%R , SITR11%I      ,
     &  SITR12%I      , SITR13%I      , STRA31%R      , STRA32%R      ,
     &  NPOIN2, NPLAN , NF    , RAISF , LT    , GRADEB, GRAPRD)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TRANSF'
      ELSE
        CALL OV ( 'X=Y     ' ,STRA02%R, SF%R , Z , C , NPOIN3*NF )
      ENDIF
!
!.....11.6 TEST POUR SAVOIR SI ON IMPRIME OU PAS.
!     """""""""""""""""""""""""""""""""""""""""""
      IMPRES=.FALSE.
      DEBRES=.FALSE.
      IF ((LT.GE.GRADEB).AND.(MOD(LT-GRADEB,GRAPRD).EQ.0)) IMPRES=.TRUE.
      IF (LT.EQ.GRADEB) DEBRES=.TRUE.
!
      IF (IMPRES) THEN
!
!.....11.7 IMPRESSION (EVENTUELLE) DES VARIABLES SUR LE MAILLAGE 2D.
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
         IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DUMP2D'
         CALL DUMP2D(LT, DEUPI, STRA02%R, NPTL )
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DUMP2D'
!
         IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_DESIMP'
         CALL BIEF_DESIMP(WAC_FILES(WACRES)%FMT,VARSOR,
     &            HIST,0,NPOIN2,WAC_FILES(WACRES)%LU,'STD',AT,
     &            LT,GRAPRD,GRAPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,GRADEB,GRADEB)
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_DESIMP'
!
!.....11.8 IMPRESSION (EVENTUELLE) DES SPECTRES DIRECTIONNELS.
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""
         IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRSPE'
         CALL ECRSPE
     &( STRA02%R, STRA01%R , STETA%R, NPLAN ,
     &  SFR%R   , NF  , NF   , NPOIN2      , AT ,LT ,
     &  STRA01%R, SITR01%I, NOLEO  , NPLEO, WAC_FILES(WACLEO)%LU ,
     &  WAC_FILES(WACLEO)%FMT , DEBRES, TITCAS, DATE  , TIME ,
     &  ISLEO , MESH%KNOLG%I )
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRSPE'
!
      ENDIF
!
!COUPLAGE TELEMAC-TOMAWAC : passage a TELEMAC des valeurs
!         de force FX, FY et des vents (si presents)
      IF(PART.EQ.1.AND.LT_WAC.EQ.NIT) THEN
        CALL OV('X=Y     ',FX_WAC%R,STRA51%R,STRA51%R,0.D0,NPOIN2)
        CALL OV('X=Y     ',FY_WAC%R,STRA52%R,STRA52%R,0.D0,NPOIN2)
        IF(VENT) THEN
          CALL OV('X=Y     ',UV_WAC%R,SUV%R,SUV%R,0.D0,NPOIN2)
          CALL OV('X=Y     ',VV_WAC%R,SVV%R,SVV%R,0.D0,NPOIN2)
        ENDIF
      ENDIF
!Fin COUPLAGE
!
   10 CONTINUE
!
!Fin COUPLAGE : fin du cycle IF(PART.NE.0)
      ENDIF
!
!=====C
!  12 C IMPRESSIONS GLOBALES (EVENTUELLES) EN FIN DE CALCUL.
!=====C=====================================================
!
!COUPLAGE TELEMAC-TOMAWAC : reset de la variable NIT (nombre
!         de pas de temps de TOMAWAC indique dans la fichier .cas)
      IF(PART.EQ.1) NIT=DUMMY
!Fin COUPLAGE
!
!COUPLAGE TELEMAC-TOMAWAC : pour impressions globales et
!     validation il faut verifier qu'on est bien a la fin de la
!     simulation TOMAWAC : on fait un cycle IF qui sera ferme
!     apres l'appel de BIEF_VALIDA.
      IF(ABS(AT-NIT*DT).LT.1.D-6) THEN
!Fin COUPLAGE
      IF (GLOB)  THEN
        CALL IMPR(LISPRD,NIT,AT,NIT,6)
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOR3D'
!COUPLAGE TELEMAC-TOMAWAC : prise en compte du courant et
!       de la hauteur d'eau dans la liste des arguments
!       d'appel
        CALL SOR3D
     &( SF%R  , NPLAN  , NF       , STETA%R   , SFR%R ,
     &  NELEM2, NPOIN2 , AT       , SUC%R     , SVC%R ,
!     *  SUV%R , SVV%R  , SDEPTH%R , VENT      , COURAN   ,
!     *  MAREE , TITCAS , WAC_FILES(WACRBI)%LU , BINRBI    )
     &  SUV%R , SVV%R  , SDEPTH%R , VENT     , COURAN.OR.PART.EQ.1,
     &  MAREE.OR.PART.EQ.1 , TITCAS , WAC_FILES(WACRBI)%LU ,
     &  BINRBI    )
!Fin COUPLAGE
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOR3D'
      ENDIF
!
!----------------------------------------------------------------------
!
!  VALIDATION DES RESULTATS SUR LE FICHIER DE REFERENCES
!
      IF(VALID) CALL BIEF_VALIDA(BST1,TEXTE,
     &                   WAC_FILES(WACREF)%LU,WAC_FILES(WACREF)%FMT,
     &                   VARSOR,TEXTE,
     &                   WAC_FILES(WACRES)%LU,WAC_FILES(WACRES)%FMT,
     &                   MAXVAR,NPOIN2,NIT,NIT,ALIRE)
!
!COUPLAGE TELEMAC-TOMAWAC : fin du cycle IF(ABS(AT-NIT*DT).LT.1.D-6)
      ENDIF
!Fin COUPLAGE
!----------------------------------------------------------------------
!
      RETURN
      END
