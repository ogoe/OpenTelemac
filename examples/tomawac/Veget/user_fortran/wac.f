!                    **************
                     SUBROUTINE WAC
!                    **************
!
     &(PART, U_TEL, V_TEL, H_TEL, FX_WAC, FY_WAC, UV_WAC, VV_WAC,
     & CODE, T_TEL, DT_TEL,NIT_TEL,PERCOU_WAC,
     & DIRMOY_TEL,HM0_TEL,TPR5_TEL)
!
!***********************************************************************
! TOMAWAC   V6P3                                   25/06/2012
!***********************************************************************
!
!brief    MAIN SUBROUTINE OF TOMAWAC
!+               SOLVES THE EQUATION FOR THE
!+               DIRECTIONAL WAVE SPECTRUM
!
!history  J-M HERVOUET (EDF - LNHE)
!+        29/01/2013
!+        V6P3
!+   Radiation stresses for Telemac now computed independently of the
!+   printouts on results file.
!+   Call to tomawac_constants moved to lecdon_tomawac.
!
!history  J-M HERVOUET (EDF - LNHE)
!+        22/03/2013
!+        V6P3
!+   New arguments DIRMOY_TEL,HM0_TEL, TPR5_TEL for transmission to
!+   Sisyphe through Telemac-2D or 3D. Values computed in case of triple
!+   coupling.
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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(IN)      :: PART,NIT_TEL,PERCOU_WAC
      CHARACTER(LEN=24), INTENT(IN)      :: CODE
      TYPE(BIEF_OBJ),    INTENT(IN)      :: U_TEL,V_TEL,H_TEL
      TYPE(BIEF_OBJ),    INTENT(INOUT)   :: DIRMOY_TEL,HM0_TEL,TPR5_TEL
      TYPE(BIEF_OBJ),    INTENT(INOUT)   :: FX_WAC,FY_WAC
      TYPE(BIEF_OBJ),    INTENT(INOUT)   :: UV_WAC,VV_WAC
      DOUBLE PRECISION,  INTENT(IN)      :: DT_TEL,T_TEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION DT_MIN,DT_MAX
      INTEGER          DUMMY,LT_WAC
!
      INTEGER LT,LT1
      INTEGER NOLEO(99)
      INTEGER DATE(3),TIME(3),IP
!
!     TV1 TEMPS CORRESPONDANT AU VENT 1
!     TV2 TEMPS CORRESPONDANT AU VENT 2
!
      DOUBLE PRECISION LAMBD0,DTSI,AT,AT0,TV1,TV2,TC1,TC2,TM1,TM2
      DOUBLE PRECISION VITVEN,VITMIN
      INTEGER ADC,MDC,JDC,HDC,NVHMA,NVCOU,NVWIN,NBD,K,IPLAN,IFREQ
      LOGICAL IMPRES, DEBRES
!
      INTEGER, ALLOCATABLE :: QINDI(:)
!
      LOGICAL DEJA
      DATA DEJA/.FALSE./
!
!     SAVING LOCAL VARIABLES FROM ONE CALL TO THE OTHER
!     VERY IMPORTANT WITH CODE COUPLING
!
      SAVE
!
!-----------------------------------------------------------------------
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
!
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
!.....1.4 INITIALISATION DES TABLEAUX DATE ET TIME
!
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
!
      IF(PART.LE.0) THEN
!
!-----------------------------------------------------------------------
!
!     MESH ORGANISATION - 2D LEVEL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECLIM POUR MESH2D'
      CALL LECLIM(SLIFBR%I,SITR31%I,SITR31%I,SITR31%I,SFBOR%R,
     &            STSDER%R,STSDER%R,STSDER%R,STSDER%R,STSDER%R,STSDER%R,
     &            NPTFR,3,.FALSE.,WAC_FILES(WACCLI)%LU,
     &            KENT,KENTU,KSORT,KADH,KLOG,KINC,SITR31%I,
     &            MESH,BOUNDARY_COLOUR%I)
      IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE LECLIM'
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INBIEF POUR MESH2D'
      CALL INBIEF(SLIFBR%I,KLOG,SITR31,SITR32,SITR33,
     &            LVMAC,IELM2,LAMBD0,SPHE,MESH,STSDER,STSTOT,1,1,EQUA)
      IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE INBIEF'
!
!     EXTENSION OF IKLE2 (SEE CALL TO POST_INTERP IN PROPA)
!
      CALL BUILD_IKLE_EXT(IKLE_EXT%I,IKLE_EXT%DIM1,IKLE2,NELEM2)
!
!     MESH ORGANISATION - 3D LEVEL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECLIM POUR MESH3D'
      CALL LECLIM
     & (SLIFBR%I,SITR31%I,SITR31%I,SITR31%I,SFBOR%R,STSDER%R,STSDER%R,
     &  STSDER%R,STSDER%R,STSDER%R,STSDER%R,NPTFR,3,.FALSE.,
     &  WAC_FILES(WACCLI)%LU,
     &  KENT,KENTU,KSORT,KADH,KLOG,KINC,SITR31%I,MESH3D)
      IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE LECLIM'
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INBIEF POUR MESH3D'
      CALL INBIEF(SLIFBR%I,KLOG,SITR31,SITR32,SITR33,
     &            LVMAC,IELM3,LAMBD0,SPHE,MESH3D,
     &            STSDER,STSTOT,1,1,EQUA)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INBIEF'
!
!     3D IFABOR
!
      CALL IFABTOM(MESH3D%IFABOR%I,NELEM2,NPLAN-1)
!
!-----------------------------------------------------------------------
!
!     V6P2 Diffraction : FREEMESH METHOD
!
      IF(DIFFRA.GT.0) THEN
        IF(NCSIZE.GT.1.AND.OPTDER.EQ.1) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) ''
            WRITE(LU,*) '***************************************'
            WRITE(LU,*) ' ATTENTION : DIFFRACTION               '
            WRITE(LU,*) ' OPTION POUR LES DERIVEES SECONDES   '
            WRITE(LU,*) ' PASSE A 2 EN PARALLELE              '
            WRITE(LU,*) '***************************************'
          ELSE
            WRITE(LU,*) ''
            WRITE(LU,*) '***************************************'
            WRITE(LU,*) ' ATTENTION : DIFFRACTION               '
            WRITE(LU,*) ' OPTION FOR THE SECOND DERIVATIVES     '
            WRITE(LU,*) ' SET TO 2 IN PARALLEL MODE             '
            WRITE(LU,*) '***************************************'
          ENDIF
        ENDIF
        WRITE(LU,*) '****************************************'
        WRITE(LU,*) 'DIFFRACTION IS TAKEN INTO ACCOUNT      '
        WRITE(LU,*) 'STARTING FROM TIME STEP ',NPTDIF
        IF(DIFFRA.EQ.1) THEN
          WRITE(LU,*) 'MILD SLOPE EQUATION FORMULATION'
        ELSE
          WRITE(LU,*)'REVISED MILD SLOPE EQUATION FORMULATION'
        ENDIF
        WRITE(LU,*) '****************************************'
!
!    SETS UP OF THE SUBDOMAINS FOR THE FREEMSESH METHOD
!    AND CALCULATES THE INVERSE MATRICES FOR EACH SUBDOMAIN
!
        IF(OPTDER.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FREEMESH'
          CALL FRMSET(MESH%X%R, MESH%Y%R,SNEIGB%I,SNB_CLOSE%I,
     &                NPOIN2  , MAXNSP , NRD    , NELEM2 ,
     &                MESH%IKLE%I,SRK%R, SRX%R  ,SRY%R   ,
     &                SRXX%R  , SRYY%R )
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FREEMESH'
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
! LECTURE DE LA COTE DU FOND (ZF) SUR LE FICHIER DE GEOMETRIE
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FONSTR'
      CALL FONSTR(ST1,SZF,ST2,ST3,WAC_FILES(WACGEO)%LU,
     &            WAC_FILES(WACGEO)%FMT,
     &            WAC_FILES(WACFON)%LU,WAC_FILES(WACFON)%NAME,MESH,
     &            1.D0,.TRUE.)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FONSTR'
!
! CORRECTION EVENTUELLE DES VALEURS DU FOND (OU CALCUL DU FOND SI CELA
! N'A PAS ETE FAIT DANS FONSTR)
! EN STANDARD, TOM_CORFON NE FAIT RIEN (ATTENTION, ALLER CHERCHER LE TOM_CORFON
! DE TOMAWAC).
! DANS LE CAS DE COUPLAGE AVEC TELEMAC, ON LIT LE FOND A PARTIR DU
! MODELE TELEMAC ET TOM_CORFON N EST PAS UTILISE
!
      IF(PART.LT.0)THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TOM_CORFON'
        CALL TOM_CORFON
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TOM_CORFON'
      ENDIF
!
!     CALCUL DE LA PROFONDEUR D'EAU (TABLEAU DEPTH)
!
      DO IP=1,NPOIN2
        DEPTH(IP)=MAX(ZREPOS-ZF(IP),0.9D0*PROMIN)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     PREPARATION DES SORTIES GRAPHIQUES
!
!     CREATION DU JEU DE DONNEES POUR UN FORMAT DE FICHIER FORMAT_RES.
!     LE JEU DE DONNEES EST CREE DANS LE FICHIER NRES, ET EST DEFINI
!     PAR UN TITRE ET LES VARIABLES A ECRIRE.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CREATE_DATASET'
      CALL CREATE_DATASET(WAC_FILES(WACRES)%FMT, ! FORMAT FICHIER RESULTAT
     &                    WAC_FILES(WACRES)%LU,  ! LU FICHIER RESULTAT
     &                    TITCAS,     ! TITRE DE L'ETUDE
     &                    MAXVAR,     ! MAX VARIABLES SORTIE
     &                    TEXTE,      ! NOMS VARIABLES SORTIE
     &                    SORLEO)     ! SORTIE OU PAS DES VARIABLES
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CREATE_DATASET'
!
!     ECRITURE DU MAILLAGE DANS LE FICHIER SORTIE :
!     SI ON EST ON PARALLEL, FAUT L'INDIQUER VIA NCSIZE ET NPTIR.
!     LES AUTRES INFORMATIONS SONT DANS MESH.
!     EN PLUS : DATE/TEMPS DE DEPART ET LES COORDONNEES DE L'ORIGINE.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WRITE_MESH'
      CALL WRITE_MESH(WAC_FILES(WACRES)%FMT, ! FORMAT FICHIER RESULTAT
     &                WAC_FILES(WACRES)%LU,  ! LU FICHIER RESULTAT
     &                MESH,          ! DESCRIPTEUR MAILLAGE
     &                1,             ! NOMBRE DE PLAN /NA/
     &                DATE,          ! DATE DEBUT
     &                TIME,          ! HEURE DEBUT
     &                I_ORIG,J_ORIG) ! COORDONNEES DE L'ORIGINE.
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
!
      LT=0
      DTSI=DT/NSITS
!
!-----------------------------------------------------------------------
!
!     INITIALISES TETA
!     BY DEFAULT THE DIRECTIONS OF PROPAGATION ARE EVENLY DISTRIBUTED
!
      DO IPLAN = 1,NPLAN+1
        TETA(IPLAN) = (IPLAN-1)*DEUPI/NPLAN
      ENDDO
!
!-----------------------------------------------------------------------
!
!     INITIALISES FREQ AND DFREQ, THE FREQUENCIES OF PROPAGATION
!     ARE DISTRIBUTED USING AN EXPONENTIAL LAW
!
      DO IFREQ = 1,NF
        FREQ(IFREQ) = F1*RAISF**(IFREQ-1)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     INITIALISING DZHDT (BUT MAYBE REDONE IN LECSUI OR CONDIW)
!
      DO IP=1,NPOIN2
        SDZHDT%R(IP)=0.D0
      ENDDO
!
      IF(SUIT) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECSUI'
        CALL LECSUI(SF%R  ,NPLAN ,NF    ,STETA%R, SFR%R  ,
     &              NELEM2,NPOIN2,AT    ,SUC%R  , SVC%R  ,
     &              SUC1%R,SVC1%R,SUC2%R,SVC2%R , SUV%R  ,
     &              SVV%R ,SUV1%R,SVV1%R,SUV2%R , SVV2%R ,
     &              VENT  ,TV1   ,TV2   ,COUSTA.OR.PART.EQ.0 ,
     &              WAC_FILES(WACPRE)%LU,
     &              BINPRE,SDEPTH%R,TC1,TC2,ZM1,ZM2,
     &              SDZHDT%R,TM1,TM2,MAREE.OR.PART.EQ.0,STSDER%R)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LECSUI'
      ELSE
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CONDIW'
        CALL CONDIW(AT,LT,TC1,TC2,TV1,TV2,TM1,TM2,
     &              NVHMA,NVCOU,NVWIN,PART,U_TEL,V_TEL,H_TEL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CONDIW'
!       JMH: DEPTH MAY BE MODIFIED IN CONDIW
        DO IP=1,NPOIN2
          IF(DEPTH(IP).LT.PROMIN) DEPTH(IP)=0.9D0*PROMIN
        ENDDO
      ENDIF
!
!      IF(RAZTIM) AT=0.D0
!
      AT0=AT
!
      DO IP=1,NPOIN2
        IF(DEPTH(IP).LE.0.D0) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) ''
            WRITE(LU,*) '*************************'
            WRITE(LU,*) ' ! PROFONDEUR NEGATIVE ! '
            WRITE(LU,*) '   ARRET DU PROGRAMME    '
            WRITE(LU,*) '*************************'
            CALL PLANTE(1)
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*) ''
            WRITE(LU,*) '**************************'
            WRITE(LU,*) ' ! NEGATIVE WATER DEPTH ! '
            WRITE(LU,*) '   END OF THE COMPUTATION '
            WRITE(LU,*) '**************************'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!=====C
!  4  C CALCULS PREPARATOIRES POUR INTERACTIONS NON-LINEAIRES.
!=====C=======================================================
!.....DIA method (Hasselmann et al., 1985)
!
      IF(STRIF.EQ.1) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRENL1'
        CALL PRENL1( IANGNL, COEFNL, NPLAN , NF , RAISF , XLAMD )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRENL1'
!
!.....MDIA method (Tolman, 2004)
!
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
          CALL PRENL2(IANMDI(1,1,K),COEMDI(1,K),NPLAN,NF,RAISF,
     &                XLAMDI(K),XMUMDI(K))
        ENDDO
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRENL2'
!
!.....GQM method (Lavrenov, 2001)
!
      ELSEIF(STRIF.EQ.3) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRENL3'
        CALL PRENL3
     &( NF    , NPLAN , RAISF , TAILF , FREQ  , TB_SCA, LBUF  , DIMBUF,
     &  F_POIN, F_COEF, T_POIN, F_PROJ, IQ_OM1, NQ_TE1, NQ_OM2, NF1   ,
     &  NT1   , K_IF1 , K_IF2 , K_IF3 , TB_V14, TB_V24, TB_V34, K_1P  ,
     &  K_1M  , K_1P2P, K_1P3M, K_1P2M, K_1P3P, K_1M2P, K_1M3M, K_1M2M,
     &  K_1M3P, TB_TPM, TB_TMP, TB_FAC, SEUIL1, SEUIL2, ELIM  , NCONF ,
     &  NCONFM, IDCONF)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRENL3'
      ENDIF
!
      IF(STRIA.EQ.2) THEN
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
      CALL OV('X=C     ',STRA41%R,STRA32%R,STRA33%R,0.D0,NPOIN2)
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
        ELSEIF (SVENT.GE.2) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR2'
          CALL USTAR2(STRA42%R,SUV%R,SVV%R,NPOIN2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR2'
        ELSEIF (SVENT.EQ.0.AND.LVENT.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR2'
          CALL USTAR2(STRA42%R,SUV%R,SVV%R,NPOIN2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR2'
        ELSEIF (SVENT.EQ.0.AND.LVENT.EQ.0.AND.SMOUT.EQ.2) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR2'
          CALL USTAR2(STRA42%R,SUV%R,SVV%R,NPOIN2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR2'
        ELSE
          IF(LNG.EQ.1) THEN
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
!=====C
!  6  C INITIALISATION DE CERTAINS TABLEAUX UTILES.
!=====C============================================
!
!     COUPLAGE TELEMAC-TOMAWAC si PART=0
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INITAB'
      CALL INITAB( SIBOR%I, MESH%IFABOR%I, NELEM2, PART)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INITAB'
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE IMPR'
      CALL IMPR(LISPRD,LT,AT,LT,3)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE IMPR'
!
!=====C
!  7  C AFFECTATION DES CONDITIONS AUX LIMITES A L'INSTANT INITIAL.
!=====C============================================================
!
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
!=====C CALCUL DES NOMBRES D'ONDE (XK), DE LA VITESSE DE GROUPE (CG) ET
!  8  C DU FACTEUR DE PASSAGE (B) EN SPECTRE DE VARIANCE EN (FR,TETA).
!=====C=================================================================
!
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
!=====C
!  9  C SORTIES GRAPHIQUES (EVENTUELLES) A L'ETAT INITIAL.
!=====C===================================================
!
!.....9.1 CHOIX DES POINTS DE SORTIE DU SPECTRE DIRECTIONNEL.
!
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROXIM'
      IF(NPLEO.GT.0) THEN
        CALL PROXIM(NOLEO,XLEO,YLEO,MESH%X%R,MESH%Y%R,
     &              NPLEO,NPOIN2,MESH%IKLE%I,NELEM2,NELEM2)
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROXIM'
!
!.....9.2 TEST POUR SAVOIR SI ON IMPRIME OU PAS.
!
      IMPRES=.FALSE.
      DEBRES=.FALSE.
      IF(LT.EQ.GRADEB) THEN
        IMPRES=.TRUE.
        DEBRES=.TRUE.
      ENDIF
!
      IF(IMPRES) THEN
!
!.....9.3 IMPRESSION (EVENTUELLE) DES VARIABLES SUR LE MAILLAGE 2D.
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DUMP2D'
!
!       THE VARIABLES ARE COMPUTED HERE WITH THE ORIGINAL SPECTRUM
!       DONE IN SPEINI, THERE IS NO CALL TRANSF BEFORE BECAUSE
!       CURRENTS ARE NOT TAKEN INTO ACCOUNT IN SPEINI
!
        CALL DUMP2D(LT,SF%R,NPOIN3*NF)
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
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRSPE'
        CALL ECRSPE
     &( SF%R    , STETA%R, NPLAN ,
     &  SFR%R   , NF  , NF   , NPOIN2      , AT ,
     &  STSDER%R, NOLEO , NPLEO , WAC_FILES(WACLEO)%LU ,
     &  WAC_FILES(WACLEO)%FMT, DEBRES , TITCAS , DATE , TIME ,
     &  MESH%KNOLG%I ,MESH ,WAC_FILES(WACSPE)%LU ,
     &  WAC_FILES(WACSPE)%NAME)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRSPE'
!
      ENDIF
!
!     CASE OF TRIPLE COUPLING, INITIAL CONDITIONS
!
      IF(INCLUS(COUPLING,'SISYPHE').AND.PART.EQ.0) THEN
!       3 VARIABLES THAT WILL BE TRANSMITTED TO SISYPHE
!       ALL THIS IF BLOCK ADAPTED FROM DUMP2D
!       MEAN DIRECTION
        CALL TETMOY(DIRMOY_TEL%R,SF%R,SCOSTE%R,SSINTE%R,
     &             NPLAN,FREQ,SDFR%R,NF,NPOIN2,TAILF,STRA36%R,
     &             STRA37%R,STRA38%R,STRA39%R)
        IF(TRIGO) THEN
          DO IP=1,NPOIN2
            DIRMOY_TEL%R(IP)=(PISUR2-DIRMOY_TEL%R(IP))*GRADEG
          ENDDO
        ELSE
          DO IP=1,NPOIN2
            DIRMOY_TEL%R(IP)=DIRMOY_TEL%R(IP)*GRADEG
          ENDDO
        ENDIF
!       SIGNIFICANT WAVE HEIGHT
        CALL TOTNRJ(STRA37%R,SF%R,SFR%R,SDFR%R,TAILF,
     &              NF,NPLAN,NPOIN2)
        DO IP=1,NPOIN2
          HM0_TEL%R(IP)=4.D0*SQRT(TRA37(IP))
        ENDDO
!       TPR5
        CALL FPREAD(TPR5_TEL%R,SF%R,SFR%R,SDFR%R,NF,NPLAN,
     &              NPOIN2,5.D0,TAILF,STRA38%R,STRA39%R)
        DO IP=1,NPOIN2
          TPR5_TEL%R(IP)=
     &    1.D0/MIN(MAX(TPR5_TEL%R(IP),FREQ(1)),FREQ(NF))
        ENDDO
      ENDIF
!
!=====C
!  10 C PREPARATION DE LA PROPAGATION (REMONTEE DES CARACTERISTIQUES).
!=====C===============================================================
!
      IF(PROP) THEN
!
        CALL IMPR(LISPRD,LT,AT,LT,1)
        CALL IMPR(LISPRD,LT,AT,LT,2)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREPRO 1'
!
        CALL PREPRO
!        EX-SCX      EX-SCY (MEMORY OPTIMISATION)
     & ( STSDER    , STSTOT   , SCT      , SCF     , DT  ,
     &   MESH%X%R , MESH%Y%R  , STETA    ,
     &   SCOSTE%R , SSINTE%R  , SFR      , MESH%IKLE%I   ,
     &   SIBOR%I  , SETAP1%I  , STRA01%R , SSHP1 ,
     &   SSHZ     , SSHF      ,
     &   SELT%I   , SETA%I    , SFRE%I   , SDEPTH%R,
     &   SDZHDT%R , SDZX%R    , SDZY%R   , SUC%R   ,
     &   SVC%R    , SDUX%R    , SDUY%R   , SDVX%R  ,
     &   SDVY%R   , SXK%R     , SCG%R    , SCOSF%R ,
     &   STGF%R   , SITR01%I  , NPOIN3   , NPOIN2  , NELEM2,
     &   NPLAN    , NF    , MESH%SURDET%R, COURAN.OR.PART.EQ.0,
     &   SPHE     , PROINF   , PROMIN,MESH,MESH3D,MESH%IKLE,TB,
     &   IELM3    , DIFFRA   , MAREE ,ISUB)
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
!
      AT=AT+DT
!Calcul de LT (NOTE JMH: WHY NOT LT=LT+1 ?)
      LT=NINT((AT-AT0)/DT)
!
!Fin COUPLAGE
!
      CALL IMPR(LISPRD,LT,AT,LT,3)
!
!     11.2 AFFECTATION DES CONDITIONS AUX LIMITES.
!
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
!     11.2b MISE A ZERO DU SPECTRE SUR LES POINTS OU PROF < PROMIN
!
      IF (.NOT.PROINF) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRETE'
        CALL ECRETE(SF%R,SDEPTH%R,NPOIN2,NPLAN,NF,PROMIN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRETE'
      ENDIF
!
      IF(MAREE) THEN
        LT1=MAX((LT/LAM)*LAM,2)
!       JMH 08/02/20013 : THIS WAS DONE TO ADD AFTER THE
!       ELEVATION READ IN CORMAR. NOW WE READ DIRECTLY THE DEPTH
!       SO THIS IS PROBABLY USELESS.
        IF(LT.EQ.LT1) THEN
          DO IP=1,NPOIN2
            DEPTH(IP)=ZREPOS-ZF(IP)
          ENDDO
        ENDIF
      ENDIF
!
!......11.3 UPDATING DEPTH AND CURRENTS
!
!     COUPLING TELEMAC-TOMAWAC OR CURRENTS AND/OR DEPTH IN A FILE
!     THEY ARE UPDATED HERE.
!
      IF(MAREE.AND.LT.EQ.LT1.OR.
     &   (PART.EQ.1.AND.LT_WAC.EQ.1)) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORMAR'
        CALL CORMAR(AT,LT,TC1,TC2,TV1,TV2,TM1,TM2,
     &              NVHMA,NVCOU,PART,U_TEL,V_TEL,H_TEL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORMAR'
        DO IP=1,NPOIN2
          IF(DEPTH(IP).LT.PROMIN) DEPTH(IP)=0.9D0*PROMIN
        ENDDO
!
!......11.3.1 PREPARING PROPAGATION (METHOD OF CHARACTERISTICS).
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INIPHY'
        CALL INIPHY(SXK%R,SCG%R,SB%R,SDEPTH%R,SFR%R,
     &              SCOSF%R,NPOIN2,NF,PROINF,SPHE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INIPHY'
!
        IF(PROP) THEN
!
          CALL IMPR(LISPRD,LT,AT,LT,1)
          CALL IMPR(LISPRD,LT,AT,LT,2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREPRO 2'
!
          CALL PREPRO
!         EX-SCX      EX-SCY (MEMORY OPTIMISATION)
     &  ( STSDER    , STSTOT     , SCT      , SCF     , DT  ,
     &    MESH%X%R , MESH%Y%R  , STETA    ,
     &    SCOSTE%R , SSINTE%R  , SFR      , MESH%IKLE%I   ,
     &    SIBOR%I  , SETAP1%I  , STRA01%R , SSHP1 ,
     &    SSHZ     , SSHF      ,
     &    SELT%I   , SETA%I    , SFRE%I   , SDEPTH%R,
     &    SDZHDT%R , SDZX%R    , SDZY%R   , SUC%R   ,
     &    SVC%R    , SDUX%R    , SDUY%R   , SDVX%R  ,
     &    SDVY%R   , SXK%R     , SCG%R    , SCOSF%R ,
     &    STGF%R   , SITR01%I  , NPOIN3   , NPOIN2  , NELEM2,
     &    NPLAN    , NF    , MESH%SURDET%R, COURAN.OR.PART.EQ.1,
     &    SPHE     , PROINF    , PROMIN,MESH,MESH3D,MESH%IKLE,TB,
     &    IELM3    , DIFFRA    , MAREE ,ISUB )
!Fin COUPLAGE
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREPRO 2'
        ENDIF
      ENDIF
!Fin cycle IF((MAREE.AND.LT.EQ.LT1).OR.(PART.EQ.1.AND.LT_WAC.EQ.1))
!------------------------------------------------------------------
!V6P2 Diffraction : diffraction term calculation
      IF(DIFFRA.GT.0) THEN
        IF(LT.EQ.NPTDIF)THEN
          WRITE(LU,*)'*********************************'
          WRITE(LU,*)'DIFFRACTION IS TAKEN INTO ACCOUNT'
          WRITE(LU,*)'*********************************'
        ENDIF
        IF(LT.GE.NPTDIF) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREDIF'
          CALL PREDIF
!        EX-SCX%R     EX-SCY%R (MEMORY OPTIMISATION)
     & ( STSDER   , STSTOT    , SCT    , DT    ,
     &   MESH%X%R  , MESH%Y%R , STETA ,
     &   SCOSTE%R , SSINTE%R  , SFR%R    , MESH%IKLE%I     ,
     &   SIBOR%I  , SETAP1%I  , STRA01%R , SSHP1 , SSHZ    ,
     &   SELT%I   , SETA%I    , SDEPTH%R,
     &   SDZX%R    , SDZY%R   ,
     &   SXK%R     , SCG%R    ,
     &   SITR01%I  , NPOIN3   , NPOIN2  , NELEM2,
     &   NPLAN    , NF        , MESH%SURDET%R, COURAN.OR.PART.EQ.1,
     &   SPHE     , PROINF    , SA%R     , SDFR%R  ,
     &   SF%R,SCCG%R,SDIV%R   , SDELTA%R , SDDX%R  ,
     &   SDDY%R   , F2DIFM    , NBOR     , NPTFR   ,
     &   SXKONPT%R , SRK%R    , SRX%R    ,
     &   SRY%R    , SRXX%R    , SRYY%R   , SNEIGB%I,
     &   SNB_CLOSE%I, DIFFRA  , MAXNSP   , FLTDIF  ,
     &   MESH3D,MESH,IELM3,TB,ISUB,MESH%IKLE)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREDIF'
        ENDIF
      ENDIF
!V6P2 End diffraction
!-------------------------------------------------------------------
!
!.....11.3 PROPAGATION (INTERPOLATION AU PIED DES CARACTERISTIQUES).
!
      IF(PROP) THEN
        CALL IMPR(LISPRD,LT,AT,LT,5)
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROPA'
        CALL PROPA(SF%R,SB%R,SSHP1,
     &             SSHZ,SSHF,SELT%I,SETA%I,SFRE%I,
     &             IKLE_EXT,NPOIN3,NPOIN2,
!                                               WORK ARRAYS HERE
     &             NPLAN,NF,COURAN.OR.PART.EQ.1,STSDER%R,STSTOT,
     &             ITR01,T3_01,T3_02,ISUB,MESH3D)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROPA'
      ENDIF
!
!.....11.4 INTEGRATION DES TERMES SOURCES.
!
      IF(TSOU) THEN
        CALL IMPR(LISPRD,LT,AT,NSITS,4)
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SEMIMP'
        CALL SEMIMP(SF%R,SXK%R,SFR%R,SDFR%R,SDEPTH%R, SUV%R, SVV%R ,
     &  MESH%X%R,MESH%Y%R,WAC_FILES(WACVEB)%LU,WAC_FILES(WACVEF)%LU ,
     &  NBOR,NPTFR,DDC,TV1,TV2,SUV1%R,SVV1%R,
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
     &  STRA39%R,ST1%R,ST2%R,ST3%R,ST4%R,SBETA%R,NQ_TE1,NQ_OM2,
     &  NF1,NF2,NT1,NCONF,NCONFM,SEUIL,LBUF,DIMBUF,F_POIN,T_POIN,
     &  F_COEF,F_PROJ,TB_SCA,K_IF1,K_1P,K_1M,K_IF2,K_IF3,K_1P2P,K_1P2M,
     &  K_1P3P,K_1P3M,K_1M2P,K_1M2M,K_1M3P,K_1M3M,IDCONF,TB_V14,TB_V24,
     &  TB_V34,TB_TPM,TB_TMP,TB_FAC,MDIA,IANMDI,COEMDI,NVWIN,DIAGHF)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SEMIMP'
      ENDIF
!

!======================================================================
!MJTS MODIF POUR CAS MONOCHROMATIQUE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!======================================================================
!     On force le spectre à 0 sauf sur le bin qui porte de l'energie
!     ici la direction 7 et la frequence 3.
      CALL MONOCRO(SF%R, NPOIN2, NF, NPLAN, 4, 10)
!MJTS FIN DE MODIF
!.....11.6 TEST POUR SAVOIR SI ON IMPRIME OU PAS.
!     """""""""""""""""""""""""""""""""""""""""""
      IMPRES=.FALSE.
      DEBRES=.FALSE.
      IF(LT.GE.GRADEB.AND.MOD(LT-GRADEB,GRAPRD).EQ.0) IMPRES=.TRUE.
      IF(LT.EQ.GRADEB) DEBRES=.TRUE.
!
      IF(IMPRES) THEN
!
!.....11.5 PASSAGE EN FREQUENCE ABSOLUE.
!     """""""""""""""""""""""""""""""""""""""""""""
!
        IF(COURAN.OR.PART.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TRANSF'
          CALL TRANSF
!         WORK ARRAY UNTIL CALL ECRSPE BELOW
     &(   STSTOT%R , SF%R  , SFR%R , SDFR%R, SCOSTE%R,
     &    SSINTE%R , SUC%R , SVC%R , SXK%R , SITR11%I,
     &    SITR12%I , SITR13%I      , STRA31%R, STRA32%R,
     &    NPOIN2   , NPLAN , NF    , RAISF , LT , GRADEB, GRAPRD)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TRANSF'
        ENDIF
!
!.....11.7 IMPRESSION (EVENTUELLE) DES VARIABLES SUR LE MAILLAGE 2D.
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DUMP2D'
        IF(COURAN.OR.PART.EQ.1) THEN
          CALL DUMP2D(LT,STSTOT%R,NPOIN3*NF)
        ELSE
          CALL DUMP2D(LT,SF%R,NPOIN3*NF)
        ENDIF
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
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRSPE'
        IF(COURAN.OR.PART.EQ.1) THEN
          CALL ECRSPE(STSTOT%R,STETA%R,NPLAN,
     &                SFR%R,NF,NF,NPOIN2,AT,
     &                STSDER%R,NOLEO,NPLEO,WAC_FILES(WACLEO)%LU,
     &                WAC_FILES(WACLEO)%FMT,DEBRES,TITCAS,DATE,TIME,
     &                MESH%KNOLG%I,MESH,WAC_FILES(WACSPE)%LU ,
     &                WAC_FILES(WACSPE)%NAME)
        ELSE
          CALL ECRSPE(SF%R,STETA%R,NPLAN,
     &                SFR%R,NF,NF,NPOIN2,AT,
     &                STSDER%R,NOLEO,NPLEO,WAC_FILES(WACLEO)%LU,
     &                WAC_FILES(WACLEO)%FMT,DEBRES,TITCAS,DATE,TIME,
     &                MESH%KNOLG%I,MESH,WAC_FILES(WACSPE)%LU ,
     &                WAC_FILES(WACSPE)%NAME)
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRSPE'
!
      ENDIF
!
!     RADIATION STRESSES COMPUTED HERE FOR TELEMAC INDEPENDENTLY
!     OF THE PRINTS TO RESULT FILE, WHICH IS NOT MANDATORY
!
      IF(PART.EQ.1.AND.LT_WAC.EQ.NIT) THEN
!       STSTOT WORK ARRAY (ABSOLUTE FREQUENCY) IN ALL THIS IF BLOCK
        CALL TRANSF(STSTOT%R , SF%R  , SFR%R , SDFR%R, SCOSTE%R,
     &              SSINTE%R , SUC%R , SVC%R , SXK%R , SITR11%I,
     &              SITR12%I , SITR13%I      , STRA31%R, STRA32%R,
     &              NPOIN2   , NPLAN , NF    , RAISF ,
!                   TO FORCE THE WORK WHATEVER LT,GRADEB,GRAPRD
     &              1,1,1)
        CALL RADIAT(FX_WAC%R,FY_WAC%R,STRA53%R,STRA54%R,STRA55%R,
     &              SXK%R,STSTOT%R,SCG%R,SDEPTH%R,
!                   STSDER%R WORK TABLE HERE
     &              STSDER%R,
     &              STRA36%R,STRA37%R,STRA38%R,STRA39%R)
        IF(VENT) THEN
          CALL OV('X=Y     ',UV_WAC%R,SUV%R,SUV%R,0.D0,NPOIN2)
          CALL OV('X=Y     ',VV_WAC%R,SVV%R,SVV%R,0.D0,NPOIN2)
        ENDIF
        IF(INCLUS(COUPLING,'SISYPHE')) THEN
!         3 VARIABLES THAT WILL BE TRANSMITTED TO SISYPHE
!         ALL THIS IF BLOCK ADAPTED FROM DUMP2D
!         MEAN DIRECTION
          CALL TETMOY(DIRMOY_TEL%R,STSTOT%R,SCOSTE%R,SSINTE%R,
     &                NPLAN,FREQ,SDFR%R,NF,NPOIN2,TAILF,STRA36%R,
     &                STRA37%R,STRA38%R,STRA39%R)
          IF(TRIGO) THEN
            DO IP=1,NPOIN2
              DIRMOY_TEL%R(IP)=(PISUR2-DIRMOY_TEL%R(IP))*GRADEG
            ENDDO
          ELSE
            DO IP=1,NPOIN2
              DIRMOY_TEL%R(IP)=DIRMOY_TEL%R(IP)*GRADEG
            ENDDO
          ENDIF
!         SIGNIFICANT WAVE HEIGHT
          CALL TOTNRJ(STRA37%R,STSTOT%R,SFR%R,SDFR%R,TAILF,
     &                NF,NPLAN,NPOIN2)
          DO IP=1,NPOIN2
            HM0_TEL%R(IP)=4.D0*SQRT(TRA37(IP))
          ENDDO
!         TPR5
          CALL FPREAD(TPR5_TEL%R,STSTOT%R,SFR%R,SDFR%R,NF,NPLAN,
     &                NPOIN2,5.D0,TAILF,STRA38%R,STRA39%R)
          DO IP=1,NPOIN2
            TPR5_TEL%R(IP)=
     &      1.D0/MIN(MAX(TPR5_TEL%R(IP),FREQ(1)),FREQ(NF))
          ENDDO
        ENDIF
!
      ENDIF
!
10    CONTINUE
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
      IF(ABS(AT-AT0-NIT*DT).LT.1.D-6) THEN
!
        IF(GLOB) THEN
          CALL IMPR(LISPRD,NIT,AT,NIT,6)
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOR3D'
          CALL SOR3D(SF%R,NPLAN,NF,STETA%R,SFR%R,
     &               NELEM2,NPOIN2,AT,SUC%R,SVC%R,
     &               SUV%R,SVV%R,SDEPTH%R,VENT,
     &               COURAN.OR.PART.EQ.1,
     &               MAREE.OR.PART.EQ.1,TITCAS,WAC_FILES(WACRBI)%LU,
     &               BINRBI,STSDER%R,MESH3D)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOR3D'
        ENDIF
!
!----------------------------------------------------------------------
!
!       VALIDATION DES RESULTATS SUR LE FICHIER DE REFERENCES
!
        IF(VALID) CALL BIEF_VALIDA(BST1,TEXTE,
     &                     WAC_FILES(WACREF)%LU,WAC_FILES(WACREF)%FMT,
     &                     VARSOR,TEXTE,
     &                     WAC_FILES(WACRES)%LU,WAC_FILES(WACRES)%FMT,
     &                     MAXVAR,NPOIN2,NIT,NIT,ALIRE)
!
      ENDIF
!
!----------------------------------------------------------------------
!
      RETURN
      END

