!                    *****************
                     SUBROUTINE SEMIMP
!                    *****************
!
     &( F     , XK    , FREQ  , DFREQ , DEPTH , VENTX , VENTY , X     ,
     &  Y     , NVEB  , NVEF  , NBOR  , NPTFR , DDC   , TV1   , TV2   ,
     &  NP    ,
     &  XRELV , YRELV , U1    , V1    , U2    , V2    , TETA  , SINTET,
     &  COSTET, INDIC , TAILF , RAISF , GRAVIT, CFROT1, CMOUT1, CMOUT2,
     &  TPROP , DTSI  , ROAIR , ROEAU , XKAPPA, BETAM , DECAL , CDRAG ,
     &  ALPHA , ZVENT , NF    , NPLAN , NPOIN2, IANGNL, COEFNL, F1    ,
     &  NSITS , SMOUT , SFROT , SVENT , STRIF , VENT  , VENSTA, VX_CTE,
     &  VY_CTE, SBREK , ALFABJ,
     &  GAMBJ1, GAMBJ2, IQBBJ , IHMBJ , IFRBJ , BORETG, GAMATG, IWHTG ,
     &  IFRTG , ALFARO, GAMARO, GAM2RO, IDISRO, IEXPRO, IFRRO , BETAIH,
     &  EM2SIH, IFRIH , COEFHS, XDTBRK, NDTBRK, STRIA , ALFLTA, RFMLTA,
     &  KSPB  , BDISPB, BDSSPB, PROINF, DF_LIM, LIMIT , CIMPLI,
     &  NOMVEB, NOMVEF, BINVEN, NBD   , QINDI , TAUWAV,
     &  USOLD , TWOLD , Z0OLD , TSTOT , TSDER , TOLD  , TNEW  , VARIAN,
     &  FMOY  , XKMOY , USNEW , Z0NEW , TWNEW , TAUX1 , TAUX2 , TAUX3 ,
     &  TAUX4 , TAUX5 , TAUX6 , TAUX7 , TRA01 , BETA)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE INTEGRATION STEP OF THE SOURCE TERMS USING
!+                A SCHEME WITH VARIABLE DEGREE OF IMPLICITATION.
!
!history  M. BENOIT
!+        26/03/95
!+        V1P0
!+   CREATED
!
!history  M. BENOIT
!+        07/11/96
!+        V1P2
!+   MODIFIED
!
!history
!+        25/08/2000
!+        V5P0
!+   MODIFIED
!
!history  JMH
!+        16/12/2008
!+        V5P9
!+   BETA HAS BEEN ADDED TO THE LIST OF ARGUMENTS AND
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALFABJ         |-->| MODELE DEFERLEMENT BJ : CONSTANTE ALPHA
!| ALFARO         |-->| MODELE DEFERLEMENT RO : CONSTANTE ALPHA
!| ALFLTA         |---|
!| ALPHA          |-->| CONSTANTE DE LA LOI DE CHARNOCK
!| BDISPB         |---|
!| BDSSPB         |---|
!| BETA           |---|
!| BETAIH         |-->| MODELE DEFERLEMENT IH : CONSTANTE BETA
!| BETAM          |-->| CONSTANTE BETAMAX DE LA FORMULE DU VENT
!| BINVEN         |-->| BINAIRE DU FICHIER DE VENT EN ENTREE
!| BORETG         |-->| MODELE DEFERLEMENT TG : CONSTANTE B
!| CDRAG          |-->| COEFFICIENT DE TRAINEE
!| CFROT1         |-->| CONSTANTE POUR LE TERME DE FROTTEMENT
!| CIMPLI         |---|
!| CMOUT1         |-->| CONSTANTE 1 POUR LE TERME DE MOUTONNEMENT
!| CMOUT2         |-->| CONSTANTE 2 POUR LE TERME DE MOUTONNEMENT
!| COEFHS         |-->| COEFFICIENT LIMITATEUR DE LA HAUTEUR HS
!| COEFNL         |---|
!| COSTET         |---|
!| DDC            |-->| DATE DE DEBUT DU CALCUL
!| DECAL          |-->| CONSTANTE DE DECALAGE DE CROISSANCE VENT
!| DEPTH          |---|
!| DFREQ          |---|
!| DF_LIM         |---|
!| DTSI           |-->| PAS DE TEMPS D'INTEGRATION (SECONDES)
!| EM2SIH         |-->| MODELE DEFERLEMENT IH : CONSTANTE M2*
!| F              |---|
!| F1             |-->| PREMIERE FREQUENCE DE DISCRETISATION
!| FMOY           |---|
!| FREQ           |---|
!| GAM2RO         |-->| MODELE DEFERLEMENT RO : CONSTANTE GAMMA2
!| GAMARO         |-->| MODELE DEFERLEMENT RO : CONSTANTE GAMMA
!| GAMATG         |-->| MODELE DEFERLEMENT TG : CONSTANTE GAMMA
!| GAMBJ1         |-->| MODELE DEFERLEMENT BJ : CONSTANTE GAMMA1
!| GAMBJ2         |-->| MODELE DEFERLEMENT BJ : CONSTANTE GAMMA2
!| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
!| IANGNL         |---|
!| IDISRO         |-->| MODELE DEFERLEMENT RO : DISTRIBUTION HOULE
!| IEXPRO         |-->| MODELE DEFERLEMENT RO : EXPOSANT N
!| IFRBJ          |-->| MODELE DEFERLEMENT BJ : MODE CALCUL DE FREQ
!| IFRIH          |-->| MODELE DEFERLEMENT IH : MODE CALCUL DE FREQ
!| IFRRO          |-->| MODELE DEFERLEMENT RO : MODE CALCUL DE FREQ
!| IFRTG          |-->| MODELE DEFERLEMENT TG : MODE CALCUL DE FREQ
!| IHMBJ          |-->| MODELE DEFERLEMENT BJ : MODE CALCUL DE HM
!| INDIC          |-->| TYPE DE FORMAT DE LECTURE
!| IQBBJ          |-->| MODELE DEFERLEMENT BJ : MODE CALCUL DE QB
!| IWHTG          |-->| MODELE DEFERLEMENT TG : MODE CALCUL DE W(H)
!| KSPB           |---|
!| LIMIT          |---|
!| NBD            |---|
!| NBOR           |---|
!| NDTBRK         |-->| NOMBRE DE SOUS-PAS DE TEMPS DE DEFERLEMENT
!| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
!| NOMVEB         |---|
!| NOMVEF         |---|
!| NP             |-->| NOMBRE DE POINTS DU CHAMP DE VENT
!| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
!| NSITS          |-->| NOMBRE DE PAS DE TEMPS D'INTEGRATION
!| NVEB           |---|
!| NVEF           |---|
!| PROINF         |-->| INDICATEUR DE PROFONDEUR INFINIE
!| QINDI          |---|
!| RAISF          |-->| RAISON FREQUENTIELLE POUR DISCRETISATION
!| RFMLTA         |---|
!| ROAIR          |-->| MASSE VOLUMIQUE DE L AIR
!| ROEAU          |-->| MASSE VOLUMIQUE DE L EAU
!| SBREK          |-->| INDICATEUR DE TYPE DE TERME DEFERLEMENT
!| SFROT          |-->| INDICATEUR DE TYPE DE TERME FROTTEMENT
!| SINTET         |---|
!| SMOUT          |-->| INDICATEUR DE TYPE DE TERME MOUTONNEMENT
!| STRIA          |---|
!| STRIF          |-->| INDICATEUR DE TYPE DE TERME INTERACTIONS
!| SVENT          |-->| INDICATEUR DE TYPE DE TERME INPUT PAR VENT
!| TAILF          |-->| FACTEUR DE QUEUE DU SPECTRE
!| TAUWAV         |---|
!| TAUX1          |---|
!| TAUX2          |---|
!| TAUX3          |---|
!| TAUX4          |---|
!| TAUX5          |---|
!| TAUX6          |---|
!| TAUX7          |---|
!| TETA           |---|
!| TNEW           |---|
!| TOLD           |---|
!| TPROP          |-->| DATE DE FIN DE L'ETAPE D'INTEGRATION
!| TRA01          |---|
!| TSDER          |---|
!| TSTOT          |---|
!| TV1            |<->| DATE DU CHAMP DE CHAMP 1
!| TV2            |<->| DATE DU CHAMP DE CHAMP 2
!| TWNEW          |---|
!| TWOLD          |---|
!| U1             |---|
!| U2             |---|
!| USNEW          |---|
!| USOLD          |---|
!| V1             |---|
!| V2             |---|
!| VARIAN         |---|
!| VENSTA         |---|
!| VENT           |-->| INDICATEUR DE PRISE EN COMPTE DE VENT
!| VENTX          |---|
!| VENTY          |---|
!| VX_CTE         |---|
!| VY_CTE         |---|
!| X              |---|
!| XDTBRK         |-->| PAS DE TEMPS POUR LE DEFERLEMENT
!| XK             |---|
!| XKAPPA         |-->| CONSTANTE DE VON KARMAN
!| XKMOY          |---|
!| XRELV          |---|
!| Y              |---|
!| YRELV          |---|
!| Z0NEW          |---|
!| Z0OLD          |---|
!| ZVENT          |-->| COTE A LAQUELLE EST MESURE LE VENT (M)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER          NPOIN2, NPLAN , NF    , NSITS , NPTFR , NVEB  ,
     &                 NVEF  , LIMIT ,
     &                 SMOUT , SFROT , SVENT , STRIF , SBREK , INDIC ,
     &                 IQBBJ , IHMBJ , IFRBJ , IWHTG , IFRTG , IFRRO ,
     &                 IEXPRO, IFRIH , NDTBRK, NP    , IDISRO, STRIA ,
     &                 NBOR(NPTFR)   , IANGNL(NPLAN,8)
      INTEGER          NBD   , QINDI(NBD)
      DOUBLE PRECISION TAILF , CFROT1, GRAVIT, RAISF , DTSI  , TPROP ,
     &                 CMOUT1, CMOUT2, DDC   , TV1   , TV2   , ZVENT ,
     &                 ROAIR , ROEAU , XKAPPA, BETAM , DECAL , CDRAG ,
     &                 ALPHA , GAMBJ1, GAMBJ2, ALFABJ, BORETG, GAMATG,
     &                 COEFHS, VX_CTE, VY_CTE, CIMPLI,
     &                 GAMARO, ALFARO, GAM2RO, EM2SIH, BETAIH, XDTBRK,
     &                 ALFLTA, RFMLTA, KSPB  , BDISPB, BDSSPB, F1
      DOUBLE PRECISION  DEPTH(NPOIN2), USNEW(NPOIN2) , USOLD(NPOIN2) ,
     &                 VARIAN(NPOIN2),  FMOY(NPOIN2) , XKMOY(NPOIN2) ,
     &                  TWOLD(NPOIN2), TWNEW(NPOIN2) , Z0OLD(NPOIN2) ,
     &                  Z0NEW(NPOIN2), VENTX(NPOIN2) , VENTY(NPOIN2) ,
     &                     U1(NPOIN2),    U2(NPOIN2) ,    V1(NPOIN2) ,
     &                     V2(NPOIN2),     X(NPOIN2) ,     Y(NPOIN2) ,
     &                 TAUWAV(NPOIN2), TAUX1(NPOIN2) , TAUX2(NPOIN2) ,
     &                  TAUX3(NPOIN2), TAUX4(NPOIN2) , TAUX5(NPOIN2) ,
     &                  TAUX6(NPOIN2), TAUX7(NPOIN2) , COEFNL(16)    ,
     &                    TETA(NPLAN), SINTET(NPLAN) , COSTET(NPLAN) ,
     &                    F(NPOIN2,NPLAN,NF),   XK(NPOIN2,NF)        ,
     &                    DF_LIM(NPOIN2,NF) ,
     &                 TSDER(NPOIN2,NPLAN,NF),TSTOT(NPOIN2,NPLAN,NF) ,
     &                 FREQ(NF), DFREQ(NF), XRELV(NP), YRELV(NP)     ,
     &                 TOLD(NPOIN2,NPLAN), TNEW(NPOIN2,NPLAN)
      DOUBLE PRECISION BETA(NPOIN2)
      DOUBLE PRECISION TRA01(NPOIN2,NPLAN)
      CHARACTER*144 NOMVEB, NOMVEF
      CHARACTER*3 BINVEN
      LOGICAL  PROINF, VENT , VENSTA
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER          ISITS , NPOIN3, NPOIN4, IFF   , IP    , JP    ,
     &                 IFCAR , MF1   , MF2   , MFMAX , IDT
      DOUBLE PRECISION AUX1  , AUX2  , AUX3  , AUX4  , COEF  , DFMAX ,
     &                 DEUPI , FM1   , FM2   , TDEB  , TFIN  , VITVEN,
     &                 VITMIN, HM0   , HM0MAX, DTN   , SUM   , AUXI  ,
     &                 USMIN
      CHARACTER*7      CHDON
!
!
      NPOIN3=NPOIN2*NPLAN
      NPOIN4=NPOIN3*NF
      DEUPI=2.D0*3.141592654D0
      VITMIN=1.D-3
!
!
!     -----------------------------------------------------------------
!     CHOPS THE SPECTRUM IN ACCORDANCE WITH THE BATHYMETRY
!     -----------------------------------------------------------------
      IF (.NOT.PROINF) THEN
!
!       0.1 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
!       --------------------------------------------
        CALL TOTNRJ
     &( VARIAN, F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2)
!
!       0.2 COMPUTES THE CORRECTION COEFFICIENT ON THE SPECTRUM
!       -------------------------------------------------------
!
        DO IP=1,NPOIN2
          HM0MAX=COEFHS*DEPTH(IP)
          HM0 =MAX(4.D0*DSQRT(VARIAN(IP)),1.D-20)
          TAUX1(IP)=MIN((HM0MAX/HM0)**2,1.D0)
        ENDDO
!
!       0.3 CORRECTS THE SPECTRUM
!       --------------------------
!
        DO IFF=1,NF
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              F(IP,JP,IFF)=F(IP,JP,IFF)*TAUX1(IP)
            ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!     ----------------------------------------------------------------
!     IF THE COMPUTATION INCLUDES STATIONARY WINDS, DUPLICATES THE
!     CONDITIONS AT THE START OF THE TIME STEP TO THE END OF THE TIME
!     STEP. (THIS IS BECAUSE ARRAYS TWNEW, USNEW AND Z0NEW ARE WORKING
!     ARRAYS USED IN DUMP2D BETWEEN 2 CALLS TO SEMIMP).
!     ----------------------------------------------------------------
      IF (VENT.AND.VENSTA) THEN
        DO IP=1,NPOIN2
          TWNEW(IP)=TWOLD(IP)
        ENDDO
        IF (SVENT.EQ.2) THEN
          DO IP=1,NPOIN2
            USNEW(IP)=USOLD(IP)
            Z0NEW(IP)=Z0OLD(IP)
          ENDDO
        ENDIF
      ENDIF
!
!
!     -----------------------------------------------------------------
!     START OF THE MAIN LOOP ON THE NUMBER OF TIME STEPS (NSITS)
!     FOR INTEGRATION OF THE SOURCE TERMS, BY PROPAGATION TIME STEP
!     -----------------------------------------------------------------
      DO 100 ISITS=1,NSITS
!
!
!       1. ASSIGNS THE START AND END DATES OF TIME STEP
!       =========================================================
        TDEB=TPROP-DBLE(NSITS-ISITS+1)*DTSI
        TFIN=TDEB+DTSI
!
!
!       2. UPDATES (IF HAS TO) THE WIND ARRAYS
!       =================================================
        IF (VENT.AND..NOT.VENSTA) THEN
!
!         2.1 UPDATES THE WIND FIELD FOR DATE TFIN
!         ---------------------------------------------------
          CHDON='VENT   '
          IF (NOMVEB(1:1).NE.' ') THEN
            CALL NOUDON
     &( VENTX , VENTY , X     , Y     , NPOIN2, NVEB  , BINVEN, NBOR  ,
     &  NPTFR , TFIN  , DDC   , TV1   , TV2   , NP    , XRELV , YRELV ,
     &  TOLD  , TNEW  , TRA01 , U1    , V1    , U2    , V2    ,
     &  INDIC , CHDON , 2 )
          ELSEIF (NOMVEF(1:1).NE.' ') THEN
            CALL NOUDON
     &( VENTX , VENTY , X     , Y     , NPOIN2, NVEF  , BINVEN, NBOR  ,
     &  NPTFR , TFIN  , DDC   , TV1   , TV2   , NP    , XRELV , YRELV ,
     &  TOLD  , TNEW  , TRA01 , U1    , V1    , U2    , V2    ,
     &  INDIC , CHDON , 2 )
          ELSE
            CALL ANAVEN
     &( VENTX , VENTY , X     , Y     , NPOIN2, TFIN  , DDC   , VX_CTE,
     &  VY_CTE)
          ENDIF
!
!         2.2 COMPUTES THE WIND DIRECTION
!         -----------------------------------
!
          DO IP=1,NPOIN2
            VITVEN=SQRT(VENTX(IP)**2+VENTY(IP)**2)
            IF (VITVEN.GT.VITMIN) THEN
              TWNEW(IP)=ATAN2(VENTX(IP),VENTY(IP))
            ELSE
              TWNEW(IP)=0.D0
            ENDIF
          ENDDO
!
!         2.3 COMPUTES THE FRICTION VELOCITIES AND ROUGHNESS LENGTHS
!         ------------------------------------------------------------
          IF (SVENT.EQ.2) CALL USTAR2
     &( USNEW , VENTX , VENTY , NPOIN2)
!
        ENDIF
!
        IF (VENT) THEN
          IF (SVENT.EQ.1) CALL USTAR1
     &( USNEW , Z0NEW , TAUWAV, VENTX , VENTY , CDRAG , ALPHA , XKAPPA,
     &  ZVENT , GRAVIT, NPOIN2)
        ENDIF
!
!
!       3. COMPUTES MEAN PARAMETERS FOR THE DIRECTIONAL SPECTRUM
!       =========================================================
!
!       3.1 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
!       --------------------------------------------
        CALL TOTNRJ
     &( VARIAN, F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2)
!
!       3.2 COMPUTES THE MEAN FREQUENCY OF THE SPECTRUM
!       ----------------------------------------------
        CALL FREMOY
     &( FMOY  , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  TAUX1 , TAUX2 )
!
!       3.3 COMPUTES THE MEAN WAVE NUMBER OF THE SPECTRUM
!       ---------------------------------------------
        CALL KMOYEN
     &( XKMOY , XK    , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN ,
     &  NPOIN2, TAUX1 , TAUX2 , TAUX3 )
!
!
!       4. COMPUTES THE CONTRIBUTIONS OF THE SOURCE TERMS FOR GENERATION,
!          WHITECAPPING AND INTERACTIONS BETWEEN QUADRUPLETS
!       =============================================================
!
!       4.1 INITIALISES THE ARRAYS FOR THE SOURCE TERMS
!       ----------------------------------------------------
        DO IFF=1,NF
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              TSTOT(IP,JP,IFF)=0.0D0
              TSDER(IP,JP,IFF)=0.0D0
            ENDDO
          ENDDO
        ENDDO
!
!       4.2 GENERATION BY WIND
!       ---------------------------
        IF (VENT) THEN
          IF (SVENT.EQ.1) THEN
            CALL QWIND1
     &( TSTOT , TSDER , F     , XK    , FREQ  , USOLD , USNEW , TWOLD ,
     &  TWNEW , Z0OLD , Z0NEW , TETA  , ROAIR , ROEAU , BETAM , XKAPPA,
     &  DECAL , GRAVIT, NF    , NPLAN , NPOIN2, CIMPLI, TOLD  , TNEW  ,
     &  TAUX1 , TAUX2 , TAUX3 , TAUX4 , TAUX5 , TAUX6 , TAUX7 )
            CALL STRESS
     &( TAUWAV, TSTOT , F     , USNEW , TWNEW , Z0NEW , FREQ  , DFREQ ,
     &  TETA  , SINTET, COSTET, ROAIR , ROEAU , XKAPPA, BETAM , DECAL ,
     &  GRAVIT, NPOIN2, NPLAN , NF    , TAUX1 , TAUX2 , TAUX3 )
          ELSE
            CALL QWIND2
     &( TSTOT , TSDER , F     , XK    , FREQ  , USOLD , USNEW , TWOLD ,
     &  TWNEW , TETA  , ROAIR , ROEAU , GRAVIT, NF    , NPLAN , NPOIN2,
     &  CIMPLI, TAUX1 , TAUX2 , TAUX3 , TAUX4 , TAUX5 )
          ENDIF
        ELSE
          DO IP=1,NPOIN2
            USNEW(IP)=0.D0
          ENDDO
        ENDIF
!
!       4.3 NON-LINEAR INTERACTIONS BETWEEN QUADRUPLETS
!       --------------------------------------------------------------
        IF (STRIF.EQ.1) CALL QNLIN1
     &( TSTOT , TSDER , IANGNL, COEFNL, NF    , NPLAN , F1    , RAISF ,
     &  TAILF , PROINF, NPOIN2, F     , DEPTH , XKMOY , TAUX1 , TAUX2 ,
     &  TAUX3 , TAUX4 , TAUX5 , TAUX6 )
!
!
!       4.4 WHITE-CAPPING DISSIPATION
!       -------------------------------------------------
        IF (SMOUT.EQ.1) CALL QMOUT1
     &( TSTOT , TSDER , F     , XK    , VARIAN, FREQ  , FMOY  , XKMOY ,
     &  PROINF, CMOUT1, CMOUT2, GRAVIT, NF    , NPLAN , NPOIN2, TAUX1 ,
     &  TAUX2 )
!
!       4.5 BOTTOM FRICTION DISSIPATION
!       -------------------------------------------
        IF ((SFROT.EQ.1).AND.(.NOT.PROINF)) CALL QFROT1
     &( TSTOT , TSDER , F     , XK    , DEPTH , CFROT1, GRAVIT, NF    ,
     &  NPLAN , NPOIN2, TAUX1 )
!.......4.6 COMPUTES THE LIMITING FACTOR OF GROWTH
!       ------------------------------------
!.......NO LIMITING FACTOR (VERY HIGH VALUE IN ACTUAL FACTS)
        IF (LIMIT.EQ.0) THEN
          AUXI=1.D99
          DO IFF=1,NF
            DO IP=1,NPOIN2
              DF_LIM(IP,IFF)=AUXI
            ENDDO
          ENDDO
!
!.......LIMITING FACTOR TAKEN FROM WAM-CYCLE 4
        ELSEIF (LIMIT.EQ.1) THEN
!          COEF=6.4D-7*GRAVIT**2*DTSI/1200.D0
          COEF=0.62D-4*DTSI/1200.D0
          DO IFF=1,NF
            AUXI=COEF/FREQ(IFF)**5
            DO IP=1,NPOIN2
              DF_LIM(IP,IFF)=AUXI
            ENDDO
          ENDDO
!
!.......LIMITING FACTOR FROM HERSBACH AND JANSSEN (1999), WITHOUT UETOILE
        ELSEIF (LIMIT.EQ.2) THEN
          COEF=3.0D-7*GRAVIT*FREQ(NF)*DTSI
          DO IFF=1,NF
            AUXI=COEF/FREQ(IFF)**4
            USMIN=GRAVIT*5.6D-3/FREQ(IFF)
            DO IP=1,NPOIN2
              DF_LIM(IP,IFF)=AUXI*MAX(USNEW(IP),USMIN)
            ENDDO
          ENDDO
        ENDIF
!
!
!       5. UPDATES THE SPECTRUM - TAKES THE SOURCE TERMS INTO ACCOUNT
!         (GENERATION, WHITECAPPING AND QUADRUPLET INTERACTIONS)
!       ==============================================================
!
        DO IFF=1,NF
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              DFMAX=DF_LIM(IP,IFF)
              AUX1=MAX( 1.D0-DTSI*TSDER(IP,JP,IFF)*CIMPLI , 1.D0 )
              AUX2=DTSI*TSTOT(IP,JP,IFF)/AUX1
              AUX3=MIN( ABS(AUX2) , DFMAX )
              AUX4=SIGN(AUX3,AUX2)
              F(IP,JP,IFF)=MAX( F(IP,JP,IFF)+AUX4 , 0.D0 )
            ENDDO
          ENDDO
        ENDDO
!
!
!       6. TREATS THE HIGH FREQUENCIES DIFFERENTLY
!       =======================================================
!
!       6.1 COMPUTES THE MEAN FREQUENCY OF THE SPECTRUM
!       ----------------------------------------------
        CALL FREMOY
     &( FMOY  , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  TAUX1 , TAUX2 )
!
        AUX1=GRAVIT/(7.D0*DEUPI*FREQ(1))
        AUX2=2.5D0/FREQ(1)
        AUX3=1.D0/LOG10(RAISF)
!
        DO IP=1,NPOIN2
!
!       6.2 COMPUTES THE LAST FREQUENCY OF THE DISCRETISED SPECTRUM.
!           THIS FREQUENCY IS THE MAXIMUM OF (FM1=4.*FPM ; FM2=2.5*FMOY).
!           ITS INDEX IS MFMAX.
!       -------------------------------------------------------------
          FM1 =AUX1/(USNEW(IP)+1.D-90)
          FM2 =AUX2*FMOY(IP)
          MF1=INT(AUX3*LOG10(FM1)+1.D0)
          MF2=INT(AUX3*LOG10(FM2)+1.D0)
          MFMAX=MIN( MAX(MF1,MF2) , NF )
!
!       6.3 MODIFIES THE HIGH FREQUENCY PART OF THE SPECTRUM
!           A DECREASE IN F**(-TAILF) IS IMPOSED BEYOND
!           FREQ(MFMAX).  (TAILF=5 IN WAM-CYCLE 4)
!       --------------------------------------------------------
          DO IFF=MFMAX+1,NF
            AUX4=(FREQ(MFMAX)/FREQ(IFF))**TAILF
            DO JP=1,NPLAN
              F(IP,JP,IFF)=AUX4*F(IP,JP,MFMAX)
            ENDDO
          ENDDO
        ENDDO
!
!       7. TAKES THE BREAKING SOURCE TERM INTO ACCOUNT
!       =================================================
!
        IF (((SBREK.GT.0).OR.(STRIA.GT.0)).AND.(.NOT.PROINF)) THEN
!
!         7.1 COMPUTES A REPRESENTATIVE FREQUENCY
!         ------------------------------------------
          IF ((SBREK.GT.0).AND.(SBREK.LT.5)) THEN
            IF (SBREK.EQ.1) IFCAR = IFRBJ
            IF (SBREK.EQ.2) IFCAR = IFRTG
            IF (SBREK.GE.3) IFCAR = IFRRO
            IF (SBREK.GE.4) IFCAR = IFRIH
!
            GOTO (751,752,753,754,755,756), IFCAR
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'FREQUENCE DE HOULE NON PREVUE......IFCAR=',
     &                     IFCAR
            ELSE
              WRITE(LU,*) 'WAVE FREQUENCY NOT EXPECTED......IFCAR=',
     &                     IFCAR
            ENDIF
            GOTO 759
!
!           MEAN FREQUENCY FMOY
!           - - - - - - - - - - - -
  751       CONTINUE
            DO IP=1,NPOIN2
              TAUX3(IP)=FMOY(IP)
            ENDDO
            GOTO 759
!
!           MEAN FREQUENCY F01
!           - - - - - - - - - - -
  752       CONTINUE
            CALL FREM01
     &( TAUX3 , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  TAUX1 , TAUX2 )
            GOTO 759
!
!           MEAN FREQUENCY F02
!           - - - - - - - - - - -
  753       CONTINUE
            CALL FREM02
     &( TAUX3 , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  TAUX1 , TAUX2 )
            GOTO 759
!
!           PEAK FREQUENCY (DISCRETE FREQUENCY WITH MAX VARIANCE)
!           - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  754       CONTINUE
            CALL FREPIC
     &( TAUX3 , F     , FREQ  , NF    , NPLAN , NPOIN2, TAUX1 , TAUX2 )
            GOTO 759
!
!           PEAK FREQUENCY (READ WITH EXPONENT 5)
!           - - - - - - - - - - - - - - - - - - - - - - - - - -
  755       CONTINUE
            CALL FPREAD
     &( TAUX3 , F     , FREQ  , DFREQ , NF    , NPLAN , NPOIN2, 5.D0  ,
     &  TAILF , TAUX1 , TAUX2 )
            GOTO 759
!
!           PEAK FREQUENCY (READ WITH EXPONENT 8)
!           - - - - - - - - - - - - - - - - - - - - - - - - - -
  756       CONTINUE
            CALL FPREAD
     &( TAUX3 , F     , FREQ  , DFREQ , NF    , NPLAN , NPOIN2, 8.D0  ,
     &  TAILF , TAUX1 , TAUX2 )
!
  759       CONTINUE
!
        ENDIF
!
!.........LOOP ON SUB-TIME STEPS FOR BREAKING
!         = = = = = = = = = = = = = = = = = = = = = = = = = = =
          SUM=(XDTBRK**NDTBRK-1.D0)/(XDTBRK-1.D0)
          DTN=DTSI/SUM
!
          DO 782 IDT=1,NDTBRK
!         7.2 INITIALISES THE ARRAYS FOR THE SOURCE-TERMS
!         ----------------------------------------------------
          DO IFF=1,NF
            DO JP=1,NPLAN
              DO IP=1,NPOIN2
                TSTOT(IP,JP,IFF)=0.0D0
              ENDDO
            ENDDO
          ENDDO
!
!         7.3 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
!         --------------------------------------------
          CALL TOTNRJ
     &( VARIAN, F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2)
!
!         7.4 COMPUTES THE WAVE BREAKING CONTRIBUTION
!         --------------------------------------
          GOTO (761,762,763,764), SBREK
          IF(SBREK.NE.0) THEN
           IF(LNG.EQ.1) THEN
           WRITE(LU,*) 'TYPE DE DEFERLEMENT NON IMPLANTE...SBREK=',SBREK
           WRITE(LU,*) 'PAS DE PRISE EN COMPTE DU DEFERLEMENT'
           ELSE
           WRITE(LU,*) 'BREAKING FORMULATION NOT PROGRAMMED...SBREK=',
     &                  SBREK
           WRITE(LU,*) 'NO CONSIDERATION OF THE DEPTH-INDUCED BREAKING'
           ENDIF
          ENDIF
          GOTO 769
!
!         7.4.1 BREAKING ACCORDING TO BATTJES AND JANSSEN (1978)
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  761     CONTINUE
          CALL QBREK1
     &( TSTOT , TSDER , F     , TAUX3 , VARIAN, DEPTH , ALFABJ, GAMBJ1,
     &  GAMBJ2, IQBBJ , IHMBJ , NF    , NPLAN , NPOIN2, BETA )
          GOTO 769
!
!         7.4.2 BREAKING ACCORDING TO THORNTON AND GUZA (1983)
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  762     CONTINUE
          CALL QBREK2
     &( TSTOT , TSDER , F     , TAUX3 , VARIAN, DEPTH , BORETG, GAMATG,
     &  IWHTG , NF    , NPLAN , NPOIN2, BETA )
          GOTO 769
!
!         7.4.3 BREAKING ACCORDING TO ROELVINK (1993)
!         - - - - - - - - - - - - - - - - - - - - - -
  763     CONTINUE
          CALL QBREK3
     &( TSTOT , TSDER , F     , TAUX3 , VARIAN, DEPTH , ALFARO, GAMARO,
     &  GAM2RO, IEXPRO, IDISRO, NF    , NPLAN , NPOIN2, BETA )
          GOTO 769
!
!         7.4.4 BREAKING ACCORDING TO IZUMIYA AND HORIKAWA (1984)
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  764     CONTINUE
          CALL QBREK4
     &( TSTOT , TSDER , F     , TAUX3 , VARIAN, DEPTH , BETAIH, EM2SIH,
     &  GRAVIT, NF    , NPLAN , NPOIN2, BETA )
          GOTO 769
!
  769     CONTINUE
!
!       7.5 NON-LINEAR INTERACTIONS BETWEEN FREQUENCY TRIPLETS
!       -----------------------------------------------------------
          IF (STRIA.EQ.1) THEN
            CALL FREMOY
     &( FMOY  , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  TAUX1 , TAUX2 )
            CALL QTRIA1
     &( F     , XK    , FREQ  , DEPTH , RAISF , GRAVIT, ALFLTA, RFMLTA,
     &  NF    , NPLAN , NPOIN2, TSTOT , TSDER , VARIAN, FMOY  )
!
        ELSEIF (STRIA.EQ.2) THEN
            CALL QTRIA2
     &( F     , XK    , FREQ  , DFREQ , DEPTH , TETA  , SINTET, COSTET ,
     &  KSPB  , BDISPB, BDSSPB, RAISF , GRAVIT, NF    , NPLAN , NPOIN2 ,
     &  NBD   , QINDI , TSTOT , TSDER )
        ENDIF
!
!         7.5 UPDATES THE SPECTRUM - TAKES THE BREAKING SOURCE TERM
!             INTO ACCOUNT (EXPLICIT EULER SCHEME)
!         ---------------------------------------------------------
!
        DO IFF=1,NF
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              F(IP,JP,IFF)=MAX(F(IP,JP,IFF)+DTN*TSTOT(IP,JP,IFF),0.D0)
            ENDDO
          ENDDO
        ENDDO
!
        DTN=DTN*XDTBRK
!
  782   CONTINUE
!
        ENDIF
!
!
!       8. TRANSFERS DATA FROM NEW TO OLD FOR THE NEXT TIME STEP
!       ==============================================================
        IF (VENT) THEN
          DO IP=1,NPOIN2
            USOLD(IP)=USNEW(IP)
            Z0OLD(IP)=Z0NEW(IP)
            TWOLD(IP)=TWNEW(IP)
          ENDDO
        ENDIF
!
!
  100 CONTINUE
!     -----------------------------------------------------------------
!     END OF THE MAIN LOOP ON THE NUMBER OF TIME STEPS (NSITS)
!     FOR INTEGRATION OF THE SOURCE TERMS, BY PROPAGATION TIME STEP
!     -----------------------------------------------------------------
!
      RETURN
      END