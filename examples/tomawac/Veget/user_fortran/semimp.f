!                    *****************
                     SUBROUTINE SEMIMP
!                    *****************
!
     &(F     ,XK    ,FREQ  ,DFREQ ,DEPTH ,VENTX ,VENTY ,X     ,Y     ,
     & NVEB  ,NVEF  ,NBOR  ,NPTFR ,DDC   ,TV1   ,TV2   ,
     & U1    ,V1    ,U2    ,V2    ,TETA  ,SINTET,COSTET,INDIC ,
     & TAILF ,RAISF ,GRAVIT,CFROT1,CMOUT1,CMOUT2,CMOUT3,CMOUT4,CMOUT5,
     & CMOUT6,TPROP ,DTSI  ,ROAIR ,ROEAU ,XKAPPA,BETAM ,DECAL ,CDRAG ,
     & ALPHA ,ZVENT ,NF    ,NPLAN ,NPOIN2,IANGNL,COEFNL,F1    ,NSITS ,
     & SMOUT ,SFROT ,SVENT ,LVENT ,STRIF ,VENT  ,VENSTA,VX_CTE,VY_CTE,
     & SBREK ,ALFABJ,GAMBJ1,GAMBJ2,IQBBJ ,IHMBJ ,IFRBJ ,BORETG,GAMATG,
     & IWHTG ,IFRTG ,ALFARO,GAMARO,GAM2RO,IDISRO,IEXPRO,IFRRO ,BETAIH,
     & EM2SIH,IFRIH ,COEFHS,XDTBRK,NDTBRK,STRIA ,ALFLTA,RFMLTA,KSPB  ,
     & BDISPB,BDSSPB,PROINF,DF_LIM,LIMIT ,CIMPLI,COEFWD,COEFWE,COEFWF,
     & COEFWH,NOMVEB,NOMVEF,BINVEN,NBD   ,QINDI ,TAUWAV,USOLD ,TWOLD ,
     & Z0OLD ,TSTOT ,TSDER ,TOLD  ,TNEW  ,VARIAN,FMOY  ,XKMOY ,USNEW ,
     & Z0NEW ,TWNEW ,TAUX1 ,TAUX2 ,TAUX3 ,TAUX4 ,TAUX5 ,TAUX6 ,TAUX7 ,
     & BETA  ,NQ_TE1,NQ_OM2,NF1   ,NF2   ,NT1   ,NCONF ,NCONFM,
     & SEUIL ,LBUF  ,DIMBUF,F_POIN,T_POIN,F_COEF,F_PROJ,TB_SCA,K_IF1 ,
     & K_1P  ,K_1M  ,K_IF2 ,K_IF3 ,K_1P2P,K_1P2M,K_1P3P,K_1P3M,K_1M2P,
     & K_1M2M,K_1M3P,K_1M3M,IDCONF,TB_V14,TB_V24,TB_V34,TB_TPM,TB_TMP,
     & TB_FAC,MDIA  ,IANMDI,COEMDI,NVWIN ,DIAGHF)
!
!***********************************************************************
! TOMAWAC   V6P2                                   27/06/2011
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
!history  E.G.RENOU (EDF), G.MATTAROLO (EDF)
!+        12/05/2011
!+        V6P1
!+   MODIFIED: integration of new source terms, developed by
!+   E.G. Renou.
!+     - modification of the variables in argument list
!+     - modification of the local variable declarations
!+     - modification concerning friction velocity and roughness
!+       length calculation
!+     - calls to subroutines QWINDL, QWIND3, QMOUT2, QNLIN2,
!+       QNLIN3
!
!history  G.MATTAROLO (EDF - LNHE)
!+        27/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  U.H.MERKEL
!+        27/06/2012
!+        V6P2
!+   Renamed SUM to SUME, due to NAG compiler
!
!history  E. GAGNAIRE-RENOU (EDF - LNHE)
!+        12/03/2013
!+        V6P3
!+   HF diagnostic tail is not necessarily imposed
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALFABJ         |-->| COEFFICIENT ALPHA OF BJ WAVE BREAKING MODEL
!| ALFARO         |-->| CONSTANTE ALPHA OF RO WAVE BREAKING MODEL
!| ALFLTA         |-->| COEFFICIENT ALPHA OF LTA TRIAD INTERACTION MODEL
!| ALPHA          |-->| CHARNOCK CONSTANT
!| BDISPB         |-->| LOWER DIRECTIONAL BOUND. OF SPB TRIAD MODEL
!| BDSSPB         |-->| UPPER DIRECTIONAL BOUND. OF SPB TRIAD MODEL
!| BETA           |<--| BREAKING WAVES COEFFICIENT
!| BETAIH         |-->| BETA0 CONSTANT OF BREAKING WAVES IH MODEL
!| BETAM          |-->| WIND GENERATION COEFFICIENT
!| BINVEN         |-->| WIND FILE BINARY
!| BORETG         |-->| COEFFICIENT B OF BREAKING WAVE TG MODEL
!| CDRAG          |-->| WIND DRAG COEFFICIENT
!| CFROT1         |-->| BOTTOM FRICTION COEFFICIENT
!| CIMPLI         |-->| IMPLICITATION COEFFICIENT FOR SOURCE TERM INTEG.
!| CMOUT1         |-->| WHITE CAPPING DISSIPATION COEFFICIENT
!| CMOUT2         |-->| WHITE CAPPING WEIGHTING COEFFICIENT
!| CMOUT3         |-->| WESTHUYSEN WHITE CAPPING DISSIPATION COEFFICIENT
!| CMOUT4         |-->| WESTHUYSEN SATURATION THRES. FOR THE DISSIPATION
!| CMOUT5         |-->| WESTHUYSEN WHITE CAPPING DISSIPATION COEFFICIENT
!| CMOUT6         |-->| WESTHUYSEN WHITE CAPPING WEIGHTING COEFFICIENT
!| COEFHS         |-->| MAXIMUM VALUE OF THE RATIO HM0 ON D
!| COEFNL         |-->| COEFFICIENTS USED FOR DIA METHOD
!| COEFWD         |-->| COEFFICIENT D OF YAN WIND GENERATION MODEL
!| COEFWE         |-->| COEFFICIENT E OF YAN WIND GENERATION MODEL
!| COEFWF         |-->| COEFFICIENT F OF YAN WIND GENERATION MODEL
!| COEFWH         |-->| COEFFICIENT H OF YAN WIND GENERATION MODEL
!| COEMDI         |-->| COEFFICIENTS USED FOR DIA METHOD
!| COSTET         |-->| COSINE OF TETA ANGLE
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| DECAL          |-->| SHIFT GROWING CURVE DUE TO WIND
!| DEPTH          |-->| WATER DEPTH
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| DF_LIM         |<--| WORK TABLE
!| DIMBUF         |-->| VARIABLE FOR SPECTRUM INTERPOLATION
!| DTSI           |-->| INTEGRATION TIME STEP (SECONDS)
!| EM2SIH         |-->| M2* CONSTANT OF BREAKING WAVES IH MODEL
!| F              |<->| DIRECTIONAL SPECTRUM
!| F1             |-->| MINIMAL DISCRETIZED FREQUENCY
!| F_COEF         |-->| WORK TABLE FOR SPECTRUM INTERPOLATION
!| F_POIN         |-->| WORK TABLE FOR SPECTRUM INTERPOLATION
!| F_PROJ         |-->| WORK TABLE FOR SPECTRUM INTERPOLATION
!| FMOY           |<--| MEAN FREQUENCIES F-10
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GAM2RO         |-->| GAMMA2 CONSTANT OF WAVE BREAKING RO MODEL
!| GAMARO         |-->| GAMMA CONSTANT OF WAVE BREAKING RO MODEL
!| GAMATG         |-->| GAMMA CONSTANT OF WAVE BREAKING TG MODEL
!| GAMBJ1         |-->| GAMMA1 CONSTANT OF WAVE BREAKING  BJ MODEL
!| GAMBJ2         |-->| GAMMA2 CONSTANT OF WAVE BREAKING BJ MODEL
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| IANGNL         |-->| ANGULAR INDICES TABLE
!| IANMDI         |-->| ANGULAR INDICES TABLE FOR MDIA
!| IDCONF         |-->| WORK TABLE
!| IDISRO         |-->| WAVE HEIGHT DISTRIBUTION SLECTION FOR RO MODEL
!| IEXPRO         |-->| EXPONENT OF WAVE HEIGHT DISTR. FOR RO MODEL
!| IFRBJ          |-->| CHARACTERISTIC FREQUENCY BJ WAVE BREKING MODEL
!| IFRIH          |-->| CHARACTERISTIC FREQUENCY IH WAVE BREKING MODEL
!| IFRRO          |-->| CHARACTERISTIC FREQUENCY RO WAVE BREKING MODEL
!| IFRTG          |-->| CHARACTERISTIC FREQUENCY TG WAVE BREKING MODEL
!| IHMBJ          |-->| DEPTH-INDUCED BREAKING CRITERIUM GIVING THE
!|                |   | BREAKING WAVE HEIGHT (BJ MODEL)
!| INDIC          |-->| FILE FORMAT
!| IQBBJ          |-->| SELECTED QB COMPUTATION METHOD FOR BJ MODEL
!| IWHTG          |-->| WEIGHT. FUN.SELECTION OF WAVE BREAKING TG MODEL
!| K_IF1          |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_1P           |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_1M           |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_IF2          |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_IF3          |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_1P2P         |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_1P2M         |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_1P3P         |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_1P3M         |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_1M2P         |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_1M2M         |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_1M3P         |-->| WORK TABLE FOR GQM QNL4 METHOD
!| K_1M3M         |-->| WORK TABLE FOR GQM QNL4 METHOD
!| KSPB           |-->| COEFFICIENT K OF SPB TRIAD INTERACTION MODEL
!| LIMIT          |-->| TYPE OF WAVE GROWTH LIMITER MODEL SELECTED
!| LBUF           |-->| VARIABLE FOR SPECTRUM INTERPOLATION
!| LVENT          |-->| LINEAR WAVE GROWTH MODEL SELECTION
!| MDIA           |-->| NUMBER OF CONFIGURATIONS FOR MDIA METHOD
!| NBD            |-->| NUMBER OF TRIAD CONFIGURATIONS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCONF          |-->| NUMBER OF RETAINED CONFIGURATIONS (GQM METHOD)
!| NCONFM         |-->| MAXIMUM NUMBER OF CONFIGURATIONS (GQM METHOD)
!| NDTBRK         |-->| NUMBER OF TIME STEPS FOR BREAKING SOURCE TERM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NF1            |-->| NUMBER OF INTEGRATION POINT ON OMEGA1
!| NF2            |-->| NUMBER OF INTEGRATION POINT ON OMEGA2
!| NOMVEB         |-->| NAME OF BINARY WIND DATA FILE
!| NOMVEF         |-->| NAME OF FORMATTED WIND DATA FILE
!| NP             |<->| NUMBER OF POINTS READ FROM THE WIND FILE
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NQ_TE1         |-->| SETTING FOR INTEGRATION ON THETA1 (GQM)
!| NQ_OM2         |-->| NUMBER OF INTEGRATION POINT ON OMEGA2
!| NSITS          |-->| NUMBER OF ITERATIONS FOR THE SOURCE TERMS
!| NT1            |-->| NUMBER OF INTEGRATION POINT ON TETA1
!| NVEB           |-->| LOGICAL UNIT N. OF BINARY WIND DATA FILE
!| NVEF           |-->| LOGICAL UNIT N. OF FORMATTED WIND DATA FILE
!| NVWIN          |-->| NUMBER OF VARIABLES IN THE WIND FILE.
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMEPTION
!| QINDI          |-->| CONFIGURATION INDEX
!| RAISF          |-->| FREQUENTIAL RATIO
!| RFMLTA         |-->| COEFFICIENT OF LTA TRIAD INTERACTION MODEL
!| ROAIR          |-->| AIR DENSITY
!| ROEAU          |-->| WATER DENSITY
!| SBREK          |-->| DEPTH-INDUCED BREAKING DISSIPATION MODEL
!| SEUIL          |-->| THRESHOLD0 FOR CONFIGURATIONS ELIMINATION (GQM)
!| SFROT          |-->| SELECTION OF THE BOTTOM FRICTION DISSIPATION
!| SINTET         |-->| SINE OF TETA ANGLE
!| SMOUT          |-->| SELECTIO OF WHITE CAPPING SOURCE TERM MODEL
!| STRIA          |-->| SELECTION OF THE TRIAD INTERACTION MODEL
!| STRIF          |-->| SELECTION OF QUADRUPLET INTERACTION MODEL
!| SVENT          |-->| SELECTION OF THE WIND GENERATION MODEL
!| T_POIN         |-->| WORK TABLE FOR SPECTRUM INTERPOLATION
!| TAILF          |-->| SPECTRUM TAIL FACTOR
!| DIAGHF         |-->| OPTION FOR SPECTRUM DIAGNOSTIC TAIL
!| TAUWAV         |<->| STRESS DUE TO THE WAVES
!| TAUX1          |<->| WORK TABLE
!| TAUX2          |<->| WORK TABLE
!| TAUX3          |<->| WORK TABLE
!| TAUX4          |<->| WORK TABLE
!| TAUX5          |<->| WORK TABLE
!| TAUX6          |<->| WORK TABLE
!| TAUX7          |<->| WORK TABLE
!| TB_FAC         |-->| WORK TABLE (GQM)
!| TB_SCA         |-->| SCALE COEFFICIENT
!| TB_V14         |-->| WORK TABLE (GQM)
!| TB_V24         |-->| WORK TABLE (GQM)
!| TB_V34         |-->| WORK TABLE (GQM)
!| TB_TMP         |-->| WORK TABLE (GQM)
!| TB_TPM         |-->| WORK TABLE (GQM)
!| TETA           |---| DISCRETIZED DIRECTIONS
!| TNEW           |<->| WORK TABLE
!| TOLD           |<->| WORK TABLE
!| TPROP          |-->| COMPUTATION TIME
!| TSDER          |<--| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<--| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| TV1            |<->| TIME T1 IN THE WIND FILE
!| TV2            |<->| TIME T2 IN THE WIND FILE
!| TWNEW          |<->| WIND DIRECTION AT TIME N+1
!| TWOLD          |<->| WIND DIRECTION AT TIME N
!| U1,V1          |<->| WIND SPEED AT TIME T1 IN THE WIND FILE
!| U2,V2          |<->| WIND SPEED AT TIME T2 IN THE WIND FILE
!| USNEW          |<->| FRICTION VELOCITY AT TIME N+1
!| USOLD          |<->| FRICTION VELOCITY AT TIME N
!| VARIAN         |-->| SPECTRUM VARIANCE
!| VENSTA         |-->| INDICATES IF THE WIND IS STATIONARY
!| VENT           |-->| INDICATES IF WIND IS TAKEN INTO ACC
!| VENTX,VENTY    |<->| WIND DATA INTERPOLATED OVER 2D MESH
!| VX_CTE         |---| WIND ALONG X (CONSTANT VALUE IN STEERING FILE)
!| VY_CTE         |---| WIND ALONG Y (CONSTANT VALUE IN STEERING FILE)
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XDTBRK         |-->| COEFFICIENT OF TIME SUB-INCREMENTS FOR BREAKING
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKAPPA         |-->| VON KARMAN CONSTANT
!| XKMOY          |<--| AVERAGE WAVE NUMBER
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z0NEW          |<->| SURFACE ROUGHNESS LENGTH AT TIME N+1
!| Z0OLD          |<->| SURFACE ROUGHNESS LENGTH AT TIME N
!| ZVENT          |-->| WIND MEASUREMENT LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  APPELS :    - PROGRAMME(S) APPELANT  :  WAC
!  ********    - PROGRAMME(S) APPELE(S) :  USTAR1, TOTNRJ, ANAVEN,
!                                          FREMOY, KMOYEN, OV    ,
!                                          QWIND1, STRESS, QNLIN1,
!                                          QMOUT1, QFROT1, NOUDON,
!                                          QWIND2, USTAR2, QBREK1,
!                                          QBREK2, QBREK3, QBREK4,
!                                          FPREAD, FREM01, FREM02,
!                                          FPEPIC, QWINDL, QWIND3,
!                                          QMOUT2, QNLIN2, QNLIN3
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,T3_01,T3_02,TEXVEB,MESH,
     &                                 NAMEWX,NAMEWY,UNITVEB,PHASVEB
      USE INTERFACE_TOMAWAC, EX_SEMIMP => SEMIMP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPOIN2,NPLAN,NF,NSITS,NPTFR,NVEB,NVEF,LIMIT,NVWIN
      INTEGER SMOUT , SFROT , SVENT , STRIF , SBREK , INDIC
      INTEGER IQBBJ , IHMBJ , IFRBJ , IWHTG , IFRTG , IFRRO
      INTEGER IEXPRO, IFRIH , NDTBRK, IDISRO, STRIA
      INTEGER NBOR(NPTFR)   , IANGNL(NPLAN,8)
      INTEGER NBD   , QINDI(NBD) , DIAGHF
      DOUBLE PRECISION TAILF , CFROT1, GRAVIT, RAISF , DTSI  , TPROP
      DOUBLE PRECISION CMOUT1, CMOUT2, DDC   , TV1   , TV2   , ZVENT
      DOUBLE PRECISION ROAIR , ROEAU , XKAPPA, BETAM , DECAL , CDRAG
      DOUBLE PRECISION ALPHA , GAMBJ1, GAMBJ2, ALFABJ, BORETG, GAMATG
      DOUBLE PRECISION COEFHS, VX_CTE, VY_CTE, CIMPLI
      DOUBLE PRECISION GAMARO, ALFARO, GAM2RO, EM2SIH, BETAIH, XDTBRK
      DOUBLE PRECISION ALFLTA, RFMLTA, KSPB  , BDISPB, BDSSPB, F1
      DOUBLE PRECISION DEPTH(NPOIN2), USNEW(NPOIN2) , USOLD(NPOIN2)
      DOUBLE PRECISION VARIAN(NPOIN2),  FMOY(NPOIN2) , XKMOY(NPOIN2)
      DOUBLE PRECISION TWOLD(NPOIN2), TWNEW(NPOIN2) , Z0OLD(NPOIN2)
      DOUBLE PRECISION Z0NEW(NPOIN2), VENTX(NPOIN2) , VENTY(NPOIN2)
      DOUBLE PRECISION U1(NPOIN2),    U2(NPOIN2) ,    V1(NPOIN2)
      DOUBLE PRECISION V2(NPOIN2),     X(NPOIN2) ,     Y(NPOIN2)
      DOUBLE PRECISION TAUWAV(NPOIN2), TAUX1(NPOIN2) , TAUX2(NPOIN2)
      DOUBLE PRECISION TAUX3(NPOIN2), TAUX4(NPOIN2) , TAUX5(NPOIN2)
      DOUBLE PRECISION TAUX6(NPOIN2), TAUX7(NPOIN2) , COEFNL(16)
      DOUBLE PRECISION TETA(NPLAN), SINTET(NPLAN) , COSTET(NPLAN)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION DF_LIM(NPOIN2)
      DOUBLE PRECISION TSDER(NPOIN2,NPLAN,NF),TSTOT(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION FREQ(NF), DFREQ(NF)
      DOUBLE PRECISION TOLD(NPOIN2,NPLAN), TNEW(NPOIN2,NPLAN)
      DOUBLE PRECISION BETA(NPOIN2)
      CHARACTER(LEN=144) NOMVEB, NOMVEF
      CHARACTER(LEN=3) BINVEN
      LOGICAL  PROINF, VENT , VENSTA
!....Linear wind growth declaration
      INTEGER           LVENT
!....Yan expression declarations
      DOUBLE PRECISION  CMOUT3, CMOUT4, CMOUT5, CMOUT6
!....Westhuysen expression decalaration
      DOUBLE PRECISION  COEFWD, COEFWE, COEFWF, COEFWH
!....MDIA method declarations
      INTEGER           MDIA
      INTEGER           IANMDI(NPLAN,16,MDIA)
      DOUBLE PRECISION  COEMDI(32,MDIA)
!....GQM method declarations
      INTEGER  NQ_TE1, NQ_OM2, NF1, NF2 , NT1
      INTEGER  NCONF , NCONFM
      DOUBLE PRECISION SEUIL
      INTEGER  LBUF  , DIMBUF
      INTEGER           F_POIN(DIMBUF), T_POIN(DIMBUF)
      DOUBLE PRECISION  F_COEF(DIMBUF), F_PROJ(DIMBUF), TB_SCA(DIMBUF)
      INTEGER K_IF1 (1:NF1)
      INTEGER K_1P  (1:NT1,1:NF1), K_1M(1:NT1,1:NF1)
      INTEGER K_IF2 (1:NF2,1:NT1,1:NF1), K_IF3 (1:NF2,1:NT1,1:NF1)
      INTEGER K_1P2P(1:NF2,1:NT1,1:NF1), K_1P2M(1:NF2,1:NT1,1:NF1)
      INTEGER K_1P3P(1:NF2,1:NT1,1:NF1), K_1P3M(1:NF2,1:NT1,1:NF1)
      INTEGER K_1M2P(1:NF2,1:NT1,1:NF1), K_1M2M(1:NF2,1:NT1,1:NF1)
      INTEGER K_1M3P(1:NF2,1:NT1,1:NF1), K_1M3M(1:NF2,1:NT1,1:NF1)
      INTEGER IDCONF(1:NCONFM,1:3)
      DOUBLE PRECISION TB_V14(1:NF1)
      DOUBLE PRECISION TB_V24(1:NF2,1:NT1,1:NF1)
      DOUBLE PRECISION TB_V34(1:NF2,1:NT1,1:NF1)
      DOUBLE PRECISION TB_TPM(1:NF2,1:NT1,1:NF1)
      DOUBLE PRECISION TB_TMP(1:NF2,1:NT1,1:NF1)
      DOUBLE PRECISION TB_FAC(1:NF2,1:NT1,1:NF1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISITS,IFF,IP,JP,K,NVENT,IFCAR,MF1,MF2,MFMAX,IDT
      DOUBLE PRECISION AUX1,AUX2,AUX3,AUX4,COEF
      DOUBLE PRECISION FM1,FM2,TDEB,TFIN,VITVEN
      DOUBLE PRECISION VITMIN,HM0,HM0MAX,DTN,SUME,AUXI,USMIN
!                              MDIA, HERE HARDCODED
      DOUBLE PRECISION  XCCMDI(4)
!
      LOGICAL TROUVE(3)
!
!-----------------------------------------------------------------------
!
      VITMIN=1.D-3
!
!     ------------------------------------------------------------------
!     CHOPS THE SPECTRUM IN ACCORDANCE WITH THE BATHYMETRY
!     -----------------------------------------------------------------
!
      IF(.NOT.PROINF) THEN
!
!       0.1 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
!       -----------------------------------------------
!
        CALL TOTNRJ(VARIAN,F,FREQ,DFREQ,TAILF,NF,NPLAN,NPOIN2)
        WRITE(6,*)'VALEUR DE F = ',F(10,2,1)
!
!       0.2 COMPUTES THE CORRECTION COEFFICIENT ON THE SPECTRUM
!       -------------------------------------------------------
!
        DO IP=1,NPOIN2
          HM0MAX=COEFHS*DEPTH(IP)
          HM0 =MAX(4.D0*SQRT(VARIAN(IP)),1.D-20)
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
!
      IF(VENT.AND.VENSTA) THEN
        DO IP=1,NPOIN2
          TWNEW(IP)=TWOLD(IP)
        ENDDO
!
        IF (SVENT.GE.2.OR.(LVENT.EQ.1.AND.SVENT.NE.1).OR.
     &                                (SMOUT.EQ.2.AND.SVENT.NE.1)) THEN
          DO IP=1,NPOIN2
            USNEW(IP)=USOLD(IP)
            Z0NEW(IP)=Z0OLD(IP)
          ENDDO
        ENDIF
      ENDIF
!
!     -----------------------------------------------------------------
!     START OF THE MAIN LOOP ON THE NUMBER OF TIME STEPS (NSITS)
!     FOR INTEGRATION OF THE SOURCE TERMS, BY PROPAGATION TIME STEP
!     -----------------------------------------------------------------
!
      DO 100 ISITS=1,NSITS
!
!       1. ASSIGNS THE START AND END DATES OF TIME STEP
!       ===============================================
!
        TDEB=TPROP-(NSITS-ISITS+1)*DTSI
        TFIN=TDEB+DTSI
!
!
!       2. UPDATES (IF HAS TO) THE WIND ARRAYS
!       ======================================
!
        IF(VENT.AND..NOT.VENSTA) THEN
!
!         2.1 UPDATES THE WIND FIELD FOR DATE TFIN
!         ---------------------------------------------------
!
          IF(NOMVEB(1:1).NE.' '.OR.NOMVEF(1:1).NE.' ') THEN
            IF(NOMVEF(1:1).NE.' ') THEN
              NVENT=NVEF
            ELSE
              NVENT=NVEB
            ENDIF
            CALL NOUDON(VENTX,NAMEWX,
     &                        'WIND ALONG X    M/S             ',2,
     &                  VENTY,NAMEWY,
     &                        'WIND ALONG Y    M/S             ',2,
     &                  VENTY,'????????????????????????????????',
     &                        '????????????????????????????????',0,
     &                  MESH%X%R,MESH%Y%R,NPOIN2,
     &                  NVENT,BINVEN,NBOR,NPTFR,TFIN,DDC,TV1,TV2,
     &                  U1,U2,V1,V2,V1,V2,INDIC,
     &                  'WIND   ',NVWIN,TEXVEB,TROUVE,UNITVEB,PHASVEB)
          ELSE
            CALL ANAVEN(VENTX,VENTY,X,Y,NPOIN2,TFIN,DDC,VX_CTE,VY_CTE)
          ENDIF
!
!         2.2 COMPUTES THE WIND DIRECTION
!         -----------------------------------
!
          DO IP=1,NPOIN2
            VITVEN=SQRT(VENTX(IP)**2+VENTY(IP)**2)
            IF(VITVEN.GT.VITMIN) THEN
              TWNEW(IP)=ATAN2(VENTX(IP),VENTY(IP))
            ELSE
              TWNEW(IP)=0.D0
            ENDIF
          ENDDO
!
!         2.3 COMPUTES THE FRICTION VELOCITIES AND ROUGHNESS LENGTHS
!         ------------------------------------------------------------
!
          IF (SVENT.GE.2.OR.(LVENT.EQ.1.AND.SVENT.NE.1).OR.
     &                                    (SMOUT.EQ.2.AND.SVENT.NE.1))
     &                    CALL USTAR2( USNEW , VENTX , VENTY , NPOIN2)
        ENDIF
!
        IF(VENT.AND.SVENT.EQ.1) THEN
          CALL USTAR1(USNEW,Z0NEW,TAUWAV,VENTX,VENTY,
     &                CDRAG,ALPHA,XKAPPA,ZVENT,GRAVIT,NPOIN2)
        ENDIF
!
!
!       3. COMPUTES MEAN PARAMETERS FOR THE DIRECTIONAL SPECTRUM
!       =========================================================
!
!       3.1 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
!       -----------------------------------------------
!
        CALL TOTNRJ(VARIAN,F,FREQ,DFREQ,TAILF,NF,NPLAN,NPOIN2)
!
!       3.2 COMPUTES THE MEAN FREQUENCY OF THE SPECTRUM
!       -----------------------------------------------
!
        CALL FREMOY(FMOY,F,FREQ,DFREQ,TAILF,NF,NPLAN,NPOIN2,
     &              TAUX1,TAUX2)
!
!       3.3 COMPUTES THE MEAN WAVE NUMBER OF THE SPECTRUM
!       -------------------------------------------------
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
              TSTOT(IP,JP,IFF)=0.D0
              TSDER(IP,JP,IFF)=0.D0
            ENDDO
          ENDDO
        ENDDO
!
!       4.2 GENERATION BY WIND
!       ----------------------
!
        IF(VENT) THEN
          IF(SVENT.EQ.1) THEN
            CALL QWIND1
     &( TSTOT , TSDER , F     , XK    , FREQ  , USOLD , USNEW , TWOLD ,
     &  TWNEW , Z0OLD , Z0NEW , TETA  , ROAIR , ROEAU , BETAM , XKAPPA,
     &  DECAL , GRAVIT, NF    , NPLAN , NPOIN2, CIMPLI, TOLD  , TNEW  ,
     &  TAUX1 , TAUX2 , TAUX3 , TAUX4 , TAUX5 , TAUX6 , TAUX7 )
            CALL STRESS
     &( TAUWAV, TSTOT , F     , USNEW , TWNEW , Z0NEW , FREQ  , DFREQ ,
     &  TETA  , SINTET, COSTET, ROAIR , ROEAU , XKAPPA, BETAM , DECAL ,
     &  GRAVIT, NPOIN2, NPLAN , NF    , TAUX1 , TAUX2 , TAUX3 )
          ELSEIF(SVENT.EQ.2) THEN
            CALL QWIND2
     &( TSTOT , TSDER , F     , XK    , FREQ  , USOLD , USNEW , TWOLD ,
     &  TWNEW , TETA  , ROAIR , ROEAU , NF    , NPLAN , NPOIN2,
     &  CIMPLI, T3_01%R,T3_02%R )
          ELSEIF(SVENT.EQ.3) THEN
            CALL QWIND3
     &( TSTOT , TSDER , F     , XK    , FREQ  , USOLD , USNEW , TWOLD ,
     &  TWNEW , TETA  , GRAVIT, NF    , NPLAN , NPOIN2, CIMPLI, COEFWD,
     &  COEFWE, COEFWF, COEFWH, TAUX1 , TAUX2 , TAUX3 , TAUX4 )
!
          ENDIF
!
!       ADDS THE LINEAR WIND GROWTH SOURCE TERME
!       """""""""""""""""""""""""""""""""""""""
!
          IF(LVENT.EQ.1) THEN
            CALL QWINDL(TSTOT,FREQ,USOLD,USNEW,TWOLD,TWNEW,TETA,
     &                  NF,NPLAN,NPOIN2,CIMPLI,T3_01%R,T3_02%R,
     &                  TAUX5,TAUX6)
          ENDIF
!
        ELSE
!
          DO IP=1,NPOIN2
            USNEW(IP)=0.D0
          ENDDO
!
        ENDIF
!
!       4.3 NON-LINEAR INTERACTIONS BETWEEN QUADRUPLETS
!       -----------------------------------------------
!
        IF(STRIF.EQ.1) THEN
!
          CALL QNLIN1(TSTOT,TSDER,IANGNL,COEFNL,NF,NPLAN,F1,RAISF,
     &                TAILF,PROINF,NPOIN2,F,DEPTH,XKMOY,TAUX1,TAUX2,
     &                TAUX3,TAUX4,TAUX5,TAUX6)
!
        ELSEIF (STRIF.EQ.2) THEN
!
!         sets XCCMDI values for MDIA method
          XCCMDI(1)=8.360D7
          XCCMDI(2)=7.280D7
          XCCMDI(3)=3.340D7
          XCCMDI(4)=2.570D6
          DO K=1,MDIA
            XCCMDI(K)=XCCMDI(K)/DBLE(MDIA)
          ENDDO
!         alls MDIA method
          DO K=1,MDIA
            CALL QNLIN2
     &( TSTOT , TSDER , IANMDI(1,1,K) , COEMDI(1,K) , NF    , NPLAN,
     &  F1    , RAISF , TAILF , PROINF, NPOIN2, F   , DEPTH , XKMOY ,
     &  TAUX1 , TAUX2 , XCCMDI(K))
          ENDDO
!....calls GQM method
        ELSEIF (STRIF.EQ.3) THEN
          CALL QNLIN3
     &( TSTOT , TSDER , F     , NPOIN2, FREQ  , TETA  , NPLAN , NF    ,
     &  RAISF , TAILF , SEUIL , TAUX1 , LBUF  , DIMBUF, F_POIN, F_COEF,
     &  F_PROJ, T_POIN, TB_SCA, NQ_TE1, NQ_OM2, NF1   , NT1   , DFREQ ,
     &  K_IF1 , K_IF2 , K_IF3 , TB_V14, TB_V24, TB_V34, K_1P  , K_1M  ,
     &  K_1P2P, K_1P3M, K_1P2M, K_1P3P, K_1M2P, K_1M3M, K_1M2M, K_1M3P,
     &  TB_TPM, TB_TMP, TB_FAC, NCONF , NCONFM, IDCONF)
!GM Fin
        ENDIF
!
!       4.4 WHITE-CAPPING DISSIPATION
!       -------------------------------------------------
!
        IF(SMOUT.EQ.1) THEN
!
          CALL QMOUT1
     &( TSTOT , TSDER , F     , XK    , VARIAN, FREQ  , FMOY  , XKMOY ,
     &  PROINF, CMOUT1, CMOUT2, NF    , NPLAN , NPOIN2, TAUX1 , TAUX2 )
!
        ELSEIF(SMOUT.EQ.2) THEN
!
          CALL QMOUT2
     &( TSTOT , TSDER , F     , XK    , VARIAN, FREQ  , FMOY  , XKMOY ,
     &  USOLD , USNEW , DEPTH , PROINF, CMOUT3, CMOUT4, CMOUT5, CMOUT6,
     &  NF    , NPLAN , NPOIN2, CIMPLI, TAUX1 , TAUX2 , TAUX5 , TAUX6 )
!
        ENDIF
!
!       4.5 BOTTOM FRICTION DISSIPATION
!       -------------------------------
!
        IF(SFROT.EQ.1.AND..NOT.PROINF) CALL QFROT1
     &( TSTOT , TSDER , F     , XK    , DEPTH , CFROT1, GRAVIT, NF    ,
     &  NPLAN , NPOIN2, TAUX1 )
!
!       5. UPDATES THE SPECTRUM - TAKES THE SOURCE TERMS INTO ACCOUNT
!         (GENERATION, WHITECAPPING AND QUADRUPLET INTERACTIONS)
!       ==============================================================
!
!
!       JMH 04/12/2012: COMPUTATION OF LIMITING FACTOR INSERTED
!                       IN THE LOOP TO HAVE DF_LIM(NPOIN2) INSTEAD
!                       OF DF_LIM(NPOIN2,NF)
!
!
        DO IFF=1,NF
!         LIMITING FACTOR TAKEN FROM WAM-CYCLE 4
          IF(LIMIT.EQ.1) THEN
            COEF=0.62D-4*DTSI/1200.D0
            AUXI=COEF/FREQ(IFF)**5
            DO IP=1,NPOIN2
              DF_LIM(IP)=AUXI
            ENDDO
!         LIMITING FACTOR FROM HERSBACH AND JANSSEN (1999)
          ELSEIF(LIMIT.EQ.2) THEN
            COEF=3.D-7*GRAVIT*FREQ(NF)*DTSI
            AUXI=COEF/FREQ(IFF)**4
            USMIN=GRAVIT*5.6D-3/FREQ(IFF)
            DO IP=1,NPOIN2
              DF_LIM(IP)=AUXI*MAX(USNEW(IP),USMIN)
            ENDDO
          ELSEIF(LIMIT.NE.0) THEN
            WRITE(LU,*) 'UNKNOWN LIMITING FACTOR:',LIMIT
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(LIMIT.NE.0) THEN
            DO JP=1,NPLAN
              DO IP=1,NPOIN2
                AUX1=MAX(1.D0-DTSI*TSDER(IP,JP,IFF)*CIMPLI,1.D0)
                AUX2=DTSI*TSTOT(IP,JP,IFF)/AUX1
                AUX3=MIN(ABS(AUX2),DF_LIM(IP))
                AUX4=SIGN(AUX3,AUX2)
                F(IP,JP,IFF)=MAX(F(IP,JP,IFF)+AUX4,0.D0)
              ENDDO
            ENDDO
          ELSE
            DO JP=1,NPLAN
              DO IP=1,NPOIN2
                AUX1=MAX(1.D0-DTSI*TSDER(IP,JP,IFF)*CIMPLI,1.D0)
                AUX2=DTSI*TSTOT(IP,JP,IFF)/AUX1
                F(IP,JP,IFF)=MAX(F(IP,JP,IFF)+AUX2,0.D0)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
!
        IF(DIAGHF.EQ.1) THEN
!
!       6. TREATS THE HIGH FREQUENCIES DIFFERENTLY
!       =======================================================
!
!       6.1 COMPUTES THE MEAN FREQUENCY OF THE SPECTRUM
!       ----------------------------------------------
!
          CALL FREMOY(FMOY,F,FREQ,DFREQ,TAILF,NF,NPLAN,NPOIN2,
     &              TAUX1,TAUX2)
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
!
            FM1 =AUX1/MAX(USNEW(IP),1.D-90)
            FM2 =AUX2*FMOY(IP)
            MF1=INT(AUX3*LOG10(FM1)+1.D0)
            MF2=INT(AUX3*LOG10(FM2)+1.D0)
            MFMAX=MIN(MAX(MF1,MF2),NF)
!
!       6.3 MODIFIES THE HIGH FREQUENCY PART OF THE SPECTRUM
!           A DECREASE IN F**(-TAILF) IS IMPOSED BEYOND
!           FREQ(MFMAX).  (TAILF=5 IN WAM-CYCLE 4)
!       -------------------------------------------------------------
!
            DO IFF=MFMAX+1,NF
              AUX4=(FREQ(MFMAX)/FREQ(IFF))**TAILF
              DO JP=1,NPLAN
                F(IP,JP,IFF)=AUX4*F(IP,JP,MFMAX)
              ENDDO
            ENDDO
!
          ENDDO
        ELSEIF(DIAGHF.GE.2) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'OPTION POUR LA QUEUE DIAGNOSTIQUE'
            WRITE(LU,*) 'INCONNUE : DIAGHF=',DIAGHF
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'OPTION FOR DIAGNOSTIC TAIL'
            WRITE(LU,*) 'UNKNOWN: DIAGHF=',DIAGHF
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
!
!======================================================================
!VBA PRIS EN COMPTE VEGETATION
!======================================================================
        CALL QVEG1
     &( TSTOT , TSDER , F , VARIAN , DEPTH, FMOY ,
     &  XKMOY , NF    , NPLAN  , NPOIN2   , BETA  )
!       WRITE(6,*)'LA VALEUR DE TSDER EN SORTIE = ', TSDER(10,2,1)
!
!
!======================================================================
!VBA FIN PRISE EN COMTPE DE VEGETATION
!======================================================================
!
!       7. TAKES THE BREAKING SOURCE TERM INTO ACCOUNT
!       =================================================
!
        IF((SBREK.GT.0.OR.STRIA.GT.0).AND..NOT.PROINF) THEN
!
!         7.1 COMPUTES A REPRESENTATIVE FREQUENCY
!         ------------------------------------------
          IF (SBREK.GT.0.AND.SBREK.LT.5) THEN
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
!
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
          SUME=(XDTBRK**NDTBRK-1.D0)/(XDTBRK-1.D0)
          DTN=DTSI/SUME
!
          DO 782 IDT=1,NDTBRK
!         7.2 INITIALISES THE ARRAYS FOR THE SOURCE-TERMS
!         ----------------------------------------------------
          DO IFF=1,NF
            DO JP=1,NPLAN
              DO IP=1,NPOIN2
                TSTOT(IP,JP,IFF)=0.D0
              ENDDO
            ENDDO
          ENDDO
!
!         7.3 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
!         --------------------------------------------
!
          CALL TOTNRJ(VARIAN,F,FREQ,DFREQ,TAILF,NF,NPLAN,NPOIN2)
!
!
!         7.4 COMPUTES THE WAVE BREAKING CONTRIBUTION
!         --------------------------------------
!
!         7.4.1 BREAKING ACCORDING TO BATTJES AND JANSSEN (1978)
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
          IF(SBREK.EQ.1) THEN
!
          CALL QVEG1
     &( TSTOT , TSDER , F , VARIAN , DEPTH, FMOY ,
     &  XKMOY , NF    , NPLAN  , NPOIN2   , BETA  )
!          CALL QBREK1
!     &( TSTOT , TSDER , F     , TAUX3 , VARIAN, DEPTH , ALFABJ, GAMBJ1,
!     &  GAMBJ2, IQBBJ , IHMBJ , NF    , NPLAN , NPOIN2, BETA )
        WRITE(6,*) 'XKMOT en sortie=',XKMOY(10)
!
!
!         7.4.2 BREAKING ACCORDING TO THORNTON AND GUZA (1983)
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
          ELSEIF(SBREK.EQ.2) THEN
!
          CALL QBREK2
     &( TSTOT , TSDER , F     , TAUX3 , VARIAN, DEPTH , BORETG, GAMATG,
     &  IWHTG , NF    , NPLAN , NPOIN2, BETA )
!
!
!         7.4.3 BREAKING ACCORDING TO ROELVINK (1993)
!         - - - - - - - - - - - - - - - - - - - - - -
!
          ELSEIF(SBREK.EQ.3) THEN
!
          CALL QBREK3
     &( TSTOT , TSDER , F     , TAUX3 , VARIAN, DEPTH , ALFARO, GAMARO,
     &  GAM2RO, IEXPRO, IDISRO, NF    , NPLAN , NPOIN2, BETA )
!
!
!         7.4.4 BREAKING ACCORDING TO IZUMIYA AND HORIKAWA (1984)
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
          ELSEIF(SBREK.EQ.4) THEN
!
          CALL QBREK4
     &( TSTOT , TSDER , F     ,TAUX3,VARIAN,DEPTH,BETAIH,EM2SIH,
     &  GRAVIT, NF    , NPLAN , NPOIN2, BETA )
!
          ELSEIF(SBREK.NE.0) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'TYPE DE DEFERLEMENT NON IMPLANTE : ',SBREK
            ELSE
              WRITE(LU,*) 'BREAKING FORMULATION NOT PROGRAMMED: ',
     &                     SBREK
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
!
!       7.5 NON-LINEAR INTERACTIONS BETWEEN FREQUENCY TRIPLETS
!       -----------------------------------------------------------
          IF(STRIA.EQ.1) THEN
            CALL FREMOY
     &( FMOY  , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  TAUX1 , TAUX2 )
            CALL QTRIA1
     &( F     , XK    , FREQ  , DEPTH , RAISF , ALFLTA, RFMLTA,
     &  NF    , NPLAN , NPOIN2, TSTOT , TSDER , VARIAN, FMOY  )
!
        ELSEIF(STRIA.EQ.2) THEN
            CALL QTRIA2
     &( F     , XK    , FREQ  , DFREQ , DEPTH , TETA  , SINTET, COSTET ,
     &  KSPB  , BDISPB, BDSSPB, RAISF , NF    , NPLAN , NPOIN2 ,
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
        IF(VENT) THEN
          DO IP=1,NPOIN2
            USOLD(IP)=USNEW(IP)
            Z0OLD(IP)=Z0NEW(IP)
            TWOLD(IP)=TWNEW(IP)
          ENDDO
        ENDIF
!
!
  100 CONTINUE
!
!     -----------------------------------------------------------------
!     END OF THE MAIN LOOP ON THE NUMBER OF TIME STEPS (NSITS)
!     FOR INTEGRATION OF THE SOURCE TERMS, BY PROPAGATION TIME STEP
!     -----------------------------------------------------------------
!
      RETURN
      END

