!                    *****************
                     SUBROUTINE SPEINI
!                    *****************
!
     &( F     , SPEC  , FRA   , UV    , VV    , FREQ  , TETA  , GRAVIT,
     &  FREMAX, FETCH , SIGMAA, SIGMAB, GAMMA , FPIC  , HM0   , ALPHIL,
     &  TETA1 , SPRED1, TETA2 , SPRED2, XLAMDA, NPOIN2, NPLAN , NF    ,
     &  INISPE, E2FMIN, DEPTH , FRABI )
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE VARIANCE SPECTRUM.
!+
!+            SEVERAL OPTIONS ARE POSSIBLE DEPENDING ON THE VALUE
!+                TAKEN BY INISPE :
!+
!+            0. ZERO EVERYWHERE
!+
!+            1. JONSWAP-TYPE SPECTRUM AS A FUNCTION OF THE WIND
!+                  (ZERO IF WIND SPEED IS ZERO)
!+
!+            2. JONSWAP-TYPE SPECTRUM AS A FUNCTION OF THE WIND
!+                  (PARAMETRIC IF WIND SPEED IS ZERO)
!+
!+            3. PARAMETRIC JONSWAP-TYPE SPECTRUM
!
!history  M. BENOIT (EDF/DER/LNH)
!+        13/07/95
!+        V1P0
!+
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
!| ALPHIL         |-->| CONSTANTE DE PHILLIPS (ALPHA)
!| DEPTH          |---|
!| E2FMIN         |-->| SEUIL MINIMUM DE VARIANCE CONSIDERE
!| F              |---|
!| FETCH          |-->| FETCH MOYEN
!| FPIC           |-->| FREQUENCE DE PIC JONSWAP
!| FRA            |---|
!| FRABI          |---|
!| FREMAX         |-->| VALEUR MAXIMUM DE LA FREQUENCE DE PIC
!| FREQ           |---|
!| GAMMA          |-->| FACTEUR DE FORME DE PIC JONSWAP
!| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
!| HM0            |-->| HAUTEUR SIGNIFICATIVE JONSWAP
!| INISPE         |-->| INDICATEUR D'INITIALISATION DU SPECTRE
!| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
!| NPLAN          |-->| NOMBRE DE DIRECTIOSN DE DISCRETISATION
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
!| SIGMAA         |-->| VALEUR DE SIGMA JONSWAP POUR F
!| SIGMAB         |-->| VALEUR DE SIGMA JONSWAP POUR F > FP
!| SPEC           |---|
!| SPRED1         |-->| ETALEMENT DIRECTIONNEL 1 POUR FRA
!| SPRED2         |-->| ETALEMENT DIRECTIONNEL 2 POUR FRA
!| TETA           |---|
!| TETA1          |-->| DIRECTION PRINCIPALE 1 POUR FRA
!| TETA2          |-->| DIRECTION PRINCIPALE 2 POUR FRA
!| UV             |---|
!| VV             |---|
!| XLAMDA         |-->| FACTEUR DE PONDERATION POUR LA FRA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NPOIN2, NPLAN , NF    , INISPE, FRABI
      DOUBLE PRECISION GRAVIT, FREMAX, FETCH , SIGMAA, SIGMAB, GAMMA
      DOUBLE PRECISION FPIC  , HM0   , ALPHIL, TETA1 , SPRED1, TETA2
      DOUBLE PRECISION SPRED2, XLAMDA, E2FMIN
      DOUBLE PRECISION FREQ(NF) , TETA(NPLAN), SPEC(NF), FRA(NPLAN)
      DOUBLE PRECISION UV(NPOIN2) , VV(NPOIN2), DEPTH(NPOIN2)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  NPOIN4, IP    , JF    , JP
      DOUBLE PRECISION GX    , GXU   , UG     , AL    , FP    , DEUPI
      DOUBLE PRECISION UVMIN  , COEFA , COEFB , COEFD
      DOUBLE PRECISION COEFE , UVENT , FPMIN  , SPR1  , SPR2  , XLAM
      DOUBLE PRECISION TET1  , TET2  , COEF
!
!
      DEUPI = 6.283185307D0
      NPOIN4= NPOIN2*NPLAN*NF
      UVMIN = 1.D-6
      COEFA = 2.84D0
      COEFB = 0.033D0
      COEFD =-3.D0/10.D0
      COEFE = 2.D0/3.D0
      GX    = GRAVIT*FETCH
      FPMIN = 1.D-4
!
!
!     ===========================================================
!     INITIAL SPECTRUM IS ZERO EVERYWHERE (INISPE=0)
!     (ALSO WORKS TO INITIALISE TO OTHER VALUES)
!     ===========================================================
      CALL OV ( 'X=C     ' , F      , F , F , 0.D0 , NPOIN4 )
      IF (INISPE.EQ.0) RETURN
!
!
!     ==/ INISPE = 1 /===========================================
!     IF NON ZERO WIND -E(F): JONSWAP FUNCTION OF THE WIND (AL,FP)
!                      -FRA : UNIMODAL ABOUT TETA(WIND)
!     IF ZERO WIND     -E(F): ZERO
!                      -FRA : ZERO
!     ===========================================================
      IF (INISPE.EQ.1) THEN
!
        DO 100 IP=1,NPOIN2
          UVENT=SQRT(UV(IP)**2+VV(IP)**2)
          IF (UVENT.GT.UVMIN) THEN
!
!...........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!           """""""""""""""""""""""""""""""""""""""""
            GXU=GX/(UVENT*UVENT)
            UG = UVENT/GRAVIT
            FP = MAX(0.13D0,COEFA*GXU**COEFD)
            FP = MIN(FP,FREMAX*UG)
            AL = MAX(0.0081D0, COEFB*FP**COEFE)
            FP = FP/UG
            CALL SPEJON
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
!
!...........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!           """""""""""""""""""""""""""""""""""""""""""""""
            SPR1=SPRED1
            TET1=ATAN2(UV(IP),VV(IP))
            SPR2=1.D0
            TET2=0.D0
            XLAM=1.D0
            IF(FRABI.EQ.2) THEN
              CALL FSPRD2
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
            ELSEIF(FRABI.EQ.3) THEN
              CALL FSPRD3
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
            ELSE
              CALL FSPRD1
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
            ENDIF
!
!...........COMPUTES THE DIRECTIONAL SPECTRUM
!           """""""""""""""""""""""""""""""
            DO 140 JF=1,NF
              DO 150 JP=1,NPLAN
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  150         CONTINUE
  140       CONTINUE
          ENDIF
!
  100   CONTINUE
!
!     ==/ INISPE = 2 /===========================================
!     IF NON ZERO WIND -E(F): JONSWAP AS A FUNCTION OF THE WIND (AL,FP)
!                      -FRA : UNIMODAL ABOUT TETA(WIND)
!     IF ZERO WIND     -E(F): PARAMETERISED JONSWAP (AL,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     ===========================================================
      ELSEIF (INISPE.EQ.2) THEN
!
        DO 200 IP=1,NPOIN2
          UVENT=SQRT(UV(IP)**2+VV(IP)**2)
!
!.........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!         """""""""""""""""""""""""""""""""""""""""
          IF (UVENT.GT.UVMIN) THEN
            GXU=GX/(UVENT*UVENT)
            UG = UVENT/GRAVIT
            FP = MAX(0.13D0,COEFA*GXU**COEFD)
            FP = MIN(FP,FREMAX*UG)
            AL = MAX(0.0081D0, COEFB*FP**COEFE)
            FP = FP/UG
          ELSE
            AL=ALPHIL
            FP=FPIC
          ENDIF
          CALL SPEJON
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
!
!.........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!         """""""""""""""""""""""""""""""""""""""""""""""
          IF (UVENT.GT.UVMIN) THEN
            TET1=ATAN2(UV(IP),VV(IP))
          ELSE
            TET1=TETA1
          ENDIF
          SPR1=SPRED1
          SPR2=1.D0
          TET2=0.D0
          XLAM=1.D0
          IF(FRABI.EQ.2) THEN
            CALL FSPRD2
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ELSEIF(FRABI.EQ.3) THEN
            CALL FSPRD3
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ELSE
            CALL FSPRD1
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ENDIF
!
!.........COMPUTES THE DIRECTIONAL SPECTRUM
!         """""""""""""""""""""""""""""""
          DO 240 JF=1,NF
            DO 250 JP=1,NPLAN
              F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  250       CONTINUE
  240     CONTINUE
!
  200   CONTINUE
!
!     ==/ INISPE = 3 /===========================================
!     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (AL,FP)
!                      -FRA : UNIMODAL ABOUT TETA(WIND)
!     IF ZERO WIND     -E(F): ZERO
!                      -FRA : ZERO
!     ===========================================================
      ELSEIF (INISPE.EQ.3) THEN
!
        DO 300 IP=1,NPOIN2
          UVENT=SQRT(UV(IP)**2+VV(IP)**2)
          IF (UVENT.GT.UVMIN) THEN
!
!...........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!           """""""""""""""""""""""""""""""""""""""""
            AL = ALPHIL
            FP = FPIC
            CALL SPEJON
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
!
!...........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!           """""""""""""""""""""""""""""""""""""""""""""""
            SPR1=SPRED1
            TET1=ATAN2(UV(IP),VV(IP))
            SPR2=1.D0
            TET2=0.D0
            XLAM=1.D0
            IF(FRABI.EQ.2) THEN
              CALL FSPRD2
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
            ELSEIF(FRABI.EQ.3) THEN
              CALL FSPRD3
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
            ELSE
              CALL FSPRD1
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
            ENDIF
!
!...........COMPUTES THE DIRECTIONAL SPECTRUM
!           """""""""""""""""""""""""""""""
            DO 340 JF=1,NF
              DO 350 JP=1,NPLAN
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  350         CONTINUE
  340       CONTINUE
          ENDIF
!
  300   CONTINUE
!
!     ==/ INISPE = 4 /===========================================
!     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (AL,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     IF ZERO WIND     -E(F): PARAMETERISED JONSWAP (AL,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     ===========================================================
      ELSEIF (INISPE.EQ.4) THEN
!
        DO 400 IP=1,NPOIN2
!
!.........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!         """""""""""""""""""""""""""""""""""""""""
          AL = ALPHIL
          FP = FPIC
          CALL SPEJON
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
!
!.........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!         """""""""""""""""""""""""""""""""""""""""""""""
          SPR1=SPRED1
          TET1=TETA1
          SPR2=SPRED2
          TET2=TETA2
          XLAM=XLAMDA
          IF(FRABI.EQ.2) THEN
            CALL FSPRD2
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ELSEIF(FRABI.EQ.3) THEN
            CALL FSPRD3
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ELSE
            CALL FSPRD1
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ENDIF
!
!.........COMPUTES THE DIRECTIONAL SPECTRUM
!         """""""""""""""""""""""""""""""
          DO 440 JF=1,NF
            DO 450 JP=1,NPLAN
              F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  450       CONTINUE
  440     CONTINUE
!
  400   CONTINUE
!
!     ==/ INISPE = 5 /===========================================
!     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (HM0,FP)
!                      -FRA : UNIMODAL ABOUT TETA(WIND)
!     IF ZERO WIND     -E(F): ZERO
!                      -FRA : ZERO
!     ===========================================================
      ELSEIF (INISPE.EQ.5) THEN
!
        COEF=0.0624D0/(0.230D0+0.0336D0*GAMMA-0.185D0/(1.9D0+GAMMA))
     &      *(DEUPI*FPIC)**4*HM0*HM0/GRAVIT**2
!
        DO 500 IP=1,NPOIN2
          UVENT=SQRT(UV(IP)**2+VV(IP)**2)
          IF (UVENT.GT.UVMIN) THEN
!
!...........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!           """""""""""""""""""""""""""""""""""""""""
            AL=COEF
            FP = FPIC
            CALL SPEJON
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
!
!...........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!           """""""""""""""""""""""""""""""""""""""""""""""
            SPR1=SPRED1
            TET1=ATAN2(UV(IP),VV(IP))
            SPR2=1.D0
            TET2=0.D0
            XLAM=1.D0
            IF(FRABI.EQ.2) THEN
              CALL FSPRD2
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
            ELSEIF(FRABI.EQ.3) THEN
              CALL FSPRD3
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
            ELSE
              CALL FSPRD1
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
            ENDIF
!
!...........COMPUTES THE DIRECTIONAL SPECTRUM
!           """""""""""""""""""""""""""""""
            DO 540 JF=1,NF
              DO 550 JP=1,NPLAN
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  550         CONTINUE
  540       CONTINUE
          ENDIF
!
  500   CONTINUE
!
!     ==/ INISPE = 6 /===========================================
!     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (HM0,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     IF ZERO WIND     -E(F): PARAMETERISED JONSWAP (HM0,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     ===========================================================
      ELSEIF (INISPE.EQ.6) THEN
!
        COEF=0.0624D0/(0.230D0+0.0336D0*GAMMA-0.185D0/(1.9D0+GAMMA))
     &      *(DEUPI*FPIC)**4*HM0*HM0/GRAVIT**2
!
        DO 600 IP=1,NPOIN2
!
!.........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!         """""""""""""""""""""""""""""""""""""""""
          AL = COEF
          FP = FPIC
          CALL SPEJON
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
!
!.........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!         """""""""""""""""""""""""""""""""""""""""""""""
          SPR1=SPRED1
          TET1=TETA1
          SPR2=SPRED2
          TET2=TETA2
          XLAM=XLAMDA
          IF(FRABI.EQ.2) THEN
            CALL FSPRD2
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ELSEIF(FRABI.EQ.3) THEN
            CALL FSPRD3
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ELSE
            CALL FSPRD1
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ENDIF
!
!.........COMPUTES THE DIRECTIONAL SPECTRUM
!         """""""""""""""""""""""""""""""
          DO 640 JF=1,NF
            DO 650 JP=1,NPLAN
              F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  650       CONTINUE
  640     CONTINUE
!
  600   CONTINUE
!
!     ==/ INISPE = 7 /===========================================
!     IF NON ZERO WIND -E(F): PARAMETERISED TMA (HM0,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     IF ZERO WIND     -E(F): PARAMETERISED TMA (HM0,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     ===========================================================
      ELSEIF (INISPE.EQ.7) THEN
!
        COEF=0.0624D0/(0.230D0+0.0336D0*GAMMA-0.185D0/(1.9D0+GAMMA))
     &      *(DEUPI*FPIC)**4*HM0*HM0/GRAVIT**2
!
        DO 700 IP=1,NPOIN2
!
!.........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!         """""""""""""""""""""""""""""""""""""""""
          AL = COEF
          FP = FPIC
!
          CALL SPETMA
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN , DEPTH(IP) )
!
!.........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!         """""""""""""""""""""""""""""""""""""""""""""""
          SPR1=SPRED1
          TET1=TETA1
          SPR2=SPRED2
          TET2=TETA2
          XLAM=XLAMDA
          IF(FRABI.EQ.2) THEN
            CALL FSPRD2
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ELSEIF(FRABI.EQ.3) THEN
            CALL FSPRD3
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ELSE
            CALL FSPRD1
     &( FRA   , TETA  , NPLAN , SPR1  , TET1  , SPR2  , TET2  , XLAM  ,
     &  DEUPI )
          ENDIF
!
!.........COMPUTES THE THE DIRECTIONAL SPECTRUM
!         """""""""""""""""""""""""""""""
          DO 740 JF=1,NF
            DO 750 JP=1,NPLAN
              F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  750       CONTINUE
  740     CONTINUE
!
  700   CONTINUE
      ENDIF
!
      RETURN
      END