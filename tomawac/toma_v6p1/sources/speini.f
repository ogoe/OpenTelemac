C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE VARIANCE SPECTRUM.
!><br>            SEVERAL OPTIONS ARE POSSIBLE DEPENDING ON THE VALUE
!>                TAKEN BY INISPE :
!><br>            0. ZERO EVERYWHERE
!><br>            1. JONSWAP-TYPE SPECTRUM AS A FUNCTION OF THE WIND
!>                  (ZERO IF WIND SPEED IS ZERO)
!><br>            2. JONSWAP-TYPE SPECTRUM AS A FUNCTION OF THE WIND
!>                  (PARAMETRIC IF WIND SPEED IS ZERO)
!><br>            3. PARAMETRIC JONSWAP-TYPE SPECTRUM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALPHIL, DEPTH, E2FMIN, F, FETCH, FPIC, FRA, FRABI, FREMAX, FREQ, GAMMA, GRAVIT, HM0, INISPE, NF, NPLAN, NPOIN2, SIGMAA, SIGMAB, SPEC, SPRED1, SPRED2, TETA, TETA1, TETA2, UV, VV, XLAMDA
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AL, COEF, COEFA, COEFB, COEFD, COEFE, DEUPI, FP, FPMIN, GX, GXU, IP, JF, JP, NPOIN4, SPR1, SPR2, TET1, TET2, UG, UVENT, UVMIN, XLAM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FSPRD1(), FSPRD2(), FSPRD3(), OV(), SPEJON(), SPETMA()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIW(), LIMWAC()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 1.0                                       </center>
!> </td><td> 13/07/95
!> </td><td> M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALPHIL
!></td><td>--></td><td>CONSTANTE DE PHILLIPS (ALPHA)
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>E2FMIN
!></td><td>--></td><td>SEUIL MINIMUM DE VARIANCE CONSIDERE
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td><--</td><td>SPECTRE DIRECTIONNEL DE VARIANCE
!>    </td></tr>
!>          <tr><td>FETCH
!></td><td>--></td><td>FETCH MOYEN
!>    </td></tr>
!>          <tr><td>FPIC
!></td><td>--></td><td>FREQUENCE DE PIC JONSWAP
!>    </td></tr>
!>          <tr><td>FRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FRA(
!></td><td><--</td><td>VECTEUR DE LA FONCTION DE REPARTITION ANG.
!>    </td></tr>
!>          <tr><td>FRABI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREMAX
!></td><td>--></td><td>VALEUR MAXIMUM DE LA FREQUENCE DE PIC
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>VECTEUR DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>GAMMA
!></td><td>--></td><td>FACTEUR DE FORME DE PIC JONSWAP
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>HM0
!></td><td>--></td><td>HAUTEUR SIGNIFICATIVE JONSWAP
!>    </td></tr>
!>          <tr><td>INISPE
!></td><td>--></td><td>INDICATEUR D'INITIALISATION DU SPECTRE
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIOSN DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL
!>    </td></tr>
!>          <tr><td>SIGMAA
!></td><td>--></td><td>VALEUR DE SIGMA JONSWAP POUR F
!>    </td></tr>
!>          <tr><td>SIGMAB
!></td><td>--></td><td>VALEUR DE SIGMA JONSWAP POUR F > FP
!>    </td></tr>
!>          <tr><td>SPEC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SPEC(
!></td><td><--</td><td>VECTEUR DU SPECTRE EN FREQUENCE
!>    </td></tr>
!>          <tr><td>SPRED1
!></td><td>--></td><td>ETALEMENT DIRECTIONNEL 1 POUR FRA
!>    </td></tr>
!>          <tr><td>SPRED2
!></td><td>--></td><td>ETALEMENT DIRECTIONNEL 2 POUR FRA
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA(
!></td><td>--></td><td>VECTEUR DES DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>TETA1
!></td><td>--></td><td>DIRECTION PRINCIPALE 1 POUR FRA
!>    </td></tr>
!>          <tr><td>TETA2
!></td><td>--></td><td>DIRECTION PRINCIPALE 2 POUR FRA
!>    </td></tr>
!>          <tr><td>UV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UV(
!></td><td>--></td><td>TABLEAU DES COMPOSANTES OUEST-EST DU VENT
!>    </td></tr>
!>          <tr><td>VV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VV(
!></td><td>--></td><td>TABLEAU DES COMPOSANTES SUD-NORD  DU VENT
!>    </td></tr>
!>          <tr><td>XLAMDA
!></td><td>--></td><td>FACTEUR DE PONDERATION POUR LA FRA
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SPEINI
     &( F     , SPEC  , FRA   , UV    , VV    , FREQ  , TETA  , GRAVIT,
     &  FREMAX, FETCH , SIGMAA, SIGMAB, GAMMA , FPIC  , HM0   , ALPHIL,
     &  TETA1 , SPRED1, TETA2 , SPRED2, XLAMDA, NPOIN2, NPLAN , NF    ,
     &  INISPE, E2FMIN, DEPTH , FRABI )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALPHIL         |-->| CONSTANTE DE PHILLIPS (ALPHA)
C| DEPTH          |---| 
C| E2FMIN         |-->| SEUIL MINIMUM DE VARIANCE CONSIDERE
C| F             |---| 
C| F(             |<--| SPECTRE DIRECTIONNEL DE VARIANCE
C| FETCH          |-->| FETCH MOYEN
C| FPIC           |-->| FREQUENCE DE PIC JONSWAP
C| FRA            |---| 
C| FRA(           |<--| VECTEUR DE LA FONCTION DE REPARTITION ANG.
C| FRABI          |---| 
C| FREMAX         |-->| VALEUR MAXIMUM DE LA FREQUENCE DE PIC
C| FREQ           |---| 
C| FREQ(          |-->| VECTEUR DES FREQUENCES DE DISCRETISATION
C| GAMMA          |-->| FACTEUR DE FORME DE PIC JONSWAP
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| HM0            |-->| HAUTEUR SIGNIFICATIVE JONSWAP
C| INISPE         |-->| INDICATEUR D'INITIALISATION DU SPECTRE
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIOSN DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| SIGMAA         |-->| VALEUR DE SIGMA JONSWAP POUR F
C| SIGMAB         |-->| VALEUR DE SIGMA JONSWAP POUR F > FP
C| SPEC           |---| 
C| SPEC(          |<--| VECTEUR DU SPECTRE EN FREQUENCE
C| SPRED1         |-->| ETALEMENT DIRECTIONNEL 1 POUR FRA
C| SPRED2         |-->| ETALEMENT DIRECTIONNEL 2 POUR FRA
C| TETA           |---| 
C| TETA(          |-->| VECTEUR DES DIRECTIONS DE DISCRETISATION
C| TETA1          |-->| DIRECTION PRINCIPALE 1 POUR FRA
C| TETA2          |-->| DIRECTION PRINCIPALE 2 POUR FRA
C| UV             |---| 
C| UV(            |-->| TABLEAU DES COMPOSANTES OUEST-EST DU VENT
C| VV             |---| 
C| VV(            |-->| TABLEAU DES COMPOSANTES SUD-NORD  DU VENT
C| XLAMDA         |-->| FACTEUR DE PONDERATION POUR LA FRA
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NPOIN2, NPLAN , NF    , INISPE, FRABI
      DOUBLE PRECISION GRAVIT, FREMAX, FETCH , SIGMAA, SIGMAB, GAMMA
      DOUBLE PRECISION FPIC  , HM0   , ALPHIL, TETA1 , SPRED1, TETA2
      DOUBLE PRECISION SPRED2, XLAMDA, E2FMIN
      DOUBLE PRECISION FREQ(NF) , TETA(NPLAN), SPEC(NF), FRA(NPLAN)
      DOUBLE PRECISION UV(NPOIN2) , VV(NPOIN2), DEPTH(NPOIN2)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  NPOIN4, IP    , JF    , JP
      DOUBLE PRECISION GX    , GXU   , UG     , AL    , FP    , DEUPI
      DOUBLE PRECISION UVMIN  , COEFA , COEFB , COEFD
      DOUBLE PRECISION COEFE , UVENT , FPMIN  , SPR1  , SPR2  , XLAM
      DOUBLE PRECISION TET1  , TET2  , COEF
C
C
      DEUPI = 6.283185307D0
      NPOIN4= NPOIN2*NPLAN*NF
      UVMIN = 1.D-6
      COEFA = 2.84D0
      COEFB = 0.033D0
      COEFD =-3.D0/10.D0
      COEFE = 2.D0/3.D0
      GX    = GRAVIT*FETCH
      FPMIN = 1.D-4
C
C
C     ===========================================================
C     INITIAL SPECTRUM IS ZERO EVERYWHERE (INISPE=0)
C     (ALSO WORKS TO INITIALISE TO OTHER VALUES)
C     ===========================================================
      CALL OV ( 'X=C     ' , F      , F , F , 0.D0 , NPOIN4 )
      IF (INISPE.EQ.0) RETURN
C
C
C     ==/ INISPE = 1 /===========================================
C     IF NON ZERO WIND -E(F): JONSWAP FUNCTION OF THE WIND (AL,FP)
C                      -FRA : UNIMODAL ABOUT TETA(WIND)
C     IF ZERO WIND     -E(F): ZERO
C                      -FRA : ZERO
C     ===========================================================
      IF (INISPE.EQ.1) THEN
C
        DO 100 IP=1,NPOIN2
          UVENT=SQRT(UV(IP)**2+VV(IP)**2)
          IF (UVENT.GT.UVMIN) THEN
C
C...........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
C           """""""""""""""""""""""""""""""""""""""""
            GXU=GX/(UVENT*UVENT)
            UG = UVENT/GRAVIT
            FP = MAX(0.13D0,COEFA*GXU**COEFD)
            FP = MIN(FP,FREMAX*UG)
            AL = MAX(0.0081D0, COEFB*FP**COEFE)
            FP = FP/UG
            CALL SPEJON
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
C
C...........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
C           """""""""""""""""""""""""""""""""""""""""""""""
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
C
C...........COMPUTES THE DIRECTIONAL SPECTRUM
C           """""""""""""""""""""""""""""""
            DO 140 JF=1,NF
              DO 150 JP=1,NPLAN
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  150         CONTINUE
  140       CONTINUE
          ENDIF
C
  100   CONTINUE
C
C     ==/ INISPE = 2 /===========================================
C     IF NON ZERO WIND -E(F): JONSWAP AS A FUNCTION OF THE WIND (AL,FP)
C                      -FRA : UNIMODAL ABOUT TETA(WIND)
C     IF ZERO WIND     -E(F): PARAMETERISED JONSWAP (AL,FP)
C                      -FRA : PARAMETERISED UNIMODAL
C     ===========================================================
      ELSEIF (INISPE.EQ.2) THEN
C
        DO 200 IP=1,NPOIN2
          UVENT=SQRT(UV(IP)**2+VV(IP)**2)
C
C.........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
C         """""""""""""""""""""""""""""""""""""""""
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
C
C.........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
C         """""""""""""""""""""""""""""""""""""""""""""""
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
C
C.........COMPUTES THE DIRECTIONAL SPECTRUM
C         """""""""""""""""""""""""""""""
          DO 240 JF=1,NF
            DO 250 JP=1,NPLAN
              F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  250       CONTINUE
  240     CONTINUE
C
  200   CONTINUE
C
C     ==/ INISPE = 3 /===========================================
C     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (AL,FP)
C                      -FRA : UNIMODAL ABOUT TETA(WIND)
C     IF ZERO WIND     -E(F): ZERO
C                      -FRA : ZERO
C     ===========================================================
      ELSEIF (INISPE.EQ.3) THEN
C
        DO 300 IP=1,NPOIN2
          UVENT=SQRT(UV(IP)**2+VV(IP)**2)
          IF (UVENT.GT.UVMIN) THEN
C
C...........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
C           """""""""""""""""""""""""""""""""""""""""
            AL = ALPHIL
            FP = FPIC
            CALL SPEJON
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
C
C...........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
C           """""""""""""""""""""""""""""""""""""""""""""""
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
C
C...........COMPUTES THE DIRECTIONAL SPECTRUM
C           """""""""""""""""""""""""""""""
            DO 340 JF=1,NF
              DO 350 JP=1,NPLAN
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  350         CONTINUE
  340       CONTINUE
          ENDIF
C
  300   CONTINUE
C
C     ==/ INISPE = 4 /===========================================
C     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (AL,FP)
C                      -FRA : PARAMETERISED UNIMODAL
C     IF ZERO WIND     -E(F): PARAMETERISED JONSWAP (AL,FP)
C                      -FRA : PARAMETERISED UNIMODAL
C     ===========================================================
      ELSEIF (INISPE.EQ.4) THEN
C
        DO 400 IP=1,NPOIN2
C
C.........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
C         """""""""""""""""""""""""""""""""""""""""
          AL = ALPHIL
          FP = FPIC
          CALL SPEJON
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
C
C.........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
C         """""""""""""""""""""""""""""""""""""""""""""""
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
C
C.........COMPUTES THE DIRECTIONAL SPECTRUM
C         """""""""""""""""""""""""""""""
          DO 440 JF=1,NF
            DO 450 JP=1,NPLAN
              F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  450       CONTINUE
  440     CONTINUE
C
  400   CONTINUE
C
C     ==/ INISPE = 5 /===========================================
C     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (HM0,FP)
C                      -FRA : UNIMODAL ABOUT TETA(WIND)
C     IF ZERO WIND     -E(F): ZERO
C                      -FRA : ZERO
C     ===========================================================
      ELSEIF (INISPE.EQ.5) THEN
C
        COEF=0.0624D0/(0.230D0+0.0336D0*GAMMA-0.185D0/(1.9D0+GAMMA))
     &      *(DEUPI*FPIC)**4*HM0*HM0/GRAVIT**2
C
        DO 500 IP=1,NPOIN2
          UVENT=SQRT(UV(IP)**2+VV(IP)**2)
          IF (UVENT.GT.UVMIN) THEN
C
C...........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
C           """""""""""""""""""""""""""""""""""""""""
            AL=COEF
            FP = FPIC
            CALL SPEJON
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
C
C...........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
C           """""""""""""""""""""""""""""""""""""""""""""""
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
C
C...........COMPUTES THE DIRECTIONAL SPECTRUM
C           """""""""""""""""""""""""""""""
            DO 540 JF=1,NF
              DO 550 JP=1,NPLAN
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  550         CONTINUE
  540       CONTINUE
          ENDIF
C
  500   CONTINUE
C
C     ==/ INISPE = 6 /===========================================
C     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (HM0,FP)
C                      -FRA : PARAMETERISED UNIMODAL
C     IF ZERO WIND     -E(F): PARAMETERISED JONSWAP (HM0,FP)
C                      -FRA : PARAMETERISED UNIMODAL
C     ===========================================================
      ELSEIF (INISPE.EQ.6) THEN
C
        COEF=0.0624D0/(0.230D0+0.0336D0*GAMMA-0.185D0/(1.9D0+GAMMA))
     &      *(DEUPI*FPIC)**4*HM0*HM0/GRAVIT**2
C
        DO 600 IP=1,NPOIN2
C
C.........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
C         """""""""""""""""""""""""""""""""""""""""
          AL = COEF
          FP = FPIC
          CALL SPEJON
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN )
C
C.........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
C         """""""""""""""""""""""""""""""""""""""""""""""
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
C
C.........COMPUTES THE DIRECTIONAL SPECTRUM
C         """""""""""""""""""""""""""""""
          DO 640 JF=1,NF
            DO 650 JP=1,NPLAN
              F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  650       CONTINUE
  640     CONTINUE
C
  600   CONTINUE
C
C     ==/ INISPE = 7 /===========================================
C     IF NON ZERO WIND -E(F): PARAMETERISED TMA (HM0,FP)
C                      -FRA : PARAMETERISED UNIMODAL
C     IF ZERO WIND     -E(F): PARAMETERISED TMA (HM0,FP)
C                      -FRA : PARAMETERISED UNIMODAL
C     ===========================================================
      ELSEIF (INISPE.EQ.7) THEN
C
        COEF=0.0624D0/(0.230D0+0.0336D0*GAMMA-0.185D0/(1.9D0+GAMMA))
     &      *(DEUPI*FPIC)**4*HM0*HM0/GRAVIT**2
C
        DO 700 IP=1,NPOIN2
C
C.........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
C         """""""""""""""""""""""""""""""""""""""""
          AL = COEF
          FP = FPIC
C
          CALL SPETMA
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN , DEPTH(IP) )
C
C.........COMPUTES THE DIRECTIONAL SPREADING FUNCTION
C         """""""""""""""""""""""""""""""""""""""""""""""
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
C
C.........COMPUTES THE THE DIRECTIONAL SPECTRUM
C         """""""""""""""""""""""""""""""
          DO 740 JF=1,NF
            DO 750 JP=1,NPLAN
              F(IP,JP,JF)=SPEC(JF)*FRA(JP)
  750       CONTINUE
  740     CONTINUE
C
  700   CONTINUE
      ENDIF
C
      RETURN
      END
C
C#######################################################################
C