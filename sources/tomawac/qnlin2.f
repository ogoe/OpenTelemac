!                       *****************
                        SUBROUTINE QNLIN2
!                       *****************
!
     &( TSTOT , TSDER , IANGNL, COEFNL, NF    , NPLAN , F1    , RAISF ,
     &  TAILF , PROINF, NPOIN2, F     , DEPTH , XKMOY , TAUX1 , DFINI ,
     &  XCOEF )
!
!***********************************************************************
! TOMAWAC   V6P1                                   24/06/2011
!***********************************************************************
!
!brief   COMPUTES THE CONTRIBUTION OF THE NON-LINEAR INTERACTIONS
!+             SOURCE TERM (FREQUENCY QUADRUPLETS) USING THE MDIA METHOD
!+             ("MULTIPLE DISCRETE INTERACTION APPROXIMATION")
!+             PROPOSED BY TOLMAN (2004)
!
!+            PROCEDURE SPECIFIC TO THE CASE WHERE THE FREQUENCIES
!+                FOLLOW A GEOMETRICAL PROGRESSION AND THE DIRECTIONS
!+                ARE EVENLY DISTRIBUTED OVER [0;2.PI].
!
!note     THIS SUBROUTINE USES THE OUTPUT FROM 'PRENL2' TO OPTIMISE
!+          THE COMPUTATIONS FOR MDIA.
!
!reference   TOLMAN H.L. (2004):
!+             "INVERSE MODELING OF DISCRETE INTERACTION APPROXIMATIONS
!+             FOR NONLINEAR INTERACTIONS IN WIND WAVES". OCEAN
!+             MODELLING, 6, 405-422
!
!history  E. GAGNAIRE-RENOU
!+        04/2011
!+        V6P1
!+   CREATED
!
!history  G.MATTAROLO (EDF - LNHE)
!+        24/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COEFNL         |-->| COEFFICIENTS USED FOR DIA METHOD
!| DEPTH          |-->| WATER DEPTH
!| DFINI          |<->| WORK TABLE
!| F              |-->| DIRECTIONAL SPECTRUM
!| F1             |-->| FIRST DISCRETIZED FREQUENCY
!| IANGNL         |-->| ANGULAR INDICES TABLE
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| RAISF          |-->| FREQUENTIAL RATIO
!| TAILF          |-->| SPECTRUM QUEUE FACTOR
!| TAUX1          |<->| WORK TABLE
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| XCOEF          |-->| COEFFICIENT FOR MDIA METHOD
!| XKMOY          |-->| AVERAGE WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                                                     !
!  APPELS :    - PROGRAMME(S) APPELANT  :  SEMIMP                     !
!  ********    - PROGRAMME(S) APPELE(S) :  CQUEUE                     !
!
      USE DECLARATIONS_TOMAWAC, ONLY : GRAVIT
!                                                                     !
      USE INTERFACE_TOMAWAC, EX_QNLIN2 => QNLIN2
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    ::   NPOIN2, NPLAN , NF
      INTEGER, INTENT(IN)    ::   IANGNL(NPLAN,16)
      DOUBLE PRECISION, INTENT(IN)    ::  F1  , RAISF , TAILF, XCOEF
      DOUBLE PRECISION, INTENT(IN)    ::  F(NPOIN2,NPLAN,NF), COEFNL(32)
      DOUBLE PRECISION, INTENT(IN)    ::   DEPTH(NPOIN2), XKMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) ::  TAUX1(NPOIN2), DFINI(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) ::  TSTOT(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(INOUT) ::  TSDER(NPOIN2,NPLAN,NF)
      LOGICAL, INTENT(IN)    ::   PROINF
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IP    , JP    , JF    , JFMIN , JFMAX
      INTEGER  JFP1  , JFM2  , JFP3  , JFM4 ,
     & JF1_0 , JF1_1 , JF2_0 , JF2_1 , JF3_0 , JF3_1 , JF4_0 , JF4_1 ,
     & JB1_0 , JB1_1 , JB2_0 , JB2_1 , JB3_0 , JB3_1 , JB4_0 , JB4_1 ,
     & JP1P_0, JP1P_1, JP1M_0, JP1M_1, JP2P_0, JP2P_1, JP2M_0, JP2M_1,
     & JP3P_0, JP3P_1, JP3M_0, JP3M_1, JP4P_0, JP4P_1, JP4M_0, JP4M_1
      DOUBLE PRECISION US1PM4, US1MM4, US1PL4, US1ML4
      DOUBLE PRECISION FREQ  , XXFAC , TERM1 , W     ,
     & C01   , C02   , C03   , C04   , C05   , C06   , C07   , C08   ,
     & C09   , C10   , C11   , C12   , C13   , C14   , C15   , C16   ,
     & D01   , D02   , D03   , D04   , D05   , D06   , D07   , D08   ,
     & D09   , D10   , D11   , D12   , D13   , D14   , D15   , D16   ,
     & C01SQ , C02SQ , C03SQ , C04SQ , C05SQ , C06SQ , C07SQ , C08SQ ,
     & C09SQ , C10SQ , C11SQ , C12SQ , C13SQ , C14SQ , C15SQ , C16SQ ,
     & CF1_0 , CF1_1 , CF2_0 , CF2_1 , CF3_0 , CF3_1 , CF4_0 , CF4_1 ,
     & F1PLUS, F1MOIN, F2PLUS, F2MOIN, F3PLUS, F3MOIN, F4PLUS, F4MOIN,
     & S_1P2M, S_1M2P, S_3P4M, S_3M4P, P_1P2M, P_1M2P, P_3P4M, P_3M4P,
     & QNL_A , QNL_B , QNL_C , QNL_D , Q_APB , Q_APC , Q_BPD , Q_CPD
!
!
!.....RECOVERS THE COEFFICIENTS COMPUTED IN 'PRENL2'
!     """"""""""""""""""""""""""""""""""""""""""""""""""
      C01   = COEFNL( 1)
      C02   = COEFNL( 2)
      C03   = COEFNL( 3)
      C04   = COEFNL( 4)
      C05   = COEFNL( 5)
      C06   = COEFNL( 6)
      C07   = COEFNL( 7)
      C08   = COEFNL( 8)
      C09   = COEFNL(17)
      C10   = COEFNL(18)
      C11   = COEFNL(19)
      C12   = COEFNL(20)
      C13   = COEFNL(21)
      C14   = COEFNL(22)
      C15   = COEFNL(23)
      C16   = COEFNL(24)
!
      JFP1   = INT(COEFNL( 9)+1.D-7)
      JFM2   = INT(COEFNL(10)-1.D-7)
      JFP3   = INT(COEFNL(25)+1.D-7)
      JFM4   = INT(COEFNL(26)-1.D-7)
!
      US1PM4= COEFNL(11)
      US1MM4= COEFNL(12)
      US1PL4= COEFNL(27)
      US1ML4= COEFNL(28)
!
      JFMIN = MIN(NINT(COEFNL(13)),NINT(COEFNL(29)))
      JFMAX = MAX(NINT(COEFNL(14)),NINT(COEFNL(30)))
!
      C01SQ = C01*C01
      C02SQ = C02*C02
      C03SQ = C03*C03
      C04SQ = C04*C04
      C05SQ = C05*C05
      C06SQ = C06*C06
      C07SQ = C07*C07
      C08SQ = C08*C08
      C09SQ = C09*C09
      C10SQ = C10*C10
      C11SQ = C11*C11
      C12SQ = C12*C12
      C13SQ = C13*C13
      C14SQ = C14*C14
      C15SQ = C15*C15
      C16SQ = C16*C16
!
!.....CORRECTION FACTOR FOR FINITE WATER DEPTH
!     """"""""""""""""""""""""""""""""""""""""""""
      IF (.NOT.PROINF) THEN
!
        DO IP=1,NPOIN2
          TERM1 = MAX(0.75D0*DEPTH(IP)*XKMOY(IP),0.5D0)
          DFINI(IP) = 1.D0+(5.5D0/TERM1)*(1.D0-0.833D0*TERM1)
     &               *EXP(-1.25D0*TERM1)
        ENDDO
      ENDIF
!
!.....FIRST LOOP ON THE FREQUENCIES
!     """""""""""""""""""""""""""""""""""
      DO JF=JFMIN,JFMAX
!
!.......COMPUTES THE CONSIDERED FREQUENCY
!       """"""""""""""""""""""""""""""""""
        FREQ = F1*RAISF**(JF-1)
!
!.......GETS THE INDICES OF THE FREQUENCIES EITHER SIDE OF THE
!......."HIGHER" 1 FREQUENCY:
!       FREQ(JF1_0) < (1+MU).FREQ(JF) < FREQ(JF1_1)
!       """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        JF1_0=JF+JFP1
        JF1_1=JF1_0+1
!
!.......GETS THE INDICES OF THE FREQUENCIES EITHER SIDE OF THE
!......."LOWER" 2 FREQUENCY:
!       FREQ(JF2_0) < (1-MU).FREQ(JF) < FREQ(JF2_1)
!       """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        JF2_0=JF+JFM2-1
        JF2_1=JF2_0+1
!
!.......GETS THE INDICES OF THE FREQUENCIES EITHER SIDE OF THE
!......."HIGHER" 3 FREQUENCY:
!       FREQ(JF3_0) < (1+LAMBDA).FREQ(JF) < FREQ(JF3_1)
!       """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        JF3_0=JF+JFP3
        JF3_1=JF3_0+1
!
!.......GETS THE INDICES OF THE FREQUENCIES EITHER SIDE OF THE
!......."LOWER" 4 FREQUENCY:
!       FREQ(JF4_0) < (1-LAMBDA).FREQ(JF) < FREQ(JF4_1)
!       """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        JF4_0=JF+JFM4-1
        JF4_1=JF4_0+1
!
!.......LIMITS THE INDICES TO NF AND TAKES INTO ACCOUNT ANALYTICALLY
!       THE SPECTRUM TAIL (DECREASE IN -TAILF).
!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
        CALL CQUEUE( NF , RAISF , TAILF , JF1_1 , JB1_1 , CF1_1 )
        CALL CQUEUE( NF , RAISF , TAILF , JF1_0 , JB1_0 , CF1_0 )
        CALL CQUEUE( NF , RAISF , TAILF , JF2_1 , JB2_1 , CF2_1 )
        CALL CQUEUE( NF , RAISF , TAILF , JF2_0 , JB2_0 , CF2_0 )
        CALL CQUEUE( NF , RAISF , TAILF , JF3_1 , JB3_1 , CF3_1 )
        CALL CQUEUE( NF , RAISF , TAILF , JF3_0 , JB3_0 , CF3_0 )
        CALL CQUEUE( NF , RAISF , TAILF , JF4_1 , JB4_1 , CF4_1 )
        CALL CQUEUE( NF , RAISF , TAILF , JF4_0 , JB4_0 , CF4_0 )
!
!.......INTERPOLATION COEFFICIENTS FOR THE MODIFIED SPECTRUM
!       """""""""""""""""""""""""""""""""""""""""""""""""
        D01=C01*CF1_0*US1PM4
        D02=C02*CF1_0*US1PM4
        D03=C03*CF1_1*US1PM4
        D04=C04*CF1_1*US1PM4
        D05=C05*CF2_0*US1MM4
        D06=C06*CF2_0*US1MM4
        D07=C07*CF2_1*US1MM4
        D08=C08*CF2_1*US1MM4
        D09=C09*CF3_0*US1PL4
        D10=C10*CF3_0*US1PL4
        D11=C11*CF3_1*US1PL4
        D12=C12*CF3_1*US1PL4
        D13=C13*CF4_0*US1ML4
        D14=C14*CF4_0*US1ML4
        D15=C15*CF4_1*US1ML4
        D16=C16*CF4_1*US1ML4
!
!.......COMPUTES THE MULTIPLICATIVE COEFFICIENT (IN F**11) AND TAKES
!       INTO ACCOUNT THE CORRECTION TERM IN FINITE DEPTH
!       """""""""""""""""""""""""""""""""""""""""""""""""""""""
        XXFAC= 0.5D0 * XCOEF / GRAVIT**4 * FREQ**11
        IF (PROINF) THEN
          DO IP=1,NPOIN2
            TAUX1(IP) = XXFAC
          ENDDO
        ELSE
          DO IP=1,NPOIN2
            TAUX1(IP) = DFINI(IP)*XXFAC
          ENDDO
        ENDIF
!
!.......SECOND LOOP ON THE DIRECTIONS.
!       """"""""""""""""""""""""""""""""""""
        DO JP=1,NPLAN
          JP1P_0 = IANGNL(JP, 1)
          JP1P_1 = IANGNL(JP, 2)
          JP2M_0 = IANGNL(JP, 3)
          JP2M_1 = IANGNL(JP, 4)
          JP1M_0 = IANGNL(JP, 5)
          JP1M_1 = IANGNL(JP, 6)
          JP2P_0 = IANGNL(JP, 7)
          JP2P_1 = IANGNL(JP, 8)
          JP3P_0 = IANGNL(JP, 9)
          JP3P_1 = IANGNL(JP,10)
          JP4M_0 = IANGNL(JP,11)
          JP4M_1 = IANGNL(JP,12)
          JP3M_0 = IANGNL(JP,13)
          JP3M_1 = IANGNL(JP,14)
          JP4P_0 = IANGNL(JP,15)
          JP4P_1 = IANGNL(JP,16)
!
!
!........./---------------------------------------------------------------/
!........./ FREQUENCIES F1, F2, F3 AND F4 MAY HAVE ENERGY
!........./---------------------------------------------------------------/
!
          DO IP=1,NPOIN2
           F1PLUS = F(IP,JP1P_0,JB1_0)*D01 + F(IP,JP1P_1,JB1_0)*D02
     &            + F(IP,JP1P_0,JB1_1)*D03 + F(IP,JP1P_1,JB1_1)*D04
           F1MOIN = F(IP,JP1M_0,JB1_0)*D01 + F(IP,JP1M_1,JB1_0)*D02
     &            + F(IP,JP1M_0,JB1_1)*D03 + F(IP,JP1M_1,JB1_1)*D04
           F2PLUS = F(IP,JP2P_0,JB2_0)*D05 + F(IP,JP2P_1,JB2_0)*D06
     &            + F(IP,JP2P_0,JB2_1)*D07 + F(IP,JP2P_1,JB2_1)*D08
           F2MOIN = F(IP,JP2M_0,JB2_0)*D05 + F(IP,JP2M_1,JB2_0)*D06
     &            + F(IP,JP2M_0,JB2_1)*D07 + F(IP,JP2M_1,JB2_1)*D08
           F3PLUS = F(IP,JP3P_0,JB3_0)*D09 + F(IP,JP3P_1,JB3_0)*D10
     &            + F(IP,JP3P_0,JB3_1)*D11 + F(IP,JP3P_1,JB3_1)*D12
           F3MOIN = F(IP,JP3M_0,JB3_0)*D09 + F(IP,JP3M_1,JB3_0)*D10
     &            + F(IP,JP3M_0,JB3_1)*D11 + F(IP,JP3M_1,JB3_1)*D12
           F4PLUS = F(IP,JP4P_0,JB4_0)*D13 + F(IP,JP4P_1,JB4_0)*D14
     &            + F(IP,JP4P_0,JB4_1)*D15 + F(IP,JP4P_1,JB4_1)*D16
           F4MOIN = F(IP,JP4M_0,JB4_0)*D13 + F(IP,JP4M_1,JB4_0)*D14
     &            + F(IP,JP4M_0,JB4_1)*D15 + F(IP,JP4M_1,JB4_1)*D16
!
           S_1P2M = F1PLUS + F2MOIN
           S_1M2P = F1MOIN + F2PLUS
           S_3P4M = F3PLUS + F4MOIN
           S_3M4P = F3MOIN + F4PLUS
           P_1P2M = F1PLUS * F2MOIN
           P_1M2P = F1MOIN * F2PLUS
           P_3P4M = F3PLUS * F4MOIN
           P_3M4P = F3MOIN * F4PLUS
!
           W=TAUX1(IP)
           QNL_A=W*( P_1P2M*S_3P4M - P_3P4M*S_1P2M )
           QNL_B=W*( P_1P2M*S_3M4P - P_3M4P*S_1P2M )
           QNL_C=W*( P_1M2P*S_3P4M - P_3P4M*S_1M2P )
           QNL_D=W*( P_1M2P*S_3M4P - P_3M4P*S_1M2P )
!
           Q_APB = QNL_A + QNL_B
           Q_APC = QNL_A + QNL_C
           Q_BPD = QNL_B + QNL_D
           Q_CPD = QNL_C + QNL_D
!
!
           IF (JB4_0.EQ.JF4_0) THEN
!..........For the frequency index before F4(-) : configurations A and C
            TSTOT(IP,JP4M_0,JF4_0)=TSTOT(IP,JP4M_0,JF4_0) + Q_APC*C13
            TSTOT(IP,JP4M_1,JF4_0)=TSTOT(IP,JP4M_1,JF4_0) + Q_APC*C14
            TSDER(IP,JP4M_0,JF4_0)=TSDER(IP,JP4M_0,JF4_0)
     &      +(-F3PLUS*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C13SQ*US1ML4*W
            TSDER(IP,JP4M_1,JF4_0)=TSDER(IP,JP4M_1,JF4_0)
     &      +(-F3PLUS*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C14SQ*US1ML4*W
!..........For the frequency index before  F4(+) : configurations B and D
            TSTOT(IP,JP4P_0,JF4_0)=TSTOT(IP,JP4P_0,JF4_0) + Q_BPD*C13
            TSTOT(IP,JP4P_1,JF4_0)=TSTOT(IP,JP4P_1,JF4_0) + Q_BPD*C14
            TSDER(IP,JP4P_0,JF4_0)=TSDER(IP,JP4P_0,JF4_0)
     &      +(-F3MOIN*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C13SQ*US1ML4*W
            TSDER(IP,JP4P_1,JF4_0)=TSDER(IP,JP4P_1,JF4_0)
     &      +(-F3MOIN*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C14SQ*US1ML4*W
           ENDIF
!
           IF (JB4_1.EQ.JF4_1) THEN
!..........For the frequency index after F4(-) : configurations A and C
            TSTOT(IP,JP4M_0,JF4_1)=TSTOT(IP,JP4M_0,JF4_1) + Q_APC*C15
            TSTOT(IP,JP4M_1,JF4_1)=TSTOT(IP,JP4M_1,JF4_1) + Q_APC*C16
            TSDER(IP,JP4M_0,JF4_1)=TSDER(IP,JP4M_0,JF4_1)
     &      +(-F3PLUS*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C15SQ*US1ML4*W
            TSDER(IP,JP4M_1,JF4_1)=TSDER(IP,JP4M_1,JF4_1)
     &      +(-F3PLUS*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C16SQ*US1ML4*W
!..........For the frequency index after F4(+) : configurations B and D
            TSTOT(IP,JP4P_0,JF4_1)=TSTOT(IP,JP4P_0,JF4_1) + Q_BPD*C15
            TSTOT(IP,JP4P_1,JF4_1)=TSTOT(IP,JP4P_1,JF4_1) + Q_BPD*C16
            TSDER(IP,JP4P_0,JF4_1)=TSDER(IP,JP4P_0,JF4_1)
     &      +(-F3MOIN*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C15SQ*US1ML4*W
            TSDER(IP,JP4P_1,JF4_1)=TSDER(IP,JP4P_1,JF4_1)
     &      +(-F3MOIN*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C16SQ*US1ML4*W
           ENDIF
!
           IF (JB2_0.EQ.JF2_0) THEN
!..........For the frequency index before F2(-) : configurations A and B
            TSTOT(IP,JP2M_0,JF2_0)=TSTOT(IP,JP2M_0,JF2_0) - Q_APB*C05
            TSTOT(IP,JP2M_1,JF2_0)=TSTOT(IP,JP2M_1,JF2_0) - Q_APB*C06
            TSDER(IP,JP2M_0,JF2_0)=TSDER(IP,JP2M_0,JF2_0)
     &      -( F1PLUS*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C05SQ*US1MM4*W
            TSDER(IP,JP2M_1,JF2_0)=TSDER(IP,JP2M_1,JF2_0)
     &      -( F1PLUS*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C06SQ*US1MM4*W
!..........For the frequency index before F2(+) : configurations C and D
            TSTOT(IP,JP2P_0,JF2_0)=TSTOT(IP,JP2P_0,JF2_0) - Q_CPD*C05
            TSTOT(IP,JP2P_1,JF2_0)=TSTOT(IP,JP2P_1,JF2_0) - Q_CPD*C06
            TSDER(IP,JP2P_0,JF2_0)=TSDER(IP,JP2P_0,JF2_0)
     &      -( F1MOIN*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C05SQ*US1MM4*W
            TSDER(IP,JP2P_1,JF2_0)=TSDER(IP,JP2P_1,JF2_0)
     &      -( F1MOIN*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C06SQ*US1MM4*W
           ENDIF
!
           IF (JB2_1.EQ.JF2_1) THEN
!..........For the frequency index after F2(-) : configurations A and B
            TSTOT(IP,JP2M_0,JF2_1)=TSTOT(IP,JP2M_0,JF2_1) - Q_APB*C07
            TSTOT(IP,JP2M_1,JF2_1)=TSTOT(IP,JP2M_1,JF2_1) - Q_APB*C08
            TSDER(IP,JP2M_0,JF2_1)=TSDER(IP,JP2M_0,JF2_1)
     &      -( F1PLUS*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C07SQ*US1MM4*W
            TSDER(IP,JP2M_1,JF2_1)=TSDER(IP,JP2M_1,JF2_1)
     &      -( F1PLUS*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C08SQ*US1MM4*W
!..........For the frequency index after F2(+) : configurations C and D
            TSTOT(IP,JP2P_0,JF2_1)=TSTOT(IP,JP2P_0,JF2_1) - Q_CPD*C07
            TSTOT(IP,JP2P_1,JF2_1)=TSTOT(IP,JP2P_1,JF2_1) - Q_CPD*C08
            TSDER(IP,JP2P_0,JF2_1)=TSDER(IP,JP2P_0,JF2_1)
     &      -( F1MOIN*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C07SQ*US1MM4*W
            TSDER(IP,JP2P_1,JF2_1)=TSDER(IP,JP2P_1,JF2_1)
     &      -( F1MOIN*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C08SQ*US1MM4*W
           ENDIF
!
           IF (JB1_0.EQ.JF1_0) THEN
!..........For the frequency index before F1(+) : configurations A and B
            TSTOT(IP,JP1P_0,JF1_0)=TSTOT(IP,JP1P_0,JF1_0) - Q_APB*C01
            TSTOT(IP,JP1P_1,JF1_0)=TSTOT(IP,JP1P_1,JF1_0) - Q_APB*C02
            TSDER(IP,JP1P_0,JF1_0)=TSDER(IP,JP1P_0,JF1_0)
     &      -( F2MOIN*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C01SQ*US1PM4*W
            TSDER(IP,JP1P_1,JF1_0)=TSDER(IP,JP1P_1,JF1_0)
     &      -( F2MOIN*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C02SQ*US1PM4*W
!..........For the frequency index before F1(-) : configurations C and D
            TSTOT(IP,JP1M_0,JF1_0)=TSTOT(IP,JP1M_0,JF1_0) - Q_CPD*C01
            TSTOT(IP,JP1M_1,JF1_0)=TSTOT(IP,JP1M_1,JF1_0) - Q_CPD*C02
            TSDER(IP,JP1M_0,JF1_0)=TSDER(IP,JP1M_0,JF1_0)
     &      -( F2PLUS*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C01SQ*US1PM4*W
            TSDER(IP,JP1M_1,JF1_0)=TSDER(IP,JP1M_1,JF1_0)
     &      -( F2PLUS*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C02SQ*US1PM4*W
           ENDIF
!
           IF (JB1_1.EQ.JF1_1) THEN
!..........For the frequency index after F1(+) : configurations A and B
            TSTOT(IP,JP1P_0,JF1_1)=TSTOT(IP,JP1P_0,JF1_1) - Q_APB*C03
            TSTOT(IP,JP1P_1,JF1_1)=TSTOT(IP,JP1P_1,JF1_1) - Q_APB*C04
            TSDER(IP,JP1P_0,JF1_1)=TSDER(IP,JP1P_0,JF1_1)
     &      -( F2MOIN*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C03SQ*US1PM4*W
            TSDER(IP,JP1P_1,JF1_1)=TSDER(IP,JP1P_1,JF1_1)
     &      -( F2MOIN*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C04SQ*US1PM4*W
!..........For the frequency index after F1(-) : configurations C and D
            TSTOT(IP,JP1M_0,JF1_1)=TSTOT(IP,JP1M_0,JF1_1) - Q_CPD*C03
            TSTOT(IP,JP1M_1,JF1_1)=TSTOT(IP,JP1M_1,JF1_1) - Q_CPD*C04
            TSDER(IP,JP1M_0,JF1_1)=TSDER(IP,JP1M_0,JF1_1)
     &      -( F2PLUS*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C03SQ*US1PM4*W
            TSDER(IP,JP1M_1,JF1_1)=TSDER(IP,JP1M_1,JF1_1)
     &      -( F2PLUS*(S_3P4M+S_3M4P) - P_3P4M - P_3M4P)*C04SQ*US1PM4*W
           ENDIF
!
           IF (JB3_0.EQ.JF3_0) THEN
!..........For the frequency index before F3(+) : configurations A and C
            TSTOT(IP,JP3P_0,JF3_0)=TSTOT(IP,JP3P_0,JF3_0) + Q_APC*C09
            TSTOT(IP,JP3P_1,JF3_0)=TSTOT(IP,JP3P_1,JF3_0) + Q_APC*C10
            TSDER(IP,JP3P_0,JF3_0)=TSDER(IP,JP3P_0,JF3_0)
     &      +(-F4MOIN*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C09SQ*US1PL4*W
            TSDER(IP,JP3P_1,JF3_0)=TSDER(IP,JP3P_1,JF3_0)
     &      +(-F4MOIN*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C10SQ*US1PL4*W
!..........For the frequency index before F3(-) : configurations B and D
            TSTOT(IP,JP3M_0,JF3_0)=TSTOT(IP,JP3M_0,JF3_0) + Q_BPD*C09
            TSTOT(IP,JP3M_1,JF3_0)=TSTOT(IP,JP3M_1,JF3_0) + Q_BPD*C10
            TSDER(IP,JP3M_0,JF3_0)=TSDER(IP,JP3M_0,JF3_0)
     &      +(-F4PLUS*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C09SQ*US1PL4*W
            TSDER(IP,JP3M_1,JF3_0)=TSDER(IP,JP3M_1,JF3_0)
     &      +(-F4PLUS*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C10SQ*US1PL4*W
           ENDIF
!
           IF (JB3_1.EQ.JF3_1) THEN
!..........For the frequency index after F3(+) : configurations A and C
            TSTOT(IP,JP3P_0,JF3_1)=TSTOT(IP,JP3P_0,JF3_1) + Q_APC*C11
            TSTOT(IP,JP3P_1,JF3_1)=TSTOT(IP,JP3P_1,JF3_1) + Q_APC*C12
            TSDER(IP,JP3P_0,JF3_1)=TSDER(IP,JP3P_0,JF3_1)
     &      +(-F4MOIN*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C11SQ*US1PL4*W
            TSDER(IP,JP3P_1,JF3_1)=TSDER(IP,JP3P_1,JF3_1)
     &      +(-F4MOIN*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C12SQ*US1PL4*W
!..........For the frequency index after F3(-) : configurations B and D
            TSTOT(IP,JP3M_0,JF3_1)=TSTOT(IP,JP3M_0,JF3_1) + Q_BPD*C11
            TSTOT(IP,JP3M_1,JF3_1)=TSTOT(IP,JP3M_1,JF3_1) + Q_BPD*C12
            TSDER(IP,JP3M_0,JF3_1)=TSDER(IP,JP3M_0,JF3_1)
     &      +(-F4PLUS*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C11SQ*US1PL4*W
            TSDER(IP,JP3M_1,JF3_1)=TSDER(IP,JP3M_1,JF3_1)
     &      +(-F4PLUS*(S_1P2M+S_1M2P) + P_1P2M + P_1M2P)*C12SQ*US1PL4*W
           ENDIF
!
          ENDDO
!
        ENDDO
!
      ENDDO
!
      RETURN
      END
!=======================================================================
