!                    ****************
                     SUBROUTINE PHBOR
!                    ****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    TRANSLATES THE BOUNDARY CONDITIONS SPECIFIED
!+                BY THE USER,
!+                I.E. COMPUTES THE COEFFICIENTS
!+                APHIR, APHII, ... FOR EACH BOUNDARY SEGMENT.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/08/2000
!+        V5P1
!+
!
!history  C. DENIS (SINETICS)
!+        18/03/2010
!+        V6P0
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      LOGICAL TRVDEB
!
      INTEGER I,IPREC,IG,IG0,IGP1,IFR,IOIDEB(5),IOIFIN(5),ITOTO,IFROI
!
      DOUBLE PRECISION PI,DEGRAD
      DOUBLE PRECISION AUXI1,AUXI2,PHASOI,AUXIC,AUXIS,RADDEG,BID
!
      DOUBLE PRECISION, ALLOCATABLE ::  APHI1BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  BPHI1BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  CPHI1BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  DPHI1BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  APHI2BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  BPHI2BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  CPHI2BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  DPHI2BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  APHI3BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  BPHI3BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  CPHI3BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  DPHI3BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  APHI4BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  BPHI4BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  CPHI4BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  DPHI4BT(:)
!
      INTRINSIC COS,SIN
!
!-----------------------------------------------------------------------
!
      PARAMETER( PI = 3.1415926535897932384626433D0 , DEGRAD=PI/180.D0 )
      PARAMETER( RADDEG = 180.D0 / PI )
!
!-----------------------------------------------------------------------
!
! INITIALISES LIDIR TO KSORT (A DIFFERENT VALUE FROM KENT)
! IN ORDER NOT TO TAKE NODES IMPOSED IN PRIDIH INTO ACCOUNT,
! WHEN IT HAS NOT BEEN REQUESTED.
!
!
      IF (NCSIZE .GT. 1) THEN
      ALLOCATE(APHI1BT(NPTFR_TOT))
      ALLOCATE(BPHI1BT(NPTFR_TOT))
      ALLOCATE(CPHI1BT(NPTFR_TOT))
      ALLOCATE(DPHI1BT(NPTFR_TOT))
      ALLOCATE(APHI2BT(NPTFR_TOT))
      ALLOCATE(BPHI2BT(NPTFR_TOT))
      ALLOCATE(CPHI2BT(NPTFR_TOT))
      ALLOCATE(DPHI2BT(NPTFR_TOT))
      ALLOCATE(APHI3BT(NPTFR_TOT))
      ALLOCATE(BPHI3BT(NPTFR_TOT))
      ALLOCATE(CPHI3BT(NPTFR_TOT))
      ALLOCATE(DPHI3BT(NPTFR_TOT))
      ALLOCATE(APHI4BT(NPTFR_TOT))
      ALLOCATE(BPHI4BT(NPTFR_TOT))
      ALLOCATE(CPHI4BT(NPTFR_TOT))
      ALLOCATE(DPHI4BT(NPTFR_TOT))
      ALLOCATE(LIDIRT(2*NPTFR_TOT))
!
      DO I=1,MESH%NPTFR
         APHI1B%R(I) = 0.D0
         BPHI1B%R(I) = 0.D0
         CPHI1B%R(I) = 0.D0
         DPHI1B%R(I) = 0.D0
         APHI2B%R(I) = 0.D0
         BPHI2B%R(I) = 0.D0
         CPHI2B%R(I) = 0.D0
         DPHI2B%R(I) = 0.D0
         APHI3B%R(I) = 0.D0
         BPHI3B%R(I) = 0.D0
         CPHI3B%R(I) = 0.D0
         DPHI3B%R(I) = 0.D0
         APHI4B%R(I) = 0.D0
         BPHI4B%R(I) = 0.D0
         CPHI4B%R(I) = 0.D0
         DPHI4B%R(I) = 0.D0
      END DO
!
!
        DO I=1,NPTFR_TOT
           LIDIRT(I) = KSORT
!     BEWARE: IT IS ASSUMED HERE THAT NPTFRX=NPTFR
           LIDIRT(I+NPTFR_TOT) = KSORT
           IF (LIHBORT(I).EQ.KENT) THEN
              LIHBORT(I) = KINC
           ENDIF
           APHI1BT(I) = 0.D0
           BPHI1BT(I) = 0.D0
           CPHI1BT(I) = 0.D0
           DPHI1BT(I) = 0.D0
           APHI2BT(I) = 0.D0
           BPHI2BT(I) = 0.D0
           CPHI2BT(I) = 0.D0
           DPHI2BT(I) = 0.D0
           APHI3BT(I) = 0.D0
           BPHI3BT(I) = 0.D0
           CPHI3BT(I) = 0.D0
           DPHI3BT(I) = 0.D0
           APHI4BT(I) = 0.D0
           BPHI4BT(I) = 0.D0
           CPHI4BT(I) = 0.D0
           DPHI4BT(I) = 0.D0
        END DO
!
!
!-----------------------------------------------------------------------
!
!
!     ************************************************
!     INITIALISES THE PHASE FOR INCIDENT WAVES
!     ************************************************
!
      PHASOI = 0.D0
!
!     ******************************************
!     PARTICULAR TREATMENT FOR INCIDENT WAVES
!     ******************************************
!
!     -------------------------------------------------
!     LOCATES THE BEGINNINGS OF THE INCIDENT WAVE BOUNDARY
!     -------------------------------------------------
!
      TRVDEB = .TRUE.
      IFROI = 0
!
      DO 10 I=1,NPTFR_TOT
         IF (LIHBORT(I).EQ.KINC) THEN
            ITOTO = KP1BOR_TOT(I+NPTFR_TOT)
               IF (LIHBORT(ITOTO).NE.KINC) THEN
                  IFROI = IFROI + 1
                  IOIDEB(IFROI) = I
               ENDIF
         ENDIF
 10   CONTINUE
!
      IF(LNG.EQ.1) WRITE(LU,11) IFROI
      IF(LNG.EQ.2) WRITE(LU,12) IFROI
11    FORMAT(1X,'PHBOR : IL Y A : ',1I3,' FRONTIERE(S) ',
     &       1X,'DE TYPE ONDE INCIDENTE ')
12    FORMAT(1X,'PHBOR : THERE ARE :',1I3,' BOUNDARIE(S) ',
     &       1X,'OF INCIDENT WAVE TYPE ')
!
!     --------------------------------------------------------------
!     COMPUTES THE COEFFICIENTS FOR INCIDENT WAVE BOUNDARIES
!     FROM IOIDEB (BEGINNING OF INCIDENT WAVE)
!     --------------------------------------------------------------
!
!
      DO 15 IFR=1,IFROI
         I = IOIDEB(IFR)
!
 20   CONTINUE
!
!        ********************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE I
!        ********************************
!
!         IG   = MESH%NBOR%I(I)
      IG   = NBOR_TOT(I)
!
!        ******************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE PRECEDING I
!        ******************************************
!
      IG0  = NBOR_TOT(KP1BOR_TOT(I+NPTFR_TOT))
!
!        ****************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE FOLLOWING I
!        ****************************************
!
      IGP1 = NBOR_TOT(KP1BOR_TOT(I))
!
         AUXIC      = COS(TETABT(I)*DEGRAD)
         AUXIS      = SIN(TETABT(I)*DEGRAD)
         AUXI1      = GRAV/OMEGA * HBT(I)/2.D0 *
     &                CTT(IG) * CGT(IG) * KT(IG)
!
!
!           DEVELOPMENT FOR NON-UNIFORM DIRECTION
!           PHASOI IS THE PHASE
!
!           PREVIOUS FORMULATION : PHASE = K * X (PLANE WAVE) :
!
!                     PHASOI = KM * ( XM*AUXIC + YM*AUXIS )
!
!           NEW FORMULATION (NON PLANE WAVE) :
!
!                                   M
!                                 /
!                    PHASOI(M) = /        K(N) * DX(N)
!                               /
!                                MDEB
!
!           WHERE MDEB IF THE BEGINNING (NODE) OF AN INCIDENT WAVE SEGMENT,
!           FROM WHICH THE PHASE IS COMPUTED. THE ABOVE INTEGRAL IS
!           COMPUTED BY LINEAR APPROXIMATION.
!
!           THE POSSIBLE DEPHASING ALFAP IS ADDED TO ENSURE COHERENCE
!           BETWEEN THE PHASES OF THE DIFFERENT WAVE  CRESTS IF SEVERAL
!           NONCONTINUOUS INCIDENT WAVE BOUNDARIES EXIST
!
      PHASOI = PHASOI + KT(IG)*AUXIC*(XT(IG) - XT(IG0))
     &                + KT(IG)*AUXIS*(YT(IG) - YT(IG0))
!
      APHI1BT(I)  = - KT(IG) * CTT(IG) * CGT(IG)
     &             * COS(TETAPT(I)*DEGRAD)
!
      BPHI1BT(I)  = 0.D0
!
      CPHI1BT(I)  = AUXI1 * COS( PHASOI + ALFAPT(I)*DEGRAD )
!
      DPHI1BT(I)  = AUXI1 * SIN( PHASOI + ALFAPT(I)*DEGRAD )
!
      I = KP1BOR_TOT(I)
!
!     UNTIL THE NODE FOLLOWING THE END OF AN INCIDENT WAVE BND IS REACHED
!
      IF (LIHBORT(I).NE.KINC) THEN
         IOIFIN(IFR) = I
         IPREC      = KP1BOR_TOT(I+NPTFR_TOT)
         TETABT(I) = TETABT(IPREC)
         HBT(I)    = HBT(IPREC)
         AUXIC      = COS(TETABT(I)*DEGRAD)
         AUXIS      = SIN(TETABT(I)*DEGRAD)
         AUXI1      = GRAV/OMEGA * HBT(I)/2.D0 *
     &                CTT(IG) * CGT(IG) * KT(IG)
         PHASOI = PHASOI + KT(IG)*AUXIC*(XT(IG) - XT(IG0))
     &                   + KT(IG)*AUXIS*(YT(IG) - YT(IG0))
!
         APHI1BT(I) = - KT(IG) * CTT(IG) * CGT(IG)
     &                 * COS(TETAPT(IPREC)*DEGRAD)
!
         BPHI1BT(I) = 0.D0
!
         CPHI1BT(I) = AUXI1*COS(PHASOI + ALFAPT(IPREC)*DEGRAD)
!
         DPHI1BT(I) = AUXI1*SIN(PHASOI + ALFAPT(IPREC)*DEGRAD)
!
         GOTO 15
!
      ELSE
         GOTO 20
      ENDIF
!
15    CONTINUE
!
!     ******************************************
!     END OF TREATMENT OF THE INCIDENT WAVE BOUNDARY
!     ******************************************
!
      DO 100 I=1,NPTFR_TOT
!
!        ********************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE I
!        ********************************
!
         IG   = NBOR_TOT(I)
!
!        ******************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE PRECEDING I
!        ******************************************
!
         IG0  = NBOR_TOT(KP1BOR_TOT(I+NPTFR_TOT))
!
!        ****************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE FOLLOWING I
!        ****************************************
!
         IGP1 = NBOR_TOT(KP1BOR_TOT(I))
!CPHI1B%R
!        -------------------------------------------------
!        COEFFICIENTS FOR A FREE EXIT BOUNDARY SEGMENT
!        -------------------------------------------------
!
         IF (LIHBORT(I).EQ.KSORT) THEN
            APHI2BT(I)  = - KT(IG) * CTT(IG) * CGT(IG)
     &                   * COS(TETAPT(I)*DEGRAD)
!
            BPHI2BT(I)  = 0.D0
!
            CPHI2BT(I)  = 0.D0
!
            DPHI2BT(I)  = 0.D0
!
         ELSEIF (LIHBORT(KP1BOR_TOT(I)).EQ.KSORT) THEN
            APHI2BT(I)  = - KT(IG) * CTT(IG) * CGT(IG)
     &                   * COS(TETAPT(KP1BOR_TOT(I))*DEGRAD)
!
            BPHI2BT(I)  = 0.D0
!
            CPHI2BT(I)  = 0.D0
!
            DPHI2BT(I)  = 0.D0
!
         ELSEIF (LIHBORT(KP1BOR_TOT(I+NPTFR_TOT)).EQ.KSORT) THEN
            APHI2BT(I)  = - KT(IG) * CTT(IG) * CGT(IG)
     &              * COS(TETAPT(KP1BOR_TOT(I+NPTFR_TOT))*DEGRAD)
!
             BPHI2BT(I)  = 0.D0
!
            CPHI2BT(I)  = 0.D0
!
            DPHI2BT(I)  = 0.D0
!
         ELSE
            APHI2BT(I)  = 0.D0
!
             BPHI2BT(I)  = 0.D0
!
            CPHI2BT(I)  = 0.D0
!
            DPHI2BT(I)  = 0.D0
!
         ENDIF
!
!        -------------------------------------------
!        COEFFICIENTS FOR A SOLID BOUNDARY SEGMENT
!        -------------------------------------------
!
         IF (LIHBORT(I).EQ.KLOG) THEN
          AUXI1 = KT(IG) * CTT(IG) * CGT(IG) *
     &      COS(TETAPT(I)*DEGRAD) /
     &      ( 1.D0 + RPT(I)*RPT(I) +
     &        2.D0*RPT(I)*COS(ALFAPT(I)*DEGRAD) )
!
          APHI3BT(I) = - (1.D0 - RPT(I) * RPT(I) ) * AUXI1
!
          BPHI3BT(I) = 2.D0*RPT(I)*SIN(ALFAPT(I)*DEGRAD) * AUXI1
!
          CPHI3BT(I)  = 0.D0
!
          DPHI3BT(I)  = 0.D0
!
         ELSEIF (LIHBORT(KP1BOR_TOT(I)).EQ.KLOG) THEN
          AUXI1 = KT(IG) * CTT(IG) * CGT(IG) *
     &      COS(TETAPT(KP1BOR_TOT(I))*DEGRAD) /
     &      (1.D0 + RPT(KP1BOR_TOT(I))*RPT(KP1BOR_TOT(I))
     &      +2.D0 * RPT(KP1BOR_TOT(I))*
     &       COS(ALFAPT(KP1BOR_TOT(I))*DEGRAD))
!
          APHI3BT(I) = - (1.D0-RPT(KP1BOR_TOT(I))*
     &      RPT(KP1BOR_TOT(I))) * AUXI1
!
          BPHI3BT(I) = 2.D0*RPT(KP1BOR_TOT(I))
     &                * SIN(ALFAPT(KP1BOR_TOT(I))*DEGRAD) * AUXI1
!
          CPHI3BT(I)  = 0.D0
!
          DPHI3BT(I)  = 0.D0
!
         ELSEIF (LIHBORT(KP1BOR_TOT(I+NPTFR_TOT)).EQ.KLOG) THEN
          AUXI1 = KT(IG) * CTT(IG) * CGT(IG) *
     &     COS(TETAPT(KP1BOR_TOT(I+NPTFR_TOT))*DEGRAD) /
     &     (1.D0 + RPT(KP1BOR_TOT(I+NPTFR_TOT))
     &      *RPT(KP1BOR_TOT(I+NPTFR_TOT))
     &      +2.D0 * RPT(KP1BOR_TOT(I+NPTFR_TOT))*
     &       COS(ALFAPT(KP1BOR_TOT(I+NPTFR_TOT))*DEGRAD))
!
          APHI3BT(I) = - (1.D0-RPT(KP1BOR_TOT(I+NPTFR_TOT))*
     &      RPT(KP1BOR_TOT(I+NPTFR_TOT))) * AUXI1
!
          BPHI3BT(I) = 2.D0*RPT(KP1BOR_TOT(I+NPTFR_TOT))
     &      * SIN(ALFAPT(KP1BOR_TOT(I+NPTFR_TOT))*DEGRAD) * AUXI1
!
          CPHI3BT(I)  = 0.D0
!
          DPHI3BT(I)  = 0.D0
!
         ELSE
          APHI3BT(I)  = 0.D0
!
          BPHI3BT(I)  = 0.D0
!
          CPHI3BT(I)  = 0.D0
!
          DPHI3BT(I)  = 0.D0
!
         ENDIF
!
!        -------------------------------------------------
!        COEFFICIENTS FOR AN IMPOSED WAVE BOUNDARY SEGMENT
!        -------------------------------------------------
!DA      -----------------------------------
!DA      KEPT FOR MEMORY!
!DA      -----------------------------------
!DA
!DA         IF (LIHBOR(I).EQ.KENT) THEN
!DA         AUXIC      = COS(TETAB(I)*DEGRAD)
!DA         AUXIS      = SIN(TETAB(I)*DEGRAD)
!DA         AUXI1      = GRAV/OMEGA * HB(I)/2.D0 * C(IG) * CG(IG) *
!DA     *                K(IG) * ( AUXIC *XSGBOR(I) +
!DA     *                          AUXIS *YSGBOR(I) )
!DA         AUXI2      = K(IG) * ( X(IG)*AUXIC +
!DA     *                          Y(IG)*AUXIS )
!DA
!DA         APHI4B(I)  = 0.D0
!DA
!DA         BPHI4B(I)  = 0.D0
!DA
!DA         CPHI4B(I)  = AUXI1 * COS( AUXI2 )
!DA
!DA         DPHI4B(I)  = AUXI1 * SIN( AUXI2 )
!DA
!DA       VALUES IMPOSED AT THE NODES OF A KENT SEGMENT
!DA         LIDIR(I)         = KENT
!DA
!DA         AUXI1 = GRAV/OMEGA * HB(I)/2.D0
!DA         AUXI2 = K(IG) * (X(IG)*AUXIC +
!DA     *                    Y(IG)*AUXIS )
!DA
!DA            PHIRB(I) =   AUXI1 * SIN( AUXI2 )
!DA            PHIIB(I) = - AUXI1 * COS( AUXI2 )
!DA         ENDIF
!
!
100   CONTINUE
!-----------------------------------------------------------------------
!
        CALL GLOBAL_TO_LOCAL_BOUND(APHI1BT,APHI1B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(BPHI1BT,BPHI1B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(CPHI1BT,CPHI1B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(DPHI1BT,DPHI1B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(APHI2BT,APHI2B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(BPHI2BT,BPHI2B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(CPHI2BT,CPHI2B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(DPHI2BT,DPHI2B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(APHI3BT,APHI3B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(BPHI3BT,BPHI3B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(CPHI3BT,CPHI3B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(DPHI3BT,DPHI3B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(APHI4BT,APHI4B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(BPHI4BT,BPHI4B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(CPHI4BT,CPHI4B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(DPHI4BT,DPHI4B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUNDI(LIDIRT,LIDIR,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(TETAPT,TETAP,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(TETABT,TETAB,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(RPT,RP,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(HBT,HB,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(ALFAPT,ALFAP,MESH%NPTFR,NPTFR_TOT)
!        CALL GLOBAL_TO_LOCAL_BOUND2(CTT,C,MESH%NPOIN,NPOIN_TOT)
!        CALL GLOBAL_TO_LOCAL_BOUND2(CGT,CG,MESH%NPOIN,NPOIN_TOT)
!        CALL GLOBAL_TO_LOCAL_BOUND2(KT,K,MESH%NPOIN,NPOIN_TOT)
!
        DEALLOCATE(APHI1BT)
        DEALLOCATE(BPHI1BT)
        DEALLOCATE(CPHI1BT)
        DEALLOCATE(DPHI1BT)
        DEALLOCATE(APHI2BT)
        DEALLOCATE(BPHI2BT)
        DEALLOCATE(CPHI2BT)
        DEALLOCATE(DPHI2BT)
        DEALLOCATE(APHI3BT)
        DEALLOCATE(BPHI3BT)
        DEALLOCATE(CPHI3BT)
        DEALLOCATE(DPHI3BT)
        DEALLOCATE(APHI4BT)
        DEALLOCATE(BPHI4BT)
        DEALLOCATE(CPHI4BT)
        DEALLOCATE(DPHI4BT)
        DEALLOCATE(LIDIRT)
!        DEALLOCATE(XT)
!        DEALLOCATE(YT)
!        DEALLOCATE(CTT)
!        DEALLOCATE(KT)
!        DEALLOCATE(CGT)
      DO 110 IFR = 1,IFROI
         I          = IOIFIN(IFR)
         IPREC      = KP1BOR_TOT(I+NPTFR_TOT)
         TETAPT(I) = TETAPT(IPREC)
110   CONTINUE
!
      ELSE
!
!
        DO I=1,NPTFR
                LIHBOR%I(I)=LIHBORT(I)
                RP%R(I)=RPT(I)
                HB%R(I)=HBT(I)
                ALFAP%R(I)=ALFAPT(I)
                TETAB%R(I)=TETABT(I)
                TETAP%R(I)=TETAPT(I)
         END DO
!
         DO 501 I=1,NPTFR
        LIDIR%I(I) = KSORT
!       BEWARE: IT IS ASSUMED HERE THAT NPTFRX=NPTFR
        LIDIR%I(I+NPTFR) = KSORT
        IF (LIHBOR%I(I).EQ.KENT) THEN
           LIHBOR%I(I) = KINC
        ENDIF
        APHI1B%R(I) = 0.D0
        BPHI1B%R(I) = 0.D0
        CPHI1B%R(I) = 0.D0
        DPHI1B%R(I) = 0.D0
        APHI2B%R(I) = 0.D0
        BPHI2B%R(I) = 0.D0
        CPHI2B%R(I) = 0.D0
        DPHI2B%R(I) = 0.D0
        APHI3B%R(I) = 0.D0
        BPHI3B%R(I) = 0.D0
        CPHI3B%R(I) = 0.D0
        DPHI3B%R(I) = 0.D0
        APHI4B%R(I) = 0.D0
        BPHI4B%R(I) = 0.D0
        CPHI4B%R(I) = 0.D0
        DPHI4B%R(I) = 0.D0
 501    CONTINUE
!
!-----------------------------------------------------------------------
!
!
!     ************************************************
!     INITIALISES THE PHASE FOR INCIDENT WAVES
!     ************************************************
!
      PHASOI = 0.D0
!
!     ******************************************
!     PARTICULAR TREATMENT FOR INCIDENT WAVES
!     ******************************************
!
!     -------------------------------------------------
!     LOCATES THE BEGINNINGS OF THE INCIDENT WAVE BOUNDARY
!     -------------------------------------------------
!
      TRVDEB = .TRUE.
      IFROI = 0
!
      DO 101 I=1,NPTFR
         IF (LIHBOR%I(I).EQ.KINC) THEN
               ITOTO = MESH%KP1BOR%I(I+NPTFR)
               IF (LIHBOR%I(ITOTO).NE.KINC) THEN
                  IFROI = IFROI + 1
                  IOIDEB(IFROI) = I
               ENDIF
         ENDIF
 101  CONTINUE
!
!
      IF(LNG.EQ.1) WRITE(LU,111) IFROI
      IF(LNG.EQ.2) WRITE(LU,121) IFROI
 111  FORMAT(1X,'PHBOR : IL Y A : ',1I3,' FRONTIERE(S) ',
     &       1X,'DE TYPE ONDE INCIDENTE ')
 121  FORMAT(1X,'PHBOR : THERE ARE :',1I3,' BOUNDARIE(S) ',
     &       1X,'OF INCIDENT WAVE TYPE ')
!
!     --------------------------------------------------------------
!     COMPUTES THE COEFFICIENTS FOR INCIDENT WAVE BOUNDARIES
!     FROM IOIDEB (BEGINNING OF INCIDENT WAVE)
!     --------------------------------------------------------------
!
!
      DO 151 IFR=1,IFROI
         I = IOIDEB(IFR)
!
 201     CONTINUE
!
!        ********************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE I
!        ********************************
!
         IG   = MESH%NBOR%I(I)
!
!        ******************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE PRECEDING I
!        ******************************************
!
         IG0  = MESH%NBOR%I(MESH%KP1BOR%I(I+NPTFR))
!
!        ****************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE FOLLOWING I
!        ****************************************
!
         IGP1 = MESH%NBOR%I(MESH%KP1BOR%I(I))
!
!
         AUXIC      = COS(TETAB%R(I)*DEGRAD)
         AUXIS      = SIN(TETAB%R(I)*DEGRAD)
         AUXI1      = GRAV/OMEGA * HB%R(I)/2.D0 *
     &                C%R(IG) * CG%R(IG) * K%R(IG)
!
!
!           DEVELOPMENT FOR NON-UNIFORM DIRECTION
!           PHASOI IS THE PHASE
!
!           PREVIOUS FORMULATION : PHASE = K * X (PLANE WAVE) :
!
!                     PHASOI = KM * ( XM*AUXIC + YM*AUXIS )
!
!           NEW FORMULATION (NON PLANE WAVE) :
!
!                                   M
!                                 /
!                    PHASOI(M) = /        K(N) * DX(N)
!                               /
!                                MDEB
!
!           WHERE MDEB IS THE BEGINNING (NODE) OF AN INCIDENT WAVE SEGMENT,
!           FROM WHICH THE PHASE IS COMPUTED. THE ABOVE INTEGRAL IS
!           COMPUTED BY LINEAR APPROXIMATION.
!
!           THE POSSIBLE DEPHASING ALFAP IS ADDED TO ENSURE COHERENCE
!           BETWEEN THE PHASES OF THE DIFFERENT WAVE CRESTS IF SEVERAL
!           NONCONTINUOUS INCIDENT WAVE BOUNDARIES EXIST
!
      PHASOI = PHASOI + K%R(IG)*AUXIC*(X(IG) - X(IG0))
     &                + K%R(IG)*AUXIS*(Y(IG) - Y(IG0))
!
      APHI1B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &             * COS(TETAP%R(I)*DEGRAD)
!
      BPHI1B%R(I)  = 0.D0
!
      CPHI1B%R(I)  = AUXI1 * COS( PHASOI + ALFAP%R(I)*DEGRAD )
!
      DPHI1B%R(I)  = AUXI1 * SIN( PHASOI + ALFAP%R(I)*DEGRAD )
!
      I = MESH%KP1BOR%I(I)
!
!     UNTIL THE NODE FOLLOWING THE END OF AN INCIDENT WAVE BND IS REACHED
!
      IF (LIHBOR%I(I).NE.KINC) THEN
         IOIFIN(IFR) = I
         IPREC      = MESH%KP1BOR%I(I+NPTFR)
         TETAB%R(I) = TETAB%R(IPREC)
         HB%R(I)    = HB%R(IPREC)
         AUXIC      = COS(TETAB%R(I)*DEGRAD)
         AUXIS      = SIN(TETAB%R(I)*DEGRAD)
         AUXI1      = GRAV/OMEGA * HB%R(I)/2.D0 *
     &                C%R(IG) * CG%R(IG) * K%R(IG)
         PHASOI = PHASOI + K%R(IG)*AUXIC*(X(IG) - X(IG0))
     &                   + K%R(IG)*AUXIS*(Y(IG) - Y(IG0))
!
         APHI1B%R(I) = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                 * COS(TETAP%R(IPREC)*DEGRAD)
!
         BPHI1B%R(I) = 0.D0
!
         CPHI1B%R(I) = AUXI1*COS(PHASOI + ALFAP%R(IPREC)*DEGRAD)
!
         DPHI1B%R(I) = AUXI1*SIN(PHASOI + ALFAP%R(IPREC)*DEGRAD)
!
         GOTO 151
!
      ELSE
         GOTO 201
      ENDIF
!
 151  CONTINUE
!
!     ******************************************
!     END OF TREATMENT OF THE INCIDENT WAVE BOUNDARY
!     ******************************************
!
      DO 1001 I=1,NPTFR
!
!        ********************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE I
!        ********************************
!
         IG   = MESH%NBOR%I(I)
!
!        ******************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE PRECEDING I
!        ******************************************
!
         IG0  = MESH%NBOR%I(MESH%KP1BOR%I(I+NPTFR))
!
!        ****************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE FOLLOWING I
!        ****************************************
!
         IGP1 = MESH%NBOR%I(MESH%KP1BOR%I(I))
!
!        -------------------------------------------------
!        COEFFICIENTS FOR A FREE EXIT BOUNDARY SEGMENT
!        -------------------------------------------------
!
         IF (LIHBOR%I(I).EQ.KSORT) THEN
            APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                   * COS(TETAP%R(I)*DEGRAD)
!
            BPHI2B%R(I)  = 0.D0
!
            CPHI2B%R(I)  = 0.D0
!
            DPHI2B%R(I)  = 0.D0
!
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I)).EQ.KSORT) THEN
            APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                   * COS(TETAP%R(MESH%KP1BOR%I(I))*DEGRAD)
!
            BPHI2B%R(I)  = 0.D0
!
            CPHI2B%R(I)  = 0.D0
!
            DPHI2B%R(I)  = 0.D0
!
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I+NPTFR)).EQ.KSORT) THEN
            APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &              * COS(TETAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD)
!
            BPHI2B%R(I)  = 0.D0
!
            CPHI2B%R(I)  = 0.D0
!
            DPHI2B%R(I)  = 0.D0
!
         ELSE
            APHI2B%R(I)  = 0.D0
!
            BPHI2B%R(I)  = 0.D0
!
            CPHI2B%R(I)  = 0.D0
!
            DPHI2B%R(I)  = 0.D0
!
         ENDIF
!
!        -------------------------------------------
!        COEFFICIENTS FOR A SOLID BOUNDARY SEGMENT
!        -------------------------------------------
!
         IF (LIHBOR%I(I).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &      COS(TETAP%R(I)*DEGRAD) /
     &      ( 1.D0 + RP%R(I)*RP%R(I) +
     &        2.D0*RP%R(I)*COS(ALFAP%R(I)*DEGRAD) )
!
          APHI3B%R(I) = - (1.D0 - RP%R(I) * RP%R(I) ) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(I)*SIN(ALFAP%R(I)*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I)).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &      COS(TETAP%R(MESH%KP1BOR%I(I))*DEGRAD) /
     &      (1.D0 + RP%R(MESH%KP1BOR%I(I))*RP%R(MESH%KP1BOR%I(I))
     &      +2.D0 * RP%R(MESH%KP1BOR%I(I))*
     &       COS(ALFAP%R(MESH%KP1BOR%I(I))*DEGRAD))
!
          APHI3B%R(I) = - (1.D0-RP%R(MESH%KP1BOR%I(I))*
     &      RP%R(MESH%KP1BOR%I(I))) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(MESH%KP1BOR%I(I))
     &                * SIN(ALFAP%R(MESH%KP1BOR%I(I))*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I+NPTFR)).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &     COS(TETAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD) /
     &     (1.D0 + RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      *RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      +2.D0 * RP%R(MESH%KP1BOR%I(I+NPTFR))*
     &       COS(ALFAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD))
!
          APHI3B%R(I) = - (1.D0-RP%R(MESH%KP1BOR%I(I+NPTFR))*
     &      RP%R(MESH%KP1BOR%I(I+NPTFR))) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      * SIN(ALFAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ELSE
          APHI3B%R(I)  = 0.D0
!
          BPHI3B%R(I)  = 0.D0
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ENDIF
!
!        -------------------------------------------------
!        COEFFICIENTS FOR AN IMPOSED WAVE BOUNDARY SEGMENT
!        -------------------------------------------------
!DA      -----------------------------------
!DA      KEPT FOR MEMORY !
!DA      -----------------------------------
!DA
!DA         IF (LIHBOR(I).EQ.KENT) THEN
!DA         AUXIC      = COS(TETAB(I)*DEGRAD)
!DA         AUXIS      = SIN(TETAB(I)*DEGRAD)
!DA         AUXI1      = GRAV/OMEGA * HB(I)/2.D0 * C(IG) * CG(IG) *
!DA     *                K(IG) * ( AUXIC *XSGBOR(I) +
!DA     *                          AUXIS *YSGBOR(I) )
!DA         AUXI2      = K(IG) * ( X(IG)*AUXIC +
!DA     *                          Y(IG)*AUXIS )
!DA
!DA         APHI4B(I)  = 0.D0
!DA
!DA         BPHI4B(I)  = 0.D0
!DA
!DA         CPHI4B(I)  = AUXI1 * COS( AUXI2 )
!DA
!DA         DPHI4B(I)  = AUXI1 * SIN( AUXI2 )
!DA
!DA       VALUES IMPOSED AT THE NODES OF A KENT SEGMENT
!DA         LIDIR(I)         = KENT
!DA
!DA         AUXI1 = GRAV/OMEGA * HB(I)/2.D0
!DA         AUXI2 = K(IG) * (X(IG)*AUXIC +
!DA     *                    Y(IG)*AUXIS )
!DA
!DA            PHIRB(I) =   AUXI1 * SIN( AUXI2 )
!DA            PHIIB(I) = - AUXI1 * COS( AUXI2 )
!DA         ENDIF
!
!
 1001 CONTINUE
!-----------------------------------------------------------------------
!
!
      DO 1101 IFR = 1,IFROI
         I          = IOIFIN(IFR)
         IPREC      = MESH%KP1BOR%I(I+NPTFR)
         TETAP%R(I) = TETAP%R(IPREC)
 1101 CONTINUE
!
!      DO I=1,NPTFR
!         ALFAP%R(I)=ALFAPT(I)
!         TETAP%R(I)=TETAPT(I)
!         TETAB%R(I)=TETABT(I)
!         RP%R(I)=RPT(I)
!         HB%R(I)=HBT(I)
!      END DO
!
      END IF
!
      WRITE(LU,*) 'END PHBOR'
      RETURN
      CONTAINS
      SUBROUTINE GLOBAL_TO_LOCAL_BOUND(TAB1,OBJ,NPTFR,NPTFR_TOT)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(INOUT)  :: TAB1(:)
      TYPE (BIEF_OBJ), INTENT(INOUT) :: OBJ
      INTEGER, INTENT(IN) :: NPTFR
      INTEGER, INTENT(IN) :: NPTFR_TOT
      INTEGER :: I,J
      OBJ%R=0.0
      DO I=1,NPTFR_TOT
         DO J=1,MESH%NPTFR
            IF (NBOR_TOT(I) .EQ. MESH%KNOLG%I(MESH%NBOR%I(J))) THEN
               OBJ%R(J)=TAB1(I)
            END IF
         END DO
      END DO
      OBJ%DIM1=NPTFR
!
      END SUBROUTINE
!
!
      SUBROUTINE GLOBAL_TO_LOCAL_BOUNDI(TAB1,OBJ,NPTFR,NPTFR_TOT)
      IMPLICIT NONE
      INTEGER, INTENT(INOUT)  :: TAB1(:)
      TYPE (BIEF_OBJ), INTENT(INOUT) :: OBJ
      INTEGER, INTENT(IN) :: NPTFR
      INTEGER, INTENT(IN) :: NPTFR_TOT
      INTEGER :: I,J
      OBJ%I=0.0
      DO I=1,NPTFR_TOT
         DO J=1,MESH%NPTFR
            IF (NBOR_TOT(I) .EQ. MESH%KNOLG%I(MESH%NBOR%I(J))) THEN
               OBJ%I(J)=TAB1(I)
            END IF
         END DO
      END DO
      DO  I=NPTFR_TOT+1,2*NPTFR_TOT
         DO J=1,MESH%NPTFR
            IF (NBOR_TOT(I/2) .EQ. MESH%KNOLG%I(MESH%NBOR%I(J))) THEN
               OBJ%I(2*J)=TAB1(I)
            END IF
         END DO
      END DO
      END SUBROUTINE
!
       SUBROUTINE GLOBAL_TO_LOCAL_BOUND2(TAB1,OBJ,NPOIN,NPOIN_TOT)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(INOUT)  :: TAB1(:)
      TYPE (BIEF_OBJ), INTENT(INOUT) :: OBJ
      INTEGER, INTENT(IN) :: NPOIN_TOT
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER :: I,J
      OBJ%R=0.0
      DO I=1,NPOIN_TOT
         DO J=1,NPOIN
            IF (I .EQ. MESH%KNOLG%I(J)) THEN
               OBJ%R(J)=TAB1(I)
            END IF
         END DO
      END DO
      OBJ%DIM1=NPOIN
!
      END SUBROUTINE
!
      END
