!
!  CHANGES VS SOURCE FILES:
!  IN BORD3D: SPECIAL BOUNDARY CONDITIONS FOR TRACER
!
!                    *****************
                     SUBROUTINE BORD3D
!                    *****************
!
     &(TIME,LT,ENTET,NPTFR2_DIM,NFRLIQ)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    SPECIFIC BOUNDARY CONDITIONS.
!
!note     1) FOR PRESCRIBED BOUNDARIES OF POINT BEING BOTH LATERAL
!+            AND BOTTOM : USE LATERAL ARRAYS.
!+
!+     2) FOR TYPES OF BOUNDARY CONDITIONS : USE SUBROUTINE LIMI3D.
!+
!+     3) SEDIMENT IS THE LAST TRACER.
!
!warning  MAY BE MODIFIED BY THE USER
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
!history  J.-M. HERVOUET (LNHE)
!+        19/09/2011
!+        V6P2
!+   Call to DEBIMP3D replaced by CALL DEBIMP_3D (new arguments)
!
!history  J.-M. HERVOUET (LNHE)
!+        11/03/2013
!+        V6P3
!+   Test IFRLIQ.NE.0 line 210.
!
!history  C. VILLARET & T. BENSON (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   Case IPROF.EQ.3 added to test IPROF.EQ.2.
!
!history  A. GINEAU, N. DURAND, N. LORRAIN, C.-T. PHAM (LNHE)
!+        09/07/2014
!+        V7P0
!+   Adding the heat balance of exchange with atmosphere
!
!history  A. JOLY (EDF LAB, LNHE)
!+        27/08/2015
!+        V7P1
!+   Imposed flowrates on the bed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS
!|                |   | CONSERVATION.
!| LT             |-->| CURRENT TIME STEP NUMBER
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPTFR2_DIM     |-->| NPTFR2? NOT USED
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, EX_NFRLIQ=>NFRLIQ
      USE DECLARATIONS_WAQTEL, ONLY: TAIR,HREL,NEBU,RO0,CP_EAU,
     &                               ATMOSEXCH,WAQPROCESS
      USE INTERFACE_TELEMAC3D, EX_BORD3D => BORD3D
      USE EXCHANGE_WITH_ATMOSPHERE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     TIME AND ENTET ARE AT AND INFOGR (NOW IN DECLARATIONS_TELEMAC3D)
      DOUBLE PRECISION, INTENT(IN)    :: TIME
      INTEGER         , INTENT(IN)    :: LT
      LOGICAL         , INTENT(IN)    :: ENTET
      INTEGER         , INTENT(IN)    :: NPTFR2_DIM
      INTEGER         , INTENT(IN)    :: NFRLIQ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2,NP,IBORD,IVIT,ICOT,IDEB,IFRLIQ,IPROF,K,N
      INTEGER IPTFR,ITRAC,IPLAN,I3D
      LOGICAL YAZMIN
      DOUBLE PRECISION ROEAU,ROAIR,VITV,PROFZ,WINDRELX,WINDRELY
!
      DOUBLE PRECISION P_DMIN,P_DSUM
      INTEGER  P_IMAX
      EXTERNAL P_IMAX,P_DMIN,P_DSUM
      DOUBLE PRECISION STA_DIS_CUR
      EXTERNAL STA_DIS_CUR
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION ZMIN(MAXFRO)
!
      INTEGER YADEB(MAXFRO),MSK1,IJK
!
!     NORMALS TO THE BED
      DOUBLE PRECISION XNB,YNB,ZNB
!
!     SIMPLE CASES FOR LATERAL BOUNDARIES ARE TREATED AUTOMATICALLY:
!
!     - PRESCRIBED DEPTH     (5 4 4)
!     - PRESCRIBED VELOCITY  (  6 6)
!     - PRESCRIBED DISCHARGE (  5 5)
!
!     CORRESPONDING KEYWORDS ARE:
!
!     'PRESCRIBED ELEVATIONS' OR 'COTES IMPOSEES'
!     'PRESCRIBED VELOCITIES' OR 'VITESSES IMPOSEES'
!     'PRESCRIBED FLOWRATES' OR 'DEBITS IMPOSES'
!
!     THE IMPLEMENTATION OF AUTOMATIC CASES MAY BE CANCELLED
!     PROVIDED THAT THE RELEVANT ARRAYS ARE FILLED
!
!
!***********************************************************************
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!              AUTOMATIC TREATMENT OF LIQUID BOUNDARIES
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!=======================================================================
!
!     SECURES NO SLIP BOUNDARY CONDITIONS
!
      IF(LT.EQ.1) THEN
!
!     VELOCITIES
!
      DO IPTFR = 1,NPTFR2
        IPOIN2 = NBOR2%I(IPTFR)
        DO IPLAN = 1,NPLAN
          IBORD = (IPLAN-1)*NPTFR2 + IPTFR
          IF(LIUBOL%I(IBORD).EQ.KADH) UBORL%R(IBORD) = 0.D0
          IF(LIVBOL%I(IBORD).EQ.KADH) VBORL%R(IBORD) = 0.D0
          IF(LIWBOL%I(IBORD).EQ.KADH) WBORL%R(IBORD) = 0.D0
        ENDDO
      ENDDO
!
      DO IPOIN2 = 1,NPOIN2
        IF(LIUBOF%I(IPOIN2).EQ.KADH) UBORF%R(IPOIN2) = 0.D0
        IF(LIVBOF%I(IPOIN2).EQ.KADH) VBORF%R(IPOIN2) = 0.D0
        IF(LIWBOF%I(IPOIN2).EQ.KADH) WBORF%R(IPOIN2) = 0.D0
        IF(LIUBOS%I(IPOIN2).EQ.KADH) UBORS%R(IPOIN2) = 0.D0
        IF(LIVBOS%I(IPOIN2).EQ.KADH) VBORS%R(IPOIN2) = 0.D0
        IF(LIWBOS%I(IPOIN2).EQ.KADH) WBORS%R(IPOIN2) = 0.D0
      ENDDO
!
!     IMPORTANT OPTION:
!     VERTICAL VELOCITIES ARE SET AS HORIZONTAL VELOCITIES
!     THIS IS AN OPTION, OTHERWISE LIWBOL=KSORT (SEE LIMI3D)
!
!     DO IPTFR = 1,NPTFR2
!       IPOIN2 = NBOR2%I(IPTFR)
!       DO IPLAN = 1,NPLAN
!         IBORD = (IPLAN-1)*NPTFR2 + IPTFR
!         LIWBOL%I(IBORD)= LIUBOL%I(IBORD)
!         IF(LIWBOL%I(IBORD).EQ.KENT) WBORL%R(IBORD) = 0.D0
!       ENDDO
!     ENDDO
!
!     TRACERS
!
! BEGIN OF PART SPECIFIC TO THIS CASE
      DO IPTFR=1,NPTFR2
        IPOIN2 = NBOR2%I(IPTFR)
        DO IPLAN = 1,NPLAN
          IBORD = (IPLAN-1)*NPTFR2 + IPTFR
          IF(NCSIZE.GT.1) IPOIN2 = MESH3D%KNOLG%I(IPOIN2)

          IF(IPOIN2.EQ.102.OR.IPOIN2.EQ.98.OR.IPOIN2.EQ.94
     &       .OR.IPOIN2.EQ.90.OR.IPOIN2.EQ.86.OR.IPOIN2.EQ.82) THEN
            DO ITRAC=1,NTRAC
              TABORL%ADR(ITRAC)%P%R(IBORD) = 1.D0
            ENDDO
          ENDIF
        ENDDO
      ENDDO
! END OF PART SPECIFIC TO THIS CASE
!
!     IF(NTRAC.NE.0) THEN
!
!       DO ITRAC = 1,NTRAC
!
!         DO IPTFR = 1,NPTFR2
!           IPOIN2 = NBOR2%I(IPTFR)
!           LITABF%ADR(ITRAC)%P%I(IPOIN2) = KSORT (DOES NOT WORK WITH SEDIMENT)
!           LITABS%ADR(ITRAC)%P%I(IPOIN2) = KSORT
!           DO IPLAN = 1,NPLAN
!             IBORD = (IPLAN-1)*NPTFR2 + IPTFR
!             IF(LITABL%ADR(ITRAC)%P%I(IBORD).EQ.KADH)
!    &           TABORL%ADR(ITRAC)%P%R(IBORD) = 0.D0
!           ENDDO
!         ENDDO
!
!         DO IPOIN2 = 1,NPOIN2
!           IF(LITABF%ADR(ITRAC)%P%I(IPOIN2).EQ.KADH)
!    &                       TABORF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           IF(LITABS%ADR(ITRAC)%P%I(IPOIN2).EQ.KADH)
!    &                       TABORS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!         ENDDO
!
!       ENDDO
!
!     ENDIF
!
      ENDIF
!
!=======================================================================
!  FOR ALL TIMESTEPS
!=======================================================================
!
!     IF VELOCITY PROFILE OPTION 5: MINIMUM ELEVATION OF EVERY BOUNDARY
!
      YAZMIN=.FALSE.
      DO IFRLIQ=1,NFRLIQ
        ZMIN(IFRLIQ)=1.D99
        IF(PROFVEL(IFRLIQ).EQ.5) YAZMIN=.TRUE.
      ENDDO
      IF(YAZMIN) THEN
        DO K=1,NPTFR2
          IFRLIQ=NUMLIQ%I(K)
          IPOIN2=NBOR2%I(K)
          IF(IFRLIQ.NE.0) THEN
            ZMIN(IFRLIQ)=MIN(ZMIN(IFRLIQ),ZF%R(IPOIN2)+H%R(IPOIN2))
          ENDIF
        ENDDO
        IF(NCSIZE.GT.1) THEN
          DO IFRLIQ=1,NFRLIQ
            ZMIN(IFRLIQ)=P_DMIN(ZMIN(IFRLIQ))
          ENDDO
        ENDIF
      ENDIF
!
!     INITIALISES YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF
!
      IDEB=0
      ICOT=0
      IVIT=0
!
!     LOOP ON ALL 2D BOUNDARY POINTS
!
      DO K=1,NPTFR2
!
!     PRESCRIBED ELEVATION GIVEN IN STEERING FILE (NCOTE<>0)
!     -------------------------------------------------------
!
      IF(LIHBOR%I(K).EQ.KENT.AND.NCOTE.NE.0) THEN
!
        IPOIN2 = NBOR2%I(K)
        ICOT=NUMLIQ%I(K)
        IF(STA_DIS_CURVES(ICOT).EQ.1) THEN
          HBOR%R(K) = STA_DIS_CUR(ICOT,FLUX_BOUNDARIES(ICOT),
     &                            PTS_CURVES(ICOT),QZ,NFRLIQ,
     &                            ZF%R(IPOIN2)+H%R(IPOIN2))
     &                - ZF%R(IPOIN2)
          HBOR%R(K) = MAX(0.D0,HBOR%R(K))
        ELSEIF(NCOTE.GE.NUMLIQ%I(K)) THEN
          N=IPOIN2
          IF(NCSIZE.GT.1) N=MESH2D%KNOLG%I(N)
          HBOR%R(K) = SL3(ICOT,AT,N,INFOGR)-ZF%R(IPOIN2)
          HBOR%R(K) = MAX(0.D0,HBOR%R(K))
        ELSE
          IF(LNG.EQ.1) WRITE(LU,100) NUMLIQ%I(K)
100       FORMAT(1X,'BORD3D : COTES IMPOSEES EN NOMBRE INSUFFISANT',/,
     &           1X,'         DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'         IL EN FAUT AU MOINS : ',1I6,/,
     &           1X,'         AUTRE POSSIBILITE :',/,
     &           1X,'         FICHIER DES COURBES DE TARAGE MANQUANT')
          IF(LNG.EQ.2) WRITE(LU,101) NUMLIQ%I(K)
101       FORMAT(1X,'BORD3D: MORE PRESCRIBED ELEVATIONS ARE REQUIRED',/,
     &           1X,'        IN THE PARAMETER FILE',/,
     &           1X,'        AT LEAST ',1I6,' MUST BE GIVEN',/,
     &           1X,'        OTHER POSSIBILITY:',/,
     &           1X,'        STAGE-DISCHARGE CURVES FILE MISSING')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
      ENDDO
!
!     PRESCRIBED DISCHARGE GIVEN IN STEERING FILE (NDEBIT<>0)
!     --------------------------------------------------------
!
      DO K=1,NPTFR2
!
!     A VELOCITY PROFILE IS SET HERE AND WILL BE CORRECTED LATER
!     TO GET THE CORRECT DISCHARGE (CALL TO DEBIMP3D)
!
      IF(LIUBOL%I(K).EQ.KENT.AND.NDEBIT.NE.0) THEN
!
        IPOIN2 = NBOR2%I(K)
        DO NP=1,NPLAN
          IJK=(NP-1)*NPTFR2+K
          I3D=(NP-1)*NPOIN2+IPOIN2
          IFRLIQ=NUMLIQ%I(K)
          IF(PROFVEL(IFRLIQ).EQ.2) THEN
!           GIVEN BY USER IN BOUNDARY CONDITIONS FILE
            UBORL%R(IJK) = UBOR2D%R(K+NPTFR2)
            VBORL%R(IJK) = VBOR2D%R(K+NPTFR2)
          ELSEIF(PROFVEL(IFRLIQ).EQ.3) THEN
!           NORMAL AND NORM GIVEN BY UBOR IN BOUNDARY CONDITIONS FILE
            UBORL%R(IJK) = -XNEBOR2%R(K)*UBOR2D%R(K+NPTFR2)
            VBORL%R(IJK) = -YNEBOR2%R(K)*UBOR2D%R(K+NPTFR2)
          ELSEIF(PROFVEL(IFRLIQ).EQ.4) THEN
!           NORMAL AND PROPORTIONAL TO SQRT(H)
            UBORL%R(IJK)=-XNEBOR2%R(K) * SQRT(MAX(H%R(IPOIN2),0.D0))
            VBORL%R(IJK)=-YNEBOR2%R(K) * SQRT(MAX(H%R(IPOIN2),0.D0))
          ELSEIF(PROFVEL(IFRLIQ).EQ.5) THEN
!           NORMAL PROFILE IN SQUARE ROOT OF H, BUT VIRTUAL H
!           DEDUCED FROM LOWEST FREE SURFACE OF THE BOUNDARY
            UBORL%R(IJK)=-XNEBOR2%R(K) *
     &                   SQRT(MAX(ZMIN(IFRLIQ)-ZF%R(IPOIN2),0.D0))
            VBORL%R(IJK)=-YNEBOR2%R(K) *
     &                   SQRT(MAX(ZMIN(IFRLIQ)-ZF%R(IPOIN2),0.D0))
          ELSE
!           NORMAL AND NORM 1
            UBORL%R(IJK)=-XNEBOR2%R(K)
            VBORL%R(IJK)=-YNEBOR2%R(K)
          ENDIF
!         NO VELOCITY IF NO WATER
          IF(H%R(IPOIN2).LT.1.D-4) THEN
            UBORL%R(IJK) = 0.D0
            VBORL%R(IJK) = 0.D0
          ENDIF
!         CASE OF A VERTICAL PROFILE
          IF(VERPROVEL(IFRLIQ).NE.1) THEN
            PROFZ=VEL_PROF_Z(IFRLIQ,NBOR2%I(K),
     &                       AT,LT,NP,INFOGR,VERPROVEL(IFRLIQ))
            UBORL%R(IJK) = UBORL%R(IJK)*PROFZ
            VBORL%R(IJK) = VBORL%R(IJK)*PROFZ
          ENDIF
!         U AND V INITIALISED WITH PRESCRIBED VALUES (FOR DEBIMP3D)
!         WILL BE CHANGED AGAIN AFTER DEBIMP3D
          U%R(I3D)=UBORL%R(IJK)
          V%R(I3D)=VBORL%R(IJK)
        ENDDO
!
        YADEB(NUMLIQ%I(K))=1
!
      ENDIF
!
      ENDDO
!
!     PRESCRIBED VELOCITY GIVEN IN STEERING FILE (NVIT<>0)
!     -----------------------------------------------------
!
      DO K=1,NPTFR2
!
!     THIS VELOCITY IS CONSIDERED NORMAL TO THE BOUNDARY
!
      IF(LIUBOL%I(K).EQ.KENTU.AND.NVIT.NE.0) THEN
        IVIT=NUMLIQ%I(K)
        IF(NVIT.GE.IVIT) THEN
          DO NP=1,NPLAN
            IBORD = (NP-1)*NPTFR2+K
            IF(NCSIZE.GT.1) THEN
              N=MESH2D%KNOLG%I(NBOR2%I(K))+(NP-1)*NPOIN2
            ELSE
              N=NBOR3%I(IBORD)
            ENDIF
            UBORL%R(IBORD)=-MESH2D%XNEBOR%R(K)*VIT3(IVIT,AT,N,INFOGR)
            VBORL%R(IBORD)=-MESH2D%YNEBOR%R(K)*VIT3(IVIT,AT,N,INFOGR)
            WBORL%R(IBORD)=0.D0
          ENDDO
        ELSE
          IF(LNG.EQ.1) WRITE(LU,200) NUMLIQ%I(K)
200       FORMAT(1X,'BORD3D : VITESSES IMPOSEES EN NOMBRE INSUFFISANT',
     &           /,1X,'       DANS LE FICHIER DES PARAMETRES',
     &           /,1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,201) NUMLIQ%I(K)
201       FORMAT(1X,'BORD3D : MORE PRESCRIBED VELOCITIES ARE REQUIRED',
     &           /,1X,'       IN THE PARAMETER FILE',
     &           /,1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      ENDDO
!
!     PRESCRIBED TRACER GIVEN IN STEERING FILE,
!     BUT POSSIBLE OVERWRITING IF LIQUID BOUNDARY FILE IS GIVEN
!     SEE FUNCTION TR3
!     -------------------------------------------------------
!
      IF(NTRAC.GT.0.AND.NTRACER.GT.0) THEN
        DO ITRAC=1,NTRAC
        DO K=1,NPTFR2
        DO NP=1,NPLAN
          IBORD = (NP-1)*NPTFR2+K
          IF(LITABL%ADR(ITRAC)%P%I(IBORD).EQ.KENT) THEN
            IFRLIQ=NUMLIQ%I(K)
            IF(IFRLIQ.EQ.0) THEN
              IF(LNG.EQ.1) WRITE(LU,298) IBORD
298           FORMAT(1X,'BORD3D : VALEURS IMPOSEES DU TRACEUR',/,
     &               1X,'         SUR PAROI SOLIDE',/,
     &               1X,'         AU POINT DE BORD ',1I6)
              IF(LNG.EQ.2) WRITE(LU,299) IBORD
299           FORMAT(1X,'BORD3D: PRESCRIBED TRACER VALUE',/,
     &               1X,'        ON A SOLID BOUNDARY',/,
     &               1X,'        AT BOUNDARY POINT ',1I6)
              CALL PLANTE(1)
              STOP
            ENDIF
            IF(NTRACER.GE.IFRLIQ*NTRAC) THEN
              IF(NCSIZE.GT.1) THEN
                N=MESH2D%KNOLG%I(NBOR2%I(K))+(NP-1)*NPOIN2
              ELSE
                N=NBOR3%I(IBORD)
              ENDIF
              TABORL%ADR(ITRAC)%P%R(IBORD)=
     &                                   TR3(IFRLIQ,ITRAC,N,AT,INFOGR)
            ELSE
              IF(LNG.EQ.1) WRITE(LU,300) NUMLIQ%I(K)*NTRAC
300           FORMAT(1X,'BORD3D : VALEURS IMPOSEES DU TRACEUR',/,
     &               1X,'         EN NOMBRE INSUFFISANT',/,
     &               1X,'         DANS LE FICHIER DES PARAMETRES',/,
     &               1X,'         IL EN FAUT AU MOINS : ',1I6)
              IF(LNG.EQ.2) WRITE(LU,301) NUMLIQ%I(K)
301           FORMAT(1X,'BORD3D: MORE PRESCRIBED TRACER VALUES',/,
     &               1X,'        ARE REQUIRED IN THE PARAMETER FILE',/,
     &               1X,'        AT LEAST ',1I6,' MUST BE GIVEN')
              CALL PLANTE(1)
              STOP
            ENDIF
!           CASE OF A PROFILE ON THE VERTICAL
            IPROF=VERPROTRA(ITRAC+(IFRLIQ-1)*NTRAC)
            IF(IPROF.NE.1) THEN
              PROFZ=TRA_PROF_Z(IFRLIQ,NBOR2%I(K),AT,LT,NP,
     &                         INFOGR,IPROF,ITRAC)
              IF(IPROF.EQ.2.OR.IPROF.EQ.0) THEN
!               Rouse concentrations profiles (IPROF=2) or values given by user (IPROF=0)
                TABORL%ADR(ITRAC)%P%R(IBORD)=PROFZ
              ELSEIF(IPROF.EQ.3) THEN
!               Normalised concentrations profiles (IPROF=3)
                TABORL%ADR(ITRAC)%P%R(IBORD)=
     &          TABORL%ADR(ITRAC)%P%R(IBORD)*PROFZ
              ELSE
                WRITE(LU,*) 'BORD3D : IPROF=',IPROF
                IF(LNG.EQ.1) THEN
                  WRITE(LU,*) 'OPTION INCONNUE POUR LES'
                  WRITE(LU,*) 'PROFILS DES TRACEURS SUR LA VERTICALE'
                ENDIF
                IF(LNG.EQ.2) THEN
                  WRITE(LU,*) 'UNKNOWN OPTION FOR THE'
                  WRITE(LU,*) 'TRACERS VERTICAL PROFILES'
                ENDIF
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDIF
          ENDIF
!
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!
      IF(NBEDFLO.GT.0) THEN
!
!       PRESCRIBED FLOWRATES ON THE BED GIVEN BY THE USER
!       -------------------------------------------------
!
        CALL VECTOR(T2_01,'=','MASBAS          ',IELM2H,1.D0,
     &              WBORF,WBORF,WBORF,WBORF,WBORF,WBORF,MESH2D,
     &              .FALSE.,MASKEL)
!
!       FIND THE AREA OF EACH BOUNDARY
        DO IFRLIQ=1,NBEDFLO
          BEDQAREA(IFRLIQ) = 0.D0
        ENDDO
!
        DO K=1,NPOIN2
          IF(LIWBOF%I(K).EQ.KENT) THEN
            IFRLIQ=NLIQBED%I(K)
            IF(IFRLIQ.GT.0) THEN
              BEDQAREA(IFRLIQ) = BEDQAREA(IFRLIQ) + T2_01%R(K)
            ENDIF
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) THEN
          DO IFRLIQ = 1 , NBEDFLO
            BEDQAREA(IFRLIQ)=P_DSUM(BEDQAREA(IFRLIQ))
          ENDDO
        ENDIF
!
        DO IFRLIQ = 1 , NBEDFLO
          IF(BEDQAREA(IFRLIQ).LE.0.D0) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'BORD3D : FRONTIERE DU FOND ',IFRLIQ
              WRITE(LU,*) '         AVEC SURFACE EGALE A : ',
     &                              BEDQAREA(IFRLIQ)
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'BORD3D: BOUNDARY ON THE BOTTOM: ',IFRLIQ
              WRITE(LU,*) '        WITH AREA EQUAL TO : ',
     &                             BEDQAREA(IFRLIQ)
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
      IF(TIDALTYPE.GE.1) CALL TIDAL_MODEL_T3D()
!
!-----------------------------------------------------------------------
!
!     PRESCRIBED DISCHARGES: FINAL TREATMENT OF VELOCITIES
!     ----------------------------------------------------
!
!     LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
      DO IFRLIQ = 1 , NFRLIQ
!
      IF(NDEBIT.NE.0) THEN
!
        MSK1=1
        IF(NDEBIT.GE.IFRLIQ) THEN
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
           IF(YADEB(IFRLIQ).EQ.1) THEN
           CALL DEBIMP_3D(Q3(IFRLIQ,AT,INFOGR),
     &                    UBORL%R,VBORL%R,WBORL%R,
     &                    U,V,NUMLIQ%I,NUMLIQ_ELM%I,IFRLIQ,T3_02,
     &                    NPTFR2,NETAGE,MASK_3D%ADR(MSK1)%P,
     &                    MESH3D,EQUA,IELM2V,SVIDE,MASKTR,
     &                    MESH3D%NELEB)
           ENDIF
          ELSE
          IF(LNG.EQ.1) WRITE(LU,400) IFRLIQ
400       FORMAT(1X,'BORD3D : DEBITS IMPOSES',/,
     &           1X,'       EN NOMBRE INSUFFISANT',/,
     &           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,401) IFRLIQ
401       FORMAT(1X,'BORD3D : MORE PRESCRIBED FLOWRATES',/,
     &           1X,'       ARE REQUIRED IN THE PARAMETER FILE',/,
     &           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      ENDDO ! IFRLIQ
      ENDIF
!
!     RESETS BOUNDARY CONDITIONS ON U AND V (WILL BE USED BY TFOND
!     AND OTHER SUBROUTINES BEFORE THE NEXT BOUNDARY CONDITIONS TREATMENT)
!
      DO K=1,NPTFR2
        IF(LIUBOL%I(K).EQ.KENT) THEN
          DO NP=1,NPLAN
            IJK=(NP-1)*NPTFR2+K
            U%R((NP-1)*NPOIN2+NBOR2%I(K))=UBORL%R(IJK)
            V%R((NP-1)*NPOIN2+NBOR2%I(K))=VBORL%R(IJK)
          ENDDO
        ENDIF
      ENDDO
!
!     EXAMPLE OF PRESCRIBED VERTICAL VELOCITIES AT ENTRANCES
!     VELOCITIES TANGENT TO BOTTOM AND FREE SURFACE
!
!     DO K=1,NPTFR2
!       IF(LIWBOL%I(K).EQ.KENT.OR.LIWBOL%I(K).EQ.KENTU) THEN
!         DO NP=1,NPLAN
!             IJK=(NP-1)*NPTFR2+K
!             I2D=NBOR2%I(K)
!             I3D=(NP-1)*NPOIN2+I2D
!             WBORL DEDUCED FROM FREE SURFACE AND BOTTOM
!             TETA=(Z(I3D)-Z(I2D))/
!    *        MAX(1.D-3,Z((NPLAN-1)*NPOIN2+I2D)-Z(I2D))
!             GX=        TETA *GRADZN%ADR(1)%P%R(I2D)
!    *            +(1.D0-TETA)*GRADZF%ADR(1)%P%R(I2D)
!             GY=        TETA *GRADZN%ADR(2)%P%R(I2D)
!    *            +(1.D0-TETA)*GRADZF%ADR(2)%P%R(I2D)
!             WBORL%R(IJK)=UBORL%R(IJK)*GX+VBORL%R(IJK)*GY
!         ENDDO
!       ENDIF
!     ENDDO
!
!     PRESCRIBED FLOWRATES ON THE BED: FINAL TREATMENT
!     --------------------------------------------------------
!
      IF(NBEDFLO.GT.0) THEN
!
        DO K=1,NPOIN2
!
!         CORRECT THE VELOCITY PROFILES BY DIVIDING THE FLOW RATE WITH
!         THE CROSS-SECTIONAL AREA OVER WHICH IT WILL BE IMPOSED
!
          IF(LIWBOF%I(K).EQ.KENT) THEN
            IFRLIQ=NLIQBED%I(K)
            IF(IFRLIQ.GT.0) THEN
!             GRADZF IS THE GRADIENT OF THE BED, I.E. OUTWARD NORMAL
!             THE Z COMPONENT IS ASSUMED TO BE ALWAYS NEGATIVE
              XNB=GRADZF%ADR(1)%P%R(K)
              YNB=GRADZF%ADR(2)%P%R(K)
              ZNB=-SQRT(1.D0-XNB**2-YNB**2)
!             NO OUTFLOW IF NO WATER
              IF(H%R(K).LT.1.D-4.AND.BEDFLO(IFRLIQ).LE.0.D0) THEN
                UBORF%R(K)=0.D0
                VBORF%R(K)=0.D0
                WBORF%R(K)=0.D0
              ELSE
                UBORF%R(K)=-XNB*BEDFLO(IFRLIQ)/BEDQAREA(IFRLIQ)
                VBORF%R(K)=-YNB*BEDFLO(IFRLIQ)/BEDQAREA(IFRLIQ)
                WBORF%R(K)=-ZNB*BEDFLO(IFRLIQ)/BEDQAREA(IFRLIQ)
              ENDIF
            ENDIF
          ENDIF
!
        ENDDO ! NPOIN2
!
      ENDIF ! IF(NBEDFLO.GT.0)
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!           END OF AUTOMATIC TREATMENT OF LIQUID BOUNDARIES
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                               WIND
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
      IF(VENT) THEN
        ROEAU = 1000.D0
        ROAIR = 1.3D0
        DO IPOIN2 = 1,NPOIN2
!         RELATIVE WIND
          WINDRELX=WIND%ADR(1)%P%R(IPOIN2)-U%R(NPOIN3-NPOIN2+IPOIN2)
          WINDRELY=WIND%ADR(2)%P%R(IPOIN2)-V%R(NPOIN3-NPOIN2+IPOIN2)
          VITV=SQRT(WINDRELX**2+WINDRELY**2)
!         A MORE ACCURATE TREATMENT
!         IF(VITV.LE.5.D0) THEN
!           FAIR = ROAIR/ROEAU*0.565D-3
!         ELSEIF (VITV.LE.19.22D0) THEN
!           FAIR = ROAIR/ROEAU*(-0.12D0+0.137D0*VITV)*1.D-3
!         ELSE
!           FAIR = ROAIR/ROEAU*2.513D-3
!         ENDIF
!         BEWARE : BUBORS IS VISCVI*DU/DN, NOT DU/DN
          IF(H%R(IPOIN2).GT.HWIND) THEN
!           EXPLICIT PART
            BUBORS%R(IPOIN2) =  FAIR*VITV*WIND%ADR(1)%P%R(IPOIN2)
            BVBORS%R(IPOIN2) =  FAIR*VITV*WIND%ADR(2)%P%R(IPOIN2)
!           IMPLICIT PART
            AUBORS%R(IPOIN2) = -FAIR*VITV
            AVBORS%R(IPOIN2) = -FAIR*VITV
          ELSE
            BUBORS%R(IPOIN2) = 0.D0
            BVBORS%R(IPOIN2) = 0.D0
            AUBORS%R(IPOIN2) = 0.D0
            AVBORS%R(IPOIN2) = 0.D0
          ENDIF
        ENDDO
      ENDIF
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                         END OF WIND TREATMENT
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                     HEAT EXCHANGE WITH ATMOSPHERE
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!                 LINES BELOW ARE AN EXAMPLE
!                                    =======
!    TO BE GIVEN :
!
!    TAIR  = AIR TEMPERATURE WHICH MAY VARY WITH TIME
!    SAL   = SALINITY WHICH MAY VARY WITH TIME
!
!     EXCHANGE WITH ATMOSPHERE CORRESPONDS NOW TO WAQPROCESS=5
      IF(INCLUS(COUPLING,'WAQTEL').AND.WAQPROCESS.EQ.5)THEN
!       IMPORTANT:
!       STATES THAT ATABOS AND BTABOS ARE NOT ZERO (SEE LIMI3D AND DIFF3D)
!       OTHERWISE THEY WILL NOT BE CONSIDERED
        ATABOS%ADR(IND_T)%P%TYPR='Q'
        BTABOS%ADR(IND_T)%P%TYPR='Q'
! 
        CALL CALCS3D_THERMICS(NPOIN2,NPOIN3,IND_T,IND_S,TA,ATABOS,
     &                        BTABOS,PATMOS,ATMOSEXCH,WIND,LISTIN)   
      ENDIF
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                 END OF HEAT EXCHANGE WITH ATMOSPHERE
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!-----------------------------------------------------------------------
!
!     OPTIMISATION:
!
!     EXPLICIT STRESSES WILL NOT BE TREATED IF SAID TO BE 0
!
!     EXPLICIT STRESSES SET TO 0 ON VELOCITIES (UNLESS PROGRAMMED
!                                               IN THIS SUBROUTINE):
!
      BUBORF%TYPR='0'
      BUBORL%TYPR='0'
      BVBORF%TYPR='0'
      BVBORL%TYPR='0'
      BWBORF%TYPR='0'
      BWBORL%TYPR='0'
      BWBORS%TYPR='0'
!
!     CASE OF WIND (SEE ABOVE)
!
      IF(VENT) THEN
        BUBORS%TYPR='Q'
        BVBORS%TYPR='Q'
        AUBORS%TYPR='Q'
        AVBORS%TYPR='Q'
      ELSE
        BUBORS%TYPR='0'
        BVBORS%TYPR='0'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END


