!
!  CHANGES VS SOURCE FILES:
!  IN BORD3D: SPECIFIC TREATMENT FOR LIQUID BOUNDARIES (H,U,V,P)
!  IN T3D_CORFON
!  IN LIMI3D: LATERAL SURFACES: DIRICHLET ON ENTRANCES, NEUMANN ELSEWHERE
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
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
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
      INTEGER I
      DOUBLE PRECISION PI,PER,L,HEIGHT,DEPTH,KK,OMEGA,ZZ,ZSURF,TPS
!
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
!     BEGIN OF PART SPECIFIC TO THIS CASE
!
      PI=3.1415926535897932384626433D0
      PER=1.01D0
      HEIGHT=0.041D0
      DEPTH=0.43D0
      OMEGA=2.D0*PI/PER
      L=1.D0
      DO I=1,100
        L=9.81D0*PER**2/2.D0/PI*TANH(2.D0*PI*DEPTH/L)
      ENDDO
      KK=2.D0*PI/L
!     TPS=AT+PER/2.D0
      TPS=AT
!
      DO I=1,NPTFR2
!
        IF(LIHBOR%I(I).EQ.KENT.OR.LIUBOL%I(I).EQ.KENTU) THEN
!
          HBOR%R(I)=-ZF%R(NBOR2%I(I))
     &             +(HEIGHT*COS(OMEGA*TPS)/2.D0
     &    +(KK*HEIGHT**2/16.D0)*(COSH(KK*DEPTH)/SINH(KK*DEPTH)**3)
     &    *(2.D0+COSH(2.D0*KK*DEPTH))*COS(2.D0*OMEGA*TPS))*
!         RAMPE DE 1 S SUR LE TEMPS
     &    MIN(1.D0,AT)
!
          DO  IPLAN=1, NPLAN
            IBORD = (IPLAN-1)*NPTFR2 + I
            ZSURF=Z(NBOR3%I((NPLAN-1)*NPTFR2 + I))
            ZZ=Z(NBOR3%I(IBORD))-ZSURF
            UBORL%R(IBORD)=OMEGA*COS(OMEGA*TPS)*HEIGHT/
     &             2.D0*COSH(KK*(ZZ+DEPTH))/SINH(KK*DEPTH)
     & +3.D0/16.D0*OMEGA*KK*HEIGHT**2*COSH(2.D0*KK*(ZZ+DEPTH))/
     &      SINH(KK*DEPTH)**4*COS(2.D0*OMEGA*TPS)
            VBORL%R(IBORD)=0.D0
            PBORL%R(IBORD)=DT*9.81D0*HEIGHT*COS(OMEGA*TPS)/2.D0*
     &                 (COSH(KK*(ZZ+DEPTH))/COSH(KK*DEPTH)-1.D0)
!
            UBORL%R(IBORD)=UBORL%R(IBORD)*MIN(1.D0,AT)
            VBORL%R(IBORD)=VBORL%R(IBORD)*MIN(1.D0,AT)
            PBORL%R(IBORD)=PBORL%R(IBORD)*MIN(1.D0,AT)
!
          ENDDO
!
        ENDIF
      ENDDO
!
!     END OF PART SPECIFIC TO THIS CASE
!
!-----------------------------------------------------------------------
!
      RETURN
      END


!                    *********************
                     SUBROUTINE T3D_CORFON
!                    *********************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!+
!+            STANDARD ACTION: SMOOTHES THE BOTTOM ELEVATION.
!+
!+           (KEYWORD:  'NUMBER OF BOTTOM SMOOTHINGS')
!
!note     EQUIVALENT TO CORFON (BIEF LIBRARY), EXCEPT THAT THIS
!+         SUBROUTINE DISTINGUISHES DATA FROM STRUCTURES.
!
!history  J.M. JANIN  (LNH)
!+        25/11/97
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  J-M HERVOUET (LNHE)
!+        29/09/2011
!+        V6P2
!+   Name changed into T3D_CORFON to avoid duplication with Telemac-2D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LISFON         |-->| NUMBER OF SMOOTHINGS REQUIRED
!| MASKEL         |-->| MASK OF ELEMENTS
!| MATR2D         |<->| WORK MATRIX IN 2DH
!| MESH2D         |<->| 2D MESH
!| MSK            |-->| IF YES, THERE ARE MASKED ELEMENTS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| PRIVE          |<->| BLOCK OF PRIVATE ARRAYS FOR USER
!| S              |-->| VOID STRUCTURE
!| ST1            |<->| STRUCTURE OF T1
!| ST2            |<->| STRUCTURE OF T2
!| SZF            |<->| STRUCTURE OF ZF
!| T1             |<->| WORK ARRAY
!| T2             |<->| WORK ARRAY
!| X              |-->| MESH COORDINATE
!| Y              |-->| MESH COORDINATE
!| ZF             |<->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2, LISFON
      LOGICAL, INTENT(IN) :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SZF, ST1, ST2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: ZF, T1, T2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(IN) :: X,Y
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: PRIVE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MATR2D
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: S
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      LOGICAL MAS
      DOUBLE PRECISION A0,A1,A2,A3,A4,A5,A6,A7
!
!-----------------------------------------------------------------------
!
!     SMOOTHES THE BOTTOM ELEVATION
!
      IF(LISFON.GT.0) THEN
!
        MAS = .TRUE.
!
        CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &              1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)
      ENDIF
!
!-----------------------------------------------------------------------
!
      A1=3.69D0
      A2=5.985D0
      A3=12.D0
      A4=14.D0
      A5=17.015D0
      A6=20.785D0
!
      A0=A1-1.D0
      A7=A6+1.D0
!
! BAR DEFINITION + SMALL TRANSITIONS
!
      DO  I=1,NPOIN2
!
        ZF(I)=-0.43D0
        IF (X(I).GE.A0.AND.X(I).LE.A1) THEN
          ZF(I)= -0.43D0+0.03D0*(X(I)-A0)/(A1-A0)
        ENDIF
!
        IF (X(I).GE.A1.AND.X(I).LE.A2) THEN
          ZF(I)=-0.40D0
        ENDIF
        IF (X(I).GE.A2.AND.X(I).LE.A3) THEN
          ZF(I)=-0.40D0+0.3D0*(X(I)-A2)/(A3-A2)
        ENDIF
        IF (X(I).GE.A3.AND.X(I).LE.A4) THEN
          ZF(I)=-0.10D0
        ENDIF
        IF (X(I).GE.A4.AND.X(I).LE.A5) THEN
          ZF(I)=-0.1D0-0.3D0*(X(I)-A4)/(A5-A4)
        ENDIF
        IF (X(I).GE.A5.AND.X(I).LE.A6) THEN
          ZF(I)=-0.40D0
        ENDIF
        IF(X(I).GE.A6.AND.X(I).LE.A7) THEN
          ZF(I)= -0.40D0-0.03D0*(X(I)-A6)/(A7-A6)
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE LIMI3D
!                    *****************
!
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES TYPES OF 3D BOUNDARY CONDITIONS.
!+
!+            SETS THE VALUE OF SOME COEFFICIENTS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNHE)
!+
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
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPOIN2, IPLAN, IPTFR, IPTFR3, ITRAC
!
!***********************************************************************
!
!     BOUNDARY CONDITIONS ON VELOCITIES
!     *********************************
!
!     BOTTOM
!     ======
!
!     DEFAULT: IMPERMEABILITY AND LOG LAW (SEE ALSO BORD3D)
!
      IF(BC_BOTTOM.EQ.1) THEN
!
        DO IPOIN2 = 1,NPOIN2
          LIUBOF%I(IPOIN2) = KLOG
          LIVBOF%I(IPOIN2) = KLOG
          LIWBOF%I(IPOIN2) = KLOG
!         USEFUL ? SHOULD NOT BE USED ANYWAY
          UBORF%R(IPOIN2)  = 0.D0
          VBORF%R(IPOIN2)  = 0.D0
          WBORF%R(IPOIN2)  = 0.D0
        ENDDO
!
      ELSEIF(BC_BOTTOM.EQ.2) THEN
!
        DO IPOIN2 = 1,NPOIN2
          LIUBOF%I(IPOIN2) = KADH
          LIVBOF%I(IPOIN2) = KADH
          LIWBOF%I(IPOIN2) = KADH
!         USEFUL ? KADH SAYS IT IS 0.D0
          UBORF%R(IPOIN2)  = 0.D0
          VBORF%R(IPOIN2)  = 0.D0
          WBORF%R(IPOIN2)  = 0.D0
        ENDDO
!
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LIMI3D : FAUSSE CONDITION A LA LIMTE AU FOND'
          WRITE(LU,*) '         VALEUR ',BC_BOTTOM,' INCONNUE'
        ENDIF
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LIMI3D : BAD BOUNDARY CONDITION ON THE BOTTOM'
          WRITE(LU,*) '         VALUE ',BC_BOTTOM,' UNKNOWN'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     COEFFICIENTS SET TO 0 BY MEANS OF THEIR COMPONENT TYPR
!     NO DIFFUSION FLUX THROUGH BOTTOM
!
      AUBORF%TYPR='0'
      AVBORF%TYPR='0'
      BUBORF%TYPR='0'
      BVBORF%TYPR='0'
      IF(NONHYD) THEN
        AWBORF%TYPR='0'
        BWBORF%TYPR='0'
      ENDIF
!
!     LATERAL BOUNDARIES
!     ==================
!
!     DEFAULT: 2D CONDITIONS DUPLICATED ON THE VERTICAL
!              FREE FOR W
!              NO FRICTION
!
      DO IPLAN = 2,NPLAN
        DO IPTFR = 1,NPTFR2
          IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
          LIUBOL%I(IPTFR3) = LIUBOL%I(IPTFR)
          LIVBOL%I(IPTFR3) = LIVBOL%I(IPTFR)
          UBORL%R(IPTFR3)  = UBORL%R(IPTFR)
          VBORL%R(IPTFR3)  = VBORL%R(IPTFR)
          AUBORL%R(IPTFR3) = AUBORL%R(IPTFR)
        ENDDO
      ENDDO
!
!     IDEA OF OPTIMISATION (BEWARE PARALLELISM)
!
!     IF(DOTS(AUBORL,AUBORL).GT.1.D-8) THEN
!       AUBORL%TYPR='Q'
!     ELSE
!       AUBORL%TYPR='0'
!     ENDIF
!
      DO IPTFR3 = 1,NPTFR3
!                           KSORT: W FREE ON LATERAL BOUNDARIES
        LIWBOL%I(IPTFR3)        = KSORT
!       VALUES SAVED IN SECOND DIMENSION BECAUSE ADVECTION
!       SCHEMES MAY CHANGE THE VALUES
        LIUBOL%I(IPTFR3+NPTFR3) = LIUBOL%I(IPTFR3)
        LIVBOL%I(IPTFR3+NPTFR3) = LIVBOL%I(IPTFR3)
        LIWBOL%I(IPTFR3+NPTFR3) = LIWBOL%I(IPTFR3)
        WBORL%R(IPTFR3)  = 0.D0
!       BUBORL%R(IPTFR3) = 0.D0
!       BVBORL%R(IPTFR3) = 0.D0
      ENDDO
      BUBORL%TYPR='0'
      BVBORL%TYPR='0'
!
      IF(NONHYD) THEN
!       DO IPTFR3 = 1,NPTFR3
!         AWBORL%R(IPTFR3) = 0.D0
!         BWBORL%R(IPTFR3) = 0.D0
!       ENDDO
        AWBORL%TYPR='0'
        BWBORL%TYPR='0'
      ENDIF
!
!     FREE SURFACE
!     ============
!
!     DEFAULT: IMPERMEABILITY AND NO FRICTION (SEE ALSO BORD3D)
!
      DO IPOIN2 = 1,NPOIN2
        LIUBOS%I(IPOIN2) = KLOG
        LIVBOS%I(IPOIN2) = KLOG
        LIWBOS%I(IPOIN2) = KLOG
        UBORS%R(IPOIN2)  = 0.D0
        VBORS%R(IPOIN2)  = 0.D0
        WBORS%R(IPOIN2)  = 0.D0
!       AUBORS%R(IPOIN2) = 0.D0
!       BUBORS%R(IPOIN2) = 0.D0
!       BVBORS%R(IPOIN2) = 0.D0
      ENDDO
      AUBORS%TYPR='0'
      BUBORS%TYPR='0'
      AVBORS%TYPR='0'
      BVBORS%TYPR='0'
!
      IF(NONHYD) THEN
!       DO IPOIN2 = 1,NPOIN2
!         AWBORS%R(IPOIN2) = 0.D0
!         BWBORS%R(IPOIN2) = 0.D0
!       ENDDO
        AWBORS%TYPR='0'
        BWBORS%TYPR='0'
      ENDIF
!
!     **************
!     TRACERS BC'S
!     **************
!
      IF (NTRAC.NE.0) THEN
        DO ITRAC = 1,NTRAC
!
!     BOTTOM
!     ======
!
!     DEFAULT: NEUMANN BC'S
!
          DO IPOIN2 = 1,NPOIN2
            LITABF%ADR(ITRAC)%P%I(IPOIN2) = KLOG
            TABORF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           ATABOF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           BTABOF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
          ENDDO
          ATABOF%ADR(ITRAC)%P%TYPR='0'
          BTABOF%ADR(ITRAC)%P%TYPR='0'
!
!     SIDES
!     =====
!
!     DEFAULT: NEUMANN BC'S
!
!           WHAT HAS BEEN READ IN THE BOUNDARY CONDITIONS FILE
!           FOR 1 TRACER IS DUPLICATED ON THE VERTICAL AND FOR
!           ALL TRACERS
!
          DO IPLAN = 1,NPLAN
            DO IPTFR = 1,NPTFR2
              IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
              LITABL%ADR(ITRAC)%P%I(IPTFR3) = LITABL%ADR(1)%P%I(IPTFR)
!             SAVING ON SECOND DIMENSION BECAUSE ADVECTION SCHEMES
!             MAY CHANGE THIS VALUE
              LITABL%ADR(ITRAC)%P%I(IPTFR3+NPTFR3) =
     &                                        LITABL%ADR(1)%P%I(IPTFR)
              TABORL%ADR(ITRAC)%P%R(IPTFR3) = TABORL%ADR(1)%P%R(IPTFR)
!             ATABOL%ADR(ITRAC)%P%R(IPTFR3) = ATABOL%ADR(1)%P%R(IPTFR)
!             BTABOL%ADR(ITRAC)%P%R(IPTFR3) = BTABOL%ADR(1)%P%R(IPTFR)
            ENDDO
          ENDDO
          ATABOL%ADR(ITRAC)%P%TYPR='0'
          BTABOL%ADR(ITRAC)%P%TYPR='0'
!
!     FREE SURFACE
!     =============
!
!     DEFAULT: NEUMANN BC'S
!
          DO IPOIN2 = 1,NPOIN2
             LITABS%ADR(ITRAC)%P%I(IPOIN2) = KLOG
             TABORS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!            ATABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!            BTABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
          ENDDO
          ATABOS%ADR(ITRAC)%P%TYPR='0'
          BTABOS%ADR(ITRAC)%P%TYPR='0'
!
        ENDDO
      ENDIF
!
!     SOLID BOUNDARIES FOR K AND EPSILON
!     **********************************
!
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
!
!     BOTTOM
!     ======
!
!     DEFAULT : NO GRADIENT
!
        DO IPOIN2 = 1,NPOIN2
          AKBORF%R(IPOIN2) = 0.D0
          BKBORF%R(IPOIN2) = 0.D0
          AEBORF%R(IPOIN2) = 0.D0
          BEBORF%R(IPOIN2) = 0.D0
        ENDDO
        AKBORF%TYPR = '0'
        BKBORF%TYPR = '0'
        AEBORF%TYPR = '0'
        BEBORF%TYPR = '0'
!
!     SIDES
!     =====
!
!     DEFAULT : NO GRADIENT
!
        DO IPTFR3 = 1,NPTFR3
          AKBORL%R(IPTFR3) = 0.D0
          BKBORL%R(IPTFR3) = 0.D0
          AEBORL%R(IPTFR3) = 0.D0
          BEBORL%R(IPTFR3) = 0.D0
        ENDDO
!
!     FREE SURFACE
!     ============
!
!     DEFAULT : NO GRADIENT
!
        DO IPOIN2 = 1,NPOIN2
           AKBORS%R(IPOIN2) = 0.D0
           BKBORS%R(IPOIN2) = 0.D0
           AEBORS%R(IPOIN2) = 0.D0
           BEBORS%R(IPOIN2) = 0.D0
        ENDDO
!
      ENDIF
!
!
!     FRICTION COEFFICIENTS
!     *********************
!
!     DEFAULT: VALUE GIVEN IN STEERING FILE
!
      CALL OV('X=C     ',RUGOL%R,RUGOL%R,RUGOL%R,RUGOL0,NPTFR2*NPLAN)
!
!======================================================================
! DEFAULT BOUNDARY CONDITION TYPES AND VALUES FOR THE
! PRESSURE POISSON EQUATION
!======================================================================
!
      IF(NONHYD) THEN
!
!-----------------------------------------------------------------------
!
! DEFAULT TYPES AND VALUES FOR THE PRESSURE BOUNDARY CONDITIONS
! BOTTOM AND FREE SURFACE
!
!       AT ALL LATERAL BOUNDARIES AND BOTTOM DP/DN = 0;
!       DIRICHLET = 0 AT THE SURFACE; DIRICHLET CONDITIONS SET TO 0 ALL OVER
!       (CORRECT AT THE SURFACE ONLY)
!
!       BOTTOM AND SURFACE
!       CHECK KLOG BOTTOM
!
        DO IPOIN2=1,NPOIN2
          LIPBOF%I(IPOIN2) = KLOG
          LIPBOS%I(IPOIN2) = KENT
          PBORF%R(IPOIN2)  = 0.D0
          PBORS%R(IPOIN2)  = 0.D0
        ENDDO
!
!       LATERAL SURFACES: ALL TREATED AS NEUMANN
!
        DO IPTFR3=1,NPTFR3
          LIPBOL%I(IPTFR3) = KLOG
          PBORL%R(IPTFR3)  = 0.D0
        ENDDO
!
!       LATERAL SURFACES: DIRICHLET ON ENTRANCES, NEUMANN ELSEWHERE
!
! BEGIN OF PART SPECIFIC TO THIS CASE
        DO IPTFR3=1,NPTFR3
          PBORL%R(IPTFR3)  = 0.D0
          IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
     &       LIUBOL%I(IPTFR3).EQ.KENTU) THEN
            LIPBOL%I(IPTFR3) = KENT
          ELSE
            LIPBOL%I(IPTFR3) = KLOG
          ENDIF
        ENDDO
! END OF PART SPECIFIC TO THIS CASE
!
      ENDIF
!
!======================================================================
!
      RETURN
      END
