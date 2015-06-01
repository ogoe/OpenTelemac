!                       *****************
                        SUBROUTINE BORD3D
!                       *****************
!
     & (AAT,LT,IINFOGR,NPTFR2_DIM,NFRLIQ)
!
!***********************************************************************
! TELEMAC 3D VERSION 5.6 18/10/2005 J.-M. HERVOUET (LNHE) 01 30 87 80 18
!
!***********************************************************************
!
!      FONCTION:
!      =========
!
!      CONDITIONS AUX LIMITES SPECIFIQUES. PEUT ETRE MODIFIE PAR
!      L'UTILISATEUR
!
!-----------------------------------------------------------------------
!
!      FUNCTION:
!      =========
!
!      SPECIFIC BOUNDARY CONDITIONS, MAY BE MODIFIED BY THE USER.
!
!-----------------------------------------------------------------------
!                          SOME USEFUL PARAMETERS
! .________________.____.______________________________________________.
! !  NOM           !MODE!                  ROLE                        !
! !________________!____!______________________________________________!
! !  UBORF         !<-- ! PRESCRIBED VELOCITY ALONG X ON THE BOTTOM
! !  UBORL         !<-- ! PRESCRIBED VELOCITY ALONG X ON THE LATERAL
! !                !    ! BOUNDARY
! !  UBORS         !<-- ! PRESCRIBED VELOCITY ALONG X AT FREE SURFACE
! !  VBORF         !<-- ! PRESCRIBED VELOCITY ALONG Y ON THE BOTTOM
! !  VBORL         !<-- ! PRESCRIBED VELOCITY ALONG Y ON THE LATERAL
! !                !    ! BOUNDARY
! !  VBORS         !<-- ! PRESCRIBED VELOCITY ALONG Y AT FREE SURFACE
! !  WBORF         !<-- ! PRESCRIBED VELOCITY ALONG Z ON THE BOTTOM
! !  WBORL         !<-- ! PRESCRIBED VELOCITY ALONG Z ON THE LATERAL
! !                !    ! BOUNDARY
! !  WBORS         !<-- ! PRESCRIBED VELOCITY ALONG Z AT FREE SURFACE
! !  TABORF        !<-- ! PRESCRIBED TRACERS ON THE BOTTOM
! !  TABORL        !<-- ! PRESCRIBED TRACERS ON THE LATERAL BOUNDARY
! !  TABORS        !<-- ! PRESCRIBED TRACERS AT FREE SURFACE
! !                !    !
! !                !    ! LOGARITHMIC LAWS : AUBORF,L,S AND BUBORF,L,S
! !                !    !                    FOR VELOCITIES
! !                !    !                    ATABO,L,S AND BTABO,L,S
! !                !    !                    FOR TRACERS
! !                !    !
! ! AUBOR,BUBOR    !<-- ! LOG LAW: NU*DU/DN = AUBOR*U + BUBOR
! !                !    ! IF BUBOR(F,L,S) ADDED HERE, SPECIFY BUBOR%TYPR='Q'
! !                !    ! SEE END OF THE ROUTINE
! ! AUBOR,BVBOR    !<-- ! LOG LAW: NU*DV/DN = AVBOR*V + BVBOR
! !                !    ! IF BVBOR(F,L,S) ADDED HERE, SPECIFY BVBOR%TYPR='Q'
! !                !    ! SEE END OF THE ROUTINE
! ! AWBOR,BWBOR    !<-- ! LOG LAW: NU*DW/DN = AWBOR*W + BWBOR
! !                !    ! IF BWBOR(F,L,S) ADDED HERE, SPECIFY BWBOR%TYPR='Q'
! !                !    ! SEE END OF THE ROUTINE
! !                !    !
! ! ATABO,BTABO    !<-- ! LOG LAW: NU*DTA/DN = ATABO*TA + BTABO
! !                !    !
! !                !    ! TYPES OF BOUNDARY CONDITIONS
! !                !    !
! !  LIU,V,WBOF    !<-->! ON BOTTOM FOR U,V,W
! !  LIU,V,WBOL    !<-->! ON LATERAL BOUNDARIES FOR U,V,W
! !  LIU,V,WBOS    !<-->! AT FREE SURFACE FOR U,V,W
! !  LITA,BF       !<-->! ON BOTTOM FOR TRACERS
! !  LITA,BL       !<-->! ON LATERAL BOUNDARIES FOR TRACERS
! !  LITA,BS       !<-->! AT FREE SURFACE FOR TRACERS
! !                !    !
! !  U,V,W         ! -->! COMPONENTS OF VELOCITY
! !  UMOY,VMOY     ! -->! DEPTH AVERAGED VELOCITY
! !  TA            ! -->! CONCENTRATION OF TRACERS
! !  ITURBV        ! -->! TURBULENCE MODEL (1:LAMINAR 2: MIXING LENGTH..)
! !  VENT          ! -->! WITH WIND (.TRUE.) OR WITHOUT (.FALSE.)
! !  FAIR          ! -->! DRAG COEFFICIENT OF WIND
! !  VENTX         ! -->! WIND VELOCITY ALONG X
! !  VENTY         ! -->! WIND VELOCITY ALONG Y
! !  AT            ! -->! TIME
! !  LT            ! -->! CURRENT NUMBER OF TIME-STEP
! !  DT            ! -->! TIME-STEP
! !  LIHBOR        ! -->! TYPE OF BOUNDARY CONDITIONS ON DEPTH
! !  HBOR          ! -->! PRESCRIBED DEPTH ON LATERAL BOUNDARIES
! !  HN            ! -->! DEPTH AT TIME TN
! !  X,Y,Z         ! -->! MESH COORDINATES
! !  ZF            ! -->! BOTTOM ELEVATION
! !  NBOR          ! -->! GLOBAL NUMBER OF 2D BOUNDARY POINTS
! !  NELEM3        ! -->! NUMBER OF 3D ELEMENTS
! !  IKLE3         ! -->! CONNECTIVITY TABLE IN 3D
! !  KP1BOR        ! -->! IN 2D : NEXT POINT ON THE BOUNDARY
! !                !    ! (BOUNDARY NUMBERING)
! ! XSGBOR,YSGBOR  ! -->! 2D NORMAL VECTORS TO THE SEGMENTS
! !  NPOIN3        ! -->! NUMBER OF POINTS IN 3D
! !  NPOIN2        ! -->! NUMBER OF POINTS IN 2D
! !  NETAGE        ! -->! NUMBER OF LAYERS OF 3D ELEMENTS
! !  NPTFR         ! -->! NUMBER OF BOUNDARY POINTS IN 2D
! !  NPTFR3        ! -->! NUMBER OF BOUNDARY POINTS IN 3D
! !  NPLAN         ! -->! NUMBER OF PLANES ON THE VERTICAL
! !  NELEM2        ! -->! NUMBER OF ELEMENTS IN 2D
! !                !    !
! !                !    ! POSSIBLE TYPES OF BOUNDARY CONDITIONS
! !                !    !
! !  KENT          ! -->! PRESCRIBED VALUE
! !  KENTU         ! -->! PRESCRIBED VELOCITY
! !  KSORT         ! -->! FREE (E.G. AT AN OUTPUT)
! !  KADH          ! -->! NO SLIP CONDITION
! !  KLOG          ! -->! SOLID BOUNDARY
! !  NTRAC         ! -->! NUMBER OF TRACERS
! !  SEDI          ! -->! IF YES, THERE IS SEDIMENT
! !  PRIVE         ! -->! BLOCK OF ARRAYS FOR THE USER
! !  NPRIV         ! -->! NUMBER OF ARRAYS IN BLOCK PRIVE
! !  NDEBIT        ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
! !  NVIT          ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED VELOCITY
! !  NCOTE         ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
! !  DEBIMP        ! -->! ARRAY OF PRESCRIBED DISCHARGES
! !  COTIMP        ! -->! ARRAY OF PRESCRIBED ELEVATIONS
! !  VITIMP        ! -->! ARRAY OF PRESCRIBED VELOCITIES
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!***********************************************************************
!
! INFORMATION : FOR PRESCRIBED BOUNDARIES OF POINT BEING BOTH LATERAL
!               AND BOTTOM, USE LATERAL ARRAYS.
!
!               FOR TYPES OF BOUNDARY CONDITIONS : USE SUBROUTINE LIMI3D
!
!               SEDIMENT IS THE LAST TRACER
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, EX_NFRLIQ=>NFRLIQ
      USE INTERFACE_TELEMAC3D, EX_BORD3D => BORD3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: AAT
      INTEGER         , INTENT(IN)    :: LT
      LOGICAL         , INTENT(IN)    :: IINFOGR
      INTEGER         , INTENT(IN)    :: NPTFR2_DIM
      INTEGER         , INTENT(IN) :: NFRLIQ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IPOIN2,NP,K1,IBORD,IVIT,ICOT,IDEB,IFRLIQ,IPROF
      INTEGER IPTFR,ITRAC,IPLAN
      DOUBLE PRECISION ROEAU,ROAIR,VITV,PROFZ,TPS
!
!     PARAMETERS FOR LIQUID BOUNDARIES
!     ********************************
!
      INTEGER K,N
!
      INTEGER  P_IMAX
      EXTERNAL P_IMAX
!
!--------------------------------------------------------------
!
      DOUBLE PRECISION XB,YB,ZB,NORM,CP
      DOUBLE PRECISION RO0,A,B,SAL,WW,TREEL,RO,LAMB
!
      DOUBLE PRECISION PI,PER,L,HEIGHT,DEPTH,KK,OMEGA,ZZ,ZSURF
!
      INTEGER YADEB(100),MSK1,IPTFR2,I2,IJK,ITEMP
!
!     SIMPLE CASES FOR LATERAL BOUNDARIES ARE TREATED AUTOMATICALLY:
!
!     - PRESCRIBED DEPTH     (5 4 4)
!     - PRESCRIBED VELOCITY  (  6 6)
!     - PRESCRIBED DISCHARGE (  5 5)
!
!     CORRESPONDING KEY-WORDS ARE:
!
!     'PRESCRIBED ELEVATIONS' OR 'COTES IMPOSEES'
!     'PRESCRIBED VELOCITIES' OR 'VITESSES IMPOSEES'
!     'PRESCRIBED FLOWRATES' OR 'DEBITS IMPOSES'
!
!     THE IMPLEMENTATION OF AUTOMATIC CASES MAY BE CANCELLED
!     PROVIDED THAT THE  RELEVANT ARRAYS ARE FILLED
!
!
!***********************************************************************
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!              AUTOMATIC TREATMENT OF LIQUID BOUNDARIES
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!=======================================================================
!
!  PREVIOUS LIMLOK : SECURING NO SLIP BOUNDARY CONDITIONS
!
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
!
!
!  FOR ALL TIME STEPS
!
!     INITIALISING YADEB
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
!     PRESCRIBED ELEVATION GIVEN IN PARAMETER FILE (NCOTE<>0)
!     -------------------------------------------------------
!
      IF(LIHBOR%I(K).EQ.KENT.AND.NCOTE.NE.0) THEN
!
        IF(NCOTE.GE.NUMLIQ%I(K)) THEN
!
        ICOT=NUMLIQ%I(K)
        HBOR%R(K) = SL3(ICOT,AT,NBOR2%I(K),INFOGR)-ZF%R(NBOR2%I(K))
        HBOR%R(K) = MAX(0.D0,HBOR%R(K))
        ELSE
          IF(LNG.EQ.1) WRITE(LU,100) NUMLIQ%I(K)
100       FORMAT(1X,'BORD3D : COTES IMPOSEES EN NOMBRE INSUFFISANT',/,
     &           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,101) NUMLIQ%I(K)
101       FORMAT(1X,'BORD3D : MORE PRESCRIBED ELEVATIONS ARE REQUIRED',
     &           /,1X,'       IN THE PARAMETER FILE',/,
     &           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!     PRESCRIBED DISCHARGE GIVEN IN PARAMETER FILE (NDEBIT<>0)
!     --------------------------------------------------------
!
!     A VELOCITY PROFILE IS SET HERE AND WILL BE CORRECTED AFTER
!     TO GET THE CORRECT DISCHARGE (CALL TO DEBIMP3D)
!
      IF(LIUBOL%I(K).EQ.KENT.AND.NDEBIT.NE.0) THEN
!
        DO NP=1,NPLAN
          IJK=(NP-1)*NPTFR2+K
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
            UBORL%R(IJK)=-XNEBOR2%R(K) * SQRT(MAX(H%R(NBOR2%I(K)),0.D0))
            VBORL%R(IJK)=-YNEBOR2%R(K) * SQRT(MAX(H%R(NBOR2%I(K)),0.D0))
          ELSE
!           NORMAL AND NORM 1
            UBORL%R(IJK)=-XNEBOR2%R(K)
            VBORL%R(IJK)=-YNEBOR2%R(K)
          ENDIF
!         NO VELOCITY IF NO WATER
          IF(H%R(NBOR2%I(K)).LT.1.D-4) THEN
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
          U%R((NP-1)*NPOIN2+NBOR2%I(K))=UBORL%R(IJK)
          V%R((NP-1)*NPOIN2+NBOR2%I(K))=VBORL%R(IJK)
        ENDDO
!
        YADEB(NUMLIQ%I(K))=1
!
      ENDIF
!
!     PRESCRIBED VELOCITY GIVEN IN PARAMETER FILE (NVIT<>0)
!     -----------------------------------------------------
!
!     THIS VELOCITY IS CONSIDERED NORMAL TO THE BOUNDARY
!
      IF(LIUBOL%I(K).EQ.KENTU.AND.NVIT.NE.0) THEN
        IVIT=NUMLIQ%I(K)
        IF(NVIT.GE.IVIT) THEN
!
          DO NP=1,NPLAN
            IBORD = (NP-1)*NPTFR2+K
            UBORL%R(IBORD) =
     &      -MESH2D%XNEBOR%R(K)*VIT3(IVIT,AT,NBOR2%I(K),INFOGR)
            VBORL%R(IBORD) =
     &      -MESH2D%YNEBOR%R(K)*VIT3(IVIT,AT,NBOR2%I(K),INFOGR)
            WBORL%R(IBORD)=0.D0
          END DO
!
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
!     PRESCRIBED TRACER GIVEN IN PARAMETER FILE, BUT POSSIBLE
!     OVERWRITING IF LIQUID BOUNDARY FILE IS GIVEN
!     SEE FUNCTION TR3
!     -------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
        DO NP=1,NPLAN
          IBORD = (NP-1)*NPTFR2+K
          IFRLIQ=NUMLIQ%I(K)
          IF(LITABL%ADR(ITRAC)%P%I(IBORD).EQ.KENT.AND.NTRACER.NE.0) THEN
            IFRLIQ=NUMLIQ%I(K)
            IF(NTRACER.GE.IFRLIQ*NTRAC) THEN
              TABORL%ADR(ITRAC)%P%R(IBORD) =
     &        TR3(IFRLIQ,ITRAC,NBOR3%I(IBORD),AT,INFOGR)
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
              PROFZ=TRA_PROF_Z(IFRLIQ,NBOR2%I(K),
     &                         AT,LT,NP,INFOGR,IPROF,ITRAC)
              TABORL%ADR(ITRAC)%P%R(IBORD)=
     &        TABORL%ADR(ITRAC)%P%R(IBORD)*PROFZ
            ENDIF
          ENDIF
!
        ENDDO
        ENDDO
      ENDIF
!
      ENDDO
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
     &                      UBORL%R,VBORL%R,WBORL%R,
     &                      U,V,NUMLIQ%I,NUMLIQ_ELM%I,IFRLIQ,T3_02,
     &                      NPTFR2,NETAGE,MASK_3D%ADR(MSK1)%P,
     &                      MESH3D,EQUA,IELM2V,SVIDE,MASKTR,
     &                      MESH3D%NELEB)
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
      ENDDO
      ENDIF
!
!     RESETTING BOUNDARY CONDITIONS ON U AND V (WILL BE USED BY TFOND
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
          VITV  = SQRT(WIND%ADR(1)%P%R(IPOIN2)**2
     &               + WIND%ADR(2)%P%R(IPOIN2)**2)
!
! A MORE ACCURATE TREATMENT
!
!CX       IF(VITV.LE.5.D0) THEN
!CX         FAIR = ROAIR/ROEAU*0.565D-3
!CX       ELSEIF (VITV.LE.19.22D0) THEN
!CX         FAIR = ROAIR/ROEAU*(-0.12D0+0.137D0*VITV)*1.D-3
!CX       ELSE
!CX         FAIR = ROAIR/ROEAU*2.513D-3
!CX       ENDIF
!
! BEWARE : BUBORS IS VISCVI*DU/DN, NOT DU/DN
!
          BUBORS%R(IPOIN2) = FAIR*VITV*WIND%ADR(1)%P%R(IPOIN2)
          BVBORS%R(IPOIN2) = FAIR*VITV*WIND%ADR(2)%P%R(IPOIN2)
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
!                 LINES BELOW WITH '!C' ARE AN EXAMPLE
!
!    TO BE GIVEN :
!
!    ITEMP = NUMBER OF TRACER WHICH IS THE HEAT
!    TAIR  = CONSTANT AIR TEMPERATURE
!    SAL   = CONSTANT WATER SALINITY
!
!C    ITEMP=1
!C    CP=4.18D3
!C    RO0=999.972D0
!C    B=0.0025D0
!C    TAIR=15.D0
!C    SAL=35.D-3
!C    WW=0.D0
!C    IF (VENT) WW=VITV
!C    DO IPOIN2=1,NPOIN2
!C       TREEL=TA%ADR(ITEMP)%P%R(NPOIN3-NPOIN2+IPOIN2)
!C       RO=RO0*(1.D0-(7.D0*(TREEL-4.D0)*(TREEL-4.D0)-750.D0*SAL)*1D-6)
!C       LAMB=RO*CP
!C       A=(4.48D0+0.049D0*TREEL)+2021.5D0*B*(1.D0+WW)*
!C   &     (1.12D0+0.018D0*TREEL+0.00158D0*TREEL*TREEL)
!C       ATABOS%ADR(ITEMP)%P%R(IPOIN2)=-A/LAMB
!C       BTABOS%ADR(ITEMP)%P%R(IPOIN2)= A*TAIR/LAMB
!C    END DO
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
!     OPTIMIZATION:
!
!     EXPLICIT STRESSES WILL NOT BE TREATED IF SAID TO BE ZERO
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
      ELSE
        BUBORS%TYPR='0'
        BVBORS%TYPR='0'
      ENDIF
!
!-----------------------------------------------------------------------
!
!     Partie specifique a ce cas
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
      RETURN
      END
!                       *********************
                        SUBROUTINE T3D_CORFON
!                       *********************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
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
      INTEGER K,I, IMED
      LOGICAL MAS
      DOUBLE PRECISION A1, A2, A3, A4, A5, A6, A0, A7
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
! bar definition ++ small transitions
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
          ZF(I)=-0.40D0 +0.3D0*(X(I)-A2)/(A3-A2)
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
      RETURN
      END
!                       *****************
                        SUBROUTINE LIMI3D
!                       *****************
!
!***********************************************************************
! TELEMAC 3D VERSION 5.6
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
!      FUNCTION:
!      =========
!                 INITIALIZING TYPES OF 3D BOUNDARY CONDITIONS
!                    SETTING THE VALUE OF SOME COEFFICIENTS
!
!-----------------------------------------------------------------------
!                          SOME USEFUL PARAMETERS
! .________________.____.______________________________________________.
! !  NOM           !MODE!                  ROLE                        !
! !________________!____!______________________________________________!
! !  UBORF         !<-- ! PRESCRIBED VELOCITY ALONG X ON THE BOTTOM
! !  UBORL         !<-- ! PRESCRIBED VELOCITY ALONG X ON THE LATERAL
! !                !    ! BOUNDARY
! !  UBORS         !<-- ! PRESCRIBED VELOCITY ALONG X AT FREE SURFACE
! !  VBORF         !<-- ! PRESCRIBED VELOCITY ALONG Y ON THE BOTTOM
! !  VBORL         !<-- ! PRESCRIBED VELOCITY ALONG Y ON THE LATERAL
! !                !    ! BOUNDARY
! !  VBORS         !<-- ! PRESCRIBED VELOCITY ALONG Y AT FREE SURFACE
! !  WBORF         !<-- ! PRESCRIBED VELOCITY ALONG Z ON THE BOTTOM
! !  WBORL         !<-- ! PRESCRIBED VELOCITY ALONG Z ON THE LATERAL
! !                !    ! BOUNDARY
! !  WBORS         !<-- ! PRESCRIBED VELOCITY ALONG Z AT FREE SURFACE
! !  TABORF        !<-- ! PRESCRIBED TRACERS ON THE BOTTOM
! !  TABORL        !<-- ! PRESCRIBED TRACERS ON THE LATERAL BOUNDARY
! !  TABORS        !<-- ! PRESCRIBED TRACERS AT FREE SURFACE
! !                !    !
! !                !    ! LOGARITHMIC LAWS : AUBORF,L,S AND BUBORF,L,S
! !                !    !                    FOR VELOCITIES
! !                !    !                    ATABO,L,S AND BTABO,L,S
! !                !    !                    FOR TRACERS
! !                !    !
! ! AUBOR,BUBOR    !<-- ! LOG LAW: NU*DU/DN = AUBOR*U + BUBOR
! ! AUBOR,BVBOR    !<-- ! LOG LAW: NU*DV/DN = AVBOR*V + BVBOR
! ! ATABO,BTABO    !<-- ! LOG LAW: NU*DTA/DN = ATABO*TA + BTABO
! !                !    !
! !                !    ! TYPES OF BOUNDARY CONDITIONS
! !                !    !
! !  LIU,V,WBOF    !<-->! ON BOTTOM FOR U,V,W
! !  LIU,V,WBOL    !<-->! ON LATERAL BOUNDARIES FOR U,V,W
! !  LIU,V,WBOS    !<-->! AT FREE SURFACE FOR U,V,W
! !  LITA,BF       !<-->! ON BOTTOM FOR TRACERS
! !  LITA,BL       !<-->! ON LATERAL BOUNDARIES FOR TRACERS
! !  LITA,BS       !<-->! AT FREE SURFACE FOR TRACERS
! !                !    !
! !  U,V,W         ! -->! COMPONENTS OF VELOCITY
! !  UMOY,VMOY     ! -->! DEPTH AVERAGED VELOCITY
! !  TA            ! -->! CONCENTRATION OF TRACERS
! !  ITURBV        ! -->! TURBULENCE MODEL (1:LAMINAR 2: MIXING LENGTH..)
! !  VENT          ! -->! WITH WIND (.TRUE.) OR WITHOUT (.FALSE.)
! !  FAIR          ! -->! DRAG COEFFICIENT OF WIND
! !  VENTX         ! -->! WIND VELOCITY ALONG X
! !  VENTY         ! -->! WIND VELOCITY ALONG Y
! !  AT            ! -->! TIME
! !  LT            ! -->! CURRENT NUMBER OF TIME-STEP
! !  DT            ! -->! TIME-STEP
! !  LIHBOR        ! -->! TYPE OF BOUNDARY CONDITIONS ON DEPTH
! !  HBOR          ! -->! PRESCRIBED DEPTH ON LATERAL BOUNDARIES
! !  HN            ! -->! DEPTH AT TIME TN
! !  X,Y,Z         ! -->! MESH COORDINATES
! !  ZF            ! -->! BOTTOM ELEVATION
! !  NBOR          ! -->! GLOBAL NUMBER OF 2D BOUNDARY POINTS
! !  NELEM3        ! -->! NUMBER OF 3D ELEMENTS
! !  IKLE3         ! -->! CONNECTIVITY TABLE IN 3D
! !  KP1BOR        ! -->! IN 2D : NEXT POINT ON THE BOUNDARY
! !                !    ! (BOUNDARY NUMBERING)
! ! XSGBOR,YSGBOR  ! -->! 2D NORMAL VECTORS TO THE SEGMENTS
! !  NPOIN3        ! -->! NUMBER OF POINTS IN 3D
! !  NPOIN2        ! -->! NUMBER OF POINTS IN 2D
! !  NETAGE        ! -->! NUMBER OF LAYERS OF 3D ELEMENTS
! !  NPTFR         ! -->! NUMBER OF BOUNDARY POINTS IN 2D
! !  NPTFR3        ! -->! NUMBER OF BOUNDARY POINTS IN 3D
! !  NPLAN         ! -->! NUMBER OF PLANES ON THE VERTICAL
! !  NELEM2        ! -->! NUMBER OF ELEMENTS IN 2D
! !                !    !
! !                !    ! POSSIBLE TYPES OF BOUNDARY CONDITIONS
! !                !    !
! !  KENT          ! -->! PRESCRIBED VALUE
! !  KENTU         ! -->! PRESCRIBED VELOCITY
! !  KSORT         ! -->! FREE (E.G. AT AN OUTPUT)
! !  KADH          ! -->! NO SLIP CONDITION
! !  KLOG          ! -->! SOLID BOUNDARY
! !  NTRAC         ! -->! NUMBER OF TRACERS
! !  SEDI          ! -->! IF YES, THERE IS SEDIMENT
! !  PRIVE         ! -->! BLOCK OF ARRAYS FOR THE USER
! !  NPRIV         ! -->! NUMBER OF ARRAYS IN BLOCK PRIVE
! !  NDEBIT        ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
! !  NVIT          ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED VELOCITY
! !  NCOTE         ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
! !  DEBIMP        ! -->! ARRAY OF PRESCRIBED DISCHARGES
! !  COTIMP        ! -->! ARRAY OF PRESCRIBED ELEVATIONS
! !  VITIMP        ! -->! ARRAY OF PRESCRIBED VELOCITIES
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
! .________________.____.______________________________________________.
! |                |    |                                              |
! | PBOR(F,L,S)    |<-- | DIRICHLET BC VALUES FOR DYNAMIC PRESSURE     |
! | A,B)PBOR(F,L,S |<-- | NEUMANN BC VALUES FOR DYNAMIC PRESSURE       |
! | A,B)WBOR(F,L,S |<-- | NEUMANN BC VALUES FOR W VELOCITY COMPONENT   |
! | LIPBO(F,L,S)   |<-- | BC TYPE FOR DYNAMIC PRESSURE                 |
! | NONHYD         | -->| NON-HYDROSTATIC FLAG                         |
! |________________|____|______________________________________________|
!
!***********************************************************************
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
!     DEFAULT: IMPERMEABILITY AND NO FRICTION (SEE ALSO BORD3D)
!
      DO IPOIN2 = 1,NPOIN2
        LIUBOF%I(IPOIN2) = KLOG
        LIVBOF%I(IPOIN2) = KLOG
        LIWBOF%I(IPOIN2) = KLOG
        UBORF%R(IPOIN2)  = 0.D0
        VBORF%R(IPOIN2)  = 0.D0
        WBORF%R(IPOIN2)  = 0.D0
!       AUBORF%R(IPOIN2) = 0.D0
!       AVBORF%R(IPOIN2) = 0.D0
!       BUBORF%R(IPOIN2) = 0.D0
!       BVBORF%R(IPOIN2) = 0.D0
      ENDDO
      AUBORF%TYPR='0'
      AVBORF%TYPR='0'
      BUBORF%TYPR='0'
      BVBORF%TYPR='0'
!
      IF(NONHYD) THEN
!       DO IPOIN2=1,NPOIN2
!         AWBORF%R(IPOIN2) = 0.D0
!         BWBORF%R(IPOIN2) = 0.D0
!       ENDDO
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
      DO IPTFR3 = 1,NPTFR3
!                           KSORT: W FREE ON LATERAL BOUNDARIES
        LIWBOL%I(IPTFR3) = KSORT
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
!     TRACERS'S BC'S
!     **************
!
      IF (NTRAC.NE.0) THEN
        DO ITRAC = 1,NTRAC
!
!     BOTTOM
!     ======
!
!     DEFAULT: Neumann BC's
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
!     DEFAULT: Neumann BC's
!
!         WHAT HAS BEEN READ IN THE BOUNDARY CONDITIONS FILE
!         FOR 1 TRACER IS DUPLICATED ON THE VERTICAL AND FOR
!         ALL TRACERS.
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
!     DEFAULT: Neumann BC's
!
          DO IPOIN2 = 1,NPOIN2
            LITABS%ADR(ITRAC)%P%I(IPOIN2) = KLOG
            TABORS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           ATABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           BTABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
          ENDDO
          ATABOS%ADR(ITRAC)%P%TYPR='0'
          BTABOS%ADR(ITRAC)%P%TYPR='0'
!
        ENDDO
      ENDIF
!
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
!     DEFAULT: VALUE GIVEN IN PARAMETER FILE
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
!       LATERAL SURFACES
!
        DO IPTFR3=1,NPTFR3
          LIPBOL%I(IPTFR3) = KLOG
          PBORL%R(IPTFR3)  = 0.D0
! ESSAI: DIRICHLET DE PRESSION A L'ENTREE
          IF(LIUBOL%I(IPTFR3).EQ.KENTU
     &   .OR.LIUBOL%I(IPTFR3).EQ.KENT) LIPBOL%I(IPTFR3) = KENT
! FIN ESSAI
        ENDDO
!
      ENDIF
!
!======================================================================
!
      RETURN
      END
