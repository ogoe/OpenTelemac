C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SPECIFIC BOUNDARY CONDITIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  1) FOR PRESCRIBED BOUNDARIES OF POINT BEING BOTH LATERAL
!>            AND BOTTOM : USE LATERAL ARRAYS.
!><br>     2) FOR TYPES OF BOUNDARY CONDITIONS : USE SUBROUTINE LIMI3D.
!><br>     3) SEDIMENT IS THE LAST TRACER.

!>  @warning  MAY BE MODIFIED BY THE USER

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 21/08/2009
!> </td><td> J.-M. HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/02/2008
!> </td><td>
!> </td><td> LOOP ON THE BOUNDARY POINTS SPLIT IN 3 LOOPS TO REVERSE
!>           THE ORDER OF THE LOOP ON THE TRACERS AND THE LOOP ON THE
!>           POINTS (TO AVOID INOPPORTUNE PRINTOUTS IN READ_FIC_LIQ)
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C
C#######################################################################
C
                        SUBROUTINE BORD3D
     &(TIME,LT,ENTET,NPTFR2_DIM,NFRLIQ)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TIME
C| ATABO,BTABO    |<--| LOG LAW: NU*DTA/DN = ATABO*TA + BTABO
C|                |   | TYPES OF BOUNDARY CONDITIONS
C| AUBOR,BUBOR    |<--| LOG LAW: NU*DU/DN = AUBOR*U + BUBOR
C|                |   | IF BUBOR(F,L,S) ADDED HERE, SPECIFY BUBOR%TYPR='Q'
C|                |   | SEE END OF THE ROUTINE
C| AUBOR,BVBOR    |<--| LOG LAW: NU*DV/DN = AVBOR*V + BVBOR
C|                |   | IF BVBOR(F,L,S) ADDED HERE, SPECIFY BVBOR%TYPR='Q'
C|                |   | SEE END OF THE ROUTINE
C| AWBOR,BWBOR    |<--| LOG LAW: NU*DW/DN = AWBOR*W + BWBOR
C|                |   | IF BWBOR(F,L,S) ADDED HERE, SPECIFY BWBOR%TYPR='Q'
C|                |   | SEE END OF THE ROUTINE
C| COTIMP         |-->| ARRAY OF PRESCRIBED ELEVATIONS
C| DEBIMP         |-->| ARRAY OF PRESCRIBED DISCHARGES
C| DT             |-->| TIME-STEP
C| ENTET          |---| 
C| FAIR           |-->| DRAG COEFFICIENT OF WIND
C| HBOR           |-->| PRESCRIBED DEPTH ON LATERAL BOUNDARIES
C| HN             |-->| DEPTH AT TIME TN
C| IKLE3          |-->| CONNECTIVITY TABLE IN 3D
C| ITURBV         |-->| TURBULENCE MODEL (1:LAMINAR 2: MIXING LENGTH..)
C| KADH           |-->| NO SLIP CONDITION
C| KENT           |-->| PRESCRIBED VALUE
C| KENTU          |-->| PRESCRIBED VELOCITY
C| KLOG           |-->| SOLID BOUNDARY
C| KP1BOR         |-->| IN 2D : NEXT POINT ON THE BOUNDARY
C|                |   | (BOUNDARY NUMBERING)
C| KSORT          |-->| FREE (E.G. AT AN OUTPUT)
C| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
C| LITA,BF        |<->| ON BOTTOM FOR TRACERS
C| LITA,BL        |<->| ON LATERAL BOUNDARIES FOR TRACERS
C| LITA,BS        |<->| AT FREE SURFACE FOR TRACERS
C| LIU,V,WBOF     |<->| ON BOTTOM FOR U,V,W
C| LIU,V,WBOL     |<->| ON LATERAL BOUNDARIES FOR U,V,W
C| LIU,V,WBOS     |<->| AT FREE SURFACE FOR U,V,W
C| LT             |-->| CURRENT NUMBER OF TIME-STEP
C| NBOR           |-->| GLOBAL NUMBER OF 2D BOUNDARY POINTS
C| NCOTE          |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
C| NDEBIT         |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
C| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
C|                |   | POSSIBLE TYPES OF BOUNDARY CONDITIONS
C| NELEM3         |-->| NUMBER OF 3D ELEMENTS
C| NETAGE         |-->| NUMBER OF LAYERS OF 3D ELEMENTS
C| NFRLIQ         |---| 
C| NPLAN          |-->| NUMBER OF PLANES ON THE VERTICAL
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| NPOIN3         |-->| NUMBER OF POINTS IN 3D
C| NPRIV          |-->| NUMBER OF ARRAYS IN BLOCK PRIVE
C| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
C| NPTFR2_DIM     |---| 
C| NPTFR3         |-->| NUMBER OF BOUNDARY POINTS IN 3D
C| NTRAC          |-->| NUMBER OF TRACERS
C| NVIT           |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED VELOCITY
C| PRIVE          |-->| BLOCK OF ARRAYS FOR THE USER
C| SEDI           |-->| IF YES, THERE IS SEDIMENT
C| TA             |-->| CONCENTRATION OF TRACERS
C| TABORF         |<--| PRESCRIBED TRACERS ON THE BOTTOM
C| TABORL         |<--| PRESCRIBED TRACERS ON THE LATERAL BOUNDARY
C| TABORS         |<--| PRESCRIBED TRACERS AT FREE SURFACE
C|                |   | LOGARITHMIC LAWS : AUBORF,L,S AND BUBORF,L,S
C|                |   | FOR VELOCITIES
C|                |   | ATABO,L,S AND BTABO,L,S
C|                |   | FOR TRACERS
C| TIME           |---| 
C| U,V,W          |-->| COMPONENTS OF VELOCITY
C| UBORF          |<--| PRESCRIBED VELOCITY ALONG X ON THE BOTTOM
C| UBORL          |<--| PRESCRIBED VELOCITY ALONG X ON THE LATERAL
C|                |   | BOUNDARY
C| UBORS          |<--| PRESCRIBED VELOCITY ALONG X AT FREE SURFACE
C| UMOY,VMOY      |-->| DEPTH AVERAGED VELOCITY
C| VBORF          |<--| PRESCRIBED VELOCITY ALONG Y ON THE BOTTOM
C| VBORL          |<--| PRESCRIBED VELOCITY ALONG Y ON THE LATERAL
C|                |   | BOUNDARY
C| VBORS          |<--| PRESCRIBED VELOCITY ALONG Y AT FREE SURFACE
C| VENT           |-->| WITH WIND (.TRUE.) OR WITHOUT (.FALSE.)
C| VENTX          |-->| WIND VELOCITY ALONG X
C| VENTY          |-->| WIND VELOCITY ALONG Y
C| VITIMP         |-->| ARRAY OF PRESCRIBED VELOCITIES
C| WBORF          |<--| PRESCRIBED VELOCITY ALONG Z ON THE BOTTOM
C| WBORL          |<--| PRESCRIBED VELOCITY ALONG Z ON THE LATERAL
C|                |   | BOUNDARY
C| WBORS          |<--| PRESCRIBED VELOCITY ALONG Z AT FREE SURFACE
C| X,Y,Z          |-->| MESH COORDINATES
C| XSGBOR,YSGBOR  |-->| 2D NORMAL VECTORS TO THE SEGMENTS
C| ZF             |-->| BOTTOM ELEVATION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, EX_NFRLIQ=>NFRLIQ
      USE INTERFACE_TELEMAC3D, EX_BORD3D => BORD3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     TIME AND ENTET ARE AT AND INFOGR (NOW IN DECLARATIONS_TELEMAC3D)
      DOUBLE PRECISION, INTENT(IN)    :: TIME
      INTEGER         , INTENT(IN)    :: LT
      LOGICAL         , INTENT(IN)    :: ENTET
      INTEGER         , INTENT(IN)    :: NPTFR2_DIM
      INTEGER         , INTENT(IN)    :: NFRLIQ
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,IPOIN2,NP,K1,IBORD,IVIT,ICOT,IDEB,IFRLIQ,IPROF,K,N,R
      INTEGER IPTFR,ITRAC,IPLAN,I3D
      LOGICAL YAZMIN
      DOUBLE PRECISION ROEAU,ROAIR,VITV,PROFZ,WINDRELX,WINDRELY
!
      DOUBLE PRECISION P_DMIN
      INTEGER  P_IMAX
      EXTERNAL P_IMAX,P_DMIN
      DOUBLE PRECISION STA_DIS_CUR
      EXTERNAL STA_DIS_CUR
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION XB,YB,ZB,NORM,CP,RO0,A,B,SAL,WW,TREEL,RO,LAMB
      DOUBLE PRECISION ZMIN(MAXFRO)
!
      INTEGER YADEB(MAXFRO),MSK1,IPTFR2,I2,IJK,ITEMP
!
C     SIMPLE CASES FOR LATERAL BOUNDARIES ARE TREATED AUTOMATICALLY:
!
C     - PRESCRIBED DEPTH     (5 4 4)
C     - PRESCRIBED VELOCITY  (  6 6)
C     - PRESCRIBED DISCHARGE (  5 5)
!
C     CORRESPONDING KEYWORDS ARE:
!
C     'PRESCRIBED ELEVATIONS' OR 'COTES IMPOSEES'
C     'PRESCRIBED VELOCITIES' OR 'VITESSES IMPOSEES'
C     'PRESCRIBED FLOWRATES' OR 'DEBITS IMPOSES'
!
C     THE IMPLEMENTATION OF AUTOMATIC CASES MAY BE CANCELLED
C     PROVIDED THAT THE RELEVANT ARRAYS ARE FILLED
!
!
!***********************************************************************
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
C              AUTOMATIC TREATMENT OF LIQUID BOUNDARIES
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!=======================================================================
!
C     SECURES NO SLIP BOUNDARY CONDITIONS
!
      IF(LT.EQ.1) THEN
!
C     VELOCITIES
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
C     IMPORTANT OPTION:
C     VERTICAL VELOCITIES ARE SET AS HORIZONTAL VELOCITIES
C     THIS IS AN OPTION, OTHERWISE LIWBOL=KSORT (SEE LIMI3D)
!
C     DO IPTFR = 1,NPTFR2
C       IPOIN2 = NBOR2%I(IPTFR)
C       DO IPLAN = 1,NPLAN
C         IBORD = (IPLAN-1)*NPTFR2 + IPTFR
C         LIWBOL%I(IBORD)= LIUBOL%I(IBORD)
C         IF(LIWBOL%I(IBORD).EQ.KENT) WBORL%R(IBORD) = 0.D0
C       ENDDO
C     ENDDO
!
C     TRACERS
!
C     IF(NTRAC.NE.0) THEN
!
C       DO ITRAC = 1,NTRAC
!
C         DO IPTFR = 1,NPTFR2
C           IPOIN2 = NBOR2%I(IPTFR)
C           LITABF%ADR(ITRAC)%P%I(IPOIN2) = KSORT (DOES NOT WORK WITH SEDIMENT)
C           LITABS%ADR(ITRAC)%P%I(IPOIN2) = KSORT
C           DO IPLAN = 1,NPLAN
C             IBORD = (IPLAN-1)*NPTFR2 + IPTFR
C             IF(LITABL%ADR(ITRAC)%P%I(IBORD).EQ.KADH)
C    &           TABORL%ADR(ITRAC)%P%R(IBORD) = 0.D0
C           ENDDO
C         ENDDO
!
C         DO IPOIN2 = 1,NPOIN2
C           IF(LITABF%ADR(ITRAC)%P%I(IPOIN2).EQ.KADH)
C    &                       TABORF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
C           IF(LITABS%ADR(ITRAC)%P%I(IPOIN2).EQ.KADH)
C    &                       TABORS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
C         ENDDO
!
C       ENDDO
!
C     ENDIF
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
          ZMIN(IFRLIQ)=MIN(ZMIN(IFRLIQ),ZF%R(IPOIN2)+H%R(IPOIN2))
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
!     PRESCRIBED ELEVATION GIVEN IN STEERING FILE (NCOTE0)
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
          HBOR%R(K) = SL3(ICOT,AT,IPOIN2,INFOGR)-ZF%R(IPOIN2)
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
!     PRESCRIBED DISCHARGE GIVEN IN STEERING FILE (NDEBIT0)
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
C           GIVEN BY USER IN BOUNDARY CONDITIONS FILE
            UBORL%R(IJK) = UBOR2D%R(K+NPTFR2)
            VBORL%R(IJK) = VBOR2D%R(K+NPTFR2)
          ELSEIF(PROFVEL(IFRLIQ).EQ.3) THEN
C           NORMAL AND NORM GIVEN BY UBOR IN BOUNDARY CONDITIONS FILE
            UBORL%R(IJK) = -XNEBOR2%R(K)*UBOR2D%R(K+NPTFR2)
            VBORL%R(IJK) = -YNEBOR2%R(K)*UBOR2D%R(K+NPTFR2)
          ELSEIF(PROFVEL(IFRLIQ).EQ.4) THEN
C           NORMAL AND PROPORTIONAL TO SQRT(H)
            UBORL%R(IJK)=-XNEBOR2%R(K) * SQRT(MAX(H%R(IPOIN2),0.D0))
            VBORL%R(IJK)=-YNEBOR2%R(K) * SQRT(MAX(H%R(IPOIN2),0.D0))
          ELSEIF(PROFVEL(IFRLIQ).EQ.5) THEN
C           NORMAL PROFILE IN SQUARE ROOT OF H, BUT VIRTUAL H
C           DEDUCED FROM LOWEST FREE SURFACE OF THE BOUNDARY
            UBORL%R(IJK)=-XNEBOR2%R(K) * 
     *                   SQRT(MAX(ZMIN(IFRLIQ)-ZF%R(IPOIN2),0.D0))
            VBORL%R(IJK)=-YNEBOR2%R(K) * 
     *                   SQRT(MAX(ZMIN(IFRLIQ)-ZF%R(IPOIN2),0.D0))
          ELSE
C           NORMAL AND NORM 1
            UBORL%R(IJK)=-XNEBOR2%R(K)
            VBORL%R(IJK)=-YNEBOR2%R(K)
          ENDIF
C         NO VELOCITY IF NO WATER
          IF(H%R(IPOIN2).LT.1.D-4) THEN
            UBORL%R(IJK) = 0.D0
            VBORL%R(IJK) = 0.D0
          ENDIF
C         CASE OF A VERTICAL PROFILE
          IF(VERPROVEL(IFRLIQ).NE.1) THEN
            PROFZ=VEL_PROF_Z(IFRLIQ,NBOR2%I(K),
     &                       AT,LT,NP,INFOGR,VERPROVEL(IFRLIQ))
            UBORL%R(IJK) = UBORL%R(IJK)*PROFZ
            VBORL%R(IJK) = VBORL%R(IJK)*PROFZ
          ENDIF
C         U AND V INITIALISED WITH PRESCRIBED VALUES (FOR DEBIMP3D)
C         WILL BE CHANGED AGAIN AFTER DEBIMP3D
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
C     PRESCRIBED VELOCITY GIVEN IN STEERING FILE (NVIT0)
!     -----------------------------------------------------
!
      DO K=1,NPTFR2
!
C     THIS VELOCITY IS CONSIDERED NORMAL TO THE BOUNDARY
!
      IF(LIUBOL%I(K).EQ.KENTU.AND.NVIT.NE.0) THEN
        IVIT=NUMLIQ%I(K)
        IF(NVIT.GE.IVIT) THEN
!
             DO NP=1,NPLAN
               IBORD = (NP-1)*NPTFR2+K
               UBORL%R(IBORD) =
     &         -MESH2D%XNEBOR%R(K)*VIT3(IVIT,AT,NBOR2%I(K),INFOGR)
               VBORL%R(IBORD) =
     &         -MESH2D%YNEBOR%R(K)*VIT3(IVIT,AT,NBOR2%I(K),INFOGR)
               WBORL%R(IBORD)=0.D0
             END DO
!
        ELSE
          IF(LNG.EQ.1) WRITE(LU,200) NUMLIQ%I(K)
200       FORMAT(1X,'BORD3D : VITESSES IMPOSEES EN NOMBRE INSUFFISANT',/,
     &           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,201) NUMLIQ%I(K)
201       FORMAT(1X,'BORD3D : MORE PRESCRIBED VELOCITIES ARE REQUIRED',/,
     &           1X,'       IN THE PARAMETER FILE',/,
     &           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      ENDDO
!
C     PRESCRIBED TRACER GIVEN IN STEERING FILE,
C     BUT POSSIBLE OVERWRITING IF LIQUID BOUNDARY FILE IS GIVEN
C     SEE FUNCTION TR3
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
C           CASE OF A PROFILE ON THE VERTICAL
            IPROF=VERPROTRA(ITRAC+(IFRLIQ-1)*NTRAC)
            IF(IPROF.NE.1) THEN
              PROFZ=TRA_PROF_Z(IFRLIQ,NBOR2%I(K),
     &                         AT,LT,NP,INFOGR,IPROF,ITRAC)
              IF(IPROF.EQ.2.OR.IPROF.EQ.3) THEN
                TABORL%ADR(ITRAC)%P%R(IBORD)=PROFZ
              ELSE
                TABORL%ADR(ITRAC)%P%R(IBORD)=
     &          TABORL%ADR(ITRAC)%P%R(IBORD)*PROFZ
              ENDIF
            ENDIF
          ENDIF
C
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!
C     PRESCRIBED DISCHARGES: FINAL TREATMENT OF VELOCITIES
!     ----------------------------------------------------
!
C     LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
      DO 10 IFRLIQ = 1 , NFRLIQ
!
      IF(NDEBIT.NE.0) THEN
!
        MSK1=1
        IF(NDEBIT.GE.IFRLIQ) THEN
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
           IF(YADEB(IFRLIQ).EQ.1) THEN
           CALL DEBIMP3D(Q3(IFRLIQ,AT,INFOGR),
     &                   UBORL%R,VBORL%R,WBORL%R,
     &                   U,V,H,NUMLIQ%I,IFRLIQ,T3_01,T3_02,T3_03,
     &                   NPTFR2,NETAGE,MASK%ADR(MSK1)%P%R,
     &                   MESH3D,EQUA,NPOIN2,
     &                   IELM2V,SIGMAG,SVIDE,MASKBR,ZPROP)
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
10    CONTINUE
      ENDIF
!
C     RESETS BOUNDARY CONDITIONS ON U AND V (WILL BE USED BY TFOND
C     AND OTHER SUBROUTINES BEFORE THE NEXT BOUNDARY CONDITIONS TREATMENT)
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
C     EXAMPLE OF PRESCRIBED VERTICAL VELOCITIES AT ENTRANCES
C     VELOCITIES TANGENT TO BOTTOM AND FREE SURFACE
!
C     DO K=1,NPTFR2
C       IF(LIWBOL%I(K).EQ.KENT.OR.LIWBOL%I(K).EQ.KENTU) THEN
C         DO NP=1,NPLAN
C             IJK=(NP-1)*NPTFR2+K
C             I2D=NBOR2%I(K)
C             I3D=(NP-1)*NPOIN2+I2D
C             WBORL DEDUCED FROM FREE SURFACE AND BOTTOM
C             TETA=(Z(I3D)-Z(I2D))/
C    *        MAX(1.D-3,Z((NPLAN-1)*NPOIN2+I2D)-Z(I2D))
C             GX=        TETA *GRADZN%ADR(1)%P%R(I2D)
C    *            +(1.D0-TETA)*GRADZF%ADR(1)%P%R(I2D)
C             GY=        TETA *GRADZN%ADR(2)%P%R(I2D)
C    *            +(1.D0-TETA)*GRADZF%ADR(2)%P%R(I2D)
C             WBORL%R(IJK)=UBORL%R(IJK)*GX+VBORL%R(IJK)*GY
C         ENDDO
C       ENDIF
C     ENDDO
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
C           END OF AUTOMATIC TREATMENT OF LIQUID BOUNDARIES
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
C                               WIND
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
      IF(VENT) THEN
         ROEAU = 1000.D0
         ROAIR = 1.3D0
         DO IPOIN2 = 1,NPOIN2
C          RELATIVE WIND
           WINDRELX=WIND%ADR(1)%P%R(IPOIN2)-U%R(NPOIN3-NPOIN2+IPOIN2)
           WINDRELY=WIND%ADR(2)%P%R(IPOIN2)-V%R(NPOIN3-NPOIN2+IPOIN2)
           VITV=SQRT(WINDRELX**2+WINDRELY**2)
C          A MORE ACCURATE TREATMENT
C          IF(VITV.LE.5.D0) THEN
C            FAIR = ROAIR/ROEAU*0.565D-3
C          ELSEIF (VITV.LE.19.22D0) THEN
C            FAIR = ROAIR/ROEAU*(-0.12D0+0.137D0*VITV)*1.D-3
C          ELSE
C            FAIR = ROAIR/ROEAU*2.513D-3
C          ENDIF
C          BEWARE : BUBORS IS VISCVI*DU/DN, NOT DU/DN
           IF(H%R(IPOIN2).GT.HWIND) THEN
C            EXPLICIT PART
             BUBORS%R(IPOIN2) =  FAIR*VITV*WIND%ADR(1)%P%R(IPOIN2)
             BVBORS%R(IPOIN2) =  FAIR*VITV*WIND%ADR(2)%P%R(IPOIN2)
C            IMPLICIT PART
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
C                         END OF WIND TREATMENT
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
C                     HEAT EXCHANGE WITH ATMOSPHERE
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
C                 LINES BELOW WITH 'C!' ARE AN EXAMPLE
!                                              =======
C    TO BE GIVEN :
!
C    ITEMP = NUMBER OF TRACER WHICH IS THE HEAT
C    TAIR  = CONSTANT AIR TEMPERATURE
C    SAL   = CONSTANT WATER SALINITY
!
C!    ITEMP=1
C!    CP=4.18D3
C!    RO0=999.972D0
C!    B=0.0025D0
C!    TAIR=15.D0
C!    SAL=35.D-3
C!    WW=0.D0
C!    IF (VENT) WW=VITV
C!    DO IPOIN2=1,NPOIN2
C!       TREEL=TA%ADR(ITEMP)%P%R(NPOIN3-NPOIN2+IPOIN2)
C!       RO=RO0*(1.D0-(7.D0*(TREEL-4.D0)*(TREEL-4.D0)-750.D0*SAL)*1D-6)
C!       LAMB=RO*CP
C!       A=(4.48D0+0.049D0*TREEL)+2021.5D0*B*(1.D0+WW)*
C!   &     (1.12D0+0.018D0*TREEL+0.00158D0*TREEL*TREEL)
C!       ATABOS%ADR(ITEMP)%P%R(IPOIN2)=-A/LAMB
C!       BTABOS%ADR(ITEMP)%P%R(IPOIN2)= A*TAIR/LAMB
C!    ENDDO
C     IMPORTANT:
C     STATES THAT ATABOS AND BTABOS ARE NOT ZERO (SEE LIMI3D AND DIFF3D)
C     OTHERWISE THEY WILL NOT BE CONSIDERED
C!    ATABOS%ADR(ITEMP)%P%TYPR='Q'
C!    BTABOS%ADR(ITEMP)%P%TYPR='Q'
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
C                 END OF HEAT EXCHANGE WITH ATMOSPHERE
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!-----------------------------------------------------------------------
!
C     OPTIMISATION:
!
C     EXPLICIT STRESSES WILL NOT BE TREATED IF SAID TO BE 0
!
C     EXPLICIT STRESSES SET TO 0 ON VELOCITIES (UNLESS PROGRAMMED
C                                               IN THIS SUBROUTINE):
!
      BUBORF%TYPR='0'
      BUBORL%TYPR='0'
      BVBORF%TYPR='0'
      BVBORL%TYPR='0'
      BWBORF%TYPR='0'
      BWBORL%TYPR='0'
      BWBORS%TYPR='0'
!
C     CASE OF WIND (SEE ABOVE)
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
C
C#######################################################################
C
