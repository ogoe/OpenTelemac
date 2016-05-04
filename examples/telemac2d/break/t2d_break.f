!                    ***************
                     SUBROUTINE BORD
!                    ***************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     & NOMIMP)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOUNDARY CONDITIONS ARRAYS
!+                WHEN THEY VARY IN TIME.
!+
!
!note     THIS SUBROUTINE CAN BE COMPLETED BY THE USER DIRECTLY,
!+         OR THROUGH THE FUNCTIONS: Q , SL , TR , VIT
!
!history  J-M HERVOUET (LNHE)
!+        27/03/2008
!+        V5P9
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
!history  J-M HERVOUET (LNHE)
!+        06/04/2012
!+        V6P2
!+   Original point numbers sent to functions SL, VIT and TR
!
!history  J-M HERVOUET (LNHE)
!+        25/04/2013
!+        V6P3
!+   A new test to see if NUMLIQ(K)>0 (it may happen with weirs that
!+   LIHBOR(K)=KENT and NUMLIQ(K)=0.
!
!history  J-M HERVOUET (LNHE)
!+        24/12/2013
!+        V7P0
!+   Stage-discharge curves Q(Z) now programmed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EQUA           |-->| STRING DESCRIBING THE EQUATIONS SOLVED
!| H              |-->| DEPTH AT TIME N
!| HBOR           |<->| PRESCRIBED DEPTH
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| MASK           |-->| BLOCK OF MASKS FOR DIFFERENT BOUNDARY CONDITIONS
!| MESH           |-->| MESH STRUCTURE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCOTE          |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NDEBIT         |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NOMIMP         |-->| NAME OF LIQUID BOUNDARIES FILE
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTFR2         |-->| NUMBER OF QUADRATIC BOUNDARY POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| NTRACE         |-->| NUMBER OF BOUNDARIES WITH TRACER PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| NVITES         |-->| NUMBER OF BOUNDARIES WITH VELOCITY PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| PROVEL         |-->| OPTION FOR VELOCITY PROFILES
!| TBOR           |<--| BLOCK WITH PRESCRIBED VALUES OF TRACERS
!| TEMPS          |-->| TIME IN SECONDS
!| TRA05          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| TRA06          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| U              |-->| X-COMPONENT OF VELOCITY AT TIME N
!| V              |-->| Y-COMPONENT OF VELOCITY AT TIME N
!| UBOR           |<->| X-COMPONENT OF PRESCRIBED VELOCITY
!| VBOR           |<->| Y-COMPONENT OF PRESCRIBED VELOCITY
!| XNEBOR         |-->| X-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!| YNEBOR         |-->| Y-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_BORD => BORD
      USE DECLARATIONS_TELEMAC2D, ONLY: STA_DIS_CURVES,PTS_CURVES,QZ,
     &                                  FLUX_BOUNDARIES,MAXFRO,
     &                                  TIDALTYPE,BOUNDARY_COLOUR
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE
      INTEGER, INTENT(IN) :: KENT,KENTU,NFRLIQ,NTRAC,NPTFR2
      INTEGER, INTENT(IN) :: PROVEL(*),LIHBOR(NPTFR),LIUBOR(NPTFR2)
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR),NBOR(NPTFR2)
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      CHARACTER(LEN=20), INTENT(IN)   :: EQUA
      CHARACTER(LEN=144), INTENT(IN)  :: NOMIMP
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR2,2),VBOR(NPTFR2,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,U,V,TRA05,TRA06,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASK,LITBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,MSK8,IFRLIQ,YADEB(MAXFRO),IERR,ITRAC,IFR,N,IELEB,KP1
!
      DOUBLE PRECISION Z,QIMP,ZMIN(MAXFRO)
!
      DOUBLE PRECISION AMP, PERIOD, PI, OMEGA
      DOUBLE PRECISION VNORM, VNORMPRED, RAD, DEP
!
      LOGICAL YAZMIN
!
      DOUBLE PRECISION P_DMIN
      INTEGER  P_IMAX
      EXTERNAL P_IMAX,P_DMIN
!
      INTRINSIC MAX
!
!     PROVISOIRE
!
      DOUBLE PRECISION DIS_STA_CUR
      EXTERNAL         DIS_STA_CUR
!
!-----------------------------------------------------------------------
!
!     IF VELOCITY PROFILE OPTION 5 OR STAGE-DISCHARGE CURVE Q(Z):
!     THE MINIMUM ELEVATION OF EVERY BOUNDARY IS NEEDED
!
      YAZMIN=.FALSE.
      DO IFR=1,NFRLIQ
        ZMIN(IFR)=1.D99
        IF(PROVEL(IFR).EQ.5.OR.STA_DIS_CURVES(IFR).EQ.2) YAZMIN=.TRUE.
      ENDDO
      IF(YAZMIN) THEN
        DO K=1,NPTFR
          IFR=NUMLIQ(K)
          IF(IFR.GT.0) ZMIN(IFR)=MIN(ZMIN(IFR),ZF(NBOR(K))+H%R(NBOR(K)))
        ENDDO
        IF(NCSIZE.GT.1) THEN
          DO IFR=1,NFRLIQ
            ZMIN(IFR)=P_DMIN(ZMIN(IFR))
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      MSK8 = 8
!
!  INITIALISATION OF YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
      AMP = 0.001D0
      PERIOD = 1000.D0 / SQRT(9.81D0)
      PI = 4.D0*ATAN(1.D0)
      OMEGA = 2.D0*PI/PERIOD
      DEP = 1.D0
!
!-----------------------------------------------------------------------
!
!  LOOP ON ALL BOUNDARY POINTS
!
      DO K=1,NPTFR
!
!  LEVEL IMPOSED WITH VALUE GIVEN IN THE CAS FILE (NCOTE0)
!
      IF(LIHBOR(K).EQ.KENT) THEN
!
        IFRLIQ=NUMLIQ(K)
!
!          IFRLIQ.EQ.0 MAY HAPPEN WITH WEIRS
        IF(IFRLIQ.GT.0) THEN
          IF(STA_DIS_CURVES(IFRLIQ).EQ.1) THEN
            Z = STA_DIS_CUR(IFRLIQ,FLUX_BOUNDARIES(IFRLIQ),
     &                      PTS_CURVES(IFRLIQ),QZ,NFRLIQ,
     &                      ZF(NBOR(K))+H%R(NBOR(K)))
            HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
            H%R(NBOR(K))=HBOR(K)
          ELSEIF(NCOTE.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
            N=NBOR(K)
            IF(NCSIZE.GT.1) N=MESH%KNOLG%I(N)
!            Z = SL(IFRLIQ,N)
             Z = AMP * SIN(OMEGA*TEMPS)
             VNORMPRED = -SQRT(9.81D0/DEP) * AMP * SIN(OMEGA*TEMPS)

             VNORM = U%R(NBOR(K)) * XNEBOR(K)
     &             + V%R(NBOR(K)) * YNEBOR(K)

            IF (TEMPS.GT.1800.D0) THEN
              RAD = (VNORM-VNORMPRED)*SQRT(H%R(NBOR(K))/9.81D0)
            ELSE
              RAD = 0.D0
            ENDIF

            Z = Z + RAD
            HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
            H%R(NBOR(K))=HBOR(K)
!         ELSE HBOR TAKEN IN BOUNDARY CONDITIONS FILE
          ENDIF
        ENDIF
!
      ENDIF
!
!  DISCHARGE IMPOSED: VARIOUS OPTIONS ACCORDING TO PROVEL
!                 ONE USES THE VALUES PROVIDED BY THE USER
!                 AS VELOCITY PROFILE.
!                 UBOR(K,2) AND VBOR(K,2) ARE THE VALUES OF
!                 THE CONLIM FILE, AND ARE CONSERVED.
!
      IF(LIUBOR(K).EQ.KENT.AND.
     &  (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
        IFR=NUMLIQ(K)
        IF(PROVEL(IFR).EQ.1) THEN
!         CONSTANT NORMAL PROFILE
          UBOR(K,1) = -XNEBOR(K)
          VBOR(K,1) = -YNEBOR(K)
        ELSEIF(PROVEL(IFR).EQ.2) THEN
!         PROFILE PROVIDED BY THE USER
          UBOR(K,1) = UBOR(K,2)
          VBOR(K,1) = VBOR(K,2)
        ELSEIF(PROVEL(IFR).EQ.3) THEN
!         NORMAL VELOCITY PROVIDED IN UBOR
          UBOR(K,1) = -XNEBOR(K)*UBOR(K,2)
          VBOR(K,1) = -YNEBOR(K)*UBOR(K,2)
        ELSEIF(PROVEL(IFR).EQ.4) THEN
!         NORMAL PROFILE IN SQUARE ROOT OF H
          UBOR(K,1) = -XNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
          VBOR(K,1) = -YNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
        ELSEIF(PROVEL(IFR).EQ.5) THEN
!         NORMAL PROFILE IN SQUARE ROOT OF H, BUT VIRTUAL H
!         DEDUCED FROM LOWEST FREE SURFACE OF THE BOUNDARY
          UBOR(K,1)=-XNEBOR(K)*SQRT(MAX(ZMIN(IFR)-ZF(NBOR(K)),0.D0))
          VBOR(K,1)=-YNEBOR(K)*SQRT(MAX(ZMIN(IFR)-ZF(NBOR(K)),0.D0))
        ENDIF
!       ONE DOES NOT SET VELOCITY IF THERE IS NO WATER.
        IF(H%R(NBOR(K)).LT.1.D-3) THEN
          UBOR(K,1) = 0.D0
          VBOR(K,1) = 0.D0
        ENDIF
!       U AND V INITIALISED WITH THE IMPOSED VALUES
        U%R(NBOR(K)) = UBOR(K,1)
        V%R(NBOR(K)) = VBOR(K,1)
        YADEB(NUMLIQ(K))=1
      ENDIF
!
!  VELOCITY IMPOSED: ONE USES THE OUTGOING DIRECTION
!                    PROVIDED BY THE USER.
!
      IF(LIUBOR(K).EQ.KENTU.AND.
     &  (NVITES.NE.0.OR.NOMIMP(1:1).NE.' ')) THEN
!       POINTS ON WEIRS HAVE NUMLIQ(K)=0
        IF(NUMLIQ(K).GT.0) THEN
          IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
            N=NBOR(K)
            IF(NCSIZE.GT.1) N=MESH%KNOLG%I(N)
            UBOR(K,1) = - XNEBOR(K) * VIT(NUMLIQ(K),N)
            VBOR(K,1) = - YNEBOR(K) * VIT(NUMLIQ(K),N)
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.2) THEN
            UBOR(K,1) = UBOR(K,2)
            VBOR(K,1) = VBOR(K,2)
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.3) THEN
            UBOR(K,1) = - XNEBOR(K) * UBOR(K,2)
            VBOR(K,1) = - YNEBOR(K) * UBOR(K,2)
          ELSE
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'FRONTIERE ',NUMLIQ(K)
              WRITE(LU,*) 'PROFIL ',PROVEL(NUMLIQ(K)),
     &                    ' DEMANDE AVEC VITESSES IMPOSEES'
              WRITE(LU,*) 'COMBINAISON ILLOGIQUE'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'BOUNDARY ',NUMLIQ(K)
              WRITE(LU,*) 'PROFILE ',PROVEL(NUMLIQ(K)),
     &                    ' ASKED'
              WRITE(LU,*) 'IMPOSSIBLE COMBINATION'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
!         U AND V INITIALISED WITH THE IMPOSED VALUES
          U%R(NBOR(K)) = UBOR(K,1)
          V%R(NBOR(K)) = VBOR(K,1)
        ENDIF
      ENDIF
!
!  IMPOSED TRACER
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
        IF(LITBOR%ADR(ITRAC)%P%I(K).EQ.KENT.AND.
     &    (NTRACE.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
!         THE CASE NUMLIQ(K)=0 CORRESPONDS TO A SINGULARITY INITIALLY
!         DECLARED AS A SOLID BOUNDARY AND FOR WHICH
!         TBOR IS FILLED IN CLHUVT
          IF(NUMLIQ(K).GT.0) THEN
            N=NBOR(K)
            IF(NCSIZE.GT.1) N=MESH%KNOLG%I(N)
            Z = TR(NUMLIQ(K),ITRAC,N,IERR)
            IF(IERR.EQ.0) TBOR%ADR(ITRAC)%P%R(K) = Z
          ENDIF
        ENDIF
        ENDDO
      ENDIF
!
      ENDDO ! K
!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
      IF(TIDALTYPE.GE.1) CALL TIDAL_MODEL_T2D()
!
!-----------------------------------------------------------------------
!
!  QUADRATIC VELOCITIES
!
      IF(U%ELM .EQ.13)THEN
        DO IELEB=1,MESH%NELEB
          K  =MESH%IKLBOR%I(IELEB)
          KP1=MESH%IKLBOR%I(IELEB+MESH%NELEBX)
          IF(LIUBOR(K+NPTFR).EQ.KENT.AND.
     &      (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
            U%R(NBOR(K+NPTFR)) = (UBOR(K,1)+UBOR(KP1,1))*0.5D0
            V%R(NBOR(K+NPTFR)) = (VBOR(K,1)+VBOR(KP1,1))*0.5D0
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CASE OF DISCHARGE IMPOSED:
!
!  LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
!
        DO IFRLIQ = 1 , NFRLIQ
!
          IF(NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
!
!           ONE TAKES THE MASK OF LIQUID BOUNDARIES MSK8, WHICH IS
!           EQUAL TO THE MASK OF THE DISCHARGE IMPOSED ON A DISCHARGE
!           IMPOSED BOUNDARY. THIS MAKES IT POSSIBLE TO CHANGE A FREE
!           VELOCITY BOUNDARY TO A DISCHARGE IMPOSED TO A LEVEL IMPOSED
!           BOUNDARY, IN SPITE OF THE FACT THAT THE MASKS ARE MADE IN
!           PROPIN BEFORE THE CALL TO BORD
!
            IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
            IF(YADEB(IFRLIQ).EQ.1) THEN
              IF(STA_DIS_CURVES(IFRLIQ).EQ.2) THEN
                QIMP=DIS_STA_CUR(IFRLIQ,PTS_CURVES(IFRLIQ),QZ,NFRLIQ,
     &                           ZMIN(IFRLIQ))
              ELSE
                QIMP=Q(IFRLIQ)
              ENDIF
              CALL DEBIMP(QIMP,UBOR,VBOR,U,V,H,NUMLIQ,
     &                    IFRLIQ,TRA05,TRA06,
     &                    NPTFR,MASK%ADR(MSK8)%P%R,MESH,MESH%KP1BOR%I,
     &                    EQUA)
            ENDIF
!
          ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  QUADRATIC VELOCITIES
!
      IF(U%ELM.EQ.13) THEN
        DO IELEB=1,MESH%NELEB
          K  =MESH%IKLBOR%I(IELEB)
          KP1=MESH%IKLBOR%I(IELEB+MESH%NELEBX)
          UBOR(K+NPTFR,1) =(UBOR(K,1)+UBOR(KP1,1))*0.5D0
          VBOR(K+NPTFR,1) =(VBOR(K,1)+VBOR(KP1,1))*0.5D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!----------------------------------------------------------------------
!
!
!                    ***************************
                     SUBROUTINE PRERES_TELEMAC2D
!                    ***************************
     &    (IMP,LEO)
!
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!history  J-M HERVOUET (LNHE)
!+        24/11/2009
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
!history  C. GOEURY (EDF R&D LNHE)
!+        25/07/2013
!+        V6P3
!+   Sum of HAP in oilspills has been added.
!
!history  J-M HERVOUET EDF R&D, LNHE)
!+        02/01/2014
!+        V7P0
!+   Securing bound checking in parallelism.
!
!history  J-M HERVOUET EDF R&D, LNHE)
!+        28/10/2014
!+        V7P0
!+   Initialising Lagrangian drifts for iteration 0 in case they are
!+   in outputs.
!
!history  R. ATA & J-M HERVOUET (EDF LAB, LNHE)
!+        10/06/2015
!+        V7P1
!+   Now all the variables asked for graphic printouts are written for
!+   remarkable points.
!
!history  R. ATA (EDF LAB, LNHE)
!+        11/01/2016
!+        V7P2
!+   Now preres gives instruction to bief_desimp to write graphical
!+   results (through leo and imp)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL ,INTENT(INOUT)::IMP,LEO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL DEJA1,DEJA2,DEJA3
!
      INTEGER LTT,N,IMAX,I,II,JJ
!
      DOUBLE PRECISION HHH,XMAX
      DOUBLE PRECISION, PARAMETER:: EPSS=1.E-10
      DOUBLE PRECISION GPRDTIME,LPRDTIME,RESTE
      INTRINSIC MAX,SQRT
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL         P_DMAX,P_DMIN
!
!-----------------------------------------------------------------------
!
      DATA DEJA1/.FALSE./
      DATA DEJA2/.FALSE./
      DATA DEJA3/.FALSE./
      SAVE DEJA1,DEJA2,DEJA3
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN BIEF_DESIMP (BIEF LIBRARY)
      IMP=.FALSE.
      LEO=.FALSE.
!     THIS WILL TRIGGER THE OUTPUT OF LAST TIMESTEP
!     BUT NOT WITH PARAMETER ESTIMATION (LISPRD WOULD STAY AT 1
!     FOR FURTHER COMPUTATIONS)
!      IF(LT.EQ.NIT.AND.ESTIME(1:1).EQ.' ') THEN
!        IMP=.FALSE.
!        LEO=.FALSE.
!      ENDIF
!     Always write the intial conditions
      IF(LT.EQ.0) THEN
        IMP=.TRUE.
        LEO=.TRUE.
        COMPLEO=0
      ELSE
        IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!         FEM
          LTT=(LT/LISPRD)*LISPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIL) IMP=.TRUE.
          LTT=(LT/LEOPRD)*LEOPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIG) LEO=.TRUE.
!         FOR GRAPHICAL OUTPUTS          
          IF(LEO)COMPLEO=COMPLEO+1
        ELSE
!         FVM
          GPRDTIME=LEOPRD*DTINI
          LPRDTIME=LISPRD*DTINI
          IF(GPRDTIME.LT.EPSS.OR.LPRDTIME.LT.EPSS)THEN
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(LT.GE.PTINIG)THEN
!           GRAPHIC OUTPUT
            LTT=CEILING(AT/GPRDTIME)
            RESTE=(LTT*GPRDTIME-AT)/GPRDTIME
            IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                   CASE WHERE RESTE=1
     &        LT.EQ.NIT)THEN
              LEO=.TRUE.
              COMPLEO=COMPLEO+1
            ENDIF
            
          ENDIF
          IF(LT.GT.PTINIL)THEN
!           LISTING OUTPUT
            LTT=CEILING(AT/LPRDTIME)
            RESTE=(LTT*LPRDTIME-AT)/LPRDTIME
            IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                   CASE WHERE RESTE=1
     &        LT.EQ.NIT)THEN
              IMP=.TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF

!
!-----------------------------------------------------------------------
!
! 1)  PART WHICH MUST BE DONE EVEN IF THERE IS NO OUTPUT FOR THIS TIMESTEP
!     BUT ONLY AFTER FIRST TIMESTEP FOR GRAPHIC PRINTOUTS
!
!-----------------------------------------------------------------------
!
      IF(LT.GE.PTINIG) THEN
!
!=======================================================================
! COMPUTES THE MAXIMUM ELEVATION AND ASSOCIATED TIME
!=======================================================================
!
      IF(SORLEO(27).OR.SORIMP(27)) THEN
        IF(.NOT.DEJA1) THEN
          CALL OS('X=Y     ',X=MAXZ ,Y=ZF)
          CALL OS('X=C     ',X=TMAXZ,C=AT)
          DEJA1=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=H%R(N)+ZF%R(N)
!           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXZ%R(N).AND.H%R(N).GT.0.01D0) THEN
! Only save maximum elevation after 1 hour 45 min
              IF (AT.GT.6300.D0) THEN
                MAXZ%R(N)=XMAX
              ELSE
                MAXZ%R(N)=0.D0
              ENDIF
              IF(SORLEO(28).OR.SORIMP(28)) TMAXZ%R(N)=AT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! COMPUTES THE MAXIMUM SPEED AND ASSOCIATED TIME
!=======================================================================
!
      IF(SORLEO(29).OR.SORIMP(29)) THEN
        IF(.NOT.DEJA2) THEN
          CALL OS('X=C     ',X=MAXV ,C=0.D0)
          CALL OS('X=C     ',X=TMAXV,C=AT)
          DEJA2=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=SQRT(U%R(N)**2+V%R(N)**2)
!           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXV%R(N).AND.H%R(N).GT.0.01D0) THEN
              MAXV%R(N)=XMAX
              IF(SORLEO(30).OR.SORIMP(30)) TMAXV%R(N)=AT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! PRINTOUTS FOR THE REMARKABLE POINTS
!=======================================================================
!
      IF(LT.EQ.NIT.AND.NPTS.GT.0) THEN
        DO I=27,30
!         BEWARE : HERE SORLEO IS USED INSTEAD OF SORIMP
          IF(SORLEO(I)) THEN
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) TEXTE(I)(1:16)
            WRITE(LU,*) ' '
            DO N=1,NPTS
!             IN PARALLEL POINT DOES NOT ALWAYS EXIST, MAYBE ELSEWHERE
              IF(NCSIZE.GT.0) THEN
                WRITE(LU,*) NAME_PTS(N),' : ',
     &                    P_DMIN(VARSOR%ADR(I)%P%R(LIST_PTS(N)))+
     &                    P_DMAX(VARSOR%ADR(I)%P%R(LIST_PTS(N)))
              ELSE
                WRITE(LU,*) NAME_PTS(N),' : ',
     &                                    VARSOR%ADR(I)%P%R(LIST_PTS(N))
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
!     CASE WHERE OUTINI=.TRUE. : PRIORITY ON PTINIG, VALUES FOR LT=0
!     OTHERWISE THEY WOULD NOT BE INITIALISED
        IF(SORLEO(27).OR.SORIMP(27)) CALL OS('X=Y     ',X=MAXZ ,Y=ZF)
        IF(SORLEO(28).OR.SORIMP(28)) CALL OS('X=C     ',X=TMAXZ,C=AT)
        IF(SORLEO(29).OR.SORIMP(29)) CALL OS('X=C     ',X=MAXV ,C=0.D0)
        IF(SORLEO(30).OR.SORIMP(30)) CALL OS('X=C     ',X=TMAXV,C=AT)
!
!     ENDIF FOR : IF(LT.GE.PTINIG) THEN
      ENDIF
!
!-----------------------------------------------------------------------
!
! 2)  PART WHICH MUST BE DONE ONLY IF THERE IS AN OUTPUT FOR THIS TIMESTEP
!
!-----------------------------------------------------------------------
!
!     NO PRINTOUT REQUIRED (LISTING OR RESULT FILE): EXITS
      IF(.NOT.(LEO.OR.IMP)) GO TO 1000
!
!
!=======================================================================
! COMPUTES CELERITY (IN FU, SEE BLOCK: VARSOR)
!=======================================================================
!
      IF((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        CALL CPSTVC(ZF,FU)
        DO N=1,NPOIN
          FU%R(N) = SQRT ( GRAV * MAX(H%R(N),0.D0) )
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FREE SURFACE ELEVATION (= H + ZF, IN FV)
!=======================================================================
!
      IF((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL CPSTVC(ZF,FV)
        DO N=1,NPOIN
          FV%R(N) = H%R(N)+ZF%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FROUDE NUMBER
!=======================================================================
!
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL CPSTVC(ZF,T2)
        DO N=1,NPOIN
          HHH = MAX( H%R(N) , 1.D-8 )
          T2%R(N) = SQRT (( U%R(N)**2 + V%R(N)**2 ) / ( HHH*GRAV ))
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE
!=======================================================================
!
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        CALL CPSTVC(ZF,T3)
        DO N=1,NPOIN
          T3%R(N) = SQRT (U%R(N)**2 + V%R(N)**2) * H%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE ALONG X
!=======================================================================
!
      IF((LEO.AND.SORLEO(13)).OR.(IMP.AND.SORIMP(13))) THEN
        CALL CPSTVC(ZF,T4)
        DO N=1,NPOIN
          T4%R(N)=H%R(N)*U%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE ALONG Y
!=======================================================================
!
      IF((LEO.AND.SORLEO(14)).OR.(IMP.AND.SORIMP(14))) THEN
        CALL CPSTVC(ZF,T5)
        DO N=1,NPOIN
          T5%R(N)=H%R(N)*V%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES SPEED
!=======================================================================
!
      IF((LEO.AND.SORLEO(15)).OR.(IMP.AND.SORIMP(15))) THEN
        CALL OS( 'X=N(Y,Z)' , X=T6 , Y=U , Z=V )
      ENDIF
!
!=======================================================================
! COMPUTES COURANT NUMBER
!=======================================================================
!
      IF((LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22))) THEN
!                             IELM
        CALL CFLPSI(T9,U,V,DT,11,MESH,MSK,MASKEL)
        CALL MAXI(XMAX,IMAX,T9%R,NPOIN)
        IF(NCSIZE.GT.1) THEN
          IF(LNG.EQ.1) WRITE(LU,78) P_DMAX(XMAX)
          IF(LNG.EQ.2) WRITE(LU,79) P_DMAX(XMAX)
        ELSE
          IF(LNG.EQ.1) WRITE(LU,78) XMAX
          IF(LNG.EQ.2) WRITE(LU,79) XMAX
        ENDIF
78      FORMAT(1X,'PRERES : NOMBRE DE COURANT MAXIMUM :',G16.7)
79      FORMAT(1X,'PRERES: MAXIMUM COURANT NUMBER: ',G16.7)
      ENDIF
!
!=======================================================================
! COMPUTES FRICTION SPEED
!=======================================================================
!
      IF((LEO.AND.SORLEO(31)).OR.(IMP.AND.SORIMP(31))) THEN
        CALL CPSTVC(CF,T7)
        DO N=1,NPOIN
          T7%R(N) = SQRT(0.5D0*CF%R(N)*(U%R(N)**2+V%R(N)**2))
        ENDDO
      ENDIF
!
!=======================================================================
!
1000  CONTINUE
!
!=======================================================================
! HARMONIC ANALYSIS USING LEAST MEAN ERROR SQUARE METHOD
!=======================================================================
!
      IF(NPERIAF.GT.0) CALL SPECTRE
!
!=======================================================================
!
      RETURN
      END
