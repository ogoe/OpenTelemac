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
! TELEMAC2D   V6P1                                   21/08/2010
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
     &                                  FLUX_BOUNDARIES,MAXFRO,TIDALTYPE
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
      INTEGER K,MSK8,IFRLIQ,YADEB(MAXFRO),IERR,ITRAC,IFR
!
      DOUBLE PRECISION Z,ZMIN(MAXFRO)
!
      LOGICAL YAZMIN
!
      DOUBLE PRECISION P_DMIN
      INTEGER  P_IMAX
      EXTERNAL P_IMAX,P_DMIN
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
!     IF VELOCITY PROFILE OPTION 5: MINIMUM ELEVATION OF EVERY BOUNDARY
!
      YAZMIN=.FALSE.
      DO IFR=1,NFRLIQ
        ZMIN(IFR)=1.D99
        IF(PROVEL(IFR).EQ.5) YAZMIN=.TRUE.
      ENDDO
      IF(YAZMIN) THEN
        DO K=1,NPTFR
          IFR=NUMLIQ(K)
          ZMIN(IFR)=MIN(ZMIN(IFR),ZF(NBOR(K))+H%R(NBOR(K)))
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
!
!  LOOP ON ALL BOUNDARY POINTS
!
      DO 5 K=1,NPTFR
!
!  LEVEL IMPOSED WITH VALUE GIVEN IN THE CAS FILE (NCOTE0)
!
      IF(LIHBOR(K).EQ.KENT) THEN
!
        IFRLIQ=NUMLIQ(K)
!
        IF(STA_DIS_CURVES(IFRLIQ).EQ.1) THEN
          Z = STA_DIS_CUR(IFRLIQ,FLUX_BOUNDARIES(IFRLIQ),
     &                    PTS_CURVES(IFRLIQ),QZ,NFRLIQ,
     &                    ZF(NBOR(K))+H%R(NBOR(K)))
          HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
          H%R(NBOR(K))=HBOR(K)
        ELSEIF(NCOTE.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
          Z = SL(IFRLIQ,NBOR(K))
          HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
          H%R(NBOR(K))=HBOR(K)
!       ELSE HBOR TAKEN IN BOUNDARY CONDITIONS FILE
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
            UBOR(K,1) = - XNEBOR(K) * VIT(NUMLIQ(K),NBOR(K))
            VBOR(K,1) = - YNEBOR(K) * VIT(NUMLIQ(K),NBOR(K))
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.2) THEN
            UBOR(K,1) = UBOR(K,2)
            VBOR(K,1) = VBOR(K,2)
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.3) THEN
            UBOR(K,1) = - XNEBOR(K) * UBOR(K,2)
            VBOR(K,1) = - YNEBOR(K) * UBOR(K,2)
          ENDIF
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
            Z = TR(NUMLIQ(K),ITRAC,NBOR(K),IERR)
            IF(IERR.EQ.0) TBOR%ADR(ITRAC)%P%R(K) = Z
          ENDIF
        ENDIF
        ENDDO
      ENDIF
!
5     CONTINUE
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
        DO K=1,NPTFR
          IF(LIUBOR(K+NPTFR).EQ.KENT.AND.
     &  (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
        U%R(NBOR(K+NPTFR)) = (UBOR(K,1)+UBOR(MESH%KP1BOR%I(K),1))/2.D0
        V%R(NBOR(K+NPTFR)) = (VBOR(K,1)+VBOR(MESH%KP1BOR%I(K),1))/2.D0
          ENDIF
        ENDDO
      ENDIF
!
!  CASE OF DISCHARGE IMPOSED:
!
!  LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
!
      DO 10 IFRLIQ = 1 , NFRLIQ
!
      IF(NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
!
!         ONE TAKES THE MASK OF LIQUID BOUNDARIES MSK8, WHICH IS
!         EQUAL TO THE MASK OF THE DISCHARGE IMPOSED ON A DISCHARGE
!         IMPOSED BOUNDARY. THIS MAKES IT POSSIBLE TO CHANGE A FREE
!         VELOCITY BOUNDARY TO A DISCHARGE IMPOSED TO A LEVEL IMPOSED
!         BOUNDARY, IN SPITE OF THE FACT THAT THE MASKS ARE MADE IN
!         PROPIN BEFORE THE CALL TO BORD
!
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
          IF(YADEB(IFRLIQ).EQ.1) THEN
            CALL DEBIMP(Q(IFRLIQ),UBOR,VBOR,U,V,H,NUMLIQ,
     &                  IFRLIQ,TRA05,TRA06,
     &                  NPTFR,MASK%ADR(MSK8)%P%R,MESH,MESH%KP1BOR%I,
     &                  EQUA)
          ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
10    CONTINUE
!
      ENDIF
!
! QUADRATIC VELOCITIES
!
      IF(U%ELM.EQ.13) THEN
        DO K=1,NPTFR
          UBOR(K+NPTFR,1) =(UBOR(K,1)+UBOR(MESH%KP1BOR%I(K),1))*0.5D0
          VBOR(K+NPTFR,1) =(VBOR(K,1)+VBOR(MESH%KP1BOR%I(K),1))*0.5D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
