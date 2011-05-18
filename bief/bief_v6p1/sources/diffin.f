!                    *****************
                     SUBROUTINE DIFFIN
!                    *****************
!
     &(MASKTR,LIMTRA,LITBOR,CLT,U,V,XNEBOR,YNEBOR,NBOR,
     & KP1BOR,NPTFR,KENT,KSORT,KLOG,KINC,KNEU,KDIR,KDDL,
     & ICONV,NELBOR,NPOIN,NELMAX,MSK,MASKEL,
     & NFRLIQ,THOMFR,FRTYPE,TN,TBOR,MESH,NUMLIQ)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS FOR TRACER DIFFUSION.
!
!history  J-M HERVOUET (LNH)
!+        25/06/2008
!+        V5P9
!+   MOVED FROM TELEMAC-2D TO ALLOW CALL BY SISYPHE
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
!history  J-M HERVOUET (LNH)
!+        28/046/2011
!+        V6P1
!+   LIQUID BOUNDARIES MASK ADDED
!+   CALL PARCOM_BORD DELETED (NOT USEFUL, WE DEAL HERE WITH SEGMENTS
!+   WHICH BELONG TO A SINGLE PROCESSOR)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CLT            |<--| A MODIFIED COPY OF LITBOR.
!| FRTYPE         |-->| TYPE OF BOUNDARY CONDITIONS
!|                |   | 1: NORMAL   2: THOMPSON
!| ICONV          |-->| OPTION FOR ADVECTION : 1) CHARACTERISTICS
!|                |   |                        2) SUPG, ETC.
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KP1BOR         |-->| KP1BOR(K,1) NEXT POINT ON THE 2D BOUNDARY
!|                |   | KP1BOR(K,2) PREVIOUS POINT ON THE 2D BOUNDARY
!| KSORT          |-->| CONVENTION FOR LIQUID OUTPUT WITH FREE VALUE
!| LIMTRA         |<--| TECHNICAL BOUNDARY CONDITIONS FOR TRACERS
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| TBOR           |-->| DIRICHLET BOUNDARY CONDITIONS ON TRACERS
!| THOMFR         |-->| IF YES, THERE ARE THOMPSON BOUNDARY CONDITIONS
!| TN             |-->| TRACERS AT OLD TIME STEP
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| XNEBOR         |-->| X-COMPONENT OF EXTERNAL NORMAL BOUNDARY VECTOR
!| YNEBOR         |-->| Y-COMPONENT OF EXTERNAL NORMAL BOUNDARY VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DIFFIN => DIFFIN
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKTR,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: TN
      INTEGER, INTENT(IN)           :: NPOIN,NPTFR,NELMAX,ICONV,NFRLIQ
      INTEGER, INTENT(IN)    :: LITBOR(NPTFR),KP1BOR(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: LIMTRA(NPTFR),CLT(NPTFR)
      INTEGER, INTENT(IN)    :: KENT,KSORT,KLOG,KDIR,KDDL,KNEU,KINC
      INTEGER, INTENT(IN)    :: NELBOR(NPTFR),NUMLIQ(NPTFR)
      INTEGER, INTENT(IN)    :: FRTYPE(NFRLIQ)
!
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN), V(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR), YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: MASKEL(NELMAX)
!
      LOGICAL, INTENT(IN) :: MSK,THOMFR
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,K1,K2,IELEM,DIR,DDL,NEU,OND,NONEU,IFRLIQ
      DOUBLE PRECISION USCALN
!
!-----------------------------------------------------------------------
!
      DIR=1
      DDL=2
      NEU=3
      OND=4
      NONEU=5
!
!     CLT CONTAINS ARRAY LITBOR, POSSIBLY MODIFIED ACCORDING TO THE SIGN
!     OF U.N ON THE LIQUID BOUNDARIES, WHERE N IS THE OUTGOING NORMAL.
!
      DO K=1,NPTFR
        CLT(K) = LITBOR(K)
!       LOCATES THE LIQUID BOUNDARIES:
        IF(CLT(K).EQ.KENT) THEN
          USCALN = U(NBOR(K))*XNEBOR(K) + V(NBOR(K))*YNEBOR(K)
!         OUTGOING VELOCITY, FREE TRACER
          IF(USCALN.GT.0.D0) CLT(K) = KSORT
        ELSEIF(CLT(K).EQ.KSORT) THEN
          USCALN = U(NBOR(K))*XNEBOR(K) + V(NBOR(K))*YNEBOR(K)
!
!         INCOMING VELOCITY, TRACER IMPOSED AT THE LAST VALUE
          IF(USCALN.LT.0.D0) THEN
            TBOR%R(K)=TN%R(NBOR(K))
            CLT(K) = KENT
          ENDIF
        ENDIF
      ENDDO
!
!     BUILDS ARRAY MASKTR ACCORDING TO CLT
!
!     MASKTR EQUALS 1 FOR A SEGMENT OF TYPE NEUMANN, 0 OTHERWISE
!
!     A SEGMENT IS OF TYPE NEUMANN IF AT LEAST ONE OF ITS POINTS
!     IS SPECIFIED AS NEUMANN BY THE USER.
!
!
!     INITIALISES THE MASKS TO 0
!
      CALL OS('X=0     ',MASKTR)
      DO K1 = 1 , NPTFR
        K2 = KP1BOR(K1)
!       K2=K1 => IN PARALLEL MODE, NEXT IN OTHER SUB-DOMAIN
!                                  IN SUCH A CASE THE MASK BY
!                                  SEGMENT SHOULD NOT BE USED
        IF(K2.NE.K1) THEN
          IF(CLT(K1).EQ.KLOG.OR.CLT(K2).EQ.KLOG) THEN
!           SEGMENTS OF TYPE NEUMANN
            MASKTR%ADR(NEU)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KENT.AND.CLT(K2).EQ.KSORT) THEN
!           SEGMENTS OF TYPE EXIT
            MASKTR%ADR(DDL)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KSORT.OR.CLT(K2).EQ.KSORT) THEN
            MASKTR%ADR(DDL)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KSORT.AND.CLT(K2).EQ.KENT) THEN
!           SEGMENTS OF TYPE EXIT
            MASKTR%ADR(DDL)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KENT.OR.CLT(K2).EQ.KENT) THEN
            MASKTR%ADR(DIR)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KINC.OR.CLT(K2).EQ.KINC) THEN
            MASKTR%ADR(OND)%P%R(K1)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,100)
            IF(LNG.EQ.2) WRITE(LU,101)
100         FORMAT(1X,'DIFFIN : CAS NON PREVU')
101         FORMAT(1X,'DIFFIN : UNEXPECTED CASE')
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ENDDO
!
!     IN PARALLEL MODE, RETRIEVES THE 1 GIVEN BY ANOTHER
!     SUB-DOMAIN (I.E. TAKES THE MAX OF EACH POINT)
!
C     IF(NCSIZE.GT.1) THEN
C       CALL PARCOM_BORD(MASKTR%ADR(NEU)%P%R,3,MESH)
C       CALL PARCOM_BORD(MASKTR%ADR(DDL)%P%R,3,MESH)
C       CALL PARCOM_BORD(MASKTR%ADR(DIR)%P%R,3,MESH)
C       CALL PARCOM_BORD(MASKTR%ADR(OND)%P%R,3,MESH)
C     ENDIF
!
!  POSSIBLE MASKING
!
      IF(MSK) THEN
        DO K1 = 1 , NPTFR
          IELEM=NELBOR(K1)
          IF(IELEM.GT.0) THEN
            MASKTR%ADR(DIR)%P%R(K1) = MASKTR%ADR(DIR)%P%R(K1) *
     &                                                     MASKEL(IELEM)
            MASKTR%ADR(DDL)%P%R(K1) = MASKTR%ADR(DDL)%P%R(K1) *
     &                                                     MASKEL(IELEM)
            MASKTR%ADR(NEU)%P%R(K1) = MASKTR%ADR(NEU)%P%R(K1) *
     &                                                     MASKEL(IELEM)
            MASKTR%ADR(OND)%P%R(K1) = MASKTR%ADR(OND)%P%R(K1) *
     &                                                     MASKEL(IELEM)
          ENDIF
        ENDDO
!
!       IN PARALLEL MODE, RETRIEVES THE 0S GIVEN BY ANOTHER
!       SUB-DOMAIN (I.E. TAKES THE MIN OF EACH POINT)
!
C       IF(NCSIZE.GT.1) THEN
C         CALL PARCOM_BORD(MASKTR%ADR(NEU)%P%R,4,MESH)
C         CALL PARCOM_BORD(MASKTR%ADR(DDL)%P%R,4,MESH)
C         CALL PARCOM_BORD(MASKTR%ADR(DIR)%P%R,4,MESH)
C         CALL PARCOM_BORD(MASKTR%ADR(OND)%P%R,4,MESH)
C       ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     LIQUID BOUNDARIES MASK
!
      DO K=1,NPTFR
        K2=KP1BOR(K)
        IF(K2.NE.K) MASKTR%ADR(NONEU)%P%R(K)=1.D0-MASKTR%ADR(NEU)%P%R(K)
      ENDDO
!
!-----------------------------------------------------------------------
!
! FROM PHYSICAL TO TECHNICAL CONDITIONS
!
      DO K=1,NPTFR
!
        IF(CLT(K).EQ.KENT ) THEN
!
!         ENTERING THE DOMAIN: IMPOSED TRACER
!
          LIMTRA(K) = KDIR
!
        ELSEIF(CLT(K).EQ.KSORT) THEN
!
!         LEAVING THE DOMAIN : FREE IF SUPG OR PSI SCHEME,
!                              RESULT OF IMPOSED ADVECTION OTHERWISE
!
          IF(ICONV.EQ.1) THEN
!           SEE DIFFCL : TTILD PUT IN TBOR
            LIMTRA(K) = KDIR
          ELSE
            LIMTRA(K) = KDDL
          ENDIF
!
        ELSEIF(CLT(K).EQ.KLOG ) THEN
!
!         WALL: NEUMANN CONDITIONS (IT'S NOT ACTUALLY USED)
!
          LIMTRA(K) = KNEU
!
        ELSE
!
!         ERROR, UNKNOWN VALUE OF LITBOR
!
          IF(LNG.EQ.1) WRITE(LU,10) K,LITBOR(K)
          IF(LNG.EQ.2) WRITE(LU,12) K,LITBOR(K)
10        FORMAT(1X,'DIFFIN: POINT ',1I6,' LITBOR= ',1I6,' ?????')
12        FORMAT(1X,'DIFFIN: POINT ',1I6,' LITBOR= ',1I6,' ?????')
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
      ENDDO
!
!----------------------------------------------------------------------
!
!     POST-TREATMENT FOR LIQUID BOUNDARY CONDITIONS (THOMPSON METHOD)
!     THE TRACER BOUNDARY CONDITION THEN IS OF TYPE DIRICHLET
!
      IF(NFRLIQ.GT.0.AND.THOMFR) THEN
!
        DO K= 1 , NPTFR
          IFRLIQ=NUMLIQ(K)
          IF(IFRLIQ.GT.0) THEN
            IF(FRTYPE(IFRLIQ).EQ.2) LIMTRA(K) = KDIR
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
