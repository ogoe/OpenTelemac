!                    ***************************
                     SUBROUTINE BEDLOAD_DIFFIN !
!                    ***************************
!
     &  (U, V, NBOR, XNEBOR, YNEBOR, KP1BOR, MASKEL, NELBOR, NPTFR,
     &   KENT, KSORT, KLOG, KINC, KDIR, KDDL, KNEU, MSK, CLT, LITBOR,
     &   MASKTR, LIMTRA)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS.
!
!history  FRANCOIS MENARD (PLACEMENT @ LNHE)
!+        17/08/2004
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CLT            |<->| TYPE OF BOUNDARY CONDITIONS FOR TRACER (MODIFIED LITBOR)
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KP1BOR         |-->| NEXT POINT ON THE BOUNDARY 
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT  
!| LIMTRA         |<->| TYPE OF BOUNDARY CONDITION FOR TRACER
!| LITBOR         |<->| TYPE OF BOUNDARY CONDITIONS FOR TRACER (***)
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MASKTR         |<->| MASKING FOR TRACERS, PER POINT
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NBOR           |-->| NUMBER OF BOUDARY POINTS
!| NELBOR         |-->| NUMBER OF BOUDARY ELEMENTS
!| NPTFR          |-->| NUMBER OF BOUDARIES
!| U              |-->| FLOW VELOCITY IN THE X DIRECTION
!| V              |-->| FLOW VELOCITY IN THE Y DIRECTION
!| XNEBOR         |-->| X-COORDINATES OF THE BOUNDARY POINT
!| YNEBOR         |-->| Y-COORDINATES OF THE BOUNDARY POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_BEDLOAD_DIFFIN => BEDLOAD_DIFFIN
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ), INTENT(IN)    :: U, V, NBOR, XNEBOR, YNEBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: KP1BOR, MASKEL, NELBOR
      INTEGER,        INTENT(IN)    :: NPTFR, KENT, KSORT, KLOG
      INTEGER,        INTENT(IN)    :: KINC, KDIR, KDDL, KNEU
      LOGICAL,        INTENT(IN)    :: MSK
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CLT
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: LITBOR, MASKTR, LIMTRA
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER            :: K, K1, K2
      DOUBLE PRECISION   :: USCALN,C
      INTEGER, PARAMETER :: DIR = 1
      INTEGER, PARAMETER :: DDL = 2
      INTEGER, PARAMETER :: NEU = 3
      INTEGER, PARAMETER :: OND = 4
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! ****************************************************** !
      ! I - TYPES OF BOUNDARY CONDITIONS FOR THE TRACER        ! (_IMP_)
      !     MAY BE MODIFIED DEPENDING ON THE SIGN OF U.N       ! (_IMP_)
      !     FOR THE LIQUID BOUNDARIES (N : OUTGOING NORMAL)    ! (_IMP_)
      ! ****************************************************** !
      DO K = 1, NPTFR
         CLT%I(K) = LITBOR%I(K)
         ! I.1 - LIQUID BOUNDARIES (_IMP_)
         ! --------------------------------------
         IF (CLT%I(K) == KENT) THEN
            USCALN = U%R(NBOR%I(K))*XNEBOR%R(K)
     &             + V%R(NBOR%I(K))*YNEBOR%R(K)
            ! OUTGOING VELOCITY, FREE TRACER
            ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            IF (USCALN >= 0.D0) CLT%I(K) = KSORT
         ELSEIF(CLT%I(K) == KSORT) THEN
            USCALN = U%R(NBOR%I(K))*XNEBOR%R(K)
     &             + V%R(NBOR%I(K))*YNEBOR%R(K)
            ! ENTERING VELOCITY, FREE TRACER
            ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            IF (USCALN <= 0.D0) CLT%I(K) = KENT
         ENDIF
      ENDDO
!
      ! **************************************************************** !
      ! II - MASKTR ARRAY DEFINED AS A FUNCTION OF CLT                   ! (_IMP_)
      !      EQUALS 1 FOR A SEGMENT OF NEUMANN TYPE, AND 0 OTHERWISE     ! (_IMP_)
      !      A SEGMENT IS OF NEUMANN TYPE IF THE USER SPECIFIES AT LEAST ! (_IMP_)
      !      ONE OF ITS NODES AS NEUMANN.                                ! (_IMP_)
      ! **************************************************************** !
      CALL OS('X=0     ', X=MASKTR)
      DO K1 = 1 , NPTFR
         K2 = KP1BOR%I(K1)
         ! II.1 - NEUMANN TYPE SEGMENTS
         ! -------------------------------
         IF (CLT%I(K1).EQ.KLOG.OR.CLT%I(K2).EQ.KLOG) THEN
            MASKTR%ADR(NEU)%P%R(K1) = 1.D0
         ! II.2 - OUTGOING TYPE SEGMENTS (_IMP_)
         ! ------------------------------
         ELSEIF ((CLT%I(K1) == KENT) .AND. (CLT%I(K2) == KSORT)) THEN
            MASKTR%ADR(DDL)%P%R(K1) = 1.D0
         ELSEIF ((CLT%I(K1) == KSORT) .OR. (CLT%I(K2) == KSORT)) THEN
            MASKTR%ADR(DDL)%P%R(K1) = 1.D0
         ! II.3 - OUTGOING TYPE SEGMENTS (_IMP_)
         ! ------------------------------
         ELSEIF ((CLT%I(K1) == KSORT) .AND. (CLT%I(K2) == KENT)) THEN
            MASKTR%ADR(DDL)%P%R(K1) = 1.D0
         ELSEIF ((CLT%I(K1) == KENT) .OR. (CLT%I(K2) == KENT)) THEN
            MASKTR%ADR(DIR)%P%R(K1) = 1.D0
         ELSEIF ((CLT%I(K1) == KINC) .OR. (CLT%I(K2) == KINC)) THEN
            MASKTR%ADR(OND)%P%R(K1)=1.D0
         ELSE
            IF (LNG == 1) WRITE(LU,101)
            IF (LNG == 2) WRITE(LU,102)
            CALL PLANTE(1)
         ENDIF
      ENDDO
!
      ! *********************** !
      ! III - POTENTIAL MASKING !
      ! *********************** !
      IF(MSK) THEN
        DO K1 = 1 , NPTFR
          C=MASKEL%R(NELBOR%I(K1))
          MASKTR%ADR(DIR)%P%R(K1) = MASKTR%ADR(DIR)%P%R(K1)*C
          MASKTR%ADR(DDL)%P%R(K1) = MASKTR%ADR(DDL)%P%R(K1)*C
          MASKTR%ADR(NEU)%P%R(K1) = MASKTR%ADR(NEU)%P%R(K1)*C
          MASKTR%ADR(OND)%P%R(K1) = MASKTR%ADR(OND)%P%R(K1)*C
        ENDDO
      ENDIF
!
      ! ************************************************************** !
      ! IV - FROM PHYSICAL CONDITION TO TECHNICAL CONDITIONS           !
      ! ************************************************************** !
      DO K = 1, NPTFR
         ! IV.1 - 'INCOMING' BOUNDARY : IMPOSED TRACER (_IMP_)
         ! -----------------------------------------
         IF(CLT%I(K).EQ.KENT) THEN
            LIMTRA%I(K) = KDIR
         ELSEIF(CLT%I(K).EQ.KSORT) THEN
            LIMTRA%I(K) = KDDL
         ! IV.2 - SOLID BOUNDARY : NEUMANN CONDITIONS (_IMP_)
         ! ------------------------------------
         ELSEIF(CLT%I(K).EQ.KLOG ) THEN
            LIMTRA%I(K) = KNEU
         ! IV.3 - ERROR: UNKNOWN LITBOR VALUE (_IMP_)
         ! ----------------------------------------
         ELSE
            IF (LNG == 1) WRITE(LU,11) K, LITBOR%I(K)
            IF (LNG == 2) WRITE(LU,12) K, LITBOR%I(K)
            CALL PLANTE(1)
            STOP
         ENDIF
      ENDDO
      !----------------------------------------------------------------!
101   FORMAT(' DIFFIN_SISYPHE : CAS NON PREVU')
11    FORMAT(' DIFFIN_SISYPHE : POINT ',1I8,' LITBOR= ',1I8,' ?')
      !----------------------------------------------------------------!
102   FORMAT(' DIFFIN_SISYPHE: UNEXPECTED CASE')
12    FORMAT(' DIFFIN_SISYPHE : POINT ',1I8,' LITBOR= ',1I8,' ?')
      !----------------------------------------------------------------!
!
!======================================================================!
!======================================================================!
      RETURN
      END
