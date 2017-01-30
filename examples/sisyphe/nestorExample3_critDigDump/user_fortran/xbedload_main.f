!                    ***********************
                     SUBROUTINE xBEDLOAD_MAIN
!                    ***********************
!
     &(ACLADM,KSP,KSR, V2DPAR,UNSV2D,CF,EBOR,FW,HN,LIQBOR,
     & MASK, MASKEL, MASKPT, Q, QBOR, U2D,
     & V2D, S,UNLADM,UW,THETAW,MU,TOB,TOBW,TW,ZF,
     & DEBUG, HIDFAC, ICF, IELMT, ISOUS, KDDL, KDIR,
     & KENT, KINC, KLOG, KNEU, KSORT, LOADMETH, LT,
     & NPOIN, NPTFR, NSICLA, OPTBAN, LS0, BETA, FD90, FDM,
     & GRAV, HIDI, HMIN, VCE, CSF_SABLE, XMVE, XMVS, XWC,
     & PI, KARMAN, ZERO, KARIM_HOLLY_YANG,MSK, SUSP, VF,
     & ENTET, CONST_ALAYER, LCONDIS, LGRAFED, MESH,
     & ELAY, LIEBOR, LIMTEC, MASKTR,
     & IT1, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11,
     & T12,T13,UNORM,AC, AT0, DTS, ELAY0, FRACSED_GF,
     &   AVAIL, BREACH, CALFA_CL, COEFPN,
     & DZF_GF, HIDING, QSCL_C, QSCL_S, QS_C,
     & QSCLXC, QSXC, QSCLYC, QSYC, SALFA_CL, ZF_C, ZFCL_C, NSOUS,
     & ENTETS, SECCURRENT, SLOPEFF,
     & PHISED, DEVIA, BETA2, BIJK,SEDCO,HOULE,
     & U3D,V3D,CODE,FLBCLA,MAXADV)
!
!***********************************************************************
! SISYPHE   V7P2                                   21/07/2011
!***********************************************************************
!
!brief    MAIN SUBROUTINE FOR THE BEDLOAD TRANSPORT.
!
!history  F. HUVELIN
!+        14/09/2004
!+
!+
!
!history  JMH
!+        21/12/2006
!+        V6P0
!+   BEDLOAD_TIMESTEP NO LONGER EXISTS
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        28/03/2014
!+        V7P0
!+  Call to bedload_diffin changed.
!history  R KOPMANN (BAW)
!+        10/05/2016
!+        V7P2
!+ CALFA,SALFA dependent of grain classes
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| AT0            |<->| TIME IN S
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| BETA           |-->| COEFFICIENT FOR SLOPING BED EFFECT ( KOCH AND FLOKSTRA)
!| BETA2          |-->| COEFFICIENT FOR THE DEVIATION  (TALMON ET AL.)
!| BIJK           |-->| COEFFICIENT OF THE BIJKER FORMULA
!| BREACH         |<->| INDICATOR FOR NON ERODIBLE BED (FINITE VOLUMES SHEMES)
!| CALFA          |<->| COSINUS OF THE ANGLE BETWEEN MEAN FLOW AND TRANSPORT
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| CODE           |-->| HYDRODYNAMIC CODE IN CASE OF COUPLING
!| COEFPN         |<->| CORRECTION OF TRANSORT FOR SLOPING BED EFFECT
!| CONST_ALAYER   |-->| CONSTANT ACTIVE LAYER THICKNESS OR NOT
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| DEVIA          |-->| SLOPE EFFECT FORMULA FOR DEVIATION
!| DTS            |<->| TIME STEP FOR SUSPENSION
!| DZF_GF         |<->| (A SUPPRIMER)
!| EBOR           |<->| IMPOSED BOUNDARY CONDITION FOR BED EVOLUTION (DIRICHLET)
!| ELAY           |<->| THICKNESS OF SURFACE LAYER
!| ELAY0          |<->| ACTIVE LAYER THICKNESS
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION
!| ENTETS         |<->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION FOR SUSPENSION
!| FD90           |-->| DIAMETER D90
!| FDM            |-->| DIAMETER DM FOR EACH CLASS
!| FLBCLA         |-->| BLOCK OF FLUXES AT BOUNDARY FOR EACH CLASS
!| FRACSED_GF     |<->| (A SUPPRIMER)
!| FW             |-->| WAVE FRICTION FACTOR
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HIDFAC         |-->| HIDING FACTOR FORMULAS
!| HIDI           |-->| HIDING FACTOR FOR PARTICULAR SIZE CLASS (HIDFAC =0)
!| HIDING         |-->| HIDING FACTOR CORRECTION
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| HOULE          |-->| LOGICAL, FOR WAVE EFFECTS
!| ICF            |-->| BED-LOAD OR TOTAL LOAD TRANSPORT FORMULAS
!| IELMT          |-->| NUMBER OF ELEMENTS
!| ISOUS          |-->| SUB-ITERATIONS
!| IT1            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| KSP            |-->| BED SKIN ROUGHNESS
!| KSR            |-->| RIPPLE BED ROUGHNESS
!| LCONDIS        |-->| LOGICAL, CONSTANT FLOW DISCHARGE
!| LGRAFED        |-->| (A SUPPRIMER)
!| LIEBOR         |<->| TYPE OF BOUNDARY CONDITIONS FOR BED EVOLUTION
!| LIMTEC         |<->| TECHNICAL BOUNDARY CONDITION (NEUMAN...)
!| LIQBOR         |-->| TYPE OF BOUNDARY CONDITION FOR QS
!| LOADMETH       |-->| (A SUPPRIMER)
!| LS0            |-->| (A SUPPRIMER)
!| LT             |-->| ITERATION
!| MASK           |-->| BLOCK OF MASKS, EVERY ONE FOR A TYPE OF BOUNDARY
!|                |   | SEE DIFFIN.F IN LIBRARY BIEF.
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MASKPT         |-->| MASKING PER POINT
!| MASKTR         |<->| MASKING FOR TRACERS, PER POINT
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| MU             |<->| CORRECTION FACTOR FOR BED ROUGHNESS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| NSOUS          |<->| NUMBER OF SUB-ITERATIONS
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS
!| PHISED         |-->| ANGLE OF REPOSE OF THE SEDIMENT
!| PI             |-->| PI
!| Q              |-->| FLOW DISCHARGE
!| QBOR           |-->| BOUNDARY CONDITION FOR TRANSPORT RATE
!| QSCLXC         |<->| TRANSPORT RATE FOR EACH CLASS X-DIRECTION
!| QSCLYC         |<->| TRANSPORT RATE FOR EACH CLASS Y-DIRECTION
!| QSCL_C         |<->| BEDLOAD TRANSPORT RATE
!| QSCL_S         |<->| SUSPENDED LOAD TRANSPORT RATE
!| QSXC           |<->| BEDLOAD TRANSPORT RATE X-DIRECTION
!| QSYC           |<->| BEDLOAD TRANSPORT RATE Y-DIRECTION
!| QS_C           |<->| BEDLOAD TRANSPORT RATE
!| S              |-->| VOID STRUCTURE
!| SALFA          |<->| SINUS OF THE ANGLE BETWEEN TRANSPORT RATE AND CURRENT
!| SECCURRENT     |-->| LOGICAL, PARAMETRISATION FOR SECONDARY CURRENTS
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| SLOPEFF        |-->| LOGICAL, SLOPING BED EFFECT OR NOT
!| SUSP           |-->| LOGICAL, SUSPENSION
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T10            |<->| WORK BIEF_OBJ STRUCTURE
!| T11            |<->| WORK BIEF_OBJ STRUCTURE
!| T12            |<->| WORK BIEF_OBJ STRUCTURE
!| T13            |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| T9             |<->| WORK BIEF_OBJ STRUCTURE
!| THETAW         |-->| ANGLE BETWEEN WAVE AND CURRENT
!| TOB            |<->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOBW           |-->| WAVE INDUCED SHEAR STRESS
!| TW             |-->| WAVE PERIOD
!| U2D            |<->| MEAN FLOW VELOCITY X-DIRECTION
!| U3D            |-->| THREE-DIMENSIONAL VELOCITY X-DIRECTION
!| UNLADM         |-->| MEAN DIAMETER OF ACTIVE STRATUM LAYER
!| UNORM          |<->| NORM OF THE MEAN FLOW VELOCITY
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| UW             |-->| ORBITAL WAVE VELOCITY
!| V2D            |<->| MEAN FLOW VELOCITY Y-DIRECTION
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
!| V3D            |-->| THREE-DIMENSIONAL VELOCITY Y-DIRECTION
!| VCE            |-->| WATER VISCOSITY
!| VF             |-->| LOGICAL, FINITE VOLUMES OR NOT
!| CSF_SABLE      |-->| BED VOLUME CONCENTRATION CSF = (1-POROSITY)
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
!| XWC            |-->| SETTLING VELOCITY
!| ZERO           |-->| ZERO
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZFCL_C         |<->| BEDLOAD EVOLUTION FOR EACH SEDIMENT CLASS
!| ZF_C           |<->| BEDLOAD EVOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_BEDLOAD_MAIN => BEDLOAD_MAIN
cgl   USE DECLARATIONS_SISYPHE, ONLY : DREDGESIM,NOMBLAY
      USE DECLARATIONS_SISYPHE, ONLY :           NOMBLAY
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, KSR,V2DPAR,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CF,FW,KSP,HN,LIQBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASK, MASKEL, MASKPT
      TYPE(BIEF_OBJ),   INTENT(IN)    :: Q, QBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: U2D,V2D,TOB,MU,UNORM,EBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,UNLADM
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UW, THETAW,  TOBW, TW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF
      INTEGER,          INTENT(IN)    :: DEBUG, HIDFAC, ICF,MAXADV
      INTEGER,          INTENT(IN)    :: IELMT, ISOUS, KDDL, KDIR, KENT
      INTEGER,          INTENT(IN)    :: KINC, KLOG, KNEU, KSORT
      INTEGER,          INTENT(IN)    :: LOADMETH, LT,NPOIN, NPTFR
      INTEGER,          INTENT(IN)    :: NSICLA, OPTBAN
      DOUBLE PRECISION, INTENT(IN)    :: LS0, BETA, FD90(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: FDM(NSICLA),GRAV
      DOUBLE PRECISION, INTENT(IN)    :: HIDI(NSICLA),HMIN,VCE
      DOUBLE PRECISION, INTENT(IN)    :: CSF_SABLE,XMVE,XMVS,XWC(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: PI,KARMAN,ZERO
      DOUBLE PRECISION, INTENT(IN)    :: KARIM_HOLLY_YANG
      LOGICAL,          INTENT(IN)    :: MSK, SUSP, VF
      LOGICAL,          INTENT(IN)    :: ENTET, CONST_ALAYER
      LOGICAL,          INTENT(IN)    :: LCONDIS, LGRAFED,SECCURRENT
      LOGICAL,          INTENT(IN)    :: SEDCO(NSICLA),HOULE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ELAY,FLBCLA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: LIEBOR, LIMTEC, MASKTR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: IT1,T1,T2,T3,T4,T5,T6,T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8,T9,T10,T11,T12,T13
      DOUBLE PRECISION, INTENT(INOUT) :: AC(NSICLA), AT0, DTS, ELAY0
      DOUBLE PRECISION, INTENT(INOUT) :: FRACSED_GF(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, CALFA_CL, COEFPN
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DZF_GF
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSCL_C,QSCL_S
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS_C, QSCLXC, QSXC, QSCLYC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSYC, SALFA_CL, ZF_C, ZFCL_C
      INTEGER,          INTENT(INOUT) :: NSOUS
      LOGICAL,          INTENT(INOUT) :: ENTETS
      DOUBLE PRECISION,   INTENT(IN)  :: BETA2, PHISED
      INTEGER, INTENT (IN)            :: SLOPEFF, DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: BIJK
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!     --> nestor
      LOGICAL :: NESTOR = .TRUE.
!
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!
!     INITIALISES TECHNICAL BOUNDARY CONDITIONS
!
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_DIFFIN'
      CALL BEDLOAD_DIFFIN
     &        (U2D, V2D, MESH%NBOR, MESH%XNEBOR, MESH%YNEBOR,
     &         MASKEL, MESH%NELBOR, NPTFR, KENT, KSORT, KLOG,
     &         KDIR, KDDL, KNEU, MSK, IT1, LIEBOR, MASKTR,LIMTEC,
     &         MESH%IKLBOR%I,MESH%NELEB,MESH%NELEBX)
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_DIFFIN'
!
      DO I = 1, NSICLA
!
!       FOR SAND
        IF(.NOT.SEDCO(I)) THEN
          IF (DEBUG > 0) WRITE(LU,*)
     &      'BEDLOAD_SOLIDISCHARGE : ',I,'/',NSICLA
          CALL BEDLOAD_SOLIDISCHARGE
     &       (MESH, U2D, V2D, UNORM,HN, TW, UW, MU,TOB,CF,
     &         TOBW,FW,THETAW,AVAIL(1:NPOIN,1,I),
     &         MASKPT, MASKEL, ACLADM,
     &         UNLADM,KSP,KSR, LIQBOR, QBOR%ADR(I)%P, DEBUG, NPOIN,
     &         NPTFR, IELMT, ICF, KENT, OPTBAN, HIDFAC, GRAV,
     &         FDM(I), FD90(I), XWC(I), XMVE, XMVS, VCE, HMIN,
     &         HIDI(I),KARMAN,ZERO,PI,
     &         KARIM_HOLLY_YANG,SUSP,MSK,T1,T2,
     &         T3, T4, T5, T6, T7, T8, T9, T10, T11,T12, AC(I),
     &         HIDING,QSCL_C%ADR(I)%P,QSCL_S%ADR(I)%P,
     &         SLOPEFF,COEFPN,PHISED,
     &         CALFA_CL%ADR(I)%P,SALFA_CL%ADR(I)%P,
     &         BETA,ZF,S,
     &         DEVIA, BETA2 , SECCURRENT, BIJK,HOULE,UNSV2D,
     &         U3D,V3D,CODE)
          IF(DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_SOLIDISCHARGE'
        ELSE
!         FOR COHESIVE SEDIMENT: ZERO BEDLOAD TRANSPORT RATE
!         JMH: IS THIS USEFUL ???
          CALL OS('X=0     ',X=QSCL_C%ADR(I)%P)
          CALL OS('X=0     ',X=QSCLXC%ADR(I)%P)
          CALL OS('X=0     ',X=QSCLYC%ADR(I)%P)
        ENDIF
!
      ENDDO
!
!     COMPUTES THE EVOLUTION FOR EACH CLASS
!
      DO I = 1, NSICLA
!
        IF(.NOT.SEDCO(I)) THEN
!
          IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_EVOL : ',I,'/',NSICLA
          CALL BEDLOAD_EVOL(HN,Q,S,ELAY,ACLADM,AVAIL(1:NPOIN,1,I),
     &                      COEFPN,CALFA_CL%ADR(I)%P,SALFA_CL%ADR(I)%P,
     &                      LIMTEC,
     &                      EBOR%ADR(I)%P,MASKEL,MASK,
     &                      V2DPAR,UNSV2D,DEBUG,NPOIN,NPTFR,IELMT,
     &                      KENT,KDIR,KDDL,LOADMETH,
     &                      DTS,FDM(I),FD90(I),HMIN,LS0,GRAV,XMVS,XMVE,
     &                      VCE,VF,ENTETS,MSK,CONST_ALAYER,
     &                      LCONDIS,MESH,QSCL_C%ADR(I)%P,
     &                      T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,
     &                      T13,CSF_SABLE,BREACH,QSCLXC%ADR(I)%P,
     &                      QSCLYC%ADR(I)%P,ZFCL_C%ADR(I)%P,SLOPEFF,
     &                      I,FLBCLA,LIQBOR,QBOR%ADR(I)%P,MAXADV)
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_BEDLOAD_EVOL'
!
!         NOW DIVIDING BY CSF_SABLE TO GET THE EVOLUTION OF BED
!         INCLUDING VOIDS
!         NOTE JMH: IN BEDLOAD_EVOL THERE IS A PRELIMINARY MULTIPLICATION BY
!                   CSF_SABLE, SO THIS COULD BE SIMPLIFIED, BUT FOR THE
!                   FINITE ELEMENT OPTION ONLY, THE FINITE VOLUME IMPLEMENTATION
!                   SEEMS MORE COMPLICATED TO SORT OUT.
!
          CALL OS('X=CX    ',X= ZFCL_C%ADR(I)%P,C=1.D0/CSF_SABLE)
!
        ELSE
!
!         NO EVOLUTION FOR COHESIVE SEDIMENT
          CALL OS('X=0     ',X=ZFCL_C%ADR(I)%P)
!
        ENDIF
!
      ENDDO
!
!     nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!      nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!       nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!        nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!         nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!          nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!           nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!            nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!            nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!           nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!          nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!         nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!        nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!       nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!      nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!     nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!
       IF (NESTOR.EQV..TRUE.) THEN
       CALL InterFaceRunNestor(   NPOIN      !  number of points (Nodes)
     &                          , NSICLA     !  number of SIze CLAsses
     &                          , LT         !  Telemac time step
     &                          , DTS        !  duration of Sisyphe time step
     &                          , AT0        !  time
     &                          , ELAY0      !  active layer thickness [m]
     &                          , ZF%R       !  bottom [m+NN]
     &                          , ZFCL_C     !  evolution per class per time step [m]
     &                          , AVAIL(1:NPOIN,1,1:NSICLA)    !
     &                          , MESH%KNOLG%I    ! index list: Local to Global node index
!    &                          , MESH%KNOGL%I    ! index list: Global to Local node index
!    &                          , SIZE( MESH%KNOGL%I(:))    ! index list: Global to Local node index
!    &                          , MESH%X%R
!    &                          , MESH%Y%R
     &                        )
       ENDIF
!
!     CALLS DREDGESIM
!
!     IF(DREDGESIM) CALL DREDGESIM_INTERFACE(2)
      ! *********************************************** !
      ! II - EVOLUTIONS AND QS FOR EACH CLASS ARE ADDED !
      ! *********************************************** !
      ! II.1 - INITIALISES
      ! ---------------------
      CALL OS('X=0     ', X=QS_C)
      CALL OS('X=0     ', X=ZF_C)
      CALL OS('X=0     ',X=QSXC)
      CALL OS('X=0     ',X=QSYC)
      ! II.2 - ADDS THE CLASSES
      ! ----------------------
      !
      DO I=1,NSICLA
        IF(.NOT.SEDCO(I)) THEN
          CALL OS('X=X+Y   ', X=QS_C, Y=QSCL_C%ADR(I)%P)
          CALL OS('X=X+Y   ', X=ZF_C, Y=ZFCL_C%ADR(I)%P)
           CALL OS('X=X+YZ  ', X=QSXC, Y=QSCL_C%ADR(I)%P,
     &                               Z=CALFA_CL%ADR(I)%P)
           CALL OS('X=X+YZ  ', X=QSYC, Y=QSCL_C%ADR(I)%P,
     &                               Z=SALFA_CL%ADR(I)%P)

        ENDIF
      ENDDO
!
!     TIDAL FLATS WITH MASKING     JMH ON 27/07/2006
!
      IF(OPTBAN.EQ.2) CALL OS('X=XY    ',X=ZF_C,Y=MASKPT)
!
      ! II.3 - SLOPE EFFECT FOR THE SUM OF THE QS
      ! -----------------------------------------
      ! QS : COEFPN AND CALFA, SALFA ALREADY ADDED IN QSCL_C
!      CALL OS('X=YZ    ', X=QSXC, Y=QS_C, Z=CALFA)
!      CALL OS('X=YZ    ', X=QSYC, Y=QS_C, Z=SALFA)
!
!======================================================================!
!======================================================================!
!
      RETURN
      END

