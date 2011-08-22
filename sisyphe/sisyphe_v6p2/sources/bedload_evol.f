!                    *************************
                     SUBROUTINE BEDLOAD_EVOL !
!                    *************************
!
     &(HN,Q,S,ELAY,ACLADM, AVA,COEFPN,CALFA,SALFA,LIMTEC,EBOR,
     & MASKEL,MASK,V2DPAR,UNSV2D,DEBUG,NPOIN,NPTFR,
     & IELMT,KENT,KDIR,KDDL,LOADMETH,
     & DTS,DM,D90,HMIN,LS0,GRAV,XMVS,XMVE,VCE,
     & VF,ENTET,MSK,CONST_ALAYER,LCONDIS,MESH,
     & QS,T1, T2, T3, T4, T5, T6, T7, T8, T9,
     & T10, T11, T12, T13, CSF_SABLE, BREACH, QSX, QSY, ZFCL,SLOPEFF)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE EVOLUTION FOR THE BEDLOAD TRANSPORT.
!
!history  F. HUVELIN
!+        14/09/2004
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
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| AVA            |-->| PERCENT AVAILABLE
!| BREACH         |<->| INDICATOR FOR NON ERODIBLE BED (FINITE VOLUMES SHEMES)
!| CALFA          |<->| COSINUS OF THE ANGLE BETWEEN MEAN FLOW AND TRANSPORT 
!| COEFPN         |<->| CORRECTION OF TRANSORT FOR SLOPING BED EFFECT
!| CONST_ALAYER   |-->| CONSTANT ACTIVE LAYER THICKNESS OR NOT
!| D90            |---| D90
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| DTS            |<->| TIME STEP FOR SUSPENSION
!| EBOR           |<->| BOUNDARY CONDITION FOR BED EVOLUTION (DIRICHLET)
!| ELAY           |<->| THICKNESS OF SURFACE LAYER
!| ELAY0          |<->| ACTIVE LAYER THICKNESS 
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| IELMT          |-->| NUMBER OF ELEMENTS
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| LCONDIS        |-->| LOGICAL, CONSTANT FLOW DISCHARGE
!| LIMTEC         |<->| TYPE OF BOUNDARY CONDITION 
!| LOADMETH       |-->| (A SUPPRIMER)
!| LS0            |-->| (A SUPPRIMER)
!| MASK           |-->| BLOCK OF MASKS, EVERY ONE FOR A TYPE OF BOUNDARY
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| Q              |-->| FLOW DISCHARGE 
!| QS             |<->| EDLOAD TRANSPORT RATE
!| QSX            |<->| SOLID DISCHARGE X 
!| QSY            |<->| SOLID DISCHARGE Y 
!| S              |-->| VOID STRUCTURE
!| SALFA          |<->| SINUS OF THE ANGLE BETWEEN TRANSPORT RATE AND CURRENT
!| SLOPEFF        |-->| LOGICAL, SLOPING BED EFFECT OR NOT  
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
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
!| VCE            |-->| WATER VISCOSITY
!| VF             |-->| LOGICAL, FINITE VOLUMES OR NOT
!| XMVE           |-->| FLUID DENSITY 
!| XMVS           |-->| SEDIMENT DENSITY 
!| ZFCL           |<->| BED EVOLUTION PER CLASS, DUE TO SUSPENDED SEDIMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_BEDLOAD_EVOL => BEDLOAD_EVOL
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HN,Q,S,UNSV2D,ELAY,ACLADM
      TYPE(BIEF_OBJ),   INTENT(IN)    :: COEFPN,CALFA,SALFA,LIMTEC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,MASK,V2DPAR
      INTEGER,          INTENT(IN)    :: DEBUG,SLOPEFF,NPOIN,NPTFR
      INTEGER,          INTENT(IN)    :: IELMT,KENT,KDIR,LOADMETH,KDDL
      DOUBLE PRECISION, INTENT(IN)    :: DTS,DM,D90,HMIN,LS0
      DOUBLE PRECISION, INTENT(IN)    :: GRAV,XMVS,XMVE,VCE,AVA(NPOIN)
      LOGICAL,          INTENT(IN)    :: VF,ENTET,MSK
      LOGICAL,          INTENT(IN)    :: CONST_ALAYER,LCONDIS
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS,EBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1, T2, T3, T4, T5, T6, T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8, T9, T10, T11, T12, T13
      DOUBLE PRECISION, INTENT(IN) :: CSF_SABLE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, QSX, QSY, ZFCL
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: J
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! **************** !
      ! I - SLOPE EFFECT !
      ! **************** !
        IF (SLOPEFF == 1) THEN
          CALL OS('X=XY    ', X=QS , Y=COEFPN)
        ENDIF
        CALL OS('X=YZ    ', X=QSX, Y=QS, Z=CALFA)
        CALL OS('X=YZ    ', X=QSY, Y=QS, Z=SALFA)
      ! ************************************* !
      ! II - TREATMENT OF NON ERODABLE BOTTOM !
      ! ************************************* !
      IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_NERBED_VF'
      IF(VF) THEN     
        CALL BEDLOAD_NERBED_VF
     &        (MESH,LIMTEC,KDDL,ELAY%R,V2DPAR%R,QSX,QSY,AVA,NPOIN,
     &         MESH%NSEG,NPTFR,DTS,QS,T1,T2,T3,BREACH,CSF_SABLE)
        CALL OS('X=YZ    ', X=QSX, Y=QS, Z=CALFA)
        CALL OS('X=YZ    ', X=QSY, Y=QS, Z=SALFA)
      ENDIF
      IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_NERBED_VF'
      ! ***************************************************** !
      ! IVA - SOLVES THE BED-EVOLUTION EQUATION : F.V.        !
      ! ***************************************************** !
      IF (VF) THEN
         IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_SOLVS_VF'
         CALL BEDLOAD_SOLVS_VF
     &        (MESH, QSX, QSY, LIMTEC,UNSV2D, EBOR, BREACH,
     &         MESH%NSEG,NPTFR,NPOIN,KDIR,KDDL,DTS,T10,ZFCL,T11,
     &         CSF_SABLE)
         IF(DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_SOLVS_VF'
      ! ****************************************************** !
      ! IVB - SOLVES THE BED-EVOLUTION EQUATION  : F.E.        !
      ! ****************************************************** !
      ELSE       
        IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_SOLVS_FE'
        DO J=1,NPOIN
!         T13 IS THE SEDIMENT HEIGHT (EXCLUDING VOIDS, SO *CSF_SABLE)
          T13%R(J)=AVA(J)*ELAY%R(J)*CSF_SABLE
        ENDDO
        CALL BEDLOAD_SOLVS_FE
     &        (MESH,S,EBOR,MASKEL,MASK,
     &         QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,
     &         LIMTEC,DTS,MSK,ENTET,T1,T2,T3,T4,T8,
     &         ZFCL,T12,T13,MESH%GLOSEG%I,
     &         MESH%GLOSEG%DIM1,MESH%MSEG%X,
     &         MESH%MSEG%X%R(MESH%NSEG+1:2*MESH%NSEG),
     &         MESH%NSEG,UNSV2D,CSF_SABLE)
        IF(DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_SOLVS_FE'
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
