C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    DECLARATIONS COMMON TO ALL PROGRAMS<br>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
C#######################################################################
C
      MODULE DECLARATIONS_TELEMAC
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C----------------------------------------------------------------------
C
C 1./ INTEGER VALUES TO DESCRIBE BOUNDARY CONDITIONS:
C
C
C     FOR THE BOUNDARY CONDITIONS FILE:
C
C     ENTRANCE: PRESCRIBED VALUES (SAVE VELOCITIES)
      INTEGER, PARAMETER :: KENT  =  5
C
C     VELOCITY IMPOSED (INSTEAD OF DISCHARGE)
      INTEGER, PARAMETER :: KENTU =  6
C
C     FREE OUTPUT
      INTEGER, PARAMETER :: KSORT =  4
C
C     NO-SLIP CONDITION
      INTEGER, PARAMETER :: KADH  =  0
C
C     WALL WITH OR WITHOUT FRICTION
      INTEGER, PARAMETER :: KLOG  =  2
C
C     OPEN BOUNDARY WITH INCIDENT WAVE
      INTEGER, PARAMETER :: KINC  =  1
C
C     ESTEL-2D : FREE DRAINAGE
      INTEGER, PARAMETER :: KDRAIN  =  3
C
C     ESTEL-2D : MIXED CONDITION
      INTEGER, PARAMETER :: KMIX  =  4
C
C     DEPENDING ON ALGORITHMS AND CASES, THESE VALUES WILL BE
C     TRANSFORMED INTO:
C
C     TECHNICAL BOUNDARY CONDITIONS
C
C     NEUMANN
      INTEGER, PARAMETER :: KNEU  =  1
C
C     DIRICHLET
      INTEGER, PARAMETER :: KDIR  =  2
C
C     DEGREE OF FREEDOM
      INTEGER, PARAMETER :: KDDL  =  3
C
C     INCIDENT WAVE
      INTEGER, PARAMETER :: KOND  =  4
C
C----------------------------------------------------------------------
C
C 2./ INTEGER VALUES TO DESCRIBE ADVECTION SCHEMES:
C
C     CHARACTERISTICS
      INTEGER, PARAMETER :: ADV_CAR     =  1
C     SUPG
      INTEGER, PARAMETER :: ADV_SUP     =  2
C     LEO POSTMA
      INTEGER, PARAMETER :: ADV_LPO     =  3
C     DISTRIBUTIVE N-SCHEME
      INTEGER, PARAMETER :: ADV_NSC     =  4
C     DISTRIBUTIVE PSI SCHEME
      INTEGER, PARAMETER :: ADV_PSI     =  5
C     NON CONSERVATIVE EQUATION, DISTRIBUTIVE PSI SCHEME
      INTEGER, PARAMETER :: ADV_PSI_NC  =  6
C     NON CONSERVATIVE EQUATION, DISTRIBUTIVE N SCHEME
      INTEGER, PARAMETER :: ADV_NSC_NC  =  7
C     LEO POSTMA, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_LPO_TF  = 13
C     DISTRIBUTIVE N SCHEME, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_NSC_TF  = 14
C     DISTRIBUTIVE PSI SCHEME, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_PSI_TF  = 15
C
C-----------------------------------------------------------------------
C
C 3./ CODE COUPLING
C
      CHARACTER*144 COUPLING
C
C 4./ NAME OF CURRENT CODE (SEE BIEF_OPEN_FILES AND CONFIG_CODE)
C
      CHARACTER(LEN=24) :: NAMECODE,NNAMECODE(3)
C
C-----------------------------------------------------------------------
C
      END MODULE DECLARATIONS_TELEMAC

C
C#######################################################################
C
