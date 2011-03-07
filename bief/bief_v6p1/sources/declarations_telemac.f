!                    ***************************
                     MODULE DECLARATIONS_TELEMAC
!                    ***************************
!
!
!***********************************************************************
! BIEF
!***********************************************************************
!
!brief    DECLARATIONS COMMON TO ALL PROGRAMS
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!----------------------------------------------------------------------
!
! 1./ INTEGER VALUES TO DESCRIBE BOUNDARY CONDITIONS:
!
!
!     FOR THE BOUNDARY CONDITIONS FILE:
!
!     ENTRANCE: PRESCRIBED VALUES (SAVE VELOCITIES)
      INTEGER, PARAMETER :: KENT  =  5
!
!     VELOCITY IMPOSED (INSTEAD OF DISCHARGE)
      INTEGER, PARAMETER :: KENTU =  6
!
!     FREE OUTPUT
      INTEGER, PARAMETER :: KSORT =  4
!
!     NO-SLIP CONDITION
      INTEGER, PARAMETER :: KADH  =  0
!
!     WALL WITH OR WITHOUT FRICTION
      INTEGER, PARAMETER :: KLOG  =  2
!
!     OPEN BOUNDARY WITH INCIDENT WAVE
      INTEGER, PARAMETER :: KINC  =  1
!
!     ESTEL-2D : FREE DRAINAGE
      INTEGER, PARAMETER :: KDRAIN  =  3
!
!     ESTEL-2D : MIXED CONDITION
      INTEGER, PARAMETER :: KMIX  =  4
!
!     DEPENDING ON ALGORITHMS AND CASES, THESE VALUES WILL BE
!     TRANSFORMED INTO:
!
!     TECHNICAL BOUNDARY CONDITIONS
!
!     NEUMANN
      INTEGER, PARAMETER :: KNEU  =  1
!
!     DIRICHLET
      INTEGER, PARAMETER :: KDIR  =  2
!
!     DEGREE OF FREEDOM
      INTEGER, PARAMETER :: KDDL  =  3
!
!     INCIDENT WAVE
      INTEGER, PARAMETER :: KOND  =  4
!
!----------------------------------------------------------------------
!
! 2./ INTEGER VALUES TO DESCRIBE ADVECTION SCHEMES:
!
!     CHARACTERISTICS
      INTEGER, PARAMETER :: ADV_CAR     =  1
!     SUPG
      INTEGER, PARAMETER :: ADV_SUP     =  2
!     LEO POSTMA
      INTEGER, PARAMETER :: ADV_LPO     =  3
!     DISTRIBUTIVE N-SCHEME
      INTEGER, PARAMETER :: ADV_NSC     =  4
!     DISTRIBUTIVE PSI SCHEME
      INTEGER, PARAMETER :: ADV_PSI     =  5
!     NON CONSERVATIVE EQUATION, DISTRIBUTIVE PSI SCHEME
      INTEGER, PARAMETER :: ADV_PSI_NC  =  6
!     NON CONSERVATIVE EQUATION, DISTRIBUTIVE N SCHEME
      INTEGER, PARAMETER :: ADV_NSC_NC  =  7
!     LEO POSTMA, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_LPO_TF  = 13
!     DISTRIBUTIVE N SCHEME, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_NSC_TF  = 14
!     DISTRIBUTIVE PSI SCHEME, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_PSI_TF  = 15
!
!-----------------------------------------------------------------------
!
! 3./ CODE COUPLING
!
      CHARACTER*144 COUPLING
!
! 4./ NAME OF CURRENT CODE (SEE BIEF_OPEN_FILES AND CONFIG_CODE)
!
      CHARACTER(LEN=24) :: NAMECODE,NNAMECODE(3)
!
!-----------------------------------------------------------------------
!
      END MODULE DECLARATIONS_TELEMAC