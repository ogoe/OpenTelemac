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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        09/05/2014
!+        V7P0
!+   Parameter MODASS added.
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
      CHARACTER(LEN=144), TARGET :: COUPLING
!
! 4./ NAME OF CURRENT CODE (SEE BIEF_OPEN_FILES AND CONFIG_CODE)
!
      CHARACTER(LEN=24) :: NAMECODE,NNAMECODE(3)
!
! 5./ DIFFERENT WAYS OF SPLITTING PRISMS :
!    
!     TO ENSURE MATCHING OF TETRAHEDRONS, FACES OF TRIANGLES ARE "SIGNED"
!     WITH 1 OR 2 DEPENDING OF THE GLOBAL NUMBERS OF THEIR POINTS, TAKEN IN
!     COUNTER-CLOCKWISE DIRECTION. A FACE 1 IN A TRIANGLE WILL BE 2 IN ITS
!     NEIGHBOUR AND THIS IS USED TO HAVE A CORRECT SPLITTING. THE SPLITTING
!     DEPENDING ON THE "SIGNS" OF THE 3 FACES IS GIVEN IN ARRAY TETRA.
!
!
!     TETRA(2,2,2,3,4)
!
!     FIRST 3 DIMENSIONS : TYPE OF FACE
!                      1 : CUT RECTANGLE BETWEEN  LOW-LEFT AND HIGH-RIGHT
!                      2 : CUT RECTANGLE BETWEEN  HIGH-LEFT AND LOW-RIGHT
!
!     4TH DIMENSION : NUMBER OF TETRAHEDRON
!     5TH DIMENSION : 4 POINTS OF THE TETRAHEDRON (IN LOCAL PRISM NUMBERING)
!
!     1 1 2 SPLITTING
!
!     TETRA(1,1,2,1,1)= 1
!     TETRA(1,1,2,1,2)= 2
!     TETRA(1,1,2,1,3)= 3
!     TETRA(1,1,2,1,4)= 6
!
!     TETRA(1,1,2,2,1)= 4
!     TETRA(1,1,2,2,2)= 6
!     TETRA(1,1,2,2,3)= 5
!     TETRA(1,1,2,2,4)= 1
!
!     TETRA(1,1,2,3,1)= 5
!     TETRA(1,1,2,3,2)= 2
!     TETRA(1,1,2,3,3)= 1
!     TETRA(1,1,2,3,4)= 6
!
!     2 1 1 SPLITTING
!
!     TETRA(2,1,1,1,1)= 1
!     TETRA(2,1,1,1,2)= 2
!     TETRA(2,1,1,1,3)= 3
!     TETRA(2,1,1,1,4)= 4
!
!     TETRA(2,1,1,2,1)= 4
!     TETRA(2,1,1,2,2)= 6
!     TETRA(2,1,1,2,3)= 5
!     TETRA(2,1,1,2,4)= 2
!
!     TETRA(2,1,1,3,1)= 6
!     TETRA(2,1,1,3,2)= 3
!     TETRA(2,1,1,3,3)= 2
!     TETRA(2,1,1,3,4)= 4
!
!     1 2 1 SPLITTING
!
!     TETRA(1,2,1,1,1)= 1
!     TETRA(1,2,1,1,2)= 2
!     TETRA(1,2,1,1,3)= 3
!     TETRA(1,2,1,1,4)= 5
!
!     TETRA(1,2,1,2,1)= 4
!     TETRA(1,2,1,2,2)= 6
!     TETRA(1,2,1,2,3)= 5
!     TETRA(1,2,1,2,4)= 3
!
!     TETRA(1,2,1,3,1)= 4
!     TETRA(1,2,1,3,2)= 1
!     TETRA(1,2,1,3,3)= 3
!     TETRA(1,2,1,3,4)= 5
!
!     2 2 1 SPLITTING
!
!     TETRA(2,2,1,1,1)= 1
!     TETRA(2,2,1,1,2)= 2
!     TETRA(2,2,1,1,3)= 3
!     TETRA(2,2,1,1,4)= 4
!
!     TETRA(2,2,1,2,1)= 4
!     TETRA(2,2,1,2,2)= 6
!     TETRA(2,2,1,2,3)= 5
!     TETRA(2,2,1,2,4)= 3
!
!     TETRA(2,2,1,3,1)= 5
!     TETRA(2,2,1,3,2)= 2
!     TETRA(2,2,1,3,3)= 4
!     TETRA(2,2,1,3,4)= 3
!
!     1 2 2 SPLITTING
!
!     TETRA(1,2,2,1,1)= 1
!     TETRA(1,2,2,1,2)= 2
!     TETRA(1,2,2,1,3)= 3
!     TETRA(1,2,2,1,4)= 5
!
!     TETRA(1,2,2,2,1)= 4
!     TETRA(1,2,2,2,2)= 6
!     TETRA(1,2,2,2,3)= 5
!     TETRA(1,2,2,2,4)= 1
!
!     TETRA(1,2,2,3,1)= 6
!     TETRA(1,2,2,3,2)= 3
!     TETRA(1,2,2,3,3)= 5
!     TETRA(1,2,2,3,4)= 1
!
!     2 1 2 SPLITTING
!
!     TETRA(2,1,2,1,1)= 1
!     TETRA(2,1,2,1,2)= 2
!     TETRA(2,1,2,1,3)= 3
!     TETRA(2,1,2,1,4)= 6
!
!     TETRA(2,1,2,2,1)= 4
!     TETRA(2,1,2,2,2)= 6
!     TETRA(2,1,2,2,3)= 5
!     TETRA(2,1,2,2,4)= 2
!
!     TETRA(2,1,2,3,1)= 4
!     TETRA(2,1,2,3,2)= 1
!     TETRA(2,1,2,3,3)= 6
!     TETRA(2,1,2,3,4)= 2
!
!     IMPORTANT : ON EACH LAYER THE BOTTOM TETRAHEDRONS MUST BE
!                 TREATED FIRST, SO THAT IKLE SENT TO SUBROUTINE
!                 VOISIN BE THE SAME AS WITH PRISMS OR TRIANGLES
!                 FOR THE NELEM2 FIRST ELEMENTS.
!                 CONSEQUENTLY THE FIRSt 3 POINTS OF TETRAHEDRON 1
!                 ARE ALWAYS 1,2 AND 3.
!
!     TETRA : SEE EXPLANATIONS ABOVE, THE 0 CORRESPOND TO SITUATIONS
!             THAT NEVER HAPPEN (TETRA(1,1,1,... OR TETRA(2,2,2,...)
      INTEGER TETRA(2,2,2,3,4)
      DATA TETRA / 0,1,1,1,1,1,1,0,0,4,4,4,4,4,4,0,0,6,4,5,5,4,6,0,
     &             0,2,2,2,2,2,2,0,0,6,6,6,6,6,6,0,0,3,1,2,2,1,3,0,
     &             0,3,3,3,3,3,3,0,0,5,5,5,5,5,5,0,0,2,3,4,1,6,5,0,
     &             0,4,5,4,6,6,5,0,0,2,3,3,1,2,1,0,0,4,5,3,6,2,1,0 /
!
!     NUMBERING OF SEGMENTS IN A TETRAHEDRON
!     ISEGT(N,I) : POINT NUMBER I OF SEGMENT N. GIVES THE POINT IN 
!                  LOCAL NUMBERING
!
      INTEGER ISEGT(6,2)
      DATA ISEGT/1,2,3,1,2,3,2,3,1,4,4,4/
!
! 6./ ASSEMBLY MODE
!
!     1: FINITE ELEMENT ASSEMBLY IN PARALLEL DONE DIRECTLY ON
!        DOUBLE PRECISION VALUES
!
!     2: FINITE ELEMENT ASSEMBLY IN PARALLEL DONE WITH INTEGERS TO AVOID
!        TRUNCATION ERRORS IN PARALLEL, DUE TO DIFFERENT ORDER OF 
!        ADDITIONS OF MORE THAN 2 NUMBERS.
!
!     HERE INITIALISED AT 1, IN CASE NO KEYWORD IS DEDICATED TO THIS
!     PARAMETER
!
      INTEGER MODASS
      DATA MODASS/1/
!
!-----------------------------------------------------------------------
!
      END MODULE DECLARATIONS_TELEMAC
