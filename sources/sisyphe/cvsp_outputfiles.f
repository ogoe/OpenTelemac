!                       ***********************
                        MODULE CVSP_OUTPUTFILES
!                       ***********************
C
C
        USE BIEF_DEF
        USE DECLARATIONS_SISYPHE, ONLY: NSICLM

        IMPLICIT NONE

        INTEGER :: OUTPUTCOUNTER             = 0
        INTEGER, PARAMETER :: NUMCP          = 30 ! NUMBER OF CHECK POINTS
        INTEGER, PARAMETER :: NUMVARCP2RES   = 3  ! NUMBER OF CHECK POINT VARIABLES TO BE WRITTEN TO FILE T2D_CP
        INTEGER, PARAMETER :: NUMVARUR2RES   = 3  ! NUMBER OF USER VARIABLES TO BE WRITTEN TO FILE T2D_UR
        INTEGER, PARAMETER :: NUMVARUR3D2RES = 13 ! NUMBER OF USER VARIABLES TO BE WRITTEN TO FILE T2D_UR3D
        INTEGER, PARAMETER :: NUMVAR2DHYD    = 6  ! 2D-HYDRAULIC TO 3D FILE Z,U,V,W,SCALARU,TAU
        INTEGER NEXTCP                            ! NEXT CP SLOT TO WRITE INTO
        INTEGER CP_ONCNT                          ! DEBUG NUMBER OF THE DEACTIVATED CHECK POINT VARIABLE

        DOUBLE PRECISION USERTIME
        DOUBLE PRECISION USERPRINTCOUNT
!
!-----------------------------------------------------------------------
!
!       1) VECTORS
!
!-----------------------------------------------------------------------
!
        TYPE(BIEF_OBJ), TARGET :: VSP_FRA(NSICLM)   
        TYPE(BIEF_OBJ), TARGET :: VSP_D, VSP_D50, VSP_ERROR
        TYPE(BIEF_OBJ), TARGET :: UR2DHYD(NUMVAR2DHYD)
!
!-----------------------------------------------------------------------
!
!       3) BLOCKS
!
!-----------------------------------------------------------------------
!
!     BLOCK OF VARIABLES FOR OPTIONAL CHECKPOINT OUTPUT
      TYPE(BIEF_OBJ), TARGET :: CPBLOC
!     BLOCK OF VARIABLES FOR OPTIONAL USER OUTPUT
      TYPE(BIEF_OBJ), TARGET :: URBLOC
!     BLOCK OF VARIABLES FOR OPTIONAL USER OUTPUT IN 3D
      TYPE(BIEF_OBJ), TARGET :: URBLOC3D
!     BLOCK OF VARIABLES FOR OPTIONAL 2D TO 3D OUTPUT
      TYPE(BIEF_OBJ), TARGET :: URBLOC2DHYD
!
!-----------------------------------------------------------------------
!
!      8)  FILES
!
!-----------------------------------------------------------------------
!
      TYPE(BIEF_FILE) :: CP_FILES(4)   ! FILES FOR OPTIONAL OUTPUT

      CHARACTER*32 UR3D_FILES_LABELS(NUMVARUR3D2RES)
      LOGICAL      UR3D_FILES_OUTVAR(NUMVARUR3D2RES)

      CHARACTER*32 UR2DHYD_FILES_LABELS(NUMVAR2DHYD)
      LOGICAL      UR2DHYD_FILES_OUTVAR(NUMVAR2DHYD)
!
!-----------------------------------------------------------------------
!
!       9) MESH STRUCTURE
!
!-----------------------------------------------------------------------
!
!     MESH FOR 3D USEROUTPUT
      TYPE(BIEF_MESH) :: USRMSH
      TYPE(BIEF_MESH) :: USRMSH_2DHYD
      INTEGER USRMSH_NPLAN               ! NUMBER OF PLANES IN T3D_UR
      INTEGER USRMSH_2DHYD_NPLAN         ! NUMBER OF PLANES FOR 2DHYD TO 3D FILE

      SAVE
!
!-----------------------------------------------------------------------
!
      END MODULE CVSP_OUTPUTFILES
