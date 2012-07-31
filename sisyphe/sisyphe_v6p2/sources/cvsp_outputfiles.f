C                       ********************************************
                        MODULE CVSP_OUTPUTFILES
C                       ********************************************
C
C
        USE BIEF_DEF
        !USE DECLARATIONS_SISYPHE, only: NSICLA

        implicit none
C
C       NOTE: THIS MODULE IS ORGANISED IN 10 PARTS
C
C      01) VECTORS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C      02) MATRICES (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C      03) BLOCKS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C      04) INTEGERS
C      05) LOGICAL VALUES
C      06) REALS
C      07) STRINGS
C      08) SLVCFG STRUCTURES
C      09) MESH STRUCTURE
C      10) ALIASES
C
C
C       ALL BIEF_OBJ AND BIEF_MESH STRUCTURES ARE ALLOCATED
C       IN SUBROUTINE POINT_TELEMAC2D

        integer       :: outputcounter = 0

        INTEGER, PARAMETER  :: NumCP = 30                  ! Number of Check Points
        INTEGER, PARAMETER  :: NumVarCP2RES = 3            ! Number of Check Point Variables to be written to file T2D_CP
        INTEGER, PARAMETER  :: NumVarUR2RES = 3            ! Number of User Variables        to be written to file T2D_UR
        INTEGER, PARAMETER  :: NumVarUR3D2RES = 13 !8 !3 + NSICLA ! Number of User Variables        to be written to file T2D_UR3D
        INTEGER, PARAMETER  :: NumVar2dHYD = 6 !2d-Hydraulic to 3d file !! Z,U,V,W,ScalarU,TAU

        INTEGER NEXTCP                           ! NEXT CP slot to write into

        INTEGER CP_ONcnt          ! UHM DEBUG Number of the deactivated Check Point variable

        double precision USERTIME
        double precision USERPRINTCOUNT


C-----------------------------------------------------------------------
C
C       1) VECTORS
C
C-----------------------------------------------------------------------

C     UHM DEBUG / Delete later
        TYPE(BIEF_OBJ), TARGET :: VSP_FRA(10)   ! NSICLA!!!
        TYPE(BIEF_OBJ), TARGET :: VSP_D, VSP_D50, VSP_ERROR

        TYPE(BIEF_OBJ), TARGET :: Ur2dHYD(NumVar2dHYD)

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
!     BLOCK OF VARIABLES FOR OPTIONAL USER OUTPUT in 3D
      TYPE(BIEF_OBJ), TARGET :: URBLOC3D
!     BLOCK OF VARIABLES FOR OPTIONAL 2D to 3d Output
      TYPE(BIEF_OBJ), TARGET :: URBLOC2dHYD
!-----------------------------------------------------------------------
!
!      8)  FILES
!
!-----------------------------------------------------------------------
!
!     FILES
!
      TYPE(BIEF_FILE) :: CP_FILES(4)   ! FILES FOR OPTIONAL OUTPUT

      !CHARACTER*32 CP_FILES_LABELS(NumVarCP2RES)
      !LOGICAL      CP_FILES_OUTVAR(NumVarCP2RES)

      !CHARACTER*32 UR_FILES_LABELS(NumVarUR2RES)
      !LOGICAL      UR_FILES_OUTVAR(NumVarUR2RES)

      CHARACTER*32 UR3D_FILES_LABELS(NumVarUR3D2RES)
      LOGICAL      UR3D_FILES_OUTVAR(NumVarUR3D2RES)

      CHARACTER*32 UR2dHYD_FILES_LABELS(NumVar2dHYD)
      LOGICAL      UR2dHYD_FILES_OUTVAR(NumVar2dHYD)



!-----------------------------------------------------------------------
!
!       9) MESH STRUCTURE
!
!-----------------------------------------------------------------------
!
!
!     MESH FOR 3D USEROUTPUT
      TYPE(BIEF_MESH) :: USRMSH
      TYPE(BIEF_MESH) :: USRMSH_2dHYD
      INTEGER USRMSH_NPLAN               ! Number of PLANES IN T3D_UR
      INTEGER USRMSH_2dHYD_NPLAN         ! Number of PLANES for 2dHyd to 3d file


      SAVE


C
      END MODULE CVSP_OUTPUTFILES
