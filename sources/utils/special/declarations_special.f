!                    ***************************
                     MODULE DECLARATIONS_SPECIAL
!                    ***************************
!
!
!***********************************************************************
! SPECIAL                                             09/05/2014
!***********************************************************************
!
!brief    Defining parameters that need to be created first because they
!+        could be used by any other library.
!
!history  Y. AUDOUIN & J-M HERVOUET (EDF LAB, LNHE)
!+        09/05/2014
!+        V7P0
!+   First version.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!----------------------------------------------------------------------
!
!     Integer values to allow the allocation of I4 and I8 integers.
!
!     The syntax will be :
!
!     INTEGER(KIND=K4) :: I (I WILL BE A 4-BYTE INTEGER)
!     INTEGER(KIND=K8) :: I (I WILL BE A 8-BYTE INTEGER)
!
!     SELECTED_INT_KIND(R):
!
!     Returns the kind type for integers N such that : -10**R<N<10**R
!
      INTEGER, PARAMETER :: K4  = 4
      INTEGER, PARAMETER :: K8  = 8
!
!     ERROR HANDLING
      INTEGER, PARAMETER :: NO_ERROR = 0
      INTEGER, PARAMETER :: UNCONTROLLED_ERR = 1
      INTEGER, PARAMETER :: UNKNOWN_ELT_TYPE_ERR = 2
!
!     ERROR FOR HERMES
      INTEGER, PARAMETER :: HERMES_RECORD_UNKNOWN_ERR = 1000
      INTEGER, PARAMETER :: HERMES_VAR_UNKNOWN_ERR = 1001
      INTEGER, PARAMETER :: HERMES_FILE_ID_ALREADY_IN_USE_ERR = 1002
      INTEGER, PARAMETER :: HERMES_FILE_NOT_OPENED_ERR = 1003
      INTEGER, PARAMETER :: HERMES_MAX_FILE_ERR = 1004
      INTEGER, PARAMETER :: HERMES_WRONG_ARRAY_SIZE_ERR = 1005
      INTEGER, PARAMETER :: HERMES_MED_NOT_LOADED_ERR = 1006
      INTEGER, PARAMETER :: HERMES_UNKNOWN_ELEMENT_TYPE_ERR = 1007
      INTEGER, PARAMETER :: HERMES_WRONG_ELEMENT_TYPE_ERR = 1008
      INTEGER, PARAMETER :: HERMES_UNKNOWN_GROUP_ERR = 1009
      INTEGER, PARAMETER :: HERMES_WRONG_HDF_FORMAT_ERR = 1010
      INTEGER, PARAMETER :: HERMES_WRONG_MED_FORMAT_ERR = 1011
      INTEGER, PARAMETER :: HERMES_WRONG_MED_VERSION_ERR = 1012
      INTEGER, PARAMETER :: HERMES_WRONG_AXE_ERR = 1013
      INTEGER, PARAMETER :: HERMES_BND_POINT_NOT_FOUND_ERR = 1014
      INTEGER, PARAMETER :: HERMES_INVALID_OPEN_MODE_ERR = 1015
      INTEGER, PARAMETER :: HERMES_INVALID_SERAFIN_FILE = 1016
!
!     TYPE OF ELEMENTS
!
      INTEGER, PARAMETER :: TYPE_NULL = -1
      INTEGER, PARAMETER :: POINT_ELT_TYPE = 0
      INTEGER, PARAMETER :: POINT_BND_ELT_TYPE = 1
      INTEGER, PARAMETER :: TRIANGLE_ELT_TYPE = 10
      INTEGER, PARAMETER :: QUADRANGLE_ELT_TYPE = 20
      INTEGER, PARAMETER :: TETRAHEDRON_ELT_TYPE = 30
      INTEGER, PARAMETER :: PRISM_ELT_TYPE = 40
      INTEGER, PARAMETER :: SPLIT_PRISM_ELT_TYPE = 50
      INTEGER, PARAMETER :: EDGE_BND_ELT_TYPE = 55
      INTEGER, PARAMETER :: TRIANGLE_BND_ELT_TYPE = 60
      INTEGER, PARAMETER :: QUADRANGLE_BND_ELT_TYPE = 70
      INTEGER, PARAMETER :: TRIANGLE_3D_BND_ELT_TYPE = 80
!
!-----------------------------------------------------------------------
!
      END MODULE DECLARATIONS_SPECIAL
