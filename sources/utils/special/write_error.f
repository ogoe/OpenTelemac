!              **********************
      CHARACTER(LEN=*) FUNCTION WRITE_ERROR
!              **********************
!
     &(IERR)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    Returns the string equivalent of the error code
!+
!
!history  Y AUDOUIN (LNHE)
!+        11/05/2015
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IERR           |-->| ERROR VALUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: IERR
!
      CHARACTER(LEN=200) :: STRING
!
!-----------------------------------------------------------------------
!
      SELECT CASE(IERR)
      CASE(UNCONTROLLED_ERR)
        STRING = 'UNCONTROLLED_ERR'
      CASE(UNKNOWN_ELT_TYPE_ERR)
        STRING = 'UNKNOWN_ELT_TYPE_ERR'
      CASE(HERMES_RECORD_UNKNOWN_ERR)
        STRING = 'HERMES_RECORD_UNKNOWN_ERR'
      CASE(HERMES_VAR_UNKNOWN_ERR)
        STRING = 'HERMES_VAR_UNKNOWN_ERR'
      CASE(HERMES_FILE_ID_ALREADY_IN_USE_ERR)
        STRING = 'HERMES_FILE_ID_ALREADY_IN_USE_ERR'
      CASE(HERMES_FILE_NOT_OPENED_ERR)
        STRING = 'HERMES_FILE_NOT_OPENED_ERR'
      CASE(HERMES_MAX_FILE_ERR)
        STRING = 'HERMES_MAX_FILE_ERR'
      CASE(HERMES_WRONG_ARRAY_SIZE_ERR)
        STRING = 'HERMES_WRONG_ARRAY_SIZE_ERR'
      CASE(HERMES_MED_NOT_LOADED_ERR)
        STRING = 'HERMES_MED_NOT_LOADED_ERR'
      CASE(HERMES_UNKNOWN_ELEMENT_TYPE_ERR)
        STRING = 'HERMES_UNKNOWN_ELEMENT_TYPE_ERR'
      CASE(HERMES_WRONG_ELEMENT_TYPE_ERR)
        STRING = 'HERMES_WRONG_ELEMENT_TYPE_ERR'
      CASE(HERMES_UNKNOWN_GROUP_ERR)
        STRING = 'HERMES_UNKNOWN_GROUP_ERR'
      CASE(HERMES_WRONG_HDF_FORMAT_ERR)
        STRING = 'HERMES_WRONG_HDF_FORMAT_ERR'
      CASE(HERMES_WRONG_MED_FORMAT_ERR)
        STRING = 'HERMES_WRONG_MED_FORMAT_ERR'
      CASE(HERMES_WRONG_MED_VERSION_ERR)
        STRING = 'HERMES_WRONG_MED_VERSION_ERR'
      CASE(HERMES_WRONG_AXE_ERR)
        STRING = 'HERMES_WRONG_AXE_ERR'
      CASE DEFAULT
        STRING = 'UNKNOWN ERROR'
      END SELECT
!
      WRITE_ERROR = STRING
!
!-----------------------------------------------------------------------
!
      END FUNCTION WRITE_ERROR
