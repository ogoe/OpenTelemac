!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief ERROR HANDLING FUNCTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history Y AUDOUIN (EDF R&D, LNHE)
!+       21/08/2013
!+       V6P3
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      MODULE API_HANDLE_ERROR

      IMPLICIT NONE
      ! STRING SIZE
      INTEGER, PARAMETER :: ERROR_LEN = 50
      INTEGER, PARAMETER :: ERROR_MESS_LEN = 250
      ! ERROR HANDLING FLAGS
      INTEGER, PARAMETER :: NO_ERROR=0
      INTEGER, PARAMETER :: UNALLOCATED_ARRAY_ERROR=1
      INTEGER, PARAMETER :: FILE_NOT_FOUND_ERROR=2
      INTEGER, PARAMETER :: CALL_POSITION_ERROR=3
      INTEGER, PARAMETER :: OVERTIME_ERROR=4
      INTEGER, PARAMETER :: MAX_INSTANCE_ERROR=5
      INTEGER, PARAMETER :: INVALID_INSTANCE_NUM_ERROR=6
      INTEGER, PARAMETER :: UNUSED_INSTANCE_ERROR=7
      INTEGER, PARAMETER :: UNKNOWN_VAR_ERROR=8
!     INTEGER, PARAMETER :: UNALLOCATED_ARRAY=5
!
      ! POSITION FOR A CALL FUNCTION
      INTEGER, PARAMETER :: NO_POSITION=0
      INTEGER, PARAMETER :: RUN_SET_CONFIG_POS=1
      INTEGER, PARAMETER :: RUN_READ_CASE_POS=2
      INTEGER, PARAMETER :: RUN_ALLOCATION_POS=3
      INTEGER, PARAMETER :: RUN_INIT_POS=4
      INTEGER, PARAMETER :: RUN_TIMESTEP_POS=5
      INTEGER, PARAMETER :: RUN_FINALIZE_POS=6
!
      CHARACTER(LEN=32) :: POS_NAME(6)
!##> SEB @ HRW: NO DATA STATEMENT FOR TYPES WITH ALLOCATABLE COMPONENTS
!      DATA POS_NAME /
      PARAMETER ( POS_NAME = (/
     &            'RUN_SET_CONFIG                  ',
     &            'RUN_READ_CASE                   ',
     &            'RUN_ALLOCATION                  ',
     &            'RUN_INIT                        ',
     &            'RUN_TIMESTEP                    ',
     &            'RUN_FINALIZE                    ' /) )
!##< SEB @ HRW
      CHARACTER(LEN=ERROR_MESS_LEN) :: ERR_MESS
!
      CONTAINS
        !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF CHECK THAT THE FLAG FOR POSTION CALL_POSITION
      !+     IS BETWEEN 'BEFORE' AND 'AFTER'
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM CALL8POSITION [IN]  ID OF THE CURRENT POSITION
      !PARAM CALL8POSITION [IN]  NAME OF THE FUNCTION
      !PARAM CALL8POSITION [IN]  ID OF THE POSTION THE FUNCTION
      !+                         MUST BE CALLED AFTER
      !PARAM CALL8POSITION [IN]  ID OF THE POSTION THE FUNCTION
      !+                         MUST BE CALLED BEFORE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CHECK_CALL_POSITION
     &     (CALL_POSITION,FNAME,PREV_POS,NEXT_POS,IERR)
        INTEGER :: CALL_POSITION
        CHARACTER(*), INTENT(IN) :: FNAME
        INTEGER, INTENT(IN) :: PREV_POS, NEXT_POS
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
!
        IF((NEXT_POS.NE.NO_POSITION).AND.
     &     (CALL_POSITION.GT.NEXT_POS)) THEN
          IERR = CALL_POSITION_ERROR
          ERR_MESS = "THE FUNCTION :"//
     &      TRIM(FNAME)// " MUST BE CALLED BEFORE "//POS_NAME(NEXT_POS)
        ENDIF
        IF((PREV_POS.NE.NO_POSITION).AND.
     &     (CALL_POSITION.LT.PREV_POS)) THEN
          IERR = CALL_POSITION_ERROR
          ERR_MESS = "THE FUNCTION :"//
     &      TRIM(FNAME)// " MUST BE CALLED AFTER "//POS_NAME(PREV_POS)
        ENDIF
      END SUBROUTINE CHECK_CALL_POSITION
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF RETURN THE ERROR MESSAGE OF THE LAST ERROR
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM IERR       [IN]    ID OF THE ERROR
      !PARAM MESSAGE   [OUT]    THE ERROR MESSAGE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_ERROR_TYPE(IERR,MESSAGE)
        INTEGER, INTENT(IN) :: IERR
        CHARACTER(*), INTENT(OUT) :: MESSAGE
!
        IF(IERR.EQ.UNALLOCATED_ARRAY_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: UNALLOCATED_ARRAY_ERROR'
        ELSE IF(IERR.EQ.FILE_NOT_FOUND_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: FILE_NOT_FOUND_ERROR'
        ELSE IF(IERR.EQ.CALL_POSITION_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: CALL_POSITION_ERROR'
        ELSE IF(IERR.EQ.MAX_INSTANCE_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: MAX_INSTANCE_ERROR'
        ELSE IF(IERR.EQ.INVALID_INSTANCE_NUM_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: INVALID_INSTANCE_NUM_ERROR'
        ELSE IF(IERR.EQ.UNUSED_INSTANCE_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: UNUSED_INSTANCE_ERROR'
        ELSE IF(IERR.EQ.UNKNOWN_VAR_ERROR) THEN
          MESSAGE = 'ERROR OF TYPE: UNKNOWN_VAR_ERROR'
        ELSE
          MESSAGE = 'ERROR OF UNKNOWN TYPE'
        ENDIF
      END SUBROUTINE GET_ERROR_TYPE
!
      END MODULE API_HANDLE_ERROR
