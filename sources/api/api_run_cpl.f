!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief $function to control coupled executions
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history R-S MOURADI (EDF R&D, LNHE)
!+       12/05/2016
!+       V7P1
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_RUN_CPL

      USE API_INTERFACE
      USE API_INSTANCE_SIS
      USE API_INSTANCE_T2D
      IMPLICIT NONE
      PRIVATE

      PUBLIC :: RUN_TIMESTEP_SIS_CPL

      CONTAINS
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF RUN SISYPHE IN CASE OF COUPLING : BEDLOAD VS SUSPENSION
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       12/05/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID_T2D   [IN]    THE INSTANCE
      !PARAM ID_SIS   [IN]    THE INSTANCE
      !PARAM IERR    [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                      ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_SIS_CPL(ID_T2D, ID_SIS, IERR)
        INTEGER,             INTENT(IN) :: ID_T2D, ID_SIS
        INTEGER,             INTENT(INOUT) :: IERR
        INTEGER CHARR_SUSP

        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID_T2D,IERR)
        IF(IERR.NE.0) RETURN
        CALL CHECK_INSTANCE_SIS(ID_SIS,IERR)
        IF(IERR.NE.0) RETURN

        CALL CHARR_OR_SUSP(ID_T2D, ID_SIS, CHARR_SUSP, IERR)
        IF(CHARR_SUSP.EQ.1.OR.CHARR_SUSP.EQ.3) THEN
           CALL SET_VAR_SIS(ID_T2D, ID_SIS, 1, IERR)
           CALL RUN_TIMESTEP_SIS(ID_SIS,IERR)
        END IF
        IF(CHARR_SUSP.EQ.2.OR.CHARR_SUSP.EQ.3) THEN
           CALL SET_VAR_SIS(ID_T2D, ID_SIS, 2, IERR)
           CALL RUN_TIMESTEP_SIS(ID_SIS,IERR)
        END IF
        CALL SET_VAR_T2D(ID_T2D, ID_SIS, IERR)

      END SUBROUTINE RUN_TIMESTEP_SIS_CPL
!
      END MODULE API_RUN_CPL
