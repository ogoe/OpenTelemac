!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief $function to control telemac2d execution
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history Y AUDOUIN (EDF R&D, LNHE)
!+       21/08/2013
!+       V6P3
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_RUN_T2D
!
      USE API_HANDLE_ERROR
      USE API_HANDLE_VAR_T2D
      USE API_INSTANCE_T2D
      USE INTERFACE_TELEMAC2D, ONLY : TELEMAC2D, LECDON_TELEMAC2D
      USE BIEF, ONLY : BIEF_OPEN_FILES, BIEF_INIT, INCLUS
      USE DECLARATIONS_PARALLEL, ONLY : COMM
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      PRIVATE
      ! COMMON VALUES TO DEFINE OUTPUT + LANGUAGE
!
      INTEGER TDEB(8),TFIN(8),NCAR,NIT_ORI,NIT_TO_DO
!
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC2D               '
      CHARACTER(LEN=24), PARAMETER :: CODE2='SISYPHE                 '
      CHARACTER(LEN=24), PARAMETER :: CODE3='TOMAWAC                 '
!
      CHARACTER(LEN=250) PATH

!
! List the public subroutines
!
      PUBLIC :: RUN_SET_CONFIG_T2D_D
      PUBLIC :: RUN_READ_CASE_T2D_D
      PUBLIC :: RUN_ALLOCATION_T2D_D
      PUBLIC :: RUN_INIT_T2D_D
      PUBLIC :: RUN_TIMESTEP_T2D_D
      PUBLIC :: RUN_FINALIZE_T2D_D

      CONTAINS
      ! SET THE LU AND LNG VALUES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF INITIALISE THE INSTANCE AND SET THE OUTPUT
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST [IN,OUT]    THE INSTANCE
      !PARAM LU       [IN]    OUTPUT STREAM ID
      !PARAM LNG      [IN]    OUTPUT KANGUAGE 2 ENGLISH 1 FRENCH
      !PARAM COMM     [IN]    THE MPI COMMUNICATOR (-1 IF NONE)
      !PARAM IERR    [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                      ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_T2D_D(INST, U_LU, U_LNG, U_COMM, IERR)
        TYPE(INSTANCE_T2D),  INTENT(INOUT) :: INST
        INTEGER,             INTENT(IN) :: U_LU, U_LNG, U_COMM
        INTEGER,             INTENT(OUT) :: IERR
!
        IERR = 0
!
        LU = U_LU
        LNG = U_LNG
        COMM = U_COMM
!
      END SUBROUTINE RUN_SET_CONFIG_T2D_D
!
!!!!!!! FUNCTION HANDLING THE EXECUTION OF THE SIMULATION
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF READS THE CASE FILE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM CAS_FILE   [IN]    PATH TO THE CASE FILE
      !PARAM DICO_FILE  [IN]    PATH TO THE DICTIONARY FILE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_T2D_D(INST,CAS_FILE, DICO_FILE,IERR)
!
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        CHARACTER(LEN=144), INTENT(IN) :: CAS_FILE
        CHARACTER(LEN=144), INTENT(IN) :: DICO_FILE
        INTEGER,            INTENT(OUT) :: IERR
!
        CHARACTER(LEN=144) MOTCAR(INST%MAXKEYWORD)
        CHARACTER(LEN=144) FILE_DESC(4,INST%MAXKEYWORD)
!
        IERR = 0
!
        CALL BIEF_INIT(CODE1,PATH,NCAR,.TRUE.)
!
!     INITIAL TIME FOR COMPUTATION DURATION
!
        CALL DATE_AND_TIME(VALUES=TDEB)
!
!     PRINTS BANNER TO LISTING
!
      CALL PRINT_HEADER(CODE1,'                        ')
!
!-----------------------------------------------------------------------
!
!     READS THE STEERING FILE
        CALL LECDON_TELEMAC2D(MOTCAR,FILE_DESC,
     &                        PATH,NCAR,CAS_FILE,DICO_FILE)
!
!-----------------------------------------------------------------------
!
!     OPENS THE FILES FOR TELEMAC2D
!
      END SUBROUTINE RUN_READ_CASE_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF ALLOCATE ALL OF TELEMAC2D VARIABLES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_T2D_D(INST,IERR)
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: IFLOT
        LOGICAL :: IS_COUPLED
!
        IERR = 0
!
        IFLOT = 0
        IS_COUPLED = INCLUS(INST%COUPLING,'SISYPHE')
     &           .OR. INCLUS(INST%COUPLING,'TOMAWAC')

        CALL BIEF_OPEN_FILES(CODE1,INST%T2D_FILES,
     &                       INST%MAXLU_T2D,
     &                       PATH,NCAR,
     &                       1,.TRUE.)
!
!-----------------------------------------------------------------------
!
!     ALLOCATES MEMORY
!
        CALL POINT_TELEMAC2D
!
!-----------------------------------------------------------------------
!
!
      END SUBROUTINE RUN_ALLOCATION_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF INITIALISE THE TELEMAC2D VARIABLES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_T2D_D(INST,IERR)
!
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        ! RUN THE INITIAL TIME STEP
        CALL TELEMAC2D(PASS=0,ATDEP=0.D0,NITER=0,CODE='       ',
     &                  NITERORI=0)
        NIT_ORI = INST%NIT
        NIT_TO_DO = NIT_ORI
!
      END SUBROUTINE RUN_INIT_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF RUN A TIMESTEP IN TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_T2D_D(INST,IERR)
!
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        IF (NIT_TO_DO.LT.0) THEN
          IERR = OVERTIME_ERROR
          ERR_MESS = 'INITIAL NUMBER OF TIMESTEP OVERREACHED'
        ENDIF
        INST%NIT = INST%LT
        CALL TELEMAC2D(PASS=1,ATDEP=0.D0,NITER=INST%LT,
     &       CODE='       ',NITERORI=NIT_ORI)
        NIT_TO_DO = NIT_TO_DO - 1
!
      END SUBROUTINE RUN_TIMESTEP_T2D_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF FINALIZE A TELEMAC2D RUN
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      SUBROUTINE RUN_FINALIZE_T2D_D(INST,IERR)
!
        TYPE(INSTANCE_T2D), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
        CALL BIEF_CLOSE_FILES(CODE1,INST%T2D_FILES,
     &                        INST%MAXLU_T2D,.FALSE.)
!
!       DEALLOCATE ALL OF BIEF AND TELEMAC2D ARRAYS
        CALL DEALL_TELEMAC2D()
!
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,///,1X,'FIN NORMALE DU PROGRAMME',///)
11      FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!     TIME OF END OF COMPUTATION
!
        CALL DATE_AND_TIME(VALUES=TFIN)
        CALL ELAPSE(TDEB,TFIN)
!
      END SUBROUTINE RUN_FINALIZE_T2D_D
!
      END MODULE API_RUN_T2D
