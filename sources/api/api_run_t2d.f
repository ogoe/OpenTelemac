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
      USE API_HANDLE_ERROR_T2D
      USE API_HANDLE_VAR_T2D
      USE API_INSTANCE_T2D
      USE INTERFACE_TELEMAC2D, ONLY : TELEMAC2D, LECDON_TELEMAC2D
      USE BIEF, ONLY : BIEF_OPEN_FILES, BIEF_INIT, INCLUS
      IMPLICIT NONE
      PRIVATE
      ! COMMON VALUES TO DEFINE OUTPUT + LANGUAGE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
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
      !PARAM IERR    [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                      ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_T2D_D(INST, U_LU, U_LNG, IERR)
        TYPE(INSTANCE_T2D),  INTENT(INOUT) :: INST
        INTEGER,             INTENT(IN) :: U_LU, U_LNG
        INTEGER,             INTENT(OUT) :: IERR
!
        IERR = 0
!
        LU = U_LU
        LNG = U_LNG
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
        CHARACTER(LEN=144) MOTCAR(INST%MAXKEY)
        CHARACTER(LEN=144) FILE_DESC(4,INST%MAXKEY)
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
        IF(LNG.EQ.1) WRITE(LU,100)
        IF(LNG.EQ.2) WRITE(LU,101)
        WRITE(LU,102)
  100   FORMAT(/////,1X,'LISTING DE TELEMAC-2D ',78('-'))
  101   FORMAT(/////,1X,'LISTING OF TELEMAC-2D ',78('-'))
  102   FORMAT(/////,
     &  14X,'   API      TELEMAC-2D       INTEROPERABLE        ',/,
     &  14X,'   BY Y AUDOUIN EDF R&D SINETICS                  ',/,
     &  14X,'   ------------------------------------------     ',/,
     &  14X,'   TTTTT  EEEEE  L      EEEEE  M   M  AAAAA  CCCCC',/,
     &  14X,'     T    E      L      E      MM MM  A   A  C    ',/,
     &  14X,'     T    EEE    L      EEE    M M M  AAAAA  C    ',/,
     &  14X,'     T    E      L      E      M   M  A   A  C    ',/,
     &  14X,'     T    EEEEE  LLLLL  EEEEE  M   M  A   A  CCCCC',/,
     &  14X,'                                                  ',/,
     &  14X,'         2D    VERSION 6.3    FORTRAN 90  01      ',/,
     &  14X,'                 WITH SEVERAL TRACERS             ',/,
!    &  14X,'           COUPLED WITH SISYPHE AND TOMAWAC       ',/,
     &  14X,/////)
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
     &                       IS_COUPLED,IFLOT,1,.TRUE.)
!
!-----------------------------------------------------------------------
!
!     ALLOCATES MEMORY
!
        CALL POINT_TELEMAC2D
!
!-----------------------------------------------------------------------
!
!     INITIALISES SISYPHE
!
!   TODO: HANDLE COUPLING WITH TOMAWAC/SISYPHE ???
!         IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
!           IF(LNG.EQ.1) WRITE(LU,103)
!           IF(LNG.EQ.2) WRITE(LU,104)
!           WRITE(LU,105)
!103        FORMAT(/////,1X,'LISTING DE SISYPHE AVEC COUPLAGE',78('-'))
!104        FORMAT(/////,1X,'LISTING OF SISYPHE WITH COUPLING',78('-'))
!105        FORMAT(/////,
!    &             14X,'    SSSS I   SSSS Y   Y PPPP  H   H EEEEE' ,/,
!    &             14X,'   S     I  S      Y Y  P   P H   H E    ' ,/,
!    &             14X,'    SSS  I   SSS    Y   PPPP  HHHHH EEEE  ',/,
!    &             14X,'       S I      S   Y   P     H   H E     ',/,
!    &             14X,'   SSSS  I  SSSS    Y   P     H   H EEEEE' ,/,
!    &             14X,'                                          ',/,
!    &             14X,'                VERSION 6.1               ',/,
!    &             14X,'      COUPLED WITH TELEMAC-2D INTERNALLY  ',/,
!    &             14X,/////)
!
!           CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,CODE1)
!
!           CALL BIEF_OPEN_FILES(CODE2,SIS_FILES,MAXLU_SIS,PATH,NCAR,
!    &              .TRUE.,IFLOT,2)
!
!     RESETS TELEMAC2D CONFIGURATION
!
!           CALL CONFIG_CODE(1)
!
!     MEMORY ORGANISATION
!
!           CALL POINT_SISYPHE
!
!         ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES TOMAWAC
!
!         IF(INCLUS(COUPLING,'TOMAWAC')) THEN
!
!            WRITE(LU,106)
!            WRITE(LU,107)
!106         FORMAT(100(1H-),////////,
!    &            16X,
!    &            'TTTTT  OOOOO  M   M  AAAAA  W   W  AAAAA  CCCCC '
!    &            ,/,16X,
!    &            '  T    O   O  MM MM  A   A  W   W  A   A  C     '
!    &            ,/,16X,
!    &            '  T    O   O  M W M  AAAAA  W W W  AAAAA  C     '
!    &            ,/,16X,
!    &            '  T    O   O  M   M  A   A  WW WW  A   A  C     '
!    &            ,/,16X,
!    &            '  T    OOOOO  M   M  A   A  W   W  A   A  CCCCC '
!    &            ,//)
!107         FORMAT(15X,
!    &            '               |    |    |                 '
!    &            ,/,15X,
!    &            '              )_)  )_)  )_) _              '
!    &            ,/,15X,
!    &            '             )___))___))___)\              '
!    &            ,/,15X,
!    &            '             )____)____)_____)\\           '
!    &            ,/,15X,
!    &            '           _____|____|____|____\\\__       '
!    &            ,/,15X,
!    &            '  ---------\               6.1  /---------  '
!    &            ,/,15X,
!    &            '    ^^^^^^^^^^^^^^^^^^^^^^^^^^^             '
!    &            ,/,15X,
!    &            '         ^^^^      ^^^^     ^^^    ^^      '
!    &            ,/,15X,
!    &            '             ^^^^      ^^^                 '
!    &            ,///)
!
!            CALL LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,CODE3)
!            CALL BIEF_OPEN_FILES(CODE3,WAC_FILES,MAXLU_WAC,PATH,NCAR,
!    &              .TRUE.,IFLOT,3)
!
!     RESETS TELEMAC2D CONFIGURATION
!
!            CALL CONFIG_CODE(1)
!
!     MEMORY ORGANISATION
!
!            CALL POINT_TOMAWAC
!
!         ENDIF
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
     &                        INST%MAXLU_T2D,.TRUE.)
!
!       IF(INCLUS(COUPLING,'SISYPHE')) THEN
!         CALL CONFIG_CODE(2)
!         CALL BIEF_CLOSE_FILES(CODE2,SIS_FILES,MAXLU_SIS,.FALSE.)
!       ENDIF
!
!       IF(INCLUS(COUPLING,'TOMAWAC')) THEN
!         CALL CONFIG_CODE(3)
!         CALL BIEF_CLOSE_FILES(CODE3,WAC_FILES,MAXLU_WAC,.FALSE.)
!       ENDIF
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
