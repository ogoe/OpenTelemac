!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief $function to control sisyphe execution
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history R-S MOURADI (EDF R&D, LNHE)
!+       17/03/2016
!+       V7P1
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_RUN_SIS
!

      USE API_HANDLE_ERROR
      USE API_HANDLE_VAR_SIS
      USE API_INSTANCE_SIS
      USE INTERFACE_SISYPHE
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_PARALLEL, ONLY : COMM
      IMPLICIT NONE
      PRIVATE
      ! COMMON VALUES TO DEFINE OUTPUT + LANGUAGE
!
      INTEGER TDEB,TFIN,IFLOT,DUMINT,NCAR,NIT_ORI,NIT_TO_DO
      LOGICAL DUMLOG
!
      CHARACTER(LEN=24), PARAMETER :: CODE='SISYPHE                 '
!
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) MOTCAR(300),FILE_DESC(4,300)
!
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
!
! List the public subroutines
!
      PUBLIC :: RUN_SET_CONFIG_SIS_D
      PUBLIC :: RUN_READ_CASE_SIS_D
      PUBLIC :: RUN_ALLOCATION_SIS_D
      PUBLIC :: RUN_INIT_SIS_D
      PUBLIC :: RUN_TIMESTEP_SIS_D
      PUBLIC :: RUN_FINALIZE_SIS_D

      CONTAINS
      ! SET THE LU AND LNG VALUES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF INITIALISE THE INSTANCE AND SET THE OUTPUT
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
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
      SUBROUTINE RUN_SET_CONFIG_SIS_D(INST, U_LU, U_LNG, U_COMM, IERR)
        TYPE(INSTANCE_SIS),  INTENT(INOUT) :: INST
        INTEGER,             INTENT(IN) :: U_LU, U_LNG, U_COMM
        INTEGER,             INTENT(OUT) :: IERR
!
        IERR = 0
!
        LU = U_LU
        LNG = U_LNG
        COMM = U_COMM
!
      END SUBROUTINE RUN_SET_CONFIG_SIS_D
!
!!!!!!! FUNCTION HANDLING THE EXECUTION OF THE SIMULATION
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF READS THE CASE FILE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM CAS_FILE   [IN]    PATH TO THE CASE FILE
      !PARAM DICO_FILE  [IN]    PATH TO THE DICTIONARY FILE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_SIS_D(INST,CODE1,CAS_FILE,DICO_FILE,IERR)
!
        TYPE(INSTANCE_SIS), INTENT(INOUT) :: INST
        CHARACTER(LEN=24),  INTENT(IN) :: CODE1
        CHARACTER(LEN=144), INTENT(IN) :: CAS_FILE
        CHARACTER(LEN=144), INTENT(IN) :: DICO_FILE
        INTEGER,            INTENT(OUT) :: IERR
!
!
        IERR = 0
!
        CALL BIEF_INIT(CODE,PATH,NCAR,.TRUE.)
!
        TDEB = TIME_IN_SECONDS()
!
!  HEADING TO THE LISTING
!
        IF(LNG.EQ.1) WRITE(LU,100)
        IF(LNG.EQ.2) WRITE(LU,101)
        WRITE(LU,102)
100     FORMAT(/////,1X,'LISTING DE SISYPHE ',78('-'))
101     FORMAT(/////,1X,'LISTING OF SISYPHE ',78('-'))
102     FORMAT(/////,
     &  14X,'    SSSS I   SSSS Y   Y PPPP  H   H EEEEE',/,
     &  14X,'   S     I  S      Y Y  P   P H   H E    ',/,
     &  14X,'    SSS  I   SSS    Y   PPPP  HHHHH EEEE  ',/,
     &  14X,'       S I      S   Y   P     H   H E     ',/,
     &  14X,'   SSSS  I  SSSS    Y   P     H   H EEEEE',/,
     &  14X,'                                          ',/,
     &  14X,'                 VERSION 7.2              ',/,
     &  14X,/////)
!
!-----------------------------------------------------------------------
!
! READS THE STEERING FILE
!
        CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,CODE1,
     &  CAS_FILE,DICO_FILE)
!
      END SUBROUTINE RUN_READ_CASE_SIS_D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF ALLOCATE ALL OF SISYPHE VARIABLES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_SIS_D(INST,IERR)
        TYPE(INSTANCE_SIS), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
        INTEGER :: I
!
        DO I=1,SIZE(INST%SIS_FILES)
           INST%SIS_FILES(I)%LU=12 + I
        END DO
!
        IERR = 0
        IFLOT = 0
        CALL BIEF_OPEN_FILES(CODE,INST%SIS_FILES,INST%MAXLU_SIS,
     &                     PATH,NCAR,2,.TRUE.)
!
!-----------------------------------------------------------------------
!
! ALLOCATES VECTORS, MATRICES AND BLOCKS
!
        CALL POINT_SISYPHE
      END SUBROUTINE RUN_ALLOCATION_SIS_D

!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF INITIALISE THE SISYPHE VARIABLES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_SIS_D(INST,IERR)
!
        TYPE(INSTANCE_SIS), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0

c$$$        DUMLOG = .FALSE.
c$$$        DUMINT = 1

c$$$        CALL SISYPHE(2,0,0,0,1,T1,T1,T1,T1,T1,T1,T1,T1,T1,
c$$$     &             DUMLOG,DUMINT,DUMLOG,CODE,1,
c$$$     &             T1,T1,0.D0,T1,INST%DT,DUMLOG,DUMLOG,
c$$$     &             T1,1,T1,T1,T1,T1,T1,T1,T1,T1,API_ITER=INST%LT)

          CALL SISYPHE(2, INST%TEL%LOOPCOUNT,INST%TEL%GRAPHCOUNT,
     &           INST%TEL%LISTCOUNT, INST%TEL%NIT, INST%TEL%U,
     &           INST%TEL%V, INST%TEL%H, INST%TEL%HN,
     &           INST%TEL%HPROP, INST%TEL%ZF, INST%TEL%UETCAR,
     &           INST%TEL%CF, INST%TEL%KS, INST%TEL%SIS_CPL%CONSTFLOW,
     &           INST%TEL%SIS_CPL%NSIS_CFD,INST%TEL%SIS_CPL%SISYPHE_CFD,
     &           INST%TEL%CODE, INST%TEL%PERICOU, INST%TEL%U3D,
     &           INST%TEL%V3D, INST%TEL%T, INST%TEL%VISC, INST%TEL%DT,
     &           INST%TEL%SIS_CPL%CHARR, INST%TEL%SIS_CPL%SUSP,
     &           INST%TEL%FLBOR,INST%TEL%SOLSYS, INST%TEL%DM1,
     &           INST%TEL%UCONV,INST%TEL%VCONV, INST%TEL%ZCONV,
     &           INST%TEL%THETAW,INST%TEL%HW, INST%TEL%TW, INST%TEL%UW,
     &           API_ITER=INST%LT)

        NIT_ORI = INST%NIT !NEED TO CHECK THESE VARIABLE
        NIT_TO_DO = NIT_ORI     !NEED TO CHECK THESE VARIABLE
      END SUBROUTINE RUN_INIT_SIS_D

!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF RUN A TIMESTEP IN SISYPHE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_SIS_D(INST,IERR)
!
        TYPE(INSTANCE_SIS), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
        DOUBLE PRECISION :: TIME1
!
        IERR = 0
c$$$        DUMINT = 1
c$$$        DUMLOG = .FALSE.
!
        IF (NIT_TO_DO.LT.0) THEN
          IERR = OVERTIME_ERROR
          ERR_MESS = 'INITIAL NUMBER OF TIMESTEP OVERREACHED'
        ENDIF
        INST%LT=INST%NIT-NIT_TO_DO+1    !RESTE A TROUVER LE LT DE SISYPHE
        !TIME1 = INST%LT*INST%DT  !INUTILE SANS COUPLAGE
c$$$        CALL SISYPHE(3,0,0,0,1,T1,T1,T1,T1,T1,T1,T1,T1,
c$$$     &             T1,DUMLOG,DUMINT,DUMLOG,CODE,1,
c$$$     &             T1,T1,0.D0,T1,INST%DT,DUMLOG,DUMLOG,
c$$$  &             T1,1,T1,T1,T1,T1,T1,T1,T1,T1,API_ITER=INST%LT)

          CALL SISYPHE(3, INST%TEL%LOOPCOUNT,INST%TEL%GRAPHCOUNT,
     &           INST%TEL%LISTCOUNT, INST%TEL%NIT, INST%TEL%U,
     &           INST%TEL%V, INST%TEL%H, INST%TEL%HN,
     &           INST%TEL%HPROP, INST%TEL%ZF, INST%TEL%UETCAR,
     &           INST%TEL%CF, INST%TEL%KS, INST%TEL%SIS_CPL%CONSTFLOW,
     &           INST%TEL%SIS_CPL%NSIS_CFD,INST%TEL%SIS_CPL%SISYPHE_CFD,
     &           INST%TEL%CODE, INST%TEL%PERICOU, INST%TEL%U3D,
     &           INST%TEL%V3D, INST%TEL%T, INST%TEL%VISC, INST%TEL%DT,
     &           INST%TEL%SIS_CPL%CHARR, INST%TEL%SIS_CPL%SUSP,
     &           INST%TEL%FLBOR, INST%TEL%SOLSYS, INST%TEL%DM1,
     &           INST%TEL%UCONV, INST%TEL%VCONV, INST%TEL%ZCONV,
     &           INST%TEL%THETAW, INST%TEL%HW, INST%TEL%TW, INST%TEL%UW,
     &           API_ITER=INST%LT)

          NIT_TO_DO = NIT_TO_DO - 1
!
      END SUBROUTINE RUN_TIMESTEP_SIS_D

!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF FINALIZE A SISYPHE RUN
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      SUBROUTINE RUN_FINALIZE_SIS_D(INST,IERR)
!
        TYPE(INSTANCE_SIS), INTENT(INOUT) :: INST
        INTEGER,            INTENT(OUT) :: IERR
!
        IERR = 0
!
!-----------------------------------------------------------------------
!
        CALL BIEF_CLOSE_FILES(CODE,INST%SIS_FILES,
     &                        INST%MAXLU_SIS,.FALSE.)
!
!
!-----------------------------------------------------------------------
!
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,///,1X,'FIN NORMALE DU PROGRAMME',///)
11      FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!-----------------------------------------------------------------------
!
        TFIN = TIME_IN_SECONDS()
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'DUREE DU CALCUL : ',TFIN-TDEB,' SECONDES'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'COMPUTER TIME: ',TFIN-TDEB,' SECONDS'
        ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE RUN_FINALIZE_SIS_D
!
      END MODULE API_RUN_SIS
