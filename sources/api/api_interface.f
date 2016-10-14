!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief USER API FUNCTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history Y AUDOUIN (EDF R&D, LNHE)
!+       21/08/2013
!+       V6P3
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_INTERFACE
!
      USE API_HANDLE_ERROR
      USE API_HANDLE_VAR_T2D
      USE API_INSTANCE_T2D
      USE API_RUN_T2D
      USE API_HANDLE_VAR_SIS
      USE API_INSTANCE_SIS
      USE API_RUN_SIS
      USE API_COUPLING
      USE DECLARATIONS_PARTEL

      IMPLICIT NONE
      INTEGER, EXTERNAL :: GLOBAL_TO_LOCAL_POINT
!
      CONTAINS
!
!***********************************************************************
!     PARTEL/GRETEL
!***********************************************************************
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF INITIALISE THE TELEMAC2D VARIABLES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN & C. GOEURY (EDF R&D, LNHE)
      !+       24/08/2016
      !+       V6P3
      !+       PARTITIONNING TREATMENT
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_PARTEL(ID,NAMEINP, NAMECLI, NPARTS, PMETHOD,
     &  FFORMAT,NAMESEC, NAMEZFI,NAMESEU, IERR)
!
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT):: IERR
        CHARACTER(LEN=250), INTENT(IN) :: NAMEINP
        CHARACTER(LEN=250), INTENT(IN) :: NAMECLI
        INTEGER, INTENT(IN) :: NPARTS
        INTEGER, INTENT(IN) :: PMETHOD
        CHARACTER(LEN=8), INTENT(INOUT) :: FFORMAT
        CHARACTER(LEN=250), INTENT(IN) :: NAMESEC
        CHARACTER(LEN=250), INTENT(IN) :: NAMEZFI
        CHARACTER(LEN=250), INTENT(IN) :: NAMESEU
!
        INTEGER :: EXEC_POS
!
        CODE = 'T2D'
           ! The partitioning is done sequentially
            ! PARITIONING THE GEOMETRY FILE
        CALL PARTEL(NAMEINP, NAMECLI, NPARTS, PMETHOD, FFORMAT,
     &  NAMESEC, NAMEZFI, NAMESEU)
!
      END SUBROUTINE RUN_PARTEL
!
!
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF INITIALISE THE TELEMAC2D VARIABLES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY C. GOEURY (EDF R&D, LNHE)
      !+       31/08/2016
      !+       V7p1
      !+       PARTITIONNING TREATMENT
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_PARRES(ID,NAMEGEO, NAMEINP, NPARTS, GEOFORMAT,
     &     INPFORMAT,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT):: IERR
        CHARACTER(LEN=250), INTENT(IN) :: NAMEGEO
        CHARACTER(LEN=250), INTENT(IN) :: NAMEINP
        INTEGER, INTENT(IN) :: NPARTS
        CHARACTER(LEN=8), INTENT(INOUT) :: GEOFORMAT
        CHARACTER(LEN=8), INTENT(INOUT) :: INPFORMAT
!
        INTEGER :: EXEC_POS
!
        CODE = 'T2D'
           ! The partitioning is done sequentially
            ! PARITIONING THE GEOMETRY FILE
        CALL PARRES(NAMEGEO, NAMEINP, NPARTS, GEOFORMAT,INPFORMAT)
!
      END SUBROUTINE RUN_PARRES
!
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF INITIALISE THE TELEMAC2D VARIABLES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY C. GOEURY (EDF R&D, LNHE)
      !+       24/08/2016
      !+       V6P3
      !+       PARTITIONNING TREATMENT
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_GRETEL(ID,GEO,GEOFORMAT,RES,RESFORMAT,NPROC,
     &     NPLAN_RES)
!
        INTEGER,           INTENT(IN) :: ID
        CHARACTER(LEN=250), INTENT(IN) :: GEO
        CHARACTER(LEN=250), INTENT(IN) :: RES
        CHARACTER(LEN=8),   INTENT(INOUT) :: GEOFORMAT,RESFORMAT
        INTEGER,            INTENT(IN) :: NPROC
        INTEGER,            INTENT(INOUT) :: NPLAN_RES
!
        INTEGER :: EXEC_POS
!
           ! The partitioning is done sequentially
            ! PARITIONING THE GEOMETRY FILE
        CALL GRETEL_AUTOP(GEO,GEOFORMAT,RES,RESFORMAT,NPROC,NPLAN_RES)

      END SUBROUTINE RUN_GRETEL
!
!***********************************************************************
!     TELEMAC2D
!***********************************************************************
!
!
! EXECUTION FUNCTIONS
!
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
      !PARAM ID   [OUT]    ID OF THE INSTANCE
      !PARAM LU    [IN]    OUTPUT STREAM ID
      !PARAM LNG   [IN]    OUTPUT LANGUAGE 2 ENGLISH 1 FRENCH
      !PARAM COMM  [IN]    MPI COMMUNICATOR
      !PARAM IERR [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                   ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_T2D(ID,LU,LNG,COMM,IERR)
!
        INTEGER,  INTENT(OUT) :: ID
        INTEGER,  INTENT(IN) :: LU, LNG, COMM
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CREATE_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        WRITE(LU, *) 'POSITION',EXEC_POS,RUN_READ_CASE_POS
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_SET_CONFIG_T2D',
     &                      NO_POSITION,
     &                      RUN_READ_CASE_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_SET_CONFIG_POS
!
        CALL RUN_SET_CONFIG_T2D_D(INSTANCE_LIST_T2D(ID),LU,LNG,
     &                            COMM,IERR)
        IF(IERR.NE.0) RETURN
        CALL SET_VAR_LIST_T2D_D(IERR)
!
      END SUBROUTINE RUN_SET_CONFIG_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief initializes variables for TELEMAC2D in case of coupling
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history R-S MOURADI (EDF R&D, LNHE)
      !+       15/04/2016
      !+       V7P1
      !+       Creation of the file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID    [IN]    ID OF THE TELEMAC2D INSTANCE
      !PARAM IERR [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                   ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CPL_INIT(ID,IERR)
!
        INTEGER,  INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN

        CALL CPL_INIT_T2D(ID,IERR)

      END SUBROUTINE CPL_INIT
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
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM CAS_FILE   [IN]    PATH TO THE CASE FILE
      !PARAM DICO_FILE  [IN]    PATH TO THE DICTIONARY FILE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_T2D(ID,CAS_FILE, DICO_FILE, IERR)
!
          INTEGER,            INTENT(IN) :: ID
          CHARACTER(LEN=144), INTENT(IN) :: CAS_FILE
          CHARACTER(LEN=144), INTENT(IN) :: DICO_FILE
          INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_READ_CASE_T2D',
     &                      RUN_SET_CONFIG_POS,
     &                      RUN_ALLOCATION_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_READ_CASE_POS
!
        CALL RUN_READ_CASE_T2D_D(INSTANCE_LIST_T2D(ID),CAS_FILE,
     &                           DICO_FILE, IERR)
!
      END SUBROUTINE RUN_READ_CASE_T2D
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
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_T2D(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_ALLOCATION_T2D',
     &                           RUN_READ_CASE_POS,
     &                           RUN_INIT_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_ALLOCATION_POS
!
        CALL RUN_ALLOCATION_T2D_D(INSTANCE_LIST_T2D(ID),IERR)
!
      END SUBROUTINE RUN_ALLOCATION_T2D
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
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_T2D(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_INIT_T2D',
     &                           RUN_ALLOCATION_POS,
     &                           RUN_TIMESTEP_POS,IERR)
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_INIT_POS
!
        CALL RUN_INIT_T2D_D(INSTANCE_LIST_T2D(ID),IERR)
      END SUBROUTINE RUN_INIT_T2D
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
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_T2D(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_TIMESTEP_T2D',
     &          RUN_INIT_POS,RUN_FINALIZE_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_TIMESTEP_POS
!
        CALL RUN_TIMESTEP_T2D_D(INSTANCE_LIST_T2D(ID),IERR)
!
      END SUBROUTINE RUN_TIMESTEP_T2D
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
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_FINALIZE_T2D(ID,IERR)
!
        INTEGER :: EXEC_POS
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_T2D(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_FINALIZE_T2D',
     &        RUN_TIMESTEP_POS,NO_POSITION,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_T2D(ID)%MYPOSITION = RUN_FINALIZE_POS
!
        CALL RUN_FINALIZE_T2D_D(INSTANCE_LIST_T2D(ID),IERR)
        IF(IERR.NE.0) RETURN
        CALL DELETE_INSTANCE_T2D(ID,IERR)
        DEALLOCATE(VNAME_T2D)
        DEALLOCATE(VINFO_T2D)
!
      END SUBROUTINE RUN_FINALIZE_T2D
!
!  VARIABLE ACCESS FUNCTIONS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A DOUBLE VARIABLE FROM TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !HISTORY C GOEURY (EDF R&D, LNHE)
      !+       01/09/2016
      !+       V7P1
      !+       TREATMENT OF PARTITIONNING
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_T2D
     &    (ID, VARNAME, VALUE, GLOBAL_NUM, INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,                    INTENT(IN)    :: ID
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)    :: VARNAME
        DOUBLE PRECISION,           INTENT(OUT)   :: VALUE
        INTEGER,                    INTENT(INOUT) :: INDEX1
        INTEGER,                    INTENT(INOUT) :: INDEX2
        INTEGER,                    INTENT(INOUT) :: INDEX3
        LOGICAL,                    INTENT(IN)    :: GLOBAL_NUM
        INTEGER,                    INTENT(OUT)   :: IERR
!
        CHARACTER(LEN=T2D_TYPE_LEN)               :: VARTYPE
        INTEGER                                   :: READONLY
        INTEGER                                   :: NDIM
        INTEGER                                   :: IENT
        INTEGER                                   :: JENT
        INTEGER                                   :: KENT
        INTEGER                                   :: ID1
        INTEGER                                   :: ID2
        INTEGER                                   :: ID3
        DOUBLE PRECISION P_DMIN,P_DMAX
        EXTERNAL P_DMIN,P_DMAX
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        VALUE = 0
        IF(GLOBAL_NUM)THEN

          CALL GET_VAR_TYPE_T2D(VARNAME, VARTYPE, READONLY,
     &                         NDIM,IENT,JENT,KENT,IERR)
          IF(IENT.EQ.1)THEN
             ID1 = GLOBAL_TO_LOCAL_POINT(INDEX1,
     &                                   INSTANCE_LIST_T2D(ID)%MESH)
          ELSE
             ID1=INDEX1
          END IF
          IF(JENT.EQ.1)THEN
             ID2 = GLOBAL_TO_LOCAL_POINT(INDEX2,
     &                                   INSTANCE_LIST_T2D(ID)%MESH)
          ELSE
             ID2=INDEX2
          END IF
          IF(KENT.EQ.1)THEN
             ID3 = GLOBAL_TO_LOCAL_POINT(INDEX3,
     &                                   INSTANCE_LIST_T2D(ID)%MESH)
          ELSE
             ID3=INDEX3
          END IF
!
          IF((.NOT.(ID1.EQ.0.AND.ID2.EQ.0.AND.ID3.EQ.0)).OR.
     &       (INDEX1.EQ.0.AND.INDEX2.EQ.0.AND.INDEX3.EQ.0)) THEN
             CALL GET_DOUBLE_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME,
     &                             VALUE,ID1,ID2, ID3, IERR)
          END IF
          VALUE=P_DMAX(VALUE)
        ELSE
          IF(IENT.EQ.1)THEN
             ID1 = INSTANCE_LIST_T2D(ID)%MESH%KNOLG%I(INDEX1)
          ELSE
             ID1=INDEX1
          END IF
          IF(JENT.EQ.1)THEN
             ID2 = INSTANCE_LIST_T2D(ID)%MESH%KNOLG%I(INDEX2)
          ELSE
             ID2=INDEX2
          END IF
          IF(KENT.EQ.1)THEN
             ID3 = INSTANCE_LIST_T2D(ID)%MESH%KNOLG%I(INDEX3)
          ELSE
             ID3=INDEX3
          END IF
          CALL GET_DOUBLE_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALUE,
     &                         ID1, ID2, ID3, IERR)
        END IF
!
      END SUBROUTINE GET_DOUBLE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF A DOUBLE VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !HISTORY C GOEURY (EDF R&D, LNHE)
      !+       01/09/2016
      !+       V7P1
      !+       TREATMENT OF PARTITIONNING
      !!
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_T2D
     &    (ID, VARNAME, VALUE, GLOBAL_NUM, INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,                    INTENT(IN)    :: ID
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)    :: VARNAME
        DOUBLE PRECISION,           INTENT(IN)    :: VALUE
        INTEGER,                    INTENT(IN) :: INDEX1
        INTEGER,                    INTENT(IN) :: INDEX2
        INTEGER,                    INTENT(IN) :: INDEX3
        LOGICAL,                    INTENT(IN)    :: GLOBAL_NUM
        INTEGER,                    INTENT(OUT)   :: IERR
!
        CHARACTER(LEN=T2D_TYPE_LEN)               :: VARTYPE
        INTEGER                                   :: READONLY
        INTEGER                                   :: NDIM
        INTEGER                                   :: IENT
        INTEGER                                   :: JENT
        INTEGER                                   :: KENT
        INTEGER                                   :: ID1
        INTEGER                                   :: ID2
        INTEGER                                   :: ID3
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        IF(GLOBAL_NUM)THEN
          CALL GET_VAR_TYPE_T2D(VARNAME, VARTYPE, READONLY,
     &                         NDIM,IENT,JENT,KENT,IERR)
          ! TODO: Create dedcaced error message

          IF ((IENT.EQ.1).AND.(ID1.LE.0).OR.
     &        (JENT.EQ.1).AND.(ID2.LE.0).OR.
     &        (KENT.EQ.1).AND.(ID3.LE.0)) THEN
            IERR = -1
          ENDIF
          IF(IENT.EQ.1)THEN
             ID1 = GLOBAL_TO_LOCAL_POINT(INDEX1,
     &                                   INSTANCE_LIST_T2D(ID)%MESH)
          ELSE
             ID1 = INDEX1
          END IF
          IF(JENT.EQ.1)THEN
             ID2 = GLOBAL_TO_LOCAL_POINT(INDEX2,
     &                                   INSTANCE_LIST_T2D(ID)%MESH)
          ELSE
             ID2 = INDEX2
          END IF
          IF(KENT.EQ.1)THEN
             ID3 = GLOBAL_TO_LOCAL_POINT(INDEX3,
     &                                   INSTANCE_LIST_T2D(ID)%MESH)
          ELSE
             ID3=INDEX3
          END IF
!
          IF((.NOT.(ID1.EQ.0.AND.ID2.EQ.0.AND.ID3.EQ.0)).OR.
     &       (INDEX1.EQ.0.AND.INDEX2.EQ.0.AND.INDEX3.EQ.0)) THEN
             CALL SET_DOUBLE_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME,
     &                            VALUE, ID1,ID2, ID3, IERR)
          END IF
        ELSE
          IF(IENT.EQ.1)THEN
             ID1 = INSTANCE_LIST_T2D(ID)%MESH%KNOLG%I(INDEX1)
          ELSE
             ID1=INDEX1
          END IF
          IF(JENT.EQ.1)THEN
             ID2 = INSTANCE_LIST_T2D(ID)%MESH%KNOLG%I(INDEX2)
          ELSE
             ID2=INDEX2
          END IF
          IF(KENT.EQ.1)THEN
             ID3 = INSTANCE_LIST_T2D(ID)%MESH%KNOLG%I(INDEX3)
          ELSE
             ID3=INDEX3
          END IF
          CALL SET_DOUBLE_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALUE,
     &                         ID1,ID2, ID3, IERR)
        END IF
!
      END SUBROUTINE SET_DOUBLE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET AN INTEGER VARIABLE FROM TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_T2D(ID, VARNAME, VALUE,
     &             INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALUE
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL GET_INTEGER_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALUE,
     &                     INDEX1, INDEX2, INDEX3, IERR)
!
      END SUBROUTINE GET_INTEGER_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF AN INTEGER VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_T2D(ID, VARNAME, VALUE,
     &             INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUE
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL SET_INTEGER_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALUE,
     &                      INDEX1, INDEX2, INDEX3, IERR)
!
      END SUBROUTINE SET_INTEGER_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A STRING VARIABLE FROM TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_STRING_T2D(ID, VARNAME, VALUE,
     &             VALUELEN, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        CHARACTER,             INTENT(OUT) :: VALUE(VALUELEN)
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL GET_STRING_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALUE,
     &                        VALUELEN, IERR)
!
      END SUBROUTINE GET_STRING_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF A STRING VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_STRING_T2D(ID, VARNAME, VALUE,
     &             VALUELEN, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        CHARACTER,             INTENT(IN) :: VALUE(VALUELEN)
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL SET_STRING_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALUE,
     &                        VALUELEN, IERR)
!
      END SUBROUTINE SET_STRING_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A BOOLEAN VARIABLE FROM TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_BOOLEAN_T2D
     &     (ID, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
          INTEGER,               INTENT(IN) :: ID
          CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(OUT) :: VALUE
          INTEGER,               INTENT(IN) :: INDEX1
          INTEGER,               INTENT(IN) :: INDEX2
          INTEGER,               INTENT(IN) :: INDEX3
          INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL GET_BOOLEAN_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALUE,
     &                      INDEX1, INDEX2, INDEX3, IERR)
!
      END SUBROUTINE GET_BOOLEAN_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF A BOOLEAN VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_BOOLEAN_T2D
     &     (ID, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
          INTEGER,               INTENT(IN) :: ID
          CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(IN) :: VALUE
          INTEGER,               INTENT(IN) :: INDEX1
          INTEGER,               INTENT(IN) :: INDEX2
          INTEGER,               INTENT(IN) :: INDEX3
          INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL SET_BOOLEAN_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME, VALUE,
     &                      INDEX1, INDEX2, INDEX3, IERR)
!
      END SUBROUTINE SET_BOOLEAN_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET INFORMATIONS ON A VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE
      !PARAM VARTYPE   [OUT]    TYPE OF THE VARIABLE
      !+                        (INTEGER, DOUBLE, STRING, BOOLEAN)
      !PARAM READONLY  [OUT]    0 IF THE VARIABLE IS READ ONLY
      !+                        1 IF IT IS WRITTABLE
      !PARAM NDIM      [OUT]    NUMBER OF DIMENSION
      !+                        (0 IF IT IS NOT AN ARRAY)
      !PARAM IENT      [OUT]    1 if the numbering is on point
      !PARAM JENT      [OUT]    1 if the numbering is on point
      !PARAM KENT      [OUT]    1 if the numbering is on point
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_TYPE_T2D
     &        (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT, IERR)
          CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
          CHARACTER(LEN=T2D_TYPE_LEN),     INTENT(OUT) :: VARTYPE
          INTEGER,               INTENT(OUT) :: READONLY
          INTEGER,               INTENT(OUT) :: NDIM
          INTEGER,               INTENT(OUT) :: IERR
          INTEGER,               INTENT(OUT) :: IENT
          INTEGER,               INTENT(OUT) :: JENT
          INTEGER,               INTENT(OUT) :: KENT
!
        CALL GET_VAR_TYPE_T2D_D
     &        (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT, IERR)
      END SUBROUTINE GET_VAR_TYPE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET THE SIZE OF EACH DIMENSION OF A VARAIBLE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARAIBLE
      !PARAM DIM1      [OUT]    SIZE OF THE FIRST DIMENSION
      !PARAM DIM2      [OUT]    SIZE OF THE SECOND DIMENSION
      !PARAM DIM3      [OUT]    SIZE OF THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_SIZE_T2D(ID, VARNAME, DIM1, DIM2, DIM3, IERR)
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: DIM1
        INTEGER,               INTENT(OUT) :: DIM2
        INTEGER,               INTENT(OUT) :: DIM3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL GET_VAR_SIZE_T2D_D(INSTANCE_LIST_T2D(ID), VARNAME,
     &                          DIM1, DIM2, DIM3, IERR)
!
      END SUBROUTINE GET_VAR_SIZE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A DESCRIPTION OF EACH VARIABLE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)
      !+       21/08/2013
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM VARNAME   [OUT]    LIST OF ALL THE VARIABLES
      !PARAM VARINFO   [OUT]    LIST OF ALL THE DESCRIPTIONS
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_LIST_T2D(VARNAME, VARINFO, IERR)
!
        CHARACTER(LEN=T2D_VAR_LEN),  INTENT(INOUT) :: VARNAME(*)
        CHARACTER(LEN=T2D_INFO_LEN), INTENT(INOUT) :: VARINFO(*)
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER I

        IERR = 0
        DO I=1,NB_VAR_T2D
          VARNAME(I) = VNAME_T2D(I)
          VARINFO(I) = VINFO_T2D(I)
        ENDDO
!
      END SUBROUTINE
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief Returns the error message of the instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history y audouin (edf r&d, lnhe)
      !+       21/08/2013
      !+       V6P3
      !+       creation of the file
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !param id    [in]    id of the instance
      !param ierr  [in]    Error code
      !param mess  [out]   The erro message
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_ERROR_MESSAGE_T2D(ID,IERR,MESS)
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(IN) :: IERR
        CHARACTER(LEN=ERROR_MESS_LEN), INTENT(OUT) :: MESS
!
        CHARACTER(LEN=T2D_INFO_LEN) :: INST_MESS
        CHARACTER(LEN=50) :: ERR_TYPE
        INTEGER :: IERR2
!
        CALL CHECK_INSTANCE_T2D(ID,IERR2)
        IF(IERR2.NE.0) THEN
          MESS = TRIM(ERR_MESS)
        ELSE
          CALL GET_INSTANCE_ERROR_T2D(ID,INST_MESS)
          CALL GET_ERROR_TYPE(ID,INST_MESS)
          MESS = TRIM(ERR_TYPE) // '\n' // INST_MESS
        ENDIF
!
      END SUBROUTINE GET_ERROR_MESSAGE_T2D
!
!***********************************************************************
!     SISYPHE
!***********************************************************************
!
! EXECUTION FUNCTIONS
!
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
      !PARAM ID   [OUT]    ID OF THE INSTANCE
      !PARAM LU    [IN]    OUTPUT STREAM ID
      !PARAM LNG   [IN]    OUTPUT LANGUAGE 2 ENGLISH 1 FRENCH
      !PARAM COMM  [IN]    MPI COMMUNICATOR
      !PARAM IERR [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                   ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_SIS(ID,LU,LNG,COMM,IERR)
!
        INTEGER,  INTENT(OUT) :: ID
        INTEGER,  INTENT(IN) :: LU, LNG,COMM
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CREATE_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        WRITE(LU, *) 'POSITION',EXEC_POS,RUN_READ_CASE_POS
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_SET_CONFIG_SIS',
     &                      NO_POSITION,
     &                      RUN_READ_CASE_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_SET_CONFIG_POS
!
        CALL RUN_SET_CONFIG_SIS_D(INSTANCE_LIST_SIS(ID),LU,LNG,COMM,
     &                            IERR)
        CALL SET_VAR_LIST_SIS_D(IERR)
!
      END SUBROUTINE RUN_SET_CONFIG_SIS
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
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM CODE       [IN]    CODE FOR COUPLED CALL
      !PARAM CAS_FILE   [IN]    PATH TO THE CASE FILE
      !PARAM DICO_FILE  [IN]    PATH TO THE DICTIONARY FILE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_READ_CASE_SIS(ID,CODE,CAS_FILE, DICO_FILE, IERR)
!
        INTEGER,            INTENT(IN) :: ID
        CHARACTER(LEN=24),  INTENT(IN) :: CODE
        CHARACTER(LEN=144), INTENT(IN) :: CAS_FILE
        CHARACTER(LEN=144), INTENT(IN) :: DICO_FILE
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_READ_CASE_SIS',
     &                      RUN_SET_CONFIG_POS,
     &                      RUN_ALLOCATION_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_READ_CASE_POS
!
        CALL RUN_READ_CASE_SIS_D(INSTANCE_LIST_SIS(ID),CODE,CAS_FILE,
     &                           DICO_FILE, IERR)
!
      END SUBROUTINE RUN_READ_CASE_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF ALLOCATE ALL OF SISYPHE VARIABLES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI(EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_ALLOCATION_SIS(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_ALLOCATION_SIS',
     &                           RUN_READ_CASE_POS,
     &                           RUN_INIT_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_ALLOCATION_POS
!
        CALL RUN_ALLOCATION_SIS_D(INSTANCE_LIST_SIS(ID),IERR)
!
      END SUBROUTINE RUN_ALLOCATION_SIS
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
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_INIT_SIS(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_INIT_SIS',
     &                           RUN_ALLOCATION_POS,
     &                           RUN_TIMESTEP_POS,IERR)
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_INIT_POS
!
        CALL RUN_INIT_SIS_D(INSTANCE_LIST_SIS(ID),IERR)
      END SUBROUTINE RUN_INIT_SIS
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
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_TIMESTEP_SIS(ID,IERR)
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_TIMESTEP_SIS',
     &          RUN_INIT_POS,RUN_FINALIZE_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_TIMESTEP_POS
!
        CALL RUN_TIMESTEP_SIS_D(INSTANCE_LIST_SIS(ID),IERR)
!
      END SUBROUTINE RUN_TIMESTEP_SIS
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
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_FINALIZE_SIS(ID,IERR)
!
        INTEGER :: EXEC_POS
!
        INTEGER,            INTENT(IN) :: ID
        INTEGER,            INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST_SIS(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_FINALIZE_SIS',
     &        RUN_TIMESTEP_POS,NO_POSITION,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST_SIS(ID)%MYPOSITION = RUN_FINALIZE_POS
!
        CALL RUN_FINALIZE_SIS_D(INSTANCE_LIST_SIS(ID),IERR)
        IF(IERR.NE.0) RETURN
        CALL DELETE_INSTANCE_SIS(ID,IERR)
!
      END SUBROUTINE RUN_FINALIZE_SIS
!
!  VARIABLE ACCESS FUNCTIONS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A DOUBLE VARIABLE FROM SISYPHE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_SIS
     &     (ID, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(OUT) :: VALUE
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL GET_DOUBLE_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALUE,
     &                        INDEX1, INDEX2, INDEX3, IERR)
!
      END SUBROUTINE GET_DOUBLE_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF A DOUBLE VARIABLE OF SISYPHE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_SIS
     &     (ID, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(IN) :: VALUE
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL SET_DOUBLE_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALUE,
     &                        INDEX1, INDEX2, INDEX3, IERR)
!
      END SUBROUTINE SET_DOUBLE_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET AN INTEGER VARIABLE FROM SISYPHE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_SIS(ID, VARNAME, VALUE,
     &             INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALUE
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL GET_INTEGER_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALUE,
     &                     INDEX1, INDEX2, INDEX3, IERR)
!
      END SUBROUTINE GET_INTEGER_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF AN INTEGER VARIABLE OF SISYPHE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_SIS(ID, VARNAME, VALUE,
     &             INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUE
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL SET_INTEGER_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALUE,
     &                      INDEX1, INDEX2, INDEX3, IERR)
!
      END SUBROUTINE SET_INTEGER_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A STRING VARIABLE FROM SISYPHE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_STRING_SIS(ID, VARNAME, VALUE,
     &             VALUELEN, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        CHARACTER,             INTENT(OUT) :: VALUE(VALUELEN)
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL GET_STRING_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALUE,
     &                        VALUELEN, IERR)
!
      END SUBROUTINE GET_STRING_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF A STRING VARIABLE OF SISYPHE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_STRING_SIS(ID, VARNAME, VALUE,
     &             VALUELEN, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        CHARACTER,             INTENT(IN) :: VALUE(VALUELEN)
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL SET_STRING_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALUE,
     &                        VALUELEN, IERR)
!
      END SUBROUTINE SET_STRING_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A BOOLEAN VARIABLE FROM SISYPHE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_BOOLEAN_SIS
     &     (ID, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
          INTEGER,               INTENT(IN) :: ID
          CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(OUT) :: VALUE
          INTEGER,               INTENT(IN) :: INDEX1
          INTEGER,               INTENT(IN) :: INDEX2
          INTEGER,               INTENT(IN) :: INDEX3
          INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL GET_BOOLEAN_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALUE,
     &                      INDEX1, INDEX2, INDEX3, IERR)
!
      END SUBROUTINE GET_BOOLEAN_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF A BOOLEAN VARIABLE OF SISYPHE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_BOOLEAN_SIS
     &     (ID, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
          INTEGER,               INTENT(IN) :: ID
          CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(IN) :: VALUE
          INTEGER,               INTENT(IN) :: INDEX1
          INTEGER,               INTENT(IN) :: INDEX2
          INTEGER,               INTENT(IN) :: INDEX3
          INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL SET_BOOLEAN_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME, VALUE,
     &                      INDEX1, INDEX2, INDEX3, IERR)
!
      END SUBROUTINE SET_BOOLEAN_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET INFORMATIONS ON A VARIABLE OF SISYPHE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE
      !PARAM VARTYPE   [OUT]    TYPE OF THE VARIABLE
      !+                        (INTEGER, DOUBLE, STRING, BOOLEAN)
      !PARAM READONLY  [OUT]    0 IF THE VARIABLE IS READ ONLY
      !+                        1 IF IT IS WRITTABLE
      !PARAM NDIM      [OUT]    NUMBER OF DIMENSION
      !+                        (0 IF IT IS NOT AN ARRAY)
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_TYPE_SIS
     &        (VARNAME, VARTYPE, READONLY, NDIM, IERR)
          CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
          CHARACTER(LEN=SIS_TYPE_LEN),     INTENT(OUT) :: VARTYPE
          INTEGER,               INTENT(OUT) :: READONLY
          INTEGER,               INTENT(OUT) :: NDIM
          INTEGER,               INTENT(OUT) :: IERR
!
        CALL GET_VAR_TYPE_SIS_D
     &        (VARNAME, VARTYPE, READONLY, NDIM, IERR)
      END SUBROUTINE GET_VAR_TYPE_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET THE SIZE OF EACH DIMENSION OF A VARAIBLE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID         [IN]    ID OF THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARAIBLE
      !PARAM DIM1      [OUT]    SIZE OF THE FIRST DIMENSION
      !PARAM DIM2      [OUT]    SIZE OF THE SECOND DIMENSION
      !PARAM DIM3      [OUT]    SIZE OF THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_SIZE_SIS(ID, VARNAME, DIM1, DIM2, DIM3, IERR)
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: DIM1
        INTEGER,               INTENT(OUT) :: DIM2
        INTEGER,               INTENT(OUT) :: DIM3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_SIS(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL GET_VAR_SIZE_SIS_D(INSTANCE_LIST_SIS(ID), VARNAME,
     &                          DIM1, DIM2, DIM3, IERR)
!
      END SUBROUTINE GET_VAR_SIZE_SIS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET A DESCRIPTION OF EACH VARIABLE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !HISTORY R-S MOURADI (EDF R&D, LNHE)
      !+       17/03/2016
      !+       V7P1
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM VARNAME   [OUT]    LIST OF ALL THE VARIABLES
      !PARAM DICO_FILE [OUT]    LIST OF ALL THE DESCRIPTIONS
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_LIST_SIS(VARNAME, VARINFO, IERR)
!
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(OUT) :: VARNAME(NB_VAR_SIS)
        CHARACTER(LEN=SIS_INFO_LEN), INTENT(OUT) :: VARINFO(NB_VAR_SIS)
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER I

        IERR = 0
        DO I=1,NB_VAR_SIS
          VARNAME(I) = VNAME_SIS(I)
          VARINFO(I) = VINFO_SIS(I)
        ENDDO
!
      END SUBROUTINE
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief Returns the error message of the instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history R-S MOURADI (edf r&d, lnhe)
      !+       17/03/2016
      !+       V7P1
      !+       creation of the file
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !param id    [in]    id of the instance
      !param ierr  [in]    Error code
      !param mess  [out]   The erro message
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_ERROR_MESSAGE_SIS(ID,IERR,MESS)
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(IN) :: IERR
        CHARACTER(LEN=ERROR_MESS_LEN), INTENT(OUT) :: MESS
!
        CHARACTER(LEN=SIS_INFO_LEN) :: INST_MESS
        CHARACTER(LEN=50) :: ERR_TYPE
        INTEGER :: IERR2
!
        CALL CHECK_INSTANCE_SIS(ID,IERR2)
        IF(IERR2.NE.0) THEN
          MESS = TRIM(ERR_MESS)
        ELSE
          CALL GET_INSTANCE_ERROR_SIS(ID,INST_MESS)
          CALL GET_ERROR_TYPE(ID,INST_MESS)
          MESS = TRIM(ERR_TYPE) // '\n' // INST_MESS
        ENDIF
!
      END SUBROUTINE GET_ERROR_MESSAGE_SIS
!
!***********************************************************************
!     COUPLING T2D_SIS
!***********************************************************************
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief saves original charr and susp values after first sisyphe call
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history R-S MOURADI (EDF R&D, LNHE)
      !+       15/04/2016
      !+       V7P1
      !+       Creation of the file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID_T2D    [IN]    ID OF THE TELEMAC2D INSTANCE
      !PARAM ID_SIS    [IN]    ID OF THE SISYPHE INSTANCE
      !PARAM IERR     [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                   ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      SUBROUTINE SAVE_CHARR_SUSP(ID_T2D, ID_SIS,IERR)
!
        INTEGER,  INTENT(IN) :: ID_T2D
        INTEGER,  INTENT(IN) :: ID_SIS
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID_T2D,IERR)
        IF(IERR.NE.0) RETURN
        CALL CHECK_INSTANCE_SIS(ID_SIS,IERR)
        IF(IERR.NE.0) RETURN

        CALL SAVE_CHARR_SUSP_CPL(INSTANCE_LIST_SIS(ID_SIS),
     &                           INSTANCE_LIST_T2D(ID_T2D), IERR)

      END SUBROUTINE SAVE_CHARR_SUSP


      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief deals with cases : BEDLOAD OF SUSPENSION
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history R-S MOURADI (EDF R&D, LNHE)
      !+       15/04/2016
      !+       V7P1
      !+       Creation of the file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID_T2D    [IN]    ID OF THE TELEMAC2D INSTANCE
      !PARAM ID_SIS    [IN]    ID OF THE SISYPHE INSTANCE
      !param CHARR_SUSP     [OUT]    DEFINES WHICH SISYPHE CALL
      !                              = 1 Means Bedload
      !                              = 2 Means Suspension
      !                              = 3 Means Both
      !PARAM IERR [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                   ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      SUBROUTINE CHARR_OR_SUSP(ID_T2D, ID_SIS, CHARR_SUSP, IERR)
!
        INTEGER,  INTENT(IN) :: ID_T2D
        INTEGER,  INTENT(IN) :: ID_SIS
        INTEGER, INTENT(OUT) :: CHARR_SUSP
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID_T2D,IERR)
        IF(IERR.NE.0) RETURN
        CALL CHECK_INSTANCE_SIS(ID_SIS,IERR)
        IF(IERR.NE.0) RETURN

        CALL CHARR_OR_SUSP_CPL(INSTANCE_LIST_SIS(ID_SIS),
     &                         INSTANCE_LIST_T2D(ID_T2D),
     &                         CHARR_SUSP,IERR)

      END SUBROUTINE CHARR_OR_SUSP



      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief sets loop variables for sisyphe in case of coupling
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history R-S MOURADI (EDF R&D, LNHE)
      !+       15/04/2016
      !+       V7P1
      !+       Creation of the file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID_T2D              [IN]    ID OF THE TELEMAC2D INSTANCE
      !PARAM ID_SIS              [IN]    ID OF THE SISYPHE INSTANCE
      !param CALL_TYPE       [IN]    DEFINES WHICH SISYPHE CALL
      !                              = 0 Means Initializing
      !                              = 1 Means Bedload CALL
      !                              = 2 Means Suspension CALL
      !PARAM IERR           [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                             ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_SIS(ID_T2D,ID_SIS, CALL_TYPE,IERR)
!
        INTEGER,  INTENT(IN) :: ID_T2D
        INTEGER,  INTENT(IN) :: ID_SIS
        INTEGER, INTENT(IN) :: CALL_TYPE
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID_T2D,IERR)
        IF(IERR.NE.0) RETURN
        CALL CHECK_INSTANCE_SIS(ID_SIS,IERR)
        IF(IERR.NE.0) RETURN

        CALL SET_VAR_SIS_CPL(INSTANCE_LIST_T2D(ID_T2D), CALL_TYPE,
     &                                  INSTANCE_LIST_SIS(ID_SIS),IERR)

      END SUBROUTINE SET_VAR_SIS

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief sends variables to telemac2d after sisyphe call
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history R-S MOURADI (EDF R&D, LNHE)
      !+       15/04/2016
      !+       V7P1
      !+       Creation of the file
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID_T2D              [IN]    ID OF THE TELEMAC2D INSTANCE
      !PARAM ID_SIS              [IN]    ID OF THE SISYPHE INSTANCE
      !PARAM IERR           [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                             ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_T2D(ID_T2D, ID_SIS, IERR)
!
        INTEGER,  INTENT(IN) :: ID_T2D
        INTEGER,  INTENT(IN) :: ID_SIS
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        CALL CHECK_INSTANCE_T2D(ID_T2D,IERR)
        IF(IERR.NE.0) RETURN
        CALL CHECK_INSTANCE_SIS(ID_SIS,IERR)
        IF(IERR.NE.0) RETURN

        CALL SET_VAR_T2D_CPL(INSTANCE_LIST_SIS(ID_SIS),
     &                                INSTANCE_LIST_T2D(ID_T2D),IERR)

      END SUBROUTINE SET_VAR_T2D

      END MODULE API_INTERFACE
