!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!BRIEF USER API FUNCTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!HISTORY Y AUDOUIN (EDF R&D, LNHE)
!+       21/08/2013
!+       CREATION OF THE FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_INTERFACE_T2D
!
      USE API_HANDLE_ERROR_T2D
      USE API_HANDLE_VAR_T2D
      USE API_INSTANCE_T2D
      USE API_RUN_T2D
      IMPLICIT NONE
!     
      CONTAINS
!
! EXECUTION FUNCTIONS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF INITIALISE THE INSTANCE AND SET THE OUTPUT                
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      !                                                                
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)                                
      !+       21/08/2013 
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM ID   [OUT]    ID OF THE INSTANCE
      !PARAM LU    [IN]    OUTPUT STREAM ID 
      !PARAM LNG   [IN]    OUTPUT LANGUAGE 2 ENGLISH 1 FRENCH
      !PARAM IERR [OUT]    0 IF SUBROUTINE SUCCESSFULL, 
      !+                   ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RUN_SET_CONFIG_T2D(ID,LU,LNG,IERR)    
!
        INTEGER,  INTENT(OUT) :: ID
        INTEGER,  INTENT(IN) :: LU, LNG
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: EXEC_POS
!
        CALL CREATE_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        EXEC_POS = INSTANCE_LIST(ID)%MYPOSITION
        WRITE(LU, *) 'POSITION',EXEC_POS,RUN_READ_CASE_T2D_POS
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_SET_CONFIG_T2D',
     &                      NO_POSITION,
     &                      RUN_READ_CASE_T2D_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST(ID)%MYPOSITION = RUN_SET_CONFIG_T2D_POS
!
        CALL RUN_SET_CONFIG_T2D_D(INSTANCE_LIST(ID),LU,LNG,IERR)
!
      END SUBROUTINE RUN_SET_CONFIG_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF READS THE CASE FILE 
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      !                                                                
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)                                
      !+       21/08/2013 
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
        EXEC_POS = INSTANCE_LIST(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_READ_CASE_T2D',
     &                      RUN_SET_CONFIG_T2D_POS,
     &                      RUN_ALLOCATION_T2D_POS,IERR)
        IF(IERR.NE.0) RETURN
!
        INSTANCE_LIST(ID)%MYPOSITION = RUN_READ_CASE_T2D_POS
!
        CALL RUN_READ_CASE_T2D_D(INSTANCE_LIST(ID),CAS_FILE, 
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
        EXEC_POS = INSTANCE_LIST(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_ALLOCATION_T2D',
     &                           RUN_READ_CASE_T2D_POS,
     &                           RUN_INIT_T2D_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST(ID)%MYPOSITION = RUN_ALLOCATION_T2D_POS
!
        CALL RUN_ALLOCATION_T2D_D(INSTANCE_LIST(ID),IERR)
!
      END SUBROUTINE RUN_ALLOCATION_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF INITIALISE THE TELEMAC2D VARIABLES
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      !                                                                
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)                                
      !+       21/08/2013 
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
        EXEC_POS = INSTANCE_LIST(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_INIT_T2D',
     &                           RUN_ALLOCATION_T2D_POS,
     &                           RUN_TIMESTEP_T2D_POS,IERR)
        INSTANCE_LIST(ID)%MYPOSITION = RUN_INIT_T2D_POS
!
        CALL RUN_INIT_T2D_D(INSTANCE_LIST(ID),IERR)
      END SUBROUTINE RUN_INIT_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF RUN A TIMESTEP IN TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      !                                                                
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)                                
      !+       21/08/2013 
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
        EXEC_POS = INSTANCE_LIST(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_TIMESTEP_T2D',
     &          RUN_INIT_T2D_POS,RUN_FINALIZE_T2D_POS,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST(ID)%MYPOSITION = RUN_TIMESTEP_T2D_POS
!
        CALL RUN_TIMESTEP_T2D_D(INSTANCE_LIST(ID),IERR)
!
      END SUBROUTINE RUN_TIMESTEP_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF FINALIZE A TELEMAC2D RUN
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      !                                                                
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)                                
      !+       21/08/2013 
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
        EXEC_POS = INSTANCE_LIST(ID)%MYPOSITION
        CALL CHECK_CALL_POSITION(EXEC_POS,'RUN_FINALIZE_T2D',
     &        RUN_TIMESTEP_T2D_POS,NO_POSITION,IERR)
        IF(IERR.NE.0) RETURN
        INSTANCE_LIST(ID)%MYPOSITION = RUN_FINALIZE_T2D_POS
!
        CALL RUN_FINALIZE_T2D_D(INSTANCE_LIST(ID),IERR)
        IF(IERR.NE.0) RETURN
        CALL DELETE_INSTANCE_T2D(ID,IERR)
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
      SUBROUTINE GET_DOUBLE_T2D
     &     (ID, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(OUT) :: VALUE
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!        
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL GET_DOUBLE_T2D_D(INSTANCE_LIST(ID), VARNAME, VALUE, INDEX1,
     &                     INDEX2, INDEX3, IERR)
!
      END SUBROUTINE GET_DOUBLE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF DEFINES THE VALUE OF A DOUBLE VARIABLE OF TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      !                                                                
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)                                
      !+       21/08/2013 
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
      SUBROUTINE SET_DOUBLE_T2D
     &     (ID, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
        INTEGER,               INTENT(IN) :: ID
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(IN) :: VALUE
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
!
        CALL SET_DOUBLE_T2D_D(INSTANCE_LIST(ID), VARNAME, VALUE, INDEX1,
     &                     INDEX2, INDEX3, IERR)
!
      END SUBROUTINE SET_DOUBLE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET AN INTEGER VARIABLE FROM TELEMAC2D
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      !                                                                
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)                                
      !+       21/08/2013 
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
        CALL GET_INTEGER_T2D_D(INSTANCE_LIST(ID), VARNAME, VALUE, 
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
        CALL SET_INTEGER_T2D_D(INSTANCE_LIST(ID), VARNAME, VALUE,
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
        CALL GET_STRING_T2D_D(INSTANCE_LIST(ID), VARNAME, VALUE, 
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
        CALL SET_STRING_T2D_D(INSTANCE_LIST(ID), VARNAME, VALUE, 
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
        CALL GET_BOOLEAN_T2D_D(INSTANCE_LIST(ID), VARNAME, VALUE,
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
        CALL SET_BOOLEAN_T2D_D(INSTANCE_LIST(ID), VARNAME, VALUE,
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
      SUBROUTINE GET_VAR_TYPE_T2D
     &        (VARNAME, VARTYPE, READONLY, NDIM, IERR)
          CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
          CHARACTER(LEN=T2D_TYPE_LEN),     INTENT(OUT) :: VARTYPE
          INTEGER,               INTENT(OUT) :: READONLY
          INTEGER,               INTENT(OUT) :: NDIM
          INTEGER,               INTENT(OUT) :: IERR
!
        CALL GET_VAR_TYPE_T2D_D
     &        (VARNAME, VARTYPE, READONLY, NDIM, IERR)
      END SUBROUTINE GET_VAR_TYPE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !BRIEF GET THE SIZE OF EACH DIMENSION OF A VARAIBLE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      !                                                                
      !HISTORY Y AUDOUIN (EDF R&D, LNHE)                                
      !+       21/08/2013 
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
        CALL GET_VAR_SIZE_T2D_D(INSTANCE_LIST(ID), VARNAME, 
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
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM VARNAME   [OUT]    LIST OF ALL THE VARIABLES
      !PARAM DICO_FILE [OUT]    LIST OF ALL THE DESCRIPTIONS
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL, 
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_LIST_T2D(VARNAME, VARINFO, IERR)
!
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(OUT) :: VARNAME(NB_VAR_T2D)
        CHARACTER(LEN=T2D_INFO_LEN), INTENT(OUT) :: VARINFO(NB_VAR_T2D)
        INTEGER, INTENT(OUT) :: IERR
!
        CALL GET_VAR_LIST_T2D_D(VARNAME, VARINFO, IERR)
!
      END SUBROUTINE 
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief Returns the error message of the instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      !                                                                
      !history y audouin (edf r&d, lnhe)                                
      !+       21/08/2013 
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
        CHARACTER(LEN=T2D_ERROR_MESS_LEN), INTENT(OUT) :: MESS
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
      END MODULE API_INTERFACE_T2D
