!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief GETTER/SETTER OF TELEMAC2D VARIABLES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history Y AUDOUIN (EDF R&D, LNHE)
!+       21/08/2013
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_HANDLE_VAR_T2D

        USE API_HANDLE_ERROR_T2D
        USE API_INSTANCE_T2D
        ! Size of the string containing the name of a variable
        INTEGER, PARAMETER :: T2D_VAR_LEN=40
        ! Size of the string containing the type of a variable
        INTEGER, PARAMETER :: T2D_TYPE_LEN=10
        ! Size of the string containing the information about a variable
        INTEGER, PARAMETER :: T2D_INFO_LEN=200
        ! The maximum number of variable
!TODO: Update nb_var_t2d to real value + update all fonctions
        INTEGER, PARAMETER :: NB_VAR_T2D=100
!
      CONTAINS
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_T2D_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_T2D),         INTENT(IN) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN) :: VARNAME
        DOUBLE PRECISION,           INTENT(OUT):: VALUE
        INTEGER,                    INTENT(IN) :: INDEX1
        INTEGER,                    INTENT(IN) :: INDEX2
        INTEGER,                    INTENT(IN) :: INDEX3
        INTEGER,                    INTENT(OUT):: IERR
!
        IERR = 0
        VALUE = 0.0
!
        IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
          VALUE = INST%HBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
          VALUE = INST%UBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
          VALUE = INST%VBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VALUE = INST%MESH%XNEBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VALUE = INST%MESH%YNEBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
          VALUE = INST%H%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
          VALUE = INST%DH%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          VALUE = INST%ZF%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
          VALUE = INST%U%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
          VALUE = INST%V%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALUE = INST%MESH%X%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALUE = INST%MESH%Y%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUNDARIES') THEN
          VALUE = INST%FLUX_BOUNDARIES(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          VALUE = INST%TE5%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
          VALUE = INST%COTE(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          VALUE = INST%CHESTR%R(INDEX1)
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_T2D_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_T2D_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_T2D),    INTENT(INOUT) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(IN) :: VALUE
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
          INST%HBOR%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
          INST%UBOR%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
          INST%VBOR%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
          INST%H%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
          INST%DH%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          INST%ZF%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
          INST%U%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
          INST%V%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUDARIES') THEN
          INST%FLUX_BOUNDARIES(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          INST%TE5%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
          INST%COTE(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
          INST%DEBIT(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          INST%CHESTR%R(INDEX1) = VALUE
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_T2D_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_T2D_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
          TYPE(INSTANCE_T2D),    INTENT(IN) :: INST
          CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(OUT) :: VALUE
          INTEGER,               INTENT(IN) :: INDEX1
          INTEGER,               INTENT(IN) :: INDEX2
          INTEGER,               INTENT(IN) :: INDEX3
          INTEGER,               INTENT(OUT) :: IERR
!
          IERR = 0
          VALUE = -1
          IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
            VALUE = INST%LIHBOR%I(INDEX1)
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
            VALUE = INST%LIUBOR%I(INDEX1)
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
            VALUE = INST%LIVBOR%I(INDEX1)
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.KP1BOR') THEN
            VALUE = INST%MESH%KP1BOR%I(INDEX1)
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NUMLIQ') THEN
            VALUE = INST%NUMLIQ%I(INDEX1)
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NBOR') THEN
            VALUE = INST%MESH%NBOR%I(INDEX1)
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPOIN') THEN
            VALUE = INST%MESH%NPOIN
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELEM') THEN
            VALUE = INST%MESH%NELEM
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTFR') THEN
            VALUE = INST%MESH%NPTFR
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
            VALUE = INST%NIT
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELMAX') THEN
            VALUE = INST%MESH%NELMAX
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
            VALUE = INST%MESH%IKLE%I(INDEX1)
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE GET_INTEGER_T2D_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_T2D_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
          TYPE(INSTANCE_T2D),    INTENT(INOUT) :: INST
          CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(IN) :: VALUE
          INTEGER,               INTENT(IN) :: INDEX1
          INTEGER,               INTENT(IN) :: INDEX2
          INTEGER,               INTENT(IN) :: INDEX3
          INTEGER,               INTENT(OUT) :: IERR
!
          IERR = 0
          IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
            INST%LIHBOR%I(INDEX1) = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
            INST%LIUBOR%I(INDEX1) = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
            INST%LIVBOR%I(INDEX1) = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
            INST%NIT = VALUE
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE SET_INTEGER_T2D_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_STRING_T2D_D
     &     (INST, VARNAME, VALUE, VALUELEN, IERR)
!
          TYPE(INSTANCE_T2D),    INTENT(IN) :: INST
          CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(IN) :: VALUELEN
          CHARACTER,             INTENT(OUT) :: VALUE(VALUELEN)
          INTEGER,               INTENT(OUT) :: IERR
!
          INTEGER I,J
!
          IERR = 0
          VALUE = ""
          IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
            I = INST%T2DRES
            DO J = 1,144
              VALUE(J:J) = INST%T2D_FILES(I)%NAME(J:J)
            ENDDO
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
            I = INST%T2DCLI
            DO J = 1,144
              VALUE(J:J) = INST%T2D_FILES(I)%NAME(J:J)
            ENDDO
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
            I = INST%T2DGEO
            DO J = 1,144
              VALUE(J:J) = INST%T2D_FILES(I)%NAME(J:J)
            ENDDO
          ELSE IF(TRIM(VARNAME).EQ.'XXX') THEN
            VALUE = ""
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE GET_STRING_T2D_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_STRING_T2D_D
     &     (INST, VARNAME, VALUE, VALUELEN, IERR)
!
          TYPE(INSTANCE_T2D),    INTENT(INOUT) :: INST
          CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(IN) :: VALUELEN
          CHARACTER,             INTENT(IN) :: VALUE(VALUELEN)
          INTEGER,               INTENT(OUT) :: IERR
!
          INTEGER I,J
!
          IERR = 0
          IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
            I = INST%T2DRES
            DO J=1,VALUELEN
              INST%T2D_FILES(I)%NAME(J:J) = VALUE(J)
            ENDDO
          ELSE IF(TRIM(VARNAME).EQ.'XXX') THEN
            IERR = 1
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE SET_STRING_T2D_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_BOOLEAN_T2D_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
          TYPE(INSTANCE_T2D),    INTENT(IN) :: INST
          CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(OUT) :: VALUE
          INTEGER,               INTENT(IN) :: INDEX1
          INTEGER,               INTENT(IN) :: INDEX2
          INTEGER,               INTENT(IN) :: INDEX3
          INTEGER,               INTENT(OUT) :: IERR
!
          IERR = 0
          VALUE = 0
          IF(TRIM(VARNAME).EQ.'MODEL.DEBUG') THEN
            VALUE = INST%DEBUG
          ELSE IF(TRIM(VARNAME).EQ.'XXXXXX') THEN
            VALUE = 0
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE GET_BOOLEAN_T2D_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_BOOLEAN_T2D_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
          TYPE(INSTANCE_T2D),    INTENT(INOUT) :: INST
          CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(IN) :: VALUE
          INTEGER,               INTENT(IN) :: INDEX1
          INTEGER,               INTENT(IN) :: INDEX2
          INTEGER,               INTENT(IN) :: INDEX3
          INTEGER,               INTENT(OUT) :: IERR
!
          IERR = 0
          IF(TRIM(VARNAME).EQ.'MODEL.DEBUG') THEN
            INST%DEBUG = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'XXXXXX') THEN
            DEBUG = 1
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE SET_BOOLEAN_T2D_D
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
      SUBROUTINE GET_VAR_SIZE_T2D_D
     &         (INST, VARNAME, DIM1, DIM2, DIM3, IERR)
        TYPE(INSTANCE_T2D),    INTENT(IN) :: INST
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: DIM1
        INTEGER,               INTENT(OUT) :: DIM2
        INTEGER,               INTENT(OUT) :: DIM3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        DIM1 = 0
        DIM2 = 0
        DIM3 = 0
!
        IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
          DIM1 = INST%HBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
          DIM1 = INST%UBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
          DIM1 = INST%VBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VALUE = INST%MESH%XNEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VALUE = INST%MESH%YNEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
          VALUE = INST%H%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
          VALUE = INST%DH%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          VALUE = INST%ZF%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
          VALUE = INST%U%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
          VALUE = INST%V%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALUE = INST%MESH%X%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALUE = INST%MESH%Y%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          DIM1 = INST%TE5%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          DIM1 = INST%LIHBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
          DIM1 = INST%LIUBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
          DIM1 = INST%LIVBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          DIM1 = SIZE(INST%TE5%R)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
          VALUE = SIZE(INST%COTE)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUNDARIES') THEN
          VALUE = SIZE(INST%FLUX_BOUNDARIES)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          DIM1 = SIZE(INST%CHESTR%R)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          DIM1 = 144
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF

      END SUBROUTINE GET_VAR_SIZE_T2D_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARAIBLE
      !PARAM DIM1      [OUT]    SIZE OF THE FIRST DIMENSION
      !PARAM DIM2      [OUT]    SIZE OF THE SECOND DIMENSION
      !PARAM DIM3      [OUT]    SIZE OF THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_TYPE_T2D_D
     &        (VARNAME, VARTYPE, READONLY, NDIM, IERR)
          CHARACTER(LEN=T2D_VAR_LEN),  INTENT(IN)  :: VARNAME
          CHARACTER(LEN=T2D_TYPE_LEN), INTENT(OUT) :: VARTYPE
          INTEGER,                     INTENT(OUT) :: READONLY
          INTEGER,                     INTENT(OUT) :: NDIM
          INTEGER,                     INTENT(OUT) :: IERR
!
          IERR = 0
          VARTYPE = ''
          READONLY = 0
          NDIM = 0
!
          IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
            VARTYPE = 'INTEGER'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
            VARTYPE = 'INTEGER'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
            VARTYPE = 'INTEGER'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUNDARIES') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
            VARTYPE = 'STRING'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = 1
            NDIM = 1
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE GET_VAR_TYPE_T2D_D
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
      SUBROUTINE GET_VAR_LIST_T2D_D(VARNAME, VARINFO, IERR)
!
        CHARACTER(LEN=T2D_VAR_LEN), INTENT(OUT) :: VARNAME(NB_VAR_T2D)
        CHARACTER(LEN=T2D_INFO_LEN), INTENT(OUT) :: VARINFO(NB_VAR_T2D)
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
!
        I=0
        IERR = 0
!
        I = I + 1
        VARNAME(I) = 'MODEL.NPOIN'
        VARINFO(I) = 'NUMBER OF POINT IN THE MESH'
        I = I + 1
        VARNAME(I) = 'MODEL.NELEM'
        VARINFO(I) = 'NUMBER OF ELEMENT IN THE MESH'
        I = I + 1
        VARNAME(I) = 'MODEL.LIHBOR'
        VARINFO(I) = 'BOUNDARY TYPE ON H FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.LIUBOR'
        VARINFO(I) = 'BOUNDARY TYPE ON U FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.LIVBOR'
        VARINFO(I) = 'BOUNDARY TYPE ON V FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.HBOR'
        VARINFO(I) = 'BOUNDARY VALUE ON H FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.UBOR'
        VARINFO(I) = 'BOUNDARY VALUE ON U FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.VBOR'
        VARINFO(I) = 'BOUNDARY VALUE ON V FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.XNEBOR'
        VARINFO(I) = ''
        I = I + 1
        VARNAME(I) = 'MODEL.YNEBOR'
        VARINFO(I) = ''
        I = I + 1
        VARNAME(I) = 'MODEL.WATERDEPTH'
        VARINFO(I) = 'DEPTH OF THE WATER'
        I = I + 1
        VARNAME(I) = 'MODEL.INCWATERDEPTH'
        VARINFO(I) = 'INCREASE IN THE THE DEPTH OF THE WATER'
        I = I + 1
        VARNAME(I) = 'MODEL.BOTTOMELEVATION'
        VARINFO(I) = 'LEVEL OF THE BOTTOM'
        I = I + 1
        VARNAME(I) = 'MODEL.VELOCITYU'
        VARINFO(I) = 'VELOCITY ON U'
        I = I + 1
        VARNAME(I) = 'MODEL.VELOCITYV'
        VARINFO(I) = 'VELOCITY ON V'
        I = I + 1
        VARNAME(I) = 'MODEL.X'
        VARINFO(I) = 'X COORDINATES FOR EACH POINT OF THE MESH'
        I = I + 1
        VARNAME(I) = 'MODEL.Y'
        VARINFO(I) = 'Y COORDINATES FOR EACH POINT OF THE MESH'
        I = I + 1
        VARNAME(I) = 'MODEL.FLUX_BOUNDARIES'
        VARINFO(I) = ''
        I = I + 1
        VARNAME(I) = 'MODEL.POROSITY'
        VARINFO(I) = 'POROSITY'
        I = I + 1
        VARNAME(I) = 'MODEL.KP1BOR'
        VARINFO(I) = ''
        I = I + 1
        VARNAME(I) = 'MODEL.NUMLIQ'
        VARINFO(I) = ''
        I = I + 1
        VARNAME(I) = 'MODEL.NBOR'
        VARINFO(I) = ''
        I = I + 1
        VARNAME(I) = 'MODEL.NPTFR'
        VARINFO(I) = 'NUMBER OF BOUNDARY POINTS'
        I = I + 1
        VARNAME(I) = 'MODEL.NTIMESTEPS'
        VARINFO(I) = 'NUMBER OF TIME STEPS'
        I = I + 1
        VARNAME(I) = 'MODEL.NELMAX'
        VARINFO(I) = ''
        I = I + 1
        VARNAME(I) = 'MODEL.IKLE'
        VARINFO(I) = 'CONNECTIVITY TABLE BETWEEN ELEMENT AND NODES'
        I = I + 1
        VARNAME(I) = 'MODEL.CHESTR'
        VARINFO(I) = 'STRIKLER ON POINT'
!
      END SUBROUTINE GET_VAR_LIST_T2D_D


!
!
      ! getter and setter for the boundaries
!
!!!!!!! getter and setter for the variable
!
!        subroutine t2d_get_velocity_u_d(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = u%r(i)
!          enddo
!        end subroutine t2d_get_velocity_u_d
!        subroutine t2d_set_velocity_u_d(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            u%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_velocity_u_d
!!
!        subroutine t2d_get_velocity_v_d(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = v%r(i)
!          enddo
!        end subroutine t2d_get_velocity_v_d
!        subroutine t2d_set_velocity_v_d(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            v%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_velocity_v_d
!!
!        subroutine t2d_get_water_depth_d(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = h%r(i)
!          enddo
!        end subroutine t2d_get_water_depth_d
!        subroutine t2d_set_water_depth_d(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            h%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_water_depth_d
!!
!        subroutine t2d_get_wave_celerity_d(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = fu%r(i)
!          enddo
!        end subroutine t2d_get_wave_celerity_d
!        subroutine t2d_set_wave_celerity_d(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            fu%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_wave_celerity_d
!!
!        subroutine t2d_get_free_surface_elevation_d(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = fv%r(i)
!          enddo
!        end subroutine t2d_get_free_surface_elevation_d
!        subroutine t2d_set_free_surface_elevation(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            fv%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_free_surface_elevation
!!
!        subroutine t2d_get_bottom_elevation(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = zf%r(i)
!          enddo
!        end subroutine t2d_get_bottom_elevation
!        subroutine t2d_set_bottom_elevation(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            zf%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_bottom_elevation
!!
!        subroutine t2d_get_froude_number(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = t2%r(i)
!          enddo
!        end subroutine t2d_get_froude_number
!        subroutine t2d_set_froude_number(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            t2%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_froude_number
!!
!        subroutine t2d_get_fluid_scalar_flowrate(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = t3%r(i)
!          enddo
!        end subroutine t2d_get_fluid_scalar_flowrate
!        subroutine t2d_set_fluid_scalar_flowrate(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            t3%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_fluid_scalar_flowrate
!!
!!       subroutine t2d_get_tracer(user_array)
!!         double precision, dimension(:), intent(out) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           user_array(i) = t%r(i)
!!         enddo
!!       end subroutine t2d_get_tracer
!!       subroutine t2d_set_tracer(user_array)
!!         double precision, dimension(:), intent(in) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           t%r(i) = user_array(i)
!!         enddo
!!       end subroutine t2d_set_tracer
!!
!!       subroutine t2d_get_kinetic_energy(user_array)
!!         double precision, dimension(:), intent(out) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           user_array(i) = k%r(i)
!!         enddo
!!       end subroutine t2d_get_kinetic_energy
!!       subroutine t2d_set_kinetic_energy(user_array)
!!         double precision, dimension(:), intent(in) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           k%r(i) = user_array(i)
!!         enddo
!!       end subroutine t2d_set_kinetic_energy
!!
!!       subroutine t2d_get_dissipation(user_array)
!!         double precision, dimension(:), intent(out) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           user_array(i) = e%r(i)
!!         enddo
!!       end subroutine t2d_get_dissipation
!!       subroutine t2d_set_dissipation(user_array)
!!         double precision, dimension(:), intent(in) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           e%r(i) = user_array(i)
!!         enddo
!!       end subroutine t2d_set_dissipation
!!
!!       subroutine t2d_get_viscosity(user_array)
!!         double precision, dimension(:), intent(out) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           user_array(i) = d%r(i)
!!         enddo
!!       end subroutine t2d_get_viscosity
!!       subroutine t2d_set_viscosity(user_array)
!!         double precision, dimension(:), intent(in) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           d%r(i) = user_array(i)
!!         enddo
!!       end subroutine t2d_set_viscosity
!!
!        subroutine t2d_get_flowrate_x(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: j
!          do j=1,mesh%npoin
!            user_array(j) = t4%r(j)
!          enddo
!        end subroutine t2d_get_flowrate_x
!        subroutine t2d_set_flowrate_x(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: j
!          do j=1,mesh%npoin
!            t4%r(j) = user_array(j)
!          enddo
!        end subroutine t2d_set_flowrate_x
!!
!        subroutine t2d_get_flowrate_y(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = t5%r(i)
!          enddo
!        end subroutine t2d_get_flowrate_y
!        subroutine t2d_set_flowrate_y(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            t5%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_flowrate_y
!!
!        subroutine t2d_get_scalar_velocity(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = t6%r(i)
!          enddo
!        end subroutine t2d_get_scalar_velocity
!        subroutine t2d_set_scalar_velocity(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            t6%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_scalar_velocity
!!
!        subroutine t2d_get_wind_x(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = windx%r(i)
!          enddo
!        end subroutine t2d_get_wind_x
!        subroutine t2d_set_wind_x(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            windx%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_wind_x
!!
!        subroutine t2d_get_wind_y(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = windy%r(i)
!          enddo
!        end subroutine t2d_get_wind_y
!        subroutine t2d_set_wind_y(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            windy%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_wind_y
!!
!        subroutine t2d_get_air_pressure(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = patmos%r(i)
!          enddo
!        end subroutine t2d_get_air_pressure
!        subroutine t2d_set_air_pressure(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            patmos%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_air_pressure
!!
!        subroutine t2d_get_friction_coeff(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = t7%r(i)
!          enddo
!        end subroutine t2d_get_friction_coeff
!        subroutine t2d_set_friction_coeff(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            t7%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_friction_coeff
!!
!!       subroutine t2d_get_drift_x(user_array)
!!         double precision, dimension(:), intent(out) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           user_array(i) = a%r(i)
!!         enddo
!!       end subroutine t2d_get_drift_x
!!       subroutine t2d_set_drift_x(user_array)
!!         double precision, dimension(:), intent(in) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           a%r(i) = user_array(i)
!!         enddo
!!       end subroutine t2d_set_drift_x
!!
!!       subroutine t2d_get_drift_y(user_array)
!!         double precision, dimension(:), intent(out) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           user_array(i) = g%r(i)
!!         enddo
!!       end subroutine t2d_get_drift_y
!!       subroutine t2d_set_drift_y(user_array)
!!         double precision, dimension(:), intent(in) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           g%r(i) = user_array(i)
!!         enddo
!!       end subroutine t2d_set_drift_y
!!
!        subroutine t2d_get_number_of_current(user_array)
!          double precision, dimension(:), intent(out) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            user_array(i) = t9%r(i)
!          enddo
!        end subroutine t2d_get_number_of_current
!        subroutine t2d_set_number_of_current(user_array)
!          double precision, dimension(:), intent(in) :: user_array
!          integer :: i
!          do i=1,mesh%npoin
!            t9%r(i) = user_array(i)
!          enddo
!        end subroutine t2d_set_number_of_current
!!
!!       subroutine t2d_get_user_var_n(user_array)
!!         double precision, dimension(:), intent(out) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           user_array(i) = n%r(i)
!!         enddo
!!       end subroutine t2d_get_user_var_n
!!       subroutine t2d_set_user_var_n(user_array)
!!         double precision, dimension(:), intent(in) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           n%r(i) = user_array(i)
!!         enddo
!!       end subroutine t2d_set_user_var_n
!!
!!       subroutine t2d_get_user_var_o(user_array)
!!         double precision, dimension(:), intent(out) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           user_array(i) = o%r(i)
!!         enddo
!!       end subroutine t2d_get_user_var_o
!!       subroutine t2d_set_user_var_o(user_array)
!!         double precision, dimension(:), intent(in) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           o%r(i) = user_array(i)
!!         enddo
!!       end subroutine t2d_set_user_var_o
!!
!!       subroutine t2d_get_user_var_r(user_array)
!!         double precision, dimension(:), intent(out) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           user_array(i) = r%r(i)
!!         enddo
!!       end subroutine t2d_get_user_var_r
!!       subroutine t2d_set_user_var_r(user_array)
!!         double precision, dimension(:), intent(in) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           r%r(i) = user_array(i)
!!         enddo
!!       end subroutine t2d_set_user_var_r
!!
!!       subroutine t2d_get_user_var_z(user_array)
!!         double precision, dimension(:), intent(out) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           user_array(i) = z%r(i)
!!         enddo
!!       end subroutine t2d_get_user_var_z
!!       subroutine t2d_set_user_var_z(user_array)
!!         double precision, dimension(:), intent(in) :: user_array
!!         integer :: i
!!         do i=1,mesh%npoin
!!           z%r(i) = user_array(i)
!!         enddo
!!       end subroutine t2d_set_user_var_z
!
      END MODULE API_HANDLE_VAR_T2D
