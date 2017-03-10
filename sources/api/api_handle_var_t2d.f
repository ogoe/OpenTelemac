!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief GETTER/SETTER OF TELEMAC2D VARIABLES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history Y AUDOUIN (EDF R&D, LNHE)
!+       21/08/2013
!+       V6P3
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_HANDLE_VAR_T2D

        USE API_HANDLE_ERROR
        USE API_INSTANCE_T2D
        IMPLICIT NONE
        ! Size of the string containing the name of a variable
        INTEGER, PARAMETER :: T2D_VAR_LEN=40
        ! Size of the string containing the type of a variable
        INTEGER, PARAMETER :: T2D_TYPE_LEN=10
        ! Size of the string containing the information about a variable
        INTEGER, PARAMETER :: T2D_INFO_LEN=200
        ! The maximum number of variable
!TODO: Update nb_var_t2d to real value + update all fonctions
        INTEGER, PARAMETER :: NB_VAR_T2D=33
        CHARACTER(LEN=T2D_VAR_LEN),ALLOCATABLE :: VNAME_T2D(:)
        CHARACTER(LEN=T2D_INFO_LEN),ALLOCATABLE :: VINFO_T2D(:)
!
      CONTAINS
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
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.AT') THEN
          VALUE = INST%AT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
          VALUE = INST%DEBIT(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALRANGE') THEN
          VALUE = INST%CTIDE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALVELOCITY') THEN
          VALUE = INST%CTIDEV
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SEALEVEL') THEN
          VALUE = INST%MSL
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
      !+       V6P3
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
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.AT') THEN
          INST%AT = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALRANGE') THEN
          INST%CTIDE = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALVELOCITY') THEN
          INST%CTIDEV = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SEALEVEL') THEN
          INST%MSL = VALUE
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
      !+       V6P3
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
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LT') THEN
            VALUE = INST%LT
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELMAX') THEN
            VALUE = INST%MESH%NELMAX
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
            VALUE = INST%MESH%IKLE%I((INDEX2-1)*INST%MESH%IKLE%DIM1
     &                               + INDEX1)
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.CPL_PERIOD') THEN
             VALUE = INST%SIS%PERCOU
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.BND_TIDE') THEN
             VALUE = INST%BND_TIDE(INDEX1)
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
      !+       V6P3
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
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.CPL_PERIOD') THEN
            INST%SIS%PERCOU = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LISTIN_PERIOD') THEN
            INST%SIS%LISPRD = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.GRAPH_PERIOD') THEN
            INST%SIS%LEOPRD = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LT') THEN
             INST%LT = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.BND_TIDE') THEN
             INST%BND_TIDE(INDEX1)=VALUE
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
      !+       V6P3
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
            DO J = 1,VALUELEN
              VALUE(J:J) = INST%T2D_FILES(I)%NAME(J:J)
            ENDDO
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
            I = INST%T2DCLI
            DO J = 1,VALUELEN
              VALUE(J:J) = INST%T2D_FILES(I)%NAME(J:J)
            ENDDO
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
            I = INST%T2DGEO
            DO J = 1,VALUELEN
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
      !+       V6P3
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
            write(*,*) value
            DO J=1,VALUELEN
              INST%T2D_FILES(I)%NAME(J:J) = VALUE(J)
            ENDDO
            write(*,*) INST%T2D_FILES(I)%NAME
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
      !+       V6P3
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
      !+       V6P3
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
            INST%DEBUG = 1
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
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !+HISTORY C. GOEURY (EDF R&D LNHE)
      !+        04/09/2016
      !+        V7P1
      !++=
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
          DIM1 = INST%MESH%XNEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          DIM1 = INST%MESH%YNEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
          DIM1 = INST%H%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
          DIM1 = INST%DH%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          DIM1 = INST%ZF%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
          DIM1 = INST%U%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
          DIM1 = INST%V%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          DIM1 = INST%MESH%X%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          DIM1 = INST%MESH%Y%DIM1
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
          DIM1 = SIZE(INST%COTE)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
          DIM1 = SIZE(INST%DEBIT)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUNDARIES') THEN
          DIM1 = SIZE(INST%FLUX_BOUNDARIES)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          DIM1 = SIZE(INST%CHESTR%R)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
           DIM1 = 144
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BND_TIDE')THEN
           DIM1 = SIZE(INST%BND_TIDE)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE')THEN
           DIM1 = INST%MESH%IKLE%DIM1
           DIM2 = INST%MESH%IKLE%DIM2
        ENDIF

      END SUBROUTINE GET_VAR_SIZE_T2D_D
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
      !HISTORY C GOEURY (EDF R&D, LNHE)
      !+       01/09/2016
      !+       V7P1
      !+       IENT,JENT AND KENT ADDED FOR MPI CONTROL IN GET AND SET
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARAIBLE
      !PARAM DIM1      [OUT]    SIZE OF THE FIRST DIMENSION
      !PARAM DIM2      [OUT]    SIZE OF THE SECOND DIMENSION
      !PARAM DIM3      [OUT]    SIZE OF THE THIRD DIMENSION
      !PARAM IENT      [OUT]    1 if the numbering is on point
      !PARAM JENT      [OUT]    1 if the numbering is on point
      !PARAM KENT      [OUT]    1 if the numbering is on point
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_TYPE_T2D_D
     &        (VARNAME, VARTYPE, READONLY, NDIM,IENT,JENT,KENT,IERR)
          CHARACTER(LEN=T2D_VAR_LEN),  INTENT(IN)  :: VARNAME
          CHARACTER(LEN=T2D_TYPE_LEN), INTENT(OUT) :: VARTYPE
          LOGICAL,                     INTENT(OUT) :: READONLY
          INTEGER,                     INTENT(OUT) :: NDIM
          INTEGER,                     INTENT(OUT) :: IERR
          INTEGER,                     INTENT(OUT) :: IENT
          INTEGER,                     INTENT(OUT) :: JENT
          INTEGER,                     INTENT(OUT) :: KENT
!
          IERR = 0
          VARTYPE = ''
          READONLY = .TRUE.
          NDIM = 0
          IENT = 0
          JENT = 0
          KENT = 0
!
          IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
            VARTYPE = 'INTEGER'
            READONLY = .FALSE.
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
            VARTYPE = 'INTEGER'
            READONLY = .FALSE.
            NDIM = 2
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIUBOR') THEN
            VARTYPE = 'INTEGER'
            READONLY = .FALSE.
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIVBOR') THEN
            VARTYPE = 'INTEGER'
            READONLY = .FALSE.
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LT') THEN
            VARTYPE = 'INTEGER'
            READONLY = .FALSE.
            NDIM = 0
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.INCWATERDEPTH') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYU') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.VELOCITYV') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLUX_BOUNDARIES') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
            VARTYPE =  'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
            VARTYPE = 'STRING'
            READONLY = .FALSE.
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 1
            IENT = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.AT') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 0
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.BND_TIDE') THEN
            VARTYPE = 'INTEGER'
            READONLY = .FALSE.
            NDIM = 1
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPOIN') THEN
            VARTYPE = 'INTEGER'
            READONLY = .FALSE.
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELEM') THEN
            VARTYPE = 'INTEGER'
            READONLY = .FALSE.
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALRANGE') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 0
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIDALVELOCITY') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 0
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.SEALEVEL') THEN
            VARTYPE = 'DOUBLE'
            READONLY = .FALSE.
            NDIM = 0
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
            VARTYPE = 'INTEGER'
            READONLY = .FALSE.
            NDIM = 0
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
      !+       V6P3
      !+       CREATION OF THE FILE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_LIST_T2D_D(IERR)
!
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
!
        I=0
        IERR = 0
        IF(.NOT.ALLOCATED(VNAME_T2D)) THEN
          ALLOCATE(VNAME_T2D(NB_VAR_T2D),STAT=IERR)
          IF(IERR.NE.0) RETURN
          ALLOCATE(VINFO_T2D(NB_VAR_T2D),STAT=IERR)
          IF(IERR.NE.0) RETURN
!
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NPOIN'
          VINFO_T2D(I) = 'NUMBER OF POINT IN THE MESH'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NELEM'
          VINFO_T2D(I) = 'NUMBER OF ELEMENT IN THE MESH'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LIHBOR'
          VINFO_T2D(I) = 'BOUNDARY TYPE ON H FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LIUBOR'
          VINFO_T2D(I) = 'BOUNDARY TYPE ON U FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LIVBOR'
          VINFO_T2D(I) = 'BOUNDARY TYPE ON V FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.HBOR'
          VINFO_T2D(I) = 'BOUNDARY VALUE ON H FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.UBOR'
          VINFO_T2D(I) = 'BOUNDARY VALUE ON U FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.VBOR'
          VINFO_T2D(I) = 'BOUNDARY VALUE ON V FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.XNEBOR'
          VINFO_T2D(I) = 'NORMAL X TO 1D BOUNDARY POINTS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.YNEBOR'
          VINFO_T2D(I) = 'NORMAL Y TO 1D BOUNDARY POINTS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.WATERDEPTH'
          VINFO_T2D(I) = 'DEPTH OF THE WATER'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.INCWATERDEPTH'
          VINFO_T2D(I) = 'INCREASE IN THE THE DEPTH OF THE WATER'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.BOTTOMELEVATION'
          VINFO_T2D(I) = 'LEVEL OF THE BOTTOM'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.VELOCITYU'
          VINFO_T2D(I) = 'VELOCITY ON U'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.VELOCITYV'
          VINFO_T2D(I) = 'VELOCITY ON V'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.X'
          VINFO_T2D(I) = 'X COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.Y'
          VINFO_T2D(I) = 'Y COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.FLUX_BOUNDARIES'
          VINFO_T2D(I) = 'FLUX AT BOUNDARIES'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.POROSITY'
          VINFO_T2D(I) = 'POROSITY'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.KP1BOR'
          VINFO_T2D(I) =
     &           'POINTS FOLLOWING AND PRECEDING A BOUNDARY POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NUMLIQ'
          VINFO_T2D(I) = 'LIQUID BOUNDARY NUMBERS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NBOR'
          VINFO_T2D(I) = 'GLOBAL NUMBER OF BOUNDARY POINTS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NPTFR'
          VINFO_T2D(I) = 'NUMBER OF BOUNDARY POINTS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NTIMESTEPS'
          VINFO_T2D(I) = 'NUMBER OF TIME STEPS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.NELMAX'
          VINFO_T2D(I) = 'MAXIMUM NUMBER OF ELEMENTS ENVISAGED'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.IKLE'
          VINFO_T2D(I) = 'CONNECTIVITY TABLE BETWEEN ELEMENT AND NODES'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.CHESTR'
          VINFO_T2D(I) = 'STRIKLER ON POINT'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.LT'
          VINFO_T2D(I) = 'CURRENT TIME STEP'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.AT'
          VINFO_T2D(I) = 'CURRENT TIME'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.BND_TIDE'
          VINFO_T2D(I) = 'OPTION FOR TIDAL BOUNDARY CONDITIONS'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.TIDALRANGE'
          VINFO_T2D(I) = 'COEFFICIENT TO CALIBRATE TIDAL RANGE'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.TIDALVELOCITY'
          VINFO_T2D(I) = 'COEFFICIENT TO CALIBRATE TIDAL VELOCITIES'
          I = I + 1
          VNAME_T2D(I) = 'MODEL.SEALEVEL'
          VINFO_T2D(I) = 'COEFFICIENT TO CALIBRATE SEA LEVEL'
        ENDIF
!
      END SUBROUTINE SET_VAR_LIST_T2D_D
!
      END MODULE API_HANDLE_VAR_T2D
