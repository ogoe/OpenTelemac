!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief GETTER/SETTER OF SISYPHE VARIABLES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history R-S MOURADI (EDF R&D, LNHE)
!+       17/03/2016
!+       V7P1
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_HANDLE_VAR_SIS

        USE API_HANDLE_ERROR
        USE API_INSTANCE_SIS
        ! Size of the string containing the name of a variable
        INTEGER, PARAMETER :: SIS_VAR_LEN=40
        ! Size of the string containing the type of a variable
        INTEGER, PARAMETER :: SIS_TYPE_LEN=10
        ! Size of the string containing the information about a variable
        INTEGER, PARAMETER :: SIS_INFO_LEN=200
        ! The maximum number of variable
!TODO: Update nb_var_t2d to real value + update all fonctions
        INTEGER, PARAMETER :: NB_VAR_SIS=100
!
      CONTAINS
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_SIS_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),         INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN) :: VARNAME
        DOUBLE PRECISION,           INTENT(OUT):: VALUE
        INTEGER,                    INTENT(IN) :: INDEX1
        INTEGER,                    INTENT(IN) :: INDEX2
        INTEGER,                    INTENT(IN) :: INDEX3
        INTEGER,                    INTENT(OUT):: IERR
!
        IERR = 0
        VALUE = 0.0
!
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
           VALUE=INST%Q%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          VALUE = INST%E%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          VALUE = INST%Z%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          VALUE = INST%ZF%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          VALUE = INST%ZF_C%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          VALUE = INST%QBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          VALUE = INST%EBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          VALUE = INST%FLBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          VALUE = INST%FLBOR_SIS%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALUE = INST%MESH%X%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALUE = INST%MESH%Y%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VALUE = INST%MESH%XNEBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VALUE = INST%MESH%YNEBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIMESTEP') THEN
          VALUE = INST%DT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          VALUE = INST%TOB%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          VALUE = INST%CHESTR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.D50') THEN
          VALUE = INST%D50(1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MPM') THEN
          VALUE = INST%MPM
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          VALUE = INST%AC(1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          VALUE = INST%XKV
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KSPRATIO') THEN
          VALUE = INST%KSPRATIO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PHISED') THEN
          VALUE = INST%PHISED
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BETA') THEN
          VALUE = INST%BETA2
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ALPHA') THEN
          VALUE = INST%ALPHA
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_DOUBLE_SIS_D

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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_SIS_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(IN) :: VALUE
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0

!
c$$$        IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
c$$$          INST%H%R(INDEX1) = VALUE
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.velocityU') THEN
c$$$          INST%U%R(INDEX1) = VALUE
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.velocityV') THEN
c$$$          INST%V%R(INDEX1) = VALUE
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          INST%Q%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          INST%E%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          INST%Z%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          INST%ZF%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          INST%ZF_C%R(INDEX1) = VALUE

c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
c$$$          VALUE = INST%HBOR%R(INDEX1)
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.velocityU') THEN
c$$$          VALUE = INST%UBOR%R(INDEX1)
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.velocityV') THEN
c$$$          VALUE = INST%VBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          INST%QBOR%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          INST%EBOR%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          INST%FLBOR%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          INST%FLBOR_SIS%R(INDEX1) = VALUE

        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          INST%MESH%X%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          INST%MESH%Y%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          INST%MESH%XNEBOR%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          INST%MESH%YNEBOR%R(INDEX1) = VALUE


        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIMESTEP') THEN
          INST%DT = VALUE

        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          INST%TOB%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          INST%CHESTR%R(INDEX1) = VALUE
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
c$$$          INST%COTE%R(INDEX1) = VALUE
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
c$$$          INST%DEBIT%R(INDEX1) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.D50') THEN
          INST%D50(:) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MPM') THEN
          INST%MPM = VALUE
          INST%MPM_ARAY%R(:) = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          INST%AC(:) = VALUE
       ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          INST%XKV = VALUE
          INST%CSF_SABLE = 1 - VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KSPRATIO') THEN
          INST%KSPRATIO = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PHISED') THEN
          INST%PHISED = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BETA') THEN
          INST%BETA2 = VALUE
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ALPHA') THEN
          INST%ALPHA = VALUE

        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE SET_DOUBLE_SIS_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_SIS_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
          TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
          CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
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
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
           VALUE = INST%CLU%I(INDEX1)
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
           VALUE =  INST%CLV%I(INDEX1)
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
           VALUE =  INST%LIQBOR%I(INDEX1)
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
           VALUE =  INST%LIEBOR%I(INDEX1)
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NUMLIQ') THEN
           VALUE =  INST%NUMLIQ%I(INDEX1)

          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPOIN') THEN
            VALUE = INST%MESH%NPOIN
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELEM') THEN
            VALUE = INST%MESH%NELEM
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTFR') THEN
            VALUE = INST%MESH%NPTFR
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELMAX') THEN
            VALUE = INST%MESH%NELMAX
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
            VALUE = INST%MESH%IKLE%I(INDEX1)

          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
            VALUE = INST%NIT
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.CURRENTSTEP') THEN
            VALUE = INST%LT
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.CPL_PERIOD') THEN
            VALUE = INST%TEL%PERICOU
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE GET_INTEGER_SIS_D

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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_SIS_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
          TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
          CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(IN) :: VALUE
          INTEGER,               INTENT(IN) :: INDEX1
          INTEGER,               INTENT(IN) :: INDEX2
          INTEGER,               INTENT(IN) :: INDEX3
          INTEGER,               INTENT(OUT) :: IERR
!
          IERR = 0
          IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
            INST%LIHBOR%I(INDEX1) = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
            INST%CLU%I(INDEX1) = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
            INST%CLV%I(INDEX1) = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
            INST%LIQBOR%I(INDEX1) = VALUE
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
            INST%LIEBOR%I(INDEX1) = VALUE

          ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
            INST%NIT = VALUE

          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
         END SUBROUTINE SET_INTEGER_SIS_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_STRING_SIS_D
     &     (INST, VARNAME, VALUE, VALUELEN, IERR)
!
          TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
          CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(IN) :: VALUELEN
          CHARACTER,             INTENT(OUT) :: VALUE(VALUELEN)
          INTEGER,               INTENT(OUT) :: IERR
!
          INTEGER I,J
!
          IERR = 0
          VALUE = ""
          IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
            I = INST%SISRES
            DO J = 1,144
              VALUE(J:J) = INST%SIS_FILES(I)%NAME(J:J)
            ENDDO
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
            I = INST%SISCLI
            DO J = 1,144
              VALUE(J:J) = INST%SIS_FILES(I)%NAME(J:J)
            ENDDO
          ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
            I = INST%SISGEO
            DO J = 1,144
              VALUE(J:J) = INST%SIS_FILES(I)%NAME(J:J)
            ENDDO
          ELSE IF(TRIM(VARNAME).EQ.'XXX') THEN
            VALUE = ""
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE GET_STRING_SIS_D

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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_STRING_SIS_D
     &     (INST, VARNAME, VALUE, VALUELEN, IERR)
!
          TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
          CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(IN) :: VALUELEN
          CHARACTER,             INTENT(IN) :: VALUE(VALUELEN)
          INTEGER,               INTENT(OUT) :: IERR
!
          INTEGER I,J
!
          IERR = 0
          IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
            I = INST%SISRES
            DO J=1,VALUELEN
              INST%SIS_FILES(I)%NAME(J:J) = VALUE(J)
            ENDDO
          ELSE IF(TRIM(VARNAME).EQ.'XXX') THEN
            IERR = 1
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE SET_STRING_SIS_D

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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO READ
      !PARAM VALUE     [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_BOOLEAN_SIS_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
          TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
          CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(OUT) :: VALUE
          INTEGER,               INTENT(IN) :: INDEX1
          INTEGER,               INTENT(IN) :: INDEX2
          INTEGER,               INTENT(IN) :: INDEX3
          INTEGER,               INTENT(OUT) :: IERR
!
          IERR = 0
          VALUE = 0
c$$$          IF(TRIM(VARNAME).EQ.'MODEL.DEBUG') THEN
c$$$            VALUE = INST%DEBUG
          IF(TRIM(VARNAME).EQ.'XXXXXX') THEN
            VALUE = 0
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE GET_BOOLEAN_SIS_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARIABLE TO WRITE
      !PARAM VALUE      [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_BOOLEAN_SIS_D
     &     (INST, VARNAME, VALUE, INDEX1, INDEX2, INDEX3, IERR)
!
      TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
          CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
          INTEGER,               INTENT(IN) :: VALUE
          INTEGER,               INTENT(IN) :: INDEX1
          INTEGER,               INTENT(IN) :: INDEX2
          INTEGER,               INTENT(IN) :: INDEX3
          INTEGER,               INTENT(OUT) :: IERR
!
          IERR = 0
c$$$          IF(TRIM(VARNAME).EQ.'MODEL.DEBUG') THEN
c$$$            INST%DEBUG = VALUE
          IF(TRIM(VARNAME).EQ.'XXXXXX') THEN
            DEBUG = 1
          ELSE
            IERR = UNKNOWN_VAR_ERROR
            ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
          ENDIF
!
      END SUBROUTINE SET_BOOLEAN_SIS_D

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
      SUBROUTINE GET_VAR_SIZE_SIS_D
     &         (INST, VARNAME, DIM1, DIM2, DIM3, IERR)
        TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: DIM1
        INTEGER,               INTENT(OUT) :: DIM2
        INTEGER,               INTENT(OUT) :: DIM3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        DIM1 = 0
        DIM2 = 0
        DIM3 = 0
c$$$        IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
c$$$          VALUE = INST%H%DIM1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.velocityU') THEN
c$$$          VALUE = INST%U%DIM1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.velocityV') THEN
c$$$          VALUE = INST%V%DIM1
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          DIM1 = INST%Q%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          DIM1 = INST%E%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          DIM1 = INST%Z%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          DIM1 = INST%ZF%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          DIM1 = INST%ZF_C%DIM1

c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
c$$$          DIM1 = INST%HBOR%DIM1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.UBOR') THEN
c$$$          DIM1 = INST%UBOR%DIM1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.VBOR') THEN
c$$$          DIM1 = INST%VBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          DIM1 = INST%QBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          DIM1 = INST%EBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          DIM1 = INST%FLBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          DIM1 = INST%FLBOR_SIS%DIM1

        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          DIM1 = INST%MESH%X%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          DIM1 = INST%MESH%Y%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          DIM1 = INST%MESH%XNEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          DIM1 = INST%MESH%YNEBOR%DIM1

        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          DIM1 = INST%TOB%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          DIM1 = INST%CHESTR%DIM1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
c$$$          DIM1 = INST%COTE%DIM1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
c$$$          DIM1 = INST%DEBIT%DIM1

        ELSEIF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
            DIM1 = INST%LIHBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
            DIM1 = INST%CLU%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
            DIM1 = INST%CLV%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
            DIM1 = INST%LIQBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
            DIM1 = INST%LIEBOR%DIM1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
            DIM1 = 144
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_VAR_SIZE_SIS_D
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
      !PARAM INST   [IN,OUT]    THE INSTANCE
      !PARAM VARNAME    [IN]    NAME OF THE VARAIBLE
      !PARAM DIM1      [OUT]    SIZE OF THE FIRST DIMENSION
      !PARAM DIM2      [OUT]    SIZE OF THE SECOND DIMENSION
      !PARAM DIM3      [OUT]    SIZE OF THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_VAR_TYPE_SIS_D
     &        (VARNAME, VARTYPE, READONLY, NDIM, IERR)
          CHARACTER(LEN=SIS_VAR_LEN),  INTENT(IN)  :: VARNAME
          CHARACTER(LEN=SIS_TYPE_LEN), INTENT(OUT) :: VARTYPE
          INTEGER,                     INTENT(OUT) :: READONLY
          INTEGER,                     INTENT(OUT) :: NDIM
          INTEGER,                     INTENT(OUT) :: IERR
!
          IERR = 0
          VARTYPE = ''
          READONLY = 0
          NDIM = 0
!
c$$$        IF(TRIM(VARNAME).EQ.'MODEL.WATERDEPTH') THEN
c$$$          VARTYPE = 'DOUBLE'
c$$$          READONLY = 1
c$$$          NDIM = 1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.velocityU') THEN
c$$$          VARTYPE = 'DOUBLE'
c$$$          READONLY = 1
c$$$          NDIM = 1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.velocityV') THEN
c$$$          VARTYPE = 'DOUBLE'
c$$$          READONLY = 1
c$$$          NDIM = 1
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1

c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.HBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.velocityU') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.velocityV') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
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
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1


        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIMESTEP') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1

        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.COTE') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1
c$$$        ELSE IF(TRIM(VARNAME).EQ.'MODEL.DEBIT') THEN
          VARTYPE = 'DOUBLE'
          READONLY = 1
          NDIM = 1

        ELSEIF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          VARTYPE = 'INTEGER'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          VARTYPE = 'INTEGER'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = 1
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          VARTYPE = 'STRING'
          READONLY = 1
          NDIM = 1
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
      END SUBROUTINE GET_VAR_TYPE_SIS_D
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
      SUBROUTINE GET_VAR_LIST_SIS_D(VARNAME, VARINFO, IERR)
!
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(OUT) :: VARNAME(*)
        CHARACTER(LEN=SIS_INFO_LEN), INTENT(OUT) :: VARINFO(*)
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
!
        I=0
        IERR = 0
!
c$$$        I = I + 1
c$$$        VARNAME(I) = 'MODEL.WATERDEPTH'
c$$$        VARINFO(I) = 'DEPTH OF WATER'
c$$$        I = I + 1
c$$$        VARNAME(I) = 'MODEL.velocityU'
c$$$        VARINFO(I) = 'VELOCITY ON U'
c$$$        I = I + 1
c$$$        VARNAME(I) = 'MODEL.velocityV'
c$$$        VARINFO(I) = 'VELOCITY ON V'
        I = I + 1
        VARNAME(I) = 'MODEL.FLOWRATEQ'
        VARINFO(I) = 'SOLID TRANSPORT FLOWRATE'
        I = I + 1
        VARNAME(I) = 'MODEL.EVOLUTION'
        VARINFO(I) = 'EVOLUTION OF BED'
        I = I + 1
        VARNAME(I) = 'MODEL.Z'
        VARINFO(I) = 'FREE SURFACE ELEVATION'
        I = I + 1
        VARNAME(I) = 'MODEL.BOTTOMELEVATION'
        VARINFO(I) = 'LEVEL OF THE BOTTOM'
        I = I + 1
        VARNAME(I) = 'MODEL.ZF_C'
        VARINFO(I) = 'EVOLUTION DUE TO BEDLOAD'
c$$$        I = I + 1
c$$$        VARNAME(I) = 'MODEL.HBOR'
c$$$        VARINFO(I) = 'BOUNDARY VALUE ON H FOR EACH BOUNDARY POINT'
c$$$        I = I + 1
c$$$        VARNAME(I) = 'MODEL.UBOR'
c$$$        VARINFO(I) = 'BOUNDARY VALUE ON U FOR EACH BOUNDARY POINT'
c$$$        I = I + 1
c$$$        VARNAME(I) = 'MODEL.VBOR'
c$$$        VARINFO(I) = 'BOUNDARY VALUE ON V FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.QBOR'
        VARINFO(I) = 'BOUNDARY VALUE ON Q FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.EBOR'
        VARINFO(I) = 'BOUNDARY VALUE ON E FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.FLBOR'
        VARINFO(I) = 'BOUNDARY VALUE ON ZF FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.FLBOR_SIS'
        VARINFO(I) = ''
        I = I + 1
        VARNAME(I) = 'MODEL.X'
        VARINFO(I) = 'X COORDINATES FOR EACH POINT OF THE MESH'
        I = I + 1
        VARNAME(I) = 'MODEL.Y'
        VARINFO(I) = 'Y COORDINATES FOR EACH POINT OF THE MESH'
        I = I + 1
        VARNAME(I) = 'MODEL.XNEBOR'
        VARINFO(I) = ''
        I = I + 1
        VARNAME(I) = 'MODEL.YNEBOR'
        VARINFO(I) = ''
        I = I + 1
        VARNAME(I) = 'MODEL.TIMESTEP'
        VARINFO(I) = 'TIME STEP'
        I = I + 1
        VARNAME(I) = 'MODEL.TOB'
        VARINFO(I) = 'SHEAR STRESS'
        I = I + 1
        VARNAME(I) = 'MODEL.CHESTR'
        VARINFO(I) = 'STRIKLER ON POINT'
c$$$        I = I + 1
c$$$        VARNAME(I) = 'MODEL.COTE'
c$$$        VARINFO(I) = ''
c$$$        I = I + 1
c$$$        VARNAME(I) = 'MODEL.DEBIT'
c$$$        VARINFO(I) = ''
!
        I = I + 1
        VARNAME(I) = 'MODEL.LIHBOR'
        VARINFO(I) = 'BOUNDARY TYPE ON H FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.CLU'
        VARINFO(I) = 'BOUNDARY TYPE ON U FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.CLV'
        VARINFO(I) = 'BOUNDARY TYPE ON V FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.LIQBOR'
        VARINFO(I) = 'BOUNDARY TYPE ON Q FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.LIEBOR'
        VARINFO(I) = 'BOUNDARY TYPE ON E FOR EACH BOUNDARY POINT'
        I = I + 1
        VARNAME(I) = 'MODEL.NUMLIQ'
        VARINFO(I) = 'LIQUID BOUNDARY NUMBERING'
        I = I + 1
        VARNAME(I) = 'MODEL.NPOIN'
        VARINFO(I) = 'NUMBER OF POINT IN THE MESH'
        I = I + 1
        VARNAME(I) = 'MODEL.NELEM'
        VARINFO(I) = 'NUMBER OF ELEMENT IN THE MESH'
        I = I + 1
        VARNAME(I) = 'MODEL.NPFTR'
        VARINFO(I) = 'NUMBER OF BOUNDARY POINTS'
        I = I + 1
        VARNAME(I) = 'MODEL.NELMAX'
        VARINFO(I) = 'MAXIMUM NUMBER OF ELEMENTS IN THE MESH'
        I = I + 1
        VARNAME(I) = 'MODEL.IKLE'
        VARINFO(I) = 'CONNECTIVITY TABLE OF ELEMENTS AND NODES'
        I = I + 1
        VARNAME(I) = 'MODEL.NTIMESTEPS'
        VARINFO(I) = 'NUMBER OF TIME STEPS'
        I = I + 1
        VARNAME(I) = 'MODEL.CURRENTSTEP'
        VARINFO(I) = ''
        I = I + 1
        VARNAME(I) = 'MODEL.RESULTFILE'
        VARINFO(I) = 'RESULTS FILE OF THE CASE'
        I = I + 1
        VARNAME(I) = 'MODEL.BCFILE'
        VARINFO(I) = 'BOUNDARY CONDITIONS FILE OF THE CASE'
        I = I + 1
        VARNAME(I) = 'MODEL.GEOMETRYFILE'
        VARINFO(I) = 'GEOMETRY FILE OF THE CASE'
c$$$        I = I + 1
c$$$        VARNAME(I) = 'MODEL.DEBUG'
c$$$        VARINFO(I) = 'DEBUGGING ON/OFF PARAMETER'
!
      END SUBROUTINE GET_VAR_LIST_SIS_D
!
      END MODULE API_HANDLE_VAR_SIS
