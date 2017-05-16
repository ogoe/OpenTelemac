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
        IMPLICIT NONE
        ! Size of the string containing the name of a variable
        INTEGER, PARAMETER :: SIS_VAR_LEN=40
        ! Size of the string containing the type of a variable
        INTEGER, PARAMETER :: SIS_TYPE_LEN=10
        ! Size of the string containing the information about a variable
        INTEGER, PARAMETER :: SIS_INFO_LEN=200
        ! The maximum number of variable
!TODO: Update nb_var_t2d to real value + update all fonctions
        INTEGER, PARAMETER :: NB_VAR_SIS=32
        CHARACTER(LEN=SIS_VAR_LEN),ALLOCATABLE :: VNAME_SIS(:)
        CHARACTER(LEN=SIS_INFO_LEN),ALLOCATABLE :: VINFO_SIS(:)
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
      !PARAM VALEUR    [OUT]    CONTAINS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_DOUBLE_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),         INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN) :: VARNAME
        DOUBLE PRECISION,           INTENT(OUT):: VALEUR
        INTEGER,                    INTENT(IN) :: INDEX1
        INTEGER,                    INTENT(IN) :: INDEX2
        INTEGER,                    INTENT(IN) :: INDEX3
        INTEGER,                    INTENT(OUT):: IERR
!
        IERR = 0
        VALEUR = 0.0
!
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
           VALEUR=INST%Q%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          VALEUR = INST%E%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          VALEUR = INST%Z%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          VALEUR = INST%ZF%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          VALEUR = INST%ZF_C%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          VALEUR = INST%QBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          VALEUR = INST%EBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          VALEUR = INST%FLBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          VALEUR = INST%FLBOR_SIS%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VALEUR = INST%MESH%X%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VALEUR = INST%MESH%Y%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VALEUR = INST%MESH%XNEBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VALEUR = INST%MESH%YNEBOR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIMESTEP') THEN
          VALEUR = INST%DT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          VALEUR = INST%TOB%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          VALEUR = INST%CHESTR%R(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.D50') THEN
          VALEUR = INST%D50(1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MPM') THEN
          VALEUR = INST%MPM
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          VALEUR = INST%AC(1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          VALEUR = INST%XKV
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KSPRATIO') THEN
          VALEUR = INST%KSPRATIO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PHISED') THEN
          VALEUR = INST%PHISED
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BETA') THEN
          VALEUR = INST%BETA2
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ALPHA') THEN
          VALEUR = INST%ALPHA
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
      !PARAM VALEUR     [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_DOUBLE_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        DOUBLE PRECISION,      INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0

!
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          INST%Q%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          INST%E%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          INST%Z%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          INST%ZF%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          INST%ZF_C%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          INST%QBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          INST%EBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          INST%FLBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          INST%FLBOR_SIS%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          INST%MESH%X%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          INST%MESH%Y%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          INST%MESH%XNEBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          INST%MESH%YNEBOR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIMESTEP') THEN
          INST%DT = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          INST%TOB%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          INST%CHESTR%R(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.D50') THEN
          INST%D50(:) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.MPM') THEN
          INST%MPM = VALEUR
          INST%MPM_ARAY%R(:) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.SHIELDS') THEN
          INST%AC(:) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.POROSITY') THEN
          INST%XKV = VALEUR
          INST%CSF_SABLE = 1 - VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.KSPRATIO') THEN
          INST%KSPRATIO = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.PHISED') THEN
          INST%PHISED = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BETA') THEN
          INST%BETA2 = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ALPHA') THEN
          INST%ALPHA = VALEUR
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
      !PARAM VALEUR    [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INTEGER_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        VALEUR = -1
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VALEUR = INST%LIHBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          VALEUR = INST%CLU%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          VALEUR =  INST%CLV%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          VALEUR =  INST%LIQBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          VALEUR =  INST%LIEBOR%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NUMLIQ') THEN
          VALEUR =  INST%NUMLIQ%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPOIN') THEN
          VALEUR = INST%MESH%NPOIN
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELEM') THEN
          VALEUR = INST%MESH%NELEM
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NPTFR') THEN
          VALEUR = INST%MESH%NPTFR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NELMAX') THEN
          VALEUR = INST%MESH%NELMAX
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.IKLE') THEN
          VALEUR = INST%MESH%IKLE%I(INDEX1)
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          VALEUR = INST%NIT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CURRENTSTEP') THEN
          VALEUR = INST%LT
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CPL_PERIOD') THEN
          VALEUR = INST%TEL%PERICOU
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
      !PARAM VALEUR     [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_INTEGER_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          INST%LIHBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          INST%CLU%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          INST%CLV%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          INST%LIQBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          INST%LIEBOR%I(INDEX1) = VALEUR
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.NTIMESTEPS') THEN
          INST%NIT = VALEUR
        ELSE
          IERR = UNKNOWN_VAR_ERROR
          ERR_MESS = 'UNKNOWN VARIABLE NAME : '//TRIM(VARNAME)
        ENDIF
!
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
      !PARAM VALEUR    [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_STRING_SIS_D
     &     (INST, VARNAME, VALEUR, VALUELEN, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        CHARACTER,             INTENT(OUT) :: VALEUR(VALUELEN)
        INTEGER,               INTENT(OUT) :: IERR
!
        INTEGER I,J
!
        IERR = 0
        VALEUR = ""
        IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          I = INST%SISRES
          DO J = 1,144
            VALEUR(J:J) = INST%SIS_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BCFILE') THEN
          I = INST%SISCLI
          DO J = 1,144
            VALEUR(J:J) = INST%SIS_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.GEOMETRYFILE') THEN
          I = INST%SISGEO
          DO J = 1,144
            VALEUR(J:J) = INST%SIS_FILES(I)%NAME(J:J)
          ENDDO
        ELSE IF(TRIM(VARNAME).EQ.'XXX') THEN
          VALEUR = ""
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
      !PARAM VALEUR     [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_STRING_SIS_D
     &     (INST, VARNAME, VALEUR, VALUELEN, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALUELEN
        CHARACTER,             INTENT(IN) :: VALEUR(VALUELEN)
        INTEGER,               INTENT(OUT) :: IERR
!
        INTEGER I,J
!
        IERR = 0
        IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          I = INST%SISRES
          DO J=1,VALUELEN
            INST%SIS_FILES(I)%NAME(J:J) = VALEUR(J)
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
      !PARAM VALEUR    [OUT]    CONTAINIS THE READ VALUE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_BOOLEAN_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(IN) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(OUT) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        IERR = 0
        VALEUR = 0
        IF(TRIM(VARNAME).EQ.'XXXXXX') THEN
          VALEUR = 0
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
      !PARAM VALEUR     [IN]    THE VALUE TO WRITE IN THE VARIABLE
      !PARAM INDEX1     [IN]    INDEX ON THE FIRST DIMENSION
      !PARAM INDEX2     [IN]    INDEX ON THE SECOND DIMENSION
      !PARAM INDEX3     [IN]    INDEX ON THE THIRD DIMENSION
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_BOOLEAN_SIS_D
     &     (INST, VARNAME, VALEUR, INDEX1, INDEX2, INDEX3, IERR)
!
        TYPE(INSTANCE_SIS),    INTENT(INOUT) :: INST
        CHARACTER(LEN=SIS_VAR_LEN), INTENT(IN)  :: VARNAME
        INTEGER,               INTENT(IN) :: VALEUR
        INTEGER,               INTENT(IN) :: INDEX1
        INTEGER,               INTENT(IN) :: INDEX2
        INTEGER,               INTENT(IN) :: INDEX3
        INTEGER,               INTENT(OUT) :: IERR
!
        INTEGER DEBUG
!
        IERR = 0
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
!
        CHARACTER(LEN=SIS_VAR_LEN),  INTENT(IN)  :: VARNAME
        CHARACTER(LEN=SIS_TYPE_LEN), INTENT(OUT) :: VARTYPE
          LOGICAL,                     INTENT(OUT) :: READONLY
        INTEGER,                     INTENT(OUT) :: NDIM
        INTEGER,                     INTENT(OUT) :: IERR
!
        IERR = 0
        VARTYPE = ''
          READONLY = .TRUE.
        NDIM = 0
!
        IF(TRIM(VARNAME).EQ.'MODEL.FLOWRATEQ') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EVOLUTION') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Z') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.BOTTOMELEVATION') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.ZF_C') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.QBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.EBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.FLBOR_SIS') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.X') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.Y') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.XNEBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.YNEBOR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TIMESTEP') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.TOB') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CHESTR') THEN
          VARTYPE = 'DOUBLE'
          READONLY = .FALSE.
          NDIM = 1
        ELSEIF(TRIM(VARNAME).EQ.'MODEL.LIHBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLU') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.CLV') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIQBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.LIEBOR') THEN
          VARTYPE = 'INTEGER'
          READONLY = .FALSE.
          NDIM = 1
        ELSE IF(TRIM(VARNAME).EQ.'MODEL.RESULTFILE') THEN
          VARTYPE = 'STRING'
          READONLY = .FALSE.
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
      !PARAM IERR      [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                        ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SET_VAR_LIST_SIS_D(IERR)
!
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
!
        I=0
        IERR = 0
!
        IF(.NOT.ALLOCATED(VNAME_SIS)) THEN
          ALLOCATE(VNAME_SIS(NB_VAR_SIS),STAT=IERR)
          IF(IERR.NE.0) RETURN
          ALLOCATE(VINFO_SIS(NB_VAR_SIS),STAT=IERR)
          IF(IERR.NE.0) RETURN
          I = I + 1
          VNAME_SIS(I) = 'MODEL.FLOWRATEQ'
          VINFO_SIS(I) = 'SOLID TRANSPORT FLOWRATE'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.EVOLUTION'
          VINFO_SIS(I) = 'EVOLUTION OF BED'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.Z'
          VINFO_SIS(I) = 'FREE SURFACE ELEVATION'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.BOTTOMELEVATION'
          VINFO_SIS(I) = 'LEVEL OF THE BOTTOM'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.ZF_C'
          VINFO_SIS(I) = 'EVOLUTION DUE TO BEDLOAD'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.QBOR'
          VINFO_SIS(I) = 'BOUNDARY VALUE ON Q FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.EBOR'
          VINFO_SIS(I) = 'BOUNDARY VALUE ON E FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.FLBOR'
          VINFO_SIS(I) = 'BOUNDARY VALUE ON ZF FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.FLBOR_SIS'
          VINFO_SIS(I) = ''
          I = I + 1
          VNAME_SIS(I) = 'MODEL.X'
          VINFO_SIS(I) = 'X COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.Y'
          VINFO_SIS(I) = 'Y COORDINATES FOR EACH POINT OF THE MESH'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.XNEBOR'
          VINFO_SIS(I) = ''
          I = I + 1
          VNAME_SIS(I) = 'MODEL.YNEBOR'
          VINFO_SIS(I) = ''
          I = I + 1
          VNAME_SIS(I) = 'MODEL.TIMESTEP'
          VINFO_SIS(I) = 'TIME STEP'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.TOB'
          VINFO_SIS(I) = 'SHEAR STRESS'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.CHESTR'
          VINFO_SIS(I) = 'STRIKLER ON POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.LIHBOR'
          VINFO_SIS(I) = 'BOUNDARY TYPE ON H FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.CLU'
          VINFO_SIS(I) = 'BOUNDARY TYPE ON U FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.CLV'
          VINFO_SIS(I) = 'BOUNDARY TYPE ON V FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.LIQBOR'
          VINFO_SIS(I) = 'BOUNDARY TYPE ON Q FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.LIEBOR'
          VINFO_SIS(I) = 'BOUNDARY TYPE ON E FOR EACH BOUNDARY POINT'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NUMLIQ'
          VINFO_SIS(I) = 'LIQUID BOUNDARY NUMBERING'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NPOIN'
          VINFO_SIS(I) = 'NUMBER OF POINT IN THE MESH'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NELEM'
          VINFO_SIS(I) = 'NUMBER OF ELEMENT IN THE MESH'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NPFTR'
          VINFO_SIS(I) = 'NUMBER OF BOUNDARY POINTS'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NELMAX'
          VINFO_SIS(I) = 'MAXIMUM NUMBER OF ELEMENTS IN THE MESH'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.IKLE'
          VINFO_SIS(I) = 'CONNECTIVITY TABLE OF ELEMENTS AND NODES'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.NTIMESTEPS'
          VINFO_SIS(I) = 'NUMBER OF TIME STEPS'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.CURRENTSTEP'
          VINFO_SIS(I) = 'CURRENT TIME STEP'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.RESULTFILE'
          VINFO_SIS(I) = 'RESULTS FILE OF THE CASE'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.BCFILE'
          VINFO_SIS(I) = 'BOUNDARY CONDITIONS FILE OF THE CASE'
          I = I + 1
          VNAME_SIS(I) = 'MODEL.GEOMETRYFILE'
          VINFO_SIS(I) = 'GEOMETRY FILE OF THE CASE'
        ENDIF
!
      END SUBROUTINE SET_VAR_LIST_SIS_D
!
      END MODULE API_HANDLE_VAR_SIS
