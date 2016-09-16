!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!brief module handling all the instance function
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!history Y AUDOUIN (EDF R&D, LNHE)
!+       21/08/2013
!+       V6P3
!+       Creation of the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE API_INSTANCE_T2D
!
      USE API_HANDLE_ERROR
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
      USE DECLARATIONS_SPECIAL, ONLY : MAXKEYWORD
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SISYPHE, ONLY : SIS_FILES,MAXLU_SIS
      USE DECLARATIONS_TOMAWAC, ONLY : WAC_FILES,MAXLU_WAC
      USE INTERFACE_TELEMAC2D
      IMPLICIT NONE

      PRIVATE

      PUBLIC :: CREATE_INSTANCE_T2D
      PUBLIC :: DELETE_INSTANCE_T2D
      PUBLIC :: CHECK_INSTANCE_T2D
      PUBLIC :: GET_INSTANCE_ERROR_T2D
      PUBLIC :: INSTANCE_T2D
      PUBLIC :: INSTANCE_LIST_T2D
      PUBLIC :: CPL_INIT_T2D
!

      ! TYPE FOR API COUPLED CALL
      TYPE SISYPHE_CPL
        INTEGER, POINTER :: LT, LEOPRD, LISPRD, NIT
        TYPE(BIEF_OBJ), POINTER :: U, V, H, HN, HPROP
        TYPE(BIEF_OBJ), POINTER :: ZF, CF, CHESTR
        TYPE(API_CPL), POINTER :: SIS_CPL
        INTEGER, POINTER :: PERCOU
        DOUBLE PRECISION, POINTER :: AT
        TYPE(BIEF_OBJ), POINTER :: VISC
        DOUBLE PRECISION, POINTER :: DT
        TYPE(BIEF_OBJ), POINTER :: FLBOR,DM1
        INTEGER, POINTER :: SOLSYS
        TYPE(BIEF_OBJ), POINTER :: USIS, VSIS, ZCONV
        TYPE(BIEF_OBJ), POINTER :: DIRMOY, HM0, TPR5, ORBVEL
      END TYPE SISYPHE_CPL


      TYPE INSTANCE_T2D
        ! RUN POSITION
        INTEGER MYPOSITION
        ! ERROR MESSAGE
        CHARACTER(LEN=200) :: ERROR_MESSAGE
        ! LIST OF ALL THE VARIABLE FOR MODEL
        TYPE(BIEF_OBJ), POINTER :: HBOR
        TYPE(BIEF_OBJ), POINTER :: UBOR
        TYPE(BIEF_OBJ), POINTER :: VBOR
        TYPE(BIEF_OBJ), POINTER :: U
        TYPE(BIEF_OBJ), POINTER :: V
        TYPE(BIEF_OBJ), POINTER :: CHESTR
        DOUBLE PRECISION, POINTER :: FLUX_BOUNDARIES(:)
        DOUBLE PRECISION, POINTER :: COTE(:)
        DOUBLE PRECISION, POINTER :: DEBIT(:)
!
        TYPE(BIEF_MESH), POINTER :: MESH
!
        TYPE(BIEF_OBJ), POINTER :: LIHBOR
        TYPE(BIEF_OBJ), POINTER :: LIUBOR
        TYPE(BIEF_OBJ), POINTER :: LIVBOR
        TYPE(BIEF_OBJ), POINTER :: NUMLIQ
!
        INTEGER,        POINTER :: NIT
        INTEGER,        POINTER :: LT
        DOUBLE PRECISION,POINTER :: AT
!
        TYPE(BIEF_FILE), POINTER :: T2D_FILES(:)
        INTEGER :: MAXLU_T2D
        INTEGER :: MAXKEYWORD
        INTEGER, POINTER :: T2DRES
        INTEGER, POINTER :: T2DGEO
        INTEGER, POINTER :: T2DCLI
!
        CHARACTER(LEN=144), POINTER :: COUPLING
!

        TYPE(BIEF_OBJ), POINTER :: TE5
        TYPE(BIEF_OBJ), POINTER :: ZF
        TYPE(BIEF_OBJ), POINTER :: H
        TYPE(BIEF_OBJ), POINTER :: DH
!
        INTEGER, POINTER :: DEBUG
        ! LIST OF ALL THE VARIABLE FOR STATE
        INTEGER :: TRUC
!

        !VARIABLES FOR SISYPHE CALL, NECESSARY FOR THE COUPLING
        TYPE(SISYPHE_CPL) :: SIS

        !TEMPORARY FOR SISYPHE CALL
        LOGICAL :: SUSP1, CHARR_TEL
        LOGICAL :: CHARR_SIS, SUSP_SIS
        INTEGER :: LEOPRD_CHARR
      END TYPE ! MODEL_T2D
!
      INTEGER, PARAMETER :: MAX_INSTANCES=10
      TYPE(INSTANCE_T2D), POINTER :: INSTANCE_LIST_T2D(:)
      LOGICAL, ALLOCATABLE :: USED_INSTANCE(:)
!
      CONTAINS
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief creates a telemac2d instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history y audouin (edf r&d, lnhe)
      !+       21/08/2013
      !+       V6P3
      !+       creation of the file
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !param id   [out]    id of the new instance
      !param ierr [out]    0 if subroutine successfull,
      !+                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CREATE_INSTANCE_T2D(ID,IERR)
      ! initialise instance for telemac2d
        INTEGER, INTENT(OUT) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: I
        ID = 0
        IERR = 0
        ! If first time createing an instance allocating the instance array
        IF(.NOT. ALLOCATED(USED_INSTANCE)) THEN
          ALLOCATE(USED_INSTANCE(MAX_INSTANCES),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERR_MESS = 'ERROR WHILE ALLOCATING USED INSTANCE ARRAY'
            RETURN
          ENDIF
          USED_INSTANCE = .FALSE.
          ALLOCATE(INSTANCE_LIST_T2D(MAX_INSTANCES),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERR_MESS = 'ERROR WHILE ALLOCATING INSTANCE ARRAY'
            RETURN
          ENDIF
        ENDIF
!
        ! look for the first instance available
        I = 1
        DO WHILE(USED_INSTANCE(I).AND.I.LE.MAX_INSTANCES)
          I = I + 1
        ENDDO
        ID = I
        USED_INSTANCE(ID) = .TRUE.
!
        ! if still equals 0 no available instance was found then we crash
        IF(ID.EQ.(MAX_INSTANCES+1))THEN
          IERR = MAX_INSTANCE_ERROR
          ERR_MESS = "MAX INSTANCE REACHED "
          RETURN
        ENDIF
        !
        INSTANCE_LIST_T2D(ID)%MYPOSITION = NO_POSITION
!       Link with telemac2d variables
        CALL UPDATE_INSTANCE_T2D(ID,IERR)

      END SUBROUTINE CREATE_INSTANCE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief updates a telemac2d instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history c goeury & y audouin (edf r&d, lnhe)
      !+       17/06/2016
      !+       V7P1
      !+       update the api instance
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !param id   [out]    id of the new instance
      !param ierr [out]    0 if subroutine successfull,
      !+                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE UPDATE_INSTANCE_T2D(ID,IERR)
      ! initialise instance for telemac2d
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
!       Link with telemac2d variables
        INSTANCE_LIST_T2D(ID)%HBOR   => HBOR
        INSTANCE_LIST_T2D(ID)%UBOR   =>  UBOR
        INSTANCE_LIST_T2D(ID)%VBOR   =>  VBOR
        INSTANCE_LIST_T2D(ID)%H      =>  H
        INSTANCE_LIST_T2D(ID)%DH     =>  DH
        INSTANCE_LIST_T2D(ID)%U      =>  U
        INSTANCE_LIST_T2D(ID)%V      =>  V
        INSTANCE_LIST_T2D(ID)%CHESTR =>  CHESTR
        INSTANCE_LIST_T2D(ID)%FLUX_BOUNDARIES => FLUX_BOUNDARIES
        INSTANCE_LIST_T2D(ID)%COTE => COTE
        INSTANCE_LIST_T2D(ID)%DEBIT  => DEBIT
!
        INSTANCE_LIST_T2D(ID)%MESH   => MESH
        INSTANCE_LIST_T2D(ID)%LIHBOR => LIHBOR
        INSTANCE_LIST_T2D(ID)%LIUBOR => LIUBOR
        INSTANCE_LIST_T2D(ID)%LIVBOR => LIVBOR
        INSTANCE_LIST_T2D(ID)%NUMLIQ => NUMLIQ
        INSTANCE_LIST_T2D(ID)%T2D_FILES => T2D_FILES
        INSTANCE_LIST_T2D(ID)%T2DRES => T2DRES
        INSTANCE_LIST_T2D(ID)%T2DGEO => T2DGEO
        INSTANCE_LIST_T2D(ID)%T2DCLI => T2DCLI
        INSTANCE_LIST_T2D(ID)%MAXLU_T2D = MAXLU_T2D
        INSTANCE_LIST_T2D(ID)%MAXKEYWORD = MAXKEYWORD
!
        INSTANCE_LIST_T2D(ID)%NIT    => NIT
        INSTANCE_LIST_T2D(ID)%LT     => LT
        INSTANCE_LIST_T2D(ID)%AT     => AT
!
        INSTANCE_LIST_T2D(ID)%T2D_FILES => T2D_FILES
        INSTANCE_LIST_T2D(ID)%T2DRES => T2DRES
        INSTANCE_LIST_T2D(ID)%T2DGEO => T2DGEO
        INSTANCE_LIST_T2D(ID)%T2DCLI => T2DCLI
        INSTANCE_LIST_T2D(ID)%MAXLU_T2D = MAXLU_T2D
        INSTANCE_LIST_T2D(ID)%MAXKEYWORD = MAXKEYWORD
!
        INSTANCE_LIST_T2D(ID)%COUPLING => COUPLING
!
        INSTANCE_LIST_T2D(ID)%TE5    => TE5
        INSTANCE_LIST_T2D(ID)%ZF     => ZF
        INSTANCE_LIST_T2D(ID)%H      => H
!
        INSTANCE_LIST_T2D(ID)%DEBUG  => DEBUG

        ! INITIALISATIONS POUR UN CAS SANS COUPLAGE
        INSTANCE_LIST_T2D(ID)%SIS%LT => LT
        INSTANCE_LIST_T2D(ID)%SIS%LEOPRD => LEOPRD
        INSTANCE_LIST_T2D(ID)%SIS%LISPRD => LISPRD
        INSTANCE_LIST_T2D(ID)%SIS%NIT => NIT
        INSTANCE_LIST_T2D(ID)%SIS%U => U
        INSTANCE_LIST_T2D(ID)%SIS%V => V
        INSTANCE_LIST_T2D(ID)%SIS%H => H
        INSTANCE_LIST_T2D(ID)%SIS%HN => HN
        INSTANCE_LIST_T2D(ID)%SIS%HPROP => HPROP
        INSTANCE_LIST_T2D(ID)%SIS%ZF => ZF
        INSTANCE_LIST_T2D(ID)%SIS%CF => CF
        INSTANCE_LIST_T2D(ID)%SIS%CHESTR => CHESTR
        INSTANCE_LIST_T2D(ID)%SIS%SIS_CPL => SIS_CPL
        INSTANCE_LIST_T2D(ID)%SIS%PERCOU => PERCOU
        INSTANCE_LIST_T2D(ID)%SIS%AT => AT
        INSTANCE_LIST_T2D(ID)%SIS%VISC => VISC
        INSTANCE_LIST_T2D(ID)%SIS%DT => DT
        INSTANCE_LIST_T2D(ID)%SIS%FLBOR => FLBOR
        INSTANCE_LIST_T2D(ID)%SIS%SOLSYS => SOLSYS
        INSTANCE_LIST_T2D(ID)%SIS%DM1 => DM1
        !USIS et VSIS modified of SOLSYS.EQ.2 after initialization
        INSTANCE_LIST_T2D(ID)%SIS%USIS => UCONV
        INSTANCE_LIST_T2D(ID)%SIS%VSIS => VCONV
        INSTANCE_LIST_T2D(ID)%SIS%ZCONV => ZCONV
        INSTANCE_LIST_T2D(ID)%SIS%DIRMOY => DIRMOY
        INSTANCE_LIST_T2D(ID)%SIS%HM0 => HM0
        INSTANCE_LIST_T2D(ID)%SIS%TPR5 => TPR5
        INSTANCE_LIST_T2D(ID)%SIS%ORBVEL => ORBVEL

      END SUBROUTINE UPDATE_INSTANCE_T2D
!
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
      !param ID             [IN]     ID FOR TELEMAC2D INSTANCE
      !PARAM IERR           [OUT]    0 IF SUBROUTINE SUCCESSFULL,
      !+                             ERROR ID OTHERWISE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CPL_INIT_T2D(ID,IERR)
        INTEGER, INTENT(IN) :: ID
        INTEGER,             INTENT(OUT) :: IERR
      !
        IERR = 0
        IF(INSTANCE_LIST_T2D(ID)%SIS%SOLSYS.EQ.2) THEN
          INSTANCE_LIST_T2D(ID)%SIS%USIS => UDEL
          INSTANCE_LIST_T2D(ID)%SIS%VSIS => VDEL
        END IF

      END SUBROUTINE CPL_INIT_T2D

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief deletes a telemac2d instance
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history y audouin (edf r&d, lnhe)
      !+       21/08/2013
      !+       V6P3
      !+       creation of the file
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !param id    [in]    id of the instance
      !param ierr [out]    0 if subroutine successfull,
      !+                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE DELETE_INSTANCE_T2D(ID,IERR)
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        !
        CALL CHECK_INSTANCE_T2D(ID,IERR)
        IF(IERR.NE.0) RETURN
        USED_INSTANCE(ID) = .FALSE.
      END SUBROUTINE DELETE_INSTANCE_T2D
!
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !brief check if the id is following convention
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      !history y audouin (edf r&d, lnhe)
      !+       21/08/2013
      !+       V6P3
      !+       creation of the file
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !param id    [in]    id of the instance
      !param ierr [out]    0 if subroutine successfull,
      !+                   error id otherwise
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE CHECK_INSTANCE_T2D(ID,IERR)
        INTEGER, INTENT(IN) :: ID
        INTEGER, INTENT(OUT) :: IERR
!
        IERR = 0
        IF(ID.LE.0 .OR. ID.GT.MAX_INSTANCES) THEN
          IERR = INVALID_INSTANCE_NUM_ERROR
          ERR_MESS = 'INVALID INSTANCE NUMBER'
          RETURN
        ENDIF
        IF(.NOT.USED_INSTANCE(ID)) THEN
          IERR = UNUSED_INSTANCE_ERROR
          ERR_MESS = 'INSTANCE NUMBER WAS NOT CREATED'
          RETURN
        ENDIF
        CALL UPDATE_INSTANCE_T2D(ID,IERR)
      END SUBROUTINE CHECK_INSTANCE_T2D
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
      !param mess  [out]   The erro message
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GET_INSTANCE_ERROR_T2D(ID,MESS)
        INTEGER, INTENT(IN) :: ID
        CHARACTER(LEN=200), INTENT(OUT) :: MESS
!
        MESS = INSTANCE_LIST_T2D(ID)%ERROR_MESSAGE
!
      END SUBROUTINE GET_INSTANCE_ERROR_T2D
      END MODULE API_INSTANCE_T2D
