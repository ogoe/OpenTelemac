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
      USE API_HANDLE_ERROR_T2D
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
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
      PUBLIC :: INSTANCE_LIST
!
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
!
        TYPE(BIEF_FILE), POINTER :: T2D_FILES(:)
        INTEGER :: MAXLU_T2D
        INTEGER :: MAXKEY
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
      END TYPE ! MODEL_T2D
!!
!      type state_t2d
!        integer :: test
!      end type ! state_t2d
!!
!      type instance_t2d
!!
!        type(model_t2d) :: model
!        type(state_t2d) :: state
!!
!      end type ! instance_t2d
!
      INTEGER, PARAMETER :: MAX_INSTANCES=10
      TYPE(INSTANCE_T2D), POINTER, SAVE :: INSTANCE_LIST(:)
      LOGICAL, ALLOCATABLE, SAVE :: USED_INSTANCE(:)
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
          ALLOCATE(INSTANCE_LIST(MAX_INSTANCES),STAT=IERR)
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
        INSTANCE_LIST(ID)%MYPOSITION = NO_POSITION
!       Link with telemac2d variables
        INSTANCE_LIST(ID)%HBOR   => HBOR
        INSTANCE_LIST(ID)%UBOR   =>  UBOR
        INSTANCE_LIST(ID)%VBOR   =>  VBOR
        INSTANCE_LIST(ID)%H      =>  H
        INSTANCE_LIST(ID)%DH     =>  DH
        INSTANCE_LIST(ID)%U      =>  U
        INSTANCE_LIST(ID)%V      =>  V
        INSTANCE_LIST(ID)%CHESTR =>  CHESTR
        INSTANCE_LIST(ID)%FLUX_BOUNDARIES => FLUX_BOUNDARIES
        INSTANCE_LIST(ID)%COTE => COTE
        INSTANCE_LIST(ID)%DEBIT  => DEBIT
!
        INSTANCE_LIST(ID)%MESH   => MESH
        INSTANCE_LIST(ID)%LIHBOR => LIHBOR
        INSTANCE_LIST(ID)%LIUBOR => LIUBOR
        INSTANCE_LIST(ID)%LIVBOR => LIVBOR
        INSTANCE_LIST(ID)%NUMLIQ => NUMLIQ
!
        INSTANCE_LIST(ID)%NIT    => NIT
        INSTANCE_LIST(ID)%LT     => LT
!
        INSTANCE_LIST(ID)%T2D_FILES => T2D_FILES
        INSTANCE_LIST(ID)%T2DRES => T2DRES
        INSTANCE_LIST(ID)%T2DGEO => T2DGEO
        INSTANCE_LIST(ID)%T2DCLI => T2DCLI
        INSTANCE_LIST(ID)%MAXLU_T2D = MAXLU_T2D
        INSTANCE_LIST(ID)%MAXKEY = MAXKEY
!
        INSTANCE_LIST(ID)%COUPLING => COUPLING
!
        INSTANCE_LIST(ID)%TE5    => TE5
        INSTANCE_LIST(ID)%ZF     => ZF
        INSTANCE_LIST(ID)%H      => H
!
        INSTANCE_LIST(ID)%DEBUG  => DEBUG



      END SUBROUTINE CREATE_INSTANCE_T2D
!
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
        MESS = INSTANCE_LIST(ID)%ERROR_MESSAGE
!
      END SUBROUTINE GET_INSTANCE_ERROR_T2D
      END MODULE API_INSTANCE_T2D
