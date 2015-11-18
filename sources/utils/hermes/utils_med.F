!                 ************
                  MODULE UTILS_MED
!                 ************
!
!***********************************************************************
! HERMES  V7P0                                                 2015
!***********************************************************************
!
!brief    INTERFACES OF MED PUBLIC SUBROUTINES
!
!warning  NOTE THAT THIS INTERFACE IS FOR THE LIBRARY INCLUDED IN MED
!+     V3.0.4 CHANGES MIGHT BE REQUIRED WITH FUTURE VERSIONS OF MED.
!
!history YOANN AUDOUIN
!+       25/05/2015
!+       V7P0
!+       ADAPTING CODE TO MED V3.0.4
!
!history VINCENT STOBIAC
!+       23/05/2014
!+       ADDING GENERAL MED SUBROUTINES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
#if defined (HAVE_MED)
      INCLUDE 'med.hf'
#endif
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NEW OBJECT RELATED TO MESHES IN A MED FILE
      TYPE MED_INFO
        INTEGER :: ID
        INTEGER :: NCLI
        INTEGER, ALLOCATABLE :: NBOR(:)
        ! Contains for each boundary element type if it is indeed a boundary
        LOGICAL, ALLOCATABLE :: IS_BND(:)
        ! Number of boundary groups
        INTEGER :: NBND_GRP
        ! Contains for each boundary group its values for h u v tracer
        INTEGER, ALLOCATABLE :: BND_GRP_VAL(:,:)
        ! Contains for each family its number and the index of the boundary group associated 
        ! 0 if it is not a boundary family
        INTEGER, ALLOCATABLE :: BND_FAM(:,:)
#if defined(HAVE_MED)
        CHARACTER(LEN=MED_NAME_SIZE) :: MESH_NAME
        ! Contains for boundary group its name
        CHARACTER(LEN=MED_LNAME_SIZE), ALLOCATABLE :: BND_GRP_NAME(:)
#endif
      END TYPE MED_INFO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      INTEGER, PARAMETER :: MAX_FILE = 30
      INTEGER :: NFILES = 0
!
!     HASH TABLE FOR FILES
      INTEGER :: HASH(MAX_FILE) =
     &         (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &            0, 0, 0, 0 /)
!
      TYPE(MED_INFO),SAVE :: MED_OBJ_TAB(MAX_FILE)
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MED_OBJ(FILE_ID,INFO,IERR)
!
!brief    GET THE ID OF A MED FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID     |<--| ID OF THE FILE
!| INFO        |-->| MED NUMBERING OF THE FILE
!| IERR        |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(INOUT) :: INFO
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER :: MED_ID, I
!
!-----------------------------------------------------------------------
!
        MED_ID = 0
!
!       LOOK FOR THE ID LINKED TO THAT FILE_ID
        IF(NFILES.GE.1) THEN
          DO I=1,MAX_FILE
            IF (HASH(I).EQ.FILE_ID) THEN
              MED_ID = I
              EXIT
            ENDIF
          ENDDO
        ENDIF
!
!       IF MED_ID == 0 THEN NO ID WAS FOUND RETURN ERROR
        IF(MED_ID.EQ.0) THEN
          IERR = HERMES_FILE_NOT_OPENED_ERR
        ELSE
          IERR = 0
        ENDIF
        INFO = MED_ID
!
        RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE ADD_MED_FILE(FILE_ID,MED_ID,IERR)
!
!brief    ADD NEW MED FILE IN HASH TABLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID     |<--| ID OF THE FILE
!| MED_ID      |-->| MED NUMBERING OF THE FILE
!| IERR        |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
!
        INTEGER, INTENT(IN)  :: FILE_ID
        INTEGER, INTENT(OUT) :: IERR
        INTEGER, INTENT(INOUT) :: MED_ID
!
!-----------------------------------------------------------------------
!
        MED_ID = 0
!
!       CHECK IF THE FILE IS ALREADY OPEN
        CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
        IF(MED_ID.NE.0) THEN
          IERR = HERMES_FILE_ID_ALREADY_IN_USE_ERR
          RETURN
        ENDIF
!
!       WE RESET THE IERR TO ZERO AS THE PREVIOUS CALL SHOULD
!       HAVE CRASHED AS THE FILE IS NOT OPENED YET
        IERR = 0
!
!       CHECK IF WE'VE REACH THE MAXIMUM NUMBER OF FILES
        IF(NFILES.EQ.MAX_FILE) THEN
          IERR = HERMES_MAX_FILE_ERR
          RETURN
        ENDIF
!       
!       Look far a place in the hash table
        MED_ID = 1
        DO
          IF(HASH(MED_ID).EQ.0) EXIT
          MED_ID = MED_ID + 1
        ENDDO
        NFILES = NFILES + 1
        HASH(MED_ID) = FILE_ID
!
        RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE OPEN_MESH_MED(FILE_NAME,FILE_ID,OPEN_MODE,IERR)
!
!BRIEF    OPENS A MESH FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |<--| NAME OF THE MED FILE
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| OPEN_MODE      |<--| OPENING MODE (READ/READWRITE/WRITE)
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      CHARACTER(LEN=*), INTENT(IN)  :: FILE_NAME
      INTEGER,          INTENT(IN)  :: FILE_ID
      CHARACTER(LEN=9), INTENT(IN)  :: OPEN_MODE
      INTEGER,          INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      LOGICAL :: HDFOK, MEDOK ! CHECK COMPATIBILITY
      INTEGER :: MED_MODE ! MED OPENING MODE
      INTEGER :: MAJOR, MINOR, REL ! MED VERSION OF THE FILE
      INTEGER :: MED_ID
      CHARACTER(LEN=200) :: CFILE_NAME
      CHARACTER(LEN=MED_NAME_SIZE)    :: TITLE_MED
      CHARACTER(LEN=MED_SNAME_SIZE)   :: DT_UNIT
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_NAME(:)
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_UNIT(:)
      CHARACTER(LEN=MED_COMMENT_SIZE) :: COMMENT
!
      INTEGER :: IMESH             ! MESH DESCRIPTOR
      INTEGER :: NB_DIM_PB         ! NUMBER OF PHYSICAL DIMENSIONS
      INTEGER :: NB_DIM_MESH       ! NUMBER OF MESH DIMENSIONS
      INTEGER :: MESH_TYPE         ! TYPE OF THE MESH
      INTEGER :: ORDER             ! SORTING ORDER FOR IT AND DT
      INTEGER :: NCOMP             ! NUMBER OF COMPUTATION STEPS
      INTEGER :: COOR_TYPE         ! TYPE OF COORDINATES
      INTEGER :: NAXIS             ! NUMBER OF AXES
!
!-----------------------------------------------------------------------
!
!     CHECK THE MODE. POSSIBLE MODES ARE READONLY, WRITEONLY OR
!     READWRITE. OTHER MODES ARE INVALID
      SELECT CASE(OPEN_MODE)
        CASE('READ     ')
          MED_MODE = MED_ACC_RDONLY
        CASE('READWRITE')
          MED_MODE = MED_ACC_RDWR
        CASE('WRITE    ')
          MED_MODE = MED_ACC_CREAT
        CASE DEFAULT
!         OPEN MODE IS INVALID
          IERR = HERMES_INVALID_OPEN_MODE_ERR
      END SELECT
      CFILE_NAME = TRIM(FILE_NAME)//CHAR(0)
!
      HDFOK = .TRUE.
      MEDOK = .TRUE.
      IF(MED_MODE.EQ.MED_ACC_RDONLY) THEN
!       CHECK IF THE FILE IS BOTH A MED & HDF5 FILE
        CALL MFICOM(CFILE_NAME, HDFOK, MEDOK, IERR)
        CALL CHECK_CALL(IERR,'OPEN_MESH_MED:MFICOM')
      ENDIF
!
!     MESH FILE NOT COMPATIBLE WITH HDF5 OR MED
      IF (.NOT. HDFOK) IERR = HERMES_WRONG_HDF_FORMAT_ERR
      IF (.NOT. MEDOK) IERR = HERMES_WRONG_MED_FORMAT_ERR
!
!     OPEN THE MED FILE AND CHECK RETURN CODE
      CALL MFIOPE(FILE_ID, CFILE_NAME, MED_MODE, IERR)
      CALL CHECK_CALL(IERR,'OPEN_MESH_MED:MFIOPE')
!
      IF(MED_MODE.EQ.MED_ACC_RDONLY) THEN
!       CHECK COMPATIBILITY (EXPECTED)
        CALL MFINVR (FILE_ID, MAJOR, MINOR, REL, IERR)
        CALL CHECK_CALL(IERR,'OPEN_MESH_MED:MFINVR')
!       MED FILE IS TOO OLD AND SHOULD BE CONVERTED WITH MEDIMPORT
        IF (MAJOR.LT.3) THEN
          IERR = HERMES_WRONG_MED_VERSION_ERR
          RETURN
        ENDIF
      ENDIF
!
!     CREATE A NEW OBJECT FOR THE NEW MED FILE
      CALL ADD_MED_FILE(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'OPEN_MESH_MED:ADD_MED_FILE')
!
!     Identify the name of the mesh if in read only
      IF(MED_MODE.EQ.MED_ACC_RDONLY) THEN
!       ONLY ONE MESH PER FILE FOR NOW
        IMESH = 1
!       
        CALL MMHNAX(FILE_ID,IMESH,NAXIS,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_TITLE_MED:MMHMII')
        ALLOCATE(COOR_NAME(NAXIS),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GET_MESH_TITLE_MED:COOR_NAME')
        ALLOCATE(COOR_UNIT(NAXIS),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GET_MESH_TITLE_MED:COOR_UNIT')
!       
!       READ MESH TITLE
        CALL MMHMII(FILE_ID,IMESH,TITLE_MED,NB_DIM_PB,NB_DIM_MESH,
     &    MESH_TYPE,COMMENT,DT_UNIT,ORDER,NCOMP,COOR_TYPE,COOR_NAME,
     &    COOR_UNIT,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_TITLE_MED:MMHMII')
        DEALLOCATE(COOR_NAME)
        DEALLOCATE(COOR_UNIT)
!       
        MED_OBJ_TAB(MED_ID)%MESH_NAME = TITLE_MED
      ENDIF
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE CLOSE_MESH_MED (FILE_ID, IERR)
!
!BRIEF    CLOSES A MESH FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(OUT) :: IERR
!
      INTEGER MED_ID
!
!-----------------------------------------------------------------------
!
#if defined (HAVE_MED)
!
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'CLOSE_MESH_MED:GET_MED_OBJ')
!
      ! Clearing id in the hash table
      HASH(MED_ID) = 0
      NFILES = NFILES - 1
!
!     CLOSE MED FILE
      CALL MFICLO(FILE_ID, IERR)
      CALL CHECK_CALL(IERR,'CLOSE_MESH_MED:MFICLO')
!
      ! CLEAN UP OF STRUCTURE
      MED_OBJ_TAB(MED_ID)%ID = 0
      MED_OBJ_TAB(MED_ID)%NCLI = 0
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!
!-----------------------------------------------------------------------
!
      SUBROUTINE OPEN_BND_MED(FILE_NAME,FILE_ID,OPEN_MODE,IERR)
!
!BRIEF    OPEN BOUNDARY FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |<--| NAME OF THE BOUNDARY FILE
!| FILE_ID        |<--| ID OF THE FILE
!| OPEN_MODE      |<--| OPENING MODE (READ/READWRITE/WRITE)
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
!
        CHARACTER(LEN=*), INTENT(IN)  :: FILE_NAME
        INTEGER,          INTENT(IN)  :: FILE_ID
        CHARACTER(LEN=9), INTENT(IN)  :: OPEN_MODE
        INTEGER,          INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
        INTEGER :: MED_ID,OFFSET,NCLI,NBND_GRP,I
        LOGICAL :: ISOPENED, IS_USED
!
!-----------------------------------------------------------------------
!
        CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
        CALL CHECK_CALL(IERR,'OPEN_BND_MED:GET_MED_OBJ')
!
        ! Open the boundary file with a set id available
        MED_OBJ_TAB(MED_ID)%NCLI = 1000 + FILE_ID
        DO
          INQUIRE(UNIT=MED_OBJ_TAB(MED_ID)%NCLI,OPENED=IS_USED)
          IF(.NOT.IS_USED) EXIT
          MED_OBJ_TAB(MED_ID)%NCLI = MED_OBJ_TAB(MED_ID)%NCLI + 1
        ENDDO
        ! First we check if the file is already opened
        ! Telemac is using one boundary file for all the mesh file
        ! so it could have been opened by another mesh before hand
        INQUIRE(FILE=FILE_NAME,OPENED=ISOPENED)
        IF(ISOPENED) THEN
          ! Id the file is already opened get its id
          INQUIRE(FILE=FILE_NAME,NUMBER=MED_OBJ_TAB(MED_ID)%NCLI)
        ELSE
          ! Otherwise open the file
          OPEN(UNIT=MED_OBJ_TAB(MED_ID)%NCLI,FILE=FILE_NAME,
     &         FORM='FORMATTED',ACTION=OPEN_MODE,IOSTAT=IERR)
        ENDIF
        IF(OPEN_MODE(1:4).eq.'READ') THEN
          ! Getting the name of the boundary groups
          NCLI = MED_OBJ_TAB(MED_ID)%NCLI
          REWIND(NCLI)
!         LOOP ON THE INFO IN THE FILE
          READ(UNIT=NCLI,FMT=*,IOSTAT=IERR) NBND_GRP
          CALL CHECK_CALL(IERR,'OPEN_BND_MED:READ')
          MED_OBJ_TAB(MED_ID)%NBND_GRP = NBND_GRP
          ALLOCATE(MED_OBJ_TAB(MED_ID)%BND_GRP_VAL(NBND_GRP,4),
     &             STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'BND_GRP_VAL')
          ALLOCATE(MED_OBJ_TAB(MED_ID)%BND_GRP_NAME(NBND_GRP),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'BND_GRP_NAME')
          DO I=1,NBND_GRP
            READ(UNIT=NCLI,FMT=*,IOSTAT=IERR)
     &               MED_OBJ_TAB(MED_ID)%BND_GRP_VAL(I,1),
     &               MED_OBJ_TAB(MED_ID)%BND_GRP_VAL(I,2),
     &               MED_OBJ_TAB(MED_ID)%BND_GRP_VAL(I,3),
     &               MED_OBJ_TAB(MED_ID)%BND_GRP_VAL(I,4),
     &               MED_OBJ_TAB(MED_ID)%BND_GRP_NAME(I)
            CALL CHECK_CALL(IERR,'OPEN_BND_MED:READ')
          ENDDO
        ENDIF
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!
!-----------------------------------------------------------------------
!
      SUBROUTINE CLOSE_BND_MED (FILE_ID, IERR)
!
!BRIEF    CLOSES A MESH BOUNDARY FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
!
        INTEGER, INTENT(IN)  :: FILE_ID
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER MED_ID
        LOGICAL ISOPENED
!
!-----------------------------------------------------------------------
!
        CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
        CALL CHECK_CALL(IERR,'CLOSE_BND_MED:GET_MED_OBJ')
!
        ISOPENED = .FALSE.
        ! CHECK IF THE FILE IS STILL OPENED AS IT COULD HAVE BEEN CLOSED
        ! BY ANOTHER MESH FILE (SEE OPEN_BND_SRF FOR MORE INFORMATION)
        INQUIRE(UNIT=MED_OBJ_TAB(MED_ID)%NCLI,OPENED=ISOPENED)
        IERR = 0
        IF(ISOPENED) THEN
          CLOSE(MED_OBJ_TAB(MED_ID)%NCLI,IOSTAT=IERR)
          IF(ALLOCATED(MED_OBJ_TAB(MED_ID)%NBOR)) THEN
            DEALLOCATE(MED_OBJ_TAB(MED_ID)%NBOR)
          ENDIF
          IF(ALLOCATED(MED_OBJ_TAB(MED_ID)%BND_GRP_VAL)) THEN
            DEALLOCATE(MED_OBJ_TAB(MED_ID)%BND_GRP_VAL)
          ENDIF
          IF(ALLOCATED(MED_OBJ_TAB(MED_ID)%BND_FAM)) THEN
            DEALLOCATE(MED_OBJ_TAB(MED_ID)%BND_FAM)
          ENDIF
#if defined(HAVE_MED)
          IF(ALLOCATED(MED_OBJ_TAB(MED_ID)%BND_GRP_NAME)) THEN
            DEALLOCATE(MED_OBJ_TAB(MED_ID)%BND_GRP_NAME)
          ENDIF
#endif
        ENDIF
        CALL CHECK_CALL(IERR,'CLOSE_BND_MED:CLOSE')
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_TITLE_MED (FILE_ID, TITLE, IERR)
!
!BRIEF    READS THE MESH TITLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TITLE          |-->| MESH TITLE OR NAME
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER,           INTENT(IN)  :: FILE_ID
      CHARACTER(LEN=80), INTENT(INOUT) :: TITLE
      INTEGER,           INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
!
      INTEGER :: MED_ID
!
!-----------------------------------------------------------------------
!
!     STORE THE NAME OF THE MESH IN THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_TITLE_MED:GET_MED_OBJ')
      TITLE = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_NELEM_MED (FILE_ID, TYPE_ELEM, NELEM, IERR)
!
!BRIEF    READS TOTAL NUMBER OF ELEMENTS FOR A GIVEN TYPE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM      |<--| TYPE OF ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(INOUT) :: NELEM
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: TYPE_ELEM_MED   ! ELEMENT TYPE IN MED NUMBERING
      INTEGER :: CHGT,TSF        ! INDICATORS OF MESH MODIF
      INTEGER :: MED_ID
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_NELEM_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_NELEM_MED:CONVERT_ELEM_TYPE')
!
!     READ TOTAL NUMBER OF ELEMENTS
      CALL MMHNME(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_CELL,
     &  TYPE_ELEM_MED,MED_CONNECTIVITY,MED_NODAL,CHGT,TSF,NELEM,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_NELEM_MED:MMHNME')
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_NPOIN_PER_ELEMENT_MED (TYPE_ELEM, NDP, IERR)
!
!BRIEF    READS NUMBER OF NODES PER ELEMENT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TYPE_ELEM      |<--| TYPE OF ELEMENT
!| NDP            |-->| NUMBER OF NODES PER ELEMENT
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(INOUT) :: NDP
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
!
      INTEGER :: TYPE_ELEM_MED   ! ELEMENT TYPE IN MED NUMBERING
!
!-----------------------------------------------------------------------
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      CALL CHECK_CALL(IERR,
     &         'GET_MESH_NPOIN_PER_ELEMENT_MED:CONVERT_ELEM_TYPE')
!
!     DEDUCES NUMBER OF NODES PER ELEMENT FROM THE ELEMENT TYPE
      CALL NDP_FROM_ELEMENT_TYPE_MED(TYPE_ELEM_MED,NDP,IERR)
      CALL CHECK_CALL(IERR,
     &  'GET_MESH_NPOIN_PER_ELEMENT_MED:NDP_FROM_ELEMENT_TYPE_MED')
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_CONNECTIVITY_MED (FILE_ID,TYPE_ELEM,IKLE,
     &                              NELEM,NDP,IERR)
!
!BRIEF    READS THE CONNECTIVITY TABLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM      |<--| TYPE OF THE ELEMENT IN FORMAT SLF
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |<--| NUMBER OF ELEMENTS
!| NDP            |<--| NUMBER OF NODES PER ELEMENT
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(IN)  :: NELEM
      INTEGER, INTENT(IN)  :: NDP
      INTEGER, INTENT(INOUT) :: IKLE(NELEM*NDP)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: MED_ID, TYPE_ELEM_MED
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_CONNECTIVITY_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      CALL CHECK_CALL(IERR,
     &                'GET_MESH_CONNECTIVITY_MED:CONVERT_ELEM_TYPE')
!
!     READ THE CONNECTIVITY TABLE
      CALL MMHCYR(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_CELL,
     &  TYPE_ELEM_MED,MED_NODAL,MED_FULL_INTERLACE,IKLE,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_CONNECTIVITY_MED:MMHCYR')
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_NPOIN_MED (FILE_ID,TYPE_ELEM,NPOIN,IERR)
!
!BRIEF    READS TOTAL NUMBER OF NODES IN A MED FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM      |<--| ELEMENT TYPE IN SLF FORMAT
!| NPOIN          |-->| TOTAL NUMBER OF NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(INOUT) :: NPOIN
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: CHGT,TSF     ! INDICATORS OF MESH MODIF
      INTEGER :: MED_ID, TYPE_ELEM_MED
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_NPOIN_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_NPOIN_MED:CONVERT_ELEM_TYPE')
!
!     READ THE TOTAL NUMBER OF NODES
      CALL MMHNME(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_NODE,
     &  TYPE_ELEM_MED,MED_COORDINATE,MED_NODAL,CHGT,TSF,NPOIN,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_NPOIN_MED:MMHNME')
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_DIMENSION_MED (FILE_ID, NDIM, IERR)
!
!BRIEF    READS THE NUMBER OF DIMENSION OF THE MESH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NDIM           |-->| NUMBER OF DIMENSION OF THE MESH
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(INOUT) :: NDIM
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE)    :: TITLE_MED
      CHARACTER(LEN=MED_NAME_SIZE)    :: DT_UNIT
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_NAME(:)
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_UNIT(:)
      CHARACTER(LEN=MED_COMMENT_SIZE) :: COMMENT
!
      INTEGER :: IMESH             ! MESH DESCRIPTOR
      INTEGER :: NB_DIM_PB         ! NUMBER OF PHYSICAL DIMENSIONS
      INTEGER :: MESH_TYPE         ! TYPE OF THE MESH
      INTEGER :: ORDER             ! SORTING ORDER FOR IT AND DT
      INTEGER :: NCOMP             ! NUMBER OF COMPUTATION STEPS
      INTEGER :: COOR_TYPE         ! TYPE OF COORDINATES
      INTEGER :: NAXIS             ! NUMBER OF AXES
!
!----------------------------------------------------------------------
!
!     ONLY ONE MESH PER FILE FOR NOW
      IMESH = 1
!
      CALL MMHNAX(FILE_ID,IMESH,NAXIS,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_DIMENSION_MED:MMHMII')
      ALLOCATE(COOR_NAME(NAXIS),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_MESH_DIMENSION_MED:COOR_NAME')
      ALLOCATE(COOR_UNIT(NAXIS),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_MESH_DIMENSION_MED:COOR_UNIT')
!
!     READ MESH INFORMATIONS
      CALL MMHMII(FILE_ID,IMESH,TITLE_MED,NB_DIM_PB,NDIM,MESH_TYPE,
     &  COMMENT,DT_UNIT,ORDER,NCOMP,COOR_TYPE,COOR_NAME,COOR_UNIT,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_DIMENSION_MED:MMHMII')
      DEALLOCATE(COOR_NAME)
      DEALLOCATE(COOR_UNIT)
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_COORD_MED(FILE_ID,JDIM,NDIM,NPOIN,COORD_AXE,
     &  IERR)
!
!BRIEF    READS THE COORDINATES OF THE MESH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| JDIM           |<--| DIMENSION NUMBER (X, Y OR Z)
!| NDIM           |<--| NUMBER OF DIMENSION OF THE MESH
!| NPOIN          |<--| TOTAL NUMBER OF NODES
!| COORD_AXE      |-->| COORDINATES OF THE MESH
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID, JDIM, NDIM, NPOIN
      INTEGER, INTENT(OUT) :: IERR
      DOUBLE PRECISION, INTENT(INOUT) :: COORD_AXE(NPOIN)
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: MED_ID
      DOUBLE PRECISION, ALLOCATABLE :: COORD(:)
!
!----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'COORD_MED:GET_MESH_OBJ_FILE')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     TEMPORARY TABLE ALLOCATION
      ALLOCATE(COORD(3*NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_MESH_COORD_MED:COORD')
!
!     READ THE COORDINATES OF THE MESH
      CALL MMHCOR(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_NO_INTERLACE,
     &  COORD,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_COORD_MED:MMHCOR')
!
!     STORE ONLY THE DIMENSION DEFINED BY JDIM
      IF ((JDIM.GE.0).AND.(JDIM.LE.NDIM)) THEN
        COORD_AXE(1:NPOIN)=COORD((JDIM-1)*NPOIN+1:JDIM*NPOIN)
      ELSE
!       ERROR ON JDIM
        IERR = HERMES_WRONG_AXE_ERR
      ENDIF
      DEALLOCATE(COORD)
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_L2G_NUMBERING_MED(FILE_ID, KNOLG, NPOIN, IERR)
!
!BRIEF    READS THE INDEX TABLE OF NODES WHICH CONVERT THE LOCAL
!         NUMBERING TO THE GLOBAL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| KNOLG          |-->| NODES INDEX TABLE FROM LOCAL TO GLOBAL
!| NPOIN          |<--| TOTAL NUMBER OF NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: NPOIN
      INTEGER, INTENT(OUT) :: IERR
      INTEGER, INTENT(INOUT) :: KNOLG(NPOIN)
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: MED_ID
!
!----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_L2G_NUMBERING_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     READ THE NODES INDEX TABLE FROM LOCAL TO GLOBAL
      CALL MMHGNR(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,
     &  KNOLG,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_L2G_NUMBERING_MED:MMHGNR')
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_NPTIR_MED (FILE_ID, NPTIR, IERR)
!
!BRIEF    READS TOTAL NUMBER OF INTERFACE NODES IN A MED FILE
!         ONLY FOR PARALLEL MODE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NPTIR          |-->| NUMBER OF INTERFACE NODES FOR THE SUB-DOMAIN
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(INOUT) :: NPTIR
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      INTEGER :: MED_ID
      CHARACTER(LEN=MED_NAME_SIZE) :: PNAME
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_NPTIR_MED:GET_MED_OBJ')
!
!     INITIALISATION
      NPTIR = 0
      PNAME = 'NPTIR'//CHAR(0)
      CALL MPRIVR(FILE_ID,PNAME,MED_NO_DT,MED_NO_IT,NPTIR,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_NPTIR_MED:MPRIVR')
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_IPOBO_MED(FILE_ID,TYPE_ELEM_BND,NPOIN,NELEBD,
     &  IPOBO,IERR)
!
!BRIEF    DETERMINES THE TABLE IPOBO: 1 IF BOUNDARY NODE, 0 OTHERWISE
!
!WARNING  ONLY WORKS FOR ONE TYPE OF BOUNDARY ELEMENTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM_BND  |<--| BOUNDARY ELEMENT TYPE IN SLF FORMAT
!| NPOIN          |<--| TOTAL NUMBER OF NODES
!| NELEBD         |<--| TOTAL NUMBER OF BOUNDARY ELEMENTS
!| IPOBO          |-->| LOGICAL TABLE FOR BOUNDARY NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID, TYPE_ELEM_BND, NPOIN, NELEBD
      INTEGER, INTENT(INOUT) :: IPOBO(NPOIN)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
!
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: I,J, MED_ID
      INTEGER :: NDP_BND_ELE  ! NUMBER OF NODES PER ELEMENT
      INTEGER :: TYPE_ELEM_MED
      INTEGER, DIMENSION(:), ALLOCATABLE :: BND_IKLE
      INTEGER :: NELEM
!
!-----------------------------------------------------------------------
!
!     INITIALISATION
      DO I = 1, NPOIN
        IPOBO(I) = 0
      ENDDO
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_IPOBO_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM_BND, TYPE_ELEM_MED, IERR)
      CALL CHECK_CALL(IERR,'GET_BND_IPOBO_MED:CONVERT_ELEM_TYPE')
!     
!     Identify bnd element if necessary
      CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_ELEM_BND,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_IPOBO_MED:IDENTIFY_BND_ELMT')
!
!     DETERMINE THE NUMBER OF NODES PER ELEMENT FROM THE TYPE
      CALL NDP_FROM_ELEMENT_TYPE_MED(TYPE_ELEM_MED,NDP_BND_ELE,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_IPOBO_MED:NDP_FROM_ELT_TYPE_MED')
!
!     Get the number of element of the boundary type
      CALL GET_MESH_NELEM_MED(FILE_ID,TYPE_ELEM_BND,NELEM,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_IPOBO_MED:GET_BND_NELEM_MED')
!
!     READ THE ONNECTIVITY TABLE OF THE BOUNDARY ELEMENTS
      ALLOCATE(BND_IKLE(NDP_BND_ELE*NELEM),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_BND_IPOBO_MED:BND_IKLE')
!
      CALL MMHCYR(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_CELL,
     &  TYPE_ELEM_MED,MED_NODAL,MED_NO_INTERLACE,BND_IKLE,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_IPOBO_MED:MMHCYR')
!
!     LOOP ON THE BOUNDARY ELEMENTS TO MODIFY IPOBO
      DO I = 1, NELEM
        ! Skipping non boundary elements
        IF(.NOT.MED_OBJ_TAB(MED_ID)%IS_BND(I)) CYCLE
        DO J=1,NDP_BND_ELE
          IPOBO(BND_IKLE((J-1)*NELEM+I)) = 1
        ENDDO
      ENDDO
!
!     FREE MEMORY
      DEALLOCATE(BND_IKLE)
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_NUMBERING_MED(FILE_ID,TYPE_ELEM_BND,NPTFR,
     & NBOR,IERR)
!
!BRIEF    GET THE TABLE OF THE BOUNDARY NODES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM_BND  |<--| BOUNDARY ELEMENT TYPE IN SLF FORMAT
!| NPTFR          |<--| TOTAL NUMBER OF BOUNDARY NODES
!| NBOR           |-->| LOGICAL TABLE FOR BOUNDARY NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID,NPTFR,TYPE_ELEM_BND
      INTEGER, INTENT(INOUT) :: NBOR(NPTFR)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined(HAVE_MED)
!
      INTEGER :: NELEBD,NDP,I
      INTEGER :: MED_ID,TYPE_ELEM_MED
      INTEGER, ALLOCATABLE :: IKLE(:)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: ISUIV, NOEUD1, NOEUD2
      INTEGER :: IILE,NILE,NPOIN,NELEM
      INTEGER, ALLOCATABLE :: TRAV1(:,:)
      INTEGER, ALLOCATABLE :: KP1BOR(:)
      DOUBLE PRECISION :: SOM1,SOM2,Y2
      DOUBLE PRECISION, PARAMETER :: EPSILO = 1.D-6
      DOUBLE PRECISION, ALLOCATABLE :: X(:),Y(:)
      LOGICAL :: SWAP
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_NUMBERING_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
      IF(ALLOCATED(MED_OBJ_TAB(MED_ID)%NBOR)) THEN
!
        DO I=1,NPTFR
          NBOR(I) = MED_OBJ_TAB(MED_ID)%NBOR(I)
        ENDDO
!
      ELSE
!
!       OTHERWISE COMUTING NBOR
!       CONVERTS TYPE OF ELEMENTS
        CALL CONVERT_ELEM_TYPE(TYPE_ELEM_BND, TYPE_ELEM_MED, IERR)
        CALL CHECK_CALL(IERR,'GET_BND_NUMBERING_MED:CONVERT_ELEM_TYPE')
!     
!       Identify bnd element if necessary
        CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_ELEM_BND,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_IPOBO_MED:IDENTIFY_BND_ELMT')
!
!       DETERMINE THE NUMBER OF NODES PER ELEMENT FROM THE TYPE
        CALL NDP_FROM_ELEMENT_TYPE_MED(TYPE_ELEM_MED,NDP,IERR)
        CALL CHECK_CALL(IERR,
     &                  'GET_BND_NUMBERING_MED:NDP_FROM_ELT_TYPE_MED')
!
!       Get the number of element of the boundary type
        CALL GET_MESH_NELEM_MED(FILE_ID,TYPE_ELEM_BND,NELEM,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_NUMBERING_MED:GET_BND_NELEM_MED')
!
!       READ THE CONNECTIVITY TABLE OF THE ELEMENTS
        ALLOCATE(IKLE(NDP*NELEM),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GET_BND_NUMBERING_MED:BND_IKLE')
!
        CALL MMHCYR(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_CELL,
     &    TYPE_ELEM_MED,MED_NODAL,MED_NO_INTERLACE,IKLE,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_NUMBERING_MED:MMHCYR')
!
!       REORDER THE BOUNDARY POINT TO COMPLY WITH TELEMAC CONVENTION (SEE DOCUMENTATION)
        CALL GET_MESH_NPOIN_MED(FILE_ID,TYPE_ELEM_BND,NPOIN,IERR)
        CALL CHECK_CALL(IERR,
     &                  'GET_BND_NUMBERING_MED:GET_MESH_COORD_MED:X')
!
        ALLOCATE(TRAV1(NPTFR,2),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GET_BND_NUMBERING_MED:TRAV1')
        ALLOCATE(KP1BOR(NPOIN),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GET_BND_NUMBERING_MED:KP1BOR')
!
!       Only take into account the real boundary elements
        NELEBD = 0
        DO I=1,NELEM
          ! Skipping non boundary elements
          IF(.NOT.MED_OBJ_TAB(MED_ID)%IS_BND(I)) CYCLE
          NELEBD = NELEBD + 1
          TRAV1(NELEBD,1) = IKLE(I)
          TRAV1(NELEBD,2) = IKLE(I+NELEM)
        ENDDO
!
!       GET MESH COORDINATES
        ALLOCATE(X(NPOIN),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GET_BND_NUMBERING_MED:X')
        ALLOCATE(Y(NPOIN),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GET_BND_NUMBERING_MED:Y')
        CALL GET_MESH_COORD_MED(FILE_ID,1,2,NPOIN,X,IERR)
        CALL CHECK_CALL(IERR,
     &                  'GET_BND_NUMBERING_MED:GET_MESH_COORD_MED:X')
        CALL GET_MESH_COORD_MED(FILE_ID,2,2,NPOIN,Y,IERR)
        CALL CHECK_CALL(IERR,
     &                  'GET_BND_NUMBERING_MED:GET_MESH_COORD_MED:Y')
!
!       EDGE ARE STORED CONTINUOUSLY
!       IT STARTS WITH SOUTH-WESTERNMOST POINT (OR SOUTHMORE POINT IN CASE OF
!       CONFLICT) IN ORDER TO START ON THE BOUNDARY AND NOT ON A ISLAND
        SOM2 = X(1) + Y(1)
        Y2   = Y(1)
!
        DO I=1,NPTFR
!
          SOM1 = X(TRAV1(I,1)) + Y(TRAV1(I,1))
          IF (ABS(SOM1-SOM2).LE.ABS(EPSILO*SOM1)) THEN
            IF (Y(TRAV1(I,1)).LE.Y2) THEN
              Y2    = Y(TRAV1(I,1))
              SOM2  = SOM1
              ISUIV = I
            ENDIF
          ELSEIF (SOM1.LE.SOM2) THEN
            Y2    = Y(TRAV1(I,1))
            SOM2  = SOM1
            ISUIV = I
          ENDIF
!
        ENDDO
!
        NOEUD1 = TRAV1(ISUIV,1)
        NOEUD2 = TRAV1(ISUIV,2)
        TRAV1(ISUIV,1) = TRAV1(1,1)
        TRAV1(ISUIV,2) = TRAV1(1,2)
        TRAV1(1,1) = NOEUD1
        TRAV1(1,2) = NOEUD2
!
        IILE = 0
        NILE = 1
!
        DO I=2,NPTFR
          SWAP = .FALSE.
!
!         SEACH OF THE EDGE WHOSE FIRST NODE IS IDENTICAL TO THE SECOND ONE
!         OF THE PREVIOUS EDGE
          DO ISUIV=I,NPTFR
!
            IF (TRAV1(ISUIV,1).EQ.TRAV1(I-1,2)) THEN
!
!             PERMUTATION OF EDGE WITH NUMBER I+1 AND ISUIV
              NOEUD1 = TRAV1(ISUIV,1)
              NOEUD2 = TRAV1(ISUIV,2)
              TRAV1(ISUIV,1) = TRAV1(I,1)
              TRAV1(ISUIV,2) = TRAV1(I,2)
              TRAV1(I,1) = NOEUD1
              TRAV1(I,2) = NOEUD2
              KP1BOR(I+NPTFR) = I-1
              KP1BOR(I-1) = I
              SWAP = .TRUE.
              EXIT
!
            ENDIF
!
          ENDDO! ISUIV
          IF(.NOT.SWAP) THEN
            DO ISUIV=I,NPTFR
!          
              IF (TRAV1(ISUIV,2).EQ.TRAV1(I-1,2)) THEN
!          
!               PERMUTATION OF EDGE WITHIN ITSELF
                NOEUD1 = TRAV1(ISUIV,2)
                NOEUD2 = TRAV1(ISUIV,1)
                TRAV1(ISUIV,1) = TRAV1(I,1)
                TRAV1(ISUIV,2) = TRAV1(I,2)
                TRAV1(I,1) = NOEUD1
                TRAV1(I,2) = NOEUD2
                KP1BOR(I+NPTFR) = I-1
                KP1BOR(I-1) = I
                SWAP = .TRUE.
                EXIT
!          
              ENDIF
!          
            ENDDO! ISUIV
          ENDIF
          IF(SWAP) CYCLE
!
!         IF NO FOLLOWING POINT IS FOUND, WE VERIFY THAT THE LAST AND THE FIRST
!         POINTS ARE IDENTICAL. IT MEANS WE FOUND AN ISLAND, SO WE RETURN TO THE
!         GLOBAL LOOP
          IF (TRAV1(NILE,1).NE.TRAV1(I-1,2)) THEN
!
!           OTHERWISE IT IS AN ERROR
            WRITE(*,4500) TRAV1(I-1,2)
4500        FORMAT(1X,'ERROR IN STORING THE EDGE SEGMENTS',/,
     &             1X,'FOR THE NODE ',I5)
            CALL PLANTE(1)
            STOP
          ENDIF
!
          KP1BOR(NILE+NPTFR) = I-1
          KP1BOR(I-1) = NILE
          IILE = IILE+1
          NILE = I
!
        ENDDO! I
!
!       WE VERIFY THAT THE LAST ISLAND IS CLOSED
        IF (TRAV1(NILE,1).NE.TRAV1(NPTFR,2)) THEN
          WRITE(*,5000) TRAV1(NILE,1),TRAV1(NPTFR,2)
5000      FORMAT(1X,'ERROR, THE BOUNDARY IS NOT CLOSED :',/,
     &           1X,'FIRST POINT :',I5,2X,'LAST POINT : ',I5)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        KP1BOR(NILE+NPTFR) = NPTFR
        KP1BOR(NPTFR) = NILE
!
        WRITE(*,5500) NPTFR
        WRITE(*,5600) IILE
 5500   FORMAT(1X,'NUMBER OF BOUNDARY POINTS      : ',I5)
 5600   FORMAT(1X,'NUMBER OF ISLANDS              : ',I5)
!
!       THE TABLE NBOR IS FILLED AND THE COLOR OF THE BOUNDARY NODES IS STORED
!       IN THE TABLE NCOLFR
        ALLOCATE(MED_OBJ_TAB(MED_ID)%NBOR(NPTFR))
        DO I=1,NPTFR
          NBOR(I      ) = TRAV1(I,1)
          MED_OBJ_TAB(MED_ID)%NBOR(I) = NBOR(I)
        ENDDO


        DEALLOCATE(IKLE)
        DEALLOCATE(X,Y)
        DEALLOCATE(TRAV1)
        DEALLOCATE(KP1BOR)
      ENDIF
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_NELEM_MED (FILE_ID, TYPE_ELEM, BND_NELEM, IERR)
!
!BRIEF    READS THE TOTAL NUMBER OF BOUNDARY ELEMENTS FOR A GIVEN TYPE
!         OF BOUNDARY ELEMENT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM      |<--| TYPE OF ELEMENT IN SLF FORMAT
!| BND_NELEM      |-->| NUMBER OF BOUNDARY ELEMENTS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(INOUT) :: BND_NELEM
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      INTEGER :: MED_ID, TYPE_ELEM_MED
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_NELEM_MED:MMHCYR')
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      CALL CHECK_CALL(IERR,'GET_BND_NELEM_MED:MMHCYR')
!
!     Identify bnd element if necessary
      CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_ELEM,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_NELEM_MED:IDENTIFY_BND_ELMT')
!
!     Compute the total number of element
      BND_NELEM = COUNT(MED_OBJ_TAB(MED_ID)%IS_BND)
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_CONNECTIVITY_MED (FILE_ID, TYPE_ELEM, NELEBD,
     &                              NDP, BND_IKLE, IERR)
!
!BRIEF    READS THE CONNECTIVITY TABLE OF BOUNDARY ELEMENTS FOR A GIVEN
!         TYPE OF BOUNDARY ELEMENT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM      |<--| TYPE OF THE ELEMENT IN MED FORMAT
!| NELEBD         |<--| TOTAL NUMBER OF ELEMENTS
!| NDP            |<--| NUMBER OF NODES PER ELEMENT
!| BND_IKLE       |-->| CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: NELEBD
      INTEGER, INTENT(IN)  :: NDP
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(INOUT) :: BND_IKLE(NELEBD*NDP)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: TYPE_ELEM_MED,MED_ID
      INTEGER,ALLOCATABLE :: IKLE(:)
      INTEGER :: IBND,NELEM,I
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_NELEM_MED:MMHCYR')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     CONVERT
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      CALL CHECK_CALL(IERR,'GET_BND_CONNECTIVITY_MED:MMHCYR')
!     
!     Identify bnd element if necessary
      CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_ELEM,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_CONNECTIVITY_MED:IDENTIFY_BND_ELMT')
!
      CALL GET_MESH_NELEM_MED(FILE_ID,TYPE_ELEM,NELEM,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_CONNECTIVITY_MED:GET_MESH_NELEM')
      ALLOCATE(IKLE(NELEM*NDP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_BND_CONNECTIVITY_MED:IKLE')
!     Read the connectivity table of the elements
      CALL MMHCYR(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_CELL,
     &  TYPE_ELEM_MED,MED_NODAL,MED_NO_INTERLACE,IKLE,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_CONNECTIVITY_MED:MMHCYR')
!
      ! Compute the boundary elements connectivity table
      IBND = 0
      DO I=1,NELEM
        ! Skipping no boundary elements
        IF(.NOT.MED_OBJ_TAB(MED_ID)%IS_BND(I)) CYCLE
        IBND = IBND + 1
        BND_IKLE(IBND) = IKLE(I)
        BND_IKLE(IBND+NELEBD) = IKLE(I+NELEM)
      ENDDO
      DEALLOCATE(IKLE)
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_GRP_VALUE_MED (FILE_ID, GRP_NAME, VALUE, IERR)
!
!BRIEF    GIVES THE VALUE OF THE BOUNDARY CONDITIONS ASSOCIATED TO A
!         FAMILY NAME
!
!WARNING  THIS FUNCTION COULD BE OPTIMIZED BY READING ALL THE INFO AT
!         THE BEGINNING. HOWEVER THE COST IT LOW AS THE SIZE OF THE FILE
!         IS LIMITED (I.E. TOTAL NUMBER OF GROUP
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| GRP_NAME       |<--| NAME OF THE FAMILY
!| VALUE          |-->| VALUE OF THE BOUNDARY CONDITIONS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID
      CHARACTER(LEN=16), INTENT(IN)  :: GRP_NAME
      INTEGER, INTENT(INOUT) :: VALUE
      INTEGER, INTENT(OUT) :: IERR
!
      INTEGER :: NCLI,MED_ID,I1,I2,I3,I4,I
      INTEGER :: NGROUP
      CHARACTER(LEN=16) :: TEMP_NAME
      LOGICAL :: FOUND
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_NELEM_MED:MMHCYR')
      NCLI = MED_OBJ_TAB(MED_ID)%NCLI
!
!     INITIALIZATION
      FOUND = .FALSE.
      IERR = 0
      REWIND(NCLI)
!
!     LOOP ON THE INFO IN THE FILE
      READ(UNIT=NCLI,FMT=*,IOSTAT=IERR) NGROUP
      CALL CHECK_CALL(IERR,'GET_BND_FAM_VALUE:READ')
      DO I=1,NGROUP
        READ(UNIT=NCLI,FMT=*,IOSTAT=IERR) I1,I2,I3,I4,TEMP_NAME
        CALL CHECK_CALL(IERR,'GET_BND_FAM_VALUE:READ')
!
!       IF THE FAMILY IS FOUND, THE VALUE IS STORED
        IF (TEMP_NAME .EQ. GRP_NAME) THEN
          FOUND = .TRUE.
          VALUE = I1*1000+I2*100+I3*10+I4
          EXIT
        ENDIF
      ENDDO
!
      IF(.NOT.FOUND) IERR = HERMES_UNKNOWN_GROUP_ERR
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_FAMILY_MED (FILE_ID, TYPE_BND_ELEM, NELEBD,
     &  FAMILY, IERR)
!
!BRIEF    CREATE THE FAMILY NAME FOR EACH BOUNDARY ELEMENT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_BND_ELEM  |<--| TYPE OF THE BOUNDARY ELEMENT
!| NELEBD         |<--| TOTAL NUMBER OF BOUNDARY ELEMENTS
!| FAMILY         |-->| TABLE OF BOUNDARY ELEMENT'S FAMILY NAME
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID, TYPE_BND_ELEM, NELEBD
      INTEGER, DIMENSION(NELEBD), INTENT(INOUT) :: FAMILY
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER ::  IELEM, IGRP
      INTEGER :: MED_ID, TYPE_ELEM_MED, NB_FAMILY
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUM_FAMILY
      INTEGER :: NELEM, IBND
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_FAMILY_MED:GET_MED_OBJ_FILE')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_BND_ELEM, TYPE_ELEM_MED, IERR)
      CALL CHECK_CALL(IERR,'GET_BND_FAMILY_MED:CONVERT_ELEM_TYPE')
!     
!     Identify bnd element if necessary
      CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_BND_ELEM,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_FAMILY_MED:IDENTIFY_BND_ELMT')
!
!     Get the number of elements
      CALL GET_MESH_NELEM_MED(FILE_ID,TYPE_BND_ELEM,NELEM,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_FAMILY_MED:GET_MESH_NELEM')
!
!     READ THE FAMILY NUMBER FOR EACH ELEMENT
      ALLOCATE(NUM_FAMILY(NELEM),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'NUM_FAMILY')
      CALL MMHFNR(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_CELL,
     &            TYPE_ELEM_MED,NUM_FAMILY,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_FAMILY_MED:MMHFNR')
!
!     READ THE TOTAL NUMBER OF FAMILY
      CALL MFANFA(FILE_ID,MNAME,NB_FAMILY,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_FAMILY_MED:MFANFA')
!
      IBND = 0
      DO IELEM = 1, NELEM
!
        IF(.NOT.MED_OBJ_TAB(MED_ID)%IS_BND(IELEM)) CYCLE
!       LOOP ON FAMILY
        IBND = IBND + 1
        DO IGRP = 1, NB_FAMILY
          ! Skipping non boundary families
          IF(MED_OBJ_TAB(MED_ID)%BND_FAM(IGRP,2).EQ.0) CYCLE
          IF (NUM_FAMILY(IELEM).EQ.
     &        MED_OBJ_TAB(MED_ID)%BND_FAM(IGRP,1)) THEN
            FAMILY(IBND) = MED_OBJ_TAB(MED_ID)%BND_FAM(IGRP,2)
          ENDIF
        ENDDO
      ENDDO
!
!     FREE MEMORY
      DEALLOCATE(NUM_FAMILY)
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_NPOIN_MED(FILE_ID,TYPE_BND_ELEM,NPTFR,IERR)
!
!BRIEF    DETERMINE THE NUMBER OF BOUNDARY POINTS
!
!WARNING  FIND ANOTHER SOLUTION THIS ONE IS WAY TOO EXPENSIVE
!         SEE WITH MED PEOPLE IF THER IS ANOTHER WAY
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_BND_ELEM  |<--| TYPE OF THE BOUNDARY ELEMENT
!| NPTFR          |-->| NUMBER OF BOUNDARY NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: FILE_ID, TYPE_BND_ELEM
        INTEGER, INTENT(INOUT) :: NPTFR
        INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: CHGT,TSF     ! INDICATORS OF MESH MODIF
      INTEGER :: MED_ID, TYPE_BND_ELEM_MED
      INTEGER :: NELEBD, NPOIN, I, J, NDP
      INTEGER, ALLOCATABLE :: IKLE(:)
      LOGICAL, ALLOCATABLE :: IS_BND(:)
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_NPOIN_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_BND_ELEM, TYPE_BND_ELEM_MED, IERR)
      CALL CHECK_CALL(IERR,'GET_BND_NPOIN_MED:CONVERT_ELEM_TYPE')
!
!     GET THE NUMBER OF BOUNDARY ELEMENTS
      CALL GET_BND_NELEM_MED(FILE_ID,TYPE_BND_ELEM,NELEBD,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_NPOIN_MED:GET_BND_NELEM_MED')
!
!     CONTINUE ONLY IF BOUNDARY ELEMENTS EXIST
      IF(NELEBD.GT.0) THEN
!
!       GET THE NUMBER OF POINT IN THE MESH
        CALL MMHNME(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_NODE,
     &            TYPE_BND_ELEM_MED,MED_COORDINATE,MED_NODAL,
     &            CHGT,TSF,NPOIN,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_NPOIN_MED:MMHNME')
!
!       GET THE NUMBER OF POINTS PER ELEMENT
        CALL GET_NODES_PER_ELEMENT(TYPE_BND_ELEM,NDP)
!
!       ALLOCATE CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
        ALLOCATE(IKLE(NELEBD*NDP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GET_BND_NPOIN_MED:IKLE')
!
!       GET THE CONNECTIVITY TABLE FOR THE BOUNDARY ELEMENTS
        CALL GET_BND_CONNECTIVITY_MED(FILE_ID, TYPE_BND_ELEM, NELEBD,
     &                        NDP, IKLE, IERR)
        CALL CHECK_CALL(IERR,
     &                  'GET_BND_NPOIN_MED:GET_BND_CONNECTIVITY_MED')
!
        ALLOCATE(IS_BND(NPOIN),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GET_BND_NPOIN_MED:IKLE')
!
!       INITIALIZE
        DO I=1,NPOIN
          IS_BND(I)  = .FALSE.
        ENDDO
!
!       Loop on all the points of the boundary elements set their value to true
        DO I=1,NELEBD
          DO J=1,NDP
            IS_BND(IKLE(I+(J-1)*NELEBD)) = .TRUE.
          ENDDO
        ENDDO
!
!       COUNT THE NUMBER OF TRUE THIS WILL GIVE US THE NUMBER OF BOUNDARY POINTS
        NPTFR = COUNT(IS_BND.EQV..TRUE.)
        DEALLOCATE(IKLE)
        DEALLOCATE(IS_BND)
!
      ELSE
        NPTFR = 0
      ENDIF
!
#else
!
!       MED LIBRARY NOT LOADED
        IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_VALUE_MED(FILE_ID,TYPE_BND_ELEM,NELEBD,
     &                             LIHBOR,LIUBOR,LIVBOR,TRAC,LITBOR,
     &                             NPTFR,NBOR,IERR)
!
!BRIEF    DETERMINE THE NUMBER OF BOUNDARY POINTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_BND_ELEM  |<--| TYPE OF THE BOUNDARY ELEMENT
!| NELEBD         |<--| NUMBER OF BOUNDARY ELEMENTS
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| TRAC           |-->| IF YES, THERE ARE TRACERS
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| NPTFR          |<--| NUMBER OF BOUNDARY NODES
!| NBOR           |<--| LOGICAL TABLE FOR BOUNDARY NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYPE_BND_ELEM
        INTEGER, INTENT(IN) :: NELEBD
        INTEGER, INTENT(IN) :: NPTFR
        INTEGER, INTENT(INOUT) :: LIUBOR(NPTFR),LIVBOR(NPTFR)
        INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR),LITBOR(*)
        LOGICAL, INTENT(IN)    :: TRAC
        INTEGER, INTENT(IN) :: NBOR(NPTFR)
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER, ALLOCATABLE :: FAMILY(:)
        INTEGER, ALLOCATABLE :: IKLE(:)
        INTEGER :: NDP,IGRP,MED_ID
        INTEGER :: INODE,IELEM,J,K,IPTFR
        INTEGER :: TYPE_ELEM_MED
!
!-----------------------------------------------------------------------
!
!       GET INFO FROM THE MED FILE OBJECT
        CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_VALUE:GET_MED_OBJ')
!       CONVERTS TYPE OF ELEMENTS
        CALL CONVERT_ELEM_TYPE(TYPE_BND_ELEM, TYPE_ELEM_MED, IERR)
        CALL CHECK_CALL(IERR,'GET_BND_VALUE_MED:CONVERT_ELEM_TYPE')
!
!       DETERMINE THE NUMBER OF NODES PER ELEMENT FROM THE TYPE
        CALL NDP_FROM_ELEMENT_TYPE_MED(TYPE_ELEM_MED,NDP,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_VALUE_MED:NDP_FROM_ELT_TYPE_MED')
!     
!       Identify bnd element if necessary
        CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_BND_ELEM,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_VALUE_MED:IDENTIFY_BND_ELMT')
!
        ALLOCATE(FAMILY(NELEBD),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GET_BND_VALUE_MED:FAMILY')
        CALL GET_BND_FAMILY_MED(FILE_ID,TYPE_BND_ELEM,NELEBD,
     &                          FAMILY,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_VALUE_MED:GET_BND_FAMILY_MED')
!
        ALLOCATE(IKLE(NELEBD*NDP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GET_BND_VALUE_MED:IKLE')
!
!       GET THE CONNECTIVITY TABLE
        CALL GET_BND_CONNECTIVITY_MED(FILE_ID, TYPE_BND_ELEM, NELEBD,
     &                                NDP, IKLE, IERR)
        CALL CHECK_CALL(IERR,'GET_BND_VALUE_MED:GET_BND_CONN_MED')
!
        DO IPTFR=1,NPTFR
          LIHBOR(IPTFR) = 0
          LIUBOR(IPTFR) = 0
          LIVBOR(IPTFR) = 0
          IF (TRAC) THEN
            LITBOR(IPTFR) =  0
          ENDIF
        ENDDO
!       LOOP ON ALL ELEMENTS
        DO IELEM=1,NELEBD
!
!         LOOP ON NODES
          DO J=1,NDP
!
            INODE = IKLE(IELEM+(J-1)*NELEBD)
!
!           GET THE NODE BOUNDARY NUMBERING
            IPTFR = 0
            DO K=1,NPTFR
              IF(NBOR(K).EQ.INODE) THEN
                IPTFR = K
                EXIT
              ENDIF
            ENDDO
!
!           ERROR IF THE NODE IS NOT FOUND
            IF(IPTFR.EQ.0) THEN
              IERR = HERMES_BND_POINT_NOT_FOUND_ERR
              RETURN
            ENDIF
!
!           STORE VALUE OF THE GIVEN NODE
            IGRP = FAMILY(IELEM)
            LIHBOR(IPTFR) = MAX(MED_OBJ_TAB(MED_ID)%BND_GRP_VAL(IGRP,1),
     &                          LIHBOR(IPTFR))
            LIUBOR(IPTFR) = MAX(MED_OBJ_TAB(MED_ID)%BND_GRP_VAL(IGRP,2),
     &                          LIUBOR(IPTFR))
            LIVBOR(IPTFR) = MAX(MED_OBJ_TAB(MED_ID)%BND_GRP_VAL(IGRP,3),
     &                          LIVBOR(IPTFR))
            IF (TRAC) THEN
              LITBOR(IPTFR) =  
     &                 MAX(MED_OBJ_TAB(MED_ID)%BND_GRP_VAL(IGRP,4),
     &                 LITBOR(IPTFR))
            ENDIF
!
          ENDDO
        ENDDO
!
!       FREE MEMORY
        DEALLOCATE(FAMILY)
        DEALLOCATE(IKLE)
!
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_DATA_NVAR_MED (FILE_ID, NVAR, IERR)
!
!BRIEF    READS THE NUMBER OF DATA IN A MED FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NVAR           |-->| NUMBER OF DATA
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(INOUT) :: NVAR
      INTEGER, INTENT(OUT) :: IERR
!
!-----------------------------------------------------------------------
!
#if defined (HAVE_MED)
!
!     READ THE NUMBER OF DATA
      CALL MFDNFD(FILE_ID,NVAR,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_NVAR_MED:MFDNFD')
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_DATA_VAR_LIST_MED (FILE_ID, NVAR, VAR_LIST,
     &                                  UNIT_LIST, IERR)
!
!BRIEF    READS THE LIST OF THE DATA NAME
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NVAR           |<--| NUMBER OF DATA
!| VAR_LIST       |-->| LIST OF THE DATA NAME
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: NVAR
      CHARACTER(LEN=16), DIMENSION(:), INTENT(INOUT) :: VAR_LIST
      CHARACTER(LEN=16), DIMENSION(:), INTENT(INOUT) :: UNIT_LIST
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: VAR_NAME, MNAME
      CHARACTER(LEN=MED_SNAME_SIZE) :: CUNIT, CNAME, DTUNIT
!
      INTEGER :: MED_ID, I, ITYPE, NSTEP, IMESH
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_VAR_LIST_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     LOOP ON THE VARIABLES
      DO I = 1, NVAR
!
!       READ THE NAME OF THE VARIABLE
        CALL MFDFDI(FILE_ID,I,VAR_NAME,MNAME,IMESH,ITYPE,
     &    CNAME,CUNIT,DTUNIT,NSTEP,IERR)
        CALL CHECK_CALL(IERR,'GET_DATA_VAR_LIST_MED:MFDFDI')
!
!       CONVERT MED FORMAT TO SLF
        VAR_LIST(I) = VAR_NAME
        UNIT_LIST(I) = CNAME
      ENDDO
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_DATA_NTIMESTEP_MED (FILE_ID, NTIMESTEP, IERR)
!
!BRIEF    READS THE NUMBER OF TIME STEP FOR A GIVEN VARIABLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NTIMESTEP      |-->| NUMBER OF TIME STEP
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(INOUT) :: NTIMESTEP
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: VAR_NAME_MED, MNAME
      CHARACTER(LEN=MED_SNAME_SIZE) :: CUNIT,CNAME,DTUNIT
!
      INTEGER :: ITYPE, MED_ID, IMESH, NVAR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_NTIMESTEP_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     READ THE NUMBER OF VARIABLES
      CALL MFDNFD(FILE_ID,NVAR,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_NTIMESTEP_MED:MFDNFD')
!
      IF(NVAR.NE.0) THEN
!
!       READ THE NUMBER OF TIME STEP FROM THE FIRST VARIABLE AS ALL
!       VARIABLE HAVE THE SAME NUMBER OF TIMESTEP
        CALL MFDFDI(FILE_ID,1,VAR_NAME_MED,MNAME,IMESH,
     &    ITYPE,CUNIT,CNAME,DTUNIT,NTIMESTEP,IERR)
        CALL CHECK_CALL(IERR,'GET_DATA_NTIMESTEP_MED:MFDFDI')
      ELSE

        NTIMESTEP = 0
      ENDIF
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_DATA_TIME_MED (FILE_ID, RECORD, TIME, IERR)
!
!BRIEF    READS PHYSICAL TIME FOR A GIVEN ITERATION NUMBER
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| RECORD         |<--| ITERATION NUMBER
!| TIME           |-->| PHYSICAL TIME
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID, RECORD
      DOUBLE PRECISION, INTENT(INOUT) :: TIME
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: VAR_NAME_MED
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
      CHARACTER(LEN=MED_SNAME_SIZE) :: CNAME,CUNIT,DTUNIT
      INTEGER :: N, MED_ID, LMESH
      INTEGER :: FTYPE
      INTEGER :: DT,IT
      INTEGER :: MED_ITER
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_TIME_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     GET THE NAME OF THE FIRST FIELD TO HAVE ACCESS TO THE TIME
      CALL MFDFDI(FILE_ID,1,VAR_NAME_MED,MNAME,LMESH,FTYPE,
     &            CNAME,CUNIT,DTUNIT,N,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_TIME_MED:MFDFDI')
!
!     READ THE PHYSICAL TIME TIME
      DT=MED_NO_DT
      IT=MED_NO_IT
      ! Iterations start from 1 and records start from 0
      MED_ITER = RECORD + 1
      CALL MFDCSI(FILE_ID,VAR_NAME_MED,MED_ITER,DT,IT,
     &            TIME,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_TIME_MED:MFDCSI')
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_DATA_VALUE_MED (FILE_ID,RECORD,VAR_NAME,
     &  RES_VALUE,N,IERR)
!
!BRIEF    READS DATA VALUES FOR A GIVEN ITERATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| RECORD         |<--| ITERATION NUMBER
!| VAR_NAME       |<--| NAME OF THE DATA
!| RES_VALUE      |-->| VECTOR WITH THE VALUE OF THE VARIABLE
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID, RECORD, N
      CHARACTER(LEN=16), INTENT(IN) :: VAR_NAME
      DOUBLE PRECISION, INTENT(INOUT) :: RES_VALUE(N)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: VAR_NAME_MED
!
      INTEGER :: MED_ID
      INTEGER :: LMESH, ITYPE, NSTEP
      CHARACTER(LEN=MED_SNAME_SIZE) :: CNAME,CUNIT,DTUNIT
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
      INTEGER :: NCOMP
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_VALUE_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     CONVERT SLF FORMAT TO MED
      VAR_NAME_MED = TRIM(VAR_NAME)//CHAR(0)
!
!     CHECKING THAT THE VARIABLE IS THE FILE
      CALL MFDNCN(FILE_ID,VAR_NAME_MED,NCOMP,IERR)
      IF(IERR.NE.0) THEN
        IERR = HERMES_VAR_UNKNOWN_ERR
        RETURN
      ENDIF
!
!     CHECKING THAT THE RECORD IS A VALID ONE (BETWEEN 0 AND NTIMESTEP-1)
!     READ THE NAME OF THE VARIABLE
      CALL MFDFIN(FILE_ID,VAR_NAME_MED,MNAME,LMESH,ITYPE,
     &  CNAME,CUNIT,DTUNIT,NSTEP,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_VALUE_MED:MFDFIN')
      IF(RECORD.GE.NSTEP.OR.RECORD.LT.0) THEN
        IERR = HERMES_RECORD_UNKNOWN_ERR
        RETURN
      ENDIF
!
!     READ THE VALUE FOR THIS VARIABLE AT THIS TIMESTEP
      CALL MFDRVR(FILE_ID,VAR_NAME_MED,RECORD,MED_NO_IT,MED_NODE,
     &  MED_NONE,MED_NO_INTERLACE,1,RES_VALUE,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_VALUE_MED:MFDRVR')
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE SET_HEADER_MED(FILE_ID,TITLE,IERR)
!
!BRIEF    WRITES THE TITLE OF THE MESH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TITLE          |<--| TITLE OF THE MESH
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      CHARACTER(LEN=80), INTENT(IN)  :: TITLE
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_COMMENT_SIZE) :: TITLE_MED
      INTEGER :: MED_ID
!
!-----------------------------------------------------------------------
!
!     CONVERSION FROM SLF FORMAT TO MED
      TITLE_MED = TITLE
!
!     WRITES THE TITLE OF THE MESH
      CALL MFICOW(FILE_ID,TITLE_MED,IERR)
      CALL CHECK_CALL(IERR,'SET_HEADER_MED:MFICOW')
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'SET_HEADER_MED:GET_MED_OBJ')
      MED_OBJ_TAB(MED_ID)%MESH_NAME = 'MESH'
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE SET_MESH_MED(FILE_ID,NB_DIM_PB,NB_DIM_MESH,TYPE_ELEM,
     &  NDP,NPTFR,NPTIR,NELEM,NPOIN,IKLE,IPOBO,KNOLG,COORD,IERR)
!
!BRIEF    WRITES A MESH IN A MED FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NB_DIM_PB      |<--| NUMBER OF DIMENSION OF THE PHYSICAL DOMAIN
!| NB_DIM_MESH    |<--| NUMBER OF DIMENSION OF THE MESH
!| TYPE_ELEM      |<--| TYPE OF THE ELEMENT IN SLF FORMAT
!| NDP            |<--| NUMBER OF POINTS PER ELEMENT
!| NPTFR          |<--| NUMBER OF BOUNDARY NODES
!| NPTIR          |<--| NUMBER OF INTERFACE NODES FOR THE SUB-DOMAIN
!| NELEM          |<--| TOTAL NUMBER OF ELEMENTS
!| NPOIN          |<--| TOTAL NUMBER OF NODES
!| IKLE           |<--| CONNECTIVITY TABLE
!| IPOBO          |<--| LOGICAL TABLE FOR BOUNDARY NODES
!| KNOLG          |<--| NODES INDEX TABLE FROM LOCAL TO GLOBAL
!| COORD          |<--| COORDINATES OF THE NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID, NB_DIM_PB, NB_DIM_MESH
      INTEGER, INTENT(IN) :: NELEM, NPOIN, TYPE_ELEM, NDP, NPTIR, NPTFR
      INTEGER, INTENT(IN) :: IKLE(NDP*NELEM),IPOBO(NPOIN), KNOLG(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: COORD(NB_DIM_MESH*NPOIN)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE)    :: MNAME
      CHARACTER(LEN=MED_NAME_SIZE)    :: DT_UNIT_MED
      CHARACTER(LEN=MED_NAME_SIZE)    :: FAM_ZERO
      CHARACTER(LEN=MED_NAME_SIZE)    :: PNAME
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_NAME_MED(:)
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_UNIT_MED(:)
      CHARACTER(LEN=MED_SNAME_SIZE)   :: COOR_X, COOR_Y, COOR_Z
      CHARACTER(LEN=MED_SNAME_SIZE)   :: COOR_UNIT
      CHARACTER(LEN=MED_COMMENT_SIZE) :: COMMENT
!
      INTEGER :: IFAM, MED_ID, TYPE_ELEM_MED,I
!
!-----------------------------------------------------------------------
!
!     INITIALISE COORDINATE UNIT AND NAME
      COMMENT = 'TELEMAC GENERATED MESH'
      COOR_X = 'X'//CHAR(0)
      COOR_Y = 'Y'//CHAR(0)
      COOR_Z = 'Z'//CHAR(0)
      COOR_UNIT = 'M'//CHAR(0)
      ALLOCATE(COOR_NAME_MED(NB_DIM_PB),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'COOR_NAME_MED')
      ALLOCATE(COOR_UNIT_MED(NB_DIM_PB),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'COOR_UNIT_MED')
      COOR_NAME_MED(1) = COOR_X
      COOR_NAME_MED(2) = COOR_Y
      IF (NB_DIM_PB.EQ.3) COOR_NAME_MED(3) = COOR_Z
      DO I=1,NB_DIM_PB
        COOR_UNIT_MED(I) = COOR_UNIT
      ENDDO
!
!     INITIALISE TIME UNIT
      DT_UNIT_MED = 'S'//CHAR(0)
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'SET_MESH_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM,TYPE_ELEM_MED,IERR)
      CALL CHECK_CALL(IERR,'SET_MESH_MED:CONVERT_ELEM_TYPE')
!
!     CREATES A MESH IN THE MED FILE
      CALL MMHCRE(FILE_ID,MNAME,NB_DIM_PB,NB_DIM_MESH,
     &  MED_UNSTRUCTURED_MESH,COMMENT,DT_UNIT_MED,MED_SORT_DTIT,
     &  MED_CARTESIAN,COOR_NAME_MED,COOR_UNIT_MED,IERR)
      CALL CHECK_CALL(IERR,'SET_MESH_MED:MMHCRE')
      DEALLOCATE(COOR_NAME_MED)
      DEALLOCATE(COOR_UNIT_MED)
!
!     WRITES NODE COORDINATES
      CALL MMHCOW(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,0.D0,
     &  MED_NO_INTERLACE,NPOIN,COORD,IERR)
      CALL CHECK_CALL(IERR,'SET_MESH_MED:MMHCOW')
!
!     WRITES CONNECTIVITY TABLE
      CALL MMHCYW(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,0.0,MED_CELL,
     &  TYPE_ELEM_MED,MED_NODAL,MED_NO_INTERLACE,NELEM,IKLE,IERR)
      CALL CHECK_CALL(IERR,'SET_MESH_MED:MMHCYW')
!
!     LOOP ON INTERFACE NODES
      IF(NPTIR.NE.0) THEN
!
!       WRITES THE GLOBAL NUMBERING
        CALL MMHGNW(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_NODE,
     &              MED_NONE,NPOIN,KNOLG,IERR)
        CALL CHECK_CALL(IERR,'SET_MESH_MED:MMHGNW')
!
!       WRITES THE NUMBER OF INTERFACE POINT AS A SCALAR PARAMETER
        PNAME = 'NPTIR'//char(0)
        COMMENT = 'Number of interface points'//char(0)
!
!       CREATES THE PARAMETER
        CALL MPRCRE(FILE_ID,PNAME,MED_INT,COMMENT,DT_UNIT_MED,IERR)
        CALL CHECK_CALL(IERR,'SET_MESH_MED:MPRCRE')
!
!       SETS THE VALUE
        CALL MPRIVW(FILE_ID,PNAME,MED_NO_DT,MED_NO_IT,0.0,NPTIR,IERR)
        CALL CHECK_CALL(IERR,'SET_MESH_MED:MPRIVW')
      ENDIF
!
!     CREATE FAMILLY ZERO
!     DEFAULT FAMILY NEEDED BY MED FILE
      FAM_ZERO = 'FAMILY_ZERO'//CHAR(0)
      IFAM = 0
      CALL MFACRE(FILE_ID,MNAME,FAM_ZERO,IFAM,0,' ',IERR)
      CALL CHECK_CALL(IERR,'SET_MESH_MED:MFACRE')
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE ADD_DATA_MED (FILE_ID, VAR_NAME, TIME, RECORD,
     &  VAR_VALUE, N, IERR)
!
!BRIEF    WRITE DATA VALUES FOR A GIVEN VARIABLE
!         ONLY FOR VALUES WITH DOUBLE PRECISION, ONE COMPONENT AND VALUE
!         ON THE NODES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| VAR_NAME       |<--| NAME OF THE DATA
!| TIME           |<--| PHYSICAL TIME
!| RECORD         |<--| ITERATION NUMBER
!| VAR_VALUE      |<--| TABLE OF VARIABLE VALUES
!| N              |<--| NUMBER OF ELEMENTS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID, N
      INTEGER, INTENT(IN) :: RECORD
      CHARACTER(LEN=32), INTENT(IN) :: VAR_NAME
      DOUBLE PRECISION, INTENT(IN) :: TIME
      DOUBLE PRECISION, INTENT(IN) :: VAR_VALUE(N)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME, VAR_NAME_MED
      CHARACTER(LEN=MED_SNAME_SIZE) :: VAR_COMP_MED, UNIT_VAR_MED
      CHARACTER(LEN=MED_SNAME_SIZE) :: UNIT_TIME_MED
      INTEGER :: NB_COMP
      INTEGER :: MED_ID
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
      NB_COMP = 1! ONLY ONE COMPONENT
!
!     CONVERT SLF FORMAT TO MED
      VAR_NAME_MED  = TRIM(VAR_NAME(1:16))//CHAR(0)
      VAR_COMP_MED  = TRIM(VAR_NAME(1:16))//CHAR(0)
      UNIT_VAR_MED  = TRIM(VAR_NAME(17:32))//CHAR(0)
      UNIT_TIME_MED = 'S'//CHAR(0)
!
      IF(RECORD.EQ.0) THEN
!       CREATE A NEW VARIABLE (DOUBLE PRECISION WITH ONE COMPONENT)
        CALL MFDCRE(FILE_ID,VAR_NAME_MED,MED_FLOAT64,NB_COMP,
     &    VAR_COMP_MED,UNIT_VAR_MED,UNIT_TIME_MED,MNAME,IERR)
        CALL CHECK_CALL(IERR,'ADD_DATA_MED:MFDCRE')
      ENDIF
!
!     WRITE DATA VALUES FOR A GIVEN VARIABLE
      CALL MFDRVW(FILE_ID,VAR_NAME_MED,RECORD,MED_NO_IT,TIME,MED_NODE,
     &  MED_NONE,MED_NO_INTERLACE,NB_COMP,N,VAR_VALUE,IERR)
      CALL CHECK_CALL(IERR,'ADD_DATA_MED:MFDRVW')
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!
!-----------------------------------------------------------------------
!
      SUBROUTINE SET_BND_MED(FILE_ID,TYPE_BND_ELT,NELEBD,NDP,IKLE_BND,
     &                       LIHBOR,LIUBOR,LIVBOR,LITBOR,IERR)
!
!BRIEF    WRITE DATA VALUES FOR A GIVEN VARIABLE
!         ONLY FOR VALUES WITH DOUBLE PRECISION, ONE COMPONENT AND VALUE
!         ON THE NODES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_BND_ELT   |<--| TYPE OF THE BOUNDARY ELEMENT
!| NELEBD         |<--| NUMBER OF BOUNDARY ELEMENT
!| NDP            |<--| NUMBER OF NODES PER ELEMENT
!| IKLE           |<--| CONNECTIVITY TABLE OF THE BOUNDARY ELEMENTS
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER,          INTENT(IN)  :: FILE_ID
      INTEGER,          INTENT(IN)  :: TYPE_BND_ELT
      INTEGER,          INTENT(IN)  :: NELEBD
      INTEGER,          INTENT(IN)  :: NDP
      INTEGER,          INTENT(IN)  :: IKLE_BND(NELEBD*NDP)
      INTEGER,          INTENT(IN)  :: LIUBOR(NELEBD),LIVBOR(NELEBD)
      INTEGER,          INTENT(IN)  :: LIHBOR(NELEBD),LITBOR(NELEBD)
      INTEGER,          INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE)    :: MNAME
      INTEGER :: MED_ID, TYPE_BND_ELEM_MED
      CHARACTER(LEN=MED_LNAME_SIZE) GRP_NAME
      CHARACTER(LEN=MED_NAME_SIZE) FAM_NAME
      INTEGER :: NCLI,I,VAL
      INTEGER,ALLOCATABLE :: FAM_NUM(:)
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'SET_BND_MED:GET_MED_OBJ')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
      NCLI = MED_OBJ_TAB(MED_ID)%NCLI
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_BND_ELT,TYPE_BND_ELEM_MED,IERR)
      CALL CHECK_CALL(IERR,'SET_BND_MED:CONVERT_ELEM_TYPE')
!
!     WRITES CONNECTIVITY TABLE
      CALL MMHCYW(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,0.0,MED_CELL,
     &  TYPE_BND_ELEM_MED,MED_NODAL,MED_NO_INTERLACE,NELEBD,
     &  IKLE_BND,IERR)
      CALL CHECK_CALL(IERR,'SET_MESH_MED:MMHCYW')
!
!     CREATING THE FAMILIES FOR EACH BOUNDARY TYPES
      GRP_NAME = 'CONLIM_2222'
      FAM_NAME = 'FAM_-1_CONLIM_2222'
      CALL MFACRE(FILE_ID,MNAME,FAM_NAME,-1,1,GRP_NAME,IERR)
      CALL CHECK_CALL(IERR,'SET_BND_MED:MFACRE:2222')
      GRP_NAME = 'CONLIM_5666'
      FAM_NAME = 'FAM_-2_CONLIM_5666'
      CALL MFACRE(FILE_ID,MNAME,FAM_NAME,-2,1,GRP_NAME,IERR)
      CALL CHECK_CALL(IERR,'SET_BND_MED:MFACRE:5666')
      GRP_NAME = 'CONLIM_4666'
      FAM_NAME = 'FAM_-3_CONLIM_4666'
      CALL MFACRE(FILE_ID,MNAME,FAM_NAME,-3,1,GRP_NAME,IERR)
      CALL CHECK_CALL(IERR,'SET_BND_MED:MFACRE:4666')
      GRP_NAME = 'CONLIM_4555'
      FAM_NAME = 'FAM_-4_CONLIM_4555'
      CALL MFACRE(FILE_ID,MNAME,FAM_NAME,-4,1,GRP_NAME,IERR)
      CALL CHECK_CALL(IERR,'SET_BND_MED:MFACRE:4555')
      GRP_NAME = 'CONLIM_5444'
      FAM_NAME = 'FAM_-5_CONLIM_5444'
      CALL MFACRE(FILE_ID,MNAME,FAM_NAME,-5,1,GRP_NAME,IERR)
      CALL CHECK_CALL(IERR,'SET_BND_MED:MFACRE:5444')
!
!     DEFINE FAMILY NUMBER
      ALLOCATE(FAM_NUM(NELEBD),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'FAM_NUM')
      DO I=1,NELEBD
        VAL = LIHBOR(I)*1000 + LIUBOR(I)*100 + LIVBOR(I)*10 + LITBOR(I)
        SELECT CASE(VAL)
        CASE(2222)
          FAM_NUM(I) = -1
        CASE(5666)
          FAM_NUM(I) = -2
        CASE(4666)
          FAM_NUM(I) = -3
        CASE(4555)
          FAM_NUM(I) = -4
        CASE(5444)
          FAM_NUM(I) = -5
        CASE DEFAULT
          FAM_NUM(I) = 0
        END SELECT
      ENDDO
!
!     WRITE FAMILY NUMBER IN THE MED FILE
      CALL MMHFNW(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_CELL,
     &            TYPE_BND_ELEM_MED,NELEBD,FAM_NUM,IERR)
      DEALLOCATE(FAM_NUM)
!
!     WRITING THE BOUNDARY FILE
      WRITE(NCLI,*) 5
      WRITE(NCLI,*) 2, 2, 2, 2, 'CONLIM_2222'
      WRITE(NCLI,*) 5, 6, 6, 6, 'CONLIM_5666'
      WRITE(NCLI,*) 4, 6, 6, 6, 'CONLIM_4666'
      WRITE(NCLI,*) 4, 5, 5, 5, 'CONLIM_4555'
      WRITE(NCLI,*) 5, 4, 4, 4, 'CONLIM_5444'

#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      END SUBROUTINE
!
!
!-----------------------------------------------------------------------
!
      SUBROUTINE UPDATE_DATA_MESH_MED (FILE_ID,TIME,RECORD,NB_DIM_MESH,
     &                                 NPOIN,COORD,IERR)
!
!BRIEF    UPDATE MESH COORDINATES
!         ONLY FOR MOVING MESH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| MED FILE DESCRIPTOR
!| TIME           |-->| PHYSICAL TIME
!| RECORD         |-->| ITERATION NUMBER
!| NB_DIM_MESH    |-->| DIMENSION OF THE MESH
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| COORD          |-->| COORDINATES TABLE
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)          :: FILE_ID
      INTEGER, INTENT(IN)          :: RECORD
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER, INTENT(IN)          :: NB_DIM_MESH, NPOIN
      DOUBLE PRECISION, INTENT(IN) :: COORD(NB_DIM_MESH*NPOIN)
      INTEGER, INTENT(OUT)         :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
      INTEGER :: MED_ID
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!
!     UPDATE MESH COORDINATES IF NECESSARY
      CALL MMHCOW(FILE_ID,MNAME,RECORD,MED_NO_IT,TIME,
     &            MED_NO_INTERLACE,NPOIN,COORD,IERR)
      CALL CHECK_CALL(IERR,'UPDATE_DATA_MESH_MED:MMHCOW')
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE IFVECTOR_(STRING,COMP_NUM,ISVECTOR)
!
!BRIEF    FIND '_U_' '_V_' '_W_' '_X_' '_Y_' '_Z_' IN NAME FIELD OF SCALAR
!         OR VECTOR
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| STRING         |<->| THE NAME OF THE VARIABLE TO TEST
!| COMP_NUM       |<--| DIRECTION OF VECTOR
!| ISVECTOR       |<--| TRUE IF VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CHARACTER(LEN=32), INTENT(INOUT) :: STRING
      INTEGER, INTENT(INOUT)             :: COMP_NUM
      LOGICAL, INTENT(INOUT)             :: ISVECTOR
!
      INTEGER :: J
!
!-----------------------------------------------------------------------
!
      ISVECTOR = .FALSE.
      COMP_NUM = 0

      IF (STRING(1:6)/='COTE_Z') THEN
      DO J = 2,31
        IF (STRING(J-1:J+1) .EQ. '_U_') THEN
          STRING(J:J) = '*'
          COMP_NUM = 1
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. '_V_') THEN
          STRING(J:J) = '*'
          COMP_NUM = 2
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. '_W_') THEN
          STRING(J:J) = '*'
          COMP_NUM = 3
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. '_X_') THEN
          STRING(J:J) = '*'
          COMP_NUM = 1
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. '_Y_') THEN
          STRING(J:J) = '*'
          COMP_NUM = 2
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. '_Z_') THEN
          STRING(J:J) = '*'
          COMP_NUM = 3
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'QX_') THEN
          STRING(J-1:J) = 'Q*'
          COMP_NUM = 1
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'QY_') THEN
          STRING(J-1:J) = 'Q*'
          COMP_NUM = 2
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'QZ_') THEN
          STRING(J-1:J) = 'Q*'
          COMP_NUM = 3
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'U0_') THEN
          STRING(J-1:J) = '*0'
          COMP_NUM = 1
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'V0_') THEN
          STRING(J-1:J) = '*0'
          COMP_NUM = 2
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'W0_') THEN
          STRING(J-1:J) = '*0'
          COMP_NUM = 3
          ISVECTOR = .TRUE.
        ENDIF
      ENDDO
      ENDIF
!
      RETURN
      END SUBROUTINE IFVECTOR_
!
!-----------------------------------------------------------------------
!
      SUBROUTINE CONVERT_ELEM_TYPE(TYPE_ELEM,TYPE_ELEM_MED,IERR)
!
!BRIEF    CONVERTS ELEMENT TYPE FROM SLF FORMAT TO MED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TYPE_ELEM         |<--| ELEMENT TYPE WITH SLF FORMAT
!| TYPE_ELEM_MED     |-->| ELEMENT TYPE WITH MED FORMAT
!| IERR              |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(INOUT) :: TYPE_ELEM_MED
      INTEGER, INTENT(OUT) :: IERR
!
!-----------------------------------------------------------------------
!
#if defined(HAVE_MED)
!     INITIALISATION
      IERR = 0
!
!     CONVERSION FROM BIEF NUMBERING TO MED
      SELECT CASE(TYPE_ELEM)
!
!       1 NODES OR 1 SEGMENT WITH 1 NODES
        CASE(POINT_BND_ELT_TYPE)
          TYPE_ELEM_MED = MED_POINT1
!
!       SEGMENT WITH 2 NODES
        CASE(EDGE_BND_ELT_TYPE)
          TYPE_ELEM_MED = MED_SEG2
!
!       TRIANGLE WITH 3 NODES
        CASE(TRIANGLE_ELT_TYPE, TRIANGLE_BND_ELT_TYPE, 
     &       TRIANGLE_3D_BND_ELT_TYPE)
          TYPE_ELEM_MED = MED_TRIA3
!
!       QUADRANGLE WITH 4 NODES
        CASE(QUADRANGLE_ELT_TYPE, QUADRANGLE_BND_ELT_TYPE)
          TYPE_ELEM_MED = MED_QUAD4
!
!       TETRAHEDRA WITH 4 NODES
        CASE(TETRAHEDRON_ELT_TYPE)
          TYPE_ELEM_MED = MED_TETRA4
!
!       PRISM WITH 6 NODES
        CASE(PRISM_ELT_TYPE, SPLIT_PRISM_ELT_TYPE)
          TYPE_ELEM_MED = MED_PENTA6
!
!       ERROR CASE
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_ELEMENT_TYPE_ERR
!
      END SELECT
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE NDP_FROM_ELEMENT_TYPE_MED(TYPE_ELEM_MED,NDP,IERR)
!
!brief    DEDUCES NUMBER OF NODES FROM ELEMENT TYPE IN MED NUMBERING
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TYPE_ELEM_MED     |-->| ELEMENT TYPE WITH MED NUMBERING
!| NDP               |-->| NUMBER OF NODES PER ELEMENT
!| IERR              |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      INTEGER, INTENT(IN)  :: TYPE_ELEM_MED
      INTEGER, INTENT(INOUT) :: NDP
      INTEGER, INTENT(OUT) :: IERR
!
!-----------------------------------------------------------------------
!
!     INITIALISATION
      IERR = 0
      NDP  = -99
#if defined(HAVE_MED)
!
!     CONVERSION FROM BIEF NUMBERING TO MED
!
!     1D ELEMENTS
      IF (TYPE_ELEM_MED .EQ. MED_POINT1) NDP = 1 ! SEGM WITH 1 NODES
      IF (TYPE_ELEM_MED .EQ. MED_SEG2)   NDP = 2 ! SEGM WITH 2 NODES
!
!     2D ELEMENTS
      IF (TYPE_ELEM_MED .EQ. MED_TRIA3)  NDP = 3 ! TRIA WITH 3 NODES
      IF (TYPE_ELEM_MED .EQ. MED_QUAD4)  NDP = 4 ! QUAD WITH 4 NODES
!
!     3D ELEMENTS
      IF (TYPE_ELEM_MED .EQ. MED_TETRA4) NDP = 4 ! TETRA WITH 4 NODES
      IF (TYPE_ELEM_MED .EQ. MED_PENTA6) NDP = 6 ! PRISM WITH 6 NODES
!
!     ERROR IF UNKNOWN ELEMNT TYPE
      IF (NDP .EQ. -99) IERR = HERMES_UNKNOWN_ELEMENT_TYPE_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE IDENTIFY_BND_ELMT(FILE_ID,TYPE_BND_ELEM,IERR)
!
!brief    DEDUCES NUMBER OF NODES FROM ELEMENT TYPE IN MED NUMBERING
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TYPE_BND_ELEM     |-->| ELEMENT TYPE WITH BIEF NUMBERING
!| FILE_ID           |-->| ID OF THE MED FILE
!| IERR              |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: TYPE_BND_ELEM
      INTEGER, INTENT(OUT) :: IERR
!
!-----------------------------------------------------------------------
!
#if defined(HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME, TEMP_FAM
      CHARACTER(LEN=MED_LNAME_SIZE),ALLOCATABLE :: GRP_NAME(:)
      INTEGER ::  INUM,IFAM,I,MED_ID,TYPE_ELEM_MED
      INTEGER :: J
      INTEGER :: NELEM,NB_FAMILY,NB_GRP
      INTEGER, ALLOCATABLE :: NUM_FAMILY(:)
      LOGICAL :: FOUND
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_MED_OBJ(FILE_ID,MED_ID,IERR)
      CALL CHECK_CALL(IERR,'IDENTIFY_BND_ELMT:GET_MED_OBJ_FILE')
      MNAME = MED_OBJ_TAB(MED_ID)%MESH_NAME
!     Only computing the array if it was not done before
      IF(.NOT.ALLOCATED(MED_OBJ_TAB(MED_ID)%IS_BND)) THEN
!       CONVERTS TYPE OF ELEMENTS
        CALL CONVERT_ELEM_TYPE(TYPE_BND_ELEM, TYPE_ELEM_MED, IERR)
        CALL CHECK_CALL(IERR,'IDENTIFY_BND_ELMT:CONVERT_ELEM_TYPE')
!       
        CALL GET_MESH_NELEM_MED(FILE_ID,TYPE_BND_ELEM,NELEM,IERR)
        CALL CHECK_CALL(IERR,'IDENTIFY_BND_ELMT:GET_BND_NELEM_MED')
        ALLOCATE(MED_OBJ_TAB(MED_ID)%IS_BND(NELEM),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'IDENTIFY_BND_ELMT:is_bnd')
!
!       READ THE FAMILY NUMBER FOR EACH ELEMENT
        ALLOCATE(NUM_FAMILY(NELEM),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'IDENTIFY_BND_ELMT:num_family')
        CALL MMHFNR(FILE_ID,MNAME,MED_NO_DT,MED_NO_IT,MED_CELL,
     &              TYPE_ELEM_MED,NUM_FAMILY,IERR)
        CALL CHECK_CALL(IERR,'IDENTIFY_BND_ELMT:MMHFNR')
!       
!       READ THE TOTAL NUMBER OF FAMILY
        CALL MFANFA(FILE_ID,MNAME,NB_FAMILY,IERR)
        CALL CHECK_CALL(IERR,'IDENTIFY_BND_ELMT:MFANFA')
        ALLOCATE(MED_OBJ_TAB(MED_ID)%BND_FAM(NB_FAMILY,2),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'BND_FAM')
!       
        DO IFAM = 1, NB_FAMILY
!       
!         READ THE NUMBER OF GROUP PER FAMILY
          CALL MFANFG(FILE_ID,MNAME,IFAM,NB_GRP,IERR)
          CALL CHECK_CALL(IERR,'IDENTIFY_BND_ELMT:MFANFG')
!       
          ALLOCATE(GRP_NAME(MAX(NB_GRP,1)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'IDENTIFY_BND_ELMT:GRP_NAME')
        
!         READ THE TYPE OF FAMILY
          CALL MFAFAI(FILE_ID,MNAME,IFAM,TEMP_FAM,INUM,GRP_NAME,IERR)
          CALL CHECK_CALL(IERR,'IDENTIFY_BND_ELMT:MFAFAI')
          MED_OBJ_TAB(MED_ID)%BND_FAM(IFAM,1) = INUM
          ! Only check for families on elements i.e. num < 0
          FOUND = .FALSE.
          IF(INUM.LT.0) THEN
            DO J=1,MED_OBJ_TAB(MED_ID)%NBND_GRP
              IF(FOUND) EXIT
              ! looping on the family groups and if one is in bnd_grp 
              ! adding it to bnd_fam
              DO I=1,NB_GRP
                IF(GRP_NAME(I).EQ.
     &             MED_OBJ_TAB(MED_ID)%BND_GRP_NAME(J)) THEN
                  FOUND = .TRUE.
                  MED_OBJ_TAB(MED_ID)%BND_FAM(IFAM,2) = J
                  EXIT
                ENDIF 
              ENDDO
            ENDDO
          ELSE
            MED_OBJ_TAB(MED_ID)%BND_FAM(IFAM,2) = 0
          ENDIF
          DEALLOCATE(GRP_NAME)
        ENDDO
        ! Looping on all element and checking if their family is in bnd_fam
        DO I=1,NELEM
          MED_OBJ_TAB(MED_ID)%IS_BND(I) = .FALSE.
          DO IFAM=1,NB_FAMILY
            IF(NUM_FAMILY(I).EQ.
     &         MED_OBJ_TAB(MED_ID)%BND_FAM(IFAM,1)) THEN
              MED_OBJ_TAB(MED_ID)%IS_BND(I) = 
     &              MED_OBJ_TAB(MED_ID)%BND_FAM(IFAM,2).NE.0
              EXIT
            ENDIF
          ENDDO
        ENDDO
        DEALLOCATE(NUM_FAMILY)
      ENDIF
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      END MODULE UTILS_MED

