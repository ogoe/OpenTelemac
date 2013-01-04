      MODULE M_MED
!***********************************************************************
! THIS FILE IS PART OF BIEF
! COPYRIGHT (C) EDF 2008
!-----------------------------------------------------------------------
!
!***********************************************************************
! BIEF
!***********************************************************************
!
!brief    INTERFACES OF MED PUBLIC SUBROUTINES
!
!warning  NOTE THAT THIS INTERFACE IS FOR THE LIBRARY INCLUDED IN MED V3.0.4
!         CHANGES MIGHT BE REQUIRED WITH FUTURE VERSIONS OF MED.
!
!history JULIEN HOLLEVILE
!
!history FABIEN DECUNG
!
!history YOANN AUDOUIN
!+       29/10/2011     
!+       V6P2
!+       ADAPTING CODE TO MED V3.0.4         
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

      IMPLICIT NONE
      ! PLACE PUBLIC DECLARATIONS HERE ...

      PUBLIC OPEN_FILE_MED
      PUBLIC CLOSE_FILE_MED
      PUBLIC WRITE_MESH_MED
      PUBLIC CREATE_DATASET_MED
      PUBLIC WRITE_DATA_MED
      PUBLIC SUITE_MED

!-----------------------------------------------------------------------
!   DEFINITIONS OF VARIABLES 'MED'
!-----------------------------------------------------------------------
! FD:
! IFDEF NOT RECOMMENDED HERE.
! WE PREFER WRITE EXPLICITLY THESE PARAMETERS HEREAFTER.
! IT AVOID US TO ADD AN OPTION IN THE CONFIGURATION FILE...
! (THOUGH WE HAVE TO MODIFY EVERY TIME THE SOURCE CODE)
!
!#if defined(HAVE_MED)
!      INCLUDE 'med.hf'
!#endif

! START OF 'med.hf' MED-V3.0.4
      INTEGER MED_ACC_RDONLY,MED_ACC_RDWR,MED_ACC_RDEXT
      INTEGER MED_ACC_CREAT,MED_ACC_UNDEF
      PARAMETER (MED_ACC_RDONLY=0,MED_ACC_RDWR=1)
      PARAMETER (MED_ACC_RDEXT=2,MED_ACC_CREAT=3,MED_ACC_UNDEF=4)

      INTEGER MED_UNSTRUCTURED_MESH, MED_STRUCTURED_MESH
      INTEGER MED_UNDEF_MESH_TYPE
      PARAMETER (MED_UNSTRUCTURED_MESH=0,MED_STRUCTURED_MESH=1)
      PARAMETER (MED_UNDEF_MESH_TYPE=-1)

      INTEGER MED_CARTESIAN, MED_CYLINDRICAL, MED_SPHERICAL
      INTEGER MED_UNDEF_AXIS_TYPE
      PARAMETER(MED_CARTESIAN=0, MED_CYLINDRICAL=1, MED_SPHERICAL=2)
      PARAMETER(MED_UNDEF_AXIS_TYPE=-1)
      INTEGER MED_CARTESIAN_GRID, MED_POLAR_GRID
      INTEGER MED_CURVILINEAR_GRID, MED_UNDEF_GRID_TYPE
      PARAMETER (MED_CARTESIAN_GRID=0, MED_POLAR_GRID=1)
      PARAMETER  (MED_CURVILINEAR_GRID=2, MED_UNDEF_GRID_TYPE=-1)

      INTEGER MED_SORT_DTIT,MED_SORT_ITDT,MED_SORT_UNDEF
      PARAMETER (MED_SORT_DTIT=0,MED_SORT_ITDT=1,MED_SORT_UNDEF=-1)

      INTEGER MED_COMMENT_SIZE,MED_NAME_SIZE,MED_SNAME_SIZE
      INTEGER MED_LNAME_SIZE
      PARAMETER (MED_COMMENT_SIZE=200,MED_NAME_SIZE=64)
      PARAMETER (MED_SNAME_SIZE=16, MED_LNAME_SIZE=80)

      INTEGER MED_NO_IT, MED_NO_DT
      PARAMETER (MED_NO_IT=-1, MED_NO_DT = -1)
      REAL*8 MED_UNDEF_DT
      PARAMETER (MED_UNDEF_DT = 0.)

      INTEGER MED_UNDEF_INTERLACE,MED_FULL_INTERLACE
      INTEGER MED_NO_INTERLACE 
      PARAMETER (MED_UNDEF_INTERLACE=-1,MED_FULL_INTERLACE=0)
      PARAMETER (MED_NO_INTERLACE=1)

      INTEGER MED_CELL, MED_DESCENDING_FACE
      INTEGER MED_DESCENDING_EDGE,MED_NODE
      INTEGER MED_NODE_ELEMENT, MED_STRUCT_ELEMENT
      INTEGER MED_ALL_ENTITY_TYPE,MED_UNDEF_ENTITY_TYPE
      INTEGER MED_N_ENTITY_TYPES
      PARAMETER (MED_N_ENTITY_TYPES=6)
      PARAMETER (MED_CELL=0, MED_DESCENDING_FACE=1)
      PARAMETER (MED_DESCENDING_EDGE=2,MED_NODE=3)
      PARAMETER (MED_NODE_ELEMENT=4, MED_STRUCT_ELEMENT=5)
      PARAMETER (MED_ALL_ENTITY_TYPE=6,MED_UNDEF_ENTITY_TYPE=-1)

      INTEGER MED_POINT1,MED_SEG2,MED_SEG3,MED_SEG4,MED_TRIA3
      INTEGER MED_QUAD4,MED_TRIA6,MED_TRIA7,MED_QUAD8,MED_QUAD9
      INTEGER MED_TETRA4,MED_PYRA5,MED_PENTA6,MED_HEXA8,MED_TETRA10
      INTEGER MED_OCTA12,MED_PYRA13,MED_PENTA15,MED_HEXA20,MED_HEXA27 
      INTEGER MED_POLYGON,MED_POLYHEDRON, MED_STRUCT_GEO_INTERNAL
      INTEGER MED_STRUCT_GEO_SUP_INTERNAL,MED_NONE 
      INTEGER MED_GEO_ALL,MED_ALL_GEOTYPE
      INTEGER MED_NO_GEOTYPE,MED_UNDEF_GEOTYPE
      PARAMETER(MED_POINT1=001)
      PARAMETER(MED_SEG2=102)
      PARAMETER(MED_SEG3=103)
      PARAMETER(MED_SEG4=104)
      PARAMETER(MED_TRIA3=203)
      PARAMETER(MED_QUAD4=204)
      PARAMETER(MED_TRIA6=206)
      PARAMETER(MED_TRIA7=207)
      PARAMETER(MED_QUAD8=208)
      PARAMETER(MED_QUAD9=209)
      PARAMETER(MED_TETRA4=304)
      PARAMETER(MED_PYRA5=305)
      PARAMETER(MED_PENTA6=306)
      PARAMETER(MED_HEXA8=308)
      PARAMETER(MED_TETRA10=310)
      PARAMETER(MED_OCTA12=312)
      PARAMETER(MED_PYRA13=313)
      PARAMETER(MED_PENTA15=315)
      PARAMETER(MED_HEXA20=320)
      PARAMETER(MED_HEXA27=327)
      PARAMETER(MED_POLYGON=400)
      PARAMETER(MED_POLYHEDRON=500)
      PARAMETER(MED_STRUCT_GEO_INTERNAL=600)
      PARAMETER(MED_STRUCT_GEO_SUP_INTERNAL=700)
      PARAMETER(MED_NONE=0)
      PARAMETER(MED_GEO_ALL=-1)
      PARAMETER(MED_ALL_GEOTYPE=-1)
      PARAMETER(MED_NO_GEOTYPE=0)
      PARAMETER(MED_UNDEF_GEOTYPE=0)

      INTEGER  MED_N_CELL_GEO,MED_N_CELL_FIXED_GEO
      INTEGER  MED_N_CELL_GEO_FIXED_CON
      INTEGER  MED_N_FACE_GEO,MED_N_FACE_FIXED_GEO
      INTEGER  MED_N_FACE_GEO_FIXED_CON
      INTEGER  MED_N_EDGE_TYPES,MED_N_EDGE_FIXED_GEO
      INTEGER  MED_N_EDGE_GEO_FIXED_CON
      INTEGER  MED_N_NODE_GEO,MED_N_NODE_FIXED_GEO
      INTEGER  MED_N_NODE_GEO_FIXED_CON   
      PARAMETER(MED_N_CELL_GEO=23)
      PARAMETER(MED_N_CELL_FIXED_GEO=22)
      PARAMETER(MED_N_CELL_GEO_FIXED_CON=20)
      PARAMETER(MED_N_FACE_GEO=7)
      PARAMETER(MED_N_FACE_FIXED_GEO=7)
      PARAMETER(MED_N_FACE_GEO_FIXED_CON=6)
      PARAMETER(MED_N_EDGE_TYPES=3)
      PARAMETER(MED_N_EDGE_FIXED_GEO=3)
      PARAMETER(MED_N_EDGE_GEO_FIXED_CON=3)
      PARAMETER(MED_N_NODE_GEO=1)
      PARAMETER(MED_N_NODE_FIXED_GEO=1)
      PARAMETER(MED_N_NODE_GEO_FIXED_CON=1)

      INTEGER MED_COORDINATE, MED_CONNECTIVITY
      INTEGER MED_NAME, MED_NUMBER
      INTEGER MED_FAMILY_NUMBER
      INTEGER MED_COORDINATE_AXIS1, MED_COORDINATE_AXIS2
      INTEGER MED_COORDINATE_AXIS3, MED_INDEX_FACE, MED_INDEX_NODE
      INTEGER MED_GLOBAL_NUMBER, MED_VARIABLE_ATTRIBUTE
      INTEGER MED_COORDINATE_TRSF, MED_UNDEF_DATATYPE
      PARAMETER(MED_COORDINATE=0, MED_CONNECTIVITY=1, MED_NAME=2)
      PARAMETER(MED_NUMBER=3, MED_FAMILY_NUMBER=4)
      PARAMETER(MED_COORDINATE_AXIS1=5, MED_COORDINATE_AXIS2=6)
      PARAMETER(MED_COORDINATE_AXIS3=7,MED_INDEX_FACE=8)
      PARAMETER(MED_INDEX_NODE=9,MED_GLOBAL_NUMBER=10)
      PARAMETER(MED_VARIABLE_ATTRIBUTE=11,MED_COORDINATE_TRSF=12)
      PARAMETER(MED_UNDEF_DATATYPE=-1)

      INTEGER MED_NODAL, MED_DESCENDING, MED_UNDEF_CONNECTIVITY_MODE
      INTEGER MED_NO_CMODE
      PARAMETER(MED_NODAL=0,MED_DESCENDING=1)
      PARAMETER(MED_UNDEF_CONNECTIVITY_MODE=-1)
      PARAMETER(MED_NO_CMODE=-1)
  
      INTEGER MED_FALSE, MED_TRUE
      PARAMETER(MED_FALSE=0,MED_TRUE=1) 

      INTEGER MED_UNDEF_PFLMODE,MED_GLOBAL_PFLMODE,MED_COMPACT_PFLMODE
      INTEGER MED_UNDEF_STMODE,MED_GLOBAL_STMODE,MED_COMPACT_STMODE
      CHARACTER*64 MED_GAUSS_ELNO,MED_IPOINT_ELNO
      PARAMETER(MED_GAUSS_ELNO='MED_GAUSS_ELNO')
      PARAMETER(MED_IPOINT_ELNO='MED_GAUSS_ELNO')
      PARAMETER(MED_UNDEF_PFLMODE=0,MED_GLOBAL_PFLMODE=1)
      PARAMETER(MED_COMPACT_PFLMODE=2)
      PARAMETER(MED_UNDEF_STMODE=0,MED_GLOBAL_STMODE=1)
      PARAMETER(MED_COMPACT_STMODE=2)
      CHARACTER*64 MED_NO_NAME,MED_NO_MESHNAME,MED_NO_MESH
      CHARACTER*64 MED_NO_MESH_SUPPORT, MED_NO_LOCALIZATION
      CHARACTER*64 MED_NO_INTERPOLATION, MED_NO_PROFILE
      CHARACTER*64 MED_ALLENTITIES_PROFILE
      CHARACTER*80 MED_NO_GROUP
      PARAMETER(MED_NO_NAME='',MED_NO_MESHNAME='',MED_NO_MESH='')
      PARAMETER(MED_NO_MESH_SUPPORT='', MED_NO_LOCALIZATION='')
      PARAMETER(MED_NO_INTERPOLATION='', MED_NO_PROFILE='')
      PARAMETER(MED_ALLENTITIES_PROFILE='')
      PARAMETER(MED_NO_GROUP='')

      INTEGER MED_ALL_CONSTITUENT
      PARAMETER(MED_ALL_CONSTITUENT=0)	

      INTEGER MED_UNDEF_SIZE,MED_NO_PROFILE_SIZE
      PARAMETER(MED_UNDEF_SIZE=0,MED_NO_PROFILE_SIZE=0)

      INTEGER MED_MESH,MED_FIELD,MED_LIBRARY,MED_FILE
      INTEGER MED_MESH_SUPPORT,MED_ELSTRUCT,MED_FAMILY
      INTEGER MED_EQUIVALENCE, MED_GROUP, MED_JOINT
      INTEGER MED_LOCALIZATION, MED_PROFILE,MED_FILTER,MED_INTERPOLATION
      INTEGER MED_NUMERICAL_DATA,MED_LINK,MED_CLASS_UNDEF,MED_CLASS_ALL
      PARAMETER(MED_MESH=0,MED_FIELD=1,MED_LIBRARY=2,MED_FILE=3)
      PARAMETER(MED_MESH_SUPPORT=4,MED_ELSTRUCT=5,MED_FAMILY=6)
      PARAMETER(MED_LOCALIZATION=7,MED_PROFILE=8,MED_FILTER=9)
      PARAMETER(MED_INTERPOLATION=10, MED_NUMERICAL_DATA=11)
      PARAMETER(MED_LINK=13, MED_CLASS_UNDEF=-1, MED_CLASS_ALL=-2)

      INTEGER MED_INT32, MED_INT64,MED_FLOAT64,MED_INT
      PARAMETER (MED_INT32=24, MED_INT64=26,MED_FLOAT64=6,MED_INT=28)

      INTEGER MED_ATT_FLOAT64,MED_ATT_INT,MED_ATT_NAME
      INTEGER MED_ATT_UNDEF
      PARAMETER(MED_ATT_FLOAT64=MED_FLOAT64)
      PARAMETER(MED_ATT_INT=MED_INT)
      PARAMETER(MED_ATT_NAME=30)
      PARAMETER(MED_ATT_UNDEF=0)

      CHARACTER*64 MED_PARTICLE_NAME, MED_BALL_NAME, MED_BEAM_NAME
      CHARACTER*64 MED_PARTICLE_LABEL,MED_BALL_DIAMETER
      CHARACTER*64 MED_BEAM_THICKNESS
      PARAMETER(MED_PARTICLE_NAME='MED_PARTICLE')
      PARAMETER(MED_BALL_NAME='MED_BALL')
      PARAMETER(MED_BEAM_NAME='MED_BEAM')
      PARAMETER(MED_PARTICLE_LABEL='MED_PARTICLE_LABEL')
      PARAMETER(MED_BALL_DIAMETER='MED_BALL_DIAMETER')
      PARAMETER(MED_BEAM_THICKNESS='MED_BEAM_THICKNESS')
! END OF 'med.hf'

!
      INTEGER :: LOCAL_TIMESTEP_2D = 1
      INTEGER :: LOCAL_TIMESTEP_3D = 1

      CONTAINS
!                    *************************
                     SUBROUTINE MED_CHECK_CRET
!                    *************************
     &(CRET,TEXT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CHECK THE RETURN VARIABLE OF A MED CALL
!+        STOP EXECUTION IF AN ERROR OCCURED.
!                     
!history  J.HOLLEVILLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CRET           |-->| 10 INTEGERS, SEE SELAFIN FILE STANDARD
!| TEXT           |-->| NUMBER OF NODES PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: CRET
      CHARACTER(LEN=6), INTENT(IN) :: TEXT
!
!-----------------------------------------------------------------------
!

      IF(CRET.LT.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'ERREUR MED : ',TEXT
        IF(LNG.EQ.2 )WRITE(LU,*) 'ERROR MED : ',TEXT
        CALL PLANTE(1)
        STOP
      ENDIF

!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE MED_CHECK_CRET
!
!                    ************************
                     SUBROUTINE OPEN_FILE_MED
!                    ************************
     &(MEDNAME,IDFILE,OPENMODE)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPEN A MED DATA FILE. IT WAS ALREADY CHECKED, THAT THE FILE IS NOT
!+        ALREADY IN USE. THUS, THIS CHECK IS NOT DONE AGAIN.
!+        THE FILE IS OPENED IN THE SPECIFIED MODE (RO, RW OR WO)
!+        THE ID OF THE MED FILE IS RETURNED AS PART OF THE FILE DESCRIPTOR
!+        MEDFILE%CANAL.
!                     
!history  J.HOLLEVILLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IDFILE         |<->| MED FILE DESCRIPTOR
!| MEDNAME        |-->| NAME OF THE MED FILE
!| OPENMODE       |-->| OPENING MODE (READ/READWRITE/WRITE)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! ARGUMENTS :
      INTEGER, INTENT(INOUT)       :: IDFILE
      CHARACTER(LEN=*), INTENT(IN) :: MEDNAME
      CHARACTER(LEN=9),INTENT(IN)  :: OPENMODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined(HAVE_MED)
      INTEGER :: CRET ! RETURN CODE OF THE CALLED MED LIBRARY FUNCTION
      INTEGER :: MED_MODE ! MED OPENING MODE
!
!-----------------------------------------------------------------------
!
!     CHECK THE MODE. POSSIBLE MODES ARE READONLY, WRITEONLY OR
!     READWRITE. OTHER MODES ARE INVALID.

      SELECT CASE(OPENMODE)
         CASE('READ     ')
            MED_MODE = MED_ACC_RDONLY
         CASE('READWRITE')
            MED_MODE = MED_ACC_RDWR
         CASE('WRITE    ')
            MED_MODE = MED_ACC_CREAT
         CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'ACTION ERRONEE DANS OPEN_FILE_MED :',OPENMODE
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*)'BAD ACTION IN OPEN_FILE_MED:',OPENMODE
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT

! OPEN THE MED FILE AND CHECK RETURN CODE.
      CALL MFIOPE(IDFILE,MEDNAME,MED_MODE,CRET)
      CALL MED_CHECK_CRET(CRET,'EFOUVR')
!
#else
!
! MED LIBRARY IS NOT AVAILABLE
! OR -DHAVE_MED HAS NOT BEEN SET IN CONFIG FILE
      WRITE(LU,*) 'ERROR : TRYING TO CALL MED FUNCTION WITHOUT ',
     *            'MED LIBRARY INSTALLED'
      CALL PLANTE(0)
!
#endif
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE OPEN_FILE_MED
!
!                    *************************
                     SUBROUTINE CLOSE_FILE_MED
!                    *************************
     &(IDFILE)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CLOSE A MED DATA FILE
!                     
!history  J.HOLLEVILLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IDFILE         |-->| MED FILE DESCRIPTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER, INTENT(IN)  :: IDFILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined(HAVE_MED)
      INTEGER :: CRET
!
!-----------------------------------------------------------------------
!
      CALL MFICLO(IDFILE,CRET)
      CALL MED_CHECK_CRET(CRET,'EFFERM')
!
#else
!
! MED LIBRARY IS NOT AVAILABLE
! OR -DHAVE_MED HAS NOT BEEN SET IN CONFIG FILE
      WRITE(LU,*) 'ERROR : TRYING TO CALL MED FUNCTION WITHOUT ',
     *            'MED LIBRARY INSTALLED'
      CALL PLANTE(0)
!
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CLOSE_FILE_MED
!      
!                    *************************
                     SUBROUTINE WRITE_MESH_MED
!                    *************************
     &(RES_FILE,MESH,X_ORIG,Y_ORIG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITE THE MESH INFORMATIONS INTO THE RESULT FILE
!                     
!history  J.HOLLEVILLE
!                     
!history  Y.AUDOUIN
!+        29/10/2011
!+        V6P2
!+        Updating med functions for med 3.0.4                        
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| RES_FILE       |-->| MED FILE DESCRIPTOR
!| MESH           |-->| BIEF'S MESH OBJECT
!| X_ORIG         |-->| VALUE OF THE X ORIGINAL POINT
!| Y_ORIG         |-->| VALUE OF THE X ORIGINAL POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER        , INTENT(IN)  :: RES_FILE
      TYPE(BIEF_MESH), INTENT(IN)  :: MESH
      DOUBLE PRECISION,INTENT(IN)  :: X_ORIG
      DOUBLE PRECISION,INTENT(IN)  :: Y_ORIG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined(HAVE_MED)
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: COOR
      CHARACTER(LEN=64) :: MESHNAME ! NAME OF THE MESH
      CHARACTER(LEN=200) :: DESC ! DESCRIPTION ASSOCIATED TO THE MESH
      CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE :: COORD_NAME
      CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE :: COORD_UNIT
      CHARACTER(LEN=16) :: DTUNIT
      INTEGER :: CRET
      INTEGER :: I
      INTEGER :: TYP_MED
      CHARACTER*16, ALLOCATABLE :: ELEMNAME(:)
! NUMBER AND NAME FOR A FAMILLY
      INTEGER           :: NUMFAM
      CHARACTER(LEN=32) :: NOMFAM
      DOUBLE PRECISION :: DT
!
!-----------------------------------------------------------------------
!
      ! WRITE THE FILE DESCRIPTION  
      DESC = 'BIEF_MESH'
      CALL MFICOW(RES_FILE,DESC,CRET)
      CALL MED_CHECK_CRET(CRET,'MFICOW')

      ! BUILD THE MESH COORDINATES INFO
      ! WRITE THE COORDINATES NAME AND UNIT
      ALLOCATE(COORD_NAME(MESH%DIM),COORD_UNIT(MESH%DIM))
      COORD_NAME(1) = 'X'
      call blanc2_(COORD_NAME(1))
      COORD_UNIT(1) = 'M'
      call blanc2_(COORD_UNIT(1))
      COORD_NAME(2) = 'Y'
      call blanc2_(COORD_NAME(2))
      COORD_UNIT(2) = 'M'
      call blanc2_(COORD_UNIT(2))
      IF(MESH%DIM .EQ. 3 ) THEN
        COORD_NAME(3) = 'Z'
        call blanc2_(COORD_NAME(3))
        COORD_UNIT(3) = 'M'
        call blanc2_(COORD_UNIT(3))
      ENDIF
      ! THE NAME OF THE MESH IS SET TO MESH
      MESHNAME = 'MESH' // char(0)
      DESC = 'NO_DESCRIPTION'//char(0)
      DTUNIT = 'S'//char(0)

      CALL MMHCRE(RES_FILE,MESHNAME,MESH%DIM,MESH%DIM,
     &            MED_UNSTRUCTURED_MESH,DESC,DTUNIT,
     &            MED_SORT_DTIT,MED_CARTESIAN,COORD_NAME,
     &            COORD_UNIT,CRET)
      call MED_CHECK_CRET(CRET,'MMHCRE')

      ! BUILDING THE COORDIANTES VALUE TABLES
      ALLOCATE(COOR(MESH%NPOIN*MESH%DIM))
      DO I = 1,MESH%NPOIN
        COOR(I) = MESH%X%R(I)+X_ORIG
        COOR(I+MESH%NPOIN) = MESH%Y%R(I)+Y_ORIG
      ENDDO
      IF (MESH%DIM .EQ. 3 ) THEN
        DO I = 1, MESH%NPOIN
          COOR(I+2*MESH%NPOIN) = MESH%Z%R(I)
        ENDDO
      ENDIF
      DT = 0.0
      CALL MMHCOW(RES_FILE,         ! FILE UNIT
     *            MESHNAME,         ! TITLE OF THE MESH
     *            MED_NO_DT,        ! NO TIME STEP
     *            MED_NO_IT,        ! NO ITERATION
     *            DT,               ! NO TIME STEP VALUE
     *            MED_NO_INTERLACE, ! STORAGE MODE FOR THE COORDINATES
     *            MESH%NPOIN,       ! NUMBER OF NODES
     *            COOR,             ! TABLE OF THE COORDIANTES
     *            CRET)             ! RETURN CODE

      CALL MED_CHECK_CRET(CRET,'MMHCOW')

      ! THE TYPE OF THE ELEMENTS : GET THE MED ELEMENT TYPE
      IF(MESH%TYPELM.EQ.10) THEN
        TYP_MED = MED_TRIA3 
      ELSEIF(MESH%TYPELM.EQ.20) THEN
        TYP_MED = MED_QUAD4 
      ELSEIF(MESH%TYPELM.EQ.30) THEN
        TYP_MED = MED_TETRA4
      ELSEIF(MESH%TYPELM.EQ.40) THEN
        TYP_MED = MED_PENTA6 
      ELSE
        WRITE(6,*) 'ERROR : ELEMENT TYPE UNKNOWN!'
        CALL PLANTE(1)
        STOP
      ENDIF
      ! WE NEED TO ALLOCATE THE TABLE FOR ELEMENTS NAME BECAUSE IT IS
      ! READ EVEN IF THE BOOLENA IS SET TO FALSE
      ALLOCATE(ELEMNAME(MESH%NELEM))
      ! WRITE THE CONNECTIVITY INTO THE MED DATA FILE :
      CALL MMHELW(RES_FILE,         ! FILE UNIT
     *            MESHNAME,         ! TITLE OF THE MESH
     *            MED_NO_DT,        ! NO TIME STEP
     *            MED_NO_IT,        ! NO ITERATION
     *            DT,               ! NO TIMESTEP VALUE
     *            MED_CELL,         ! GEOMETRY TYPE
     *            TYP_MED,          ! ENTITY TYPE
     *            MED_NODAL,        ! CONNECTIVITY TYPE
     *            MED_NO_INTERLACE, ! STORAGE MODE FOR THE CONNECTIVITY
     *            MESH%NELEM,       ! NUMBER OF ELEMENTS
     *            MESH%IKLE%I,      ! CONNECTIVITY TABLE
     *            MED_FALSE,        ! NO NAME OF THE ELEMENT
     *            ELEMNAME,         ! NAME OF THE ELEMENT
     *            MED_FALSE,        ! NO NUMBER OF ELEMENT
     *            I,                ! NUMBER OF ELEMENT
     *            MED_FALSE,        ! NO FAMILY NUMBER
     *            I,                ! FAMILY NUMBER
     *            CRET)
      CALL MED_CHECK_CRET(CRET,'MMHELW')
      DEALLOCATE(ELEMNAME)

      ! We need a default familly called ZERO FAMILY
      NUMFAM = 0
      NOMFAM="FAMILLE_ZERO"//char(0)
      CALL MFACRE(RES_FILE,MESHNAME,NOMFAM,NUMFAM,0,' ',CRET)
      CALL MED_CHECK_CRET(CRET,'MFACRE')
!
#else
!
      WRITE(LU,*) 'ERROR : TRYING TO CALL MED FUNCTION WITHOUT ',
     *            'MED LIBRARY INSTALLED'
      CALL PLANTE(0)
!
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE WRITE_MESH_MED
!      
!                       *****************************
                        SUBROUTINE CREATE_DATASET_MED
!                       *****************************
!
     *(RES_FILE,TITLE,NVAR,NOMVAR,OUTVAR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CREATE THE DATA FIELDS FOR THE MED DATA FILE. A DATA FIELD IS DEFINED
!+        BY A NAME, A UNIT, A TYPE (INTEGER OR REAL) AND THE NUMBER OF
!+        COMPONENTS (A 2D VECTOR HAS 2 COMPONENTS, A 3D VECTOR HAS 3 COMPONENTS
!+        ...)
!                     
!history  J.HOLLEVILLE
!                     
!history  Y.AUDOUIN
!+        29/10/2011
!+        V6P2
!+        Updating med functions for med 3.0.4                        
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| RES_FILE       |-->| MED FILE DESCRIPTOR
!| TITLE          |-->| TITRE DU MAILLAGE
!| NAR            |-->| NUMBER OF VARIABLES TO BE PUT IN THE FILE
!| NOMVAR         |-->| NAME OF VARIABLES 
!| OUTVAR         |-->| VARIABLES TO BE PUT IN THE FILE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                          , INTENT(IN) :: RES_FILE
      CHARACTER(LEN=72)                , INTENT(IN) :: TITLE
      INTEGER                          , INTENT(IN) :: NVAR
      CHARACTER(LEN=32),DIMENSION(NVAR), INTENT(IN) :: NOMVAR
      LOGICAL          ,DIMENSION(NVAR), INTENT(IN) :: OUTVAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined(HAVE_MED)
      INTEGER :: DIMVEC ! DIMENSION OF VECTOR
      INTEGER           :: NCOMP
      INTEGER           :: CRET
      CHARACTER(LEN=64) :: FIELD_NAME,MESHNAME
      CHARACTER(LEN=16) :: COMP_NAME,COMP_UNIT,DTUNIT
      CHARACTER(LEN=32) :: NOMVAR2
      INTEGER           :: TYP_MED
      INTEGER           :: IVAR
      CHARACTER(LEN=32) :: CHA
      INTEGER           :: COMP_NUM,i
      LOGICAL           :: ISVECTOR   
      CHARACTER(LEN=16),DIMENSION(:),allocatable :: NAME2  
      CHARACTER(LEN=16),DIMENSION(:),allocatable :: VUNIT2    
      CHARACTER(LEN=16),DIMENSION(:),allocatable :: VAR_NAME
      CHARACTER(LEN=16),DIMENSION(:),allocatable :: VAR_UNIT
!LOCAL DECLARATION
      INTEGER           :: NB_VAR
!
!-----------------------------------------------------------------------
!
      ! Set the mesh name
      MESHNAME = 'MESH'//char(0)
      ! Set the tine unit
      DTUNIT='S'//char(0)
      DIMVEC = 1
      ! Define the size of the vector variables 
      DO IVAR=1,NVAR
        IF (.NOT.OUTVAR(IVAR)) CYCLE
        NOMVAR2   = NOMVAR(IVAR)
        CALL BLANC2_(NOMVAR2)
        call IFVECTOR_(NOMVAR2,COMP_NUM,ISVECTOR)
        DIMVEC = max(DIMVEC,COMP_NUM)  
      ENDDO
      NB_VAR=0
      ! Loop on all the variables
      DO IVAR = 1, NVAR
        ! If the variable is not to print keep looping
        IF (.NOT.OUTVAR(IVAR)) CYCLE
        NB_VAR=NB_VAR+1
        ! Identify the name and the unit of the variable
        NOMVAR2   = NOMVAR(IVAR)
        CALL BLANC2_(NOMVAR2)
        FIELD_NAME = NOMVAR2(1:16)//CHAR(0)
        COMP_NAME = NOMVAR2(1:16)//char(0)
        COMP_UNIT = NOMVAR2(17:32)//CHAR(0)
        NCOMP     = 1 
        TYP_MED   = MED_FLOAT64        
        ! Check if the varaible is the componant of a vector
        call IFVECTOR_(FIELD_NAME,COMP_NUM,ISVECTOR)
        ! If we have a vector set the nuber of component to DIMVEC
        ! else set the componant number to 1
        if (ISVECTOR) then
          NCOMP = DIMVEC 
        else
          COMP_NUM = 1
        endif
        ! If first time we meet the variable (multiple time for vector)
        if (COMP_NUM.eq.1) then
          allocate(VAR_NAME(NCOMP),VAR_UNIT(NCOMP))
        endif
        ! Add name and unit to the variable table
        VAR_NAME(COMP_NUM) = COMP_NAME
        VAR_UNIT(COMP_NUM) = COMP_UNIT
        ! If the variable is a scalar or if we are on the last componant
        ! of a vector we create a new field
        if ((ISVECTOR.and.COMP_NUM==DIMVEC).or.(.not.ISVECTOR)) then
          CALL MFDCRE(RES_FILE,   ! FILE UNIT
     *                FIELD_NAME, ! NAME OF THE DATA FIELD
     *                TYP_MED,    ! TYPE (REAL,INTEGER)  
     *                NCOMP,      ! NUMBER OF COMPONENT
     *                VAR_NAME,   ! NAME OF THE COMPONENT
     *                VAR_UNIT,   ! UNITS OF THE COMPONENT
     *                DTUNIT,        ! UNIT OF THE TIME 
     *                MESHNAME,   ! NAME OF THE MESH
     *                CRET)       ! RETURN CODE
          CALL MED_CHECK_CRET(CRET,'MFDCRE') 
          DEALLOCATE(VAR_NAME,VAR_UNIT)
        ENDIF
      ENDDO

! Geeting reading to write scalar grid for deformed mesh
      IF (DIMVEC==3) THEN
          allocate(NAME2((3+NB_VAR)),VUNIT2((3+NB_VAR)))
          NOMVAR2   = 'MODIF FIELD'
          CALL BLANC2_(NOMVAR2)
          FIELD_NAME = NOMVAR2(1:16)//CHAR(0)
          NAME2(1)    = 'X'
          VUNIT2(1)   = 'M'
          NAME2(2)    = 'Y'
          VUNIT2(2)   = 'M'
          NAME2(3)    = 'Z'
          VUNIT2(3)   = 'M'
          NB_VAR=0
          DO IVAR = 1, NVAR
              IF (.NOT.OUTVAR(IVAR)) CYCLE
              NB_VAR=NB_VAR+1
              NOMVAR2   = NOMVAR(IVAR)
              CALL BLANC2_(NOMVAR2)
              NAME2(3+NB_VAR) =NOMVAR2(1:16)
              VUNIT2(3+NB_VAR) = NOMVAR2(17:32)
          ENDDO
          NCOMP      = 3+NB_VAR

          CALL MFDCRE(RES_FILE,    ! FILE UNIT
     *                FIELD_NAME,  ! NAME OF THE DATA FIELD
     *                TYP_MED,     ! TYPE (REAL,INTEGER)  
     *                NCOMP,       ! NUMBER OF COMPONENT
     *                NAME2,       ! NAME OF THE COMPONENT
     *                VUNIT2,      ! UNITS OF THE COMPONENT
     *                DTUNIT,      ! UNIT OF THE TIME 
     *                MESHNAME,    ! NAME OF THE MESH
     *                CRET)        ! RETURN CODE
        CALL MED_CHECK_CRET(CRET,'MFDCRE')
         deallocate(NAME2,VUNIT2)
      ENDIF

#else
!
! MED LIBRARY IS NOT AVAILABLE
! OR -DHAVE_MED HAS NOT BEEN SET IN CONFIG FILE
      WRITE(LU,*) 'ERROR : TRYING TO CALL MED FUNCTION WITHOUT ',
     *            'MED LIBRARY INSTALLED'
      CALL PLANTE(0)
!
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CREATE_DATASET_MED
!      
!                       *************************
                        SUBROUTINE WRITE_DATA_MED
!                       *************************
!
     *(RES_FILE,NVARS,TIME,TIMESTEP,NOMVAR,OUTVAR,BVARSOR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
Cbrief    WRITE DATA VALUES ON A MESH INTO THE MED DATA FILE.
!+        DATA VALUES ARE STORED IN A BIEF_OBJ BLOCK (BVARSOR),
!+        AND THE LOGICAL OUTVAR INDICATES FOR EACH VARIABLE IF 
!+        WE SHOULD PRINT IT OUT OR NOT. THE BLOCK BVARSOR CONTAINS
!+        ALL VARIABLES WHICH WE CAN WRITE OUT. IF THE VARIABLE IS A
!+        VECTOR, THE CORRESPONDING ENTRY INTO THE BLOCK BVARSOR IS
!+        A BLOCK, HAVING AS MUCH COMPONENTS AS THE VECTOR HAS.
!+        TODO / TOCHECK : IF IT IS A TENSOR ...
!+        TIME IS GIVEN AS A REAL NUMBER, TIMESTEPS ARE INTEGER VALUES.
!+        TODO / TOCHECK : ARE THIS THE TIMESTEPS OF SIMULATION OR OF OUTPUT?
!+        !!!! THE ALGORITHM FOR NAMING THE DATA FIELDS SHOULD BE THE SAME AS
!+        FOR THE SUBROUTINE CREATING THIS FIELDS (MED_DATASET_CREATE)
!                     
!history  J.HOLLEVILLE
!                     
!history  Y.AUDOUIN
!+        29/10/2011
!+        V6P2
!+        Updating med functions for med 3.0.4                        
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| RES_FILE       |-->| MED FILE DESCRIPTOR
!| NVARS          |-->| NUMBER OF VARIABLES
!| TIME           |-->| TIME FOR WHICH THE DATA ARE WRITTEN 
!| TIMESTEP       |-->| TIME STEP FOR WHICH THE DATA ARE WRITTEN
!| NOMVAR         |-->| TABLE CONTAING THE NAME OF EACH VARIABLES
!| OUTVAR         |-->| TABLE SAYING IF A VARIABLE IS TO BE PRINTED
!| BVARSOR        |-->| VALUES FOR EACH VARIABLES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
      USE BIEF
      USE DECLARATIONS_TELEMAC, only : NNAMECODE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          ,INTENT(IN)                   :: RES_FILE
      INTEGER          ,INTENT(IN)                   :: NVARS
      DOUBLE PRECISION ,INTENT(IN)                   :: TIME
      INTEGER          ,INTENT(IN)                   :: TIMESTEP
      CHARACTER(LEN=32),DIMENSION(NVARS), INTENT(IN) :: NOMVAR
      LOGICAL          ,DIMENSION(NVARS), INTENT(IN) :: OUTVAR
      TYPE(BIEF_OBJ)   ,INTENT(IN)                   :: BVARSOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined(HAVE_MED)
      INTEGER :: CRET     ! RETURN CODE
      INTEGER :: IVAR     ! VARIABLE ID TO OUTPUT
      INTEGER :: NDATA    ! NUMBER OF DATA POINTS TO WRITE
      INTEGER :: TYP_MED  ! TYPE OF THE ELEMENT
      INTEGER :: TYP_DISC ! TYPE OF DISCRETISATION
!
      INTEGER :: NUMMESH
      INTEGER :: DIMMESH ! DIMENSION OF THE MESH
      INTEGER :: TYPE1   ! TYPE OF THE MESH (STRUCTURED / UNSTRUCTURED)
      CHARACTER(LEN=64) :: MESHNAME ! NAME OF THE ASSOCIATED MESH
      CHARACTER(LEN=200) :: DESC ! DESCRIPTION ASSOCIATED TO THE MESH

      CHARACTER(LEN=64) :: FIELD_NAME ! NAME OF THE MED FIELD
      CHARACTER(LEN=32) :: NOMVAR2   ! MED NAME OF THE VARIABLE
!
      INTEGER         , DIMENSION(:), allocatable :: IDATAVAR
      DOUBLE PRECISION, DIMENSION(:), allocatable :: DATAVAR
      DOUBLE PRECISION, DIMENSION(:), allocatable,save :: DEF_MAP
      CHARACTER(LEN=32) :: CHA
      INTEGER           :: COMP_NUM
      LOGICAL           :: ISVECTOR  
      INTEGER           ::  LME,TIMESTEP2
      CHARACTER*16      :: DTUNIT
      CHARACTER*16, ALLOCATABLE :: COORD_NAME(:), COORD_UNIT(:)
      INTEGER           :: NB_VAR,I,I1,I2,I3,I4,I5
!
!-----------------------------------------------------------------------
!
      ! Setting the meshname
      MESHNAME = 'MESH'//char(0)
      ! Get the mesh dimension
      call mmhnan(RES_FILE,MESHNAME,DIMMESH,CRET)
      CALL MED_CHECK_CRET(CRET,'MMHNAN')
      ! The timestep are to be counted consecutively from 1
      ! We use the local variable local_timestep_2d to recount the timestep
      ! The variable is defined at the beginning of the module
      ! This way the value is global and is increased each time the
      ! function is called
      ! The function is called twice in 3D 
      ! Defining which timstep to use
      if(NNAMECODE(1).eq.'TELEMAC3D') then
        TIMESTEP2 = local_timestep_3d
        local_timestep_3d = local_timestep_3d + 1
      else if (NNAMECODE(1).eq.'TELEMAC2D') then
        TIMESTEP2 = local_timestep_2d
        local_timestep_2d = local_timestep_2d + 1
      endif

      ! PUISQU'IL Y A DES VALEURS SUR VECTEURS LIBRE ...
      ! ON DETERMINE LE TYPE DE VARIABLE QU'UNE SEULE FOIS ET CROISE LES
      ! DOIGTS (DANS TELEMAC2D, L'ENERGIE TURBULENTE A UN ELM DE -1000
      ! BIZARREMENT ...
      
      IVAR=0
      I=1
      DO WHILE(IVAR == 0)
        IF(BVARSOR%ADR(I)%P%ELM.GT.0) IVAR=I
        I=I+1
      ENDDO
      IF(BVARSOR%ADR(IVAR)%P%ELM/10 .EQ. 1 ) THEN
        TYP_MED = MED_TRIA3 
      ELSEIF(BVARSOR%ADR(IVAR)%P%ELM/10 .EQ. 2 ) THEN
        TYP_MED = MED_QUAD4 
      ELSEIF(BVARSOR%ADR(IVAR)%P%ELM/10 .EQ. 3 ) THEN
        TYP_MED = MED_TETRA4
      ELSEIF(BVARSOR%ADR(IVAR)%P%ELM/10 .EQ. 4 ) THEN
        TYP_MED = MED_PENTA6 
      ELSE
        WRITE(LU,*)'ERROR ON ELEMENT TYPE!',BVARSOR%ADR(IVAR)%P%ELM
        CALL PLANTE(1)
        STOP
      ENDIF
      IVAR=0
      I=1


      IVAR=1
      NB_VAR = 0
      ! Loop on all the variables
      DO IVAR = 1 , NVARS
        ! If the variable is not printed we keep looping
        IF (.NOT. OUTVAR(IVAR)) CYCLE
        TYP_DISC = MED_NODE
        LME = BVARSOR%ADR(IVAR)%P%ELM
        IF ((LME.GT.0) .and. ((LME-(LME/10)*10)==0)) 
     *                            TYP_DISC = MED_CELL 
        ! Defining the varaible name
        NB_VAR = NB_VAR+1
        NOMVAR2   = NOMVAR(IVAR)
        CALL BLANC2_(NOMVAR2)
        FIELD_NAME = NOMVAR2(1:16)//CHAR(0)
        NDATA   =  BVARSOR%ADR(IVAR)%P%DIM1
        ! Check if the varaible is part of a vector
        call IFVECTOR_(FIELD_NAME(1:32),COMP_NUM,ISVECTOR)
        ! we allocate the result tables only on the first componant
        if (COMP_NUM == 1) allocate(DATAVAR(DIMMESH*NDATA))
        if (COMP_NUM == 1) allocate(IDATAVAR(DIMMESH*NDATA))
        ! Filling the tables for real/integer values
        if (BVARSOR%ADR(IVAR)%P%NAT == 1) then         !real
          if (ISVECTOR)then
            DATAVAR((COMP_NUM-1)*NDATA+1:(COMP_NUM)*NDATA)=
     *                            BVARSOR%ADR(IVAR)%P%R(1:NDATA)
          else
            allocate(DATAVAR(1:NDATA))
            allocate(IDATAVAR(1:NDATA))
            DATAVAR(1:NDATA)=BVARSOR%ADR(IVAR)%P%R(1:NDATA)
          endif
        elseif (BVARSOR%ADR(IVAR)%P%NAT == 2) then    ! integer
          IF (ISVECTOR)THEN
            IDATAVAR((COMP_NUM-1)*NDATA+1:(COMP_NUM)*NDATA)=
     *                          BVARSOR%ADR(IVAR)%P%I(1:NDATA)
          ELSE
            allocate(DATAVAR(1:NDATA))
            allocate(IDATAVAR(1:NDATA))
            IDATAVAR(1:NDATA) = BVARSOR%ADR(IVAR)%P%I(1:NDATA)
            DATAVAR(1:NDATA) = dble(IDATAVAR(1:NDATA))
          ENDIF
        endif

        ! If we are on the last componant of a vector
        ! or if the varaible is a scalar
        ! we write the data on the med file
        if ((COMP_NUM==DIMMESH).or.(.not.ISVECTOR)) then
          CALL MFDRVW(RES_FILE,            ! FILE UNIT
     *                FIELD_NAME,          ! NAME OF THE FIELD
     *                TIMESTEP2,      ! TIME STEP (INTEGER)
     *                MED_NO_IT,           ! NO ITERATION
     *                TIME,                ! TIME STEP (REAL)
     *                MED_NODE,            ! GEOMETRY TYPE
     *                MED_NONE,            ! ELEMENT TYPE
     *                MED_NO_INTERLACE,    ! STORAGE MODE
     *                MED_ALL_CONSTITUENT, ! COMPONENT TO WRITE
     *                NDATA,               ! NUMBER OF VALUES
     *                DATAVAR,             ! VALUES TABLE
     *                CRET)                ! RETURN CODE
          CALL MED_CHECK_CRET(CRET,'MFDRVW')
          deallocate(DATAVAR)
          deallocate(IDATAVAR)
        endif

      END DO ! LOOP OVER ENTRIES IN BVARSOR

!On rajoute la possibilite en 3D d'ecrire un champ de scalaire pour la 
!deformation du maillage
!les 3 premieres composantes correspondnant au vecteur deformation du 
!maillage (pas de deformation = (0,0,0)
!les autres composantes correspondnant aux diffÃ©rents champs ecrit dans
! le fichier med il s'agit d une surdefintion qui peut etre couteuse en 
!espace memoire suivant le nombre de variable et le type de cas de calcu
! This function is also called for the 2d mesh in telemac3d
      IF (NNAMECODE(1) == 'TELEMAC3D               '
     * .AND. dimmesh.EQ.3)THEN
          NOMVAR2   = 'MODIF FIELD'
          CALL BLANC2_(NOMVAR2)
          FIELD_NAME = NOMVAR2(1:16)//CHAR(0)
          NDATA   =  BVARSOR%ADR(1)%P%DIM1
          if (TIMESTEP==0) allocate(DEF_MAP(1:NDATA*3))
          if (TIMESTEP==0) DEF_MAP(1:2*NDATA)        = 0.0d0  
          if (TIMESTEP==0) DEF_MAP(2*NDATA+1:3*NDATA)= 
     *                                BVARSOR%ADR(1)%P%R(1:NDATA)
          allocate(DATAVAR(1:NDATA*(3+NB_VAR)))
          DATAVAR(2*NDATA+1:3*NDATA)= BVARSOR%ADR(1)%P%R(1:NDATA)-
     *            DEF_MAP(2*NDATA+1:3*NDATA)
          DATAVAR(1:2*NDATA)        = 0.0d0
          NB_VAR=0
          DO IVAR = 1, NVARS
              IF (.NOT. OUTVAR(IVAR)) CYCLE
              NB_VAR=NB_VAR+1
              DATAVAR((2+NB_VAR)*NDATA+1:(3+NB_VAR)*NDATA)= 
     *                                BVARSOR%ADR(IVAR)%P%R(1:NDATA)
          ENDDO

          CALL MFDRVW(RES_FILE,            ! FILE UNIT
     *                FIELD_NAME,          ! NAME OF THE FIELD
     *                TIMESTEP2,           ! TIME STEP (INTEGER)
     *                MED_NO_IT,           ! NO ITERATION
     *                TIME,                ! TIME STEP (REAL)
     *                MED_NODE,            ! GEOMETRY TYPE
     *                MED_NONE,            ! ELEMENT TYPE
     *                MED_NO_INTERLACE,    ! STORAGE MODE
     *                MED_ALL_CONSTITUENT, ! COMPONENT TO WRITE
     *                NDATA,               ! NUMBER OF VALUES
     *                DATAVAR,             ! VALUES TABLE
     *                CRET)                ! RETURN CODE
          CALL MED_CHECK_CRET(CRET,'MFDRVW')
          deallocate(DATAVAR)          
      ENDIF
!
#else
!
! MED LIBRARY IS NOT AVAILABLE
! OR -DHAVE_MED HAS NOT BEEN SET IN CONFIG FILE
      WRITE(LU,*) 'ERROR : TRYING TO CALL MED FUNCTION WITHOUT ',
     *            'MED LIBRARY INSTALLED'
      CALL PLANTE(0)
!
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE WRITE_DATA_MED
!      
!                       ********************
                        SUBROUTINE SUITE_MED
!                       ********************
!
     *(VARSOR,CLAND,NUMDEB,
     * RES_FILE,STD,HIST,NHIST,NPOIN,AT,TEXTPR,VARCLA,NVARCL,
     * TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NPLAN,DT,NDT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
Cbrief    READING RESULTS FROM A MED FILE
!                     
!history  J.HOLLEVILLE
!                     
!history  Y.AUDOUIN
!+        29/10/2011
!+        V6P2
!+        Updating med functions for med 3.0.4                        
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| VARSOR         |<--| BLOC DES TABLEAUX CONTENANT LES VARIABLES
!| CLAND          |<--| BLOC DES VARIABLES CLANDESTI-NES
!| NUMDEB         |<->| FIN = .TRUE. NUMERO DU DERNIER ENREGISTREMENT
!|                |   | FIN = .FALSE. : NUMERO DE L'ENREGISTREMENT
!|                |   |                 QUE L'ON VEUT LIRE.
!| RES_FILE       |-->| NUMERO DE CANAL DU FICHIER
!| STD            |-->| BINAIRE DU FICHIER : STD, IBM OU I3E
!| HIST           |-->| TABLEAU DE VALEURS MISES DANS L'ENREGISTREMENT
!|                |   | DU TEMPS.
!| NHIST          |-->| NOMBRE DE VALEURS DANS LE TABLEAU HIST.
!| NPOIN          |-->| NOMBRE DE POINTS DANS LE MAILLAGE
!| AT             |-->| TEMPS
!| TEXTPR         |-->| NOMS ET UNITES DES VARIABLES.
!| VARCLA         |-->| TABLEAU OU L'ON RANGE LES VARIABLES
!|                |   | CLANDESTIINES.
!| NVARCL         |-->| NOMBRE DE VARIABLES CLANDESTI-NES.
!| TROUVE         |<--| INDIQUE (TROUVE(K)=1) LES VARIABLES TROUVEES
!|                |   | DANS LE FICHIER.
!|                |   | DE K =  1 A 26 VARIABLES NORMALES
!|                |   | DE K = 27 A 36 VARIABLES CLANDESTI-NES.
!| ALIRE          |-->| VARIABLES QU'IL FAUT LIRE (POUR LES AUTRES ON
!|                |   | SAUTE L'ENREGISTREMENT CORRESPONDANT)
!|                |   | LES VARIABLES CLANDESTI-NES SONT LUES
!|                |   | SYSTEMATIQUEMENT.
!| LISTIN         |-->| SI OUI, IMPRESSION D'INFORMATIONS SUR LISTING
!| FIN            |-->| VOIR LE TROISIEME ARGUMENT NUMDEB
!| MAXVAR         |-->| DIMENSION DES TABLEAUX DES VARIABLES : ALIRE, ETC
!| NPLAN          |-->| DIMENSION DES TABLEAUX DES VARIABLES : ALIRE, ETC
!| DT             |-->| DIMENSION DES TABLEAUX DES VARIABLES : ALIRE, ETC
!| NDT            |-->| DIMENSION DES TABLEAUX DES VARIABLES : ALIRE, ETC
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARSOR,CLAND
      INTEGER, INTENT(IN), OPTIONAL :: NPLAN
      INTEGER, INTENT(IN)           :: NHIST,NVARCL,MAXVAR,RES_FILE
      DOUBLE PRECISION, INTENT(OUT), OPTIONAL :: DT
      INTEGER, INTENT(OUT), OPTIONAL :: NDT
      INTEGER                       :: NUMDEB,NPOIN,TROUVE(MAXVAR)
      INTEGER                       :: ALIRE(MAXVAR)        
      CHARACTER(LEN=*)              :: STD
      CHARACTER(LEN=32)             :: TEXTPR(MAXVAR),VARCLA(NVARCL)
      DOUBLE PRECISION              :: HIST(*),AT
      LOGICAL                       :: FIN,LISTIN 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
#if defined(HAVE_MED)      
      INTEGER :: CRET    ! RETURN CODE
      INTEGER :: IVAR    ! VARIABLE ID TO OUTPUT
      INTEGER :: TYPE1   ! TYPE OF THE MESH (STRUCTURED / UNSTRUCTURED)
      INTEGER :: NDATA   ! NUMBER OF DATA POINTS TO WRITE
      INTEGER :: DIMMESH ! DIMENSION OF THE MESH
      INTEGER :: TYP_MED ! NUMBER OF COMPONENTS
!
      CHARACTER(LEN=64) :: MESHNAME ! NAME OF THE ASSOCIATED MESH
      CHARACTER(LEN=64) :: FIELD_NAME ! NAME OF THE MED FIELD
      CHARACTER(LEN=32) :: NOMVAR2   ! MED NAME OF THE VARIABLE
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DATAVAR
      INTEGER :: NTIME
!     DUMMY OUTPUT ARGUMENT NEED FOR CALCUL
      INTEGER           :: I,NGAUSS,NUMDT,NUMO,NMAA,TYP,NCOMP,NUMDT2
      CHARACTER(LEN=64) :: CHA,CHA2,MESHNAME2
      CHARACTER(LEN=16), allocatable :: COMP(:), UNIT(:)
      character(len=16) :: DTUNIT
      LOGICAL :: ISVECTOR,LOCAL
      INTEGER :: COMP_NUM,DIMVEC,NFIELD,IDUM
!
!-----------------------------------------------------------------------
!
      IVAR=0
      I=1
      ! Finding the 
      DO WHILE(IVAR == 0)
       IF ((VARSOR%ADR(I)%P%ELM.GT.0)) IVAR=I
       I=I+1
      ENDDO
      IF(VARSOR%ADR(IVAR)%P%ELM/10 .EQ. 1 ) THEN
        TYP_MED = MED_TRIA3
      ELSEIF ( VARSOR%ADR(IVAR)%P%ELM/10 .EQ. 2 ) THEN
        TYP_MED = MED_QUAD4  
      ELSEIF ( VARSOR%ADR(IVAR)%P%ELM/10 .EQ. 3 ) THEN
        TYP_MED = MED_TETRA4
      ELSEIF ( VARSOR%ADR(IVAR)%P%ELM/10 .EQ. 4 ) THEN
        TYP_MED = MED_PENTA6 
      ELSE
        WRITE(LU,*) 'ERROR ON ELEMENT TYPE!',VARSOR%ADR(IVAR)%P%ELM
        CALL PLANTE(1)
        STOP
      ENDIF
      IVAR=0
      I=1
      DO WHILE(IVAR == 0)
        IF (ALIRE(I)==1) IVAR=I
        I=I+1
      ENDDO
      ! Build the field anme associated wit the varaible
      NOMVAR2   = TEXTPR(IVAR)
      CALL BLANC2_(NOMVAR2)
      FIELD_NAME = NOMVAR2(1:16)//CHAR(0)
      CALL IFVECTOR_(FIELD_NAME,COMP_NUM,ISVECTOR)      
      ! Get the number of componant in the field
      CALL MFDNCN(RES_FILE,FIELD_NAME,NCOMP,CRET)
      CALL MED_CHECK_CRET(CRET,'MFDNFC')
      ! Get the number of time step in the field
      ALLOCATE(UNIT(NCOMP),COMP(NCOMP))
      CALL MFDFIN(RES_FILE,FIELD_NAME,MESHNAME,IDUM,TYP,COMP,UNIT
     *            ,DTUNIT,NTIME,CRET)
      CALL MED_CHECK_CRET(CRET,'MFDFIN') 
      DEALLOCATE(COMP,UNIT)
      ! Get the time associated to the last time step
      CALL MFDCSI(RES_FILE,FIELD_NAME,NTIME,IDUM,IDUM,
     *           DT,CRET)
      CALL MED_CHECK_CRET(CRET,'MFDCSI')
      NUMDEB = NTIME
      AT = DT
      NUMDT = NTIME
      ! Search for the name of the field CHA and compare to the reading
      ! list => TROUVE = 1
      ! Get the number of fields
      CALL MFDNFD(RES_FILE,NFIELD,CRET)
      CALL MED_CHECK_CRET(CRET,'MFDNFD')
      TROUVE = 0
      DIMVEC = 1
      ! Loop on each field
      DO I=1,NFIELD
        ! Get the number fo componant of the field I
        CALL MFDNFC(RES_FILE,I,NCOMP,CRET)
        CALL MED_CHECK_CRET(CRET,'MFDNFC')
        ! get the name of the field I
        ALLOCATE(UNIT(NCOMP),COMP(NCOMP))
        CALL MFDFDI(RES_FILE,I,CHA,MESHNAME2,IDUM,IDUM,COMP,
     &              UNIT,DTUNIT,IDUM,CRET)
        CALL MED_CHECK_CRET(CRET,'MFDFDI')
        DEALLOCATE(COMP,UNIT)
        ! Identify the varaible associated with the field I
        DO IVAR = 1,MAXVAR
          NOMVAR2   = TEXTPR(IVAR)
          CALL BLANC2_(NOMVAR2)
          CALL IFVECTOR_(NOMVAR2,COMP_NUM,ISVECTOR)
          DIMVEC = MAX(DIMVEC,COMP_NUM)
          IF (NOMVAR2(1:16) == CHA(1:16)) TROUVE(IVAR)=1
        ENDDO
      ENDDO
      ! Get the table value for the last time step for each 'found'
      DO IVAR = 1, MAXVAR
        IF (TROUVE(IVAR)/=1) CYCLE
        IF (ALIRE(IVAR)==1) THEN 
          NOMVAR2   = TEXTPR(IVAR)
          CALL BLANC2_(NOMVAR2)
          FIELD_NAME = NOMVAR2(1:16)//CHAR(0)
          CALL IFVECTOR_(FIELD_NAME,COMP_NUM,ISVECTOR)
          NDATA   =  VARSOR%ADR(IVAR)%P%DIM1
          IF(ISVECTOR) THEN
            ALLOCATE(DATAVAR(DIMVEC*NDATA))
          ELSE
            ALLOCATE(DATAVAR(NDATA))
          ENDIF
          write(lu,*) 'FIELD_NAME : ',FIELD_NAME 
          write(lu,*) 'NUMDT : ',NUMDT 
          write(lu,*) 'DT : ',DT 
          write(lu,*) 'ISVECTOR : ',ISVECTOR 
          write(lu,*) 'NDATA : ',NDATA 
          write(lu,*) 'DIMVEC : ',DIMVEC 
         
          CALL MFDRVR(RES_FILE,           ! FILE UNIT
     *                FIELD_NAME,         ! FIELD NAME
     *                NUMDT,              ! TIME STEP (INTEGER)
     *                MED_NO_IT,          ! NO ITERATION
     *                MED_NODE,           ! GEOMETRY TYPE
     *                MED_NONE,           ! NO ENTITY TYPE
     *                MED_NO_INTERLACE,   ! STORGAE MODE
     *                MED_ALL_CONSTITUENT,! COMPONENT
     *                DATAVAR,            ! RESULT TABLE
     *                CRET)               ! RETURN CODE
          CALL MED_CHECK_CRET(CRET,'EFCHAL')
          NDATA   =  VARSOR%ADR(IVAR)%P%DIM1
          IF (.NOT.ISVECTOR) VARSOR%ADR(IVAR)%P%R = DATAVAR
          IF (ISVECTOR) VARSOR%ADR(IVAR)%P%R(1:NDATA) =
     *                DATAVAR((COMP_NUM-1)*NDATA+1:(COMP_NUM)*NDATA)
          DEALLOCATE(DATAVAR)
        ELSE
          IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,75) TEXTPR(IVAR)
          IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,76) TEXTPR(IVAR)
75        FORMAT(/,1X,'LA VARIABLE : ',A32,/,1X,
     *              'EST DANS LE FICHIER MAIS ELLE N''EST PAS LUE')
76        FORMAT(/,1X,'VARIABLE : ',A32,/,1X,
     *              'IS IN THE FILE BUT WILL NOT BE READ')
        ENDIF
      END DO ! LOOP OVER ENTRIES IN VARSOR
!
!-----------------------------------------------------------------------
!
!  IMPRESSIONS :
!
      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,300) MESHNAME
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,301) MESHNAME
300   FORMAT(1X,//,1X,'TITRE DU CAS PRECEDENT: ',A72,/)
301   FORMAT(1X,//,1X,'TITLE OF PREVIOUS COMPUTATION: ',A72,/)
!
      DO IVAR=1,MAXVAR
        IF ((TROUVE(IVAR) ==1).and.(ALIRE(IVAR) == 1)) THEN
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,11)
     *    TEXTPR(IVAR)(1:16),TEXTPR(IVAR)(17:32)
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,111)
     *    TEXTPR(IVAR)(1:16),TEXTPR(IVAR)(17:32)
        ENDIF
11      FORMAT(1X,'NOM: ' ,A16,'  UNITE: ',A16)
111     FORMAT(1X,'NAME: ',A16,'  UNIT: ' ,A16)
      ENDDO

      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,130) NUMDEB
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,131) NUMDEB
130   FORMAT(/,1X,'SUITE_MED : LECTURE A L''ENREGISTREMENT ',1I5)
131   FORMAT(/,1X,'SUITE_MED : READ OF RECORD ',1I5)

      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,140) AT
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,141) AT
140   FORMAT(//,1X,'TEMPS DE L''ENREGISTREMENT : ',G16.7,' S')
141   FORMAT(//,1X,'TIME OF RECORD: ',G16.7,' S')

      IF(PRESENT(NDT)) NDT=NUMDT
           
#else
!
! MED LIBRARY IS NOT AVAILABLE
! OR -DHAVE_MED HAS NOT BEEN SET IN CONFIG FILE
      WRITE(LU,*) 'ERROR : TRYING TO CALL MED FUNCTION WITHOUT ',
     *            'MED LIBRARY INSTALLED'
      CALL PLANTE(-1)
!
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SUITE_MED
!
!                       ******************
                        SUBROUTINE BLANC2_
!                       ******************
!
     *(STRING)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITE ' ' TO '_'
!                     
!history  J.HOLLEVILLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| STRING         |<->| THE STRING TO CONVERT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CHARACTER*(*), INTENT(INOUT) :: STRING
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: J
!
!-----------------------------------------------------------------------
!
      DO J = 1,LEN(STRING)
        IF (STRING(J:J) .EQ. ' ') THEN
          STRING(J:J) = '_'
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE BLANC2_
!      
!                       ********************
                        SUBROUTINE IFVECTOR_
!                       ********************
!
     *(STRING,COMP_NUM,ISVECTOR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FIND '_U_' '_V_' '_W_' '_X_' '_Y_' '_Z_' in name field of scalar 
!+        or vector
!+        return : ISVECTOR = 
!+        return : COMP_NUM =  
!                     
!history  J.HOLLEVILLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| STRING         |<->| THE NAME OF THE VARIABLE TO TEST
!| COMP_NUM       |<--| direction of vector
!| ISVECTOR       |<--| TRUE, it's a vector
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CHARACTER(LEN=32), INTENT(INOUT) :: STRING
      INTEGER, INTENT(OUT)             :: COMP_NUM
      LOGICAL, INTENT(OUT)             :: ISVECTOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: J
!
!-----------------------------------------------------------------------
!
      ISVECTOR = .FALSE.
      COMP_NUM = 0
       
      IF (STRING(1:6)/='COTE_Z') then
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
      endif
!
      RETURN
      END SUBROUTINE IFVECTOR_

!
!-----------------------------------------------------------------------
!
      END MODULE M_MED
