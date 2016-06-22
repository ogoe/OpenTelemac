!                 ********************
                  MODULE UTILS_SERAFIN
!                 ********************
!
!***********************************************************************
! HERMES  V7P1
!***********************************************************************
!
!brief    a number of subroutines dedicated to the serafin format.
!
!history YOANN AUDOUIN
!+       29/10/2011
!+       V7P1
!+       Creation of the file
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+       12/05/2015
!+       V7P1
!+       Correcting an old mistake on serafin files in prisms when the
!+       computation is done with tetrahedra. See SET_MESH_SRF.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: MAX_FILE = 3000
      INTEGER :: NFILES = 0
!
!> JR @ RWTH: ALGORITHMIC DIFFERENTIATION
      INTEGER, PARAMETER :: IS = 4                            ! INTEGER SIZE
      INTEGER, PARAMETER :: I4 = SELECTED_INT_KIND(  8 )      ! INTEGER SIZE
      INTEGER, PARAMETER :: I8 = SELECTED_INT_KIND( 16 )      ! INTEGER ON 8 BYTES SIZE
      INTEGER, PARAMETER :: R4 = SELECTED_REAL_KIND( 5, 20 )  ! SINGLE PRECISION SIZE
      INTEGER, PARAMETER :: R8 = SELECTED_REAL_KIND( 10, 60 ) ! DOUBLE PRECISION SIZE
!      INTEGER, PARAMETER :: IS = 4 ! INTEGER SIZE
!      INTEGER, PARAMETER :: I4 = 4 ! INTEGER SIZE
!      INTEGER, PARAMETER :: I8 = 8 ! INTEGER ON 8 BYTES SIZE
!      INTEGER, PARAMETER :: R4 = 4 ! SINGLE PRECISION SIZE
!      INTEGER, PARAMETER :: R8 = 8 ! DOUBLE PRECISION SIZE
!< JR @ RWTH
      INTEGER, PARAMETER :: VAR_SIZE = 32 ! SIZE OF A VARIABLE TEXT
      INTEGER, PARAMETER :: TITLE_SIZE = 80 ! SIZE OF A TITLE
!
      TYPE SRF_INFO
        ! SIZE OF ELEMENTS
        INTEGER :: RS ! REAL SIZE (4 OR 8)
        ! POSITION IN FILE
        INTEGER(KIND=I8) :: POS_TITLE
        INTEGER(KIND=I8) :: POS_NVAR != POS_TITLE + 4 + TITLE_SIZE + 4
        INTEGER(KIND=I8) :: POS_VARINFO != POS_NVAR + 4 + 2*IS + 4
        INTEGER(KIND=I8) :: POS_IB != POS_VARINFO + 4 + NVAR*VAR_SIZE + 4
        INTEGER(KIND=I8) :: POS_DATE != POS_IB + 4 + 10*IS + 4
        INTEGER(KIND=I8) :: POS_NUM != POS_DATE + (IB(10).NE.0)*(4 + 6*IS + 4)
        INTEGER(KIND=I8) :: POS_IKLE != POS_NUM + 4 + 4*IS + 4
        INTEGER(KIND=I8) :: POS_IPOBO != POS_IKLE + 4 + NELEM*NDP*IS + 4
        INTEGER(KIND=I8) :: POS_COORD != POS_IPOBO + 4 + NPOIN*IS + 4
        INTEGER(KIND=I8) :: POS_DATA != POS_COORD + (4 + NPOIN*RS + 4)*NDIM
        ! COMPUTED INFORMATIONS
        INTEGER :: SIZE_DATA != 4 + NPOIN*RS + 4
        INTEGER :: SIZE_DATA_SET != 4 + RS + 4 + NVAR*(4 + NPOIN*RS + 4)
        ! STOCKED QUANTITIES AND SMALL VARIABLES
        INTEGER :: NTIMESTEP
        INTEGER :: NPOIN
        INTEGER :: NVAR
        INTEGER :: NELEM
        INTEGER :: NDP
        INTEGER :: NPLAN
        INTEGER :: NPTIR
        INTEGER :: NDIM
        INTEGER :: TYP_ELT
        CHARACTER(LEN=VAR_SIZE),ALLOCATABLE :: VAR_LIST(:)
        ! BOUNDARY INFORMATIONS
        INTEGER :: TYP_BND_ELT
        INTEGER :: NPTFR
        INTEGER :: NCLI
      END TYPE SRF_INFO

      ! HASH TABLE FOR SERAFIN FILES
      INTEGER :: HASH(MAX_FILE) = 0
      TYPE(SRF_INFO),SAVE :: SRF_OBJ_TAB(MAX_FILE)
!
!-----------------------------------------------------------------------
!
      CONTAINS
!***********************************************************************
      SUBROUTINE GET_SRF_OBJ
!***********************************************************************
!
     &(FILE_ID,INFO,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the index in the srf_obj_tab for a give file id
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| ID OF THE FILE
!| INFO           |<->| ID IN THE SRF_OBJ_TAB
!| IERR           |-->| 0 IF EVERYTHING WENT FINE, ERROR INDEX OTHERWISE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(INOUT) :: INFO
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID, I
        !
        SRF_ID = 0
        ! LOOK FOR THE ID LINKED TO THAT FILE_ID
        IF(NFILES.GE.1) THEN
          DO I=1,MAX_FILE
            IF (HASH(I).EQ.FILE_ID) THEN
              SRF_ID = I
              EXIT
            ENDIF
          ENDDO
        ENDIF
        ! IF SRF_ID == 0 THEN NO ID WAS FOUND RETURN ERROR
        IF(SRF_ID.EQ.0) THEN
          IERR = HERMES_FILE_NOT_OPENED_ERR
        ELSE
          IERR = 0
        ENDIF
        INFO = SRF_ID
        !
        RETURN
      END SUBROUTINE

!***********************************************************************
      SUBROUTINE ADD_SRF_FILE
!***********************************************************************
!
     &(FILE_ID,SRF_ID,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Add a new file to the srf_obj_tab and returns its new id
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| ID OF THE FILE
!| SRF_ID         |<->| ID IN THE SRF_OBJ_TAB
!| IERR           |-->| 0 IF EVERYTHING WENT FINE, ERROR INDEX OTHERWISE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN)  :: FILE_ID
        INTEGER, INTENT(INOUT) :: SRF_ID
        INTEGER, INTENT(OUT) :: IERR
        !
        SRF_ID = 0
        ! CHECK IF THE FILE IS ALREADY OPEN
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        IF(SRF_ID.NE.0) THEN
          IERR = HERMES_FILE_ID_ALREADY_IN_USE_ERR
          RETURN
        ENDIF
        ! WE RESET THE IERR TO ZERO AS THE PREVIOUS CALL SHOULD
        ! HAVE CRASHED AS THE FILS IS NOT OPENED YET
        IERR = 0
        ! CHECK IF WE'VE REACH THE MAXIMUM NUMBER OF FILES
        IF(NFILES.EQ.MAX_FILE) THEN
          IERR = HERMES_MAX_FILE_ERR
          RETURN
        ENDIF
        !
        ! Look far a place in the hash table
        SRF_ID = 1
        DO
          IF(HASH(SRF_ID).EQ.0) EXIT
          SRF_ID = SRF_ID + 1
        ENDDO
        NFILES = NFILES + 1
        HASH(SRF_ID) = FILE_ID
        !
        RETURN
      END SUBROUTINE

!***********************************************************************
      SUBROUTINE IDENTIFY_TYP_ELT
!***********************************************************************
!
     &(NDP,NDIM,TYP_ELT)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of point per element and dimension
!+        for the given element type
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NDP            |<--| NUMBER OF POINT PER ELEMENT
!| NDIM           |-->| DIMENSION OF THE ELEMENT (2D OR 1D)
!| TYP_ELT        |-->| TYPE OF THE ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !TODO: Merge with the one in special
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN)  :: NDP
        INTEGER, INTENT(IN)  :: NDIM
        INTEGER, INTENT(OUT) :: TYP_ELT
        !
        TYP_ELT = 0
        ! Returns the element type by checking the couple
        ! (dimension, number of point per element)
        IF(NDIM.EQ.3) THEN
          IF(NDP.EQ.4) TYP_ELT = TETRAHEDRON_ELT_TYPE
          IF(NDP.EQ.6) TYP_ELT = PRISM_ELT_TYPE
        ELSE
          IF(NDP.EQ.3) TYP_ELT = TRIANGLE_ELT_TYPE
          IF(NDP.EQ.4) TYP_ELT = QUADRANGLE_ELT_TYPE
        ENDIF
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE IDENTIFY_ENDIAN_TYPE
!***********************************************************************
!
     &(FILE_NAME,OPENMODE,ENDIAN)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Read the first tag of a Serafin file to detect if it is in
!+        little or big Endian
!
!history  Y AUDOUIN (LNHE)
!+        11/05/2016
!+        V7P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |<--| Name of the file
!| OPENMODE       |-->| READ, READWRITE OR WRITE
!| ENDIAN         |-->| Encoding of integer and real in the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        CHARACTER(LEN=*), INTENT(IN) :: FILE_NAME
        CHARACTER(LEN=9), INTENT(IN) :: OPENMODE
        CHARACTER(LEN=13),INTENT(OUT) :: ENDIAN
        !
        INTEGER(KIND=K4) :: I
        INTEGER :: FILE_ID
        LOGICAL :: FILE_EXIST
        INTEGER :: IERR
        !
        ENDIAN = 'BIG_ENDIAN'
        ! If the file does not exist return BIG_ENDIAN (i.e output file)
        INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXIST)
        IF(.NOT.FILE_EXIST) RETURN

        OPEN(FILE=FILE_NAME, ACTION='READ', UNIT=FILE_ID,
     &       FORM='UNFORMATTED', ACCESS='STREAM',CONVERT=ENDIAN,
     &       IOSTAT=IERR)
        CALL CHECK_CALL(IERR,'IDENTIFY_ENDIAN_TYPE:OPEN:BIG')
        READ(FILE_ID,POS=1,IOSTAT=IERR) I
        CALL CHECK_CALL(IERR,'IDENTIFY_ENDIAN_TYPE:READ:BIG')
        CLOSE(FILE_ID)
        IF(I.NE.80) THEN
          ENDIAN = 'LITTLE_ENDIAN'
          OPEN(FILE=FILE_NAME, ACTION='READ', UNIT=FILE_ID,
     &         FORM='UNFORMATTED', ACCESS='STREAM',CONVERT=ENDIAN,
     &         IOSTAT=IERR)
          CALL CHECK_CALL(IERR,'IDENTIFY_ENDIAN_TYPE:OPEN:LITTLE')
          READ(FILE_ID,POS=1,IOSTAT=IERR) I
          CALL CHECK_CALL(IERR,'IDENTIFY_ENDIAN_TYPE:READ:LITTLE')
          CLOSE(FILE_ID)
        ENDIF
      END SUBROUTINE

!***********************************************************************
      SUBROUTINE OPEN_MESH_SRF
!***********************************************************************
!
     &(FILE_NAME,FILE_ID,OPENMODE,FFORMAT,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    OPENS A MESH FILE
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |-->| NAME OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| OPENMODE       |-->| ONE OF THE FOLLOWING VALUE 'READ','READWRITE'
!| FFORMAT        |<->| FORMAT OF THE FILE Can be modified if the user
!|                |   |  made a mistake
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        CHARACTER(LEN=9), INTENT(IN)     :: OPENMODE
        CHARACTER(LEN=*), INTENT(IN) :: FILE_NAME
        CHARACTER(LEN=8), INTENT(INOUT) :: FFORMAT
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=I8) :: MY_POS
        INTEGER :: SRF_ID
        CHARACTER(LEN=9) :: SRF_OPENMODE
        INTEGER(KIND=I4) :: B1, B2, IB(10), IDUM
        INTEGER :: NTIMESTEP
        INTEGER(KIND=K4) :: TAG
        REAL :: TIME
        INTEGER(KIND=I8) :: I, POS, FSIZE
        CHARACTER(LEN=13) :: ENDIAN
        !
        ! ADD A NEW FILE TO THE HASH TABLE
        CALL ADD_SRF_FILE(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'OPEN_MESH_SRF:ADD_SRF_FILE')
        !
        ! BECAUSE of the stream mode in write only we need to have read access
        ! to position the file pointer so if the file is in write only
        ! we open it in readwrite
        IF(OPENMODE(1:5).EQ.'WRITE') THEN
          SRF_OPENMODE = 'READWRITE'
        ELSE
          SRF_OPENMODE = OPENMODE
        ENDIF
        ! Get the Endian for the file
        CALL IDENTIFY_ENDIAN_TYPE(FILE_NAME,OPENMODE,ENDIAN)
        ! OPEN THE FILE IN STREAM MODE
        OPEN(FILE=FILE_NAME, ACTION=SRF_OPENMODE, UNIT=FILE_ID,
     &       FORM='UNFORMATTED', ACCESS='STREAM',CONVERT=ENDIAN,
     &       IOSTAT=IERR)
        CALL CHECK_CALL(IERR,'OPEN_MESH_SRF:OPEN')
        !
        ! IF IN SERAFIN DOUBLE PRECISION REAL ARE 8 BITE
        ! 4 OTHERWISE
        IF(FFORMAT.EQ.'SERAFIN ') THEN
          SRF_OBJ_TAB(SRF_ID)%RS = 4
        ELSE
          SRF_OBJ_TAB(SRF_ID)%RS = 8
        ENDIF
        ! ONLY DO THE SCAN OF THE FILE IF THE FILE IS READ ONLY
        IF(OPENMODE.EQ.'READ     ') THEN
!
          ! IDENTIFY THE POSITION OF THE DIFFERENT MARKER OF THE FILE
          ! EACH "SET" IS DELIMITED BETWEEN TWO 4 BYTE INTEGER
          ! INDICATING THE SIZE OF THE SET
          ! SEE COMMENT FOR TYPE SRF_INFO FOR A DESCRIPTION OF EVERY VARIABLE
          SRF_OBJ_TAB(SRF_ID)%POS_TITLE = 1
          SRF_OBJ_TAB(SRF_ID)%POS_NVAR = SRF_OBJ_TAB(SRF_ID)%POS_TITLE
     &                            + 4 + TITLE_SIZE + 4
          SRF_OBJ_TAB(SRF_ID)%POS_VARINFO = SRF_OBJ_TAB(SRF_ID)%POS_NVAR
     &                                   + 4 + 2*IS + 4
!
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_NVAR+4
          READ(FILE_ID,POS=MY_POS,IOSTAT=IERR)
     &              B1,B2
          CALL CHECK_CALL(IERR,'OPEN_MESH_SRF:READ')
          SRF_OBJ_TAB(SRF_ID)%NVAR = B1 + B2
          ALLOCATE(SRF_OBJ_TAB(SRF_ID)%VAR_LIST(B1+B2),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'OPEN_MESH_SRF:VAR_LIST')
          DO I=1,SRF_OBJ_TAB(SRF_ID)%NVAR
            POS = SRF_OBJ_TAB(SRF_ID)%POS_VARINFO + (I-1)*(4+VAR_SIZE+4)
            READ(FILE_ID,POS=POS+4,IOSTAT=IERR)
     &                      SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I)
            CALL CHECK_CALL(IERR,'OPEN_MESH_SRF:READ')
          ENDDO
!
          SRF_OBJ_TAB(SRF_ID)%POS_IB = SRF_OBJ_TAB(SRF_ID)%POS_VARINFO
     &                     + SRF_OBJ_TAB(SRF_ID)%NVAR*(4 + VAR_SIZE + 4)
!
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IB+4
          READ(FILE_ID, POS=MY_POS, IOSTAT=IERR) IB(1:10)
          CALL CHECK_CALL(IERR,'OPEN_MESH_SRF:READ')
          SRF_OBJ_TAB(SRF_ID)%NPLAN = IB(7)
          SRF_OBJ_TAB(SRF_ID)%NPTFR = IB(8)
          SRF_OBJ_TAB(SRF_ID)%NPTIR = IB(9)
          IF(IB(7).GT.0) THEN
            SRF_OBJ_TAB(SRF_ID)%NDIM = 3
          ELSE
            SRF_OBJ_TAB(SRF_ID)%NDIM = 2
          ENDIF
!
          SRF_OBJ_TAB(SRF_ID)%POS_NUM = SRF_OBJ_TAB(SRF_ID)%POS_IB
     &                               + 4 + 10*IS + 4
!
          ! IF IB(10).NE.0  THEN WE HAVE A DATE OF 6 INTEGER AFTER IB
          IF(IB(10).NE.0) THEN

            SRF_OBJ_TAB(SRF_ID)%POS_DATE = SRF_OBJ_TAB(SRF_ID)%POS_NUM
            SRF_OBJ_TAB(SRF_ID)%POS_NUM = SRF_OBJ_TAB(SRF_ID)%POS_NUM
     &                                 + 4 + 6*IS + 4
          ELSE
            SRF_OBJ_TAB(SRF_ID)%POS_DATE = 0
          ENDIF
!
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_NUM+4
          READ(FILE_ID,POS=MY_POS,IOSTAT=IERR) IB(1:4)
          CALL CHECK_CALL(IERR,'OPEN_MESH_SRF:READ')

          SRF_OBJ_TAB(SRF_ID)%NELEM = IB(1)
          SRF_OBJ_TAB(SRF_ID)%NPOIN = IB(2)
          SRF_OBJ_TAB(SRF_ID)%NDP = IB(3)
!         Identify the type of element:
          CALL IDENTIFY_TYP_ELT(SRF_OBJ_TAB(SRF_ID)%NDP,
     &                           SRF_OBJ_TAB(SRF_ID)%NDIM,
     &                           SRF_OBJ_TAB(SRF_ID)%TYP_ELT)
          SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT = POINT_BND_ELT_TYPE
!
!
          SRF_OBJ_TAB(SRF_ID)%POS_IKLE = SRF_OBJ_TAB(SRF_ID)%POS_NUM
     &                                + (4 + 4*IS + 4)
          SRF_OBJ_TAB(SRF_ID)%POS_IPOBO = SRF_OBJ_TAB(SRF_ID)%POS_IKLE
     &        + (4 +
     &             SRF_OBJ_TAB(SRF_ID)%NELEM*SRF_OBJ_TAB(SRF_ID)%NDP*IS
     &             + 4)
          SRF_OBJ_TAB(SRF_ID)%POS_COORD = SRF_OBJ_TAB(SRF_ID)%POS_IPOBO
     &        + (4 + SRF_OBJ_TAB(SRF_ID)%NPOIN*IS + 4)
          !
          !Check if we are indeed in single or double precision
          ! If we are in single precision the tag for the coordiantes should be:
          ! npoin*4 if we are in double it will be npoin*8
          !
          READ(FILE_ID,POS=SRF_OBJ_TAB(SRF_ID)%POS_COORD) TAG
          IF(FFORMAT.EQ.'SERAFIN ') THEN
            IF(TAG.EQ.SRF_OBJ_TAB(SRF_ID)%NPOIN*8) THEN
              WRITE(*,*) 'ERROR IN FORMAT OF FILE '//TRIM(FILE_NAME)//
     &                    'IT IS A SERAFIND FILE'
              SRF_OBJ_TAB(SRF_ID)%RS = 8
              FFORMAT = 'SERAFIND'
            ELSE IF (TAG.NE.SRF_OBJ_TAB(SRF_ID)%NPOIN*4) THEN
              IERR = HERMES_INVALID_SERAFIN_FILE
              RETURN
            ENDIF
          ELSE
            IF(TAG.EQ.SRF_OBJ_TAB(SRF_ID)%NPOIN*4) THEN
              WRITE(*,*) 'ERROR IN FORMAT OF FILE '//TRIM(FILE_NAME)//
     &                    'IT IS A SERAFIN FILE'
              SRF_OBJ_TAB(SRF_ID)%RS = 4
              FFORMAT = 'SERAFIN '
            ELSE IF (TAG.NE.SRF_OBJ_TAB(SRF_ID)%NPOIN*8) THEN
              IERR = HERMES_INVALID_SERAFIN_FILE
              RETURN
            ENDIF
          ENDIF
!
          ! EVEN IN 3D THE SERAFON ONLY CONTAINS THE X AND Y COORDINATES AS THE Y VARIES WITH TIME
          SRF_OBJ_TAB(SRF_ID)%POS_DATA = SRF_OBJ_TAB(SRF_ID)%POS_COORD
     &        + (4 +
     &             SRF_OBJ_TAB(SRF_ID)%NPOIN*SRF_OBJ_TAB(SRF_ID)%RS
     &             + 4)*2
          SRF_OBJ_TAB(SRF_ID)%SIZE_DATA =
     &         4 + SRF_OBJ_TAB(SRF_ID)%NPOIN*SRF_OBJ_TAB(SRF_ID)%RS + 4
          SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET =
     &         4 + SRF_OBJ_TAB(SRF_ID)%RS + 4
     &         + SRF_OBJ_TAB(SRF_ID)%NVAR*SRF_OBJ_TAB(SRF_ID)%SIZE_DATA
!
          ! IF NPTFR IS NOT IN IB
          ! WE COMPUTE IT BY COUNTING THE NUMBER OF POINT FOR WHICH IPOBO == 1
          IF((SRF_OBJ_TAB(SRF_ID)%NPTFR.EQ.0)
     &       .AND. (SRF_OBJ_TAB(SRF_ID)%NPTIR.EQ.0)) THEN
            ! POSITION OF THE IPOBO ARRAY IN THE FILE
            MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IPOBO + 4
            ! Positionning the file pointer
            READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) IDUM
            ! LOOP ON ALL THE POINTS
            DO I=1,SRF_OBJ_TAB(SRF_ID)%NPOIN
              READ(FILE_ID,IOSTAT=IERR) IDUM
              CALL CHECK_CALL(IERR,'OPEN_MESH_SRF:READ')
              IF(IDUM.NE.0) SRF_OBJ_TAB(SRF_ID)%NPTFR =
     &                          SRF_OBJ_TAB(SRF_ID)%NPTFR + 1
            ENDDO
          ENDIF

          ! COUTING THE NUMBER OF TIME STEPS
          NTIMESTEP = 0
          IERR = 0
#ifdef NO_INQUIRE_SIZE
          ! Need this solution for intel 10 in which inquire size is not
          ! implemented
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATA
          DO
            READ(FILE_ID,POS=MY_POS+4,IOSTAT=IERR) TIME
            IF(IERR.LT.0) EXIT
            NTIMESTEP = NTIMESTEP + 1
            MY_POS = MY_POS + SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET
          ENDDO
          ! THE LAST READ IS GOING TO CRASH ON PURPOSE BECAUSE
          ! WE'VE REACHED THE END OF THE FILE
          IERR = 0
#else
          ! Get the size of the file
          INQUIRE(UNIT=FILE_ID,SIZE=FSIZE)
          ! Ntimestep = (size_of_fil - size_of_geom)/size_of_a_timestep
          NTIMESTEP = (FSIZE - (SRF_OBJ_TAB(SRF_ID)%POS_DATA - 1) )
     &             /SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET
#endif
          SRF_OBJ_TAB(SRF_ID)%NTIMESTEP = NTIMESTEP
        ELSE
          ! NOT READ ONLY
          ! INTIALIZE SRF_OBJ_TAB(SRF_ID) TO ZEROS
          ! POSITION IN FILE
          SRF_OBJ_TAB(SRF_ID)%POS_TITLE = 1
          SRF_OBJ_TAB(SRF_ID)%POS_NVAR = 0
          SRF_OBJ_TAB(SRF_ID)%POS_VARINFO = 0
          SRF_OBJ_TAB(SRF_ID)%POS_IB = 0
          SRF_OBJ_TAB(SRF_ID)%POS_DATE = 0
          SRF_OBJ_TAB(SRF_ID)%POS_NUM = 0
          SRF_OBJ_TAB(SRF_ID)%POS_IKLE = 0
          SRF_OBJ_TAB(SRF_ID)%POS_IPOBO = 0
          SRF_OBJ_TAB(SRF_ID)%POS_COORD = 0
          SRF_OBJ_TAB(SRF_ID)%POS_DATA =0
          ! COMPUTED INFORMATIONS
          SRF_OBJ_TAB(SRF_ID)%SIZE_DATA = 0
          SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET = 0
          ! STOCKED QUANTITIES AND SMALL VARIABLES
          SRF_OBJ_TAB(SRF_ID)%NTIMESTEP = 0
          SRF_OBJ_TAB(SRF_ID)%NPOIN = 0
          SRF_OBJ_TAB(SRF_ID)%NVAR = 0
          SRF_OBJ_TAB(SRF_ID)%NELEM = 0
          SRF_OBJ_TAB(SRF_ID)%NDP = 0
          SRF_OBJ_TAB(SRF_ID)%NPLAN = 0
          SRF_OBJ_TAB(SRF_ID)%NPTFR = 0
          SRF_OBJ_TAB(SRF_ID)%NPTIR = 0
          SRF_OBJ_TAB(SRF_ID)%NDIM = 0
          SRF_OBJ_TAB(SRF_ID)%TYP_ELT = 0
          SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT = 0
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE OPEN_BND_SRF
!***********************************************************************
!
     &(FILE_NAME,FILE_ID,OPENMODE,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    OPENS A BOUNDARY FILE
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |-->| NAME OF THE BOUNDARY FILE
!| FILE_ID        |-->| FILE DESCRIPTOR OF THE "MESH" FILE
!| OPENMODE       |-->| ONE OF THE FOLLOWING VALUE 'READ','WRITE','READWRITE'
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        CHARACTER(LEN=*), INTENT(IN) :: FILE_NAME
        INTEGER,          INTENT(IN) :: FILE_ID
        CHARACTER(LEN=9), INTENT(IN) :: OPENMODE
        INTEGER, INTENT(OUT)         :: IERR
        !
        INTEGER SRF_ID,I
        INTEGER :: NPTFR
        LOGICAL :: ISOPENED, IS_USED
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'OPEN_BND_SRF:GET_SRF_OBJ')
        !
        ! Open the boundary file with a set id available
        SRF_OBJ_TAB(SRF_ID)%NCLI = 1000 + FILE_ID
        DO
          INQUIRE(UNIT=SRF_OBJ_TAB(SRF_ID)%NCLI,OPENED=IS_USED)
          IF(.NOT.IS_USED) EXIT
          SRF_OBJ_TAB(SRF_ID)%NCLI = SRF_OBJ_TAB(SRF_ID)%NCLI + 1
        ENDDO
        ! First we check if the file is already opened
        ! Telemac is using one boundary file for all the mesh file
        ! so it could have been opened by another mesh before hand
        INQUIRE(FILE=FILE_NAME,OPENED=ISOPENED)
        IF(ISOPENED) THEN
          ! Id the file is already opened get its id
          INQUIRE(FILE=FILE_NAME,NUMBER=SRF_OBJ_TAB(SRF_ID)%NCLI)
        ELSE
          ! Otherwise open the file
          OPEN(UNIT=SRF_OBJ_TAB(SRF_ID)%NCLI,FILE=FILE_NAME,
     &         FORM='FORMATTED',ACTION=OPENMODE,IOSTAT=IERR)
        ENDIF
        ! If we are not in write only
        ! We compute the number of boundary point i.e. number of line in the file
        IF(OPENMODE(1:5).NE.'WRITE') THEN
          REWIND(SRF_OBJ_TAB(SRF_ID)%NCLI)
          CALL CHECK_CALL(IERR,'OPEN_BND_SRF:OPEN')
          NPTFR = 0
          DO
            READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*,IOSTAT=IERR)
            IF (IERR.LT.0) THEN
              ! END OF FILE REACHED
              EXIT
            ELSE IF (IERR.GT.0) THEN
              ! Error during read
              CALL CHECK_CALL(IERR,'OPEN_BND_SRF:READ')
            ENDIF
            NPTFR = NPTFR + 1
          ENDDO
          IERR = 0
          SRF_OBJ_TAB(SRF_ID)%NPTFR = NPTFR
        ENDIF
        RETURN
        !
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE CLOSE_BND_SRF
!***********************************************************************
!
     &(FILE_ID,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!BRIEF    CLOSES A BOUNDARY FILE
!
!HISTORY  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER,          INTENT(IN) :: FILE_ID
        INTEGER, INTENT(OUT)         :: IERR
        !
        INTEGER SRF_ID
        LOGICAL ISOPENED
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'CLOSE_BND_SRF:GET_SRF_OBJ')
        !
        ISOPENED = .FALSE.
        ! Check if the file is still opened as it could have been closed
        ! by another mesh file (see open_bnd_srf for more information)
        INQUIRE(UNIT=SRF_OBJ_TAB(SRF_ID)%NCLI,OPENED=ISOPENED)
        IERR = 0
        IF(ISOPENED) THEN
          CLOSE(SRF_OBJ_TAB(SRF_ID)%NCLI,IOSTAT=IERR)
        ENDIF
        CALL CHECK_CALL(IERR,'CLOSE_BND_SRF:CLOSE')
        !
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE CLOSE_MESH_SRF
!***********************************************************************
!
     &(FILE_ID,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!BRIEF    CLOSES A MESH FILE
!
!HISTORY  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER SRF_ID,I
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'CLOSE_MESH:GET_SRF_OBJ')
        !
        ! Clearing id in the hash table
        HASH(SRF_ID) = 0
        NFILES = NFILES - 1
        ! Closing the file
        CLOSE(FILE_ID,IOSTAT=IERR)
        ! RESET SRF_OBJ_TAB(SRF_ID) TO ZEROS
        ! POSITION IN FILE
        SRF_OBJ_TAB(SRF_ID)%POS_TITLE = 1
        SRF_OBJ_TAB(SRF_ID)%POS_NVAR = 0
        SRF_OBJ_TAB(SRF_ID)%POS_VARINFO = 0
        SRF_OBJ_TAB(SRF_ID)%POS_IB = 0
        SRF_OBJ_TAB(SRF_ID)%POS_DATE = 0
        SRF_OBJ_TAB(SRF_ID)%POS_NUM = 0
        SRF_OBJ_TAB(SRF_ID)%POS_IKLE = 0
        SRF_OBJ_TAB(SRF_ID)%POS_IPOBO = 0
        SRF_OBJ_TAB(SRF_ID)%POS_COORD = 0
        SRF_OBJ_TAB(SRF_ID)%POS_DATA =0
        ! COMPUTED INFORMATIONS
        SRF_OBJ_TAB(SRF_ID)%SIZE_DATA = 0
        SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET = 0
        ! STOCKED QUANTITIES AND SMALL VARIABLES
        SRF_OBJ_TAB(SRF_ID)%NTIMESTEP = 0
        SRF_OBJ_TAB(SRF_ID)%NPOIN = 0
        SRF_OBJ_TAB(SRF_ID)%NVAR = 0
        SRF_OBJ_TAB(SRF_ID)%NELEM = 0
        SRF_OBJ_TAB(SRF_ID)%NDP = 0
        SRF_OBJ_TAB(SRF_ID)%NPLAN = 0
        SRF_OBJ_TAB(SRF_ID)%NPTFR = 0
        SRF_OBJ_TAB(SRF_ID)%NPTIR = 0
        SRF_OBJ_TAB(SRF_ID)%NDIM = 0
        SRF_OBJ_TAB(SRF_ID)%TYP_ELT = 0
        SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT = 0
        IF(ALLOCATED(SRF_OBJ_TAB(SRF_ID)%VAR_LIST))
     &    DEALLOCATE(SRF_OBJ_TAB(SRF_ID)%VAR_LIST)
        !
        RETURN
      END SUBROUTINE
!
!     Mesh functions
!
!***********************************************************************
      SUBROUTINE GET_MESH_TITLE_SRF
!***********************************************************************
!
     &(FILE_ID,TITLE,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the title from a mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TITLE          |<->| TITLE OF THE MESH FILE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        CHARACTER(LEN=TITLE_SIZE), INTENT(INOUT) :: TITLE
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=I8) :: MY_POS
        INTEGER          :: SRF_ID
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'CLOSE_MESH:GET_SRF_OBJ')
        !
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_TITLE + 4
        READ(FILE_ID,POS=MY_POS,IOSTAT=IERR)
     &           TITLE(1:TITLE_SIZE)
        CALL CHECK_CALL(IERR,'GET_MESH_TITLE_SRF:READ')
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_DATE_SRF
!***********************************************************************
!
     &(FILE_ID,DATE,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the date of the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| DATE           |<->| THE DATE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(INOUT) :: DATE(6)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=I8) :: MY_POS
        INTEGER          :: SRF_ID
        INTEGER :: I
        INTEGER(KIND=I4) :: TMP
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_DATE_SRF:GET_SRF_OBJ')
        !
        ! Default value
        DATE(1:6) = (/ 0,0,0,0,0,0 /)
        ! POS_DATE=0 <=> NO DATE
        IF(SRF_OBJ_TAB(SRF_ID)%POS_DATE.NE.0) THEN
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATE + 4
          DO I=1,6
            READ(FILE_ID,POS=MY_POS+(I-1)*IS,IOSTAT=IERR) TMP
            CALL CHECK_CALL(IERR,'GET_MESH_DATE_SRF:READ')
            DATE(I) = TMP
          ENDDO
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NELEM_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_ELT,NELEM,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of elements of type typ_elem in the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_ELEM       |-->| TYPE OF THE ELEMENT
!| NELEM          |<->| THE NUMBER OF ELEMENTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_ELT
        INTEGER, INTENT(INOUT) :: NELEM
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_NELEM_SRF:GET_SRF_OBJ')
        !
        IF(TYP_ELT.EQ.SRF_OBJ_TAB(SRF_ID)%TYP_ELT) THEN
          NELEM = SRF_OBJ_TAB(SRF_ID)%NELEM
        ELSE
          NELEM = 0
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPOIN_PER_ELEMENT_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_ELT,NDP,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of point per element of type typ_elem
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_ELEM       |-->| TYPE OF THE ELEMENT
!| NDP            |<->| THE NUMBER OF POINT PER ELEMENT
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_ELT
        INTEGER, INTENT(INOUT) :: NDP
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,
     &                  'GET_MESH_NPOIN_PER_ELEMENT_SRF:GET_SRF_OBJ')
        !
        NDP = 0
        IF(TYP_ELT.EQ.SRF_OBJ_TAB(SRF_ID)%TYP_ELT) THEN
          NDP = SRF_OBJ_TAB(SRF_ID)%NDP
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_CONNECTIVITY_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_ELT,IKLE,NELEM,NDP,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the connectivity table for
!+        the element of type typ_elem in the mesh
!+        will do nothing if there are no element of typ_elem in the mesh
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_ELEM       |-->| TYPE OF THE ELEMENT
!| IKLE           |<->| THE CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_ELT
        INTEGER, INTENT(IN) :: NELEM
        INTEGER, INTENT(IN) :: NDP
        INTEGER, INTENT(INOUT) :: IKLE(NELEM*NDP)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID, ARRAY_SIZE
        INTEGER(KIND=I8) :: MY_POS
        INTEGER          :: I
        INTEGER(KIND=I4) :: TMP
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_CONNECTIVITY_SRF:GET_SRF_OBJ')
        !
        ARRAY_SIZE = SRF_OBJ_TAB(SRF_ID)%NELEM * SRF_OBJ_TAB(SRF_ID)%NDP
        !
        IF(TYP_ELT.EQ.SRF_OBJ_TAB(SRF_ID)%TYP_ELT) THEN
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IKLE + 4
          ! Positioning the file pointer
          READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TMP
          ! WE NEED TO CERTIFY THAT THE INTEGER READ IS ON 4 BYTES
          DO I=1,ARRAY_SIZE
            READ(FILE_ID,IOSTAT=IERR) TMP
            CALL CHECK_CALL(IERR,'GET_MESH_CONNECTIVITY_SRF:READ')
            IKLE(I) = TMP
          ENDDO
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPOIN_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_ELT,NPOIN,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of point for the given element type in the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_ELEM       |-->| TYPE OF THE ELEMENT
!| NPOIN          |<->| THE NUMBER OF POINTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_ELT
        INTEGER, INTENT(INOUT) :: NPOIN
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_NPOIN_SRF:GET_SRF_OBJ')
        !
        NPOIN = SRF_OBJ_TAB(SRF_ID)%NPOIN
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPLAN_SRF
!***********************************************************************
!
     &(FILE_ID,NPLAN,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of layers
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NPLAN          |<->| THE NUMBER OF LAYERS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(INOUT) :: NPLAN
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_NPLAN_SRF:GET_SRF_OBJ')
        !
        NPLAN = SRF_OBJ_TAB(SRF_ID)%NPLAN
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_DIMENSION_SRF
!***********************************************************************
!
     &(FILE_ID,NDIM,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of dimensions of the space
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NDIM           |<->| NUMBER OF DIMENSION
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(INOUT) :: NDIM
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_DIMENSION_SRF:GET_SRF_OBJ')
        !
        NDIM = SRF_OBJ_TAB(SRF_ID)%NDIM
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_COORD_SRF
!***********************************************************************
!
     &(FILE_ID,JDIM,NPOIN,COORD,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the coordinates for the given dimension
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| JDIM           |-->| DIMENSION NUMBER
!| NPOIN          |-->| TOTAL NUMBER OF NODES
!| COORD          |<->| LOCAL TO GLOBAL NUMBERING ARRAY
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: JDIM
        INTEGER, INTENT(IN) :: NPOIN
        DOUBLE PRECISION, INTENT(INOUT) :: COORD(NPOIN)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=I8) :: MY_POS
        INTEGER          :: SRF_ID
        INTEGER :: I, ARRAY_SIZE
        INTEGER :: DBL_TYP
        REAL(KIND=R4) :: W
        REAL(KIND=R8) :: D
        INTEGER(KIND=I4) :: TAG
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_COORD_SRF:GET_SRF_OBJ')
        !
        ARRAY_SIZE = SRF_OBJ_TAB(SRF_ID)%NPOIN
        DBL_TYP = SRF_OBJ_TAB(SRF_ID)%RS
        !
        ! Move to the position of the coordinates in the mesh file
        IF ((JDIM.GE.0).AND.(JDIM.LE.2)) THEN
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_COORD
     &             + INT((JDIM-1)*(4 + ARRAY_SIZE*DBL_TYP + 4),I8)
     &             + 4
        ELSE
!         ERROR ON JDIM
          IERR = HERMES_WRONG_AXE_ERR
          RETURN
        ENDIF
        ! Positioning the file pointer
        READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TAG
        ! Loop on all value depending on real precision (single or double)
        DO I=1,ARRAY_SIZE
          IF(DBL_TYP.EQ.4) THEN
            READ(FILE_ID,IOSTAT=IERR) W
            COORD(I) = DBLE(W)
          ELSE
            READ(FILE_ID,IOSTAT=IERR) D
            COORD(I) = D
          ENDIF
          CALL CHECK_CALL(IERR,'GET_MESH_COORD_SRF:READ')
        ENDDO
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_L2G_NUMBERING_SRF
!***********************************************************************
!
     &(FILE_ID,KNOLG,NPOIN,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the local to global numbering array
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| KNOLG          |<->| LOCAL TO GLOBAL NUMBERING ARRAY
!| NPOIN          |-->| NUMBER OF NODES
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: NPOIN
        INTEGER, INTENT(INOUT) :: KNOLG(NPOIN)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=I8) :: MY_POS
        INTEGER          :: SRF_ID
        INTEGER :: ARRAY_SIZE,I
        INTEGER(KIND=I4) :: TMP
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_L2G_NUMBERING_SRF:GET_SRF_OBJ')
        !
        ARRAY_SIZE = SRF_OBJ_TAB(SRF_ID)%NPOIN
        !
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IPOBO + 4
        ! Positionning the file pointer
        READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TMP
        DO I=1,ARRAY_SIZE
          ! Reading 4 byte integer
          READ(FILE_ID,IOSTAT=IERR) TMP
          CALL CHECK_CALL(IERR,'GET_MESH_L2G_NUMBERING_SRF:READ')
          KNOLG(I) = TMP
        ENDDO
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPTIR_SRF
!***********************************************************************
!
     &(FILE_ID,NPTIR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of interface point
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NPTIR          |<->| NUMBER OF INTERFACE POINT
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(INOUT) :: NPTIR
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        !TODO: Remove that function ??
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_NPTIR_SRF:GET_SRF_OBJ')
        !
        NPTIR = SRF_OBJ_TAB(SRF_ID)%NPTIR
        !
        RETURN
      END SUBROUTINE
!
!     Boundary functions
!
!***********************************************************************
      SUBROUTINE GET_BND_IPOBO_SRF
!***********************************************************************
!
     &(FILE_ID,TYPE_BND_ELEM,NPOIN,IPOBO,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns an array containing
!+        1 if a point is a boundary point 0 otherwise
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENT
!| NPOIN          |-->| TOTAL NUMBER OF NODES
!| IPOBO          |<->| AN ARRAY CONTAINING
!|                |   | 1 IF A POINT IS A BOUNDARY POINT 0 OTHERWISE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: NPOIN
        INTEGER, INTENT(IN) :: TYPE_BND_ELEM
        INTEGER, INTENT(INOUT) :: IPOBO(NPOIN)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=I8) :: MY_POS
        INTEGER          :: SRF_ID
        INTEGER :: ARRAY_SIZE, I
        INTEGER(KIND=I4) TMP
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_IPOBO_SRF:GET_SRF_OBJ')
        !
        ARRAY_SIZE = SRF_OBJ_TAB(SRF_ID)%NPOIN
        !
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IPOBO + 4
        ! Positionning the file pointer
        READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TMP
        DO I=1,ARRAY_SIZE
          READ(FILE_ID,IOSTAT=IERR) TMP
          CALL CHECK_CALL(IERR,'GET_MESH_IPOBO_SRF:READ')
          IPOBO(I) = TMP
        ENDDO
        !
        RETURN
      END SUBROUTINE
!
!***********************************************************************
      SUBROUTINE GET_BND_NUMBERING_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_ELEM_BND,NPTFR,NBOR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns an array containing
!+        The association of boundary numbering to mesh numbering
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENT
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NBOR           |<->| AN ARRAY CONTAINING THE NUMBERING IN THE MESH
!|                |   | OF ALL BOUNDARY POINTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
        INTEGER, INTENT(IN) :: FILE_ID,NPTFR,TYP_ELEM_BND
        INTEGER, INTENT(INOUT) :: NBOR(NPTFR)
        INTEGER, INTENT(OUT) :: IERR
        !
        CALL GET_BND_CONNECTIVITY_SRF(FILE_ID,TYP_ELEM_BND,NPTFR,
     &                                1,NBOR,IERR)
        CALL CHECK_CALL(IERR,
     &             'GET_BND_NUMBERING_SRF:GET_BND_CONNECTIVITY_SRF')
        !
        RETURN
      END SUBROUTINE
      SUBROUTINE GET_BND_NELEM_SRF(FILE_ID,TYPE_BND_ELEM, NELEM,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Reads the number of boundary elements
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYPE_BND_ELEM  |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NELEM          |<->| NUMBER OF BOUNDARY ELEMENTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYPE_BND_ELEM
        INTEGER, INTENT(INOUT) :: NELEM
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_NELEM_SRF:GET_SRF_OBJ')
        !
        ! Checking that we have the right element type
        IF(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT.EQ.TYPE_BND_ELEM) THEN
          NELEM = SRF_OBJ_TAB(SRF_ID)%NPTFR
        ELSE
          NELEM = 0
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_BND_CONNECTIVITY_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_BND_ELT,NELEBD,NDP,IKLE,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Reads the connectivity of the boundary elements
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NELEBD         |-->| NUMBER OF BOUNDARY ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| IKLE           |<->| THE CONNECTIVITY OF THE BOUNDARY ELEMENTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_BND_ELT
        INTEGER, INTENT(IN) :: NELEBD
        INTEGER, INTENT(IN) :: NDP
        INTEGER, INTENT(INOUT) :: IKLE(NELEBD*NDP)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID, I
        DOUBLE PRECISION :: DDUM
        INTEGER :: IDUM
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_CONNECTIVITY_SRF:GET_SRF_OBJ')
        !
        ! Checking that we have the right element type
        IF(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT.NE.TYP_BND_ELT) THEN
          IERR = HERMES_WRONG_ELEMENT_TYPE_ERR
          RETURN
        ENDIF
        ! Read the boundary file we only care about the connectivity
        REWIND(SRF_OBJ_TAB(SRF_ID)%NCLI)
        DO I=1,SRF_OBJ_TAB(SRF_ID)%NPTFR
          READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*,IOSTAT=IERR) IDUM,IDUM,IDUM,
     &                         DDUM  ,DDUM  ,DDUM,
     &                         DDUM ,IDUM,DDUM,DDUM,DDUM,
     &                         IKLE(I),IDUM
          IF(IERR.LT.0) THEN
            ! End of file reached
            CALL CHECK_CALL(IERR,
     &               'GET_BND_CONNECTIVITY_SRF:READ:END OF FILE')
          ELSE IF (IERR.GT.0) THEN
            ! Error during read
            CALL CHECK_CALL(IERR,'GET_BND_CONNECTIVITY_SRF:READ')
          ENDIF
        ENDDO
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_BND_VALUE_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_BND_ELEM,NPTFR,LIHBOR,LIUBOR,
     & LIVBOR,HBOR,UBOR,VBOR,CHBORD,TRAC,
     & LITBOR,TBOR,ATBOR,BTBOR, IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns an array containing the boundary type for each
!+        boundary point
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| TRAC           |-->| IF YES, THERE ARE TRACERS
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| ATBOR,BTBOR    |<--| THERMAL EXCHANGE COEFFICIENTS.
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_BND_ELEM
        INTEGER, INTENT(IN) :: NPTFR
        INTEGER, INTENT(INOUT) :: LIUBOR(NPTFR),LIVBOR(NPTFR)
        INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR),LITBOR(*)
        DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
        DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR),CHBORD(NPTFR)
        DOUBLE PRECISION, INTENT(INOUT) :: TBOR(NPTFR),ATBOR(NPTFR)
        DOUBLE PRECISION, INTENT(INOUT) :: BTBOR(NPTFR)
        LOGICAL, INTENT(IN)    :: TRAC
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID, I
        INTEGER :: IDUM
        DOUBLE PRECISION :: DDUM
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_VALUE_SRF:GET_SRF_OBJ')
        !
        ! Checking that we have the right element type
        IF(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT.NE.TYP_BND_ELEM) THEN
          IERR = HERMES_WRONG_ELEMENT_TYPE_ERR
          RETURN
        ENDIF
        ! Reading the boundary file informations we only care
        ! about the boundary type li[huvt]bor
        REWIND(SRF_OBJ_TAB(SRF_ID)%NCLI)
        DO I=1,SRF_OBJ_TAB(SRF_ID)%NPTFR
          IF(TRAC) THEN
            READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*,IOSTAT=IERR)
     &             LIHBOR(I),LIUBOR(I),LIVBOR(I),
     &             HBOR(I)  ,UBOR(I)  ,VBOR(I),
     &             CHBORD(I) ,LITBOR(I),
     &             TBOR(I),ATBOR(I),BTBOR(I),
     &             IDUM,IDUM
          ELSE
            READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*,IOSTAT=IERR)
     &             LIHBOR(I),LIUBOR(I),LIVBOR(I),
     &             HBOR(I)  ,UBOR(I)  ,VBOR(I),
     &             CHBORD(I) ,IDUM,DDUM,DDUM,DDUM,
     &             IDUM,IDUM
          ENDIF
          IF(IERR.LT.0) THEN
            ! End of file reached
            CALL CHECK_CALL(IERR,'GET_BND_VALUE_SRF:READ:END OF FILE')
          ELSE IF (IERR.GT.0) THEN
            ! Error during read
            CALL CHECK_CALL(IERR,'GET_BND_VALUE_SRF:READ')
          ENDIF
        ENDDO
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_BND_NPOIN_SRF
!***********************************************************************
!
     &(FILE_ID,TYPE_BND_ELEM,NPTFR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of boundary points
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYPE_BND_ELEM  |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NPTFR          |<->| NUMBER OF BOUNDARY POINTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYPE_BND_ELEM
        INTEGER, INTENT(INOUT) :: NPTFR
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_BND_NPOIN_SRF:GET_SRF_OBJ')
        !
        IF(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT.EQ.TYPE_BND_ELEM) THEN
          NPTFR = SRF_OBJ_TAB(SRF_ID)%NPTFR
        ELSE
          NPTFR = 0
        ENDIF
        !
        !
        RETURN
      END SUBROUTINE
!
!     Data functions
!
!***********************************************************************
      SUBROUTINE GET_DATA_NVAR_SRF
!***********************************************************************
!
     &(FILE_ID,NVAR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of varaibles in the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NVAR           |<->| NUMBER OF VARIABLE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(INOUT) :: NVAR
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_DATA_NVAR_SRF:GET_SRF_OBJ')
        !
        NVAR = SRF_OBJ_TAB(SRF_ID)%NVAR
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_VAR_LIST_SRF
!***********************************************************************
!
     &(FILE_ID,NVAR,VAR_LIST,UNIT_LIST,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns a list of all the name of the variables in the mesh file
!+        and a list of their units
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| VARLIST        |<->| LIST OF VARIABLE NAME
!| UNTILIST       |<->| LIST OF VARIABLE UNIT
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: NVAR
        CHARACTER(LEN=16), INTENT(INOUT) :: VAR_LIST(NVAR)
        CHARACTER(LEN=16), INTENT(INOUT) :: UNIT_LIST(NVAR)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        INTEGER :: I
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_DATA_VAR_LIST_SRF:GET_SRF_OBJ')
        !
        ! Test if the number of variable given as argument is the same
        ! as the one in the file
        IF(NVAR.NE.SRF_OBJ_TAB(SRF_ID)%NVAR) THEN
          IERR = HERMES_WRONG_ARRAY_SIZE_ERR
          RETURN
        ENDIF
        DO I=1,SRF_OBJ_TAB(SRF_ID)%NVAR
          VAR_LIST(I) = SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I)(1:16)
          UNIT_LIST(I) = SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I)(17:32)
        ENDDO
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_NTIMESTEP_SRF
!***********************************************************************
!
     &(FILE_ID,NTIMESTEP,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of time step in the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NTIMESTEP      |<->| THE NUMBER OF TIME STEPS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(INOUT) :: NTIMESTEP
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_DATA_NTIMSTEP_SRF:GET_SRF_OBJ')
        !
        NTIMESTEP = SRF_OBJ_TAB(SRF_ID)%NTIMESTEP
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_TIME_SRF
!***********************************************************************
!
     &(FILE_ID,RECORD,TIME,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the time value of a given time step
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!history  R ATA (EDF R&D, LNHE)
!+        24/05/2016
!+        V7P2
!+   The case with no record in the file was not treated.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| RECORD         |-->| NUMBER OF THE TIME STEP
!| TIME           |<->| TIME IN SECOND OF THE TIME STEP
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: RECORD
        DOUBLE PRECISION, INTENT(INOUT) :: TIME
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=I8) :: MY_POS
        INTEGER          :: SRF_ID
        REAL :: W
        INTEGER :: IREC,NTIMESTEP
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_DATA_TIME_SRF:GET_SRF_OBJ')
        !
        NTIMESTEP = SRF_OBJ_TAB(SRF_ID)%NTIMESTEP
        ! CHECK IF THE RECORD IS IN THE FILE
        IF((RECORD.GE.NTIMESTEP.OR.RECORD.LT.0).AND.
     &     NTIMESTEP.GT.0) THEN
          IERR = HERMES_RECORD_UNKNOWN_ERR
          RETURN
        ELSE
          IREC = RECORD
        ENDIF
        ! POSITION OF THE TIME TO READ
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATA + 4
     &           + INT(IREC,I8)*SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET
        ! DIFFERENCE BETWEEN REAL AND DOUBLE PRECISION
        IF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.4) THEN
          READ(FILE_ID,POS=MY_POS,IOSTAT=IERR) W
          TIME = DBLE(W)
        ELSE
          READ(FILE_ID,POS=MY_POS,IOSTAT=IERR) TIME
        ENDIF
        CALL CHECK_CALL(IERR,'GET_DATA_TIME_SRF:READ')
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_VALUE_SRF
!***********************************************************************
!
     &(FILE_ID,RECORD,VAR_NAME,RES_VALUE,N,IERR)
!
!***********************************************************************
! HERMES   V7P2                                               01/05/2014
!***********************************************************************
!
!brief    Returns The value for each point of a given variable
!+        for a given time step
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+   First version.
!
!history  R ATA (EDF R&D, LNHE)
!+        24/05/2016
!+        V7P2
!+   The case with no record in the file was not treated.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| RECORD         |-->| TIME STEP TO READ IN THE FILE
!| VAR_NAME       |-->| VARIABLE FOR WHICH WE NEED THE VALUE
!| RES_VALUE      |<->| VALUE FOR EACH POINT AT TIME STEP RECORD
!|                |   | FOR THE VARIABLE VAR_NAME
!| N              |-->| SIZE OF RES_VALUE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: RECORD, N
        CHARACTER(LEN=16), INTENT(IN) :: VAR_NAME
        DOUBLE PRECISION, INTENT(INOUT) :: RES_VALUE(N)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=I8) :: MY_POS
        INTEGER          :: SRF_ID, IVAR
        INTEGER :: I, ARRAY_SIZE, NTIMESTEP, IREC
        REAL :: W
        INTEGER(KIND=I4) :: TAG
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_DATA_VALUE_SRF:GET_SRF_OBJ')
        !
        NTIMESTEP = SRF_OBJ_TAB(SRF_ID)%NTIMESTEP
        ! CHECK IF THE RECORD IS IN THE FILE
        IF((RECORD.GE.NTIMESTEP.OR.RECORD.LT.0).AND.
     &     NTIMESTEP.GT.0) THEN
          IERR = HERMES_RECORD_UNKNOWN_ERR
          RETURN
        ELSE
          IREC = RECORD
        ENDIF
        !
        ! GET THE POSITION OF THE VARIABLE
        IVAR = 0
        DO I=1,SRF_OBJ_TAB(SRF_ID)%NVAR
          IF(SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I)(1:16).EQ.VAR_NAME) THEN
            IVAR = I
            EXIT
          ENDIF
        ENDDO
        ! NO VARIABLE WAS FOUND
        IF(IVAR.EQ.0) THEN
          IERR = HERMES_VAR_UNKNOWN_ERR
          RETURN
        ENDIF
        !
        ! Check the size of the array compare to the number of points
        IF (SRF_OBJ_TAB(SRF_ID)%NPOIN.LT.N) THEN
          IERR = HERMES_WRONG_SIZE
          RETURN
        ENDIF
        ARRAY_SIZE = SRF_OBJ_TAB(SRF_ID)%NPOIN
        !
        ! READ THE VARIABLES RESULT FOR THE GIVEN RECORD
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATA + 4
     &          + INT(IREC,I8)*(SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET)
     &          + 4 + SRF_OBJ_TAB(SRF_ID)%RS + 4 ! THE TIME VALUE
     &          + INT(IVAR-1,I8) * SRF_OBJ_TAB(SRF_ID)%SIZE_DATA
        ! Quick read to position the file pointer
        READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TAG
        DO I=1,N
          IF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.4) THEN
            READ(FILE_ID,IOSTAT=IERR) W
            RES_VALUE(I) = DBLE(W)
          ELSE
            READ(FILE_ID,IOSTAT=IERR) RES_VALUE(I)
          ENDIF
          CALL CHECK_CALL(IERR,'GET_DATA_VALUE_SRF:READ')
        ENDDO
        READ(FILE_ID,IOSTAT=IERR) TAG
        !
        RETURN
      END SUBROUTINE
!
!     Writing functions
!
!***********************************************************************
      SUBROUTINE SET_HEADER_SRF
!***********************************************************************
!
     &(FILE_ID,TITLE,NVAR,VAR_NAME,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Writes the Title and the name and units of the variables
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TITLE          |-->| TITLE OF THE MESH
!| NVAR           |-->| NUMBER OF VARIABLES
!| VAR_NAME       |-->| NAME AND UNITS OF THE VARIABLES
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        INTEGER,                          INTENT(IN)  :: FILE_ID
        CHARACTER(LEN=TITLE_SIZE),        INTENT(IN)  :: TITLE
        INTEGER,                          INTENT(IN)  :: NVAR
        CHARACTER(LEN=VAR_SIZE),          INTENT(IN)  :: VAR_NAME(NVAR)
        INTEGER,                          INTENT(OUT) :: IERR
        !
        INTEGER(KIND=I8) :: MY_POS
        INTEGER ::  TAG,I,SRF_ID
        INTEGER(KIND=I4) :: TMP, TMP2
        !
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'SET_HEADER_SRF:GET_SRF_OBJ')
        !
        ! WRITING THE TITLE RECORD
        TAG = TITLE_SIZE ! 80 CHARCATERS

        SRF_OBJ_TAB(SRF_ID)%POS_TITLE = 1
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_TITLE
        WRITE(FILE_ID,POS=MY_POS,IOSTAT=IERR) TAG,TITLE,TAG
          CALL CHECK_CALL(IERR,'SET_HEADER_SRF:WRITE')
        SRF_OBJ_TAB(SRF_ID)%POS_NVAR = SRF_OBJ_TAB(SRF_ID)%POS_TITLE
     &                          + 4 + TITLE_SIZE + 4
        !
        ! WRINTING THE NUMBER OF VARIABLE RECORD
        TAG = 2*IS ! TWO INTEGERS
        TMP = NVAR
        TMP2 = 0
        WRITE(FILE_ID,IOSTAT=IERR) TAG,TMP,TMP2,TAG
        CALL CHECK_CALL(IERR,'SET_HEADER_SRF:WRITE')
        SRF_OBJ_TAB(SRF_ID)%POS_VARINFO = SRF_OBJ_TAB(SRF_ID)%POS_NVAR
     &                               + 4 + 2*IS + 4
        !
        ! WRITING THE NAME AND UNITS FOR EACH VARIABLE
        ! I.E. NVAR RECORD OF VAR_SIZE CHARACTERS
        IF(.NOT.ALLOCATED(SRF_OBJ_TAB(SRF_ID)%VAR_LIST)) THEN
          ALLOCATE(SRF_OBJ_TAB(SRF_ID)%VAR_LIST(NVAR),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'SET_HEADER:SRF_OBJ_TAB')
        ENDIF
        TAG = VAR_SIZE
        DO I=1, NVAR
          WRITE(FILE_ID,IOSTAT=IERR) TAG,VAR_NAME(I),TAG
          CALL CHECK_CALL(IERR,'SET_HEADER_SRF:WRITE')
          SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I) = VAR_NAME(I)
        ENDDO
        ! NOW WE UPDATE THE POSITION INFORMATION IN SRF_OBJ_TAB
!
        SRF_OBJ_TAB(SRF_ID)%NVAR = NVAR
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE SET_MESH_SRF
!***********************************************************************
!
     &(FFORMAT,FILE_ID,MESH_DIM,TYPELT,NDP,NPTFR,NPTIR,NELEM,NPOIN,
     & IKLE,IPOBO,KNOLG,X,Y,NPLAN,DATE,TIME,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Writes the mesh geometry in the file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| MESH_DIM       |-->| DIMENSION OF THE MESH
!| TYPELM         |-->| TYPE OF THE MESH ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NPTFR          |-->| NUMBER OF BOUNDARY POINT
!| NPTIR          |-->| NUMBER OF INTERFACE POINT
!| NELEM          |-->| NUMBER OF ELEMENT IN THE MESH
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| IKLE           |-->| CONNECTIVITY ARRAY FOR THE MAIN ELEMENT
!| IPOBO          |-->| IS A BOUNDARY POINT ? ARRAY
!| KNOLG          |-->| LOCAL TO GLOBAL NUMBERING ARRAY
!| X              |-->| X COORDINATES OF THE MESH POINTS
!| Y              |-->| Y COORDINATES OF THE MESH POINTS
!| NPLAN          |-->| NUMBER OF PLANES
!| DATE           |-->| DATE OF THE CREATION OF THE MESH
!| TIME           |-->| TIME OF THE CREATION OF THE MESH
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| Z  (OPTIONAL)  |-->| Z COORDINATES OF THE MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        CHARACTER(LEN=8) ,     INTENT(IN) :: FFORMAT
        INTEGER          ,     INTENT(IN) :: FILE_ID,NPLAN
        INTEGER, DIMENSION(3), INTENT(IN) :: DATE
        INTEGER, DIMENSION(3), INTENT(IN) :: TIME
        INTEGER,               INTENT(IN) :: MESH_DIM
        INTEGER,               INTENT(IN) :: TYPELT
        INTEGER,               INTENT(IN) :: NDP
        INTEGER,               INTENT(IN) :: NPTFR
        INTEGER,               INTENT(IN) :: NPTIR
        INTEGER,               INTENT(IN) :: NELEM
        INTEGER,               INTENT(IN) :: NPOIN
        INTEGER,               INTENT(IN) :: IKLE(NDP*NELEM)
        INTEGER,               INTENT(IN) :: IPOBO(*)
        INTEGER,               INTENT(IN) :: KNOLG(*)
        DOUBLE PRECISION,      INTENT(IN) :: X(NPOIN),Y(NPOIN)
        INTEGER,               INTENT(OUT) :: IERR
        !
        INTEGER(KIND=I8) :: MY_POS
        INTEGER :: I,SRF_ID,IKLES_SIZE,IELEM,IPLAN
        INTEGER :: NELEM2
        INTEGER :: IELEMP,IELEMT,NPOIN2,IELEM2
        !
        INTEGER(KIND=I4) :: TMP(10), TAG
        INTEGER(KIND=I4), ALLOCATABLE :: IKLES(:)
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'GET_MESH_SRF:GET_SRF_OBJ')
        !
        ! DEFINES THE SIZE OF REAL IN THE FILE
        IF(FFORMAT.EQ.'SERAFIND') THEN
          SRF_OBJ_TAB(SRF_ID)%RS = 8
        ELSE
          SRF_OBJ_TAB(SRF_ID)%RS = 4
        ENDIF
        ! ASSOCIATE THE ELEMENT TYPE WITH THE NUMBER OF POINT PER ELEMENTS
        SRF_OBJ_TAB(SRF_ID)%TYP_ELT = (TYPELT/10)*10
        SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT = 0
        SELECT CASE (SRF_OBJ_TAB(SRF_ID)%TYP_ELT)
          CASE (TRIANGLE_ELT_TYPE) ! TRIANGLES
            SRF_OBJ_TAB(SRF_ID)%NDP = 3
          CASE (QUADRANGLE_ELT_TYPE) ! QUADRANGLES
            SRF_OBJ_TAB(SRF_ID)%NDP = 4
          CASE (PRISM_ELT_TYPE) ! PRISMS
            SRF_OBJ_TAB(SRF_ID)%NDP = 6
          CASE (SPLIT_PRISM_ELT_TYPE) ! PRISMS SPLIT IN TETRAS
            SRF_OBJ_TAB(SRF_ID)%NDP = 6
          CASE DEFAULT
            IERR = UNKNOWN_ELT_TYPE_ERR
            RETURN
        END SELECT
        !
        ! BUILDING THE 10 INTEGERS ARRAY
        !
        IF(NPLAN.LE.1) THEN
          SRF_OBJ_TAB(SRF_ID)%NPLAN = 0
        ELSE
          SRF_OBJ_TAB(SRF_ID)%NPLAN = NPLAN
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%NPTIR = NPTIR
        SRF_OBJ_TAB(SRF_ID)%NPTFR = NPTFR
        SRF_OBJ_TAB(SRF_ID)%NDIM = MESH_DIM
        TMP(1) = 1
        TMP(2) = 0
        TMP(3) = 0
        TMP(4) = 0
        TMP(5) = 0
        TMP(6) = 0
        TMP(7) = SRF_OBJ_TAB(SRF_ID)%NPLAN
        TMP(8) = NPTFR
        TMP(9) = NPTIR
        TMP(10) = 0
        IF((DATE(1)+DATE(2)+DATE(3)+TIME(1)+TIME(2)+TIME(3)).NE.0) THEN
          TMP(10) = 1
        ENDIF
        ! WRITING THE RECORD WITH THE TAG
        SRF_OBJ_TAB(SRF_ID)%POS_IB = SRF_OBJ_TAB(SRF_ID)%POS_VARINFO
     &                   + SRF_OBJ_TAB(SRF_ID)%NVAR*(4 + VAR_SIZE + 4)
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IB
        ! Set position for writing
        READ(FILE_ID,POS=MY_POS-4) TAG
        TAG = IS*10
        WRITE(FILE_ID,IOSTAT=IERR) TAG,TMP,TAG
        CALL CHECK_CALL(IERR,'SET_MESH_SRF:WRITE')

        ! WRITING THE DATE RECORD
        IF(TMP(10).NE.0) THEN
          SRF_OBJ_TAB(SRF_ID)%POS_DATE =
     &           SRF_OBJ_TAB(SRF_ID)%POS_IB + 4 + 10*IS + 4
          TAG = IS*6
          WRITE(FILE_ID,IOSTAT=IERR) TAG,DATE,TIME,TAG
          CALL CHECK_CALL(IERR,'SET_MESH_SRF:WRITE')
          SRF_OBJ_TAB(SRF_ID)%POS_NUM = SRF_OBJ_TAB(SRF_ID)%POS_DATE
     &                               + 4 + 6*IS + 4
        ELSE
          SRF_OBJ_TAB(SRF_ID)%POS_DATE = 0
          SRF_OBJ_TAB(SRF_ID)%POS_NUM = SRF_OBJ_TAB(SRF_ID)%POS_IB
     &                               + 4 + 10*IS + 4
        ENDIF
        !
        ! BUILDING THE 4 INTEGER RECORD
        !
        IF(TYPELT.EQ.50) THEN
          SRF_OBJ_TAB(SRF_ID)%NELEM = NELEM/3
        ELSE
          SRF_OBJ_TAB(SRF_ID)%NELEM = NELEM
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%NPOIN = NPOIN
        TMP(1) = SRF_OBJ_TAB(SRF_ID)%NELEM
        TMP(2) = SRF_OBJ_TAB(SRF_ID)%NPOIN
        TMP(3) = SRF_OBJ_TAB(SRF_ID)%NDP
        TMP(4) = 1
        TAG = 4*IS
        WRITE(FILE_ID,IOSTAT=IERR) TAG,TMP(1:4),TAG
        CALL CHECK_CALL(IERR,'SET_MESH_SRF:WRITE')
        SRF_OBJ_TAB(SRF_ID)%POS_IKLE = SRF_OBJ_TAB(SRF_ID)%POS_NUM
     &                              + (4 + 4*IS + 4)
        !
        ! Writing ikle
        !
        ! Converting ikle->  ikles
        ! Building ikles
        IF(TYPELT.NE.50) THEN
          IKLES_SIZE = NELEM*SRF_OBJ_TAB(SRF_ID)%NDP
        ELSE
!         tetrahedrons regrouped into prisms
!         nelem*2=(nelem/3)*6
          IKLES_SIZE = NELEM*2
        ENDIF
        ALLOCATE(IKLES(IKLES_SIZE),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'SET_MESH_SRF:IKLES')
!       inversion of ikle in ikles
        IF(TYPELT.NE.50) THEN
          DO I = 1,SRF_OBJ_TAB(SRF_ID)%NDP
            DO IELEM = 1,NELEM
              IKLES((IELEM-1)*SRF_OBJ_TAB(SRF_ID)%NDP+I) =
     &                                     IKLE((I-1)*NELEM+IELEM)
            ENDDO
          ENDDO
        ELSE
!         tetrahedrons regrouped into prisms
!         the first tetrahedrons in a layer have the first 3 points like the bottom of the prism
!         ikle is ikle(NELEM,4)
!         ikles is ikles(6,nelem/3)
          NELEM2=NELEM/3/(NPLAN-1)
          NPOIN2=NPOIN/NPLAN
!         loop on layers
          DO IPLAN=1,NPLAN-1
            DO IELEM2=1,NELEM2
!             prism number
              IELEMP=(IPLAN-1)*NELEM2+IELEM2
!             tetrahedron number (the first of the 3 in the prism ielemp)
              IELEMT=(IPLAN-1)*NELEM2*3+IELEM2
!             ikles of the prism
              IKLES((IELEMP-1)*6+1) = IKLE(IELEMT)
              IKLES((IELEMP-1)*6+2) = IKLE(NELEM+IELEMT)
              IKLES((IELEMP-1)*6+3) = IKLE(2*NELEM+IELEMT)
              IKLES((IELEMP-1)*6+4) = IKLE(IELEMT)+NPOIN2
              IKLES((IELEMP-1)*6+5) = IKLE(NELEM+IELEMT)+NPOIN2
              IKLES((IELEMP-1)*6+6) = IKLE(2*NELEM+IELEMT)+NPOIN2
            ENDDO
          ENDDO
        ENDIF
        !
        TAG = IS*IKLES_SIZE
        WRITE(FILE_ID,IOSTAT=IERR)
     &                       TAG,IKLES(1:IKLES_SIZE),TAG
        CALL CHECK_CALL(IERR,'SET_MESH_SRF:WRITE')
        SRF_OBJ_TAB(SRF_ID)%POS_IPOBO = SRF_OBJ_TAB(SRF_ID)%POS_IKLE
     &      + (4 +
     &           SRF_OBJ_TAB(SRF_ID)%NELEM*SRF_OBJ_TAB(SRF_ID)%NDP*IS
     &           + 4)
        DEALLOCATE(IKLES)
        !
        ! Write ipobo or knolg depending if serial or parallel
        !
        ! We are in serial if both nptfr and nptir are equal to 0
        TAG = IS*NPOIN
        IF(NPTIR.EQ.0) THEN
          WRITE(FILE_ID,IOSTAT=IERR) TAG,IPOBO(1:NPOIN),TAG
        ELSE
          WRITE(FILE_ID,IOSTAT=IERR) TAG,KNOLG(1:NPOIN),TAG
        ENDIF
        CALL CHECK_CALL(IERR,'SET_MESH_SRF:WRITE')
        SRF_OBJ_TAB(SRF_ID)%POS_COORD = SRF_OBJ_TAB(SRF_ID)%POS_IPOBO
     &      + (4 + SRF_OBJ_TAB(SRF_ID)%NPOIN*IS + 4)
        !
        ! Writing coordinates
        !
        TAG = SRF_OBJ_TAB(SRF_ID)%RS*NPOIN
        ! convert in real if file in single precision
        IF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.4) THEN
          ! X COORDINATES
          WRITE(FILE_ID,IOSTAT=IERR) TAG,(REAL(X(I)),I=1,NPOIN),TAG
          CALL CHECK_CALL(IERR,'SET_MESH_SRF:WRITE')
          ! Y COORDINATES
          WRITE(FILE_ID,IOSTAT=IERR) TAG,(REAL(Y(I)),I=1,NPOIN),TAG
          CALL CHECK_CALL(IERR,'SET_MESH_SRF:WRITE')
        ELSE
          ! X Y COORDINATES
          WRITE(FILE_ID,IOSTAT=IERR) TAG,X(1:NPOIN),TAG
          CALL CHECK_CALL(IERR,'SET_MESH_SRF:WRITE')
          WRITE(FILE_ID,IOSTAT=IERR) TAG,Y(1:NPOIN),TAG
          CALL CHECK_CALL(IERR,'SET_MESH_SRF:WRITE')
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%POS_DATA = SRF_OBJ_TAB(SRF_ID)%POS_COORD
     &      + (4 + SRF_OBJ_TAB(SRF_ID)%NPOIN*SRF_OBJ_TAB(SRF_ID)%RS + 4)
     &       *2
        SRF_OBJ_TAB(SRF_ID)%SIZE_DATA =
     &       4 + SRF_OBJ_TAB(SRF_ID)%NPOIN*SRF_OBJ_TAB(SRF_ID)%RS + 4
        SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET =
     &       4 + SRF_OBJ_TAB(SRF_ID)%RS + 4
     &       + SRF_OBJ_TAB(SRF_ID)%NVAR*SRF_OBJ_TAB(SRF_ID)%SIZE_DATA

      END SUBROUTINE
!***********************************************************************
      SUBROUTINE ADD_DATA_SRF
!***********************************************************************
!
     &(FILE_ID,VAR_NAME,TIME,RECORD,FIRST_VAR,VAR_VALUE,N,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Add data information for a given variable and a given time on
!+        all points of the mesh
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| VAR_NAME       |-->| NAME OF THE VARIABLE
!| TIME           |-->| TIME OF THE DATA
!| RECORD         |-->| TIME STEP OF THE DATA
!| FIRST_VAR      |-->| TRUE IF IT IS THE FIRST VARIABLE OF THE DATASET
!| VAR_VALUE      |-->| THE VALUE FOR EACH POINT OF THE MESH
!| N              |-->| SIZE OF VAR_VALUE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        INTEGER,          INTENT(IN)  :: FILE_ID,N
        CHARACTER(LEN=VAR_SIZE), INTENT(IN)  :: VAR_NAME
        DOUBLE PRECISION, INTENT(IN)  :: TIME
        INTEGER,          INTENT(IN)  :: RECORD
        LOGICAL,          INTENT(IN)  :: FIRST_VAR
        DOUBLE PRECISION, INTENT(IN)  :: VAR_VALUE(N)
        INTEGER,          INTENT(OUT) :: IERR
        !
        INTEGER(KIND=I8) :: MY_POS
        INTEGER :: SRF_ID,I,IVAR, IREC
        INTEGER(KIND=I4) :: TAG
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'ADD_DATA_SRF:GET_SRF_OBJ')
        !
        ! Get the position of the variable
        IVAR = 0
        DO I=1,SRF_OBJ_TAB(SRF_ID)%NVAR
          IF(SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I)(1:16).EQ.VAR_NAME(1:16))
     &    THEN
            IVAR = I
            EXIT
          ENDIF
        ENDDO
        IF(IVAR.EQ.0) THEN
          IERR = HERMES_VAR_UNKNOWN_ERR
          RETURN
        ENDIF
        IREC = RECORD
        IF(IREC.LT.0) THEN
          IERR = HERMES_RECORD_UNKNOWN_ERR
          RETURN
        ENDIF
        ! Write time of the dataset if it is the first variable
        IF(FIRST_VAR) THEN
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATA
     &            + INT(IREC,I8)*(SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET)
          READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TAG
          TAG = SRF_OBJ_TAB(SRF_ID)%RS
          IF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.4) THEN
            WRITE(FILE_ID,IOSTAT=IERR) TAG,REAL(TIME),TAG
          ELSE
            WRITE(FILE_ID,IOSTAT=IERR) TAG,TIME,TAG
          ENDIF
          CALL CHECK_CALL(IERR,'ADD_DATA_SRF:WRITE')
          ! We only increase the number of timesteps if we add a new done
          ! i.e. irec is greater than ntimesteps
          IF(IREC.EQ.SRF_OBJ_TAB(SRF_ID)%NTIMESTEP) THEN
            SRF_OBJ_TAB(SRF_ID)%NTIMESTEP =
     &                   SRF_OBJ_TAB(SRF_ID)%NTIMESTEP + 1
          ENDIF
        ELSE
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATA
     &            + INT(IREC,I8)*(SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET)
     &            + 4 + SRF_OBJ_TAB(SRF_ID)%RS + 4 ! THE TIME VALUE
     &            + INT(IVAR-1,I8) * SRF_OBJ_TAB(SRF_ID)%SIZE_DATA
          ! Using a read to go to position in file
          READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TAG

        ENDIF
        ! If the file is in single precision we convert the data

        IF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.4) THEN
          TAG = SRF_OBJ_TAB(SRF_ID)%RS*N
          WRITE(FILE_ID,IOSTAT=IERR) TAG,(REAL(VAR_VALUE(I)),I=1,N),TAG
          CALL CHECK_CALL(IERR,'ADD_DATA_SRF:WRITE')
        ELSE
          ! write time of the dataset if it is the first variable
          ! write the data for that variable
          TAG = SRF_OBJ_TAB(SRF_ID)%RS*N
          WRITE(FILE_ID,IOSTAT=IERR) TAG,VAR_VALUE,TAG
          CALL CHECK_CALL(IERR,'ADD_DATA_SRF:WRITE')
        ENDIF
        !
      END SUBROUTINE
!
!***********************************************************************
      SUBROUTINE SET_BND_SRF
!***********************************************************************
!
     &(FILE_ID,TYPE_BND_ELT,NELEBD,NDP,IKLE,
     & LIHBOR,LIUBOR,
     & LIVBOR,HBOR,UBOR,VBOR,CHBORD,
     & LITBOR,TBOR,ATBOR,BTBOR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Writes the boundary information into the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYPE_BND_ELT   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NELEBD         |-->| NUMBER OF BOUNDARY ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER BOUNDARY ELEMENT
!| IKLE           |-->| CONNECTIVITY ARRAY FOR THE BOUNDARY ELEMENTS
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| ATBOR,BTBOR    |<--| THERMAL EXCHANGE COEFFICIENTS.
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        INTEGER,          INTENT(IN)  :: FILE_ID
        INTEGER,          INTENT(IN)  :: TYPE_BND_ELT
        INTEGER,          INTENT(IN)  :: NELEBD
        INTEGER,          INTENT(IN)  :: NDP
        INTEGER,          INTENT(IN)  :: IKLE(NELEBD*NDP)
        INTEGER,          INTENT(IN)  :: LIUBOR(NELEBD),LIVBOR(NELEBD)
        INTEGER,          INTENT(IN)  :: LIHBOR(NELEBD),LITBOR(NELEBD)
        DOUBLE PRECISION, INTENT(IN)  :: UBOR(NELEBD),VBOR(NELEBD)
        DOUBLE PRECISION, INTENT(IN)  :: HBOR(NELEBD),CHBORD(NELEBD)
        DOUBLE PRECISION, INTENT(IN)  :: TBOR(NELEBD),ATBOR(NELEBD)
        DOUBLE PRECISION, INTENT(IN)  :: BTBOR(NELEBD)
        INTEGER,          INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID, I, NCLI
        !
        CALL GET_SRF_OBJ(FILE_ID,SRF_ID,IERR)
        CALL CHECK_CALL(IERR,'SET_BND_SRF:GET_SRF_OBJ')
        !
        NCLI = SRF_OBJ_TAB(SRF_ID)%NCLI
        SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT = TYPE_BND_ELT
        REWIND(NCLI)
        DO I=1,NELEBD
          ! Write connectivity and bnoundary value the rest is set to 0.D0
          WRITE(NCLI,4000,IOSTAT=IERR) LIHBOR(I),LIUBOR(I),LIVBOR(I),
     &                         HBOR(I),UBOR(I),VBOR(I),
     &                         CHBORD(I),LITBOR(I),
     &                         TBOR(I),ATBOR(I),BTBOR(I),
     &                         IKLE(I),I
 4000       FORMAT (1X,I2,1X,2(I1,1X),3(F24.12,1X),1X,
     &           F24.12,3X,I1,1X,3(F24.12,1X),1I9,1X,1I9,
     &           1X,I10,1X,2(F27.15,1X),I8)
          CALL CHECK_CALL(IERR,'SET_BND_SRF:WRITE:NCLI')
          SRF_OBJ_TAB(SRF_ID)%NPTFR=NELEBD
        ENDDO

      END SUBROUTINE
!
      END MODULE UTILS_SERAFIN

