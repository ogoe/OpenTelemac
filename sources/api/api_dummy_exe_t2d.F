      PROGRAM API_DUMMY_EXE_T2D
        USE API_INTERFACE_T2D
        USE DECLARATIONS_PARALLEL
        USE DECLARATIONS_PARTEL, ONLY : CODE
        IMPLICIT NONE
        INTEGER ::  I, K, IERR, ID, IDUM
        INTEGER :: NPOIN,NPTFR,NELEM,NTIME_STEPS,NELMAX
        CHARACTER(LEN=144) :: CAS_FILE, DICO_FILE
        CHARACTER(LEN=250) ::  RES_FILE,GEO_FILE, CLI_FILE, RES_FILE2
        CHARACTER(LEN=250) :: NAMESEC, NAMEZONE
        CHARACTER(LEN=8) :: FFORMAT
        CHARACTER(LEN=T2D_VAR_LEN) :: VARNAME
        INTEGER LU,LNG
        INTEGER IPID,NCSIZE,PMETHOD,VAR_SIZE
        INTEGER SPLIT_COMM,NCSIZE2,IPID2,COLOR
        INTEGER NPLAN
!       PARAMETER FOR TELEMAC2D/MASCARET COUPLING
        ! TYPE FOR MASCARET BOUNDARY CONDITION

        ! OUTPUT FOR WRITING
        LU=6
        ! 1 FOR FRENCH 2 FOR ENGLISH
        LNG=2
        ID = 0
        NPLAN = 0
        CAS_FILE = 't2d_gouttedo.cas'
        DICO_FILE = '/home/B61570/opentelemac/'
     &     //'git/trunk/sources/telemac2d/'
     &     //'telemac2d.dico'
        GEO_FILE='geo_gouttedo.slf'
        CLI_FILE='geo_gouttedo.cli'
        FFORMAT = 'SERAFIN '
        NAMESEC = REPEAT(' ',250)
        NAMEZONE = REPEAT(' ',250)
        ! Partitioning method to use 1: metis
        PMETHOD=1
!
#if defined(HAVE_MPI)
        ! Initialising mpi
        CALL MPI_INIT(IERR)
        ! Getting rank
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,IPID,IERR)
        ! Getting the number of process
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NCSIZE,IERR)
!
        COLOR = IPID/2
        CALL MPI_COMM_SPLIT(MPI_COMM_WORLD,COLOR,IPID,SPLIT_COMM,IERR)
        PRINT*,'MPI_COMM_SPLIT',IERR
        CALL MPI_COMM_SIZE(SPLIT_COMM,NCSIZE2,IERR)
        PRINT *,IERR
        CALL MPI_COMM_RANK(SPLIT_COMM,IPID2,IERR)
        PRINT *,IERR
        PRINT *,'IPID/NCSIZE',IPID,'/',NCSIZE,
     &          'IPID2/NCSIZE2',IPID2,'/',NCSIZE2

        ! The partitioning is done sequentially

        IF(IPID.EQ.0) THEN
          ! PARITIONING THE GEOMETRY FILE
          CODE='T2D'
          CALL PARTEL(GEO_FILE,CLI_FILE,NCSIZE2,PMETHOD,
     &                FFORMAT,NAMESEC,NAMEZONE)
        ENDIF
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
#endif

!
        CALL RUN_SET_CONFIG_T2D(ID,666+COLOR,LNG,SPLIT_COMM,IERR)
        PRINT *, 'IERR',IERR
        PRINT *, 'ID',ID

        CALL RUN_READ_CASE_T2D(ID,CAS_FILE,DICO_FILE,IERR)
        PRINT *, 'IERR',IERR

        ! Changing the name of the result file
        VARNAME = 'MODEL.RESULTFILE'
        CALL GET_VAR_SIZE_T2D(ID,VARNAME,VAR_SIZE,IDUM,IDUM,IERR)
        PRINT *, 'RANK:',IPID,'VAR_SIZE:',VAR_SIZE
        RES_FILE='toto_'//CHAR(74+COLOR)//'.srf'
        CALL SET_STRING_T2D(ID,VARNAME,RES_FILE,VAR_SIZE,IERR)
        PRINT *, 'IERR',IERR

        CALL RUN_ALLOCATION_T2D(ID,IERR)
        PRINT *, 'IERR',IERR

        CALL RUN_INIT_T2D(ID,IERR)
        PRINT *, 'IERR',IERR

        VARNAME = 'MODEL.NPOIN'
        CALL GET_INTEGER_T2D(ID, VARNAME, NPOIN, 0, 0, 0, IERR)
        PRINT *, 'IERR',IERR

        VARNAME = 'MODEL.NPTFR'
        CALL GET_INTEGER_T2D(ID, VARNAME, NPTFR, 0, 0, 0, IERR)
        PRINT *, 'IERR',IERR

        VARNAME = 'MODEL.NELEM'
        CALL GET_INTEGER_T2D(ID, VARNAME, NELEM, 0, 0, 0, IERR)
        PRINT *, 'IERR',IERR

        VARNAME = 'MODEL.NELMAX'
        CALL GET_INTEGER_T2D(ID, VARNAME, NELMAX, 0, 0, 0, IERR)
        PRINT *, 'IERR',IERR

        VARNAME = 'MODEL.NTIMESTEPS'
        CALL GET_INTEGER_T2D(ID, VARNAME, NTIME_STEPS,
     &                       0, 0, 0, IERR)
        PRINT *, 'IERR',IERR

        PRINT *, NPOIN, NPTFR, NELEM, NTIME_STEPS
!
        DO I=1,NTIME_STEPS
          CALL RUN_TIMESTEP_T2D(ID,IERR)
        ENDDO
!
        CALL RUN_FINALIZE_T2D(ID,IERR)
#if defined(HAVE_MPI)
        ! Mergin step
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
        IF(IPID2.EQ.0) THEN
          PRINT *,'Gretel step for ',TRIM(RES_FILE)
          CALL GRETEL_AUTOP(GEO_FILE,FFORMAT,RES_FILE,FFORMAT,
     &                      NCSIZE2,NPLAN)
        ENDIF
        CALL MPI_FINALIZE(IERR)
#endif

        END PROGRAM
