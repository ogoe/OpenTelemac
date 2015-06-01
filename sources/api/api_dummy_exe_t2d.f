      PROGRAM API_DUMMY_EXE_T2D
        USE API_INTERFACE_T2D
        IMPLICIT NONE
!       include 'mpif.h'
        INTEGER ::  I, K, IERR, ID, IDUM
        INTEGER :: NPOIN,NPTFR,NELEM,NTIME_STEPS,NELMAX
        CHARACTER(LEN=144) :: CAS_FILE, DICO_FILE, RES_FILE
        CHARACTER(LEN=250) :: GEO_FILE, CLI_FILE, RES_FILE2
        CHARACTER(LEN=T2D_VAR_LEN) :: VARNAME
        INTEGER LU,LNG
        INTEGER RANK,NCSIZE,PMETHOD,VAR_SIZE
!       PARAMETER FOR TELEMAC2D/MASCARET COUPLING
        ! TYPE FOR MASCARET BOUNDARY CONDITION

        ! OUTPUT FOR WRITING
        LU=6
        ! 1 FOR FRENCH 2 FOR ENGLISH
        LNG=2
        ID = 0
        CAS_FILE = 't2d_gouttedo.cas'
        DICO_FILE = '/home/B61570/opentelemac/'
     &     //'branches/weirdfish/sources/telemac2d/'
     &     //'telemac2d.dico'
        RES_FILE='toto.srf'
        RES_FILE2='toto.srf'
        GEO_FILE='geo_gouttedo.slf'
        CLI_FILE='geo_gouttedo.cli'
        ! Partitioning method to use 1: metis
        PMETHOD=1
!
        ! Initialising mpi
!       CALL MPI_INIT(IERR)
        ! Getting rank
!       CALL MPI_COMM_RANK(MPI_COMM_WORLD,RANK,IERR)
        ! Getting the number of process
!       CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NCSIZE,IERR)
!
        ! The partitioning is done sequentially
        IF(RANK.EQ.0) THEN
          ! PARITIONING THE GEOMETRY FILE
!         CALL PARTEL(GEO_FILE,CLI_FILE,NCSIZE,PMETHOD,
!    &                .FALSE.,' ',.FALSE.,' ')
        ENDIF
!
        CALL RUN_SET_CONFIG_T2D(ID,LU,LNG,IERR)
        PRINT *, 'IERR',IERR
        PRINT *, 'ID',ID

        CALL RUN_READ_CASE_T2D(ID,CAS_FILE,DICO_FILE,IERR)
        PRINT *, 'IERR',IERR

        ! Changing the name of the result file
        VARNAME = 'MODEL.RESULTFILE'
        CALL GET_VAR_SIZE_T2D(ID,VARNAME,VAR_SIZE,IDUM,IDUM,IERR)
        PRINT *, 'RANK:',RANK,'VAR_SIZE:',VAR_SIZE
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
        ! Mergin step
!       IF(RANK.EQ.0) THEN
!         CALL GRETEL_AUTOP(GEO_FILE,RES_FILE2,NCSIZE)
!       ENDIF
!       CALL MPI_FINALIZE(IERR)

        END PROGRAM
