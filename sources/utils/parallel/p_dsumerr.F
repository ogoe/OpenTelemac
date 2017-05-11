!                    ********************************
                     DOUBLE PRECISION FUNCTION P_DSUMERR
!                    ********************************
!
     &(PARTIAL)
!
!***********************************************************************
! PARALLEL   V7                                  24/02/2016
!***********************************************************************
!
!brief    SUM AND ERROR ROUNDING OF VALUES FROM ALL THE PROCESSORS.
!
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| PARTIAL         |-->| SEND BUFFER (SUM_VALUE, ERROR VALUE)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: PARTIAL(2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined (HAVE_MPI)

      INTEGER IER,NUM_PROCS
      DOUBLE PRECISION,ALLOCATABLE, DIMENSION(:)::ALL_PARTIAL_SUM
!
!-----------------------------------------------------------------------
!
      CALL MPI_COMM_SIZE (MPI_COMM_WORLD, NUM_PROCS, IER)
      ALLOCATE(ALL_PARTIAL_SUM(1:2*NUM_PROCS))
      ALL_PARTIAL_SUM=0.D0
      CALL MPI_ALLGATHER (PARTIAL, 2, MPI_DOUBLE_PRECISION,
     & ALL_PARTIAL_SUM, 2, MPI_DOUBLE_PRECISION, MPI_COMM_WORLD, IER)
!
      CALL SUMTWO(2*NUM_PROCS, ALL_PARTIAL_SUM,P_DSUMERR)
      DEALLOCATE(ALL_PARTIAL_SUM)
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_DSUMERR: ERREUR DANS MPI_ALLGATHER'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_DSUMERR: ERROR IN MPI_ALLGATHER'
        WRITE(LU,*) 'MPI ERROR: ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
      RETURN
#else
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_DSUM VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_DSUM IN ITS VOID VERSION'
!
      P_DSUMERR=PARTIAL(1)
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
