!                    **************************
                     SUBROUTINE P_MPI_ALLTOALLV
!                    **************************
!
     &(SEND_BUFFER,NSEND,SEND_DISPL,SEND_DATYP,RECV_BUFFER,NRECV,
     & RECV_DISPL,RECV_DATYP,COMM,IERR)
!
!***********************************************************************
! PARALLEL   V6P2                                  21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_ALLTOALLV.
!
!history  C. DENIS (SINETICS)
!+        27/10/2009
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SEND_BUFFER    |-->| SEND BUFFER  
!| NSEND          |-->| SPECIFIES THE NUMBER OF ELEMENTS TO SEND TO EACH
!|                |   | PROCESSOR 
!| SEND_DISPL     |-->| DISPLACEMENT ARRAY FOR THE SEND BUFFER 
!| SEND_DATYP     |-->| DATA TYPE OF SEND BUFFER ELEMENTS
!| RECV_BUFFER    |<--| RECEIVE BUFFER
!| NRECV          |-->| SPECIFIES THE MAXIMUM NUMBER OF ELEMENTS THAT 
!|                |   | CAN BE RECEIVED FROM EACH PROCESSOR
!| RECV_DISPL     |-->| DISPLACEMENT ARRAY FOR THE RECEIVE BUFFER 
!| RECV_DATYP     |-->| DATA TYPE OF RECEIVE BUFFER ELEMENTS
!| COMM           |-->| COMMUNICATOR 
!| IERR           |-->| ERROR VALUE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE CHARAC_TYPE 
        SEQUENCE   ! BUT SEEMS USELESS (HENCE TRICK BELOW WITH VOID)  
        INTEGER :: MYPID ! PARTITION OF THE TRACEBACK ORIGIN (HEAD) 
        INTEGER :: NEPID ! THE NEIGHBOUR PARTITION THE TRACEBACK ENTERS TO  
        INTEGER :: INE   ! THE LOCAL 2D ELEMENT NR THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION    
        INTEGER :: KNE   ! THE LOCAL LEVEL THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION    
        INTEGER :: IOR   ! THE POSITION OF THE TRAJECTORY -HEAD- IN MYPID [THE 2D/3D NODE OF ORIGIN] 
        INTEGER :: ISP,NSP ! NUMBERS OF RUNGE-KUTTA PASSED AS COLLECTED AND TO FOLLOW AT ALL 
        INTEGER :: VOID  ! TRICK FOR ALIGNMENT 
        DOUBLE PRECISION :: XP,YP,ZP,FP             ! THE (X,Y,Z)-POSITION NOW 
        DOUBLE PRECISION :: DX,DY,DZ,DF             ! THE DISPLACEMENTS
        DOUBLE PRECISION :: BASKET(10) ! VARIABLES INTERPOLATED AT THE FOOT   
      END TYPE CHARAC_TYPE 
      INTEGER, INTENT(IN)  :: NSEND(*),SEND_DISPL(*),SEND_DATYP,NRECV(*)
      INTEGER, INTENT(IN)  :: RECV_DISPL(*),RECV_DATYP,COMM
      INTEGER, INTENT(OUT) :: IERR
      TYPE(CHARAC_TYPE), INTENT(IN)  :: SEND_BUFFER(*)
      TYPE(CHARAC_TYPE), INTENT(OUT) :: RECV_BUFFER(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL MPI_ALLTOALLV(SEND_BUFFER,NSEND,SEND_DISPL,SEND_DATYP,
     &                   RECV_BUFFER,NRECV,RECV_DISPL,RECV_DATYP,
     &                   COMM,IERR)
!
      IF(IERR.NE.0) THEN
        WRITE(LU,*) 'P_MPI_ALLTOALLV:'
        WRITE(LU,*) 'MPI ERROR ',IERR
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
