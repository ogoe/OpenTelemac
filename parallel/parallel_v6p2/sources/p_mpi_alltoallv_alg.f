!                    ******************************
                     SUBROUTINE P_MPI_ALLTOALLV_ALG
!                    ******************************
!
     &(SEND_BUFFER,NSEND,SEND_DISPL,SEND_DATYP,RECV_BUFFER,NRECV,
     & RECV_DISPL,RECV_DATYP,COMM,IERR)
!
!***********************************************************************
! PARALLEL   V6P3                                  21/08/2010
!***********************************************************************
!
!brief    Calls function MPI_ALLTOALLV with type ALG_TYPE.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        23/05/2013
!+        V6P3
!+   First version, inpired from P_MPI_ALLTOALLV.
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
      TYPE ALG_TYPE 
        SEQUENCE   ! NECESSARY TO DEFINE MPI TYPE ALG_CHAR
        INTEGER :: MYPID ! PARTITION OF THE TRACEBACK ORIGIN (HEAD) 
        INTEGER :: NEPID ! THE NEIGHBOUR PARTITION THE TRACEBACK ENTERS TO  
        INTEGER :: IGLOB  ! THE GLOBAL NUMBER OF THE PARTICLES 
        INTEGER :: FLAG  ! USED TO ALIGN FIELDS
        DOUBLE PRECISION :: VX,VY,VZ  ! THE (X,Y,Z) PARTICLE VELOCITY  
        DOUBLE PRECISION :: UX,UY,UZ  ! THE (X,Y,Z) FLUID VELOCITY  
        DOUBLE PRECISION :: UX_AV,UY_AV,UZ_AV  ! THE (X,Y,Z) AVERAGE FLUID VELOCITY  
        DOUBLE PRECISION :: K_AV,EPS_AV  ! THE VALUES OF K AND EPS  
        DOUBLE PRECISION :: H_FLU  ! THE WATER DEPTH AT POSITION OF VELOCITY 
      END TYPE ALG_TYPE 
      INTEGER, INTENT(IN)  :: NSEND(*),SEND_DISPL(*),SEND_DATYP,NRECV(*)
      INTEGER, INTENT(IN)  :: RECV_DISPL(*),RECV_DATYP,COMM
      INTEGER, INTENT(OUT) :: IERR
      TYPE(ALG_TYPE), INTENT(IN)  :: SEND_BUFFER(*)
      TYPE(ALG_TYPE), INTENT(OUT) :: RECV_BUFFER(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL MPI_ALLTOALLV(SEND_BUFFER,NSEND,SEND_DISPL,SEND_DATYP,
     &                   RECV_BUFFER,NRECV,RECV_DISPL,RECV_DATYP,
     &                   COMM,IERR)
!
      IF(IERR.NE.0) THEN
        WRITE(LU,*) 'P_MPI_ALLTOALLV_ALG:'
        WRITE(LU,*) 'MPI ERROR ',IERR
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
