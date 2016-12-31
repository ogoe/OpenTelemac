!                    ******************************
                     SUBROUTINE P_MPI_ALLTOALLV_OIL
!                    ******************************
!
     &(SEND_BUFFER,NSEND,SEND_DISPL,SEND_DATYP,RECV_BUFFER,NRECV,
     & RECV_DISPL,RECV_DATYP,IERR)
!
!***********************************************************************
! PARALLEL   V6P3                                  21/08/2010
!***********************************************************************
!
!brief    Calls function MPI_ALLTOALLV with type OIL_TYPE.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        27/06/2013
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
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: NSEND(*),SEND_DISPL(*),SEND_DATYP,NRECV(*)
      INTEGER, INTENT(IN)  :: RECV_DISPL(*),RECV_DATYP
      INTEGER, INTENT(OUT) :: IERR
      TYPE(OIL_TYPE), INTENT(IN)  :: SEND_BUFFER(*)
      TYPE(OIL_TYPE), INTENT(OUT) :: RECV_BUFFER(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RECV_BUFFER(1)%MYPID = 0
      IERR = 0
#if defined (HAVE_MPI)
      CALL MPI_ALLTOALLV(SEND_BUFFER,NSEND,SEND_DISPL,SEND_DATYP,
     &                   RECV_BUFFER,NRECV,RECV_DISPL,RECV_DATYP,
     &                   COMM,IERR)
!
      IF(IERR.NE.0) THEN
        WRITE(LU,*) 'P_MPI_ALLTOALLV_OIL:'
        WRITE(LU,*) 'MPI ERROR ',IERR
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_MPI_ALLTOALLV_OIL '//
     &                         'VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_MPI_ALLTOALLV_OIL IN '//
     &                         'ITS VOID VERSION'
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
