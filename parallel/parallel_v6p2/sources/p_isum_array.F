!                    ***********************
                     SUBROUTINE P_ISUM_ARRAY
!                    ***********************
!
     &(SEND_BUFFER, RECV_BUFFER, NCOUNT, IERR)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SUM VALUES FROM ALL PROCESSES AND DISTRIBUTES THE RESULT BACK TO ALL PROCESSES
!
!history  AUDOUIN YOANN
!+        25/04/2013
!+
!+   CREATED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SEND_BUFFER       |-->| SEND BUFFER.
!| RECV_BUFFER       |<--| RECV BUFFER.
!| NCOUNT            |-->| SIZE IF THE BUFFERS
!| IERR              |<--| RETURN VALUE OF THE MPI_FUNCTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: SEND_BUFFER(NCOUNT)
      INTEGER, INTENT(OUT) :: RECV_BUFFER(NCOUNT)
      INTEGER, INTENT(IN)  :: NCOUNT
      INTEGER, INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      IERR = 0
#if defined (HAVE_MPI)
      CALL MPI_ALLREDUCE(SEND_BUFFER,RECV_BUFFER,NCOUNT,MPI_INTEGER,
     &                   MPI_SUM,MPI_COMM_WORLD,IERR)
!
      IF(IERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*)  
     &           'P_ISUM_ARRAY: ERREUR DANS MPI_ALLREDUCE'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_ISUM_ARRAY: ERROR IN MPI_ALLREDUCE'
        WRITE (LU,*) 'MPI ERROR ',IERR
        STOP
      ENDIF
      RETURN
#else
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_ISUM_ARRAY VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*)'CALL OF P_ISUM_ARRAY IN ITS VOID VERSION'
      STOP
!
!-----------------------------------------------------------------------
!
#endif
!
!-----------------------------------------------------------------------
!
      END

