!                    *************************
                     SUBROUTINE P_ALLGATHERV_I
!                    *************************
!
     &(SEND_BUFFER, SEND_COUNT, SEND_TYPE, RECV_BUFFER, 
     & RECV_COUNT, DISPLS, RECV_TYPE, COMM, IERR)
!
!***********************************************************************
! PARALLEL   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    Gathers data from all tasks and deliver the combined data to all tasks
!
!warning  THIS ROUTINE MUST BE CALLED BY ALL THE NODES/PROCESSORS,
!+            OTHERWISE THE PROGRAM WILL "HANG"
!
!history  AUDOUIN YOANN
!+        25/04/2013
!+
!+   CREATED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     
      INTEGER, INTENT(IN)    :: SEND_BUFFER(:)
      INTEGER, INTENT(IN)    :: SEND_COUNT 
      INTEGER, INTENT(IN)    :: SEND_TYPE
      INTEGER, INTENT(OUT)   :: RECV_BUFFER(:)
      INTEGER, INTENT(IN)    :: RECV_COUNT
      INTEGER, INTENT(IN)    :: DISPLS(:)
      INTEGER, INTENT(IN)    :: RECV_TYPE
      INTEGER, INTENT(IN)    :: COMM
      INTEGER, INTENT(OUT)   :: IERR      
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IERR = 0
#if defined (HAVE_MPI)
      CALL MPI_ALLGATHERV(SEND_BUFFER, SEND_COUNT, SEND_TYPE, 
     &                   RECV_BUFFER, RECV_COUNT,
     &                   DISPLS, RECV_TYPE, COMM, IERR)
!
      IF(IERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_ALLGATHERV_I : '//
     &                           'ERREUR DANS MPI_BARRIER'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_ALLGATHERV_I: ERROR IN MPI_BARRIER'
        WRITE(LU,*) 'MPI ERROR ',IERR
        STOP
      ENDIF
      RETURN
#else
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_ALLGATHERV_I VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_ALLGATHERV_I '//
     &                         'IN ITS VOID VERSION'
      STOP
#endif
!
!-----------------------------------------------------------------------
!
      END

