!                    *******************
                     SUBROUTINE P_IREADI
!                    *******************
!
     &(BUFFER,NVAL,SOURCE,ITAG,IREQ)
!
!***********************************************************************
! PARALLEL   V7P0                                   21/08/2010
!***********************************************************************
!
!brief
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        19/11/2013
!+        V7P0
!+     A mere copy of P_IREAD by Pascal Vezolles for integers. However
!+     NBYTES changed into NVAL to avoid *4 in the calls.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BUFFER         |-->| BUFFER / PUFFERFELD
!| IREQ           |-->| NUMERO DE REQUEST POUR MPI_IRECV
!| ITAG           |-->| MESSAGE TAG
!| NVAL           |-->| NUMBER OF VALUES TO BE TRANSMITTED
!| SOURCE         |-->| ORIGINE DES DONNEES
!|                |   | TID OF THE SENDER / KNOTEN-ID DES SENDER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: NVAL,SOURCE,ITAG,IREQ
      INTEGER, INTENT(OUT) :: BUFFER(NVAL)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined (HAVE_MPI)
      INTEGER IER
!
!-----------------------------------------------------------------------
!
!     RECEIVES DATA
!
!-----------------------------------------------------------------------
!
!                                4: TO BE PARAMETERISED ????
      CALL MPI_IRECV(BUFFER,NVAL*4,MPI_BYTE,SOURCE,ITAG,
     &               MPI_COMM_WORLD,IREQ,IER)
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_IREADI: ERREUR IN MPI_IRECV'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_IREADI: ERROR IN MPI_IRECV'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      BUFFER(1) = 0
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_IREADI VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_IREADI IN ITS VOID VERSION'
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
