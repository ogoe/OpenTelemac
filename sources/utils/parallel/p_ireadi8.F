!                    ********************
                     SUBROUTINE P_IREADI8
!                    ********************
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
!+        09/05/2014
!+        V7P0
!+     A mere copy of P_IREAD by Pascal Vezolles for integers. However
!+     NBYTES changed into NVAL to avoid *4 in the calls.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BUFFER         |-->| BUFFER IN INTEGER 8 BYTES.
!| IREQ           |-->| NUMERO DE REQUEST POUR MPI_IRECV
!| ITAG           |-->| MESSAGE TAG
!| NVAL           |-->| NUMBER OF VALUES TO BE TRANSMITTED
!| SOURCE         |-->| ORIGINE DES DONNEES
!|                |   | TID OF THE SENDER / KNOTEN-ID DES SENDER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_PARALLEL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: NVAL,SOURCE,ITAG,IREQ
      INTEGER(KIND=K8), INTENT(OUT) :: BUFFER(NVAL)
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
!                                8: TO BE PARAMETERISED ????
      CALL MPI_IRECV(BUFFER,NVAL*8,MPI_BYTE,SOURCE,ITAG,
     &               MPI_COMM_WORLD,IREQ,IER)
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_IREADI8: ERREUR IN MPI_IRECV'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_IREADI8: ERROR IN MPI_IRECV'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      BUFFER(1) = 0
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_IREADI8 VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_IREADI8 IN ITS VOID VERSION'
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
