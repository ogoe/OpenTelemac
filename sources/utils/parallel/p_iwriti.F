!                    *******************
                     SUBROUTINE P_IWRITI
!                    *******************
!
     &(BUFFER,NVAL,DEST,ITAG,IREQ)
!
!***********************************************************************
! PARALLEL   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    EXCHANGES VALUES BETWEEN PROCESSORS.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        19/11/2013
!+        V7P0
!+     A mere copy of P_IWRIT by Pascal Vezolles for integers. However
!+     NBYTES changed into NVAL to avoid *4 in the calls.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BUFFER         |-->| BUFFER / PUFFERFELD
!| DEST           |-->| DESTINATION DES DONNEES
!|                |   | TID OF THE DEST.  / KNOTEN-ID DES EMPFAENGERS
!| IREQ           |-->| NUMERO DE REQUEST POUR MPI_ISEND
!| ITAG           |-->| MESSAGE TAG
!| NVAL           |-->| NUMBER OF VALUES TO BE TRANSMITTED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
!##> JR @ RWTH: USE OF MPI MODULE
!      USE MPI
!##< JR @ RWTH
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NVAL,DEST,ITAG,IREQ
      INTEGER, INTENT(IN) :: BUFFER(NVAL)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER IER
!
!-----------------------------------------------------------------------
!
!                                4: TO BE PARAMETERISED ????
      CALL MPI_ISEND(BUFFER,NVAL*4,MPI_BYTE,DEST,ITAG,
     &               COMM,IREQ,IER)
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_IWRITI: ERREUR IN MPI_ISEND'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_IWRITI: ERROR IN MPI_ISEND'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_IWRITI VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_IWRITI IN ITS VOID VERSION'
#endif
!
!----------------------------------------------------------------------
!
      RETURN
      END
