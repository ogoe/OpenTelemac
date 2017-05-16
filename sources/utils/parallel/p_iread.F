!                    ******************
                     SUBROUTINE P_IREAD
!                    ******************
!##> JR @ RWTH: DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
     &(BUFFER,N,BYTES,SOURCE,ITAG,IREQ)
!##< JR @ RWTH
!
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief
!
!history  PASCAL VEZOLLES (IBM)
!+        23/06/2008
!+        V5P9
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
!| BUFFER         |-->| ZONE TAMPON POUR LES DONNEES
!|                |   | BUFFER / PUFFERFELD
!| IREQ           |-->| NUMERO DE REQUEST POUR MPI_IRECV
!| ITAG           |-->| MESSAGE TAG
!|  N             | -->| NOMBRE DE ELEMENTS A TRANSMETTRE
!|                |    | NUMBER OF ELEMENTS / ANZAHL DER ELEMENTE
!|  BYTES         | -->| NOMBRE DE BYTES A ELEMTENT
!|                |    | SIZE OF ELEMENTS IN BYTES / GR"OSSE DER ELEMENTE
!| SOURCE         |-->| ORIGINE DES DONNEES
!|                |   | TID OF THE SENDER / KNOTEN-ID DES SENDER
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
!##> JR @ RWTH: DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
      INTEGER, INTENT(IN) :: N,BYTES,SOURCE,ITAG,IREQ
!!      INTEGER, INTENT(IN) :: NBYTES,SOURCE,ITAG,IREQ
!##< JR @ RWTH
!> JR @ RWTH: ASSUMED SIZE ARRAY (*)
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      DOUBLE PRECISION, INTENT(INOUT) :: BUFFER(*)
!      DOUBLE PRECISION, INTENT(OUT) :: BUFFER(*)
!< JR @ RWTH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER IER
!
!-----------------------------------------------------------------------
!
!     RECEIVES DATA
!
!-----------------------------------------------------------------------
!

!##> JR @ RWTH: DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
!      NBYTES = N * MPI_SIZEOF_COMPAD_TYPE( ier )
#  ifdef COMPAD
      CALL AMPI_IRECV(BUFFER,N,AMPI_TYPE,SOURCE,ITAG,
     &                COMM,IREQ,IER)
#  else
      CALL MPI_IRECV(BUFFER,N*BYTES,MPI_BYTE,SOURCE,ITAG,
     &                COMM,IREQ,IER)
#  endif
!##< JR @ RWTH
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_IREAD: ERREUR IN MPI_IRECV'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_IREAD: ERROR IN MPI_IRECV'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      BUFFER(1) = 0.0
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_IREAD VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_IREAD IN ITS VOID VERSION'
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
