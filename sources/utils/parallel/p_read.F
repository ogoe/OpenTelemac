!                    *****************
                     SUBROUTINE P_READ
!                    *****************
!
!##> JR @ RWTH: DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
     &(BUFFER,N,BYTES,SOURCE,DATATYPE)
!##< JR @ RWTH
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    RECEIVES DATA.
!
!history  REINHARD HINKELMANN (HANOVER)
!+        08/06/1996
!+
!+   MODIFIED
!
!history  J-M MERVOUET (LNH)
!+        17/12/1996
!+
!+   MODIFIED
!
!history  HANS HERRMANN (HANOVRE)
!+        08/01/1997
!+
!+
!
!history  RAINER JOHANNI (SGI MUNICH)
!+        **/10/1999
!+
!+   ADAPTED FOR MPI
!
!history  J.A. JANKOWSKI (BAW KARLSRUHE)
!+        28/12/1999
!+
!+   RELEASE 5.0 MODIFIED
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
!| BUFFER         |<--| ZONE TAMPON POUR LES DONNEES
!|                |   | BUFFER / PUFFERFELD
!|  N             |-->| NOMBRE DE ELEMENTS A TRANSMETTRE
!|                |   | NUMBER OF ELEMENTS / ANZAHL DER ELEMENTE
!|  BYTES         |-->| NOMBRE DE BYTES A ELEMTENT
!|                |   | SIZE OF ELEMENTS IN BYTES / GR"OSSE DER ELEMENTE
!| SOURCE         |-->| ORIGINE DES DONNEES
!|                |   | TID OF THE SENDER / KNOTEN-ID DES SENDER
!| DATATYPE           |-->| TYPE DES DONNEES (MSGTAG DE PVM)
!|                |   | 0 - STRING
!|                |   | 1 - BYTE1
!|                |   | 2 - INTEGER2
!|                |   | 3 - INTEGER4
!|                |   | 4 - REAL4
!|                |   | 5 - COMPLEX8
!|                |   | 6 - REAL8
!|                |   | 7 - COMPLEX16
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
      INTEGER, INTENT(IN) :: N, BYTES,SOURCE,DATATYPE
!##< JR @ RWTH
!> SEB @ HRW: ASSUMED SIZE ARRAY (*)
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      DOUBLE PRECISION, INTENT(INOUT) :: BUFFER(*)
!      DOUBLE PRECISION, INTENT(OUT) :: BUFFER(*)
!< SEB @ HRW
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER STATUS(MPI_STATUS_SIZE)
      INTEGER IER
!
!-----------------------------------------------------------------------
!
!     RECEIVES DATA
!
!##> JR @ RWTH: DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
!      NBYTES = N * MPI_SIZEOF_COMPAD_TYPE( ier )
#  ifdef COMPAD
      CALL AMPI_RECV(BUFFER,N,AMPI_TYPE,SOURCE,4711,
     &              COMM,STATUS,IER)
#  else
      CALL MPI_RECV(BUFFER,N*BYTES,MPI_BYTE,SOURCE,4711,
     &              COMM,STATUS,IER)
#  endif
!##< JR @ RWTH
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_READ: ERREUR IN MPI_RECV'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_READ: ERROR IN MPI_RECV'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      BUFFER(1)=0.D0
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_READ VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_READ IN ITS VOID VERSION'
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
