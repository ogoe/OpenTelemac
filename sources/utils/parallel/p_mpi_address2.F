!                       *************************
                        SUBROUTINE P_MPI_ADDRESS2
!                       *************************
!
     &(LOCATION,ADDRESS,IER)
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_ADDRESS (HERE 1ST ARGUMENT
!+                DOUBLE PRECISION).
!
!history  J.-M. HERVOUET (LNHE)
!+        19/08/2008
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
!| ADDRESS        |-->| LOCATION IN CALLER MEMORY
!| IER            |<--| ERROR VALUE
!| LOCATION       |<--| ADDRESS OF LOCATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)            :: LOCATION
      INTEGER, INTENT(OUT)                    :: IER
      INTEGER (MPI_ADDRESS_KIND), INTENT(OUT) :: ADDRESS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined (HAVE_MPI)
      CALL MPI_GET_ADDRESS(LOCATION,ADDRESS,IER)
!
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'P_MPI_ADDRESS2:'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      IER=0
      ADDRESS=0
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_MPI_ADDRESS2 VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_MPI_ADDRESS2 VOID VERSION'
#endif
!
!----------------------------------------------------------------------
!
      RETURN
      END
