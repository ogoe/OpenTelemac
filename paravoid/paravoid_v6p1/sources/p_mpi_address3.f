!                    *************************
                     SUBROUTINE P_MPI_ADDRESS3
!                    *************************
!
     &(LOCATION,ADDRESS,IER)
!
!***********************************************************************
! PARAVOID   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_ADDRESS (HERE 1ST ARGUMENT
!+                DOUBLE PRECISION ARRAY).
!
!warning  EMPTY SHELL IN SCALAR MODE FOR PARALLEL COMPATIBILITY
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
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION LOCATION(*)
      INTEGER ADDRESS,IER
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_MPI_ADDRESS3 VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_MPI_ADDRESS3 VOID VERSION'
!
!----------------------------------------------------------------------
!
      RETURN
      END
