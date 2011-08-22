!                       *************************
                        SUBROUTINE P_MPI_ADDRESS3
!                       *************************
!
     *(LOCATION,ADDRESS,IER)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_ADDRESS (HERE 1ST ARGUMENT
!+                DOUBLE PRECISION ARRAY).
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
      INCLUDE 'mpif.h'
      DOUBLE PRECISION LOCATION(10)
      INTEGER IER
      INTEGER (MPI_ADDRESS_KIND) ADDRESS
!
!-----------------------------------------------------------------------
!
      CALL MPI_GET_ADDRESS(LOCATION,ADDRESS,IER)
!
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'P_MPI_ADDRESS3:'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
!
!----------------------------------------------------------------------
!
      RETURN
      END
