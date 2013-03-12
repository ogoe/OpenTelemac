!                    ********************************
                     SUBROUTINE P_MPI_TYPE_GET_EXTENT
!                    ********************************
!
     &(DATATYPE,LOWER_BOUND,EXTENT,IERR)
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_TYPE_GET_EXTENT.
!
!history  C. DENIS (SINETICS)
!+        27/10/2009
!+        V6P0
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
!| DATATYPE             |-->|  DATATYPE
!| LOWER_BOUND             |<--|  LOWER BOUND OF THE DATATYPE
!| EXTENT             |<--|  EXTENT OF THE DATATYPE
!| IERR           |<--|  ERROR VALUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INCLUDE 'mpif.h'
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(INOUT):: LOWER_BOUND,EXTENT
      INTEGER, INTENT(INOUT)                       :: DATATYPE
      INTEGER, INTENT(OUT)                         :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL MPI_TYPE_GET_EXTENT(DATATYPE,LOWER_BOUND,EXTENT,IERR)
!
      IF(IERR.NE.0) THEN
        WRITE(LU,*) 'P_MPI_TYPE_EXTENT:'
        WRITE(LU,*) 'MPI ERROR ',IERR
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
