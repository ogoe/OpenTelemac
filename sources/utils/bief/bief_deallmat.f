!                    ************************
                     SUBROUTINE BIEF_DEALLMAT
!                    ************************
!
     &(MAT)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    DEALLOCATES MEMORY FOR A REAL MATRIX STRUCTURE.
!
!history  Y AUDOUIN (LNHE)
!+        25/05/2013
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MAT            |<->| THE MATRIX TO BE DEALLOCATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: MAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL BIEF_DEALLVEC(MAT%D)
      DEALLOCATE(MAT%D)
      CALL BIEF_DEALLVEC(MAT%X)
      DEALLOCATE(MAT%X)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
