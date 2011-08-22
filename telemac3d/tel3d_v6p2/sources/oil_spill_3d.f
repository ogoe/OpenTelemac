!                    ***********************
                     SUBROUTINE OIL_SPILL_3D
!                    ***********************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OIL SPILL MODEL.
!+
!+
!+            CALLED IF KEYWORD 'OIL SPILL MODEL' IS SET TO YES
!+                AND USES 'MIGRHYCAR STEERING FILE'.
!
!note     LOGICAL UNIT OF MIGRHYCAR STEERING FILE IS: T2D_FILES(T2DMIG)%LU
!
!warning  DOES NOTHING BY DEFAULT
!
!history  CEDRIC GOEURY (LHSV)
!+        20/04/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DOUBLE PRECISION
!     LOGICAL
!     INTEGER
!     SAVE
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
