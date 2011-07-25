!                    ********************
                     SUBROUTINE DEF_ZONES
!                    ********************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DEFINES ZONES IN THE MESH. THE RESULT MUST BE :
!+
!+            NZONE : THE NUMBER OF ZONES,
!+
!+            ZONE : STRUCTURE OF SIZE NPOIN STATING THE ZONE NUMBER
!+                       OF EVERY POINT.
!
!warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!
!history  J-M HERVOUET
!+        17/08/2001
!+        V5P3
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
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
!
!     NZONE = ???
!     ZONE%I(I) = ???
!
!-----------------------------------------------------------------------
!
      RETURN
      END
