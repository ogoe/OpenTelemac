!                    ***************************
                     SUBROUTINE SOURCE_TELEMAC2D
!                    ***************************
!
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    REDEFINES THE CHARACTERISTICS OF THE SOURCES
!+                WITHOUT USING THE STEERING FILE.
!
!warning  USER SUBROUTINE
!code
!+     EXAMPLE
!+
!+     NREJET = ?  (UNTIL MAXSCE)
!+     NREJEU = NREJET (IF VELOCITIES GIVEN)
!+
!+     DO I=1,NREJET
!+
!+       XSCE(I) = ???
!+       YSCE(I) = ???
!+       DSCE(I) = ???
!+       TSCE(I) = ???
!+       USCE(I) = ???
!+       VSCE(I) = ???
!+
!+     ENDDO
!
!history  J-M HERVOUET LNH
!+        26/10/1994
!+        V5P2
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
!     USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!     EXAMPLE
!
!     NREJET = ?  (UNTIL MAXSCE)
!     NREJEU = NREJET (IF VELOCITIES GIVEN)
!
!     DO I=1,NREJET
!
!       XSCE(I) = ???
!       YSCE(I) = ???
!       DSCE(I) = ???
!       TSCE(I) = ???
!       USCE(I) = ???
!       VSCE(I) = ???
!
!     ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END