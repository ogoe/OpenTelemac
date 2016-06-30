!                    *****************
                     SUBROUTINE CORSTR
!                    *****************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CORRECTS THE FRICTION COEFFICIENT ON THE BOTTOM
!+                WHEN IT IS VARIABLE IN TIME.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!code
!+2D   DO I = 1 , NPOIN
!+2D     IF(AT.GT.1200.D0) THEN
!+2D       CHESTR%R(I) = 40.D0
!+2D     ELSE
!+2D       CHESTR%R(I) = 60.D0
!+2D     ENDIF
!+2D   ENDDO
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
!+        V5P6
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
!
!     C2D: EXAMPLE FOR TELEMAC-2D
!     C3D: EXAMPLE FOR TELEMAC-3D
!
!2D   USE DECLARATIONS_TELEMAC2D
!3D   USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!2D   INTEGER I
!3D   INTEGER I
!
!-----------------------------------------------------------------------
!
!2D   DO I = 1 , NPOIN
!2D     IF(AT.GT.1200.D0) THEN
!2D       CHESTR%R(I) = 40.D0
!2D     ELSE
!2D       CHESTR%R(I) = 60.D0
!2D     ENDIF
!2D   ENDDO
!
!-----------------------------------------------------------------------
!
!3D   DO I = 1 , NPOIN2
!3D     IF(AT.GT.1200.D0) THEN
!3D       RUGOF%R(I) = 40.D0
!3D     ELSE
!3D       RUGOF%R(I) = 60.D0
!3D     ENDIF
!3D   ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
