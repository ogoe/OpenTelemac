!                    *****************
                     SUBROUTINE STRCHE
!                    *****************
!
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE BOTTOM FRICTION COEFFICIENT
!+                IF VARIABLE IN SPACE.
!
!note     IN PARAMETER ESTIMATION WITH A LIST OF TESTS,
!+         THESE VALUES ARE DISCARDED.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!code
!+  COMMENTS CEX MUST BE REMOVED TO IMPLEMENT THE EXAMPLE.
!+  HERE A CONSTANT FRICTION VALUE IS GIVEN:
!+
!+CEX   DO I=1,NPOIN
!+CEX     CHESTR%R(I) = 60.D0
!+CEX   ENDDO
!
!history  J-M HERVOUET (LNH)
!+        01/10/96
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
!
!     DECLARATIONS MUST BE ADAPTED TO EVERY CODE
!     THIS EXAMPLE APPLIES TO TELEMAC2D
!
!EX   USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!EX   INTEGER I
!
!-----------------------------------------------------------------------
!
!     HERE A CONSTANT FRICTION VALUE IS GIVEN
!
!EX   DO I=1,NPOIN
!EX     CHESTR%R(I) = 60.D0
!EX   ENDDO
!
!-----------------------------------------------------------------------
!
!     COMMENTS HERE MAY BE CHANGED
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'STRCHE (BIEF) : PAS DE MODIFICATION DU FROTTEMENT'
        WRITE(LU,*)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'STRCHE (BIEF): NO MODIFICATION OF FRICTION'
        WRITE(LU,*)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
