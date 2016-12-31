!                    ************************
                     SUBROUTINE FRICTION_QUAD
!                    ************************
!
     &(IKLE, NPOIN, NELEM, NELMAX, LINDNER, NKFROT, CHESTR, NDEFMA,
     & LINDDP, LINDSP)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FRICTION VECTOR FOR THE QUADRATIC ELEMENT.
!
!history  F. HUVELIN
!+        22/12/2004
!+
!+
!
!history  JACEK JANKOWSKI (BAW)
!+
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
!| CHESTR         |<->| FRICTION COEFFICIENTS
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LINDDP         |<--| DIAMETER OF ROUGHNESS ELEMENT IN LINDNER CASE
!| LINDNER        |-->| IF YES, THERE IS NON-SUBMERGED VEGETATION FRICTION
!| LINDSP         |<--| SPACING OF ROUGHNESS ELEMENT IN LINDNER CASE
!| NDEFMA         |<--| DEFAULT MANNING COEFFICIENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NKFROT         |<->| LAW OF BOTTOM FRICTION FOR EVERY POINT
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,        INTENT(IN)    :: NPOIN,NELEM,NELMAX
      INTEGER,        INTENT(IN)    :: IKLE(NELMAX,6)
      LOGICAL,        INTENT(IN)    :: LINDNER
      TYPE(BIEF_OBJ), INTENT(INOUT) :: NKFROT,CHESTR,NDEFMA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: LINDDP,LINDSP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: IELEM
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
! IF THE 11 NODES HAVE THE SAME FRICTION LAW, INTERPOLATION A LA CG1113
! FOR THE VALUES AT 13 ADDITIONAL NODES IN THE MIDDLE OF THE EDGES
!
!        X(IKLE(IELEM,4)) = 0.5D0 * ( X(IKLE(IELEM,1))
!     &                             + X(IKLE(IELEM,2)) )
!        X(IKLE(IELEM,5)) = 0.5D0 * ( X(IKLE(IELEM,2))
!     &                             + X(IKLE(IELEM,3)) )
!        X(IKLE(IELEM,6)) = 0.5D0 * ( X(IKLE(IELEM,3))
!     &                             + X(IKLE(IELEM,1)) )
!
! WELL, IF THE THE FRICTION LAWS DIFFER, TAKE THE VALUE ON THE PREVIOUS
! NODE BY CIRCUMVENTING THE ELEMENT...
!
!        X(IKLE(IELEM,4)) = X(IKLE(IELEM,1))
!        X(IKLE(IELEM,5)) = X(IKLE(IELEM,2))
!        X(IKLE(IELEM,6)) = X(IKLE(IELEM,3))
!
! ASSUMED THE TRIVIAL CASE OF -ONE- ZONE ONLY IS NATURALLY EXCLUDED
!
      DO IELEM = 1,NELEM
        IF(NKFROT%I(IKLE(IELEM,1)).EQ.NKFROT%I(IKLE(IELEM,2))) THEN
          NKFROT%I(IKLE(IELEM,4)) = NKFROT%I(IKLE(IELEM,1))
          CHESTR%R(IKLE(IELEM,4)) =
     &      0.5D0*(CHESTR%R(IKLE(IELEM,1))+CHESTR%R(IKLE(IELEM,2)))
          NDEFMA%R(IKLE(IELEM,4)) =
     &      0.5D0*(NDEFMA%R(IKLE(IELEM,1))+NDEFMA%R(IKLE(IELEM,2)))
        ELSE
          NKFROT%I(IKLE(IELEM,4)) = NKFROT%I(IKLE(IELEM,1))
          CHESTR%R(IKLE(IELEM,4)) = CHESTR%R(IKLE(IELEM,1))
          NDEFMA%R(IKLE(IELEM,4)) = NDEFMA%R(IKLE(IELEM,1))
        ENDIF
        IF(NKFROT%I(IKLE(IELEM,2)).EQ.NKFROT%I(IKLE(IELEM,3))) THEN
          NKFROT%I(IKLE(IELEM,5)) = NKFROT%I(IKLE(IELEM,2))
          CHESTR%R(IKLE(IELEM,5)) =
     &      0.5D0*(CHESTR%R(IKLE(IELEM,2))+CHESTR%R(IKLE(IELEM,3)))
          NDEFMA%R(IKLE(IELEM,5)) =
     &      0.5D0*(NDEFMA%R(IKLE(IELEM,2))+NDEFMA%R(IKLE(IELEM,3)))
        ELSE
          NKFROT%I(IKLE(IELEM,5)) = NKFROT%I(IKLE(IELEM,2))
          CHESTR%R(IKLE(IELEM,5)) = CHESTR%R(IKLE(IELEM,2))
          NDEFMA%R(IKLE(IELEM,5)) = NDEFMA%R(IKLE(IELEM,2))
        ENDIF
        IF(NKFROT%I(IKLE(IELEM,3)).EQ.NKFROT%I(IKLE(IELEM,1))) THEN
          NKFROT%I(IKLE(IELEM,6)) = NKFROT%I(IKLE(IELEM,3))
          CHESTR%R(IKLE(IELEM,6)) =
     &      0.5D0*(CHESTR%R(IKLE(IELEM,3))+CHESTR%R(IKLE(IELEM,1)))
          NDEFMA%R(IKLE(IELEM,6)) =
     &      0.5D0*(NDEFMA%R(IKLE(IELEM,3))+NDEFMA%R(IKLE(IELEM,1)))
        ELSE
          NKFROT%I(IKLE(IELEM,6)) = NKFROT%I(IKLE(IELEM,3))
          CHESTR%R(IKLE(IELEM,6)) = CHESTR%R(IKLE(IELEM,3))
          NDEFMA%R(IKLE(IELEM,6)) = NDEFMA%R(IKLE(IELEM,3))
        ENDIF
      ENDDO
      ! THIS RARE CASE SEPARATELY
      IF (LINDNER) THEN
        DO IELEM = 1,NELEM
          IF (NKFROT%I(IKLE(IELEM,1)).EQ.NKFROT%I(IKLE(IELEM,2))) THEN
            LINDDP%R(IKLE(IELEM,4)) =
     &        0.5D0*(LINDDP%R(IKLE(IELEM,1))+LINDDP%R(IKLE(IELEM,2)))
            LINDSP%R(IKLE(IELEM,4)) =
     &        0.5D0*(LINDSP%I(IKLE(IELEM,1))+LINDSP%R(IKLE(IELEM,2)))
          ELSE
            LINDDP%R(IKLE(IELEM,4)) = LINDDP%R(IKLE(IELEM,1))
            LINDSP%R(IKLE(IELEM,4)) = LINDSP%R(IKLE(IELEM,1))
          ENDIF
          IF (NKFROT%I(IKLE(IELEM,2)).EQ.NKFROT%I(IKLE(IELEM,3))) THEN
            LINDDP%R(IKLE(IELEM,5)) =
     &        0.5D0*(LINDDP%R(IKLE(IELEM,2))+LINDDP%R(IKLE(IELEM,3)))
            LINDSP%R(IKLE(IELEM,5)) =
     &        0.5D0*(LINDSP%I(IKLE(IELEM,2))+LINDSP%R(IKLE(IELEM,3)))
          ELSE
            LINDDP%R(IKLE(IELEM,5)) = LINDDP%R(IKLE(IELEM,2))
            LINDSP%R(IKLE(IELEM,5)) = LINDSP%R(IKLE(IELEM,2))
          ENDIF
          IF (NKFROT%I(IKLE(IELEM,3)).EQ.NKFROT%I(IKLE(IELEM,1))) THEN
            LINDDP%R(IKLE(IELEM,6)) =
     &        0.5D0*(LINDDP%R(IKLE(IELEM,3))+LINDDP%R(IKLE(IELEM,1)))
            LINDSP%R(IKLE(IELEM,6)) =
     &        0.5D0*(LINDSP%I(IKLE(IELEM,3))+LINDSP%R(IKLE(IELEM,1)))
          ELSE
            LINDDP%R(IKLE(IELEM,6)) = LINDDP%R(IKLE(IELEM,3))
            LINDSP%R(IKLE(IELEM,6)) = LINDSP%R(IKLE(IELEM,3))
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END SUBROUTINE FRICTION_QUAD
