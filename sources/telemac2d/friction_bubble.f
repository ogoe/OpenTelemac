!                    **************************
                     SUBROUTINE FRICTION_BUBBLE
!                    **************************
!
     &(IKLE, NPOIN, NELEM, NELMAX, LINDNER, NKFROT, CHESTR, NDEFMA,
     & LINDDP, LINDSP)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FRICTION VECTOR FOR THE QUASI-BUBBLE ELEMENT.
!
!history  F. HUVELIN
!+        22/12/2004
!+
!+
!
!history  J-M HERVOUET (LNHE)
!+
!+        V5P5
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
      TYPE(BIEF_OBJ), INTENT(IN)    :: IKLE
      INTEGER,        INTENT(IN)    :: NPOIN, NELEM, NELMAX
      LOGICAL,        INTENT(IN)    :: LINDNER
      TYPE(BIEF_OBJ), INTENT(INOUT) :: NKFROT, CHESTR, NDEFMA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: LINDDP, LINDSP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I, I1, I2, I3
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      DO I = NPOIN + 1, NPOIN + NELEM
!
        I1 = IKLE%I(I - NPOIN           )
        I2 = IKLE%I(I - NPOIN +   NELMAX)
        I3 = IKLE%I(I - NPOIN + 2*NELMAX)
!
        ! COMPUTING THE VALUES OF THE MIDDLE-NODE
        ! ---------------------------------------
        IF (NKFROT%I(I1).EQ.NKFROT%I(I2)) THEN
          ! THE 3 NODES HAVE THE SAME LAW !
          ! ***************************** !
          IF (NKFROT%I(I1).EQ.NKFROT%I(I3))THEN
!
            NKFROT%I(I) = NKFROT%I(I1)
            CHESTR%R(I) = (CHESTR%R(I3) + CHESTR%R(I2) +CHESTR%R(I1))
     &                  / 3.D0
            NDEFMA%R(I) = (NDEFMA%R(I3) + NDEFMA%R(I2) +NDEFMA%R(I1))
     &                  / 3.D0
!
            IF (LINDNER) THEN
              LINDDP%R(I) = ( LINDDP%R(I3) + LINDDP%R(I2)
     &                       +LINDDP%R(I1) )/3.D0
!
              LINDSP%R(I) = ( LINDSP%R(I3) + LINDSP%R(I2)
     &                       +LINDSP%R(I1) )/3.D0
            ENDIF
!
          ! THE NODES "1" AND "2" HAVE THE SAME LAW !
          ! *************************************** !
          ELSE
            NKFROT%I(I) = NKFROT%I(I1)
            CHESTR%R(I) = (CHESTR%R(I2) + CHESTR%R(I1))/2.D0
            NDEFMA%R(I) = (NDEFMA%R(I2) + NDEFMA%R(I1))/2.D0
!
            IF (LINDNER) THEN
              LINDDP%R(I) = (LINDDP%R(I2) + LINDDP%R(I1))/2.D0
              LINDSP%R(I) = (LINDSP%R(I2) + LINDSP%R(I1))/2.D0
            ENDIF
          ENDIF
!
        ! THE NODES "2" AND "3" HAVE THE SAME LAW !
        ! *************************************** !
        ELSE IF (NKFROT%I(I2).EQ.NKFROT%I(I3)) THEN
!
          NKFROT%I(I) = NKFROT%I(I2)
          CHESTR%R(I) = (CHESTR%R(I3) + CHESTR%R(I2))/2.D0
          NDEFMA%R(I) = (NDEFMA%R(I3) + NDEFMA%R(I2))/2.D0
!
          IF (LINDNER) THEN
            LINDDP%R(I) = (LINDDP%R(I3) + LINDDP%R(I2))/2.D0
            LINDSP%R(I) = (LINDSP%R(I3) + LINDSP%R(I2))/2.D0
          ENDIF
!
        ! THE 3 NODES HAVE DIFFERENT LAWS : VALUE OF THE NODE "1" KEPT !
        ! ************************************************************ !
        ELSE
          NKFROT%I(I) = NKFROT%I(I1)
          CHESTR%R(I) = CHESTR%R(I1)
          NDEFMA%R(I) = NDEFMA%R(I1)
!
          IF (LINDNER) THEN
            LINDDP%R(I) = LINDDP%R(I1)
            LINDSP%R(I) = LINDSP%R(I1)
          ENDIF
        ENDIF
      ENDDO
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END
