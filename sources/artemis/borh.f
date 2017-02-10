!                    ***************
                     SUBROUTINE BORH
!                    ***************
!
!
!***********************************************************************
! ARTEMIS   V7P2                                     Nov 2016
!***********************************************************************
!
!brief    TAKES INTO ACCOUNT USER-SPECIFIED BOUNDARY CONDITIONS.
!+              THEY ARE GIVEN BY SEGMENT.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/08/2000
!+        V5P1
!+
!
!history  C. DENIS (SINETICS)
!+        18/03/2010
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
!history  N.DURAND (HRW)
!+        November 2016
!+        V7P2
!+   BORH void for general use now that wave / boundary parameters
!+   can be read from cli file, and that TETAB is assigned before
!+   call to BORH for all cases (uni or multidirectional waves)
!+   The subroutine can still be used for advanced definition of
!+   boundaries or if the user chooses not to use the cli file, but
!+   does nothing by default
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      DOUBLE PRECISION PI
!
      INTRINSIC COS,SIN,ATAN
!
!-----------------------------------------------------------------------
!
!> SEB @ HRW: ALGORITHMIC DIFFERENTIATION
      PI = 4.D0 * ATAN( 1.D0 )
!      PARAMETER( PI = 3.1415926535897932384626433D0)
!< SEB @ HRW
!
!-----------------------------------------------------------------------
!
! ALL ANGLES ARE IN  DEGREES
!                    -------
! ---------------------------------------
! EXAMPLE
! ---------------------------------------
!
!      DO I=1,NPTFR
!        JB=BOUNDARY_COLOUR%I(I)
!!       SOLID BOUNDARY
!        IF(JB.GE.2.AND.JB.LE.139)THEN
!          RP%R(I) = 1.D0
!          TETAP%R(I) = 90.D0
!          ALFAP%R(I) = 0.D0
!        ENDIF
!      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
