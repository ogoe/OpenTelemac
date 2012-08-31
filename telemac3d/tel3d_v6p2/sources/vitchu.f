!                    *****************
                     SUBROUTINE VITCHU
!                    *****************
!
     & (W_SED, WCHU_CONST)
!
!***********************************************************************
! TELEMAC3D   V6P1                                  21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE SETTLING VELOCITY AS A FUNCTION
!+                OF TEMPERATURE, SALINITY AND CONCENTRATION OF
!+                SUSPENDED SEDIMENT.
!
!history  C LE NORMANT (LNH)
!+        01/08/91
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!| WCHU_CONST     |-->| CONSTANT SEDIMENT SETTLING VELOCITY (M/S)
!| W_SED          |<->| SEDIMENT SETTLING VELOCITY (M/S)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: WCHU_CONST
      TYPE(BIEF_OBJ), INTENT(INOUT) :: W_SED
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     CONSTANT VALUE GIVEN HERE
!
      CALL OS( 'X=C     ' , X=W_SED , C=WCHU_CONST )
!
      RETURN
      END SUBROUTINE VITCHU
