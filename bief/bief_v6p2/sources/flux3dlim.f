!                    ********************
                     SUBROUTINE FLUX3DLIM
!                    ********************
!
     &(FLOW,FLULIM,NPLAN,NSEG2D)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    LIMITS 3D HORIZONTAL EDGE BY EDGE FLUXES ON POINTS.
!
!history  J-M HERVOUET (LNHE)
!+        19/05/09
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
!| FLOW           |-->| FLUXES (SIZE OF FLOW MAY NOT EXCEED
!|                |   | NSEG2D*NPLAN, THOUGH THE TOTAL NUMBER OF
!|                |   | SEGMENTS IS LARGER)
!| FLULIM         |-->| LIMITING FACTOR OF 2D SEGMENTS
!| NPLAN          |-->| NUMBER OF PLANES
!| NSEG2D         |-->| NUMBER OF SEGMENTS IN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER, INTENT(IN)             :: NSEG2D,NPLAN
      DOUBLE PRECISION, INTENT(INOUT) :: FLOW(*)
!                                        HERE * = NESG2D*NPLAN
      DOUBLE PRECISION, INTENT(IN)    :: FLULIM(NSEG2D)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER ISEG,ISEG3D,IPLAN
!
!-----------------------------------------------------------------------
!
!     LIMITS 3D FLUXES BY COEFFICIENT OF 2D FLUXES
!
      DO IPLAN=1,NPLAN
        DO ISEG=1,NSEG2D
          ISEG3D=ISEG+(IPLAN-1)*NSEG2D
          FLOW(ISEG3D)=FLOW(ISEG3D)*FLULIM(ISEG)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
