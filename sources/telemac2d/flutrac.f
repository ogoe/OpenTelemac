!                    ******************
                     SUBROUTINE FLUTRAC
!                    ******************
!
     &(NSEG,NPTFR,DT,FLUXT,FLUHBOR,FLUXTEMP,FLUHBTEMP,DTT)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INCREMENTS TRACER FLUXES BY ONE HYDRO TIMESTEP.
!
!history  INRIA
!+
!+        V5P4
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
!| DT             |-->| HYDRAULIC TIME STEP
!| DTT            |<->| TRACER TIME STEP
!| FLUHBOR        |<->| TRACER FLUX AT THE BOUNDARY (INCREMENTED)
!| FLUHBTEMP      |-->| HYDRO BOUNDARY FLUX OF ONE TIME STEP
!| FLUXT          |<->| INCREMENTED TRACER FLUX
!| FLUXTEMP       |-->| FLUX OF ONE HYDRO TIME STEP
!| NPTFR          |-->| NUMBER OF BOUNDARY NODES
!| NSEG           |-->| NUMBER OF SEGMENT OF THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG, NPTFR
      DOUBLE PRECISION, INTENT(IN)    :: DT,FLUXTEMP(*),FLUHBTEMP(*)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUHBOR(*),FLUXT(*),DTT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,NSG
!
!-----------------------------------------------------------------------
!
      DTT=DTT+DT
!
      DO NSG=1,NSEG
        FLUXT(NSG) = FLUXT(NSG) + DT * FLUXTEMP(NSG)
      ENDDO
!
      DO IS=1,NPTFR
        FLUHBOR(IS) = FLUHBOR(IS) + DT * FLUHBTEMP(IS)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
