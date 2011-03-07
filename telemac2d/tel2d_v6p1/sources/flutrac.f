!                    ******************
                     SUBROUTINE FLUTRAC
!                    ******************
!
     &(NSEG,NPTFR,DT,FLUXT,FLUHBOR,FLUXTEMP,FLUHBTEMP,DTT)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
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
!| DT             |-->| PAS DE TEMPS HYDRO
!| DTT            |<->| PAS DE TEMPS TRACEUR
!| FLUHBOR        |<->| FLUX  TRACEUR FRONTIERE INCREMENTE
!| FLUHBTEMP      |-->| FLUX FRONTIERE D'UN PAS DE TEMPS HYDRO
!| FLUXT          |<->| FLUX  TRACEUR INCREMENTE
!| FLUXTEMP       |-->| FLUX D'UN PAS DE TEMPS HYDRO
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
!| NSEG           |-->| NOMBRE D'ARETES DU MAILLAGE
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