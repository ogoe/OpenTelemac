!                    *****************
                     SUBROUTINE REINIT
!                    *****************
!
     &(NS,NSEG,NPTFR,H,SMTR,HSTOK,HC,HCSTOK,FLUXT,FLUHBOR,DTT,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES A TIME STEP (TRACER).
!
!history  INRIA
!+
!+        V5P8
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
!| DTT            |<--| TIME STEP FOR TRACER
!| FLUHBOR        |<--| TRACER FLUX AT HE BOUNDARY (UPDATED)
!| FLUXT          |<--| TRACER FLUX (UPDATED)
!| H              |-->| WATER DEPTH
!| HC             |-->| RECONSRUCTED H FOR ORDRE 2 (CORRECTED)
!| HCSTOK         |<--| RECONSTRUCTED H FOR ORDRE 2(CORRECTED AND STOCKE)
!| HSTOK          |<--| STOCKED WATER DEPTHS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NS             |-->| TOTAL NUMER OF POINTS IN THE MESH
!| NSEG           |-->| NUMBER OF EDGES IN THE MESH
!| NTRAC          |-->| NUMBER OF TRACERS
!| SMTR           |<--| TRACER SOURCE TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,NSEG,NPTFR,NTRAC
      DOUBLE PRECISION, INTENT(INOUT) :: DTT
      DOUBLE PRECISION, INTENT(INOUT) :: HSTOK(*),HCSTOK(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: H(*),HC(2,*)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SMTR,FLUXT,FLUHBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,NSG,ITRAC
!
      DTT = 0.D0
!
      DO IS=1,NS
        HSTOK(IS)=H(IS)
      ENDDO
!
      DO ITRAC=1,NTRAC
        DO IS=1,NS
          SMTR%ADR(ITRAC)%P%R(IS)=0.D0
        ENDDO
      ENDDO
!
      DO NSG=1,NSEG
        HCSTOK(1,NSG) = HC(1,NSG)
        HCSTOK(2,NSG) = HC(2,NSG)
      ENDDO
!
      DO ITRAC=1,NTRAC
        DO NSG=1,NSEG
          FLUXT%ADR(ITRAC)%P%R(NSG)=0.D0
        ENDDO
      ENDDO
!
      DO ITRAC=1,NTRAC
        DO IS=1,NPTFR
          FLUHBOR%ADR(ITRAC)%P%R(IS)=0.D0
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
