!                    *****************
                     SUBROUTINE REINIT
!                    *****************
!
     &(NS,NSEG,NPTFR,H,SMTR,HSTOK,HC,HCSTOK,FLUXT,FLUHBOR,DTT,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
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
!| DTT            |<--| PAS DE TEMPS TRACEUR
!| FLUHBOR        |<--| FLUX  TRACEUR FRONTIERE REINITIALISE
!| FLUXT          |<--| FLUX  TRACEUR  REINITIALISE
!| H              |-->| HAUTEURS D'EAU
!| HC             |-->| H RECONSTRUIT ORDRE 2   CORRIGE
!| HCSTOK         |<--| H RECONSTRUIT ORDRE 2   CORRIGE  STOCKE
!| HSTOK          |<--| HAUTEURS D'EAU  STOCKEES
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
!| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
!| NSEG           |-->| NOMBRE D'ARETES DU MAILLAGE
!| NTRAC          |-->| NUMBER OF TRACERS
!| SMTR           |<--| TERMES SOURCES DU TRACEUR
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
