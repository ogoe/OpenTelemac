!                    *******************************
                     DOUBLE PRECISION FUNCTION EXLIM
!                    *******************************
!
     &(ILIM,BETA,GRI,GRIJ)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    EXTRAPOLATES THE GRADIENT AND USES OF A SLOPE LIMITER.
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
!| BETA           |-->| EXTRAPOLATION COEFFICIENT FOR ORDRE 2
!| GRI,GRIJ       |-->| GRADIENTS
!| ILIM           |-->| OPTIONS FOR THE LIMITER
!|                |   | 1 : MINMOD
!|                |   | 2 : VAN ALBADA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ILIM
      DOUBLE PRECISION, INTENT(IN) :: GRI,GRIJ,BETA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION GRI1,GRI2,GRIJ2,AUX1,E2
!
!-----------------------------------------------------------------------
!
!    EXTRAPOLATES THE GRADIENT AND USES SLOPE LIMITER
!
      GRI1 = (1.D0+BETA)*GRI - BETA*GRIJ
!
      IF(ILIM.EQ.1) THEN
!
!    MINMOD
!
        EXLIM=0.5D0*(SIGN(1.D0,GRI1)+SIGN(1.D0,GRIJ))
     &   *MIN(ABS(GRI1),ABS(GRIJ))
!
!
      ELSEIF (ILIM.EQ.2) THEN
!
!       VAN ALBADA
!
        E2 = 1.D-12
!
        AUX1 = 0.5D0*(1.D0+SIGN(1.D0,GRI1*GRIJ))
        GRI2  = GRI1*GRI1  + E2
        GRIJ2 = GRIJ*GRIJ  + E2
!
        EXLIM  = AUX1*(GRI2*GRIJ+GRIJ2*GRI)/(GRI2+GRIJ2)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
