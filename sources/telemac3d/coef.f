!                    ***************
                     SUBROUTINE COEF
!                    ***************
!
     &     (IVIDE , EPAI , TRA01 ,
     &      NPFMAX, IMAX , NDEB  ,
     &      RHOS  , GRAV , DTC   , DSIG1    )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS RESULTING FROM THE
!+                DISCRETISATION OF THE DIFFERENTIAL EQUATION GOVERNING
!+                THE CONSOLIDATION OF MUDDY BEDS.
!
!history  C LE NORMANT (LNH)
!+        13/05/92
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
!| DSIG1          |-->| DERIVATIVE OF THE EFFECTIVE STRESS
!|                |   | AT THE FIRST POINT OF THE MESH
!| DTC            |-->| TIME STEP FOR CONSOLIDATION
!| EPAI           |-->| THICKNESS OF MESH ELEMENTS DISCRETISING THE BED
!| GRAV           |-->| GRAVITY ACCELERATION
!| IMAX           |-->| NUMBER OF POINTS AT THE BOTTOM MESH
!| IVIDE          |-->| INDEX OF EMPTY SPACES AT MESH POINTS
!| NDEB           |-->| INDEX LIMITING THE RANGE OF RESOLUTION
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES THAT
!|                |   | DISCRETISE MUDDY BOTTOM
!| RHOS           |-->| SEDIMENT DENSITY
!| TRA01          |<->| WORKING ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) ::  NPFMAX , IMAX,NDEB
!
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPFMAX,6)
      DOUBLE PRECISION, INTENT(IN) :: EPAI(NPFMAX-1), IVIDE(NPFMAX)
      DOUBLE PRECISION, INTENT(IN) :: RHOS , GRAV ,DTC , DSIG1
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION A , D
!
!=======================================================================
!
!      -----INITIALISES THE COEFFICIENTS-----
!
      DO I=NDEB,IMAX
        TRA01(I,3)=0.D0
        TRA01(I,4)=1.D0
        TRA01(I,5)=0.D0
        TRA01(I,6)=0.D0
      END DO
!
!     -----COMPUTES THE COEFFICIENTS AT THE MESH NODES-----
!
!     ...NODE ON THE BOTTOM:
      TRA01(NDEB,5)=-1.D0
      TRA01(NDEB,6)=(RHOS-1000.D0)*GRAV*EPAI(1)/DSIG1
!
!     ...INTERIOR NODE:
      DO I=NDEB+1,IMAX-1
!
        IF (TRA01(I,1).GE.1.D-10) THEN
          A=TRA01(I,1)/(EPAI(I-1))
          D=1.D0
        ELSE
          A=TRA01(I,1)/(EPAI(I))
          D=0.D0
        ENDIF
!
        TRA01(I,3)=DTC*(-(TRA01(I,2)+TRA01(I-1,2))/((EPAI(I)+EPAI(I-1))
     &                *EPAI(I-1))-D*A)
        TRA01(I,5)=DTC*((1.D0-D)*A-((TRA01(I,2)+TRA01(I+1,2))
     &                /((EPAI(I)+EPAI(I-1))*EPAI(I))))
        TRA01(I,4)=1.D0-TRA01(I,3)-TRA01(I,5)
        TRA01(I,6)=IVIDE(I)
!
      END DO
!
!     ...INTERFACE NODE:
      TRA01(IMAX,6)=IVIDE(IMAX)
!
      RETURN
      END SUBROUTINE COEF
