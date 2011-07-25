!                    *****************
                     SUBROUTINE DISPER
!                    *****************
!
     &( VISC , U , V , H , CF , ELDER , PROPNU )
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE TENSORIAL DISPERSION COEFFICIENTS
!+                ACCORDING TO THE LONGITUDINAL AND TRANSVERSE
!+                COEFFICIENTS.
!
!history  C MOULIN (LNH)
!+        26/05/2006
!+        V5P6
!+   + MODIFS JMH
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
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| ELDER          |-->| ADIMENSIONAL DISPERSION COEFFICIENTS
!| H              |-->| WATER DEPTH
!| PROPNU         |-->| LAMINAR VISCOSITY
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| VISC           |<--| COEFFICIENTS OF DISPERSION TENSOR VISC(NPOIN,3)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: ELDER(2),PROPNU
      DOUBLE PRECISION, INTENT(IN)  :: H(*),CF(*),U(*),V(*)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VISC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,NPOIN,NPX
!
      DOUBLE PRECISION KL,KT,COST,SINT,NORMV,USTAR
!
      INTRINSIC SQRT,MAX
!
!-----------------------------------------------------------------------
! COMPUTES DISPERSION COEFFICIENTS
!-----------------------------------------------------------------------
!
      NPOIN = VISC%DIM1
      NPX   = VISC%MAXDIM1
!
      DO 20 I=1,NPOIN
!
         NORMV = MAX(SQRT(U(I)**2+V(I)**2),1.D-6)
         COST = U(I)/NORMV
         SINT = V(I)/NORMV
         USTAR = SQRT( 0.5D0 * CF(I) * ( U(I)**2 + V(I)**2 ) )
         KL = ELDER(1) * USTAR * H(I)
         KT = ELDER(2) * USTAR * H(I)
         VISC%R(I      ) = PROPNU + ( KL - KT ) * COST**2    + KT
         VISC%R(I+NPX  ) = PROPNU + ( KT - KL ) * COST**2    + KL
         VISC%R(I+2*NPX) = PROPNU + ( KL - KT ) * COST*SINT
!
20    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
