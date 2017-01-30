!                       ********************************
                        DOUBLE PRECISION FUNCTION CLIMIT
!                       ********************************
!
!
     &( Q1CR, Q2CR, X1, X2)
!
      IMPLICIT NONE
!
      DOUBLE PRECISION Q1CR, Q2CR, X1, X2
      DOUBLE PRECISION :: FLXCR
      COMMON/FLUX/FLXCR
!
!-----------------------------------------------------------------------
!
      CLIMIT = (X2 - X1)/(Q2CR - Q1CR) * ( FLXCR - Q1CR) + X1
!
!-----------------------------------------------------------------------
!
      RETURN
      END

