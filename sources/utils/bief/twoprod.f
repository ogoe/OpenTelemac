!                    ******************
                     SUBROUTINE TWOPROD
!                    ******************
!
     &(A,B,X,Y)
!
!***********************************************************************
! BIEF   V7                                  24/02/2016
!***********************************************************************
!
!brief    SCALAR PRODUCT AND CALCULATE ROUNDING ERROR.
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A,B          |-->| DOUBLE PRECISION SCALAR
!| X            |<--| DOUBLE PRECISION PRODUC RESULT
!| Y            |<--| DOUBLE PRECISION ROUNDING ERROR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: A,B
      DOUBLE PRECISION, INTENT(OUT) :: X,Y
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION AH,AL,BH,BL
!
!-----------------------------------------------------------------------
!
      AH=0.D0
      AL=0.D0
      BH=0.D0
      BL=0.D0
      X=A*B
      CALL SPLIT(A,AH,AL)
      CALL SPLIT(B,BH,BL)
      Y=AL*BL-(((X-AH*BH)-AL*BH)-AH*BL)
!
!
!-----------------------------------------------------------------------
!
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE SPLIT(A,X,Y)
        DOUBLE PRECISION, INTENT(IN)  :: A
        DOUBLE PRECISION, INTENT(OUT) :: X,Y
        DOUBLE PRECISION Z
        INTEGER CONSTANTE
        CONSTANTE= 134217729
        Z=0.D0
        Z=CONSTANTE*A
        X=Z-(Z-A)
        Y=A-X
      END SUBROUTINE SPLIT
