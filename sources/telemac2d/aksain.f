!                    *****************
                     SUBROUTINE AKSAIN
!                    *****************
     &(VISCSA,NPOIN,NUMIN,PROPNU)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   31/08/2015
!***********************************************************************
!
!BRIEF    INITIALISES VISCSA.
!
!
!history  A BOURGOIN (LNHE)
!+        31/08/2015
!+        V7p0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NUMIN          |-->| MINIMUM VISCSA IF CLIPPING
!| PROPNU         |-->| KINEMATIC VISCOSITY
!| VISCSA         |-->| SPALART-ALLMARAS VISCOSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TELEMAC2D, EX_AKSAIN => AKSAIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         ,INTENT(IN   ) :: NPOIN
      DOUBLE PRECISION,INTENT(INOUT) :: VISCSA(NPOIN)
      DOUBLE PRECISION,INTENT(IN   ) :: NUMIN,PROPNU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER           N,IELMU
      DOUBLE PRECISION  CS,CS2,CV1
!
!-----------------------------------------------------------------------
!
      CS = 0.1D0
      CS2 = CS**2
      CV1=7.1D0
!
      DO N=1,NPOIN
        VISCSA(N)=1.D-5
      ENDDO
!
!-----------------------------------------------------------------------
!      
      RETURN
      END
      
