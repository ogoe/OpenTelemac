!                   ***********************
                     SUBROUTINE TAUB_WAQTEL
!                    **********************
!
     &(CF,DENSITY,TAUB,NPOIN,UN,VN)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES BED SHEAR STRESS FOR WAQTEL - SEE THE USE OF TOB_SISYPHE
!
!                              
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| FRICTION COEFFICIENT
!| DENSITY        |-->| DENSITY
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| TAUB            |<--| BED SHEAR STRESS
!| UN,VN          |-->| VELOCITY COMPONENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_WAQTEL, EX_TAUB_WAQTEL => TAUB_WAQTEL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: DENSITY
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TAUB
      INTRINSIC ABS,SQRT,MAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     LOCAL VARIABLES
      DOUBLE PRECISION  CC
      TYPE(BIEF_OBJ)    UNORM
!     
!
!     MEAN VELOCITY
!
      CALL OS('X=N(Y,Z)',X=UNORM,Y=UN,Z=VN)
!
!     TOB=.5*RHO*CF*U^2
      CC=0.5D0*DENSITY
      CALL OV('X=CYZ   ',TAUB%R,CF%R,UNORM%R,CC,NPOIN) 
!      
      RETURN
      END
!
!-----------------------------------------------------------------------
!
