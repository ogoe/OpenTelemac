!                    *******************************
                     SUBROUTINE SUSPENSION_FREDSOE !
!                    *******************************
!
     &  (ACLADM, TAUP, NPOIN, GRAV,
     &   XMVE, XMVS, ZERO, AC,  CSTAEQ)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE REFERENCE CONCENTRATION AT Z= 2*D50
!+                ACCORDING TO ZYSERMAN AND FREDSOE FORMULATION (1994).
!
!history  C. VILLARET
!+        14/04/2004
!+        V5P5
!+
!
!history  F. HUVELIN
!+        04/01/2005
!+        V5P6
!+
!
!history  JMH
!+        13/06/2008
!+
!+   FORMULATION OPTIMISED WITH AUX
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
!| AC             |---|
!| ACLADM         |-->|
!| CSTAEQ         |---|
!| GRAV           |-->|
!| NPOIN          |-->|
!| TAUP           |---|
!| XMVE           |-->|
!| XMVS           |-->|
!| ZERO           |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_SUSPENSION_FREDSOE => SUSPENSION_FREDSOE
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, TAUP
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER            :: I
      DOUBLE PRECISION   ::  TETAP,AUX
!
      DOUBLE PRECISION   :: CMAX
!
!     MAXIMUM CONCENTRATION CORRESPONDING TO DENSE PACKING
!
      DATA CMAX/0.6D0/
      INTRINSIC MAX
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! ******************************** !
      !    I - CRITICAL SHIELDS PARAMETER!
      ! ******************************** !
      DO I=1,NPOIN
         ! ****************** !
         ! II - SKIN FRICTION !
         ! ****************** !
         TETAP = TAUP%R(I) / (GRAV*(XMVS-XMVE)*ACLADM%R(I))
         ! ***************** !
         ! IV - EROSION FLUX ! (_IMP_)
         ! ***************** !
         ! CONCENTRATION INCREASED BY AVA BECAUSE IT IS COMPUTED
         ! ONLY WITH ONE CLASS OF SEDIMENT (ASSUMPTION)
         IF(TETAP.GT.AC) THEN
           AUX=(TETAP-AC)**1.75D0
           CSTAEQ%R(I) = 0.331D0*AUX/(1.D0+0.72D0*AUX)
           CSTAEQ%R(I) = MIN(CSTAEQ%R(I),CMAX)
         ELSE
           CSTAEQ%R(I) = 0.D0
         ENDIF
      ENDDO
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE SUSPENSION_FREDSOE