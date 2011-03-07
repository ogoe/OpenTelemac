!                    ***************************************
                     SUBROUTINE SUSPENSION_VANRIJN ! (_IMP_)
!                    ***************************************
!
     &  (ACLADM, TAUP, NPOIN, GRAV,
     &   XMVE, XMVS, ZERO, AC, CSTAEQ,ZREF)
!
!***********************************************************************
! SISYPHE
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| 
!| ACLADM         |-->| 
!| GRAV           |-->| 
!| NPOIN          |-->| 
!| XMVE           |-->| 
!| XMVS           |-->| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : VCE
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, TAUP,ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    ::  GRAV,  XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ

      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER            :: I
      DOUBLE PRECISION   ::  TAUC,AUX,DSTAR,DENS
      INTRINSIC MAX
!======================================================================!
!======================================================================!
!                               PROGRAMME                              !
!======================================================================!
!======================================================================!
      ! ******************************** !
      !    I - CRITICAL SHIELD PARAMETER !
      ! ******************************** !
!
      DO I=1,NPOIN
         ! ****************** !
         ! II - SKIN FRICTION !
         ! ****************** !
          TAUC = AC * (GRAV*(XMVS-XMVE)*ACLADM%R(I))
          DENS  = (XMVS - XMVE )/ XMVE
          DSTAR = ACLADM%R(I)*(GRAV*DENS/VCE**2)**(1.D0/3.D0)
         ! ***************** !
         ! IV - EROSION FLUX ! (_IMP_)
         ! ***************** !
         ! Concentration increased by AVA because it is assumed
         ! that it is computed only with one class of sediment
           IF(DSTAR.LE. ZERO) THEN
           	PRINT*, 'ERROR SUSPENSION_VANRIJN'
           	CALL PLANTE(1)
           ENDIF
           AUX=(TAUP%R(I)-TAUC)/TAUC
           IF(AUX.GT.ZERO) THEN
              CSTAEQ%R(I)=0.015*ACLADM%R(I)*SQRT(AUX**3.D0)/
     &                (ZREF%R(I)*DSTAR**0.3D0)
           ELSE
             CSTAEQ%R(I) = 0.D0
           ENDIF
!
      ENDDO
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE SUSPENSION_VANRIJN