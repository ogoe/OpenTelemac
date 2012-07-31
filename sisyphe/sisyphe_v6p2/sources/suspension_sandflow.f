!                    ***************************************
                     SUBROUTINE SUSPENSION_SANDFLOW ! (_IMP_)
!                    ***************************************
!
     &  (FDM, FD90, TAUP, NPOIN, GRAV,
     &   XMVE, XMVS, ZERO, AC, CSTAEQ,ZREF,HN,U2D,V2D,CSRATIO)
!
!***********************************************************************
! SISYPHE
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| 
!| FDM            |-->| D50
!| GRAV           |-->| Acceleration due to gravity
!| NPOIN          |-->|
!| XMVE           |-->| Water density
!| XMVS           |-->| Sediment density
!| CSTAEQ         |<->| Equilibrium sediment concentration
!! HN             |-->| Water deptj
!| U2D            |-->| Depth averaged velocity in x direction
!| V2D            |-->| Depth averaged velocity in y direction
!| T2             |<->| Work array
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! + Purpose:
! + --------------------------------------------------------------------
! + Attempt to ensure Sisyphe computes C_a in the same way as HRW-SANDFLOW.
! + Uses Soulsby-van Rijn formula to give a (depth-averaged) saturated concentration.
! + --------------------------------------------------------------------
! + Author:        Dr. D. M. Kelly (HRW)
! + Date Modified: 10/06/2011
! +

      USE INTERFACE_SISYPHE,
     & EX_SUSPENSION_SANDFLOW => SUSPENSION_SANDFLOW
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : VCE,UW,HOULE,PRIVE
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
!mak      TYPE(BIEF_OBJ),   INTENT(IN)       :: TAUP,ZREF,HN,U2D,V2D,T2
      TYPE(BIEF_OBJ),   INTENT(IN)       :: TAUP,ZREF,HN,U2D,V2D,CSRATIO
      INTEGER,          INTENT(IN)       :: NPOIN
      DOUBLE PRECISION, INTENT(IN)       :: GRAV, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)       :: ZERO,AC,FDM,FD90
      TYPE(BIEF_OBJ),   INTENT(INOUT)    :: CSTAEQ
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER            :: I
      DOUBLE PRECISION   :: TAUC,AUX,DSTAR,DENS,COEFF,UCR,D50,D90,ASS,UB
      DOUBLE PRECISION   :: WORB,CD
      DOUBLE PRECISION, PARAMETER :: Z0=0.006D0
      INTRINSIC MAX
!======================================================================!
!======================================================================!
!                               PROGRAMME                              !
!======================================================================!
!======================================================================!
 
      DENS  = (XMVS - XMVE )/ XMVE
      
      DO I=1,NPOIN
         ! ***************** !
         ! IV - EROSION FLUX ! (_IMP_)
         ! ***************** !      
        DSTAR = FDM*(GRAV*DENS/(VCE*VCE))**(1.D0/3.D0)  
        COEFF = (DENS*GRAV*FDM)**1.2D0   
        IF(DSTAR.LE. ZERO) THEN
         PRINT*,'ERROR SUSPENSION_SANDFLOW:DSTAR = ',DSTAR,FDM,VCE,GRAV
          CALL PLANTE(1)
        ENDIF
        IF ((HN%R(I).GT.ZERO).AND.(COEFF.GT.ZERO)) THEN
!         Critical velocities:
          IF (FDM<500.E-6) THEN
!            UCR = 0.19D0*(D50**0.1D0)*log10(4.0D0*(HN%R(I)/D50))    ! SANDFLOW
            UCR = 0.19D0*(FDM**0.1D0)*log10(4.0D0*HN%R(I)/FD90)  ! Correct
          ELSE
!               UCR = 8.5D0*(D50**0.6D0)*log10(4.D0*(HN%R(I)/D50))      ! SANDFLOW
            UCR = 8.5D0*(FDM**0.6D0)*log10(4.D0*HN%R(I)/FD90)    ! Correct
          END IF  
!              Wave orbital velocities:
          IF (HOULE) THEN
            CD   = (0.4D0/(log(max(HN%R(I),Z0)/Z0)-1.D0))**2.D0
            WORB = (0.018D0/CD)*(UW%R(I)**2.D0)
          ELSE
            WORB = 0.D0
          ENDIF
          ASS = (0.012D0*FDM*(DSTAR**(-0.6D0)))/COEFF      ! Just for suspended load (a la SANDFLOW)
          UB  = (U2D%R(I)*U2D%R(I))+(V2D%R(I)*V2D%R(I))+WORB ! vel
!             Note we multiply eq. (136a) of Soulsby (1997) through by (uh)^{-1} to get dimensionless
!              depth-averaged saturation concentration Ca:
          CSTAEQ%R(I)=ASS*(max(0.D0,(sqrt(UB)-UCR))**2.4D0)/HN%R(I) !Missing orb vels
!              Dump saturated conc output to supplementary variable A (private array with index 1):
          PRIVE%ADR(1)%P%R(I)=CSTAEQ%R(I)
          CSTAEQ%R(I)=CSTAEQ%R(I)*CSRATIO%R(I)
        ELSE
          CSTAEQ%R(I) = 0.D0
        ENDIF
      ENDDO
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE SUSPENSION_SANDFLOW
