!                    ***************************
                     SUBROUTINE SUSPENSION_MILES
!                    ***************************
!
     &(HN,NPOIN,KARMAN,HMIN,ZERO,FDM,FD90,XWC,ZREF,CSRATIO)
!
!***********************************************************************
! SISYPHE   V6P1                                   16/05/2011
!***********************************************************************
!
!BRIEF    COMPUTES THE BED EXCHANGE FACTOR BETA BASED ON MILES (1986) 
!         FOUND IN HR WALLINGFORD REPORT: SR75.
!
!HISTORY  D. M. KELLY (HRW) 29/06/2011
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HMIN           |-->|
!| HN             |-->|
!| KARMAN         |-->|
!| NPOIN          |-->|
!| CSRATIO        |---| RATIO BETWEEN BOTTOM CONC. AND AVERAGE CONC.
!| XWC            |-->| FALL VELOCITY W_S
!| ZERO           |-->|
!| ZREF           |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE, ONLY: SFON,U2D,V2D,DT
      USE INTERFACE_SISYPHE,EX_SUSPENSION_MILES => SUSPENSION_MILES
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HN,ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: FDM,FD90,KARMAN,XWC,HMIN,ZERO
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSRATIO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                    :: I
      DOUBLE PRECISION           :: FVR, Z, Z_DASH
      DOUBLE PRECISION           :: UB,USTARS,RB,BETAS,SS,SB
      DOUBLE PRECISION           :: SFBETA(NPOIN),SB1
      DOUBLE PRECISION, PARAMETER:: RRTPI = 1.D0/(SQRT(3.1415926535D0))
      DOUBLE PRECISION           :: HRERF,HRERFC,SBERF,SBERFC,FVINV
      DOUBLE PRECISION           :: DZ,TAU,TAU_SQ,TST
      DOUBLE PRECISION           :: UCR,DTS
!
!     INTRINSIC ERF
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     COMPUTE CONSTANTS:
!
      FVINV = 1.D0/XWC**2 
!
!     DMK MOD 03/05/2011
!
!     MODIFYING SUSPENDED LOAD TO BE LIKE SANDFLOW AND ALSO TO TAKE
!     LAG INTO CONSIDERATION VIA A BETA COMPUTATION.
!
      DO I=1,NPOIN
!         
        IF (FDM<500.E-6) THEN
!         UCR = 0.19D0*(D50**0.1D0)*LOG10(4.0D0*(HN%R(I)/FDM))    ! SANDFLOW
          UCR = 0.19D0*(FDM**0.1D0)*LOG10(4.0D0*HN%R(I)/FD90)  ! CORRECT
        ELSE
!         UCR = 8.5D0*(D50**0.6D0)*LOG10(4.D0*(HN%R(I)/D50))      ! SANDFLOW
          UCR = 8.5D0*(FDM**0.6D0)*LOG10(4.D0*HN%R(I)/FD90)    ! CORRECT
        END IF  
!
        UB = SQRT(U2D%R(I)**2+V2D%R(I)**2)
!
        IF((HN%R(I)>HMIN).AND.(UB>UCR)) THEN ! STOP PROBLEMS WITH LOW VELOCITIES
          USTARS = 1.3D0*UB*SQRT(SFON/8.D0)
          RB     = XWC*15.D0/USTARS
!         THIS IS DIFFERENT BETWEEN MILES’ PAPERS DUE TO TYPO IN DEF OF R
          BETAS  = RB/(1.D0-EXP(-RB)) 
!         ACTUAL EQUATION FROM MILES (1981)  [1]
          DZ     = (1.D0/6.D0)*0.4D0*HN%R(I)*USTARS
!         USE WITH [1] 
          TAU    = XWC*SQRT(DT/(4.D0*DZ))          
          TAU_SQ = TAU*TAU
!         EQ. (27) NOTE THIS IS ALREADY INTEGRATED WITH RESPECT TO TIME (DTS)
!
!
          WRITE(LU,*) 'SUSPENSION_MILES'
          WRITE(LU,*) 'ERF AND ERFC UNKNOWN IN SOME COMPILERS'
          CALL PLANTE(1)
          STOP
!
!         SFBETA(I) = DZ*FVINV*BETAS*(4.D0*TAU_SQ*(1.D0+TAU_SQ)*(1.D0-ERF(TAU))
!    &    + ERF(TAU) - 2.D0*TAU*(1.D0+2.D0*TAU_SQ)*EXP(-TAU_SQ)*RRTPI) 
! 
          IF((SFBETA(I)*XWC/HN%R(I))>1.D0) SFBETA(I)=1.D0/(XWC*HN%R(I))
!         DIVIDE BACK THROUGH BY DT AS WE WILL INTEGRATE UP  WRTT LATER
          CSRATIO%R(I)=SFBETA(I)/DT ! FOR USE WITH EQ. (27)
!         NOTE WE RECORD BETA_S (PROFILE PARAMETER) I.E. THE RATIO OF REF LEVEL CONC
!         TO DEPTH AVERAGED CONC THIS WILL THEN BE USED IN SUSPENSION_SANDFLOW.F:
!         NB: STORED IN T14 
        ELSE
          SFBETA(I) = 1.D0
          CSRATIO%R(I) = 1.D0
        ENDIF
!
      ENDDO
!
!     END BETA FACTOR COMPUTATION
!       
!======================================================================!
!======================================================================!
!
      RETURN
      END
