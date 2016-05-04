!                    ***************************
                      SUBROUTINE CALCS_MICROPOL
!                    **************************
     & (NPOIN,NTRAC,TN,TEXP,HPROP,CF,UN,VN,T1,T2,T3,T4)
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                        21/09/2014
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR MICROPOL WAQ PROCESS
!          WAQ PROCESS OF CODE_TRACER (MASCARET SYSTEM)
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION (VOID)
!history  R. ATA
!+        28/09/2015
!+        V7P1
!+       REAL CREATION
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! !  TAUB     ! BED SHEAR
! !  ERO      ! EROSION RATE
! !  TAUS     ! CRITICAL STRESS OF RESUSPENSION
! !  TAUR     ! SEDIMENTATION CRITICAL STRESS
! !  VITCHU   ! SEDIMENT SETTLING VELOCITY
! !  CCSEDIM  ! CONSTANT OF EXPONENTIAL DESINTEGRATION
! !  CDISTRIB ! COEFFICIENT OF DISTRIBUTION (KD)
! !  KDESORP  ! KINETIC CONSTANT OF  DESORPTION
! !___________!____!____!______________________________________________
!-----------------------------------------------------------------------
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NTRAC          |-->| NUMBER OF TRACERS
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| TN             |-->| TRACERS
!| TEXP           |<--| SOURCE TERMS OF TRACERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY:ERO,TAUR,TAUS,VITCHU,CDISTRIB,
     &                             ROO,KDESORP,CCSEDIM
!      USE EXCHANGE_WITH_ATMOSPHERE
      USE INTERFACE_WAQTEL, EX_CALCS_MICROPOL => CALCS_MICROPOL

      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER          , INTENT(IN   ) :: NPOIN,NTRAC
!      LOGICAL          , INTENT(IN   ) :: YATEMP  ! IF TEMPERATURE IS VARIABLE
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,T1,T2,T3,T4
!
!     LOCAL VARIABLES
      INTEGER                     :: RANKTR1,RANKTR2,RANKTR3,RANKTR4
      INTEGER                     :: RANKTR5
      INTEGER         , PARAMETER :: ADDTR = 5
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION            :: CC
!
!     =======================================
!     PRELIMINARY COMPUTATIONS
!     =======================================
!
      RANKTR1 = NTRAC-ADDTR+1  ! SS (SUSPENDED LOAD KG/M3)
      RANKTR2 = RANKTR1+1      ! SF (BED SEDIMENTS KG/M2)
      RANKTR3 = RANKTR2+1      ! C (MICROPOLLUTANT DENSITY KG/M3 OR Bq/M3)
      RANKTR4 = RANKTR3+1      ! CSS (ABSORBED POLLUTANT BY SUSP. LOAD)
      RANKTR5 = NTRAC          ! CFF (ABSORBED POLLUTANT BY BED SEDIMENT)
!
!     BED SHEAR STRESS (TAUB-STOCKED IN T1)
!
      CALL TAUB_WAQTEL(CF,ROO,T1,NPOIN,UN,VN)
!
!     DEPOTION PROBABILITY (SED): STOCKED IN T2
!
      CALL DEPOS_FX(T2,T1,TAUS,VITCHU,NPOIN)
!
!     EROSION FLUX (RS): STOCKED IN T3
!
      CALL EROSION_FX(T3,T1,TN%ADR(RANKTR2)%P,TAUR,ERO,1.D-10,NPOIN)
!
!
!     =======================================
!     LET'S NOW COMPUTE SOURCE TERMS
!     =======================================
!
!     FIRST TRACER: SUSPENDED LOAD [SS] (RANKTR1)
!
      CALL OS ('X=Y-Z   ',TEXP%ADR(RANKTR1)%P,T3,T2)
      CALL OVD('X=Y/Z   ',TEXP%ADR(RANKTR1)%P%R,TEXP%ADR(RANKTR1)%P%R,
     &         HPROP%R,0.D0,NPOIN,2,0.D0,EPS)
!
!     SECOND TRACER: BED SEDIMENT [SF] (RANKTR2)
!      warning: no advection neither diffusion for this tracer
!
      CALL OS ('X=Y-Z   ',TEXP%ADR(RANKTR2)%P,T2,T3)
!
!     THIRD TRACER: POLLUTANT DENSITY [C] (RANKTR3)
!
      CALL OV( 'X=CY    ' ,TEXP%ADR(RANKTR3)%P%R,TN%ADR(RANKTR3)%P%R,
     &                     T1%R,-CCSEDIM,NPOIN)
      CALL OV( 'X=X+CY  ' ,TEXP%ADR(RANKTR3)%P%R,TN%ADR(RANKTR4)%P%R,
     &                     T1%R,KDESORP,NPOIN)
      CC=-KDESORP*CDISTRIB
      CALL OV( 'X=X+CYZ ' ,TEXP%ADR(RANKTR3)%P%R,TN%ADR(RANKTR3)%P%R,
     &                     TN%ADR(RANKTR1)%P%R,CC,NPOIN)
!
!     FORTH TRACER: ABSORBED POLLUTANT BY SUSPENDED LOAD [CSS] (RANKTR4)
!
      CALL OV( 'X=CY    ' ,TEXP%ADR(RANKTR4)%P%R,TN%ADR(RANKTR4)%P%R,
     &                     T1%R,-CCSEDIM,NPOIN)
      CALL OV( 'X=X+CY  ' ,TEXP%ADR(RANKTR4)%P%R,TN%ADR(RANKTR4)%P%R,
     &                     T1%R,-KDESORP,NPOIN)
      CALL OV( 'X=X+CYZ ' ,TEXP%ADR(RANKTR4)%P%R,TN%ADR(RANKTR1)%P%R,
     &                     TN%ADR(RANKTR3)%P%R,-CC,NPOIN)
      CALL OVD('X=Y/Z   ' ,T4%R,T3%R,TN%ADR(RANKTR2)%P%R,
     &                     1.D0,NPOIN,2,0.D0,EPS)
      CALL OV( 'X=XY    ' ,T4%R,TN%ADR(RANKTR5)%P%R,
     &                     T1%R,CC,NPOIN)
      CALL OV( 'X=X+CYZ ' ,T4%R,TN%ADR(RANKTR5)%P%R,
     &                     T2%R,-1.0D0,NPOIN)
      CALL OVD('X=X/Y   ' ,T4%R,HPROP%R,
     &                     HPROP%R,0.D0,NPOIN,2,0.D0,EPS)
      CALL OV( 'X=X+Y   ' ,TEXP%ADR(RANKTR4)%P%R,T4%R,
     &                     T4%R,0.D0,NPOIN)
!
!     FIFTH TRACER: ABSORBED POLLUTANT BY BED SEDIMENT [CFF] (RANKTR5)
!
      CALL OV( 'X=CY    ' ,TEXP%ADR(RANKTR5)%P%R,TN%ADR(RANKTR5)%P%R,
     &                     T1%R,-CCSEDIM,NPOIN)
      CALL OV( 'X=X+YZ  ' ,TEXP%ADR(RANKTR5)%P%R,TN%ADR(RANKTR4)%P%R,
     &                     T2%R,0.D0,NPOIN)
      CALL OVD('X=Y/Z   ' ,T4%R,T3%R,TN%ADR(RANKTR2)%P%R,
     &                     1.D0,NPOIN,2,0.D0,EPS)
      CALL OV( 'X=XY    ' ,T4%R,TN%ADR(RANKTR5)%P%R,
     &                     T1%R,CC,NPOIN)
      CALL OV( 'X=X+Y   ' ,TEXP%ADR(RANKTR5)%P%R,T4%R,
     &                     T4%R,0.D0,NPOIN)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
