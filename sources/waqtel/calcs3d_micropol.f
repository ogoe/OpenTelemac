!                    ***************************
                      SUBROUTINE CALCS3D_MICROPOL
!                    **************************
     & (NPOIN3,NPOIN2,NPLAN,NTRAC,TN,TEXP,TIMP,ZPROP,CF,UN,VN,
     &  T2_1,T2_2,T2_3,T3_1,DEBUG)
!
!
!***********************************************************************
! TELEMAC2D   V7P2                                        21/05/2016
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR MICROPOL WAQ PROCESS IN 3D
!          WAQ PROCESS OF CODE_TRACER (MASCARET SYSTEM) 
!
!history  R. ATA
!+        21/05/2016
!+        V7P0
!+       REAL CREATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CCSEDIM        |-->| CONSTANT OF EXPONENTIAL DESINTEGRATION       
!| CDISTRIB       |-->| COEFFICIENT OF DISTRIBUTION (KD)             
!| DEBUG          |-->| IF NE.0 THEN DEBUG MODE
!| DT             |-->| TIME STEP
!| ERO            |-->| EROSION RATE    
!| KDESORP        |-->| KINETIC CONSTANT OF  DESORPTION                     
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES 
!| NTRAC          |-->| NUMBER OF TRACERS
!| TAUB           |-->| BED SHEAR       
!| TAUS           |-->| CRITICAL STRESS OF RESUSPENSION              
!| TAUR           |-->| SEDIMENTATION CRITICAL STRESS
!| TEXP           |<--| EXPLICIT SOURCE TERMS OF TRACERS
!| TIMP           |<--| IMPLICIT SOURCE TERMS OF TRACERS
!| TN             |-->| TRACERS
!| VITCHU         |-->| SEDIMENT SETTLING VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL,ONLY:ERO,TAUR,TAUS,VITCHU,CDISTRIB,
     &                             RO0,KDESORP,CCSEDIM
      USE INTERFACE_WAQTEL, EX_CALCS3D_MICROPOL => CALCS3D_MICROPOL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER          , INTENT(IN   ) :: NPOIN2,NPOIN3,NTRAC
      INTEGER          , INTENT(IN   ) :: NPLAN,DEBUG
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,ZPROP,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,T2_1,T2_2,T2_3,T3_1
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
!     LOCAL VARIABLES
      INTEGER                     :: RANKTR1,RANKTR2,RANKTR3,RANKTR4
      INTEGER                     :: RANKTR5,I
      INTEGER         , PARAMETER :: ADDTR = 5
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION            :: CC
!
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 0'
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
!     INITIALISATION
!
      CALL OS( 'X=0     ',X=TEXP%ADR(RANKTR1)%P)
      CALL OS( 'X=0     ',X=TEXP%ADR(RANKTR2)%P)
      CALL OS( 'X=0     ',X=TEXP%ADR(RANKTR3)%P)
      CALL OS( 'X=0     ',X=TEXP%ADR(RANKTR4)%P)
      CALL OS( 'X=0     ',X=TEXP%ADR(RANKTR5)%P)
!
      CALL OS( 'X=0     ',X=TIMP%ADR(RANKTR3)%P)
      CALL OS( 'X=0     ',X=TIMP%ADR(RANKTR4)%P)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 1'
!
!     BED SHEAR STRESS (TAUB-STOCKED IN T2_1==>2D TABLE)
!    
      CALL TAUB_WAQTEL(CF,RO0,T2_1,NPOIN2,UN,VN)
!
!     DEPOTION PROBABILITY (SED): STOCKED IN T2_2==>2D TABLE
!
      CALL DEPOS_FX(T2_2,T2_1,TN%ADR(RANKTR1)%P,TAUS,VITCHU,NPOIN2)
!
!     EROSION FLUX (RS): STOCKED IN T2_3 ==> 2D TABLE
!
      CALL EROSION_FX(T2_3,T2_1,TN%ADR(RANKTR2)%P,TAUR,ERO,1.D-10,
     &                NPOIN2)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 4'
!
!
!     =======================================
!     LET'S NOW COMPUTE SOURCE TERMS
!     =======================================
!
!     FIRST TRACER: SUSPENDED LOAD [SS] (RANKTR1)
!
!     BED SOURCES
      DO I=1,NPOIN2
        TEXP%ADR(RANKTR1)%P%R(I)=T2_3%R(I)-T2_2%R(I)
      ENDDO
      CALL OVD('X=Y/Z   ',TEXP%ADR(RANKTR1)%P%R,TEXP%ADR(RANKTR1)%P%R,
     &         ZPROP%R,0.D0,NPOIN2,2,0.D0,EPS                          )    
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 5'
!
!     SECOND TRACER: BED SEDIMENT [SF] (RANKTR2)
!      warning: no advection neither diffusion for this tracer
!
      DO I=1,NPOIN2
        TEXP%ADR(RANKTR2)%P%R(I)=T2_2%R(I)-T2_3%R(I)
      ENDDO
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 6'
!
!     THIRD TRACER: POLLUTANT DENSITY [C] (RANKTR3)
!
!     implicit part  
      CALL OS( 'X=C     ' ,X=TIMP%ADR(RANKTR3)%P,C=CCSEDIM             )
!     explicit part  
      CALL OS( 'X=CY    ' ,X=TEXP%ADR(RANKTR3)%P,Y=TN%ADR(RANKTR4)%P,
     &                     C=KDESORP                                   )
      CC =-KDESORP*CDISTRIB
!  warning: the following term causes divergence of the code, it should
!           be traited implicitly- it is commented: to be investigated
!           more in depth
!      CALL OS( 'X=X+CYZ ' ,X=TEXP%ADR(RANKTR3)%P,Y=TN%ADR(RANKTR3)%P,
!     &                     Z=TN%ADR(RANKTR1)%P             ,C=CC       )
!
!
!     FORTH TRACER: ABSORBED POLLUTANT BY SUSPENDED LOAD [CSS] (RANKTR4)
!
!     implicit part
      CALL OS( 'X=C     ' ,X=TIMP%ADR(RANKTR4)%P,C=CCSEDIM             )
!     explicit part
      CALL OS( 'X=-Y    ' ,X=TEXP%ADR(RANKTR4)%P,Y=TEXP%ADR(RANKTR3)%P )
      DO I=1,NPOIN2
        T3_1%R(I)=T2_3%R(I)*TN%ADR(RANKTR5)%P%R(I)-
     &            T2_2%R(I)*TN%ADR(RANKTR4)%P%R(I)
      ENDDO
      CALL OVD('X=Y/Z   ' ,T3_1%R,T3_1%R,
     &                     ZPROP%R,0.D0,NPOIN2,2,0.D0,EPS              )
      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(RANKTR4)%P,Y=T3_1                )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 8'
!
!     FIFTH TRACER: ABSORBED POLLUTANT BY BED SEDIMENT [CFF] (RANKTR5)
!
      DO I=1,NPOIN2
        TEXP%ADR(RANKTR5)%P%R(I)=T2_2%R(I)*TN%ADR(RANKTR4)%P%R(I)-
     &                           T2_3%R(I)*TN%ADR(RANKTR5)%P%R(I)
      ENDDO
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(RANKTR5)%P,Y=TN%ADR(RANKTR5)%P,
     &                     C=-CCSEDIM                                  )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN MICROPOL3D, STEP 9'
!    
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS 
!                   (IMPLICIT PART IS ADDED IN CVDFTR)
!
!     ACTIVATE BIEF_OBJ FOR FURTHER CALCULATIONS
!
      TEXP%ADR(RANKTR1)%P%TYPR='Q'
      TEXP%ADR(RANKTR2)%P%TYPR='Q'
      TEXP%ADR(RANKTR3)%P%TYPR='Q'
      TEXP%ADR(RANKTR4)%P%TYPR='Q'
      TEXP%ADR(RANKTR5)%P%TYPR='Q'
!
      TIMP%ADR(RANKTR3)%P%TYPR='Q'
      TIMP%ADR(RANKTR4)%P%TYPR='Q'

!      
!-----------------------------------------------------------------------
!
      RETURN
      END
