!                    ***************************
                      SUBROUTINE CALCS2D_MICROPOL
!                    **************************
     & (NPOIN,NTRAC,TN,TEXP,TIMP,HN,HPROP,CF,UN,VN,
     &  T1,T2,T3,T4,DT,VOLU2D,MASSOU)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CCSEDIM        |-->| CONSTANT OF EXPONENTIAL DESINTEGRATION       
!| CDISTRIB       |-->| COEFFICIENT OF DISTRIBUTION (KD)             
!| DT             |-->| TIME STEP
!| ERO            |-->| EROSION RATE    
!| KDESORP        |-->| KINETIC CONSTANT OF  DESORPTION   
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM                     
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES 
!| NTRAC          |-->| NUMBER OF TRACERS
!| TAUB           |-->| BED SHEAR       
!| TAUS           |-->| CRITICAL STRESS OF RESUSPENSION              
!| TAUR           |-->| SEDIMENTATION CRITICAL STRESS
!| TEXP           |<--| EXPLICIT SOURCE TERMS OF TRACERS
!| TIMP           |<--| IMPLICIT SOURCE TERMS OF TRACERS
!| TN             |-->| TRACERS
!| VITCHU         |-->| SEDIMENT SETTLING VELOCITY    
!| VOLU2D         |-->| BASE AREA (NOT ASSEMBLED)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL,ONLY:ERO,TAUR,TAUS,VITCHU,CDISTRIB,
     &                             RO0,KDESORP,CCSEDIM
      USE INTERFACE_WAQTEL, EX_CALCS2D_MICROPOL => CALCS2D_MICROPOL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
!
      INTEGER          , INTENT(IN   ) :: NPOIN,NTRAC
      DOUBLE PRECISION , INTENT(IN   ) :: DT
      DOUBLE PRECISION , INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,CF,HN,UN,VN,VOLU2D
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,T1,T2,T3,T4,TIMP
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
!     LOCAL VARIABLES
      INTEGER                     :: RANKTR1,RANKTR2,RANKTR3,RANKTR4
      INTEGER                     :: RANKTR5,I,J,ITRAC
      INTEGER         , PARAMETER :: ADDTR = 5
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION            :: CC
!
!-----------------------------------------------------------------------
!
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
      CALL TAUB_WAQTEL(CF,RO0,T1,NPOIN,UN,VN)
!
!     DEPOTION PROBABILITY (SED): STOCKED IN T2
!
      CALL DEPOS_FX(T2,T1,TN%ADR(RANKTR1)%P,TAUS,VITCHU,NPOIN)
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
!     implicit part  
      CALL OS( 'X=C     ' ,X=TIMP%ADR(RANKTR3)%P,C=CCSEDIM             )
!     explicit part  
      CALL OS( 'X=CY    ' ,X=TEXP%ADR(RANKTR3)%P,Y=TN%ADR(RANKTR4)%P,
     &                     C=KDESORP                                   )
      CC =-KDESORP*CDISTRIB
      CALL OS( 'X=X+CYZ ' ,X=TEXP%ADR(RANKTR3)%P,Y=TN%ADR(RANKTR3)%P,
     &                     Z=TN%ADR(RANKTR1)%P             ,C=CC       )
!
!     FORTH TRACER: ABSORBED POLLUTANT BY SUSPENDED LOAD [CSS] (RANKTR4)
!
!     implicit part
      CALL OS( 'X=C     ' ,X=TIMP%ADR(RANKTR4)%P,C=CCSEDIM             )
!     explicit part
      CALL OS( 'X=-Y    ' ,X=TEXP%ADR(RANKTR4)%P,Y=TEXP%ADR(RANKTR3)%P )
      CALL OS( 'X=YZ    ' ,X=T4,Y=T3,Z=TN%ADR(RANKTR5)%P               )
      CALL OS( 'X=X+CYZ ' ,X=T4,Y=TN%ADR(RANKTR4)%P,Z=T2,C=-1.D0       )
      CALL OVD('X=Y/Z   ' ,T4%R,T4%R,
     &                     HPROP%R,0.D0,NPOIN,2,0.D0,EPS)
      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(RANKTR4)%P,Y=T4                  )
!
!     FIFTH TRACER: ABSORBED POLLUTANT BY BED SEDIMENT [CFF] (RANKTR5)
!
      CALL OS( 'X=YZ    ' ,X=TEXP%ADR(RANKTR5)%P,Y=TN%ADR(RANKTR4)%P,
     &                     Z=T2                                        )
      CALL OS( 'X=X+CYZ ' ,X=TEXP%ADR(RANKTR5)%P,Y=TN%ADR(RANKTR5)%P,
     &                     Z=T3,C=-1.D0                                )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(RANKTR5)%P,Y=TN%ADR(RANKTR5)%P,
     &                     C=-CCSEDIM                                  )
!
!    
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS 
!                   (IMPLICIT PART IS ADDED IN CVDFTR)
!
       DO J=1,ADDTR
         ITRAC=NTRAC-J+1
         MASSOU(ITRAC) = 0.D0
         DO I=1,NPOIN
           MASSOU(ITRAC)= MASSOU(ITRAC)
     &                  + HN%R(I)*TEXP%ADR(ITRAC)%P%R(I)*VOLU2D%R(I)
         ENDDO
         MASSOU(ITRAC)=MASSOU(ITRAC)*DT
         IF(NCSIZE.GT.0) MASSOU(ITRAC)=P_DSUM(MASSOU(ITRAC))
       ENDDO
!      
!-----------------------------------------------------------------------
!
      RETURN
      END
