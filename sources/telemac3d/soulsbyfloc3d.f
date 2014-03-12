!                    ************************ 
                     SUBROUTINE SOULSBYFLOC3D 
!                    ************************ 
! 
     &(WCHU, FC, MESH3, NPOIN2, NPOIN3, NPLAN, HN, HMIN, UETCAR) 
! 
!*********************************************************************** 
! TELEMAC3D   V7P0                                   29/11/2011 
!*********************************************************************** 
! 
!brief  COMPUTES THE FALL VELOCITY of mud flocs based on R. Soulsby's 
!+      latest, 2011, formulation taken from a method based on 
!+      Manning's floc database. 
! 
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!| WC             |<--| SPATIALLY VARYING FALL VELOCITY 
!| MESH3          |-->| 3D MESH 
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH 
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D MESH (=NPOIN2*NPLAN) 
!| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES 
!| HN             |-->| WATER DEPTH 
!| HMIN           |-->| MINIMUM WATER DEPTH FOR SETTLING 
!| TOB            |-->| BED SHEAR STRESS (size NPOIN2) 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      USE BIEF 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+  
!     
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WCHU 
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH3 
      TYPE(BIEF_OBJ), INTENT(IN)    :: HN 
      INTEGER, INTENT(IN)           :: NPOIN2,NPOIN3,NPLAN 
      DOUBLE PRECISION, INTENT(IN)  :: UETCAR(NPOIN2),FC(NPOIN3) 
      DOUBLE PRECISION, INTENT(IN)  :: HMIN 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
!  
      INTEGER                         :: IPOIN,IP,IPLAN 
      DOUBLE PRECISION                :: DENS,USTAR,EPS,DI4,DM4 
      DOUBLE PRECISION                :: WS_MIC,WS_MAC,RR,DLC,BIGX 
      DOUBLE PRECISION                :: EXP1,NU_WCUB,ETA,Z 
 
!     CONSTANTS: 
      DOUBLE PRECISION, PARAMETER     :: KAPPA=0.4D0 
      DOUBLE PRECISION, PARAMETER     :: RHO_FIX=1000.D0, NU_W=1.004E-6 
 
!     CALIBRATED COEFFS: 
!     MICRO: 
      DOUBLE PRECISION, PARAMETER     :: USTARSMIC=0.025D0 
      DOUBLE PRECISION, PARAMETER     :: BMIC=0.363D0, LITN=0.66D0 
 
!     MACRO: 
      DOUBLE PRECISION, PARAMETER     :: USTARSMAC=0.067D0 
      DOUBLE PRECISION, PARAMETER     :: BMAC=0.860D0, LITK=0.0825D0 
      DOUBLE PRECISION, PARAMETER     :: DENS2=1.15D0, BIGN=0.463D0 
!
!----------------------------------------------------------------------- 
! 
!     COMPUTE FALL VELOCITY SO THAT IT IS A FUNCTION OF SUSPENDED CONC 
!     (AT PREVIOUS TIME STEP) 
!     USING THE FORMULA DERIVED BY RICHARD SOULSBY (2011) AT HRW.  
! 
!     RELATIVE WATER SEDIMENT DENSITY
!     (HARD CODED FOR NOW AFTER SOULSBY [2011])  
      DENS    = 2.64D0 
! 
!     OPTIMISE VARIABLES WHERE POSSIBLE (OUTSIDE LOOP) 
      NU_WCUB = NU_W**3      ! KINEMATIC VISCOCITY OF WATER CUBED 
      DI4     = 1.D-20       ! di^4 
      DM4     = 1.D-16       !(d_micro)^4 
      EXP1    = LITK*2.672D0 
! 
      DO IPOIN = 1,NPOIN2 
! 
!     2D COUNTER FOR ARRAY VARIABLES OF SIZE NPOIN2 
!      II=II+1 
      ! reset for 2d arrays 
!      IF (II>NPOIN2) II=1 
!          
!     CV: Direct from TMAC - NB do not use since not damped, use TOB       
      USTAR  = SQRT(UETCAR(IPOIN)) 
!     USTAR  = MAX((SQRT(TOB(IPOIN)/RHO_FIX)),1.D-25) !EQ. (32) Soulsby (1997) 
!       
      DO IPLAN=1,NPLAN 
!           
      ! 3D NODE INDEX 
      IP = IPOIN+NPOIN2*(IPLAN-1) 
!       
      ! limit settling in very shallow water 
      IF(HN%R(IPOIN).LT.0.01D0) THEN 
        WCHU%R(IP) = 0.0002D0 
      ELSE 
!           
!     COMPUTE DIMENSIONLESS CONCENTRATION IN SAME WAY AS SOULSBY 
!     (I.E USING WATER DENSITY) 
!     NOTE THAT SEDIMENT MUST BE THE LAST TRACER 
!     (I.E. THE NTRAC ADDRESS IN THE BIEF OBJECT TA) 
! 
!     NOTE ALSO THAT UNITS OF TRACERS ARE WHATEVER ARE INPUTTED 
!     AS INITIAL CONDITIONS I.E. 
!     TRACERS ARE TREATED AS 'DIMLESS' WITHIN THE CODE.  
!     WE NEED MASS CONC OF SEDIMENT PER UNIT MASS OF WATER E.G: 
! 
!     DLC = TA%ADR(NTRAC)%P%R(I)/RHO_FIX 
      DLC = FC(IP)/RHO_FIX 
! 
!     NB: AS UNITS OF RHO_FIX ARE KG/M3 IT IS IMPORTANT THAT THE SED 
!         CONC IS IN KG/M3 
!    
!     calculate the height of the cell centre for top and bottom layers 
      IF (IPLAN.EQ.1) THEN ! 25% of distance from bed to second layer 
        Z = (MESH3%Z%R(IP+NPOIN2)-MESH3%Z%R(IP))*0.25  
      ELSEIF (IPLAN.EQ.NPLAN) THEN ! 25% of distance from surface to second to last layer 
        Z = (MESH3%Z%R(IP)-MESH3%Z%R(IP-NPOIN2))*0.75 +  
     &       MESH3%Z%R(IP-NPOIN2) - MESH3%Z%R(IPOIN) 
      ELSE ! cell centres are on the planes for all other vertical cells 
        Z = MESH3%Z%R(IP) - MESH3%Z%R(IPOIN) 
      ENDIF 
!      
!     FIND ETA & PREVENT FAILURE IN SHALLOW WATERS (in case HMIN<0.01) 
      IF(HN%R(IPOIN).GE.HMIN) THEN 
        ETA = 1.D0-Z/HN%R(IPOIN) 
      ELSE 
        ETA = 0.5D0 
      ENDIF 
! 
! calculate epsilon (could replace with epsilon if using k-e model) 
! n.b. since we are using cell centre heights then Z should never be zero  
! (we have used min depth of 0.01 above to prevent problems with zero depth) 
!
      EPS    = USTAR**3*ETA/(KAPPA*Z) 
!
!     MICROFLOCS: 
      WS_MIC = BMIC*(DENS-1.D0)*((EPS*DI4/NU_WCUB)**0.39D0)   *  
     &         9.81D0            *SQRT(NU_W/EPS)              * 
     &         EXP(-(USTARSMIC/(USTAR*SQRT(ETA)))**LITN) 
! 
!     MACROFLOCS: 
      WS_MAC = BMAC*(DENS2-1.D0)*((EPS*DM4/NU_WCUB)**0.166D0) * 
     &         9.81D0*(DLC**EXP1)*SQRT(NU_W/EPS)              * 
     &         EXP(-(USTARSMAC/(USTAR*SQRT(ETA)))**BIGN) 
! 
! Second set of equations which substitute epsilon (but keep above) 
!     MICROFLOCS: 
!        WS_MIC = 0.5372D0*((USTAR^3*ETA*DI4)/(NU_WCUB*Z))**0.39D0 *  
!                 9.81D0                  * SQRT((NU_W*Z)/(USTAR**3*ETA)) * 
!                 exp(-(USTARSMIC/(USTAR*sqrt(ETA)))**0.266);   
!                      
!     MACROFLOCS: 
!        WS_MAC = 0.095D0*((USTAR**3*ETA*DM4)/(NU_WCUB*Zi))**0.166D0 *  
!     &           9.81D0*(DLC**0.22044D0) * sqrt((NU_W*Zi)/(USTAR**3*ETA)) * 
!     &           EXP(-(USTARSMAC/(USTAR*sqrt(ETA)))**0.463); 
!                  
      BIGX = LOG10(DLC)+6.D0 
!               
      IF(BIGX.LT.0.D0) THEN 
        RR=0.1D0 
      ELSEIF ((BIGX.GE.0.D0).AND.(BIGX.LT.4.07D0)) THEN 
        RR=0.1D0+0.221D0*BIGX 
      ELSE 
        RR=1.D0 
      ENDIF 
!       
!     GET WEIGHTED AVERAGE (BASED ON RR) TO PROVIDE THE EFFECTIVE
!     SETTLING VELOCITY: 
      WCHU%R(IP) = MAX(WS_MAC*RR+(1.D0-RR)*WS_MIC,0.0002D0) 
! 
      ENDIF 
!       
      END DO ! end of layer loop 
      END DO ! end of 2d node loop 
!
!----------------------------------------------------------------------- 
!       
      RETURN 
      END
 
