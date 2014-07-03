!                    ******************
                     SUBROUTINE SET_DIF 
!                    ****************** 
! 
     &(FC,VOLU2D,MESH3D,NPOIN2,NPOIN3,DT,FLUX,NPLAN, 
     & WCC,FLUDPT,FLUDP,FLUER,IPBOT,VISCTA) 
! 
!*********************************************************************** 
! TELEMAC3D   V7P0                                   21/08/2010 
!*********************************************************************** 
! 
!brief    ADVECTION OF A VARIABLE WITH AN UPWIND FINITE 
!+                VOLUME SCHEME. 
!+ 
!+           (THE ADVECTION IS DONE EDGE BY EDGE, WHICH ENABLES 
!+                LOCAL DEPTHS EQUAL TO ZERO). 
! 
!warning  HERE FLUXES IN FLODEL ARE FROM POINT 2 TO POINT 1. 
!+ 
!+        SEE FLUX3D (HORIZONTAL FLUXES BASED ON FLUINT) 
!+            AND PRECON (VERTICAL FLUXES BASED ON WSCONV) 
! 
!history  C. VILLARET & T. BENSON (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments for sediment, merged on 25/02/2014.
! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!| DT             |-->| TIME STEP 
!| FC             |<->| VARIABLE AFTER CONVECTION 
!| FLUER          |-->| EROSION FLUX (SEDIMENT) 
!| FLUDPT         |-->| DEPOSITION FLUX - IMPLICIT PART (SEDIMENT) 
!| FLUDP          |-->| DEPOSITION FLUX  (SEDIMENT) 
!| FLUX           |<->| GLOBAL FLUXES TO BE CHANGED 
!| IPBOT          |-->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE) 
!| MESH3          |<->| 3D MESH 
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS 
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D 
!| NPOIN3         |-->| NUMBER OF 3D POINTS 
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS IN 2D
!| WCC            |-->| SETTLING VELOCITY (SEDIMENT) 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      USE BIEF 
      USE DECLARATIONS_TELEMAC 
      USE DECLARATIONS_TELEMAC3D, ONLY: ITURBV,NLAYMAX 
! 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER, INTENT(IN)             :: NPOIN3,NPOIN2,NPLAN       
      INTEGER, INTENT(IN)             :: IPBOT(NPOIN2) 
! 
      DOUBLE PRECISION, INTENT(INOUT) :: FC(NPOIN3) 
! 
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX 
      DOUBLE PRECISION, INTENT(IN)    :: DT 
! 
      TYPE(BIEF_OBJ), INTENT(IN)      :: WCC,FLUDPT,VOLU2D 
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUDP,FLUER 
      TYPE(BIEF_OBJ), INTENT(IN)      :: VISCTA 
! 
      TYPE(BIEF_MESH), INTENT(IN)     :: MESH3D 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER IPOIN,I,I1,I2,IPLAN 
! 
      DOUBLE PRECISION :: FLUSET,FLUERO,FLUDEP,FLUNET 
      DOUBLE PRECISION :: DCDZ,FLUDIF, Z1,Z2,NUT,FCMASS 
      DOUBLE PRECISION :: SETLOSS(NLAYMAX),DIFLOSS(NLAYMAX) 
      DOUBLE PRECISION :: TOTLOSS
! 
!     CV vertical grid finite volume SCHEME   
! 
      DOUBLE PRECISION :: DZ(NLAYMAX),VOL(NLAYMAX) 
! 
      DOUBLE PRECISION EPS 
      DATA EPS /1.D-6/ 
! 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! 
! NOTE 1: Work from bottom layer up, compute settling flux, and take  
!         material from the layer above. Use volumes (in VOLU2) to effect  
!         correct mass balance.  
! NOTE 2: the warning above regarding VOLU2 has not been addressed yet 
!         so if there are sources or exiting fluxes at nodes there could
!         be a problem. 
!  
      DO IPOIN = 1,NPOIN2   ! we loop through horizontal just once 
! 
!       TIDAL FLATS 
! 
        IF(IPBOT(IPOIN).EQ.NPLAN-1) THEN 
          FLUER%R(IPOIN) = 0.D0 
          FLUDP%R(IPOIN) = 0.D0 
!         SKIP THIS NODE 
          CYCLE                  
        ENDIF   
!            
!       Changes in concentration 
!
!       NOTE JMH: SETLOSS REDONE AFTER, SO NO USE ???
        DO IPLAN=1,NPLAN 
          SETLOSS(IPLAN)=0.D0 
          DIFLOSS(IPLAN)=0.D0  
        ENDDO 
!           
!       FINITE VOLUME SCHEME: Vertical grid definition           
! 
        DO IPLAN = 1,NPLAN-1 
          I1 = IPOIN + (IPLAN-1)*NPOIN2 
          I2 = IPOIN + IPLAN*NPOIN2 
          Z1=MESH3D%Z%R(I1) 
          Z2=MESH3D%Z%R(I2) 
          DZ(IPLAN)= Z2-Z1 
!         test JMH
!         DZ(IPLAN)=MAX(DZ(IPLAN),EPS)
!         end of test JMH
          IF(IPLAN.EQ.1) THEN  
            VOL(1)=DZ(1)*0.5D0 
          ELSE 
            VOL(IPLAN)= (DZ(IPLAN)+ DZ(IPLAN-1))*0.5D0 
          ENDIF   
        ENDDO
        VOL(NPLAN)=DZ(NPLAN-1)*0.5D0
! 
!       BOTTOM POINT (IPLAN = 1) 
! 
!       NET EROSION - DEPOSITION FLUX 
! 
        FLUERO = FLUER%R(IPOIN)*DT 
        FLUDEP = FLUDPT%R(IPOIN)*FC(IPOIN)*DT 
!           
!       apply net erosion/deposition immediately (before settling and diffusion) 
!       NOTE: SETLOSS(1) could be set to FLUDEP...and FC updated at the end 
!       it gives the same answer for steady state (I tried it) n.b. if 
!       this is done then FLUDP needs to be set later (see below) 
        SETLOSS(1)= FLUDEP 
! 
        DO IPLAN = 1,NPLAN-1 
!               
!         ### SETTLING ### 
! 
          I1 = IPOIN + (IPLAN-1)*NPOIN2 
          I2 = IPOIN + IPLAN*NPOIN2 
!           
!         Positive (downward) settling flux --> FLUSET > 0 
!         WCC >0 (6.3) 
          FLUSET = WCC%R(I2)*FC(I2)*DT 
!             
!         not here! We need correct proportions of potential up/down flux 
!         so it gets done further down              
!         apply to settling loss array
! 
          SETLOSS(IPLAN+1)=FLUSET 
!            
!         ### DIFFUSIVITY ###        
!            
!         Positive (upwards) Diffusion flux ---> FLUDIF >0 
!            
!         First calculate the concentration gradient 
          IF(IPLAN.EQ.1) THEN       
            DCDZ=(LOG(MAX(FC(I2),EPS))-LOG(MAX(FC(I1),EPS))) 
     &          *(FC(I1)+FC(I2))/2.0/DZ(IPLAN)  
          ELSE 
            DCDZ=(FC(I2)-FC(I1))/DZ(IPLAN)             
          ENDIF 
!         viscosity (for mixing length it is already calculated at mid point) 
          IF(ITURBV.EQ.2) THEN 
            NUT=VISCTA%R(I1) 
          ELSE   
!           K-EPS  viscosity is calculated at each node 
            NUT=(VISCTA%R(I1)+ VISCTA%R(I2))*0.5D0 
          ENDIF
! 
!         (Diffusion flux)*DT=Mass exchange 
!
          FLUDIF = -NUT*DCDZ*DT   
! 
          IF(FLUDIF.GT.0.D0) THEN 
!           if positive then add it to diffusion loss of lower plane (iplan)
            DIFLOSS(IPLAN)=FLUDIF 
!           if negative add it to settling loss of upper plane (iplan+1) 
          ELSE  
            SETLOSS(IPLAN+1)=SETLOSS(IPLAN+1)-FLUDIF 
          ENDIF 
!             
        ENDDO ! end of layer loop 
!            
!       Now adjust the fluxes depending on the available mass in each plane 
!
        DO IPLAN=1,NPLAN 
          I = IPOIN + (IPLAN-1)*NPOIN2    
! 
          FCMASS=FC(I)*VOL(IPLAN) 
!              
!         IF (FCMASS>1.e-9) FCMASS=FCMASS*0.5d0 
!         combine the losses and gains for settling/depo and diffusion/ero 
          TOTLOSS=SETLOSS(IPLAN)+DIFLOSS(IPLAN) 
!         modify the settling and diffusion proportionaly 
          IF(TOTLOSS.GT.FCMASS) THEN 
            !write(*,*) 'Warning: Limiting mass exchange!' 
            ! Perhaps we need to look at the concentrations  
            ! in planes above and below then 
            ! make sure we don't fall below the average of the two 
            ! (or just one if at surface or bed) 
            ! although tricky because the concentrations have changed 
            ! QUESTION: can we do subiterations somehow? 
!           MAX ADDED BY JMH ON 28/02/2014 (CRASH ON NAG COMPILER)
!           EVEN > FCMASS, TOTLOSS CAN BE VERY SMALL
            SETLOSS(IPLAN)=FCMASS*SETLOSS(IPLAN)/MAX(TOTLOSS,1.D-20) 
            DIFLOSS(IPLAN)=FCMASS-SETLOSS(IPLAN) 
          ENDIF 
        ENDDO            
! 
!       These 2 lines should be uncommented if SETLOSS(1)=FLUDEP above
! 
        FLUDEP=SETLOSS(1) 
        FLUNET=FLUERO - FLUDEP  
! 
!       Record the net deposition flux onto the bed  
!       (FLUDP > 0 => net increase of bed sediment) 
!          
        FLUDP%R(IPOIN) = FLUDEP/DT 
!              
!       Update the flux summation out of the domain  
! 
        FLUX = FLUX - FLUNET*VOLU2D%R(IPOIN) 
! 
!       Now simply pass all the masses between the planes using the up/down
!       fluxes to give the final concentration 
! 
        I = IPOIN         
!       mass in bottom plane 
        FCMASS=FC(I)*VOL(1) 
!       add erosion to bottom plane (if using SETLOSS(1)=FLUDEP above) 
        FCMASS = FCMASS + (FLUERO - FLUDEP) 
!       contribution to/from plane above 
        FCMASS = FCMASS+(SETLOSS(2)-DIFLOSS(1)) 
!       convert back to concentration 
!          
        FC(I)=FCMASS/VOL(1) 
!       now apply the fluxes to internal 
        DO IPLAN = 2,NPLAN-1 
          I = IPOIN + (IPLAN-1)*NPOIN2   
          FCMASS=FC(I)*VOL(IPLAN)  
!         contributions to/from top plane 
          FCMASS = FCMASS+(SETLOSS(IPLAN+1)-DIFLOSS(IPLAN)) 
!         contributions to/from bottom plane 
          FCMASS = FCMASS+(DIFLOSS(IPLAN-1)-SETLOSS(IPLAN) ) 
!         convert back to conc 
          FC(I)=FCMASS/VOL(IPLAN)  
        ENDDO        
!       apply fluxes to top plane 
        I = IPOIN + (NPLAN-1)*NPOIN2    
! 
        FCMASS=FC(I)*VOL(NPLAN) 
! 
!       contribution to/from plane below 
        FCMASS = FCMASS + (DIFLOSS(NPLAN-1)-SETLOSS(NPLAN)) 
!       convert back to conc 
! 
        FC(I)=FCMASS/VOL(NPLAN) 
!  
! add on erosion at the end      
! 
      ENDDO  ! END OF NODE LOOP 
! 
!----------------------------------------------------------------------- 
! 
      RETURN 
      END
