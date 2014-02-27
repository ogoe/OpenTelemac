! ZR not used anymore
! ZF_S et ESOMT added
! CALCUL DE MASDEP : MASSE TOTALE DEPOSE 
!                   *****************
                     SUBROUTINE FONVAS
!                    *****************
!
     &(IVIDE  , EPAI   , CONC  , TREST  , TEMP   , HDEP  ,
     & FLUDP, FLUDPT ,FLUER, ZF    , TA     , WC     , TRA01 ,
     & TRA02  , TRA03  , NPOIN2, NPOIN3 , NPFMAX , NCOUCH,
     & NPF    , LT     , DT     , DTC   , GRAV   , RHOS  ,
     & CFMAX, TASSE  , ITASS, 
     & ZF_S, ESOMT,  VOLU2D, MASDEP, SETDEP)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    MODELS THE MUD BED EVOLUTION.
!
!history  C LE NORMANT (LNH)
!+        13/05/92
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CFDEP          |-->| CONCENTRATION OF MUD DEPOSIT (G/L)
!| CFMAX          |<->| CONCENTRATION OF CONSOLIDATED MUD (G/L)
!| CONC           |-->| MUD BED LAYER CONCENTRATION
!|                |   | (MULTILAYER MODEL)
!| DT             |-->| HYDRAULIC TIME STEP
!| DTC            |-->| TIME STEP FOR CONSOLIDATION PHENOMENON
!| EPAI           |<->| THICKNESS OF SOLID FRACTION OF THE BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS
!| EPAI0          |<->| REFERENCE THICKNESS TP CREATE NEW ELEMENTS
!|    SETDEP      |-->| CHOICE OF CONVECTION SCHEME FOR VERTICAL SETTLING
!| GIBSON         |-->| LOGICAL FOR GIBSON SETTLING MODEL
!| GRAV           |-->| GRAVITY ACCELERATION
!| HDEP           |<->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| IVIDE          |<->| VOID RATIO
!|                |   | (GIBSON MODEL ONLY)
!| LT             |-->| CURRENT TIME STEP NUMBER
!| MASDEP         |<->| DEPOSITED MASS
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (GIBSON MULTILAYER SETTLING MODEL)
!| NPF            |-->| NUMBER OF POINTS OF THE BOTTOM ON ONE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES
!|                |   | DISCRETIZATION OF MUD BED (GIBSON MODEL)
!| NPOIN2         |-->| NUMBER OF POINTS  (2D MESH)
!| NPOIN3         |-->| NUMBER OF POINTS  (3D MESH)
!| PDEPOT         |<->| PROBABILITY OF DEPOSIT
!| RHOS           |-->| SEDIMENT DENSITY
!| TA             |-->| ACTIVE TRACOR
!| TASSE          |-->| MULTILAYER SETTLING MODEL LOGICAL
!| TEMP           |<->| TIME COUNTER FOR CONSOLIDATION MODEL
!|                |   | (MULTILAYER MODEL)
!| TRA01          |<->| WORK ARRAY
!| TRA02          |<->| WORK ARRAY
!| TRA03          |<->| WORK ARRAY
!| TREST          |<->| CONSOLIDATION TIME SCALE
!|                |   | (ONLY FOR MULTILAYER MODEL)
!| VOLU2D         |-->|  INTEGRAL OF TEST FUNCTIONS IN 2D (SURFACE OF ELEMENTS)  
!| WC             |-->| SETTLING VELOCITY
!| ZF             |<->| BOTTOM ELEVATION
!| ZR             |-->| RIGID BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_FONVAS => FONVAS
      USE DECLARATIONS_TELEMAC3D, ONLY : IPBOT,OPTBAN,NPLAN,CGEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) ::  LT, NPOIN2, NPOIN3
      INTEGER, INTENT(IN) ::  NCOUCH,NPFMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPOIN2,NCOUCH+1)
      DOUBLE PRECISION, INTENT(INOUT) :: TREST(NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: TEMP(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPOIN2,NCOUCH)
!
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TA(NPOIN3), WC(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPFMAX,6)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPFMAX),TRA03(NPFMAX)
      DOUBLE PRECISION, INTENT(IN)    :: FLUDPT(NPOIN2), FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUDP(NPOIN2) , ZF_S(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ESOMT(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: DT, RHOS, GRAV, DTC
      DOUBLE PRECISION, INTENT(INOUT) ::  CFMAX 
      DOUBLE PRECISION, INTENT(INOUT) :: MASDEP
!
      INTEGER, INTENT(INOUT) ::  NPF(NPOIN2)
!
      LOGICAL, INTENT(IN) :: TASSE
      INTEGER, INTENT(IN) :: SETDEP
      INTEGER, INTENT(IN) :: ITASS
!
      TYPE(BIEF_OBJ), INTENT(IN) :: VOLU2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION C, TOTMASS, QERODE, QS,DELTAF, FLUX, SEDBED
      INTEGER IPOIN, IC
      INTRINSIC MOD
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!=======================================================================
! FIRST STEP
!     COMPUTES THE DEPOSITED QUANTITY (IN MATERIAL COORDINATES)
!     CFDEP=CONC(NCOUCH)
!     COMPUTES THE ERODED QUANTITY 
!=======================================================================
!
! Multi layer model      
! +++++++++++++++++++
! deposition in the first top layer
! calculate the layers thicknesses and deposited thicknes:  HDEP = sum ( EPAI)
!
!       
      FLUX=0.D0
!
!Schema explicite (EXPTSED) FLUDP calcule dans MURD3D_POS
!
!Schema implicite  CONSTRUCTION OF FLUDP:  
! correction for tidal flats: take the first point above crushed planes
! IPBOT =0  :  no tidal flats
! Ipbot = nplan-1 : dry element
! CV 01/2014 ....
!
      IF(SETDEP.EQ.0) THEN
        IF(OPTBAN.EQ.1) THEN
          DO IPOIN=1,NPOIN2
            IF(IPBOT%I(IPOIN).NE.NPLAN-1) THEN
              FLUDP(IPOIN)= FLUDPT(IPOIN)*
     &                TA(IPBOT%I(IPOIN)*NPOIN2+IPOIN)
            ELSE
              FLUDP(IPOIN)=0.D0
            ENDIF  
          ENDDO
        ELSE
          DO IPOIN=1,NPOIN2
            FLUDP(IPOIN)= FLUDPT(IPOIN)*TA(IPOIN)
          ENDDO     
        ENDIF
      ENDIF    
!
!     BED EVOLUTION
!
      DO IPOIN=1,NPOIN2
!
        DELTAF=FLUDP(IPOIN) -FLUER(IPOIN)
        FLUX=FLUX + DELTAF*VOLU2D%R(IPOIN)          
!
!
!!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Uniform bed and non cohesif (NCOUCH = 1) 
! simulatenous treatment of erosion and deposition flux
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 
!         IF(NCOUCH.EQ.1) THEN  
!            ZF_S(IPOIN)=   DELTAF*DT/CONC(IPOIN,1)
!            HDEP(IPOIN)=   HDEP(IPOIN)+ZF_S(IPOIN)
!            EPAI(IPOIN,1)= HDEP(IPOIN)
!            
! HDEP< 0. 
! ---> message error
!         ELSE

! Erosiion first   
! modification of layer thickness due to erosion is now done here (not ERODC)
! Recalculate the erosion of layers by adding up the mass in each layer
!
            TOTMASS = 0.D0
            QERODE  = FLUER(IPOIN)*DT
!
            DO IC=1,NCOUCH
!                   
                QS = CONC(IPOIN,IC)*EPAI(IPOIN,IC)
!               
                TOTMASS = TOTMASS + QS
!               check if we have eroded enough entire layers
                IF(TOTMASS.LT.QERODE) THEN
                  EPAI(IPOIN,IC) = 0.D0
                ELSE
!
                    ! we have got to the correct layer. 
                    ! How much of it do we need to erode?
                    QS = TOTMASS - QERODE
! 
                    ! calculate new thickness
                    EPAI(IPOIN,IC) = QS/CONC(IPOIN,IC)
!
                    ! jump out of layer loop
                    GOTO 10
                ENDIF     
! Check : if IC=1 and all layers are empty 
! -> add a message error
!
            ENDDO
!
10      CONTINUE
! 
!Then Deposition in Top layer
!  
          EPAI(IPOIN,1)=EPAI(IPOIN,1)+
     &         FLUDP(IPOIN)*DT/CONC(IPOIN,1)
!
! Recalucate the sediment bed thickness
! 
         SEDBED= 0.D0
         DO IC=1,NCOUCH
           SEDBED =  SEDBED+EPAI(IPOIN,IC)
         ENDDO
!         
! BED  EVOLUTION 
! AND BED THICKNESS : HDEP
!
        ZF_S(IPOIN)=  SEDBED -HDEP(IPOIN)
        HDEP(IPOIN) = SEDBED
!
!        ENDIF
!  
       ENDDO
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 
!     UPDATE THE CUMULATED BED EVOLUTION : ESOMT
!     BOTTOM ELEVATION : ZF 
!                 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      CALL OV( 'X=Y+Z   ' , ESOMT,ESOMT, ZF_S, C, NPOIN2)
      CALL OV( 'X=Y+Z   ' , ZF, ZF, ZF_S, C, NPOIN2)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
! TASSEMENT HERE
! ---> add the other models here
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!      IF (TASS) THEN
!
!     -----CHECKS THE TIMESTEP TO TAKE CONSOLIDATION-----
!     -----              INTO ACCOUNT               -----
!
!         IF(ITASS.EQ.2)
!         
!         IF (MOD(LT*DT,DTC).LT.1.D-8) THEN
!
!     -----MANAGES THE DEPOSITED QUANTITY : CREATES-----
!     -----     NEW LAYERS NEAR THE MUDDY BED      -----
!
!            CALL GESTDP( IVIDE  , EPAI   , HDEP    ,
!     &                  NPOIN2 , NPFMAX , NPF     ,
!     &                  EPAI0  , CONC(1)  , RHOS    )
!
!     -----CONSOLIDATES THE MUDDY BED-----
!     -----    (GIBSON EQUATION)     -----
!
!           CALL TASSEM( IVIDE , EPAI ,
!     &                  NPOIN2, NPFMAX, NPF  ,
!     &                  GRAV  , RHOS  , DTC  , CFMAX ,
!     &                  TRA01 , TRA02 ,TRA03 )
!
!         ENDIF
!
!     -----UPDATES THE BOTTOM ELEVATION-----
!
!         CALL ACTUZF(IVIDE,EPAI,ZF,NPOIN2,NPFMAX,NPF)
! 
! Will have to be rewritten
!          CALL GIBSON(ZF,NPOIN2,DT,ELAY,
!     &               T3,T2,LT,XMVS,XMVE,GRAV,NOMBLAY,
!     &               ES,CONC_VASE,CONC,IVIDE,MS_VASE%R,XWC(1),
!     &                 TRA01,TRA02,TRA03,CGEL,COEF_N,CFMAX)
!      ELSEIF(ITASS.EQ.1) THEN
!
!     -----MODELS CONSOLIDATION (SIMPLE)-----
!
!         IF (MOD(LT*DT,DTC).LT.1.D-8)
!     &    CALL TASSEC( CONC   , EPAI , TREST , TEMP , DTC ,
!     &                 NPOIN2 , NCOUCH )
!
!
!
!     -----UPDATES THE BOTTOM ELEVATION-----
!
!          DO IPOIN = 1 , NPOIN2
!            HDEP(IPOIN)=0.D0
!            DO IC = 1 , NCOUCH
!              HDEP(IPOIN)= HDEP(IPOIN) + EPAI(IPOIN,IC)
!            ENDDO
!            ZF(IPOIN) = ZR(IPOIN) + HDEP(IPOIN)
!          ENDDO
!
!      ENDIF
!      ENDIF
!
!!+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
! COMPUTES HERE DEPOSITED MASS
!
!      TOTAL DEPOSITED MASS --> MASDEP
!
       MASDEP = MASDEP + FLUX*DT
!
! CV   Check for parallel
! note JMH: questionable (if you do it at every time-step
!                         P_DSUM should be done only at the end
!                         or on FLUX only)
!      IF(NCSIZE.GT.1) MASDEP=P_DSUM(MASDEP)
! ..CV        
!
!=======================================================================
!
      RETURN
      END
      
