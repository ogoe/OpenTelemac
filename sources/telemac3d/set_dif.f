!                    ******************
                     SUBROUTINE SET_DIF
!                    ******************
!
     &(FC,VOLU2D,Z,NPOIN2,NPOIN3,DT,FLUX, 
     & NPLAN,WCC,FLUDPT,FLUDP, FLUER,IPBOT,VISCTA)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   23/01/2014
!***********************************************************************
!
!brief    1D VERTICAL PROFILE MODEL FOR SETTLING & DIFFUSION  
!+        PLUS BED EXCHANGE DUE TO NET EROSION & DEPOSITION
!+        
!+        SOLVED USING A TRIDIAGONAL MATRIX SOLVER (trid1d.f)
!+
!
!history  T BENSON, D KELLY & C VILLARET (HRW)
!+        23/01/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP
!| FC             |<->| VARIABLE AFTER CONVECTION
!| FLUER          |-->| EROSION FLUX (SEDIMENT)
!| FLUDP          |-->| DEPOSITION FLUX  (SEDIMENT)
!| FLUDPT         |-->| IMPLICIT PART OF DEPOSITION FLUX  (SEDIMENT)
!| FLUX           |<->| GLOBAL FLUXES TO BE CHANGED
!| FN             |-->| VARIABLE AT TIME N
!| IPBOT          |-->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| VISCTA         |-->| VISCOSITY OF THE TRACER
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS IN 2D (SURFACE OF ELEMENTS) 
!| WCC            |-->| SETTLING VELOCITY (SEDIMENT)
!| Z              |-->| NODE ELEVATIONS OF THE 3D MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
! TRAV1 IS NEW 1D BIEF_OBJ BLOCK OF WORK ARRAYS (NPLANx11)
! THIS SHOULD BE AN INPUT ARGUMENT (REQUIRES EXTRA CVDF3D ARGUMENT)
      USE DECLARATIONS_TELEMAC3D, ONLY: TRAV1
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN3,NPOIN2
      INTEGER, INTENT(IN)             :: NPLAN
!      
      INTEGER, INTENT(IN)             :: IPBOT(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: FC(NPOIN3)
!
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX
      DOUBLE PRECISION, INTENT(IN)    :: DT,Z(NPOIN3)
!
      TYPE(BIEF_OBJ), INTENT(IN)      :: WCC, FLUDPT, VOLU2D
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUDP, FLUER
      TYPE(BIEF_OBJ), INTENT(IN)      :: VISCTA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,I1,IPLAN
!
      DOUBLE PRECISION :: FLUERO,FLUNET,FLUDEP,A,B
!     POINTERS TO 1DV ARRAY BLOCK
      TYPE(BIEF_OBJ),POINTER :: DZA,DZB,DZ
      TYPE(BIEF_OBJ),POINTER :: AA,BB,CC,DD
      TYPE(BIEF_OBJ),POINTER :: WS,EV,CONC,GAM
! 
! a block of vectors was allocated using statut=0 in point_telemac3d
! pointers get set each time
!
! SET POINTERS
      DZA  => TRAV1%ADR(01)%P
      DZB  => TRAV1%ADR(02)%P
      DZ   => TRAV1%ADR(03)%P
      AA   => TRAV1%ADR(04)%P
      BB   => TRAV1%ADR(05)%P
      CC   => TRAV1%ADR(06)%P
      DD   => TRAV1%ADR(07)%P
      WS   => TRAV1%ADR(08)%P
      EV   => TRAV1%ADR(09)%P
      CONC => TRAV1%ADR(10)%P
      GAM  => TRAV1%ADR(11)%P
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!      
! LOOP THROUGH HORIZONTAL JUST ONCE
      DO IPOIN = 1,NPOIN2   
!
! TIDAL FLATS
      IF(IPBOT(IPOIN).EQ.(NPLAN-1)) THEN
          FLUER%R(IPOIN) = 0.D0
          FLUDP%R(IPOIN) = 0.D0
!         SKIP THIS NODE
          CYCLE
      ENDIF  
!      
! CALCULATE DZ VARIABLES: DZA,DZB,DZ (NPLAN IN SIZE)
!
!     BOTTOM PLANE
      DZA%R(1)  = 0.D0
      I1=IPOIN
      DZB%R(1)  = Z(I1+NPOIN2)-Z(I1)
      DZ%R(1)   = 0.5D0*DZB%R(1)
!    INTERMEDIATE PLANES
      DO IPLAN=2,NPLAN-1
          I1 = IPOIN + (IPLAN-1)*NPOIN2
          DZA%R(IPLAN)  = Z(I1)-Z(I1-NPOIN2)
          DZB%R(IPLAN)  = Z(I1+NPOIN2)-Z(I1)
          DZ%R(IPLAN)   = 0.5D0*(DZA%R(IPLAN)+DZB%R(IPLAN))
      ENDDO    
!     SURFACE PLANE
      I1 = IPOIN + (NPLAN-1)*NPOIN2
      DZA%R(NPLAN)  = Z(I1)-Z(I1-NPOIN2)
      DZB%R(NPLAN)  = 0.D0
      DZ%R(NPLAN)   = 0.5D0*DZA%R(NPLAN)
!              
! NET EROSION - DEPOSITION FLUX (N.B. BOTH ARE +VE VALUES)
      FLUERO = FLUER%R(IPOIN)*DT 
!          
! SETTLING AND DIFFUSION
!
! FILL THE CONCENTRATION PROFILE ARRAY (RHS OF MATRIX)
      DO IPLAN=1,NPLAN
          I1 = IPOIN + (IPLAN-1)*NPOIN2
          DD%R(IPLAN) = FC(I1) !CONC(IPLAN)
      ENDDO
      DD%R(1)=DD%R(1)+FLUERO/DZ%R(1)
!
! VISCOSITY AND SETTLING VELOCITY ARRAYS
! DEAL WITH VISCOSITY SCHEME OPTION 2 (SEE VISCLM)
      IF (VISCTA%DIMDISC.EQ.4111) THEN
        DO IPLAN=1,NPLAN
          I1 = IPOIN + (IPLAN-1)*NPOIN2
          EV%R(IPLAN)=VISCTA%R(I1)
          WS%R(IPLAN)= WCC%R(I1)
        ENDDO
      ELSE 
        DO IPLAN=1,NPLAN-1
          I1 = IPOIN + (IPLAN-1)*NPOIN2
          EV%R(IPLAN)=0.5*(VISCTA%R(I1)+VISCTA%R(I1+NPOIN2))
          WS%R(IPLAN)= WCC%R(I1)
        ENDDO
        I1 = IPOIN + (NPLAN-1)*NPOIN2
        EV%R(NPLAN)=EV%R(NPLAN-1)
        WS%R(NPLAN)=WCC%R(I1)
      ENDIF
!
! APPLY THE DEPOSITION RATE TO BOTTOM PLANE
      WS%R(1)=FLUDPT%R(IPOIN)
!
! SETUP THE DIFFUSION PARAMETERS 
!
!     BOTTOM PLANE
      B=EV%R(1)/(DZ%R(1)*DZB%R(1))
      BB%R(1)=1.D0+(WS%R(1)/DZ%R(1)+B)*DT
      CC%R(1)=    -(WS%R(2)/DZ%R(1)+B)*DT
!     INTERMEDIATE PLANES
      DO IPLAN=2,NPLAN-1
!         
!       PARAMETERS A AND B
        A=EV%R(IPLAN-1)/(DZ%R(IPLAN)*DZA%R(IPLAN))
        B=EV%R(IPLAN  )/(DZ%R(IPLAN)*DZB%R(IPLAN))
!
!       DIFFUSION MATRIX
        AA%R(IPLAN)=-A*DT
        BB%R(IPLAN)=1.D0+ (WS%R(IPLAN)/DZ%R(IPLAN)+(A+B))*DT
        CC%R(IPLAN)=-(WS%R(IPLAN+1)/DZ%R(IPLAN)+B)*DT
!         
      ENDDO
!     SURFACE PLANE
      A = EV%R(NPLAN-1)/(DZ%R(NPLAN)*DZA%R(NPLAN))
      AA%R(NPLAN)=-A*DT
      BB%R(NPLAN)=1.D0+(WS%R(NPLAN)/DZ%R(NPLAN)+A)*DT
!
! MAIN SETTLING & DIFFUSION - DIRECT SOLVER
!    
      CALL TRID1D(CONC%R,AA%R,BB%R,CC%R,DD%R,GAM%R,NPLAN)
!   
! OUTPUT THE FINAL CONCENTRATION PROFILE 
      DO IPLAN=1,NPLAN
          I1 = IPOIN + (IPLAN-1)*NPOIN2
          FC(I1)=CONC%R(IPLAN)
      ENDDO
      FLUDP%R(IPOIN)=FLUDPT%R(IPOIN)*FC(IPOIN)
!
! IMPLICIT DEPOSITION FLUX      
      FLUDEP = FLUDPT%R(IPOIN)*FC(IPOIN)*DT
      FLUNET = FLUERO - FLUDEP ! NET EROSION IS POSITIVE   
! UPDATE FLUX OUT OF DOMAIN (+VE OUT, SO SUBTRACT)
      FLUX = FLUX-FLUNET*VOLU2D%R(IPOIN)
!
      ENDDO  ! END OF NODE LOOP   
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
