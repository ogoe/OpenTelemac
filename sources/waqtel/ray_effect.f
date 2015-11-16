!                    **********************
                     SUBROUTINE RAY_EFFECT
!                    **********************
!
     &(SECCHI,TRR,NPOIN,BETA,I0,IK,EFF,H)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES RAY EFFECT: COEFFICIENT OF SUNSHINE ON 
!                              THE GROWTH OF ALGAE
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETA           |-->| COEFFICIENT OF VEGETAL TURBIDITY WITHOUT
!|                |   | PHYTOPLANKTON
!| EFF            |<--| SUNSHINE EFFECT ON ALGAE GROWTH 
!| H              |-->| WATER DEPTH ON ALL MESH NODES
!| IK             |-->| PARAMETER FOR THE CALIBRATION OF SMITH FORMULA
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES 
!| SECCHI         |-->| SECCHI DEPTH
!| TRR            |-->| TRACER (CAN BE PHY: PHYTOPLAKTONIC BIOMASS)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_WAQTEL, EX_RAY_EFFCT => RAY_EFFECT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: TRR(NPOIN),BETA,I0,IK,SECCHI
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: H
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: EFF
!   LOCAL VARIABLES 
      INTEGER                    :: KK,ERR
      DOUBLE PRECISION, PARAMETER:: EPS=1.E-6
      DOUBLE PRECISION, PARAMETER:: MOSS=0.015D0
      DOUBLE PRECISION           :: CC,IK2,I02,CNUM
      DOUBLE PRECISION, ALLOCATABLE::KE(:),IH(:)
      INTRINSIC MAX,SQRT,LOG,EXP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     PRELEMINARIES
!
      IK2=IK**2
      I02=I0**2
      CNUM=I0+SQRT(I02+IK2)
!
!     ALLOCATION
!
      ALLOCATE( KE(NPOIN),STAT=ERR)
      ALLOCATE( IH(NPOIN),STAT=ERR) 
      IF(ERR.NE.0)GOTO 100
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INITIALISATION
      CALL OV( 'X=C     ' ,KE,KE,KE,0.D0,NPOIN )
      CALL OV( 'X=C     ' ,IH,IH,IH,0.D0,NPOIN )
      CALL OS( 'X=0     ' ,X=EFF)
!
!     COMPUTE KE 
!
      IF(SECCHI.GT.EPS)THEN
        CC=1.7D0/SECCHI
        CALL OV( 'X=C     ' ,KE,KE,KE,CC,NPOIN )
      ELSE
        CALL OV( 'X=CY    ' ,KE,TRR,KE,MOSS,NPOIN )
        CALL OV( 'X=X+C   ' ,KE,KE,KE,BETA,NPOIN )
      ENDIF
!
!     COMPUTE Ih: 
! 
      DO KK=1,NPOIN
         IH(KK)=I0*EXP(-KE(KK)*MAX(H%R(KK),0.D0))
      ENDDO
!
!     RAY EFFECT IS READY TO BE COMPUTED
!
      DO KK=1,NPOIN
        CC=H%R(KK)*KE(KK)
        IF(CC.GT.EPS)THEN
          EFF%R(KK)=LOG(CNUM/(IH(KK)+SQRT(IK2+IH(KK)**2)))/CC
        ENDIF
      ENDDO
!
!     DEALLOCATION
!
      DEALLOCATE(KE,STAT=ERR)
      DEALLOCATE(IH,STAT=ERR) 
!
!-----------------------------------------------------------------------
!
100   CONTINUE    
      IF(ERR.NE.0)THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'RAY_EFFECT: PROBLEME D''ALLOCATION DE VECTEURS'
          WRITE(LU,*) '            PEUT ETRE MEMOIRE INSUFFISANTE'
          WRITE(LU,*) '            LIBERER MEMOIRE ET RECOMMENCER'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'RAY_EFFECT: PROBLEM WITH ARRAY ALLOCATION'
          WRITE(LU,*) '            COULD BE NOT ENOUGH MEMORY'
          WRITE(LU,*) '            LIBERATE MEMORY AND RESTART'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      RETURN
      END
