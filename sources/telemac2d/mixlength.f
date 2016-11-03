!                    ********************
                     SUBROUTINE MIXLENGTH
!                    ********************
!
     &(VISC,CF,U,V,H,MESH,T1,T2,T3,T4,MSK,MASKEL,PROPNU,
     & UNSV2D,IELMU,NPTFR)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    COMPUTES THE EDDY VISCOSITY USING THE MIXING LENGTH
!         FOR THE HORIZONTAL + PARABOLIC MODEL FOR THE VERTICAL
!
!history  C. DORFMANN (TU GRAZ)
!+        15/03/2016
!+        V7P2
!+   First version, with negative depths secured and some optimisation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| PROPNU         |-->| MOLECULAR DIFFUSION
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| VISC           |-->| TURBULENT DIFFUSION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL,          INTENT(IN)    :: MSK
      INTEGER,          INTENT(IN)    :: IELMU,NPTFR
      DOUBLE PRECISION, INTENT(IN)    :: PROPNU
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: VISC,T1,T2,T3,T4
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,CF,U,V,H,UNSV2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,N,NPOIN
      DOUBLE PRECISION CL,LM,LMB,USTAR,VISCVERT,VISCHOR2,ALPHA,HC
      DOUBLE PRECISION,PARAMETER :: KARMAN = 0.4D0
!
!-----------------------------------------------------------------------
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
      NPOIN = MESH%NPOIN
!
!-----------------------------------------------------------------------
!
!     COEFFICIENTS:

!     Horizontal mixing length model:
!     CL: theoretically from integration of lm along depth
!     can be used as calibration coefficient
!
      CL = 0.2666667D0
!
!-----------------------------------------------------------------------
!
!     Vertical parabolic model:
!     ALPHA: theoretically from integration of parabolic model along depth
!     can be used as calibration coefficient
!
      ALPHA = KARMAN/6.D0
!
!-----------------------------------------------------------------------
!
      CALL VECTOR(T1,'=','GRADF          X',IELMU,
     &            1.D0,U,U,U,U,U,U,MESH,MSK,MASKEL)
      CALL VECTOR(T2,'=','GRADF          Y',IELMU,
     &            1.D0,U,U,U,U,U,U,MESH,MSK,MASKEL)
      CALL VECTOR(T3,'=','GRADF          X',IELMU,
     &            1.D0,V,V,V,V,V,V,MESH,MSK,MASKEL)
      CALL VECTOR(T4,'=','GRADF          Y',IELMU,
     &            1.D0,V,V,V,V,V,V,MESH,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM (T1, 2, MESH)
        CALL PARCOM (T2, 2, MESH)
        CALL PARCOM (T3, 2, MESH)
        CALL PARCOM (T4, 2, MESH)
      ENDIF
!
      DO I = 1,NPOIN
        USTAR = SQRT(0.5D0*CF%R(I)*(U%R(I)**2+V%R(I)**2))
        HC=MAX(0.D0,H%R(I))
        VISCVERT = ALPHA*HC*USTAR
        LM = CL*KARMAN*HC
        VISCHOR2 = LM**4 * (2*T1%R(I)**2+2*T4%R(I)**2
     &            +(T2%R(I)+T3%R(I))**2) * UNSV2D%R(I)**2
        VISC%R(I)=PROPNU + SQRT(VISCVERT**2+VISCHOR2)
      ENDDO
!
!     LOOP ON THE BOUNDARY NODES
!     reducing eventually the mixing length LMB at the nodes near the wall
!
      DO K = 1,NPTFR
        N = MESH%NBOR%I(K)
        USTAR = SQRT(0.5D0*CF%R(N)*(U%R(N)**2+V%R(N)**2))
        HC=MAX(0.D0,H%R(N))
        VISCVERT = ALPHA*HC*USTAR
        LMB = KARMAN*MIN(CL*HC,MESH%DISBOR%R(K))
        VISCHOR2 = LMB**4 * (2*T1%R(N)**2+2*T4%R(N)**2
     &            +(T2%R(N)+T3%R(N))**2) * UNSV2D%R(N)**2
        VISC%R(N)=PROPNU + SQRT(VISCVERT**2+VISCHOR2)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

