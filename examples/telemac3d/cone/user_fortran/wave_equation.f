!                    ************************
                     SUBROUTINE WAVE_EQUATION
!                    ************************
!
     &(LT,ISOUSI)
!
!***********************************************************************
! TELEMAC3D   V6P3                                   21/08/2010
!***********************************************************************
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ISOUSI         |-->| RANK OF CURRENT SUB-ITERATION
!| LT             |-->| CURRENT TIME STEP NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_WAVE_EQUATION => WAVE_EQUATION
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LT,ISOUSI
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER           :: I
!
!=======================================================================
!    1) COMPUTES THE DIFFUSION TERMS DIFF
!
!       AND THEN UC + DT(F - DIFF -G GRAD(Z))
!
!       STORED IN T3_01 AND T3_02
!
!=======================================================================
!
      CALL OS('X=Y     ',X=UC,Y=UN)
      CALL OS('X=Y     ',X=VC,Y=VN)
!
      DO I=1,U%DIM1
        UCONV%R(I)=UN%R(I)
        VCONV%R(I)=VN%R(I)
      ENDDO
!
      CALL VECTOR(T3_06,'=','FLUBOR          ',IELBOR(IELM3,2),
     &            1.D0,SVIDE,SVIDE,SVIDE,UCONV,VCONV,SVIDE,
     &            MESH3D,.TRUE.,MASK_3D%ADR(8)%P)
!
      CALL SUMVER(FLBOR%R,T3_06%R,NPOIN2,NPLAN,MESH2D%NPTFR)
!
      CALL OS('X=0     ',X=DH)
!
      CALL MAKE_ZCONV(ZCONV,GRAZCO,ZFLATS,DH,HN,ZF,TETAZCOMP,TETAH,
     &                NELEM2,OPTBAN,MESH2D%IKLE%I,MESH2D)
!
      CALL OS('X=0     ',X=DM1)
!
      IF(NONHYD) CALL OS ('X=0     ', X=WD)
      IF(NONHYD) CALL OS ('X=0     ', X=W)
!
      IF(NONHYD) THEN
        DO I=1,NPOIN3
          WCONV%R(I)=WN%R(I)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      CALL VERMOY(U2D%R,V2D%R,U%R,V%R,2,Z,
     &            T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
!
!-----------------------------------------------------------------------
!
!     CLASSICAL ADVECTION FIELD IS USED FOR CHARACTERISTICS
!     IT IS REBUILT HERE IN UCONVC AND VCONVC FOR USE IN PRECON
!
      IF(N_ADV(ADV_CAR).GT.0) THEN
        CALL OS( 'X=CY    ' , X=UCONVC, Y=UN , C=1.D0-TETAU )
        CALL OS( 'X=X+CY  ' , X=UCONVC, Y=U  , C=     TETAU )
        CALL OS( 'X=CY    ' , X=VCONVC, Y=VN , C=1.D0-TETAU )
        CALL OS( 'X=X+CY  ' , X=VCONVC, Y=V  , C=     TETAU )
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

