!                    *****************
                     SUBROUTINE SMAGOR
!                    *****************
!
     &(VISC,CF,U,V,MESH,T1,T2,T3,T4,MSK,MASKEL,PROPNU)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES VISCOSITY USING SMAGORINSKY'S MODEL.
!code
!+                                     (1/2)
!+    NU    =   CS2 * ( 2.0 * SIJ * SIJ )  * (MESH SIZE)**2
!+
!+                         2        2            2
!+                      DU       DV     DU   DV
!+     2*SIJ*SIJ = ( 2*(--) + 2*(--) + (-- + --)
!+                      DX       DY     DY   DX
!
!history  ADRIAN KLINGS (ENPC)
!+        06/10/1997
!+        V5P6
!+
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
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN) :: MSK
      DOUBLE PRECISION, INTENT(IN)   :: PROPNU
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: VISC,T1,T2,T3,T4
      TYPE(BIEF_OBJ), INTENT(IN)     :: MASKEL,CF,U,V
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,NPOIN,IELMU,IELMC
      DOUBLE PRECISION CS,CS2
!
!-----------------------------------------------------------------------
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
      CS = 0.1D0
      CS2 = CS**2
!
!-----------------------------------------------------------------------
!
      IELMU = U%ELM
      IELMC = VISC%ELM
!
!     COMPUTES GRADIENTS (IN FACT AVERAGED GRADIENT MULTIPLIED BY
!     A SURFACE WHICH IS THE INTEGRAL OF TEST FUNCTIONS ON THE DOMAIN,
!     THIS SURFACE IS CONSIDERED TO BE (MESH SIZE)**2)
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
      NPOIN = VISC%DIM1
!
      DO N = 1,NPOIN
        VISC%R(N)=PROPNU+SQRT((2*T1%R(N)**2+2*T4%R(N)**2
     &                                       +(T2%R(N)+T3%R(N))**2))*CS2
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
