!                    *****************
                     SUBROUTINE MT03BB
!                    *****************
!
     &( A11 , A12 , A13 , A14 ,
     &  A21 , A22 , A23 , A24 ,
     &  A31 , A32 , A33 , A34 ,
     &  A41 , A42 , A43 , A44 ,
     &  XMUL,SF,SG,SU,SV,F,G,U,V,
     &  XEL,YEL,IKLE1,IKLE2,IKLE3,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+                 /  -->   - -->          -->  --->
!+ A(I,J) = XMUL  /   KEL . GRAD(PSI1(I)) * U . GRAD(PSI2(J)) D(OMEGA)
!+               /OMEGA
!+         -->
!+         KEL CONSTANT VECTOR ON THE ELEMENT, WITH COMPONENTS F AND G
!+
!+         PSI1 OF P1 DISCRETISATION
!+         PSI2 OF P2 DISCRETISATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
!+        12/04/93
!+        V5P1
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
!| A11,A12        |<--| ELEMENTS DE LA MATRICE
!| A13            |---| 
!| A14            |---| 
!| A21            |---| 
!| A22            |---| 
!| A23            |---| 
!| A24            |---| 
!| A31            |---| 
!| A32            |---| 
!| A33            |---| 
!| A34            |---| 
!| A41            |---| 
!| A42            |---| 
!| A43            |---| 
!| A44            |---| 
!| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!| IKLE2          |---| 
!| IKLE3          |---| 
!| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| XMUL           |-->| FACTEUR MULTIPLICATIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT03BB => MT03BB
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*),A44(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),U(*),V(*)
!
!     STRUCTURES OF      F, G, U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SU,SV
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM,IELMF,IELMG,IELMU,IELMV
!
      DOUBLE PRECISION X2,X3,Y2,Y3,U1,U2,U3,U4,V1,V2,V3,V4
      DOUBLE PRECISION KXEL,KYEL
!
!-----------------------------------------------------------------------
!
      IELMF = SF%ELM
      IELMG = SG%ELM
      IELMU = SU%ELM
      IELMV = SV%ELM
!
!-----------------------------------------------------------------------
! CASE WHERE U IS OF P1 DISCRETISATION
!-----------------------------------------------------------------------
!
      IF(IELMF.EQ.10.AND.IELMG.EQ.10.AND.
     &   IELMU.EQ.11.AND.IELMV.EQ.11) THEN
!
!   LOOP ON THE ELEMENTS
!
      DO 1 IELEM = 1 , NELEM
!
      X2  =   XEL(IELEM,2)
      X3  =   XEL(IELEM,3)
      Y2  =   YEL(IELEM,2)
      Y3  =   YEL(IELEM,3)
!
      U1   =  U(IKLE1(IELEM))
      U2   =  U(IKLE2(IELEM))
      U3   =  U(IKLE3(IELEM))
      V1   =  V(IKLE1(IELEM))
      V2   =  V(IKLE2(IELEM))
      V3   =  V(IKLE3(IELEM))
!
      KXEL =  F(IELEM)
      KYEL =  G(IELEM)
!
! COMPUTES 9 OF THE 16 TERMS
!
      A11(IELEM)=
     &  (X2**2*KYEL*(8*V3+17*V2+20*V1)+4*X2*X3*KYEL*(-
     &  5*V3-5*V2-8*V1)+X2*Y2*KXEL*(-8*V3-17*V2-20*V1)+X2*Y2*
     &  KYEL*(-8*U3-17*U2-20*U1)+2*X2*Y3*KXEL*(5*V3+5*V2+8*V1)
     &  +2*X2*Y3*KYEL*(5*U3+5*U2+8*U1)+X3**2*KYEL*(17*V3+8*V2+
     &  20*V1)+2*X3*Y2*KXEL*(5*V3+5*V2+8*V1)+2*X3*Y2*KYEL*(5*U3
     &  +5*U2+8*U1)+X3*Y3*KXEL*(-17*V3-8*V2-20*V1)+X3*Y3*KYEL*(-
     &  17*U3-8*U2-20*U1)+Y2**2*KXEL*(8*U3+17*U2+20*U1)+4*Y2*
     &  Y3*KXEL*(-5*U3-5*U2-8*U1)+Y3**2*KXEL*(17*U3+8*U2+20*U1)
     &  )*XMUL/(54*X2*Y3-54*X3*Y2)
!
      A12(IELEM)=
     & (2*X2**2*KYEL*(V3+4*V2+4*V1)+X2*X3*KYEL*(V3+4*
     &  V2+4*V1)+2*X2*Y2*KXEL*(-V3-4*V2-4*V1)+2*X2*Y2*KYEL*(-U3-
     &  4*U2-4*U1)+X2*Y3*KXEL*(V3+4*V2+4*V1)+2*X2*Y3*KYEL*(-U3-4
     &  *U2-4*U1)+X3**2*KYEL*(-V3-4*V2-4*V1)+2*X3*Y2*KXEL*(-V3-4
     &  *V2-4*V1)+X3*Y2*KYEL*(U3+4*U2+4*U1)+X3*Y3*KXEL*(V3+4*V2+
     &  4*V1)+X3*Y3*KYEL*(U3+4*U2+4*U1)+2*Y2**2*KXEL*(U3+4*U2+4*
     &  U1)+Y2*Y3*KXEL*(U3+4*U2+4*U1)+Y3**2*KXEL*(-U3-4*U2-4*U1))
     &  *XMUL/(54*X2*Y3-54*X3*Y2)
!
      A13(IELEM)=
     &  (X2**2*KYEL*(-4*V3-V2-4*V1)+X2*X3*KYEL*(4*V3+V2+
     &  4*V1)+X2*Y2*KXEL*(4*V3+V2+4*V1)+X2*Y2*KYEL*(4*U3+U2+4*U1)
     &  +2*X2*Y3*KXEL*(-4*V3-V2-4*V1)+X2*Y3*KYEL*(4*U3+U2+4*U1)+
     &  2*X3**2*KYEL*(4*V3+V2+4*V1)+X3*Y2*KXEL*(4*V3+V2+4*V1)+2*
     &  X3*Y2*KYEL*(-4*U3-U2-4*U1)+2*X3*Y3*KXEL*(-4*V3-V2-4*V1)+
     &  2*X3*Y3*KYEL*(-4*U3-U2-4*U1)+Y2**2*KXEL*(-4*U3-U2-4*U1)+
     &  Y2*Y3*KXEL*(4*U3+U2+4*U1)+2*Y3**2*KXEL*(4*U3+U2+4*U1))
     &  *XMUL/(54*X2*Y3-54*X3*Y2)
!
      A21(IELEM)=
     &  (2*X2**2*KYEL*(V3+4*V2+4*V1)+X2*X3*KYEL*(V3+4*
     &  V2+4*V1)+2*X2*Y2*KXEL*(-V3-4*V2-4*V1)+2*X2*Y2*KYEL*(-U3-
     &  4*U2-4*U1)+2*X2*Y3*KXEL*(-V3-4*V2-4*V1)+X2*Y3*KYEL*(U3+4
     &  *U2+4*U1)+X3**2*KYEL*(-V3-4*V2-4*V1)+X3*Y2*KXEL*(V3+4*V2+
     &  4*V1)+2*X3*Y2*KYEL*(-U3-4*U2-4*U1)+X3*Y3*KXEL*(V3+4*V2+4
     &  *V1)+X3*Y3*KYEL*(U3+4*U2+4*U1)+2*Y2**2*KXEL*(U3+4*U2+4*
     &  U1)+Y2*Y3*KXEL*(U3+4*U2+4*U1)+Y3**2*KXEL*(-U3-4*U2-4*U1))
     &  *XMUL/(54*X2*Y3-54*X3*Y2)
!
      A23(IELEM)=
     &  (2*X2**2*KYEL*(4*V3+4*V2+V1)+5*X2*X3*KYEL*(-4*
     &  V3-4*V2-V1)+2*X2*Y2*KXEL*(-4*V3-4*V2-V1)+2*X2*Y2*KYEL*(-
     &  4*U3-4*U2-U1)+4*X2*Y3*KXEL*(4*V3+4*V2+V1)+X2*Y3*KYEL*(4*
     &  U3+4*U2+U1)+2*X3**2*KYEL*(4*V3+4*V2+V1)+X3*Y2*KXEL*(4*V3
     &  +4*V2+V1)+4*X3*Y2*KYEL*(4*U3+4*U2+U1)+2*X3*Y3*KXEL*(-4*
     &  V3-4*V2-V1)+2*X3*Y3*KYEL*(-4*U3-4*U2-U1)+2*Y2**2*KXEL*(
     &  4*U3+4*U2+U1)+5*Y2*Y3*KXEL*(-4*U3-4*U2-U1)+2*Y3**2*KXEL*
     &  (4*U3+4*U2+U1))*XMUL/(54*X2*Y3-54*X3*Y2)
!
      A24(IELEM)=
     &  (X2**2*KYEL*(-5*V3-8*V2-5*V1)+X2*X3*KYEL*(11*V3
     &  +8*V2-V1)+X2*Y2*KXEL*(5*V3+8*V2+5*V1)+X2*Y2*KYEL*(5*U3+
     &  8*U2+5*U1)+X2*Y3*KXEL*(-7*V3-4*V2+2*V1)+X2*Y3*KYEL*(-4*
     &  U3-4*U2-U1)+2*X3**2*KYEL*(-4*V3-4*V2-V1)+X3*Y2*KXEL*(-4*
     &  V3-4*V2-V1)+X3*Y2*KYEL*(-7*U3-4*U2+2*U1)+2*X3*Y3*KXEL*(
     &  4*V3+4*V2+V1)+2*X3*Y3*KYEL*(4*U3+4*U2+U1)+Y2**2*KXEL*(-5
     &  *U3-8*U2-5*U1)+Y2*Y3*KXEL*(11*U3+8*U2-U1)+2*Y3**2*KXEL*(
     &  -4*U3-4*U2-U1))*XMUL/(18*X2*Y3-18*X3*Y2)
!
      A31(IELEM)=
     &  (X2**2*KYEL*(-4*V3-V2-4*V1)+X2*X3*KYEL*(4*V3+V2+
     &  4*V1)+X2*Y2*KXEL*(4*V3+V2+4*V1)+X2*Y2*KYEL*(4*U3+U2+4*U1)
     &  +X2*Y3*KXEL*(4*V3+V2+4*V1)+2*X2*Y3*KYEL*(-4*U3-U2-4*U1)+
     &  2*X3**2*KYEL*(4*V3+V2+4*V1)+2*X3*Y2*KXEL*(-4*V3-V2-4*V1)
     &  +X3*Y2*KYEL*(4*U3+U2+4*U1)+2*X3*Y3*KXEL*(-4*V3-V2-4*V1)+
     &  2*X3*Y3*KYEL*(-4*U3-U2-4*U1)+Y2**2*KXEL*(-4*U3-U2-4*U1)+
     &  Y2*Y3*KXEL*(4*U3+U2+4*U1)+2*Y3**2*KXEL*(4*U3+U2+4*U1))
     &  *XMUL/(54*X2*Y3-54*X3*Y2)
!
      A32(IELEM)=
     &  (2*X2**2*KYEL*(4*V3+4*V2+V1)+5*X2*X3*KYEL*(-4*
     &  V3-4*V2-V1)+2*X2*Y2*KXEL*(-4*V3-4*V2-V1)+2*X2*Y2*KYEL*(-
     &  4*U3-4*U2-U1)+X2*Y3*KXEL*(4*V3+4*V2+V1)+4*X2*Y3*KYEL*(4*
     &  U3+4*U2+U1)+2*X3**2*KYEL*(4*V3+4*V2+V1)+4*X3*Y2*KXEL*(4
     &  *V3+4*V2+V1)+X3*Y2*KYEL*(4*U3+4*U2+U1)+2*X3*Y3*KXEL*(-4*
     &  V3-4*V2-V1)+2*X3*Y3*KYEL*(-4*U3-4*U2-U1)+2*Y2**2*KXEL*(
     &  4*U3+4*U2+U1)+5*Y2*Y3*KXEL*(-4*U3-4*U2-U1)+2*Y3**2*KXEL*
     &  (4*U3+4*U2+U1))*XMUL/(54*X2*Y3-54*X3*Y2)
!
      A34(IELEM)=
     &  (2*X2**2*KYEL*(-4*V3-4*V2-V1)+X2*X3*KYEL*(8*V3+
     &  11*V2-V1)+2*X2*Y2*KXEL*(4*V3+4*V2+V1)+2*X2*Y2*KYEL*(4*U3
     &  +4*U2+U1)+X2*Y3*KXEL*(-4*V3-4*V2-V1)+X2*Y3*KYEL*(-4*U3-7
     &  *U2+2*U1)+X3**2*KYEL*(-8*V3-5*V2-5*V1)+X3*Y2*KXEL*(-4*V3
     &  -7*V2+2*V1)+X3*Y2*KYEL*(-4*U3-4*U2-U1)+X3*Y3*KXEL*(8*V3+
     &  5*V2+5*V1)+X3*Y3*KYEL*(8*U3+5*U2+5*U1)+2*Y2**2*KXEL*(-4
     &  *U3-4*U2-U1)+Y2*Y3*KXEL*(8*U3+11*U2-U1)+Y3**2*KXEL*(-8*U3
     &  -5*U2-5*U1))*XMUL/(18*X2*Y3-18*X3*Y2)
!
!  USES HERE THE 'MAGIC SQUARE' PROPERTIES OF A DIFFUSION-LIKE MATRIX
!  (SUM OF EACH LINE AND EACH COLUMN IS 0)
!
      A14(IELEM) = - A11(IELEM) - A12(IELEM) - A13(IELEM)
      A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
      A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
      A41(IELEM) = - A11(IELEM) - A21(IELEM) - A31(IELEM)
      A42(IELEM) = - A12(IELEM) - A22(IELEM) - A32(IELEM)
      A43(IELEM) = - A13(IELEM) - A23(IELEM) - A33(IELEM)
      A44(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
!
1     CONTINUE
!
!-----------------------------------------------------------------------
! CASE WHERE U IS OF QUASI-BUBBLE DISCRETISATION
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.10.AND.IELMG.EQ.10.AND.
     &       IELMU.EQ.12.AND.IELMV.EQ.12) THEN
!
!   LOOP ON THE ELEMENTS
!
      DO 2 IELEM = 1 , NELEM
!
      X2  =   XEL(IELEM,2)
      X3  =   XEL(IELEM,3)
      Y2  =   YEL(IELEM,2)
      Y3  =   YEL(IELEM,3)
!
      U1   =  U(IKLE1(IELEM))
      U2   =  U(IKLE2(IELEM))
      U3   =  U(IKLE3(IELEM))
      U4   =  U(IKLE3(IELEM))
      V1   =  V(IKLE1(IELEM))
      V2   =  V(IKLE2(IELEM))
      V3   =  V(IKLE3(IELEM))
      V4   =  V(IKLE3(IELEM))
!
      KXEL =  F(IELEM)
      KYEL =  G(IELEM)
!
! COMPUTES 9 OF THE 16 TERMS
!
      A11(IELEM)=
     &  (X2**2*KYEL*(V3+5*V4+4*V2+5*V1)+4*X2*X3*KYEL*(-
     &  V3-2*V4-V2-2*V1)+X2*Y2*KXEL*(-V3-5*V4-4*V2-5*V1)+X2*Y2
     &  *KYEL*(-U3-5*U4-4*U2-5*U1)+2*X2*Y3*KXEL*(V3+2*V4+V2+2*
     &  V1)+2*X2*Y3*KYEL*(U3+2*U4+U2+2*U1)+X3**2*KYEL*(4*V3+5*V4
     &  +V2+5*V1)+2*X3*Y2*KXEL*(V3+2*V4+V2+2*V1)+2*X3*Y2*KYEL*(
     &  U3+2*U4+U2+2*U1)+X3*Y3*KXEL*(-4*V3-5*V4-V2-5*V1)+X3*Y3
     &  *KYEL*(-4*U3-5*U4-U2-5*U1)+Y2**2*KXEL*(U3+5*U4+4*U2+5*
     &  U1)+4*Y2*Y3*KXEL*(-U3-2*U4-U2-2*U1)+Y3**2*KXEL*(4*U3+5*
     &  U4+U2+5*U1))*XMUL/(18*X2*Y3-18*X3*Y2)
!
      A12(IELEM)=
     &  (2*X2**2*KYEL*(V4+V2+V1)+X2*X3*KYEL*(V4+V2+V1)-(2
     &  *X2*Y2*KXEL)*(V4+V2+V1)-(2*X2*Y2*KYEL)*(U4+U2+U1)+X2*Y3*KXEL*(
     &  V4+V2+V1)-(2*X2*Y3*KYEL)*(U4+U2+U1)-(X3**2*KYEL)*(V4+V2+V1)-
     &  (2*X3*Y2*KXEL)*(V4+V2+V1)+X3*Y2*KYEL*(U4+U2+U1)+X3*Y3*KXEL*(V4
     &  +V2+V1)+X3*Y3*KYEL*(U4+U2+U1)+2*Y2**2*KXEL*(U4+U2+U1)+Y2*Y3*
     &  KXEL*(U4+U2+U1)-(Y3**2*KXEL)*(U4+U2+U1))
     &  *XMUL/(18*X2*Y3-18*X3*Y2)
!
      A13(IELEM)=
     &  (-(X2**2*KYEL)*(V3+V4+V1)+X2*X3*KYEL*(V3+V4+V1)+X2*
     &  Y2*KXEL*(V3+V4+V1)+X2*Y2*KYEL*(U3+U4+U1)-(2*X2*Y3*KXEL)*(V3+V4
     &  +V1)+X2*Y3*KYEL*(U3+U4+U1)+2*X3**2*KYEL*(V3+V4+V1)+X3*Y2*KXEL*
     &  (V3+V4+V1)-(2*X3*Y2*KYEL)*(U3+U4+U1)-(2*X3*Y3*KXEL)*(V3+V4+
     &  V1)-(2*X3*Y3*KYEL)*(U3+U4+U1)-(Y2**2*KXEL)*(U3+U4+U1)+Y2*Y3*
     &  KXEL*(U3+U4+U1)+2*Y3**2*KXEL*(U3+U4+U1))
     &  *XMUL/(18*X2*Y3-18*X3*Y2)
!
      A21(IELEM)=
     &  (2*X2**2*KYEL*(V4+V2+V1)+X2*X3*KYEL*(V4+V2+V1)-(2
     &  *X2*Y2*KXEL)*(V4+V2+V1)-(2*X2*Y2*KYEL)*(U4+U2+U1)-(2*X2*Y3*
     &  KXEL)*(V4+V2+V1)+X2*Y3*KYEL*(U4+U2+U1)-(X3**2*KYEL)*(V4+V2+V1)+
     &  X3*Y2*KXEL*(V4+V2+V1)-(2*X3*Y2*KYEL)*(U4+U2+U1)+X3*Y3*KXEL*(V4
     &  +V2+V1)+X3*Y3*KYEL*(U4+U2+U1)+2*Y2**2*KXEL*(U4+U2+U1)+Y2*Y3*
     &  KXEL*(U4+U2+U1)-(Y3**2*KXEL)*(U4+U2+U1))
     &  *XMUL/(18*X2*Y3-18*X3*Y2)
!
      A23(IELEM)=
     &  (2*X2**2*KYEL*(V3+V4+V2)-(5*X2*X3*KYEL)*(V3+V4+V2
     &  )-(2*X2*Y2*KXEL)*(V3+V4+V2)-(2*X2*Y2*KYEL)*(U3+U4+U2)+4*X2
     &  *Y3*KXEL*(V3+V4+V2)+X2*Y3*KYEL*(U3+U4+U2)+2*X3**2*KYEL*(V3+V4+
     &  V2)+X3*Y2*KXEL*(V3+V4+V2)+4*X3*Y2*KYEL*(U3+U4+U2)-(2*X3*Y3*
     &  KXEL)*(V3+V4+V2)-(2*X3*Y3*KYEL)*(U3+U4+U2)+2*Y2**2*KXEL*(U3+
     &  U4+U2)-(5*Y2*Y3*KXEL)*(U3+U4+U2)+2*Y3**2*KXEL*(U3+U4+U2))
     &  *XMUL/(18*X2*Y3-18*X3*Y2)
!
      A24(IELEM)=
     &  (X2**2*KYEL*(-V3-2*V4-2*V2-V1)+X2*X3*KYEL*(3*V3+
     &  2*V4+2*V2-V1)+X2*Y2*KXEL*(V3+2*V4+2*V2+V1)+X2*Y2*KYEL*(U3+
     &  2*U4+2*U2+U1)+X2*Y3*KXEL*(-2*V3-V4-V2+V1)-(X2*Y3*KYEL)*(U3+
     &  U4+U2)-(2*X3**2*KYEL)*(V3+V4+V2)-(X3*Y2*KXEL)*(V3+V4+V2)+X3*
     &  Y2*KYEL*(-2*U3-U4-U2+U1)+2*X3*Y3*KXEL*(V3+V4+V2)+2*X3*Y3*
     &  KYEL*(U3+U4+U2)+Y2**2*KXEL*(-U3-2*U4-2*U2-U1)+Y2*Y3*KXEL*(3*
     &  U3+2*U4+2*U2-U1)-(2*Y3**2*KXEL)*(U3+U4+U2))
     &  *XMUL/(6*X2*Y3-6*X3*Y2)
!
      A31(IELEM)=
     &  (-(X2**2*KYEL)*(V3+V4+V1)+X2*X3*KYEL*(V3+V4+V1)+X2*
     &  Y2*KXEL*(V3+V4+V1)+X2*Y2*KYEL*(U3+U4+U1)+X2*Y3*KXEL*(V3+V4+V1)-
     &  (2*X2*Y3*KYEL)*(U3+U4+U1)+2*X3**2*KYEL*(V3+V4+V1)-(2*X3*Y2
     &  *KXEL)*(V3+V4+V1)+X3*Y2*KYEL*(U3+U4+U1)-(2*X3*Y3*KXEL)*(V3+V4+
     &  V1)-(2*X3*Y3*KYEL)*(U3+U4+U1)-(Y2**2*KXEL)*(U3+U4+U1)+Y2*Y3*
     &  KXEL*(U3+U4+U1)+2*Y3**2*KXEL*(U3+U4+U1))
     &  *XMUL/(18*X2*Y3-18*X3*Y2)
!
      A32(IELEM)=
     &  (2*X2**2*KYEL*(V3+V4+V2)-(5*X2*X3*KYEL)*(V3+V4+V2
     &  )-(2*X2*Y2*KXEL)*(V3+V4+V2)-(2*X2*Y2*KYEL)*(U3+U4+U2)+X2*Y3
     &  *KXEL*(V3+V4+V2)+4*X2*Y3*KYEL*(U3+U4+U2)+2*X3**2*KYEL*(V3+V4+
     &  V2)+4*X3*Y2*KXEL*(V3+V4+V2)+X3*Y2*KYEL*(U3+U4+U2)-(2*X3*Y3*
     &  KXEL)*(V3+V4+V2)-(2*X3*Y3*KYEL)*(U3+U4+U2)+2*Y2**2*KXEL*(U3+
     &  U4+U2)-(5*Y2*Y3*KXEL)*(U3+U4+U2)+2*Y3**2*KXEL*(U3+U4+U2))
     &  *XMUL/(18*X2*Y3-18*X3*Y2)
!
      A34(IELEM)=
     &  (-(2*X2**2*KYEL)*(V3+V4+V2)+X2*X3*KYEL*(2*V3+2*
     &  V4+3*V2-V1)+2*X2*Y2*KXEL*(V3+V4+V2)+2*X2*Y2*KYEL*(U3+U4+U2
     &  )-(X2*Y3*KXEL)*(V3+V4+V2)+X2*Y3*KYEL*(-U3-U4-2*U2+U1)+X3**2*
     &  KYEL*(-2*V3-2*V4-V2-V1)+X3*Y2*KXEL*(-V3-V4-2*V2+V1)-(X3*Y2
     &  *KYEL)*(U3+U4+U2)+X3*Y3*KXEL*(2*V3+2*V4+V2+V1)+X3*Y3*KYEL*(2
     &  *U3+2*U4+U2+U1)-(2*Y2**2*KXEL)*(U3+U4+U2)+Y2*Y3*KXEL*(2*U3
     &  +2*U4+3*U2-U1)+Y3**2*KXEL*(-2*U3-2*U4-U2-U1))
     &  *XMUL/(6*X2*Y3-6*X3*Y2)
!
!
!  USES HERE THE 'MAGIC SQUARE' PROPERTIES OF A DIFFUSION-LIKE MATRIX
!  (SUM OF EACH LINE AND EACH COLUMN IS 0)
!
      A14(IELEM) = - A11(IELEM) - A12(IELEM) - A13(IELEM)
      A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
      A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
      A41(IELEM) = - A11(IELEM) - A21(IELEM) - A31(IELEM)
      A42(IELEM) = - A12(IELEM) - A22(IELEM) - A32(IELEM)
      A43(IELEM) = - A13(IELEM) - A23(IELEM) - A33(IELEM)
      A44(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
!
2     CONTINUE
!
!     OTHER TYPES OF FUNCTIONS F AND G
!
!-----------------------------------------------------------------------
!
      ELSE
!
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF (LNG.EQ.1) WRITE(LU,110) IELMG,SG%NAME
       IF (LNG.EQ.1) WRITE(LU,200) IELMU,SU%NAME
       IF (LNG.EQ.1) WRITE(LU,300)
       IF (LNG.EQ.1) WRITE(LU,101) IELMF,SF%NAME
       IF (LNG.EQ.1) WRITE(LU,111) IELMG,SG%NAME
       IF (LNG.EQ.1) WRITE(LU,201) IELMU,SU%NAME
       IF (LNG.EQ.1) WRITE(LU,301)
100    FORMAT(1X,'MT03BB (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,
     &        1X,'NOM REEL : ',A6)
110    FORMAT(1X,'DISCRETISATION DE G : ',1I6,
     &        1X,'NOM REEL : ',A6)
200    FORMAT(1X,'DISCRETISATION DE U : ',1I6,
     &        1X,'NOM REEL : ',A6)
300    FORMAT(1X,'CAS NON PREVU')
101    FORMAT(1X,'MT03BB (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F:',1I6,
     &        1X,'REAL NAME: ',A6)
111    FORMAT(1X,'DISCRETIZATION OF G:',1I6,
     &        1X,'REAL NAME: ',A6)
201    FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &        1X,'REAL NAME: ',A6)
301    FORMAT(1X,'CASE NOT IMPLEMENTED')
       CALL PLANTE(0)
       STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END