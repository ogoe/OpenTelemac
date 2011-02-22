C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code
!>  EXAMPLE WITH ICOORD=1<br>
!>                 /           D       -> -->
!>  A(I,J)= XMUL  /  PSI2(J) * --(F) * U.GRAD(PSI1(I))  D(OMEGA)
!>               /OMEGA        DX<br><br>
!>  PSI1 : BASES OF TYPE QUASI-BUBBLE
!>  PSI2 : BASES OF TYPE LINEAR
!>  F    : FUNCTION OF TYPE QUASI-BUBBLE TRIANGLE
!>  U    : VECTOR OF TYPE LINEAR OR P0<br>
!>  IT WOULD BE A DERIVATIVE WRT Y WITH ICOORD=2
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A21, A22, A23, A31, A32, A33, A41, A42, A43, F, ICOORD, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SF, SU, SURFAC, SV, U, V, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX036, AUX108, AUX432, AX1296, F1, F2, F3, F4, IELEM, IELMF, IELMU, IELMV, U1, U2, U3, UX, UY, V1, V2, V3, X2, X3, XS1296, XSU108, XSU432, XSUR36, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT12BA
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MATRIY()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 09/12/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; C MOULIN (LNH) 30 87 83 81
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A11,A12
!></td><td><--</td><td>ELEMENTS DE LA MATRICE
!>    </td></tr>
!>          <tr><td>A13
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A21
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A23
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A31
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A32
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A33
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A41
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A42
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A43
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
!>    </td></tr>
!>          <tr><td>ICOORD
!></td><td>--></td><td>1: DERIVEE SUIVANT X, 2:SUIVANT Y
!>    </td></tr>
!>          <tr><td>IKLE1
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DE F,G ET H.
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DE U,V ET W.
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES.
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
!>                  CALCUL DE LA MATRICE.
!>    </td></tr>
!>          <tr><td>XEL,YEL,ZEL
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT12BA
     &(  A11 , A12 , A13 ,
     &   A21 , A22 , A23 ,
     &   A31 , A32 , A33 ,
     &   A41 , A42 , A43 ,
     &   XMUL,SF,SU,SV,F,U,V,
     &   XEL,YEL,SURFAC,
     &   IKLE1,IKLE2,IKLE3,IKLE4,
     &   NELEM,NELMAX,ICOORD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A21            |---| 
C| A22            |---| 
C| A23            |---| 
C| A31            |---| 
C| A32            |---| 
C| A33            |---| 
C| A41            |---| 
C| A42            |---| 
C| A43            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| ICOORD         |-->| 1: DERIVEE SUIVANT X, 2:SUIVANT Y
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SU,SV,SW       |-->| STRUCTURES DE U,V ET W.
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| XEL,YEL,ZEL    |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT12BA => MT12BA
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
C
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
C
C     STRUCTURES OF F, U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SU,SV
C
C
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IELMF,IELMU,IELMV
      DOUBLE PRECISION X2,X3,Y2,Y3,F1,F2,F3,F4
      DOUBLE PRECISION U1,U2,U3,V1,V2,V3,UX,UY,AUX108,XSU108,AUX036
      DOUBLE PRECISION AX1296,XS1296,XSUR36,AUX432,XSU432
C
C-----------------------------------------------------------------------
C
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
C
      XSUR36 = XMUL /  36.D0
      XSU108 = XMUL / 108.D0
      XSU432 = XMUL / 432.D0
      XS1296 = XMUL /1296.D0
C
C-----------------------------------------------------------------------
C  CASE WHERE F IS OF TYPE P1
C-----------------------------------------------------------------------
C
      IF(IELMF.EQ.12) THEN
C
      IF(IELMU.EQ.10.AND.IELMV.EQ.10) THEN
C
C================================
C  DERIVATIVE WRT X  =
C================================
C
        IF(ICOORD.EQ.1) THEN
C
C   LOOP ON THE ELEMENTS
C
        DO 1 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
C
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM)) - F1
        F3  =  F(IKLE3(IELEM)) - F1
        F4  =  F(IKLE4(IELEM)) - F1
C
        UX  =  U(IELEM)
        UY  =  V(IELEM)
C
        AUX108 = XSU108 / SURFAC(IELEM)
        AUX036 = XSUR36 / SURFAC(IELEM)
C
C   EXTRADIAGONAL TERMS
C
      A12(IELEM)=((((F3-3*F4)*Y3+Y2*F3)*X2*UY-2*((F3-3*F4)*Y3
     & +Y2*F3)*X3*UY+8*((3*F4-F2)*Y2-Y3*F2)*X2*UY-4*((3*F4-
     & F2)*Y2-Y3*F2)*X3*UY+(Y3*F3-3*Y3*F4+Y2*F3)*(2*Y3-Y2)*UX-
     & 4*(Y3*F2-3*Y2*F4+Y2*F2)*(Y3-2*Y2)*UX))*AUX108
      A13(IELEM)=((4*((F3-3*F4)*Y3+Y2*F3)*X2*UY-8*((F3-3*F4)
     & *Y3+Y2*F3)*X3*UY+2*((3*F4-F2)*Y2-Y3*F2)*X2*UY-((3*F4-
     & F2)*Y2-Y3*F2)*X3*UY+4*(Y3*F3-3*Y3*F4+Y2*F3)*(2*Y3-Y2)*
     & UX-(Y3*F2-3*Y2*F4+Y2*F2)*(Y3-2*Y2)*UX))*AUX108
      A21(IELEM)=(-(((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X2
     & *UY-2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY-4
     & *((3*F4-F2)*Y2-Y3*F2)*X2*UY-4*((3*F4-F2)*Y2-Y3*F2)*X3*
     & UY-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(2*
     & Y3-Y2)*UX-4*(Y3*F2-3*Y2*F4+Y2*F2)*(Y3+Y2)*UX))*AUX108
      A23(IELEM)=(-(4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)
     & *X2*UY-8*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY
     & -((3*F4-F2)*Y2-Y3*F2)*X2*UY-((3*F4-F2)*Y2-Y3*F2)*X3*UY-
     & 4*(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(2*
     & Y3-Y2)*UX-(Y3*F2-3*Y2*F4+Y2*F2)*(Y3+Y2)*UX))*AUX108
      A31(IELEM)=(-(2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)
     & *X2*UY-((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY+4
     & *((F3-3*F4)*Y3+Y2*F3)*X2*UY+4*((F3-3*F4)*Y3+Y2*F3)*X3*
     & UY-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(Y3-
     & 2*Y2)*UX-4*(Y3*F3-3*Y3*F4+Y2*F3)*(Y3+Y2)*UX))*AUX108
      A32(IELEM)=(-(8*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)
     & *X2*UY-4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY
     & +((F3-3*F4)*Y3+Y2*F3)*X2*UY+((F3-3*F4)*Y3+Y2*F3)*X3*UY-
     & 4*(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(Y3-
     & 2*Y2)*UX-(Y3*F3-3*Y3*F4+Y2*F3)*(Y3+Y2)*UX))*AUX108
      A41(IELEM)=(-(X2*UY*Y3*F3-3*X2*UY*Y3*F4-2*X2*UY*Y3*F2-2
     & *X2*UY*Y2*F3+15*X2*UY*Y2*F4-5*X2*UY*Y2*F2-5*X3*UY*Y3*
     & F3+15*X3*UY*Y3*F4-2*X3*UY*Y3*F2-2*X3*UY*Y2*F3-3*X3*UY
     & *Y2*F4+X3*UY*Y2*F2+5*UX*Y3**2*F3-15*UX*Y3**2*F4+2*UX*
     & Y3**2*F2+UX*Y3*Y2*F3+6*UX*Y3*Y2*F4+UX*Y3*Y2*F2+2*UX*Y2
     & **2*F3-15*UX*Y2**2*F4+5*UX*Y2**2*F2))*AUX036
      A42(IELEM)=((4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*
     & X2*UY-4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY+
     & ((F3-3*F4)*Y3+Y2*F3)*X3*UY-((F3-3*F4)*Y3+Y2*F3)*UX*Y3-
     & 4*((3*F4-F2)*Y2-Y3*F2)*X2*UY+4*((3*F4-F2)*Y2-Y3*F2)*UX
     & *Y2-4*(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*
     & (Y3-Y2)*UX))*AUX036
      A43(IELEM)=((4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*
     & X2*UY-4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY+
     & 4*((F3-3*F4)*Y3+Y2*F3)*X3*UY-4*((F3-3*F4)*Y3+Y2*F3)*UX
     & *Y3-((3*F4-F2)*Y2-Y3*F2)*X2*UY+((3*F4-F2)*Y2-Y3*F2)*UX*
     & Y2-4*(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(
     & Y3-Y2)*UX))*AUX036
C
C   DIAGONAL TERMS
C   THE SUM OF EACH COLUMN IS 0
C
        A11(IELEM) = - A21(IELEM) - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM) - A43(IELEM)
C
1     CONTINUE
C
        ELSEIF(ICOORD.EQ.2) THEN
C
C================================
C  DERIVATIVE WRT Y  =
C================================
C
        DO 2 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
C
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM)) - F1
        F3  =  F(IKLE3(IELEM)) - F1
        F4  =  F(IKLE4(IELEM)) - F1
C
        UX  =  U(IELEM)
        UY  =  V(IELEM)
C
        AUX108 = XSU108 / SURFAC(IELEM)
        AUX036 = XSUR36 / SURFAC(IELEM)
C
C   EXTRADIAGONAL TERMS
C
      A12(IELEM)=(-(X2**2*UY*F3+24*X2**2*UY*F4-8*X2**2*UY*F2-
     & X2*X3*UY*F3-15*X2*X3*UY*F4-4*X2*X3*UY*F2+2*X2*UX*Y3*F3
     & +12*X2*UX*Y3*F4-4*X2*UX*Y3*F2-X2*UX*Y2*F3-24*X2*UX*Y2*
     & F4+8*X2*UX*Y2*F2-2*X3**2*UY*F3+6*X3**2*UY*F4+4*X3**2*
     & UY*F2+2*X3*UX*Y3*F3-6*X3*UX*Y3*F4-4*X3*UX*Y3*F2-X3*UX*
     & Y2*F3+3*X3*UX*Y2*F4+8*X3*UX*Y2*F2))*AUX108
      A13(IELEM)=(-(4*X2**2*UY*F3+6*X2**2*UY*F4-2*X2**2*UY*F2
     & -4*X2*X3*UY*F3-15*X2*X3*UY*F4-X2*X3*UY*F2+8*X2*UX*Y3*
     & F3+3*X2*UX*Y3*F4-X2*UX*Y3*F2-4*X2*UX*Y2*F3-6*X2*UX*Y2*
     & F4+2*X2*UX*Y2*F2-8*X3**2*UY*F3+24*X3**2*UY*F4+X3**2*UY
     & *F2+8*X3*UX*Y3*F3-24*X3*UX*Y3*F4-X3*UX*Y3*F2-4*X3*UX*
     & Y2*F3+12*X3*UX*Y2*F4+2*X3*UX*Y2*F2))*AUX108
      A21(IELEM)=((2*X2**2*UY*F3-15*X2**2*UY*F4+5*X2**2*UY*F2
     & -5*X2*X3*UY*F3-3*X2*X3*UY*F4+4*X2*X3*UY*F2+4*X2*UX*Y3
     & *F3+6*X2*UX*Y3*F4-2*X2*UX*Y3*F2-2*X2*UX*Y2*F3+15*X2*
     & UX*Y2*F4-5*X2*UX*Y2*F2+2*X3**2*UY*F3-6*X3**2*UY*F4+8*
     & X3**2*UY*F2-2*X3*UX*Y3*F3+6*X3*UX*Y3*F4-8*X3*UX*Y3*F2+
     & X3*UX*Y2*F3-3*X3*UX*Y2*F4-2*X3*UX*Y2*F2))*AUX108
      A23(IELEM)=((8*X2**2*UY*F3-15*X2**2*UY*F4+5*X2**2*UY*F2
     & -20*X2*X3*UY*F3+33*X2*X3*UY*F4-14*X2*X3*UY*F2+16*X2*
     & UX*Y3*F3-21*X2*UX*Y3*F4+7*X2*UX*Y3*F2-8*X2*UX*Y2*F3+
     & 15*X2*UX*Y2*F4-5*X2*UX*Y2*F2+8*X3**2*UY*F3-24*X3**2*UY
     & *F4+17*X3**2*UY*F2-8*X3*UX*Y3*F3+24*X3*UX*Y3*F4-17*X3
     & *UX*Y3*F2+4*X3*UX*Y2*F3-12*X3*UX*Y2*F4+7*X3*UX*Y2*F2))*AUX108
      A31(IELEM)=((8*X2**2*UY*F3-6*X2**2*UY*F4+2*X2**2*UY*F2+
     & 4*X2*X3*UY*F3-3*X2*X3*UY*F4-5*X2*X3*UY*F2-2*X2*UX*Y3*
     & F3-3*X2*UX*Y3*F4+X2*UX*Y3*F2-8*X2*UX*Y2*F3+6*X2*UX*Y2*
     & F4-2*X2*UX*Y2*F2+5*X3**2*UY*F3-15*X3**2*UY*F4+2*X3**2
     & *UY*F2-5*X3*UX*Y3*F3+15*X3*UX*Y3*F4-2*X3*UX*Y3*F2-2*
     & X3*UX*Y2*F3+6*X3*UX*Y2*F4+4*X3*UX*Y2*F2))*AUX108
      A32(IELEM)=((17*X2**2*UY*F3-24*X2**2*UY*F4+8*X2**2*UY*
     & F2-14*X2*X3*UY*F3+33*X2*X3*UY*F4-20*X2*X3*UY*F2+7*X2*
     & UX*Y3*F3-12*X2*UX*Y3*F4+4*X2*UX*Y3*F2-17*X2*UX*Y2*F3+
     & 24*X2*UX*Y2*F4-8*X2*UX*Y2*F2+5*X3**2*UY*F3-15*X3**2*UY
     & *F4+8*X3**2*UY*F2-5*X3*UX*Y3*F3+15*X3*UX*Y3*F4-8*X3*
     & UX*Y3*F2+7*X3*UX*Y2*F3-21*X3*UX*Y2*F4+16*X3*UX*Y2*F2))*AUX108
      A41(IELEM)=(-(2*X2**2*UY*F3-15*X2**2*UY*F4+5*X2**2*UY*
     & F2+X2*X3*UY*F3+6*X2*X3*UY*F4+X2*X3*UY*F2-2*X2*UX*Y3*F3-
     & 3*X2*UX*Y3*F4+X2*UX*Y3*F2-2*X2*UX*Y2*F3+15*X2*UX*Y2*F4-
     & 5*X2*UX*Y2*F2+5*X3**2*UY*F3-15*X3**2*UY*F4+2*X3**2*UY*
     & F2-5*X3*UX*Y3*F3+15*X3*UX*Y3*F4-2*X3*UX*Y3*F2+X3*UX*Y2
     & *F3-3*X3*UX*Y2*F4-2*X3*UX*Y2*F2))*AUX036
      A42(IELEM)=(-(8*X2**2*UY*F3-24*X2**2*UY*F4+8*X2**2*UY*
     & F2-11*X2*X3*UY*F3+24*X2*X3*UY*F4-8*X2*X3*UY*F2+7*X2*
     & UX*Y3*F3-12*X2*UX*Y3*F4+4*X2*UX*Y3*F2-8*X2*UX*Y2*F3+
     & 24*X2*UX*Y2*F4-8*X2*UX*Y2*F2+5*X3**2*UY*F3-15*X3**2*UY
     & *F4+8*X3**2*UY*F2-5*X3*UX*Y3*F3+15*X3*UX*Y3*F4-8*X3*
     & UX*Y3*F2+4*X3*UX*Y2*F3-12*X3*UX*Y2*F4+4*X3*UX*Y2*F2))*AUX036
      A43(IELEM)=(-(8*X2**2*UY*F3-15*X2**2*UY*F4+5*X2**2*UY*
     & F2-8*X2*X3*UY*F3+24*X2*X3*UY*F4-11*X2*X3*UY*F2+4*X2*
     & UX*Y3*F3-12*X2*UX*Y3*F4+4*X2*UX*Y3*F2-8*X2*UX*Y2*F3+
     & 15*X2*UX*Y2*F4-5*X2*UX*Y2*F2+8*X3**2*UY*F3-24*X3**2*UY
     & *F4+8*X3**2*UY*F2-8*X3*UX*Y3*F3+24*X3*UX*Y3*F4-8*X3*
     & UX*Y3*F2+4*X3*UX*Y2*F3-12*X3*UX*Y2*F4+7*X3*UX*Y2*F2))*AUX036
C
C   DIAGONAL TERMS
C   THE SUM OF EACH COLUMN IS 0
C
        A11(IELEM) = - A21(IELEM) - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM) - A43(IELEM)
C
2       CONTINUE
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(0)
          STOP
        ENDIF
C
C
      ELSEIF(IELMU.EQ.11) THEN
C
C================================
C  DERIVATIVE WRT X  =
C================================
C
        IF(ICOORD.EQ.1) THEN
C
C   LOOP ON THE ELEMENTS
C
        DO 3 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
C
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        F4 = F(IKLE4(IELEM)) - F1
C
        U1 = U(IKLE1(IELEM))
        U2 = U(IKLE2(IELEM))
        U3 = U(IKLE3(IELEM))
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
C
        AX1296 = XS1296 / SURFAC(IELEM)
        AUX432 = XSU432 / SURFAC(IELEM)
C
C   EXTRADIAGONAL TERMS
C
      A12(IELEM)=((((F3-3*F4)*Y3+Y2*F3)*(5*V3+2*V2+5*V1)*X2-
     & 2*((F3-3*F4)*Y3+Y2*F3)*(5*V3+2*V2+5*V1)*X3+2*((3*F4
     & -F2)*Y2-Y3*F2)*(5*V3+26*V2+17*V1)*X2-((3*F4-F2)*Y2-Y3
     & *F2)*(5*V3+26*V2+17*V1)*X3+(Y3*F3-3*Y3*F4+Y2*F3)*(2*
     & Y3-Y2)*(5*U3+2*U2+5*U1)-(Y3*F2-3*Y2*F4+Y2*F2)*(Y3-2*
     & Y2)*(5*U3+26*U2+17*U1)))*AX1296
      A13(IELEM)=((((F3-3*F4)*Y3+Y2*F3)*(26*V3+5*V2+17*V1)*
     & X2-2*((F3-3*F4)*Y3+Y2*F3)*(26*V3+5*V2+17*V1)*X3+2*(
     & (3*F4-F2)*Y2-Y3*F2)*(2*V3+5*V2+5*V1)*X2-((3*F4-F2)*
     & Y2-Y3*F2)*(2*V3+5*V2+5*V1)*X3+(Y3*F3-3*Y3*F4+Y2*F3)*(
     & 2*Y3-Y2)*(26*U3+5*U2+17*U1)-(Y3*F2-3*Y2*F4+Y2*F2)*(Y3
     & -2*Y2)*(2*U3+5*U2+5*U1)))*AX1296
      A21(IELEM)=(-(((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*(
     & 5*V3+5*V2+2*V1)*X2-2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2
     & *F2)*Y3)*(5*V3+5*V2+2*V1)*X3-((3*F4-F2)*Y2-Y3*F2)*(5
     & *V3+17*V2+26*V1)*X2-((3*F4-F2)*Y2-Y3*F2)*(5*V3+17*V2
     & +26*V1)*X3-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2
     & *F2)*(2*Y3-Y2)*(5*U3+5*U2+2*U1)-(Y3*F2-3*Y2*F4+Y2*F2
     & )*(Y3+Y2)*(5*U3+17*U2+26*U1)))*AX1296
      A23(IELEM)=(-(((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*(
     & 26*V3+17*V2+5*V1)*X2-2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+
     & 2*F2)*Y3)*(26*V3+17*V2+5*V1)*X3-((3*F4-F2)*Y2-Y3*F2)*
     & (2*V3+5*V2+5*V1)*X2-((3*F4-F2)*Y2-Y3*F2)*(2*V3+5*V2
     & +5*V1)*X3-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*
     & F2)*(2*Y3-Y2)*(26*U3+17*U2+5*U1)-(Y3*F2-3*Y2*F4+Y2*
     & F2)*(Y3+Y2)*(2*U3+5*U2+5*U1)))*AX1296
      A31(IELEM)=(-(2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)
     & *(5*V3+5*V2+2*V1)*X2-((2*F3-3*F4+F2)*Y2-(F3-3*F4+2
     & *F2)*Y3)*(5*V3+5*V2+2*V1)*X3+((F3-3*F4)*Y3+Y2*F3)*(
     & 17*V3+5*V2+26*V1)*X2+((F3-3*F4)*Y3+Y2*F3)*(17*V3+5*
     & V2+26*V1)*X3-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-
     & Y2*F2)*(Y3-2*Y2)*(5*U3+5*U2+2*U1)-(Y3*F3-3*Y3*F4+Y2*
     & F3)*(Y3+Y2)*(17*U3+5*U2+26*U1)))*AX1296
      A32(IELEM)=(-(2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)
     & *(17*V3+26*V2+5*V1)*X2-((2*F3-3*F4+F2)*Y2-(F3-3*F4+
     & 2*F2)*Y3)*(17*V3+26*V2+5*V1)*X3+((F3-3*F4)*Y3+Y2*F3)*
     & (5*V3+2*V2+5*V1)*X2+((F3-3*F4)*Y3+Y2*F3)*(5*V3+2*V2
     & +5*V1)*X3-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*
     & F2)*(Y3-2*Y2)*(17*U3+26*U2+5*U1)-(Y3*F3-3*Y3*F4+Y2*
     & F3)*(Y3+Y2)*(5*U3+2*U2+5*U1)))*AX1296
      A41(IELEM)=((((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*(5
     & *V3+5*V2+2*V1)*X2-((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)
     & *Y3)*(5*V3+5*V2+2*V1)*X3+((F3-3*F4)*Y3+Y2*F3)*(17*V3
     & +5*V2+26*V1)*X3-((F3-3*F4)*Y3+Y2*F3)*(17*U3+5*U2+26
     & *U1)*Y3-((3*F4-F2)*Y2-Y3*F2)*(5*V3+17*V2+26*V1)*X2+((
     & 3*F4-F2)*Y2-Y3*F2)*(5*U3+17*U2+26*U1)*Y2-(Y3*F3-3*Y3*
     & F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(Y3-Y2)*(5*U3+5*U2
     & +2*U1)))*AUX432
      A42(IELEM)=((((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*(
     & 17*V3+26*V2+5*V1)*X2-((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*
     & F2)*Y3)*(17*V3+26*V2+5*V1)*X3+((F3-3*F4)*Y3+Y2*F3)*(
     & 5*V3+2*V2+5*V1)*X3-((F3-3*F4)*Y3+Y2*F3)*(5*U3+2*U2+
     & 5*U1)*Y3-((3*F4-F2)*Y2-Y3*F2)*(5*V3+26*V2+17*V1)*X2+(
     & (3*F4-F2)*Y2-Y3*F2)*(5*U3+26*U2+17*U1)*Y2-(Y3*F3-3*
     & Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(Y3-Y2)*(17*U3+
     & 26*U2+5*U1)))*AUX432
      A43(IELEM)=((((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*(
     & 26*V3+17*V2+5*V1)*X2-((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*
     & F2)*Y3)*(26*V3+17*V2+5*V1)*X3+((F3-3*F4)*Y3+Y2*F3)*(
     & 26*V3+5*V2+17*V1)*X3-((F3-3*F4)*Y3+Y2*F3)*(26*U3+5*
     & U2+17*U1)*Y3-((3*F4-F2)*Y2-Y3*F2)*(2*V3+5*V2+5*V1)*
     & X2+((3*F4-F2)*Y2-Y3*F2)*(2*U3+5*U2+5*U1)*Y2-(Y3*F3-3
     & *Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(Y3-Y2)*(26*U3+
     & 17*U2+5*U1)))*AUX432
C
C   DIAGONAL TERMS
C   THE SUM OF EACH COLUMN IS 0
C
        A11(IELEM) = - A21(IELEM) - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM) - A43(IELEM)
C
3     CONTINUE
C
        ELSEIF(ICOORD.EQ.2) THEN
C
C================================
C  DERIVATIVE WRT Y  =
C================================
C
        DO 4 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
C
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM)) - F1
        F3  =  F(IKLE3(IELEM)) - F1
        F4  =  F(IKLE4(IELEM)) - F1
C
        U1  =  U(IKLE1(IELEM))
        U2  =  U(IKLE2(IELEM))
        U3  =  U(IKLE3(IELEM))
        V1  =  V(IKLE1(IELEM))
        V2  =  V(IKLE2(IELEM))
        V3  =  V(IKLE3(IELEM))
C
        AX1296 = XS1296 / SURFAC(IELEM)
        AUX432 = XSU432 / SURFAC(IELEM)
C
C   EXTRADIAGONAL TERMS
C
      A12(IELEM)=(-(((2*Y3-Y2)*(5*U3+2*U2+5*U1)*F3-(F3+3*F4
     & )*(5*V3+2*V2+5*V1)*X3)*X2+((Y3-2*Y2)*(3*F4-F2)*(5*
     & U3+26*U2+17*U1)-(3*F4+F2)*(5*V3+26*V2+17*V1)*X3)*X2
     & +(2*Y3-Y2)*(F3-3*F4)*(5*U3+2*U2+5*U1)*X3-(Y3-2*Y2)*
     & (5*U3+26*U2+17*U1)*X3*F2-2*(F3-3*F4)*(5*V3+2*V2+5
     & *V1)*X3**2+2*(3*F4-F2)*(5*V3+26*V2+17*V1)*X2**2+(5*
     & V3+26*V2+17*V1)*X3**2*F2+(5*V3+2*V2+5*V1)*X2**2*F3))*AX1296
      A13(IELEM)=(-(((2*Y3-Y2)*(26*U3+5*U2+17*U1)*F3-(F3+3*
     & F4)*(26*V3+5*V2+17*V1)*X3)*X2+((Y3-2*Y2)*(3*F4-F2)*(
     & 2*U3+5*U2+5*U1)-(3*F4+F2)*(2*V3+5*V2+5*V1)*X3)*X2+(
     & 2*Y3-Y2)*(F3-3*F4)*(26*U3+5*U2+17*U1)*X3-(Y3-2*Y2)*(
     & 2*U3+5*U2+5*U1)*X3*F2-2*(F3-3*F4)*(26*V3+5*V2+17*
     & V1)*X3**2+2*(3*F4-F2)*(2*V3+5*V2+5*V1)*X2**2+(26*V3
     & +5*V2+17*V1)*X2**2*F3+(2*V3+5*V2+5*V1)*X3**2*F2))*AX1296
      A21(IELEM)=((((2*Y3-Y2)*(2*F3-3*F4+F2)*(5*U3+5*U2+2*
     & U1)-(5*F3-9*F4+4*F2)*(5*V3+5*V2+2*V1)*X3)*X2+((Y3+
     & Y2)*(3*F4-F2)*(5*U3+17*U2+26*U1)-(3*F4-2*F2)*(5*V3
     & +17*V2+26*V1)*X3)*X2-(2*Y3-Y2)*(F3-3*F4+2*F2)*(5*U3
     & +5*U2+2*U1)*X3-(Y3+Y2)*(5*U3+17*U2+26*U1)*X3*F2+(2*
     & F3-3*F4+F2)*(5*V3+5*V2+2*V1)*X2**2+2*(F3-3*F4+2*F2
     & )*(5*V3+5*V2+2*V1)*X3**2-(3*F4-F2)*(5*V3+17*V2+26*
     & V1)*X2**2+(5*V3+17*V2+26*V1)*X3**2*F2))*AX1296
      A23(IELEM)=((((2*Y3-Y2)*(2*F3-3*F4+F2)*(26*U3+17*U2+
     & 5*U1)-(5*F3-9*F4+4*F2)*(26*V3+17*V2+5*V1)*X3)*X2+((
     & Y3+Y2)*(3*F4-F2)*(2*U3+5*U2+5*U1)-(3*F4-2*F2)*(2*
     & V3+5*V2+5*V1)*X3)*X2-(2*Y3-Y2)*(F3-3*F4+2*F2)*(26*
     & U3+17*U2+5*U1)*X3-(Y3+Y2)*(2*U3+5*U2+5*U1)*X3*F2+(2
     & *F3-3*F4+F2)*(26*V3+17*V2+5*V1)*X2**2+2*(F3-3*F4+2
     & *F2)*(26*V3+17*V2+5*V1)*X3**2-(3*F4-F2)*(2*V3+5*V2+
     & 5*V1)*X2**2+(2*V3+5*V2+5*V1)*X3**2*F2))*AX1296
      A31(IELEM)=(-(((Y3+Y2)*(17*U3+5*U2+26*U1)*F3-(2*F3-3*
     & F4)*(17*V3+5*V2+26*V1)*X3)*X2-((Y3-2*Y2)*(2*F3-3*F4
     & +F2)*(5*U3+5*U2+2*U1)-(4*F3-9*F4+5*F2)*(5*V3+5*V2
     & +2*V1)*X3)*X2+(Y3+Y2)*(F3-3*F4)*(17*U3+5*U2+26*U1)*
     & X3+(Y3-2*Y2)*(F3-3*F4+2*F2)*(5*U3+5*U2+2*U1)*X3-2*
     & (2*F3-3*F4+F2)*(5*V3+5*V2+2*V1)*X2**2-(F3-3*F4+2*
     & F2)*(5*V3+5*V2+2*V1)*X3**2-(F3-3*F4)*(17*V3+5*V2+
     & 26*V1)*X3**2-(17*V3+5*V2+26*V1)*X2**2*F3))*AX1296
      A32(IELEM)=(-(((Y3+Y2)*(5*U3+2*U2+5*U1)*F3-(2*F3-3*F4
     & )*(5*V3+2*V2+5*V1)*X3)*X2-((Y3-2*Y2)*(2*F3-3*F4+F2)
     & *(17*U3+26*U2+5*U1)-(4*F3-9*F4+5*F2)*(17*V3+26*V2
     & +5*V1)*X3)*X2+(Y3+Y2)*(F3-3*F4)*(5*U3+2*U2+5*U1)*X3+
     & (Y3-2*Y2)*(F3-3*F4+2*F2)*(17*U3+26*U2+5*U1)*X3-2*(
     & 2*F3-3*F4+F2)*(17*V3+26*V2+5*V1)*X2**2-(F3-3*F4+2*
     & F2)*(17*V3+26*V2+5*V1)*X3**2-(F3-3*F4)*(5*V3+2*V2+
     & 5*V1)*X3**2-(5*V3+2*V2+5*V1)*X2**2*F3))*AX1296
      A41(IELEM)=(-(((Y3-Y2)*(2*F3-3*F4+F2)*(5*U3+5*U2+2*U1
     & )-3*(F3-2*F4+F2)*(5*V3+5*V2+2*V1)*X3)*X2+((3*F4-F2)
     & *(5*U3+17*U2+26*U1)*Y2+(5*V3+17*V2+26*V1)*X3*F2)*X2
     & +((17*V3+5*V2+26*V1)*X3-(17*U3+5*U2+26*U1)*Y3)*X2*
     & F3-(Y3-Y2)*(F3-3*F4+2*F2)*(5*U3+5*U2+2*U1)*X3+(2*F3
     & -3*F4+F2)*(5*V3+5*V2+2*V1)*X2**2+(F3-3*F4+2*F2)*(5
     & *V3+5*V2+2*V1)*X3**2+(F3-3*F4)*(17*V3+5*V2+26*V1)*
     & X3**2-(F3-3*F4)*(17*U3+5*U2+26*U1)*X3*Y3-(3*F4-F2)*(
     & 5*V3+17*V2+26*V1)*X2**2-(5*U3+17*U2+26*U1)*X3*Y2*F2))*AUX432
      A42(IELEM)=(-(((Y3-Y2)*(2*F3-3*F4+F2)*(17*U3+26*U2+5*
     & U1)-3*(F3-2*F4+F2)*(17*V3+26*V2+5*V1)*X3)*X2+((3*F4
     & -F2)*(5*U3+26*U2+17*U1)*Y2+(5*V3+26*V2+17*V1)*X3*F2
     & )*X2+((5*V3+2*V2+5*V1)*X3-(5*U3+2*U2+5*U1)*Y3)*X2*
     & F3-(Y3-Y2)*(F3-3*F4+2*F2)*(17*U3+26*U2+5*U1)*X3+(2*
     & F3-3*F4+F2)*(17*V3+26*V2+5*V1)*X2**2+(F3-3*F4+2*F2)
     & *(17*V3+26*V2+5*V1)*X3**2+(F3-3*F4)*(5*V3+2*V2+5*
     & V1)*X3**2-(F3-3*F4)*(5*U3+2*U2+5*U1)*X3*Y3-(3*F4-F2)
     & *(5*V3+26*V2+17*V1)*X2**2-(5*U3+26*U2+17*U1)*X3*Y2*
     & F2))*AUX432
      A43(IELEM)=(-(((Y3-Y2)*(2*F3-3*F4+F2)*(26*U3+17*U2+5*
     & U1)-3*(F3-2*F4+F2)*(26*V3+17*V2+5*V1)*X3)*X2+((3*F4
     & -F2)*(2*U3+5*U2+5*U1)*Y2+(2*V3+5*V2+5*V1)*X3*F2)*X2
     & +((26*V3+5*V2+17*V1)*X3-(26*U3+5*U2+17*U1)*Y3)*X2*
     & F3-(Y3-Y2)*(F3-3*F4+2*F2)*(26*U3+17*U2+5*U1)*X3+(2*
     & F3-3*F4+F2)*(26*V3+17*V2+5*V1)*X2**2+(F3-3*F4+2*F2)
     & *(26*V3+17*V2+5*V1)*X3**2+(F3-3*F4)*(26*V3+5*V2+17
     & *V1)*X3**2-(F3-3*F4)*(26*U3+5*U2+17*U1)*X3*Y3-(3*F4-
     & F2)*(2*V3+5*V2+5*V1)*X2**2-(2*U3+5*U2+5*U1)*X3*Y2*F2))*AUX432
C
C   DIAGONAL TERMS
C   THE SUM OF EACH COLUMN IS 0
C
        A11(IELEM) = - A21(IELEM) - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM) - A43(IELEM)
C
4       CONTINUE
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(0)
          STOP
        ENDIF
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,300) IELMU
        IF (LNG.EQ.2) WRITE(LU,301) IELMU
        CALL PLANTE(0)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
       IF (LNG.EQ.1) WRITE(LU,100) IELMF
       IF (LNG.EQ.2) WRITE(LU,101) IELMF
100    FORMAT(1X,'MT12BA (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,' NON PREVUE')
101    FORMAT(1X,'MT12BA (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
       CALL PLANTE(0)
       STOP
      ENDIF
C
200       FORMAT(1X,'MT12BA (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'MT12BA (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
C
300    FORMAT(1X,'MT12BA (BIEF) :',/,
     &        1X,'DISCRETISATION DE U : ',1I6,' NON PREVUE')
301    FORMAT(1X,'MT12BA (BIEF) :',/,
     &        1X,'DISCRETIZATION OF U : ',1I6,' NOT AVAILABLE')
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C