C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE SUPG MATRIX:
!>  @code
!>            ->--->        ->--->
!>           (U.GRAD(PI))* (U.GRAD(PJ))  WITH<br>
!>          PI OF QUASI-BUBBLE DISCRETISATION
!>          PJ OF QUASI-BUBBLE DISCRETISATION
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A14, A22, A23, A24, A33, A34, A44, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SU, SV, U, V, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ANS1, ANS2, IELEM, IELMU, IELMV, U1, U2, U3, U4, V1, V2, V3, V4, X2, X3, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT04BB
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
!> </td><td> 10/01/95
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
!>          <tr><td>A14
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A23
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A24
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A33
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A34
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A44
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
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
                        SUBROUTINE MT04BB
     &( A11 , A12 , A13 , A14 ,
     &        A22 , A23 , A24 ,
     &              A33 , A34 ,
     &                    A44 ,
     &  XMUL,SU,SV,U,V,XEL,YEL,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A14            |---| 
C| A22            |---| 
C| A23            |---| 
C| A24            |---| 
C| A33            |---| 
C| A34            |---| 
C| A44            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
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
      USE BIEF, EX_MT04BB => MT04BB
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
C
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) ::                      A44(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
C
C     STRUCTURES OF      U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SU,SV
C
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELMU,IELMV,IELEM
C
      DOUBLE PRECISION X2,X3,Y2,Y3,ANS1,ANS2
      DOUBLE PRECISION U1,U2,U3,U4
      DOUBLE PRECISION V1,V2,V3,V4
C
C=======================================================================
C
C     EXTRACTS THE TYPE OF ELEMENT FOR VELOCITY
C
      IELMU = SU%ELM
      IELMV = SV%ELM
C
C-----------------------------------------------------------------------
C
      IF(IELMU.EQ.11.AND.IELMV.EQ.11) THEN
C
C-----------------------------------------------------------------------
C
C  P1 DISCRETISATION OF THE VELOCITY:
C
      DO 4 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         X2  =  XEL(IELEM,2)
         X3  =  XEL(IELEM,3)
C
         Y2  =  YEL(IELEM,2)
         Y3  =  YEL(IELEM,3)
C
         U1 = U(IKLE1(IELEM))
         U2 = U(IKLE2(IELEM))
         U3 = U(IKLE3(IELEM))
         V1 = V(IKLE1(IELEM))
         V2 = V(IKLE2(IELEM))
         V3 = V(IKLE3(IELEM))
C
C
C   INITIALISES THE INTERMEDIATE VARIABLES
C
C
C  COMPUTES 6 OF THE 16 TERMS (SELECTED AMONG THE LEAST COMPLEX)
C
         A11(IELEM)=
     &  (X2**2*(17*V3**2+25*V3*V2+37*V3*V1+53*V2**2
     &  +73*V2*V1+65*V1**2)+8*X2*X3*(-7*V3**2-5*V3*V2-11*V3
     &  *V1-7*V2**2-11*V2*V1-13*V1**2)+X2*Y2*(-34*V3*U3-25*
     &  V3*U2-37*V3*U1-25*V2*U3-106*V2*U2-73*V2*U1-37*V1*U3-
     &  73*V1*U2-130*V1*U1)+4*X2*Y3*(14*V3*U3+5*V3*U2+11*V3*
     &  U1+5*V2*U3+14*V2*U2+11*V2*U1+11*V1*U3+11*V1*U2+26*
     &  V1*U1)+X3**2*(53*V3**2+25*V3*V2+73*V3*V1+17*V2**2+37
     &  *V2*V1+65*V1**2)+4*X3*Y2*(14*V3*U3+5*V3*U2+11*V3*U1+
     &  5*V2*U3+14*V2*U2+11*V2*U1+11*V1*U3+11*V1*U2+26*V1*U1
     &  )+X3*Y3*(-106*V3*U3-25*V3*U2-73*V3*U1-25*V2*U3-34*V2
     &  *U2-37*V2*U1-73*V1*U3-37*V1*U2-130*V1*U1)+Y2**2*(17*
     &  U3**2+25*U3*U2+37*U3*U1+53*U2**2+73*U2*U1+65*U1**2)+
     &  8*Y2*Y3*(-7*U3**2-5*U3*U2-11*U3*U1-7*U2**2-11*U2*U1-
     &  13*U1**2)+Y3**2*(53*U3**2+25*U3*U2+73*U3*U1+17*U2**2+
     &  37*U2*U1+65*U1**2))*XMUL/(324*(X2*Y3-X3*Y2))
C
         A12(IELEM)=
     &  (4*X2**2*(5*(V2+V1)*V3+V3**2+13*V2**2+17*V2
     &  *V1+13*V1**2)+X2*(2*(5*(V2+V1)*V3+V3**2+13*V2**2+17*
     &  V2*V1+13*V1**2)*X3-(5*U3+26*U2+17*U1)*(Y3+4*Y2)*V2-(
     &  5*U3+17*U2+26*U1)*(Y3+4*Y2)*V1-(2*U3+5*U2+5*U1)*(Y3
     &  +4*Y2)*V3)-2*X3**2*(5*(V2+V1)*V3+V3**2+13*V2**2+17*
     &  V2*V1+13*V1**2)+X3*((5*U3+26*U2+17*U1)*V2+(5*U3+17*
     &  U2+26*U1)*V1+(2*U3+5*U2+5*U1)*V3)*(2*Y3-Y2)+2*(Y3+
     &  Y2)*(Y3-2*Y2)*(-5*(U2+U1)*U3-U3**2-13*U2**2-17*U2*U1-
     &  13*U1**2))*XMUL/(648*(X2*Y3-X3*Y2))
C
         A13(IELEM)=
     &  (-2*X2**2*((5*V2+17*V1)*V3+13*V3**2+V2**2+
     &  5*V2*V1+13*V1**2)+X2*(2*((5*V2+17*V1)*V3+13*V3**2+V2
     &  **2+5*V2*V1+13*V1**2)*X3-(26*U3+5*U2+17*U1)*(Y3-2*
     &  Y2)*V3-(17*U3+5*U2+26*U1)*(Y3-2*Y2)*V1-(5*U3+2*U2+
     &  5*U1)*(Y3-2*Y2)*V2)+4*X3**2*((5*V2+17*V1)*V3+13*V3**
     &  2+V2**2+5*V2*V1+13*V1**2)-X3*((26*U3+5*U2+17*U1)*V3+
     &  (17*U3+5*U2+26*U1)*V1+(5*U3+2*U2+5*U1)*V2)*(4*Y3+
     &  Y2)+2*(2*Y3-Y2)*(Y3+Y2)*((5*U2+17*U1)*U3+13*U3**2+U2
     &  **2+5*U2*U1+13*U1**2))*XMUL/(648*(X2*Y3-X3*Y2))
C
         A22(IELEM)=
     &  (2*X2**2*(7*V3**2+11*V3*V2+5*V3*V1+13*V2**
     &  2+11*V2*V1+7*V1**2)+2*X2*X3*(-25*V3**2-29*V3*V2-5*
     &  V3*V1-13*V2**2+7*V2*V1+11*V1**2)+2*X2*Y2*(-14*V3*U3-
     &  11*V3*U2-5*V3*U1-11*V2*U3-26*V2*U2-11*V2*U1-5*V1*U3-
     &  11*V1*U2-14*V1*U1)+X2*Y3*(50*V3*U3+29*V3*U2+5*V3*U1+
     &  29*V2*U3+26*V2*U2-7*V2*U1+5*V1*U3-7*V1*U2-22*V1*U1)+
     &  X3**2*(53*V3**2+73*V3*V2+25*V3*V1+65*V2**2+37*V2*V1+
     &  17*V1**2)+X3*Y2*(50*V3*U3+29*V3*U2+5*V3*U1+29*V2*U3+
     &  26*V2*U2-7*V2*U1+5*V1*U3-7*V1*U2-22*V1*U1)+X3*Y3*(-
     &  106*V3*U3-73*V3*U2-25*V3*U1-73*V2*U3-130*V2*U2-37*V2
     &  *U1-25*V1*U3-37*V1*U2-34*V1*U1)+2*Y2**2*(7*U3**2+11
     &  *U3*U2+5*U3*U1+13*U2**2+11*U2*U1+7*U1**2)+2*Y2*Y3*(-
     &  25*U3**2-29*U3*U2-5*U3*U1-13*U2**2+7*U2*U1+11*U1**2)
     &  +Y3**2*(53*U3**2+73*U3*U2+25*U3*U1+65*U2**2+37*U2*U1
     &  +17*U1**2))*XMUL/(324*(X2*Y3-X3*Y2))
C
         A23(IELEM)=
     &  (-((10*((17*V2+5*V1)*V3+13*V3**2+13*V2**2+
     &  5*V2*V1+V1**2)*X3-(26*U3+17*U2+5*U1)*(5*Y3-4*Y2)*V3-
     &  (17*U3+26*U2+5*U1)*(5*Y3-4*Y2)*V2-(5*U3+5*U2+2*U1
     &  )*(5*Y3-4*Y2)*V1)*X2-4*((17*V2+5*V1)*V3+13*V3**2+
     &  13*V2**2+5*V2*V1+V1**2)*X2**2-4*((17*V2+5*V1)*V3+13*
     &  V3**2+13*V2**2+5*V2*V1+V1**2)*X3**2+((26*U3+17*U2+5*
     &  U1)*V3+(17*U3+26*U2+5*U1)*V2+(5*U3+5*U2+2*U1)*V1)*(
     &  4*Y3-5*Y2)*X3-2*(17*U2+5*U1)*(2*Y3-Y2)*(Y3-2*Y2)*U3
     &  -26*(2*Y3-Y2)*(Y3-2*Y2)*U3**2-26*(2*Y3-Y2)*(Y3-2*Y2
     &  )*U2**2-10*(2*Y3-Y2)*(Y3-2*Y2)*U2*U1-2*(2*Y3-Y2)*(Y3
     &  -2*Y2)*U1**2))*XMUL/(648*(X2*Y3-X3*Y2))
C
         A33(IELEM)=
     &  (X2**2*(65*V3**2+73*V3*V2+37*V3*V1+53*V2**2
     &  +25*V2*V1+17*V1**2)+2*X2*X3*(-13*V3**2-29*V3*V2+7*
     &  V3*V1-25*V2**2-5*V2*V1+11*V1**2)+X2*Y2*(-130*V3*U3-
     &  73*V3*U2-37*V3*U1-73*V2*U3-106*V2*U2-25*V2*U1-37*V1*
     &  U3-25*V1*U2-34*V1*U1)+X2*Y3*(26*V3*U3+29*V3*U2-7*V3*
     &  U1+29*V2*U3+50*V2*U2+5*V2*U1-7*V1*U3+5*V1*U2-22*V1*
     &  U1)+2*X3**2*(13*V3**2+11*V3*V2+11*V3*V1+7*V2**2+5*
     &  V2*V1+7*V1**2)+X3*Y2*(26*V3*U3+29*V3*U2-7*V3*U1+29*
     &  V2*U3+50*V2*U2+5*V2*U1-7*V1*U3+5*V1*U2-22*V1*U1)+2*
     &  X3*Y3*(-26*V3*U3-11*V3*U2-11*V3*U1-11*V2*U3-14*V2*U2
     &  -5*V2*U1-11*V1*U3-5*V1*U2-14*V1*U1)+Y2**2*(65*U3**2+
     &  73*U3*U2+37*U3*U1+53*U2**2+25*U2*U1+17*U1**2)+2*Y2*
     &  Y3*(-13*U3**2-29*U3*U2+7*U3*U1-25*U2**2-5*U2*U1+11*
     &  U1**2)+2*Y3**2*(13*U3**2+11*U3*U2+11*U3*U1+7*U2**2+
     &  5*U2*U1+7*U1**2))*XMUL/(324*(X2*Y3-X3*Y2))
C
C
C USES HERE THE 'MAGIC SQUARE' PROPERTIES
C (SUM OF EACH LINE = SUM OF EACH COLUMN = 0)
C
         A14(IELEM) = - A11(IELEM) - A12(IELEM) - A13(IELEM)
C
         A24(IELEM) = - A12(IELEM) - A22(IELEM) - A23(IELEM)
C
         A34(IELEM) = - A13(IELEM) - A23(IELEM) - A33(IELEM)
C
         A44(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
C
4     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMU.EQ.12.AND.IELMV.EQ.12) THEN
C
C-----------------------------------------------------------------------
C
C  QUASI-BUBBLE DISCRETISATION OF THE VELOCITY:
C
      DO 5 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         X2  =  XEL(IELEM,2)
         X3  =  XEL(IELEM,3)
C
         Y2  =  YEL(IELEM,2)
         Y3  =  YEL(IELEM,3)
C
         U1 = U(IKLE1(IELEM))
         U2 = U(IKLE2(IELEM))
         U3 = U(IKLE3(IELEM))
         U4 = U(IKLE4(IELEM))
         V1 = V(IKLE1(IELEM))
         V2 = V(IKLE2(IELEM))
         V3 = V(IKLE3(IELEM))
         V4 = V(IKLE4(IELEM))
C
C   INITIALISES THE INTERMEDIATE VARIABLES
C
C
C  COMPUTES 6 OF THE 16 TERMS (SELECTED AMONG THE LEAST COMPLEX)
C
         A12(IELEM)=
     &    ((2*((V2+V1)*V4+V4**2+V2**2+V2*V1+V1**2)*X3-(
     &    2*U4+U2+U1)*(Y3+4*Y2)*V4-(U4+2*U2+U1)*(Y3+4*Y2)*V2-(U4
     &    +U2+2*U1)*(Y3+4*Y2)*V1)*X2+2*((V2+V1)*V4+V4**2+V2**2+
     &    V2*V1+V1**2)*(2*X2**2-X3**2)+((2*U4+U2+U1)*V4+(U4+2*U2
     &    +U1)*V2+(U4+U2+2*U1)*V1)*(2*Y3-Y2)*X3-2*(U4**2+U2**2+
     &    U2*U1+U1**2)*(Y3+Y2)*(Y3-2*Y2)-2*(U2+U1)*(Y3+Y2)*(Y3-2
     &    *Y2)*U4)*XMUL/(72*(X2*Y3-X3*Y2))
C
         A13(IELEM)=
     &  (-(2*X2**2)*(V3**2+V3*V4+V3*V1+V4**2+V4*V1+V1
     &  **2)+2*X2*X3*(V3**2+V3*V4+V3*V1+V4**2+V4*V1+V1**2)+2*X2
     &  *Y2*(2*V3*U3+V3*U4+V3*U1+V4*U3+2*V4*U4+V4*U1+V1*U3+V1*
     &  U4+2*V1*U1)+X2*Y3*(-2*V3*U3-V3*U4-V3*U1-V4*U3-2*V4*U4-
     &  V4*U1-V1*U3-V1*U4-2*V1*U1)+4*X3**2*(V3**2+V3*V4+V3*V1+
     &  V4**2+V4*V1+V1**2)+X3*Y2*(-2*V3*U3-V3*U4-V3*U1-V4*U3-2*
     &  V4*U4-V4*U1-V1*U3-V1*U4-2*V1*U1)+4*X3*Y3*(-2*V3*U3-V3*
     &  U4-V3*U1-V4*U3-2*V4*U4-V4*U1-V1*U3-V1*U4-2*V1*U1)-(2*
     &  Y2**2)*(U3**2+U3*U4+U3*U1+U4**2+U4*U1+U1**2)+2*Y2*Y3*(U3
     &  **2+U3*U4+U3*U1+U4**2+U4*U1+U1**2)+4*Y3**2*(U3**2+U3*U4+
     &  U3*U1+U4**2+U4*U1+U1**2))*XMUL/(72*X2*Y3-72*X3*Y2)
C
         A14(IELEM)=
     &  (-(4*X2**2)*(V4**2+V4*V2+V4*V1+V2**2+V2*V1+V1
     &  **2)+2*X2*X3*(V3**2+V3*V4+V3*V1+2*V4**2+V4*V2+2*V4*V1+
     &  V2**2+V2*V1+2*V1**2)+4*X2*Y2*(2*V4*U4+V4*U2+V4*U1+V2*
     &  U4+2*V2*U2+V2*U1+V1*U4+V1*U2+2*V1*U1)+X2*Y3*(-2*V3*U3-
     &  V3*U4-V3*U1-V4*U3-4*V4*U4-V4*U2-2*V4*U1-V2*U4-2*V2*U2-
     &  V2*U1-V1*U3-2*V1*U4-V1*U2-4*V1*U1)-(4*X3**2)*(V3**2+V3
     &  *V4+V3*V1+V4**2+V4*V1+V1**2)+X3*Y2*(-2*V3*U3-V3*U4-V3*U1
     &  -V4*U3-4*V4*U4-V4*U2-2*V4*U1-V2*U4-2*V2*U2-V2*U1-V1*U3
     &  -2*V1*U4-V1*U2-4*V1*U1)+4*X3*Y3*(2*V3*U3+V3*U4+V3*U1+
     &  V4*U3+2*V4*U4+V4*U1+V1*U3+V1*U4+2*V1*U1)-(4*Y2**2)*(U4
     &  **2+U4*U2+U4*U1+U2**2+U2*U1+U1**2)+2*Y2*Y3*(U3**2+U3*U4+
     &  U3*U1+2*U4**2+U4*U2+2*U4*U1+U2**2+U2*U1+2*U1**2)-(4*
     &  Y3**2)*(U3**2+U3*U4+U3*U1+U4**2+U4*U1+U1**2))
     &  *XMUL/(24*X2*Y3-24*X3*Y2)
C
      A23(IELEM)=
     &  (-((10*((V4+V2)*V3+V3**2+V4**2+V4*V2+V2**2)*X3
     &  -(2*U3+U4+U2)*(5*Y3-4*Y2)*V3-(U3+2*U4+U2)*(5*Y3-4*
     &  Y2)*V4-(U3+U4+2*U2)*(5*Y3-4*Y2)*V2)*X2-4*((V4+V2)*V3+
     &  V3**2+V4**2+V4*V2+V2**2)*X2**2-4*((V4+V2)*V3+V3**2+V4**2
     &  +V4*V2+V2**2)*X3**2+((2*U3+U4+U2)*V3+(U3+2*U4+U2)*V4+(
     &  U3+U4+2*U2)*V2)*(4*Y3-5*Y2)*X3-2*(U4+U2)*(2*Y3-Y2)*(
     &  Y3-2*Y2)*U3-2*(2*Y3-Y2)*(Y3-2*Y2)*U3**2-2*(2*Y3-Y2)
     &  *(Y3-2*Y2)*U4**2-2*(2*Y3-Y2)*(Y3-2*Y2)*U4*U2-2*(2*
     &  Y3-Y2)*(Y3-2*Y2)*U2**2))*XMUL/(72*(X2*Y3-X3*Y2))
C
      ANS2=-(4*Y3**2)*(U3**2+U3*U4+U3*U2+U4**2+U4*U2+U2**2)
C
      ANS1=2*X2**2*(-V3**2-V3*V4-V3*V2-2*V4**2-2*V4*V2-V4*V1-
     & 2*V2**2-V2*V1-V1**2)+2*X2*X3*(3*V3**2+3*V3*V4+3*V3*V2
     & +2*V4**2+2*V4*V2-V4*V1+2*V2**2-V2*V1-V1**2)+2*X2*Y2*(
     & 2*V3*U3+V3*U4+V3*U2+V4*U3+4*V4*U4+2*V4*U2+V4*U1+V2*U3+
     & 2*V2*U4+4*V2*U2+V2*U1+V1*U4+V1*U2+2*V1*U1)+X2*Y3*(-6*
     & V3*U3-3*V3*U4-3*V3*U2-3*V4*U3-4*V4*U4-2*V4*U2+V4*U1-
     & 3*V2*U3-2*V2*U4-4*V2*U2+V2*U1+V1*U4+V1*U2+2*V1*U1)-(4
     & *X3**2)*(V3**2+V3*V4+V3*V2+V4**2+V4*V2+V2**2)+X3*Y2*(-6*
     & V3*U3-3*V3*U4-3*V3*U2-3*V4*U3-4*V4*U4-2*V4*U2+V4*U1-
     & 3*V2*U3-2*V2*U4-4*V2*U2+V2*U1+V1*U4+V1*U2+2*V1*U1)+4*
     & X3*Y3*(2*V3*U3+V3*U4+V3*U2+V4*U3+2*V4*U4+V4*U2+V2*U3+V2
     & *U4+2*V2*U2)+2*Y2**2*(-U3**2-U3*U4-U3*U2-2*U4**2-2*U4
     & *U2-U4*U1-2*U2**2-U2*U1-U1**2)+2*Y2*Y3*(3*U3**2+3*U3*
     & U4+3*U3*U2+2*U4**2+2*U4*U2-U4*U1+2*U2**2-U2*U1-U1**2)
     & +ANS2
C
         A24(IELEM)= ANS1*XMUL/(24*X2*Y3-24*X3*Y2)
C
      ANS2=2*Y3**2*(-2*U3**2-2*U3*U4-U3*U2-U3*U1-2*U4**2-U4*
     & U2-U4*U1-U2**2-U1**2)
C
      ANS1=-(4*X2**2)*(V3**2+V3*V4+V3*V2+V4**2+V4*V2+V2**2)+2*
     & X2*X3*(2*V3**2+2*V3*V4+3*V3*V2-V3*V1+2*V4**2+3*V4*V2
     & -V4*V1+3*V2**2-V1**2)+4*X2*Y2*(2*V3*U3+V3*U4+V3*U2+V4*
     & U3+2*V4*U4+V4*U2+V2*U3+V2*U4+2*V2*U2)+X2*Y3*(-4*V3*U3-
     & 2*V3*U4-3*V3*U2+V3*U1-2*V4*U3-4*V4*U4-3*V4*U2+V4*U1-
     & 3*V2*U3-3*V2*U4-6*V2*U2+V1*U3+V1*U4+2*V1*U1)+2*X3**2*
     & (-2*V3**2-2*V3*V4-V3*V2-V3*V1-2*V4**2-V4*V2-V4*V1-V2**
     & 2-V1**2)+X3*Y2*(-4*V3*U3-2*V3*U4-3*V3*U2+V3*U1-2*V4*
     & U3-4*V4*U4-3*V4*U2+V4*U1-3*V2*U3-3*V2*U4-6*V2*U2+V1*
     & U3+V1*U4+2*V1*U1)+2*X3*Y3*(4*V3*U3+2*V3*U4+V3*U2+V3*
     & U1+2*V4*U3+4*V4*U4+V4*U2+V4*U1+V2*U3+V2*U4+2*V2*U2+V1*
     & U3+V1*U4+2*V1*U1)-(4*Y2**2)*(U3**2+U3*U4+U3*U2+U4**2+U4
     & *U2+U2**2)+2*Y2*Y3*(2*U3**2+2*U3*U4+3*U3*U2-U3*U1+2*
     & U4**2+3*U4*U2-U4*U1+3*U2**2-U1**2)+ANS2
C
         A34(IELEM)=ANS1*XMUL/(24*X2*Y3-24*X3*Y2)
C
C
C USES HERE THE 'MAGIC SQUARE' PROPERTIES TO GET THE DIAGONAL TERMS
C (SUM OF EACH LINE = SUM OF EACH COLUMN = 0)
C
         A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
C
         A22(IELEM) = - A12(IELEM) - A23(IELEM) - A24(IELEM)
C
         A33(IELEM) = - A13(IELEM) - A23(IELEM) - A34(IELEM)
C
         A44(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
C
5     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSE
       IF(IELMU.EQ.IELMV) THEN
       IF (LNG.EQ.1) WRITE(LU,100) IELMU
       IF (LNG.EQ.2) WRITE(LU,101) IELMU
100    FORMAT(1X,'MT04BB (BIEF) :',/,
     &        1X,'DISCRETISATION DE U ET V : ',1I6,' NON PREVUE')
101    FORMAT(1X,'MT04BB (BIEF) :',/,
     &        1X,'DISCRETIZATION OF U AND V : ',1I6,' NOT AVAILABLE')
       ELSE
       IF (LNG.EQ.1) WRITE(LU,200) IELMU,IELMV
       IF (LNG.EQ.2) WRITE(LU,201) IELMU,IELMV
200    FORMAT(1X,'MT04BB (BIEF) :',/,
     &        1X,'U ET V DE DISCRETISATIONS DIFFERENTES :',1I6,3X,1I6)
201    FORMAT(1X,'MT04BB (BIEF) :',/,
     &        1X,'U AND V OF A DIFFERENT DISCRETISATION:',1I6,3X,1I6)
       ENDIF
C
       CALL PLANTE(0)
       STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C