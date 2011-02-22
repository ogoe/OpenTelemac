C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code
!>                 /  -->   - -->          -->  --->
!> A(I,J) = XMUL  /    U  . GRAD(PSI1(I)) * U . GRAD(PSI2(J)) D(OMEGA)
!>               /OMEGA<br>
!>  U: VECTOR WITH COMPONENTS U, V
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
!>    </th><td> A11, A12, A13, A22, A23, A33, IKLE, NELEM, NELMAX, SU, SURFAC, SV, U, V, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ANS1, ANS2, ANS3, AUX, AUX1, AUX2, AUX3, IELEM, IELMU, IELMV, SUR144, SUR48, SUR720, U1, U123, U2, U3, U4, U5, U6, V1, V123, V2, V3, V4, V5, V6, X2, X3, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT04AA
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 19/06/08
!> </td><td> ALGIANE FROEHLY (MATMECA) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 12/04/93
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
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
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A23
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A33
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
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
                        SUBROUTINE MT04AA
     &( A11 , A12 , A13 ,
     &        A22 , A23 ,
     &              A33 ,
     &  XMUL,SU,SV,U,V,XEL,YEL,SURFAC,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A22            |---| 
C| A23            |---| 
C| A33            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE           |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
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
      USE BIEF, EX_MT04AA => MT04AA
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL,U(*),V(*)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: SU,SV
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELMU,IELMV,IELEM
C
      DOUBLE PRECISION SUR48,X2,X3,Y2,Y3,U1,U2,U3,U4,U5,U6
      DOUBLE PRECISION V1,V2,V3,V4,V5,V6,AUX
      DOUBLE PRECISION AUX1,AUX2,AUX3
      DOUBLE PRECISION SUR144,SUR720,U123,V123,ANS1,ANS2,ANS3
C
C-----------------------------------------------------------------------
C
      SUR48  = XMUL/48.D0
      SUR144 = XMUL/144.D0
      SUR720 = XMUL/720.D0
C
C-----------------------------------------------------------------------
C
      IELMU = SU%ELM
      IELMV = SV%ELM
C
C  CASE WHERE U AND V ARE LINEAR
C
      IF(IELMU.EQ.11.AND.IELMV.EQ.11) THEN
C
C   LOOP ON THE ELEMENTS
C
      DO 1 IELEM = 1 , NELEM
C
      X2  =   XEL(IELEM,2)
      X3  =   XEL(IELEM,3)
      Y2  =   YEL(IELEM,2)
      Y3  =   YEL(IELEM,3)
C
      U1   =  U(IKLE(IELEM,1))
      U2   =  U(IKLE(IELEM,2))
      U3   =  U(IKLE(IELEM,3))
C
      V1   =  V(IKLE(IELEM,1))
      V2   =  V(IKLE(IELEM,2))
      V3   =  V(IKLE(IELEM,3))
C
      U123 = U1 + U2 + U3
      V123 = V1 + V2 + V3
C
      AUX = SUR48 / SURFAC(IELEM)
C
      AUX1 = U1*U123+U2**2+U2*U3+U3**2
      AUX2 = U1*(V123+V1)+U2*(V123+V2)+U3*(V123+V3)
      AUX3 = V1*V123+V2**2+V2*V3+V3**2
C
      A12(IELEM) = (  2*Y3*(Y2-Y3)            *AUX1
     &                +(X3*(Y3-Y2)+(X3-X2)*Y3)*AUX2
     &                +2*X3*(X2-X3)           *AUX3 ) * AUX
C
      A13(IELEM) = (  2*Y2*(Y3-Y2)            *AUX1
     &                +(2*X2*Y2-X2*Y3-X3*Y2)  *AUX2
     &                +2*X2*(X3-X2)           *AUX3 ) * AUX
C
      A23(IELEM) = ( -2*Y2*Y3                 *AUX1
     &                +(X2*Y3+X3*Y2)          *AUX2
     &                -2*X2*X3                *AUX3 ) * AUX
C
C  USES HERE THE 'MAGIC SQUARE' PROPERTIES
C  (SUM OF EACH LINE = 0 FOR EXAMPLE)
C  AND THE SYMMETRICAL PROPERTIES OF THE MATRIX
C
      A11(IELEM) = - A12(IELEM) - A13(IELEM)
      A22(IELEM) = - A23(IELEM) - A12(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C  CASE WHERE U AND V ARE QUASI-BUBBLE
C
      ELSEIF(IELMU.EQ.12.AND.IELMV.EQ.12) THEN
C
C   LOOP ON THE ELEMENTS
C
      DO 2 IELEM = 1 , NELEM
C
      X2  =   XEL(IELEM,2)
      X3  =   XEL(IELEM,3)
      Y2  =   YEL(IELEM,2)
      Y3  =   YEL(IELEM,3)
C
      U1   =  U(IKLE(IELEM,1))
      U2   =  U(IKLE(IELEM,2))
      U3   =  U(IKLE(IELEM,3))
      U4   =  U(IKLE(IELEM,4))
C
      V1   =  V(IKLE(IELEM,1))
      V2   =  V(IKLE(IELEM,2))
      V3   =  V(IKLE(IELEM,3))
      V4   =  V(IKLE(IELEM,4))
C
      AUX = SUR144 / SURFAC(IELEM)
C
      A12(IELEM) = (X2*((2*((V4+V2)*V3+V3**2+V4**2+V4*V2+V2**2)*
     & X3-(2*U3+U4+U2)*V3*Y3-(U3+2*U4+U2)*V4*Y3-(U3+U4+2*U2)*
     & V2*Y3)+(2*((V4+V1)*V3+V3**2+V4**2+V4*V1+V1**2)*X3-(2*U3
     & +U4+U1)*V3*Y3-(U3+2*U4+U1)*V4*Y3-(U3+U4+2*U1)*V1*Y3)+(
     & 2*((V2+V1)*V4+V4**2+V2**2+V2*V1+V1**2)*X3-(2*U4+U2+U1)*
     & V4*Y3-(U4+2*U2+U1)*V2*Y3-(U4+U2+2*U1)*V1*Y3))-(2*X3**2
     & )*(((V4+V2)*V3+V3**2+V4**2+V4*V2+V2**2)+((V4+V1)*V3+V3**2
     & +V4**2+V4*V1+V1**2)+((V2+V1)*V4+V4**2+V2**2+V2*V1+V1**2))
     & +X3*(2*Y3-Y2)*(((2*U3+U4+U2)*V3+(U3+2*U4+U2)*V4+(U3+U4
     & +2*U2)*V2)+((2*U3+U4+U1)*V3+(U3+2*U4+U1)*V4+(U3+U4+2*
     & U1)*V1)+((2*U4+U2+U1)*V4+(U4+2*U2+U1)*V2+(U4+U2+2*U1)*
     & V1))+2*Y3*(Y3-Y2)*(-(U4+U2)*U3-(U4+U1)*U3-(U2+U1)*U4-2*
     & U3**2-3*U4**2-U4*U2-U4*U1-2*U2**2-U2*U1-2*U1**2))*AUX
C
      A13(IELEM) = (-(2*X2**2)*(((V4+V2)*V3+V3**2+V4**2+V4*V2+V2
     & **2)+((V4+V1)*V3+V3**2+V4**2+V4*V1+V1**2)+((V2+V1)*V4+V4
     & **2+V2**2+V2*V1+V1**2))+X2*((2*((V4+V2)*V3+V3**2+V4**2+
     & V4*V2+V2**2)*X3-(2*U3+U4+U2)*(Y3-2*Y2)*V3-(U3+2*U4+U2)
     & *(Y3-2*Y2)*V4-(U3+U4+2*U2)*(Y3-2*Y2)*V2)+(2*((V4+V1)*
     & V3+V3**2+V4**2+V4*V1+V1**2)*X3-(2*U3+U4+U1)*(Y3-2*Y2)*
     & V3-(U3+2*U4+U1)*(Y3-2*Y2)*V4-(U3+U4+2*U1)*(Y3-2*Y2)*
     & V1)+(2*((V2+V1)*V4+V4**2+V2**2+V2*V1+V1**2)*X3-(2*U4+U2
     & +U1)*(Y3-2*Y2)*V4-(U4+2*U2+U1)*(Y3-2*Y2)*V2-(U4+U2+2*
     & U1)*(Y3-2*Y2)*V1))-(X3*Y2)*(((2*U3+U4+U2)*V3+(U3+2*U4+
     & U2)*V4+(U3+U4+2*U2)*V2)+((2*U3+U4+U1)*V3+(U3+2*U4+U1)*
     & V4+(U3+U4+2*U1)*V1)+((2*U4+U2+U1)*V4+(U4+2*U2+U1)*V2+(
     & U4+U2+2*U1)*V1))+2*Y2*(Y3-Y2)*((U4+U2)*U3+(U4+U1)*U3+(
     & U2+U1)*U4+2*U3**2+3*U4**2+U4*U2+U4*U1+2*U2**2+U2*U1+2
     & *U1**2))*AUX
C
      A23(IELEM) = (2*X2*X3*(-2*V3**2-2*V3*V4-V3*V2-V3*V1-3*V4
     & **2-2*V4*V2-2*V4*V1-2*V2**2-V2*V1-2*V1**2)+X2*Y3*(4*
     & V3*U3+2*V3*U4+V3*U2+V3*U1+2*V4*U3+6*V4*U4+2*V4*U2+2*
     & V4*U1+V2*U3+2*V2*U4+4*V2*U2+V2*U1+V1*U3+2*V1*U4+V1*U2+
     & 4*V1*U1)+X3*Y2*(4*V3*U3+2*V3*U4+V3*U2+V3*U1+2*V4*U3+6
     & *V4*U4+2*V4*U2+2*V4*U1+V2*U3+2*V2*U4+4*V2*U2+V2*U1+V1
     & *U3+2*V1*U4+V1*U2+4*V1*U1)+2*Y2*Y3*(-2*U3**2-2*U3*U4
     & -U3*U2-U3*U1-3*U4**2-2*U4*U2-2*U4*U1-2*U2**2-U2*U1-2
     & *U1**2))*AUX

C
C  USES HERE THE 'MAGIC SQUARE' PROPERTIES
C  (SUM OF EACH LINE = 0 FOR EXAMPLE)
C  AND THE SYMMETRICAL PROPERTIES OF THE MATRIX
C
      A11(IELEM) = - A12(IELEM) - A13(IELEM)
      A22(IELEM) = - A23(IELEM) - A12(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
2     CONTINUE
C
C
C-----------------------------------------------------------------------
C  CASE WHERE U AND V ARE P2
C
      ELSEIF(IELMU.EQ.13.AND.IELMV.EQ.13) THEN
C
C   LOOP ON THE ELEMENTS
C
      DO 3 IELEM = 1 , NELEM
C
      X2  =   XEL(IELEM,2)
      X3  =   XEL(IELEM,3)
      Y2  =   YEL(IELEM,2)
      Y3  =   YEL(IELEM,3)
C
      U1   =  U(IKLE(IELEM,1))
      U2   =  U(IKLE(IELEM,2))
      U3   =  U(IKLE(IELEM,3))
      U4   =  U(IKLE(IELEM,4))
      U5   =  U(IKLE(IELEM,5))
      U6   =  U(IKLE(IELEM,6))
C
      V1   =  V(IKLE(IELEM,1))
      V2   =  V(IKLE(IELEM,2))
      V3   =  V(IKLE(IELEM,3))
      V4   =  V(IKLE(IELEM,4))
      V5   =  V(IKLE(IELEM,5))
      V6   =  V(IKLE(IELEM,6))
C
      AUX = SUR720 / SURFAC(IELEM)
C
      ANS1 = 3.D0*U2**2*Y2**2-4.D0*U4*Y3**2*U3+V2*X2*U3*Y2-
     &       4.D0*V1*X3**2*V5+U2*Y3*V3*X3-U2*Y3**2*U3-4.D0*V4*X3**2*V3-
     &       V2*X2**2*V3-4.D0*V2*X2**2*V6-V2*X2**2*V1-V2*X3**2*V1-
     &       4.D0*U4*Y2**2*U3-V1*X2**2*V3-6.D0*U3**2*Y3*Y2-
     &       32.D0*U5**2*Y2*Y3+16.D0*U4*Y3**2*U6+16.D0*V6*X3**2*V5+
     &       16.D0*U6*Y3**2*U5+16.D0*V6**2*X3**2+16.D0*U4*Y2**2*U5-
     &       4.D0*U1*Y3**2*U5-32.D0*U6**2*Y2*Y3+3.D0*V1**2*X3**2+
     &       16.D0*V4*X2**2*V5-4.D0*V1*X2**2*V5+16.D0*V5**2*X3**2-
     &       6.D0*U1**2*Y3*Y2-U1*Y3**2*U3-32.D0*V6**2*X3*X2-
     &       32.D0*U4**2*Y2*Y3+16.D0*U4*Y2**2*U6+16.D0*V4**2*X2**2-
     &       4.D0*U2*Y3**2*U6+16.D0*U5**2*Y3**2+3.D0*V2**2*X3**2-
     &       6.D0*V1**2*X2*X3+3.D0*U3**2*Y3**2+8.D0*U2*Y2*U6*Y3+
     &       2.D0*V2*X2*V3*X3+2.D0*U2*Y2*U3*Y3-U2*Y2*V1*X3-
     &       6.D0*V3**2*X2*X3+16.D0*V4*X3**2*V6-4.D0*U2*Y2*V6*X3+
     &       3.D0*V3**2*X3**2+16.D0*U4**2*Y3**2-6.D0*U2**2*Y2*Y3-
     &       6.D0*V2**2*X2*X3+16.D0*V4*X2**2*V6-4.D0*V4*X2**2*V3+
     &       6.D0*U2*Y2*V2*X3+6.D0*V2*X2*U2*Y3+32.D0*U5*Y2*V5*X3+
     &       8.D0*U4*Y3*U3*Y2-6.D0*U1*Y3*V1*X3+4.D0*V4*X3*U3*Y3-
     &       16.D0*V4*X3*U6*Y3+U1*Y3*V3*X3-16.D0*U4*Y3*V5*X3
      ANS2 = -32.D0*V4*X3*U4*Y3-16.D0*V4*X3*U5*Y3-6.D0*V3*X3*U3*Y3-
     &       32.D0*U5*Y3*V5*X3+4.D0*U4*Y3*V3*X3-16.D0*U4*Y3*V6*X3-
     &       16.D0*U6*Y3*V5*X3+4.D0*U1*Y3*V5*X3+4.D0*V1*X3*U5*Y3-
     &       32.D0*V6*X3*U6*Y3-16.D0*V6*X3*U5*Y3+V1*X3*U3*Y3+
     &       4.D0*V2*X3*U6*Y3+4.D0*U2*Y3*V6*X3+U2*Y3*V1*X3+V2*X3*U1*Y3+
     &       V2*X3*U3*Y3+3.D0*U1**2*Y3**2+3.D0*U3**2*Y2**2+
     &       3.D0*V3**2*X2**2-6.D0*U2*Y3*V2*X3-4.D0*U4*Y3*V3*X2-
     &       32.D0*U4*Y3*U6*Y2+32.D0*V6*X3*U6*Y2-32.D0*V5**2*X2*X3+
     &       3.D0*U2**2*Y3**2+16.D0*V4**2*X3**2+16.D0*U6*Y3*V5*X2+
     &       8.D0*U1*Y3*U5*Y2-4.D0*U1*Y3*V5*X2-32.D0*U6*Y2*U5*Y3-
     &       4.D0*U1*Y2*V5*X3+16.D0*U6*Y2*V5*X3-4.D0*V1*X3*U5*Y2+
     &       32.D0*V6*X2*U6*Y3+16.D0*V6*X2*U5*Y3-32.D0*V6*X3*V5*X2-
     &       4.D0*V1*X2*U5*Y3+8.D0*V1*X2*V5*X3+16.D0*V6*X3*U5*Y2-
     &       V1*X3*U3*Y2+6.D0*U1*Y2*V1*X3-V2*X3*U1*Y2-U1*Y2*V3*X3-
     &       4.D0*V2*X3*U6*Y2-U2*Y3*V1*X2-4.D0*U2*Y3*V6*X2-V2*X3*U3*Y2-
     &       4.D0*U4*Y2*V3*X3+16.D0*U4*Y2*V6*X3-U2*Y3*V3*X2-
     &       V1*X2*U3*Y3+8.D0*V2*X2*V6*X3+16.D0*U6**2*Y3**2-
     &       V2*X2*U3*Y3-U2*Y2*V3*X3+2.D0*U2*Y2*U1*Y3-V2*X2*U1*Y3+
     &       2.D0*V2*X2*V1*X3-4.D0*V2*X2*U6*Y3-32.D0*V4*X2*V5*X3
      ANS3 = 16.D0*V4*X2*U5*Y3+32.D0*V4*X2*U4*Y3+2.D0*U1*Y3*U3*Y2-
     &       U1*Y3*V3*X2+16.D0*U4*Y3*V6*X2+16.D0*V4*X2*U6*Y3+
     &       16.D0*V4*X3*U6*Y2+8.D0*V4*X2*V3*X3-32.D0*V4*X2*V6*X3-
     &       4.D0*V4*X2*U3*Y3+2.D0*V1*X2*V3*X3-4.D0*V4*X3*U3*Y2+
     &       6.D0*U1*Y3*V1*X2+6.D0*V3*X2*U3*Y3+16.D0*U4*Y3*V5*X2+
     &       16.D0*V4*X3*U5*Y2+16.D0*U6*Y2**2*U5-32.D0*U4*Y2*U5*Y3+
     &       32.D0*U4*Y2*V4*X3+16.D0*U4*Y2*V5*X3+16.D0*U4*Y3**2*U5-
     &       6.D0*U2*Y2*V2*X2+U2*Y2*V1*X2+4.D0*U2*Y2*V6*X2+
     &       6.D0*V3*X3*U3*Y2+32.D0*U5*Y3*V5*X2-V1*X3**2*V3
     &       -32.D0*U5*Y2*V5*X2+16.D0*V6*X2**2*V5-U1*Y2**2*U3-
     &       U2*Y2**2*U1-4.D0*U1*Y2**2*U5+3.D0*V2**2*X2**2-
     &       V2*X3**2*V3+16.D0*U6**2*Y2**2+4.D0*U1*Y2*V5*X2-
     &       16.D0*U6*Y2*V5*X2-4.D0*U2*Y2**2*U6+3.D0*V1**2*X2**2+
     &       16.D0*V6**2*X2**2-32.D0*V4**2*X2*X3+16.D0*V4*X3**2*V5+
     &       V1*X2*U3*Y2-6.D0*V1*X2*U1*Y2+4.D0*V4*X2*U3*Y2-
     &       16.D0*V4*X2*U6*Y2+U2*Y2*V3*X2+4.D0*V2*X2*U6*Y2+
     &       V2*X2*U1*Y2-16.D0*V4*X2*U5*Y2+3.D0*U1**2*Y2**2+
     &       U1*Y2*V3*X2+4.D0*U4*Y2*V3*X2-16.D0*U4*Y2*V6*X2-
     &       32.D0*V6*X2*U6*Y2-16.D0*V6*X2*U5*Y2+4.D0*V1*X2*U5*Y2
      A11(IELEM) = (16.D0*U4**2*Y2**2-U2*Y3**2*U1-4*V2*X3**2*V6-
     &             U2*Y2**2*U3+16.D0*V5**2*X2**2-32.D0*U4*Y2*V4*X2-
     &             6.D0*V3*X2*U3*Y2-16.D0*U4*Y2*V5*X2+
     &             16.D0*U5**2*Y2**2 + ANS1 + ANS2 + ANS3)*2.D0*AUX
C
      A22(IELEM) = (-4.D0*U4*Y3**2*U3-4.D0*V1*X3**2*V5+U2*Y3*V3*X3-
     &       U2*Y3**2*U3-4.D0*V4*X3**2*V3-V2*X3**2*V1+
     &       16.D0*U4*Y3**2*U6+16.D0*V6*X3**2*V5+16.D0*U6*Y3**2*U5+
     &       16.D0*V6**2*X3**2-4.D0*U1*Y3**2*U5+3*V1**2*X3**2+
     &       16.D0*V5**2*X3**2-U1*Y3**2*U3-4.D0*U2*Y3**2*U6+
     &       16.D0*U5**2*Y3**2+3.D0*V2**2*X3**2+3.D0*U3**2*Y3**2+
     &       16.D0*V4*X3**2*V6+3.D0*V3**2*X3**2+16.D0*U4**2*Y3**2-
     &       6.D0*U1*Y3*V1*X3+4.D0*V4*X3*U3*Y3-16.D0*V4*X3*U6*Y3+
     &       U1*Y3*V3*X3-16.D0*U4*Y3*V5*X3-32.D0*V4*X3*U4*Y3-
     &       16.D0*V4*X3*U5*Y3-6.D0*V3*X3*U3*Y3-32.D0*U5*Y3*V5*X3+
     &       4.D0*U4*Y3*V3*X3-16.D0*U4*Y3*V6*X3-16.D0*U6*Y3*V5*X3+
     &       4.D0*U1*Y3*V5*X3+4.D0*V1*X3*U5*Y3-32.D0*V6*X3*U6*Y3-
     &       16.D0*V6*X3*U5*Y3+V1*X3*U3*Y3+4.D0*V2*X3*U6*Y3+
     &       4.D0*U2*Y3*V6*X3+U2*Y3*V1*X3+V2*X3*U1*Y3+V2*X3*U3*Y3+
     &       3.D0*U1**2*Y3**2-6.D0*U2*Y3*V2*X3+3.D0*U2**2*Y3**2+
     &       16.D0*V4**2*X3**2+16.D0*U6**2*Y3**2+16.D0*U4*Y3**2*U5-
     &       V1*X3**2*V3-V2*X3**2*V3+16.D0*V4*X3**2*V5-U2*Y3**2*U1-
     &       4.D0*V2*X3**2*V6)*2.D0*AUX
C
      ANS1 = 6.D0*U3**2*Y3*Y2+32.D0*U5**2*Y2*Y3+32.D0*U6**2*Y2*Y3+
     &       6.D0*U1**2*Y3*Y2+32.D0*V6**2*X3*X2+32.D0*U4**2*Y2*Y3+
     &       6.D0*V1**2*X2*X3-8.D0*U2*Y2*U6*Y3-2.D0*V2*X2*V3*X3-
     &       2.D0*U2*Y2*U3*Y3+U2*Y2*V1*X3+6.D0*V3**2*X2*X3+
     &       4.D0*U2*Y2*V6*X3+6.D0*U2**2*Y2*Y3+6.D0*V2**2*X2*X3-
     &       6.D0*U2*Y2*V2*X3-6.D0*V2*X2*U2*Y3-32.D0*U5*Y2*V5*X3-
     &       8.D0*U4*Y3*U3*Y2+4.D0*U4*Y3*V3*X2+32.D0*U4*Y3*U6*Y2-
     &       32.D0*V6*X3*U6*Y2+32.D0*V5**2*X2*X3-16.D0*U6*Y3*V5*X2-
     &       8.D0*U1*Y3*U5*Y2+4.D0*U1*Y3*V5*X2+32.D0*U6*Y2*U5*Y3+
     &       4.D0*U1*Y2*V5*X3-16.D0*U6*Y2*V5*X3+4.D0*V1*X3*U5*Y2-
     &       32.D0*V6*X2*U6*Y3-16.D0*V6*X2*U5*Y3+32.D0*V6*X3*V5*X2+
     &       4.D0*V1*X2*U5*Y3-8.D0*V1*X2*V5*X3-16.D0*V6*X3*U5*Y2+
     &       V1*X3*U3*Y2-6.D0*U1*Y2*V1*X3+V2*X3*U1*Y2+U1*Y2*V3*X3+
     &       4.D0*V2*X3*U6*Y2+U2*Y3*V1*X2+4.D0*U2*Y3*V6*X2+V2*X3*U3*Y2+
     &       4.D0*U4*Y2*V3*X3-16.D0*U4*Y2*V6*X3+U2*Y3*V3*X2+V1*X2*U3*Y3-
     &       8.D0*V2*X2*V6*X3+V2*X2*U3*Y3+U2*Y2*V3*X3-2.D0*U2*Y2*U1*Y3+
     &       V2*X2*U1*Y3-2.D0*V2*X2*V1*X3+4.D0*V2*X2*U6*Y3+
     &       32.D0*V4*X2*V5*X3-16.D0*V4*X2*U5*Y3-32.D0*V4*X2*U4*Y3-
     &       2.D0*U1*Y3*U3*Y2+U1*Y3*V3*X2-16.D0*U4*Y3*V6*X2
      A23(IELEM) = -(-16.D0*V4*X2*U6*Y3-16.D0*V4*X3*U6*Y2-
     &             8.D0*V4*X2*V3*X3+32.D0*V4*X2*V6*X3+
     &             4.D0*V4*X2*U3*Y3-2.D0*V1*X2*V3*X3+
     &             4.D0*V4*X3*U3*Y2-6.D0*U1*Y3*V1*X2-
     &             6.D0*V3*X2*U3*Y3-16.D0*U4*Y3*V5*X2-
     &             16.D0*V4*X3*U5*Y2+32.D0*U4*Y2*U5*Y3-
     &             32.D0*U4*Y2*V4*X3-16.D0*U4*Y2*V5*X3-
     &             6.D0*V3*X3*U3*Y2-32.D0*U5*Y3*V5*X2+
     &             32.D0*V4**2*X2*X3 + ANS1)*AUX
C
C  USES HERE THE 'MAGIC SQUARE' PROPERTIES
C  (SUM OF EACH LINE = 0 FOR EXAMPLE)
C  AND THE SYMMETRICAL PROPERTIES OF THE MATRIX
C
      A12(IELEM) = - A22(IELEM) - A23(IELEM)
      A13(IELEM) = - A11(IELEM) - A12(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
3     CONTINUE
C
C
C     OTHER TYPES OF DISCRETISATION FOR U
C
C-----------------------------------------------------------------------
C
      ELSE
C
       IF(IELMU.EQ.IELMV) THEN
       IF (LNG.EQ.1) WRITE(LU,100) IELMU
       IF (LNG.EQ.2) WRITE(LU,101) IELMU
100    FORMAT(1X,'MT04AA (BIEF) :',/,
     &        1X,'DISCRETISATION DE U ET V : ',1I6,' NON PREVUE')
101    FORMAT(1X,'MT04AA (BIEF) :',/,
     &        1X,'DISCRETIZATION OF U AND V : ',1I6,' NOT AVAILABLE')
       ELSE
       IF (LNG.EQ.1) WRITE(LU,200) IELMU,IELMV
       IF (LNG.EQ.2) WRITE(LU,201) IELMU,IELMV
200    FORMAT(1X,'MT04AA (BIEF) :',/,
     &        1X,'U ET V DE DISCRETISATIONS DIFFERENTES :',1I6,3X,1I6)
201    FORMAT(1X,'MT04AA (BIEF) :',/,
     &        1X,'U AND V OF A DIFFERENT DISCRETISATION:',1I6,3X,1I6)
       ENDIF
C
       CALL PLANTE(1)
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