C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE MATVGR MATRIX.
!>  @code
!>     STORAGE CONVENTION FOR EXTRA-DIAGONAL TERMS:<br>
!>     XM(IELEM, 1)  ---->  M(1,2)
!>     XM(IELEM, 2)  ---->  M(1,3)
!>     XM(IELEM, 3)  ---->  M(1,4)
!>     XM(IELEM, 4)  ---->  M(2,3)
!>     XM(IELEM, 5)  ---->  M(2,4)
!>     XM(IELEM, 6)  ---->  M(3,4)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, NELEM, NELMAX, SU, SV, SW, T, U, V, W, X, XM, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, I3, I4, IELEM, IELMU, U1, U2, U3, U4, V1, V2, V3, V4, W1, W2, W3, W4, X2, X3, X4, XSUR120, Y2, Y3, Y4, Z2, Z3, Z4
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT05TT
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
!>      <td><center> 5.4                                       </center>
!> </td><td> 04/01/02
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
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
!>          <tr><td>SU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT05TT
     &( T,XM,XMUL,SU,SV,SW,U,V,W,X,Y,Z,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| IKLE           |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SU             |---| 
C| SV             |---| 
C| SW             |---| 
C| T             |---| 
C| U             |---| 
C| V             |---| 
C| W             |---| 
C| X,Y,Z          |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XM             |---| 
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT05TT => MT05TT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,4)
C
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,4),XM(NELMAX,12)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
C
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
C
C     STRUCTURES OF U, V
C
      TYPE(BIEF_OBJ),   INTENT(IN) :: SU, SV, SW
      DOUBLE PRECISION, INTENT(IN) :: U(*), V(*), W(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     SPECIFIC DECLARATIONS
C
      DOUBLE PRECISION X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4,W1,W2,W3,W4
      INTEGER I1,I2,I3,I4,IELEM,IELMU
C
      DOUBLE PRECISION XSUR120
C
C***********************************************************************
C
      XSUR120=XMUL/120.D0
C
C-----------------------------------------------------------------------
C
      IELMU = SU%ELM

C      U P1
      IF(IELMU.EQ.31) THEN
C AND IELMV=11 AND IELMW=11

C-----------------------------------------------------------------------
C     LOOP ON THE TETRAHEDRONS
C
      DO 20 IELEM=1,NELEM
C
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
C
      U1 = U(I1)
      U2 = U(I2)
      U3 = U(I3)
      U4 = U(I4)
      V1 = V(I1)
      V2 = V(I2)
      V3 = V(I3)
      V4 = V(I4)
      W1 = W(I1)
      W2 = W(I2)
      W3 = W(I3)
      W4 = W(I4)
C
C-----------------------------------------------------------------------
C
      X2=X(I2)-X(I1)
      Y2=Y(I2)-Y(I1)
      Z2=Z(I2)-Z(I1)
      X3=X(I3)-X(I1)
      Y3=Y(I3)-Y(I1)
      Z3=Z(I3)-Z(I1)
      X4=X(I4)-X(I1)
      Y4=Y(I4)-Y(I1)
      Z4=Z(I4)-Z(I1)
C
C     JACOBIAN : Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3)
C
C     VOLUME OF THE TETRAHEDRON:
C
C     (Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3))/6
C
C
      T(IELEM,1) = (W2*Y2*X3-W2*X2*Y3-V2*Z2*X3-U2*Z2*Y4+V2*
     &X2*Z3+U2*Z2*Y3-U4*Z2*Y4+U4*Y4*Z3-U4*Y3*Z4-V4*X
     &4*Z3-W4*Y2*X4-W4*X3*Y4-U1*Z2*Y4*2-U1*Y3*Z4*2-W1*Y2*X
     &4*2+V1*Z2*X4*2-V1*X2*Z4*2+V1*X3*Z4*2+W1*X2*Y4*2+U3*Y2*Z4+
     &U3*Y4*Z3-U3*Y3*Z4-W1*X3*Y4*2-U3*Z2*Y4+V3*Z2*X4+W3
     &*X2*Y4+W3*X4*Y3-W3*X3*Y4+U1*Y2*Z4*2+U1*Y4*Z3*2-V1*X4
     &*Z3*2+W1*X4*Y3*2-U2*Y2*Z3+W3*Y2*X3+V4*Z2*X4-W3*X2*Y3-W2*Y2*X4
     &+W2*X2*Y4+W2*X4*Y3-W2*X3*Y4+V2*X3*Z4+V2*Z2*X4
     &-V2*X2*Z4+W4*X2*Y4-V2*X4*Z3+V4*X3*Z4+U2*Y2*Z4
     &+U2*Y4*Z3-U2*Y3*Z4-V3*X2*Z4-W3*Y2*X4+U4*Z2*Y3
     &-U4*Y2*Z3-V4*Z2*X3+V4*X2*Z3+W4*Y2*X3-W4*X2*Y3
     &-U1*Y2*Z3*2-V1*Z2*X3*2+W1*Y2*X3*2+W4*X4*Y3-W1*X2*Y3*2+U3*Z
     &2*Y3-U3*Y2*Z3-V3*X4*Z3-V3*Z2*X3+U1*Z2*Y3*2+V3*X2*
     &Z3+U4*Y2*Z4+V3*X3*Z4-V4*X2*Z4+V1*X2
     &*Z3*2)*XSUR120
C
      T(IELEM,2) = (U2*Y3*Z4*2-U2*Y4*Z3*2+V2*X4*Z3*2-V2*X3*Z4*2+W2*X3*Y
     &4*2-W2*X4*Y3*2-W1*X4*Y3+V1*X4*Z3-U1*Y4*Z3+W3*X3*Y4
     &-W3*X4*Y3+V3*X4*Z3-V3*X3*Z4+U3*Y3*Z4-U3*Y4*Z3
     &-V1*X3*Z4+W1*X3*Y4+U1*Y3*Z4+W4*X3*Y4-W4*X4*Y3
     &+V4*X4*Z3-V4*X3*Z4+U4*Y3*Z4-U4*Y4*Z3)*XSUR120
C
      T(IELEM,3) = (W3*Y2*X4*2+U2*Z2*Y4-U2*Y2*Z4+V2*X2*Z4-V2*Z
     &2*X4-W2*X2*Y4+W2*Y2*X4-U1*Y2*Z4-W3*X2*Y4*2-V3*Z2*
     &X4*2+V3*X2*Z4*2+U3*Z2*Y4*2-U3*Y2*Z4*2-W1*X2*Y4+V1*X2*Z4
     &-V1*Z2*X4+W1*Y2*X4+U1*Z2*Y4-W4*X2*Y4+W4*Y2*X4
     &+V4*X2*Z4-V4*Z2*X4-U4*Y2*Z4+U4*Z2*Y4)*XSUR120
C
      T(IELEM,4) = (W3*X2*Y3-W3*Y2*X3+U2*Y2*Z3-U2*Z2*Y3-V2*
     &X2*Z3+V2*Z2*X3+W2*X2*Y3-W2*Y2*X3-V3*X2*Z3+V3*Z
     &2*X3+U3*Y2*Z3-U3*Z2*Y3+W1*X2*Y3-U1*Z2*Y3-V1*X2
     &*Z3-W1*Y2*X3+V1*Z2*X3+U1*Y2*Z3+W4*X2*Y3*2-W4*Y2*X
     &3*2-V4*X2*Z3*2+V4*Z2*X3*2+U4*Y2*Z3*2-U4*Z2*Y3*2)*XSUR120
C
      XM(IELEM,1) =
     &(U2*Y3*Z4-U2*Y4*Z3+V2*X4*Z3-V2*X3*Z4+W2*
     &X3*Y4-W2*X4*Y3-W1*X4*Y3*2+V1*X4*Z3*2-U1*Y4*Z3*2+W3*X3*Y
     &4-W3*X4*Y3+V3*X4*Z3-V3*X3*Z4+U3*Y3*Z4-U3*Y4*Z3
     &-V1*X3*Z4*2+W1*X3*Y4*2+U1*Y3*Z4*2+W4*X3*Y4-W4*X4*Y3
     &+V4*X4*Z3-V4*X3*Z4+U4*Y3*Z4-U4*Y4*Z3)*XSUR120 !MAT[2,1]
C
      XM(IELEM,2) =
     &(W3*Y2*X4+U2*Z2*Y4-U2*Y2*Z4+V2*X2*Z4-V2*
     &Z2*X4-W2*X2*Y4+W2*Y2*X4-U1*Y2*Z4*2-W3*X2*Y4-V3*Z2
     &*X4+V3*X2*Z4+U3*Z2*Y4-U3*Y2*Z4-W1*X2*Y4*2+V1*X2*Z
     &4*2-V1*Z2*X4*2+W1*Y2*X4*2+U1*Z2*Y4*2-W4*X2*Y4+W4*Y2*X4
     &+V4*X2*Z4-V4*Z2*X4-U4*Y2*Z4+U4*Z2*Y4)*XSUR120 !MAT[3,1]
C
      XM(IELEM,3) =
     &(W3*X2*Y3-W3*Y2*X3+U2*Y2*Z3-U2*Z2*Y3-V2*
     &X2*Z3+V2*Z2*X3+W2*X2*Y3-W2*Y2*X3-V3*X2*Z3+V3*Z
     &2*X3+U3*Y2*Z3-U3*Z2*Y3+W1*X2*Y3*2-U1*Z2*Y3*2-V1*X2*Z
     &3*2-W1*Y2*X3*2+V1*Z2*X3*2+U1*Y2*Z3*2+W4*X2*Y3-W4*Y2*X3
     &-V4*X2*Z3+V4*Z2*X3+U4*Y2*Z3-U4*Z2*Y3)*XSUR120 !MAT[4,1]
C
      XM(IELEM,7) = (W3*X4*Y3-V3*X2*Z4+V3*Z2*X4+U1*Y2*Z4-W4*
     &Y2*X4+W4*X2*Y4-U3*Y2*Z3-V4*X2*Z4-W1*Y2*X4+W1*X
     &2*Y4-U1*Y2*Z3-V4*X4*Z3+V4*X3*Z4+U4*Y4*Z3+U4*Z2
     &*Y3-U4*Y3*Z4+W2*Y2*X3*2+U2*Y4*Z3*2-U2*Y2*Z3*2-U2*Y3*Z4*2
     &+V1*Z2*X4+U1*Y4*Z3-V2*X2*Z4*2+U4*Y2*Z4+W4*X4*Y3
     &-U4*Z2*Y4-V3*Z2*X3+W4*Y2*X3+U2*Z2*Y3*2+U3*Y2*Z4
     &-W4*X3*Y4+W2*X4*Y3*2-W3*X3*Y4-U1*Z2*Y4+U3*Z2*Y3+W
     &2*X2*Y4*2+W1*X4*Y3-U3*Y3*Z4-U4*Y2*Z3-W2*X2*Y3*2-W4*X
     &2*Y3+V4*X2*Z3+W3*Y2*X3+V1*X2*Z3-U3*Z2*Y4+U1*Z2
     &*Y3+V2*Z2*X4*2-W2*Y2*X4*2-W3*X2*Y3+V4*Z2*X4-V1*X2*Z4
     &-W3*Y2*X4+W3*X2*Y4-U2*Z2*Y4*2+U2*Y2*Z4*2-W1*X3*Y4
     &-V1*X4*Z3+V1*X3*Z4-U1*Y3*Z4+W1*Y2*X3-V4*Z2*X3
     &-V1*Z2*X3-W2*X3*Y4*2+V3*X2*Z3-W1*X2*Y3+V2*X3*Z4*2
     &-V2*Z2*X3*2+V2*X2*Z3*2-V2*X4*Z3*2+U3*Y4*Z3-V3*X4*Z3+V3
     &*X3*Z4)*XSUR120 !MAT[1,2]
C
      XM(IELEM,4) = (-W2*X2*Y4*2+W2*Y2*X4*2+V2*X2*Z4*2
     &-V2*Z2*X4*2-V3*Z2*
     &X4+V3*X2*Z4-U3*Y2*Z4+U3*Z2*Y4-U2*Y2*Z4*2+U2*Z2*Y4
     &*2-W3*X2*Y4+W3*Y2*X4+V1*X2*Z4-V4*Z2*X4+V4*X2*Z4
     &+U4*Z2*Y4-U4*Y2*Z4-W4*X2*Y4+W4*Y2*X4-U1*Y2*Z4
     &+U1*Z2*Y4-V1*Z2*X4-W1*X2*Y4+W1*Y2*X4)*XSUR120 !MAT[3,2]
C
      XM(IELEM,5) =
     &(W3*X2*Y3-W3*Y2*X3+U2*Y2*Z3*2-U2*Z2*Y3*2-V2*X2
     &*Z3*2+V2*Z2*X3*2+W2*X2*Y3*2-W2*Y2*X3*2-V3*X2*Z3+V3*Z2*X3
     &+U3*Y2*Z3-U3*Z2*Y3+W1*X2*Y3-U1*Z2*Y3-V1*X2*Z3
     &-W1*Y2*X3+V1*Z2*X3+U1*Y2*Z3+W4*X2*Y3-W4*Y2*X3
     &-V4*X2*Z3+V4*Z2*X3+U4*Y2*Z3-U4*Z2*Y3)*XSUR120 !MAT[4,2]
C
      XM(IELEM,8) = (
     &W3*X4*Y3*2-V3*X2*Z4*2+V3*Z2*X4*2+U1*Y2*Z4-W4*Y2*
     &X4+W4*X2*Y4-U3*Y2*Z3*2-V4*X2*Z4-W1*Y2*X4+W1*X2*Y4
     &-U1*Y2*Z3-V4*X4*Z3+V4*X3*Z4+U4*Y4*Z3+U4*Z2*Y3
     &-U4*Y3*Z4+W2*Y2*X3+U2*Y4*Z3-U2*Y2*Z3-U2*Y3*Z4
     &+V1*Z2*X4+U1*Y4*Z3-V2*X2*Z4+U4*Y2*Z4+W4*X4*Y3
     &-U4*Z2*Y4-V3*Z2*X3*2+W4*Y2*X3+U2*Z2*Y3+U3*Y2*Z4*2
     &-W4*X3*Y4+W2*X4*Y3-W3*X3*Y4*2-U1*Z2*Y4+U3*Z2*Y3*2+W2
     &*X2*Y4+W1*X4*Y3-U3*Y3*Z4*2-U4*Y2*Z3-W2*X2*Y3-W4*X
     &2*Y3+V4*X2*Z3+W3*Y2*X3*2+V1*X2*Z3-U3*Z2*Y4*2+U1*Z2*Y
     &3+V2*Z2*X4-W2*Y2*X4-W3*X2*Y3*2+V4*Z2*X4-V1*X2*Z4
     &-W3*Y2*X4*2+W3*X2*Y4*2-U2*Z2*Y4+U2*Y2*Z4-W1*X3*Y4
     &-V1*X4*Z3+V1*X3*Z4-U1*Y3*Z4+W1*Y2*X3-V4*Z2*X3
     &-V1*Z2*X3-W2*X3*Y4+V3*X2*Z3*2-W1*X2*Y3+V2*X3*Z4
     &-V2*Z2*X3+V2*X2*Z3-V2*X4*Z3+U3*Y4*Z3*2-V3*X4*Z3*2+
     &V3*X3*Z4*2)*XSUR120 !MAT[1,3]
C
      XM(IELEM,10) =
     &(U2*Y3*Z4-U2*Y4*Z3+V2*X4*Z3-V2*X3*Z4+W2*
     &X3*Y4-W2*X4*Y3-W1*X4*Y3+V1*X4*Z3-U1*Y4*Z3+W3*X
     &3*Y4*2-W3*X4*Y3*2+V3*X4*Z3*2-V3*X3*Z4*2+U3*Y3*Z4*2-2*U3*Y4*Z3
     &-V1*X3*Z4+W1*X3*Y4+U1*Y3*Z4+W4*X3*Y4-W4*X4*Y3
     &+V4*X4*Z3-V4*X3*Z4+U4*Y3*Z4-U4*Y4*Z3)*XSUR120 !MAT[2,3]
C
      XM(IELEM,6) =
     &(W3*X2*Y3*2-W3*Y2*X3*2+U2*Y2*Z3-U2*Z2*Y3-V2*X2
     &*Z3+V2*Z2*X3+W2*X2*Y3-W2*Y2*X3-V3*X2*Z3*2+V3*Z2*X
     &3*2+U3*Y2*Z3*2-U3*Z2*Y3*2+W1*X2*Y3-U1*Z2*Y3-V1*X2*Z3
     &-W1*Y2*X3+V1*Z2*X3+U1*Y2*Z3+W4*X2*Y3-W4*Y2*X3
     &-V4*X2*Z3+V4*Z2*X3+U4*Y2*Z3-U4*Z2*Y3)*XSUR120 !MAT[4,3]
C
      XM(IELEM,9) =
     &(W3*X4*Y3-V3*X2*Z4+V3*Z2*X4+U1*Y2*Z4-W4*
     &Y2*X4*2+W4*X2*Y4*2-U3*Y2*Z3-V4*X2*Z4*2-W1*Y2*X4+W1*X2*Y
     &4-U1*Y2*Z3-V4*X4*Z3*2+V4*X3*Z4*2+U4*Y4*Z3*2+U4*Z2*Y3*2
     &-U4*Y3*Z4*2+W2*Y2*X3+U2*Y4*Z3-U2*Y2*Z3-U2*Y3*Z4+
     &V1*Z2*X4+U1*Y4*Z3-V2*X2*Z4+U4*Y2*Z4*2+W4*X4*Y3*2
     &-U4*Z2*Y4*2-V3*Z2*X3+W4*Y2*X3*2+U2*Z2*Y3+U3*Y2*Z4-W
     &4*X3*Y4*2+W2*X4*Y3-W3*X3*Y4-U1*Z2*Y4+U3*Z2*Y3+W2*
     &X2*Y4+W1*X4*Y3-U3*Y3*Z4-U4*Y2*Z3*2-W2*X2*Y3-W4*X2
     &*Y3*2+V4*X2*Z3*2+W3*Y2*X3+V1*X2*Z3-U3*Z2*Y4+U1*Z2*Y3
     &+V2*Z2*X4-W2*Y2*X4-W3*X2*Y3+V4*Z2*X4*2-V1*X2*Z4
     &-W3*Y2*X4+W3*X2*Y4-U2*Z2*Y4+U2*Y2*Z4-W1*X3*Y4
     &-V1*X4*Z3+V1*X3*Z4-U1*Y3*Z4+W1*Y2*X3-V4*Z2*X3*2-
     &V1*Z2*X3-W2*X3*Y4+V3*X2*Z3-W1*X2*Y3+V2*X3*Z4-V
     &2*Z2*X3+V2*X2*Z3-V2*X4*Z3+U3*Y4*Z3-V3*X4*Z3+V3
     &*X3*Z4)*XSUR120 !MAT[1,4]
C
      XM(IELEM,11) =
     &(U2*Y3*Z4-U2*Y4*Z3+V2*X4*Z3-V2*X3*Z4+W2*
     &X3*Y4-W2*X4*Y3-W1*X4*Y3+V1*X4*Z3-U1*Y4*Z3+W3*X
     &3*Y4-W3*X4*Y3+V3*X4*Z3-V3*X3*Z4+U3*Y3*Z4-U3*Y4
     &*Z3-V1*X3*Z4+W1*X3*Y4+U1*Y3*Z4+W4*X3*Y4*2-W4*X4*Y
     &3*2+V4*X4*Z3*2-V4*X3*Z4*2+U4*Y3*Z4*2-U4*Y4*Z3*2)*XSUR120 !MAT[2,4]
C
      XM(IELEM,12) =
     &(W3*Y2*X4+U2*Z2*Y4-U2*Y2*Z4+V2*X2*Z4-V2*
     &Z2*X4-W2*X2*Y4+W2*Y2*X4-U1*Y2*Z4-W3*X2*Y4-V3*Z
     &2*X4+V3*X2*Z4+U3*Z2*Y4-U3*Y2*Z4-W1*X2*Y4+V1*X2
     &*Z4-V1*Z2*X4+W1*Y2*X4+U1*Z2*Y4-W4*X2*Y4*2+W4*Y2*X
     &4*2+V4*X2*Z4*2-V4*Z2*X4*2-U4*Y2*Z4*2+U4*Z2*Y4*2)*XSUR120 !MAT[3,4]
C
C-----------------------------------------------------------------------
C
 20   CONTINUE
C
C-----------------------------------------------------------------------
      ELSE IF (IELMU .EQ. 30) THEN
C AND V AND W
C-----------------------------------------------------------------------
C     LOOP ON THE TETRAHEDRONS
C
      DO 21 IELEM=1,NELEM
C
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
C
      U1 = U(IELEM)
      U2 = U1
      U3 = U1
      U4 = U1
      V1 = V(IELEM)
      V2 = V1
      V3 = V1
      V4 = V1
      W1 = W(IELEM)
      W2 = W1
      W3 = W1
      W4 = W1
C
C-----------------------------------------------------------------------
C
      X2=X(I2)-X(I1)
      Y2=Y(I2)-Y(I1)
      Z2=Z(I2)-Z(I1)
      X3=X(I3)-X(I1)
      Y3=Y(I3)-Y(I1)
      Z3=Z(I3)-Z(I1)
      X4=X(I4)-X(I1)
      Y4=Y(I4)-Y(I1)
      Z4=Z(I4)-Z(I1)
C
C     JACOBIAN : Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3)
C
C     VOLUME OF THE TETRAHEDRON:
C
C     (Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3))/6
C
      T(IELEM,1) = (W2*Y2*X3-W2*X2*Y3-V2*Z2*X3-U2*Z2*Y4+V2*
     &X2*Z3+U2*Z2*Y3-U4*Z2*Y4+U4*Y4*Z3-U4*Y3*Z4-V4*X
     &4*Z3-W4*Y2*X4-W4*X3*Y4-U1*Z2*Y4*2-U1*Y3*Z4*2-W1*Y2*X
     &4*2+V1*Z2*X4*2-V1*X2*Z4*2+V1*X3*Z4*2+W1*X2*Y4*2+U3*Y2*Z4+
     &U3*Y4*Z3-U3*Y3*Z4-W1*X3*Y4*2-U3*Z2*Y4+V3*Z2*X4+W3
     &*X2*Y4+W3*X4*Y3-W3*X3*Y4+U1*Y2*Z4*2+U1*Y4*Z3*2-V1*X4
     &*Z3*2+W1*X4*Y3*2-U2*Y2*Z3+W3*Y2*X3+V4*Z2*X4-W3*X2*Y3-W2*Y2*X4
     &+W2*X2*Y4+W2*X4*Y3-W2*X3*Y4+V2*X3*Z4+V2*Z2*X4
     &-V2*X2*Z4+W4*X2*Y4-V2*X4*Z3+V4*X3*Z4+U2*Y2*Z4
     &+U2*Y4*Z3-U2*Y3*Z4-V3*X2*Z4-W3*Y2*X4+U4*Z2*Y3
     &-U4*Y2*Z3-V4*Z2*X3+V4*X2*Z3+W4*Y2*X3-W4*X2*Y3
     &-U1*Y2*Z3*2-V1*Z2*X3*2+W1*Y2*X3*2+W4*X4*Y3-W1*X2*Y3*2+U3*Z
     &2*Y3-U3*Y2*Z3-V3*X4*Z3-V3*Z2*X3+U1*Z2*Y3*2+V3*X2*
     &Z3+U4*Y2*Z4+V3*X3*Z4-V4*X2*Z4+V1*X2
     &*Z3*2)*XSUR120
C
      T(IELEM,2) = (U2*Y3*Z4*2-U2*Y4*Z3*2+V2*X4*Z3*2-V2*X3*Z4*2+W2*X3*Y
     &4*2-W2*X4*Y3*2-W1*X4*Y3+V1*X4*Z3-U1*Y4*Z3+W3*X3*Y4
     &-W3*X4*Y3+V3*X4*Z3-V3*X3*Z4+U3*Y3*Z4-U3*Y4*Z3
     &-V1*X3*Z4+W1*X3*Y4+U1*Y3*Z4+W4*X3*Y4-W4*X4*Y3
     &+V4*X4*Z3-V4*X3*Z4+U4*Y3*Z4-U4*Y4*Z3)*XSUR120
C
      T(IELEM,3) = (W3*Y2*X4*2+U2*Z2*Y4-U2*Y2*Z4+V2*X2*Z4-V2*Z
     &2*X4-W2*X2*Y4+W2*Y2*X4-U1*Y2*Z4-W3*X2*Y4*2-V3*Z2*
     &X4*2+V3*X2*Z4*2+U3*Z2*Y4*2-U3*Y2*Z4*2-W1*X2*Y4+V1*X2*Z4
     &-V1*Z2*X4+W1*Y2*X4+U1*Z2*Y4-W4*X2*Y4+W4*Y2*X4
     &+V4*X2*Z4-V4*Z2*X4-U4*Y2*Z4+U4*Z2*Y4)*XSUR120
C
      T(IELEM,4) = (W3*X2*Y3-W3*Y2*X3+U2*Y2*Z3-U2*Z2*Y3-V2*
     &X2*Z3+V2*Z2*X3+W2*X2*Y3-W2*Y2*X3-V3*X2*Z3+V3*Z
     &2*X3+U3*Y2*Z3-U3*Z2*Y3+W1*X2*Y3-U1*Z2*Y3-V1*X2
     &*Z3-W1*Y2*X3+V1*Z2*X3+U1*Y2*Z3+W4*X2*Y3*2-W4*Y2*X
     &3*2-V4*X2*Z3*2+V4*Z2*X3*2+U4*Y2*Z3*2-U4*Z2*Y3*2)*XSUR120
C
      XM(IELEM,1) =
     &(U2*Y3*Z4-U2*Y4*Z3+V2*X4*Z3-V2*X3*Z4+W2*
     &X3*Y4-W2*X4*Y3-W1*X4*Y3*2+V1*X4*Z3*2-U1*Y4*Z3*2+W3*X3*Y
     &4-W3*X4*Y3+V3*X4*Z3-V3*X3*Z4+U3*Y3*Z4-U3*Y4*Z3
     &-V1*X3*Z4*2+W1*X3*Y4*2+U1*Y3*Z4*2+W4*X3*Y4-W4*X4*Y3
     &+V4*X4*Z3-V4*X3*Z4+U4*Y3*Z4-U4*Y4*Z3)*XSUR120 !MAT[2,1]
C
      XM(IELEM,2) =
     &(W3*Y2*X4+U2*Z2*Y4-U2*Y2*Z4+V2*X2*Z4-V2*
     &Z2*X4-W2*X2*Y4+W2*Y2*X4-U1*Y2*Z4*2-W3*X2*Y4-V3*Z2
     &*X4+V3*X2*Z4+U3*Z2*Y4-U3*Y2*Z4-W1*X2*Y4*2+V1*X2*Z
     &4*2-V1*Z2*X4*2+W1*Y2*X4*2+U1*Z2*Y4*2-W4*X2*Y4+W4*Y2*X4
     &+V4*X2*Z4-V4*Z2*X4-U4*Y2*Z4+U4*Z2*Y4)*XSUR120 !MAT[3,1]
C
      XM(IELEM,3) =
     &(W3*X2*Y3-W3*Y2*X3+U2*Y2*Z3-U2*Z2*Y3-V2*
     &X2*Z3+V2*Z2*X3+W2*X2*Y3-W2*Y2*X3-V3*X2*Z3+V3*Z
     &2*X3+U3*Y2*Z3-U3*Z2*Y3+W1*X2*Y3*2-U1*Z2*Y3*2-V1*X2*Z
     &3*2-W1*Y2*X3*2+V1*Z2*X3*2+U1*Y2*Z3*2+W4*X2*Y3-W4*Y2*X3
     &-V4*X2*Z3+V4*Z2*X3+U4*Y2*Z3-U4*Z2*Y3)*XSUR120 !MAT[4,1]
C
      XM(IELEM,7) = (W3*X4*Y3-V3*X2*Z4+V3*Z2*X4+U1*Y2*Z4-W4*
     &Y2*X4+W4*X2*Y4-U3*Y2*Z3-V4*X2*Z4-W1*Y2*X4+W1*X
     &2*Y4-U1*Y2*Z3-V4*X4*Z3+V4*X3*Z4+U4*Y4*Z3+U4*Z2
     &*Y3-U4*Y3*Z4+W2*Y2*X3*2+U2*Y4*Z3*2-U2*Y2*Z3*2-U2*Y3*Z4*2
     &+V1*Z2*X4+U1*Y4*Z3-V2*X2*Z4*2+U4*Y2*Z4+W4*X4*Y3
     &-U4*Z2*Y4-V3*Z2*X3+W4*Y2*X3+U2*Z2*Y3*2+U3*Y2*Z4
     &-W4*X3*Y4+W2*X4*Y3*2-W3*X3*Y4-U1*Z2*Y4+U3*Z2*Y3+W
     &2*X2*Y4*2+W1*X4*Y3-U3*Y3*Z4-U4*Y2*Z3-W2*X2*Y3*2-W4*X
     &2*Y3+V4*X2*Z3+W3*Y2*X3+V1*X2*Z3-U3*Z2*Y4+U1*Z2
     &*Y3+V2*Z2*X4*2-W2*Y2*X4*2-W3*X2*Y3+V4*Z2*X4-V1*X2*Z4
     &-W3*Y2*X4+W3*X2*Y4-U2*Z2*Y4*2+U2*Y2*Z4*2-W1*X3*Y4
     &-V1*X4*Z3+V1*X3*Z4-U1*Y3*Z4+W1*Y2*X3-V4*Z2*X3
     &-V1*Z2*X3-W2*X3*Y4*2+V3*X2*Z3-W1*X2*Y3+V2*X3*Z4*2
     &-V2*Z2*X3*2+V2*X2*Z3*2-V2*X4*Z3*2+U3*Y4*Z3-V3*X4*Z3+V3
     &*X3*Z4)*XSUR120 !MAT[1,2]
C
      XM(IELEM,4) = (-W2*X2*Y4*2+W2*Y2*X4*2+V2*X2*Z4*2
     &-V2*Z2*X4*2-V3*Z2*
     &X4+V3*X2*Z4-U3*Y2*Z4+U3*Z2*Y4-U2*Y2*Z4*2+U2*Z2*Y4
     &*2-W3*X2*Y4+W3*Y2*X4+V1*X2*Z4-V4*Z2*X4+V4*X2*Z4
     &+U4*Z2*Y4-U4*Y2*Z4-W4*X2*Y4+W4*Y2*X4-U1*Y2*Z4
     &+U1*Z2*Y4-V1*Z2*X4-W1*X2*Y4+W1*Y2*X4)*XSUR120 !MAT[3,2]
C
      XM(IELEM,5) =
     &(W3*X2*Y3-W3*Y2*X3+U2*Y2*Z3*2-U2*Z2*Y3*2-V2*X2
     &*Z3*2+V2*Z2*X3*2+W2*X2*Y3*2-W2*Y2*X3*2-V3*X2*Z3+V3*Z2*X3
     &+U3*Y2*Z3-U3*Z2*Y3+W1*X2*Y3-U1*Z2*Y3-V1*X2*Z3
     &-W1*Y2*X3+V1*Z2*X3+U1*Y2*Z3+W4*X2*Y3-W4*Y2*X3
     &-V4*X2*Z3+V4*Z2*X3+U4*Y2*Z3-U4*Z2*Y3)*XSUR120 !MAT[4,2]
C
      XM(IELEM,8) = (
     &W3*X4*Y3*2-V3*X2*Z4*2+V3*Z2*X4*2+U1*Y2*Z4-W4*Y2*
     &X4+W4*X2*Y4-U3*Y2*Z3*2-V4*X2*Z4-W1*Y2*X4+W1*X2*Y4
     &-U1*Y2*Z3-V4*X4*Z3+V4*X3*Z4+U4*Y4*Z3+U4*Z2*Y3
     &-U4*Y3*Z4+W2*Y2*X3+U2*Y4*Z3-U2*Y2*Z3-U2*Y3*Z4
     &+V1*Z2*X4+U1*Y4*Z3-V2*X2*Z4+U4*Y2*Z4+W4*X4*Y3
     &-U4*Z2*Y4-V3*Z2*X3*2+W4*Y2*X3+U2*Z2*Y3+U3*Y2*Z4*2
     &-W4*X3*Y4+W2*X4*Y3-W3*X3*Y4*2-U1*Z2*Y4+U3*Z2*Y3*2+W2
     &*X2*Y4+W1*X4*Y3-U3*Y3*Z4*2-U4*Y2*Z3-W2*X2*Y3-W4*X
     &2*Y3+V4*X2*Z3+W3*Y2*X3*2+V1*X2*Z3-U3*Z2*Y4*2+U1*Z2*Y
     &3+V2*Z2*X4-W2*Y2*X4-W3*X2*Y3*2+V4*Z2*X4-V1*X2*Z4
     &-W3*Y2*X4*2+W3*X2*Y4*2-U2*Z2*Y4+U2*Y2*Z4-W1*X3*Y4
     &-V1*X4*Z3+V1*X3*Z4-U1*Y3*Z4+W1*Y2*X3-V4*Z2*X3
     &-V1*Z2*X3-W2*X3*Y4+V3*X2*Z3*2-W1*X2*Y3+V2*X3*Z4
     &-V2*Z2*X3+V2*X2*Z3-V2*X4*Z3+U3*Y4*Z3*2-V3*X4*Z3*2+
     &V3*X3*Z4*2)*XSUR120 !MAT[1,3]
C
      XM(IELEM,10) =
     &(U2*Y3*Z4-U2*Y4*Z3+V2*X4*Z3-V2*X3*Z4+W2*
     &X3*Y4-W2*X4*Y3-W1*X4*Y3+V1*X4*Z3-U1*Y4*Z3+W3*X
     &3*Y4*2-W3*X4*Y3*2+V3*X4*Z3*2-V3*X3*Z4*2+U3*Y3*Z4*2-2*U3*Y4*Z3
     &-V1*X3*Z4+W1*X3*Y4+U1*Y3*Z4+W4*X3*Y4-W4*X4*Y3
     &+V4*X4*Z3-V4*X3*Z4+U4*Y3*Z4-U4*Y4*Z3)*XSUR120 !MAT[2,3]
C
      XM(IELEM,6) =
     &(W3*X2*Y3*2-W3*Y2*X3*2+U2*Y2*Z3-U2*Z2*Y3-V2*X2
     &*Z3+V2*Z2*X3+W2*X2*Y3-W2*Y2*X3-V3*X2*Z3*2+V3*Z2*X
     &3*2+U3*Y2*Z3*2-U3*Z2*Y3*2+W1*X2*Y3-U1*Z2*Y3-V1*X2*Z3
     &-W1*Y2*X3+V1*Z2*X3+U1*Y2*Z3+W4*X2*Y3-W4*Y2*X3
     &-V4*X2*Z3+V4*Z2*X3+U4*Y2*Z3-U4*Z2*Y3)*XSUR120 !MAT[4,3]
C
      XM(IELEM,9) =
     &(W3*X4*Y3-V3*X2*Z4+V3*Z2*X4+U1*Y2*Z4-W4*
     &Y2*X4*2+W4*X2*Y4*2-U3*Y2*Z3-V4*X2*Z4*2-W1*Y2*X4+W1*X2*Y
     &4-U1*Y2*Z3-V4*X4*Z3*2+V4*X3*Z4*2+U4*Y4*Z3*2+U4*Z2*Y3*2
     &-U4*Y3*Z4*2+W2*Y2*X3+U2*Y4*Z3-U2*Y2*Z3-U2*Y3*Z4+
     &V1*Z2*X4+U1*Y4*Z3-V2*X2*Z4+U4*Y2*Z4*2+W4*X4*Y3*2
     &-U4*Z2*Y4*2-V3*Z2*X3+W4*Y2*X3*2+U2*Z2*Y3+U3*Y2*Z4-W
     &4*X3*Y4*2+W2*X4*Y3-W3*X3*Y4-U1*Z2*Y4+U3*Z2*Y3+W2*
     &X2*Y4+W1*X4*Y3-U3*Y3*Z4-U4*Y2*Z3*2-W2*X2*Y3-W4*X2
     &*Y3*2+V4*X2*Z3*2+W3*Y2*X3+V1*X2*Z3-U3*Z2*Y4+U1*Z2*Y3
     &+V2*Z2*X4-W2*Y2*X4-W3*X2*Y3+V4*Z2*X4*2-V1*X2*Z4
     &-W3*Y2*X4+W3*X2*Y4-U2*Z2*Y4+U2*Y2*Z4-W1*X3*Y4
     &-V1*X4*Z3+V1*X3*Z4-U1*Y3*Z4+W1*Y2*X3-V4*Z2*X3*2-
     &V1*Z2*X3-W2*X3*Y4+V3*X2*Z3-W1*X2*Y3+V2*X3*Z4-V
     &2*Z2*X3+V2*X2*Z3-V2*X4*Z3+U3*Y4*Z3-V3*X4*Z3+V3
     &*X3*Z4)*XSUR120 !MAT[1,4]
C
      XM(IELEM,11) =
     &(U2*Y3*Z4-U2*Y4*Z3+V2*X4*Z3-V2*X3*Z4+W2*
     &X3*Y4-W2*X4*Y3-W1*X4*Y3+V1*X4*Z3-U1*Y4*Z3+W3*X
     &3*Y4-W3*X4*Y3+V3*X4*Z3-V3*X3*Z4+U3*Y3*Z4-U3*Y4
     &*Z3-V1*X3*Z4+W1*X3*Y4+U1*Y3*Z4+W4*X3*Y4*2-W4*X4*Y
     &3*2+V4*X4*Z3*2-V4*X3*Z4*2+U4*Y3*Z4*2-U4*Y4*Z3*2)*XSUR120 !MAT[2,4]
C
      XM(IELEM,12) =
     &(W3*Y2*X4+U2*Z2*Y4-U2*Y2*Z4+V2*X2*Z4-V2*
     &Z2*X4-W2*X2*Y4+W2*Y2*X4-U1*Y2*Z4-W3*X2*Y4-V3*Z
     &2*X4+V3*X2*Z4+U3*Z2*Y4-U3*Y2*Z4-W1*X2*Y4+V1*X2
     &*Z4-V1*Z2*X4+W1*Y2*X4+U1*Z2*Y4-W4*X2*Y4*2+W4*Y2*X
     &4*2+V4*X2*Z4*2-V4*Z2*X4*2-U4*Y2*Z4*2+U4*Z2*Y4*2)*XSUR120 !MAT[3,4]
C
 21   CONTINUE
C-----------------------------------------------------------------------
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,1000) SU%ELM,SV%ELM,SW%ELM
        IF (LNG.EQ.2) WRITE(LU,1001) SU%ELM,SV%ELM,SW%ELM
1000    FORMAT(1X,'MT05TT (BIEF) : MAUVAIS TYPE DE U,V OU W : ',
     &  I6,1X,I6,1X,I6)
1001    FORMAT(1X,'MT05TT (BIEF) : WRONG TYPE OF U,V OR W: ',
     &  I6,1X,I6,1X,I6)
        CALL PLANTE(1)
        STOP
C
      ENDIF
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C