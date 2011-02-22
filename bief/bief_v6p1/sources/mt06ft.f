C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code
!>                              /
!>                    A    =   /  F * (P *P )*J(X,Y) DXDY
!>                     I J    /S        I  J<br>
!>     BY ELEMENTARY CELL;
!>  !! THE ELEMENT IS THE P1 TRIANGLE, BUT IN A MESH OF PRISMS !!
!>  !! SPLIT IN TETRAHEDRONS                                   !!<br>
!>     J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
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
!>    </th><td> A11, A12, A13, A22, A23, A33, F, IKLE1, IKLE2, IKLE3, NBOR, NELEM, NELMAX, SF, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DET1, DET2, F1, F123, F2, F3, I1, I2, I3, IELEM, IELMF, S, SUR60, X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT06FT
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
!>      <td><center> 5.5                                       </center>
!> </td><td> 26/04/04
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
!>          <tr><td>IKLE1
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
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
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XEL,YEL,ZEL
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT06FT
     &( A11 , A12 , A13 ,
     &        A22 , A23 ,
     &              A33 ,
     &  XMUL,SF,F,X,Y,Z,IKLE1,IKLE2,IKLE3,NBOR,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A22            |---| 
C| A23            |---| 
C| A33            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| NBOR           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SU,SV,SW       |-->| STRUCTURES DE U,V ET W.
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| X             |---| 
C| XEL,YEL,ZEL    |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C| Y             |---| 
C| Z             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT06FT => MT06FT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NBOR(*),NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
C
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
C
C     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
C
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELMF,I1,I2,I3,IELEM
C
      DOUBLE PRECISION SUR60,S,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,F1,F2,F3,F123
      DOUBLE PRECISION DET1,DET2
C
C**********************************************************************
C
      IELMF=SF%ELM
C
C-----------------------------------------------------------------------
C
C     F LINEAR BY BOUNDARY SIDE
C
      IF(IELMF.EQ.61.OR.IELMF.EQ.81) THEN
C
         SUR60  = XMUL/60.D0
C
C   LOOP ON THE BOUNDARY SIDES
C
         DO 1 IELEM = 1,NELEM
C
C           GLOBAL NUMBERING OF THE SIDE VERTICES
C
            I1 = NBOR(IKLE1(IELEM))
            I2 = NBOR(IKLE2(IELEM))
            I3 = NBOR(IKLE3(IELEM))
C
            X1 = X(I1)
            Y1 = Y(I1)
            Z1 = Z(I1)
C
            X2 = X(I2)-X1
            X3 = X(I3)-X1
            Y2 = Y(I2)-Y1
            Y3 = Y(I3)-Y1
            Z2 = Z(I2)-Z1
            Z3 = Z(I3)-Z1
C
            F1 = F(IKLE1(IELEM))
            F2 = F(IKLE2(IELEM))
            F3 = F(IKLE3(IELEM))
            F123  = F1 + F2 + F3
C
C           COMPUTES THE AREA OF THE TRIANGLE (BY VECTOR PRODUCT)
C
            S=0.5D0*SQRT(  (Y2*Z3-Y3*Z2)**2
     &                    +(X3*Z2-X2*Z3)**2
     &                    +(X2*Y3-X3*Y2)**2   )
C
            DET1 = S * SUR60
            DET2 = DET1 + DET1
C
C***********************************************************************
C
C          ELEMENTS OFF THE DIAGONAL
C
           A12(IELEM) = DET1 * (F123+F123-F3)
           A13(IELEM) = DET1 * (F123+F123-F2)
           A23(IELEM) = DET1 * (F123+F123-F1)
C
C          DIAGONAL TERMS
C
           A11(IELEM) = DET2 * (F123+F1+F1)
           A22(IELEM) = DET2 * (F123+F2+F2)
           A33(IELEM) = DET2 * (F123+F3+F3)
C
1        CONTINUE
C
C-----------------------------------------------------------------------
C
C     OTHER TYPES OF DISCRETISATION OF F
C
      ELSE
C
         IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
         IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
100      FORMAT(1X,'MT06FT (BIEF) :',/,
     &          1X,'DISCRETISATION DE F NON PREVUE : ',1I6,
     &          1X,'NOM REEL : ',A6)
101      FORMAT(1X,'MT06FT (BIEF) :',/,
     &          1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &          1X,'REAL NAME: ',A6)
         CALL PLANTE(1)
         STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     NOTE: ON A TRIANGULAR MESH IN PLANE (X, Y)
C
C     DO 1 IELEM = 1 , NELEM
C
C     F1 = F(IKLE1(IELEM))
C     F2 = F(IKLE2(IELEM))
C     F3 = F(IKLE3(IELEM))
C
C     F123 = F1 + F2 + F3
C
C     DET1 = SURFAC(IELEM) * SUR60
C     DET2 = DET1 + DET1
C
C***********************************************************************
C
C  ELEMENTS OFF THE DIAGONAL
C
C     A12(IELEM) = DET1 * (F123+F123-F3)
C     A13(IELEM) = DET1 * (F123+F123-F2)
C     A23(IELEM) = DET1 * (F123+F123-F1)
C
C  DIAGONAL TERMS
C
C     A11(IELEM) = DET2 * (F123+F1+F1)
C     A22(IELEM) = DET2 * (F123+F2+F2)
C     A33(IELEM) = DET2 * (F123+F3+F3)
C
C1     CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C