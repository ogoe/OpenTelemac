C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                    /
!>    VEC(I) = XMUL  /    PSI(I)  D(OMEGA)
!>                  /OMEGA<br>
!>    PSI(I) IS A BASE OF TYPE P1 SEGMENT BUT ON A MESH WITH
!>    VERTICAL TRIANGLES IN THE X,Y,Z SPACE<br>
!>    F IS A VECTOR OF TYPE IELMF
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE1, IKLE2, IKLE3, NBOR, NELEM, NELMAX, W1, W2, W3, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COEF, I1, I2, I3, IELEM, S, X1, X2, X3, XSUR3, Y1, Y2, Y3, Z1, Z2, Z3
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VECTOS()

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
!> </td><td> 05/02/91
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE1,
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
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
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES ELEMENTS.
!>    </td></tr>
!>          <tr><td>W1,2,3
!></td><td>--></td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
!>    </td></tr>
!>          <tr><td>W2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XEL,YEL,
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
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
                        SUBROUTINE VC00FT
     &( XMUL,X,Y,Z,
     &  IKLE1,IKLE2,IKLE3,NBOR,NELEM,NELMAX,W1,W2,W3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| NBOR           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| W1,2,3         |-->| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| X             |---| 
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C| Y             |---| 
C| Z             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: NBOR(*)
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,I1,I2,I3
      DOUBLE PRECISION XSUR3,COEF,X1,X2,X3,Y1,Y2
      DOUBLE PRECISION Y3,Z1,Z2,Z3,S
C
      INTRINSIC SQRT
C
C***********************************************************************
C
         XSUR3 = XMUL/3.D0
C
C   LOOP ON THE BOUNDARY SIDES
C
         DO 1 IELEM = 1,NELEM
C
C           GLOBAL NUMBERING OF THE SIDE NODES
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
C           COMPUTES THE AREA OF THE TRIANGLE (BY VECTOR PRODUCT)
C
            S=0.5D0*SQRT(  (Y2*Z3-Y3*Z2)**2
     &                    +(X3*Z2-X2*Z3)**2  )
C    *                    +(X2*Y3-X3*Y2)**2  )  THIS TERM IS 0
C
            COEF=XSUR3*S
C
            W1(IELEM) = COEF
            W2(IELEM) = COEF
            W3(IELEM) = COEF
C
1        CONTINUE
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C
C     NOTE: FOR A PLANE TRIANGLE (VC00AA):
C
C     XSUR3 = XMUL / 3.D0
C
C     DO 3 IELEM = 1 , NELEM
C
C
C       COEF = XSUR3 * SURFAC(IELEM)
C
C       W1(IELEM) = COEF
C       W2(IELEM) = COEF
C       W3(IELEM) = COEF
C
C3     CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C