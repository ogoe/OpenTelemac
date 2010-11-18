C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE MASS MATRIX FOR P1 PRISMS.
!>  @code
!>    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:<br>
!>                                 /
!>                    A    = XMUL /  (P *P )*J(X,Y) DXDY
!>                     I J       /S    I  J<br>
!>    BY ELEMENTARY CELL (REAL MESH)<br>
!>    J(X,Y) : JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
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
!>    </th><td> IKLE, NELEM, NELMAX, SURFAC, T, XM, XMUL, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COEF, EPSILON, H1, H2, H3, HT, IELEM, SUR360, Z41, Z52, Z63
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT01PP
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 21/05/2010
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
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES.
!>    </td></tr>
!>          <tr><td>T
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT01PP
     &( T,XM,XMUL,Z,SURFAC,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| IKLE           |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| T             |---| 
C| XM             |---| 
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C| Z             |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT01PP => MT01PP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,6)
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,30)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL,Z(*),SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELEM
      DOUBLE PRECISION SUR360,COEF,H1,H2,H3,HT,Z41,Z52,Z63
C
      DOUBLE PRECISION EPSILON
      DATA EPSILON/1.D-3/
C
C-----------------------------------------------------------------------
C
      SUR360 = XMUL / 360.D0
C
C   LOOP ON THE ELEMENTS
C
      DO 1 IELEM = 1,NELEM
C
         COEF = SURFAC(IELEM) * SUR360
C
C        TREATS HERE THE DRY ZONES
C
         H1 = MAX(Z(IKLE(IELEM,4)) - Z(IKLE(IELEM,1)),EPSILON) * COEF
         H2 = MAX(Z(IKLE(IELEM,5)) - Z(IKLE(IELEM,2)),EPSILON) * COEF
         H3 = MAX(Z(IKLE(IELEM,6)) - Z(IKLE(IELEM,3)),EPSILON) * COEF
         HT = H1 + H2 + H3
C
C-----------------------------------------------------------------------
C
C  EXTRA-DIAGONAL TERMS
C
         XM(IELEM,4)  = H1 + H2 + HT
         XM(IELEM,5)  = H1 + H3 + HT
         XM(IELEM,9)  = H2 + H3 + HT
         XM(IELEM,7)  = XM(IELEM,4)
         XM(IELEM,10) = XM(IELEM,5)
         XM(IELEM,11) = XM(IELEM,9)
C
         XM(IELEM,3)  =  4*H1 + HT + HT
         XM(IELEM,8)  =  4*H2 + HT + HT
         XM(IELEM,12) =  4*H3 + HT + HT
C
         XM(IELEM,1)  = XM(IELEM,4) + XM(IELEM,4)
         XM(IELEM,2)  = XM(IELEM,5) + XM(IELEM,5)
         XM(IELEM,6)  = XM(IELEM,9) + XM(IELEM,9)
         XM(IELEM,13) = XM(IELEM,1)
         XM(IELEM,14) = XM(IELEM,2)
         XM(IELEM,15) = XM(IELEM,6)
C
C  DIAGONAL TERMS
C
         T(IELEM,1) = XM(IELEM,3)  + XM(IELEM,3)
         T(IELEM,2) = XM(IELEM,8)  + XM(IELEM,8)
         T(IELEM,3) = XM(IELEM,12) + XM(IELEM,12)
         T(IELEM,4) = T(IELEM,1)
         T(IELEM,5) = T(IELEM,2)
         T(IELEM,6) = T(IELEM,3)
C
C   END OF THE LOOP ON THE ELEMENTS
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C