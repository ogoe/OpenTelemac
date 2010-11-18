C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE MASS MATRIX FOR TETRAHEDRONS.
!>  @code
!>     STORAGE CONVENTION FOR EXTRA-DIAGONAL TERMS:<br>
!>     XM(IELEM, 1)  ---->  M(1,2) = M(2,1)
!>     XM(IELEM, 2)  ---->  M(1,3) = M(3,1)
!>     XM(IELEM, 3)  ---->  M(1,4) = M(4,1)
!>     XM(IELEM, 4)  ---->  M(2,3) = M(3,2)
!>     XM(IELEM, 5)  ---->  M(2,4) = M(4,2)
!>     XM(IELEM, 6)  ---->  M(3,4) = M(4,3)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, NELEM, NELMAX, T, X, XM, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, I3, I4, IELEM, VOLSUR20, X2, X3, X4, XSUR120, Y2, Y3, Y4, Z2, Z3, Z4
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT01TT
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
!>      <td><center> 5.3                                       </center>
!> </td><td> 04/01/02
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>T,XM
!></td><td><--</td><td>ELEMENTS DE LA MATRICE
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT01TT
     &( T,XM,XMUL,X,Y,Z,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| T,XM           |<--| ELEMENTS DE LA MATRICE
C| X,Y,Z          |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT01TT => MT01TT
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,4)
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,4),XM(NELMAX,6)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),Z(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     SPECIFIC DECLARATIONS
C
      DOUBLE PRECISION X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,VOLSUR20
      INTEGER I1,I2,I3,I4,IELEM
C
      DOUBLE PRECISION XSUR120
C
C***********************************************************************
C
      XSUR120=XMUL/120.D0
C
C-----------------------------------------------------------------------
C
C     LOOP ON THE TETRAHEDRONS
C
      DO 20 IELEM=1,NELEM
C
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
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
C     EXTRA-DIAGONAL TERMS
C
C     VOLUME OF THE TETRAHEDRON:
C
C     (Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3))/6
C
C     XM(IELEM,1) = VOLUME / 20
C
C     SUMS UP THE TERMS (INC. SYMMETRIC ONES) TO YIELD VOLUME OF THE TETRAHEDRON
C
      VOLSUR20 =
     &(Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3))*XSUR120
      XM(IELEM,1) = MAX(VOLSUR20,1.D-4)
      XM(IELEM,2) = XM(IELEM,1)
      XM(IELEM,3) = XM(IELEM,1)
      XM(IELEM,4) = XM(IELEM,1)
      XM(IELEM,5) = XM(IELEM,1)
      XM(IELEM,6) = XM(IELEM,1)
C
C     DIAGONAL TERMS
C
      T(IELEM,1) = 2 * XM(IELEM,1)
      T(IELEM,2) = T(IELEM,1)
      T(IELEM,3) = T(IELEM,1)
      T(IELEM,4) = T(IELEM,1)
C
C-----------------------------------------------------------------------
C
20    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C