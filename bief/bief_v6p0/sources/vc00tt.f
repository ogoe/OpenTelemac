C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                    /
!>    VEC(I) = XMUL  /    PSI(I)  D(OMEGA)
!>                  /OMEGA<br>
!>    PSI(I) IS A BASE OF TYPE P1 TETRAHEDRON
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SURFAC, W1, W2, W3, W4, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, I3, I4, IELEM, X2, X3, X4, XSUR24, Y2, Y3, Y4, Z2, Z3, Z4
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
!>      <td><center> 5.3                                       </center>
!> </td><td> 22/03/02
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
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
!>          <tr><td>IKLE4
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
!></td><td><--</td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
!>    </td></tr>
!>          <tr><td>W2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VC00TT
     &( XMUL,X,Y,Z,SURFAC,IKLE1,IKLE2,IKLE3,IKLE4,
     &  NELEM,NELMAX,W1,W2,W3,W4)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| W1,2,3         |<--| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| W4             |---| 
C| X             |---| 
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C| Y             |---| 
C| Z             |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION XSUR24,X2,X3,X4,Y2,Y3,Y4,Z2,Z3,Z4
      INTEGER I1,I2,I3,I4,IELEM
C
C-----------------------------------------------------------------------
C
      XSUR24  = XMUL/24.D0
C
C   LOOP ON THE ELEMENTS
C
      DO 3 IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
C
         X2 = X(I2)-X(I1)
         X3 = X(I3)-X(I1)
         X4 = X(I4)-X(I1)
C
         Y2 = Y(I2)-Y(I1)
         Y3 = Y(I3)-Y(I1)
         Y4 = Y(I4)-Y(I1)
C
         Z2 = Z(I2)-Z(I1)
         Z3 = Z(I3)-Z(I1)
         Z4 = Z(I4)-Z(I1)
C
         W1(IELEM) =
     &    (X2*Y3*Z4-X2*Y4*Z3-Y2*X3*Z4+Y2*X4*Z3+Z2*X3*Y4-Z2*X4*Y3)*XSUR24
         W2(IELEM) = W1(IELEM)
         W3(IELEM) = W1(IELEM)
         W4(IELEM) = W1(IELEM)
C
3     CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C