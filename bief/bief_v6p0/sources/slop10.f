C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENT 1 / COS(ALFA)
!>                WHERE ALFA IS THE SLOPE OF A TRIANGULAR ELEMENT.
!><br>            THIS COEFFICIENT IS USED IN THE BOTTOM FRICTION
!>                TERM.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> COEF, IKLE, NELEM, NELMAX, XEL, YEL, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, B, C, IELEM, X2, X3, Y2, Y3, Z2, Z3
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SLOPES()

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
!> </td><td> 27/01/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COEF
!></td><td><--</td><td>RESULTAT
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>XEL,YEL
!></td><td>--></td><td>COORDONNEES DES POINTS DES ELEMENTS
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COTES DU FOND (DONNEES PAR POINT)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SLOP10
     &(COEF,XEL,YEL,Z,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COEF           |<--| RESULTAT
C| IKLE           |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| XEL,YEL        |-->| COORDONNEES DES POINTS DES ELEMENTS
C| Z             |-->| COTES DU FOND (DONNEES PAR POINT)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
C
      DOUBLE PRECISION, INTENT(INOUT) :: COEF(NELEM)
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,*),YEL(NELMAX,*),Z(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM
      DOUBLE PRECISION X2,X3,Y2,Y3,Z2,Z3,A,B,C
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
         DO 1 IELEM = 1 , NELEM
C
         X2 = XEL(IELEM,2)
         X3 = XEL(IELEM,3)
C
         Y2 = YEL(IELEM,2)
         Y3 = YEL(IELEM,3)
C
         Z2 = Z(IKLE(IELEM,2)) -  Z(IKLE(IELEM,1))
         Z3 = Z(IKLE(IELEM,3)) -  Z(IKLE(IELEM,1))
C
         A = (X2*Y3-X3*Y2)**2
         B = (Y2*Z3-Z2*Y3)**2
         C = (X3*Z2-Z3*X2)**2
C
         COEF(IELEM) = SQRT( (A+B+C)/A )
C
1        CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C