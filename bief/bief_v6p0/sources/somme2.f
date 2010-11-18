C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SUMS THE COMPONENTS OF A VECTOR WHILE MINIMISING
!>                THE TRUNCATION ERRORS.+

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference    GOLDBERG, DAVID  1991. WHAT EVERY COMPUTER SCIENTIST
!> SHOULD KNOW   ABOUT FLOATING-POINT ARITHMETIC, ACM  COMPUTING
!> SURVEYS, 23(1), PP5-48 (CORRIGENDUM, COMPUTING SURVEYS, 1991, 23(3))
!>(ARTICLE REPRODUCED IN THE NUMERICAL COMPUTATION GUIDE OF SUN
!> MICROSYSTEM, HTTP://DOCS.SUN.COM )

!>  @reference    KNUTH, D.E. 1981. THE ART OF PROGRAMMING.
!> ADDISON-WESLEY, READING, MASS., VOL. II, 2ND ED.
!> PROOF P 572

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NPX, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, I, T, Y
!>   </td></tr>
!>     </table>

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
!> </td><td> 08/12/98
!> </td><td> A. DESITTER (NAG)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NPX
!></td><td>--></td><td>NOMBRE DE VALEURS A ADDITIONNER
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>TABLEAU FORTRAN
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION SOMME2
     &( X , NPX )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NPX            |-->| NOMBRE DE VALEURS A ADDITIONNER
C| X             |-->| TABLEAU FORTRAN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPX
C
      DOUBLE PRECISION, INTENT(IN) :: X(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
      DOUBLE PRECISION C,Y,T
C
C-----------------------------------------------------------------------
C KAHAN SUMMATION FORMULA
C
      SOMME2 = X(1)
      C = 0.D0
      DO I = 2 , NPX
         Y = X(I) - C
         T = SOMME2 + Y
         C = (T - SOMME2) - Y
         SOMME2 = T
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END



C
C#######################################################################
C