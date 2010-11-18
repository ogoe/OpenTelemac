C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  
!>  @code
!>   RESOLUTION OF THE EQUATION OF AX**3+BX**2+CX+D=0 (E1)<br>
!>   1/ CHANGE THE VARIABLE X BY X = X -B/(3A)
!>                              P = (C/A) - B*B/(3A*A)
!>                              Q = 2B**3/(27A**3)+D/A-BC/(3A*A)<br>
!>      =>  (E1)  X**3+PX+Q = 0 (E2)<br>
!>   2/ COMPUTE DELTA = (Q*Q)/4 + (P**3)/27<br>
!>   3/ DELTA  > 0 : SOLUTION OF CARDAN      (1 REAL ROOT)<br>
!>   4/ DELTA
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ACOF, BCOF, CCOF, DCOF, REALS, X
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BA, CA, EXPO, P, PHI, PI, Q, Q2P3, SIGN, TMP, U, V
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FRICTION_LINDNER()

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
!> </td><td> 20/04/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ACOF
!></td><td>--></td><td>CONSTANT FOR X**3
!>    </td></tr>
!>          <tr><td>BCOF
!></td><td>--></td><td>CONSTANT FOR X**2
!>    </td></tr>
!>          <tr><td>CCOF
!></td><td>--></td><td>CONSTANT FOR X
!>    </td></tr>
!>          <tr><td>DCOF
!></td><td>--></td><td>CONSTANT OF THE EQUATION
!>    </td></tr>
!>          <tr><td>REALS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CUBEEQUATION
     & (ACOF, BCOF, CCOF, DCOF, REALS, X)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ACOF           |-->| CONSTANT FOR X**3
C| BCOF           |-->| CONSTANT FOR X**2
C| CCOF           |-->| CONSTANT FOR X
C| DCOF           |-->| CONSTANT OF THE EQUATION
C| REALS          |---| 
C| X             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN)  :: ACOF, BCOF, CCOF, DCOF
      INTEGER,          INTENT(OUT) :: REALS
      DOUBLE PRECISION, INTENT(OUT) :: X(3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, PARAMETER :: PI = 3.14159265358979323846D0
      DOUBLE PRECISION            :: BA, CA, P, Q, Q2P3, U, V
      DOUBLE PRECISION            :: EXPO, SIGN, TMP, PHI
C
C=======================================================================
C=======================================================================
C
      BA = BCOF / ACOF / 3.D0
      CA = CCOF / ACOF
C
      P  = CA/3.D0 - BA**2
      Q  = BA**3 - BA*CA/2.D0 + DCOF/ACOF/2.D0
C
      Q2P3 = Q**2 + P**3
C
      IF (Q2P3 > 0.D0) THEN
         REALS = 1
         EXPO  = 1.D0/3.D0
         TMP   = -Q + SQRT(Q2P3)
         SIGN  = TMP / ABS(TMP)
         U     = SIGN * ABS(TMP)**(EXPO)
         TMP   = -Q - SQRT(Q2P3)
         SIGN  = TMP / ABS(TMP)
         V     = SIGN * ABS(TMP)**EXPO
         X(1)  = (U + V) - BA
C
      ELSE
C
        REALS = 3
        TMP = -Q / (-P)**(1.5D0)
C
        IF (TMP >= 1.D0) THEN
           PHI = 0.D0
        ELSE IF (TMP <= -1.D0) THEN
           PHI = PI
        ELSE
          PHI = ACOS (TMP)
        ENDIF
C
        X(1) = 2.D0* SQRT(-P)* COS(PHI/3.D0)           -  BA
        X(2) = 2.D0* SQRT(-P)* COS((PHI+2.D0*PI)/3.D0) -  BA
        X(3) = 2.D0* SQRT(-P)* COS((PHI+4.D0*PI)/3.D0) -  BA
C
      ENDIF
C
C=======================================================================
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C