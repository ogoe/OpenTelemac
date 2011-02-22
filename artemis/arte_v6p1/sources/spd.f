C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE ENERGY DENSITY BASED ON GODA.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference "RANDOM SEA AND DESIGN OF MARITIME STRUCTURES",
!>                       UNIVERSITY OF TOKYO PRESS - 1985

!>  @code
!> SPD(TETA) = COS( (TETA)/2 )**(2*EXPO)
!>
!>
!> WHERE TETA IS THE WAVE PROPAGATION ANGLE
!>       (THE MAIN PROPAGATION DIRECTION IS TETA=0)
!>       EXPO IS AN EXPONENT SPECIFIED BY THE USER
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> TETA
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU<hr>
!> COEFHD : EXPO
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DEGRAD, PI
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DIRALE()

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
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 04/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center> 2.0                                    </center></td>
!>    <td> 01/06/1993                                              </td>
!>    <td> F. LEPEINTRE (LNH) 01.30.87.78.54                       </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>TETA
!></td><td>--></td><td>ANGLE DE PROPAGATION DE LA HOULE D'ENERGIE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION SPD
     &(TETA)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| TETA           |-->| ANGLE DE PROPAGATION DE LA HOULE D'ENERGIE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION TETA,PI,DEGRAD
C
      DOUBLE PRECISION EXPO
      COMMON /COEFHD/ EXPO
C
      INTRINSIC COS
C
C-----------------------------------------------------------------------
C
      PARAMETER( PI = 3.1415926535897932384626433D0 ,
     &           DEGRAD = PI/180.D0 )
C
C-----------------------------------------------------------------------
C
      SPD = COS ( TETA*DEGRAD / 2.D0 )**(2*EXPO)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C