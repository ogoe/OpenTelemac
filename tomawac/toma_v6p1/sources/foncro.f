C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE VALUE OF THE FUNCTION TO BE INTEGRATED
!>                FOR WAVE BREAKING (ROELVINK, 1993).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, N, X, XM
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, FONCRO
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BORNES(), QGAUSS()

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
!>      <td><center> 1.1                                       </center>
!> </td><td> 26/03/96
!> </td><td> F. BECQ (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td>--></td><td>PARAMETRE A DE LA FONCTION A EVALUER
!>    </td></tr>
!>          <tr><td>B
!></td><td>--></td><td>PARAMETRE B DE LA FONCTION A EVALUER
!>    </td></tr>
!>          <tr><td>FONCRO
!></td><td><--</td><td>VALEUR DE LA FONCTION
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>EXPOSANT N  DE LA FONCTION A EVALUER
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>VALEUR A LAQUELLE LA FONCTION EST EVALUEE
!>    </td></tr>
!>          <tr><td>XM
!></td><td>--></td><td>PARAMETRE M DE LA FONCTION A EVALUER
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        FUNCTION FONCRO
     &( X     , B     , N     , A     , XM    )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| PARAMETRE A DE LA FONCTION A EVALUER
C| B             |-->| PARAMETRE B DE LA FONCTION A EVALUER
C| FONCRO         |<--| VALEUR DE LA FONCTION
C| N             |-->| EXPOSANT N  DE LA FONCTION A EVALUER
C| X             |-->| VALEUR A LAQUELLE LA FONCTION EST EVALUEE
C| XM             |-->| PARAMETRE M DE LA FONCTION A EVALUER
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C     VARIABLES IN ARGUMENT
C     """""""""""""""""""""
      INTEGER  N
      DOUBLE PRECISION X      , B     , A     , XM    , FONCRO
C
C     LOCAL VARIABLES
C     """"""""""""""""""
      DOUBLE PRECISION AUX
C
C
      AUX   = A*X**XM
      FONCRO= XM*AUX*DEXP(-AUX)*(1.D0-DEXP(-(B*X)**N))
C
      RETURN
      END
C
C#######################################################################
C