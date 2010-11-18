C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COUPLING COEFFICIENT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEPTH, FREQ1, FREQ2, TETA1, TETA2, XK1, XK2
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DANG, DEUPI, GRAVIT, KERBOU, PI, VAR1, VAR2, VAR3, VAR4
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>QTRIA2()

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
!> </td><td> 11/06/98
!> </td><td> EDF/DER/LNH
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEPTH
!></td><td>--></td><td>PROFONDEUR
!>    </td></tr>
!>          <tr><td>FREQ1
!></td><td>--></td><td>FREQUENCE DE L'ONDE 1
!>    </td></tr>
!>          <tr><td>FREQ2
!></td><td>--></td><td>FREQUENCE DE L'ONDE 2
!>    </td></tr>
!>          <tr><td>TETA1
!></td><td>--></td><td>PLAN DE L'ONDE 1
!>    </td></tr>
!>          <tr><td>TETA2
!></td><td>--></td><td>PLAN DE L'ONDE 2
!>    </td></tr>
!>          <tr><td>XK1
!></td><td>--></td><td>NOMBRE D'ONDE DE 1
!>    </td></tr>
!>          <tr><td>XK2
!></td><td>--></td><td>NOMBRE D'ONDE DE 2
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        FUNCTION KERBOU
     &( XK1   , XK2   , FREQ1  , FREQ2  , DEPTH , TETA1 , TETA2 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEPTH          |-->| PROFONDEUR
C| FREQ1          |-->| FREQUENCE DE L'ONDE 1
C| FREQ2          |-->| FREQUENCE DE L'ONDE 2
C| TETA1          |-->| PLAN DE L'ONDE 1
C| TETA2          |-->| PLAN DE L'ONDE 2
C| XK1            |-->| NOMBRE D'ONDE DE 1
C| XK2            |-->| NOMBRE D'ONDE DE 2
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      DOUBLE PRECISION  XK1, XK2, FREQ1, FREQ2 , TETA1 , TETA2
      DOUBLE PRECISION  DEPTH
      DOUBLE PRECISION  KERBOU
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      DOUBLE PRECISION  VAR1 , VAR2 , VAR3 , VAR4 , DANG
      DOUBLE PRECISION  PI , DEUPI , GRAVIT
      PARAMETER(PI=3.141592654D0, DEUPI=2.D0*PI )
      PARAMETER(GRAVIT=9.81D0)
C
C
      VAR1  = XK1*XK1
      VAR2  = XK2*XK2
      VAR3  = XK1*XK2
      VAR4  = DEUPI*DEUPI*FREQ1*FREQ2
      DANG  = DCOS(TETA1-TETA2)
C
      KERBOU = GRAVIT*0.5D0*(VAR1+VAR2+2.D0*VAR3*DANG) +
     &         (VAR4/VAR3)*((VAR1+VAR2)*DANG+VAR3*(1+DANG*DANG))/
     &         DEPTH
C
      RETURN
      END
C
C#######################################################################
C