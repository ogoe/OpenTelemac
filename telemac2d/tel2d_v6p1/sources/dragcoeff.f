C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE DRAG COEFFICIENT BEHIND A CYLINDER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CW, D, V, VK
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> RE
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
!>          <tr><td>CW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>D
!></td><td>--></td><td>DIAMETER
!>    </td></tr>
!>          <tr><td>V
!></td><td>--></td><td>VELOCITY UPSTREAM
!>    </td></tr>
!>          <tr><td>VK
!></td><td>--></td><td>LAMINAR VISCOSITY
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DRAGCOEFF
     & (V, D, VK, CW)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CW             |---| 
C| D             |-->| DIAMETER
C| V             |-->| VELOCITY UPSTREAM
C| VK             |-->| LAMINAR VISCOSITY
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN)  :: V, D, VK
      DOUBLE PRECISION, INTENT(OUT) :: CW
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION              :: RE
!
!=======================================================================!
!=======================================================================!
!
      RE = V * D / VK
!
      IF (RE.LE.800.0D0) THEN
         CW = 3.07D0 / RE**(0.168D0)
      ELSEIF(RE.LE.6000.D0) THEN
         CW = 1.D0
      ELSEIF(RE.LE.11000.0D0) THEN
         CW = 1.0D0+0.2D0*(RE-6000.D0)/5000.D0
      ELSE
         CW = 1.2D0
      ENDIF
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END
C
C#######################################################################
C