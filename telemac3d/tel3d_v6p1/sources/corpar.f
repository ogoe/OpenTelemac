C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES VERTICAL AND HORIZONTAL CORIOLIS PARAMETERS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  PHILAT IN *GRAD* IS POSITIVE FOR THE NORTHERN HEMISPHERE
!>         AND NEGATIVE FOR THE SOUTHERN HEMISPHERE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FHORI, FVERT, PHILATI
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DAYASTR, PI
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI, UNIVERSITAET HANNOVER
!> </td><td> FORTRAN95
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FHORI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FVER, FHOR
!></td><td><--</td><td>VERTICAL, HORIZONTAL CORIOLIS PARAMETERS
!>    </td></tr>
!>          <tr><td>FVERT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PHILAT
!></td><td>--></td><td>GEOGRAPHICAL LATITUDE IN GRAD, POSITIVE FOR
!>                  NORTHERN AND NEGATIVE FOR SOUTHERN HEMISPHERE
!>    </td></tr>
!>          <tr><td>PHILATI
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                       SUBROUTINE CORPAR
     &  (FVERT,FHORI,PHILATI)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FHORI          |---| 
C| FVER, FHOR     |<--| VERTICAL, HORIZONTAL CORIOLIS PARAMETERS
C| FVERT          |---| 
C| PHILAT         |-->| GEOGRAPHICAL LATITUDE IN GRAD, POSITIVE FOR
C|                |   | NORTHERN AND NEGATIVE FOR SOUTHERN HEMISPHERE
C| PHILATI        |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(INOUT) :: FVERT,FHORI
      DOUBLE PRECISION, INTENT(IN)    :: PHILATI
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION PI,DAYASTR
      PARAMETER (PI=3.141592654D0, DAYASTR = 86164.091D0)
!
      FVERT = (4.0D0 * PI / DAYASTR) * SIN (PHILATI*PI/180.0D0)
      FHORI = (4.0D0 * PI / DAYASTR) * COS (PHILATI*PI/180.0D0)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C