C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE REFERENCE CELERITY (C0)
!>                WHICH IS USED IN THE CASE OF INCIDENTAL WAVE.<br>
!>                HERE C0 = SQRT (G H INITIAL),
!>                BUT C0 IS A BOUNDARY ARRAY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::C0 C0@endlink, 
!> @link DECLARATIONS_TELEMAC2D::GRAV GRAV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MESH MESH@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), OSBD()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 17/08/1994
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C0
!></td><td><--</td><td>CELERITE INITIALE
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>PESANTEUR
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR INITIALE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CELERITE
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C0             |<--| CELERITE INITIALE
C| GRAV           |-->| PESANTEUR
C| H             |-->| HAUTEUR INITIALE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
      CALL OSBD( 'X=CY    ' , C0 , H  , H , GRAV , MESH )
      CALL OS  ( 'X=+(Y,C)' , C0 , C0 , H , 0.D0        )
      CALL OS  ( 'X=SQR(Y)' , C0 , C0 , H , 0.D0        )
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C