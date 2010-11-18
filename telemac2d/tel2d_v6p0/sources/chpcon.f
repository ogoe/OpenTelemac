C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE ADVECTION VECTOR FIELD UCONV,VCONV.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> TETAU, U, UCONV, UN, V, VCONV, VN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS()
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
!>          <tr><td>TETAU
!></td><td>--></td><td>IMPLICITATION SUR U.
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE.
!>    </td></tr>
!>          <tr><td>UCONV,VCONV
!></td><td>--></td><td>COMPOSANTES DU CHAMP CONVECTEUR.
!>    </td></tr>
!>          <tr><td>UN,VN
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE A L'ETAPE N.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CHPCON
     &(UCONV,VCONV,U,V,UN,VN,TETAU)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| TETAU          |-->| IMPLICITATION SUR U.
C| U,V            |-->| COMPOSANTES DE LA VITESSE.
C| UCONV,VCONV    |-->| COMPOSANTES DU CHAMP CONVECTEUR.
C| UN,VN          |-->| COMPOSANTES DE LA VITESSE A L'ETAPE N.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN)  :: TETAU
      TYPE(BIEF_OBJ), INTENT(IN)    :: U,UN,V,VN
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UCONV,VCONV
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C  CREATES CONVECTION ARRAYS UCONV AND VCONV
C
      CALL OS( 'X=CY    ' , UCONV , UN , U , 1.D0-TETAU )
      CALL OS( 'X=X+CY  ' , UCONV , U  , U ,      TETAU )
      CALL OS( 'X=CY    ' , VCONV , VN , U , 1.D0-TETAU )
      CALL OS( 'X=X+CY  ' , VCONV , V  , U ,      TETAU )
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C