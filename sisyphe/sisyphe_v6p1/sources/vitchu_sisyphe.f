C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FALL VELOCITY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DENS, DM, GRAV, VCE, WS
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INIT_SEDIMENT()

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
!> </td><td> 20/05/96
!> </td><td>
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DENS
!></td><td>--></td><td>POIDS DEJAUGE
!>    </td></tr>
!>          <tr><td>DM
!></td><td>--></td><td>DIAMETRE MOYEN DU SEDIMENT
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>VCE
!></td><td>--></td><td>VISCOSITE DE L'EAU
!>    </td></tr>
!>          <tr><td>WS
!></td><td>--></td><td>VITESSE DE CHUTE DES PARTICULES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VITCHU_SISYPHE
     & ( WS , DENS , DM , GRAV , VCE )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DENS           |-->| POIDS DEJAUGE
C| DM             |-->| DIAMETRE MOYEN DU SEDIMENT
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| VCE            |-->| VISCOSITE DE L'EAU
C| WS             |-->| VITESSE DE CHUTE DES PARTICULES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN)    :: DENS,  DM,  GRAV, VCE
      DOUBLE PRECISION, INTENT(INOUT) :: WS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C FALL VELOCITY
C ================
C
      IF (DM.LT.1.D-4) THEN
        WS = DENS * DM * DM * GRAV / ( 18.D0 * VCE )
      ELSEIF (DM.LT.1D-3) THEN
        WS = 10.D0 * VCE / DM * (SQRT( 1.D0 + 0.01D0* DENS * GRAV *
     &       DM**3.D0 / (VCE*VCE) ) -1.D0 )
      ELSE
        WS = 1.1D0 * SQRT( DENS * GRAV * DM )
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE VITCHU_SISYPHE
C
C#######################################################################
C