C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE REFERENCE DEPTH FOR THE BOUSSINESQ
!>                EQUATIONS. BY DEFAULT THIS IS THE INITIAL DEPTH.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note     THIS SUBROUTINE CAN BE USER-MODIFIED.
!>            FOR EXAMPLE IT CAN BE A LINEARISED DEPTH.
!>            TO GET BACK TO SAINT-VENANT, CAN HAVE H0 = 0.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC2D::H0 H0@endlink
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
!> </td><td> 01/03/1990
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COTINI
!></td><td>--></td><td>COTE INITIALE
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR INITIALE
!>    </td></tr>
!>          <tr><td>H0
!></td><td><--</td><td>HAUTEUR DE REFERENCE
!>    </td></tr>
!>          <tr><td>HAULIN
!></td><td>--></td><td>PROFONDEUR DE LINEARISATION
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU PRIVE POUR L'UTILISATEUR.
!>    </td></tr>
!>          <tr><td>X,Y,(Z)
!></td><td>--></td><td>COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>FOND A MODIFIER.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE HREF
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COTINI         |-->| COTE INITIALE
C| H             |-->| HAUTEUR INITIALE
C| H0             |<--| HAUTEUR DE REFERENCE
C| HAULIN         |-->| PROFONDEUR DE LINEARISATION
C| MESH           |-->| MAILLAGE
C| PRIVE          |-->| TABLEAU PRIVE POUR L'UTILISATEUR.
C| X,Y,(Z)        |-->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
C| ZF             |-->| FOND A MODIFIER.
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
      CALL OS( 'X=Y     ' , H0 , H , H , 0.D0 )
C     NEXT LINE WILL DEGENERATE BOUSSINESQ INTO SAINT-VENANT
C     CALL OS( 'X=C     ' , H0 , H , H , 0.D0 )
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C