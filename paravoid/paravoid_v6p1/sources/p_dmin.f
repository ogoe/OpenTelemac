!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @brief
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MYPART
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>
!
!>  @par Called by
!><br>CVTRVF(), CVTRVF_POS(), FLUXPR(), FLUXPR_SISYPHE(), FLUXPR_TELEMAC2D(), MURD3D(), POSITIVE_DEPTHS(), PRERES_TELEMAC2D(), SISYPHE(), SPECTRE(), SUSPENSION_LISTING(), TELEMAC2D()
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
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
!>  </table>
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>MYPART
!></td><td>--></td><td>CONTRIBUTION DU PROCESSEUR APPELANT.
!>    </td></tr>
!>     </table>
!
!#######################################################################
!
                        DOUBLE PRECISION FUNCTION P_DMIN
     &(MYPART)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MYPART         |-->| CONTRIBUTION DU PROCESSEUR APPELANT.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION MYPART
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_DMIN VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_DMIN IN ITS VOID VERSION'
!
      P_DMIN=MYPART
!
!-----------------------------------------------------------------------
!
      STOP
      END
!
!
!#######################################################################
!