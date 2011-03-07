!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @brief       REDUCES A VECTOR OF LOGICALS WITH DIFFUSION OF
!>                THE RESULT TO ALL THE PROCESSORS.
!>               (DUMMY VERSION)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IARG1, LARG2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>
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
!>  <tr>
!>    <td><center> 5.8                                    </center></td>
!>    <td> 01/07/2006                                              </td>
!>    <td> O.BOITEAU (SINETICS)                                    </td>
!>    <td>                                                         </td>
!>  </table>
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IARG1
!></td><td>--></td><td>TAILLE DU VECTEUR
!>    </td></tr>
!>          <tr><td>LARG2
!></td><td><-></td><td>CONTRIBUTION DU PROCESSEUR APPELANT.
!>    </td></tr>
!>     </table>
!
!#######################################################################
!
                            SUBROUTINE P_LSUM
     &(IARG1,LARG2)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IARG1          |-->| TAILLE DU VECTEUR
!| LARG2          |<->| CONTRIBUTION DU PROCESSEUR APPELANT.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IARG1
      LOGICAL, DIMENSION(IARG1), INTENT(OUT) :: LARG2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_LSUM VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_LSUM IN ITS VOID VERSION'
!
!     JMH 14/01/2008, TO AVOID A WARNING CAUSED BY INTENT(OUT)
      LARG2 = .FALSE.
!
      RETURN
      END
!
!#######################################################################
!