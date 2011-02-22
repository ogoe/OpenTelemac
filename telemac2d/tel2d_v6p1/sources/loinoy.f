C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DISCHARGE LAW FOR A WET WEIR.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEB, G, PHI, YAM, YAV, YS
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CLSING()

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
!> </td><td> 03/10/1996
!> </td><td> J.-M. HERVOUET (LNH) 30 87 80 18
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 19/04/1996
!> </td><td> V. GUINOT (LHF)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEB
!></td><td><--</td><td>DEBIT DU SEUIL.
!>    </td></tr>
!>          <tr><td>G
!></td><td>--></td><td>GRAVITE.
!>    </td></tr>
!>          <tr><td>PHI
!></td><td>--></td><td>COEFFICIENT DE DEBIT DU SEUIL.
!>    </td></tr>
!>          <tr><td>YAM
!></td><td>--></td><td>COTE AMONT.
!>    </td></tr>
!>          <tr><td>YAV
!></td><td>--></td><td>COTE AVAL.
!>    </td></tr>
!>          <tr><td>YS
!></td><td>--></td><td>COTE DU SEUIL.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LOINOY
     &(YAM,YAV,YS,PHI,DEB,G)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEB            |<--| DEBIT DU SEUIL.
C| G             |-->| GRAVITE.
C| PHI            |-->| COEFFICIENT DE DEBIT DU SEUIL.
C| YAM            |-->| COTE AMONT.
C| YAV            |-->| COTE AVAL.
C| YS             |-->| COTE DU SEUIL.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(INOUT) :: DEB
      DOUBLE PRECISION, INTENT(IN)    :: G,YAM,YAV,PHI,YS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      IF(YAM.LT.YS.AND.YAV.LT.YS) THEN
        DEB=0.D0
      ELSE
        DEB=2.598D0*PHI*SQRT(2.D0*G)*(YAV-YS)*SQRT(YAM-YAV)
C           2.598D0 IS THE INVERSE OF SQRT(1/3)*2/3
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C