C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GOES FROM A VECTOR BY POINTS TO A VECTOR BY ELEMENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MESH, X, XEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PTTOEL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), PTEL11()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 10/01/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>VECTEUR PAR POINTS
!>    </td></tr>
!>          <tr><td>XEL
!></td><td><--</td><td>VECTEUR SUR LES ELEMENTS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PTTOEL
     &(XEL,X,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MESH           |---| 
C| X             |-->| VECTEUR PAR POINTS
C| XEL            |<--| VECTEUR SUR LES ELEMENTS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PTTOEL => PTTOEL
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C  VECTORS STRUCTURES
C
      TYPE(BIEF_OBJ), INTENT(IN)    :: X
      TYPE(BIEF_OBJ), INTENT(INOUT) :: XEL
C
C-----------------------------------------------------------------------
C
C  MESH STRUCTURE
C
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      IF(X%ELM.EQ.11) THEN
C
        CALL PTEL11(XEL%R,X%R,MESH%IKLE%I,MESH%NELMAX,MESH%NELEM)
C
      ELSE
       IF (LNG.EQ.1) WRITE(LU,100) X%ELM
       IF (LNG.EQ.2) WRITE(LU,101) X%ELM
100    FORMAT(1X,'PTTOEL (BIEF) : IELM = ',1I6,' ELEMENT NON PREVU')
101    FORMAT(1X,'PTTOEL (BIEF) : IELM = ',1I6,' ELEMENT NOT AVAILABLE')
       CALL PLANTE(1)
       STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C