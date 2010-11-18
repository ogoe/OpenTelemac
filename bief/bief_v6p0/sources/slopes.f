C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENT 1 / COS(ALFA)
!>                WHERE ALFA IS THE SLOPE OF A TRIANGULAR ELEMENT.
!><br>            THIS COEFFICIENT IS USED IN THE BOTTOM FRICTION
!>                TERM.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> COEF, MESH, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELM, NELEM, NELMAX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SLOPES
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NBPTS(), PLANTE(), SLOP10()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>COST_FUNCTION(), PROPAG(), PROPAG_ADJ(), WAVE_EQUATION()

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
!> </td><td> 17/08/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COEF
!></td><td><--</td><td>RESULTAT
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COTE DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SLOPES
     &(COEF,Z,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COEF           |<--| RESULTAT
C| MESH           |-->| MAILLAGE
C| Z             |-->| COTE DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SLOPES => SLOPES
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  STRUCTURES OF VECTOR
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: COEF
      TYPE(BIEF_OBJ), INTENT(IN)    :: Z
C
C-----------------------------------------------------------------------
C
C  MESH STRUCTURE
C
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER NELEM,NELMAX,IELM
C
C-----------------------------------------------------------------------
C
      IELM   = MESH%TYPELM
      NELEM  = MESH%NELEM
      NELMAX = MESH%NELMAX
C
C-----------------------------------------------------------------------
C
      COEF%ELM = IELM
      COEF%DIM1= NBPTS(IELM)
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.10) THEN
        CALL SLOP10(COEF%R,MESH%XEL%R,MESH%YEL%R,
     &              Z%R,MESH%IKLE%I,NELEM,NELMAX)
      ELSE
        IF(LNG.EQ.1) WRITE(LU,300) MESH%TYPELM
        IF(LNG.EQ.2) WRITE(LU,301) MESH%TYPELM
300     FORMAT(1X,'SLOPES (BIEF) : ELEMENT NON PREVU : ',1I6)
301     FORMAT(1X,'SLOPES (BIEF) : UNKNOWN ELEMENT:',1I6)
        CALL PLANTE(0)
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