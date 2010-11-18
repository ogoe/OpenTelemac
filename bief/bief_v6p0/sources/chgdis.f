C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CHANGES THE DISCRETISATION OF A VECTOR.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MESH, NEWELT, OLDELT, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CHGDIS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CG1112(), CG1113(), NBPTS(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BILANT1(), CARACT(), COEFRO(), CVDFTR(), DRAGFO(), FRICTI(), FRICTION_UNIF(), FRICTION_ZONES(), MESURES(), PROPAG(), PROSOU(), STREAMLINE(), STREAMLINE_TOMAWAC(), TELEMAC2D(), TELEMAC3D()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 13/02/08
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>STRUCTURE DE MAILLAGE
!>    </td></tr>
!>          <tr><td>NEWELT
!></td><td>--></td><td>NOUVEAU TYPE
!>    </td></tr>
!>          <tr><td>OLDELT
!></td><td>--></td><td>ANCIEN TYPE
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VECTOR A MODIFIER
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CHGDIS
     &(X,OLDELT,NEWELT,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MESH           |-->| STRUCTURE DE MAILLAGE
C| NEWELT         |-->| NOUVEAU TYPE
C| OLDELT         |-->| ANCIEN TYPE
C| X             |<--| VECTOR A MODIFIER
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CHGDIS => CHGDIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      INTEGER, INTENT(IN)           :: OLDELT,NEWELT
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      IF(NBPTS(NEWELT).GT.X%MAXDIM1) THEN
        IF(LNG.EQ.1) WRITE(LU,200) X%NAME
        IF(LNG.EQ.2) WRITE(LU,201) X%NAME
200     FORMAT(1X,'CHGDIS (BIEF) : EXTENSION IMPOSSIBLE POUR ',A6)
201     FORMAT(1X,'CHGDIS (BIEF) : EXTENSION IMPOSSIBLE FOR ',A6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      X%DIM1 = NBPTS(NEWELT)

      IF(OLDELT.EQ.11.AND.NEWELT.EQ.12) THEN
C
        CALL CG1112(X%R,X%DIM1,X%DIM2,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX)
C
      ELSEIF(OLDELT.EQ.11.AND.NEWELT.EQ.13) THEN
C
        CALL CG1113(X%R,X%DIM1,X%DIM2,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX)
C
      ELSEIF((OLDELT.EQ.12.OR.OLDELT.EQ.13).AND.NEWELT.EQ.11) THEN
C
C       DOES NOTHING
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,10) OLDELT,NEWELT
        IF(LNG.EQ.2) WRITE(LU,11) OLDELT,NEWELT
10      FORMAT(1X,'CHGDIS : CAS NON PREVU :'    ,I6,' ',I6)
11      FORMAT(1X,'CHGDIS: CASE NOT IMPLEMENTED:',I6,' ',I6)
        WRITE(LU,*) 'STRUCTURE X = ',X%NAME
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      X%ELM=NEWELT
      X%DIM1=NBPTS(NEWELT)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C