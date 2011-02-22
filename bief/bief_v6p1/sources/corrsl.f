C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CORRECTS THE FREE SURFACE COMPUTATION BY ELEMENTS
!>                TO TAKE ACCOUNT OF THE TIDAL FLATS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MESH, NEWSL, OLDSL, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELM, NELEM, NELMAX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CORRSL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CRSL11(), CRSL12(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PROPAG()

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
!> </td><td> 27/11/92
!> </td><td> J-M JANIN (LNH) 30 87 72 84; J-M HERVOUET (LNH) 30 87 80 18
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
!>          <tr><td>NEWSL
!></td><td>---</td><td>SURFACE LIBRE MODIFIEE, PAR ELEMENTS
!>    </td></tr>
!>          <tr><td>OLDSL
!></td><td>--></td><td>SURFACE LIBRE REELLE.
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORRSL
     &(NEWSL,OLDSL,ZF,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MESH           |-->| STRUCTURE DE MAILLAGE
C| NEWSL          |---| SURFACE LIBRE MODIFIEE, PAR ELEMENTS
C| OLDSL          |-->| SURFACE LIBRE REELLE.
C| ZF             |-->| COTE DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CORRSL => CORRSL
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: NEWSL
      TYPE(BIEF_OBJ) , INTENT(IN)    :: OLDSL,ZF
      TYPE(BIEF_MESH), INTENT(IN)    :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NELEM,NELMAX,IELM
C
C-----------------------------------------------------------------------
C
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
C
      IELM=OLDSL%ELM
C
C-----------------------------------------------------------------------
C
C     VECTOR NEWSL IS MARKED AS BEING DISCONTINUOUS
      NEWSL%DIMDISC=IELM
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.11) THEN
C
        CALL CRSL11(NEWSL%R,OLDSL%R,ZF%R,MESH%IKLE%I,NELEM,NELMAX)
        NEWSL%ELM=10
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELM.EQ.12) THEN
C
        CALL CRSL12(NEWSL%R,OLDSL%R,ZF%R,MESH%IKLE%I,NELEM,NELMAX)
        NEWSL%ELM=10
C
C-----------------------------------------------------------------------
C
      ELSE
C
         IF(LNG.EQ.1) WRITE(LU,10) IELM
         IF(LNG.EQ.2) WRITE(LU,11) IELM
10       FORMAT(1X,'CORRSL : DISCRETISATION INCONNUE :',I6)
11       FORMAT(1X,'CORRSL : UNKNOWN DISCRETIZATION :',I6)
         CALL PLANTE(1)
         STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C