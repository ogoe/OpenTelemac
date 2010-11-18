C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       IDENTIFIES TIDAL FLATS.
!><br>            DRYING ELEMENT : TETA = 0,
!><br>            NORMAL ELEMENT : TETA = 1.
!><br>            THE CRITERION FOR DRYING ELEMENTS IS THAT OF
!>                J.-M. JANIN : BOTTOM ELEVATION OF A POINT IN AN
!>                ELEMENT BEING HIGHER THAN THE FREE SURFACE
!>                ELEVATION OF ANOTHER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MESH, SL, TETA, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELMS, IELMZ, NELEM, NELMAX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DECVRT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DECV11(), DECV21(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CVDFTR(), PROPAG()

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
!> </td><td> 09/12/94
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
!></td><td>--></td><td>STRUCTURE DE MAILLAGE
!>    </td></tr>
!>          <tr><td>SL,ZF
!></td><td>--></td><td>SURFACE LIBRE ET FOND
!>    </td></tr>
!>          <tr><td>TETA
!></td><td><--</td><td>INDICATEUR (PAR ELEMENT)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DECVRT
     &(TETA,SL,ZF,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MESH           |-->| STRUCTURE DE MAILLAGE
C| SL,ZF          |-->| SURFACE LIBRE ET FOND
C| TETA           |<--| INDICATEUR (PAR ELEMENT)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DECVRT => DECVRT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: TETA
      TYPE(BIEF_OBJ) , INTENT(IN)    :: SL,ZF
      TYPE(BIEF_MESH), INTENT(IN)    :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NELEM,NELMAX,IELMS,IELMZ
C
C-----------------------------------------------------------------------
C
      IELMS=SL%ELM
      IELMZ=ZF%ELM
C
C-----------------------------------------------------------------------
C
C  DEPLOYS THE MESH STRUCTURE
C
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
C
C-----------------------------------------------------------------------
C
      IF(IELMS.EQ.11.AND.IELMZ.EQ.11) THEN
C
        CALL DECV11(TETA%R,SL%R,ZF%R,MESH%IKLE%I,NELEM,NELMAX)
C
      ELSEIF((IELMS.EQ.21.AND.IELMZ.EQ.21).OR.
     &       (IELMS.EQ.12.AND.IELMZ.EQ.12)      ) THEN
C
        CALL DECV21(TETA%R,SL%R,ZF%R,MESH%IKLE%I,NELEM,NELMAX)
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,10) IELMS,IELMZ
        IF(LNG.EQ.2) WRITE(LU,11) IELMS,IELMZ
10      FORMAT(1X,'DECVRT : DISCRETISATION NON PREVUE :'    ,I6,' ',I6)
11      FORMAT(1X,'DECVRT : DISCRETIZATION NOT IMPLEMENTED:',I6,' ',I6)
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