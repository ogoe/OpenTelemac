C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       IDENTIFIES TIDAL FLATS.<br>
!><br>            IMPLEMENTS DELFINA ET AL WETTING/DRYING ALGORITHM.
!><br>            PARTIALLY WET ELEMENT : TETA = 0
!><br>            WET ELEMENT           : TETA = NU = 1.0
!><br>            DRY ELEMENT           : TETA = NU = 0.0<br>
!><br>            THE DRYING CRITERION IS THAT OF J.-M. JANIN :
!>                BOTTOM ELEVATION AT ONE NODE OF THE ELEMENT IS
!>                HIGHER THAN FREE SURFACE ELEVATION AT ANOTHER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> HN, MESH, TETA, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELMH, IELMZ
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_POROS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CORPOR(), PLANTE(), PORO11()
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
!> </td><td> 01/08/1997
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; PAUL BATES (BRISTOL) 44 117 928 9108
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>HN,ZF
!></td><td>--></td><td>HAUTEUR ET FOND
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>STRUCTURE DE MAILLAGE
!>    </td></tr>
!>          <tr><td>TETA
!></td><td><--</td><td>NU (PAR ELEMENT)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE POROS
     &(TETA,ZF,HN,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| HN,ZF          |-->| HAUTEUR ET FOND
C| MESH           |-->| STRUCTURE DE MAILLAGE
C| TETA           |<--| NU (PAR ELEMENT)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_POROS => POROS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(IN)    :: ZF,HN
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TETA
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELMZ,IELMH
C
      IELMZ=ZF%ELM
      IELMH=HN%ELM
C
C-----------------------------------------------------------------------
C
C     1) COMPUTES POROSITY ON TIDAL FLATS
C
      IF(IELMZ.EQ.11.AND.IELMH.EQ.11) THEN
C
        CALL PORO11(TETA%R,ZF%R,
     &              HN%R,MESH%IKLE%I,MESH%NELEM,MESH%NELMAX)
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,10) IELMH,IELMZ
        IF(LNG.EQ.2) WRITE(LU,11) IELMH,IELMZ
10      FORMAT(1X,'POROS : DISCRETISATION NON PREVUE :'    ,I6,' ',I6)
11      FORMAT(1X,'POROS : DISCRETIZATION NOT IMPLEMENTED:',I6,' ',I6)
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C     2) CORRECTED BY A USER SUBROUTINE
C
      CALL CORPOR(TETA)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C