C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES PROPAGATION DEPTH.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> H, HAULIN, HN, HPROP, NSOUSI, PROLIN, TETA
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
!>      <td><center> 5.8                                       </center>
!> </td><td> 16/07/2007
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>H
!></td><td><--</td><td>HAUTEUR
!>    </td></tr>
!>          <tr><td>HAULIN
!></td><td>--></td><td>PROFONDEUR MOYENNE POUR LA LINEARISATION
!>    </td></tr>
!>          <tr><td>HN
!></td><td><--</td><td>HAUTEUR AU PAS DE TEMPS PRECEDENT
!>    </td></tr>
!>          <tr><td>HPROP
!></td><td><--</td><td>HAUTEUR DE PROPAGATION
!>    </td></tr>
!>          <tr><td>NSOUSI
!></td><td>--></td><td>NOMBRE DE SOUS ITERATIONS
!>    </td></tr>
!>          <tr><td>PROLIN
!></td><td>--></td><td>CORRESPOND AU MOT CLE:"PROPAGATON LINEARISEE"
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>--></td><td>SEMI-IMPLICITATION SUR H.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE HPROPA
     &(HPROP ,HN,H,PROLIN,HAULIN,TETA,NSOUSI)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| H             |<--| HAUTEUR
C| HAULIN         |-->| PROFONDEUR MOYENNE POUR LA LINEARISATION
C| HN             |<--| HAUTEUR AU PAS DE TEMPS PRECEDENT
C| HPROP          |<--| HAUTEUR DE PROPAGATION
C| NSOUSI         |-->| NOMBRE DE SOUS ITERATIONS
C| PROLIN         |-->| CORRESPOND AU MOT CLE:"PROPAGATON LINEARISEE"
C| TETA           |-->| SEMI-IMPLICITATION SUR H.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: NSOUSI
      LOGICAL, INTENT(IN)           :: PROLIN
      DOUBLE PRECISION, INTENT(IN)  :: TETA,HAULIN
      TYPE(BIEF_OBJ), INTENT(IN)    :: HN,H
      TYPE(BIEF_OBJ), INTENT(INOUT) :: HPROP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      IF(PROLIN) THEN
        CALL OS( 'X=C     ' , X=HPROP , C=HAULIN    )
      ELSEIF(NSOUSI.EQ.1) THEN
        CALL OS( 'X=Y     ' , X=HPROP , Y=HN )
      ELSE
        CALL OS( 'X=CY    ' , X=HPROP , Y=HN , C=1.D0-TETA )
        CALL OS( 'X=X+CY  ' , X=HPROP , Y=H  , C= TETA )
      ENDIF
C
C-----------------------------------------------------------------------
C
C     CLIPS HPROP
C
      IF(.NOT.PROLIN) THEN
        CALL OS('X=+(Y,C)',X=HPROP,Y=HPROP,C=0.D0)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C