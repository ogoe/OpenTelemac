C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOWS THE USER TO CODE THEIR OWN BEDLOAD TRANSPORT
!>                FORMULATION, BEST SUITED TO THEIR APPLICATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; SAND TRANSPORT FORMULA MUST BE CODED BY THE USER

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_FORMULA()

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
!>      <td><center> 5.4                                       </center>
!> </td><td> **/11/2003
!> </td><td> F. HUVELIN
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 20/05/1996
!> </td><td>
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AC
!></td><td>--></td><td>CRITICAL SHIELDS PARAMETER
!>    </td></tr>
!>          <tr><td>CF
!></td><td>--></td><td>QUADRATIC FRICTION COEFFICIENT (TOTAL FRICTION)
!>    </td></tr>
!>          <tr><td>CFP
!></td><td>--></td><td>QUADRATIC FRICTION COEFFICIENT (SKIN FRICTION)
!>    </td></tr>
!>          <tr><td>CHESTR
!></td><td>--></td><td>FRICTION COEFFICIENT (HEZY, NIKURADSE OR STICKLER)
!>    </td></tr>
!>          <tr><td>DM
!></td><td>--></td><td>MEAN SAND DIAMETER
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITY
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>WATER DEPTH
!>    </td></tr>
!>          <tr><td>HW
!></td><td>--></td><td>WAVE HEIGHT
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS
!>    </td></tr>
!>          <tr><td>Q
!></td><td>--></td><td>WATER FLOW RATE
!>    </td></tr>
!>          <tr><td>QSS,QSC
!></td><td><--</td><td>SUSPENDED AND BED LOAD SAND TRANSPORT
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>--></td><td>MEAN BOTTOM FRICTION
!>    </td></tr>
!>          <tr><td>TOBW
!></td><td>--></td><td>WAVE INDUCED BOTTOM FRICTION
!>    </td></tr>
!>          <tr><td>TW
!></td><td>--></td><td>WAVE PERIOD
!>    </td></tr>
!>          <tr><td>VCE
!></td><td>--></td><td>FLOW VISCOSITY
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>--></td><td>FLOW DENSITY
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>--></td><td>SAND DENSITY
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE QSFORM
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |-->| CRITICAL SHIELDS PARAMETER
C| CF             |-->| QUADRATIC FRICTION COEFFICIENT (TOTAL FRICTION)
C| CFP            |-->| QUADRATIC FRICTION COEFFICIENT (SKIN FRICTION)
C| CHESTR         |-->| FRICTION COEFFICIENT (HEZY, NIKURADSE OR STICKLER)
C| DM             |-->| MEAN SAND DIAMETER
C| GRAV           |-->| GRAVITY
C| HN             |-->| WATER DEPTH
C| HW             |-->| WAVE HEIGHT
C| NPOIN          |-->| NUMBER OF POINTS
C| Q             |-->| WATER FLOW RATE
C| QSS,QSC        |<--| SUSPENDED AND BED LOAD SAND TRANSPORT
C| TOB            |-->| MEAN BOTTOM FRICTION
C| TOBW           |-->| WAVE INDUCED BOTTOM FRICTION
C| TW             |-->| WAVE PERIOD
C| VCE            |-->| FLOW VISCOSITY
C| XMVE           |-->| FLOW DENSITY
C| XMVS           |-->| SAND DENSITY
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C  FOLLOWING LINES NEED TO BE COMMENTED OUT
C
      IF(LNG.EQ.1) WRITE(LU,52)
      IF(LNG.EQ.2) WRITE(LU,53)
C
52    FORMAT(/,1X,' STOP :',/
     &     ,1X,' LE TAUX DE TRANSPORT DOIT ETRE
     &       CALCULE DANS QSFORM')
53    FORMAT(/,1X,'SISYPHE IS STOPPED : ',/
     &      ,1X,' SAND TRANSPORT MUST BE CALCULATED IN QSFORM')
      CALL PLANTE(1)
      STOP
C
      RETURN
      END
C
C#######################################################################
C