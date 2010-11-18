C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       USER DEFINED DAMPING FUNCTIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; DEFAULT VALUE FOR THE DUMPING
!>            FUNCTION (FRI) IS 1

!>  @warning  THE KEYWORD 'DAMPING FUNCTION' MUST BE SET TO 1 IN
!>            THE STEERING FILE (USER-DEFINED)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FRI, ITRAC, ITYP, NPOIN3, RI
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VISCLM()

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
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 25/11/97
!> </td><td> A MALCHEREK (HANOVRE); E PELTIER (LNH) 30 87 78 27; F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FRI
!></td><td><--</td><td>DAMPING FUNCTION
!>    </td></tr>
!>          <tr><td>ITRAC
!></td><td>--></td><td>TRACER NUMBER
!>    </td></tr>
!>          <tr><td>ITYP
!></td><td>--></td><td>=1 FOR VELOCITIES
!>                  =2 FOR TRACERS
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF POINTS IN THE 3D MESH
!>    </td></tr>
!>          <tr><td>RI
!></td><td>--></td><td>RICHARDSON NUMBER
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DRIUTI
     & (FRI, RI, ITYP, ITRAC, NPOIN3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FRI            |<--| DAMPING FUNCTION
C| ITRAC          |-->| TRACER NUMBER
C| ITYP           |-->| =1 FOR VELOCITIES
C|                |   | =2 FOR TRACERS
C| NPOIN3         |-->| NUMBER OF POINTS IN THE 3D MESH
C| RI             |-->| RICHARDSON NUMBER
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: ITYP, ITRAC, NPOIN3
      DOUBLE PRECISION, INTENT(INOUT) :: FRI(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: RI(NPOIN3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      IF(ITYP.EQ.1) THEN
!
C        DAMPING FUNCTION FOR VELOCITIES
!
         CALL OV('X=C     ',FRI,FRI,FRI,1.D0,NPOIN3)
!
      ELSEIF(ITYP.EQ.2) THEN
!
C        DAMPING FUNCTION FOR TRACERS
!
         CALL OV('X=C     ',FRI,FRI,FRI,1.D0,NPOIN3)
!
      ELSE
!
         IF (LNG.EQ.1) WRITE(LU,11) ITYP
         IF (LNG.EQ.2) WRITE(LU,12) ITYP
         CALL PLANTE(1)
         STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
11    FORMAT('DRIUTI: VARIABLE NON PREVUE ITYP: ',I2)
12    FORMAT('DRIUTI: UNEXPECTED PARAMETER ITYP: ',I2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DRIUTI
C
C#######################################################################
C