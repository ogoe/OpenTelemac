C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       THE USER MUST GIVE :<br>
!><br>   1) THE TIMESTEP WHEN THE FLOATING BODY IS RELEASED.<br>
!><br>   2) THE TIME WHEN THE COMPUTATION IS STOPPED FOR THIS FLOATING BODY.<br>
!><br>   3) THE INITIAL POSITION OF THE FLOATING BODY AT THE TIME OF RELEASE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEBFLO, FINFLO, FLOPRD, NFLOT, NIT, NITFLO, NPOIN, X, XFLOT, Y, YFLOT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IFLOT
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
!> </td><td> 17/08/1994
!> </td><td> J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEBFLO
!></td><td><--</td><td>TIME STEP OF INITIAL RELEASE
!>    </td></tr>
!>          <tr><td>FINFLO
!></td><td><--</td><td>TIME STEP FOR END OF FOLLOW UP
!>    </td></tr>
!>          <tr><td>FLOPRD
!></td><td>--></td><td>NUMBER OF TIME-STEPS BETWEEN 2 RECORDS OF
!>                  SUCCESSIVE POSITIONS OF FLOATING BODIES.
!>    </td></tr>
!>          <tr><td>NFLOT
!></td><td>--></td><td>NOMBRE DE FLOTTEURS.
!>    </td></tr>
!>          <tr><td>NIT
!></td><td>--></td><td>NUMBER OF TIME STEPS
!>    </td></tr>
!>          <tr><td>NITFLO
!></td><td>--></td><td>MAXIMUM NUMBER OF RECORDS OF SUCCESIVE
!>                  POSITIONS OF FLOATING BODIES.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDINATES OF POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>XFLOT,YFLOT
!></td><td><--</td><td>POSITIONS OF FLOATING BODIES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLOT
     &(XFLOT,YFLOT,NFLOT,NITFLO,FLOPRD,X,Y,NPOIN,DEBFLO,FINFLO,NIT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEBFLO         |<--| TIME STEP OF INITIAL RELEASE
C| FINFLO         |<--| TIME STEP FOR END OF FOLLOW UP
C| FLOPRD         |-->| NUMBER OF TIME-STEPS BETWEEN 2 RECORDS OF
C|                |   | SUCCESSIVE POSITIONS OF FLOATING BODIES.
C| NFLOT          |-->| NOMBRE DE FLOTTEURS.
C| NIT            |-->| NUMBER OF TIME STEPS
C| NITFLO         |-->| MAXIMUM NUMBER OF RECORDS OF SUCCESIVE
C|                |   | POSITIONS OF FLOATING BODIES.
C| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
C| X,Y            |-->| COORDINATES OF POINTS IN THE MESH
C| XFLOT,YFLOT    |<--| POSITIONS OF FLOATING BODIES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN,NIT,NFLOT,NITFLO,FLOPRD
      INTEGER, INTENT(INOUT)          :: DEBFLO(NFLOT),FINFLO(NFLOT)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NITFLO,NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NITFLO,NFLOT)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IFLOT
C
C-----------------------------------------------------------------------
C
C  1) STEP FOR THE BEGINNING OF RELEASE (DEBFLO)
C  2) STEP FOR THE END OF RELEASE (FINFLO)
C
      DO 10 IFLOT=1,NFLOT
         DEBFLO(IFLOT) = 1
         FINFLO(IFLOT) = NIT
10    CONTINUE
C
C-----------------------------------------------------------------------
C
C  3) COORDINATES OF FLOATING BODIES AT THE BEGINNING
C
C     INITIAL POSITION OF FLOATING BODIES WHEN RELEASED.
C     DEFAULT VALUE NUMBER "IFLOT" IS RELEASED AT POINT "IFLOT"
C
C-----------------------------------------------------------------------
C
C     XFLOT(1,1)=-300000.D0
C     YFLOT(1,1)= 300000.D0
C
C     XFLOT(1,2)= 0.D0
C     YFLOT(1,2)= 300000.D0
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C