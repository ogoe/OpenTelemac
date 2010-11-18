C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ASSEMBLES HORIZONTAL EDGE BY EDGE FLUXES ON POINTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FLOW, GLOSEG, INIFLO, NPLAN, NPOIN3, NSEG2D, SIZGLO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ISEG
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FLUX3D()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 18/05/09
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F
!></td><td><--</td><td>F WHERE THE FLUXES ARE ASSEMBLED
!>    </td></tr>
!>          <tr><td>FLOW
!></td><td>--></td><td>FLUXES (SIZE OF FLOW MAY NOT EXCEED
!>                  NSEG2D*NPLAN, THOUGH THE TOTAL NUMBER OF
!>                  SEGMENTS IS LARGER)
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>--></td><td>GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT
!>    </td></tr>
!>          <tr><td>INIFLO
!></td><td>--></td><td>IF(YES) F WILL BE INITIALISED AT 0.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF POINTS
!>    </td></tr>
!>          <tr><td>NSEG2D
!></td><td>--></td><td>NUMBER OF SEGMENTS IN 2D
!>    </td></tr>
!>          <tr><td>SIZGLO
!></td><td>--></td><td>FIRST DIMENSION OF GLOSEG
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ASSEG_3D
     &(FLOW,F,NPOIN3,NPLAN,NSEG2D,GLOSEG,SIZGLO,INIFLO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F             |<--| F WHERE THE FLUXES ARE ASSEMBLED
C| FLOW           |-->| FLUXES (SIZE OF FLOW MAY NOT EXCEED
C|                |   | NSEG2D*NPLAN, THOUGH THE TOTAL NUMBER OF
C|                |   | SEGMENTS IS LARGER)
C| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT
C| INIFLO         |-->| IF(YES) F WILL BE INITIALISED AT 0.
C| NPLAN          |-->| NUMBER OF PLANES
C| NPOIN3         |-->| NUMBER OF POINTS
C| NSEG2D         |-->| NUMBER OF SEGMENTS IN 2D
C| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER, INTENT(IN)             :: NSEG2D,NPOIN3,SIZGLO,NPLAN
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      DOUBLE PRECISION, INTENT(IN)    :: FLOW(*)
C                                        HERE * = NESG2D*NPLAN
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN3)
      LOGICAL, INTENT(IN)             :: INIFLO
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER ISEG,I
C
C-----------------------------------------------------------------------
C
C     INITIALISES THE FLOW TO 0.D0
C
      IF(INIFLO) THEN
        DO I = 1,NPOIN3
          F(I) = 0.D0
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
C     ASSEMBLES THE FLUXES OF HORIZONTAL SEGMENTS
C
      DO ISEG = 1,NSEG2D*NPLAN
        F(GLOSEG(ISEG,1))=F(GLOSEG(ISEG,1))+FLOW(ISEG)
        F(GLOSEG(ISEG,2))=F(GLOSEG(ISEG,2))-FLOW(ISEG)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C