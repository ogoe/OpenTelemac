C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       LIMITS 3D HORIZONTAL EDGE BY EDGE FLUXES ON POINTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FLOW, FLULIM, NPLAN, NSEG2D
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IPLAN, ISEG, ISEG3D
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FLUX3D(), TEL4DEL()

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
!> </td><td> 19/05/09
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FLOW
!></td><td>--></td><td>FLUXES (SIZE OF FLOW MAY NOT EXCEED
!>                  NSEG2D*NPLAN, THOUGH THE TOTAL NUMBER OF
!>                  SEGMENTS IS LARGER)
!>    </td></tr>
!>          <tr><td>FLULIM
!></td><td>--></td><td>LIMITING FACTOR OF 2D SEGMENTS
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES
!>    </td></tr>
!>          <tr><td>NSEG2D
!></td><td>--></td><td>NUMBER OF SEGMENTS IN 2D
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUX3DLIM
     &(FLOW,FLULIM,NPLAN,NSEG2D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FLOW           |-->| FLUXES (SIZE OF FLOW MAY NOT EXCEED
C|                |   | NSEG2D*NPLAN, THOUGH THE TOTAL NUMBER OF
C|                |   | SEGMENTS IS LARGER)
C| FLULIM         |-->| LIMITING FACTOR OF 2D SEGMENTS
C| NPLAN          |-->| NUMBER OF PLANES
C| NSEG2D         |-->| NUMBER OF SEGMENTS IN 2D
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
      INTEGER, INTENT(IN)             :: NSEG2D,NPLAN
      DOUBLE PRECISION, INTENT(INOUT) :: FLOW(*)
C                                        HERE * = NESG2D*NPLAN
      DOUBLE PRECISION, INTENT(IN)    :: FLULIM(NSEG2D)
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER ISEG,ISEG3D,IPLAN
C
C-----------------------------------------------------------------------
C
C     LIMITS 3D FLUXES BY COEFFICIENT OF 2D FLUXES
C
      DO IPLAN=1,NPLAN
        DO ISEG=1,NSEG2D
          ISEG3D=ISEG+(IPLAN-1)*NSEG2D
          FLOW(ISEG3D)=FLOW(ISEG3D)*FLULIM(ISEG)
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C