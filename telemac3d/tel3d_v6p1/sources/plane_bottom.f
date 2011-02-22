C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FOR EVERY 2D POINT, FINDS THE LAST PLANE WITH NO
!>                NORMAL HEIGHT ABOVE, I.E. DELTA(Z) EQUAL TO ZERO.
!>                IF NO PROBLEM IPBOT=0.
!><br>            IF TIDAL FLAT IPBOT=NPLAN-1, SO PLANE IPBOT+1
!>                ALWAYS EXISTS AND HAS THE FIRST FREE POINT, UNLESS
!>                THERE IS NO DEPTH.

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
!> </td><td> 21/08/09
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE PLANE_BOTTOM
     &(IPBOT,Z,NPOIN2,NPLAN,SIGMAG,OPTBAN,HN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| HN             |-->| WATER DEPTH
C| IPBOT          |<--| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
C| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
C| NPOIN2         |-->| NUMBER OF 2D POINTS
C| OPTBAN         |-->| 
C| SIGMAG         |-->| 
C| Z              |-->| ELEVATION OF POINTS IN THE MESH
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: NPOIN2,NPLAN,OPTBAN
      INTEGER, INTENT(INOUT)       :: IPBOT(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN),HN(NPOIN2)
      LOGICAL, INTENT(IN)          :: SIGMAG
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPOIN2,IPLAN
C
      DOUBLE PRECISION EPSILON
      DATA EPSILON/1.D-4/
C
C-----------------------------------------------------------------------
C
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
C
        DO IPOIN2=1,NPOIN2
          IPBOT(IPOIN2)=0
          DO IPLAN=1,NPLAN-1
            IF(Z(IPOIN2,IPLAN+1)-Z(IPOIN2,IPLAN).LT.EPSILON) THEN
              IPBOT(IPOIN2)=IPLAN
            ENDIF
          ENDDO
        ENDDO
C
      ELSE
C
        DO IPOIN2=1,NPOIN2
          IPBOT(IPOIN2)=0
        ENDDO
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
