C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES HW* AT EACH NODE WITH THE HELP OF HW*
!>                AT EACH LEVEL.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> HWSTAR, NPLAN, NPOIN2, WS, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPSH, HH, I, IAD, IPLAN
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRECON()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 22/01/09
!> </td><td> J-M HERVOUET   (LNHE) 01 30 87 80 18
!> </td><td> VELOCITY CANCELLED IF DEPTH LESS THAN 1 CM
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>HWSTAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WSS
!></td><td><--</td><td>COMPOSANTE WSTAR DE LA VITESSE
!>    </td></tr>
!>          <tr><td>Z
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE WSTAR
     & (WS,HWSTAR,Z,NPOIN2,NPLAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| HWSTAR         |---| 
C| NPLAN          |---| 
C| NPOIN2         |---| 
C| WS             |---| 
C| WSS            |<--| COMPOSANTE WSTAR DE LA VITESSE
C| Z             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ),    INTENT(IN   ) :: HWSTAR
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: WS
      DOUBLE PRECISION,  INTENT(IN   ) :: Z(*)
      INTEGER,           INTENT(IN   ) :: NPOIN2,NPLAN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,IAD,IPLAN
      DOUBLE PRECISION HH,EPSH
      DATA EPSH/1.D-2/
!
!-----------------------------------------------------------------------
!
C     INITIALISES TO 0.D0
C     WILL REMAIN FOR BOTTOM, FREE SURFACE, AND TIDAL FLATS WITH
C     DEPTH LESS THAN A MINIMUM
!
      DO I=1,NPOIN2*NPLAN
        WS%R(I)=0.D0
      ENDDO
!
C     INTERMEDIATE LEVELS
!
      IF(NPLAN.GT.2) THEN
!
        DO I=1,NPOIN2
          HH=Z(I+(NPLAN-1)*NPOIN2)-Z(I)
          IF(HH.GT.EPSH) THEN
            DO IPLAN = 2,NPLAN-1
              IAD=(IPLAN-1)*NPOIN2+I
              WS%R(IAD) = (HWSTAR%R(IAD)+HWSTAR%R(IAD-NPOIN2))*0.5D0
            ENDDO
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C