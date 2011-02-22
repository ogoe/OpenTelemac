C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHAINE, LNG, LU, NCAR
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CONFIG, NC, YACONFIG
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BIEF_INIT()

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
!> </td><td>
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHAINE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LNG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCAR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE READ_CONFIG
     &(LNG,LU,CHAINE,NCAR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHAINE         |---| 
C| LNG            |---| 
C| LU             |---| 
C| NCAR           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER      , INTENT(INOUT) :: LNG,LU
      CHARACTER*250, INTENT(IN)    :: CHAINE
      INTEGER      , INTENT(IN)    :: NCAR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL YACONFIG
      INTEGER NC
      CHARACTER*257 CONFIG
C
C-----------------------------------------------------------------------
C
      IF(NCAR.GT.0) THEN
        CONFIG(1:NCAR+6)=CHAINE(1:NCAR) // 'CONFIG'
        NC=NCAR+6
      ELSE
        CONFIG(1:6)='CONFIG'
        NC=6
      ENDIF
C
      YACONFIG=.FALSE.
      INQUIRE(FILE=CONFIG(1:NC),EXIST=YACONFIG)
      IF(YACONFIG) THEN
C
        OPEN(40,FILE=CONFIG(1:NC), FORM='FORMATTED')
        READ(40,*) LNG
C
C$DC$:  DO NOT OVERLOAD LU IN PARALLEL MODE (WINNT)
C       (KEEP THE REDIRECTION ON CHANNEL 95 MADE BY P_INIT)
        IF(LU.NE.95) READ(40,*) LU
        CLOSE(40)
C
      ELSE
C
        WRITE(LU,*) 'READ_CONFIG : FICHIER CONFIG NON TROUVE : ',CONFIG
        WRITE(LU,*) 'VALEURS PAR DEFAUT DE LU ET LNG : ',LU,' ET ',LNG
        WRITE(LU,*) ' '
        WRITE(LU,*) 'READ_CONFIG: FILE CONFIG NOT FOUND: ',CONFIG
        WRITE(LU,*) 'DEFAULTS VALUES OF LU AND LNG: ',LU,' AND ',LNG
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