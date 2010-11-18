C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PRESCRIBES THE DISCHARGE FOR FLOW IMPOSED
!>                LIQUID BOUNDARIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ENTET, I, TIME
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::DEBIMP DEBIMP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DIMP T3DIMP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3D_FILES T3D_FILES@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DEJA, FCT, N, OK
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), READ_FIC_FRLIQ()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BORD3D()

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
!> </td><td> 08/04/09
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TIME
!>    </td></tr>
!>          <tr><td>ENTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I
!></td><td>--></td><td>NUMBER OF THE LIQUID BOUNDARY.
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>IF YES, LISTING PRINTOUTS ALLOWED
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION Q3
     &( I , TIME , ENTET )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TIME
C| ENTET          |---| 
C| I             |-->| NUMBER OF THE LIQUID BOUNDARY.
C| INFOGR         |-->| IF YES, LISTING PRINTOUTS ALLOWED
C| TIME           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER          , INTENT(IN) :: I
      DOUBLE PRECISION , INTENT(IN) :: TIME
      LOGICAL          , INTENT(IN) :: ENTET
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER*8 FCT
      INTEGER N
      LOGICAL DEJA,OK(99)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
C
C     FIRST CALL, INITIALISES OK TO .TRUE.
C
      IF(.NOT.DEJA) THEN
        DO N=1,99
          OK(N)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
C
C     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
C     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
C                      IF  NO, OK IS SET  TO .FALSE.
C
      IF(OK(I).AND.T3D_FILES(T3DIMP)%NAME(1:1).NE.' ') THEN
C
C       FCT WILL BE Q(1), Q(2), ETC, Q(99), DEPENDING ON I
        FCT(1:2)='Q('
        IF(I.LT.10) THEN
          WRITE(FCT(3:3),FMT='(I1)') I
          FCT(4:8)=')    '
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(3:4),FMT='(I2)') I
          FCT(5:8)=')   '
        ELSE
          CALL PLANTE(1)
          STOP 'Q3 NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
        ENDIF
        CALL READ_FIC_FRLIQ(Q3,FCT,TIME,T3D_FILES(T3DIMP)%LU,
     &                      ENTET,OK(I))
C
      ENDIF
C
      IF(.NOT.OK(I).OR.T3D_FILES(T3DIMP)%NAME(1:1).EQ.' ') THEN
C
C     PROGRAMMABLE PART
C     Q IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
C
        Q3 = DEBIMP(I)
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