C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PRESCRIBES THE FREE SURFACE ELEVATION FOR LEVEL
!>                IMPOSED LIQUID BOUNDARIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ENTET, I, N, TIME
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::COTIMP COTIMP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DIMP T3DIMP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3D_FILES T3D_FILES@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DEJA, FCT, J, OK
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 12/12/00
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ENTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I
!></td><td>--></td><td>NUMBER OF LIQUID BOUNDARY
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>GLOBAL NUMBER OF POINT
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION SL3
     &( I , TIME , N , ENTET )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ENTET          |---| 
C| I             |-->| NUMBER OF LIQUID BOUNDARY
C| N             |-->| GLOBAL NUMBER OF POINT
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
      INTEGER          , INTENT(IN) :: I,N
      DOUBLE PRECISION , INTENT(IN) :: TIME
      LOGICAL          , INTENT(IN) :: ENTET
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER*8 FCT
      INTEGER J
      LOGICAL DEJA,OK(99)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
C
C     FIRST CALL, INITIALISES OK TO .TRUE.
C
      IF(.NOT.DEJA) THEN
        DO J=1,99
          OK(J)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
C
C-----------------------------------------------------------------------
C
C     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
C     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
C                      IF  NO, OK IS SET  TO .FALSE.
C
      IF(OK(I).AND.T3D_FILES(T3DIMP)%NAME(1:1).NE.' ') THEN
C
C       FCT WILL BE SL(1), SL(2), ETC, SL(99), DEPENDING ON I
        FCT(1:3)='SL('
        IF(I.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') I
          FCT(5:8)=')   '
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') I
          FCT(6:8)=')  '
        ELSE
          CALL PLANTE(1)
          STOP 'SL3 NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
        ENDIF
        CALL READ_FIC_FRLIQ(SL3,FCT,TIME,T3D_FILES(T3DIMP)%LU,
     &                      ENTET,OK(I))
C
      ENDIF
C
      IF(.NOT.OK(I).OR.T3D_FILES(T3DIMP)%NAME(1:1).EQ.' ') THEN
C
C     PROGRAMMABLE PART
C     SL IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
C
        SL3 = COTIMP(I)
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