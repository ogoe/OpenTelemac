C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PRESCRIBES THE DISCHARGE FOR FLOW IMPOSED
!>                LIQUID BOUNDARIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC2D, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::AT AT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DEBIT DEBIT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ENTET ENTET@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MAXFRO MAXFRO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NDEBIT NDEBIT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2DIMP T2DIMP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2D_FILES T2D_FILES@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DEJA, FCT, N, OK
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_Q
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), READ_FIC_FRLIQ()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BORD(), INIT_ZERO(), POINT_SISYPHE(), PREDES(), SISYPHE()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 09/01/2004
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>I
!></td><td>--></td><td>NUMBER OF THE LIQUID BOUNDARY.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION Q
     &( I )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I             |-->| NUMBER OF THE LIQUID BOUNDARY.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_Q => Q
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN) :: I
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER*8 FCT
      INTEGER N
      LOGICAL DEJA,OK(MAXFRO)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
C
C     FIRST CALL, OK INITIALISED TO .TRUE.
C
      IF(.NOT.DEJA) THEN
        DO N=1,MAXFRO
          OK(N)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
C
C     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
C     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
C                      IF  NO, OK IS SET  TO .FALSE.
C
      IF(OK(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
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
          WRITE(LU,*) 'Q NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_FRLIQ(Q,FCT,AT,T2D_FILES(T2DIMP)%LU,ENTET,OK(I))
C
      ENDIF
C
      IF(.NOT.OK(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
C
C     PROGRAMMABLE PART
C     Q IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
C
        IF(NDEBIT.GE.I) THEN
          Q = DEBIT(I)
        ELSE
          IF(LNG.EQ.1) WRITE(LU,400) I
400       FORMAT(1X,/,1X,'Q : DEBITS IMPOSES',/,
     &                1X,'    EN NOMBRE INSUFFISANT',/,
     &                1X,'    DANS LE FICHIER DES PARAMETRES',/,
     &                1X,'    IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,401) I
401       FORMAT(1X,/,1X,'Q : MORE PRESCRIBED FLOWRATES',/,
     &                1X,'    ARE REQUIRED IN THE PARAMETER FILE',/,
     &                1X,'    AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
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