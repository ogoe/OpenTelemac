C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PRESCRIBES THE TRACER VALUES FOR TRACER IMPOSED
!>                LIQUID BOUNDARIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC2D, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I, IERR, ITRAC, N
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::AT AT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ENTET ENTET@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MAXFRO MAXFRO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NTRAC NTRAC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NTRACE NTRACE@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2DIMP T2DIMP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2D_FILES T2D_FILES@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TRACER TRACER@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DEJA, FCT, IRANK, J, OK
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_TR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), READ_FIC_FRLIQ()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BORD()

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
!> </td><td> 02/04/2009
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
!></td><td>--></td><td>RANG DE LA FRONTIERE A DEBIT IMPOSE
!>                  (1 S'IL N'Y EN A QU'UNE)
!>    </td></tr>
!>          <tr><td>IERR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>NUMERO GLOBAL DU POINT
!>                  (LU DANS LE FICHIER DES PARAMETRES)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION TR
     &( I , ITRAC , N , IERR )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I             |-->| RANG DE LA FRONTIERE A DEBIT IMPOSE
C|                |   | (1 S'IL N'Y EN A QU'UNE)
C| IERR           |---| 
C| ITRAC          |---| 
C| N             |-->| NUMERO GLOBAL DU POINT
C|                |   | (LU DANS LE FICHIER DES PARAMETRES)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_TR => TR
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: I,N,ITRAC
      INTEGER, INTENT(INOUT) :: IERR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER*8 FCT
      INTEGER J,IRANK
      LOGICAL DEJA,OK(MAXFRO*MAXTRA)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
C
C     A PRIORI ASSUMES THAT TR WILL BE FOUND
C
      IERR=0
C
C     FIRST CALL, OK INITIALISED TO .TRUE.
C
      IF(.NOT.DEJA) THEN
        DO J=1,MAXFRO
          OK(J)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
C
C     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
C     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
C                      IF  NO, OK SET     TO .FALSE.
C     RANK OF VALUE IN ARRAY TRACER OR IN LIQUID BOUNDARY FILE
C
      IRANK=ITRAC+(I-1)*NTRAC
      IF(OK(IRANK).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
C
C       FCT WILL BE TR(1), TR(2), ETC, TR(99), DEPENDING ON I
        FCT(1:3)='TR('
        IF(IRANK.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') IRANK
          FCT(5:8)=')   '
        ELSEIF(IRANK.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') IRANK
          FCT(6:8)=')  '
        ELSE
          WRITE(LU,*) 'TR NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_FRLIQ(TR,FCT,AT,T2D_FILES(T2DIMP)%LU,
     &                      ENTET,OK(IRANK))
C
      ENDIF
C
C     IF VALUE NOT FOUND IN THE LIQUID BOUNDARY FILE
C     OR IF THERE IS NO LIQUID BOUNDARY FILE
C     ATTEMPTS TO FIND IT IN THE STEERING FILE
C
      IF(.NOT.OK(IRANK).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
C
        IF(NTRACE.GE.IRANK) THEN
          TR = TRACER(IRANK)
          OK(IRANK)=.TRUE.
        ELSEIF(NTRACE.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,300) IRANK
300       FORMAT(1X,/,1X,'TR : VALEURS IMPOSEES DU TRACEUR'
     &             ,/,1X,'     EN NOMBRE INSUFFISANT'
     &             ,/,1X,'     DANS LE FICHIER DES PARAMETRES'
     &             ,/,1X,'     IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,301) IRANK
301       FORMAT(1X,/,1X,'TR : MORE PRESCRIBED TRACER VALUES'
     &             ,/,1X,'     ARE REQUIRED IN THE PARAMETER FILE'
     &             ,/,1X,'     AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
      ENDIF
C
C     NOTHING FOUND: VALUES WILL BE TAKEN FROM BOUNDARY CONDITION FILE
C
      IF(.NOT.OK(IRANK)) THEN
        TR=0.D0
        IERR=1
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C