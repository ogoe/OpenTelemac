C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PRESCRIBES THE TRACER AT THE SOURCES
!>               (CAN BE A FUNCTION OF TIME).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I, ITRAC, TIME
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::AT AT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::INFOGR INFOGR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NSCE NSCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NTRAC NTRAC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DVEF T3DVEF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3D_FILES T3D_FILES@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TASCE TASCE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DEJA, FCT, IRANK, N, OK
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), READ_FIC_SOURCES()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!> </td><td> 04/04/08
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
!></td><td>--></td><td>SOURCE RANK
!>    </td></tr>
!>          <tr><td>ITRAC
!></td><td>--></td><td>TRACER RANK
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>--></td><td>TIME
!>    </td></tr>
!>          <tr><td>TSCE
!></td><td>--></td><td>ARRAY OF PRESCRIBED VALUES OF THE TRACER
!>                  (READ IN THE PARAMETER FILE)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      DOUBLE PRECISION FUNCTION T3D_TRSCE
     &( TIME , I , ITRAC )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I             |-->| SOURCE RANK
C| ITRAC          |-->| TRACER RANK
C| TIME           |-->| TIME
C| TSCE           |-->| ARRAY OF PRESCRIBED VALUES OF THE TRACER
C|                |   | (READ IN THE PARAMETER FILE)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY: AT,INFOGR,NTRAC,TASCE,NSCE,
     &                                  T3D_FILES,T3DVEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER         , INTENT(IN) :: I,ITRAC
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER*8 FCT
      INTEGER N,IRANK
      LOGICAL DEJA,OK(99)  ! 99.GE.NSCE*NTRAC
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
C
C     FIRST CALL, INITIALISES OK TO .TRUE.
C
      IF(.NOT.DEJA) THEN
        IF(NSCE*NTRAC.GT.99) THEN
          WRITE(LU,*) 'CHANGE DIMENSION OF OK IN TRSCE, ',NSCE*NTRAC,
     &                ' REQUIRED'
          CALL PLANTE(1)
          STOP
        ENDIF
        DO N=1,NSCE*NTRAC
          OK(N)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
C
C     IF SOURCE FILE EXISTS, ATTEMPTS TO FIND
C     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
C                      IF  NO, OK IS SET  TO .FALSE.
C
C     IRANK CORRESPONDS TO TELEMAC-3D DOCUMENTATION FOR
C     VALUES OF TRACERS AT SOURCES
      IRANK=ITRAC+NTRAC*(I-1)
      IF(OK(IRANK).AND.T3D_FILES(T3DVEF)%NAME(1:1).NE.' ') THEN
C
C       FCT WILL BE T(1), T(2), ETC, T(99), DEPENDING ON I AND ITRAC
        FCT='TR(     '
        IF(IRANK.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') IRANK
          FCT(5:5)=')'
        ELSEIF(IRANK.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') IRANK
          FCT(6:6)=')'
        ELSE
          WRITE(LU,*) 'TRSCE NOT PROGRAMMED FOR MORE THAN 99 DATA'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_SOURCES(T3D_TRSCE,FCT,AT,T3D_FILES(T3DVEF)%LU,
     &                        INFOGR,OK(IRANK))
C
      ENDIF
C
C     BEWARE: AN ERROR IN THE SOURCES FILE MAY REMAIN UNNOTICED
C     BECAUSE RESORTS HERE TO THE STEERING FILE
C
      IF(.NOT.OK(I).OR.T3D_FILES(T3DVEF)%NAME(1:1).EQ.' ') THEN
C
C       PROGRAMMABLE PART
C       TASCE IS TAKEN FROM THE STEERING FILE
C
C       GLOBAL NUMBER OF SOURCE I IS ISCE(I) IN TELEMAC-3D
        T3D_TRSCE = TASCE(I,ITRAC)
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