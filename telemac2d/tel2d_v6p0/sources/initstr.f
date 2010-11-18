C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ASSIGNS INITIAL VALUES OF STRICKLERS PER ZONE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHESTR, NPOIN, NZONE, PZONE, SETSTR, T1
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, J
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ADJ_T2D(), TELEMAC2D()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 22/10/2001
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHESTR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS
!>    </td></tr>
!>          <tr><td>NZONE
!></td><td>--></td><td>NUMBER OF ZONES
!>    </td></tr>
!>          <tr><td>PZONE
!></td><td>--></td><td>TABLE OF ZONES
!>    </td></tr>
!>          <tr><td>SETSTR
!></td><td>--></td><td>SET OF STRICKLERS' (ZONES)
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INITSTR
     &(CHESTR,SETSTR,PZONE,NZONE,NPOIN,T1)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHESTR         |---| 
C| NPOIN          |-->| NUMBER OF POINTS
C| NZONE          |-->| NUMBER OF ZONES
C| PZONE          |-->| TABLE OF ZONES
C| SETSTR         |-->| SET OF STRICKLERS' (ZONES)
C| T1             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(IN)      :: CHESTR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SETSTR,T1
      INTEGER, INTENT(IN)             :: PZONE(*)
      INTEGER, INTENT(IN)             :: NZONE
      INTEGER, INTENT(IN)             :: NPOIN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,J
C
C----------------------------------------------------------------------
C
      IF(NZONE.GT.0) THEN
C
C       ZONATION : SETSTR=AVERAGE PER ZONE OF CHESTR
C
        CALL OS('X=C     ',X=SETSTR,C=0.D0)
        CALL OS('X=Y     ',X=T1    ,Y=SETSTR)
        DO J=1,NZONE
          DO I=1,NPOIN
            IF(PZONE(I).EQ.J) THEN
              SETSTR%R(J)=SETSTR%R(J)+CHESTR%R(I)
              T1%R(J)=T1%R(J)+1.D0
            ENDIF
          ENDDO
          SETSTR%R(J)=SETSTR%R(J)/T1%R(J)
        ENDDO
C
      ELSE
C
C       NO ZONATION : SETSTR=CHESTR
C
        CALL OS('X=Y     ',X=SETSTR,Y=CHESTR)
C
      ENDIF
C
C----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C