C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PRESCRIBES THE FREE SURFACE ELEVATION AS A FUNCTION OF
!>                THE DISCHARGE BY INTERPOLATING FROM A STAGE-DISCHARGE
!>                CURVE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FLUX, IFRLIQ, NFRLIQ, PTS, QZ, ZN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> GOAL, I, Q1, Q2, TETA
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BORD(), BORD3D()

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
!> </td><td> 17/08/1994
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FLUX
!></td><td>--></td><td>ACTUAL FLUX AT THIS BOUNDARY
!>    </td></tr>
!>          <tr><td>IFRLIQ
!></td><td>--></td><td>LIQUID BOUNDARY NUMBER
!>    </td></tr>
!>          <tr><td>NFRLIQ
!></td><td>--></td><td>NUMBER OF LIQUID BOUNDARIES
!>    </td></tr>
!>          <tr><td>PTS
!></td><td>--></td><td>NUMBER OF POINTS IN THE STAGE-DISCHARGE CURVE
!>    </td></tr>
!>          <tr><td>QZ
!></td><td>--></td><td>ARRAY WITH STAGE-DISCHARGE CURVES
!>    </td></tr>
!>          <tr><td>ZN
!></td><td>--></td><td>PREVIOUS ELEVATION (FOR RELAXATION)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                     DOUBLE PRECISION FUNCTION STA_DIS_CUR
     &(IFRLIQ,FLUX,PTS,QZ,NFRLIQ,ZN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FLUX           |-->| ACTUAL FLUX AT THIS BOUNDARY
C| IFRLIQ         |-->| LIQUID BOUNDARY NUMBER
C| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
C| PTS            |-->| NUMBER OF POINTS IN THE STAGE-DISCHARGE CURVE
C| QZ             |-->| ARRAY WITH STAGE-DISCHARGE CURVES
C| ZN             |-->| PREVIOUS ELEVATION (FOR RELAXATION)
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
      INTEGER, INTENT(IN) :: IFRLIQ,NFRLIQ,PTS
      DOUBLE PRECISION, INTENT(IN) :: ZN,FLUX,QZ(2,NFRLIQ,PTS)
C                                                         PTS AT LEAST
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
      DOUBLE PRECISION GOAL,TETA,Q1,Q2
C
C-----------------------------------------------------------------------
C
      Q1=QZ(1,IFRLIQ,1)
      Q2=QZ(1,IFRLIQ,PTS)
      IF(FLUX.LE.Q1) THEN
C       OUTSIDE THE CURVE WITH LOWER DISCHARGE
        GOAL=QZ(2,IFRLIQ,1)
      ELSEIF(FLUX.GE.Q2) THEN
C       OUTSIDE THE CURVE WITH HIGHER DISCHARGE
        GOAL=QZ(2,IFRLIQ,PTS)
      ELSE
C       IN BETWEEN: CASE WITH INTERPOLATION
        I=1
1       CONTINUE
        Q2=QZ(1,IFRLIQ,I+1)
        IF(FLUX.GE.Q1.AND.FLUX.LE.Q2) THEN
          TETA=(FLUX-Q1)/MAX(Q2-Q1,1.D-8)
          GOAL=QZ(2,IFRLIQ,I)+TETA*(QZ(2,IFRLIQ,I+1)-QZ(2,IFRLIQ,I))
        ELSE
          I=I+1
          Q1=Q2
          GO TO 1
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C     RELAXATION OF RESULT
C
      STA_DIS_CUR=ZN+0.02D0*(GOAL-ZN)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C