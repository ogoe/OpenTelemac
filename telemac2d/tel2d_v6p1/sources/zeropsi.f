C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ZERO OF PSI-A2 BY NEWTON'S METHOD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A2, CA1, NIT, X, X0
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AMMOINS, AMPLUS, CAPHI, DPHIPH3, EPS, EPSX, FDF, NITEPS, PHI, PHI1, SQ32, SQ3I
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CDL()

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
!>      <td><center> 5.4                                       </center>
!> </td><td>
!> </td><td> INRIA
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CA1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X0
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ZEROPSI
     &(X0,X,NIT,CA1,A2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A2             |---| 
C| CA1            |---| 
C| NIT            |---| 
C| X             |---| 
C| X0             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(INOUT)          :: NIT
      DOUBLE PRECISION, INTENT(IN)    :: X0,A2,CA1
      DOUBLE PRECISION, INTENT(INOUT) :: X
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NITEPS
C
      DOUBLE PRECISION EPS,EPSX,SQ32,SQ3I
      DOUBLE PRECISION AMPLUS,AMMOINS,PHI1,PHI,DPHIPH3,CAPHI,FDF
C
C-----------------------------------------------------------------------
C
      SQ32=SQRT(1.5D0)
      SQ3I=1.D0/SQRT(3.D0)
      EPS=1.E-12
      EPSX=1.E-12
      NIT=0
      NITEPS=0
      X=X0
C
1     NIT=NIT+1
C
      IF(X.LE.-SQ32) THEN
      X= -SQ32 + EPSX
      NITEPS= NITEPS +1
      ENDIF
      IF(NITEPS.EQ.3) THEN
      X= -SQ32
      GOTO 10
      ENDIF
C
      AMPLUS=MAX(-X,+SQ32)
      AMMOINS=MAX(-X,-SQ32)
C
      PHI1=0.5D0*(AMPLUS+AMMOINS+2.*X)
      PHI = PHI1*SQ3I*(AMPLUS-AMMOINS)
      DPHIPH3 = 1.D0/(3.D0*PHI1)
C
      CAPHI = CA1* PHI**(1.D0/3.D0)
      FDF= (X - 2.D0 - A2 *CAPHI)/(1.D0- DPHIPH3*(X-2.D0))
C
      IF(ABS(FDF).LT.EPS) GOTO 10
      IF(NIT.EQ.20) GOTO 10
C
      X=X-FDF
      GOTO 1
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C