C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE DAMPING FUNCTION ACCORDING TO THE
!>                RICHARDSON NUMBER FOR VISCOSITIES OBTAINED USING
!>                A MIXING LENGTH MODEL.
!><br>            HERE MUNK AND ANDERSON MODEL.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference  JOURNAL OF MARINE RESEARCH VOLUME 1  1948.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FRI, FRT, NPOIN3, RI
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, B, EPS, I
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VISCLM()

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
!> </td><td> 25/02/03
!> </td><td> C. VILLARET (LNHE) 0130878328
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FRI
!></td><td><--</td><td>FONCTION D'AMORTISSEMENT
!>    </td></tr>
!>          <tr><td>FRT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>RI
!></td><td>--></td><td>NOMBRE DE RICHARDSON
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DRICV
     & (FRI,FRT,RI,NPOIN3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FRI            |<--| FONCTION D'AMORTISSEMENT
C| FRT            |---| 
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| RI             |-->| NOMBRE DE RICHARDSON
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN3
      DOUBLE PRECISION, INTENT(INOUT) :: FRI(NPOIN3),FRT(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: RI(NPOIN3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
      DOUBLE PRECISION A,B,EPS
!
!-----------------------------------------------------------------------
!
C DAMPING FUNCTION FOR VELOCITIES: (1+A*RI)**B
!
      A=10.D0
      B=-0.5D0
      EPS=1.D-8
!
      DO I=1,NPOIN3
        IF(RI(I).GT.EPS) THEN
          FRI(I)=(1.D0+A*RI(I))**B
        ELSE
          FRI(I)=1.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
C DAMPING FUNCTION FOR TRACERS: (1+A*RI)**B
!
      A=3.33D0
      B=-1.5D0
      EPS=1.D-8
!
      DO I=1,NPOIN3
        IF(RI(I).GT.EPS) THEN
          FRT(I)=(1.D0+A*RI(I))**B
        ELSE
          FRT(I)=1.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C