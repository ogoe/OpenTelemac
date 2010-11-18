C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FRICTION TERM.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, DT, G, H, NS, QU, QV, UA
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AKAP, AKAP1, IS, STRIC2
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MAJ()

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
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>G
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UA
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FRICTION
     &(NS,G,DT,UA,H,QU,QV,CF)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |---| 
C| DT             |---| 
C| G             |---| 
C| H             |---| 
C| NS             |---| 
C| QU             |---| 
C| QV             |---| 
C| UA             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NS
      DOUBLE PRECISION, INTENT(IN)    :: G,DT
      DOUBLE PRECISION, INTENT(IN)    :: CF(NS)
      DOUBLE PRECISION, INTENT(IN)    :: H(NS),QU(NS),QV(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: UA(3,NS)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS
      DOUBLE PRECISION AKAP,AKAP1,STRIC2
C
C-----------------------------------------------------------------------
C
        DO IS =1,NS
C
          STRIC2=CF(IS)**2
C
C FH-FRDATA
C          IF(H(IS).LE.1.D-12.OR.UA(1,IS).LE.1.D-12)  THEN
          IF((H(IS)   .LE.1.D-12).OR.
     &       (UA(1,IS).LE.1.D-12).OR.
     &       (CF(IS)  .LE.1.D-12)    ) THEN
C FH-FRDATA
            AKAP=0.D0
          ELSE
            AKAP= G*DT*SQRT(QU(IS)**2+QV(IS)**2)/
     &           (STRIC2*H(IS)*UA(1,IS)**(4.D0/3.D0))
          ENDIF
C
          AKAP1=1.D0/(1.D0+AKAP)
          UA(2,IS) = AKAP1*UA(2,IS)
          UA(3,IS) = AKAP1*UA(3,IS)
C
        ENDDO
C
C-----------------------------------------------------------------------
C
       RETURN
       END
C
C#######################################################################
C