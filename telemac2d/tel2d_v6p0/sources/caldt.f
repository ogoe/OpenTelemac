C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE TIMESTEP SATISFYING THE CFL CONDITION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CFL, DT, DTHAUT, G, H, NS, U, V
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPSL, IS, RA3, SIGMAX, UA2, UA3, UNORM
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>RESOLU()

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
!> </td><td> ??????
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CFL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DTHAUT
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
!>          <tr><td>U
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CALDT
     &(NS,G,H,U,V,DTHAUT,DT,CFL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CFL            |---| 
C| DT             |---| 
C| DTHAUT         |---| 
C| G             |---| 
C| H             |---| 
C| NS             |---| 
C| U             |---| 
C| V             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NS
      DOUBLE PRECISION, INTENT(INOUT) :: DT
      DOUBLE PRECISION, INTENT(IN) :: H(NS),U(NS),V(NS),DTHAUT(NS)
      DOUBLE PRECISION, INTENT(IN) :: G,CFL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS
C
      DOUBLE PRECISION RA3,EPSL
      DOUBLE PRECISION SIGMAX ,UA2, UA3, UNORM
C
C-----------------------------------------------------------------------
C
      RA3 = SQRT(1.5D0*G)
C
      DT = 1.E+12
      EPSL = 0.01D0
C
      DO IS=1,NS
        SIGMAX = H(IS)
        UA2    = U(IS)
        UA3    = V(IS)
        UNORM=SQRT(UA2*UA2 + UA3*UA3)
        SIGMAX= MAX(EPSL, RA3*SQRT(SIGMAX) +UNORM )
        DT = MIN(DT, CFL*DTHAUT(IS)/SIGMAX)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C