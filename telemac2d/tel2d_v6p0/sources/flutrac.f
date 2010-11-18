C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INCREMENTS TRACER FLUXES BY ONE HYDRO TIMESTEP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, DTT, FLUHBOR, FLUHBTEMP, FLUXT, FLUXTEMP, NPTFR, NSEG
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IS, NSG
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
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS HYDRO
!>    </td></tr>
!>          <tr><td>DTT
!></td><td><-></td><td>PAS DE TEMPS TRACEUR
!>    </td></tr>
!>          <tr><td>FLUHBOR
!></td><td><-></td><td>FLUX  TRACEUR FRONTIERE INCREMENTE
!>    </td></tr>
!>          <tr><td>FLUHBTEMP
!></td><td>--></td><td>FLUX FRONTIERE D'UN PAS DE TEMPS HYDRO
!>    </td></tr>
!>          <tr><td>FLUXT
!></td><td><-></td><td>FLUX  TRACEUR INCREMENTE
!>    </td></tr>
!>          <tr><td>FLUXTEMP
!></td><td>--></td><td>FLUX D'UN PAS DE TEMPS HYDRO
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE D'ARETES DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUTRAC
     &(NSEG,NPTFR,DT,FLUXT,FLUHBOR,FLUXTEMP,FLUHBTEMP,DTT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS HYDRO
C| DTT            |<->| PAS DE TEMPS TRACEUR
C| FLUHBOR        |<->| FLUX  TRACEUR FRONTIERE INCREMENTE
C| FLUHBTEMP      |-->| FLUX FRONTIERE D'UN PAS DE TEMPS HYDRO
C| FLUXT          |<->| FLUX  TRACEUR INCREMENTE
C| FLUXTEMP       |-->| FLUX D'UN PAS DE TEMPS HYDRO
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NSEG           |-->| NOMBRE D'ARETES DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NSEG, NPTFR
      DOUBLE PRECISION, INTENT(IN)    :: DT,FLUXTEMP(*),FLUHBTEMP(*)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUHBOR(*),FLUXT(*),DTT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS,NSG
C
C-----------------------------------------------------------------------
C
      DTT=DTT+DT
C
      DO NSG=1,NSEG
        FLUXT(NSG) = FLUXT(NSG) + DT * FLUXTEMP(NSG)
      ENDDO
C
      DO IS=1,NPTFR
        FLUHBOR(IS) = FLUHBOR(IS) + DT * FLUHBTEMP(IS)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C