C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES A TIME STEP (TRACER).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DTT, FLUHBOR, FLUXT, H, HC, HCSTOK, HSTOK, NPTFR, NS, NSEG, NTRAC, SMTR
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IS, ITRAC, NSG
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
!>      <td><center> 5.8                                       </center>
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
!>          <tr><td>DTT
!></td><td><--</td><td>PAS DE TEMPS TRACEUR
!>    </td></tr>
!>          <tr><td>FLUHBOR
!></td><td><--</td><td>FLUX  TRACEUR FRONTIERE REINITIALISE
!>    </td></tr>
!>          <tr><td>FLUXT
!></td><td><--</td><td>FLUX  TRACEUR  REINITIALISE
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEURS D'EAU
!>    </td></tr>
!>          <tr><td>HC
!></td><td>--></td><td>H RECONSTRUIT ORDRE 2   CORRIGE
!>    </td></tr>
!>          <tr><td>HCSTOK
!></td><td><--</td><td>H RECONSTRUIT ORDRE 2   CORRIGE  STOCKE
!>    </td></tr>
!>          <tr><td>HSTOK
!></td><td><--</td><td>HAUTEURS D'EAU  STOCKEES
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NS
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE D'ARETES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SMTR
!></td><td><--</td><td>TERMES SOURCES DU TRACEUR
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE REINIT
     &(NS,NSEG,NPTFR,H,SMTR,HSTOK,HC,HCSTOK,FLUXT,FLUHBOR,DTT,NTRAC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DTT            |<--| PAS DE TEMPS TRACEUR
C| FLUHBOR        |<--| FLUX  TRACEUR FRONTIERE REINITIALISE
C| FLUXT          |<--| FLUX  TRACEUR  REINITIALISE
C| H             |-->| HAUTEURS D'EAU
C| HC             |-->| H RECONSTRUIT ORDRE 2   CORRIGE
C| HCSTOK         |<--| H RECONSTRUIT ORDRE 2   CORRIGE  STOCKE
C| HSTOK          |<--| HAUTEURS D'EAU  STOCKEES
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
C| NSEG           |-->| NOMBRE D'ARETES DU MAILLAGE
C| NTRAC          |---| 
C| SMTR           |<--| TERMES SOURCES DU TRACEUR
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NS,NSEG,NPTFR,NTRAC
      DOUBLE PRECISION, INTENT(INOUT) :: DTT
      DOUBLE PRECISION, INTENT(INOUT) :: HSTOK(*),HCSTOK(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: H(*),HC(2,*)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SMTR,FLUXT,FLUHBOR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS,NSG,ITRAC
C
      DTT = 0.D0
C
      DO IS=1,NS
        HSTOK(IS)=H(IS)
      ENDDO
C
      DO ITRAC=1,NTRAC
        DO IS=1,NS
          SMTR%ADR(ITRAC)%P%R(IS)=0.D0
        ENDDO
      ENDDO
C
      DO NSG=1,NSEG
        HCSTOK(1,NSG) = HC(1,NSG)
        HCSTOK(2,NSG) = HC(2,NSG)
      ENDDO
C
      DO ITRAC=1,NTRAC
        DO NSG=1,NSEG
          FLUXT%ADR(ITRAC)%P%R(NSG)=0.D0
        ENDDO
      ENDDO
C
      DO ITRAC=1,NTRAC
        DO IS=1,NPTFR
          FLUHBOR%ADR(ITRAC)%P%R(IS)=0.D0
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C