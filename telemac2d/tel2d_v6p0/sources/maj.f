C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE HYDRO SOLUTIONS AT TIME N+1
!>                USING AN EXPLICIT SCHEME.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRS, CE, CF, DT, G, H, KFROT, KNEU, LIMPRO, NBOR, NPTFR, NS, NSEG, QU, QV, SMH, UA, XNEBOR, YNEBOR
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IS, USAIS
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MAJ
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CDLPROJ(), FRICTION()
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
!>          <tr><td>AIRS
!></td><td>--></td><td>AIRES DES CELLULES
!>    </td></tr>
!>          <tr><td>CE
!></td><td>--></td><td>FLUX
!>    </td></tr>
!>          <tr><td>CF
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>G
!></td><td>--></td><td>CONSTANTE DE GRAVITE
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEURS D'EAU AU TEMPS N
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LOI DE FROTTEMENT SUR LE FOND
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>--></td><td>CONVENTION POUR LES POINTS NEUMANN
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE BORD
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
!>          <tr><td>QU,QV
!></td><td>--></td><td>COMPOSANTES DU DEBIT AU TEMPS N
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>--></td><td>TERMES SOURCES DE L'EQUATION DE CONTINUITE
!>    </td></tr>
!>          <tr><td>UA
!></td><td><--</td><td>H, QU, QV  AU TEMPS N+1
!>    </td></tr>
!>          <tr><td>XNEBOR,YNEBOR
!></td><td>--></td><td>NORMALE AUX POINTS FRONTIERE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MAJ
     &(NS,NSEG,NPTFR,G,DT,AIRS,
     & H,QU,QV,UA,CE,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,SMH,KFROT,CF)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRS           |-->| AIRES DES CELLULES
C| CE             |-->| FLUX
C| CF             |-->| COEFFICIENT DE FROTTEMENT
C| DT             |-->| PAS DE TEMPS
C| G             |-->| CONSTANTE DE GRAVITE
C| H             |-->| HAUTEURS D'EAU AU TEMPS N
C| KFROT          |-->| LOI DE FROTTEMENT SUR LE FOND
C| KNEU           |-->| CONVENTION POUR LES POINTS NEUMANN
C| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
C| NSEG           |-->| NOMBRE D'ARETES DU MAILLAGE
C| QU,QV          |-->| COMPOSANTES DU DEBIT AU TEMPS N
C| SMH            |-->| TERMES SOURCES DE L'EQUATION DE CONTINUITE
C| UA             |<--| H, QU, QV  AU TEMPS N+1
C| XNEBOR,YNEBOR  |-->| NORMALE AUX POINTS FRONTIERE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_TELEMAC2D, EX_MAJ => MAJ
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NS,NSEG,NPTFR,KNEU,KFROT
      INTEGER, INTENT(IN) :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: H(NS),QU(NS),QV(NS),SMH(NS)
      DOUBLE PRECISION, INTENT(IN) :: CE(3,NS),CF(NS),AIRS(NS)
      DOUBLE PRECISION, INTENT(IN) :: G,DT
      DOUBLE PRECISION, INTENT(INOUT) :: UA(3,NS)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS
C
      DOUBLE PRECISION USAIS
C
      INTRINSIC ABS
C
C-----------------------------------------------------------------------
C
C EXPLICIT RESOLUTION
C
C     UPDATES THE PHYSICAL SOLUTION
C
      DO IS =1,NS
C
        USAIS = DT/AIRS(IS)
        UA(1,IS)  = H(IS) + USAIS*(CE(1,IS)+SMH(IS))
        UA(2,IS)  = QU(IS) + USAIS*CE(2,IS)
        UA(3,IS)  = QV(IS) + USAIS*CE(3,IS)
C
      ENDDO
C
C     PROJECTS ON THE SLIPPING BOUNDARY CONDITIONS
C
      CALL CDLPROJ(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,UA)
C
      DO IS =1,NS
        IF(UA(1,IS).LE.1.D-12) UA(1,IS)=0.D0
        IF(ABS(UA(2,IS)).LE.1.D-12) UA(2,IS)=0.D0
        IF(ABS(UA(3,IS)).LE.1.D-12) UA(3,IS)=0.D0
      ENDDO
C
      IF(KFROT.NE.0) CALL FRICTION(NS,G,DT,UA,H,QU,QV,CF)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C