C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES K AND EPSILON.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AK, CMU, DNUVIH, DNUVIV, EMIN, EP, KARMAN, KMIN, NPLAN, NPOIN2, U, V, Z, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, DIST, FICTIFEPS, IPLAN, IPOIN2, IT, N2
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
!>      <td><center> 5.4                                       </center>
!> </td><td>
!> </td><td> V. BOYER UMIST
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AK
!></td><td><--</td><td>ENERGIE TURBULENTE
!>    </td></tr>
!>          <tr><td>CMU
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>DNUVIH
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION HORIZONTALE
!>    </td></tr>
!>          <tr><td>DNUVIV
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION VERTICALE
!>    </td></tr>
!>          <tr><td>EMIN
!></td><td>--></td><td>EPSILON MINIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>EP
!></td><td><--</td><td>DISSIPATION TURBULENTE
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>CONSTANTE DE KARMAN
!>    </td></tr>
!>          <tr><td>KMIN
!></td><td>--></td><td>K MINIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS  DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COTES DES POINTS DU MAILLAGE 3D REEL
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE KEPINI
     &(AK,EP,U,V,Z,ZF,NPOIN2,NPLAN,DNUVIH,DNUVIV,KARMAN,CMU,KMIN,EMIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AK             |<--| ENERGIE TURBULENTE
C| CMU            |-->| CONSTANTE DU MODELE K-EPSILON
C| DNUVIH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE
C| DNUVIV         |-->| COEFFICIENT DE DIFFUSION VERTICALE
C| EMIN           |-->| EPSILON MINIMUM EN CAS DE CLIPPING
C| EP             |<--| DISSIPATION TURBULENTE
C| H             |-->| HAUTEUR D'EAU
C| KARMAN         |-->| CONSTANTE DE KARMAN
C| KMIN           |-->| K MINIMUM EN CAS DE CLIPPING
C| NPLAN          |-->| NOMBRE DE PLANS  DU MAILLAGE 3D
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| U,V            |-->| COMPOSANTES DE LA VITESSE
C| Z             |-->| COTES DES POINTS DU MAILLAGE 3D REEL
C| ZF             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG, LU
      COMMON/INFO/ LNG, LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: NPOIN2,NPLAN
      DOUBLE PRECISION, INTENT(INOUT):: AK(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT):: EP(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: U(NPOIN2,NPLAN), V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: KARMAN, DNUVIH, DNUVIV
      DOUBLE PRECISION, INTENT(IN)   :: CMU
      DOUBLE PRECISION, INTENT(IN)   :: KMIN, EMIN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPOIN2,IPLAN,N2,IT
      DOUBLE PRECISION C,DIST
!
      INTRINSIC LOG, SQRT, MAX
!
      DOUBLE PRECISION, PARAMETER :: FICTIFEPS = 2.D0
!
!-----------------------------------------------------------------------
!
C     A THEORY BY VINCENT BOYER MODIFIED BY MARTIN FERRAND
!
C     DO IPOIN2 = 1,NPOIN2
C       DO IPLAN = 1,NPLAN
!
C         ARBITRARY COMPUTATION OF K EXPRESSED AS A PERCENTAGE OF SPEED
!
C         AK(IPOIN2,IPLAN) = 1.D-3*U(IPOIN2,IPLAN)**2
C         AK(IPOIN2,IPLAN) = MAX(AK(IPOIN2,IPLAN),KMIN)
!
C         COMPUTATION OF EPSILON
!
C         EP INITIALISED ACCORDING TO UETOIL**3/KAPPA/Y
C         WHERE UETOIL IS CALCULATED FROM THE VALUE OF K AT THE WALL
!
C         IF(IPLAN.EQ.1) THEN
C           DIST = (Z(IPOIN2,2)-ZF(IPOIN2))/FICTIFEPS
C         ELSE
C           DIST = Z(IPOIN2,IPLAN)-ZF(IPOIN2)
C         ENDIF
C         EP(IPOIN2,IPLAN)=CMU**0.75*SQRT(AK(IPOIN2,1)**3)/KARMAN/DIST
C         EP(IPOIN2,IPLAN)=MAX(EP(IPOIN2,IPLAN),EMIN)
C       ENDDO
C     ENDDO
!
!-----------------------------------------------------------------------
!
C     HERE: NO INITIAL TURBULENCE
!
      DO IPOIN2 = 1,NPOIN2
        DO IPLAN = 1,NPLAN
          AK(IPOIN2,IPLAN) = KMIN
          EP(IPOIN2,IPLAN) = EMIN
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C