C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES K AND EPSILON.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note     FRICTION LAWS :
!>
!>  @note     KFROT = 0:  NO FRICTION   (NOT CODED)
!>  @note     KFROT = 1:  LINEAR LAW      (NOT CODED)
!>  @note     KFROT = 2:  LAW OF CHEZY
!>  @note     KFROT = 3:  LAW OF STRICKLER
!>  @note     KFROT = 4:  LAW OF MANNING
!>  @note     KFROT = 5:  LAW OF NIKURADSE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AK, C2, CF, CMU, EMIN, EP, ESTAR, H, KFROT, KMIN, NPOIN, SCHMIT, U, V
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CEPS, HAUT, K, TIERS, USTAR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!> </td><td> 30/05/1994
!> </td><td> L. VAN HAREN (LNH) 30 87 84 14
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 27/11/1992
!> </td><td> J-M HERVOUET (LNHE) 30 87 80 18
!> </td><td>
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
!>          <tr><td>C1
!></td><td>---</td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>C2
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CHESTR
!></td><td>--></td><td>TABLEAU DES COEFFICIENTS DE FROTTEMENT SUR LE
!>                  FOND.
!>    </td></tr>
!>          <tr><td>CMU
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>EMAX
!></td><td>--></td><td>EPSILON MAXIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>EMIN
!></td><td>--></td><td>EPSILON MINIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>EP
!></td><td><--</td><td>DISSIPATION TURBULENTE
!>    </td></tr>
!>          <tr><td>ESTAR
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>FFON
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT.
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR.
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>CONSTANTE DE KARMAN
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>CORRESPOND AU MOT CLE: "LOI DE FROTTEMENT SUR
!>                  LE FOND"  (1:CHEZY 2:LINEAIRE 3:STRICKLER).
!>    </td></tr>
!>          <tr><td>KMAX
!></td><td>---</td><td>K MAXIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>KMIN
!></td><td>--></td><td>K MINIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>SCHMIT
!></td><td>--></td><td>NOMBRE DE SCHMITT
!>    </td></tr>
!>          <tr><td>SIGMAE
!></td><td>---</td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>SIGMAK
!></td><td>---</td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE AKEPIN
     &(AK,EP,U,V,H,NPOIN,KFROT,CMU,C2,ESTAR,SCHMIT,KMIN,EMIN,CF)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AK             |<--| ENERGIE TURBULENTE
C| C1             |---| CONSTANTE DU MODELE K-EPSILON
C| C2             |-->| CONSTANTE DU MODELE K-EPSILON
C| CF             |---| 
C| CHESTR         |-->| TABLEAU DES COEFFICIENTS DE FROTTEMENT SUR LE
C|                |   | FOND.
C| CMU            |-->| CONSTANTE DU MODELE K-EPSILON
C| EMAX           |-->| EPSILON MAXIMUM EN CAS DE CLIPPING
C| EMIN           |-->| EPSILON MINIMUM EN CAS DE CLIPPING
C| EP             |<--| DISSIPATION TURBULENTE
C| ESTAR          |-->| CONSTANTE DU MODELE K-EPSILON
C| FFON           |-->| COEFFICIENT DE FROTTEMENT.
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR.
C| H             |-->| HAUTEUR D'EAU
C| KARMAN         |---| CONSTANTE DE KARMAN
C| KFROT          |-->| CORRESPOND AU MOT CLE: "LOI DE FROTTEMENT SUR
C|                |   | LE FOND"  (1:CHEZY 2:LINEAIRE 3:STRICKLER).
C| KMAX           |---| K MAXIMUM EN CAS DE CLIPPING
C| KMIN           |-->| K MINIMUM EN CAS DE CLIPPING
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| SCHMIT         |-->| NOMBRE DE SCHMITT
C| SIGMAE         |---| CONSTANTE DU MODELE K-EPSILON
C| SIGMAK         |---| CONSTANTE DU MODELE K-EPSILON
C| U,V            |-->| COMPOSANTES DE LA VITESSE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,KFROT
      DOUBLE PRECISION, INTENT(INOUT) :: AK(NPOIN),EP(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: KMIN,EMIN,CMU,C2,ESTAR,SCHMIT
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN),V(NPOIN),H(NPOIN),CF(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K
C
      DOUBLE PRECISION TIERS,HAUT,USTAR,CEPS
C
      INTRINSIC SQRT,MAX
C
C-----------------------------------------------------------------------
C
      TIERS = 1.D0/3.D0
C
C  INITIALISATION OF K AND EPSILON
C
C     *******************
      IF(KFROT.EQ.0) THEN
C     *******************
C
        IF(LNG.EQ.1) WRITE(LU,100)
        IF(LNG.EQ.2) WRITE(LU,101)
100     FORMAT(1X,'AKEPIN N''EST PAS PREVU SANS FROTTEMENT SUR LE FOND')
101     FORMAT(1X,'AKEPIN IS NOT PROVIDED WITHOUT BOTTOM FRICTION')
        CALL PLANTE(1)
        STOP
C
C     ****
      ELSE
C     ****
C
        DO 20 K=1,NPOIN
           HAUT  = MAX(H(K),1.D-4)
           USTAR = SQRT( 0.5D0 * CF(K) * ( U(K)**2 + V(K)**2 ) )
           CEPS  = C2*SQRT(CMU)/SQRT(ESTAR*SCHMIT)/(0.5D0*CF(K))**0.75D0
           AK(K) = C2*USTAR**2/(0.5D0*CF(K)*CEPS)
           EP(K) = MAX( USTAR**3/(HAUT*SQRT(0.5D0*CF(K))) , EMIN )
20      CONTINUE
C
C     *****
      ENDIF
C     *****
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C