C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SETS THE CONSTANTS FOR THE K-EPSILON MODEL.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C1, C2, CMU, EMAX, EMIN, ESTAR, KARMAN, KMAX, KMIN, SCHMIT, SIGMAE, SIGMAK
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
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
!>          <tr><td>C1
!></td><td><--</td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>C2
!></td><td><--</td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>CMU
!></td><td><--</td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>EMAX
!></td><td><--</td><td>EPSILON MAXIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>EMIN
!></td><td><--</td><td>EPSILON MINIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>ESTAR
!></td><td><--</td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td><--</td><td>CONSTANTE DE KARMAN
!>    </td></tr>
!>          <tr><td>KMAX
!></td><td><--</td><td>K MAXIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>KMIN
!></td><td><--</td><td>K MINIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>SCHMIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SCHMITT
!></td><td><--</td><td>NOMBRE DE SCHMITT
!>    </td></tr>
!>          <tr><td>SIGMAE
!></td><td><--</td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>SIGMAK
!></td><td><--</td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE COSAKE
     &(KARMAN,CMU,C1,C2,SIGMAK,SIGMAE,ESTAR,SCHMIT,KMIN,KMAX,EMIN,EMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C1             |<--| CONSTANTE DU MODELE K-EPSILON
C| C2             |<--| CONSTANTE DU MODELE K-EPSILON
C| CMU            |<--| CONSTANTE DU MODELE K-EPSILON
C| EMAX           |<--| EPSILON MAXIMUM EN CAS DE CLIPPING
C| EMIN           |<--| EPSILON MINIMUM EN CAS DE CLIPPING
C| ESTAR          |<--| CONSTANTE DU MODELE K-EPSILON
C| KARMAN         |<--| CONSTANTE DE KARMAN
C| KMAX           |<--| K MAXIMUM EN CAS DE CLIPPING
C| KMIN           |<--| K MINIMUM EN CAS DE CLIPPING
C| SCHMIT         |---| 
C| SCHMITT        |<--| NOMBRE DE SCHMITT
C| SIGMAE         |<--| CONSTANTE DU MODELE K-EPSILON
C| SIGMAK         |<--| CONSTANTE DU MODELE K-EPSILON
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(OUT) :: KMIN,KMAX,EMIN,EMAX
      DOUBLE PRECISION, INTENT(OUT) :: KARMAN,CMU,C1,C2
      DOUBLE PRECISION, INTENT(OUT) :: SIGMAK,SIGMAE,ESTAR,SCHMIT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C KARMAN CONSTANT
C
      KARMAN = 0.41D0
      CMU    = 0.09D0
      C1     = 1.44D0
      C2     = 1.92D0
      SIGMAK = 1.00D0
      SIGMAE = 1.30D0
      ESTAR  = 0.15D0
C
C SCHMIDT NUMBER
C
      SCHMIT = 0.50D0
C
C RANGE OF VALUES USED TO CLIP K AND EPSILON
C
      KMIN = 1.D-8
      EMIN = 1.D-8
      KMAX = 1.D10
      EMAX = 1.D10
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C