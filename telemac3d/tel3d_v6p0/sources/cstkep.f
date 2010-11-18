C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SETS CONSTANTS OF K-EPSILON AND K-OMEGA MODELS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALPHA, BETA, BETAS, C1, C2, CMU, EMAX, EMIN, ITURBV, KARMAN, KMAX, KMIN, OMSTAR, PRANDTL, SCHMIT, SIGMAE, SIGMAK, VIRT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 14/12/09
!> </td><td> J-M HERVOUET(LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 18/03/04
!> </td><td> OLIVER GOETHEL
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 01/02/01
!> </td><td> VINCENT BOYER
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALPHA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETAS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>C1
!></td><td><--</td><td>K-EPSILON CONSTANT
!>    </td></tr>
!>          <tr><td>C2
!></td><td><--</td><td>K-EPSILON CONSTANT
!>    </td></tr>
!>          <tr><td>CMU
!></td><td><--</td><td>K-EPSILON CONSTANT
!>    </td></tr>
!>          <tr><td>EMAX
!></td><td><--</td><td>EPSILON MAXIMUM
!>    </td></tr>
!>          <tr><td>EMIN
!></td><td><--</td><td>EPSILON MINIMUM
!>    </td></tr>
!>          <tr><td>ESTAR
!></td><td><--</td><td>K-EPSILON CONSTANT
!>    </td></tr>
!>          <tr><td>ITURBV
!></td><td>--></td><td>TURBULENCE MODEL (3:K-EPSILON 7:K-OMEGA)
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td><--</td><td>VON KARMAN CONSTANT
!>    </td></tr>
!>          <tr><td>KMAX
!></td><td><--</td><td>K MAXIMUM
!>    </td></tr>
!>          <tr><td>KMIN
!></td><td><--</td><td>K MINIMUM
!>    </td></tr>
!>          <tr><td>OMSTAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PRANDTL
!></td><td><--</td><td>PRANDTL NUMBER
!>    </td></tr>
!>          <tr><td>SCHMIT
!></td><td><--</td><td>SCHMIT NUMBER
!>    </td></tr>
!>          <tr><td>SIGMAE
!></td><td><--</td><td>K-EPSILON CONSTANT
!>    </td></tr>
!>          <tr><td>SIGMAK
!></td><td><--</td><td>K-EPSILON CONSTANT
!>    </td></tr>
!>          <tr><td>VIRT
!></td><td><--</td><td>VIRTUAL ORIGIN FOR EPSILON
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CSTKEP
     & (KARMAN,CMU,C1,C2,SIGMAK,SIGMAE,VIRT,SCHMIT,
     &  KMIN,KMAX,EMIN,EMAX,PRANDTL,ALPHA,BETA,BETAS,OMSTAR,ITURBV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALPHA          |---| 
C| BETA           |---| 
C| BETAS          |---| 
C| C1             |<--| K-EPSILON CONSTANT
C| C2             |<--| K-EPSILON CONSTANT
C| CMU            |<--| K-EPSILON CONSTANT
C| EMAX           |<--| EPSILON MAXIMUM
C| EMIN           |<--| EPSILON MINIMUM
C| ESTAR          |<--| K-EPSILON CONSTANT
C| ITURBV         |-->| TURBULENCE MODEL (3:K-EPSILON 7:K-OMEGA)
C| KARMAN         |<--| VON KARMAN CONSTANT
C| KMAX           |<--| K MAXIMUM
C| KMIN           |<--| K MINIMUM
C| OMSTAR         |---| 
C| PRANDTL        |<--| PRANDTL NUMBER
C| SCHMIT         |<--| SCHMIT NUMBER
C| SIGMAE         |<--| K-EPSILON CONSTANT
C| SIGMAK         |<--| K-EPSILON CONSTANT
C| VIRT           |<--| VIRTUAL ORIGIN FOR EPSILON
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN )   :: ITURBV
      DOUBLE PRECISION, INTENT(INOUT) :: KMIN,KMAX,EMIN,EMAX
      DOUBLE PRECISION, INTENT(INOUT) :: KARMAN,CMU,C1,C2,SIGMAK,SIGMAE
      DOUBLE PRECISION, INTENT(INOUT) :: VIRT,PRANDTL,SCHMIT
      DOUBLE PRECISION, INTENT(INOUT) :: ALPHA,BETA,BETAS,OMSTAR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     VON KARMAN CONSTANT
!
      KARMAN = 0.41D0
!
C     SCHMIDT NUMBER
!
      SCHMIT = 1.D0
!
C     PRANDTL NUMBER (BETWEEN 0.8 AND 0.9 FOR TEMPERATURE)
!
      PRANDTL = 1.D0
!
C     OTHER CONSTANTS
!
      CMU    = 0.09D0
      C1     = 1.44D0
      C2     = 1.92D0
!
      IF(ITURBV.EQ.3) THEN
        SIGMAK = 1.D0
        SIGMAE = 1.3D0
      ELSEIF(ITURBV.EQ.7) THEN
        SIGMAK = 2.D0
        SIGMAE = 2.D0
      ENDIF
!
C     K-OMEGA MODEL
!
      ALPHA  = 5.D0/9.D0
      BETA   = 3.D0/40.D0
      BETAS  = 0.09D0
C     TO COMPUTE THE FREE SURFACE VALUE OF OMEGA
      OMSTAR  = 100.D0
C     VIRTUAL ORIGIN FOR EPSILON
      VIRT = 0.07D0
!
C     MINIMA AND MAXIMA FOR CLIPPING
!
      IF(ITURBV.EQ.3) THEN
        KMIN = 1.D-10
        EMIN = 1.D-10
        KMAX = 1.D4
        EMAX = 1.D10
      ELSEIF(ITURBV.EQ.7) THEN
        KMIN = 1.D-8
        EMIN = 1.D-3
        KMAX = 1.D-1
        EMAX = 1.D4
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C