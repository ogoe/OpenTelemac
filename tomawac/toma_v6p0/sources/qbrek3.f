C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE CONTRIBUTION OF THE DEPTH-INDUCED
!>                BREAKING SOURCE TERM BASED ON ROELVINK (1993).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   THIS SOURCE TERM IS LINEAR IN F(FREQ,TETA), AND THE LINEAR
!>          COEFFICIENT DOES NOT VARY WITH TIME.

!>  @reference ROELVINK (1993) :
!>                     "DISSIPATION IN RANDOM WAVE GROUPS INCIDENT ON A
!>                      BEACH". COASTAL ENG. VOL 19, PP 127-150.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALFARO, BETA, DEPTH, F, FCAR, GAM2RO, GAMARO, IDISRO, IEXPRO, NF, NPLAN, NPOIN2, TSDER, TSTOT, VARIAN
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, BX, COEF1, COEF2, DEUPI, FN, IFF, IP, JP, PIS2, SEUIL, SIGMA, XM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> GAMMLN(), QGAUSS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SEMIMP()

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
!>      <td><center> 1.1                                       </center>
!> </td><td> 26/03/96
!> </td><td> F. BECQ; M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALFARO
!></td><td>--></td><td>CONSTANTE ALPHA  DU MODELE RO
!>    </td></tr>
!>          <tr><td>BETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETA(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEPTH(
!></td><td>--></td><td>TABLEAU DES PROFONDEURS
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td>--></td><td>SPECTRE DIRECTIONNEL
!>    </td></tr>
!>          <tr><td>FCAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FCAR(
!></td><td>--></td><td>FREQUENCE CARACTERISTIQUE
!>    </td></tr>
!>          <tr><td>GAM2RO
!></td><td>--></td><td>CONSTANTE GAMMA2 DU MODELE RO
!>    </td></tr>
!>          <tr><td>GAMARO
!></td><td>--></td><td>CONSTANTE GAMMA  DU MODELE RO
!>    </td></tr>
!>          <tr><td>IDISRO
!></td><td>--></td><td>CHOIX DE LA DISTRIBUTION DES HAUTEURS
!>    </td></tr>
!>          <tr><td>IEXPRO
!></td><td>--></td><td>EXPOSANT N DU MODELE RO
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL
!>    </td></tr>
!>          <tr><td>TSDER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TSDER(
!></td><td>---</td><td>CONTRIBUTION TERME SOURCE - PARTIE DERIVEE
!>    </td></tr>
!>          <tr><td>TSTOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TSTOT(
!></td><td>---</td><td>CONTRIBUTION TERME SOURCE - PARTIE TOTALE
!>    </td></tr>
!>          <tr><td>VARIAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARIAN(
!></td><td>--></td><td>VARIANCE TOTALE DU SPECTRE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE QBREK3
     &( TSTOT , TSDER , F     , FCAR  , VARIAN, DEPTH , ALFARO, GAMARO,
     &  GAM2RO, IEXPRO, IDISRO, NF    , NPLAN , NPOIN2, BETA  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALFARO         |-->| CONSTANTE ALPHA  DU MODELE RO
C| BETA           |---| 
C| BETA(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| DEPTH          |---| 
C| DEPTH(         |-->| TABLEAU DES PROFONDEURS
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL
C| FCAR           |---| 
C| FCAR(          |-->| FREQUENCE CARACTERISTIQUE
C| GAM2RO         |-->| CONSTANTE GAMMA2 DU MODELE RO
C| GAMARO         |-->| CONSTANTE GAMMA  DU MODELE RO
C| IDISRO         |-->| CHOIX DE LA DISTRIBUTION DES HAUTEURS
C| IEXPRO         |-->| EXPOSANT N DU MODELE RO
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| TSDER          |---| 
C| TSDER(         |---| CONTRIBUTION TERME SOURCE - PARTIE DERIVEE
C| TSTOT          |---| 
C| TSTOT(         |---| CONTRIBUTION TERME SOURCE - PARTIE TOTALE
C| VARIAN         |---| 
C| VARIAN(        |-->| VARIANCE TOTALE DU SPECTRE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER          NF    , NPLAN , NPOIN2, IEXPRO, IDISRO
      DOUBLE PRECISION ALFARO, GAMARO, GAM2RO
      DOUBLE PRECISION DEPTH(NPOIN2), BETA(NPOIN2), FCAR(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF), VARIAN(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  JP    , IFF   , IP
      DOUBLE PRECISION COEF1 , COEF2 , SEUIL , PIS2  , DEUPI
      DOUBLE PRECISION A     , XM    , SIGMA , BX    , FN
C
C.....EXTERNAL FUNCTIONS
C     """"""""""""""""""
      DOUBLE PRECISION   GAMMLN, QGAUSS
      EXTERNAL           GAMMLN, QGAUSS
C
      PARAMETER (PIS2 = 1.570796327D0 , DEUPI = 6.283185307D0)
C
C
      SEUIL  = 1.D-6
      COEF1  = -2.D0*ALFARO
      COEF2  = 8.D0/(GAMARO*GAMARO)
C
      IF (IDISRO.EQ.1) THEN
C
C.......COMPUTES THE LINEAR COEFFICIENT BETA (WEIBULL FIT)
C       """""""""""""""""""""""""""""""""""""""""""""""""""""
        DO 40 IP = 1,NPOIN2
          IF (VARIAN(IP).GT.SEUIL) THEN
            BX    = COEF2*VARIAN(IP)/(DEPTH(IP)*DEPTH(IP))
            SIGMA = DSQRT(8.D0*VARIAN(IP))/DEPTH(IP)
            XM    = 1.D0 + 0.7D0*(DTAN(PIS2*SIGMA/GAM2RO))**2
            A     = DEXP(XM*(GAMMLN(1.D0+1.D0/XM,DEUPI)))
            IF (XM.GT.98.D0) THEN
               FN = 1.D0
            ELSE
               FN = QGAUSS(BX,IEXPRO,A,XM)
            ENDIF
            BETA(IP) = COEF1*FCAR(IP)*FN
          ELSE
            BETA(IP) = 0.D0
          ENDIF
   40   CONTINUE
C
      ELSE
C
C.......COMPUTES THE LINEAR COEFFICIENT BETA (RAYLEIGH FIT)
C       """"""""""""""""""""""""""""""""""""""""""""""""""""""
        DO 50 IP = 1,NPOIN2
          BX = COEF2*VARIAN(IP)/(DEPTH(IP)*DEPTH(IP))
          XM = 1.D0
          A  = 1.D0
          FN = QGAUSS(BX,IEXPRO,A,XM)
          BETA(IP) = COEF1*FCAR(IP)*FN
   50   CONTINUE
      ENDIF
C
C.....TAKES THE SOURCE TERM INTO ACCOUNT
C     """"""""""""""""""""""""""""""""
      DO 10 IFF = 1,NF
        DO 20 JP = 1,NPLAN
          DO 30 IP = 1,NPOIN2
            TSTOT(IP,JP,IFF) = TSTOT(IP,JP,IFF)+BETA(IP)*F(IP,JP,IFF)
C            TSDER(IP,JP,IFF) = TSDER(IP,JP,IFF)+BETA(IP)
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C