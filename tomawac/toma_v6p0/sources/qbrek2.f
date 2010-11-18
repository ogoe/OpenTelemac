C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE CONTRIBUTION OF THE DEPTH-INDUCED
!>                BREAKING SOURCE TERM BASED ON THORNTON AND GUZA (1983).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   THIS SOURCE TERM IS LINEAR IN F(FREQ,TETA), AND THE LINEAR
!>          COEFFICIENT DOES NOT VARY WITH TIME.

!>  @reference THORNTON AND GUZA (1983) :
!>                     "TRANSFORMATION OF WAVE HEIGHT DISTRIBUTION".

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BETA, BORETG, DEPTH, F, FCAR, GAMATG, IWHTG, NF, NPLAN, NPOIN2, TSDER, TSTOT, VARIAN
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COEF, DEUPI, GAMMA2, IFF, IP, JP, SEUIL
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
!> </td><td> 14/02/96
!> </td><td> F. BECQ; M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETA(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>BORETG
!></td><td>--></td><td>MODELE DEFERLEMENT TG : CONSTANTE B
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
!></td><td>--></td><td>FREQUENCE CARACTERISTIQUE DU SPECTRE
!>    </td></tr>
!>          <tr><td>GAMATG
!></td><td>--></td><td>MODELE DEFERLEMENT TG : CONSTANTE GAMMA
!>    </td></tr>
!>          <tr><td>IWHTG
!></td><td>--></td><td>MODELE DEFERLEMENT TG : MODE CALCUL DE W(H)
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
                        SUBROUTINE QBREK2
     &( TSTOT , TSDER , F     , FCAR  , VARIAN, DEPTH , BORETG, GAMATG,
     &  IWHTG , NF    , NPLAN , NPOIN2, BETA  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BETA           |---| 
C| BETA(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| BORETG         |-->| MODELE DEFERLEMENT TG : CONSTANTE B
C| DEPTH          |---| 
C| DEPTH(         |-->| TABLEAU DES PROFONDEURS
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL
C| FCAR           |---| 
C| FCAR(          |-->| FREQUENCE CARACTERISTIQUE DU SPECTRE
C| GAMATG         |-->| MODELE DEFERLEMENT TG : CONSTANTE GAMMA
C| IWHTG          |-->| MODELE DEFERLEMENT TG : MODE CALCUL DE W(H)
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
      INTEGER  NF    , NPLAN , NPOIN2, IWHTG
      DOUBLE PRECISION BORETG, GAMATG
      DOUBLE PRECISION DEPTH(NPOIN2), BETA(NPOIN2), FCAR(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF), VARIAN(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  JP    , IFF   , IP
      DOUBLE PRECISION COEF  , GAMMA2, SEUIL , DEUPI
C
C
      DEUPI  = 6.283185307D0
      SEUIL  = 1.D-6
      GAMMA2 = GAMATG*GAMATG
      COEF   = -24.D0*DSQRT(DEUPI)*BORETG**3/GAMMA2
C
      IF (IWHTG.EQ.1) THEN
C
C.......COMPUTES THE LINEAR COEFFICIENT BETA : QBREK2 = BETA * F
C       WITH THE WEIGHT FUNCTION W(H) = CONSTANT
C       """""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO 25 IP = 1,NPOIN2
          BETA(IP) = COEF*8.D0*VARIAN(IP)**2.5D0*FCAR(IP)
     &             /(GAMMA2*DEPTH(IP)**5)
   25   CONTINUE
C
      ELSEIF (IWHTG.EQ.2) THEN
C
C.......COMPUTES THE LINEAR COEFFICIENT BETA : QBREK2 = BETA * F
C       WITH THE WEIGHT FUNCTION W(H) != CONSTANT
C       """""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO 35 IP = 1,NPOIN2
          BETA(IP) = (COEF*VARIAN(IP)**1.5D0*FCAR(IP)/
     &                DEPTH(IP)**3)*(1.D0-1.D0/(1.D0+VARIAN(IP)*8.D0
     &                /(GAMMA2*DEPTH(IP)*DEPTH(IP)))**2.5D0)
  35    CONTINUE
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