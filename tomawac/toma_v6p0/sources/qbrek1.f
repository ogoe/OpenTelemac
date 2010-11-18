C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE CONTRIBUTION OF THE DEPTH-INDUCED
!>                BREAKING SOURCE TERM BASED ON BATTJES AND JANSSEN (1978).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   THIS SOURCE TERM IS LINEAR IN F(FREQ,TETA), AND THE LINEAR
!>          COEFFICIENT DOES NOT VARY WITH TIME.

!>  @reference BATTJES AND JANSSEN (1978) :
!>                     "ENERGY LOSS AND SET-UP DUE TO BREAKING
!>                      OF RANDOM WAVES". ICCE'78.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALFABJ, BETA, DEPTH, F, FCAR, GAMBJ1, GAMBJ2, IHMBJ, IQBBJ, NF, NPLAN, NPOIN2, TSDER, TSTOT, VARIAN
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> B, COEF, HM, IFF, IP, JP, QB, SEUIL, XK8, XKCAR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> QBBJ78(), WNSCOU()
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
!>      <td><center> 5.2                                       </center>
!> </td><td> 14/06/2001
!> </td><td> OPTIMER
!> </td><td>
!> </td></tr>
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
!>          <tr><td>ALFABJ
!></td><td>--></td><td>CONSTANTE ALPHA DU MODELE BJ
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
!>          <tr><td>GAMBJ1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GAMBJ2
!></td><td>--></td><td>CONSTANTE GAMMA DU MODELE BJ
!>    </td></tr>
!>          <tr><td>IHMBJ
!></td><td>--></td><td>TYPE DE HAUTEUR DE HOULE MAX POUR MODELE BJ
!>    </td></tr>
!>          <tr><td>IQBBJ
!></td><td>--></td><td>MODE DE CALCUL DE QB POUR MODELE BJ
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
                        SUBROUTINE QBREK1
     &( TSTOT , TSDER , F     , FCAR  , VARIAN, DEPTH , ALFABJ, GAMBJ1,
     &  GAMBJ2, IQBBJ , IHMBJ , NF    , NPLAN , NPOIN2, BETA  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALFABJ         |-->| CONSTANTE ALPHA DU MODELE BJ
C| BETA           |---| 
C| BETA(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| DEPTH          |---| 
C| DEPTH(         |-->| TABLEAU DES PROFONDEURS
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL
C| FCAR           |---| 
C| FCAR(          |-->| FREQUENCE CARACTERISTIQUE
C| GAMBJ1         |---| 
C| GAMBJ2         |-->| CONSTANTE GAMMA DU MODELE BJ
C| IHMBJ          |-->| TYPE DE HAUTEUR DE HOULE MAX POUR MODELE BJ
C| IQBBJ          |-->| MODE DE CALCUL DE QB POUR MODELE BJ
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
      INTEGER          NF    , NPLAN  , NPOIN2, IQBBJ , IHMBJ
      DOUBLE PRECISION ALFABJ, GAMBJ1 , GAMBJ2
      DOUBLE PRECISION DEPTH(NPOIN2), BETA(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF), VARIAN(NPOIN2)
      DOUBLE PRECISION     FCAR(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER          JP   , IFF , IP
      DOUBLE PRECISION COEF , HM  , XK8 , XKCAR , B , QB , SEUIL
C
C.....EXTERNAL FUNCTIONS
C     """"""""""""""""""
      DOUBLE PRECISION   QBBJ78
      EXTERNAL           QBBJ78
C
C
      SEUIL=1.D-6
      COEF =-.25D0*ALFABJ
C
C.....COMPUTES THE LINEAR COEFFICIENT BETA: QBREK1 = BETA * F
C     """""""""""""""""""""""""""""""""""""""""""""""""""""""
      DO 25 IP = 1,NPOIN2
         IF (VARIAN(IP).GT.SEUIL) THEN
C
C..........COMPUTES THE MAXIMUM WAVE HEIGHT
C          """""""""""""""""""""""""""""""""""""""
           IF (IHMBJ.EQ.1) THEN
             HM  = GAMBJ2*DEPTH(IP)
           ELSEIF (IHMBJ.EQ.2) THEN
             CALL WNSCOU(XKCAR,FCAR(IP),DEPTH(IP))
             XK8 = GAMBJ1/XKCAR
             HM  = XK8*DTANH(GAMBJ2*DEPTH(IP)/XK8)
           ENDIF
C
C..........COMPUTES THE FRACTION OF BREAKING WAVES
C          """"""""""""""""""""""""""""""""""""""""""""
           B   = DSQRT(8.D0*VARIAN(IP))/HM
           QB  = QBBJ78(B,IQBBJ)
C
           BETA(IP) = COEF*QB*FCAR(IP)*HM*HM/VARIAN(IP)
         ELSE
           BETA(IP) = 0.D0
         ENDIF
   25 CONTINUE
C
C.....TAKES THE SOURCE TERM INTO ACCOUNT
C     """"""""""""""""""""""""""""""""
      DO 10 IFF = 1,NF
        DO 20 JP = 1,NPLAN
          DO 30 IP = 1,NPOIN2
            TSTOT(IP,JP,IFF) = TSTOT(IP,JP,IFF)+BETA(IP)*F(IP,JP,IFF)
C           TSDER(IP,JP,IFF) = TSDER(IP,JP,IFF)+BETA(IP)
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C