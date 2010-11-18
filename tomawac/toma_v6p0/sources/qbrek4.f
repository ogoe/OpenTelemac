C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE CONTRIBUTION OF THE DEPTH-INDUCED
!>                BREAKING SOURCE TERM BASED ON IZUMIYA ET HORIKAWA (1984).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   THIS SOURCE TERM IS LINEAR IN F(FREQ,TETA), AND THE LINEAR
!>          COEFFICIENT DOES NOT VARY WITH TIME.

!>  @reference IZUMIYA T., HORIKAWA K. (1984) :
!>                     "WAVE ENERGY EQUATION APPLICABLE IN AND OUTSIDE
!>                      THE SURF ZONE". COASTAL ENGINEERING IN JAPAN, VOL 17, PP 119-137.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BETA, BETAIH, DEPTH, EM2SIH, F, FCAR, GRAVIT, NF, NPLAN, NPOIN2, TSDER, TSTOT, VARIAN
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COEF, DEUKD, GG1, GG2, IFF, IP, JP, XKCAR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> WNSCOU()
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
!>          <tr><td>BETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETA(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>BETAIH
!></td><td>--></td><td>CONSTANTE BETA0 DU MODELE IH
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEPTH(
!></td><td>--></td><td>TABLEAU DES PROFONDEURS
!>    </td></tr>
!>          <tr><td>EM2SIH
!></td><td>--></td><td>CONSTANTE M2*   DU MODELE IH
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
!>          <tr><td>GRAVIT
!></td><td>---</td><td>
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
                        SUBROUTINE QBREK4
     &( TSTOT , TSDER , F     , FCAR  , VARIAN, DEPTH , BETAIH, EM2SIH,
     &  GRAVIT, NF    , NPLAN , NPOIN2, BETA  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BETA           |---| 
C| BETA(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| BETAIH         |-->| CONSTANTE BETA0 DU MODELE IH
C| DEPTH          |---| 
C| DEPTH(         |-->| TABLEAU DES PROFONDEURS
C| EM2SIH         |-->| CONSTANTE M2*   DU MODELE IH
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL
C| FCAR           |---| 
C| FCAR(          |-->| FREQUENCE CARACTERISTIQUE
C| GRAVIT         |---| 
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
      INTEGER          NF    , NPLAN , NPOIN2
      DOUBLE PRECISION BETAIH, EM2SIH, GRAVIT
      DOUBLE PRECISION DEPTH(NPOIN2), BETA(NPOIN2), FCAR(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF), VARIAN(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  JP    , IFF   , IP
      DOUBLE PRECISION COEF  , XKCAR , DEUKD , GG1   , GG2
C
C
      COEF   = -DSQRT(GRAVIT)*BETAIH
C
C.....COMPUTES THE LINEAR COEFFICIENT BETA : QBREK4 = BETA * F
C     """""""""""""""""""""""""""""""""""""""""""""""""""""""
      DO 40 IP = 1,NPOIN2
        CALL WNSCOU( XKCAR, FCAR(IP), DEPTH(IP) )
        DEUKD=2.D0*XKCAR*DEPTH(IP)
        IF (DEUKD.GT.7.D2) THEN
          GG1 = 0.D0
          GG2 = 0.5D0
        ELSE
          GG1 = DEUKD/SINH(DEUKD)
          GG2 = 0.5D0*(1.D0+GG1)
        ENDIF
        BETA(IP) = COEF/DEPTH(IP)**1.5*DSQRT(VARIAN(IP)*GG1)
     &               *DSQRT(DMAX1(0.D0,GG2*VARIAN(IP)
     &               /(DEPTH(IP)*DEPTH(IP))-EM2SIH))
   40 CONTINUE
C
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