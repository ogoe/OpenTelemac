C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE WAVE STRESSES FOR ALL THE NODES
!>                IN THE MESH.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   THIS SUBROUTINE COMPUTES TAUHF DIRECTLY FROM USTAR AND
!>          ALFA. (THIS WAS DONE VIA A CALL TO 'TAUWHF' PREVIOUSLY).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BETAM, COSTET, DECAL, DFREQ, F, FREQ, GRAVIT, NF, NPLAN, NPOIN2, ROAIR, ROEAU, SINTET, TAUHF, TAUWAV, TETA, TSTOT, TWNEW, USNEW, XKAPPA, XTAUW, YTAUW, Z0NEW
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALF2, ALFA, AUX, C1, C2, CM, COEF1, COEF2, CONST1, COSTMP, DELY, DEUPI, DIREC, DTETAR, FRMAX, IP, J, JF, JP, JTOT, OMEGA, OMEGAM, TTAUHF, UST, UST2, USTAR, X0, Y, YC, Z0, ZARG, ZBETA, ZLOG, ZMU, ZX
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DIFF3D(), SEMIMP(), WAVE_EQUATION()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 03/05/95
!> </td><td> M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BETAM
!></td><td>--></td><td>CONSTANTE BETAMAX DE LA FORMULE DU VENT
!>    </td></tr>
!>          <tr><td>COSTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COSTET(
!></td><td>--></td><td>VECTEUR DES COSINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>DECAL
!></td><td>--></td><td>CONSTANTE DE DECALAGE DE CROISSANCE VENT
!>    </td></tr>
!>          <tr><td>DFREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DFREQ(
!></td><td>--></td><td>VECTEUR DES INTERVALLES FREQUENTIELS
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td>--></td><td>SPECTRE DIRECTIONNEL
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>VECTEUR DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
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
!>          <tr><td>ROAIR
!></td><td>--></td><td>MASSE VOLUMIQUE DE L AIR
!>    </td></tr>
!>          <tr><td>ROEAU
!></td><td>--></td><td>MASSE VOLUMIQUE DE L EAU
!>    </td></tr>
!>          <tr><td>SINTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SINTET(
!></td><td>--></td><td>VECTEUR DES   SINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>TAUHF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUHF(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TAUWAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUWAV(
!></td><td><--</td><td>TABLEAU DES CONTRAINTES DUES A LA HOULE
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA(
!></td><td>--></td><td>VECTEUR DES DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>TSTOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TSTOT(
!></td><td>--></td><td>CONTRIBUTION TERME SOURCE - PARTIE TOTALE
!>    </td></tr>
!>          <tr><td>TWNEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TWNEW(
!></td><td>--></td><td>TABLEAU DES DIRECTIONS DU VENT
!>    </td></tr>
!>          <tr><td>USNEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>USNEW(
!></td><td>--></td><td>TABLEAU DES VITESSES DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>XKAPPA
!></td><td>--></td><td>CONSTANTE DE VON KARMAN
!>    </td></tr>
!>          <tr><td>XTAUW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XTAUW(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>YTAUW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YTAUW(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>Z0NEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z0NEW(
!></td><td>--></td><td>TABLEAU DES LONGUEURS DE RUGOSITE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE STRESS
     &( TAUWAV, TSTOT , F     , USNEW , TWNEW , Z0NEW , FREQ  , DFREQ ,
     &  TETA  , SINTET, COSTET, ROAIR , ROEAU , XKAPPA, BETAM , DECAL ,
     &  GRAVIT, NPOIN2, NPLAN , NF    , XTAUW , YTAUW , TAUHF )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BETAM          |-->| CONSTANTE BETAMAX DE LA FORMULE DU VENT
C| COSTET         |---| 
C| COSTET(        |-->| VECTEUR DES COSINUS DES DIRECTIONS
C| DECAL          |-->| CONSTANTE DE DECALAGE DE CROISSANCE VENT
C| DFREQ          |---| 
C| DFREQ(         |-->| VECTEUR DES INTERVALLES FREQUENTIELS
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL
C| FREQ           |---| 
C| FREQ(          |-->| VECTEUR DES FREQUENCES DE DISCRETISATION
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| ROAIR          |-->| MASSE VOLUMIQUE DE L AIR
C| ROEAU          |-->| MASSE VOLUMIQUE DE L EAU
C| SINTET         |---| 
C| SINTET(        |-->| VECTEUR DES   SINUS DES DIRECTIONS
C| TAUHF          |---| 
C| TAUHF(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TAUWAV         |---| 
C| TAUWAV(        |<--| TABLEAU DES CONTRAINTES DUES A LA HOULE
C| TETA           |---| 
C| TETA(          |-->| VECTEUR DES DIRECTIONS DE DISCRETISATION
C| TSTOT          |---| 
C| TSTOT(         |-->| CONTRIBUTION TERME SOURCE - PARTIE TOTALE
C| TWNEW          |---| 
C| TWNEW(         |-->| TABLEAU DES DIRECTIONS DU VENT
C| USNEW          |---| 
C| USNEW(         |-->| TABLEAU DES VITESSES DE FROTTEMENT
C| XKAPPA         |-->| CONSTANTE DE VON KARMAN
C| XTAUW          |---| 
C| XTAUW(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| YTAUW          |---| 
C| YTAUW(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| Z0NEW          |---| 
C| Z0NEW(         |-->| TABLEAU DES LONGUEURS DE RUGOSITE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NPOIN2, NPLAN , NF
      DOUBLE PRECISION ROAIR , ROEAU , XKAPPA , BETAM , DECAL , GRAVIT
      DOUBLE PRECISION TAUWAV(NPOIN2), USNEW(NPOIN2), TWNEW(NPOIN2)
      DOUBLE PRECISION  Z0NEW(NPOIN2), FREQ(NF) , DFREQ(NF)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION TETA(NPLAN), SINTET(NPLAN), COSTET(NPLAN)
      DOUBLE PRECISION XTAUW(NPOIN2) , YTAUW(NPOIN2), TAUHF(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  IP    , JP    , JF    , JTOT  , J
      DOUBLE PRECISION DEUPI , DTETAR, FRMAX , COEF1 , COEF2 , TTAUHF
      DOUBLE PRECISION USTAR , ALFA  , COSTMP, C1    , C2    , DIREC
      DOUBLE PRECISION Y     , OMEGA , CM    , ZX    , ZARG  , ZMU
      DOUBLE PRECISION ZLOG  , ZBETA , UST   , Z0    , UST2  , ALF2
      DOUBLE PRECISION CONST1, OMEGAM, X0    , YC    , DELY  , AUX
C
C
      DEUPI = 6.283185307D0
      DTETAR= DEUPI/DBLE(NPLAN)
      FRMAX = FREQ(NF)
      COEF1 = DTETAR*DEUPI**4*FRMAX**5/GRAVIT**2
      COEF2 = DEUPI*ROEAU/ROAIR*DTETAR
C
      DO 12 IP=1,NPOIN2
        XTAUW(IP)=0.D0
        YTAUW(IP)=0.D0
   12 CONTINUE
C
C.....INTEGRATES THE SOURCE TERM OVER FREQUENCIES AND DIRECTIONS
C     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      DO 110 JF=1,NF
        AUX=COEF2*FREQ(JF)*DFREQ(JF)
        DO 120 JP=1,NPLAN
          C1=AUX*SINTET(JP)
          C2=AUX*COSTET(JP)
C
          DO 100 IP=1,NPOIN2
            XTAUW(IP)=XTAUW(IP)+TSTOT(IP,JP,JF)*C1
            YTAUW(IP)=YTAUW(IP)+TSTOT(IP,JP,JF)*C2
  100     CONTINUE
  120   CONTINUE
  110 CONTINUE
C
C.....COMPUTES THE PARAMETERISED HIGH FREQUENCY PART
C     """""""""""""""""""""""""""""""""""""""""""""""""""
      DO 170 IP=1,NPOIN2
        TAUHF(IP)=0.D0
  170 CONTINUE
C
      DO 200 JP=1,NPLAN
        DIREC=TETA(JP)
        DO 171 IP=1,NPOIN2
          COSTMP=MAX(COS(DIREC-TWNEW(IP)),0.D0)
          TAUHF(IP)=TAUHF(IP)+F(IP,JP,NF)*COSTMP**3
  171   CONTINUE
  200 CONTINUE
C
      JTOT  = 50
      CONST1= BETAM/XKAPPA**2
      OMEGAM= 6.283185307D0*FRMAX
      X0    = 0.05D0
C
      DO 173 IP=1,NPOIN2
        USTAR=USNEW(IP)
        ALFA =Z0NEW(IP)*GRAVIT/USTAR**2
C
C----------------------------------------------OLD SUBROUTINE TAUWHF
CC      CALL TAUWHF
CC   *( TAUHF , USTAR , ALFA  , BETAM , XKAPPA , DECAL , FRMAX , GRAVIT)
C----------------------------------------------
C
CMB.....LIMITING FACTORS TO REPRODUCE WAM4 (SUBROUTINE TAUHF OF PREPROC)
C.......(THIS LIMITATION IS NOT JUSTIFIED A PRIORI. IT IS AN ARTEFACT
C.......OF TAUHF BEING DISCRETISED ON A GRID)
        UST2  = MIN(USTAR,5.D0)
        ALF2  = MIN(ALFA,0.11D0)
CMB.....END OF LIMITATION
C
        UST   = MAX(UST2,0.000001D0)
        Z0    = ALF2*UST**2/GRAVIT
C
        YC    = MAX(OMEGAM,X0*GRAVIT/UST)*SQRT(Z0/GRAVIT)
        DELY  = MAX((1.D0-YC)/FLOAT(JTOT),0.D0)
        TTAUHF = 0.D0
        DO 102 J=1,JTOT
          Y     = YC+DBLE(J-1)*DELY
          OMEGA = Y*SQRT(GRAVIT/Z0)
          CM    = GRAVIT/OMEGA
          ZX    = UST/CM +DECAL
          ZARG  = MIN(XKAPPA/ZX,20.D0)
          ZMU   = MIN(GRAVIT*Z0/CM**2*DEXP(ZARG),1.D0)
          ZLOG  = MIN(DLOG(ZMU),0.D0)
          ZBETA = CONST1*ZMU*ZLOG**4
          TTAUHF= TTAUHF+ZBETA/Y*DELY
  102   CONTINUE
C----------------------------------------------ANCIENNE SUB TAUWHF
C
        TAUHF(IP) = TTAUHF*COEF1*USTAR**2*TAUHF(IP)
  173 CONTINUE
C
C.....TAKES THE PARAMETERISED HIGH FREQUENCY PART INTO ACCOUNT
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C
      DO 190 IP=1,NPOIN2
        XTAUW(IP) = XTAUW(IP) + TAUHF(IP)*SIN(TWNEW(IP))
        YTAUW(IP) = YTAUW(IP) + TAUHF(IP)*COS(TWNEW(IP))
        TAUWAV(IP)= SQRT(XTAUW(IP)**2+YTAUW(IP)**2)
  190 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C