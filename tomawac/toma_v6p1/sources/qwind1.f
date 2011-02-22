C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE CONTRIBUTION OF THE WAVE GENERATION
!>               (BY WIND) SOURCE TERM BASED ON JANSSEN (1989,1991).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference JANSSEN P.A.E.M (1989) :
!>                     "WIND-INDUCED STRESS AND THE DRAG OF AIR
!>                      FLOW OVER SEA WAVES". JPO, VOL 19, PP 745-754.

!>  @reference JANSSEN P.A.E.M (1991) :
!>                     "QUASI-LINEAR THEORY OF WIND-WAVE GENERATION
!>                      APPLIED TO WAVE FORECASTING". JPO, VOL 21, PP 1631-1642.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BETAM, BETAN, BETAO, CIMPLI, CPHAS, DECAL, F, FREQ, GRAVIT, NF, NPLAN, NPOIN2, OMNEW, OMOLD, ROAIR, ROEAU, TETA, TNEW, TOLD, TSDER, TSTOT, TWNEW, TWOLD, USN, USNEW, USO, USOLD, XK, XKAPPA, Z0NEW, Z0OLD
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C1, CONST, DEUPI, DIMPLI, DIREC, IP, JF, JP, XX, ZLOGMU
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
!>      <td><center> 1.0                                       </center>
!> </td><td> 11/04/95
!> </td><td> P. THELLIER; M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BETAM
!></td><td>--></td><td>PARAMETRE D'APPORT DU AU VENT
!>    </td></tr>
!>          <tr><td>BETAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETAN(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>BETAO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETAO(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>CIMPLI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CPHAS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CPHAS(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>DECAL
!></td><td>--></td><td>CONSTANTE DE DECALAGE DE COURBE CROISSANCE
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
!></td><td>--></td><td>TABLEAU DES FREQUENCES DE DISCRETISATION
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
!>          <tr><td>OMNEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OMNEW(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>OMOLD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OMOLD(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>ROAIR
!></td><td>--></td><td>DENSITE DE L'AIR
!>    </td></tr>
!>          <tr><td>ROEAU
!></td><td>--></td><td>DENSITE DE L'EAU
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA(
!></td><td>--></td><td>TABLEAU DES DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>TNEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TNEW(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2*NPLAN)
!>    </td></tr>
!>          <tr><td>TOLD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOLD(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2*NPLAN)
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
!>          <tr><td>TWNEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TWNEW(
!></td><td>--></td><td>DIRECTION DU VENT A L'INSTANT N+1
!>    </td></tr>
!>          <tr><td>TWOLD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TWOLD(
!></td><td>--></td><td>DIRECTION DU VENT A L'INSTANT N
!>    </td></tr>
!>          <tr><td>USN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>USN(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>USNEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>USNEW(
!></td><td>--></td><td>VITESSE DE FROTTEMENT A L'INSTANT N+1
!>    </td></tr>
!>          <tr><td>USO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>USO(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>USOLD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>USOLD(
!></td><td>--></td><td>VITESSE DE FROTTEMENT A L'INSTANT N
!>    </td></tr>
!>          <tr><td>XK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XK(
!></td><td>--></td><td>NOMBRE D'ONDE
!>    </td></tr>
!>          <tr><td>XKAPPA
!></td><td>--></td><td>CONSTANTE DE VON KARMAN
!>    </td></tr>
!>          <tr><td>Z0NEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z0NEW(
!></td><td>--></td><td>LARGEUR DE RUGOSITE L'INSTANT N+1
!>    </td></tr>
!>          <tr><td>Z0OLD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z0OLD(
!></td><td>--></td><td>LARGEUR DE RUGOSITE L'INSTANT N
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE QWIND1
     &( TSTOT , TSDER , F     , XK    , FREQ  , USOLD , USNEW , TWOLD ,
     &  TWNEW , Z0OLD , Z0NEW , TETA  , ROAIR , ROEAU , BETAM , XKAPPA,
     &  DECAL , GRAVIT, NF    , NPLAN , NPOIN2, CIMPLI, TOLD  , TNEW  ,
     &  CPHAS , USN   , USO   , OMNEW , OMOLD , BETAN , BETAO )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BETAM          |-->| PARAMETRE D'APPORT DU AU VENT
C| BETAN          |---| 
C| BETAN(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| BETAO          |---| 
C| BETAO(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| CIMPLI         |---| 
C| CPHAS          |---| 
C| CPHAS(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| DECAL          |-->| CONSTANTE DE DECALAGE DE COURBE CROISSANCE
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL
C| FREQ           |---| 
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| OMNEW          |---| 
C| OMNEW(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| OMOLD          |---| 
C| OMOLD(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| ROAIR          |-->| DENSITE DE L'AIR
C| ROEAU          |-->| DENSITE DE L'EAU
C| TETA           |---| 
C| TETA(          |-->| TABLEAU DES DIRECTIONS DE DISCRETISATION
C| TNEW           |---| 
C| TNEW(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2*NPLAN)
C| TOLD           |---| 
C| TOLD(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2*NPLAN)
C| TSDER          |---| 
C| TSDER(         |---| CONTRIBUTION TERME SOURCE - PARTIE DERIVEE
C| TSTOT          |---| 
C| TSTOT(         |---| CONTRIBUTION TERME SOURCE - PARTIE TOTALE
C| TWNEW          |---| 
C| TWNEW(         |-->| DIRECTION DU VENT A L'INSTANT N+1
C| TWOLD          |---| 
C| TWOLD(         |-->| DIRECTION DU VENT A L'INSTANT N
C| USN            |---| 
C| USN(           |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| USNEW          |---| 
C| USNEW(         |-->| VITESSE DE FROTTEMENT A L'INSTANT N+1
C| USO            |---| 
C| USO(           |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| USOLD          |---| 
C| USOLD(         |-->| VITESSE DE FROTTEMENT A L'INSTANT N
C| XK             |---| 
C| XK(            |-->| NOMBRE D'ONDE
C| XKAPPA         |-->| CONSTANTE DE VON KARMAN
C| Z0NEW          |---| 
C| Z0NEW(         |-->| LARGEUR DE RUGOSITE L'INSTANT N+1
C| Z0OLD          |---| 
C| Z0OLD(         |-->| LARGEUR DE RUGOSITE L'INSTANT N
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION GRAVIT, ROAIR , ROEAU , BETAM , XKAPPA, DECAL ,
     &                 CIMPLI
      DOUBLE PRECISION  FREQ(NF)    , TETA(NPLAN)  , USOLD(NPOIN2)
      DOUBLE PRECISION TWOLD(NPOIN2), TWNEW(NPOIN2), USNEW(NPOIN2)
      DOUBLE PRECISION Z0OLD(NPOIN2), Z0NEW(NPOIN2), BETAN(NPOIN2)
      DOUBLE PRECISION CPHAS(NPOIN2),   USN(NPOIN2), OMOLD(NPOIN2)
      DOUBLE PRECISION OMNEW(NPOIN2),   USO(NPOIN2), BETAO(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF),    XK(NPOIN2,NF)
      DOUBLE PRECISION TOLD(NPOIN2,NPLAN)    ,  TNEW(NPOIN2,NPLAN)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION DEUPI , C1 , DIREC , CONST , DIMPLI
      DOUBLE PRECISION XX    , ZLOGMU
C
C
      DEUPI = 2.D0* 3.14159265358978D0
      C1 = DEUPI * (ROAIR/ROEAU) * (BETAM/XKAPPA**2.D0)
      DIMPLI=1.0D0-CIMPLI
C
C.....COMPUTES (1ST PASS) THE DIRECTIONAL DEPENDENCES
C     """"""""""""""""""""""""""""""""""""""""""""""
      DO JP=1,NPLAN
        DIREC=TETA(JP)
        DO IP=1,NPOIN2
          TOLD(IP,JP)=COS(DIREC-TWOLD(IP))
          TNEW(IP,JP)=COS(DIREC-TWNEW(IP))
        ENDDO
      ENDDO
C
C.....LOOP ON THE DISCRETISED FREQUENCIES
C     """"""""""""""""""""""""""""""""""""""""""""
      DO JF=1,NF
        CONST=C1*FREQ(JF)
C
C.......COMPUTES THE PHASE VELOCITY
C       """""""""""""""""""""""""""""
        DO IP=1,NPOIN2
          CPHAS(IP) = DEUPI * FREQ(JF) / XK(IP,JF)
        ENDDO
C
C.......COMPUTES (1ST PASS) THE FREQUENCIES (OMEGA AND UETOILE/CPHASE)
C       """""""""""""""""""""""""""""""""""""""""""""""""
        DO IP=1,NPOIN2
          OMOLD(IP) = GRAVIT * Z0OLD(IP) / CPHAS(IP)**2.D0
          OMNEW(IP) = GRAVIT * Z0NEW(IP) / CPHAS(IP)**2.D0
          USO(IP) = (USOLD(IP) / CPHAS(IP)) + DECAL
          USN(IP) = (USNEW(IP) / CPHAS(IP)) + DECAL
        ENDDO
C
C.......LOOP ON THE DISCRETISED DIRECTIONS
C       """"""""""""""""""""""""""""""""""""""""""""
        DO JP=1,NPLAN
C
          DO IP=1,NPOIN2
            BETAO(IP)=0.D0
            BETAN(IP)=0.D0
          ENDDO
C
C.........COMPUTES THE SOURCE TERM
C         """"""""""""""""""""""
          DO IP=1,NPOIN2
            IF (TOLD(IP,JP).GT.0.01D0) THEN
              XX = USO(IP) * TOLD(IP,JP)
              ZLOGMU = DLOG(OMOLD(IP)) + XKAPPA/XX
              IF (ZLOGMU.LT.0.D0) THEN
                BETAO(IP) = CONST*OMOLD(IP)*EXP(XKAPPA/XX)*
     &                          ZLOGMU**4.D0*XX**2.D0
              ENDIF
            ENDIF
          ENDDO
          DO IP=1,NPOIN2
            IF (TNEW(IP,JP).GT.0.01D0) THEN
              XX = USN(IP) * TNEW(IP,JP)
              ZLOGMU = DLOG(OMNEW(IP)) + XKAPPA/XX
              IF (ZLOGMU.LT.0.D0) THEN
                BETAN(IP) = CONST*OMNEW(IP)*EXP(XKAPPA/XX)*
     &                          ZLOGMU**4.D0*XX**2.D0
              ENDIF
            ENDIF
          ENDDO
C
C.........TAKES THE SOURCE TERM INTO ACCOUNT
C         """"""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)
     &              + (DIMPLI*BETAO(IP)+CIMPLI*BETAN(IP)) * F(IP,JP,JF)
            TSDER(IP,JP,JF) = TSDER(IP,JP,JF) + BETAN(IP)
          ENDDO
C
        ENDDO
      ENDDO
C
      RETURN
      END
C
C#######################################################################
C