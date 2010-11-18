C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE CONTRIBUTION OF THE WAVE GENERATION
!>               (BY WIND) SOURCE TERM BASED ON SNYDER ET AL. (1981),
!>                MODIFIED BY KOMEN ET AL. (1984) TO MAKE USE OF THE
!>                FRICTION VELOCITY U* INSTEAD OF THE U5 VELOCITY
!>               (MEASURED 5 METERS ABOVE) FOR THE WIND.
!><br>            THIS GENERATION THEORY IS IDENTICAL TO THAT IN WAM-CYCLE 3.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference SNYDER ET AL. (1981) :
!>                     "ARRAY MEASUREMENTS OF ATMOSPHERIC PRESSURE
!>                      FLUCTUATIONS ABOVE SURFACE GRAVITY WAVES".
!>                      JOURNAL OF FLUID MECH., VOL 102., PP 1-59.

!>  @reference KOMEN ET AL.  (1984) :
!>                     "ON THE EXISTENCE OF A FULLY DEVELOPED
!>                      WINDSEA SPECTRUM". JPO, VOL 14, PP 1271-1285.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BETAN, BETAO, CIMPLI, CPHAS, F, FREQ, GRAVIT, NF, NPLAN, NPOIN2, ROAIR, ROEAU, TETA, TSDER, TSTOT, TWNEW, TWOLD, USN, USNEW, USO, USOLD, XK
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C1, CONST, DEUPI, DIMPLI, DIREC, IP, JF, JP
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
!> </td><td> M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
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
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE QWIND2
     &( TSTOT , TSDER , F     , XK    , FREQ  , USOLD , USNEW , TWOLD ,
     &  TWNEW , TETA  , ROAIR , ROEAU , GRAVIT, NF    , NPLAN , NPOIN2,
     &  CIMPLI, CPHAS , USN   , USO   , BETAN , BETAO )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BETAN          |---| 
C| BETAN(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| BETAO          |---| 
C| BETAO(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| CIMPLI         |---| 
C| CPHAS          |---| 
C| CPHAS(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL
C| FREQ           |---| 
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| ROAIR          |-->| DENSITE DE L'AIR
C| ROEAU          |-->| DENSITE DE L'EAU
C| TETA           |---| 
C| TETA(          |-->| TABLEAU DES DIRECTIONS DE DISCRETISATION
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
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION GRAVIT, ROAIR , ROEAU , CIMPLI
      DOUBLE PRECISION  FREQ(NF)    , TETA(NPLAN)
      DOUBLE PRECISION TWOLD(NPOIN2), TWNEW(NPOIN2), USNEW(NPOIN2)
      DOUBLE PRECISION BETAO(NPOIN2), BETAN(NPOIN2), USOLD(NPOIN2)
      DOUBLE PRECISION CPHAS(NPOIN2),   USO(NPOIN2),   USN(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF),    XK(NPOIN2,NF)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION DEUPI , C1    , DIREC , CONST , DIMPLI
C
C
      DEUPI = 2.D0* 3.14159265358978D0
      C1 = 0.25D0 * (ROAIR/ROEAU) * DEUPI
      DIMPLI=1.0D0-CIMPLI
C
C.....LOOP ON THE DISCRETISED DIRECTIONS
C     """"""""""""""""""""""""""""""""""""""""""""
      DO JP=1,NPLAN
C
C.......COMPUTES (1ST PASS) THE DIRECTIONAL DEPENDENCES
C       """"""""""""""""""""""""""""""""""""""""""""""
        DIREC=TETA(JP)
        DO IP=1,NPOIN2
          USO(IP)=USOLD(IP)*COS(DIREC-TWOLD(IP))
          USN(IP)=USNEW(IP)*COS(DIREC-TWNEW(IP))
        ENDDO
C
C.......LOOP ON THE DISCRETISED FREQUENCIES
C       """"""""""""""""""""""""""""""""""""""""""""
        DO JF=1,NF
          CONST=C1*FREQ(JF)
C
C.........COMPUTES THE PROPORTIONALITY FACTORS BETA
C         """"""""""""""""""""""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            CPHAS(IP) = DEUPI * FREQ(JF) / XK(IP,JF)
            BETAO(IP)=MAX(28.D0*USO(IP)/CPHAS(IP)-1.D0,0.D0)*CONST
            BETAN(IP)=MAX(28.D0*USN(IP)/CPHAS(IP)-1.D0,0.D0)*CONST
          ENDDO
C
C.........TAKES THE SOURCE TERM INTO ACCOUNT
C         """"""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)
     &             + (DIMPLI*BETAO(IP)+CIMPLI*BETAN(IP)) * F(IP,JP,JF)
            TSDER(IP,JP,JF) = TSDER(IP,JP,JF) + BETAN(IP)
          ENDDO
C
        ENDDO
C
      ENDDO
C
      RETURN
      END
C
C#######################################################################
C