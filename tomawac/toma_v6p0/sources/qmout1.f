C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE CONTRIBUTION OF THE WHITECAPPING
!>                SOURCE TERM BASED ON KOMEN ET AL. (1984).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   CMOUT1 (USED IN WAM-CYCLE 4) EQUALS 4.5.
!>  @note   CMOUT2 (USED IN WAM-CYCLE 4) EQUALS 0.5.

!>  @reference KOMEN G.J., HASSELMANN S., HASSELMANN K. (1984) :
!>                     "ON THE EXISTENCE OF A FULLY DEVELOPED WINDSEA
!>                      SPECTRUM". JPO, VOL 14, PP 1271-1285.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BETA, CMOUT1, CMOUT2, ENRJ, F, FMOY, FREQ, GRAVIT, NF, NPLAN, NPOIN2, PROINF, TAUX1, TSDER, TSTOT, XK, XKMOY
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, C1, C2, DEUPI, IP, JF, JP
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
!> </td><td> 06/04/95
!> </td><td> P. THELLIER; M. BENOIT (EDF/DER/LNH)
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
!>          <tr><td>CMOUT1
!></td><td>--></td><td>CONSTANTE DE L'EXPRESSION DE MOUTONEMENT
!>    </td></tr>
!>          <tr><td>CMOUT2
!></td><td>--></td><td>CONSTANTE DE L'EXPRESSION DE MOUTONEMENT
!>    </td></tr>
!>          <tr><td>ENRJ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ENRJ(
!></td><td>--></td><td>ENERGIE TOTALE DU SPECTRE
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td>--></td><td>SPECTRE DIRECTIONNEL
!>    </td></tr>
!>          <tr><td>FMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FMOY(
!></td><td>--></td><td>TABLEAU DES FREQUENCES MOYENNES
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>TABLEAU DES FREQUENCES
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
!>          <tr><td>PROINF
!></td><td>--></td><td>INDICATEUR DE PROFONDEUR INFINIE
!>    </td></tr>
!>          <tr><td>TAUX1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUX1(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
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
!>          <tr><td>XK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XK(
!></td><td>--></td><td>NOMBRE D'ONDE
!>    </td></tr>
!>          <tr><td>XKMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XKMOY(
!></td><td>--></td><td>TABLEAU DES NOMBRES D'ONDE MOYENS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE QMOUT1
     &( TSTOT , TSDER , F     , XK    , ENRJ  , FREQ  , FMOY  , XKMOY ,
     &  PROINF, CMOUT1, CMOUT2, GRAVIT, NF    , NPLAN , NPOIN2, TAUX1 ,
     &  BETA  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BETA           |---| 
C| BETA(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| CMOUT1         |-->| CONSTANTE DE L'EXPRESSION DE MOUTONEMENT
C| CMOUT2         |-->| CONSTANTE DE L'EXPRESSION DE MOUTONEMENT
C| ENRJ           |---| 
C| ENRJ(          |-->| ENERGIE TOTALE DU SPECTRE
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL
C| FMOY           |---| 
C| FMOY(          |-->| TABLEAU DES FREQUENCES MOYENNES
C| FREQ           |---| 
C| FREQ(          |-->| TABLEAU DES FREQUENCES
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| PROINF         |-->| INDICATEUR DE PROFONDEUR INFINIE
C| TAUX1          |---| 
C| TAUX1(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TSDER          |---| 
C| TSDER(         |---| CONTRIBUTION TERME SOURCE - PARTIE DERIVEE
C| TSTOT          |---| 
C| TSTOT(         |---| CONTRIBUTION TERME SOURCE - PARTIE TOTALE
C| XK             |---| 
C| XK(            |-->| NOMBRE D'ONDE
C| XKMOY          |---| 
C| XKMOY(         |-->| TABLEAU DES NOMBRES D'ONDE MOYENS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION CMOUT1, CMOUT2, GRAVIT
      DOUBLE PRECISION XKMOY(NPOIN2), ENRJ(NPOIN2) ,  BETA(NPOIN2)
      DOUBLE PRECISION      FREQ(NF), FMOY(NPOIN2) , TAUX1(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF),    XK(NPOIN2,NF)
      LOGICAL PROINF
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION AUX   , DEUPI , C1    , C2
C
C
      DEUPI = 6.283185307D0
      C1 = - CMOUT1*DEUPI**9.D0/GRAVIT**4.D0
      C2 = - CMOUT1*DEUPI
C
      IF (PROINF) THEN
C     ---------------- INFINITE WATER DEPTH (USES F).
C
C.......WORKING ARRAY (THIS TERM ONLY DEPENDS ON THE POINT IN SPACE)
C       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO IP=1,NPOIN2
          TAUX1(IP) = C1 * ENRJ(IP)**2.D0 * FMOY(IP)**9.D0
        ENDDO
C
C.......LOOP OVER DISCRETISED FREQUENCIES
C       """""""""""""""""""""""""""""""""""""""""""
        DO JF=1,NF
C
C.........COMPUTES THE BETA COEFFICIENT : QMOUT1 = BETA * F
C         """"""""""""""""""""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            AUX = (FREQ(JF)/FMOY(IP))**2.D0
            BETA(IP)=TAUX1(IP)*((1.D0-CMOUT2)*AUX+CMOUT2*AUX**2.D0)
          ENDDO
C
C.........TAKES THE SOURCE TERM INTO ACCOUNT
C         """"""""""""""""""""""""""""""""
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)+BETA(IP)*F(IP,JP,JF)
              TSDER(IP,JP,JF) = TSDER(IP,JP,JF)+BETA(IP)
            ENDDO
          ENDDO
        ENDDO
C
      ELSE
C     ---------------- FINITE WATER DEPTH (USES K).
C
C.......WORKING ARRAY (THIS TERM ONLY DEPENDS ON THE POINT IN SPACE)
C       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO IP=1,NPOIN2
          TAUX1(IP) = C2 * ENRJ(IP)**2.D0 * FMOY(IP) * XKMOY(IP)**4.D0
        ENDDO
C
C.......LOOP OVER THE DISCRETISED FREQUENCIES
C       """""""""""""""""""""""""""""""""""""""""""
        DO JF=1,NF
C
C.........COMPUTES THE BETA COEFFICIENT : QMOUT1 = BETA * F
C         """"""""""""""""""""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            AUX = XK(IP,JF) / XKMOY(IP)
            BETA(IP)=TAUX1(IP)*((1.D0-CMOUT2)*AUX+CMOUT2*AUX**2.D0)
          ENDDO
C
C.........TAKES THE SOURCE TERM INTO ACCOUNT
C         """"""""""""""""""""""""""""""""
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)+BETA(IP)*F(IP,JP,JF)
              TSDER(IP,JP,JF) = TSDER(IP,JP,JF)+BETA(IP)
            ENDDO
          ENDDO
        ENDDO
C
      ENDIF
C
      RETURN
      END
C
C#######################################################################
C