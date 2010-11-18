C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES HEADERS TO THE LISTING AT THE VARIOUS STAGES
!>                OF THE PROGRAM.
!><br>           (NON-HYDROSTATIC VERSION MESSAGES ADDED).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, IETAPE, LT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FR, FRNH, GB, GBNH, H, J, M, S
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BIL3D(), DIFF3D(), MURD3D(), MURD3D_POS(), TELEMAC3D()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 05/07/05
!> </td><td> J.M. HERVOUET  (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT , LT
!></td><td>--></td><td>TEMPS , NUMERO DU PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>IETAPE
!></td><td>--></td><td>INDICATEUR D'AVANCEMENT DANS LE PROGRAMME.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MITTIT
     & (IETAPE,AT,LT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT , LT        |-->| TEMPS , NUMERO DU PAS DE TEMPS.
C| IETAPE         |-->| INDICATEUR D'AVANCEMENT DANS LE PROGRAMME.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: IETAPE,LT
      DOUBLE PRECISION, INTENT(IN) :: AT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=50) :: FR(16), GB(16)
      CHARACTER(LEN=50) :: FRNH(4), GBNH(4)
!
      DOUBLE PRECISION S
      INTEGER J,H,M
!
!-----------------------------------------------------------------------
!
      DATA FR / 'TEMPS :                                           ',
     &          ' SECONDES                                         ',
     &          'IEME  ITERATION                                   ',
     &          'ETAPE DE CONVECTION-DIFFUSION DES VITESSES        ',
     &          'ETAPE DE CONVECTION-DIFFUSION DES TRACEURS        ',
     &          'PROPAGATION ET DIFFUSION AVEC EQUATION D''ONDE     ',
     &          'ETAPE DE CONVECTION-DIFFUSION DU K-EPSILON        ',
     &          'ETAPE DE SAINT-VENANT                             ',
     &          'ETAPE DE CALCUL DE LA VITESSE VERTICALE           ',
     &          '         BILAN DE MASSE                           ',
     &          '         BILAN DE MASSE FINAL                     ',
     &          '         DERIVE DE FLOTTEUR(S)                    ',
     &          'CONVECTION PAR METHODE DES CARACTERISTIQUES       ',
     &          'CONVECTION PAR SCHEMA SUPG                        ',
     &          'CONVECTION PAR SCHEMA N, PSI OU VOLUMES FINIS     ',
     &          'DIFFUSION                                         '/
!
      DATA FRNH/' + ETAPE DE CONVECTION - DIFFUSION +              ',
     &          ' + ETAPE DE PRESSION DYNAMIQUE +                  ',
     &          ' + ETAPE DE PROJECTION DES VITESSES +             ',
     &          ' + ETAPE DE PRESSION DYNAMIQUE, PREDICTION +      '/

!
!-----------------------------------------------------------------------
!
      DATA GB / ' TIME :                                           ',
     &          ' SECONDS                                          ',
     &          'TH  ITERATION                                     ',
     &          'ADVECTION-DIFFUSION OF VELOCITIES STEP            ',
     &          'ADVECTION-DIFFUSION OF TRACERS                    ',
     &          'PROPAGATION AND DIFFUSION WITH WAVE EQUATION      ',
     &          'ADVECTION-DIFFUSION OF K-EPSILON OR OMEGA STEP    ',
     &          'SHALLOW WATER STEP                                ',
     &          'CALCULATION OF VERTICAL VELOCITY STEP             ',
     &          '         MASS BALANCE                             ',
     &          '         FINAL MASS BALANCE                       ',
     &          '         DRIFT OF DROGUE(S)                       ',
     &          'ADVECTION BY CHARACTERISTIC CURVE METHOD          ',
     &          'ADVECTION BY SUPG METHOD                          ',
     &          'ADVECTION BY N, PSI OR FINITE VOLUME SCHEME       ',
     &          'DIFFUSION                                         '/
!
      DATA GBNH/' + ADVECTION AND DIFFUSION-FORCING STEP +         ',
     &          ' + DYNAMIC PRESSURE STAGE +                       ',
     &          ' + VELOCITY PROJECTION STEP +                     ',
     &          ' + DYNAMIC PRESSURE STAGE, PREDICTION +           '/
!
!***********************************************************************
!
C  DECOMPOSES TIME IN DAYS, HOURS, MINUTES AND SECONDS
!
      IF(IETAPE.EQ.1) THEN
        S = AT
        J = INT(AT/86400.D0)
        S = S - 86400.D0 * J
        H = INT(S/3600.D0)
        S = S - 3600.D0 * H
        M = INT(S/60.D0)
        S = S - 60.D0 * M
      ENDIF
!
C  PRINTS OUT
!
      IF(LNG.EQ.1) THEN
        IF(IETAPE.EQ.1) THEN
          WRITE(LU,10) 'ITERATION ',LT,' TEMPS ',J,H,M,S,AT
        ENDIF
        IF (IETAPE.GE. 4.AND.IETAPE.LE.12) WRITE(LU,200) FR(IETAPE)
        IF (IETAPE.GE.13.AND.IETAPE.LE.16) WRITE(LU,300) FR(IETAPE)
        IF (IETAPE.GE.17.AND.IETAPE.LE.20) WRITE(LU,200) FRNH(IETAPE-16)
      ELSEIF (LNG.EQ.2) THEN
        IF(IETAPE.EQ. 1) THEN
          WRITE(LU,11) 'ITERATION ',LT,' TIME ',J,H,M,S,AT
        ENDIF
        IF (IETAPE.GE. 4.AND.IETAPE.LE.12) WRITE(LU,200) GB(IETAPE)
        IF (IETAPE.GE.13.AND.IETAPE.LE.16) WRITE(LU,300) GB(IETAPE)
        IF (IETAPE.GE.17.AND.IETAPE.LE.20) WRITE(LU,200) GBNH(IETAPE-16)
      ENDIF
!
!-----------------------------------------------------------------------
!
10    FORMAT(/,80('='),/,A10,I8,A7,
     &     1I4,' J ',1I2,' H ',1I2,' MIN ',F8.4,' S',3X,'(',F16.4,' S)')
11    FORMAT(/,80('='),/,A10,I8,A6,
     &     1I4,' D ',1I2,' H ',1I2,' MN ',F8.4,' S',3X,'(',F16.4,' S)')
100   FORMAT(/,80('='),/,7X,A8,F12.4,A9,24X,I5,A15)
200   FORMAT(80('-'),/,7X,A50)
300   FORMAT(7X,A50)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C