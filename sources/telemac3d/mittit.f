!                    *****************
                     SUBROUTINE MITTIT
!                    *****************
!
     & (IETAPE,AT,LT)
!
!***********************************************************************
! TELEMAC3D   V7P2
!***********************************************************************
!
!brief    WRITES HEADERS TO THE LISTING AT THE VARIOUS STAGES
!+        OF THE PROGRAM.
!+
!+           (NON-HYDROSTATIC VERSION MESSAGES ADDED).
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        24/06/2016
!+        V7P2
!+   Adding the step of 3D continuity in the transformed mesh, when
!+   TRIDW3 is called (call hardcoded in preadv.f).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME OF TIME STEP
!| IETAPE         |-->| ADVANCING GAUGE IN THE PROGRAMME
!| LT             |-->| CURRENT TIME STEP NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IETAPE,LT
      DOUBLE PRECISION, INTENT(IN) :: AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=50) :: FR(17), GB(17)
      CHARACTER(LEN=50) :: FRNH(5), GBNH(5)
!
      DOUBLE PRECISION S
      INTEGER J,H,M
!
!-----------------------------------------------------------------------
!
!##> SEB @ HRW: NO DATA STATEMENT FOR TYPES WITH ALLOCATABLE COMPONENTS
!      DATA FR /
      PARAMETER ( FR = (/
     &     'TEMPS :                                           ',
     &     ' SECONDES                                         ',
     &     'IEME  ITERATION                                   ',
     &     'ETAPE DE CONVECTION-DIFFUSION DES VITESSES        ',
     &     'ETAPE DE CONVECTION-DIFFUSION DES TRACEURS        ',
     &     'PROPAGATION ET DIFFUSION AVEC EQUATION D''ONDE     ',
     &     'ETAPE DE CONVECTION-DIFFUSION DU K-EPSILON        ',
     &     'ETAPE DE SAINT-VENANT                             ',
     &     'ETAPE DE CALCUL DE LA VITESSE VERTICALE           ',
     &     '         BILAN DE MASSE                           ',
     &     '         BILAN DE MASSE FINAL                     ',
     &     '         DERIVE DE FLOTTEUR(S)                    ',
     &     'CONVECTION PAR METHODE DES CARACTERISTIQUES       ',
     &     'CONVECTION PAR SCHEMA SUPG                        ',
     &     'CONVECTION PAR SCHEMA N, PSI OU VOLUMES FINIS     ',
     &     '                DIFFUSION                         ',
     &     '            ETAPE DE CONVECTION                   ' /) )
!
!      DATA FRNH/
      PARAMETER ( FRNH = (/
     &     '        ETAPE DE CONVECTION - DIFFUSION           ',
     &     '         ETAPE DE PRESSION DYNAMIQUE              ',
     &     '         ETAPE DE PROJECTION DES VITESSES         ',
     &     '     ETAPE DE PRESSION DYNAMIQUE, PREDICTION      ',
     &     'ETAPE DE CONTINUITE 3D DANS LE DOMAINE TRANSFORME ' /) )
!
!-----------------------------------------------------------------------
!
!      DATA GB /
      PARAMETER ( GB = (/
     &     ' TIME :                                           ',
     &     ' SECONDS                                          ',
     &     'TH  ITERATION                                     ',
     &     'ADVECTION-DIFFUSION OF VELOCITIES STEP            ',
     &     'ADVECTION-DIFFUSION OF TRACERS                    ',
     &     'PROPAGATION AND DIFFUSION WITH WAVE EQUATION      ',
     &     'ADVECTION-DIFFUSION OF K-EPSILON OR OMEGA STEP    ',
     &     'SHALLOW WATER STEP                                ',
     &     'CALCULATION OF VERTICAL VELOCITY STEP             ',
     &     '         MASS BALANCE                             ',
     &     '         FINAL MASS BALANCE                       ',
     &     '         DRIFT OF DROGUE(S)                       ',
     &     '   ADVECTION BY CHARACTERISTIC CURVE METHOD       ',
     &     '       ADVECTION BY SUPG METHOD                   ',
     &     '   ADVECTION BY N, PSI OR FINITE VOLUME SCHEME    ',
     &     '             DIFFUSION                            ',
     &     '             ADVECTION STEP                       ' /) )
!
!      DATA GBNH/
      PARAMETER ( GBNH = (/
     &     '     ADVECTION AND DIFFUSION-FORCING STEP         ',
     &     '         DYNAMIC PRESSURE STAGE                   ',
     &     '         VELOCITY PROJECTION STEP                 ',
     &     '     DYNAMIC PRESSURE STAGE, PREDICTION           ',
     &     '  STAGE OF 3D CONTINUITY IN TRANSFORMED MESH      ' /) )
!
!***********************************************************************
!
!  DECOMPOSES TIME IN DAYS, HOURS, MINUTES AND SECONDS
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
!  PRINTS OUT
!
      IF(LNG.EQ.1) THEN
        IF(IETAPE.EQ.1) THEN
          WRITE(LU,10) 'ITERATION ',LT,' TEMPS ',J,H,M,S,AT
        ENDIF
        IF(IETAPE.GE.4.AND.IETAPE.LE.12.OR.IETAPE.EQ.17) THEN
          WRITE(LU,200) FR(IETAPE)
        ENDIF
        IF(IETAPE.GE.13.AND.IETAPE.LE.16) WRITE(LU,300) FR(IETAPE)
        IF(IETAPE.GE.18.AND.IETAPE.LE.22) WRITE(LU,200) FRNH(IETAPE-17)
      ELSEIF(LNG.EQ.2) THEN
        IF(IETAPE.EQ. 1) THEN
          WRITE(LU,11) 'ITERATION ',LT,' TIME ',J,H,M,S,AT
        ENDIF
        IF(IETAPE.GE.4.AND.IETAPE.LE.12.OR.IETAPE.EQ.17) THEN
          WRITE(LU,200) GB(IETAPE)
        ENDIF
        IF(IETAPE.GE.13.AND.IETAPE.LE.16) WRITE(LU,300) GB(IETAPE)
        IF(IETAPE.GE.18.AND.IETAPE.LE.22) WRITE(LU,200) GBNH(IETAPE-17)
      ENDIF
!
!-----------------------------------------------------------------------
!
10    FORMAT(/,80('='),/,A10,I8,A7,
     &     1I4,' J ',1I2,' H ',1I2,' MIN ',F8.4,' S',3X,'(',F16.4,' S)',
     &       /,80('='))
11    FORMAT(/,80('='),/,A10,I8,A6,
     &     1I4,' D ',1I2,' H ',1I2,' MN ',F8.4,' S',3X,'(',F16.4,' S)',
     &       /,80('='))
!100   FORMAT(/,80('='),/,7X,A8,F12.4,A9,24X,I5,A15)
200   FORMAT(80('-'),/,7X,A50)
300   FORMAT(7X,A50)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

