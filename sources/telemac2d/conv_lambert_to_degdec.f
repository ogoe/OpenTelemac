!                       *********************************
                        SUBROUTINE CONV_LAMBERT_TO_DEGDEC
!                       *********************************
!
     &(NTAB,XTAB,YTAB,LAMBDATAB,PHITAB,NUMZONE)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   25/06/2010
!***********************************************************************
!
!brief    CONVERSION OF COORDINATES METRIC LAMBERT
!+        INTO LATITUDES, LONGITUDES (DECIMAL DEGREES)
!
!history  C-T PHAM (LNHE)
!+        25/06/2010
!+        V6P1
!+
!
!history  U.H.Merkel
!+        18/07/2012
!+        V6P2
!+  NAG doesn't like EPSILON -> renamed to EPSI
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LAMBDATAB      |<--| LONGITUDE
!| NUMZONE        |-->| NUMBER OF LAMBERT ZONE
!| NTAB           |-->| NUMBER OF COORDINATES 
!| PHITAB         |<--| LATITUDE
!| XTAB           |-->| METRIC COORDINATES (LAMBERT)
!| YTAB           |-->| METRIC COORDINATES (LAMBERT)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_CONV_LAMBERT_TO_DEGDEC
     &                         => CONV_LAMBERT_TO_DEGDEC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NTAB,NUMZONE
      DOUBLE PRECISION, INTENT(IN)  :: XTAB(NTAB),YTAB(NTAB)
      DOUBLE PRECISION, INTENT(OUT) :: LAMBDATAB(NTAB),PHITAB(NTAB)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI,DTR,RTD
      DOUBLE PRECISION X,Y,LAMBDA,PHI
      DOUBLE PRECISION LAMBDAC,EEE,ES2,NNN,CCC,XS,YS,RRR,GAMMA,LATISO
      DOUBLE PRECISION HE,AAA,GNORM,Z,TX,TY,TZ,PHIM,EPSI,CORRPHI
!
      INTEGER I,J
!
!-----------------------------------------------------------------------
!
      PI  = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
      RTD = 180.D0/PI
!
      EPSI = 1.D-11
!
!  LAMBDAC : PARIS MERIDIAN / GREENWICH MERIDIAN
!
      LAMBDAC = (2.D0+20.D0/60.D0+14.025D0/3600.D0)*DTR
!
!  TRANSF PROJECTION CC LAMBERT X, Y --> COORD GEO LAMBERT LAMBDA, PHI
!  (IGN: ALG0004)
!
      EEE = 0.08248325676D0
!
      ES2 = EEE/2.D0
!
!  NNN, CCC, XS, YS IN CONSTANTES DE PROJECTION, PROJECTION LAMBERT FRANCE
!
!  LAMBERT 1
      IF(NUMZONE.EQ.1) THEN
!       NNN = 0.760405966D0
!       CCC = 11603796.9767D0
        NNN = 0.7604059656D0
        CCC = 11603796.98D0
        XS  = 600000.D0
        YS  = 5657616.674D0
!  LAMBERT 2
      ELSEIF(NUMZONE.EQ.2) THEN
        NNN = 0.7289686274D0
        CCC = 11745793.39D0
        XS  = 600000.D0
        YS  = 6199695.768D0
!  LAMBERT 3
      ELSEIF(NUMZONE.EQ.3) THEN
        NNN = 0.6959127966D0
        CCC = 11947992.52D0
        XS  = 600000.D0
        YS  = 6791905.085D0
!  LAMBERT 4
      ELSEIF(NUMZONE.EQ.4) THEN
        NNN = 0.6712679322D0
        CCC = 12136281.99D0
        XS  = 234.358D0
        YS  = 7239161.542D0
!  LAMBERT 2 ETENDU
      ELSEIF(NUMZONE.EQ.22) THEN
        NNN = 0.7289686274D0
        CCC = 11745793.39D0
        XS  = 600000.D0
        YS  = 8199695.768D0
!  LAMBERT 93
      ELSEIF(NUMZONE.EQ.93) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'CONVERSION DE LAMBERT 93 PAS ENCORE IMPLEMENTEE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CONVERSION FROM LAMBERT 93 NOT YET IMPLEMENTED'
        ENDIF
        CALL PLANTE(1)
        STOP
!  DEFAULT VALUE
      ELSEIF(NUMZONE.EQ.-1) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'VALEUR PAR DEFAUT INCORRECTE POUR LA PROJECTION'
          WRITE(LU,*) 'LAMBERT. CHOISIR PARMI LES CHOIX POSSIBLES :'
          WRITE(LU,*) '  -1  : LAMBERT 1 NORD ;'
          WRITE(LU,*) '  -2  : LAMBERT 2 CENTRE ;'
          WRITE(LU,*) '  -3  : LAMBERT 3 SUD ;'
          WRITE(LU,*) '  -4  : LAMBERT 4 CORSE ;'
          WRITE(LU,*) '  -22 : LAMBERT 2 ETENDU.'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'INCORRECT DEFAULT VALUE FOR LAMBERT PROJECTION.'
          WRITE(LU,*) 'TO BE CHOSEN AMONG THE POSSIBLE CHOICES:'
          WRITE(LU,*) '  -1 : LAMBERT 1 NORTH ;'
          WRITE(LU,*) '  -2 : LAMBERT 2 CENTER ;'
          WRITE(LU,*) '  -3 : LAMBERT 3 SOUTH ;'
          WRITE(LU,*) '  -4 : LAMBERT 4 CORSICA ;'
          WRITE(LU,*) '  -22: LAMBERT 2 EXTENDED.'
        ENDIF
        CALL PLANTE(1)
        STOP
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'NUMERO DE PROJECTION LAMBERT',NUMZONE,'INCONNUE.'
          WRITE(LU,*) 'CHOISIR PARMI LES CHOIX POSSIBLES :'
          WRITE(LU,*) '  -1  : LAMBERT 1 NORD ;'
          WRITE(LU,*) '  -2  : LAMBERT 2 CENTRE ;'
          WRITE(LU,*) '  -3  : LAMBERT 3 SUD ;'
          WRITE(LU,*) '  -4  : LAMBERT 4 CORSE ;'
          WRITE(LU,*) '  -22 : LAMBERT 2 ETENDU.'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'UNKNOWN NUMBER OF LAMBERT PROJECTION',NUMZONE
          WRITE(LU,*) 'TO BE CHOSEN AMONG THE POSSIBLE CHOICES:'
          WRITE(LU,*) '  -1 : LAMBERT 1 NORTH ;'
          WRITE(LU,*) '  -2 : LAMBERT 2 CENTER ;'
          WRITE(LU,*) '  -3 : LAMBERT 3 SOUTH ;'
          WRITE(LU,*) '  -4 : LAMBERT 4 CORSICA ;'
          WRITE(LU,*) '  -22: LAMBERT 2 EXTENDED.'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO J=1,NTAB
        X = XTAB(J)
        Y = YTAB(J)
        RRR = SQRT((X-XS)**2+(Y-YS)**2)
        GAMMA = ATAN((X-XS)/(YS-Y))
!
        LAMBDA = GAMMA/NNN+LAMBDAC
!
        LATISO = -1.D0/NNN*LOG(ABS(RRR/CCC))
!
!  CALCULATION OF LATITUDE PHI FROM ISOMETRIC LATITUDE LATISO
!  (IGN: ALG0002)
!
!  I = 0
        PHIM = 2.D0*ATAN(EXP(LATISO))-PI/2.D0
!  I = 1
        PHI  = 2.D0*ATAN(EXP(LATISO)*( (1.D0+EEE*SIN(PHIM))
     &                                /(1.D0-EEE*SIN(PHIM)))**ES2)
     &        -PI/2.D0
!
        I = 1
!
        DO WHILE (ABS(PHI-PHIM).GE.EPSI)
          PHIM = PHI
          PHI  = 2.D0*ATAN(EXP(LATISO)*( (1.D0+EEE*SIN(PHIM))
     &                                  /(1.D0-EEE*SIN(PHIM)))**ES2)
     &          -PI/2.D0
          I = I + 1
        ENDDO
!
!  TRANSFORMATION GEOGRAPHIC COORDINATES LAMBDA, PHI --> CARTESIAN COORDINATES X, Y, Z
!  (IGN: ALG0009)
!
!  COMPUTATION OF THE BIG NORMAL (GRANDE NORMALE) GNORM
!  LAT PHI --> GNORM (IGN: ALG0021)
!
        HE = 0.D0               ! FOR THE TEST, NOT USEFUL FINALLY
!
        AAA = 6378249.2D0
        EEE = 0.08248325679D0
!
        GNORM = AAA/SQRT(1.D0-EEE**2*SIN(PHI)**2)
!
        X = (GNORM+HE)*COS(PHI)*COS(LAMBDA)
        Y = (GNORM+HE)*COS(PHI)*SIN(LAMBDA)
        Z = (GNORM*(1.D0-EEE**2)+HE)*SIN(PHI)
!
!  COORDINATE TRANSFORMATION AT 7 PARAMETERS BETWEEN 2 GEODESIC SYSTEMS
!  SIMPLIFIED TRANSFORMATION, JUST A TRANSLATION WITH 3 PARAMETERS (IGN: ALG0013 SIMPLIFIED)
!  NTF LAMBERT --> WGS84 (IGN)
!
        TX = -168.D0
        TY = -60.D0
        TZ =  320.D0
!
        X = X + TX
        Y = Y + TY
        Z = Z + TZ
!
!  TRANSFORMATION CARTESIAN COORDINATES X, Y, Z --> GEOGRAPHIC COORDINATES LAMBDA, PHI
!  HEISKANEN-MORITZ-BOUCHER METHOD (IGN: ALG0012)
! 
        LAMBDA = ATAN(Y/X)
!  I = 0
        PHIM   = ATAN(Z/(SQRT(X**2+Y**2)*(1.D0-(AAA*EEE**2)
     &                                        /(SQRT(X**2+Y**2+Z**2)))))
!  I = 1
        PHI = ATAN(Z/(SQRT(X**2+Y**2)*(1.D0-(AAA*EEE**2*COS(PHIM))
     &              /(SQRT(X**2+Y**2)*SQRT(1.D0-EEE**2*SIN(PHIM)**2)))))
!
        I = 1
!
        DO WHILE (ABS(PHI-PHIM).GE.EPSI)
          PHIM = PHI
          PHI = ATAN(Z/(SQRT(X**2+Y**2)*(1.D0-(AAA*EEE**2*COS(PHIM))
     &              /(SQRT(X**2+Y**2)*SQRT(1.D0-EEE**2*SIN(PHIM)**2)))))
          I = I + 1
        ENDDO
!
!  ANGLE CORRECTION FOR PHI (RE-CALIBRATION)
!
        CORRPHI = -5.439609D-5
        PHI = PHI + CORRPHI
!
!  CONVERSION INTO DECIMAL DEGREES
!
        LAMBDA = LAMBDA*RTD
        PHI    = PHI*RTD
!
        LAMBDATAB(J) = LAMBDA
        PHITAB(J)    = PHI
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
