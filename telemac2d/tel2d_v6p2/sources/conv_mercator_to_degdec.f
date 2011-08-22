!                     **********************************
                      SUBROUTINE CONV_MERCATOR_TO_DEGDEC
!                     **********************************
!
     &(NTAB,XTAB,YTAB,LAMBDATAB,PHITAB,GEOSYST,NUMZONE)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   22/04/2011
!***********************************************************************
!
!brief    CONVERSION OF COORDINATES METRIC MERCATOR
!+        INTO LATITUDES, LONGITUDES (DECIMAL DEGREES)
!
!history  C-T PHAM (LNHE)
!+        22/04/2011
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LAMBDATAB      |<--| LONGITUDE (DECIMAL DEGREES)
!| NTAB           |-->| NUMBER OF COORDINATES
!| GEOSYST        |-->| TYPE OF GEOGRAPHIC SYSTEM
!|                |   | 2: UTM NORTH     3: UTM SOUTH
!| NUMZONE        |-->| NUMBER OF UTM ZONE
!| PHITAB         |<--| LATITUDE (DECIMAL DEGREES)
!| XTAB           |-->| METRIC COORDINATES (WGS84 UTM)
!| YTAB           |-->| METRIC COORDINATES (WGS84 UTM)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NTAB,GEOSYST,NUMZONE
      DOUBLE PRECISION, INTENT(IN)  :: XTAB(NTAB),YTAB(NTAB)
      DOUBLE PRECISION, INTENT(OUT) :: LAMBDATAB(NTAB),PHITAB(NTAB)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI,EEE,AAA,FFF,EEE2,EEE4,EEE6,EEE8
      DOUBLE PRECISION LAMBDAC,NNN,XS,YS,LAMBDA,PHI
      DOUBLE PRECISION PHIM,LATISO,LATISOS,ES2,EPSILON
      DOUBLE PRECISION X,Y
      DOUBLE PRECISION CITM(5)
      COMPLEX(KIND(1.D0)) ZPRIME,ZZZ
!
      INTEGER I,J,K
!
!-----------------------------------------------------------------------
!
      PI = ACOS(-1.D0)
!
      EPSILON = 1.D-11
!
      AAA = 6378137.D0
      FFF = 1.D0/298.257223563D0
      EEE = SQRT(2.D0*FFF-FFF**2)
c$$$      EEE = 0.081991889980000D0
!
      NNN = 0.9996D0 * AAA
      LAMBDAC = (6.D0*REAL(NUMZONE)-183.D0)*PI/180.D0 ! RADIANS
      XS = 500000.D0
!
!     NORTHERN UTM
!
      IF(GEOSYST.EQ.2) THEN
        YS = 0.D0
!       SOUTHERN UTM
      ELSEIF(GEOSYST.EQ.3) THEN
        YS = 10000000.D0
      ENDIF
!
      ES2 = EEE/2.D0
!
      EEE2 = EEE**2
      EEE4 = EEE2**2
      EEE6 = EEE2*EEE4
      EEE8 = EEE4**2
!
!  PROJECTION COEFFICIENTS
!  MERCATOR TRANSVERSE PROJECTION "BACKWARD"
!  (IGN: ALG0029)
!
      CITM(1) = 1.D0 - 1.D0/4.D0*EEE2 - 3.D0/64.D0*EEE4
     &               - 5.D0/256.D0*EEE6 - 175.D0/16384.D0*EEE8
      CITM(2) = 1.D0/8.D0*EEE2 + 1.D0/48.D0*EEE4 + 7.D0/2048.D0*EEE6
     &                         + 1.D0/61440.D0*EEE8
      CITM(3) = 1.D0/768.D0*EEE4 + 3.D0/1280.D0*EEE6
     &                           + 559.D0/368640.D0*EEE8
      CITM(4) = 17.D0/30720.D0*EEE6 + 283.D0/430080.D0*EEE8
      CITM(5) = 4397.D0/41287680.D0*EEE8
!
!  BEGINNING OF LOOP ON POINTS
!
      DO J=1,NTAB
        X = XTAB(J)
        Y = YTAB(J)
!       ZPRIME = DCMPLX((Y-YS), (X-XS))/NNN/CITM(1)
        ZPRIME = CMPLX(Y-YS,X-XS,KIND(1.D0))/NNN/CITM(1)

        ZZZ = ZPRIME

        DO K=1,4
          ZZZ = ZZZ - CITM(K+1)*SIN(2.D0*REAL(K)*ZPRIME)
        ENDDO

        LATISO  =  REAL(ZZZ)
        LATISOS = AIMAG(ZZZ)
!
        LAMBDA = LAMBDAC + ATAN(SINH(LATISOS)/COS(LATISO))
        PHI    =           ASIN(SIN(LATISO)/COSH(LATISOS))
!
!  COMPUTATION OF LATITUDE PHI FROM ISOMETRIC LATITUDE LATISO
!  (ELLIPSOID OF 1ST EXCENTRICITY EEE AT POINT OF LATITUDE PHI)
!  (IGN: ALG0001)
!
        LATISO = LOG(TAN(PI/4.D0+PHI/2.D0)) ! EEE = 0.D0 HERE
!
!  COMPUTATION OF LATITUDE PHI FROM ISOMETRIC LATITUDE LATISO
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
        DO WHILE (ABS(PHI-PHIM).GE.EPSILON)
          PHIM = PHI
          PHI  = 2.D0*ATAN(EXP(LATISO)*( (1.D0+EEE*SIN(PHIM))
     &                                  /(1.D0-EEE*SIN(PHIM)))**ES2)
     &          -PI/2.D0
          I = I + 1
        ENDDO
!
!  CONVERSION INTO DECIMAL DEGREES
!
        LAMBDA = LAMBDA*180.D0/PI
        PHI    = PHI*180.D0/PI
!
        LAMBDATAB(J) = LAMBDA
        PHITAB(J)    = PHI
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
