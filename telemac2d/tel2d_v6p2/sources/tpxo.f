!                     ***********
                      MODULE TPXO
!                     ***********
!
!***********************************************************************
! TELEMAC2D   V6P2                                   06/12/2011
!***********************************************************************
!
!brief    Module containing TPXO variables and subroutines
!+        TPXO is a tidal harmonic model based on the topex-poseidon
!+        satellite mission of sea surface observations.
!+        This module might also host variables from the other
!+        tidal model methodologies.
!
!history  G.Egbert of the Oregon State University (OSU)
!+        13/02/1996
!+        OTPS v 1.4
!+  Developments of the Tidal Prediction Software:
!+     OTPS (Fortran90 code) outside the TELEMAC system.
!+  ARGUMENTS and ASTROL subroutines supplied by
!+        R.Ray, March 1999, attached to OTIS
!
!history  Lana Erofeeva of the Oregon State University (OSU)
!+        23/02/1998
!+        OTPS v 1.5
!+        Gary's approach to minor constituents interpolation
!+        new FORTRAN version
!
!history  Lana Erofeeva of the Oregon State University (OSU)
!+        01/06/2004
!+        OTPS v 1.6
!+        OTPS based on R.Ray code perth2
!
!history  M.S.TURNBULL (HRW), N.DURAND (HRW), S.E.BOURBAN (HRW)
!+   06/12/2011
!+   V6P2
!+   Integration of OTPS in the TELEMAC system (for both 2D and 3D)
!
!reference RODNEY'S CONSTITUENT.H, 2/23/96
!
!reference RICHARD RAY'S
!+  "ARGUMENTS" AND "ASTROL", FOR JAN 1, 1992, 00:00 GREENWICH TIME
!+  CORRECTED JULY 12, 2000
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!note     THIS FILE IS ORGANISED IN SEVERAL PARTS:
!+      1ST: PARAMETERS AND VARIABLES OF THE TPXO MODEL
!+      ...
!+      2ND: CONTAINS SUBROUTINE AND FUNCTIONS OF THE TPXO MODEL
!+      ...
!
      IMPLICIT NONE
!
      PRIVATE
      PUBLIC :: ALLBORD_TPXO,CONDI_TPXO,STIME_MJD,DATE_MJD,VITU_TPXO,
     &          VITV_TPXO,SL_TPXO
!
!-----------------------------------------------------------------------
!
!       1a) TPXO -- CONSTIT
!
!-----------------------------------------------------------------------
!
!brief  THIS PART CONTAINS THE STANDARD PARAMETERS WHICH DEFINE THE
!+  AMPLITUDES, FREQUENCIES, ETC. FOR THE PRIMARY TIDAL CONSTITUENTS
!+  (CURRENTLY 29)
!
      INTEGER, PARAMETER :: TPXO_NCMX = 29
      CHARACTER(LEN=4) TPXO_CONSTID(TPXO_NCMX)
      DATA TPXO_CONSTID
     &            /'m2  ','s2  ','k1  ','o1  ',
     &             'n2  ','p1  ','k2  ','q1  ',
     &             '2n2 ','mu2 ','nu2 ','l2  ',
     &             't2  ','j1  ','m1  ','oo1 ',
     &             'rho1','mf  ','mm  ','ssa ',
     &             'm4  ','ms4 ','mn4 ','m6  ',
     &             'm8  ','mk3 ','s6  ','2sm2',
     &             '2mk3'/
!
!brief  FOR EACH CONSTITUENT, THE FOLLOWING PARAMETERS ARE GIVEN:
!+  - ALPHA = CORRECTION FACTOR FOR FIRST ORDER LOAD TIDES
!+  - AMP = AMPLITUDE OF EQUILIBRIUM TIDE, IN M
!+  - PH = CURRENTLY SET TO ZERO ...
!+            PHASES FOR EACH CONSTITUENT ARE REFERRED TO THE TIME
!+            WHEN THE PHASE OF THE FORCING FOR THAT CONSTITUENT
!+            IS ZERO ON THE GREENWICH MERIDIAN
!+  - OMEGA = ANGULAR FREQUENCY OF CONSTITUENT, IN RADIANS
!+  TIDAL PARAMETERS TAKEN FROM RODNEY'S CONSTITUENT.H, 2/23/96:
!+     (EXCEPT FOR ISPEC).
!
      DOUBLE PRECISION TPXO_OMEGA_D(TPXO_NCMX)
      DOUBLE PRECISION TPXO_PHASE_MKB(TPXO_NCMX),TPXO_BETA_SE(TPXO_NCMX)
!
!     DOUBLE PRECISION TPXO_ALPHA_D(TPXO_NCMX)
!     DATA TPXO_ALPHA_D/
!    &    0.693,       0.693,       0.736,       0.695,
!    &    0.693,       0.706,       0.693,       0.695,
!    &    0.693,       0.693,       0.693,       0.693,
!    &    0.693,       0.695,       0.695,       0.695,
!    &    0.695,       0.693,       0.693,       0.693,
!    &    0.693,       0.693,       0.693,       0.693,
!    &    0.693,       0.693,       0.693,       0.693,
!    &    0.693/
!
!     DOUBLE PRECISION TPXO_AMP_D(TPXO_NCMX)
!     DATA TPXO_AMP_D/
!    &    0.242334,    0.112743,    0.141565,    0.100661,
!    &    0.046397,    0.046848,    0.030684,    0.019273,
!    &    0.006141,    0.007408,    0.008811,    0.006931,
!    &    0.006608,    0.007915,    0.007915,    0.004338,
!    &    0.003661,    0.042041,    0.022191,    0.019567,
!         AMPLITUDE FOR M4 ETC. IS ZERO
!    &    0.,          0.,          0.,          0.,
!    &    0.,          0.,          0.,          0.,
!    &    0./
!
      DATA TPXO_OMEGA_D/
     &    1.405189D-04,1.454441D-04,7.292117D-05,6.759774D-05,
     &    1.378797D-04,7.252295D-05,1.458423D-04,6.495854D-05,
     &    1.352405D-04,1.355937D-04,1.382329D-04,1.431581D-04,
     &    1.452450D-04,7.556036D-05,7.028195D-05,7.824458D-05,
     &    6.531174D-05,0.053234D-04,0.026392D-04,0.003982D-04,
     &    2.810377D-04,2.859630D-04,2.783984D-04,4.215566D-04,
     &    5.620755D-04,2.134402D-04,4.363323D-04,1.503693D-04,
     &    2.081166D-04/
!
!brief   ASTRONOMICAL ARGUMENTS, OBTAINED WITH RICHARD RAY'S
!+  "ARGUMENTS" AND "ASTROL", FOR JAN 1, 1992, 00:00 GREENWICH TIME
!+  CORRECTED JULY 12, 2000
!
      DATA TPXO_PHASE_MKB/
     &    1.731557546D0, 0.000000000D0, 0.173003674D0, 1.558553872D0,
     &    6.050721243D0, 6.110181633D0, 3.487600001D0, 5.877717569D0,
     &    4.086699633D0, 3.463115091D0, 5.427136701D0, 0.553986502D0,
     &    0.052841931D0, 2.137025284D0, 2.436575100D0, 1.929046130D0,
     &    5.254133027D0, 1.756042456D0, 1.964021610D0, 3.487600001D0,
     &    3.463115091D0, 1.731557546D0, 1.499093481D0, 5.194672637D0,
     &    6.926230184D0, 1.904561220D0, 0.000000000D0, 4.551627762D0,
     &    3.809122439D0/
!
!note I AM PUTTING 0 FOR MS2, MN4 ETC. FOR NOW: CORRECT LATER
!+ NOW THIS CORRECTION IS DONE USING THE SAL FILE (H_TPXO3_90-90.LOAD)
!+ I REPLACE BETA_SE WITH UNITS FOR NOW (IN CASE WE DECIDE TO SWITCH
!+ BACK TO OLD VERSION) AND COMMENT THE OLD NUMBERS - THIS WAY I DO
!+ NOT CHANGE ANYTHING IN SUBROUTINES
!+ THIS WAS IN WEIGHTS.f BEFORE - PLACED HERE NOT TO MIX WITH W!
!+ TO REMOVE SOLID EARTH TIDE MULTIPLY BY BETA:
!
      DATA TPXO_BETA_SE/
     &    0.9540D0,      0.9540D0,      0.9400D0,      0.9400D0,
     &    0.9540D0,      0.9400D0,      0.9540D0,      0.9400D0,
     &    0.9540D0,      0.9540D0,      0.9540D0,      0.9540D0,
     &    0.9540D0,      0.9400D0,      0.9400D0,      0.9400D0,
     &    0.9400D0,      0.9400D0,      0.9400D0,      0.9400D0,
!         FOR M4 JUST USING VALUE FOR SEMI-DIURNALS (NO GOOD REASON!)
     &    0.9540D0,      0.9540D0,      0.9540D0,      0.954D0,
     &    0.9540D0,      0.9540D0,      0.9540D0,      0.954D0,
     &    0.9540D0/
!      DATA BETA_SE/29*1./
!
!     INTEGER TPXO_ISPEC_D(TPXO_NCMX)
!     DATA TPXO_ISPEC_D/
!    &    2,2,1,1,
!    &    2,1,2,1,
!    &    2,2,2,2,
!    &    2,1,1,1,
!    &    1,0,0,0,
!         NOTE: FOR NOW ISPEC FOR M4 SET TO 0 (ISPEC IS ONLY USED
!         TO DEFINE FORCING IN ATGF, AND THIS IS ALWAYS 0 FOR M4)
!    &    0,0,0,0,
!    &    0,0,0,0,
!    &    0/
!
!-----------------------------------------------------------------------
!
!       1b) TPXO -- WEIGHTS FOR M2, S2, K1, O1, N2, P1, K2, Q1
!
!-----------------------------------------------------------------------
!
!warning  This is good only for 8 constituents in order:
!+        M2, S2, K1, O1, N2, P1, K2, Q1
!
!brief   THE SAME ORDER IS SUPPORTED IN THE PREVIOUS SECTION (CONSTIT)
!+  I.E. NO NEED TO CARE ABOUT THE CORRESPONDING INDICES
!
      ! FOR CASE OF USING MODULE WEIGHTS
      INTEGER, PARAMETER :: TPXO_NCON = 17
!
!     DOUBLE PRECISION TPXO_BETA(TPXO_NCON)
      DOUBLE PRECISION TPXO_W(TPXO_NCON,8)
!
      DATA TPXO_W(1,:) /1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/
      DATA TPXO_W(2,:) /0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/
      DATA TPXO_W(3,:) /0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/
      DATA TPXO_W(4,:) /0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0/
      DATA TPXO_W(5,:) /0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0/
      DATA TPXO_W(6,:) /0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0/
      DATA TPXO_W(7,:) /0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0/
      DATA TPXO_W(8,:) /0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0/
      DATA TPXO_W(9,:) /-0.0379D0, 0.D0,0.D0,0.D0, 
     &                  0.30859D0,0.D0, 0.03289D0,0.D0/
      DATA TPXO_W(10,:)/-0.03961D0,0.D0,0.D0,0.D0, 
     &                  0.34380D0,0.D0, 0.03436D0,0.D0/
      DATA TPXO_W(11,:)/ 0.00696D0,0.D0,0.D0,0.D0, 
     &                   0.15719D0,0.D0,-0.00547D0,0.D0/
      DATA TPXO_W(12,:)/ 0.02884D0,0.D0,0.D0,0.D0,
     &                  -0.05036D0,0.D0, 0.07424D0,0.D0/
      DATA TPXO_W(13,:)/ 0.00854D0,0.D0,0.D0,0.D0,-0.01913D0,0.D0,
     &                   0.17685D0,0.D0/
      DATA TPXO_W(14,:)/0.D0,0.D0,-0.00571D0,0.11234D0,0.D0,
     &                  0.05285D0,0.D0,-0.26257D0/
      DATA TPXO_W(15,:)/0.D0,0.D0, 0.00749D0,0.07474D0,0.D0,
     &                  0.03904D0,0.D0,-0.12959D0/
      DATA TPXO_W(16,:)/0.D0,0.D0,-0.03748D0,0.12419D0,0.D0,
     &                  0.05843D0,0.D0,-0.29027D0/
      DATA TPXO_W(17,:)/0.D0,0.D0,0.00842D0,0.01002D0,0.D0,
     &                 -0.03064D0,0.D0,0.15028D0/
!
!-----------------------------------------------------------------------
!
!       1c) TPXO -- VARIABLES USED IN CONDIN/CONDIM
!
!-----------------------------------------------------------------------
!
!     DAYS IN MODIFIED JULIAN DAYS
!
      DOUBLE PRECISION STIME_MJD
!
!     NUMBER OF CONSTITUENTS TURNED ON
!
      INTEGER NCON
!
!     WHETHER TO INTERPOLATE FOR MINOR CONSTITUENTS
!
      LOGICAL I_MICON
!
!     INDICES OF AVAILABLE CONTITUENTS AMONGST THE ALL POSSIBLE
!
      CHARACTER(LEN=4) C_ID(TPXO_NCMX)
      INTEGER, ALLOCATABLE :: CCIND(:)
!
!     INTERPOLATED CONSTITUENTS FOR LIQUID BOUNDARY NODES
!
      INTEGER NPTNFR
      INTEGER, ALLOCATABLE :: TPXO_NFR(:)
      DOUBLE PRECISION, ALLOCATABLE :: TPXO_LATFR(:)
      COMPLEX, ALLOCATABLE :: TPXO_BOR(:,:,:)
!
!-----------------------------------------------------------------------
!
      SAVE
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                    **********************
                     SUBROUTINE DEF_CON_IND
!                    **********************
!
     &( C_ID, NCON, C_ID_MOD, NC, CIND )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    DEFINES CONSTITUENT INDICES IN A MODEL FOR GIVEN SET OF CONSTITUENTS
!
!warning  CONSTITUENT NAMES ARE ASSUMED TO BE ALL IN LOWER CASE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER NC,NCON
      INTEGER CIND(NCON)
      CHARACTER(LEN=4) C_ID(*),C_ID_MOD(*)
!
!-----------------------------------------------------------------------
!
      INTEGER IC1,IC2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO IC1 = 1,NCON
         CIND(IC1) = 0
         DO IC2 = 1,NC
            IF( C_ID(IC1).EQ.C_ID_MOD(IC2) ) CIND(IC1) = IC2
         ENDDO
      ENDDO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END SUBROUTINE DEF_CON_IND

!                    ******************
                     SUBROUTINE DEF_CID
!                    ******************
!
     &( NC0, CID, IND )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER NC0
      INTEGER IND(NC0)
      CHARACTER(LEN=4) CID(*)
!
!-----------------------------------------------------------------------
!
      INTEGER IC,JC,K
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      K = 1
      DO IC = 1,NC0
         IND(K) = 0
         DO JC = 1,TPXO_NCMX
            IF( CID(IC).EQ.TPXO_CONSTID(JC) ) THEN
               IND(K) = JC
               EXIT
            ENDIF
         ENDDO
         IF( IND(K).EQ.0 ) THEN
            IF(LNG.EQ.1) WRITE(LU,*) 'TPXO : ATTENTION :' //
     &         'COMPOSANTE ID ',CID(IC),' N''EST PAS PERMISE'
            IF(LNG.EQ.2) WRITE(LU,*) 'TPXO : WARNING :' //
     &         'CONSTITUENT ID ',CID(IC),' IS NOT ALLOWED'
         ENDIF
         K = K + 1
      ENDDO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END SUBROUTINE DEF_CID


!             ************************
              INTEGER FUNCTION IPSHFT
!             ************************
!
     &( I,ISH,N )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    PERIODIC SHIFT MAPS I TO I+ISH , MOD N;
!+        (ALWAYS BETWEEN 1 AND N;  NEVER 0 )
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I,N,ISH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IPSHFT = MOD( I+ISH+N-1,N ) + 1
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END FUNCTION IPSHFT

!                ********************************
                 DOUBLE PRECISION FUNCTION HEIGHT
!                ********************************
!
     &( A,P,NC )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    RETURNS HEIGHT FROM MODEL ARRAY OF COMPLEX CONSTITUENTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER NC
      COMPLEX A(NC),P(NC)
!
!-----------------------------------------------------------------------
!
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF( NC.EQ.0 ) THEN
         HEIGHT = 0.D0
         RETURN
      ENDIF
!
      HEIGHT = 0.D0
!     HEIGHT(I)=SUM_OF_REAL(A(I)*P(I))
      DO I = 1,NC
        HEIGHT = HEIGHT + REAL(P(I))*REAL(A(I))-AIMAG(P(I))*AIMAG(A(I))
      ENDDO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END FUNCTION HEIGHT

!                    ******************
                     SUBROUTINE INTERPT
!                    ******************
!
     &( UV,NT,N,M,MZ,TH_LIM,PH_LIM,XLAT,XLON,UV1,IERR,ZUV )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    INTERPOLATES COMPLEX ARRAY UV(NT,N,M) AT POINT XLAT,XLON
!+        TH_LIM AND PH_LIM GIVE LATITUDE AND LONGITUDE LIMITS OF GRID
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,M,IERR
      INTEGER MZ(N,M),NT
      CHARACTER(LEN=1) ZUV
      REAL TH_LIM(2),PH_LIM(2)
      DOUBLE PRECISION XLON,XLAT
      COMPLEX UV1(NT),UV(NT,N,M)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K
      INTEGER IW(0:1),JW(0:1)
!
      DOUBLE PRECISION WW(0:1,0:1),DLAT,DLON
      DOUBLE PRECISION DTHETA,DPHI,XLONC
!
!-----------------------------------------------------------------------
!
      IERR = 0
      DTHETA = ( TH_LIM(2)-TH_LIM(1) )/( REAL(M) )
      DPHI   = ( PH_LIM(2)-PH_LIM(1) )/( REAL(N) )
!
!     CHECKS XLON LONGITUDE CONVENTION
!
      XLONC = XLON
      IF( XLONC.LT.PH_LIM(1) ) XLONC = XLONC + 360.D0
      IF( XLONC.GT.PH_LIM(2) ) XLONC = XLONC - 360.D0
      IF( XLONC.LT.PH_LIM(1).OR.XLONC.GT.PH_LIM(2) ) THEN
         IERR = -1
         RETURN
      END IF
      IF( XLAT.LT.TH_LIM(1).OR.XLAT.GT.TH_LIM(2) ) THEN
         IERR = -1
         RETURN
      END IF
      DLAT = XLAT
      DLON = XLONC
!
      CALL BSI_WEIGHTS( ZUV,DLAT,DLON,TH_LIM,PH_LIM,
     &                 DPHI,DTHETA,MZ,N,M,WW,IW,JW )
!
      IF( (WW(0,0)+WW(1,0)+WW(0,1)+WW(1,1)).LT.0.01D0 ) THEN
         IERR = -2
         DO K = 1,NT
            UV1(K) = CMPLX(0.D0,0.D0)
         ENDDO
      ELSE
         IERR = 0
         DO K = 1,NT
            UV1(K) = UV(K,IW(0),JW(0))*WW(0,0) +
     &               UV(K,IW(1),JW(0))*WW(1,0) +
     &               UV(K,IW(0),JW(1))*WW(0,1) +
     &               UV(K,IW(1),JW(1))*WW(1,1)
         ENDDO
      ENDIF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END SUBROUTINE INTERPT

!                    **********************
                     SUBROUTINE BSI_WEIGHTS
!                    **********************
!
     &(ZUV,THETA,PHI,THETA_LIM,PHI_LIM,DX,DY,MASK,N,M,WW,IW,JW)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    BILINEAR SPLINE INTERPOLATION WEIGHTS FOR DELTA-FORCING
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER N,M
      INTEGER MASK(N,M)             ! GRID DIMENSIONS AND MASK
      INTEGER IW(0:1),JW(0:1)
      CHARACTER(LEN=1) ZUV          ! C-GRID ZUV: 'u','v','z' ('U','V','Z')
      DOUBLE PRECISION THETA, PHI   ! POINT COORDINATES
      REAL THETA_LIM(2),PHI_LIM(2)  ! GRID LIMITS
      DOUBLE PRECISION DX,DY        ! STEP X, STEP Y
      DOUBLE PRECISION WW(0:1,0:1)
!
!-----------------------------------------------------------------------
!
      INTEGER I0,J0,I1,J1
      INTEGER SM !,IPSHFT
      DOUBLE PRECISION XI,XJ,X,Y
      DOUBLE PRECISION W00,W01,W10,W11,WTOT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF( ZUV.EQ.'U' ) ZUV = 'u'
      IF( ZUV.EQ.'V' ) ZUV = 'v'
      IF( ZUV.EQ.'Z' ) ZUV = 'z'
!
      IF( ZUV.NE.'u'.AND.ZUV.NE.'v'.AND.ZUV.NE.'z' ) THEN
         IF(LNG.EQ.1) WRITE(LU,*) 'TPXO : PAS DE NOEUDS ',ZUV
         IF(LNG.EQ.2) WRITE(LU,*) 'TPXO : NO ',ZUV,' NODES'
         CALL PLANTE(1)
         STOP
      ENDIF
!
      IF( ZUV.EQ.'z' ) THEN
!
!     FINDS GRID COORDINATES FOR LOWER LEFT HAND Z NODE
!     AMONG THE SQUARE (OF Z NODES WHICH SURROUND THE OBS POINT)
!
!                 |
!                \|/
!     -> -z---u---z---u---z--
!         |       |       |     x IS THE LOCATION GIVEN BY THETA,PHI
!         v       v       v     THE FOUR SURROUNDING ELEVATION
!         | x     |       |     NODES ARE USED FOR THE BILINEAR
!     -> -z---u---z---u---z-    SPLINE INTERPOLATION
!         |      /|\      |
!                 |
!
         XI = (PHI  -PHI_LIM(1)  )/DX + 0.5D0
         XJ = (THETA-THETA_LIM(1))/DY + 0.5D0
!
      ELSEIF( ZUV.EQ.'u' ) THEN
!
!     INTERIOR POINT: MEASUREMENT OF CURRENT VECTOR
!                     DIRECTION IS GIVEN AS UNIT VECTOR (TH,PH)
!                     (TH,PH)=(1.,0.)->U
!                     (TH,PH)=(0.,1.)->V
!
!     FINDS GRID COORDINATES FOR LOWER LEFT HAND Z NODE
!     AMONG THE SQUARE (OF Z NODES WHICH SURROUND THE OBS POINT)
!
!             u---z---u--
!             |       |         x IS THE LOCATION GIVEN BY THETA,PHI
!             |   v---|---v     THE 8 SURROUNDING U,V NODES
!             |   | x |   |     ARE USED FOR THE BILINEAR SPLINE
!             u---z---u---z-    INTERPOLATION
!                 |       |     CURRENT VECTOR DIRECTION IS GIVEN
!                 v-------v     WITH A UNIT VECTOR (TH,PH):
!                               (1.,0)->EW, (0.,1.)->NS
!
         XI = (PHI  -PHI_LIM(1)  )/DX + 1.D0
         XJ = (THETA-THETA_LIM(1))/DY + 0.5D0
!
      ELSEIF( ZUV.EQ.'v' ) THEN
!
         XI = (PHI  -PHI_LIM(1)  )/DX + 0.5D0
         XJ = (THETA-THETA_LIM(1))/DY + 1.D0
!
      ENDIF
!
      IF( XI.LT.1.D0 ) XI = REAL(N) + XI
      I0 = INT(XI)
      X = XI - REAL(I0)
      J0 = INT(XJ)
      Y = XJ - REAL(J0)
!     CHECK TO SEE IF CALCULATED INDICES ARE IN RANGE
      IF( (I0.GT.N).OR.(I0.LT.1).OR.(J0.GT.M).OR.(J0.LT.1) ) THEN
         WW(0,0) = 0.D0
         WW(0,1) = 0.D0
         WW(1,0) = 0.D0
         WW(1,1) = 0.D0
         RETURN
      ENDIF
!
!     COMPUTES WEIGHTS FOR BILINEAR SPLINE INTERPOLATION; ONLY
!     USE OCEAN NODES (FOR WHICH MASK IS = 1)
!
      J1 = IPSHFT(J0,1,M)
      I1 = IPSHFT(I0,1,N)
      WW(0,0) = 0.D0
      WW(0,1) = 0.D0
      WW(1,0) = 0.D0
      WW(1,1) = 0.D0
      SM = MASK(I0,J0) + MASK(I0,J1) + MASK(I1,J0) + MASK(I1,J1)
      IF( SM.GT.0 ) THEN
         W00 = (1.D0-X)*(1.D0-Y)*REAL(MASK(I0,J0))
         W01 = (1.D0-X)*Y*REAL(MASK(I0,J1))
         W10 = X*(1.D0-Y)*REAL(MASK(I1,J0))
         W11 = X*Y*REAL(MASK(I1,J1))
         WTOT = W00+W01+W10+W11
         IF( WTOT.EQ.0.D0 ) THEN
            WW(0,0) = 0.D0
            WW(0,1) = 0.D0
            WW(1,0) = 0.D0
            WW(1,1) = 0.D0
            RETURN
         ENDIF
         WW(0,0) = W00/WTOT
         WW(0,1) = W01/WTOT
         WW(1,0) = W10/WTOT
         WW(1,1) = W11/WTOT
      ENDIF
!
      IW(0) = I0
      IW(1) = I1
      JW(0) = J0
      JW(1) = J1
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END SUBROUTINE BSI_WEIGHTS


!              *******************************
               DOUBLE PRECISION FUNCTION PTIDE
!              *******************************
!
     &( Z1,CID,NCON,IND,LAT,TIME_MJD,INTERP ) !,NTIME=1
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NCON !,NTIME
      INTEGER IND(NCON)
      LOGICAL INTERP
      CHARACTER(LEN=4) CID(NCON)
      DOUBLE PRECISION LAT !,ZPRED(NTIME)
      DOUBLE PRECISION TIME_MJD !(NTIME)
      COMPLEX Z1(NCON)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IERR
      DOUBLE PRECISION, PARAMETER :: SECONDSPERDAY = 86400.D0
      DOUBLE PRECISION TIME,DH
      DOUBLE PRECISION WW(NCON,8)
      DOUBLE PRECISION PU(TPXO_NCMX),PF(TPXO_NCMX),DLAT
! NODAL ARGUMENTS SHOUD BE REAL*8
!
      COMPLEX, ALLOCATABLE:: A(:)
!
!-----------------------------------------------------------------------
!
      IF( INTERP ) CALL MKW( INTERP,IND,NCON,WW )
      ALLOCATE( A(NCON) )
      DLAT = LAT
      IERR = 0
      DH = 0.D0
!
      K=1 !DO K=1,NTIME
!
         CALL NODAL( TIME_MJD,DLAT,PU,PF )
!        TO USE PHASE SHIFTS FROM CONSTIT, TIME SHOULD BE
!        IN SECONDS, RELATIVE TO JAN 1 1992 (48622MJD)
         TIME = (TIME_MJD-48622.D0)*SECONDSPERDAY
!        .TRUE. MEANS NO SOLID EARTH CORRECTION APPLIED IN MAKE_A
         CALL MAKE_A(.FALSE.,IND,NCON,TIME,PU,PF,WW,A,.TRUE.)
         PTIDE = HEIGHT(A,Z1,NCON)
         IF( INTERP )
     &      CALL INFER_MINOR( Z1,CID,NCON,TIME_MJD,DH,IERR )
         IF( IERR.EQ.-1 ) THEN
            IF(LNG.EQ.1) WRITE(LU,*) 'PAS ASSEZ DE COMPOSANTES' //
     &        ' POUR EN DEDUIRE LES COMPOSANTES MINEURES : IGNORE'
            IF(LNG.EQ.2) WRITE(LU,*) 'NOT ENOUGH CONSTITUENTS FOR' //
     &        ' INFERENCE OF MINOR CONSTITUENTS: IGNORED'
            INTERP=.FALSE.
         ENDIF
!        ADDS MINOR CONSTITUENTS
         PTIDE = PTIDE + DH
!
      !ENDDO
!
      DEALLOCATE(A)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END FUNCTION PTIDE

c$$$!              ************************************
c$$$               DOUBLE PRECISION FUNCTION TPXO_PTIDE
c$$$!              ************************************
c$$$!
c$$$     &( IV,ITFR,LAT,TIME_MJD,INTERP )
c$$$!
c$$$!***********************************************************************
c$$$! TELEMAC2D   V6P2                                   16/01/2012
c$$$!***********************************************************************
c$$$!
c$$$!brief
c$$$!
c$$$!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c$$$!|                |-->|
c$$$!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c$$$!
c$$$      IMPLICIT NONE
c$$$!
c$$$!-----------------------------------------------------------------------
c$$$!
c$$$      INTEGER IV,ITFR
c$$$      LOGICAL INTERP
c$$$      DOUBLE PRECISION LAT, TIME_MJD
c$$$!
c$$$!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
c$$$!
c$$$      TPXO_PTIDE = PTIDE( TPXO_BOR(IV,ITFR,:),
c$$$     &                    C_ID,NCON,CCIND,LAT,TIME_MJD,INTERP )
c$$$!
c$$$!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
c$$$!
c$$$      RETURN
c$$$      END FUNCTION TPXO_PTIDE
c$$$
!                    **************
                     SUBROUTINE MKW
!                    **************
!
     &( INTERP,IND,NC,WR )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER NC
      INTEGER IND(NC)
      DOUBLE PRECISION WR(TPXO_NCON,8)
      LOGICAL INTERP
!
!-----------------------------------------------------------------------
!
      INTEGER I,J
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO J=1,TPXO_NCON
         DO I=1,8
            WR(J,I) = TPXO_W(J,I)
         ENDDO
      ENDDO
!
      IF( .NOT.INTERP ) THEN
         DO J=1,NC
            IF( IND(J).NE.0 ) THEN
               DO I=1,8
                  WR(IND(J),I) = 0.D0
               ENDDO
            ENDIF
         ENDDO
      ENDIF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END SUBROUTINE MKW
!                    ****************
                     SUBROUTINE NODAL
!                    ****************
!
     &( DTIME,LATITUDE,PU,PF )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief
!
!note     "NO1" IN CONSTIT CORRESPONDS TO "M1" IN ARGUMENTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION DTIME,LATITUDE,PU(*),PF(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER INDEX(TPXO_NCMX),I
      DOUBLE PRECISION ARG(53),F(53),U(53)
      DOUBLE PRECISION, PARAMETER :: DTR = ATAN(1.D0)/45.D0
!
!     INDEX GIVES CORRESPONDENCE BETWEEN CONSTIT AND RICHARD'S SUBROUTINES
!     IN CONSTIT   M2,S2,K1,O1,N2,P1,K2,q1,2N2,mu2,nu2,L2,t2,
!                  J1,M1(no1),OO1,rho1,Mf,Mm,SSA,M4,
!                  MS4,MN4,M6,M8,MK3,S6,2SM2,2MK3
      DATA INDEX/30,35,19,12,27,17,37,10,25,26,28,33,34,
     &           23,14,24,11,5,3,2,45,46,44,50,0,42,51,40,0/
!
!------------------------------------------------------------------------
!
!     F, U - SAME AS PF, PU IN OLD NODAL.F; ARG IS NOT NEEDED;
!     DTIME - MJD
      CALL ARGUMENTS( DTIME,ARG,F,U )
      DO I=1,TPXO_NCMX
         PU(I) = 0.D0
         PF(I) = 1.D0
      ENDDO
      DO I = 1,TPXO_NCMX
         IF( INDEX(I).GT.0 ) THEN
!          U IS RETURNED BY "ARGUMENTS" IN DEGREES
           PU(I) = U(INDEX(I))*DTR
           PF(I) = F(INDEX(I))
         ENDIF
      ENDDO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END SUBROUTINE NODAL
!                    ********************
                     SUBROUTINE ARGUMENTS
!                    ********************
!
     &(TIME1,ARG,F,U)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    CALCULATES TIDAL ARGUMENTS
!+        KERNEL ROUTINE FOR SUBROUTINE HAT53
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION TIME1, ARG(*), F(*), U(*)
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION SHPN(4),S,H,P,OMEGA,PP,HOUR,T1,T2
      DOUBLE PRECISION TMP1,TMP2,TEMP1,TEMP2
      DOUBLE PRECISION COSN,COS2N,SINN,SIN2N,SIN3N
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,FIVE
      DOUBLE PRECISION NINETY
      DOUBLE PRECISION DTR,RTD
!
      PARAMETER   (DTR=ATAN(1.D0)/45.D0, RTD=45.D0/ATAN(1.D0))
      PARAMETER   (ZERO=0.D0, ONE=1.D0)
      PARAMETER   (TWO=2.D0, THREE=3.D0, FOUR=4.D0, FIVE=5.D0)
      PARAMETER   (NINETY=90.D0)
      PARAMETER   (PP=282.94D0)        ! SOLAR PERIGEE AT EPOCH 2000.
!
      EQUIVALENCE (SHPN(1),S),(SHPN(2),H),(SHPN(3),P),(SHPN(4),OMEGA)
!
      INTRINSIC COS,SIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DETERMINES EQUILIBRIUM ARGUMENTS
!     --------------------------------
      CALL ASTROL( TIME1, SHPN )
      HOUR = (TIME1 - INT(TIME1))*24.D0
      T1 = 15.D0*HOUR
      T2 = 30.D0*HOUR
      ARG( 1) = H - PP                                  ! Sa
      ARG( 2) = TWO*H                                   ! Ssa
      ARG( 3) = S - P                                   ! Mm
      ARG( 4) = TWO*S - TWO*H                           ! MSf
      ARG( 5) = TWO*S                                   ! Mf
      ARG( 6) = THREE*S - P                             ! Mt
      ARG( 7) = T1 - FIVE*S + THREE*H + P - NINETY      ! alpha1
      ARG( 8) = T1 - FOUR*S + H + TWO*P - NINETY        ! 2Q1
      ARG( 9) = T1 - FOUR*S + THREE*H - NINETY          ! sigma1
      ARG(10) = T1 - THREE*S + H + P - NINETY           ! q1
      ARG(11) = T1 - THREE*S + THREE*H - P - NINETY     ! rho1
      ARG(12) = T1 - TWO*S + H - NINETY                 ! o1
      ARG(13) = T1 - TWO*S + THREE*H + NINETY           ! tau1
      ARG(14) = T1 - S + H + NINETY                     ! M1
      ARG(15) = T1 - S + THREE*H - P + NINETY           ! chi1
      ARG(16) = T1 - TWO*H + PP - NINETY                ! pi1
      ARG(17) = T1 - H - NINETY                         ! p1
      ARG(18) = T1 + NINETY                             ! s1
      ARG(19) = T1 + H + NINETY                         ! k1
      ARG(20) = T1 + TWO*H - PP + NINETY                ! psi1
      ARG(21) = T1 + THREE*H + NINETY                   ! phi1
      ARG(22) = T1 + S - H + P + NINETY                 ! theta1
      ARG(23) = T1 + S + H - P + NINETY                 ! J1
      ARG(24) = T1 + TWO*S + H + NINETY                 ! OO1
      ARG(25) = T2 - FOUR*S + TWO*H + TWO*P             ! 2N2
      ARG(26) = T2 - FOUR*S + FOUR*H                    ! mu2
      ARG(27) = T2 - THREE*S + TWO*H + P                ! n2
      ARG(28) = T2 - THREE*S + FOUR*H - P               ! nu2
      ARG(29) = T2 - TWO*S + H + PP                     ! M2a
      ARG(30) = T2 - TWO*S + TWO*H                      ! M2
      ARG(31) = T2 - TWO*S + THREE*H - PP               ! M2b
      ARG(32) = T2 - S + P + 180.D0                     ! lambda2
      ARG(33) = T2 - S + TWO*H - P + 180.D0             ! L2
      ARG(34) = T2 - H + PP                             ! t2
      ARG(35) = T2                                      ! S2
      ARG(36) = T2 + H - PP + 180.D0                    ! R2
      ARG(37) = T2 + TWO*H                              ! K2
      ARG(38) = T2 + S + TWO*H - PP                     ! eta2
      ARG(39) = T2 - FIVE*S + FOUR*H + P                ! MNS2
      ARG(40) = T2 + TWO*S - TWO*H                      ! 2SM2
      ARG(41) = 1.5D0*ARG(30)                           ! M3
      ARG(42) = ARG(19) + ARG(30)                       ! MK3
      ARG(43) = THREE*T1                                ! S3
      ARG(44) = ARG(27) + ARG(30)                       ! MN4
      ARG(45) = TWO*ARG(30)                             ! M4
      ARG(46) = ARG(30) + ARG(35)                       ! MS4
      ARG(47) = ARG(30) + ARG(37)                       ! MK4
      ARG(48) = FOUR*T1                                 ! S4
      ARG(49) = FIVE*T1                                 ! S5
      ARG(50) = THREE*ARG(30)                           ! M6
      ARG(51) = THREE*T2                                ! S6
      ARG(52) = 7.D0*T1                                 ! S7
      ARG(53) = FOUR*T2                                 ! S8
!
!-----------------------------------------------------------------------
!
!     DETERMINES NODAL CORRECTIONS F AND U
!     ------------------------------------
      SINN = SIN(OMEGA*DTR)
      COSN = COS(OMEGA*DTR)
      SIN2N = SIN(TWO*OMEGA*DTR)
      COS2N = COS(TWO*OMEGA*DTR)
      SIN3N = SIN(THREE*OMEGA*DTR)
      F( 1) = ONE                                         ! Sa
      F( 2) = ONE                                         ! Ssa
      F( 3) = ONE - 0.130D0*COSN                          ! Mm
      F( 4) = ONE                                         ! MSf
      F( 5) = 1.043D0 + 0.414D0*COSN                      ! Mf
      F( 6) = SQRT((ONE+.203D0*COSN+.040D0*COS2N)**2 +
     &              (.203D0*SINN+.040D0*SIN2N)**2)        ! Mt

      F( 7) = ONE                                         ! alpha1
      F( 8) = SQRT((ONE+.188D0*COSN)**2+(.188D0*SINN)**2) ! 2Q1
      F( 9) = F(8)                                        ! sigma1
      F(10) = F(8)                                        ! q1
      F(11) = F(8)                                        ! rho1
      F(12) = SQRT((ONE+0.189D0*COSN-0.0058D0*COS2N)**2 +
     &             (0.189D0*SINN-0.0058D0*SIN2N)**2)      ! O1
      F(13) = ONE                                         ! tau1
!     TMP1  = 2.D0*COS(P*DTR)+.4D0*COS((P-OMEGA)*DTR)     ! Doodson's
!     TMP2  = SIN(P*DTR)+.2D0*SIN((P-OMEGA)*DTR)
      TMP1  = 1.36D0*COS(P*DTR)+.267D0*COS((P-OMEGA)*DTR) ! Ray's
      TMP2  = 0.64D0*SIN(P*DTR)+.135D0*SIN((P-OMEGA)*DTR)
      F(14) = SQRT(TMP1**2 + TMP2**2)                     ! M1
      F(15) = SQRT((ONE+.221D0*COSN)**2+(.221D0*SINN)**2) ! chi1
      F(16) = ONE                                         ! pi1
      F(17) = ONE                                         ! P1
      F(18) = ONE                                         ! S1
      F(19) = SQRT((ONE+.1158D0*COSN-.0029D0*COS2N)**2 +
     &             (.1554D0*SINN-.0029D0*SIN2N)**2)       ! K1
      F(20) = ONE                                         ! psi1
      F(21) = ONE                                         ! phi1
      F(22) = ONE                                         ! theta1
      F(23) = SQRT((ONE+.169D0*COSN)**2+(.227D0*SINN)**2) ! J1
      F(24) = SQRT((ONE+0.640D0*COSN+0.134D0*COS2N)**2 +
     &             (0.640D0*SINN+0.134D0*SIN2N)**2 )      ! OO1
      F(25) = SQRT((ONE-.03731D0*COSN+.00052D0*COS2N)**2 +
     &             (.03731D0*SINN-.00052D0*SIN2N)**2)     ! 2N2
      F(26) = F(25)                                       ! mu2
      F(27) = F(25)                                       ! N2
      F(28) = F(25)                                       ! nu2
      F(29) = ONE                                         ! M2a
      F(30) = F(25)                                       ! M2
      F(31) = ONE                                         ! M2b
      F(32) = ONE                                         ! lambda2
      TEMP1 = ONE-0.25D0*COS(TWO*P*DTR)
     &        -0.11D0*COS((TWO*P-OMEGA)*DTR)-0.04D0*COSN
      TEMP2 = 0.25D0*SIN(TWO*P)+0.11D0*SIN((TWO*P-OMEGA)*DTR)
     &        + 0.04D0*SINN
      F(33) = SQRT(TEMP1**2 + TEMP2**2)                   ! L2
      F(34) = ONE                                         ! t2
      F(35) = ONE                                         ! S2
      F(36) = ONE                                         ! R2
      F(37) = SQRT((ONE+.2852D0*COSN+.0324D0*COS2N)**2 +
     &             (.3108D0*SINN+.0324D0*SIN2N)**2)       ! K2
      F(38) = SQRT((ONE+.436D0*COSN)**2+(.436D0*SINN)**2) ! eta2
      F(39) = F(30)**2                                    ! MNS2
      F(40) = F(30)                                       ! 2SM2
      F(41) = ONE   ! WRONG                               ! M3
      F(42) = F(19)*F(30)                                 ! MK3
      F(43) = ONE                                         ! S3
      F(44) = F(30)**2                                    ! MN4
      F(45) = F(44)                                       ! M4
      F(46) = F(30)                                       ! MS4 ! BUG IN TPXO F(46) SHOULD BE F(30) RATHER THAN F(44)
      F(47) = F(30)*F(37)                                 ! MK4
      F(48) = ONE                                         ! S4
      F(49) = ONE                                         ! S5
      F(50) = F(30)**3                                    ! M6
      F(51) = ONE                                         ! S6
      F(52) = ONE                                         ! S7
      F(53) = ONE                                         ! S8
!
!-----------------------------------------------------------------------
!
      U( 1) = ZERO                                         ! Sa
      U( 2) = ZERO                                         ! Ssa
      U( 3) = ZERO                                         ! Mm
      U( 4) = ZERO                                         ! MSf
      U( 5) = -23.7D0*SINN + 2.7D0*SIN2N - 0.4D0*SIN3N     ! Mf
      U( 6) = ATAN(-(.203D0*SINN+.040D0*SIN2N)/
     &              (ONE+.203D0*COSN+.040D0*COS2N))*RTD    ! Mt
      U( 7) = ZERO                                         ! alpha1
      U( 8) = ATAN(.189D0*SINN/(ONE+.189D0*COSN))*RTD      ! 2Q1
      U( 9) = U(8)                                         ! sigma1
      U(10) = U(8)                                         ! q1
      U(11) = U(8)                                         ! rho1
      U(12) = 10.8D0*SINN - 1.3D0*SIN2N + 0.2D0*SIN3N      ! O1
      U(13) = ZERO                                         ! tau1
      U(14) = ATAN2(TMP2,TMP1)*RTD                         ! M1
      U(15) = ATAN(-.221D0*SINN/(ONE+.221D0*COSN))*RTD     ! chi1
      U(16) = ZERO                                         ! pi1
      U(17) = ZERO                                         ! P1
      U(18) = ZERO                                         ! S1
      U(19) = ATAN((-.1554D0*SINN+.0029D0*SIN2N)/
     &             (ONE+.1158D0*COSN-.0029D0*COS2N))*RTD   ! K1
      U(20) = ZERO                                         ! psi1
      U(21) = ZERO                                         ! phi1
      U(22) = ZERO                                         ! theta1
      U(23) = ATAN(-.227D0*SINN/(ONE+.169D0*COSN))*RTD     ! J1
      U(24) = ATAN(-(.640D0*SINN+.134D0*SIN2N)/
     &             (ONE+.640D0*COSN+.134D0*COS2N))*RTD     ! OO1
      U(25) = ATAN((-.03731D0*SINN+.00052D0*SIN2N)/
     &             (ONE-.03731D0*COSN+.00052D0*COS2N))*RTD ! 2N2
      U(26) = U(25)                                        ! mu2
      U(27) = U(25)                                        ! N2
      U(28) = U(25)                                        ! nu2
      U(29) = ZERO                                         ! M2a
      U(30) = U(25)                                        ! M2
      U(31) = ZERO                                         ! M2b
      U(32) = ZERO                                         ! lambda2
      U(33) = ATAN(-TEMP2/TEMP1)*RTD                       ! L2
      U(34) = ZERO                                         ! t2
      U(35) = ZERO                                         ! S2
      U(36) = ZERO                                         ! R2
      U(37) = ATAN(-(.3108D0*SINN+.0324D0*SIN2N)/
     &             (ONE+.2852D0*COSN+.0324D0*COS2N))*RTD   ! K2
      U(38) = ATAN(-.436D0*SINN/(ONE+.436D0*COSN))*RTD     ! eta2
      U(39) = U(30)*TWO                                    ! MNS2
      U(40) = U(30)                                        ! 2SM2
      U(41) = 1.5D0*U(30)                                  ! M3
      U(42) = U(30) + U(19)                                ! MK3
      U(43) = ZERO                                         ! S3
      U(44) = U(30)*TWO                                    ! MN4
      U(45) = U(44)                                        ! M4
      U(46) = U(30)                                        ! MS4
      U(47) = U(30)+U(37)                                  ! MK4
      U(48) = ZERO                                         ! S4
      U(49) = ZERO                                         ! S5
      U(50) = U(30)*THREE                                  ! M6
      U(51) = ZERO                                         ! S6
      U(52) = ZERO                                         ! S7
      U(53) = ZERO                                         ! S8
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END SUBROUTINE ARGUMENTS

!                    *****************
                     SUBROUTINE MAKE_A
!                    *****************
!
     &( INTERP,IND,NC,TIME,PU,PF,W,A,L_SAL )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    COMPUTES A MATRIX ELEMENTS FOR ONE DATA POINT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER NC
      INTEGER IND(NC)
      LOGICAL INTERP, L_SAL
      DOUBLE PRECISION W(TPXO_NCON,8)
      DOUBLE PRECISION PU(*),PF(*)
      DOUBLE PRECISION TIME
      COMPLEX A(NC)
!
!-----------------------------------------------------------------------
!
      INTEGER I,J
      DOUBLE PRECISION OMEGA(TPXO_NCMX),PHASE(TPXO_NCMX)
      COMPLEX C(TPXO_NCMX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     IF L_SAL=.TRUE. - NO SOLID EARTH CORRECTION IS APPLIED
!     USING BETA_SE COEFFICIENTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF( .NOT.INTERP ) THEN
         DO J = 1,TPXO_NCMX
            OMEGA(J) = TPXO_OMEGA_D(J)
            PHASE(J) = TPXO_PHASE_MKB(J)
         ENDDO
         DO J = 1,NC
           I = IND(J)
           IF( I.NE.0 ) THEN
              C(J) = CMPLX( PF(I)*COS(OMEGA(I)*TIME+PHASE(I)+PU(I)),
     &                      PF(I)*SIN(OMEGA(I)*TIME+PHASE(I)+PU(I)))
            ENDIF
         ENDDO
!        REMOVE SOLID EARTH TIDE
         IF( .NOT.L_SAL ) THEN
            DO J = 1,NC
              A(J) = CMPLX(0.D0,0.D0)
              IF( IND(J).NE.0 ) A(J) = C(J)*TPXO_BETA_SE(IND(J))
            ENDDO
         ELSE
            DO J = 1,NC
              A(J) = C(J)
            ENDDO
         ENDIF
!
      ELSE
!     THIS IS THE CASE WHEN W FROM MODULE WEIGHTS IS USED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         DO I=1,TPXO_NCON
            OMEGA(I) = TPXO_OMEGA_D(I)
            PHASE(I) = TPXO_PHASE_MKB(I)
         ENDDO
!
         DO I=1,TPXO_NCON
            C(I) = CMPLX( PF(I)*COS(OMEGA(I)*TIME+PHASE(I)+PU(I)),
     &	                  PF(I)*SIN(OMEGA(I)*TIME+PHASE(I)+PU(I)))
         ENDDO
         A = CMPLX(0.D0,0.D0)
!
!        IND(J)=0 MEANS THE CONSTITUENT IS EXCLUDED
         DO I = 1,TPXO_NCON
            DO J = 1,NC
               IF( IND(J).NE.0 ) A(J) =
     &            A(J)+C(I)*TPXO_BETA_SE(I)*W(I,IND(J))
            ENDDO
         ENDDO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ENDIF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END SUBROUTINE MAKE_A

!                    **********************
                     SUBROUTINE INFER_MINOR
!                    **********************
!
     &( ZMAJ,CID,NCON,TIME,DH,IERR )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    RETURNS CORRECTION FOR THE 16 MINOR CONSTITUENTS
!+        LISTED IN SUBROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NCON
      INTEGER IERR             ! -1 IF NOT ENOUGH CONSTITUENTS
                               !    FOR INFERENCE
      CHARACTER(LEN=4) CID(NCON)  ! GIVEN CONSTITUENTS
      DOUBLE PRECISION TIME    ! TIME, MJD
      DOUBLE PRECISION DH      ! OUTPUT: CORRECTION AT GIVEN TIME
                               ! FOR 16 MINOR CONSTITUENTS
      COMPLEX ZMAJ(NCON)       ! HC FOR GIVEN CONSTITUENTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,NI,K
      DOUBLE PRECISION HOUR,T1,T2,SHPN(4),S,H,P,OMEGA
      DOUBLE PRECISION SINN,COSN,SIN2N,COS2N
      DOUBLE PRECISION U(18),F(18),ARG(18)
      DOUBLE PRECISION, PARAMETER:: DTR=ATAN(1.D0)/45.D0
      DOUBLE PRECISION, PARAMETER:: RTD=45.D0/ATAN(1.D0)
      DOUBLE PRECISION, PARAMETER:: PP=282.8D0
      COMPLEX ZMIN(18)
      COMPLEX Z8(8)
!
      EQUIVALENCE (SHPN(1),S),(SHPN(2),H),(SHPN(3),P),(SHPN(4),OMEGA)
!
      CHARACTER(LEN=4) CID8(8)      ! IN ORDER TO CORRESPOND RR COEFFICIENTS
      DATA CID8/'q1  ','o1  ','p1  ','k1  ',
     &          'n2  ','m2  ','s2  ','k2  '/
!
      INTRINSIC COS,SIN,SQRT,ATAN2
!
!-----------------------------------------------------------------------
!
!     RE-ORDER TO CORRESPOND TO CID8
      IERR = 0
      Z8 = CMPLX(0.D0,0.D0)
      NI = 0
      DO I = 1,8
         DO J = 1,NCON
            IF( CID(J).EQ.CID8(I) ) THEN
               Z8(I)=ZMAJ(J)
               IF( I.NE.3.AND.I.NE.8 ) NI = NI+1
            ENDIF
         ENDDO
      ENDDO
!
      IF( NI.LT.6 ) THEN
         IERR=-1 ! NOT ENOUGH CONSTITUENTS FOR INFERENCE
         RETURN
      ENDIF
!
      ZMIN(1)  = 0.263D0 *Z8(1) - 0.0252D0*Z8(2)  ! 2Q1
      ZMIN(2)  = 0.297D0 *Z8(1) - 0.0264D0*Z8(2)  ! sigma1
      ZMIN(3)  = 0.164D0 *Z8(1) + 0.0048D0*Z8(2)  ! rho1 +
      ZMIN(4)  = 0.0140D0*Z8(2) + 0.0101D0*Z8(4)  ! M1
      ZMIN(5)  = 0.0389D0*Z8(2) + 0.0282D0*Z8(4)  ! M1
      ZMIN(6)  = 0.0064D0*Z8(2) + 0.0060D0*Z8(4)  ! chi1
      ZMIN(7)  = 0.0030D0*Z8(2) + 0.0171D0*Z8(4)  ! pi1
      ZMIN(8)  =-0.0015D0*Z8(2) + 0.0152D0*Z8(4)  ! phi1
      ZMIN(9)  =-0.0065D0*Z8(2) + 0.0155D0*Z8(4)  ! theta1
      ZMIN(10) =-0.0389D0*Z8(2) + 0.0836D0*Z8(4)  ! J1 +
      ZMIN(11) =-0.0431D0*Z8(2) + 0.0613D0*Z8(4)  ! OO1 +
      ZMIN(12) = 0.264D0 *Z8(5) - 0.0253D0*Z8(6)  ! 2N2 +
      ZMIN(13) = 0.298D0 *Z8(5) - 0.0264D0*Z8(6)  ! mu2 +
      ZMIN(14) = 0.165D0 *Z8(5) + 0.00487D0*Z8(6) ! nu2 +
      ZMIN(15) = 0.0040D0*Z8(6) + 0.0074D0*Z8(7)  ! lambda2
      ZMIN(16) = 0.0131D0*Z8(6) + 0.0326D0*Z8(7)  ! L2 +
      ZMIN(17) = 0.0033D0*Z8(6) + 0.0082D0*Z8(7)  ! L2 +
      ZMIN(18) = 0.0585D0*Z8(7)                   ! t2 +
!
      HOUR = (TIME - INT(TIME))*24.D0
      T1 = 15.D0*HOUR
      T2 = 30.D0*HOUR
      CALL ASTROL( TIME, SHPN )
!
      ARG(1)  = T1 - 4.D0*S + H + 2.D0*P - 90.D0 ! 2Q1
      ARG(2)  = T1 - 4.D0*S + 3.D0*H - 90.D0     ! sigma1
      ARG(3)  = T1 - 3.D0*S + 3.D0*H - P - 90.D0 ! rho1
      ARG(4)  = T1 - S + H - P + 90.D0           ! M1
      ARG(5)  = T1 - S + H + P + 90.D0           ! M1
      ARG(6)  = T1 - S + 3.D0*H - P + 90.D0      ! chi1
      ARG(7)  = T1 - 2.D0*H + PP - 90.D0         ! pi1
      ARG(8)  = T1 + 3.D0*H + 90.D0              ! phi1
      ARG(9)  = T1 + S - H + P + 90.D0           ! theta1
      ARG(10) = T1 + S + H - P + 90.D0           ! J1
      ARG(11) = T1 + 2.D0*S + H + 90.D0          ! OO1
      ARG(12) = T2 - 4.D0*S + 2.D0*H + 2.D0*P    ! 2N2
      ARG(13) = T2 - 4.D0*S + 4.D0*H             ! mu2
      ARG(14) = T2 - 3.D0*S + 4.D0*H - P         ! nu2
      ARG(15) = T2 - S + P + 180.D0              ! lambda2
      ARG(16) = T2 - S + 2.D0*H - P + 180.D0     ! L2
      ARG(17) = T2 - S + 2.D0*H + P              ! L2
      ARG(18) = T2 - H + PP                      ! t2
!
!     DETERMINES NODAL CORRECTIONS F AND U
      SINN = SIN(OMEGA*DTR)
      COSN = COS(OMEGA*DTR)
      SIN2N = SIN(2.D0*OMEGA*DTR)
      COS2N = COS(2.D0*OMEGA*DTR)
!
      DO I = 1,18
        F(I) = 1.D0
      ENDDO
      F(1) = SQRT((1.D0 + 0.189D0*COSN - 0.0058D0*COS2N)**2 +
     &            (0.189D0*SINN - 0.0058D0*SIN2N)**2)
      F(2) = F(1)
      F(3) = F(1)
      F(4) = SQRT((1.D0 + 0.185D0*COSN)**2 + (0.185D0*SINN)**2)
      F(5) = SQRT((1.D0 + 0.201D0*COSN)**2 + (0.201D0*SINN)**2)
      F(6) = SQRT((1.D0 + 0.221D0*COSN)**2 + (0.221D0*SINN)**2)
      F(10) = SQRT((1.D0 + 0.198D0*COSN)**2 + (0.198D0*SINN)**2)
      F(11) = SQRT((1.D0 + 0.640D0*COSN + 0.134D0*COS2N)**2 +
     &             (0.640D0*SINN + 0.134D0*SIN2N)**2 )
      F(12) = SQRT((1.D0 - 0.0373D0*COSN)**2 + (0.0373D0*SINN)**2)
      F(13) = F(12)
      F(14) = F(12)
      F(16) = F(12)
      F(17) = SQRT((1.D0 + 0.441D0*COSN)**2 + (0.441D0*SINN)**2)
!
      DO I = 1,18
        U(I) = 0.D0
      ENDDO
      U(1) = ATAN2(0.189D0*SINN - 0.0058D0*SIN2N,
     &             1.D0 + 0.189D0*COSN - 0.0058D0*SIN2N)*RTD
      U(2) = U(1)
      U(3) = U(1)
      U(4) = ATAN2( 0.185D0*SINN, 1.D0 + 0.185D0*COSN)*RTD
      U(5) = ATAN2(-0.201D0*SINN, 1.D0 + 0.201D0*COSN)*RTD
      U(6) = ATAN2(-0.221D0*SINN, 1.D0 + 0.221D0*COSN)*RTD
      U(10) = ATAN2(-0.198D0*SINN, 1.D0 + 0.198D0*COSN)*RTD
      U(11) = ATAN2(-0.640D0*SINN - 0.134D0*SIN2N,
     &              1.D0 + 0.640D0*COSN + 0.134D0*COS2N)*RTD
      U(12) = ATAN2(-0.0373D0*SINN, 1.D0 - 0.0373D0*COSN)*RTD
      U(13) = U(12)
      U(14) = U(12)
      U(16) = U(12)
      U(17) = ATAN2(-0.441D0*SINN, 1.D0 + 0.441D0*COSN)*RTD
!
!     SUM OVER ALL TIDES
!     ------------------
      DH = 0.D0
      DO I = 1,18
!       NOTE JMH: DREAL AND DIMAG ARE NOT ACCEPTED BY NAG
!                 DON'T KNOW WHAT TO DO
        DH = DH + REAL(ZMIN(I))*F(I)*COS((ARG(I)+U(I))*DTR)-
     &           AIMAG(ZMIN(I))*F(I)*SIN((ARG(I)+U(I))*DTR)
      ENDDO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END SUBROUTINE INFER_MINOR


!                    *****************
                     SUBROUTINE ASTROL
!                    *****************
!
     &( TIME,SHPN )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    COMPUTES THE BASIC ASTRONOMICAL MEAN LONGITUDES  S, H, P, N
!+        NOTE N IS NOT N', I.E. N IS DECREASING WITH TIME.
!+        THESE FORMULAE ARE FOR THE PERIOD 1990 - 2010, AND WERE
!+        DERIVED BY DAVID CARTWRIGHT (PERSONAL COMM., NOV. 1990)
!+        TIME IS UTC IN DECIMAL MJD.
!+        ALL LONGITUDES RETURNED IN DEGREES.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION SHPN(4),TIME
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION CIRCLE,T
      PARAMETER ( CIRCLE=360.0D0 )
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     NON-VECTORIZED VERSION
!
      T = TIME - 51544.4993D0
!
!     MEAN LONGITUDE OF MOON
!     ----------------------
      SHPN(1) = 218.3164D0 + 13.17639648D0 * T
!
!     MEAN LONGITUDE OF SUN
!     ---------------------
      SHPN(2) = 280.4661D0 +  0.98564736D0 * T
!
!     MEAN LONGITUDE OF LUNAR PERIGEE
!     -------------------------------
      SHPN(3) =  83.3535D0 +  0.11140353D0 * T
!
!     MEAN LONGITUDE OF ASCENDING LUNAR NODE
!     --------------------------------------
      SHPN(4) = 125.0445D0 -  0.05295377D0 * T

      SHPN(1) = MOD(SHPN(1),CIRCLE)
      SHPN(2) = MOD(SHPN(2),CIRCLE)
      SHPN(3) = MOD(SHPN(3),CIRCLE)
      SHPN(4) = MOD(SHPN(4),CIRCLE)

      IF( SHPN(1).LT.0.D0 ) SHPN(1) = SHPN(1) + CIRCLE
      IF( SHPN(2).LT.0.D0 ) SHPN(2) = SHPN(2) + CIRCLE
      IF( SHPN(3).LT.0.D0 ) SHPN(3) = SHPN(3) + CIRCLE
      IF( SHPN(4).LT.0.D0 ) SHPN(4) = SHPN(4) + CIRCLE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END SUBROUTINE ASTROL
!                *************************
                 INTEGER FUNCTION DATE_MJD
!                *************************
!
     &( MM,ID,IYYY )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    CONVERTS DATE TO MJD (MODIFIED JULIAN DAYS)
!+  INPUT:  ID - DAY, MM - MONTH, IYYY - YEAR
!+  OUTPUT: MJD > 0 - MODIFIED JULIAN DAYS
!+  DATE >= 11.17.1858 CORRESPONDS TO MJD = 0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  MJD           |<--|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER MM,ID,IYYY
!
!-----------------------------------------------------------------------
!
      INTEGER DPM(12),DAYS,I,NLEAP,K
      DATA DPM/31,28,31,30,31,30,31,31,30,31,30,31/
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DATE_MJD = 0
!     NO EARLIER DATES
      IF( IYYY.LT.1858 ) IYYY = 1858
      IF( IYYY.EQ.1858.AND.MM.LT.11 ) MM = 11
      IF( IYYY.EQ.1858.AND.MM.EQ.11.AND.ID.LT.17 ) ID = 17
!
      DAYS = 0
      DO I = 1,MM-1
         DAYS = DAYS+DPM(I)
         IF( I.EQ.2.AND.INT(IYYY/4)*4.EQ.IYYY ) DAYS = DAYS+1
      ENDDO
      DAYS = DAYS+ID-321

!     LEAP DAY CORRECTION
      DO K = 1900,IYYY,100
         IF( K.EQ.IYYY.AND.MM.GT.2 ) DAYS = DAYS-1
      ENDDO
      DO K = 2000,IYYY,400
         IF( K.EQ.IYYY.AND.MM.GT.2 ) DAYS = DAYS+1
      ENDDO
!     EACH 4TH YEAR IS LEAP YEAR
      NLEAP = INT((IYYY-1-1860)*0.25)
      IF( IYYY.GT.1860 ) NLEAP = NLEAP+1
!     EXCEPT
      DO K = 1900,IYYY-1,100
        IF( K.LT.IYYY ) NLEAP = NLEAP-1
        IF( K.EQ.IYYY.AND.MM.GT.2 ) DAYS = DAYS-1
      ENDDO
!     BUT EACH IN THE ROW 2000:400:... IS LEAP YEAR AGAIN
      DO K = 2000,IYYY-1,400
        IF( IYYY.GT.K ) NLEAP = NLEAP+1
        IF( IYYY.EQ.K.AND.MM.GT.2 ) DAYS = DAYS+1
      ENDDO
      DATE_MJD = 365*(IYYY-1858)+NLEAP+DAYS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END FUNCTION DATE_MJD
!                    ***********************
                     SUBROUTINE ALLBORD_TPXO
!                    ***********************
!
     &(MESH,LIHBOR,LIUBOR,KENT,KENTU)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   06/12/2011
!***********************************************************************
!
!brief    Prepare a level boundary filter to store the TPXO constituents
!+        at the boundary. In particular,
!+        count NPTNFR and ALLOCATE and set the filter TPXO_NFR
!
!note     Passing MESH, LIHBOR and LIUBOR as arguments allows
!+        this SUBROUTINE to be called from TELEMAC-2D or TELEMAC-3D
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Implementation and generalised for interfacing with
!+        TELEMAC-2D AND 3D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  KENT          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!|  KENTU         |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!|  LIHBOR        |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!|                |-->| (KENT IS HERE OF INTEREST)
!|  LIUBOR        |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!|  MESH          |-->| 2D MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
      INTEGER, INTENT(IN) :: KENT,KENTU
      INTEGER, INTENT(IN) :: LIHBOR(*),LIUBOR(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER     K
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     PREPARE STORAGE ON LEVEL BOUNDARIES
!
      ALLOCATE( TPXO_NFR(MESH%NPOIN) )
      DO K=1,MESH%NPOIN
         TPXO_NFR(K) = 0
      ENDDO
      NPTNFR = 0
      DO K = 1,MESH%NPTFR
         IF( LIHBOR(K).EQ.KENT.OR.LIUBOR(K).EQ.KENTU ) THEN
            NPTNFR = NPTNFR + 1
            TPXO_NFR(MESH%NBOR%I(K)) = NPTNFR
         ENDIF
      ENDDO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      END SUBROUTINE ALLBORD_TPXO
!                    *********************
                     SUBROUTINE CONDI_TPXO
!                    *********************
!
     &(NPOIN,X,Y,H,U,V,LAMBD0,PHI0,T2D_FILES,T2DBB1,T2DBB2,
     & MARDAT,MARTIM)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   06/12/2011
!***********************************************************************
!
!brief
!
!note     Passing NPOIN, X,Y and H as arguments allows this SUBROUTINE
!+        to be called from TELEMAC-2D or TELEMAC-3D
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Generalised for interfacing with TELEMAC-2D AND 3D
!
!warning  (-ZF) should be stored in H as you enter this subroutine
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  NPOIN         |-->| NUMBER OF 2D NODES IN THE MESH
!|  X,Y           |-->| COORDINATES X AND Y OF THE NODES OF THE MESH
!|  H             |<->| COMES IN AS -ZF, TO WHICH THE TPXO FREE SURFACE
!|                |   | WILL BE ADDED TO PRODUCE WATER DEPTH
!|  MARDAT        |-->| DATE (YEAR, MONTH,DAY)
!|  MARTIM        |-->| TIME (HOUR, MINUTE,SECOND)
!|  PHI0          |-->| 
!|  LAMBD0        |-->| 
!|  T2DBB1        |-->| ADDRESS OF DATA BASE 1 IN T2D_FILES
!|  T2DBB2        |-->| ADDRESS OF DATA BASE 2 IN T2D_FILES
!|  T2D_FILES     |-->| ARRAY OF FILES
!|  U,V           |<--| 2D DEPTH-AVERAGED VELOCITY COMPONENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,T2DBB1,T2DBB2
      INTEGER, INTENT(IN)             :: MARDAT(3),MARTIM(3)
      DOUBLE PRECISION, INTENT(IN)    :: LAMBD0,PHI0
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN)
      TYPE(BIEF_FILE), INTENT(IN)     :: T2D_FILES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC,I,J,K,IPOIN,IERR,NC,N,M
      INTEGER, ALLOCATABLE :: CIND(:)
      INTEGER, ALLOCATABLE :: MASKT(:,:),MASKU(:,:),MASKV(:,:)
      DOUBLE PRECISION, PARAMETER :: RADIUS = 6371000.D0
      DOUBLE PRECISION, PARAMETER :: PI = 4.D0*ATAN(1.D0)
      DOUBLE PRECISION, PARAMETER :: DTR = PI/180.D0
      DOUBLE PRECISION, PARAMETER :: RTD = 180.D0/PI
      DOUBLE PRECISION LAT,LON,SPD
      DOUBLE PRECISION LAT0,LONG0,CONST
      REAL PH_LIM(2),TH_LIM(2)
      COMPLEX, ALLOCATABLE :: ZT(:,:,:)
      COMPLEX, ALLOCATABLE :: UT(:,:,:), VT(:,:,:)
      COMPLEX, ALLOCATABLE :: UV(:,:,:), ZCON(:)
      CHARACTER(LEN=4) C_ID_MOD(TPXO_NCMX)
!
!     N,M: SIZES OF THE GRID SUPPORTING THE TPXO MODEL
!     NC: NUMBER OF CONSTITUENTS AVAILABLE IN THE FILE
!     MASKT,MASKU,MASKV MASKS TO FILTER VALID AND INVALID (U,V,H) VALUES
!     RADIUS: RADIUS OF THE EARTH
!     LAT0,LONG0,CONST: REFERENCE LATITUDE AND LONGITUDE
!     PH_LIM,TH_LIM: MIN AND MAX RANGES FOR PHASES AND PERIODES
!     ZT,UT,VT,UV,ZCON: PHASES AND PERIODS FOR U, V AND H
!     HERE DEFINED AS COMPLEX
!     C_ID_MOD INDICES OF AVAILABLE CONTITUENTS AMONGST THE ALL POSSIBLE
!
      INTRINSIC TAN,ATAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     USER AND CONVERTION CONSTANTS
!
      LAT0 = LAMBD0 * DTR
      LONG0 = PHI0 * DTR
      CONST = TAN( 0.5D0*LAT0 + 0.25D0*PI )
!
      I_MICON = .FALSE.
!
!-----------------------------------------------------------------------
!
      IF(T2D_FILES(T2DBB1)%NAME(1:1).EQ.' ' .OR.
     &   T2D_FILES(T2DBB2)%NAME(1:1).EQ.' ') THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'FICHIERS TPXO NON DISPONIBLES'
        IF(LNG.EQ.2) WRITE(LU,*) 'TPXO FILES NOT AVAILABLE'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,*) 'INITIALISATION BASEE SUR TPXO :'
      IF(LNG.EQ.2) WRITE(LU,*) 'INITIALISATION BASED ON TPXO:'
!
!-----------------------------------------------------------------------
!
!     READ AVAILABLE DIMENSIONS
!
      REWIND(T2D_FILES(T2DBB1)%LU)
      READ(T2D_FILES(T2DBB1)%LU) N,M,NC,TH_LIM,PH_LIM,C_ID_MOD(1:NC)
!
!-----------------------------------------------------------------------
!
!     GET ALL AVAILABLE CONSTITUENTS AND SET THEIR INDICES
!
      NCON = NC
      DO IC = 1,NC
        C_ID(IC) = C_ID_MOD(IC)
      ENDDO
      ALLOCATE( CIND(NCON), CCIND(NCON) )
      CALL DEF_CON_IND( C_ID,NCON,C_ID_MOD,NC,CIND )
      CALL DEF_CID( NCON,C_ID,CCIND )
!
!-----------------------------------------------------------------------
!
!     PREPARE STORAGE ON LEVEL BOUNDARIES
!
      ALLOCATE( TPXO_BOR(3,NPTNFR,NCON) )
      ALLOCATE( TPXO_LATFR(NPTNFR) )
      DO K=1,NCON
         DO J=1,NPTNFR
            DO I=1,3
               TPXO_BOR(I,J,K) = CMPLX(0.D0,0.D0)
            ENDDO
         ENDDO
      ENDDO
!     AJOUTE PAR JMH 03/04/2012, A VOIR SEB ?
      DO J=1,NPTNFR
         TPXO_LATFR(J) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
      STIME_MJD = DATE_MJD( MARDAT(2),MARDAT(3),MARDAT(1) ) +
     &            MARTIM(1)/24.D0+MARTIM(2)/1440.D0+MARTIM(3)/86400.D0
!
!-----------------------------------------------------------------------
!
!     GET DATA FROM THE H-FILE
!
      IF(LNG.EQ.1) WRITE(LU,*) ' - OBTENTION DES NIVEAUX'
      IF(LNG.EQ.2) WRITE(LU,*) ' - ACQUIRING LEVELS'
!
      ALLOCATE( ZT(NCON,N,M), MASKT(N,M) )
      DO J=1,M
         DO I=1,N
            MASKT(I,J) = 0
         ENDDO
      ENDDO
!
      DO IC = 1,NCON
         REWIND(T2D_FILES(T2DBB1)%LU)
         READ(T2D_FILES(T2DBB1)%LU)  ! HEADER LINE
         DO K = 1,CIND(IC)-1
            READ(T2D_FILES(T2DBB1)%LU)
         ENDDO
         READ(T2D_FILES(T2DBB1)%LU) ( ( ZT(IC,I,J), I=1,N ), J=1,M )
         WHERE( ZT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKT = 1
      ENDDO
!
!     INTERPOLATE TPXO IN SPACE
!
      IF(LNG.EQ.1) WRITE(LU,*) ' - INTERPOLATION DES NIVEAUX'
      IF(LNG.EQ.2) WRITE(LU,*) ' - INTERPOLATING LEVELS'
!
      ALLOCATE( ZCON(NCON) )
      DO IPOIN = 1,NPOIN
!
         LON = RTD*( X(IPOIN)/RADIUS + LONG0 )
         LAT = RTD*
     &      ( 2.D0*ATAN(CONST*EXP(Y(IPOIN)/RADIUS)) - 0.5D0*PI )
         IF( LON.GT.PH_LIM(2) ) LON = LON - 360.D0
         IF( LON.LT.PH_LIM(1) ) LON = LON + 360.D0
!
         CALL INTERPT( ZT,NCON,N,M,MASKT,
     &                TH_LIM,PH_LIM,LAT,LON,ZCON,IERR,'z' )
         IF( IERR.EQ.0 ) H(IPOIN) = H(IPOIN) +
     &        PTIDE( ZCON,C_ID,NCON,CCIND,LAT,STIME_MJD,I_MICON )
         IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
               TPXO_BOR(1,TPXO_NFR(IPOIN),K) = ZCON(K)
            ENDDO
         ENDIF
!###> MST@HRW: CHECKING DRY LANDS
!         IF(H(IPOIN).LT.0.D0) H(IPOIN) = 0.D0
         H(IPOIN) = MAX(H(IPOIN),0.D0)
!###< MST@HRW
!
      ENDDO
      DEALLOCATE( ZCON,ZT,MASKT )
!
!-----------------------------------------------------------------------
!
!     GET DATA FROM THE U-FILE
!
      IF(LNG.EQ.1) WRITE(LU,*) ' - OBTENTION DES VITESSES'
      IF(LNG.EQ.2) WRITE(LU,*) ' - ACQUIRING VELOCITIES'
!
      ALLOCATE( UT(NCON,N,M),VT(NCON,N,M),MASKU(N,M),MASKV(N,M) )
      DO J=1,M
         DO I=1,N
            MASKU(I,J) = 0
            MASKV(I,J) = 0
         ENDDO
      ENDDO
!
      ALLOCATE( UV(2,N,M) )
      DO IC = 1,NCON
         REWIND(T2D_FILES(T2DBB2)%LU)
         READ(T2D_FILES(T2DBB2)%LU)  ! HEADER LINE
         DO K = 1,CIND(IC)-1
            READ(T2D_FILES(T2DBB2)%LU)
         ENDDO
         READ(T2D_FILES(T2DBB2)%LU) UV
         UT(IC,:,:) = UV(1,:,:)
         VT(IC,:,:) = UV(2,:,:)
         WHERE( UT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKU = 1
         WHERE( VT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKV = 1
      ENDDO
      DEALLOCATE( UV )
!
!     INTERPOLATE TPXO IN SPACE
!
      IF(LNG.EQ.1) WRITE(LU,*) ' - INTERPOLATION DES VITESSES'
      IF(LNG.EQ.2) WRITE(LU,*) ' - INTERPOLATING VELOCITIES'
!
      ALLOCATE( ZCON(NCON) )
      DO IPOIN = 1,NPOIN
!
         LON = RTD*( X(IPOIN)/RADIUS + LONG0 )
         LAT = RTD*
     &      ( 2.D0*ATAN(CONST*EXP(Y(IPOIN)/RADIUS)) - 0.5D0*PI )
         IF( LON.GT.PH_LIM(2) ) LON = LON - 360.D0
         IF( LON.LT.PH_LIM(1) ) LON = LON + 360.D0
!
         CALL INTERPT(UT,NCON,N,M,MASKU,
     &               TH_LIM,PH_LIM,LAT,LON,ZCON,IERR,'u')
         IF( IERR.EQ.0 ) U(IPOIN) =
     &        PTIDE( ZCON,C_ID,NCON,CCIND,LAT,STIME_MJD,I_MICON )
         IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
               TPXO_BOR(2,TPXO_NFR(IPOIN),K) = ZCON(K)
            ENDDO
          ENDIF
!
         CALL INTERPT(VT,NCON,N,M,MASKV,
     &               TH_LIM,PH_LIM,LAT,LON,ZCON,IERR,'v')
         IF( IERR.EQ.0 ) V(IPOIN) =
     &        PTIDE( ZCON,C_ID,NCON,CCIND,LAT,STIME_MJD,I_MICON )
         IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
               TPXO_BOR(3,TPXO_NFR(IPOIN),K) = ZCON(K)
            ENDDO
         ENDIF
!
!###> MST@HRW: ADDING A CAP AT 2 M/S FOR STABILITY ON DRY LANDS
         SPD = SQRT(U(IPOIN)**2+V(IPOIN)**2)
         IF(SPD.GT.2.D0) THEN
           U(IPOIN) = 2.D0*U(IPOIN)/SPD
           V(IPOIN) = 2.D0*V(IPOIN)/SPD
         ENDIF
!###< MST@HRW
!
      ENDDO
      DEALLOCATE( UT,VT,ZCON,MASKU,MASKV )
!
      IF(LNG.EQ.1) WRITE(LU,*) 'FIN DE L''INITIALISATION TPXO'
      IF(LNG.EQ.2) WRITE(LU,*) 'END OF TPXO INITIALISATION'
      DEALLOCATE( CIND ) ! NOW KEPT IN MEMORY FOR PTIDE =>, CCIND )
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CONDI_TPXO
!              *********************************
               DOUBLE PRECISION FUNCTION SL_TPXO
!              *********************************
!
     &(KFR,TEMPS)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   06/12/2011
!***********************************************************************
!
!brief    Prescribes the free surface elevation based on the TPXO tidal
!+        model, for level imposed liquid boundary
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Implementation for interfacing with TELEMAC-2D AND 3D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  KFR           |-->| LEVEL BOUNDARY GLOBAL NODE NUMBER
!|  TEMPS         |-->| TIME IS SECONDS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: KFR
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SL_TPXO = 0.D0
      IF( TPXO_NFR(KFR).NE.0 ) THEN
!
!        TPXO_BOR(1,,) IS WATER LEVEL
         SL_TPXO = PTIDE( TPXO_BOR(1,TPXO_NFR(KFR),:),
     &         C_ID, NCON, CCIND, TPXO_LATFR(TPXO_NFR(KFR)),
     &        ( STIME_MJD+TEMPS/86400.D0 ), I_MICON )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      END FUNCTION SL_TPXO
!               ***********************************
                DOUBLE PRECISION FUNCTION VITU_TPXO
!               ***********************************
!
     &( KFR,TEMPS )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   06/12/2011
!***********************************************************************
!
!brief    Prescribes the u-component of the velocity based on the
!+        TPXO tidal model, for velcoity imposed liquid boundary
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Implementation for interfacing with TELEMAC-2D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  KFR           |-->| VELOCITY BOUNDARY GLOBAL NODE NUMBER
!|  TEMPS         |-->| TIME IS SECONDS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: KFR
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      VITU_TPXO = 0.D0
      IF( TPXO_NFR(KFR).NE.0 ) THEN
!
         ! TPXO_BOR(2,,) IS U-VELOCITY COMPONENT
         VITU_TPXO = PTIDE( TPXO_BOR(2,TPXO_NFR(KFR),:),
     &         C_ID, NCON, CCIND, TPXO_LATFR(TPXO_NFR(KFR)),
     &        ( STIME_MJD+TEMPS/86400.D0 ), I_MICON )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      END FUNCTION VITU_TPXO
!               ***********************************
                DOUBLE PRECISION FUNCTION VITV_TPXO
!               ***********************************
!
     &( KFR,TEMPS )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   06/12/2011
!***********************************************************************
!
!brief    Prescribes the v-component of the velocity based on the
!+        TPXO tidal model, for velcoity imposed liquid boundary
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Implementation for interfacing with TELEMAC-2D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  KFR           |-->| VELOCITY BOUNDARY GLOBAL NODE NUMBER
!|  TEMPS         |-->| TIME IS SECONDS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: KFR
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      VITV_TPXO = 0.D0
      IF( TPXO_NFR(KFR).NE.0 ) THEN
!
!        TPXO_BOR(3,,) IS V-VELOCITY COMPONENT
         VITV_TPXO = PTIDE( TPXO_BOR(3,TPXO_NFR(KFR),:),
     &         C_ID, NCON, CCIND, TPXO_LATFR(TPXO_NFR(KFR)),
     &        ( STIME_MJD+TEMPS/86400.D0 ), I_MICON )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      END FUNCTION VITV_TPXO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE TPXO
