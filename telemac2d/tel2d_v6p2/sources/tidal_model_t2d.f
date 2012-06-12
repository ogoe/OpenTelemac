!                    **************************
                     SUBROUTINE TIDAL_MODEL_T2D
!                    **************************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   28/10/2010
!***********************************************************************
!
!brief    FINDS TIDAL BOUNDARY CONDITIONS AT THE OPEN SEA BOUNDARIES
!+
!
!history  C-T PHAM (LNHE)
!+        30/05/2011
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE TPXO
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,NODALCORR,ICALHW
      DOUBLE PRECISION XSHIFT,YSHIFT
      LOGICAL TIDALBCGEN
!
!-----------------------------------------------------------------------
!
!     PARAMETERS FOR TIDAL BOUNDARY CONDITIONS
!
!     CTIDEV: COEFFICIENT TO CALIBRATE THE VELOCITIES, DEFAULT = SQRT(CTIDE)
      IF(CTIDEV.EQ.-999.D0) CTIDEV = SQRT(CTIDE)
!
!     TIDALBCGEN: LOGICAL FOR GENERATION OF TIDAL BOUNDARY CONDITIONS OR NOT
!                 CURRENTLY WORKS ONLY FOR SCALAR COMPUTATIONS
!                 FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      TIDALBCGEN = .FALSE.
!
!     NODALCORR (OPTION FOR CALCULATION OF NODAL FACTOR CORRECTION)
!             IN SUBROUTINE BORD_TIDE
!             DEFAULT = 1 CURRENTLY (FROZEN AT THE DATE MARDAT)
!     IN THE STEERING FILE, THE KEYWORD 'ORIGINAL DATE OF TIME' HAVE TO BE SET
!     WARNING, FORMAT: YEAR, MONTH, DAY
!
!     OPTION FOR CALCULATION OF NODAL FACTOR CORRECTION F
!     0: NOT FROZEN, COMPUTED AT EACH TIME STEP
!     1: FROZEN WITH VALUE AT THE BEGINNING OF THE SIMULATION
!     2: FROZEN WITH VALUE AT THE MIDDLE OF THE YEAR OF THE SIMULATION
!     (SINGLE POSSIBILITY FOR TIDALTYPE = 7)
!
      NODALCORR = 1
      IF(TIDALTYPE.EQ.7) NODALCORR = 2
!
!     ICALHW: NUMBER THAT MAY BE CHOSEN BY THE USER TO CALIBRATE HIGH WATER
!             OR AUTOMATICALLY CHOSEN, IN CASE OF THE MODELLING OF A SCHEMATIC TIDE
!             IN SUBROUTINE BORD_TIDE
!             DEFAULT = 0 (AUTOMATICALLY CHOSEN)
!
      ICALHW = 0
!
!     OPTIONAL SHIFT OF COORDINATES
!
      XSHIFT = 0.D0
      YSHIFT = 0.D0
!
!     FILES:
!
!     T2DBDD: TIDE DATA BASE
!     T2DHAR: HARMONIC CONSTANTS FILE
!     T2DTID: TIDAL MODEL FILE
!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
      IF(TIDALDB.EQ.1) THEN
        IF(TIDALBCGEN) THEN
          CALL BORD_TIDAL_BC(MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                       NPTFR,KENT,KENTU,
     &                       MESH,GEOSYST,NUMZONE,TIDALTYPE,
     &                       BOUNDARY_COLOUR,MAXFRO,
     &                       T2D_FILES(T2DBDD)%LU,
     &                       T2D_FILES(T2DTID)%LU,
     &                       T2D_FILES(T2DHAR)%LU,XSHIFT,YSHIFT)
        ENDIF
!
        CALL BORD_TIDE(ZF%R,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                 NPOIN,NPTFR,AT,NCOTE,NVITES,
     &                 NUMLIQ%I,KENT,KENTU,
     &                 T2D_FILES(T2DIMP)%NAME,TIDALTYPE,
     &                 CTIDE,MSL,NODALCORR,T2D_FILES(T2DHAR)%LU,
     &                 BOUNDARY_COLOUR,
     &                 HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     &                 MARDAT,MARTIM)
      ELSEIF(TIDALDB.EQ.2) THEN
        CALL BORD_TIDE_TPXO(ZF%R,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                      NPOIN,NPTFR,AT,NCOTE,NVITES,
     &                      NUMLIQ%I,KENT,KENTU,MESH,
     &                      T2D_FILES(T2DIMP)%NAME,TIDALTYPE,
     &                      CTIDE,MSL,
     &                      CTIDEV,NODALCORR,T2D_FILES(T2DFO1)%LU,
     &                      BOUNDARY_COLOUR,
     &                      HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     &                      MARDAT,MARTIM,
     &                      T2D_FILES,T2DBB1,T2DBB2,
     &                      X,Y,GEOSYST,NUMZONE,LAMBD0,PHI0,INTMICON)
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO K=1,NPTFR
        IF(NUMTIDE%I(K).GT.0) THEN
!         POSSIBLE SMOOTHING AT THE BEGINNING
!         IF(AT.LT.1800.D0) THEN
!           UBTIDE%R(K) = UBTIDE%R(K)*(AT/1800.D0)
!           VBTIDE%R(K) = VBTIDE%R(K)*(AT/1800.D0)
!         ENDIF
          IF(LIUBOR%I(K).EQ.KENTU) THEN
            UBOR%R(K) = UBTIDE%R(K)
            VBOR%R(K) = VBTIDE%R(K)
            U%R(MESH%NBOR%I(K)) = UBOR%R(K)
            V%R(MESH%NBOR%I(K)) = VBOR%R(K)
          ENDIF
          IF(LIHBOR%I(K).EQ.KENT) THEN
            HBOR%R(K) = HBTIDE%R(K)
            H%R(MESH%NBOR%I(K)) = HBOR%R(K)
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
