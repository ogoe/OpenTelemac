!                    **************************
                     SUBROUTINE TIDAL_MODEL_T3D
!                    **************************
!
!
!***********************************************************************
! TELEMAC3D   V6P2                                   27/09/2011
!***********************************************************************
!
!brief    FINDS TIDAL BOUNDARY CONDITIONS AT THE OPEN SEA BOUNDARIES
!+
!
!history  C-T PHAM (LNHE)
!+        27/09/2011
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE INTERFACE_TELEMAC2D
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
      INTEGER K,NODALCORR,ICALHW,NP,IBORD
      DOUBLE PRECISION XSHIFT,YSHIFT
      LOGICAL TIDALBCGEN
!
!-----------------------------------------------------------------------
!
!     PARAMETERS FOR TIDAL BOUNDARY CONDITIONS
!
!     CTIDEV: COEFFICIENT TO CALIBRATE THE VELOCITIES, DEFAULT = SQRT(CTIDE)
!     ACCORDING TO DV, IF A CORRECTION COEFFICIENT CTIDE IS APPLIED
!     FOR WATER DEPTHS, ANOTHER ONE MUST BE APPLIED FOR VELOCITIES
!     = SQRT(CTIDE)
!
      IF(CTIDEV.EQ.999999.D0) CTIDEV = SQRT(CTIDE)
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
!             FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      ICALHW = 0
!
!     OPTIONAL SHIFT OF COORDINATES
!     FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      XSHIFT = 0.D0
      YSHIFT = 0.D0
!
!     FILES:
!
!     T3DBDD: TIDE DATA BASE
!     T3DHAR: HARMONIC CONSTANTS FILE
!     T3DTID: TIDAL MODEL FILE
!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
      IF(TIDALDB.EQ.1) THEN
        IF(TIDALBCGEN) THEN
          CALL BORD_TIDAL_BC(MESH2D%NBOR%I,LIHBOR%I,LIUBOL%I,
     &                       NPTFR2,KENT,KENTU,
     &                       MESH2D,GEOSYST,NUMZONE,LATIT,LONGIT,
     &                       TIDALTYPE,BOUNDARY_COLOUR,MAXFRO,
     &                       T3D_FILES(T3DBDD)%LU,
     &                       T3D_FILES(T3DTID)%LU,
     &                       T3D_FILES(T3DHAR)%LU,XSHIFT,YSHIFT)
        ENDIF
!
        CALL BORD_TIDE(ZF%R,MESH2D%NBOR%I,LIHBOR%I,LIUBOL%I,
     &                 NPOIN2,NPTFR2,AT,NCOTE,NVIT,
     &                 NUMLIQ%I,KENT,KENTU,
     &                 T3D_FILES(T3DIMP)%NAME,TIDALTYPE,
     &                 CTIDE,MSL,CTIDEV,NODALCORR,T3D_FILES(T3DHAR)%LU,
     &                 BOUNDARY_COLOUR,
     &                 HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     &                 MARDAT,MARTIM)
      ELSEIF(TIDALDB.EQ.2) THEN
        CALL BORD_TIDE_TPXO(ZF%R,MESH2D%NBOR%I,LIHBOR%I,LIUBOL%I,
     &                      NPOIN2,NPTFR2,AT,NCOTE,NVIT,
     &                      NUMLIQ%I,KENT,KENTU,MESH2D,
     &                      T3D_FILES(T3DIMP)%NAME,TIDALTYPE,
     &                      CTIDE,MSL,CTIDEV,NODALCORR,
     &                      BOUNDARY_COLOUR,
     &                      HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     &                      MARDAT,MARTIM,T3D_FILES,T3DBB1,T3DBB2,
     &                      X,Y,GEOSYST,NUMZONE,LATIT,LONGIT,INTMICON)
      ELSEIF(TIDALDB.EQ.3) THEN
        CALL BORD_TIDE_LEGOS(ZF%R,MESH2D%NBOR%I,LIHBOR%I,LIUBOL%I,
     &                 NPOIN2,NPTFR2,AT,NCOTE,NVIT,
     &                 NUMLIQ%I,KENT,KENTU,
     &                 T3D_FILES(T3DIMP)%NAME,TIDALTYPE,
     &                 CTIDE,MSL,CTIDEV,NODALCORR,T3D_FILES(T3DHAR)%LU,
     &                 BOUNDARY_COLOUR,
     &                 HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     &                 MARDAT,MARTIM)
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO K=1,NPTFR2
        IF(NUMTIDE%I(K).GT.0) THEN
!         POSSIBLE SMOOTHING AT THE BEGINNING
!         IF(AT.LT.1800.D0) THEN
!           UBTIDE%R(K) = UBTIDE%R(K)*(AT/1800.D0)
!           VBTIDE%R(K) = VBTIDE%R(K)*(AT/1800.D0)
!         ENDIF
          IF(LIUBOL%I(K).EQ.KENTU) THEN
            DO NP=1,NPLAN
              IBORD=(NP-1)*NPTFR2+K
              UBORL%R(IBORD) = UBTIDE%R(K)
              VBORL%R(IBORD) = VBTIDE%R(K)
              WBORL%R(IBORD) = 0.D0
            ENDDO
          ENDIF
          IF(LIHBOR%I(K).EQ.KENT) THEN
            HBOR%R(K) = HBTIDE%R(K)
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
