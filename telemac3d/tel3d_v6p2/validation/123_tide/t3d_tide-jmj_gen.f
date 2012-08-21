!
!
!   THIS FORTRAN FILE CONTAINS SUBROUTINES:
!   - TIDAL_MODEL_T3D FOR IMPOSITION OF BOUNDARY CONDITIONS FOR TIDE
!     (FREE SURFACE AND VELOCITIES)
!   - T3D_CORFON FOR TIDAL FLATS TREATMENT
!
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
      DOUBLE PRECISION XSHIFT,YSHIFT,BETA0
      LOGICAL TIDALBCGEN,TM2S2N2EQUAL
!
!-----------------------------------------------------------------------
!
!     PARAMETERS FOR TIDAL BOUNDARY CONDITIONS
!
!     CTIDEV: COEFFICIENT TO CALIBRATE THE VELOCITIES
!             DEFAULT = SQRT(CTIDE)
!
!     ACCORDING TO DV, IF A CORRECTION COEFFICIENT CTIDE IS APPLIED
!     FOR WATER DEPTHS, ANOTHER ONE MUST BE APPLIED FOR VELOCITIES
!     = SQRT(CTIDE)
!
      IF(CTIDEV.EQ.999999.D0) CTIDEV = SQRT(CTIDE)
!
!     NODALCORR: OPTION FOR CALCULATION OF NODAL FACTOR CORRECTION F
!                IN SUBROUTINES BORD_TIDE AND BORD_TIDE_LEGOS
!                DEFAULT = 1 (FROZEN AT THE DATE MARDAT + HOUR MARTIM + TEMPS)
!                0: NOT FROZEN, COMPUTED AT EACH TIME STEP
!                1: FROZEN WITH VALUE AT THE BEGINNING OF THE SIMULATION
!                2: FROZEN WITH VALUE AT THE MIDDLE OF THE YEAR IN MARDAT
!                   (SINGLE FORMER POSSIBILITY FOR TIDALTYPE = 7)
!     IN THE STEERING FILE, THE KEYWORDS 'ORIGINAL DATE OF TIME'
!     AND 'ORIGINAL HOUR OF TIME' HAVE TO BE SET
!     WARNING, FORMAT: YEAR, MONTH, DAY
!
      NODALCORR = 1
!
!     TIDALBCGEN: LOGICAL FOR GENERATION OF TIDAL BOUNDARY CONDITIONS OR NOT
!                 CURRENTLY WORKS ONLY FOR SCALAR COMPUTATIONS
!                 FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      TIDALBCGEN = .TRUE.
!
!     ICALHW: NUMBER THAT MAY BE CHOSEN BY THE USER TO CALIBRATE HIGH WATER
!             OR AUTOMATICALLY CHOSEN, WHEN MODELLING A SCHEMATIC TIDE
!             IN SUBROUTINE BORD_TIDE
!             DEFAULT = 0 (AUTOMATICALLY CHOSEN)
!             FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      ICALHW = 0
!
!     TM2S2N2EQUAL: LOGICAL TO IMPOSE THE PERIODS OF S2 AND N2 WAVES
!                   TO BE EQUAL TO THE PERIOD OF M2 WAVE
!                   DEFAULT = .FALSE.
!                   FOR SCHEMATIC TIDES MODELLING ONLY! 
!                   FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      TM2S2N2EQUAL = .FALSE.
!
!     OPTIONAL SHIFT OF COORDINATES
!     FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      XSHIFT =  +7000.D0
      YSHIFT = +11400.D0
!
!     BETA0: OPTIONAL ANGLE (IN DEGREES) BETWEEN LAMBERT AND MERCATOR-JMJ
!            REFERENCES (EAST OR X AXES, TRIGONOMETRIC)
!            DEFAULT = 0.D0 DEGREES
!            FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      BETA0 = -4.D0
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
          IF(T3D_FILES(T3DBDD)%NAME.EQ.' ') THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'POUR GENERER LE FICHIER DES CONSTANTES'
              WRITE(LU,*) 'HARMONIQUES POUR LA BASE DE DONNEES DE JMJ,'
              WRITE(LU,*) 'DONNER LE FICHIER'
              WRITE(LU,*) 'BASE ASCII DE DONNEES DE MAREE.'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'TO GENERATE THE HARMONIC CONTANTS FILE'
              WRITE(LU,*) 'FOR JMJ DATA BASE, PLEASE GIVE'
              WRITE(LU,*) 'ASCII DATABASE FOR TIDE FILE.'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(T3D_FILES(T3DTID)%NAME.EQ.' ') THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'POUR GENERER LE FICHIER DES CONSTANTES'
              WRITE(LU,*) 'HARMONIQUES POUR LA BASE DE DONNEES DE JMJ,'
              WRITE(LU,*) 'DONNER LE FICHIER DU MODELE DE MAREE.'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'TO GENERATE THE HARMONIC CONTANTS FILE'
              WRITE(LU,*) 'FOR JMJ DATA BASE, PLEASE GIVE'
              WRITE(LU,*) 'THE TIDAL MODEL FILE'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(T3D_FILES(T3DHAR)%NAME.EQ.' ') THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'DONNER LE FICHIER DES CONSTANTES HARMONIQUES'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL BORD_TIDAL_BC(MESH2D%NBOR%I,LIHBOR%I,LIUBOL%I,
     &                       NPTFR2,KENT,KENTU,
     &                       MESH2D,GEOSYST,NUMZONE,LATIT,LONGIT,
     &                       TIDALTYPE,BOUNDARY_COLOUR,MAXFRO,
     &                       T3D_FILES(T3DBDD)%LU,
     &                       T3D_FILES(T3DTID)%LU,
     &                       T3D_FILES(T3DHAR)%LU,XSHIFT,YSHIFT,BETA0)
        ENDIF
!
        IF(T3D_FILES(T3DHAR)%NAME.EQ.' ') THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'DONNER LE FICHIER DES CONSTANTES HARMONIQUES.'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL BORD_TIDE(ZF%R,MESH2D%NBOR%I,LIHBOR%I,LIUBOL%I,
     &                 NPOIN2,NPTFR2,AT,DT,NCOTE,NVIT,
     &                 NUMLIQ%I,KENT,KENTU,
     &                 T3D_FILES(T3DIMP)%NAME,TIDALTYPE,
     &                 CTIDE,MSL,CTIDEV,NODALCORR,T3D_FILES(T3DHAR)%LU,
     &                 BOUNDARY_COLOUR,
     &                 HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     &                 MARDAT,MARTIM,TM2S2N2EQUAL)
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
        IF(T3D_FILES(T3DHAR)%NAME.EQ.' ') THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'DONNER LE FICHIER DES CONSTANTES HARMONIQUES.'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL BORD_TIDE_LEGOS(ZF%R,MESH2D%NBOR%I,LIHBOR%I,LIUBOL%I,
     &                       NPOIN2,NPTFR2,AT,DT,NCOTE,NVIT,
     &                       NUMLIQ%I,KENT,KENTU,
     &                       T3D_FILES(T3DIMP)%NAME,TIDALTYPE,
     &                       CTIDE,MSL,CTIDEV,NODALCORR,
     &                       T3D_FILES(T3DHAR)%LU,BOUNDARY_COLOUR,
     &                       HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     &                       MARDAT,MARTIM)
      ELSEIF(TIDALDB.EQ.-1) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'VALEUR PAR DEFAUT INCORRECTE POUR LA BASE'
          WRITE(LU,*) 'DE DONNEES DE MAREE. CHOIX POSSIBLES :'
          WRITE(LU,*) '  -1 : JMJ ;'
          WRITE(LU,*) '  -2 : TPXO ;'
          WRITE(LU,*) '  -3 : LEGOS-NEA.'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'INCORRECT DEFAULT VALUE FOR TIDAL DATA BASE.'
          WRITE(LU,*) 'POSSIBLE CHOICES:'
          WRITE(LU,*) '  -1: JMJ,'
          WRITE(LU,*) '  -2: TPXO,'
          WRITE(LU,*) '  -3: LEGOS-NEA.'
        ENDIF
        CALL PLANTE(1)
        STOP
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'BASE DE DONNEES DE MAREE NON TRAITEE.'
          WRITE(LU,*) 'CHOIX POSSIBLES :'
          WRITE(LU,*) '  -1 : JMJ ;'
          WRITE(LU,*) '  -2 : TPXO ;'
          WRITE(LU,*) '  -3 : LEGOS-NEA.'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TIDAL DATA BASE NOT TAKEN INTO ACCOUNT.'
          WRITE(LU,*) 'POSSIBLE CHOICES:'
          WRITE(LU,*) '  -1: JMJ,'
          WRITE(LU,*) '  -2: TPXO,'
          WRITE(LU,*) '  -3: LEGOS-NEA.'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO K=1,NPTFR2
        IF(NUMTIDE%I(K).GT.0) THEN
!         POSSIBLE SMOOTHING AT THE BEGINNING
          IF(AT.LT.1800.D0) THEN
            UBTIDE%R(K) = UBTIDE%R(K)*(AT/1800.D0)
            VBTIDE%R(K) = VBTIDE%R(K)*(AT/1800.D0)
          ENDIF
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
!                    *********************
                     SUBROUTINE T3D_CORFON
!                    *********************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!+
!+            STANDARD ACTION: SMOOTHES THE BOTTOM ELEVATION.
!+
!+           (KEYWORD:  'NUMBER OF BOTTOM SMOOTHINGS')
!
!note     EQUIVALENT TO CORFON (BIEF LIBRARY), EXCEPT THAT THIS
!+         SUBROUTINE DISTINGUISHES DATA FROM STRUCTURES.
!
!history  J.M. JANIN  (LNH)
!+        25/11/97
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  J-M HERVOUET (LNHE)
!+        29/09/2011
!+        V6P2
!+   Name changed into T3D_CORFON to avoid duplication with Telemac-2D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LISFON         |-->| NUMBER OF SMOOTHINGS REQUIRED
!| MASKEL         |-->| MASK OF ELEMENTS
!| MATR2D         |<->| WORK MATRIX IN 2DH
!| MESH2D         |<->| 2D MESH
!| MSK            |-->| IF YES, THERE ARE MASKED ELEMENTS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| PRIVE          |<->| BLOCK OF PRIVATE ARRAYS FOR USER
!| S              |-->| VOID STRUCTURE
!| ST1            |<->| STRUCTURE OF T1
!| ST2            |<->| STRUCTURE OF T2
!| SZF            |<->| STRUCTURE OF ZF
!| T1             |<->| WORK ARRAY
!| T2             |<->| WORK ARRAY
!| X              |-->| MESH COORDINATE
!| Y              |-->| MESH COORDINATE
!| ZF             |<->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY : BOUNDARY_COLOUR
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2, LISFON
      LOGICAL, INTENT(IN) :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SZF, ST1, ST2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: ZF, T1, T2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(IN) :: X,Y
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: PRIVE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MATR2D
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: S
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,I
      LOGICAL MAS
!
      INTEGER          N,FIRST,LAST
!
!***********************************************************************
!
!     SMOOTHES THE BOTTOM ELEVATION
!
      IF(LISFON.GT.0) THEN
!
         MAS = .TRUE.
!
         CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &               1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  SUPPRESS TIDAL FLATS ON MARITIME BOUNDARIES
!  PARAMETERS FIRST AND LAST THAT MAY BE CHANGED!!!
!
      FIRST = 250
      LAST  = 339
!
      DO N=1,MESH2D%NPTFR
        I=BOUNDARY_COLOUR%I(N)
        IF(I.GE.FIRST.AND.I.LE.LAST) THEN
          ZF(MESH2D%NBOR%I(N)) = MIN(0.D0,ZF(MESH2D%NBOR%I(N)))
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
