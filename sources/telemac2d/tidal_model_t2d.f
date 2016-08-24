!                    **************************
                     SUBROUTINE TIDAL_MODEL_T2D
!                    **************************
!
!
!***********************************************************************
! TELEMAC2D   V7P1
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
!history  C-T PHAM (LNHE)
!+        02/10/2014
!+        V7P0
!+   BORD_TIDE_LEGOS changed into BORD_TIDE_MISC
!+   (e.g. LEGOS-NEA, FES20XX, Previmer)
!+   Default NODALCORR = 0 (not frozen, computed at each time step)
!+
!history  R. ATA (LNHE)
!+        24/08/2016
!+        V7P2
!+   tentative fix of a bug with the use of BND_TIDE
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
      USE TPXO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,NODALCORR,IFRLIQ
      DOUBLE PRECISION XSHIFT,YSHIFT,BETA
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
!                IN SUBROUTINES BORD_TIDE AND BORD_TIDE_MISC
!                DEFAULT = 0 (NOT FROZEN, WARNING: CHANGED VALUE, 1 UNTIL V6P3)
!                0: NOT FROZEN, COMPUTED AT EACH TIME STEP
!                1: FROZEN WITH VALUE AT THE BEGINNING OF THE SIMULATION
!                2: FROZEN WITH VALUE AT THE MIDDLE OF THE YEAR IN MARDAT
!                   (SINGLE FORMER POSSIBILITY FOR TIDALTYPE = 7)
!     IN THE STEERING FILE, THE KEYWORDS 'ORIGINAL DATE OF TIME'
!     AND 'ORIGINAL HOUR OF TIME' HAVE TO BE SET
!     WARNING, FORMAT: YEAR, MONTH, DAY
!
      NODALCORR = 0
!
!     TIDALBCGEN: LOGICAL FOR GENERATION OF TIDAL BOUNDARY CONDITIONS OR NOT
!                 CURRENTLY WORKS ONLY FOR SCALAR COMPUTATIONS
!                 FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      TIDALBCGEN = .FALSE.
!
!     TM2S2N2EQUAL: LOGICAL TO IMPOSE THE PERIODS OF S2 AND N2 WAVES
!                   TO BE EQUAL TO THE PERIOD OF M2 WAVE
!                   DEFAULT = .TRUE. (WARNING: CHANGED VALUE, .FALSE. IN V6P2)
!                   FOR SCHEMATIC TIDES MODELLING ONLY!
!                   FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      TM2S2N2EQUAL = .TRUE.
!
!     OPTIONAL SHIFT OF COORDINATES
!     FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      XSHIFT = 0.D0
      YSHIFT = 0.D0
!
!     BETA: OPTIONAL ANGLE (IN DEGREES) BETWEEN LAMBERT AND MERCATOR-JMJ
!           REFERENCES (EAST OR X AXES, TRIGONOMETRIC)
!           DEFAULT = 0.D0 DEGREES
!           FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      BETA = 0.D0
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
          IF(T2D_FILES(T2DBDD)%NAME(1:1).EQ.' ') THEN
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
          IF(T2D_FILES(T2DTID)%NAME(1:1).EQ.' ') THEN
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
          IF(T2D_FILES(T2DHAR)%NAME(1:1).EQ.' ') THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'DONNER LE FICHIER DES CONSTANTES HARMONIQUES'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL BORD_TIDAL_BC(MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                       NPTFR,KENT,KENTU,
     &                       MESH,GEOSYST,NUMZONE,LAMBD0,PHI0,
     &                       TIDALTYPE,BOUNDARY_COLOUR,MAXFRO,
     &                       T2D_FILES(T2DBDD)%LU,
     &                       T2D_FILES(T2DTID)%LU,
     &                       T2D_FILES(T2DHAR)%LU,XSHIFT,YSHIFT,BETA)
        ENDIF
!
        IF(T2D_FILES(T2DHAR)%NAME(1:1).EQ.' ') THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'DONNER LE FICHIER DES CONSTANTES HARMONIQUES.'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL BORD_TIDE(ZF%R,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                 NPOIN,NPTFR,AT,DT,NCOTE,NVITES,
     &                 NUMLIQ%I,KENT,KENTU,
     &                 T2D_FILES(T2DIMP)%NAME,TIDALTYPE,
     &                 CTIDE,MSL,CTIDEV,NODALCORR,T2D_FILES(T2DHAR)%LU,
     &                 BOUNDARY_COLOUR,
     &                 HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHWB,
     &                 MARDAT,MARTIM,TM2S2N2EQUAL)
      ELSEIF(TIDALDB.EQ.2) THEN
        CALL BORD_TIDE_TPXO(ZF%R,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                      NPOIN,NPTFR,AT,NCOTE,NVITES,
     &                      NUMLIQ%I,KENT,KENTU,MESH,
     &                      T2D_FILES(T2DIMP)%NAME,TIDALTYPE,
     &                      CTIDE,MSL,CTIDEV,NODALCORR,
     &                      BOUNDARY_COLOUR,
     &                      HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHWG,
     &                      MARDAT,MARTIM,T2D_FILES,T2DBB1,T2DBB2,
     &                      X,Y,GEOSYST,NUMZONE,LAMBD0,PHI0,INTMICON)
      ELSEIF(TIDALDB.EQ.3) THEN
        IF(T2D_FILES(T2DHAR)%NAME(1:1).EQ.' ') THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'DONNER LE FICHIER DES CONSTANTES HARMONIQUES.'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL BORD_TIDE_MISC(ZF%R,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                      NPOIN,NPTFR,AT,DT,NCOTE,NVITES,
     &                      NUMLIQ%I,KENT,KENTU,
     &                      T2D_FILES(T2DIMP)%NAME,TIDALTYPE,
     &                      CTIDE,MSL,CTIDEV,NODALCORR,
     &                      T2D_FILES(T2DHAR)%LU,BOUNDARY_COLOUR,
     &                      HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHWB,
     &                      MARDAT,MARTIM)
      ELSEIF(TIDALDB.EQ.-1) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'VALEUR PAR DEFAUT INCORRECTE POUR LA BASE'
          WRITE(LU,*) 'DE DONNEES DE MAREE. CHOIX POSSIBLES :'
          WRITE(LU,*) '  1 : JMJ ;'
          WRITE(LU,*) '  2 : TPXO ;'
          WRITE(LU,*) '  3 : DIVERS (LEGOS-NEA, FES20XX, PREVIMER...).'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'INCORRECT DEFAULT VALUE FOR TIDAL DATA BASE.'
          WRITE(LU,*) 'POSSIBLE CHOICES:'
          WRITE(LU,*) '  1: JMJ,'
          WRITE(LU,*) '  2: TPXO,'
          WRITE(LU,*) '  3: MISC (LEGOS-NEA, FES20XX, PREVIMER...).'
        ENDIF
        CALL PLANTE(1)
        STOP
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'BASE DE DONNEES DE MAREE NON TRAITEE.'
          WRITE(LU,*) 'CHOIX POSSIBLES :'
          WRITE(LU,*) '  1 : JMJ ;'
          WRITE(LU,*) '  2 : TPXO ;'
          WRITE(LU,*) '  3 : DIVERS (LEGOS-NEA, FES20XX, PREVIMER...).'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TIDAL DATA BASE NOT TAKEN INTO ACCOUNT.'
          WRITE(LU,*) 'POSSIBLE CHOICES:'
          WRITE(LU,*) '  1: JMJ,'
          WRITE(LU,*) '  2: TPXO,'
          WRITE(LU,*) '  3: MISC (LEGOS-NEA, FES20XX, PREVIMER...).'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO K=1,NPTFR
        IFRLIQ=NUMLIQ%I(K)
!       TEST ON NUMTIDE PROBABLY NO LONGER USEFUL
        IF(NUMTIDE%I(K).GT.0.AND.IFRLIQ.GT.0) THEN
!        IF(BND_TIDE(IFRLIQ).GT.0) THEN
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
!        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
