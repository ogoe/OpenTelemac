!                       *****************
                        SUBROUTINE LIMWAC
!                       *****************
!
     &(F     , FBOR  , LIFBOR, NPTFR , NPLAN , NF    ,  TETA , FREQ  ,
     & NPOIN2, NBOR  , AT    , LT    , DDC   , LIMSPE, FPMAXL, FETCHL,
     & SIGMAL, SIGMBL, GAMMAL, FPICL , HM0L  , APHILL, TETA1L, SPRE1L,
     & TETA2L, SPRE2L, XLAMDL, X ,Y  , KENT  , KSORT , NFO1  , NBI1  ,
     & BINBI1, UV    , VV    , SPEULI, VENT  , VENSTA, GRAVIT, 
     & PRIVE , NPRIV , SPEC  , FRA   , DEPTH , FRABL ,BOUNDARY_COLOUR,
     & IMP_FILE)
!
!***********************************************************************
! TOMAWAC   V7P3                                   23/02/2017
!***********************************************************************
!
!brief    BOUNDARY CONDITIONS.
!
!warning  BY DEFAULT, THE BOUNDARY CONDITIONS SPECIFIED IN THE FILE
!+            DYNAM ARE DUPLICATED ON ALL THE DIRECTIONS AND FREQUENCIES
!
!history  F. MARCOS (LNH)
!+        01/02/95
!+        V1P0
!+
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
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  E. GAGNAIRE-RENOU & J.-M. HERVOUET (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   A line IF(LIMSPE.EQ.0...) RETURN removed.
!
!history  A. JOLY (EDF R&D, LNHE)
!+        23/02/2017
!+        V7P3
!+   SPECTRA READ FROM AN EXTERNAL MESH CAN NOW BE IMPOSED ON THE
!+   OPEN BOUNDARIES.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| APHILL         |-->| BOUNDARY PHILLIPS CONSTANT
!| AT             |-->| COMPUTATION TIME
!| BINBI1         |-->| BINARY FILE 1 BINARY
!| BOUNDARY_COLOUR|-->| COLOUR OF BOUNDARY POINT (DEFAULT: ITS RANK)
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FBOR           |<->| SPECTRAL VARIANCE DENSITY AT THE BOUNDARIES
!| FETCHL         |-->| BOUNDARY MEAN FETCH VALUE
!| FPICL          |-->| BOUNDARY PEAK FREQUENCY
!| FPMAXL         |-->| BOUNDARY MAXIMUM PEAK FREQUENCY
!| FRA            |<--| DIRECTIONAL SPREADING FUNCTION VALUES
!| FRABL          |-->| BOUNDARY ANGULAR DISTRIBUTION FUNCTION
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GAMMAL         |-->| BOUNDARY PEAK FACTOR
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| HM0L           |-->| BOUNDARY SIGNIFICANT WAVE HEIGHT
!| IMP_FILE       |-->| MESH FILE WITH THE IMPOSED SPECTRA 
!| KENT           |-->| B.C.: A SPECTRUM IS PRESCRIBED AT THE BOUNDARY
!| KSORT          |-->| B.C.: FREE BOUNDARY: NO ENERGY ENTERING THE DOMAIN
!| LIFBOR         |-->| TYPE OF BOUNDARY CONDITION ON F
!| LIMSPE         |-->| TYPE OF BOUNDARY DIRECTIONAL SPECTRUM
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NBI1           |-->| LOGICAL UNIT NUMBER OF THE USER BINARY FILE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NF             |-->| NUMBER OF FREQUENCIES
!| NFO1           |-->| LOGICAL UNIT NUMBER OF THE USER FORMATTED FILE
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPRIV          |-->| NUMBER OF PRIVATE ARRAYS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| PRIVE          |-->| USER WORK TABLE
!| SIGMAL         |-->| BOUNDARY SPECTRUM VALUE OF SIGMA-A
!| SIGMBL         |-->| BOUNDARY SPECTRUM VALUE OF SIGMA-B
!| SPEC           |<--| VARIANCE DENSITY FREQUENCY SPECTRUM
!| SPEULI         |-->| INDICATES IF B.C. SPECTRUM IS MODIFIED BY USER
!| SPRE1L         |-->| BOUNDARY DIRECTIONAL SPREAD 1
!| SPRE2L         |-->| BOUNDARY DIRECTIONAL SPREAD 2
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TETA1L         |-->| BOUNDARY MAIN DIRECTION 1
!| TETA2L         |-->| BOUNDARY MAIN DIRECTION 2
!| UV, VV         |-->| WIND VELOCITIES AT THE MESH POINTS
!| VENSTA         |-->| INDICATES IF THE WIND IS STATIONARY
!| VENT           |-->| INDICATES IF WIND IS TAKEN INTO ACCOUNT
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XLAMDL         |-->| BOUNDARY WEIGHTING FACTOR FOR ANGULAR
!|                |   | DISTRIBUTION FUNCTION
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_LIMWAC => LIMWAC
      USE DECLARATIONS_TOMAWAC, ONLY : UV2D,VV2D,PROF,FB_CTE,NPB
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI
      USE DECLARATIONS_SPECIAL
      USE BND_SPECTRA
      USE BIEF_DEF, ONLY : BIEF_FILE
      IMPLICIT NONE
!
!
      INTEGER, INTENT(IN)            :: NPTFR,NPLAN,NF,NPOIN2,LT,NPRIV
      INTEGER, INTENT(IN)            :: LIMSPE,KENT,KSORT,FRABL
      INTEGER, INTENT(IN)            :: NFO1,NBI1
      INTEGER, INTENT(IN)            :: LIFBOR(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(IN)            :: BOUNDARY_COLOUR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)   :: TETA(NPLAN),FREQ(NF)
      DOUBLE PRECISION, INTENT(IN)   :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: UV(NPOIN2),VV(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: SPEC(NF)
      DOUBLE PRECISION, INTENT(IN)   ::PRIVE(NPOIN2,NPRIV),DEPTH(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: AT,DDC,FPMAXL,FETCHL,SIGMAL
      DOUBLE PRECISION, INTENT(IN)   :: GAMMAL,FPICL, SIGMBL
      DOUBLE PRECISION, INTENT(IN)   :: HM0L  , APHILL,TETA1L,SPRE1L
      DOUBLE PRECISION, INTENT(IN)   :: SPRE2L,XLAMDL,TETA2L
      DOUBLE PRECISION, INTENT(IN)   :: GRAVIT
      LOGICAL,          INTENT(IN)   :: SPEULI, VENT, VENSTA
      CHARACTER(LEN=8), INTENT(IN)   :: BINBI1
      TYPE(BIEF_FILE), INTENT(IN)    :: IMP_FILE
      DOUBLE PRECISION, INTENT(INOUT):: F(NPOIN2,NPLAN,NF), FRA(NPLAN)
      DOUBLE PRECISION, INTENT(INOUT):: FBOR(NPTFR,NPLAN,NF)
!
      INTEGER IFF,IPLAN,IPTFR
!
      DOUBLE PRECISION, ALLOCATABLE :: TRAV(:)
      DOUBLE PRECISION E2FMIN
!
      INTEGER IFREQ
!
      DOUBLE PRECISION S00, DTETA
      LOGICAL FLAG
!
!
!***********************************************************************
!
!     MODIFIES THE TYPE OF BOUNDARY CONDITION (OPTIONAL)
!
!     CAN BE CODED BY THE USER (SPEULI=.TRUE.)
!
!     LIFBOR(IPTFR)=KENT OR KSORT
!
      FLAG=.FALSE.
      IF (VENT .AND. (LIMSPE.EQ.1 .OR. LIMSPE.EQ.2 .OR. LIMSPE.EQ.3
     & .OR. LIMSPE.EQ.5)) FLAG=.TRUE.
!
!     THE FIRST TIME, ALLOCATES MEMORY FOR THE USEFUL ARRAYS
!     ---------------------------------------------------------------
!
      IF(LT.LT.1) THEN
        NPB=1
        IF(FLAG) THEN
          ALLOCATE(UV2D(1:NPTFR),VV2D(1:NPTFR))
          NPB=NPTFR
        ENDIF
        IF(LIMSPE.EQ.7 .OR. SPEULI) THEN
          IF (.NOT.ALLOCATED(PROF)) ALLOCATE(PROF(1:NPTFR))
          NPB=NPTFR
        ENDIF
        IF(NPB.EQ.1) THEN
          IF (.NOT.ALLOCATED(FB_CTE)) ALLOCATE(FB_CTE(1:NPLAN,1:NF))
        ENDIF
      ENDIF
      IF (.NOT.ALLOCATED(UV2D)) ALLOCATE(UV2D(NPTFR))
      IF (.NOT.ALLOCATED(VV2D)) ALLOCATE(VV2D(NPTFR))
      IF (.NOT.ALLOCATED(PROF)) ALLOCATE(PROF(NPTFR))
      IF (.NOT.ALLOCATED(FB_CTE)) ALLOCATE(FB_CTE(1:NPLAN,1:NF))
!
!     THE FIRST TIME (AND POSSIBLY SUBSEQUENTLY IF THE WIND IS NOT
!     STATIONARY AND IF THE BOUNDARY SPECTRUM DEPENDS ON IT),
!     COMPUTES THE BOUNDARY SPECTRUM
!
      IF(LT.LT.1 .OR. (.NOT.VENSTA.AND.FLAG) .OR. SPEULI .OR.
     &   (IMP_FILE%NAME(1:1).NE.' ')) THEN
        IF(FLAG) THEN
          DO IPTFR=1,NPTFR
            UV2D(IPTFR)=UV(NBOR(IPTFR))
            VV2D(IPTFR)=VV(NBOR(IPTFR))
          ENDDO
        ENDIF
        IF(LIMSPE.EQ.7 .OR. SPEULI) THEN
          DO IPTFR=1,NPTFR
            PROF(IPTFR)=DEPTH(NBOR(IPTFR))
          ENDDO
        ENDIF
!
        E2FMIN = 1.D-30
!
!       WHEN NPB=1 FBOR ONLY FILLED FOR FIRST POINT
!
!       SPECTRUM ON BOUNDARIES
!
        IF(NPB.EQ.NPTFR) THEN
          CALL SPEINI
     &(   FBOR  ,SPEC  ,FRA   ,UV2D  ,VV2D  ,FREQ ,
     &    TETA  ,GRAVIT,FPMAXL,FETCHL,SIGMAL,SIGMBL,GAMMAL,FPICL,
     &    HM0L  ,APHILL,TETA1L,SPRE1L,TETA2L,SPRE2L,XLAMDL,
     &    NPB   ,NPLAN ,NF    ,LIMSPE,E2FMIN,PROF  ,FRABL )
        ELSE
          CALL SPEINI
     &(   FB_CTE,SPEC  ,FRA   ,UV2D  ,VV2D  ,FREQ ,
     &    TETA  ,GRAVIT,FPMAXL,FETCHL,SIGMAL,SIGMBL,GAMMAL,FPICL,
     &    HM0L  ,APHILL,TETA1L,SPRE1L,TETA2L,SPRE2L,XLAMDL,
     &    NPB   ,NPLAN ,NF    ,LIMSPE,E2FMIN,PROF  ,FRABL )
        ENDIF
!
!       IF THERE IS A MESHED FILE WITH THE BOUNDARY SPECTRA
!       THEY NEED TO BE IMPOSED
!
        IF(IMP_FILE%NAME(1:1).NE.' ')THEN
          CALL IMPOSE_BND_SPECTRA(IMP_FILE,LT,AT,FBOR,NPTFR,NPLAN,NF)
        ENDIF
!
!     ===========================================================
!     TO BE MODIFIED BY USER - RESU CAN BE CHANGED
!     ===========================================================
!
        IF(SPEULI) THEN
!
!        EXEMPLE DE MODIFICATION DE FRA - A MODIFIER SUIVANT VOTRE CAS
!        EXAMPLE OF MODIFICATION OF FRA - TO BE MODIFIED DEPENDING
!        ON YOUR CASE
          ALLOCATE(TRAV(1:NF))
          S00=0.4538D0
          DO IFREQ=1,NF
            IF (FREQ(IFREQ).LT.FPICL) THEN
              TRAV(IFREQ)=S00*(FREQ(IFREQ)/FPICL)**(-2.03D0)
            ELSE
              TRAV(IFREQ)=S00*(FREQ(IFREQ)/FPICL)**(1.04D0)
            ENDIF
          ENDDO
!
          DO IPTFR=1,NPTFR
            IF (LIFBOR(IPTFR).EQ.KENT) THEN
              DO IPLAN=1,NPLAN
                DTETA=TETA(IPLAN)-TETA1L
                IF ((TETA(IPLAN)-TETA1L).GT.DEUPI/2) THEN
                  DTETA=DEUPI-DTETA
                ENDIF
                DO IFREQ=1,NF
                  FRA(IPLAN)=1.D0/SQRT(DEUPI)*TRAV(IFREQ)*
     &            EXP(-DTETA**2/(2.D0*TRAV(IFREQ)**2))
!                  F(NBOR(IPTFR),IPLAN,IFREQ)=SPEC(IFREQ)*FRA(IPLAN)
                  FBOR(IPTFR,IPLAN,IFREQ)=SPEC(IFREQ)*FRA(IPLAN)
                ENDDO
              ENDDO
            ENDIF
          ENDDO
!
          DEALLOCATE(TRAV)
!
        ENDIF
!
!     ===========================================================
!     END OF USER MODIFICATIONS
!     ===========================================================
!
      ENDIF
!
!     -----------------------------------------------------------------
!     DUPLICATES THE BOUNDARY CONDITION FROM DYNAM ON ALL THE
!     DIRECTIONS AND FREQUENCIES, IF LIQUID BOUNDARY
!     -----------------------------------------------------------------
!
      IF(FLAG.OR.LIMSPE.EQ.7.OR.SPEULI.OR.
     &   (IMP_FILE%NAME(1:1).NE.' ')) THEN
        DO IPTFR=1,NPTFR
          IF(LIFBOR(IPTFR).EQ.KENT) THEN
            DO IFF=1,NF
              DO IPLAN=1,NPLAN
                F(NBOR(IPTFR),IPLAN,IFF)=FBOR(IPTFR,IPLAN,IFF)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ELSE
        DO IPTFR=1,NPTFR
          IF(LIFBOR(IPTFR).EQ.KENT) THEN
            DO IFF=1,NF
              DO IPLAN=1,NPLAN
                F(NBOR(IPTFR),IPLAN,IFF)=FB_CTE(IPLAN,IFF)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
