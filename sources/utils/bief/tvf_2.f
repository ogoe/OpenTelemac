!                    ****************
                     SUBROUTINE TVF_2
!                    ****************
!
     &(F,FN,FC,UNSV2D,DT,FXBOR,FXBORPAR,FBOR,SMH,YASMH,FSCEXP,
     & NPOIN,NPTFR,NBOR,LIMTRA,KDIR,KDDL,OPTSOU,HLIN,
     & IOPT2,FLBORTRA,SURNIT,RAIN,PLUIE,TRAIN,PHI_I)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    As TVF, computes the new value of tracer in a sub-iteration,
!+        but here based on contributions per point, not on fluxes.
!
!history  S. PAVAN & J-M HERVOUET (EDF LAB, LNHE)
!+        22/04/2014
!+        V7P0
!+   First version, derived from TVF.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME-STEP
!| F              |<--| VALUES OF F AT TIME N+1 OF SUB-ITERATION
!| FBOR           |-->| VALUES OF F AT THE PRESCRIBED BOUNDARIES
!| FC             |-->| VALUES OF F AT TIME N OF SUB-ITERATION
!| FLBORTRA       |<->| FLUX OF TRACER AT THE BOUNDARIES
!| FN             |-->| VALEURS DU TRACEUR A L'ETAPE N.
!| FSCEXP         |-->| EXPLICIT SOURCE TERM FOR F
!| FXBOR          |-->| FLUXES ON BOUNDARIES
!| FXBORPAR       |-->| FLUXES ON BOUNDARIES (DEFINED ON ALL DOMAIN
!|                |   | AND ASSEMBLED IN PARALLEL)
!| HLIN           |-->| WATER DEPTH AT TIME N+1
!|                |   | (WITH LINEAR INTERPOLATION IN TIME BETWEEN
!|                |   | HN AND H)
!| IOPT2          |-->| 0: CONSERVATIVE ADVECTION FIELD
!|                |   | 1: NON CONSERVATIVE ADVECTION FIELD
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| LIMTRA         |-->| TECHNICAL BOUNDARY CONDITIONS FOR TRACERS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
!| PHI_I          |-->| CONTRIBUTIONS PER POINT
!| PLUIE          |-->| RAIN OR EVAPORATION, IN M/S
!| RAIN           |-->| IF YES: RAIN OR EVAPORATION
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SURNIT         |-->| SURNIT=1/NIT
!| TRAIN          |-->| VALUE OF TRACER IN RAIN
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| YASMH          |-->| IF YES, SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_TVF_2 => TVF_2
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,KDIR,KDDL
      INTEGER, INTENT(IN)             :: OPTSOU,IOPT2
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMTRA(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,SURNIT,TRAIN
      DOUBLE PRECISION, INTENT(INOUT) :: FLBORTRA(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: FC(NPOIN),HLIN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SMH(NPOIN),UNSV2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: PLUIE(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FSCEXP(NPOIN),FN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FBOR(NPTFR),FXBORPAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: PHI_I(NPOIN)
      LOGICAL, INTENT(IN)             :: YASMH,RAIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N
!
!-----------------------------------------------------------------------
!
      IF(IOPT2.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TVF_2 : OPTION IOPT2 INCONNUE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TVF_2: UNKNOWN IOPT2 OPTION'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
        F(I)=F(I)-(DT/HLIN(I))*UNSV2D(I)*PHI_I(I)
      ENDDO
!
!     SOURCE TERMS
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            F(I)=F(I)+DT/HLIN(I)*SMH(I)*(FSCEXP(I)+FN(I)-FC(I))
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
           F(I)=F(I)+DT/HLIN(I)*UNSV2D(I)*SMH(I)*(FSCEXP(I)+FN(I)-FC(I))
          ENDDO
        ENDIF
      ENDIF
!
!     RAIN-EVAPORATION
!
      IF(RAIN) THEN
        DO I=1,NPOIN
          IF(PLUIE(I).GT.0.D0) THEN
!           REAL RAIN, VALUE IN RAIN CONSIDERED...
            F(I)=F(I)+DT/HLIN(I)*PLUIE(I)*(TRAIN-FC(I))
          ELSE
!           EVAPORATION, VALUE IN RAIN NOT CONSIDERED...
            F(I)=F(I)+DT/HLIN(I)*PLUIE(I)*(     -FC(I))
          ENDIF
        ENDDO
      ENDIF
!
!     ON THE DIRICHLET BOUNDARIES, FLUX TERMS TAKEN INTO ACCOUNT
!     ON OTHERS, FBOR IS TAKEN AS FN, SO NO CONTRIBUTION
!
!     FC : FN AT THE PREDICTOR STEP!
      DO I=1,NPTFR
        IF(LIMTRA(I).EQ.KDIR) THEN
          N=NBOR(I)
          F(N)=F(N)-DT/HLIN(N)*UNSV2D(N)*FXBORPAR(N)*(FBOR(I)-FC(N))
!
!       FOR CHECKING THE MASS BALANCE
!       THIS IS ALREADY DONE IN THE PREDICTOR STEP AND MUST NOT BE DONE
!       TWICE
!
!       ELSEIF(LIMTRA(I).EQ.KDDL) THEN
!         N=NBOR(I)
!         FLBORTRA(I)=FLBORTRA(I)+FXBOR(I)*FC(N)*SURNIT
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
