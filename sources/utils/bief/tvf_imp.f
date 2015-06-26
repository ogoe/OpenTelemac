!                    ******************
                     SUBROUTINE TVF_IMP
!                    ******************
!
     &(F,FC,FXMAT,FXMATPAR,
     & UNSV2D,DT,FXBOR,FXBORPAR,HNP1MT,FBOR,SMH,YASMH,FSCEXP,
     & NSEG,NPOIN,NPTFR,GLOSEG,SIZGLO,NBOR,LIMTRA,KDIR,KDDL,OPTSOU,
     & IOPT2,FLBORTRA,SURNIT,MESH,SF,RAIN,PLUIE,TRAIN,TETAF,INFOGT,
     & VOLU2D,V2DPAR,SM,PSIEXP,AM2,BB,SLVPSI,
     & PREDICTOR,CORRECTOR,ICOR,NCOR,MASSOU)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    Semi-implicit distributive scheme.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        26/06/2015
!+        V7P1
!+   First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME-STEP
!| F              |<--| VALUES OF F AT TIME N+1 OF SUB-ITERATION
!| FBOR           |-->| VALUES OF F AT THE PRESCRIBED BOUNDARIES
!| FC             |-->| VALUES OF F AT TIME N OF SUB-ITERATION
!| FLBORTRA       |<->| FLUX OF TRACER AT THE BOUNDARIES
!| FSCEXP         |-->| EXPLICIT SOURCE TERM FOR F
!| FXBOR          |-->| FLUXES ON BOUNDARIES
!| FXBORPAR       |-->| FLUXES ON BOUNDARIES (DEFINED ON ALL DOMAIN
!|                |   | AND ASSEMBLED IN PARALLEL)
!| FXMAT          |-->| FLUXES (NON ASSEMBLED IN PARALLEL)
!| FXMATPAR       |-->| FLUXES (ASSEMBLED IN PARALLEL)
!| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT.
!| INFOGT         |-->| IF YES, PRINT INFORMATION ON SOLVER
!| IOPT2          |-->| 0: CONSERVATIVE ADVECTION FIELD
!|                |   | 1: NON CONSERVATIVE ADVECTION FIELD
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| LIMTRA         |-->| TECHNICAL BOUNDARY CONDITIONS FOR TRACERS
!| MASSOU         |-->| MASS ADDED BY SOURCE TERM
!| MESH           |-->| MESH STRUCTURE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
!| PLUIE          |-->| RAIN OR EVAPORATION, IN M/S
!| RAIN           |-->| IF YES: RAIN OR EVAPORATION
!| SF             |<->| BIEF_OBJ STRUCTURE OF F
!| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SURNIT         |-->| SURNIT=1/NIT
!| TETAF          |-->| IMPLICITATION COEFFICIENT ON F
!| HNP1MT         |-->| INTERMEDIATE DEPTH H(N+1-TETA)
!| TRAIN          |-->| VALUE OF TRACER IN RAIN
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| VOLU2D         |-->| INTEGRALS OF TEST FUNCTIONS, NOT ASSEMBLED IN //
!| V2DPAR         |-->| INTEGRALS OF TEST FUNCTIONS ASSEMBLED IN //
!| YASMH          |-->| IF YES, SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_TVF_IMP => TVF_IMP
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NPTFR,KDIR,KDDL
      INTEGER, INTENT(IN)             :: SIZGLO,OPTSOU,IOPT2
      INTEGER, INTENT(IN)             :: ICOR,NCOR
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMTRA(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,SURNIT,TRAIN,TETAF(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLBORTRA(NPTFR),MASSOU
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: FC(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HNP1MT(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SMH(NPOIN),UNSV2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: V2DPAR(NPOIN),VOLU2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: PLUIE(NPOIN),PSIEXP(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FSCEXP(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FBOR(NPTFR),FXBORPAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG),FXMATPAR(NSEG)
      LOGICAL, INTENT(IN)             :: YASMH,RAIN,INFOGT
      LOGICAL, INTENT(IN)             :: PREDICTOR,CORRECTOR
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: SF,SM,AM2,BB
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(SLVCFG),    INTENT(INOUT)  :: SLVPSI
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N,I1,I2
!
!-----------------------------------------------------------------------
!
      IF(PREDICTOR) THEN
!
        IF(IOPT2.EQ.0) THEN
!         CONSERVATIVE ADVECTION FIELD
          DO I = 1,NPOIN
            F(I) = FC(I)
          ENDDO
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'TVF_IMP : OPTION INCONNUE'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'TVF_IMP: UNKNOWN OPTION'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!     BUILDING THE MATRIX AND THE RIGHT-HAND SIDE
!     THE MATRIX IS NOT DONE WITH A CALL MATRIX
!     SO ITS FEATURES HAVE TO BE HARDCODED HERE 
!
      AM2%X%DIM1=2*NSEG
      AM2%X%DIM2=1
      AM2%TYPEXT='Q'
      AM2%TYPDIA='Q'
      AM2%ELMLIN=11
      AM2%ELMCOL=11
      CALL CPSTVC(SF,AM2%D)
!
!     DIAGONAL  
!
      DO I=1,NPOIN
        AM2%D%R(I)=HNP1MT(I)*VOLU2D(I)
      ENDDO
!
!     IMPLICIT BOUNDARY TERM
!
      DO I=1,NPTFR
        N=NBOR(I)
        IF(LIMTRA(I).EQ.KDIR) THEN
          AM2%D%R(N)=AM2%D%R(N)-DT*TETAF(N)*FXBOR(I)
        ENDIF
      ENDDO
!
!     DIAGONAL AND OFF-DIAGONAL TERMS
!
      DO I=1,NSEG
        I1=GLOSEG(I,1)  
        I2=GLOSEG(I,2)
        IF(FXMATPAR(I).LT.0.D0) THEN
          AM2%D%R(I1) = AM2%D%R(I1) - DT*TETAF(I1)*FXMAT(I)
          AM2%X%R(I)=DT*TETAF(I2)*FXMAT(I)
          AM2%X%R(I+NSEG)=0.D0
        ELSE
          AM2%D%R(I2) = AM2%D%R(I2) + DT*TETAF(I2)*FXMAT(I)
          AM2%X%R(I)=0.D0
          AM2%X%R(I+NSEG)=-DT*TETAF(I1)*FXMAT(I)
        ENDIF        
      ENDDO
!
!     SOURCES IN CONTINUITY EQUATION (SMH)
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            AM2%D%R(I)=AM2%D%R(I)
     &                +DT*TETAF(I)*VOLU2D(I)*MAX(SMH(I),0.D0)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            AM2%D%R(I)=AM2%D%R(I)+DT*TETAF(I)*MAX(SMH(I),0.D0)
          ENDDO
        ENDIF
      ENDIF 
!
!     RIGHT HAND SIDE 
!
!     TERM FROM THE DERIVATIVE IN TIME
      IF(PREDICTOR) THEN
        DO I=1,NPOIN
          SM%R(I)=VOLU2D(I)*HNP1MT(I)*FC(I)
        ENDDO
      ELSEIF(CORRECTOR) THEN
!       THE PREDICTOR IS TAKEN, AT THIS LEVEL IT IS STILL F
        DO I=1,NPOIN
          SM%R(I)=VOLU2D(I)*HNP1MT(I)*F(I)
        ENDDO
      ELSE
        WRITE(LU,*) 'TVF_IMP, CHECK ARGUMENTS PREDICTOR, CORRECTOR'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     TERMES BII * CIN ET BIJ * CJN
!
!     ASSEMBLED CONTRIBUTION, DONE BY FLUX_EF_VF_3, POSSIBLY
!     WITH DERIVATIVE IN TIME.
      DO I=1,NPOIN
        SM%R(I)=SM%R(I)-DT*PSIEXP(I)
      ENDDO 
!
!     ADD FLUX ON BOUNDARY AND OTHER TERMS
!
      DO I=1,NPTFR
        IF(LIMTRA(I).EQ.KDIR) THEN
          N=NBOR(I)
          SM%R(N)=SM%R(N)+DT*FXBOR(I)*((1.D0-TETAF(N))*FC(N)-FBOR(I))
        ENDIF
      ENDDO
!
!     SOURCE TERMS
!
      IF(YASMH) THEN
!       SEE DIFSOU, FSCEXP CONTAINS THE VALUE OF THE TRACER AT THE SOURCE
        IF(OPTSOU.EQ.1) THEN
          DO I=1,MESH%NPOIN
            SM%R(I)=SM%R(I)+(FSCEXP(I)-(1.D0-TETAF(I))*FC(I))
     &                     *VOLU2D(I)*DT*MAX(SMH(I),0.D0)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,MESH%NPOIN
            SM%R(I)=SM%R(I)+(FSCEXP(I)-(1.D0-TETAF(I))*FC(I))
     &             *DT*MAX(SMH(I),0.D0)
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
            SM%R(I)=SM%R(I)+DT*VOLU2D(I)*PLUIE(I)
     &          *(TRAIN-(1.D0-TETAF(I))*FC(I))
          ELSE
!           EVAPORATION, VALUE IN RAIN NOT CONSIDERED...
            SM%R(I)=SM%R(I)+DT*VOLU2D(I)*PLUIE(I)
     &          *(     -(1.D0-TETAF(I))*FC(I))
          ENDIF
          AM2%D%R(I)=AM2%D%R(I)+DT*TETAF(I)*VOLU2D(I)*PLUIE(I)
        ENDDO
      ENDIF
!
!     TIDAL FLATS
! 
      CALL OS('X=Y     ',X=BB%ADR(1)%P,Y=AM2%D)
      IF(NCSIZE.GT.1) CALL PARCOM(BB%ADR(1)%P,2,MESH)
      DO I=1,NPOIN
!       SEE PRECD1 EPSILON HERE MUST BE GREATER THAN 1.D-20, OR PRECD1 WILL
!       DO THE CLIPPING ITSELF, IN A LESS CONSISTANT WAY
!       THE TEST MUST BE DONE ON THE ASSEMBLED DIAGONAL
        IF(BB%ADR(1)%P%R(I).LT.1.D-15) THEN
!         VOLU2D IS A NON ASSEMBLED COEFFICIENT, ANY SUCH COEFFICIENT WOULD WORK...
!         DRY POINT THAT RECEIVES NO WATER, F=FC IS GIVEN AS EQUATION
          AM2%D%R(I)=VOLU2D(I)
          SM%R(I)   =VOLU2D(I)*FC(I)
        ENDIF
      ENDDO
!
!     CALL MINI(C,I1,F,NPOIN)
!     PRINT*,'AVANT SOLVE MIN DE F=',C
!     IF(C.LT.0.999999D0) THEN
!       PRINT*,'F=',C,' AU POINT ',I1
!       CALL PLANTE(1)
!       STOP
!     ENDIF
!     CALL MAXI(C,I1,F,NPOIN)
!     PRINT*,'AVANT SOLVE MAX DE F=',C
!     IF(C.GT.2.D0) THEN
!       PRINT*,'F=',C,' AU POINT ',I1
!       CALL PLANTE(1)
!       STOP
!     ENDIF
!
!     SOLVING THE FINAL LINEAR SYSTEM
!
      CALL SOLVE(SF,AM2,SM,BB,SLVPSI,INFOGT,MESH,AM2)
!
!
!     CALL MINI(C,I1,F,NPOIN)
!     PRINT*,'APRES SOLVE MIN DE F=',C
!     IF(C.LT.0.999999D0) THEN
!       PRINT*,'F=',C,' AU POINT ',I1
!       CALL PLANTE(1)
!       STOP
!     ENDIF
!     CALL MAXI(C,I1,F,NPOIN)
!     PRINT*,'APRES SOLVE MAX DE F=',C
!     IF(C.GT.2.D0) THEN
!       PRINT*,'F=',C,' AU POINT ',I1
!       CALL PLANTE(1)
!       STOP
!     ENDIF
!
!     ON EXITS, EXITING FLUX DEPENDS ON F AND FN AT EVERY SUB-TIME
!     STEP, SO IT MUST BE COMPUTED AT THIS LEVEL.
!     THE CASE KDIR IS TREATED IN CVTRVF
!
!     FLUX AND ADDED MASS FOR MASS BALANCE
! 
      IF(ICOR.EQ.NCOR) THEN
        DO I=1,NPTFR
          IF(LIMTRA(I).EQ.KDIR) THEN
            FLBORTRA(I)=FLBORTRA(I)+FXBOR(I)*FBOR(I)*SURNIT  
          ELSEIF(LIMTRA(I).EQ.KDDL) THEN
            N=NBOR(I) 
            FLBORTRA(I)=FLBORTRA(I)
     &            +FXBOR(I)*(TETAF(N)*F(N)+(1.D0-TETAF(N))*FC(N))*SURNIT  
          ENDIF
        ENDDO
        IF(YASMH) THEN
!         FOR MASS BALANCE
          IF(OPTSOU.EQ.1) THEN
            DO I=1,MESH%NPOIN
              IF(SMH(I).GE.0.D0) THEN
                MASSOU=MASSOU+VOLU2D(I)*FSCEXP(I)*DT*SMH(I)
              ELSE
                MASSOU=MASSOU+VOLU2D(I)*
     &                (TETAF(I)*F(I)+(1.D0-TETAF(I))*FC(I))*DT*SMH(I)
              ENDIF
            ENDDO
          ELSEIF(OPTSOU.EQ.2) THEN
            DO I=1,MESH%NPOIN
              IF(SMH(I).GE.0.D0) THEN
                MASSOU=MASSOU+FSCEXP(I)*DT*SMH(I)
              ELSE
                MASSOU=MASSOU+
     &                  (TETAF(I)*F(I)+(1.D0-TETAF(I))*FC(I))*DT*SMH(I)
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDIF 
!
!-----------------------------------------------------------------------
!
      RETURN
      END

