!                    **************************
                     SUBROUTINE POSITIVE_DEPTHS
!                    **************************
!
     &(T1,T2,T3,T4,H,HN,MESH,FLODEL,COMPUTE_FLODEL,FLBOR,DT,
     & UNSV2D,NPOIN,GLOSEG1,GLOSEG2,NBOR,NPTFR,
     & SMH,YASMH,OPTSOU,FLULIM,LIMPRO,HBOR,KDIR,INFO,FLOPOINT,
     & NAMECODE,OPTION,NITMAX)
!
!***********************************************************************
! BIEF   V7P2
!***********************************************************************
!
!brief    SUPPRESSES NEGATIVE DEPTHS BY A LIMITATION OF FLUXES.
!
!history  J-M HERVOUET (LNHE)
!+        12/03/2010
!+        V6P0
!+   ADDED COMPUTE_FLODEL
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
!+        16/07/2012
!+        V6P2
!+   The value of transmitted fluxes is given back in FLODEL in all
!+   cases, YAFLODEL no longer used (but left for compatibility)
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        30/05/2013
!+        V6P2
!+   Argument NITMAX added.
!
!history  J-M HERVOUET (LNHE)
!+        30/12/2013
!+        V7P0
!+   Argument YAFLODEL removed.
!
!history  J-M HERVOUET (LNHE)
!+        18/09/2014
!+        V7P0
!+   Now positive sources are first added, then the transfers between
!+   points are done, then the negative sources are treated. In this way
!+   the transfers cannot be corrupted by an initial negative depth.
!+   + one NCSIZE.GT.0 changed into NCSISE.GT.1.
!
!history  J-M HERVOUET (LNHE)
!+        19/06/2015
!+        V7P1
!+   Adaptation to new coefficient IFAC instead of FAC.
!+   Option 1 removed. FLULIM now equal on either side of a segment
!+   in parallel. FLODEL differently shared at the exit.
!
!history  J-M HERVOUET (LNHE)
!+        25/03/2016
!+        V7P2
!+   OPTION has a new meaning (2 was the only previous possibility)
!+   Now : 1 : positive depths ensured with an EBE approach (new !)
!+             that is necessary for the ERIA advection scheme.
!+         2 : positive depths ensured with an edge-based approach (old)
!+   Option 1 will be mandatory with the new advection scheme 15.
!
!history  J-M HERVOUET (LNHE)
!+        16/07/2016
!+        V7P2
!+   In ERIA scheme, now forbidding that two fluxes on either side of
!+   a segment be of opposite sign (one is cancelled and the other is
!+   reduced). The final flux limitation is thus in the range [0,1] and
!+   OPTION=1 (i.e. TREATMENT OF NEGATIVE DEPTHS=3) is now also possible
!+   for the LIPS scheme.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COMPUTE_FLODEL |-->| IF YES, COMPUTE FLODEL HERE
!| DT             |-->| TIME STEP
!| FLBOR          |<->| BOUNDARY FLUXES
!| FLODEL         |<->| FLUXES GIVEN BY SEGMENT
!|                |   | MAY BE COMPUTED HERE (SEE COMPUTE-FLODEL)
!|                |   | OR SIMPLY GIVEN. AT THE EXIT, THE REAL FLUX
!|                |   | TRANSMITTED IS GIVEN BACK.
!| FLOPOINT       |-->| FLUXES GIVEN BY POINTS (ELEMENT BY ELEMENT)
!| FLULIM         |<--| PER SEGMENT: PERCENTAGE OF FLUX THAT HAS NOT
!|                |   | BEEN TRANSMITTED AT THE END OF THE ALGORITHM
!| GLOSEG1        |-->| FIRST POINT OF SEGMENTS
!| GLOSEG2        |-->| SECOND POINT OF SEGMENTS
!| H              |<->| NEW DEPTH
!| HBOR           |-->| PRESCRIBED DEPTHS AT BOUNDARIES
!| HN             |-->| OLD DEPTH
!| INFO           |-->| IF YES, PRINTING INFORMATION ON LISTING
!| KDIR           |-->| CONVENTION FOR DIRICHLET BOUNDARY CONDITION
!| LIMPRO         |-->| TYPE OF BOUNDARY CONDITIONS
!|                |   | IF EQUAL TO KDIR: PRESCRIBED DEPTH.
!| MESH           |<->| MESH STRUCTURE
!| NAMECODE       |-->| NAME OF CALLING CODE (SISYPHE, TELEMEC2D, ETC.)
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS
!| NITMAX         |-->| MAXIMUM NUMBER OF ITERATIONS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPTION         |-->| OPTION OF ALGORITHM FOR EDGE-BASED ADVECTION
!|                |   | 1: FAST BUT SENSITIVE TO SEGMENT NUMBERING
!|                |   | 2: INDEPENDENT OF SEGMENT NUMBERING
!| OPTSOU         |-->| OPTION FOR SOURCES 1: NORMAL 2: DIRAC
!| SMH            |-->| SOURCE TERMS
!| T1             |-->| WORK ARRAY
!| T2             |-->| WORK ARRAY
!| T3             |-->| WORK ARRAY
!| T4             |-->| WORK ARRAY
!| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASIS FUNCTIONS
!| YASMH          |-->| IF(YES) SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_POSITIVE_DEPTHS => POSITIVE_DEPTHS
      USE DECLARATIONS_TELEMAC, ONLY : DEJA_PDEPT, INDIC_PDEPT
      USE INTERFACE_PARALLEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,OPTSOU,KDIR,OPTION
      INTEGER, INTENT(IN)             :: NITMAX
      INTEGER, INTENT(IN)             :: GLOSEG1(*),GLOSEG2(*)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,HBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(*)
      TYPE(BIEF_MESH),INTENT(INOUT)   :: MESH
      DOUBLE PRECISION, INTENT(INOUT) :: FLOPOINT(MESH%NELEM,3)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,FLODEL,H,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: UNSV2D,HN,SMH
      LOGICAL, INTENT(IN)             :: YASMH,INFO
      LOGICAL, INTENT(IN)             :: COMPUTE_FLODEL
      CHARACTER(LEN=24)               :: NAMECODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I1,I2,I3,IPTFR,IOPT1,REMAIN,NEWREMAIN,IR,NITER
      INTEGER NELEM,NSEG,ISEG,IELEM,ISEG1,ISEG2,ISEG3
      DOUBLE PRECISION C,CPREV,CINIT,VOL1,VOL2,VOL3,HFL1,F1,F2,F3
      DOUBLE PRECISION DTLIM1,DTLIM2,DTLIM3,FP1,FP2,FP3,DT1,DT2,DT3
      DOUBLE PRECISION SURDT,HSEG1,HSEG2,TET,HFL2,A1,A2,A3
      DOUBLE PRECISION, PARAMETER :: TIERS=1.D0/3.D0
!
      DOUBLE PRECISION, PARAMETER :: EPS_FLUX = 1.D-15
      LOGICAL, PARAMETER :: TESTING = .FALSE.
!
!-----------------------------------------------------------------------
!
!     INDIC_PDEPT WILL BE A LIST OF SEGMENTS WITH NON ZERO FLUXES
!     HSEG IS THE DEPTH SHARED BETWEEN SEGMENTS
!
!
!-----------------------------------------------------------------------
!
      SURDT=1.D0/DT
      NELEM=MESH%NELEM
      NSEG=MESH%NSEG
!
      IF(.NOT.DEJA_PDEPT) THEN
        IF(OPTION.EQ.1) THEN
          ALLOCATE(INDIC_PDEPT(NELEM))
        ELSEIF(OPTION.EQ.2) THEN
          ALLOCATE(INDIC_PDEPT(NSEG))
        ENDIF
        DEJA_PDEPT=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(OPTION.NE.1.AND.OPTION.NE.2) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'OPTION INCONNUE DANS POSITIVE_DEPTHS : ',OPTION
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'UNKNOWN OPTION IN POSITIVE_DEPTHS: ',OPTION
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(TESTING) THEN
        C=1.D99
        CINIT=1.D99
        DO I=1,NPOIN
          C    =MIN(C    ,H%R(I))
          CINIT=MIN(CINIT,HN%R(I))
        ENDDO
        IF(NCSIZE.GT.1) THEN
          C=P_DMIN(C)
          CINIT=P_DMIN(CINIT)
        ENDIF
        WRITE(LU,*) 'AVANT TRAITEMENT HAUTEURS NEGATIVES, H MIN=',C
        WRITE(LU,*) 'AVANT TRAITEMENT HAUTEURS NEGATIVES, HN MIN=',CINIT
        C=0.D0
        CINIT=0.D0
        IF(NCSIZE.GT.1) THEN
          DO I=1,NPOIN
            C    =C    +H%R(I) *MESH%IFAC%I(I)/UNSV2D%R(I)
            CINIT=CINIT+HN%R(I)*MESH%IFAC%I(I)/UNSV2D%R(I)
          ENDDO
          C    =P_DSUM(C    )
          CINIT=P_DSUM(CINIT)
        ELSE
          DO I=1,NPOIN
            C    =C    +H%R(I) /UNSV2D%R(I)
            CINIT=CINIT+HN%R(I)/UNSV2D%R(I)
          ENDDO
        ENDIF
        WRITE(LU,*) 'AVANT TRAITEMENT MASSE INITIALE=',CINIT
        WRITE(LU,*) 'AVANT TRAITEMENT MASSE FINALE  =',C
      ENDIF
!
!     CALCUL DES FLUX PAR SEGMENT (T1 SUIVI DE FALSE NON UTILISE)
!     WHEN RECEIVED FLODEL IS NOT ASSEMBLED IN //
!
      IF(COMPUTE_FLODEL) THEN
!       SO FAR HARDCODED OPTION 2
        CALL FLUX_EF_VF(FLODEL%R,FLOPOINT,NSEG,NELEM,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.TRUE.,2)
      ENDIF
!
!     ASSEMBLING FLODEL IN PARALLEL
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2_SEG(FLODEL%R,FLODEL%R,FLODEL%R,
     &                   NSEG,1,2,1,MESH,1,11)
      ENDIF
!
      IF(OPTION.EQ.1) THEN
!
!       CHANGING FLUXES FROM POINTS INTO N FLUXES BETWEEN POINTS
        DO IELEM = 1,NELEM
          A1 = ABS(FLOPOINT(IELEM,1))
          A2 = ABS(FLOPOINT(IELEM,2))
          A3 = ABS(FLOPOINT(IELEM,3))
          IF(A1.GE.A2.AND.A1.GE.A3) THEN
!           ALL FLOW TO AND FROM NODE 1
            FLOPOINT(IELEM,1)=-FLOPOINT(IELEM,2)
            FLOPOINT(IELEM,2)=0.D0
!           FLOPOINT(IELEM,3)= UNCHANGED!
          ELSEIF(A2.GE.A1.AND.A2.GE.A3) THEN
!           ALL FLOW TO AND FROM NODE 2
!           FLOPOINT(IELEM,1)= UNCHANGED!
            FLOPOINT(IELEM,2)=-FLOPOINT(IELEM,3)
            FLOPOINT(IELEM,3)=0.D0
          ELSE
!           ALL FLOW TO AND FROM NODE 3
            FLOPOINT(IELEM,3)=-FLOPOINT(IELEM,1)
            FLOPOINT(IELEM,1)=0.D0
!           FLOPOINT(IELEM,2)= UNCHANGED!
          ENDIF
!         REMOVING FLUXES OF OPPOSITE SIGNS ON EITHER SIDES OF A SEGMENT
!         SEE SIMILAR ALGORITHM IN FLUX_EF_VF_3
!         IF THIS IS NOT DONE THE PERCENTAGE OF FLUX TRANSMITTED MAY BE
!         OUT OF THE RANGE [0,1] AFTER ASSEMBLY
!         SEGMENT 1
          ISEG1=MESH%ELTSEG%I(IELEM)
          IF(MESH%ORISEG%I(IELEM).EQ.1) THEN
            IF(FLODEL%R(ISEG1).GT.0.D0) THEN
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,1)=MAX(0.D0,FLOPOINT(IELEM,1))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,1)=MIN(FLODEL%R(ISEG1),FLOPOINT(IELEM,1))
            ELSE
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,1)=MIN(0.D0,FLOPOINT(IELEM,1))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,1)=MAX(FLODEL%R(ISEG1),FLOPOINT(IELEM,1))
            ENDIF
          ELSE
            IF(FLODEL%R(ISEG1).LT.0.D0) THEN
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,1)=MAX(0.D0,FLOPOINT(IELEM,1))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,1)=MIN(-FLODEL%R(ISEG1),FLOPOINT(IELEM,1))
            ELSE
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,1)=MIN(0.D0,FLOPOINT(IELEM,1))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,1)=MAX(-FLODEL%R(ISEG1),FLOPOINT(IELEM,1))
            ENDIF
          ENDIF
!         SEGMENT 2
          ISEG2=MESH%ELTSEG%I(IELEM+NELEM)
          IF(MESH%ORISEG%I(IELEM+NELEM).EQ.1) THEN
            IF(FLODEL%R(ISEG2).GT.0.D0) THEN
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,2)=MAX(0.D0,FLOPOINT(IELEM,2))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,2)=MIN(FLODEL%R(ISEG2),FLOPOINT(IELEM,2))
            ELSE
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,2)=MIN(0.D0,FLOPOINT(IELEM,2))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,2)=MAX(FLODEL%R(ISEG2),FLOPOINT(IELEM,2))
            ENDIF
          ELSE
            IF(FLODEL%R(ISEG2).LT.0.D0) THEN
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,2)=MAX(0.D0,FLOPOINT(IELEM,2))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,2)=MIN(-FLODEL%R(ISEG2),FLOPOINT(IELEM,2))
            ELSE
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,2)=MIN(0.D0,FLOPOINT(IELEM,2))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,2)=MAX(-FLODEL%R(ISEG2),FLOPOINT(IELEM,2))
            ENDIF
          ENDIF
!         SEGMENT 3
          ISEG3=MESH%ELTSEG%I(IELEM+2*NELEM)
          IF(MESH%ORISEG%I(IELEM+2*NELEM).EQ.1) THEN
            IF(FLODEL%R(ISEG3).GT.0.D0) THEN
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,3)=MAX(0.D0,FLOPOINT(IELEM,3))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,3)=MIN(FLODEL%R(ISEG3),FLOPOINT(IELEM,3))
            ELSE
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,3)=MIN(0.D0,FLOPOINT(IELEM,3))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,3)=MAX(FLODEL%R(ISEG3),FLOPOINT(IELEM,3))
            ENDIF
          ELSE
            IF(FLODEL%R(ISEG3).LT.0.D0) THEN
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,3)=MAX(0.D0,FLOPOINT(IELEM,3))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,3)=MIN(-FLODEL%R(ISEG3),FLOPOINT(IELEM,3))
            ELSE
!             WRONG SIGN, VALUE CANCELLED
              FLOPOINT(IELEM,3)=MIN(0.D0,FLOPOINT(IELEM,3))
!             TOO LARGE (THE OTHER SEGMENT MUST HAVE AN OPPOSITE SIGN)
              FLOPOINT(IELEM,3)=MAX(-FLODEL%R(ISEG3),FLOPOINT(IELEM,3))
            ENDIF
          ENDIF
        ENDDO
!
      ELSE
!
!       ON INTERFACE SEGMENTS, ONLY ONE OF THE TWO TWIN SEGMENTS
!       WILL RECEIVE THE TOTAL FLUX, THE OTHER WILL GET 0.
!
        IF(NCSIZE.GT.1) THEN
          CALL MULT_INTERFACE_SEG(FLODEL%R,MESH%NH_COM_SEG%I,
     &                            MESH%NH_COM_SEG%DIM1,
     &                            MESH%NB_NEIGHB_SEG,
     &                            MESH%NB_NEIGHB_PT_SEG%I,
     &                            MESH%LIST_SEND_SEG%I,NSEG)
        ENDIF
!
      ENDIF
!
      CALL CPSTVC(H,T2)
      CALL CPSTVC(H,T4)
      CALL CPSTVC(H,T1)
!
      DO I=1,NSEG
!       SAVING INITIAL FLODEL INTO FLULIM
        FLULIM(I)=FLODEL%R(I)
      ENDDO
!
!     ADDING THE SOURCES (SMH IS NATURALLY ASSEMBLED IN //)
!     FIRST THE POSITIVE SOURCES
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            H%R(I)=HN%R(I)+DT*MAX(SMH%R(I),0.D0)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            H%R(I)=HN%R(I)+DT*MAX(SMH%R(I),0.D0)*UNSV2D%R(I)
          ENDDO
        ENDIF
      ELSE
        DO I=1,NPOIN
          H%R(I)=HN%R(I)
        ENDDO
      ENDIF
!
!     BOUNDARY FLUXES : ADDING THE ENTERING (NEGATIVE) FLUXES
!     FIRST PUTTING FLBOR (BOUNDARY) IN T2 (DOMAIN)
!
      CALL OSDB( 'X=Y     ' ,T2,FLBOR,FLBOR,0.D0,MESH)
!     ASSEMBLING T2 (FLBOR IS NOT ASSEMBLED)
      IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
        H%R(I)=H%R(I)-DT*UNSV2D%R(I)*MIN(T2%R(I),0.D0)
      ENDDO
!
!     FOR OPTIMIZING THE LOOP ON ELEMENTS, ONLY ELEMENTS
!     WITH NON ZERO FLUXES WILL BE CONSIDERED, THIS LIST
!     WILL BE UPDATED. TO START WITH, ALL ELEMENTS IN THE LIST
!
      IF(OPTION.EQ.1) THEN
        REMAIN=NELEM
      ELSE
        REMAIN=NSEG
      ENDIF
!
      DO I=1,REMAIN
        INDIC_PDEPT(I)=I
      ENDDO
!
      CPREV=0.D0
      IF(OPTION.EQ.1) THEN
!       MAXIMUM INITIAL FLUX
        DO IR=1,NELEM
          CPREV=CPREV+ABS(FLOPOINT(IR,1))
     &               +ABS(FLOPOINT(IR,2))
     &               +ABS(FLOPOINT(IR,3))
        ENDDO
      ELSE
!       INITIAL SUM OF FLUXES
        DO I=1,NSEG
          CPREV=CPREV+ABS(FLODEL%R(I))
!         SAVING INITIAL FLODEL INTO FLULIM
          FLULIM(I)=FLODEL%R(I)
        ENDDO
      ENDIF
      IF(NCSIZE.GT.1) CPREV=P_DSUM(CPREV)
      IF(TESTING) WRITE(LU,*) 'INITIAL SUM OF FLUXES=',CPREV
      CINIT=CPREV
!
!     LOOP OVER THE LOOP OVER THE ELEMENTS
!
      NITER = 0
777   CONTINUE
      NITER = NITER + 1
!
      IF(OPTION.EQ.1) THEN
!
!       T4 IS THE EVOLUTION OF VOLUME, HERE INITIALISED TO 0
        CALL OS('X=0     ',X=T4)
!
!       LOOP OVER THE ELEMENTS
!
        NEWREMAIN=0
        C=0.D0
!
!       COMPUTING DEMAND (T1) AND OFFER (T3)
!
        CALL OS('X=0     ',X=T1)
        CALL OS('X=0     ',X=T3)
        DO IR=1,REMAIN
          I=INDIC_PDEPT(IR)
          I1=MESH%IKLE%I(I        )
          I2=MESH%IKLE%I(I  +NELEM)
          I3=MESH%IKLE%I(I+2*NELEM)
!         A PRIORI AVAILABLE VOLUMES
          VOL1=MESH%SURFAC%R(I)*H%R(I1)*TIERS
          VOL2=MESH%SURFAC%R(I)*H%R(I2)*TIERS
          VOL3=MESH%SURFAC%R(I)*H%R(I3)*TIERS
!         FLUXES FROM POINTS
          F1= FLOPOINT(I,1)-FLOPOINT(I,3)
          F2=-FLOPOINT(I,1)+FLOPOINT(I,2)
          F3=-FLOPOINT(I,2)+FLOPOINT(I,3)
!         DEMAND OR OFFER, COMPARED TO A PRIORI AVAILABLE VOLUMES
          IF(F1*DT.GT.VOL1) THEN
            T1%R(I1)=T1%R(I1)+F1*DT-VOL1
          ELSE
            T3%R(I1)=T3%R(I1)+MIN(VOL1,VOL1-F1*DT)
          ENDIF
          IF(F2*DT.GT.VOL2) THEN
            T1%R(I2)=T1%R(I2)+F2*DT-VOL2
          ELSE
            T3%R(I2)=T3%R(I2)+MIN(VOL2,VOL2-F2*DT)
          ENDIF
          IF(F3*DT.GT.VOL3) THEN
            T1%R(I3)=T1%R(I3)+F3*DT-VOL3
          ELSE
            T3%R(I3)=T3%R(I3)+MIN(VOL3,VOL3-F3*DT)
          ENDIF
        ENDDO
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(T1,2,MESH)
          CALL PARCOM(T3,2,MESH)
        ENDIF
!
        DO IR=1,REMAIN
!
          I=INDIC_PDEPT(IR)
!
          I1=MESH%IKLE%I(I        )
          I2=MESH%IKLE%I(I  +NELEM)
          I3=MESH%IKLE%I(I+2*NELEM)
!
!         A PRIORI AVAILABLE VOLUMES
!
          VOL1=MESH%SURFAC%R(I)*H%R(I1)*TIERS
          VOL2=MESH%SURFAC%R(I)*H%R(I2)*TIERS
          VOL3=MESH%SURFAC%R(I)*H%R(I3)*TIERS
!
!         FLUXES FROM POINTS
!
          F1= FLOPOINT(I,1)-FLOPOINT(I,3)
          F2=-FLOPOINT(I,1)+FLOPOINT(I,2)
          F3=-FLOPOINT(I,2)+FLOPOINT(I,3)
!
!         PRELIMINARY REDISTRIBUTION OF VOLUMES ACCORDING TO DEMAND AND OFFER
!
          IF(F1*DT.GT.VOL1) THEN
            IF(T1%R(I1).GT.T3%R(I1)) THEN
              VOL1=VOL1+(F1*DT-VOL1)*(T3%R(I1)/T1%R(I1))
            ELSE
              VOL1=F1*DT
            ENDIF
          ELSE
            IF(T3%R(I1).GT.T1%R(I1)) THEN
              VOL1=VOL1-MIN(VOL1,VOL1-F1*DT)*(T1%R(I1)/T3%R(I1))
            ELSE
              VOL1=MAX(F1,0.D0)*DT
            ENDIF
          ENDIF
          IF(F2*DT.GT.VOL2) THEN
            IF(T1%R(I2).GT.T3%R(I2)) THEN
              VOL2=VOL2+(F2*DT-VOL2)*(T3%R(I2)/T1%R(I2))
            ELSE
              VOL2=F2*DT
            ENDIF
          ELSE
            IF(T3%R(I2).GT.T1%R(I2)) THEN
              VOL2=VOL2-MIN(VOL2,VOL2-F2*DT)*(T1%R(I2)/T3%R(I2))
            ELSE
              VOL2=MAX(F2,0.D0)*DT
            ENDIF
          ENDIF
          IF(F3*DT.GT.VOL3) THEN
            IF(T1%R(I3).GT.T3%R(I3)) THEN
              VOL3=VOL3+(F3*DT-VOL3)*(T3%R(I3)/T1%R(I3))
            ELSE
              VOL3=F3*DT
            ENDIF
          ELSE
            IF(T3%R(I3).GT.T1%R(I3)) THEN
              VOL3=VOL3-MIN(VOL3,VOL3-F3*DT)*(T1%R(I3)/T3%R(I3))
            ELSE
              VOL3=MAX(F3,0.D0)*DT
            ENDIF
          ENDIF
!
          IF(F1*DT.GT.VOL1) THEN
            DT1=DT*(VOL1/(F1*DT))
          ELSE
            DT1=DT
          ENDIF
          IF(F2*DT.GT.VOL2) THEN
            DT2=DT*(VOL2/(F2*DT))
          ELSE
            DT2=DT
          ENDIF
          IF(F3*DT.GT.VOL3) THEN
            DT3=DT*(VOL3/(F3*DT))
          ELSE
            DT3=DT
          ENDIF
!
!         LIMITED VOLUMES TRANSITING BETWEEN POINTS (1/DT MISSING)
!
          DTLIM1=MIN(DT1,DT2)
          DTLIM2=MIN(DT2,DT3)
          DTLIM3=MIN(DT3,DT1)
!
          FP1=FLOPOINT(I,1)*DTLIM1
          FP2=FLOPOINT(I,2)*DTLIM2
          FP3=FLOPOINT(I,3)*DTLIM3
!
!         CORRESPONDING VARIATIONS OF VOLUMES OF POINTS (DT MISSING SO OK)
!
          T4%R(I1)=T4%R(I1)-( FP1-FP3)
          T4%R(I2)=T4%R(I2)-(-FP1+FP2)
          T4%R(I3)=T4%R(I3)-(-FP2+FP3)
!
!         IF REMAINING FLUXES, THE ELEMENT IS KEPT IN THE LIST
!
          IF(DTLIM1.EQ.DT.AND.DTLIM2.EQ.DT.AND.DTLIM3.EQ.DT) THEN
            FLOPOINT(I,1)=0.D0
            FLOPOINT(I,2)=0.D0
            FLOPOINT(I,3)=0.D0
          ELSE
            NEWREMAIN=NEWREMAIN+1
!           BEFORE NEWREMAIN: FOR NEXT ITERATION
!           AFTER  NEWREMAIN: STILL VALID FOR NEXT ITERATION
            INDIC_PDEPT(NEWREMAIN)=I
            FLOPOINT(I,1)=FLOPOINT(I,1)*(1.D0-DTLIM1*SURDT)
            FLOPOINT(I,2)=FLOPOINT(I,2)*(1.D0-DTLIM2*SURDT)
            FLOPOINT(I,3)=FLOPOINT(I,3)*(1.D0-DTLIM3*SURDT)
            C=C+ABS(FLOPOINT(I,1))+ABS(FLOPOINT(I,2))
     &         +ABS(FLOPOINT(I,3))
          ENDIF
!
        ENDDO
!
      ELSEIF(OPTION.EQ.2) THEN
!
        IF(NITER.EQ.1) THEN
          DO I=1,NPOIN
            T1%R(I)=0.D0
            T4%R(I)=H%R(I)
          ENDDO
        ELSE
!         NOT ALL THE POINTS NEED TO BE INITIALISED NOW
          DO IR=1,REMAIN
            I=INDIC_PDEPT(IR)
            I1=GLOSEG1(I)
            I2=GLOSEG2(I)
            T1%R(I1)=0.D0
            T1%R(I2)=0.D0
!           SAVING THE DEPTH
            T4%R(I1)=H%R(I1)
            T4%R(I2)=H%R(I2)
          ENDDO
!         CANCELLING INTERFACE POINTS (SOME MAY BE ISOLATED IN A SUBDOMAIN
!         AT THE TIP OF AN ACTIVE SEGMENT WHICH IS ELSEWHERE)
          IF(NCSIZE.GT.1) THEN
            DO IPTFR=1,NPTIR
              I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
              T1%R(I)=0.D0
!             SAVING THE DEPTH
              T4%R(I)=H%R(I)
            ENDDO
          ENDIF
        ENDIF
!
!       COMPUTING THE DEMAND FOR EVERY POINT
!       CANCELLING DEPTHS THAT WILL BE DISTRIBUTED TO ACTIVE SEGMENTS
!       I.E. AS SOON AS THERE IS A DEMAND
!       ANYWAY THEY ARE STORED IN T4 THAT WILL BE USED INSTEAD
!
        DO IR=1,REMAIN
          I=INDIC_PDEPT(IR)
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          IF(FLODEL%R(I).GT.EPS_FLUX) THEN
            T1%R(I1)=T1%R(I1)+FLODEL%R(I)
            H%R(I1)=0.D0
          ELSEIF(FLODEL%R(I).LT.-EPS_FLUX) THEN
            T1%R(I2)=T1%R(I2)-FLODEL%R(I)
            H%R(I2)=0.D0
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) THEN
!         DEMAND ASSEMBLED IN PARALLEL
          CALL PARCOM(T1,2,MESH)
!         FOR ISOLATED POINTS CONNECTED TO AN ACTIVE
!         SEGMENT THAT IS IN ANOTHER SUBDOMAIN
!         H MUST BE CANCELLED, IF NOT, IT IS SHARED
!         SO THAT IT IS FOUND AGAIN ONCE ASSEMBLED
          DO IPTFR=1,NPTIR
            I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
!           AT THIS LEVEL H IS THE SAME AT INTERFACE POINTS
!           NOW H IS SHARED BETWEEN PROCESSORS TO ANTICIPATE
!           THE FINAL PARALLEL ASSEMBLY
            IF(T1%R(I).GT.EPS_FLUX) THEN
!             POINT THAT WILL GIVE
              H%R(I)=0.D0
            ELSE
!             POINT THAT WILL ONLY RECEIVE
!             IN THIS CASE THEIR DEPTH WILL NOT BE DISTRIBUTED
!             IN THE LOOP ON SEGMENTS, IT IS LEFT UNCHANGED
!             H IS SHARED TO ANTICIPATE THE FURTHER PARCOM
              H%R(I)=H%R(I)*MESH%IFAC%I(I)
            ENDIF
          ENDDO
        ENDIF
!
        C=0.D0
        NEWREMAIN=0
!
!       TRANSFER OF FLUXES
!
        DO IR=1,REMAIN
          I=INDIC_PDEPT(IR)
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          IF(FLODEL%R(I).GT.EPS_FLUX) THEN
!           SHARING ON DEMAND
            HSEG1=T4%R(I1)*FLODEL%R(I)/T1%R(I1)
!           END OF SHARING ON DEMAND
            HFL1= DT*UNSV2D%R(I1)*FLODEL%R(I)
            IF(HFL1.GT.HSEG1) THEN
              TET=HSEG1/HFL1
              H%R(I2)=H%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)*TET
              FLODEL%R(I)=FLODEL%R(I)*(1.D0-TET)
              C=C+FLODEL%R(I)
              NEWREMAIN=NEWREMAIN+1
              INDIC_PDEPT(NEWREMAIN)=I
            ELSE
              H%R(I1)=H%R(I1)+HSEG1-HFL1
              H%R(I2)=H%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)
              FLODEL%R(I)=0.D0
            ENDIF
          ELSEIF(FLODEL%R(I).LT.-EPS_FLUX) THEN
!           SHARING ON DEMAND
            HSEG2=-T4%R(I2)*FLODEL%R(I)/T1%R(I2)
!           END OF SHARING ON DEMAND
            HFL2=-DT*UNSV2D%R(I2)*FLODEL%R(I)
            IF(HFL2.GT.HSEG2) THEN
              TET=HSEG2/HFL2
!             GATHERING DEPTHS
              H%R(I1)=H%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)*TET
              FLODEL%R(I)=FLODEL%R(I)*(1.D0-TET)
              C=C-FLODEL%R(I)
              NEWREMAIN=NEWREMAIN+1
              INDIC_PDEPT(NEWREMAIN)=I
            ELSE
!             GATHERING DEPTHS
              H%R(I1)=H%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)
              H%R(I2)=H%R(I2)+HSEG2-HFL2
              FLODEL%R(I)=0.D0
            ENDIF
          ENDIF
!
        ENDDO
!
      ENDIF
!
      REMAIN=NEWREMAIN
!
      IF(OPTION.EQ.1) THEN
!     ADDING THE EVOLUTIONS TO THE DEPTHS
!     AFTER ASSEMBLY AT INTERFACES AND AFTER
!     CHANGING VOLUMES INTO DEPTHS.
!
      IF(NCSIZE.GT.1) CALL PARCOM(T4,2,MESH)
        CALL OS('X=XY    ',X=T4,Y=UNSV2D)
        CALL OS('X=X+Y   ',X=H,Y=T4)
        DO I=1,H%DIM1
          H%R(I)=MAX(H%R(I),0.D0)
        ENDDO
      ELSEIF(OPTION.EQ.2) THEN
!       SUMMING THE NEW POSITIVE PARTIAL DEPTHS OF INTERFACE POINTS
        IF(NCSIZE.GT.1) CALL PARCOM(H,2,MESH)
      ENDIF
      IF(NCSIZE.GT.1) C=P_DSUM(C)
      IF(TESTING) WRITE(LU,*) 'FLUX NON PRIS EN COMPTE=',C
!
!     STOP CRITERION
!
      IF(C.NE.CPREV.AND.ABS(C-CPREV).GT.CINIT*1.D-9
     &             .AND.C.NE.0.D0) THEN
        CPREV=C
        IF(NITER.LT.NITMAX) GO TO 777
      ENDIF
!
!     BOUNDARY FLUXES : ADDING THE EXITING (POSITIVE) FLUXES
!                       WITH A POSSIBLE LIMITATION
!
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
!                               T2 = // ASSEMBLED FLBOR
        HFL1=DT*UNSV2D%R(I)*MAX(T2%R(I),0.D0)
        IF(HFL1.GT.H%R(I)) THEN
!         FLBOR ACTUALLY TAKEN INTO ACCOUNT
          FLBOR%R(IPTFR)=FLBOR%R(IPTFR)*H%R(I)/HFL1
          H%R(I)=0.D0
        ELSE
          H%R(I)=H%R(I)-HFL1
        ENDIF
        IF(LIMPRO(IPTFR).EQ.KDIR) THEN
          IF(HBOR(IPTFR).LT.0.D0) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'HAUTEUR NEGATIVE IMPOSEE A UNE FRONTIERE'
              WRITE(LU,*) 'VERIFIER VOTRE SOUS-PROGRAMME :'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'NEGATIVE DEPTH PRESCRIBED ON BOUNDARY'
              WRITE(LU,*) 'CHECK YOUR SPECIFIC SUBROUTINE:'
            ENDIF
            IF(NAMECODE(1:7).EQ.'SISYPHE') THEN
              WRITE(LU,*) 'BEDLOAD_SOLVS_FE'
            ELSEIF(NAMECODE(1:9).EQ.'TELEMAC2D') THEN
              WRITE(LU,*) 'BORD'
            ELSEIF(NAMECODE(1:9).EQ.'TELEMAC3D') THEN
              WRITE(LU,*) 'BORD3D'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
!         HERE WE WOULD NEED V2DPAR....
          FLBOR%R(IPTFR)=FLBOR%R(IPTFR)
     &                  +(H%R(I)-HBOR(IPTFR))/(DT*UNSV2D%R(I))
          H%R(I)= HBOR(IPTFR)
        ENDIF
      ENDDO
!
!     ADDING THE SOURCES (SMH IS NATURALLY ASSEMBLED IN //)
!     NOW THE NEGATIVE SOURCES
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            H%R(I)=H%R(I)+DT*MIN(SMH%R(I),0.D0)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            H%R(I)=H%R(I)+DT*MIN(SMH%R(I),0.D0)*UNSV2D%R(I)
          ENDDO
        ENDIF
      ENDIF
!
      IF(TESTING) THEN
        C=1.D99
        DO I=1,NPOIN
          C=MIN(C,H%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_DMIN(C)
        WRITE(LU,*) 'APRES TRAITEMENT HAUTEURS NEGATIVES, HMIN=',C
        C=0.D0
        IF(NCSIZE.GT.1) THEN
          DO I=1,NPOIN
            C=C+H%R(I)*MESH%IFAC%I(I)/UNSV2D%R(I)
          ENDDO
          C=P_DSUM(C)
        ELSE
          DO I=1,NPOIN
            C=C+H%R(I)/UNSV2D%R(I)
          ENDDO
        ENDIF
        WRITE(LU,*) 'APRES TRAITEMENT MASSE FINALE =',C
      ENDIF
!
!-----------------------------------------------------------------------
!
!     NOW WE WANT:
!     FLULIM TO BE THE PERCENTAGE OF FLUX THAT HAS BEEN TRANSMITTED
!     FLODEL TO BE THE ACTUAL FLUX THAT HAS BEEN TRANSMITTED
!
!     WE START WITH FLULIM=THE ORIGINAL FLODEL
!
!     WORKING ON ASSEMBLED VALUES TO FIND AN AVERAGE FLULIM
!     THAT WILL BE THE SAME AT INTERFACES
!
!     ASSEMBLING THE REMAINING FLUXES IN FLODEL
!
      IF(OPTION.EQ.1) THEN
!
        DO I=1,NSEG
          FLODEL%R(I)=0.D0
        ENDDO
        DO I=1,NELEM
          ISEG=MESH%ELTSEG%I(I)
          IF(  MESH%ORISEG%I(I).EQ.1) THEN
            FLODEL%R(ISEG)=FLODEL%R(ISEG)+FLOPOINT(I,1)
          ELSE
            FLODEL%R(ISEG)=FLODEL%R(ISEG)-FLOPOINT(I,1)
          ENDIF
          ISEG=MESH%ELTSEG%I(I+NELEM)
          IF(  MESH%ORISEG%I(I+NELEM).EQ.1) THEN
            FLODEL%R(ISEG)=FLODEL%R(ISEG)+FLOPOINT(I,2)
          ELSE
            FLODEL%R(ISEG)=FLODEL%R(ISEG)-FLOPOINT(I,2)
          ENDIF
          ISEG=MESH%ELTSEG%I(I+2*NELEM)
          IF(  MESH%ORISEG%I(I+2*NELEM).EQ.1) THEN
            FLODEL%R(ISEG)=FLODEL%R(ISEG)+FLOPOINT(I,3)
          ELSE
            FLODEL%R(ISEG)=FLODEL%R(ISEG)-FLOPOINT(I,3)
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM2_SEG(FLODEL%R,FLODEL%R,FLODEL%R,
     &                     NSEG,1,2,1,MESH,1,11)
!         FLULIM IS THE ORIGINAL FLODEL THAT HAS BEEN ASSEMBLED
!         CALL PARCOM2_SEG(FLULIM,FLULIM,FLULIM,
!    &                     NSEG,1,2,1,MESH,1,11)
        ENDIF
!
        DO I=1,NSEG
!         ACTUAL FLUX TRANSMITTED (=ORIGINAL-REMAINING)
          FLODEL%R(I)=FLULIM(I)-FLODEL%R(I)
!         PERCENTAGE OF ACTUAL FLUX WITH RESPECT TO ORIGINAL FLUX
          IF(ABS(FLULIM(I)).GT.EPS_FLUX) THEN
            FLULIM(I)=FLODEL%R(I)/FLULIM(I)
          ELSE
            FLULIM(I)=0.D0
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) THEN
!         SHARING AGAIN FLODEL FOR FURTHER USES
!         ON INTERFACE SEGMENTS, ONLY ONE OF THE TWO TWIN SEGMENTS
!         WILL RECEIVE THE TOTAL FLUX, THE OTHER WILL GET 0.
          CALL MULT_INTERFACE_SEG(FLODEL%R,MESH%NH_COM_SEG%I,
     &                            MESH%NH_COM_SEG%DIM1,
     &                            MESH%NB_NEIGHB_SEG,
     &                            MESH%NB_NEIGHB_PT_SEG%I,
     &                            MESH%LIST_SEND_SEG%I,NSEG)
        ENDIF
!
      ELSEIF(OPTION.EQ.2) THEN
!
!       NOW WE WANT:
!       FLULIM TO BE THE PERCENTAGE OF FLUX THAT HAS BEEN TRANSMITTED
!       FLODEL TO BE THE ACTUAL FLUX THAT HAS BEEN TRANSMITTED
!
!       WE START WITH FLULIM=THE ORIGINAL FLODEL
!
!       WORKING ON ASSEMBLED VALUES TO FIND AN AVERAGE FLULIM
!       THAT WILL BE THE SAME AT INTERFACES
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM2_SEG(FLODEL%R,FLODEL%R,FLODEL%R,
     &                     NSEG,1,2,1,MESH,1,11)
          CALL PARCOM2_SEG(FLULIM,FLULIM,FLULIM,
     &                     NSEG,1,2,1,MESH,1,11)
        ENDIF
!
        DO I=1,NSEG
!         ACTUAL FLUX TRANSMITTED (=ORIGINAL-REMAINING)
          FLODEL%R(I)=FLULIM(I)-FLODEL%R(I)
!         PERCENTAGE OF ACTUAL FLUX WITH RESPECT TO ORIGINAL FLUX
          IF(ABS(FLULIM(I)).GT.EPS_FLUX) THEN
            FLULIM(I)=FLODEL%R(I)/FLULIM(I)
          ELSE
            FLULIM(I)=0.D0
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) THEN
!         SHARING AGAIN FLODEL FOR FURTHER USES
!         ON INTERFACE SEGMENTS, ONLY ONE OF THE TWO TWIN SEGMENTS
!         WILL RECEIVE THE TOTAL FLUX, THE OTHER WILL GET 0.
          CALL MULT_INTERFACE_SEG(FLODEL%R,MESH%NH_COM_SEG%I,
     &                            MESH%NH_COM_SEG%DIM1,
     &                            MESH%NB_NEIGHB_SEG,
     &                            MESH%NB_NEIGHB_PT_SEG%I,
     &                            MESH%LIST_SEND_SEG%I,NSEG)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CHECKING: COMPARING H(N+1) WITH H RECONSTRUCTED WITH THE FLUXES
!               SOURCES LACKING...
!
      IF(TESTING) THEN
        DO I=1,NPOIN
          T1%R(I)=0.D0
        ENDDO
        DO I=1,NSEG
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          T1%R(I1)=T1%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)
          T1%R(I2)=T1%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)
        ENDDO
        DO IPTFR=1,NPTFR
          I=NBOR(IPTFR)
          T1%R(I)=T1%R(I)-DT*UNSV2D%R(I)*FLBOR%R(IPTFR)
        ENDDO
        IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
        DO I=1,NPOIN
          T1%R(I)=T1%R(I)+HN%R(I)-H%R(I)
        ENDDO
        WRITE(LU,*) 'ERREUR POSITIVE_DEPTHS=',P_DOTS(T1,T1,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(INFO) THEN
        IF(NAMECODE(1:7).EQ.'SISYPHE') THEN
          IF(LNG.EQ.1) WRITE(LU,101) NITER
          IF(LNG.EQ.2) WRITE(LU,102) NITER
        ELSE
          IF(LNG.EQ.1) WRITE(LU,201) NITER
          IF(LNG.EQ.2) WRITE(LU,202) NITER
        ENDIF
      ENDIF
!
101   FORMAT(' EQUATION DE CHARRIAGE RESOLUE EN ',1I5,' ITERATIONS')
102   FORMAT(' BEDLOAD EQUATION SOLVED IN ',1I5,' ITERATIONS')
201   FORMAT(' HAUTEURS POSITIVES OBTENUES EN ',1I5,' ITERATIONS')
202   FORMAT(' POSITIVE DEPTHS OBTAINED IN ',1I5,' ITERATIONS')
!
!-----------------------------------------------------------------------
!
      RETURN
      END

