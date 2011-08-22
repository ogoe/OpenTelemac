!                    **************************
                     SUBROUTINE POSITIVE_DEPTHS
!                    **************************
!
     &(T1,T2,T3,T4,H,HN,MESH,FLODEL,COMPUTE_FLODEL,FLBOR,DT,
     & UNSV2D,NPOIN,GLOSEG1,GLOSEG2,NBOR,NPTFR,YAFLODEL,
     & SMH,YASMH,OPTSOU,FLULIM,LIMPRO,HBOR,KDIR,INFO,FLOPOINT,
     & NAMECODE,OPTION)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COMPUTE_FLODEL |-->| IF YES, COMPUTE FLODEL WITH FLOPOINT
!| DT             |-->| TIME STEP
!| FLBOR          |<->| BOUNDARY FLUXES
!| FLODEL         |<->| FLUXES GIVEN BY SEGMENT
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
!| YAFLODEL       |-->| IF(YES) FLUXES IN FLODEL WILL NOT BE
!|                |   | INITIALISED BY TEL4DEL (THUS WE PUT HERE
!|                |   | THE DISCARDED FLUXES WITH A MINUS SIGN)
!|                |   | AND TEL4DEL WILL ADD THE COMPLETE FLUXES.
!| YASMH          |-->| IF(YES) SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_POSITIVE_DEPTHS => POSITIVE_DEPTHS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,OPTSOU,KDIR,OPTION
      INTEGER, INTENT(IN)             :: GLOSEG1(*),GLOSEG2(*)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,FLOPOINT(NPOIN),HBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(*)
      TYPE(BIEF_MESH),INTENT(INOUT)   :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,FLODEL,H,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: UNSV2D,HN,SMH
      LOGICAL, INTENT(IN)             :: YAFLODEL,YASMH,INFO
      LOGICAL, INTENT(IN)             :: COMPUTE_FLODEL
      CHARACTER(LEN=24)               :: NAMECODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION P_DSUM,P_DMIN
      EXTERNAL         P_DSUM,P_DMIN
!
      INTEGER I,I1,I2,IPTFR,IOPT1,REMAIN_SEG,NEWREMAIN,IR,NITER
      DOUBLE PRECISION C,CPREV,CINIT,TET,HFL1,HFL2,HSEG1,HSEG2
!
      DOUBLE PRECISION EPS_FLUX
      DATA             EPS_FLUX/1.D-15/
      LOGICAL TESTING
      DATA TESTING/.FALSE./
      INTEGER NITMAX
      DATA NITMAX/1000/
!
!-----------------------------------------------------------------------
!
!     INDIC WILL BE A LIST OF SEGMENTS WITH NON ZERO FLUXES
!     HSEG IS THE DEPTH SHARED BETWEEN SEGMENTS
!
      LOGICAL DEJA
      DATA DEJA/.FALSE./
      INTEGER,          ALLOCATABLE :: INDIC(:)
      SAVE
      IF(.NOT.DEJA) THEN
        ALLOCATE(INDIC(MESH%NSEG))
        DEJA=.TRUE.
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
      ENDIF
!
!     CALCUL DES FLUX PAR SEGMENT (T1 SUIVI DE FALSE NON UTILISE)
!     FLODEL IS NOT ASSEMBLED IN //
!
      IF(COMPUTE_FLODEL) THEN
!       SO FAR HARDCODED OPTION
        IOPT1=2
        CALL FLUX_EF_VF(FLODEL%R,FLOPOINT,MESH%NSEG,MESH%NELEM,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.TRUE.,IOPT1)
      ENDIF
!
!     AVERAGING FLUXES ON INTERFACE SEGMENTS BY ASSEMBLING AND
!     DIVIDING BY 2. THIS IS NOT MANDATORY HERE BUT WILL GIVE
!     THE UPWINDING INFORMATION FOR TRACERS, AND THEY MUST BE TREATED
!     IN THE SAME WAY
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2_SEG(FLODEL%R,FLODEL%R,FLODEL%R,
     &                   MESH%NSEG,1,2,1,MESH,1)
        CALL MULT_INTERFACE_SEG(FLODEL%R,MESH%NH_COM_SEG%I,
     &                          MESH%NH_COM_SEG%DIM1,
     &                          MESH%NB_NEIGHB_SEG,
     &                          MESH%NB_NEIGHB_PT_SEG%I,
     &                          0.5D0,MESH%NSEG)
      ENDIF
!
      CALL CPSTVC(H,T2)
!
      CPREV=0.D0
      DO I=1,MESH%NSEG
        CPREV=CPREV+ABS(FLODEL%R(I))
!       SAVING INITIAL FLODEL INTO FLULIM
        FLULIM(I)=FLODEL%R(I)
      ENDDO
      IF(NCSIZE.GT.1) CPREV=P_DSUM(CPREV)
      CINIT=CPREV
      IF(TESTING) WRITE(LU,*) 'SOMME INITIALE DES FLUX=',CPREV
!
!     BOUCLE SUR LES SEGMENTS, POUR PRENDRE EN COMPTE LES FLUX
!     ADMISSIBLES
!
!     ADDING THE SOURCES (SMH IS NATURALLY ASSEMBLED IN //)
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            H%R(I)=HN%R(I)+DT*SMH%R(I)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            H%R(I)=HN%R(I)+DT*SMH%R(I)*UNSV2D%R(I)
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
      CALL OSDB( 'X=Y     ' ,T2,FLBOR,FLBOR,0.D0,MESH)
!     ASSEMBLING T2 (FLBOR IS NOT ASSEMBLED)
      IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
        H%R(I)=H%R(I)-DT*UNSV2D%R(I)*MIN(T2%R(I),0.D0)
      ENDDO
!
!     FOR OPTIMIZING THE LOOP ON SEGMENTS, ONLY SEGMENTS
!     WITH NON ZERO FLUXES WILL BE CONSIDERED, THIS LIST
!     WILL BE UPDATED. TO START WITH, ALL FLODEL ASSUMED NON ZERO
!
      REMAIN_SEG=MESH%NSEG
      DO I=1,REMAIN_SEG
        INDIC(I)=I
      ENDDO
!
      NITER = 0
777   CONTINUE
      NITER = NITER + 1
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FOR DISTRIBUTING THE DEPTHS BETWEEN SEGMENTS
!
      IF(OPTION.EQ.2) THEN
!
!       T1 : TOTAL FLUX REMOVED OF EACH POINT
!       T4 : DEPTH H SAVED
!
        CALL CPSTVC(H,T1)
        IF(NITER.EQ.1) THEN
          DO I=1,NPOIN
            T1%R(I)=0.D0
            T4%R(I)=H%R(I)
          ENDDO
        ELSE
!         NOT ALL THE POINTS NEED TO BE INITIALISED NOW
          DO IR=1,REMAIN_SEG
            I=INDIC(IR)
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
!       COMPUTING DEMAND FOR EVERY POINT
!       CANCELLING DEPTHS THAT WILL BE DISTRIBUTED TO ACTIVE SEGMENTS
!       I.E. AS SOON AS THERE IS A DEMAND
!       ANYWAY THEY ARE STORED IN T4 THAT WILL BE USED INSTEAD
        DO IR=1,REMAIN_SEG
          I=INDIC(IR)
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
          CALL PARCOM(T1,2,MESH)
!         FOR ISOLATED POINTS CONNECTED TO AN ACTIVE
!         SEGMENT THAT IS IN ANOTHER SUBDOMAIN
!         H MUST BE CANCELLED, IF NOT, IT IS SHARED
          DO IPTFR=1,NPTIR
            I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
!           AT THIS LEVEL H IS THE SAME AT INTERFACE POINTS
!           NOW H IS SHARED BETWEEN PROCESSORS TO ANTICIPATE
            IF(T1%R(I).GT.EPS_FLUX) THEN
              H%R(I)=0.D0
            ELSE
              H%R(I)=H%R(I)*MESH%FAC%R(I)
            ENDIF
          ENDDO
        ENDIF
!
      ELSEIF(OPTION.EQ.1) THEN
!
!       AT THIS LEVEL H IS THE SAME AT INTERFACE POINTS
!       NOW H IS SHARED BETWEEN PROCESSORS TO ANTICIPATE
!       A FURTHER PARCOM
        IF(NCSIZE.GT.1) THEN
          DO IPTFR=1,NPTIR
!           AVAILABLE DEPTH IS SHARED BETWEEN PROCESSORS
!           NACHB(1,IPTFR) WITH DIMENSION NACHB(NBMAXNSHARE,NPTIR)
            I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
            H%R(I)=H%R(I)*MESH%FAC%R(I)
          ENDDO
        ENDIF
!
      ENDIF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      C=0.D0
      NEWREMAIN=0
!
      IF(OPTION.EQ.1) THEN
!
      DO IR=1,REMAIN_SEG
        I=INDIC(IR)
        IF(FLODEL%R(I).GT.EPS_FLUX) THEN
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          HFL1= DT*UNSV2D%R(I1)*FLODEL%R(I)
          IF(HFL1.GT.H%R(I1)) THEN
            TET=H%R(I1)/HFL1
            H%R(I1)=0.D0
            H%R(I2)=H%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)*TET
            FLODEL%R(I)=FLODEL%R(I)*(1.D0-TET)
            C=C+FLODEL%R(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ELSE
            H%R(I1)=H%R(I1)-HFL1
            H%R(I2)=H%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)
            FLODEL%R(I)=0.D0
          ENDIF
        ELSEIF(FLODEL%R(I).LT.-EPS_FLUX) THEN
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          HFL2=-DT*UNSV2D%R(I2)*FLODEL%R(I)
          IF(HFL2.GT.H%R(I2)) THEN
            TET=H%R(I2)/HFL2
            H%R(I1)=H%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)*TET
            H%R(I2)=0.D0
            FLODEL%R(I)=FLODEL%R(I)*(1.D0-TET)
            C=C-FLODEL%R(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ELSE
            H%R(I1)=H%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)
            H%R(I2)=H%R(I2)-HFL2
            FLODEL%R(I)=0.D0
          ENDIF
        ENDIF
      ENDDO
!
      ELSEIF(OPTION.EQ.2) THEN
!
      DO IR=1,REMAIN_SEG
        I=INDIC(IR)
        I1=GLOSEG1(I)
        I2=GLOSEG2(I)
        IF(FLODEL%R(I).GT.EPS_FLUX) THEN
!         SHARING ON DEMAND
          HSEG1=T4%R(I1)*FLODEL%R(I)/T1%R(I1)
!         END OF SHARING ON DEMAND
          HFL1= DT*UNSV2D%R(I1)*FLODEL%R(I)
          IF(HFL1.GT.HSEG1) THEN
            TET=HSEG1/HFL1
            H%R(I2)=H%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)*TET
            FLODEL%R(I)=FLODEL%R(I)*(1.D0-TET)
            C=C+FLODEL%R(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ELSE
            H%R(I1)=H%R(I1)+HSEG1-HFL1
            H%R(I2)=H%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)
            FLODEL%R(I)=0.D0
          ENDIF
        ELSEIF(FLODEL%R(I).LT.-EPS_FLUX) THEN
!         SHARING ON DEMAND
          HSEG2=-T4%R(I2)*FLODEL%R(I)/T1%R(I2)
!         END OF SHARING ON DEMAND
          HFL2=-DT*UNSV2D%R(I2)*FLODEL%R(I)
          IF(HFL2.GT.HSEG2) THEN
            TET=HSEG2/HFL2
!           GATHERING DEPTHS
            H%R(I1)=H%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)*TET
            FLODEL%R(I)=FLODEL%R(I)*(1.D0-TET)
            C=C-FLODEL%R(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ELSE
!           GATHERING DEPTHS
            H%R(I1)=H%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)
            H%R(I2)=H%R(I2)+HSEG2-HFL2
            FLODEL%R(I)=0.D0
          ENDIF
        ENDIF
      ENDDO
!
!     ELSE
!       UNKNOWN OPTION
      ENDIF
!
      REMAIN_SEG=NEWREMAIN
!
!     SUMMING THE NEW POSITIVE PARTIAL DEPTHS OF INTERFACE POINTS
      IF(NCSIZE.GT.1) CALL PARCOM(H,2,MESH)
!
      IF(NCSIZE.GT.1) C=P_DSUM(C)
      IF(TESTING) WRITE(LU,*) 'FLUX NON PRIS EN COMPTE=',C
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
              WRITE(LU,*) 'VERIFIER VOTRE SOUS-PROGRAMME BORD3D'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'NEGATIVE DEPTH PRESCRIBED ON BOUNDARY'
              WRITE(LU,*) 'CHECK YOUR SPECIFIC SUBROUTINE BORD3D'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          FLBOR%R(IPTFR)=FLBOR%R(IPTFR)
     &                  +(H%R(I)-HBOR(IPTFR))/(DT*UNSV2D%R(I))
          H%R(I)= HBOR(IPTFR)
        ENDIF
      ENDDO
!
      IF(TESTING) THEN
        C=1.D99
        DO I=1,NPOIN
          C=MIN(C,H%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_DMIN(C)
        WRITE(LU,*) 'APRES TRAITEMENT HAUTEURS NEGATIVES, HMIN=',C
      ENDIF
!
!-----------------------------------------------------------------------
!
!     NOW PERCENTAGE OF FLUX THAT HAS BEEN TRANSMITTED
!     NOTE: FLULIM MAY BE DIFFERENT ON EITHER SIDE OF AN INTERFACE
!           IN PARALLEL MODE (BUT FLUXES CORRECTED WITH FLULIM
!           WILL BE AVERAGED SO THIS IS NOT A PROBLEM)
!
      DO I=1,MESH%NSEG
        IF(ABS(FLULIM(I)).GT.EPS_FLUX) THEN
          FLULIM(I)=(FLULIM(I)-FLODEL%R(I))/FLULIM(I)
        ELSE
          FLULIM(I)=0.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     FLUXES CORRESPONDING TO LIMITATION ARE KEPT IN FLODEL
!     THEY WILL BE THE INITIAL VALUE IN TEL4DEL
!
      IF(YAFLODEL) THEN
        DO I=1,MESH%NSEG
          FLODEL%R(I)=-FLODEL%R(I)
        ENDDO
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
