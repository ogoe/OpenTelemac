!                       *****************
                        SUBROUTINE RESOLU
!                       *****************
!
     & (W,FLUSCE,NUBO,VNOIN,WINF,AT,DT,LT,
     &  NELEM,NSEG,NPTFR,FLUX,AIRS,AIRE,
     &  X,Y,IKLE,ZF,CF,NPOIN,HN,H,U,V,QU,QV,G,LISTIN,
     &  XNEBOR,YNEBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL,
     &  HBOR,UBOR,VBOR,FLUSORT,FLUENT,CFLWTD,DTVARI,NELMAX,KFROT,
     &  NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,YASMH,SMH,MASSES,
     &  NTRAC,DIMT,T,HTN,TN,DLIMT,
     &  TBOR,MASSOU,FLUTENT,FLUTSOR,DTHAUT,DPX,DPY,DJX,DJY,CMI,JMI,
     &  SMTR,DXT,DYT,DJXT,DJYT,
     &  DIFVIT,ITURB,PROPNU,DIFT,DIFNU,
     &  DX,DY,OPTVF,FLUSORTN,FLUENTN,
     &  DSZ,AIRST,HSTOK,HCSTOK,FLUXT,FLUHBOR,FLBOR,
     &  LOGFR,LTT,DTN,FLUXTEMP,FLUHBTEMP,
     &  HC,TMAX,DTT,T1,T2,T3,T4,T5,
     &  GAMMA,FLUX_OLD,NVMAX,NEISEG,ELTSEG,IFABOR,HROPT,MESH,
     &  RAIN,PLUIE,MASS_RAIN,BILMAS,FU,FV,YACORIOL,CORIOLIS,YASMO,
     &  SPHERIC,COSLAT,SINLAT)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    1. SOLVES THE PROBLEM BY A METHOD OF TYPE ROE OR BY A
!         KINETIC SCHEME (ORDER 1 OR 2) OR
!        TCHAMEN/ZOKAGOA SCHEMES (ORDER 1)
!        HLLC (ORDER1) OR WAF (ORDER2 IN TIME AND SPACE)
!            FOR INTERIOR FLUXES
!            AND OF TYPE STEGER AND WARMING FOR I/O;
!+
!+
!+            2. SOLVES IN TIME USING A NEWMARK TYPE SCHEME OF SECOND ORDER.
!
!history  N.GOUTAL; INRIA
!+        22/03/1998
!+
!+   ROE SCHEME (NG); KINETIC SCHEMES (INRIA)
!
!history  J-M HERVOUET (LNHE)
!+        05/09/2007
!+
!+   MULTIPLE TRACERS
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
!history  R. ATA (EDF-LNHE)
!+        03/15/2011
!+        V6P1
!+    CHANGE EXPLICIT EULER BY NEWMARK SCHEME
!+    ADD TCHAMEN AND ZOKAGOA FLUXES
!
!history  R. ATA (EDF-LNHE)
!+        07/15/2012
!+        V6P2
!+    ADD HLLC AND WAF FLUXES
!
!history  R. ATA (EDF-LNHE)
!+
!+        01/07/2013
!+        V6P3
!+      adaptation with the new data structure (common with FEM)
!+      remove unused variables
!+      parallel version
!
!history  R. ATA
!+        28/01/2014
!+        V7P0
!+    change diemensions of CMI
!+    from (2,nseg) to (nseg,2)
!
! history S.PAVAN
!+        02/05/2014
!+        V7P0
!+    Initialization of flux_old
!+    for kinetic schemes
!
! history R. ATA (EDF R&D-LNHE)
!+        20/06/2014
!+        V7P0
!+    change winf values which are directly
!+    obtained by bord
!+    add parcom_bord after cdl routines
!+    change cdl routines to exactly impose boundary conditions
!+    initiliaze QU,QV and Hn
!+
! history R. ATA (EDF R&D-LNHE)
!+        20/01/2015
!+        V7P0
!+    correction for parallelization
!+    parcom_bord removed and parcom placed
!+    after cdl of each scheme
!
!history  R. ATA
!+        25/12/2016
!+        V7P2
!+    include rain and evaporation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AIRE           |-->| ELEMENT AREA
!| AIRS           |-->| CELL AREA
!| AIRST          |-->| AREA OF SUB-TRIANGLES (SECOND ORDER)
!| AT,DT,LT       |-->| TIME, TIME STEP AND NUMBER OF THE STEP
!| BILMAS         |-->| LOGICAL TRIGGERING A MASS BALANCE INFORMATION
!| CF             |-->| FRICTION COEFFICIENT
!| CFLWTD         |-->| WANTED CFL NUMBER
!| CMI            |-->| COORDINATES OF MIDDLE PONTS OF EDGES
!| CORIOLIS       |-->| CORIOLIS COEFFICIENT
!| COSLAT         |-->| COSINUS OF LATITUDE (SPHERICAL COORDINATES)
!| DIFNU          |-->| COEFFICIENT OF DIFFUSION FOR TRACER
!| DIFT           |-->| LOGICAL: DIFFUSION FOR TRACER OR NOT
!| DIFVIT         |-->| LOGICAL: DIFFUSION FOR VELOCITY OR NOT
!| DIMT           |-->| DIMENSION OF TRACER
!| DJXT,DJYT      |---| WORKING TABLES FOR TRACER
!| DLIMT          |-->| DIMENSION OF TRACER ON THE BOUNDARY
!| DSZ            |<->| VARIATION OF Z FOR ORDER 2
!| DTHAUT         |-->| CHARACTERISTIC LENGTH (DX) USED FOR CFL
!| DTN            |<->| TIME STEP   FROM TN+1 TO TN+2
!| DTT            |<->| TIME STEP FOR TRACER
!| DTVARI         |-->| DT VARIALE OR NOT
!| DX,DY          |---| WORKING TABLES
!| DXT,DYT        |---| WORKING TABLES FOR TRACER
!| FLUENT,FLUSORT |<--| MASS FLUX INLET AND OUTLET FROM TN TO TN+1
!| FLUHBTEMP      |<->| BORD FLUX FOR TRACER
!| FLUSCE         |-->| SOURCE FLUXES
!| FLUSORTN,FLUENT|<->| MASS FLUX INLET AND OUTLET FROM TN+1 TO TN+2
!| FLUTENT,FLUTSOR|<--| FLUX TRACER INLET AND OUTLET
!| FLUX           |---| FLUX
!| FLUXT,FLUHBOR  |<->| FLUX, FLUX BORD FOR TRACER
!| FLUXTEMP       |<->| FLUX FOR TRACER
!| FU,FV          |-->|  SOURCE TERMS FOR MOMENTUM (ON X AND Y)
!| G              |-->| GRAVITY
!| H              |<--| WATER DEPTH AT TIME N+1
!| HBOR           |-->| IMPOSED VALUE FOR H
!| HC             |<->| H RECONSTRUCTED (ORDER 2) CORRECTED
!| HN             |-->| WATER DEPTH AT TIME N
!| HSTOK,HCSTOK   |<->| H, H CORRECTED TO STOCK FOR TRACER
!| HTN,TN         |-->| HT, T  AT TIME N
!| HROPT          |-->| OPTION FOR HYDROSTATIC RECONSTRUCTION:
!|                |   | 1:AUDUSSE, 2: NOELLE
!| IKLE           |-->| INDICES OF NODES FOR TRIANGLE
!| ISCE           |-->| SOURCE POINTS
!| ITURB          |-->| MODEL OF TURBULENCE  1 : LAMINAIRE
!| JMI            |-->| NUMBER OF THE TRIANGLE IN WHICH IS LOCATED
!|                |   | THE MIDPOINT OF THE INTERFACE
!| KDDL           |-->| CONVENTION FOR FREE POINTS (BC)
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINTS
!| KFROT          |-->| BED FRICTION LAW
!| KNEU           |-->| CONVENTION NEUMANN POINTS
!| LIMPRO         |-->| TYPES OF BOUNDARY CONDITION
!| LISTIN         |-->| IF YES, PRINT MESSAGES AT LISTING.
!| LOGFR          |<->| REFERENCE OF BOUNDARY NODES
!| LT             |-->| NUMBER OF TIME STEP
!| LTT            |<->| NUMBER OF TIME STEP FOR TRACER
!| MASSES         |<--| ADDED MASS BY SOURCE TERMS
!| MASSOU         |<--| ADDED TRACER MASS BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NBOR           |-->| GLOBAL INDICES FOR BORD NODES
!| NB_NEIGHB      |-->| NUMBER OF NEIGHBORING SUBDOMAINS(SHARING POINTS)
!| NEISEG         |-->| NEIGHBORS OF SEGMENT (FOR LIMITER)
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NIT            |-->| TOTAL NUMBER OF TIME STEPS
!| NPOIN          |-->| TOTAL NUMBER OF NODES
!| NPTFR          |-->| TOTAL NUMBER OF BOUNDARY NODES
!| NREJET         |-->| NUMBER OF SOURCE/SINK
!| NSEG           |-->| NUMBER OF EDGES
!| NTRAC          |-->| NUMBER OF TRACERS
!| NUBO           |-->| GLOBAL INDICES OF EDGE EXTREMITIES
!| MVMAX          |-->| MAX NUMBER OF NEIGHBOR FOR A NODE
!| MASS_RAIN      |-->| MASS ADDED BY RAIN OR EVAPORATION
!| OPTVF          |-->| OPTION OF THE SCHEME
!|                |   | 0:ROE, 1:KINETIC ORDRE 1,2:KINETIC ORDRE 2
!|                |   | 3:ZOKAGOA, 4:TCHAMEN,4:HLLC,5:WAF
!| PLUIE          |-->| RAIN
!| PROPNU         |-->| COEFFICIENT OF MOLECULAR DIFFUSION
!| QU,QV          |<->| FLOW COMPOENENTS AT TIME N THEN AT TIME  N+1
!| SMH            |-->| SOURCE TERMS FOR CONTINUITY EQUATION
!| RAIN           |-->| IF YES TAKE RAIN INTO ACCOUNT
!| SINLAT         |-->| SINUS OF LATITUDE (SPHERICAL COORDINATES)
!| SPHERIC        |-->| IF TRUE : SPHERICAL COORDINATES
!| SMTR           |---| SOURCE TERMS FOR TRACEUR
!| T              |<--| TRACER UPDATED
!| T1,T2,T3,T4,T5 |---| WORKING TABLES
!| TBOR           |-->| BC FOR T
!| TMAX           |-->| FINAL TIME
!| TSCE2          |---|
!| U,V            |<--| VELOCITY COMPONENTS AT TIME N+1
!| UBOR           |-->| IMPOSED VALUES FOR U
!| VBOR           |-->| IMPOSED VALUES FOR V
!| VNOIN          |-->| NORMAL TO THE INTERFACE
!|                |   | (2 FIRS COMPOSANTES) AND
!|                |   | SEGMENT LENGTH (3RD COMPONENT)
!| W              |<->| WORKING TABLE
!| WINF           |-->| BOUNDARY CONDITIONS COMPUTED BY BORD
!| X,Y            |-->| COORDINATES FOR MESH NODES
!| XNEBOR,YNEBOR  |-->| NORMAL TO BOUNDARY POINTS
!| YACORIOL       |-->| LOGIC: IF YES CONSIDER CORIOLIS FORCE
!| YASMH          |-->| LOGICAL: TO TAKE INTO ACCOUNT SMH
!| YASMO          |-->| LOGICAL: TO TAKE INTO ACCOUNT FU AND FV
!| ZF             |-->| BED TOPOGRAPHY (BATHYMETRY)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_RESOLU => RESOLU
      USE DECLARATIONS_TELEMAC2D, ONLY:MSK,MASKEL,T6
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NPOIN,NSEG,NPTFR,LT,NREJET,DIMT
      INTEGER, INTENT(IN) :: MAXSCE,MAXTRA,NVMAX,HROPT
      INTEGER, INTENT(IN) :: DLIMT,OPTVF,JMI(*)
      INTEGER, INTENT(IN) :: KDIR,KNEU,KDDL,ITURB,NELMAX,KFROT,NTRAC
      INTEGER, INTENT(IN) :: NUBO(2,*),LIMPRO(NPTFR,6),NBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3),ISCE(NREJET)
      INTEGER, INTENT(INOUT) :: LTT,LOGFR(*),NEISEG(2,NSEG)
      INTEGER, INTENT(IN)    :: ELTSEG(NELEM,3)
      INTEGER,  INTENT(IN)   :: IFABOR(NELEM,*)
!
      LOGICAL, INTENT(IN) :: LISTIN,DTVARI,YASMH,DIFVIT,DIFT,RAIN,BILMAS
      LOGICAL, INTENT(IN) :: YACORIOL,SPHERIC
      LOGICAL, INTENT(INOUT)    :: YASMO
      DOUBLE PRECISION, INTENT(INOUT) :: T1(*),T2(*),T3(*),T4(*),T5(*)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: DT,MASS_RAIN
      DOUBLE PRECISION, INTENT(IN)    :: AT,VNOIN(3,*),GAMMA
      DOUBLE PRECISION, INTENT(IN)    :: TSCE2(MAXSCE,MAXTRA)
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN),FLUSORTN,FLUENTN
      DOUBLE PRECISION, INTENT(IN)    :: AIRE(NPOIN),DTHAUT(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR),UBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: VBOR(NPTFR),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN),CF(NPOIN),CORIOLIS
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN),SMH(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN),QU(NPOIN),QV(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NELMAX),DPY(3,NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: WINF(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),AIRS(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSCE(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(NPOIN,3),FLUX_OLD(NPOIN,3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSORT,FLUENT,MASSES
      DOUBLE PRECISION, INTENT(INOUT) :: FLUTENT(*),FLUTSOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU(*)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFLWTD,AIRST(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: HSTOK(*),HCSTOK(2,*),DTT
      DOUBLE PRECISION, INTENT(INOUT) :: CMI(NSEG,2)
      DOUBLE PRECISION, INTENT(IN)    :: PROPNU,DIFNU,TMAX
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,NELMAX),DJY(3,NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,NPOIN),DY(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(NELMAX),DJYT(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: DXT(NPOIN),DYT(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: HC(2,NSEG),DTN
!
      TYPE(BIEF_OBJ) , INTENT(IN)     :: TBOR,TN,PLUIE,FU,FV
      TYPE(BIEF_OBJ) , INTENT(IN)     :: COSLAT,SINLAT
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: T,HTN,SMTR,FLUHBOR,FLUHBTEMP
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLUXTEMP,FLUXT,FLBOR
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL RECONSTRUCT
      INTEGER I,IS,K,ICIN,IVIS,NORDRE,ITRAC
      DOUBLE PRECISION XNC,W1,DMIN,BETA,TEST
!
      DOUBLE PRECISION P_DMIN
      EXTERNAL P_DMIN
!
      DOUBLE PRECISION,PARAMETER:: EPS =  1.D-6
!
      RECONSTRUCT=.FALSE.
!
      IF(OPTVF.EQ.0) THEN
        ICIN = 0
        NORDRE = 1
      ELSEIF(OPTVF.EQ.1) THEN
        ICIN = 1
        NORDRE = 1
      ELSEIF(OPTVF.EQ.2) THEN
        ICIN = 1
        NORDRE = 2
        RECONSTRUCT=.TRUE.
      ELSEIF(OPTVF.EQ.3) THEN
        ICIN = 2
        NORDRE=1
      ELSEIF(OPTVF.EQ.4) THEN
        ICIN = 3
        NORDRE = 1
      ELSEIF(OPTVF.EQ.5) THEN
        ICIN = 4
        NORDRE = 1
      ELSEIF(OPTVF.EQ.6) THEN
        ICIN = 5
        NORDRE = 2
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'SCHEMA INCONNU : ',OPTVF
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN SCHEME: ',OPTVF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!  * WINF CONTAINS BC COMPUTED BY BORD
!
!      DO K=1,NPTFR
!        IF(LIMPRO(K,1).EQ.KDIR) THEN
!          WINF(1,K) =  HBOR(K)
!          WINF(2,K) =  HBOR(K)*UBOR(K)
!          WINF(3,K) =  HBOR(K)*VBOR(K)
!        ELSE
!          WINF(1,K) =  H(NBOR(K))
!          WINF(2,K) =  H(NBOR(K))*UBOR(K)
!          WINF(3,K) =  H(NBOR(K))*VBOR(K)
!        ENDIF
!       ENDDO
!     ==================================================================
!     VALUES COMPUTED BY BORD ARE GOOD
!     ==================================================================
      DO K=1,NPTFR
        WINF(1,K) =  H(NBOR(K))
        WINF(2,K) =  H(NBOR(K))*U(NBOR(K))
        WINF(3,K) =  H(NBOR(K))*V(NBOR(K))
      ENDDO
!     ==================================================================
!     INITIALIZE QU AND QV AND FLUX_OLD
!     ==================================================================
      IF(LT.EQ.1) THEN
        CALL OV('X=YZ    ',QU,HN,U,1.D0,NPOIN)
        CALL OV('X=YZ    ',QV,HN,V,1.D0,NPOIN)
        DO I=1,NPOIN
          FLUX_OLD(I,1) = 0.0D0
          FLUX_OLD(I,2) = 0.0D0
          FLUX_OLD(I,3) = 0.0D0
        ENDDO
      ENDIF
!     ==================================================================
!     LISTING FOR 1ST TIME STEP 
!     ==================================================================
      IF(LT.EQ.1) CALL ENTETE(1,AT,LT,SCHEME=ICIN,ORDRE=NORDRE)
!      
      IF(ICIN .EQ.0) THEN
!-----------------------------------------------------------------------
!        ROE SCHEME
!-----------------------------------------------------------------------
!
!      COPY  VARIABLES INTO W
!
        DO I=1,NPOIN
          W(1,I)= HN(I)
          W(2,I)= QU(I)
          W(3,I)= QV(I)
        ENDDO ! I
!
!CALCUL DU DT QUI SATISFAIT CFL
!
        CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DT,AT,TMAX,
     &             CFLWTD,ICIN,DTVARI,LISTIN)
!
!      WINF CONTAINS BORDVALUE AFTER THE USE OF RIEMANN INVARIANTS
!
        CALL FLUSEW
!
     &     (WINF,NPOIN,EPS,G,W,XNEBOR,YNEBOR,
     &      NPTFR,LIMPRO,NBOR,KDIR,KDDL)
!
!
        CALL FLUROE(W,FLUSCE,NUBO,VNOIN,
     &              WINF,FLUX,FLUSORT,FLUENT,NELEM,NSEG,NPTFR,
     &              NPOIN,X,Y,AIRS,ZF,EPS,DMIN,G,
     &              XNEBOR,YNEBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL,FLBOR,
     &              ELTSEG,IFABOR,MESH)
!
! INTEGRATION IN TIME
!
        CALL INTEMP(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,ZF,CF,EPS,KFROT,
     &             SMH,HN,QU,QV,LT,GAMMA)
!
!
        XNC = 0.D0
        DO I=1,NPOIN
          IF(H(I).GT.EPS) THEN
           W1=SQRT((QU(I)/H(I))**2+(QV(I)/H(I))**2)+SQRT(G*H(I))
           IF(W1.GE.XNC) XNC = W1
           IF(W1.GE.50.D0) THEN
             QU(I) = 0.D0
             QV(I) = 0.D0
           ENDIF
          ENDIF
        ENDDO
!
      ELSE IF(ICIN.EQ.1) THEN
!     ************************
!
!-----------------------------------------------------------------------
!            KINETIC SCHEME
!-----------------------------------------------------------------------
!
        IVIS=0
        IF(DIFVIT.AND.ITURB.EQ.1) IVIS=1
!
        IF(LT.EQ.1) THEN
!
!     COMPUTE GRADIENT OF ZF FOR ORDRE2
!
          IF(NORDRE.EQ.2) THEN
            CALL GRADZ(NPOIN,NELMAX,NSEG,IKLE,NUBO,X,Y,AIRE,AIRS,CMI,
     &               JMI,ZF,DPX,DPY,DSZ,BETA,AIRST,T1,T2,T3,T4,T5,
     &               ELTSEG,IFABOR,MESH)
          ENDIF
!
!    INITIALIZATION FOR TRACER
!
          IF(NTRAC.GT.0) THEN
            DO ITRAC=1,NTRAC
              MASSOU(ITRAC) = 0.D0
              FLUTENT(ITRAC)= 0.D0
              FLUTSOR(ITRAC)= 0.D0
              DO IS=1,NPOIN
                HTN%ADR(ITRAC)%P%R(IS) = HN(IS) * TN%ADR(ITRAC)%P%R(IS)
              ENDDO
              CALL OS('X=Y     ',X=T%ADR(ITRAC)%P,Y=TN%ADR(ITRAC)%P)
            ENDDO
          ENDIF
!
!          DEFINITION OF A REFERENCE TO DISTINGUISH INTERIOR
!          AND BOUNDARY NODES FOR TRACER ORDRE 2
!
          DO IS=1,NPOIN
            LOGFR(IS)=0
          ENDDO
!
          IF(NPTFR.GT.0)THEN !FOR PARALLLEL CASES
            DO K=1,NPTFR
              IS=NBOR(K)
              IF(LIMPRO(K,2).EQ.KDIR) LOGFR(IS)=1
              IF(LIMPRO(K,1).EQ.KDIR) LOGFR(IS)=3
              IF(LIMPRO(K,1).EQ.KNEU) LOGFR(IS)=2
            ENDDO
          ENDIF
        ENDIF
!-----------------------------------------------------------------------
!
        IF(LT.EQ.1.OR.NTRAC.EQ.0) THEN
!
!       REWRITE VARIABLES IN W
!
        DO I=1,NPOIN
            W(1,I)= HN(I)
          IF (HN(I).GT.EPS) THEN
            W(2,I) = QU(I) / HN(I)
            W(3,I) = QV(I) / HN(I)
          ELSE
            W(2,I) = 0.D0
            W(3,I) = 0.D0
          ENDIF
        ENDDO
!
!  TIME STEP UNDER CFL CONDITION (ORDRE 1)
!
        DTN = DT
        CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DTN,AT,TMAX,
     &            CFLWTD,ICIN,DTVARI,LISTIN)
!
! COMPUTE HYDRAULIC FLUXES
!
        CALL FLUHYD
     &        (NPOIN,NELMAX,NSEG,NPTFR,NUBO,G,DTN,X,Y,AIRS,IKLE,AIRE,
     &         W,ZF,VNOIN,FLUX,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &         HBOR,UBOR,VBOR,FLUENTN,FLUSORTN,NORDRE,CMI,JMI,
     &         DJX,DJY,DX,DY,DTHAUT,CFLWTD,FLBOR,
     &         DPX,DPY,IVIS,PROPNU,FLUHBTEMP,BETA,DSZ,AIRST,HC,FLUXTEMP,
     &         NTRAC,ELTSEG,IFABOR,MESH)
!
        IF(NTRAC.GT.0) THEN
!         INITIALIZATION FOR TRACER
          CALL REINIT(NPOIN,NSEG,NPTFR,HN,
     &                SMTR,HSTOK,HC,HCSTOK,FLUXT,FLUHBOR,DTT,NTRAC)
        ENDIF
!
      ENDIF
!-----------------------------------------------------------------------
!
!                     HYDRO UPDATING
!-----------------------------------------------------------------------
!
      DT = MIN(DTN,TMAX-AT)
!
      FLUENT =FLUENTN
      FLUSORT =FLUSORTN
      DO ITRAC=1,NTRAC
        FLUTENT(ITRAC)=0.D0
        FLUTSOR(ITRAC)=0.D0
        MASSOU(ITRAC) =0.D0
      ENDDO
!
! TIME INTEGRATION
!
      CALL MAJZZ(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,CF,KFROT,SMH,
     &           HN,QU,QV,LT,GAMMA,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,
     &           G,RAIN,PLUIE%R,FU%R,FV%R)
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
!
        DO ITRAC=1,NTRAC
!
!         INCREMENT OF MASS FLUX AND SOURCES FOR TRACER
          CALL FLUTRAC(NSEG,NPTFR,DT,FLUXT%ADR(ITRAC)%P%R,
     &                               FLUHBOR%ADR(ITRAC)%P%R,
     &                               FLUXTEMP%ADR(ITRAC)%P%R,
     &                               FLUHBTEMP%ADR(ITRAC)%P%R,DTT)
!
!        CALCULATION OF SECOND MEMBER FOR TRACER
          CALL SMTRAC(NPOIN,DIMT,AT,DT,SMTR%ADR(ITRAC)%P%R,
     &                SMH,NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,ITRAC)
!
        ENDDO
!
      ENDIF
!
! VOLUME ADDEED BY SOURCES
!
      IF(BILMAS)THEN
        MASSES   = 0.D0
        MASS_RAIN= 0.D0
!       IF SOURCE TERMS (EXCEPT RAIN AND EVAPORATION)
        IF(YASMH) THEN
          DO  I=1,NPOIN
            MASSES = MASSES + SMH(I)
          ENDDO
        ENDIF
!       RAIN AND EVAPORATION
        IF(RAIN)THEN
           CALL VECTOR(T6,'=','MASVEC          ',PLUIE%ELM,
     &                 1.D0,PLUIE,T6,T6,T6,T6,T6,MESH,MSK,MASKEL)
           MASS_RAIN =BIEF_SUM(T6)
        ENDIF
        MASSES = DT*(MASSES + MASS_RAIN)
      ENDIF
!
      DO I=1,NPOIN
        H(I)  = W(1,I)
        QU(I) = W(2,I)
        QV(I) = W(3,I)
!
! SAVE FLUXES FOR NEXT TIME STEP
!
        FLUX_OLD(I,1) = FLUX(I,1)
        FLUX_OLD(I,2) = FLUX(I,2)
        FLUX_OLD(I,3) = FLUX(I,3)
!
!      CALCULATION OF U,V
!
        IF (H(I).GT.EPS) THEN
          U(I)  = W(2,I) / H(I)
          V(I)  = W(3,I) / H(I)
        ELSE
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
      ENDDO ! I
!
      IF(NTRAC.EQ.0)  RETURN
!
!-----------------------------------------------------------------------
! IF END OF COMPUTATION, WE UPDATE TRACER
!
      IF(AT+DT.GE.TMAX) GOTO 200
!
!-----------------------------------------------------------------------
!    IF TRACER, WE ANTICIPATE THE COMPUTATION OF FLUXES
!-----------------------------------------------------------------------
!
! WE PUT PRIMITIVE VARIABLES  IN W
!
      DO I=1,NPOIN
        W(1,I) = H(I)
        W(2,I) = U(I)
        W(3,I) = V(I)
      ENDDO
!
! TIME STEP UNDER CFL CONDITION (ORDRE 1)
!
!  we may use H and not HN for DT computing !!!!  to be verified
!
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DTN,AT,TMAX,
     &           CFLWTD,ICIN,DTVARI,LISTIN)
!
! HYDRO FLUXES OF THE NEXT TIME STEP
!
      CALL FLUHYD
     &       (NPOIN,NELMAX,NSEG,NPTFR,NUBO,G,DTN,X,Y,AIRS,IKLE,AIRE,
     &        W,ZF,VNOIN,FLUX,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &        HBOR,UBOR,VBOR,FLUENTN,FLUSORTN,NORDRE,CMI,JMI,
     &        DJX,DJY,DX,DY,DTHAUT,CFLWTD,FLBOR,
     &        DPX,DPY,IVIS,PROPNU,FLUHBTEMP,BETA,
     &        DSZ,AIRST,HC,FLUXTEMP,NTRAC,ELTSEG,IFABOR,MESH)
!
! TEST OF TRACER FLUX (FOR POSITIVITY)
!
!    USELESS, BUT TO AVOID COMPILER ERROR
      TEST=-1.D0
!
      CALL TESTEUR(NPOIN,NSEG,NPTFR,NUBO,DTN,NBOR,
     &             NORDRE,AIRS,AIRST,HSTOK,HCSTOK,
     &             FLUXT,FLUXTEMP,FLUHBOR,FLUHBTEMP,LOGFR,TEST,NTRAC)
!
!  IF THERE IS NEGATIVE TEST ALL PROC WILL CONTINUE
      TEST = P_DMIN(TEST)
      IF(TEST.GE.0.D0) RETURN
 200  CONTINUE
!
!TRACER UPDATING
!
      LTT=LTT+1
!
      DO ITRAC=1,NTRAC
!
      CALL MAJTRAC(NPOIN,NELMAX,DIMT,DLIMT,NSEG,NPTFR,NUBO,
     &             X,Y,AIRS,IKLE,AIRE,T%ADR(ITRAC)%P%R,
     &             HTN%ADR(ITRAC)%P%R,TN%ADR(ITRAC)%P%R,ZF,NBOR,
     &             TBOR%ADR(ITRAC)%P%R,FLUTENT(ITRAC),FLUTSOR(ITRAC),
     &             SMTR%ADR(ITRAC)%P%R,NORDRE,CMI,JMI,
     &             DJXT,DJYT,DXT,DYT,
     &             DPX,DPY,DIFT,DIFNU,BETA,DSZ,AIRST,HSTOK,
     &             HCSTOK,FLUXT%ADR(ITRAC)%P%R,FLUHBOR%ADR(ITRAC)%P%R,
     &             MASSOU(ITRAC),DTT,MESH,ELTSEG,IFABOR,VNOIN)
!
!
      DO I=1,NPOIN
        HTN%ADR(ITRAC)%P%R(I) = T%ADR(ITRAC)%P%R(I)
        IF(H(I).GT.EPS) THEN
          T%ADR(ITRAC)%P%R(I) = T%ADR(ITRAC)%P%R(I) / H(I)
        ELSE
          T%ADR(ITRAC)%P%R(I) = 0.D0
        ENDIF
      ENDDO
!
      ENDDO
!
! INITIALIZATION FOR TRACER
!
      CALL REINIT(NPOIN,NSEG,NPTFR,H,
     &            SMTR,HSTOK,HC,HCSTOK,FLUXT,FLUHBOR,DTT,NTRAC)
!
      ELSE IF(ICIN.EQ.2) THEN
!   *****************************
!
!-----------------------------------------------------------------------
!            ZOKAGOA SCHEME
!-----------------------------------------------------------------------
!
!    COPY VARIABLES INTO W
!
      DO I=1,NPOIN
        W(1,I)= HN(I)
        W(2,I)= QU(I)
        W(3,I)= QV(I)
      ENDDO
!
! TIME STEP UNDER CFL CONDITION
!
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DT,AT,TMAX,
     &           CFLWTD,ICIN,DTVARI,LISTIN)
!
!INFLOW AND OUTFLOWS
!
      CALL FLUSEW(WINF,NPOIN,EPS,G,W,XNEBOR,YNEBOR,
     &            NPTFR,LIMPRO,NBOR,KDIR,KDDL)
!
!-----------------------------------------------------------------------
! FLUX COMPUTATION
      CALL FLUXZZ(X,Y,NPOIN,NSEG,NELMAX,NUBO,G,W,ZF,VNOIN,
     &            ELTSEG,FLUX,IFABOR)
!BOUNDARY CONDITIONS
      CALL CDLZZ(NPOIN,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &           KDDL,G,W,FLUX,FLUENT,FLUSORT,
     &           FLBOR,ZF,WINF)
! FOR PARALLESM
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(FLUX(:,1),FLUX(:,2),FLUX(:,3),NPOIN,1,2,3,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
! TIME INTEGRATION
!
      CALL MAJZZ(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,CF,KFROT,SMH,
     &           HN,QU,QV,LT,GAMMA,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,
     &           G,RAIN,PLUIE%R,FU%R,FV%R)
!
!-----------------------------------------------------------------------
!
!
      ELSE IF(ICIN.EQ.3) THEN
!     ***********************
!
!-----------------------------------------------------------------------
!             TCHAMEN SCHEME
!-----------------------------------------------------------------------
!
!   CPY VARIABLES INTO W
!
      DO I=1,NPOIN
        W(1,I)= HN(I)
        W(2,I)= QU(I)
        W(3,I)= QV(I)
      ENDDO
!
! TIME STEP UNDER CFL CONDITION
!
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DT,AT,TMAX,
     &           CFLWTD,ICIN,DTVARI,LISTIN)
!
!
!INFLOW AND OUTFLOWS ! USELESS
!
!      CALL FLUSEW(WINF,NPOIN,EPS,G,W,XNEBOR,YNEBOR,
!     &            NPTFR,LIMPRO,NBOR,KDIR,KDDL)
!
!-----------------------------------------------------------------------
! FLUX COMPUTATION

      CALL FLUX_TCH(X,Y,NPOIN,NSEG,NELMAX,NUBO,G,W,ZF,VNOIN,
     &              ELTSEG,FLUX,IFABOR)
!
!BOUNDARY CONDITIONS
        CALL CDL_TCH(NPOIN,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &               KDDL,G,W,FLUX,FLUENT,FLUSORT,
     &               FLBOR,EPS,ZF,WINF)

!  FOR PARALLESM
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(FLUX(:,1),FLUX(:,2),FLUX(:,3),NPOIN,1,2,3,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
! TIME INTEGRATION
!
      CALL MAJZZ(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,CF,KFROT,SMH,
     &           HN,QU,QV,LT,GAMMA,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,
     &           G,RAIN,PLUIE%R,FU%R,FV%R)
!
!    *****************************
      ELSE IF(ICIN.EQ.4) THEN
!    *****************************
!-----------------------------------------------------------------------
!             HLLC SCHEME
!-----------------------------------------------------------------------
!
!     COPY VARIABLES INTO W
!
      DO I=1,NPOIN
        W(1,I)= HN(I)
        W(2,I)= QU(I)
        W(3,I)= QV(I)
      ENDDO
!
!  TIME STEP UNDER CFL CONDITION
!
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DT,AT,TMAX,
     &           CFLWTD,ICIN,DTVARI,LISTIN)
!
!
! INFLOW AND OUTFLOWS
!
!      CALL FLUSEW(WINF,NPOIN,EPS,G,W,XNEBOR,YNEBOR,
!     &            NPTFR,LIMPRO,NBOR,KDIR,KDDL)
!
!-----------------------------------------------------------------------
!  FLUX COMPUTATION
!
      CALL HYD_HLLC(NPOIN,NELEM,NSEG,NUBO,G,W,ZF,VNOIN,
     &              X,Y,ELTSEG,FLUX,IFABOR,HROPT)
!
!  FOR PARALLESM
!
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(FLUX(1,1),FLUX(1,2),FLUX(1,3),NPOIN,1,2,3,MESH)
      ENDIF
!
!
! BOUNDARY CONDITIONS
!
      CALL CDL_HLLC(NPOIN,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,
     &              W,FLUX,FLUENT,FLUSORT,FLBOR,EPS,WINF,
     &              G,HBOR,UBOR,VBOR,MESH)
!
!-----------------------------------------------------------------------
!
!  TIME INTEGRATION
!
      CALL MAJZZ(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,CF,KFROT,SMH,
     &           HN,QU,QV,LT,GAMMA,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,
     &           G,RAIN,PLUIE%R,FU%R,FV%R)
!
!  SEMI-IMPLICIT MOMENTUM SOURCE TERMS  
!
      CALL  SOURCE_MOMENT(NPOIN,G,DT,W,HN,QU,QV,KFROT.NE.0,
     &                    CF,YACORIOL,CORIOLIS,
     &                    SPHERIC,COSLAT,SINLAT,LT,FU,FV,YASMO)

!
!    *****************************
      ELSEIF(ICIN.EQ.5) THEN
!    *****************************
!-----------------------------------------------------------------------
!             WAF SCHEME
!-----------------------------------------------------------------------
!
!
      IF(LT.EQ.1) THEN
!
! INITIALIZATION OF NEISEG
        DO I=1,NSEG
          NEISEG(1,I) = 0
          NEISEG(2,I) = 0
        ENDDO
! SEARCH FOR NEIGHBORS OF SEGMENT (FOR LIMITER)
!       CALL SEG_NEIGHBORS
!    &       (X,Y,NPOIN,NVMAX,NELEM,NSEG,
!    &        ELTSEG,NUBO,IFABOR,KNOLG,NEISEG)
        CALL SEG_NEIGHBORS
     &      (X,Y,IKLE,NPOIN,NVMAX,NELEM,NELMAX,NSEG,NEISEG)
      ENDIF
!-----------------------------------------------------------------------
!     COPY VARIABLES INTO W
!
      DO I=1,NPOIN
        W(1,I)= HN(I)
        W(2,I)= QU(I)
        W(3,I)= QV(I)
      ENDDO
!
!  TIME STEP UNDER CFL CONDITION
!
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DT,AT,TMAX,
     &           CFLWTD,ICIN,DTVARI,LISTIN)
!
!
! INFLOW AND OUTFLOWS
!
      CALL FLUSEW
     &   (WINF,NPOIN,EPS,G,W,XNEBOR,YNEBOR,
     &    NPTFR,LIMPRO,NBOR,KDIR,KDDL)
!
!-----------------------------------------------------------------------
!  FLUX COMPUTATION
      CALL HYD_WAF
     &   (NPOIN,NSEG,NELEM,NUBO,G,W,ZF,VNOIN,DT,DTHAUT,
     &    X,Y,FLUX,ELTSEG,NEISEG,HROPT)
!
! BOUNDARY CONDITIONS
!
      CALL CDL_WAF(NPOIN,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &             KDDL,W,FLUX,FLUENT,FLUSORT,FLBOR,DTHAUT,DT,
     &             EPS,WINF)
!
!-----------------------------------------------------------------------
!
!  TIME INTEGRATION
!
      CALL MAJZZ(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,CF,KFROT,SMH,
     &           HN,QU,QV,LT,GAMMA,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,
     &           G,RAIN,PLUIE%R,FU%R,FV%R)
!
!  SEMI-IMPLICIT MOMENTUM SOURCE TERMS  
!
      CALL SOURCE_MOMENT(NPOIN,G,DT,W,HN,QU,QV,KFROT.NE.0,
     &                    CF,YACORIOL,CORIOLIS,
     &                    SPHERIC,COSLAT,SINLAT,LT,FU,FV,YASMO)
!
!-----------------------------------------------------------------------
!
      ENDIF ! THIS IF FOR THE CHOICE OF SCHEME
!
      IF(ICIN.NE.1.AND.ICIN.NE.2)THEN  ! FOR KINETIC SCHEMES IT IS ALREADY DONE 
!
!-----------------------------------------------------------------------
!  COMPUTES VOLUME ADDED BY SOURCES
!-----------------------------------------------------------------------
!
       IF(BILMAS)THEN
         MASSES   = 0.D0
         MASS_RAIN= 0.D0
!        IF SOURCE TERMS (EXCEPT RAIN AND EVAPORATION)
         IF(YASMH) THEN
           DO  I=1,NPOIN
             MASSES = MASSES + SMH(I)
           ENDDO
         ENDIF
!        RAIN AND EVAPORATION
         IF(RAIN)THEN
            CALL VECTOR(T6,'=','MASVEC          ',PLUIE%ELM,
     &                  1.D0,PLUIE,T6,T6,T6,T6,T6,MESH,MSK,MASKEL)
            MASS_RAIN =BIEF_SUM(T6)
         ENDIF
         MASSES = DT*(MASSES + MASS_RAIN)
       ENDIF
!
!-----------------------------------------------------------------------
!      PREPARE VARIABLES FOR OUTPUT AND FOR NEXT TIME STEP
!-----------------------------------------------------------------------
!
       DO I=1,NPOIN
         H(I)  = W(1,I)
         QU(I) = W(2,I)
         QV(I) = W(3,I)
!        SAVE FLUXES FOR NEXT TIME STEP
         FLUX_OLD(I,1) = FLUX(I,1)
         FLUX_OLD(I,2) = FLUX(I,2)
         FLUX_OLD(I,3) = FLUX(I,3)
!
!        COMPUTE U AND V
!
         IF (H(I).GT.EPS) THEN
           U(I)  = W(2,I) / H(I)
           V(I)  = W(3,I) / H(I)
         ELSE
           U(I) = 0.D0
           V(I) = 0.D0
         ENDIF
       ENDDO 
      ENDIF
!
!-----------------------------------------------------------------------
!
!
      RETURN
      END
