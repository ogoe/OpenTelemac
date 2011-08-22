!                       *****************
                        SUBROUTINE RESOLU
!                       *****************
!
     &(W,FLUSCE,NUBO,VNOIN,WINF,AT,DT,LT,NIT,
     & NELEM,NSEG,NPTFR,FLUX,AIRS,AIRE,
     & X,Y,IKLE,ZF,CF,NPOIN,HN,H,U,V,QU,QV,G,LISTIN,
     & XNEBOR,YNEBOR,XSGBOR,YSGBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL,
     & HBOR,UBOR,VBOR,FLUSORT,FLUENT,CFLWTD,DTVARI,NELMAX,KFROT,  
     & NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,YASMH,SMH,MASSES,
     & NTRAC,DIMT,T,HTN,TN,DLIMT,LIMTRA,
     & TBOR,MASSOU,FLUTENT,FLUTSOR,DTHAUT,DPX,DPY,DJX,DJY,CMI,JMI,
     & SMTR,DXT,DYT,DJXT,DJYT,DIFVIT,ITURB,PROPNU,DIFT,DIFNU,
     & DX,DY,OPTVF,FLUSORTN,FLUENTN,
     & DSZ,AIRST,HSTOK,HCSTOK,FLUXT,FLUHBOR,FLBOR,
     & LOGFR,LTT,DTN,FLUXTEMP,FLUHBTEMP,
     & HC,TMAX,DTT,T1,T2,T3,T4,T5,GAMMA,FLUX_OLD)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    1. SOLVES THE PROBLEM BY A METHOD OF TYPE ROE OR BY A KINETIC 
!            SCHEME (ORDER 1 OR 2) OR TCHAMEN/ZOKAGOA SCHEME
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
!+    ADD TCHAMEN AND ZOKAGA FLUXES 
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AIRE           |-->| ELEMENT AREA
!| AIRS           |-->| CELL AREA
!| AIRST          |-->| AREA OF SUB-TRIANGLES (SECOND ORDER)
!| AT,DT,LT       |-->| TIME, TIME STEP AND NUMBER OF THE STEP
!| CF             |-->| FRICTION COEFFICIENT
!| CFLWTD         |-->| WANTED CFL NUMBER
!| CMI            |-->| COORDINATES OF MIDDLE PONTS OF EDGES
!| DIFNU          |-->| COEFFICIENT OF DIFFUSION FOR TRACER
!| DIFT           |-->| LOGICAL: DIFFUSION FOR TRACER OR NOT
!| DIFVIT         |-->|  LOGICAL: DIFFUSION FOR VELOCITY OR NOT
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
!| FLUENT,FLUSORT |<--| MASS FLUX MASSE INLET AND OUTLET FROM TN TO TN+1
!| FLUHBTEMP      |<->| BORD FLUX FOR TRACER
!| FLUSCE         |-->| SOURCE FLUXES
!| FLUSORTN,FLUENT|<->| MASS FLUX MASSE INLET AND OUTLET FROM TN+1 TO TN+2
!| FLUTENT,FLUTSOR|<--| FLUX TRACER INLET AND OUTLET
!| FLUX           |---| FLUX
!| FLUXT,FLUHBOR  |<->| FLUX, FLUX BORD FOR TRACER
!| FLUXTEMP       |<->| FLUX FOR TRACER
!| G              |-->| GRAVITY
!| H              |<--| WATER DEPTH AT TIME N+1
!| HBOR           |-->| IMPOSED VALUE FOR H
!| HC             |<->| H RECONSTRUCTED (ORDER 2) CORRECTED
!| HN             |-->| WATER DEPTH AT TIME N
!| HSTOK,HCSTOK   |<->| H, H CORRECTED TO STOCK FOR TRACER
!| HTN,TN         |-->| HT, T  AT TIME N
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
!| LIMTRA         |-->| TYPES OF BOUNDARY CONDITION FOR TRACER
!| LISTIN         |-->| IF YES, PRINT MESSAGES AT LISTING.
!| LOGFR          |<->| REFERENCE OF BOUNDARY NODES
!| LTT            |<->| NUMBER OF TIME STEP FOR TRACER
!| MASSES         |<--| ADDED MASS BY SOURCE TERMS
!| MASSOU         |<--| ADDED TRACER MASS BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NBOR           |-->| GLOBAL INDICES FOR BORD NODES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NIT            |-->| TOTAL NUMBER OF TIME STEPS
!| NPOIN          |-->| TOTAL NUMBER OF NODES
!| NPTFR          |-->| TOTAL NUMBER OF BOUNDARY NODES
!| NREJET         |-->| NUMBER OF SOURCE/SINK
!| NSEG           |-->| NUMBER OF EDGES
!| NTRAC          |-->| NUMBER OF TRACERS
!| NUBO           |-->| GLOBAL INDICES OF EDGE EXTREMITIES
!| OPTVF          |-->| OPTION OF THE SCHEME
!|                |   | 0:ROE, 1:KINETIC ORDRE 1,2:KINETIC ORDRE 2
!|                |   | 3:ZOKAGOA, 4:TCHAMEN,4:HLLC
!| PROPNU         |-->| COEFFICIENT OF MOLECULAR DIFFUSION 
!| QU,QV          |<->| FLOW COMPOENENTS AT TIME N THEN AT TIME  N+1
!| SMH            |-->| SOURCE TERMS FOR CONTINUITY EQUATION
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
!| WINF           |---| ???? NOT USED
!| X,Y            |-->| COORDINATES FOR MESH NODES
!| XNEBOR,YNEBOR  |-->| NORMAL TO BOUNDARY POINTS
!| YASMH          |-->| LOGICAL: TO TAKE INTO ACCOUNT SMH
!| ZF             |-->| BED TOPOGRAPHY (BATHYMETRY)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_RESOLU => RESOLU
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NPOIN,NSEG,NPTFR,LT,NIT,NREJET,DIMT
      INTEGER, INTENT(IN) :: MAXSCE,MAXTRA
      INTEGER, INTENT(IN) :: DLIMT,OPTVF,JMI(*)
      INTEGER, INTENT(IN) :: KDIR,KNEU,KDDL,ITURB,NELMAX,KFROT,NTRAC
      INTEGER, INTENT(IN) :: NUBO(2,*),LIMPRO(NPTFR,6),NBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3),ISCE(NREJET),LIMTRA(DLIMT)
      INTEGER, INTENT(INOUT) :: LTT,LOGFR(*)
! 
      LOGICAL, INTENT(IN) :: LISTIN,DTVARI,YASMH,DIFVIT,DIFT
      DOUBLE PRECISION, INTENT(INOUT) :: T1(*),T2(*),T3(*),T4(*),T5(*)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: XSGBOR(NPTFR,4),YSGBOR(NPTFR,4)	
      DOUBLE PRECISION, INTENT(INOUT) :: DT
      DOUBLE PRECISION, INTENT(IN)    :: AT,VNOIN(3,*),GAMMA
      DOUBLE PRECISION, INTENT(IN)    :: TSCE2(MAXSCE,MAXTRA)   
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN),FLUSORTN,FLUENTN   
      DOUBLE PRECISION, INTENT(IN)    :: AIRE(NPOIN),DTHAUT(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR),UBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: VBOR(NPTFR),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SMH(NPOIN),ZF(NPOIN),CF(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN),QU(NPOIN),QV(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NELMAX),DPY(3,NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: WINF(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),AIRS(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSCE(3,NPOIN),FLUX(NPOIN,3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSORT,FLUENT,MASSES
      DOUBLE PRECISION, INTENT(INOUT) :: FLUTENT(*),FLUTSOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU(*)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFLWTD,AIRST(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: HSTOK(*),HCSTOK(2,*),DTT
      DOUBLE PRECISION, INTENT(INOUT) :: CMI(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: PROPNU,DIFNU,TMAX
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,NELMAX),DJY(3,NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,NPOIN),DY(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(NELMAX),DJYT(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: DXT(NPOIN),DYT(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ(2,NSEG),FLUX_OLD(NPOIN,3)
      DOUBLE PRECISION, INTENT(INOUT) :: HC(2,NSEG),DTN 
!
      TYPE(BIEF_OBJ) , INTENT(IN)     :: TBOR,TN
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: T,HTN,SMTR,FLUHBOR,FLUHBTEMP
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLUXTEMP,FLUXT,FLBOR         
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IS,K,ICIN,IVIS,NORDRE,ITRAC,ERR
!
      DOUBLE PRECISION XNC,W1,EPS,DMIN,BETA,TEST
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
      ELSEIF(OPTVF.EQ.3) THEN
        ICIN = 2
        NORDRE=1
      ELSEIF(OPTVF.EQ.4) THEN
        ICIN = 3
        NORDRE = 1
      ELSEIF(OPTVF.EQ.5) THEN
        ICIN = 4
        NORDRE = 1
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'SCHEMA INCONNU : ',OPTVF
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN SCHEME: ',OPTVF
        CALL PLANTE(1)
        STOP
      ENDIF
! 
      EPS =  1.D-6 
!  
! IF NOT CFL VARIABLE WE IMPOSE CFL=0.5
!      IF(.NOT.DTVARI)CFLWTD=0.5D0
!
!   CONSIDER BOUNDARY CONDITIONS 
!
!   WINF CONTAINS INITIAL WINF COMPUTED BY BORD
!
      DO 110 K=1,NPTFR
        IF(LIMPRO(K,1).EQ.KDIR) THEN
          WINF(1,K) =  HBOR(K)
          WINF(2,K) =  HBOR(K)*UBOR(K)
          WINF(3,K) =  HBOR(K)*VBOR(K)
        ELSE
          WINF(1,K) =  H(NBOR(K))
          WINF(2,K) =  H(NBOR(K))*UBOR(K)
          WINF(3,K) =  H(NBOR(K))*VBOR(K)
        ENDIF 
110   CONTINUE
!
      IF(ICIN .EQ.0) THEN
!
!-----------------------------------------------------------------------
!        ROE SCHEME
!-----------------------------------------------------------------------
!
      IF(LT.EQ.1) THEN
!
        WRITE(LU,*) ' '
        WRITE(LU,*) '          ***************** '
        WRITE(LU,*) '          *   ROE SCHEME  * '              
        WRITE(LU,*) '          ***************** '
        WRITE(LU,*) ' '
!
!       INITIALIZATION OF FLUX_OLD
!
        DO I=1,NPOIN
          FLUX_OLD(I,1) = 0.D0
          FLUX_OLD(I,2) = 0.D0
          FLUX_OLD(I,3) = 0.D0
        ENDDO
      ENDIF
!
!     COPY VARIABLES INTO W
!
      DO I=1,NPOIN
        W(1,I)= HN(I)
        W(2,I)= QU(I)
        W(3,I)= QV(I)
      ENDDO
!
!     COMPUTING DT THAT OBEYS CFL
!
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DT,CFLWTD,ICIN,DTVARI,LISTIN)
      DT = MIN(DT,TMAX-AT) 
!
!     WINF CONTAINS BORDVALUE AFTER THE USE OF RIEMANN INVARIANTS
!
      CALL FLUSEW(WINF,UBOR,VBOR,NPOIN,EPS,G,W,       
     &            XNEBOR,YNEBOR,XSGBOR,YSGBOR,
     &            NPTFR,LIMPRO,NBOR,KDIR,KNEU,KDDL)    
!
!
      CALL FLUROE(W,FLUSCE,NUBO,VNOIN,
     &            WINF,FLUX,FLUSORT,FLUENT,NELEM,NSEG,NPTFR,   
     *            NPOIN,X,Y,AIRS,ZF,EPS,DMIN,G,
     *            XNEBOR,YNEBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL,FLBOR)             
!
!     INTEGRATION IN TIME
! 
      CALL INTEMP(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,ZF,CF,EPS,KFROT,
     &            SMH,HN,QU,QV,LT,GAMMA)  
!
!     VOLUME ADDEED BY SOURCES
!
      IF(YASMH) THEN
        MASSES=0.D0
        DO I=1,NPOIN
          MASSES = MASSES + SMH(I)
        ENDDO
        MASSES = DT * MASSES
      ENDIF
!
      DO  I=1,NPOIN 
        H(I)  = W(1,I) 
        QU(I) = W(2,I)
        QV(I) = W(3,I)
!       SAVE FLUXES FOR NEXT TIME STEP
        FLUX_OLD(I,1) = FLUX(I,1)
        FLUX_OLD(I,2) = FLUX(I,2)
        FLUX_OLD(I,3) = FLUX(I,3)
!
!       COMPUTE U,V 
!
        IF(H(I).GT.EPS) THEN
          U(I)  = W(2,I) / H(I)
          V(I)  = W(3,I) / H(I) 
        ELSE
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
      ENDDO
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
      ELSEIF(ICIN.EQ.1) THEN
!     **********************
!
!-----------------------------------------------------------------------
!            KINETIC SCHEME
!-----------------------------------------------------------------------
!
      IVIS=0
      IF(DIFVIT.AND.ITURB.EQ.1) IVIS=1
      IF(LT.EQ.1)THEN
!
!       INITIALIZATIONS FOR THE 1ST TIME STEP
!       *************************************
!
        WRITE(LU,*) ' '
        WRITE(LU,*) '          ******************** '
        WRITE(LU,*) '          *  KINETIC SCHEME  * '              
        WRITE(LU,*) '          ******************** '
        WRITE(LU,*) ' '
!
!       INITIALIZATION OF FLUX_OLD
!
        DO I=1,NPOIN
          FLUX_OLD(I,1) = 0.D0
          FLUX_OLD(I,2) = 0.D0
          FLUX_OLD(I,3) = 0.D0
        ENDDO
      ENDIF
!
!     INITIALIZAION AND HYDRODYNAMIC COMPUTATIONS
!
      IF(LT.EQ.1.OR.NTRAC.EQ.0) THEN
!
      CALL INIT_KIN  
     & (W,FLUSCE,NUBO,VNOIN,AT,DT,LT,NIT,
     &  NELEM,NSEG,NPTFR,FLUX,AIRS,AIRE,
     &  X,Y,IKLE,ZF,CF,NPOIN,HN,H,U,V,QU,QV,G,LISTIN,
     &  XNEBOR,YNEBOR,XSGBOR,YSGBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL, 
     &  HBOR,UBOR,VBOR,FLUSORT,FLUENT,CFLWTD,DTVARI,NELMAX,KFROT,  
     &  NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,YASMH,SMH,MASSES,
     &  NTRAC,DIMT,T,HTN,TN,DLIMT,LIMTRA,
     &  TBOR,MASSOU,FLUTENT,FLUTSOR,DTHAUT,DPX,DPY,DJX,DJY,CMI,JMI,
     &  SMTR,DXT,DYT,DJXT,DJYT,DIFVIT,ITURB,PROPNU,DIFT,DIFNU,
     &  DX,DY,OPTVF,FLUSORTN,FLUENTN,
     &  DSZ,AIRST,HSTOK,HCSTOK,FLUXT,FLUHBOR,FLBOR,
     &  LOGFR,LTT,DTN,FLUXTEMP,FLUHBTEMP,
     &  HC,TMAX,DTT,T1,T2,T3,T4,T5,ICIN,EPS,NORDRE,IVIS)
      ENDIF
!
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
!     TIME INTEGRATION 
!
      CALL MAJZZ(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,ZF,CF,EPS,KFROT,SMH,
     &           HN,QU,QV,LT,GAMMA,
     &           NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU)     
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
!         CALCULATION OF SECOND MEMBER FOR TRACER
          CALL SMTRAC(NPOIN,DIMT,AT,DT,SMTR%ADR(ITRAC)%P%R,
     &                SMH,NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,ITRAC)
!
        ENDDO

!
      ENDIF
!
! VOLUME ADDEED BY SOURCES
!
      IF(YASMH) THEN
      MASSES=0.D0
      DO  I=1,NPOIN
      MASSES = MASSES + SMH(I)
      ENDDO
       MASSES = DT * MASSES
      ENDIF
!
      DO 115 I=1,NPOIN
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
115   CONTINUE
!
      IF(NTRAC.EQ.0)  RETURN
!
!-----------------------------------------------------------------------
!
! IF END OF COMPUTATION, WE UPDATE TRACER
!
      IF(AT+DT.GE.TMAX) GOTO 200
!
!-----------------------------------------------------------------------
!    IF TRACER, WE ANTICIPATE THE COMPUTATION OF FLUXES 
!
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
      CALL CALDT(NPOIN,G,H,U,V,DTHAUT,DTN,CFLWTD,ICIN,DTVARI,LISTIN)
!
! HYDRO FLUXES OF THE NEXT TIME STEP
!
      CALL FLUHYD(NPOIN,NELMAX,NSEG,NPTFR,NUBO,G,DTN,X,Y,AIRS,IKLE,AIRE,
     &            W,ZF,VNOIN,FLUX,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &            KDDL,HBOR,UBOR,VBOR,FLUENTN,FLUSORTN,NORDRE,CMI,JMI,
     &            DJX,DJY,DX,DY,DTHAUT,CFLWTD,FLBOR,
     &            DPX,DPY,IVIS,PROPNU,FLUHBTEMP,BETA,DSZ,AIRST,HC,
     &            FLUXTEMP,NTRAC)
!
! TEST OF TRACER FLUX (FOR POSITIVITY)
! 
!    NE SERT A RIEN MAIS EVITE WARNING DE COMPILATEUR
      TEST=-1.D0
!
      CALL TESTEUR(NPOIN,NSEG,NPTFR,NUBO,DTN,NBOR,
     &             NORDRE,AIRS,AIRST,HSTOK,HCSTOK,
     &             FLUXT,FLUXTEMP,FLUHBOR,FLUHBTEMP,LOGFR,TEST,NTRAC)
!
      IF(TEST.GE.0.D0) RETURN
 200  CONTINUE
!
!     TRACER UPDATING
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
     &             MASSOU(ITRAC),DTT)
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
!     ***********************
!
!-----------------------------------------------------------------------
!            ZOKAGOA SCHEME
!-----------------------------------------------------------------------
!
!
      IF(LT.EQ.1) THEN
!
!       INITIALIZATIONS FOR THE FIRST TIME STEP
!       ***************************************
!
        WRITE(LU,*) ' '
        WRITE(LU,*) '          *********************** '
        WRITE(LU,*) '          *   ZOKAGOA SCHEME    * '              
        WRITE(LU,*) '          *********************** '
        WRITE(LU,*) ' '
!       INITIALIZATION OF FLUX_OLD
        DO I=1,NPOIN
          FLUX_OLD(I,1) = 0.0D0
          FLUX_OLD(I,2) = 0.0D0
          FLUX_OLD(I,3) = 0.0D0
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COPY  VARIABLES INTO W
!
      DO I=1,NPOIN
        W(1,I)= HN(I) 
        W(2,I)= QU(I)
        W(3,I)= QV(I)
      ENDDO
!
!     TIME STEP UNDER CFL CONDITION
!
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DT,CFLWTD,ICIN,DTVARI,LISTIN)
!
      DT = MIN(DT,TMAX-AT) 
!
!     INFLOW AND OUTFLOWS
!
      CALL FLUSEW(WINF,UBOR,VBOR,NPOIN,EPS,G,W,       
     &            XNEBOR,YNEBOR,XSGBOR,YSGBOR,
     &            NPTFR,LIMPRO,NBOR,KDIR,KNEU,KDDL)  
!
!-----------------------------------------------------------------------
! FLUX COMPUTATION

      CALL FLUXZZ(NPOIN,NSEG,NUBO,G,X,Y,W,ZF,VNOIN,FLUX,AIRS)
!
!     BOUNDARY CONDITIONS
!
       CALL CDLZZ(NPOIN,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &             KDDL,G,HBOR,UBOR,VBOR,W,FLUX,FLUENT,FLUSORT,
     &             FLBOR,DTHAUT,
     &             DT,CFLWTD,EPS,ZF,WINF)
!
!-----------------------------------------------------------------------
!
! TIME INTEGRATION 
!
      CALL MAJZZ(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,ZF,CF,EPS,KFROT,SMH,
     &           HN,QU,QV,LT,GAMMA,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU) 
!
!-----------------------------------------------------------------------

! VOLUME ADDED BY SOURCE TERMS
!
      IF(YASMH) THEN
        MASSES=0.D0
        DO  I=1,NPOIN
          MASSES = MASSES + SMH(I)
        ENDDO
        MASSES = DT * MASSES
      ENDIF
!
      DO 715 I=1,NPOIN 
        H(I)  = W(1,I)
        QU(I) = W(2,I)
        QV(I) = W(3,I)
!       SAVE FLUXES FOR NEXT TIME STEP
        FLUX_OLD(I,1) = FLUX(I,1)
        FLUX_OLD(I,2) = FLUX(I,2)
        FLUX_OLD(I,3) = FLUX(I,3)
!
!      COMPUTE  U,V 
!
        IF (H(I).GT.EPS) THEN
          U(I)  = W(2,I) / H(I)
          V(I)  = W(3,I) / H(I) 
        ELSE
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
715   CONTINUE 
!
!-----------------------------------------------------------------------
!
!
      ELSEIF(ICIN.EQ.3) THEN
!     **********************
!
!-----------------------------------------------------------------------
!             TCHAMEN SCHEME
!-----------------------------------------------------------------------
!
!
      IF(LT.EQ.1) THEN
!
!            INITIALIZATIONS FOR THE 1ST TIME STEP
!            ***********************************
!
       WRITE(LU,*) ' '
       WRITE(LU,*) '          *********************** '
       WRITE(LU,*) '          *   TCHAMEN SCHEME    * '              
       WRITE(LU,*) '          *********************** '
       WRITE(LU,*) ' '
!
!      INITIALIZATION OF FLUX_OLD
!
        DO I=1,NPOIN
          FLUX_OLD(I,1) = 0.0D0
          FLUX_OLD(I,2) = 0.0D0
          FLUX_OLD(I,3) = 0.0D0
        ENDDO
      ENDIF
!
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
!     TIME STEP UNDER CFL CONDITION
!
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DT,CFLWTD,ICIN,DTVARI,LISTIN)
!
      DT = MIN(DT,TMAX-AT) 
!
!     INFLOW AND OUTFLOWS
!
      CALL FLUSEW(WINF,UBOR,VBOR,NPOIN,EPS,G,W,       
     &            XNEBOR,YNEBOR,XSGBOR,YSGBOR,
     &            NPTFR,LIMPRO,NBOR,KDIR,KNEU,KDDL)  
!
!-----------------------------------------------------------------------
!
!     FLUX COMPUTATION

      CALL FLUX_TCH(NPOIN,NSEG,NUBO,G,X,Y,W,ZF,VNOIN,FLUX,AIRS)
!
!     BOUNDARY CONDITIONS
!
      CALL CDL_TCH(NPOIN,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &             KDDL,G,HBOR,UBOR,VBOR,W,FLUX,FLUENT,FLUSORT,
     &             FLBOR,DTHAUT,
     &             DT,CFLWTD,EPS,ZF,WINF)
!
!-----------------------------------------------------------------------
!
!     TIME INTEGRATION 
!
      CALL MAJZZ(W,FLUX,FLUX_OLD,AIRS,DT,NPOIN,ZF,CF,EPS,KFROT,SMH,
     &           HN,QU,QV,LT,GAMMA,
     &           NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU)     
!
!
!-----------------------------------------------------------------------
!
!     VOLUME ADDED BY SOURCES
!
      IF(YASMH) THEN
      MASSES=0.D0
      DO  I=1,NPOIN 
      MASSES = MASSES + SMH(I)
      ENDDO
       MASSES = DT * MASSES
      ENDIF
!
      DO 815 I=1,NPOIN
        H(I)  = W(1,I)
        QU(I) = W(2,I)
        QV(I) = W(3,I)
!       SAVE FLUXES FOR NEXT TIME STEP
        FLUX_OLD(I,1) = FLUX(I,1)
        FLUX_OLD(I,2) = FLUX(I,2)
        FLUX_OLD(I,3) = FLUX(I,3)
!       COMPUTATION U,V 
        IF(H(I).GT.EPS) THEN
          U(I)  = W(2,I) / H(I)
          V(I)  = W(3,I) / H(I) 
        ELSE
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
815   CONTINUE   
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN 
      END
