!                    *****************
                     SUBROUTINE DIFSOU
!                    *****************
!
     &(TEXP,TIMP,YASMI,TSCEXP,HPROP,TN,TETAT,NREJET,ISCE,DSCE,TSCE,
     & MAXSCE,MAXTRA,AT,DT,MASSOU,NTRAC,FAC,NSIPH,ENTSIP,SORSIP,
     & DSIP,TSIP,NBUSE,ENTBUS,SORBUS,DBUS,TBUS,NWEIRS,TYPSEUIL,
     & N_NGHB_W_NODES,NDGA1,NDGB1,TWEIRA,TWEIRB)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    PREPARES THE SOURCES TERMS IN THE DIFFUSION EQUATION
!+                FOR THE TRACER.
!
!warning  BEWARE OF NECESSARY COMPATIBILITIES FOR HPROP, WHICH
!+            SHOULD REMAIN UNCHANGED UNTIL THE COMPUTATION OF THE
!+            TRACER MASS IN CVDFTR
!
!history  J-M HERVOUET (LNHE); C MOULIN (LNH)
!+        23/02/2009
!+        V6P0
!+
!
!history  J-M HERVOUET (LNHE)
!+        01/10/2009
!+       V6P0
!+   MODIFIED TEST ON ICONVF(3)
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
!history  C.COULET (ARTELIA)
!+        23/05/2012
!+        V6P2
!+   Modification for culvert management
!+   Addition of Tubes management
!
!history  C.COULET (ARTELIA)
!+        14/06/2012
!+        V6P2
!+   Addition of tracer degradation law treatment
!
!history  J-M HERVOUET (LNHE)
!+        26/07/2012
!+        V6P2
!+   In parallel, P_DSUM on MASSOU must be done once at the end
!
!history  C.COULET (ARTELIA)
!+        14/06/2013
!+        V6P2
!+   Modification for weirs (type 2) management
!
!history  D WANG & P TASSI (LNHE)
!+        10/07/2014
!+        V7P0
!+   Secondary flow correction:
!+   first calculate the local radius r_sec,
!+   then set the production and dissipation terms of Omega,
!
!history  R. ATA (LNHE)
!+        10/11/2014
!+        V7P0
!+   add new water quality processes
!
!history  J-M HERVOUET (LNHE)
!+        08/06/2015
!+        V7P1
!+   Treatment of sources modified for distributive schemes.
!
!history  J-M HERVOUET (LNHE)
!+        18/09/2015
!+        V7P1
!+   FAC is now an integer. NREJET is now the number of sources, before
!+   NREJTR was sent by telemac2d.f.
!
!history  R. ATA (LNHE)
!+        02/11/2015
!+        V7P1
!+   Updates for water quality: new subroutine for weir reaeration
!
!history  C. COULET (ARTELIA)
!+        01/09/2016
!+        V7P2
!+   Tentative update for weirs type 2 (parallel treatment)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DBUS           |-->| DISCHARGE OF TUBES.
!| DSCE           |-->| DISCHARGE OF POINT SOURCES
!| DSIP           |-->| DISCHARGE OF CULVERT.
!| DT             |-->| TIME STEP
!| ENTBUS         |-->| INDICES OF ENTRY OF TUBES IN GLOBAL NUMBERING
!| ENTSIP         |-->| INDICES OF ENTRY OF PIPE IN GLOBAL NUMBERING
!| FAC            |-->| IN PARALLEL :
!|                |   | 1/(NUMBER OF SUB-DOMAINS OF THE POINT)
!| HPROP          |-->| PROPAGATION DEPTH
!| ISCE           |-->| NEAREST POINTS OF DISCHARGES
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NBUSE          |-->| NUMBER OF TUBES
!| NREJET         |-->| NUMBER OF POINT SOURCES.
!| NSIPH          |-->| NUMBER OF CULVERTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| NWEIRS         |-->| NUMBER OF WEIRS
!| SORBUS         |-->| INDICES OF TUBES EXITS IN GLOBAL NUMBERING
!| SORSIP         |-->| INDICES OF PIPES EXITS IN GLOBAL NUMBERING
!| TBUS           |-->| VALUES OF TRACERS AT TUBES EXTREMITY
!| TETAT          |-->| COEFFICIENT OF IMPLICITATION FOR TRACERS.
!| TEXP           |-->| EXPLICIT SOURCE TERM.
!| TIMP           |-->| IMPLICIT SOURCE TERM.
!| TN             |-->| TRACERS AT TIME N
!| TSCE           |-->| PRESCRIBED VALUES OF TRACERS AT POINT SOURCES
!| TSCEXP         |<--| EXPLICIT SOURCE TERM OF POINT SOURCES
!|                |   | IN TRACER EQUATION, EQUAL TO:
!|                |   | TSCE - ( 1 - TETAT ) TN
!| TSIP           |-->| VALUES OF TRACERS AT CULVERT EXTREMITY
!| TWEIRA         |-->| VALUES OF TRACERS ON SIDE A OF WEIR
!| TWEIRB         |-->| VALUES OF TRACERS ON SIDE B OF WEIR
!| TYPSEUIL       |-->| TYPE OF WEIRS (IF = 2, WEIRS TREATED AS SOURCES POINTS)
!| YASMI          |<--| IF YES, THERE ARE IMPLICIT SOURCE TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_TELEMAC2D, ONLY: LOITRAC, COEF1TRAC, QWA, QWB,
     &  MAXNPS,U,V,UNSV2D,V2DPAR,VOLU2D,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
     &  T11,T12,MESH,MSK,
     &  IELMU,S,NPOIN,CF,H,SECCURRENTS,SEC_AS,SEC_DS,SEC_R,IND_T,LT,
     &  ICONVFT,OPTADV_TR,PATMOS,LISTIN,GRAV,ZF,DEBUG,IND_S,MASKEL,
     &  MARDAT,MARTIM,LAMBD0,PHI0,
     &  WNODES_PROC,WNODES
      USE DECLARATIONS_WAQTEL,ONLY: FORMRS,O2SATU,ADDTR,WAQPROCESS,
     &  WATTEMP,RSW,ABRS,RAYEFF
      USE INTERFACE_WAQTEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: ISCE(*),NREJET,NTRAC
      INTEGER          , INTENT(IN)    :: NSIPH,NBUSE,NWEIRS
      INTEGER          , INTENT(IN)    :: N_NGHB_W_NODES
      INTEGER          , INTENT(IN)    :: ENTSIP(NSIPH),SORSIP(NSIPH)
      INTEGER          , INTENT(IN)    :: ENTBUS(NBUSE),SORBUS(NBUSE)
      INTEGER          , INTENT(IN)    :: MAXSCE,MAXTRA,TYPSEUIL
      INTEGER          , INTENT(IN)    :: FAC(*)
      LOGICAL          , INTENT(INOUT) :: YASMI(*)
      DOUBLE PRECISION , INTENT(IN)    :: AT,DT,TETAT,DSCE(*)
      DOUBLE PRECISION , INTENT(IN)    :: DSIP(NSIPH),DBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: TSCE(MAXSCE,MAXTRA)
      DOUBLE PRECISION , INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: TN,HPROP,TSIP,TBUS
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: TWEIRA,TWEIRB
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: NDGA1,NDGB1
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TSCEXP,TEXP,TIMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,IR,ITRAC,N,INDIC,NTRA
      LOGICAL DISTRI
!
      DOUBLE PRECISION DEBIT,TRASCE
      DOUBLE PRECISION DENOM,NUMER,NORM2,SEC_RMAX,RMAX
!
      DOUBLE PRECISION H1,H2,TRUP,TRDO,AB,DZ
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-6
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!     SECONDARY CURRENTS WILL BE TREATED APART
!
      NTRA=NTRAC
      IF(SECCURRENTS) NTRA=NTRA-1
!
!-----------------------------------------------------------------------
!
!     EXPLICIT SOURCE TERMS
!
      DO ITRAC=1,NTRA
        CALL OS('X=0     ',X=TSCEXP%ADR(ITRAC)%P)
        CALL OS('X=0     ',X=TEXP%ADR(ITRAC)%P)
        MASSOU(ITRAC) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!     INITIALIALIZATION OF YASMI
      IF(LT.EQ.1)THEN
        DO ITRAC=1,NTRA   
          IF(LOITRAC(ITRAC).EQ.0) THEN
            YASMI(ITRAC)=.FALSE.
          ELSEIF(LOITRAC(ITRAC).EQ.1) THEN
            YASMI(ITRAC)=.TRUE.
          ELSE
            IF(LNG.EQ.1) WRITE(LU,*) 'DIFSOU : LOI NON PROGRAMMEE'
            IF(LNG.EQ.2) WRITE(LU,*) 'DIFSOU : LAW NOT IMPLEMENTED'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
!       WHEN COUPLING WITH WAQTEL, PREPARE IMPLICIT SOURCE TERMS
!
        IF(INCLUS(COUPLING,'WAQTEL')) THEN
          CALL YASMI_WAQ(NTRA,YASMI)
        ENDIF
      ENDIF
!
!     IMPLICIT SOURCE TERMS (DEPENDING ON THE LAW CHOSEN)
!
      DO ITRAC=1,NTRA
        IF(LOITRAC(ITRAC).EQ.1) THEN
          CALL OS('X=CY    ',X=TIMP%ADR(ITRAC)%P,Y=HPROP,
     &            C=-2.3D0/COEF1TRAC(ITRAC)/3600.D0)
        ENDIF
      ENDDO
!
!                                   N+1
!     EXAMPLE WHERE WE ADD -0.0001 F      IN THE RIGHT HAND-SIDE
!     OF THE TRACER EQUATION THAT BEGINS WITH DF/DT=...
!     (T12=SMI WILL BE DIVIDED BY HPROP IN CVDFTR, THE EQUATION IS:
!     DT/DT=...+SMI*T(N+1)/H
!
!     HERE THIS IS DONE FOR TRACER 3 ONLY IN A RECTANGULAR ZONE
!
!     CALL OS('X=0     ',X=TIMP%ADR(3)%P)
!     DO I=1,HPROP%DIM1
!       IF(X(I).GE.263277.D0.AND.X(I).LE.265037.D0) THEN
!       IF(Y(I).GE.379007.D0.AND.Y(I).LE.380326.D0) THEN
!         TIMP%ADR(3)%P%R(I)=-0.00001D0*HPROP%R(I)
!       ENDIF
!       ENDIF
!     ENDDO
!     YASMI(3)=.TRUE.
!
!-----------------------------------------------------------------------
!
!  TAKES THE SOURCES OF TRACER INTO ACCOUNT
!
!-----------------------------------------------------------------------
!
      DO ITRAC=1,NTRA
!
        IF(NREJET.GT.0) THEN
!
          DO I = 1 , NREJET
!
            IR = ISCE(I)
!           TEST IR.GT.0 FOR THE PARALLELISM
            IF(IR.GT.0) THEN
              DEBIT=DSCE(I)
              IF(DEBIT.GT.0.D0) THEN
                TRASCE = TSCE(I,ITRAC)
              ELSE
!               THE VALUE AT THE SOURCE IS TN IF THE FLOW IS OUTGOING
!               IT WILL BE WRONG BUT NOT CONSIDERED FOR LOCALLY IMPLICIT
!               SCHEMES
                TRASCE = TN%ADR(ITRAC)%P%R(IR)
              ENDIF
!
!             SCHEME SENSITIVE, HERE NOT FOR LOCALLY IMPLICIT SCHEMES
!             BECAUSE THEY WILL DO THE JOB THEMSELVES
!
              DISTRI=.FALSE.
              IF(ICONVFT(ITRAC).EQ.ADV_NSC) DISTRI=.TRUE.
              IF(ICONVFT(ITRAC).EQ.ADV_PSI) DISTRI=.TRUE.
!
              IF(.NOT.DISTRI) THEN
!               SOURCE TERM ADDED TO THE MASS OF TRACER
                IF(NCSIZE.GT.1) THEN
!                 FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!                 (SEE CALL TO P_DSUM BELOW)
                  MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE*FAC(IR)
                ELSE
                  MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE
                ENDIF
                TRASCE = TRASCE - (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
              ENDIF
              TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR)+TRASCE
!
!             THE IMPLICIT PART OF THE TERM - T * SCE
!             IS DEALT WITH IN CVDFTR.
!
            ENDIF
!
          ENDDO
!
        ENDIF
!
        IF(NSIPH.GT.0) THEN
          DO I = 1 , NSIPH
            IR = ENTSIP(I)
            IF(IR.GT.0) THEN
              IF(NCSIZE.GT.1) THEN
!               FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!               (SEE CALL TO P_DSUM BELOW)
                MASSOU(ITRAC)=MASSOU(ITRAC)-DT*DSIP(I)*
     &                        TSIP%ADR(ITRAC)%P%R(I)*FAC(IR)
              ELSE
                MASSOU(ITRAC)=MASSOU(ITRAC)-DT*DSIP(I)*
     &                        TSIP%ADR(ITRAC)%P%R(I)
              ENDIF
              TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &           TSIP%ADR(ITRAC)%P%R(I) -
     &           (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
            ENDIF
            IR = SORSIP(I)
            IF(IR.GT.0) THEN
              IF(NCSIZE.GT.1) THEN
!               FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!               (SEE CALL TO P_DSUM BELOW)
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DSIP(I)*
     &                        TSIP%ADR(ITRAC)%P%R(NSIPH+I)*FAC(IR)
              ELSE
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DSIP(I)*
     &                        TSIP%ADR(ITRAC)%P%R(NSIPH+I)
              ENDIF
              TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &           TSIP%ADR(ITRAC)%P%R(NSIPH+I) -
     &           (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
            ENDIF
          ENDDO
        ENDIF
!
        IF(NBUSE.GT.0) THEN
          DO I = 1 , NBUSE
            IR = ENTBUS(I)
            IF(IR.GT.0) THEN
              IF(NCSIZE.GT.1) THEN
!               FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!               (SEE CALL TO P_DSUM BELOW)
                MASSOU(ITRAC)=MASSOU(ITRAC)-DT*DBUS(I)*
     &                        TBUS%ADR(ITRAC)%P%R(I)*FAC(IR)
              ELSE
                MASSOU(ITRAC)=MASSOU(ITRAC)-DT*DBUS(I)*
     &                        TBUS%ADR(ITRAC)%P%R(I)
              ENDIF
              TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &           TBUS%ADR(ITRAC)%P%R(I) -
     &           (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
            ENDIF
            IR = SORBUS(I)
            IF(IR.GT.0) THEN
              IF(NCSIZE.GT.1) THEN
!               FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!               (SEE CALL TO P_DSUM BELOW)
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DBUS(I)*
     &                        TBUS%ADR(ITRAC)%P%R(NBUSE+I)*FAC(IR)
              ELSE
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DBUS(I)*
     &                        TBUS%ADR(ITRAC)%P%R(NBUSE+I)
              ENDIF
              TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &           TBUS%ADR(ITRAC)%P%R(NBUSE+I) -
     &           (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
            ENDIF
          ENDDO
        ENDIF
!
        IF(NWEIRS.GT.0.AND.TYPSEUIL.EQ.2) THEN
          DO N=1,N_NGHB_W_NODES
            IF(WNODES_PROC(N)%NUM_NEIGH.EQ.IPID) GOTO 50
          ENDDO
50        CONTINUE
          DO I=1, WNODES_PROC(N)%NB_NODES
            IR = WNODES_PROC(N)%NUM_LOC(I)
            K  = WNODES_PROC(N)%LIST_NODES(I)
            IF(NCSIZE.GT.1) THEN
!             FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!             (SEE CALL TO P_DSUM BELOW)
              MASSOU(ITRAC)=MASSOU(ITRAC)+DT*WNODES(K)%QN*
     &                      WNODES(K)%TRAC(ITRAC)*FAC(IR)
            ELSE
              MASSOU(ITRAC)=MASSOU(ITRAC)+DT*WNODES(K)%QN*
     &                      WNODES(K)%TRAC(ITRAC)
            ENDIF
            WRITE(LU,*) 'difsou ',I,IR,K,TSCEXP%ADR(ITRAC)%P%R(IR),
     &         WNODES(K)%TRAC(ITRAC)
            IF(WNODES(K)%QN.GT.0.D0) THEN
            TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &         WNODES(K)%TRAC(ITRAC) -
     &         (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
            ELSE
            TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &         WNODES(K)%TRAC(ITRAC) -
     &         (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
            ENDIF
!
!              H1 = 0.D0
!              TRUP = 0.D0
!              IF(IR.GT.0) THEN
!!               RECUPERATE H FOR WAQ (O2 OR EUTRO)
!                IF(INCLUS(COUPLING,'WAQTEL').AND.(WAQPROCESS.EQ.1.OR.
!     &                                            WAQPROCESS.EQ.3))THEN
!                  H1   = HPROP%R(IR)
!                  TRUP = TN%ADR(NTRAC-ADDTR+1)%P%R(IR)
!                  IF(NCSIZE.GT.1)THEN 
!                    H1   = P_DMIN(H1  )+P_DMAX(H1  )
!                    TRUP = P_DMIN(TRUP)+P_DMAX(TRUP)
!                  ENDIF
!                ENDIF
!              ENDIF
!              H2   = 0.D0
!              TRDO = 0.D0
!              IF(IR.GT.0) THEN
!!               RECUPERATE H FOR WAQ (O2 OR EUTRO)
!                IF(INCLUS(COUPLING,'WAQTEL').AND.(WAQPROCESS.EQ.1.OR.
!     &                                            WAQPROCESS.EQ.3))THEN
!                  H2  = HPROP%R(IR)
!                  IF(NCSIZE.GT.1)THEN 
!                    H2   = P_DMIN(H2  )+P_DMAX(H2  )
!                  ENDIF
!                ENDIF
!!               CONTRIBUTION TO WAQ
!                IF(INCLUS(COUPLING,'WAQTEL').AND.(WAQPROCESS.EQ.1.OR.
!    &                                             WAQPROCESS.EQ.3))THEN                  DZ= ABS(H2-H1)
!       warning: this process is a bit strange and then difficult to
!                implement: impose that tracer TN increases spontaneously
!                under the effect of "nothing" (sources,boundary conditions... )
!                needs to think more about it.
!                  CALL REAER_WEIR (FORMRS,H1,H2,ABRS,WATTEMP,EPS,
!     &                             O2SATU,TRUP,TN,ADDTR,WAQPROCESS,
!     &                             IR,NTRAC)

!                ENDIF
!              ENDIF               
          ENDDO
        ENDIF
!
        IF(NCSIZE.GT.1.AND.
     &     (NREJET.GT.0.OR.NSIPH.GT.0.OR.NBUSE.GT.0.OR.
     &     (NWEIRS.GT.0.AND.TYPSEUIL.EQ.2))) THEN
           MASSOU(ITRAC)=P_DSUM(MASSOU(ITRAC))
        ENDIF
!
      ENDDO
!
!     WATER QUALITY CONTRIBUTION TO TRACER SOURCES
!
      IF(INCLUS(COUPLING,'WAQTEL')) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_WAQ'
        CALL SOURCE_WAQ
     & (NPOIN,NPOIN,TEXP,TIMP,TN,NTRAC,WAQPROCESS,RAYEFF,IND_T,IND_S,H,
     &  HPROP,U,V,CF,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T1,T2,T3,
     &  PATMOS,LISTIN,GRAV,ZF,DEBUG,MASSOU,DT,2,VOLU2D,1,LAMBD0,PHI0,
     &  AT,MARDAT,MARTIM,MESH%X)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SOURCE_WAQ'
      ENDIF

!
!-----------------------------------------------------------------------
!
!     SECONDARY CURRENTS (OMEGA IS THE TRACER OF RANK NTRAC)
!
      IF(SECCURRENTS) THEN
!
        CALL VECTOR(T1,'=','GRADF          Y',IELMU,
     &              1.D0,V,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(T2,'=','GRADF          X',IELMU,
     &              1.D0,U,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(T3,'=','GRADF          X',IELMU,
     &              1.D0,V,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(T4,'=','GRADF          Y',IELMU,
     &              1.D0,U,S,S,S,S,S,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM (T1, 2, MESH)
          CALL PARCOM (T2, 2, MESH)
          CALL PARCOM (T3, 2, MESH)
          CALL PARCOM (T4, 2, MESH)
        ENDIF
!
!       INITIALISATIONS
!
        CALL OS('X=0     ',X=TSCEXP%ADR(NTRAC)%P)
        YASMI(NTRAC)=.TRUE.
!
!       SOURCE TERMS
!
        DO K=1,NPOIN
          NORM2=U%R(K)**2+V%R(K)**2
          NUMER = (U%R(K)*V%R(K)*(T1%R(K)-T2%R(K))+U%R(K)**2*(T3%R(K))
     &           -V%R(K)**2*(T4%R(K)))*UNSV2D%R(K)
          SEC_R%R(K)=NUMER/MAX(SQRT(NORM2)**3,1.D-9)
!         GEOMETRY: R OBVIOUSLY LARGER THAN 0.5 LOCAL MESH SIZE
!         THEORY ALSO SAYS R > 2H
!         LOCAL MESH SIZE HERE ASSUMED TO BE SQRT(V2DPAR)
          RMAX=MAX(2.D0*H%R(K),0.5D0*SQRT(V2DPAR%R(K)))
!         RMAX=0.5D0*SQRT(V2DPAR%R(K))
          SEC_RMAX=1.D0/RMAX
          SEC_R%R(K)=MAX(-SEC_RMAX,MIN(SEC_RMAX,SEC_R%R(K)))
!         EXPLICIT SOURCE TERMS (CREATION OF OMEGA)
!         CLIPPING OF H AT 1.D-2
!         NOTE: IMPLICIT TERMS (DESTRUCTION) IN CVDFTR CLIPPED AT 1.D-4
          DENOM=MAX(H%R(K),1.D-2)*(9.D0*(H%R(K)*SEC_R%R(K))**2+1.D0)
          TEXP%ADR(NTRAC)%P%R(K)=
     &                 SEC_AS*SQRT(0.5D0*CF%R(K))*NORM2*SEC_R%R(K)/DENOM
!         IMPLICIT SOURCE TERMS (DEPENDING ON THE LAW CHOSEN)
          TIMP%ADR(NTRAC)%P%R(K)=-SEC_DS*SQRT(0.5D0*CF%R(K)*NORM2)
        ENDDO
!
!       MASS ADDED BY EXPLICIT TERMS
!       THE MASS ADDED BY IMPLICIT TERMS IS COMPUTED IN CVDFTR
!
        MASSOU(NTRAC) = 0.D0
        DO K=1,NPOIN
          MASSOU(NTRAC)=MASSOU(NTRAC)
     &                 +H%R(K)*TEXP%ADR(NTRAC)%P%R(K)*VOLU2D%R(K)
        ENDDO
        MASSOU(NTRAC)=MASSOU(NTRAC)*DT
        IF(NCSIZE.GT.1) MASSOU(NTRAC)=P_DSUM(MASSOU(NTRAC))
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

