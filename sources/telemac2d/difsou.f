!                    *****************
                     SUBROUTINE DIFSOU
!                    *****************
!
     &(TEXP,TIMP,YASMI,TSCEXP,HPROP,TN,TETAT,NREJTR,ISCE,DSCE,TSCE,
     & MAXSCE,MAXTRA,AT,DT,MASSOU,NTRAC,FAC,NSIPH,ENTSIP,SORSIP,
     & DSIP,TSIP,NBUSE,ENTBUS,SORBUS,DBUS,TBUS,NWEIRS,TYPSEUIL,
     & NPSING,NDGA1,NDGB1,TWEIRA,TWEIRB)
!
!***********************************************************************
! TELEMAC2D   V7P0
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
!+
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
!| NREJTR         |-->| NUMBER OF POINT SOURCES AS GIVEN BY TRACERS KEYWORDS
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
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_TELEMAC2D, ONLY : LOITRAC, COEF1TRAC, QWA, QWB,
     &   MAXNPS,U,V,UNSV2D,V2DPAR,VOLU2D,T1,T2,T3,T4,MESH,MSK,MASKEL,
     &   IELMU,S,NPOIN,CF,H,SECCURRENTS,SEC_AS,SEC_DS,SEC_R,WATQUA,IND_T
      USE DECLARATIONS_WAQTEL,ONLY: WAQPROCESS,FORMRS,O2SATU,ADDTR,
     &                              WATTEMP,RSW,ABRS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: ISCE(*),NREJTR,NTRAC
      INTEGER          , INTENT(IN)    :: NSIPH,NBUSE,NWEIRS
      INTEGER          , INTENT(IN)    :: ENTSIP(NSIPH),SORSIP(NSIPH)
      INTEGER          , INTENT(IN)    :: ENTBUS(NBUSE),SORBUS(NBUSE)
      INTEGER          , INTENT(IN)    :: MAXSCE,MAXTRA,TYPSEUIL
      LOGICAL          , INTENT(INOUT) :: YASMI(*)
      DOUBLE PRECISION , INTENT(IN)    :: AT,DT,TETAT,DSCE(*)
      DOUBLE PRECISION , INTENT(IN)    :: DSIP(NSIPH),DBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: TSCE(MAXSCE,MAXTRA),FAC(*)
      DOUBLE PRECISION , INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: TN,HPROP,TSIP,TBUS
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: TWEIRA,TWEIRB
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: NPSING,NDGA1,NDGB1
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TSCEXP,TEXP,TIMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,IR,ITRAC,N,INDIC,NTRA
!
      DOUBLE PRECISION DEBIT,TRASCE
      DOUBLE PRECISION DENOM,NUMER,NORM2,SEC_RMAX,RMAX
!
      DOUBLE PRECISION H1,H2,TRUP,TRDO,AB,DZ
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-6
!
!      DOUBLE PRECISION P_DSUM,P_DMIN,P_DMAX
!      EXTERNAL         P_DSUM,P_DMIN,P_DMAX
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
!
!     IMPLICIT SOURCE TERMS (DEPENDING ON THE LAW CHOSEN)
!
      DO ITRAC=1,NTRA
        IF(LOITRAC(ITRAC).EQ.0) THEN
          YASMI(ITRAC)=.FALSE.
        ELSEIF(LOITRAC(ITRAC).EQ.1) THEN
          YASMI(ITRAC)=.TRUE.
          CALL OS('X=CY    ',X=TIMP%ADR(ITRAC)%P,Y=HPROP,
     &            C=-2.3D0/COEF1TRAC(ITRAC)/3600.D0)
        ELSE
          IF(LNG.EQ.1) WRITE(LU,*) 'DIFSOU : LOI NON PROGRAMMEE'
          IF(LNG.EQ.2) WRITE(LU,*) 'DIFSOU : LAW NOT IMPLEMENTED'
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
        IF(NREJTR.GT.0) THEN
!
          DO I = 1 , NREJTR
!
            IR = ISCE(I)
!           TEST IR.GT.0 FOR THE PARALLELISM
            IF(IR.GT.0) THEN
              DEBIT=DSCE(I)
              IF(DEBIT.GT.0.D0) THEN
                TRASCE = TSCE(I,ITRAC)
              ELSE
!               THE VALUE AT THE SOURCE IS TN IF THE FLOW IS OUTGOING
                TRASCE = TN%ADR(ITRAC)%P%R(IR)
              ENDIF
!             SOURCE TERM ADDED TO THE MASS OF TRACER
              IF(NCSIZE.GT.1) THEN
!               FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!               (SEE CALL TO P_DSUM BELOW)
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE*FAC(IR)
              ELSE
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE
              ENDIF
              TRASCE = TRASCE - (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
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
          DO N = 1 , NWEIRS
            DO I = 1 , NPSING%I(N)
              INDIC = (N-1)*MAXNPS + I
              IR = NDGA1%ADR(N)%P%I(I)
              H1 = 0.D0
              TRUP = 0.D0
              IF(IR.GT.0) THEN
                IF(NCSIZE.GT.1) THEN
!                 FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!                 (SEE CALL TO P_DSUM BELOW)
                  MASSOU(ITRAC)=MASSOU(ITRAC)-DT*QWA%ADR(N)%P%R(I)*
     &                          TWEIRA%ADR(ITRAC)%P%R(INDIC)*FAC(IR)
                ELSE
                  MASSOU(ITRAC)=MASSOU(ITRAC)-DT*QWA%ADR(N)%P%R(I)*
     &                          TWEIRA%ADR(ITRAC)%P%R(INDIC)
                ENDIF
                TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &             TWEIRA%ADR(ITRAC)%P%R(INDIC) -
     &             (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
!               RECUPERATE H FOR WAQ
                IF(WATQUA.AND.WAQPROCESS.EQ.1)THEN
                  H1   = HPROP%R(IR)
                  TRUP = TN%ADR(NTRAC-ADDTR+1)%P%R(IR)
                  IF(NCSIZE.GT.1)THEN
                    H1   = P_DMIN(H1  )+P_DMAX(H1  )
                    TRUP = P_DMIN(TRUP)+P_DMAX(TRUP)
                  ENDIF
                ENDIF
              ENDIF
              IR = NDGB1%ADR(N)%P%I(I)
              H2   = 0.D0
              TRDO = 0.D0
              IF(IR.GT.0) THEN
                IF(NCSIZE.GT.1) THEN
!                 FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!                 (SEE CALL TO P_DSUM BELOW)
                  MASSOU(ITRAC)=MASSOU(ITRAC)+DT*QWB%ADR(N)%P%R(I)*
     &                          TWEIRB%ADR(ITRAC)%P%R(INDIC)*FAC(IR)
                ELSE
                  MASSOU(ITRAC)=MASSOU(ITRAC)+DT*QWB%ADR(N)%P%R(I)*
     &                          TWEIRB%ADR(ITRAC)%P%R(INDIC)
                ENDIF
                TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &             TWEIRB%ADR(ITRAC)%P%R(INDIC) -
     &             (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
!               RECUPERATE H FOR WAQ
                IF(WATQUA.AND.WAQPROCESS.EQ.1)THEN
                  H2  = HPROP%R(IR)
                  IF(NCSIZE.GT.1)THEN
                    H2   = P_DMIN(H2  )+P_DMAX(H2  )
                  ENDIF
                ENDIF
!               CONTRIBUTION TO WAQ
                IF(WATQUA.AND.WAQPROCESS.EQ.1)THEN
                  DZ= ABS(H2-H1)
                  RSW = 0.D0
!                 LETS'S COMPUTE RS IF IT IS NOT TAKEN CONSTANT
                  IF(FORMRS.NE.0) THEN
                    AB=ABRS(1)*ABRS(2)
!                   GAMESON FORMULA 1
                    IF(FORMRS.EQ.1)     THEN
                      RSW=1.D0+0.5D0*AB*DZ
!                   GAMESON FORMULA2
                    ELSEIF(FORMRS.EQ.2) THEN
                      RSW = 0.11D0*AB*(1.D0+0.046D0*WATTEMP)*DZ
!                   WRL FORMULA 1 (NO NEED TO AB ? )
                    ELSEIF(FORMRS.EQ.3 )THEN
                      RSW = 1.D0+0.69D0*DZ*(1.D0-0.11D0*DZ )
     &                      *( 1.D0+0.046D0*WATTEMP)
!                   WRL FORMULA 2
                    ELSEIF (FORMRS.EQ.4)THEN
                      RSW = 1.D0+0.38D0*AB*DZ*(1.D0-0.11D0*DZ)
     &                      * (1.D0+0.046D0*WATTEMP )
                    ELSE
                      IF(LNG.EQ.1)THEN
                        WRITE(LU,*)'FORMULE DE RS (REAERATION AU SEUIL)'
                        WRITE(LU,*)'INCONNUE  :',FORMRS
                        WRITE(LU,*)'LES CHOIX POSSIBLES SONT DE 1 A 4'
                      ELSE
                        WRITE(LU,*)'FORMULA FOR RS (WEIR REAERATION) '
                        WRITE(LU,*)' NOT VALID  :',FORMRS
                        WRITE(LU,*)'POSSIBLE CHOICES ARE FROM 1 TO 4'
                      ENDIF
                      CALL PLANTE(1)
                      STOP
                    ENDIF
!
!                   FORCING O2 DENSITY DOWNSTREAM THE WEIR
!
                    IF(ABS(RSW).GT.EPS)THEN
                      TRDO = O2SATU + (TRUP-O2SATU)/RSW
                    ELSE
                      WRITE(LU,*)'DIFSOU:RSW VERY SMALL',RSW
                      CALL PLANTE(1)
                      STOP
                    ENDIF
                    IF(NCSIZE.GT.1)TRDO = P_DMIN(TRDO)+P_DMAX(TRDO)
                    TN%ADR(NTRAC-ADDTR+1)%P%R(IR)=TRDO
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!
        IF(NCSIZE.GT.1.AND.
     &     (NREJTR.GT.0.OR.NSIPH.GT.0.OR.NBUSE.GT.0.OR.
     &      (NWEIRS.GT.0.AND.TYPSEUIL.EQ.2))) THEN
          MASSOU(ITRAC)=P_DSUM(MASSOU(ITRAC))
        ENDIF
!
      ENDDO
!
!     WATER QUALITY CONTRIBUTION TO TRACER SOURCES
      IF(WATQUA)THEN
        CALL SOURCE_WAQ(NPOIN,TEXP,TIMP,YASMI,TSCEXP,HPROP,TN,TETAT,
     &                  AT,DT,NTRAC,WAQPROCESS)
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

