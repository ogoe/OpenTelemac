!                    *****************
                     SUBROUTINE CONDIN
!                    *****************
!
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE PHYSICAL PARAMETERS H, U, V ETC.
!
!history  J-M HERVOUET (LNHE)
!+        30/08/2007
!+        V6P0
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE TIME
!
      AT = 0.D0
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE VELOCITIES: ZERO VELOCITIES
!
      CALL OS( 'X=0     ' , X=U )
      CALL OS( 'X=0     ' , X=V )
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE WATER DEPTH H
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=0     ' , X=H )
        CALL OS( 'X=X-Y   ' , X=H , Y=ZF )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , HAUTIN )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!  TO BE MODIFIED BY USER
!  MODIFIED BY QINGHUI ZHANG 15 JULY 2013
!  FOR THE PURPOSE OF PRECSCRIBING A CONSTANT INITIAL DEPTH AND VELOCITY
        DO I=1,NPOIN
!
          H%R(I) = 10.D0
          U%R(I) = 3.D-2
          V%R(I) = 0.D0
!
        ENDDO

!  END OF CODE TO BE MODIFIED BY USER
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIN: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE TRACERS
!
! MODIFIED BY QINGHUI ZHANG 15 JULY 2013
! PURPOSE IS TO PRESCRIBE A SPACE-DEPENDENT INITIAL TRACER VALUES
! THE TRACER VALUES FOR LEFT BOUNDARY NODES ARE 30
! CENCENTRATION FOR THE REST PART OF LEFT BOUNDARY ELEMENTS ARE INTERPOLATED
! FOR REST PART OF THE DOMAIN, CONCENTRATION IS 0
! THE MESH ELEMENT LENGTH ALONG CHANNEL BANK IS 40 METERS
! SO HERE IF THE X COORDINATE OF NODE LESS THAN 5 METERS, INITIAL VALUE: 30
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
            DO I =1, NPOIN
                IF(MESH%X%R(I).LT.(5.D0)) THEN
                T%ADR(ITRAC)%P%R(I) =3.D1
            ELSE
                T%ADR(ITRAC)%P%R(I) =0.D-0
                ENDIF
            ENDDO
        ENDDO
      ENDIF


!
!-----------------------------------------------------------------------
!
! INITIALISES THE VISCOSITY
!
      CALL OS( 'X=C     ' , X=VISC , C=PROPNU )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    ***************
                     SUBROUTINE BORD
!                    ***************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     & NOMIMP)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOUNDARY CONDITIONS ARRAYS
!+                WHEN THEY VARY IN TIME.
!+
!
!note     THIS SUBROUTINE CAN BE COMPLETED BY THE USER DIRECTLY,
!+         OR THROUGH THE FUNCTIONS: Q , SL , TR , VIT
!
!history  J-M HERVOUET (LNHE)
!+        27/03/2008
!+        V5P9
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
!history  J-M HERVOUET (LNHE)
!+        06/04/2012
!+        V6P2
!+   Original point numbers sent to functions SL, VIT and TR
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EQUA           |-->| STRING DESCRIBING THE EQUATIONS SOLVED
!| H              |-->| DEPTH AT TIME N
!| HBOR           |<->| PRESCRIBED DEPTH
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| MASK           |-->| BLOCK OF MASKS FOR DIFFERENT BOUNDARY CONDITIONS
!| MESH           |-->| MESH STRUCTURE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCOTE          |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NDEBIT         |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NOMIMP         |-->| NAME OF LIQUID BOUNDARIES FILE
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTFR2         |-->| NUMBER OF QUADRATIC BOUNDARY POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| NTRACE         |-->| NUMBER OF BOUNDARIES WITH TRACER PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| NVITES         |-->| NUMBER OF BOUNDARIES WITH VELOCITY PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| PROVEL         |-->| OPTION FOR VELOCITY PROFILES
!| TBOR           |<--| BLOCK WITH PRESCRIBED VALUES OF TRACERS
!| TEMPS          |-->| TIME IN SECONDS
!| TRA05          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| TRA06          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| U              |-->| X-COMPONENT OF VELOCITY AT TIME N
!| V              |-->| Y-COMPONENT OF VELOCITY AT TIME N
!| UBOR           |<->| X-COMPONENT OF PRESCRIBED VELOCITY
!| VBOR           |<->| Y-COMPONENT OF PRESCRIBED VELOCITY
!| XNEBOR         |-->| X-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!| YNEBOR         |-->| Y-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_BORD => BORD
      USE DECLARATIONS_TELEMAC2D, ONLY: STA_DIS_CURVES,PTS_CURVES,QZ,
     &                                  FLUX_BOUNDARIES,MAXFRO,TIDALTYPE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE
      INTEGER, INTENT(IN) :: KENT,KENTU,NFRLIQ,NTRAC,NPTFR2
      INTEGER, INTENT(IN) :: PROVEL(*),LIHBOR(NPTFR),LIUBOR(NPTFR2)
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR),NBOR(NPTFR2)
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      CHARACTER(LEN=20), INTENT(IN)   :: EQUA
      CHARACTER(LEN=144), INTENT(IN)  :: NOMIMP
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR2,2),VBOR(NPTFR2,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,U,V,TRA05,TRA06,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASK,LITBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,MSK8,IFRLIQ,YADEB(MAXFRO),IERR,ITRAC,IFR,N
!
      DOUBLE PRECISION Z,ZMIN(MAXFRO)
!
      LOGICAL YAZMIN
!
      DOUBLE PRECISION P_DMIN
      INTEGER  P_IMAX
      EXTERNAL P_IMAX,P_DMIN
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
!     IF VELOCITY PROFILE OPTION 5: MINIMUM ELEVATION OF EVERY BOUNDARY
!
      YAZMIN=.FALSE.
      DO IFR=1,NFRLIQ
        ZMIN(IFR)=1.D99
        IF(PROVEL(IFR).EQ.5) YAZMIN=.TRUE.
      ENDDO
      IF(YAZMIN) THEN
        DO K=1,NPTFR
          IFR=NUMLIQ(K)
          ZMIN(IFR)=MIN(ZMIN(IFR),ZF(NBOR(K))+H%R(NBOR(K)))
        ENDDO
        IF(NCSIZE.GT.1) THEN
          DO IFR=1,NFRLIQ
            ZMIN(IFR)=P_DMIN(ZMIN(IFR))
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      MSK8 = 8
!
!  INITIALISATION OF YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!  LOOP ON ALL BOUNDARY POINTS
!
      DO K=1,NPTFR
!
!  LEVEL IMPOSED WITH VALUE GIVEN IN THE CAS FILE (NCOTE0)
!
      IF(LIHBOR(K).EQ.KENT) THEN
!
        IFRLIQ=NUMLIQ(K)
!
        IF(STA_DIS_CURVES(IFRLIQ).EQ.1) THEN
          Z = STA_DIS_CUR(IFRLIQ,FLUX_BOUNDARIES(IFRLIQ),
     &                    PTS_CURVES(IFRLIQ),QZ,NFRLIQ,
     &                    ZF(NBOR(K))+H%R(NBOR(K)))
          HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
          H%R(NBOR(K))=HBOR(K)
        ELSEIF(NCOTE.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
          N=NBOR(K)
          IF(NCSIZE.GT.1) N=MESH%KNOLG%I(N)
          Z = SL(IFRLIQ,N)
          HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
          H%R(NBOR(K))=HBOR(K)
!       ELSE HBOR TAKEN IN BOUNDARY CONDITIONS FILE
        ENDIF
!
      ENDIF
!
!  DISCHARGE IMPOSED: VARIOUS OPTIONS ACCORDING TO PROVEL
!                 ONE USES THE VALUES PROVIDED BY THE USER
!                 AS VELOCITY PROFILE.
!                 UBOR(K,2) AND VBOR(K,2) ARE THE VALUES OF
!                 THE CONLIM FILE, AND ARE CONSERVED.
!
      IF(LIUBOR(K).EQ.KENT.AND.
     &  (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
        IFR=NUMLIQ(K)
        IF(PROVEL(IFR).EQ.1) THEN
!         CONSTANT NORMAL PROFILE
          UBOR(K,1) = -XNEBOR(K)
          VBOR(K,1) = -YNEBOR(K)
        ELSEIF(PROVEL(IFR).EQ.2) THEN
!         PROFILE PROVIDED BY THE USER
          UBOR(K,1) = UBOR(K,2)
          VBOR(K,1) = VBOR(K,2)
        ELSEIF(PROVEL(IFR).EQ.3) THEN
!         NORMAL VELOCITY PROVIDED IN UBOR
          UBOR(K,1) = -XNEBOR(K)*UBOR(K,2)
          VBOR(K,1) = -YNEBOR(K)*UBOR(K,2)
        ELSEIF(PROVEL(IFR).EQ.4) THEN
!         NORMAL PROFILE IN SQUARE ROOT OF H
          UBOR(K,1) = -XNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
          VBOR(K,1) = -YNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
        ELSEIF(PROVEL(IFR).EQ.5) THEN
!         NORMAL PROFILE IN SQUARE ROOT OF H, BUT VIRTUAL H
!         DEDUCED FROM LOWEST FREE SURFACE OF THE BOUNDARY
          UBOR(K,1)=-XNEBOR(K)*SQRT(MAX(ZMIN(IFR)-ZF(NBOR(K)),0.D0))
          VBOR(K,1)=-YNEBOR(K)*SQRT(MAX(ZMIN(IFR)-ZF(NBOR(K)),0.D0))
        ENDIF
!       ONE DOES NOT SET VELOCITY IF THERE IS NO WATER.
        IF(H%R(NBOR(K)).LT.1.D-3) THEN
          UBOR(K,1) = 0.D0
          VBOR(K,1) = 0.D0
        ENDIF
!       U AND V INITIALISED WITH THE IMPOSED VALUES
        U%R(NBOR(K)) = UBOR(K,1)
        V%R(NBOR(K)) = VBOR(K,1)
        YADEB(NUMLIQ(K))=1
      ENDIF
!
!  VELOCITY IMPOSED: ONE USES THE OUTGOING DIRECTION
!                    PROVIDED BY THE USER.
!
      IF(LIUBOR(K).EQ.KENTU.AND.
     &  (NVITES.NE.0.OR.NOMIMP(1:1).NE.' ')) THEN
!       POINTS ON WEIRS HAVE NUMLIQ(K)=0
        IF(NUMLIQ(K).GT.0) THEN
          IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
            N=NBOR(K)
            IF(NCSIZE.GT.1) N=MESH%KNOLG%I(N)
            UBOR(K,1) = - XNEBOR(K) * VIT(NUMLIQ(K),N)
            VBOR(K,1) = - YNEBOR(K) * VIT(NUMLIQ(K),N)
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.2) THEN
            UBOR(K,1) = UBOR(K,2)
            VBOR(K,1) = VBOR(K,2)
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.3) THEN
            UBOR(K,1) = - XNEBOR(K) * UBOR(K,2)
            VBOR(K,1) = - YNEBOR(K) * UBOR(K,2)
          ELSE
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'FRONTIERE ',NUMLIQ(K)
              WRITE(LU,*) 'PROFIL ',PROVEL(NUMLIQ(K)),
     &                    ' DEMANDE AVEC VITESSES IMPOSEES'
              WRITE(LU,*) 'COMBINAISON ILLOGIQUE'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'BOUNDARY ',NUMLIQ(K)
              WRITE(LU,*) 'PROFILE ',PROVEL(NUMLIQ(K)),
     &                    ' ASKED'
              WRITE(LU,*) 'IMPOSSIBLE COMBINATION'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
!         U AND V INITIALISED WITH THE IMPOSED VALUES
          U%R(NBOR(K)) = UBOR(K,1)
          V%R(NBOR(K)) = VBOR(K,1)
        ENDIF
      ENDIF
!
!  IMPOSED TRACER
!  MODIFIED BY QINGHUI ZHANG. 15 JULY 2013
!  THE PURPOSE IS TO IMPOSE A TIME-DEPENDENT BOUNDARY TRACER VALUE
!  FOR THE FIRST 6 HOURS,TRACER CONCENTRATION IS 0 MG/L
!  AFTER, THE CONCENTRATION IS 0
!  AN ADDITION OF AN INNER LOOP HELPS TO REALIZE IT
!  IF (TEMPS.LE.2.13D4) THEN
!        TBOR%ADR(ITRAC)%P%R(K) = 3.D1
!  ELSE
!        BOR%ADR(ITRAC)%P%R(K) = 0
!  ENDIF

      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC

          IF(LITBOR%ADR(ITRAC)%P%I(K).EQ.KENT.AND.
     &      (NTRACE.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
!           THE CASE NUMLIQ(K)=0 CORRESPONDS TO A SINGULARITY INITIALLY
!           DECLARED AS A SOLID BOUNDARY AND FOR WHICH
!           TBOR IS FILLED IN CLHUVT
            IF(NUMLIQ(K).GT.0) THEN
              N=NBOR(K)
              IF(NCSIZE.GT.1) N=MESH%KNOLG%I(N)
              Z = TR(NUMLIQ(K),ITRAC,N,IERR)
              IF(IERR.EQ.0) TBOR%ADR(ITRAC)%P%R(K) = Z
              IF (TEMPS.LE.2.13D4) THEN
                TBOR%ADR(ITRAC)%P%R(K) = 3.D1
              ELSE
                TBOR%ADR(ITRAC)%P%R(K) = 0
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF


      ENDDO
!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
      IF(TIDALTYPE.GE.1) CALL TIDAL_MODEL_T2D()
!
!-----------------------------------------------------------------------
!
!  QUADRATIC VELOCITIES
!
      IF(U%ELM .EQ.13)THEN
        DO K=1,NPTFR
          IF(LIUBOR(K+NPTFR).EQ.KENT.AND.
     &  (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
        U%R(NBOR(K+NPTFR)) = (UBOR(K,1)+UBOR(MESH%KP1BOR%I(K),1))/2.D0
        V%R(NBOR(K+NPTFR)) = (VBOR(K,1)+VBOR(MESH%KP1BOR%I(K),1))/2.D0
          ENDIF
        ENDDO
      ENDIF
!
!  CASE OF DISCHARGE IMPOSED:
!
!  LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
!
      DO IFRLIQ = 1 , NFRLIQ
!
      IF(NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
!
!       ONE TAKES THE MASK OF LIQUID BOUNDARIES MSK8, WHICH IS
!       EQUAL TO THE MASK OF THE DISCHARGE IMPOSED ON A DISCHARGE
!       IMPOSED BOUNDARY. THIS MAKES IT POSSIBLE TO CHANGE A FREE
!       VELOCITY BOUNDARY TO A DISCHARGE IMPOSED TO A LEVEL IMPOSED
!       BOUNDARY, IN SPITE OF THE FACT THAT THE MASKS ARE MADE IN
!       PROPIN BEFORE THE CALL TO BORD
!
        IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
        IF(YADEB(IFRLIQ).EQ.1) THEN
          CALL DEBIMP(Q(IFRLIQ),UBOR,VBOR,U,V,H,NUMLIQ,
     &                IFRLIQ,TRA05,TRA06,
     &                NPTFR,MASK%ADR(MSK8)%P%R,MESH,MESH%KP1BOR%I,
     &                EQUA)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ENDDO
!
      ENDIF
!
! QUADRATIC VELOCITIES
!
      IF(U%ELM.EQ.13) THEN
        DO K=1,NPTFR
          UBOR(K+NPTFR,1) =(UBOR(K,1)+UBOR(MESH%KP1BOR%I(K),1))*0.5D0
          VBOR(K+NPTFR,1) =(VBOR(K,1)+VBOR(MESH%KP1BOR%I(K),1))*0.5D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
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
! TELEMAC2D   V6P2                                   21/08/2010
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
      USE DECLARATIONS_TELEMAC2D, ONLY : LOITRAC, COEF1TRAC, QWA, QWB,
     &                                   MAXNPS
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
      INTEGER I,IR,ITRAC
!
      DOUBLE PRECISION DEBIT,TRASCE,RAIN_MPS
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!-----------------------------------------------------------------------
!
!     EXPLICIT SOURCE TERMS
!
      DO ITRAC=1,NTRAC
        CALL OS('X=0     ',X=TSCEXP%ADR(ITRAC)%P)
        CALL OS('X=0     ',X=TEXP%ADR(ITRAC)%P)
        MASSOU(ITRAC) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     IMPLICIT SOURCE TERMS (DEPENDING ON THE LAW CHOSEN)
!
      DO ITRAC=1,NTRAC
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
!     EXAMPLE WHERE WE ADD -0.0001 T      IN THE RIGHT HAND-SIDE
!     OF THE TRACER EQUATION THAT BEGINS WITH DT/DT=...
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


! MODIFIED BY QINGHUI ZHANG 15 JULY 2013
! FOR THE PURPOSE OF STUDYING NON-CONSERVATIVE (DECAYING) TRACERS WITH DIFFERENT DECAY RATE
! HERE THE DECAY RATE IS :1/DAY AND 2/DAY
! 1/DAY = 1/24/3600 = -0.0000115D0

        DO ITRAC=1,NTRAC
        CALL OS('X=0     ',X=TIMP%ADR(ITRAC)%P)
        ENDDO
        DO I=1,HPROP%DIM1
 !       TIMP%ADR(1)%P%R(I)=-0.0000115D0*HPROP%R(I)
        TIMP%ADR(1)%P%R(I)=-0.0000115D0*2.*HPROP%R(I)
        ENDDO
        YASMI(1)=.TRUE.
!-----------------------------------------------------------------------
!
!  TAKES THE SOURCES OF TRACER INTO ACCOUNT
!
!-----------------------------------------------------------------------
!
      DO ITRAC=1,NTRAC
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
!       HERE PART ON TUBES HAS BEEN REMOVED
!
        IF(NCSIZE.GT.1.AND.
     &     (NREJTR.GT.0.OR.NSIPH.GT.0.OR.NBUSE.GT.0)) THEN
          MASSOU(ITRAC)=P_DSUM(MASSOU(ITRAC))
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

