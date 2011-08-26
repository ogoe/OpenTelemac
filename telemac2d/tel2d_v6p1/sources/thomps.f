!                    *****************
                     SUBROUTINE THOMPS
!                    *****************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,T,ZF,X,Y,NBOR,FRTYPE,UNA,C,
     & UCONV,VCONV,T6,T7,XCONV,YCONV,FU,FV,LIHBOR,LIUBOR,LIVBOR,
     & LITBOR,IT,T8,ITRAV2,W1R,W2R,W3R,
     & HBTIL,UBTIL,VBTIL,TBTIL,ZBTIL,SURDET,IKLE,CF,SMH,IFABOR,NELEM,
     & MESH,XNEBOR,YNEBOR,NPOIN,NPTFR,LT,TEMPS,DT,GRAV,
     & NTRAC,NFRLIQ,KSORT,KINC,KENT,KENTU,LV,MSK,MASKEL,
     & NELMAX,IELM,NORD,FAIR,WINDX,WINDY,VENT,HWIND,CORIOL,FCOR,SPHERI,
     & MAREE,MARDAT,MARTIM,PHI0,OPTSOU,ISCE,DSCE,SHPP,
     & COUROU,NPTH,VARCL,NVARCL,VARCLA,NUMLIQ,SHP,UNSV2D,HFROT,
     & FXWAVE,FYWAVE,DX_T,DY_T,DZ_T,ITBIS,IT3,IT4,HFIELD,UFIELD,VFIELD,
     & ZS,GZSX,GZSY)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    TREATS LIQUID BOUNDARIES USING THOMPSON METHOD
!+                BASED ON CHARACTERISTICS.
!
!history  J-M HERVOUET (LNHE)
!+        01/09/2008
!+
!+   POINTS GROUPED REGARDLESS OF THEIR BOUNDARY NUMBER.
!
!history  E DAVID (LHF)
!+        05/09/2008
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
!| C              |-->| WORK ARRAY: CELERITY OF WAVES
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| CORIOL         |-->| IF YES, CORIOLIS FORCE
!| COUROU         |-->| IF YES, WAVE DRIVEN CURRENTS
!| DSCE           |-->| DISCHARGE OF SOURCES
!| DT             |-->| TIME STEP
!| DX_T           |<->| WORK ARRAY
!| DY_T           |<->| WORK ARRAY
!| DZ_T           |<->| WORK ARRAY
!| ELT_T          |<->| WORK ARRAY
!| FAIR           |-->| COEFFICIENT OF WIND INFLUENCE
!| FCOR           |-->| CORIOLIS COEFFICIENT
!| FRTYPE         |-->| TYPE OF LIQUID BOUNDARIES
!| FU             |-->| SOURCE TERMS ON U
!| FV             |-->| SOURCE TERMS ON V
!| FXWAVE         |<->| FORCING OF WAVES ALONG X
!| FYWAVE         |<->| FORCING OF WAVES ALONG Y
!| GRAV           |-->| GRAVITY
!| GZSX           |<--| FREE SURFACE GRADIENT ALONG X
!| GZSY           |<--| FREE SURFACE GRADIENT ALONG Y
!| H              |-->| WATER DEPTH AT TIME N
!| HBOR           |<--| PRESCRIBED DEPTH AT BOUNDARIES
!| HBTIL          |<->| WORK ARRAY, DEPTH AT BOUNDARIES AFTER ADVECTION
!| HFIELD         |<->| WORK ARRAY, DEPTH WITH RELAXATION
!| HFROT          |-->| KEYWORD : 'DEPTH IN FRICTION TERM'
!| HWIND          |-->| THRESHOLD DEPTH FOR WIND
!| IELM           |-->| TYPE OF ELEMENT
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE
!| ISCE           |-->| GLOBAL NUMBER OF POINT SOURCES
!| ITRAV2         |-->| INTEGER WORK ARRAY
!| IT3            |<->| INTEGER WORK ARRAY
!| IT3            |<->| INTEGER WORK ARRAY
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LISPFR         |-->| LIST OF BOUNDARY POINTS TO BE DEALT WITH
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| LT             |-->| CURRENT TIME-STEP
!| LV             |-->| VECTOR LENGTH (FOR VECTOR MACHINES)
!| MARDAT         |-->| DATE (YEAR, MONTH,DAY)
!| MAREE          |-->| IF YES, TIDE GENERATING FORCE
!| MARTIM         |-->| TIME (HOUR, MINUTE,SECOND)
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NORD           |-->| ANGLE OF NORTH WITH VERTICAL AXIS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTH           |-->| RECORD NUMBER IN THE WAVE CURRENTS FILE
!| NTRAC          |-->| NUMBER OF TRACERS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| NVARCL         |-->| NUMBER OF CLANDESTINE VARIABLES
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
!| PHI0           |-->| LATITUDE OF ORIGIN POINT
!| SHP            |<--| BARYCENTRIC COORDINATES AT THE FOOT
!|                |   | OF CHARACTERISTICS
!| SHPP           |<--| BARYCENTRIC COORDINATES AT THE FOOT
!|                |   | OF CHARACTERISTICS, FOR BOUNDARY POINTS
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SPHERI         |-->| IF TRUE : SPHERICAL COORDINATES
!| SURDET         |-->| 1/(DETERMINANT OF ISOPARAMETRIC TRANSFORMATION)
!| T              |-->| BLOCK OF TRACERS AT TIME N
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| TBTIL          |<--| BLOCK OF WORK ARRAYS, TRACERS AFTER ADVECTION
!| TEMPS          |-->| TIME IN SECONDS
!| U              |<->| X-COMPONENT OF VELOCITY
!| UBOR           |<--| PRESCRIBED VELOCITY U.
!| UBTIL          |<--| WORK ARRAY, U AT BOUNDARIES AFTER ADVECTION
!| UCONV          |-->| WORK ARRAY: ADVECTION FIELDS
!| UFIELD         |<->| WORK ARRAY, U WITH RELAXATION
!| UNA            |<->| WORK ARRAY
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| V              |<->| Y-COMPONENT OF VELOCITY
!| VARCL          |-->| BLOCK OF CLANDESTINE VARIABLES
!| VARCLA         |-->| NAMES OF CLANDESTINE VARIABLES
!| UBOR           |<--| PRESCRIBED VELOCITY V.
!| VBTIL          |<--| WORK ARRAY, V AT BOUNDARIES AFTER ADVECTION
!| VCONV          |-->| WORK ARRAY: ADVECTION FIELDS
!| VENT           |-->| IF YES, WIND TAKEN INTO ACCOUNT
!| VFIELD         |<->| WORK ARRAY, V WITH RELAXATION
!| W1R            |<->| WORK ARRAY
!| W2R            |<->| WORK ARRAY
!| W3R            |<->| WORK ARRAY
!| WINDX          |-->| VELOCITY OF WIND ALONG X
!| WINDY          |-->| VELOCITY OF WIND ALONG Y
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XNEBOR         |-->| X-COMPONENT OF NORMAL AT NODES
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YNEBOR         |-->| Y-COMPONENT OF NORMAL AT NODES
!| ZBTIL          |<--| WORK ARRAY, BOTTOM AT BOUNDARIES AFTER ADVECTION
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZS             |<--| FREE SURFACE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_THOMPS => THOMPS
      USE STREAMLINE, ONLY : SCARACT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR,LT,NPOIN,NELEM,NELMAX,NFRLIQ,LV
      INTEGER, INTENT(IN) :: NVARCL,NPTH,IELM,NTRAC,HFROT
      INTEGER, INTENT(IN) :: KSORT,KINC,KENT,KENTU
      INTEGER, INTENT(IN) :: MARDAT(3),MARTIM(3),OPTSOU,ISCE(*)
      INTEGER, INTENT(IN) :: NBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(*),IFABOR(*)
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER, INTENT(IN) :: FRTYPE(NFRLIQ),NUMLIQ(NPTFR)
      INTEGER, INTENT(INOUT),TARGET  :: IT(2*NPTFR),ITBIS(2*NPTFR)
!     ITRAV2 : OF DIMENSION NPOIN
      INTEGER, INTENT(INOUT) :: ITRAV2(*)
      LOGICAL, INTENT(IN) :: VENT,MAREE,CORIOL,SPHERI,MSK,COUROU
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(*),DSCE(*)
      DOUBLE PRECISION, INTENT(IN)    :: TEMPS,GRAV,DT,FAIR,FCOR,NORD
      DOUBLE PRECISION, INTENT(IN)    :: HWIND,PHI0
      DOUBLE PRECISION, INTENT(INOUT) :: W1R(NPTFR),W2R(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: W3R(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPP(3,NPTFR),SHP(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DX_T(NPTFR),DY_T(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: DZ_T(NPTFR)
      TYPE(BIEF_OBJ), INTENT(IN)      :: WINDX,WINDY,MASKEL
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: HBTIL,UBTIL,VBTIL,ZBTIL,T7
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: VARCL,FXWAVE,FYWAVE,XCONV,YCONV
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FU,FV,T8,UNA,UCONV,VCONV,C,U,V
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,T,SMH,TBOR,TBTIL,T6,IT3,IT4
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: HFIELD,UFIELD,VFIELD,ZS
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: GZSX,GZSY
      TYPE(BIEF_OBJ), INTENT(IN)      :: ZF,CF,LITBOR,UNSV2D
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      CHARACTER(LEN=32), INTENT(IN)   :: VARCLA(NVARCL)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, DIMENSION(:), POINTER  :: LISPFR
      INTEGER, DIMENSION(:),POINTER :: ELT_T
      INTEGER K,NDEB,NPT,KP,J,ITRAC,N,NOMB               
      DOUBLE PRECISION HMIN,HHBOR,DETADX,DETADY,TBAR(100),TT(100)
      DOUBLE PRECISION UCSI,UCSIBAR,UETA,UETABAR,CBAR,HH,TETA
      DOUBLE PRECISION ZSTAR(1),ZCONV(1,1),SHZ(1),Z(1,1),UNORM,NORMZS
      INTEGER ETA(1)
      LOGICAL QUAD
      INTEGER NDP,NPLAN,IELMU,I
!
      DATA HMIN  /2.D-2/
!      
      LOGICAL INIT
      DATA    INIT/.TRUE./
      TYPE(BIEF_OBJ) :: FNCAR1,FTILD1
      SAVE
!
!-----------------------------------------------------------------------
! 
      LISPFR=>IT(1:NPTFR)
      ELT_T=>IT(NPTFR+1:2*NPTFR)
      IF(INIT) THEN
        CALL ALLBLO(FNCAR1,'FNCAR1')
        CALL ALLBLO(FTILD1,'FTILD1')
        CALL ADDBLO(FNCAR1,UFIELD)
        CALL ADDBLO(FNCAR1,VFIELD)
        CALL ADDBLO(FNCAR1,HFIELD)
        CALL ADDBLO(FNCAR1,ZF)
        CALL ADDBLO(FTILD1,UBTIL)
        CALL ADDBLO(FTILD1,VBTIL)
        CALL ADDBLO(FTILD1,HBTIL) 
        CALL ADDBLO(FTILD1,ZBTIL)                     
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            CALL ADDBLO(FNCAR1,T%ADR(ITRAC)%P)
            CALL ADDBLO(FTILD1,TBTIL%ADR(ITRAC)%P)
          ENDDO
        ENDIF
        INIT=.FALSE.     
      ENDIF
!
      ETA(1)=1  
      NDP=3
      NPLAN=1
      IELMU=IELM 
!
!-----------------------------------------------------------------------
!
!     CREATING FIELDS OF H, U AND V, THAT CONTAIN THE PRESCRIBED DATA
!     WITH RELAXATION TETA (IF 1.D0, THE ROUGH DATA ARE TAKEN ON THE 
!     THOMPSON BOUNDARIES FOR INTERPOLATION AT THE FOOT OF 
!     CHARACTERISTICS)
!
      CALL OS('X=Y     ',X=HFIELD,Y=H)
      CALL OS('X=Y     ',X=UFIELD,Y=U)
      CALL OS('X=Y     ',X=VFIELD,Y=V)
!    
      TETA=1.D0
!     CAN BE RELAXED, TESTED UP TO 0.05 IN PALUEL BOX MODEL
!     TETA=0.05D0  
      DO K=1,NPTFR
        IF(NUMLIQ(K).NE.0) THEN
          IF(FRTYPE(NUMLIQ(K)).EQ.2) THEN
            N=NBOR(K)
            IF(LIHBOR(K).EQ.KENT) THEN
              HFIELD%R(N)=TETA*HBOR(K)+(1.D0-TETA)*HFIELD%R(N)
            ENDIF
            IF(LIUBOR(K).EQ.KENT.OR.LIUBOR(K).EQ.KENTU) THEN
              UFIELD%R(N)=TETA*UBOR(K)+(1.D0-TETA)*UFIELD%R(N)
            ENDIF
            IF(LIVBOR(K).EQ.KENT.OR.LIVBOR(K).EQ.KENTU) THEN
              VFIELD%R(N)=TETA*VBOR(K)+(1.D0-TETA)*VFIELD%R(N)
            ENDIF            
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      QUAD=.FALSE.     
!
!     COMPUTES THE CELERITY
!     
      CALL OS('X=CY    ',C,H,H,GRAV)
      CALL CLIP(C,0.D0,.TRUE.,1.D6,.FALSE.,0)
      CALL OS('X=SQR(Y)',X=C,Y=C)
!
!     COMPUTES MINUS THE FREE SURFACE GRADIENT 
!
      CALL OS('X=Y+Z   ',X=ZS,Y=H,Z=ZF)
      CALL VECTOR(GZSX,'=','GRADF          X',IELM,
     &            -1.D0,ZS,ZS,ZS,ZS,ZS,ZS,MESH,MSK,MASKEL)
      CALL VECTOR(GZSY,'=','GRADF          Y',IELM,
     &            -1.D0,ZS,ZS,ZS,ZS,ZS,ZS,MESH,MSK,MASKEL)
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(GZSX,2,MESH)
        CALL PARCOM(GZSY,2,MESH)
      ENDIF
!
!     REGROUPS THE POINTS WITH THOMPSON TREATMENT
!     NPT : NUMBER OF POINTS
!     LISPFR : LIST OF THOSE POINTS (BOUNDARY NODE NUMBERS)
!
      NPT=0      
      DO K=1,NPTFR
        IF(NUMLIQ(K).NE.0) THEN
          IF(FRTYPE(NUMLIQ(K)).EQ.2) THEN
            NPT=NPT+1
            LISPFR(NPT)=K             
          ENDIF
        ENDIF
      ENDDO
!
!--------------------------------------------------------------------     
!     CHARACTERISTICS WITH ADVECTION FIELD U
!     MAY BE CALLED WITH NPT=0 IF OTHER PROCESSORS STILL AT WORK
!--------------------------------------------------------------------
!
!     COMPUTES THE ADVECTION FIELD U
!     
      DO N=1,NPOIN
        UCONV%R(N)=U%R(N)
        VCONV%R(N)=V%R(N)
      ENDDO
!
!     NOTE JMH: COULD BE SIMPLIFIED WITH ARRAYS NELBOR AND NULONE !!!
      CALL GTSH11(SHP,ITRAV2,IKLE,NPOIN,NELEM,NELMAX,MSK,MASKEL%R)
!
      CALL PRE_SCARACT_THOMPSON(NPTFR,NPT,NDP,NELEM,NBOR,LISPFR,X,Y,
     *                          ITRAV2,SHP,XCONV%R,YCONV%R,SHPP,ELT_T)
!
!     PROVISIONAL, SHOULD WORK WITHOUT THIS IN VERSION 6.2
      IF(NCSIZE.GT.1) THEN
        CALL OS('X=0     ',X=UBTIL)
        CALL OS('X=0     ',X=VBTIL)
        CALL OS('X=0     ',X=HBTIL)
        CALL OS('X=0     ',X=ZBTIL)
      ENDIF
!
      NOMB=4+NTRAC
      CALL SCARACT(FNCAR1,FTILD1,UCONV%R,VCONV%R,VCONV%R,X,Y,
     *             ZSTAR,XCONV%R,YCONV%R,ZCONV,DX_T,DY_T,DZ_T,Z,
     *             SHPP,SHZ,SURDET,DT,IKLE,IFABOR,ELT_T,
     *             ETA,IT3%I,IT4%I,IELM,IELMU,NELEM,NELMAX,
     *             NOMB,NPOIN,NPOIN,NDP,NPLAN,LV,MSK,MASKEL%R,
     *             MESH,MESH%FAC%R,T7%R,T7,.FALSE.,QUAD,NPT,
     *             .FALSE.,.FALSE.)
!
!----------------------------------------------------------------------
!     UBTIL, VBTIL, HBTIL, TBTIL AT BOUNDARY NODES NUMBERING
!----------------------------------------------------------------------
!
!     BACKWARD LOOP TO AVOID ERASING DATA, K ALWAYS GREATER THAN J
      DO J=NPT,1,-1
        K=LISPFR(J)
        UBTIL%R(K)=UBTIL%R(J)
        VBTIL%R(K)=VBTIL%R(J)
        HBTIL%R(K)=HBTIL%R(J)
!       ZBTIL%R(K)=ZBTIL%R(J)
      ENDDO
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          DO J=NPT,1,-1
            K=LISPFR(J)
            TBTIL%ADR(ITRAC)%P%R(K)=TBTIL%ADR(ITRAC)%P%R(J)
          ENDDO
        ENDDO
      ENDIF
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM_BORD(UBTIL%R,1,MESH)
        CALL PARCOM_BORD(VBTIL%R,1,MESH)
        CALL PARCOM_BORD(HBTIL%R,1,MESH)
!       CALL PARCOM_BORD(ZBTIL%R,1,MESH)
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            CALL PARCOM_BORD(TBTIL%ADR(ITRAC)%P%R,1,MESH)
          ENDDO
        ENDIF
      ENDIF
!
!     COMPUTES THE RIEMANN INVARIANTS W1 AND W4 (SECOND DIMENSION OF TBOR)
!     CARRIED BY THIS FIELD
!
!     IF W1=0 IS OK, THIS PART COULD BE DONE ONLY WHEN NTRAC.GT.0
!
      IF(NPT.GT.0) THEN 
        DO J=1,NPT
          K=LISPFR(J)
          N=NBOR(K)  
          UNORM=SQRT(U%R(N)**2+V%R(N)**2)
          IF(UNORM.GT.1.D-12) THEN
            DETADX=U%R(N)/UNORM
            DETADY=V%R(N)/UNORM
          ELSE
            DETADX=0.D0
            DETADY=0.D0
          ENDIF
          UCSIBAR=-U%R(N)*DETADY+V%R(N)*DETADX
          HH=HBTIL%R(K)
          UCSI=-UBTIL%R(K)*DETADY+VBTIL%R(K)*DETADX
          W1R(K)=HH*(UCSIBAR-UCSI) 
          IF(NTRAC.GT.0) THEN
            DO ITRAC=1,NTRAC
              TBAR(ITRAC)=T%ADR(ITRAC)%P%R(N)
            ENDDO
            IF(UCONV%R(N)*XNEBOR(K)+VCONV%R(N)*YNEBOR(K).GT.0.D0) THEN
!             VELOCITY EXITING THE DOMAIN, THE CHARACTERISTICS ARE USED
              DO ITRAC=1,NTRAC
                TT(ITRAC)=TBTIL%ADR(ITRAC)%P%R(K)
              ENDDO
            ELSE
!             VELOCITY ENTERING THE DOMAIN, PRESCRIBED VALUES TAKEN
              DO ITRAC=1,NTRAC
                TT(ITRAC)=TBOR%ADR(ITRAC)%P%R(K)
              ENDDO
            ENDIF
            DO ITRAC=1,NTRAC
!             W4
              TBOR%ADR(ITRAC)%P%R(K+NPTFR)=HH*(TT(ITRAC)-TBAR(ITRAC))
            ENDDO
          ENDIF       
        ENDDO
      ENDIF
!
!----------------------------------------------------------
!     COMPUTES THE ADVECTION FIELD U + C 
!----------------------------------------------------------
!
      DO N=1,NPOIN
        UNORM=SQRT(U%R(N)**2+V%R(N)**2)
        IF(UNORM.GT.1.D-12) THEN
          DETADX=U%R(N)/UNORM
          DETADY=V%R(N)/UNORM
        ELSE
!         VERY IMPORTANT
          NORMZS=SQRT(GZSX%R(N)**2+GZSY%R(N)**2)
          IF(NORMZS.GT.1.D-12) THEN
            DETADX=GZSX%R(N)/NORMZS
            DETADY=GZSY%R(N)/NORMZS
          ELSE
            DETADX=0.D0
            DETADY=0.D0
          ENDIF
        ENDIF
        UETABAR=U%R(N)*DETADX+V%R(N)*DETADY+C%R(N)                   
        UCONV%R(N)=UETABAR*DETADX
        VCONV%R(N)=UETABAR*DETADY
      ENDDO
!
!----------------------------------------------------------------------
!     CHARACTERISTICS FOR THE GROUP OF POINTS, FIELD U + C
!     MAY BE CALLED WITH NPT=0 IF OTHER PROCESSORS STILL AT WORK
!----------------------------------------------------------------------
!
      CALL PRE_SCARACT_THOMPSON(NPTFR,NPT,NDP,NELEM,
     *                          NBOR,LISPFR,X,Y,ITRAV2,
     *                          SHP,XCONV%R,YCONV%R,SHPP,ELT_T)
!
!     PROVISIONAL, SHOULD WORK WITHOUT THIS IN VERSION 6.2
      IF(NCSIZE.GT.1) THEN
        CALL OS('X=0     ',X=UBTIL)
        CALL OS('X=0     ',X=VBTIL)
        CALL OS('X=0     ',X=HBTIL)
        CALL OS('X=0     ',X=ZBTIL)
      ENDIF
!
      NOMB=4
      CALL SCARACT(FNCAR1,FTILD1,UCONV%R,VCONV%R,VCONV%R,X,Y,
     *             ZSTAR,XCONV%R,YCONV%R,ZCONV,DX_T,DY_T,DZ_T,Z,
     *             SHPP,SHZ,SURDET,DT,IKLE,IFABOR,ELT_T,ETA,
     *             IT3%I,IT4%I,IELM,IELMU,NELEM,NELMAX,NOMB,NPOIN,
     *             NPOIN,NDP,NPLAN,LV,MSK,MASKEL%R,MESH,MESH%FAC%R,
     *             T7%R,T7,.FALSE.,QUAD,NPT,.FALSE.,.FALSE.)
!
!----------------------------------------------------------------------
!     UBTIL, VBTIL, HBTIL AT BOUNDARY NODES NUMBERING
!----------------------------------------------------------------------
!
!     BACKWARD LOOP TO AVOID ERASING DATA, K ALWAYS GREATER THAN J
      DO J=NPT,1,-1
        K=LISPFR(J)
        UBTIL%R(K)=UBTIL%R(J)
        VBTIL%R(K)=VBTIL%R(J)
        HBTIL%R(K)=HBTIL%R(J)
        ZBTIL%R(K)=ZBTIL%R(J)
      ENDDO
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM_BORD(UBTIL%R,1,MESH)
        CALL PARCOM_BORD(VBTIL%R,1,MESH)
        CALL PARCOM_BORD(HBTIL%R,1,MESH)
        CALL PARCOM_BORD(ZBTIL%R,1,MESH)
      ENDIF
!
!----------------------------------------------------------------------
!     COMPUTES THE RIEMANN INVARIANTS W2 CARRIED BY THIS ADVECTION FIELD
!----------------------------------------------------------------------
!
      IF(NPT.GT.0) THEN
        DO J=1,NPT
          K=LISPFR(J)
          N=NBOR(K)
          UNORM=SQRT(U%R(N)**2+V%R(N)**2)
          IF(UNORM.GT.1.D-12) THEN
            DETADX=U%R(N)/UNORM
            DETADY=V%R(N)/UNORM
          ELSE
            DETADX=0.D0
            DETADY=0.D0
          ENDIF
!         UETABAR=U%R(N)*DETADX+V%R(N)*DETADY
          UETABAR=UNORM
          CBAR=C%R(N)
          HH  =HBTIL%R(K)
          UETA=UBTIL%R(K)*DETADX+VBTIL%R(K)*DETADY
          W2R(K)=(HH+ZBTIL%R(K)-ZF%R(N))*CBAR+HH*(UETA-UETABAR)                               
        ENDDO
      ENDIF           
!
!     COMPUTES THE ADVECTION FIELD U-C 
!    
      DO N=1,NPOIN
        UNORM=SQRT(U%R(N)**2+V%R(N)**2)
        IF(UNORM.GT.1.D-12) THEN
          DETADX=U%R(N)/UNORM
          DETADY=V%R(N)/UNORM
        ELSE
          NORMZS=SQRT(GZSX%R(N)**2+GZSY%R(N)**2)
          IF(NORMZS.GT.1.D-12) THEN
            DETADX=GZSX%R(N)/NORMZS
            DETADY=GZSY%R(N)/NORMZS
          ELSE
            DETADX=0.D0
            DETADY=0.D0
          ENDIF
        ENDIF
        UETABAR=U%R(N)*DETADX+V%R(N)*DETADY-C%R(N)                   
        UCONV%R(N)=UETABAR*DETADX
        VCONV%R(N)=UETABAR*DETADY
      ENDDO
!
!     CHARACTERISTICS FOR THE GROUP OF POINTS, FIELD U + C
!     
      CALL PRE_SCARACT_THOMPSON(NPTFR,NPT,NDP,NELEM,NBOR,LISPFR,X,Y,
     *                          ITRAV2,SHP,XCONV%R,YCONV%R,SHPP,ELT_T)
!
!     PROVISIONAL, SHOULD WORK WITHOUT THIS IN VERSION 6.2
      IF(NCSIZE.GT.1) THEN
        CALL OS('X=0     ',X=UBTIL)
        CALL OS('X=0     ',X=VBTIL)
        CALL OS('X=0     ',X=HBTIL)
        CALL OS('X=0     ',X=ZBTIL)
      ENDIF
!
      NOMB=4
      CALL SCARACT(FNCAR1,FTILD1,UCONV%R,VCONV%R,VCONV%R,X,Y,ZSTAR,
     *             XCONV%R,YCONV%R,ZCONV,DX_T,DY_T,DZ_T,Z,SHPP,SHZ,
     *             SURDET,DT,IKLE,IFABOR,ELT_T,ETA,IT3%I,IT4%I,IELM,
     *             IELMU,NELEM,NELMAX,NOMB,NPOIN,NPOIN,NDP,NPLAN, 
     *             LV,MSK,MASKEL%R,MESH,MESH%FAC%R,T7%R,T7,
     *             .FALSE.,QUAD,NPT,.FALSE.,.FALSE.)
!
!----------------------------------------------------------------------
!     UBTIL, VBTIL, HBTIL AT BOUNDARY NODES NUMBERING
!----------------------------------------------------------------------
!
!     BACKWARD LOOP TO AVOID ERASING DATA, K ALWAYS GREATER THAN J
      DO J=NPT,1,-1
        K=LISPFR(J)
        UBTIL%R(K)=UBTIL%R(J)
        VBTIL%R(K)=VBTIL%R(J)
        HBTIL%R(K)=HBTIL%R(J)
        ZBTIL%R(K)=ZBTIL%R(J)
      ENDDO
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM_BORD(UBTIL%R,1,MESH)
        CALL PARCOM_BORD(VBTIL%R,1,MESH)
        CALL PARCOM_BORD(HBTIL%R,1,MESH)
        CALL PARCOM_BORD(ZBTIL%R,1,MESH)
      ENDIF
!
! COMPUTES THE RIEMANN INVARIANTS W3 CARRIED BY THIS ADVECTION FIELD
!
      IF(NPT.GT.0) THEN
        DO J=1,NPT
          K=LISPFR(J)
          N=NBOR(K)
          UNORM=SQRT(U%R(N)**2+V%R(N)**2)
          IF(UNORM.GT.1.D-12) THEN
            DETADX=U%R(N)/UNORM
            DETADY=V%R(N)/UNORM
          ELSE
            DETADX=0.D0
            DETADY=0.D0
          ENDIF
!         UETABAR=U%R(N)*DETADX+V%R(N)*DETADY
!         MARCHE AUSSI
          UETABAR=UNORM
          CBAR=C%R(N)
          HH  =HBTIL%R(K)
          UETA=UBTIL%R(K)*DETADX+VBTIL%R(K)*DETADY
          W3R(K)=(HH+ZBTIL%R(K)-ZF%R(N))*CBAR+HH*(-UETA+UETABAR)        
        ENDDO
      ENDIF
!
!----------------------------------------------------------------------
!       RE-BUILDS THE TELEMAC-2D VARIABLES
!----------------------------------------------------------------------       
!
      IF(NPT.GT.0) THEN     
        DO J=1,NPT    
          K=LISPFR(J)
          N=NBOR(K) 
          UNORM=SQRT(U%R(N)**2+V%R(N)**2)
          IF(UNORM.GT.1.D-12) THEN
            DETADX=U%R(N)/UNORM
            DETADY=V%R(N)/UNORM
          ELSE
            NORMZS=SQRT(GZSX%R(N)**2+GZSY%R(N)**2)
            IF(NORMZS.GT.1.D-12) THEN
              DETADX=GZSX%R(N)/NORMZS
              DETADY=GZSY%R(N)/NORMZS
            ELSE
              DETADX=0.D0
              DETADY=0.D0
            ENDIF
          ENDIF     
          IF(C%R(N)**2.GT.GRAV*HMIN) THEN
            HBOR(K)=(W2R(K)+W3R(K))/(2*C%R(N))                                      
            IF(HBOR(K).GT.HMIN) THEN
!             BEWARE TIDAL FLATS, AND HIDDEN PARAMETER 0.1
              HHBOR=MAX(0.1D0,HBOR(K))
              UBOR(K)=(       DETADY* W1R(K)
     &                 +0.5D0*DETADX*(W2R(K)-W3R(K)))/HHBOR+U%R(N)              
              VBOR(K)=(      -DETADX* W1R(K)
     &                 +0.5D0*DETADY*(W2R(K)-W3R(K)))/HHBOR+V%R(N)            
              IF(NTRAC.GT.0) THEN
                DO ITRAC=1,NTRAC
!                 REMEMBER THAT W4 IS STORED INTO SECOND DIMENSION OF TBOR
                  TBOR%ADR(ITRAC)%P%R(K)=
     *            TBOR%ADR(ITRAC)%P%R(K+NPTFR)/HHBOR+T%ADR(ITRAC)%P%R(N)
                ENDDO
              ENDIF
            ELSE
!             BECOMES DRY
              HBOR(K)=MAX(0.D0,HBOR(K))
              UBOR(K)=0.D0
              VBOR(K)=0.D0
            ENDIF
          ELSE
!           WAS DRY, H IS GIVEN BY BORD
            UBOR(K)=0.D0
            VBOR(K)=0.D0
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
