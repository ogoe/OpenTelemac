!                    *****************
                     SUBROUTINE PRECON
!                    *****************
!
     &(WP,WPS,ZPROPS,ISOUSI,LT)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES THE ADVECTION STEP BY COMPUTING THE
!+                PARAMETERS COMMON TO ALL THE VARIABLES TO ADVECT.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  JMH
!+        07/08/2008
!+
!+   CALLS CHARAC INSTEAD OF CARACT
!
!history  JMH
!+        13/08/2008
!+
!+   IMMEDIATE INTERPOLATION IN CHARAC
!
!history  JMH
!+        29/06/2009
!+
!+   POINT TO POINT FLUXES COMPUTED IN FLODEL
!
!history  JMH
!+        18/08/2009
!+
!+   UCONVC AND VCONVC FOR ADVECTION FIELD
!
!history  JMH
!+        16/02/2010
!+
!+   COMPUTES ZCHAR TO CALL CHARAC
!
!history  JM HERVOUET (LNHE)     ; JM JANIN (LNH)
!+        26/04/2010
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
!| ISOUSI         |-->| RANK OF CURRENT SUB-ITERATION
!| LT             |-->| CURRENT TIME STEP NUMBER
!| WP             |<->| W VELOCITY
!| WPS            |<->| W VELOCITY IN TRANSFORMED MESH
!| ZPROPS         |<->| TRANSFORMED ZPROP, TEMPORARILY PUT IN MESH3D%Z
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_PRECON => PRECON
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WP,WPS,ZPROPS
!
      INTEGER, INTENT(IN) :: ISOUSI,LT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ::I,IS,IP,OPTHNEG,IWS,NSEG3D,IPLAN,OPT_TRID
      CHARACTER(LEN=16) FORMUL
      CHARACTER(LEN=8) OPER
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
!=======================================================================
!
!     MESH MODIFIED TO BE EQUIVALENT TO THE DEPTH USED IN THE 2D
!     CONTINUITY EQUATION, TO CALL : FLUX3D, VECTOR, MATRIX
!
!     ZPROPS IS TEMPORARILY PUT IN MESH3D%Z
      SAVEZ=>MESH3D%Z%R
      MESH3D%Z%R=>ZPROPS%R
      NSEG3D=MESH3D%NSEG
!
!=======================================================================
!
! COMPUTES INTERNAL AND EXTERNAL FLUXES AND ADVECTION FIELDS
!
!=======================================================================
!
      OPTHNEG=OPT_HNEG
      IF(LT.EQ.0) OPTHNEG=0
!
      CALL FLUX3D
     & (FLUINT,FLUEXT,FLUEXTPAR,UCONV,VCONV,T3_01,T3_02,T3_03,MESH3D%W,
     &  NETAGE,NPLAN,NELEM3,IELM3,IELM2H,IELM2V,SVIDE,MESH3D,
     &  MASK%ADR(8)%P,MSK,MASKEL,MASKBR,
     &  LIMPRO%I,KDIR,NPTFR2,DT,VOLU,VOLUN,MESH2D,
     &  GRAPRD,SIGMAG,T2_01,NPOIN2,NPOIN3,DM1,ZCONV,FLBOR,
     &  PLUIE,RAIN,FLODEL,FLOPAR,OPTHNEG,FLULIM,
     &  (N_ADV(ADV_LPO).GT.0.OR.N_ADV(ADV_LPO_TF).GT.0),
     &  LT,BYPASS,N_ADV,MTRA1)
!
!=======================================================================
!   COMPUTES (DZW*)JH,IV+1/2 AND ACCUMULATES IN WSCONV
!=======================================================================
!
!     HARDCODED OPTION !!!!!!!!!!!!
!
!     2: DIVERGENCE-FREE FLUXES OBTAINED BY MODIFYING VERTICAL FLUXES
!
!     3: DIVERGENCE-FREE FLUXES OBTAINED BY MODIFYING ALL FLUXES
!        WITH THE HELP OF WCONV AND A PRESSURE EQUATION
!
      OPT_TRID=2
!
      IF(OPT_TRID.EQ.2) THEN
        CALL TRIDW2(WSCONV)
      ELSEIF(OPT_TRID.EQ.3) THEN
!       OTHERWISE WCONV DONE IN WAVE_EQUATION
        IF(LT.EQ.0) CALL OS('X=Y     ',X=WCONV,Y=W)
        CALL TRIDW3(WSCONV,T3_01,T3_02,T3_03,T3_04,T3_05,MTRA1%D,LT)
      ENDIF
!
!=======================================================================
!     FOR DEBUGGING: SUMMARY OF ADVECTED VARIABLES AND THEIR SCHEME
!=======================================================================
!
!     DO I=1,15
!       IF(N_ADV(I).GT.0) THEN
!         DO IS=1,N_ADV(I)
!           WRITE(LU,*) 'ADVECTION OF ',
!    &                  BL_FN%ADR(LIST_ADV(IS,I))%P%NAME,
!    &                  ' BY SCHEME ',I
!         ENDDO
!       ENDIF
!     ENDDO
!
!=======================================================================
!     PREPARES ADVECTION BY MURD METHOD
!     STORAGE IS ALWAYS EBE
!=======================================================================
!
      IF(N_ADV(ADV_NSC).GT.0.OR.N_ADV(ADV_PSI).GT.0) THEN
!
!       NOTE: THE MATRIX IS THE SAME IN BOTH CASES BUT
!             WITH PSI SCHEME THE DIAGONAL IS NOT ASSEMBLED BECAUSE
!             IT IS ASSEMBLED IN MURD3D
        IF(N_ADV(ADV_NSC).GT.0.AND..NOT.(OPT_HNEG.EQ.2.OR.SIGMAG)) THEN
          FORMUL = 'MAMURD 2     N  '
        ELSE
          FORMUL = 'MAMURD 2     PSI'
        ENDIF
        CALL MATRIX
!                                                           !!!!!!!
     &  (MMURD,'M=N     ',FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,MTRA1%X,
     &   UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!       HERE THE BYPASS IS NOT OPTIONAL, OTHERWISE
!       THE SCHEMES ARE NOT MASS-CONSERVATIVE
!       IF(BYPASS) THEN
        IF(OPT_HNEG.EQ.2.OR.SIGMAG) THEN
          CALL BYPASS_CRUSHED_POINTS_EBE(VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                                   MMURD%X%R,T3_01,MESH2D,MESH3D,
     &                                   NPOIN3,NELEM2,NELEM3,NPLAN,
     &                                   MESH3D%IKLE%I)
          IF(N_ADV(ADV_NSC).GT.0) THEN
            CALL DIAG_MURD(MMURD%D%R,MMURD%X%R,NELEM3,MESH3D%NELMAX,
     &                     NPOIN3,MESH3D%IKLE%I)
          ENDIF
        ENDIF
!       ENDIF
!
      ENDIF
!
!=======================================================================
!     PREPARES ADVECTION BY MURD METHOD IN EDGE-BASED FORM
!     STORAGE IS ALWAYS EDGE-BASED
!=======================================================================
!
      IF(N_ADV(ADV_NSC_TF).GT.0) THEN
!
!       NOTE: THE MATRIX IS THE SAME IN BOTH CASES BUT
!             WITH PSI SCHEME THE DIAGONAL IS NOT ASSEMBLED
!             IT IS WHAT WE WANT HERE
        FORMUL = 'MAMURD 2     PSI'
        CALL MATRIX
!                                                             !!!!!!!
     &  (MURD_TF,'M=N     ',FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,MTRA1%X,
     &   UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!
!       FROM 30 SEGMENTS WITH POSITIVE FLUXES, WE GO TO 15 WITH
!       POSITIVE OR NEGATIVE FLUXES
        DO I=1,NSEG3D
          MURD_TF%X%R(I) = MURD_TF%X%R(I) - MURD_TF%X%R(I+NSEG3D)
        ENDDO
!       CALL BYPASS: OPTIONAL BUT SAVES ITERATIONS
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.BYPASS) THEN
          CALL BYPASS_CRUSHED_POINTS_SEG(VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                                   MURD_TF%X%R,
     &                                   T3_01,MESH2D,MESH3D,
     &                                   NPOIN3,ADV_NSC_TF,NPOIN2,
     &                                   MESH3D%GLOSEG%I,
     &                                   MESH3D%GLOSEG%DIM1,
     &                                   MESH2D%NSEG,NPLAN)
        ENDIF
        IF(NCSIZE.GT.1) THEN
!         ASSEMBLED FORM OF FLUXES STORED IN SECOND PART
!         OF MATRIX WHICH OTHERWISE IS NOT USED
          CALL OV('X=Y     ',MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                       MURD_TF%X%R(       1:  NSEG3D),
     &                       MURD_TF%X%R(       1:  NSEG3D),
     &                       0.D0,NSEG3D)
          CALL PARCOM2_SEG(MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MESH2D%NSEG,NPLAN,2,1,MESH2D,2)
        ENDIF
!
      ENDIF
!
!=======================================================================
!     PREPARES LEO POSTMA ADVECTION SCHEMES
!=======================================================================
!
!     RETRIEVES VERTICAL FLUXES FROM WSCONV
!     VERTICAL FLUXES ARE STORED IN FLODEL AFTER
!     THE HORIZONTAL FLUXES (THERE ARE NSEG*NPLAN HORIZONTAL FLUXES)
!     USEFUL SIZE OF WSCONV IS (NPOIN2,NPLAN-1)
!
      IF(N_ADV(ADV_LPO).GT.0.OR.N_ADV(ADV_LPO_TF).GT.0) THEN
        IS=MESH2D%NSEG*NPLAN
        DO IP=1,NPLAN-1
          DO I=1,NPOIN2
            IWS=I+(IP-1)*NPOIN2
!           NOTE 1: WSCONV IS ALREADY ASSEMBLED
!                   USING VOLU2D FLODEL WILL BE THE NON ASSEMBLED FORM
!           NOTE 2: WE COULD KEEP THE ORIGINAL RIGHT HAND SIDE IN
!                   TRIDW2
!           NOTE 3: AGAIN CONVENTION REVERSED, HERE FLOW FROM
!                   POINT 2 (UP) TO POINT 1 (DOWN)
            FLODEL%R(IS+IWS)=-WSCONV%R(IWS)*VOLU2D%R(I)
          ENDDO
        ENDDO
!       CALL BYPASS: OPTIONAL BUT SAVES ITERATIONS
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.BYPASS) THEN
          CALL BYPASS_CRUSHED_POINTS_SEG(VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                                   FLODEL%R,
     &                                   T3_01,MESH2D,MESH3D,
     &                                   NPOIN3,ADV_LPO_TF,NPOIN2,
     &                                   MESH3D%GLOSEG%I,
     &                                   MESH3D%GLOSEG%DIM1,
     &                                   MESH2D%NSEG,NPLAN)
        ENDIF
      ENDIF
!
!     FLOPAR = FLODEL ASSEMBLED IN PARALLEL MODE
!
      IF(OPTHNEG.EQ.2.OR.N_ADV(ADV_LPO)   .GT.0
     &               .OR.N_ADV(ADV_LPO_TF).GT.0) THEN
        IF(NCSIZE.GT.1) THEN
          CALL OS('X=Y     ',X=FLOPAR,Y=FLODEL)
          CALL PARCOM2_SEG(FLOPAR%R,FLOPAR%R,FLOPAR%R,
     &                     MESH2D%NSEG,NPLAN,2,1,MESH2D,1)
        ELSE
          FLOPAR%R=>FLODEL%R
        ENDIF
      ENDIF
!
!=======================================================================
!     PREPARES ADVECTION BY SUPG METHOD
!=======================================================================
!
      IF(N_ADV(ADV_SUP).GT.0) THEN
!
         IF(OPTSUP(1).EQ.2) THEN
!          HORIZONTAL UPWIND (HERE UPWIND COEFFICIENT=CFL)
           FORMUL = 'MAUGUG2         '
           CALL MATRIX
     &     (MSUPG,'M=N     ',FORMUL,IELM3,IELM3,0.5D0*DT,SVIDE,SVIDE,
     &      SVIDE,UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!          MSUPG IS SYMMETRICAL
         ELSEIF(OPTSUP(1).EQ.1) THEN
!          HORIZONTAL UPWIND (HERE UPWIND COEFFICIENT=1)
           FORMUL = 'MAUGUG1         '
           CALL MATRIX
     &     (MSUPG,'M=N     ',FORMUL,IELM3,IELM3,1.D0,SVIDE,SVIDE,
     &      SVIDE,UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!          MSUPG IS NOT SYMMETRICAL
         ELSEIF(OPTSUP(1).NE.0) THEN
           CALL PLANTE(1)
           STOP 'UNEXPECTED VALUE OF OPTSUP IN PRECON'
         ENDIF
!
!        MSUPG TRANSFORMED INTO NON SYMMETRICAL MATRIX
         IF(OPTSUP(1).EQ.2) THEN
           CALL OM('M=X(M)  ',MSUPG,MSUPG,SVIDE,0.D0,MESH3D)
           OPER='M=M+N   '
         ELSEIF(OPTSUP(1).EQ.1) THEN
           OPER='M=M+N   '
         ELSE
           OPER='M=N     '
         ENDIF
!
!        ADDS CENTRED ADVECTION TERM
!
         FORMUL = 'MATVGR          '
         FORMUL(8:8) = '2'
         CALL MATRIX
     &   (MSUPG,OPER,FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,SVIDE,
     &    UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!
!        VERTICAL UPWIND (SUBROUTINE UPWIND EXPECTS SYMMETRICAL MATRICES)
!        HERE UPWIND COEFFICIENT = 1, BUT WSCONV USED INSTEAD OF W
!
         CALL UPWIND(MSUPG,WSCONV,1.D0,MESH2D,MESH3D,NPLAN)
!
      ENDIF
!
!=======================================================================
!
!     RESTORES MESH3D%Z
!
      MESH3D%Z%R=>SAVEZ
!
!=======================================================================
!
!     COMPUTES DELTAZ*WSTAR (IN WPS) AT NODES
!
      CALL WSTAR(WPS,WSCONV,Z,NPOIN2,NPLAN)
!
!=======================================================================
!
!     COMPUTES W FROM  (DZW*)JH,IV+1/2
!
!        (WITH HYDROSTATIC ASSUMPTION, W IS NEVER USED,
!                  IT IS DONE HERE FOR OUTPUTS)
!        HOWEVER IT IS ALWAYS USED WITH THE K-EPSILON OR K-OMEGA MODELS
!
      IF(.NOT.NONHYD) THEN
        IF(((LT/GRAPRD)*GRAPRD.EQ.LT.AND.LT.GE.GRADEB).OR.
     &      (ITURBV.EQ.3.OR.ITURBV.EQ.7)) THEN
          CALL WSTARW(WP,WSCONV,T3_03%R,T3_04%R,T3_05%R)
        ENDIF
      ENDIF
!
!=======================================================================
!
! ADVECTION BY METHOD OF CHARACTERISTICS
!
!=======================================================================
!
      IF(N_ADV(ADV_CAR).GT.0) THEN
!
!       NOTES:
!
!       IN BLOCK FN3D THERE IS U,V,W INSTEAD OF UN,VN,WN
!       BECAUSE ADVECTION IS DONE FOR THE NEXT TIME STEP
!
!       FN3D%ADR(ADV_CAR)%P IS THE BLOCK OF VARIABLES ADVECTED WITH
!       SCHEME ADV_CAR (SEE POINT_TELEMAC3D)
!
        CALL CHARAC(FN3D,FC3D,FC3D%N,UCONVC,VCONVC,WPS,ZCHAR,
     &              DT,MESH3D%IFABOR,IELM3,NPOIN2,NPLAN,NPLINT,
     &              MSK,MASKEL,MTRA2%X,MTRA2%D,TRAV3,
     &              IT1%I,IT2%I,IT3%I,IT4%I,
     &              MESH3D,NELEM2,MESH2D%NELMAX,IKLE2,MESH2D%SURDET)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
