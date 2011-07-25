!                    *****************
                     SUBROUTINE DIFF3D
!                    *****************
!
     &(FD,FC,FN,VISCF,SIGMAF,S0F,YAS0F,S1F,YAS1F,
     & FBORL,FBORF,FBORS,AFBORL,AFBORF,AFBORS,
     & BFBORL,BFBORF,BFBORS,LIFBOF,LIFBOL,LIFBOS,
     & FMIN,CLIMIN,FMAX,CLIMAX,SCHCF,SCHDF,SLVDIF,TRBAF,INFO,
     & NEWDIF,DT,T2_01,T2_02,T2_03,T3_01,T3_02,T3_03,T3_04,
     & NPOIN2,NPOIN3,INCHYD,SEM3D,YASEM3D,IT1,NPTFR3,NBOR3,MASKPT,
     & TRAV3,MESH2D,MESH3D,MTRA1,MTRA2,IELM3,MSUPG,IELM2H,IELM2V,
     & MDIFF,MATR2H,MASKBR,SVIDE,MSK,MASKEL,H,
     & NPLAN,OPTBAN,OPTDIF,TETADI,YAWCC,WCC,AGGLOD,VOLU,
     & YASCE,NSCE,FSCE,SOURCES,TETASUPG,VELOCITY,RAIN,PLUIE,SIGMAG,
     & IPBOT)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE DIFFUSION AND SUPG ADVECTION STEPS
!+               (IF REQUIRED).
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  JMH
!+        14/12/2009
!+
!+   DIRICHLET POINTS ON THE BOTTOM
!
!history  J.M. HERVOUET (LNHE)
!+        18/03/2010
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
!| AFBORF         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE BOTTOM:
!|                |   |  NU*DF/DN = AFBORF*U + BFBORF
!| AFBORL         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE
!|                |   | LATERAL BOUNDARIES:
!|                |   | NU*DF/DN = AFBORL*U + BFBORL
!| AFBORS         |-->| LOGARITHMIC LAW FOR COMPONENT AT THE SURFACE:
!|                |   | NU*DF/DN = AFBORS*U + BFBORS
!| AGGLOD         |-->| MASS-LUMPING IN DIFFUSION
!| BFBORF         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE BOTTOM:
!|                |   |  NU*DF/DN = AFBORF*U + BFBORF
!| BFBORL         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE
!|                |   | LATERAL BOUNDARIES:
!|                |   | NU*DF/DN = AFBORL*U + BFBORL
!| BFBORS         |-->| LOGARITHMIC LAW FOR COMPONENT AT THE SURFACE:
!|                |   | NU*DF/DN = AFBORS*U + BFBORS
!| CLIMAX         |-->| LOGICAL FOR CLIPPING (MAX VALUE)
!| DT             |-->| TIME STEP
!| FBORF          |<->| DIRICHLET CONDITIONS ON F AT THE BOTTOM
!| FBORL          |<->| DIRICHLET CONDITIONS ON F ON LATERAL BOUNDARIES
!| FBORS          |<->| DIRICHLET CONDITIONS ON F AT THE SURFACE
!| FC             |<->| VARIABLE AFTER CONVECTION
!| FD             |<->| VARIABLE AFTER DIFFUSION
!| FMAX           |-->| MAX CLIPPING VALUE
!| FMIN           |-->| MIN CLIPPING VALUE
!| FN             |<->| VARIABLE F AT TIME N
!| FSCE           |-->| SOURCE TERM OF F
!| H              |-->| WATER DEPTH
!| IELM2H         |-->| DISCRETISATION TYPE FOR 2D HORIZONTAL MESH
!| IELM2V         |-->| DISCRETISATION TYPE FOR 2D VERTICAL MESH
!| IELM3          |-->| DISCRETISATION TYPE FOR 3D
!| INCHYD         |-->| IF YES, HYDROSTATIC INCONSISTENCY FILTER
!| INFO           |-->| INFORMATIONS FOR SOLVERS
!| IPBOT          |-->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
!| IT1            |<->| BIEF_OBJ STRUCTURES FOR INTEGER ARRAYS
!| LIFBOF         |<->| TYPE OF BOUNDARY CONDITIONS AT THE BOTTOM
!| LIFBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON LATERAL BOUNDARIES
!| LIFBOS         |<->| TYPE OF BOUNDARY CONDITIONS AT THE SURFACE
!| MASKBR         |-->| 3D MASK ON LATERAL BOUNDARIES
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MATR2H         |<->| WORK MATRIX 2DH
!| MDIFF          |<->| DIFFUSION MATRIX
!| MESH2D         |<->| 2D MESH
!| MESH3D         |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| MSUPG          |<->| NON SYMMETRIC SUPG MATRIX
!| MTRA1          |<->| 3D WORK MATRIX
!| MTRA2          |<->| 3D WORK MATRIX
!| NBOR3          |-->| GLOBAL NUMBER OF 3D BOUNDARY POINTS
!| NEWDIF         |-->| RECALCULATE OR NOT DIFFUSION MATRIX
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NPTFR3         |-->| NUMBER OF LATERAL BOUNDARY POINTS IN 3D
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| OPTDIF         |-->| OPTION FOR THE DIFFUSION OF F
!| PLUIE          |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
!| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
!| S0F            |<->| EXPLICIT SOURCE TERM (DIM=F/T)
!| S1F            |<->| IMPLICIT SOURCE TERM (DIM=1/T)
!| SCHCF          |-->| ADVECTION SCHEME OF F
!| SCHDF          |-->| DIFFUSION SCHEME OF F
!| SEM3D          |<->| SECOND MEMBERS (RIGHT HAND SIDE)
!|                |   | FOR THE LINEAR EQUATIONS 3D
!| SIGMAF         |-->| COEFFICIENT OF VISCOSITY REDUCTION
!|                |   | ONLY USED FOR K AND EPSILON
!| SIGMAG         |-->| LOGICAL FOR GENERALISED SIGMA TRANSFORMATION
!| SLVDIF         |-->| SOLVER FOR DIFFUSION OF VELOCITIES
!| SOURCES        |-->| RIGHT HAND SIDE OF CONTINUITY EQUATION WHEN SOURCES
!| SVIDE          |-->| VOID STRUCTURE
!| T2_01          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T2_02          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T2_03          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_02          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_03          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_04          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| TETADI         |<->| IMPLICITATION COEFFICIENT OF THE DIAGONAL
!|                |   | OF DIFFUSION IF OPTDIF = 2
!|                |   | IMPLICITATION COEFFICIENT OF DIFFUSION
!|                |   | IF OPTDIF = 1
!| TETASUPG       |-->| IMPLICITATION COEFFICIENT FOR SUPG
!| TRAV3          |<->| 3D WORK ARRAYS
!| TRBAF          |-->| TREATMENT ON TIDAL FLATS FOR F
!| VELOCITY       |-->| IF TRUE, COMPONENT IS VELOCITY: NOT USED
!| VISCF          |<->| VISCOSITY COEFFICIENTS
!|                |   | VISCF(*,1 OU 2) HORIZONTAL VISCOSITY
!|                |   | VISCF(*,3)      VERTICAL VISCOSITY
!| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
!| WCC            |-->| VELOCITY (NEGATIVE IF SEDIMENT SETTLING VELOCITY)
!| YAS0F          |-->| LOGICAL TO TAKE INTO ACCOUNT S0F TERM IN DIFF3D
!| YAS1F          |-->| LOGICAL TO TAKE INTO ACCOUNT S1F TERM IN DIFF3D
!| YASCE          |-->| IF TRUE, THERE IS SOURCE
!| YASEM3D        |-->| IF TRUE, RIGHT HAND SIDE HAS BEEN PARTLY
!|                |   | COMPUTED BEFORE CALLING DIFF3D
!| YAWCC          |-->| LOGICAL TO TAKE INTO ACCOUNT WCC FOR SEDIMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)             :: SCHCF,SCHDF,TRBAF,OPTDIF,NSCE
      INTEGER, INTENT(IN)             :: NPOIN2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FD,FC,FN,S0F,S1F,VISCF
      TYPE(BIEF_OBJ), INTENT(IN)      :: LIFBOL,LIFBOF,LIFBOS
      TYPE(BIEF_OBJ), INTENT(IN)      :: FBORL,FBORF,FBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: AFBORL,AFBORF,AFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: BFBORL,BFBORF,BFBORS,H,WCC
      DOUBLE PRECISION, INTENT(IN)    :: SIGMAF,FMIN,FMAX,DT,AGGLOD
      DOUBLE PRECISION, INTENT(IN)    :: FSCE(NSCE),TETASUPG
      DOUBLE PRECISION, INTENT(INOUT) :: TETADI
      LOGICAL, INTENT(IN)             :: CLIMIN,CLIMAX,YASCE,YAS0F
      LOGICAL, INTENT(IN)             :: INFO,NEWDIF,YASEM3D,YAS1F
      LOGICAL, INTENT(IN)             :: SIGMAG
      TYPE(SLVCFG)                    :: SLVDIF
!
      LOGICAL, INTENT(IN)             :: INCHYD,MSK,YAWCC,VELOCITY,RAIN
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKPT,MASKBR,MASKEL,VOLU
      TYPE(BIEF_OBJ), INTENT(IN)      :: NBOR3,SVIDE,SOURCES
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3_01,T3_02,T3_03,T3_04
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T2_01,T2_02,T2_03
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D,MESH2D
      INTEGER, INTENT(IN)             :: NPOIN3,IELM3,IELM2H,IELM2V
      INTEGER, INTENT(IN)             :: NPTFR3,NPLAN,OPTBAN
      INTEGER, INTENT(IN)             :: IPBOT(NPOIN2)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SEM3D,IT1,TRAV3,MTRA1,MTRA2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MSUPG,MDIFF
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MATR2H
      DOUBLE PRECISION, INTENT(IN)    :: PLUIE(NPOIN2)
!
!-----------------------------------------------------------------------
!
      INTEGER IPOIN3,IPOIN2,IPTFR3,ITERD,NITERD,IS,IIS,I,IPLAN,I2D,NP
      DOUBLE PRECISION C,AGGLO
      CHARACTER(LEN=16) FORMUL
!
!-----------------------------------------------------------------------
!
!   SEMI-IMPLICITATION COEFFICIENT FOR SUPG
!
!     TETASUPG = 1.D0  (IS REQUIRED FOR MASS CONSERVATION)
!     IF NOT 1.D0, FLUX AT THE END OF CVDF3D SHOULD BE MODIFIED
!     TETASUPG = 0.55D0
!
      NITERD = SLVDIF%NITMAX
      IF(OPTDIF.EQ.2) TETADI = 0.D0
!
!=======================================================================
!   MASS MATRIX
!=======================================================================
!
!   COMPUTES MTRA2 : MASS MATRIX DIVIDED BY DT
!
      FORMUL='MATMAS          '
      CALL MATRIX(MTRA2, 'M=N     ', FORMUL, IELM3, IELM3, 1.D0/DT,
     &         SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE, MESH3D, MSK, MASKEL)
!
! MASS-LUMPING OF THE MASS MATRIX (FOR THE MOMENT ONLY IF
! EXPLICIT DIFFUSION OR 2 X IMPLICIT DIAGONAL)
! OF IF COMPATIBILITY WITH PSI SCHEME NEEDED (USE OF VOLU WHICH
! IS DIFFERENT FROM INTEGRAL OF TEST FUNCTIONS IF MASS-LUMPING
! IS DONE IN TELEMAC-2D)
!
      AGGLO=AGGLOD
      IF(TETADI.LT.0.001D0.OR.SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI
     &                    .OR.SCHCF.EQ.ADV_LPO.OR.SCHCF.EQ.ADV_LPO_TF
     &                    .OR.SCHCF.EQ.ADV_NSC_TF) AGGLO=1.D0
      IF(AGGLO.GT.0.001D0) THEN
!       VOLU/DT REPLACES MTRA2 LUMPED
        CALL OS('X=CY    ',X=T3_01,Y=VOLU,C=AGGLO/DT)
!       IF(MSK) THERE IS A RISK THAT VOLU=0 SOMEWHERE
!               THIS MAY CAUSE PROBLEMS IN LINEAR SYSTEMS
        IF(MSK) CALL OS('X=+(Y,C)',X=T3_01,Y=T3_01,C=1.D-3)
!       CALL LUMP(T3_01,MTRA2,MESH3D,AGGLO)
        CALL OM( 'M=CN    ',MTRA2,MTRA2 , SVIDE , 1.D0-AGGLO , MESH3D )
        CALL OM( 'M=M+D   ',MTRA2,MTRA2 , T3_01 , 0.D0       , MESH3D )
      ENDIF
!
!=======================================================================
!   SECOND MEMBER OF ADVECTION SCHEME + EXPLICIT SOURCE TERM
!=======================================================================
!
!   COMPUTES SEM3D = DT * S0F + FC
!
      IF(S0F%TYPR.NE.'0'.AND.YAS0F) THEN
        CALL OS( 'X=Y+CZ  ' , T3_04 , FC , S0F , DT )
      ELSE
        CALL OS( 'X=Y     ' , X=T3_04 , Y=FC )
      ENDIF
!
      IF(YASEM3D) THEN
!       HAS STARTED COMPUTING SEM3D IN TRISOU OR SOURCE
        CALL MATVEC ('X=X+CAY  ',SEM3D, MTRA2,T3_04,DT, MESH3D)
!                                                   DT BECAUSE MTRA2
!                                                   HAS A FACTOR 1/DT
      ELSE
        CALL MATVEC ('X=AY    ',SEM3D, MTRA2,T3_04, C, MESH3D)
      ENDIF
!
!=======================================================================
!
!     SOURCES INSIDE THE DOMAIN
!
      IF(YASCE.AND.NSCE.GT.0) THEN
      DO IS=1,NSCE
!       IF INTAKE FSCE=F, SO NO EXTRA TERM
        IIS=IS
!       IN PARALLEL MODE SOURCES WITHOUT PARCOM
        IF(NCSIZE.GT.1) IIS=IIS+NSCE
        DO I=1,NPOIN3
!         EXPLICIT SOURCE TERM
          SEM3D%R(I) = SEM3D%R(I)
     &               + MAX(SOURCES%ADR(IIS)%P%R(I),0.D0)*
     &            (FSCE(IS)-(1.D0-TETASUPG)*FN%R(I))
!         IMPLICIT SOURCE TERM : SEE BELOW
        ENDDO
      ENDDO
      ENDIF
!
!=======================================================================
!
!     RAIN (ALL TRACERS) - EXPLICIT PART
!
      IF(RAIN) THEN
        DO I=1,NPOIN2
          IPOIN3=NPOIN3-NPOIN2+I
          SEM3D%R(IPOIN3)=SEM3D%R(IPOIN3)-
     &                    PLUIE(I)*(1.D0-TETASUPG)*FN%R(IPOIN3)
        ENDDO
      ENDIF
!
!=======================================================================
!
!   EXPLICIT ADVECTION TERM:
!
      IF(SCHCF.EQ.ADV_SUP) THEN
        CALL MATVEC('X=X+CAY ',SEM3D,MSUPG,FN,TETASUPG-1.D0,MESH3D)
      ENDIF
!
!=======================================================================
!   IMPLICIT SOURCE TERM (MASS-LUMPED AND ADDED TO THE DIAGONAL)
!=======================================================================
!
! JMH PROPOSITION (LUMPED IMPLICIT SOURCE TERM)
!                  BEWARE THE + SIGN: THE TERM S1F HAS A + SIGN WHEN
!                  TO THE LEFT OF THE = SIGN
!                 (CAREFUL: OPPOSITE IN CVDFTR)
!                  VOLU USED FOR MASS-CONSERVATION
!                 (COMPATIBILITY WITH CONTINUITY EQUATION)
      IF(S1F%TYPR.NE.'0'.AND.YAS1F) THEN
        CALL OS( 'X=YZ    ' , X=T3_01 , Y=S1F , Z=VOLU )
        CALL OM( 'M=M+D   ' , MTRA2 , MTRA2 , T3_01 , C , MESH3D )
      ENDIF
!
!=======================================================================
!
!     SOURCES INSIDE THE DOMAIN
!
      IF(YASCE.AND.NSCE.GT.0) THEN
      DO IS=1,NSCE
!       IF INTAKE FSCE=T, SO NO EXTRA TERM
        IIS=IS
!       IN PARALLEL MODE SOURCES WITHOUT PARCOM
        IF(NCSIZE.GT.1) IIS=IIS+NSCE
        DO I=1,NPOIN3
!         IMPLICIT SOURCE TERM
          MTRA2%D%R(I)=MTRA2%D%R(I)+
     &                 MAX(SOURCES%ADR(IIS)%P%R(I),0.D0)*TETASUPG
        ENDDO
      ENDDO
      ENDIF
!
!     RAIN (ALL TRACERS) - IMPLICIT PART
!
      IF(RAIN) THEN
        DO I=1,NPOIN2
          IPOIN3=NPOIN3-NPOIN2+I
          MTRA2%D%R(IPOIN3)=MTRA2%D%R(IPOIN3)+PLUIE(I)*TETASUPG
        ENDDO
      ENDIF
!
!=======================================================================
!   DIFFUSION MATRIX + BOUNDARY TERMS
!=======================================================================
!
      IF(SCHDF.NE.0) THEN
!
         IF(INFO) THEN
           IF(SCHCF.EQ.ADV_SUP) THEN
             IF(LNG.EQ.1) WRITE(LU,*) 'DIFFUSION DE ',FN%NAME,
     &                                ' AVEC CONVECTION PAR SUPG'
             IF(LNG.EQ.2) WRITE(LU,*) 'DIFFUSION OF ',FN%NAME,
     &                                ' WITH SUPG ADVECTION'
           ELSE
             IF(LNG.EQ.1) WRITE(LU,*) 'DIFFUSION DE ',FN%NAME
             IF(LNG.EQ.2) WRITE(LU,*) 'DIFFUSION OF ',FN%NAME
           ENDIF
         ENDIF
!
         IF(NEWDIF) THEN
!
!           RELEASE 5.7
!           FORMUL='MATDIF          '
!           RELEASE 5.8 : MONOTONICITY ENSURED
            FORMUL='MATDIF       MON'
            IF (INCHYD) FORMUL(7:7)='2'
!
             CALL MATRIX
     &      (MDIFF, 'M=N     ', FORMUL, IELM3, IELM3, 1.D0,
     &       VISCF%ADR(1)%P, VISCF%ADR(2)%P, VISCF%ADR(3)%P,
     &       SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
!
         ENDIF
!
!=======================================================================
!
         IF(OPTDIF.EQ.1) THEN
!
!        SEMI-IMPLICITATION OF THE DIFFUSION
!
         IF(TETADI.LT.0.999D0) THEN
!          IF(TETADI.GT.0.001) THEN
           CALL MATVEC ('X=X+CAY  ',SEM3D,MDIFF,FN,(TETADI-1.D0)/SIGMAF,
     &                              MESH3D)
!          ELSE
!          CALL VECTOR
!    *      (SEM3D, '+', 'VECDIF          ',IELM3,(TETADI-1.D0)/SIGMAF,
!    *       VISCF%ADR(1)%P,VISCF%ADR(2)%P,VISCF%ADR(3)%P,
!    *       FN, SVIDE, SVIDE, MESH3D,MSK, MASKEL)
!          ENDIF
         ENDIF
!        IF TETADI=0, NO NEED TO ADD MDIFF
         IF(TETADI.GT.0.001D0) THEN
           CALL OM('M=M+CN  ',MTRA2,MDIFF,SVIDE,TETADI/SIGMAF,MESH3D)
         ENDIF
!
         ENDIF
!
! TAKES THE IMPLICIT BOUNDARY TERMS INTO ACCOUNT (FRICTION FOR EXAMPLE)
! ---------------------------------------------
!
!   LATERAL FACES : (MASKBR SET ACCORDING TO MASKEL IN MASK3D)
!  (MASS-LUMPED FORM)
!
         IF(AFBORL%TYPR.NE.'0') THEN
           CALL VECTOR(T3_02,'=','MASBAS          ',IELM2V,-1.D0,SVIDE,
     &                 SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,MSK,MASKEL)
           CALL OS('X=XY    ',T3_02,AFBORL,AFBORL,0.D0)
           CALL OSDB( 'X=X+Y   ' , MTRA2%D , T3_02 , T3_02, C , MESH3D )
         ENDIF
!
!  (NON MASS-LUMPED FORM, BUT MATR2V NOW SUPPRESSED)
!        FORMUL='FMATMA          '
!        CALL MATRIX
!    *   (MATR2V, 'M=N     ', FORMUL, IELM2V, IELM2V, -1.D0, AFBORL,
!    *    SVIDE, SVIDE, SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKBR)
!        CALL OM('M=M+N   ',MTRA2,MATR2V,SVIDE,C,MESH3D)
!
!   BOTTOM (MASS-LUMPED FORM AS IN 2D):
!
         IF(AFBORF%TYPR.NE.'0') THEN
           CALL VECTOR(T2_03, '=','MASBAS          ',IELM2H,-1.D0,SVIDE,
     &                 SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
           CALL OV('X=XY    ',T2_03%R,AFBORF%R,AFBORF%R,0.D0,NPOIN2)
!          DRY ZONES OR CRUSHED ELEMENTS
!          SEE EQUIVALENT TREATMENT IN WAVE_EQUATION
           IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
             DO IPOIN2 = 1,NPOIN2
               DO NP=0,IPBOT(IPOIN2)
                 I=NP*NPOIN2+IPOIN2
                 MTRA2%D%R(I)=MTRA2%D%R(I)+T2_03%R(IPOIN2)
               ENDDO
             ENDDO
           ELSE
             DO IPOIN2 = 1,NPOIN2
               MTRA2%D%R(IPOIN2)=MTRA2%D%R(IPOIN2)+T2_03%R(IPOIN2)
             ENDDO
           ENDIF
         ENDIF
!
!   SURFACE (MASS-LUMPED FORM):
!
         IF(AFBORS%TYPR.NE.'0') THEN
           CALL VECTOR(T2_03, '=','MASBAS          ',IELM2H,-1.D0,SVIDE,
     &                 SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
           CALL OV('X=XY    ',T2_03%R,AFBORS%R,AFBORS%R,0.D0,NPOIN2)
           CALL OV('X=X+Y   ',MTRA2%D%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                        T2_03%R,T2_03%R,0.D0,NPOIN2)
         ENDIF
!           (NON MASS-LUMPED FORM):
!        FORMUL='FMATMA          '
!        CALL MATRIX
!    *   (MATR2H, 'M=N     ',FORMUL, IELM2H, IELM2H, -1.D0, AFBORS,
!    *    SVIDE, SVIDE, SVIDE, SVIDE, SVIDE, MESH2D, MSK, MASKEL)
!        CALL OM('M=M+NS  ', MTRA2, MATR2H, SVIDE, C, MESH3D)
!
!   TAKES THE EXPLICIT BOUNDARY TERMS INTO ACCOUNT
!   ---------------------------------------------
!
         CALL STRESS(SEM3D,'X=X+Y   ',T2_01,T3_02,
     &               BFBORL,BFBORF,BFBORS,NPOIN2,NPOIN3,MESH2D,
     &               MESH3D,IELM3,IELM2H,IELM2V,SVIDE,MSK,MASKBR,MASKEL)
!
!=======================================================================
!   SEDIMENT-SPECIFIC
!                                D
!   THE MATRIX - PSI1(J) * WCC * --( PSI2(I) ) IS ADDED
!                                DZ
!                                                     D          N+1
!   IT IS AN INTEGRATION BY PART OF TERM :  PSI2(I) * --( WCC * C    )
!                                                     DZ
!
!   CORRESPONDING BOUNDARY TERMS ARE THE DEPOSITION
!   THIS TERM IS INCLUDED IN ATABOF !!!
!
!   NOTE: IT IS DONE IF AND ONLY IF SED. DIFFUSION IS REQUIRED !
!=======================================================================
!
!
!       RELEASE 5.5
!
!       IF(YAWCC) THEN
!       FOR BOUNDARY TERMS, SEE SUBROUTINE FLUSED
!       CALL MATRIX
!    &      (MTRA1, 'M=N     ', 'MATFGR         Z', IELM3, IELM3, -1.D0,
!    &       WCC, SVIDE, SVIDE, SVIDE, SVIDE, SVIDE, MESH3D,MSK, MASKEL)
!
!       CALL OM('M=X(M)  ',MTRA2,MTRA2,SVIDE,C,MESH3D)
!       CALL OM('M=M+N   ',MTRA2,MTRA1,SVIDE,C,MESH3D)
!       ENDIF
!
!       RELEASE 5.6 (UPWINDING VERTICAL ADVECTION)
!
        IF(YAWCC) THEN
!       FOR BOUNDARY TERMS, SEE SUBROUTINE FLUSED
        CALL MATRIX
     &      (MTRA1, 'M=N     ', 'MATFGR         Z', IELM3, IELM3, -1.D0,
     &       WCC, SVIDE, SVIDE, SVIDE, SVIDE, SVIDE, MESH3D,MSK, MASKEL)
!       UPWINDING VERTICAL ADVECTION (HERE UPWIND COEFFICIENT 1.D0)
!       MTRA2 MUST STILL BE SYMMETRIC HERE
        CALL UPWIND(MTRA2,WCC,1.D0,MESH2D,MESH3D,NPLAN)
!       MTRA2 TRANSFORMED INTO NON SYMMETRIC MATRIX
        CALL OM('M=X(M)  ',MTRA2,MTRA2,SVIDE,C,MESH3D)
        CALL OM('M=M+N   ',MTRA2,MTRA1,SVIDE,C,MESH3D)
        ENDIF
!
      ENDIF
!
!=======================================================================
!
!   ADDS SUPG MATRIX IF SCHCF=AADV_SUP
!
!=======================================================================
!
!
      IF(SCHCF.EQ.ADV_SUP.AND.OPTDIF.EQ.1) THEN
!
         IF(MTRA2%TYPEXT.EQ.'S') THEN
           CALL OM('M=X(M)  ',MTRA2,MTRA2,SVIDE,C,MESH3D)
         ENDIF
         CALL OM ('M=M+CN  ',MTRA2, MSUPG, SVIDE, TETASUPG, MESH3D )
!
      ENDIF
!
!=======================================================================
!
!   BOUNDARY CONDITIONS + PRECONDITIONING + SOLVER
!
!=======================================================================
!
!   BOUNDARY CONDITIONS FOR BOUNDARY POINTS (POINTS OF TYPE DIRICHLET)
!
      CALL CPSTVC(MTRA2%D,T3_03)
!
      DO IPOIN3 = 1,NPOIN3
        IT1%I(IPOIN3) = KDDL
        T3_03%R(IPOIN3) = 0.D0
      ENDDO
!
!   LATERAL BOUNDARY CONDITIONS
!
      DO IPTFR3 = 1,NPTFR3
        IF(LIFBOL%I(IPTFR3).EQ.KENT.OR.
     &    LIFBOL%I(IPTFR3).EQ.KENTU.OR.LIFBOL%I(IPTFR3).EQ.KADH) THEN
          IT1%I(NBOR3%I(IPTFR3)) = KDIR
          T3_03%R(NBOR3%I(IPTFR3)) = FBORL%R(IPTFR3)
        ENDIF
      ENDDO
!
!   BOTTOM AND SURFACE BOUNDARY CONDITIONS
!
      DO IPOIN2 = 1,NPOIN2
        IF(LIFBOF%I(IPOIN2).EQ.KENT.OR.LIFBOF%I(IPOIN2).EQ.KADH) THEN
          IT1%I(IPOIN2) = KDIR
          T3_03%R(IPOIN2) = FBORF%R(IPOIN2)
        ENDIF
        IF(LIFBOS%I(IPOIN2).EQ.KENT.OR.LIFBOS%I(IPOIN2).EQ.KADH) THEN
          IT1%I(NPOIN3-NPOIN2+IPOIN2) = KDIR
          T3_03%R(NPOIN3-NPOIN2+IPOIN2) = FBORS%R(IPOIN2)
        ENDIF
      ENDDO
!
!   CRUSHED POINTS AND TIDAL FLATS: FIRST TREATED AS DIRICHLET
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        IF(NCSIZE.GT.1) THEN
!         ONLY DIFFERENCE : VALUE OF MTRA2 EQUAL TO FAC INSTEAD OF 1
          CALL OS('X=Y     ',X=T3_02,Y=VOLU)
!         T3_02 WILL BE THE ASSEMBLED VOLUME
          CALL PARCOM(T3_02,2,MESH3D)
          DO I2D=1,NPOIN2
            IF(IPBOT(I2D).GT.0) THEN
              IF(LIFBOF%I(I2D).EQ.KENT.OR.LIFBOF%I(I2D).EQ.KADH) THEN
!               DIRICHLET POINT ON THE BOTTOM: ALL POINTS UP TO THE
!               FIRST FREE ARE TREATED AS DIRICHLET WITH FBORF VALUE
                DO I=0,IPBOT(I2D)
                  IPOIN3 = I2D+I*NPOIN2
                  MTRA2%D%R(IPOIN3)=MESH3D%FAC%R(IPOIN3)
                  IT1%I(IPOIN3)   = KDIR
                  T3_03%R(IPOIN3) = FBORF%R(I2D)
                ENDDO
              ELSE
!               POINTS WITH NO VOLUME
!               NECESSARY TO HAVE A WELL DEFINED LINEAR SYSTEM
!               NOT A DIRICHLET POINT ON THE BOTTOM
!               CRUSHED POINTS PROVISIONALLY SET TO FN
                DO I=0,IPBOT(I2D)-1
                  IPOIN3 = I2D+I*NPOIN2
                  IF(T3_02%R(IPOIN3).LT.1.D-10) THEN
                    MTRA2%D%R(IPOIN3)=MESH3D%FAC%R(IPOIN3)
                    IT1%I(IPOIN3)   = KDIR
                    T3_03%R(IPOIN3) = FN%R(IPOIN3)
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
          ENDDO
        ELSE
          DO I2D=1,NPOIN2
            IF(IPBOT(I2D).GT.0) THEN
              IF(LIFBOF%I(I2D).EQ.KENT.OR.LIFBOF%I(I2D).EQ.KADH) THEN
!               DIRICHLET POINT ON THE BOTTOM: ALL POINTS UP TO THE
!               FIRST FREE ARE TREATED AS DIRICHLET WITH FBORF VALUE
                DO I=0,IPBOT(I2D)
                  IPOIN3 = I2D+I*NPOIN2
                  MTRA2%D%R(IPOIN3)=1.D0
                  IT1%I(IPOIN3) = KDIR
                  T3_03%R(IPOIN3) = FBORF%R(I2D)
                ENDDO
              ELSE
!               POINTS WITH NO VOLUME
!               NECESSARY TO HAVE A WELL DEFINED LINEAR SYSTEM
!               NOT A DIRICHLET POINT ON THE BOTTOM
!               CRUSHED POINTS PROVISIONALLY SET TO FN
                DO I=0,IPBOT(I2D)-1
                  IPOIN3 = I2D+I*NPOIN2
                  IF(VOLU%R(IPOIN3).LT.1.D-10) THEN
                    MTRA2%D%R(IPOIN3)=1.D0
                    IT1%I(IPOIN3) = KDIR
                    T3_03%R(IPOIN3) = FN%R(IPOIN3)
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!---> IMPLICIT DIFFUSION (STANDARD)
!
      IF(OPTDIF.EQ.1) THEN
!
        CALL DIRI01(FD,MTRA2,SEM3D,T3_03,IT1%I,T3_01,T3_02,MESH3D,
     &              KDIR,MSK,MASKPT)
!
!   SOLVES THE LINEAR SYSTEM:
!
        IF(TETADI.GT.0.001D0.OR.SCHCF.EQ.ADV_SUP) THEN
          CALL SOLVE (FD, MTRA2, SEM3D, TRAV3, SLVDIF,
     &                INFO, MESH3D, MTRA1)
        ELSE
          IF(NCSIZE.GT.1) CALL PARCOM(MTRA2%D,2,MESH3D)
          CALL OS('X=Y/Z   ',FD,SEM3D,MTRA2%D,0.D0,2,0.D0,1.D-10)
        ENDIF
!
!---> EXPLICIT DIFFUSION / PARTIAL IMPLICITATION OF 2XDIAGONAL
!
      ELSEIF(OPTDIF.EQ.2) THEN
!---> BACKS UP IF NITERD> 1
        CALL OS( 'X=Y     ' , T3_04 , SEM3D , SVIDE , C )
!---> NITER=1
        CALL OS('X=CY    ',T3_01,MDIFF%D,T3_01,2.D0/SIGMAF )
        CALL OM( 'M=M+D   ',MTRA2,MTRA2,T3_01,C,MESH3D)
! CALL OM( 'M=N     ',MTRA1,MTRA2,MTRA2,C,MESH3D)
!
        CALL MATVEC
     &       ('X=X+CAY ',SEM3D,MDIFF,FN,-1.D0/SIGMAF,MESH3D)
        CALL OS('X=X+CYZ ',SEM3D,FN,MDIFF%D,2.D0/SIGMAF)
!
        CALL DIRI01(FD,MTRA2,SEM3D,T3_03,IT1%I,T3_01,T3_02,MESH3D,
     &            KDIR,MSK,MASKPT)
!
!-->  SOLVES THE LINEAR SYSTEM:
!
        CALL LUMP(T3_01, MTRA2, MESH3D, 1.D0)
        CALL OS( 'X=Y/Z   ' , FD , SEM3D , T3_01 , C )
!
!---> GAUSS-SEIDEL ITERATIONS
!
        IF(NITERD.GE.2) THEN
        DO ITERD = 2,NITERD
          CALL OS( 'X=Y     ' , SEM3D , T3_04 , SVIDE , C )
          CALL MATVEC
     &       ('X=X+CAY ',SEM3D,MDIFF,FD,-1.D0/SIGMAF,MESH3D)
          CALL OS('X=X+CYZ ',SEM3D,FD,MDIFF%D,2.D0/SIGMAF)
!
          CALL DIRI01(FD,MTRA2,SEM3D,T3_03,IT1%I,T3_01,T3_02,MESH3D,
     &              KDIR,MSK,MASKPT)
!
!---> SOLVES THE DIAGONAL LINEAR SYSTEM:
!
          CALL LUMP(T3_01, MTRA2, MESH3D, 1.D0)
          CALL OS( 'X=Y/Z   ' , FD , SEM3D , T3_01 , C )
!
        ENDDO
        ENDIF
!
      ENDIF
!
!=======================================================================
!
!   TREATS THE POINTS WHICH ARE COMPLETELY DRY
!    (BY DEFAULT FORCED TO 0)
!
      IF(MSK.AND.TRBAF.EQ.1) THEN
        DO IPOIN3 = 1,NPOIN3
          IF(MASKPT%R(IPOIN3).LT.0.5D0) FD%R(IPOIN3) = FN%R(IPOIN3)
        ENDDO
      ENDIF
!
!=======================================================================
!
!   THOSE CRUSHED POINTS THAT HAVE NO VOLUME MUST HAVE VALUES EQUAL
!   TO THE FIRST FREE POINT ABOVE (GIVEN BY PLANE IPBOT+1)
!
!   TO AVOID SPURIOUS GRADIENTS... BUT WELL, HEAVY
!
!=======================================================================
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        IF(NCSIZE.GT.1) THEN
          CALL OS('X=Y     ',X=T3_02,Y=VOLU)
!         T3_02 WILL BE THE ASSEMBLED VOLUME
          CALL PARCOM(T3_02,2,MESH3D)
          DO I2D=1,NPOIN2
            IF(IPBOT(I2D).GT.0) THEN
!             VALUE OF THE FIRST FREE POINT IS COPIED BELOW
!             FOR DIRICHLET CASES THIS FREE POINT HAS BEEN GIVEN ABOVE
!             THE FBORF DIRICHLET VALUE
              DO I=0,IPBOT(I2D)-1
                IF(T3_02%R(I2D+I*NPOIN2).LT.1.D-10) THEN
                  FD%R(I2D+I*NPOIN2)=FD%R(I2D+IPBOT(I2D)*NPOIN2)
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ELSE
          DO I2D=1,NPOIN2
            IF(IPBOT(I2D).GT.0) THEN
!             VALUE OF THE FIRST FREE POINT IS COPIED BELOW
!             FOR DIRICHLET CASES THIS FREE POINT HAS BEEN GIVEN ABOVE
!             THE FBORF DIRICHLET VALUE
              DO I=0,IPBOT(I2D)-1
                IF(VOLU%R(I2D+I*NPOIN2).LT.1.D-10) THEN
                  FD%R(I2D+I*NPOIN2)=FD%R(I2D+IPBOT(I2D)*NPOIN2)
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!
!   CLIPS F (IF REQUESTED WITH CLIMIN AND CLIMAX)
!
!=======================================================================
!
      CALL CLIP(FD,FMIN,CLIMIN,FMAX,CLIMAX,0)
!
!=======================================================================
!
      RETURN
      END
