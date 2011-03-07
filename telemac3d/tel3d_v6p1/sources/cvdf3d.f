!                    *****************
                     SUBROUTINE CVDF3D
!                    *****************
!
     & (FD,FC,FN,VISCF,SIGMAF,S0F,YAS0F,S1F,YAS1F,
     &  FBORL,FBORF,FBORS,AFBORL,AFBORF,AFBORS,
     &  BFBORL,BFBORF,BFBORS,LIFBOL,LIFBOF,LIFBOS,
     &  FLUXF,FLUEXT,FLUEXTPAR,FMIN,CLIMIN,FMAX,CLIMAX,
     &  SCHCF,SCHDF,SLVDIF,TRBAF,INFOR,NEWDIF,CALFLU,
     &  T2_01,T2_02,T2_03,
     &  T3_01,T3_02,T3_03,T3_04,MESH3D,IKLE3,MASKEL,MTRA1,
     &  W1,NPTFR3,MMURD,MURD_TF,VOLU,VOLUPAR,VOLUN,VOLUNPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     &  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,IT2,
     &  TRAV3,MESH2D,MATR2H,H,OPTBAN,OPTDIF,TETADI,
     &  YAWCC,WCC,AGGLOD,NSCE,SOURCES,FSCE,NUMLIQ,DIRFLU,NFRLIQ,
     &  VOLUT,ZT,ZPROP,RAIN,PLUIE,PARAPLUIE,FLODEL,FLOPAR,SIGMAG,IPBOT)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE ADVECTION-DIFFUSION STEP.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J.M. HERVOUET (LNHE)
!+        18/12/2009
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
!| AFBORF         |---|
!| AFBORS         |---|
!| AGGLOD         |-->| MASS-LUMPING DANS LA DIFFUSION
!| BFBORF         |---|
!| BFBORS         |---|
!| CALFLU         |-->| INDIQUE SI ON CALCULE LE FLUX POUR LE BILAN
!| CLIMAX         |---|
!| DIRFLU         |---|
!| DT             |-->| PAS DE TEMPS
!| FBORF          |---|
!| FBORS          |---|
!| FC             |<--| VARIABLE APRES CONVECTION
!| FD             |<--| VARIABLE APRES DIFFUSION
!| FLODEL         |---|
!| FLOPAR         |---|
!| FLUEXT         |---|
!| FLUXF          |---|
!| FMIN,FMAX      |-->| VALEURS DE CLIPPING
!| FN             |-->| VARIABLE AU TEMPS N
!| FSCE           |---|
!| H              |---|
!| IELM2H         |-->| TYPE DE DISCRETISATION 2DH
!| IELM2V         |-->| TYPE DE DISCRETISATION 2DV
!| IELM3          |-->| TYPE DE DISCRETISATION 3D
!| IKLE3          |-->| CORRESPONDANCE NUMEROTATION LOCALE ET GLOBALE
!| INCHYD         |---|
!| INFOR          |-->| INFORMATIONS SUR LES SOLVEURS
!| IPBOT          |---|
!| IT1            |---|
!| IT2            |---|
!| LIFBOF         |---|
!| LIFBOS         |---|
!| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
!| MASKBR         |---|
!| MASKEL         |-->| MASQUAGE DES ELEMENTS
!| MASKPT         |-->| MASQUAGE DES POINTS
!| MATR2H         |<->| MATRICE DE TRAVAIL 2DH
!| MDIFF          |---|
!| MESH2D         |---|
!| MESH3D         |---|
!| MMURD          |---|
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
!| MSUPG          |---|
!| MTRA1          |---|
!| MTRA2          |---|
!| MURD_TF        |---|
!| NBOR3          |-->| NUMEROS GLOBAUX DES POINTS FRONTIERES 3D
!| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
!| NELEM3         |-->| NOMBRE D'ELEMENTS 3D
!| NEWDIF         |-->| RECALCULE OU NON LA MATRICE DE DIFFUSION
!| NFRLIQ         |---|
!| NPLAN          |-->| NOMBRE DE PLANS DU MAILLAGE 3D
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| NPOIN3         |-->| NOMBRE DE POINTS 3D
!| NPTFR3         |-->| NOMBRE DE POINTS FRONTIERE BORDS LATERAUX
!| NSCE           |---|
!| NUMLIQ         |---|
!| OPTBAN         |---|
!| OPTDIF         |---|
!| PARAPLUIE      |-->| RAIN (IN ASSEMBLED MODE IN PARALLEL)
!| PLUIE          |-->| RAIN
!| RAIN           |---|
!| SCHCF          |-->| SCHEMA DE CONVECTION DE F
!| SCHDF          |-->| SCHEMA DE DIFFUSION DE F
!| SEM3D          |---|
!| SIGMAF         |-->| COEFFICIENT DE REDUCTION DE LA VISCOSITE
!| SIGMAG         |---|
!| SLVDIF         |---|
!| SOURCES        |---|
!| SVIDE          |-->| STRUCTURE VIDE
!| T2_01          |---|
!| T2_02          |---|
!| T2_03          |---|
!| T3_01          |---|
!| T3_02          |---|
!| T3_03          |---|
!| T3_04          |---|
!| TETADI         |---|
!| TRAV3          |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 3D
!| TRBAF          |---|
!| VISCF          |-->| COEFFICIENTS DE VISCOSITE
!|                |   | VISCF(*,1 OU 2) VISCOSITE HORIZONTALE
!|                |   | VISCF(*,3)      VISCOSITE VERTICALE
!| VOLU           |-->| VOLUME DE CONTROLE A L'INSTANT N+1
!| VOLUN          |-->| VOLUME DE CONTROLE A L'INSTANT N
!| VOLUT          |---|
!| W1             |<->| TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
!| WCC            |---|
!| YASEM3D        |---|
!| YAWCC          |---|
!| ZPROP          |---|
!| ZT             |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_TELEMAC3D, EX_CVDF3D => CVDF3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FD, FC, FN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: S0F, S1F, VISCF
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: LIFBOL, LIFBOF, LIFBOS
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FBORL, FBORF, FBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: AFBORL, AFBORF, AFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: BFBORL, BFBORF, BFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: FLUEXT,PLUIE,PARAPLUIE
      TYPE(BIEF_OBJ), INTENT(IN)      :: FLUEXTPAR
      DOUBLE PRECISION, INTENT(IN)    :: SIGMAF,FMIN,FMAX,DT
      DOUBLE PRECISION, INTENT(IN)    :: AGGLOD
      DOUBLE PRECISION, INTENT(INOUT) :: FLUXF,TETADI
      INTEGER, INTENT(IN)             :: SCHCF,SCHDF,TRBAF,NPTFR3,NFRLIQ
      INTEGER, INTENT(IN)             :: NUMLIQ(*),DIRFLU(*)
      LOGICAL, INTENT(IN)             :: CLIMIN,CLIMAX,RAIN,YAS0F,YAS1F
      LOGICAL, INTENT(IN)             :: INFOR,NEWDIF,CALFLU,MSK,SIGMAG
      TYPE(SLVCFG)                    :: SLVDIF
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,IKLE3,FLODEL,FLOPAR
      TYPE(BIEF_OBJ), INTENT(IN)      :: NBOR3,WCC,SOURCES,ZPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3_01,T3_02,T3_03,T3_04,W1
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T2_01,T2_02,T2_03,ZT
      TYPE(BIEF_OBJ), TARGET, INTENT(INOUT) :: VOLUT
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D
      INTEGER, INTENT(IN)             :: NPOIN3,NPOIN2
      INTEGER, INTENT(IN)             :: IPBOT(NPOIN2)
      INTEGER, INTENT(IN)             :: NPLAN,NELEM2,NELEM3,LV
      INTEGER, INTENT(IN)             :: OPTBAN,OPTDIF
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MMURD,MURD_TF,MTRA1
      TYPE(BIEF_OBJ), INTENT(IN)      :: VOLUN,VOLUNPAR,VOLUPAR
      TYPE(BIEF_OBJ), TARGET, INTENT(IN) :: VOLU
      LOGICAL, INTENT(IN)             :: INCHYD,YASEM3D,YAWCC
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKPT,MASKBR,H,SVIDE
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D
      INTEGER, INTENT(IN)             :: IELM3,IELM2H,IELM2V,NSCE
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SEM3D,IT1,IT2,TRAV3,MTRA2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MSUPG,MDIFF,MATR2H
      DOUBLE PRECISION, INTENT(IN)    :: FSCE(NSCE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP,K,NPTFR,IPLAN,IPTFR,IS,IPTFR2,I,IIS,PARA
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
      DOUBLE PRECISION STOFD,TETASUPG
      TYPE(BIEF_OBJ), POINTER :: VOLUME
!
!     FUNCTIONS
!
      DOUBLE PRECISION P_DSUM,LAMBDA
      EXTERNAL         P_DSUM
!
      LOGICAL YADIRFLU,YASCE,VELOCITY,YARAIN
!
!***********************************************************************
!
      VELOCITY=.FALSE.
      IF(FN%NAME(1:1).EQ.'U'.OR.
     &   FN%NAME(1:1).EQ.'V'.OR.
     &   FN%NAME(1:1).EQ.'W') VELOCITY=.TRUE.
!
!     EVEN IF.NOT.CALFLU
!
      FLUXF = 0.D0
!
!     WITH DISTRIBUTIVE SCHEMES : COMPUTES PRESCRIBED VALUES THAT
!     WILL ENSURE THE CORRECT FLUX (REAL PRESCRIBED VALUES DISCARDED)
!     THESE CORRECTED PRESCRIBED VALUES ARE SET BEFORE ADVECTION
!
!     YADIRFLU=.TRUE. : THERE IS AT LEAST ONE BOUNDARY WITH
!                       TREATMENT OF FLUXES AT BOUNDARIES = 2
      YADIRFLU=.FALSE.
!     DIRFLU DISCARDED FOR VELOCITIES
      IF(NFRLIQ.GT.0.AND..NOT.VELOCITY) THEN
        DO K=1,NFRLIQ
          IF(DIRFLU(K).EQ.2) YADIRFLU=.TRUE.
        ENDDO
      ENDIF
!
!=======================================================================
!
!     FOR TRACERS (=NOT VELOCITY) : DIRICHLET VALUES ARE NOT RESPECTED IF EXIT
!     THERE IS NO NEED TO TEST KENTU OR KADH FOR TRACERS
!
      IF(NPTFR3.GT.0.AND.NFRLIQ.GT.0.AND..NOT.VELOCITY) THEN
        DO IPTFR=1,NPTFR3
          IF(LIFBOL%I(IPTFR).EQ.KENT) THEN
!           EXITS ARE TREATED AS FREE BOUNDARIES
            IP=NBOR3%I(IPTFR)
            IF(FLUEXTPAR%R(IP).GE.0.D0) LIFBOL%I(IPTFR)=KSORT
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
!
!     A PRIORI CORRECTION OF FN FOR REAL ENTRANCES
!     I.E. LIFBOL STILL KENT DESPITE ABOVE CHANGE
!
      IF((SCHCF.EQ.ADV_SUP   .OR.SCHCF.EQ.ADV_NSC    .OR.
     &    SCHCF.EQ.ADV_PSI   .OR.SCHCF.EQ.ADV_LPO    .OR.
     &    SCHCF.EQ.ADV_LPO_TF.OR.SCHCF.EQ.ADV_NSC_TF)
     &                                              .AND.YADIRFLU) THEN
!
        IF(NPTFR3.GT.0) THEN
!
        DO IP=1,NPTFR3
          IF(NUMLIQ(IP).GE.1) THEN
          IF(DIRFLU(NUMLIQ(IP)).EQ.2.AND.LIFBOL%I(IP).EQ.KENT) THEN
            I=NBOR3%I(IP)
            LAMBDA=-FLUEXTPAR%R(I)*DT/
     &      (MAX(VOLUNPAR%R(I),1.D-10)-FLUEXTPAR%R(I)*DT)
            FN%R(I)=FN%R(I)+LAMBDA*(FBORL%R(IP)-FN%R(I))
!           CORRECTION OF FLUX
!           IN THE PROOF OF MASS-CONSERVATION, FLUEXT IS MULTIPLIED
!           BY FN INSTEAD OF FBOR, TO INTERPRET THE ADDED MASS AS
!           A FLUX THIS CORRECTION IS NECESSARY
!           HERE IT IS THE FN MODIFIED ABOVE
!           EVEN IF NOT CALFLU (CHEAPER)
            FLUXF=FLUXF+(FBORL%R(IP)-FN%R(I))*FLUEXT%R(I)*DT
!           AVOIDS A DIRICHLET TREATMENT HEREAFTER AND BY DIFF3D -
!           WILL BE RESTORED AFTER DIFF3D
            LIFBOL%I(IP)=KSORT
          ENDIF
          ENDIF
        ENDDO
!
        ENDIF
!
      ENDIF
!
!=======================================================================
!
!     PUTS DIRICHLET VALUES IN FN
!     MAY HAVE NO EFFECT IF TREATMENT OF FLUXES AT THE BOUNDARIES=2
!     BECAUSE LIFBOL CHANGED ABOVE
!
      IF(NPTFR3.GT.0) THEN
        DO IPTFR=1,NPTFR3
          IF(LIFBOL%I(IPTFR).EQ.KENT .OR.
     &       LIFBOL%I(IPTFR).EQ.KENTU.OR.
     &       LIFBOL%I(IPTFR).EQ.KADH) THEN
             FN%R(NBOR3%I(IPTFR)) = FBORL%R(IPTFR)
          ENDIF
        ENDDO
      ENDIF
!
!     HERE BOTTOM AND FREE SURFACE SHOULD BE TREATED AS WELL
!
!=======================================================================
!
!     3D ADVECTION (OTHER THAN SUPG)
!
!=======================================================================
!
!     WITH DISTRIBUTIVE SCHEMES, RIGHT-HAND SIDE MUST BE
!     IN INTEGRATED FORM (BEWARE, ORIGINAL S0F THUS MODIFIED)
!
      IF(SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI.OR.SCHCF.EQ.ADV_LPO.OR.
     &   SCHCF.EQ.ADV_NSC_TF.OR.SCHCF.EQ.ADV_LPO_TF) THEN
!
        IF(S0F%TYPR.NE.'0') THEN
!
          CALL VECTOR(S0F,'=','MASVEC          ',IELM3,1.D0,
     &                S0F,S0F,S0F,S0F,S0F,S0F,MESH3D,MSK,MASKEL)
          IF(NCSIZE.GT.1) CALL PARCOM(S0F,2,MESH3D)
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY CHARACTERISTICS
!
      IF(SCHCF.EQ.ADV_CAR) THEN
!
!       THIS IS NOW DONE IN CHARAC CALLED BY PRECON
!
!       CALL CARA3D(FC%R,FN%R,SHP%R,SHZ%R,ELT%I,ETA%I,IKLE2%I,
!    &              NELEM2,NPOIN2,NPOIN3,DT,INFOR)
!       IF(NCSIZE.GT.1) CALL PARCOM(FC,2,MESH3D)
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY MURD DISTRIBUTIVE SCHEME, OPTION N
!
      ELSEIF(SCHCF.EQ.ADV_NSC) THEN
!
        CALL MURD3D(FC%R,FN%R,VOLU%R,VOLUN%R,T3_01%R,T3_01,
     &              MMURD%D%R,MMURD%X%R,MMURD%D%R,MMURD%X%R,
     &              T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &              MESH3D%W%R,IKLE3%I,MESH3D,
     &              NELEM3,NPOIN3,DT,SCHCF,LV,MSK,MASKEL%R,INFOR,
     &              CALFLU,FLUXF,FLUEXT%R,S0F,NSCE,SOURCES,FSCE,
     &              RAIN,PARAPLUIE%R,NPOIN2,
     &              TRAV3%ADR(5)%P,TRAV3%ADR(6)%P,MASKPT%R,OPTBAN,
     &              FLODEL%R,FLOPAR%R,MESH3D%GLOSEG%I,
     &              MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN)
!
!       S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
!       IF DIFF3D IS CALLED AFTER
!       CALL OS('X=C     ',X=S0F,C=0.D0)
        S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY MURD DISTRIBUTIVE SCHEME, OPTION PSI
!
      ELSEIF(SCHCF.EQ.ADV_PSI) THEN
!
         CALL MURD3D(FC%R,FN%R,VOLU%R,VOLUN%R,T3_01%R,T3_01,
     &               MMURD%D%R,MMURD%X%R,MESH3D%M%D%R,MESH3D%M%X%R,
     &               T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &               W1%R,IKLE3%I,MESH3D,
     &               NELEM3,NPOIN3,DT,SCHCF,LV,MSK,MASKEL%R,INFOR,
     &               CALFLU,FLUXF,FLUEXT%R,S0F,NSCE,SOURCES,FSCE,
     &               RAIN,PARAPLUIE%R,NPOIN2,
     &               TRAV3%ADR(5)%P,TRAV3%ADR(6)%P,MASKPT%R,OPTBAN,
     &               FLODEL%R,FLOPAR%R,MESH3D%GLOSEG%I,
     &               MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN)
!
!        S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
!        IF DIFF3D IS CALLED AFTER
!
!        CALL OS('X=C     ',X=S0F,C=0.D0)
         S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY UPWIND EXPLICIT FINITE VOLUME SCHEME
!
      ELSEIF(SCHCF.EQ.ADV_LPO) THEN
!
         CALL MURD3D(FC%R,FN%R,VOLU%R,VOLUN%R,T3_01%R,T3_01,
     &               MMURD%D%R,MMURD%X%R,MESH3D%M%D%R,MESH3D%M%X%R,
     &               T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &               W1%R,IKLE3%I,MESH3D,
     &               NELEM3,NPOIN3,DT,SCHCF,LV,MSK,MASKEL%R,INFOR,
     &               CALFLU,FLUXF,FLUEXT%R,S0F,NSCE,SOURCES,FSCE,
     &               RAIN,PARAPLUIE%R,NPOIN2,
     &               TRAV3%ADR(5)%P,TRAV3%ADR(6)%P,MASKPT%R,OPTBAN,
     &               FLODEL%R,FLOPAR%R,MESH3D%GLOSEG%I,
     &               MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN)
!
!        S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
!        IF DIFF3D IS CALLED AFTER
!
!        CALL OS('X=C     ',X=S0F,C=0.D0)
         S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY UPWIND EXPLICIT FINITE VOLUME SCHEME
!
      ELSEIF(SCHCF.EQ.ADV_LPO_TF) THEN
!
         CALL MURD3D_POS(FC%R,FN%R,VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                   T3_01%R,T3_01,MESH3D%M%X%R,
     &                   T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &                   MESH2D,MESH3D,
     &                   NELEM3,NPOIN3,DT,SCHCF,MSK,MASKEL%R,INFOR,
     &                   CALFLU,FLUXF,FLUEXT%R,S0F,NSCE,SOURCES,FSCE,
     &                   RAIN,PARAPLUIE%R,NPOIN2,OPTBAN,
     &                   FLODEL%R,FLOPAR%R,MESH3D%GLOSEG%I,
     &                   MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN,
     &                   TRAV3%ADR(6)%P,TRAV3%ADR(7)%P,
     &                   TRAV3%ADR(8)%P,
     &                   TRAV3%ADR(9)%P,2)
!
!        S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
!        IF DIFF3D IS CALLED AFTER
!
!        CALL OS('X=C     ',X=S0F,C=0.D0)
         S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY UPWIND EXPLICIT FINITE VOLUME SCHEME
!
      ELSEIF(SCHCF.EQ.ADV_NSC_TF) THEN
!
         PARA=0
         IF(NCSIZE.GT.1) PARA=MESH3D%NSEG
         CALL MURD3D_POS(FC%R,FN%R,VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                   T3_01%R,T3_01,MESH3D%M%X%R,
     &                   T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &                   MESH2D,MESH3D,
     &                   NELEM3,NPOIN3,DT,SCHCF,MSK,MASKEL%R,INFOR,
     &                   CALFLU,FLUXF,FLUEXT%R,S0F,NSCE,SOURCES,FSCE,
     &                   RAIN,PARAPLUIE%R,NPOIN2,OPTBAN,
     &                   MURD_TF%X%R(1     :MESH3D%NSEG     ),
     &                   MURD_TF%X%R(1+PARA:MESH3D%NSEG+PARA),
     &                   MESH3D%GLOSEG%I,
     &                   MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN,
     &                   TRAV3%ADR(6)%P,TRAV3%ADR(7)%P,
     &                   TRAV3%ADR(8)%P,
     &                   TRAV3%ADR(9)%P,2)
!
!        S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
!        IF DIFF3D IS CALLED AFTER
!
!        CALL OS('X=C     ',X=S0F,C=0.D0)
         S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
!     OTHER CASES (SUPG OR NO ADVECTION)
!
      ELSE
!
        CALL OS ( 'X=Y     ' , X=FC , Y=FN )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     RE-ENFORCES DIRICHLET POINTS (MAY CAUSE MASS ERRORS)
!     IN FACT NOT DONE IF LIFBOL HAS BEEN CHANGED ABOVE INTO KSORT
!     HENCE NO EFFECT WHEN YADIRFLU=.TRUE.
!
      IF(NPTFR3.GT.0) THEN
      DO IP=1,NPTFR3
        IF(LIFBOL%I(IP).EQ.KENT .OR.
     &     LIFBOL%I(IP).EQ.KENTU.OR.
     &     LIFBOL%I(IP).EQ.KADH) THEN
           I=NBOR3%I(IP)
           FC%R(I) = FBORL%R(IP)
        ENDIF
      ENDDO
      ENDIF
!
!     BOTTOM AND FREE SURFACE
!
      K = NPOIN3 - NPOIN2
      DO IP = 1,NPOIN2
        IF(LIFBOF%I(IP).EQ.KENT.OR.LIFBOF%I(IP).EQ.KADH) THEN
          FC%R(IP)   = FBORF%R(IP)
        ENDIF
        IF(LIFBOS%I(IP).EQ.KENT.OR.LIFBOS%I(IP).EQ.KADH) THEN
          FC%R(IP+K) = FBORS%R(IP)
        ENDIF
      ENDDO
!
!=======================================================================
!
!  SUPG ADVECTION AND/OR DIFFUSION
! (IN THIS CASE IT IS NECESSARY TO SOLVE A LINEAR SYSTEM)
!
!=======================================================================
!
      IF(SCHCF.EQ.ADV_SUP.OR.SCHDF.NE.0) THEN
!
        IF(SCHCF.EQ.ADV_SUP) THEN
!         IT SEEMS THAT WITH SUBITERATIONS ONLY TETASUPG=1-TETAH WORKS
!         FOR MASS-CONSERVATION. SEE ALSO DIFF3D WITH ANOTHER COMMENT
          TETASUPG=0.55D0
        ELSE
          TETASUPG=1.D0
        ENDIF
!
        IF(SCHCF.EQ.ADV_SUP.AND..NOT.VELOCITY) THEN
          CALL OS('X=CY    ',X=VOLUT,Y=VOLUN    ,C=     TETASUPG)
          CALL OS('X=X+CY  ',X=VOLUT,Y=VOLU     ,C=1.D0-TETASUPG)
          CALL OS('X=CY    ',X=ZT   ,Y=ZPROP    ,C=     TETASUPG)
          CALL OS('X=X+CY  ',X=ZT   ,Y=MESH3D%Z ,C=1.D0-TETASUPG)
!         ZT IS TEMPORARILY PUT IN MESH3D%Z
          SAVEZ=>MESH3D%Z%R
          MESH3D%Z%R=>ZT%R
          VOLUME=>VOLUT
        ELSE
          VOLUME=>VOLU
        ENDIF
!
        IF(SCHCF.EQ.ADV_CAR.OR.SCHCF.EQ.ADV_SUP) THEN
!         SOURCES HAVE TO BE TREATED
          YASCE=.TRUE.
          YARAIN=RAIN
        ELSE
!         SOURCES HAVE ALREADY BEEN TREATED BY DISTRIBUTIVE SCHEMES
          YASCE=.FALSE.
!         RAIN HAS ALREADY BEEN TREATED BY DISTRIBUTIVE SCHEMES
          YARAIN=.FALSE.
        ENDIF
!
        CALL DIFF3D(FD,FC,FN,VISCF,SIGMAF,
     &              S0F,YAS0F,S1F,YAS1F,
     &              FBORL,FBORF,FBORS,AFBORL,AFBORF,AFBORS,
     &              BFBORL,BFBORF,BFBORS,LIFBOF,LIFBOL,LIFBOS,
     &              FMIN,CLIMIN,FMAX,CLIMAX,
     &              SCHCF,SCHDF,SLVDIF,TRBAF,INFOR,NEWDIF,
     &              DT,T2_01,T2_02,T2_03,T3_01,T3_02,T3_03,T3_04,
     &              NPOIN2,NPOIN3,INCHYD,SEM3D,YASEM3D,IT1,
     &              NPTFR3,NBOR3,MASKPT,TRAV3,MESH2D,
     &              MESH3D,MTRA1,MTRA2,IELM3,MSUPG,IELM2H,IELM2V,
     &              MDIFF,MATR2H,MASKBR,SVIDE,MSK,MASKEL,H,
     &              NPLAN,OPTBAN,OPTDIF,TETADI,YAWCC,WCC,AGGLOD,
     &              VOLUME,YASCE,NSCE,FSCE,SOURCES,TETASUPG,
     &              VELOCITY,YARAIN,PLUIE%R,SIGMAG,IPBOT)
!
        IF(SCHCF.EQ.ADV_SUP.AND..NOT.VELOCITY) THEN
!         MESH3D%Z RESTORED
          MESH3D%Z%R=>SAVEZ
        ENDIF
!
      ELSE
        CALL OS ( 'X=Y     ', X=FD, Y=FC )
      ENDIF
!
!-----------------------------------------------------------------------
!
!     ADVECTIVE FLUXES AND SOURCES
!
      IF(CALFLU) THEN
!
        IF(SCHCF.EQ.ADV_CAR) THEN
          DO IP = 1,NPOIN3
            FLUXF = FLUXF + FN%R(IP)*FLUEXT%R(IP)*DT
          ENDDO
        ELSEIF(SCHCF.EQ.ADV_SUP) THEN
          DO IP = 1,NPOIN3
            FLUXF = FLUXF + FLUEXT%R(IP)*DT*
     &                      (TETASUPG*FD%R(IP)+(1.D0-TETASUPG)*FN%R(IP))
          ENDDO
        ENDIF
!
!       CHARACTERISTICS OR SUPG : FLUX DUE TO SOURCES
!       (FOR DISTRIBUTIVE SCHEMES IT IS DONE IN MURD3D)
!
        IF(NSCE.GT.0.AND.(SCHCF.EQ.ADV_CAR.OR.SCHCF.EQ.ADV_SUP)) THEN
          DO IS=1,NSCE
            IIS=IS
!           HERE IN PARALLEL SOURCES WITHOUT PARCOM
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO IP=1,NPOIN3
              IF(SOURCES%ADR(IS)%P%R(IP).GT.0.D0) THEN
                FLUXF=FLUXF-FSCE(IS)*SOURCES%ADR(IIS)%P%R(IP)*DT
              ELSE
!                           FN FOR CHARACTERISTICS ?
                FLUXF=FLUXF-FD%R(IP)*SOURCES%ADR(IIS)%P%R(IP)*DT
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     A POSTERIORI CORRECTION OF SUPG RESULTS
!
      IF(SCHCF.EQ.ADV_SUP.AND.YADIRFLU) THEN
!
!       CORRECTED VALUE AND CORRESPONDING FLUX CORRECTION
!
        IF(NPTFR3.GT.0) THEN
        DO IP=1,NPTFR3
          IF(DIRFLU(NUMLIQ(IP)).EQ.2) THEN
            IF(LIFBOL%I(IP+NPTFR3).EQ.KENT .OR.
     &         LIFBOL%I(IP+NPTFR3).EQ.KENTU.OR.
     &         LIFBOL%I(IP+NPTFR3).EQ.KADH     ) THEN
!              ONLY ENTRANCES
               I=NBOR3%I(IP)
               IF(FLUEXTPAR%R(I).LT.0.D0) THEN
                 STOFD=FD%R(I)
                 LAMBDA=-FLUEXTPAR%R(I)*TETASUPG*DT/
     &                   MAX(VOLUPAR%R(I),1.D-10)
                 FD%R(I)=STOFD+LAMBDA*(FN%R(I)-STOFD)
!                CORRECTION OF FLUX
!                IF(CALFLU) THEN
!                  A POSTERIORI ADDED MASS DUE TO CORRECTION
                   FLUXF=FLUXF-VOLU%R(I)*(FD%R(I)-STOFD)
!                ENDIF
               ENDIF
            ENDIF
          ENDIF
        ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(CALFLU) THEN
!       NOW RETURNS TO REAL FLUXES, NOT FLUXES*DT
        FLUXF = FLUXF / DT
!       PARALLEL MODE
        IF(NCSIZE.GT.1) FLUXF = P_DSUM(FLUXF)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     RESTORES ORIGINAL LIFBOL FROM SECOND DIMENSION
!
      DO IP=1,NPTFR3
        LIFBOL%I(IP)=LIFBOL%I(IP+NPTFR3)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END