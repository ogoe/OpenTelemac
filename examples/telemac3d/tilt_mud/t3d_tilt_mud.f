!                    ********************
                     SUBROUTINE TELEMAC3D
!                    ********************
!
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  JMH
!+        26/08/2009
!+
!+   VOLU3D INSTEAD OF VOLU AND VOLUN IN THE CALLS TO CVDF3D
!
!history  JMH
!+        16/02/2010
!+
!+   ZCHAR INSTEAD OF ZSTAR IN CALL TO DERI3D
!
!history  J-M HERVOUET (LNHE)
!+        05/05/2010
!+        V6P0   
!+   K-OMEGA MODEL BY HOLGER WEILBEER (ISEB/UHA)
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
!+        02/08/2011
!+        V6P1   
!+   CALL MITTIT(18,AT,LT) changed into CALL MITTIT(19,AT,LT)
!+   CALL MITTIT(19,AT,LT) changed into CALL MITTIT(20,AT,LT)
!+   2 fractional steps were not correctly labelled in the listing
!
!history  J-M HERVOUET (LNHE)
!+        12/08/2011
!+        V6P2   
!+   Calls to CHECK and BIL3D changed
!
!history  J-M HERVOUET (LNHE)
!+        02/04/2012
!+        V6P2   
!+   Clean restart implemented.
!
!history  J-M HERVOUET (LNHE)
!+        01/06/2012
!+        V6P2   
!+   Call to vector before call to Tel4del corrected (GRAZCO)
!+   Initialisation of TAN after call to condim.
!
!history  J-M HERVOUET (LNHE)
!+        18/129/2012
!+        V6P3   
!+   Call to IFAB3DT added, arguments of cstkep removed.
!
!history  J-M HERVOUET (LNHE)
!+        25/01/2013
!+        V6P3   
!+   TAN renamed TRN, copy of TRN on TA moved from after CONDIM to
!+   after BIEF_SUITE, FLULIM set to 1 before first call to PREADV 
!
!history  J-M HERVOUET (LNHE)
!+        11/03/2013
!+        V6P3   
!+   Call to METEO modified. Stop if variables not found for a 2D
!+   continuation.
!
!history  R. KOPMANN (EDF R&D, LNHE)
!+        16/04/2013
!+        V6P3
!+   Adding the file format in calls to FIND_IN_SEL.
!
!history  J-M HERVOUET (LNHE)
!+        18/03/2013
!+        V6P3   
!+   Dealing with the newly created FILE FOR 2D CONTINUATION.
!
!history  J-M HERVOUET (LNHE)
!+        25/04/2013
!+        V6P3   
!+   AKN and EPN initialised in case of computation continued, for the
!+   first call to PREADV.
!+   Mesh better updated in case of coupling with Sisyphe.
!
!history  J-M HERVOUET (LNHE)
!+        20/09/2013
!+        V6P3   
!+   CALL PLANE_BOTTOM added at the beginning of time loop (otherwise
!+   when calling kepcl3 IPBOT is done with ZPROP at the first iteration
!+   and with Z for the others, while ZPROP is always sent as argument.
!+   This could trigger unexpected divisions by 0.
!
!history  J-M HERVOUET (LNHE)
!+        15/11/2013
!+        V6P3   
!+   After second call to bief_suite, checking that Z has been found,
!+   otherwise stop
!
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  J-M HERVOUET (LNHE)
!+        14/03/2014
!+        V7P0  
!+   CALL BIL3D put out of the IF(SEDI) test. Address of depth-averaged
!+   tracers from 38 to 37+NTRAC in ALIRE2D.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/03/2014
!+        V7P0
!+   Boundary segments have now their own numbering, independent of
!+   boundary points numbering. Differents calls changed accordingly.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        02/05/2014
!+        V7P0
!+   Argument ZR added to FONVAS. HDEP updated differently after calling
!+   Sisyphe, to avoid truncation errors that would give HDEP<0.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE OILSPILL
      USE INTERFACE_TELEMAC3D
      USE INTERFACE_TELEMAC2D
      USE INTERFACE_SISYPHE, ONLY: SISYPHE
      USE INTERFACE_TOMAWAC, ONLY: WAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
! DECLARATIONS
!-----------------------------------------------------------------------
!
      INTEGER  P_IMAX
      EXTERNAL P_IMAX  
!
!-----------------------------------------------------------------------
! DECLARES LOCAL VARIABLES FOR TELEMAC3D
!-----------------------------------------------------------------------
!
      INTEGER LT,DATE(3),TIME(3)
      INTEGER ITRAC, NVARCL,ISOUSI
      INTEGER SCHDVI_HOR,SCHDVI_VER,SCHCVI_HOR,SCHCVI_VER
      INTEGER, PARAMETER :: NSOR = 26 ! HERE MAXVAR FOR 2D
      INTEGER ALIRE2D(MAXVAR),TROUVE(MAXVAR+10),ALIRE3D(MAXVAR)
      INTEGER IBID,I,K,I3D,IP
!
      DOUBLE PRECISION TETADIVER
      DOUBLE PRECISION UMIN,  UMAX,  SIGMAU, VMIN,  VMAX, SIGMAV
      DOUBLE PRECISION WMIN,  WMAX,  SIGMAW
      DOUBLE PRECISION TAMIN, TAMAX, SIGMTA,TETATRA
!
      DOUBLE PRECISION HIST(1)
      DATA HIST /9999.D0/
!
      LOGICAL CLUMIN, CLUMAX, CLVMIN, CLVMAX, CLWMIN, CLWMAX
      LOGICAL CTAMIN, CTAMAX, YASEM3D,YAS0U,YAS1U
      LOGICAL CLKMIN, CLKMAX, CLEMIN, CLEMAX
      LOGICAL TRAC,YAWCHU,NEWDIF,LBID,BC,CHARR,SUSP
!
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC3D               '
      CHARACTER(LEN=16) FORMUL
!
      INTRINSIC MOD
!
      TYPE(SLVCFG) :: SLVD
!
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
      DOUBLE PRECISION ERROR
!
!=======================================================================
!
!  VARIABLES TO BE READ WHEN SUITE IS CALLED:
!  0 : DISCARD    1 : READ  (SAME NUMBERING AS IN NOMVAR)
!
!                  U V   H   ZF
      DATA ALIRE2D/1,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
!
!     IN 3D FILES
!                                                        U V
!                                            U V W       C C          
!                                            C C C     D O O      
!                                            O O O   D H N N     
!                                        D   N N N   M H V V U V W 
!                  Z U V W       K E     P   V V V   1 N C C D D D  
      DATA ALIRE3D/1,1,1,1,0,0,0,1,1,0,0,1,0,1,1,1,0,1,1,1,1,1,1,1,0,0,
     &             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
!
!     READS TRACERS IN PREVIOUS FILES
!
      IF(NTRAC.GT.0) THEN
        DO I=ADR_TRAC,ADR_TRAC+NTRAC-1
          ALIRE3D(I)=1
        ENDDO
        DO I=38,37+NTRAC
          ALIRE2D(I)=1
        ENDDO
!
!       THIS IS ESOMT...
!       IF(SEDI) ALIRE2D(37)=1
!
      ENDIF
!
!=======================================================================
! FOR DROGUES (CALLS TO FLOT3D WILL INCREASE OR DECREASE NFLOT)
!=======================================================================
!
      NFLOT=0 
!
!=======================================================================
! FOR COMPUTING FLUXES OF ADVECTED VARIABLES
!=======================================================================
!
!     NO FLUX COMPUTED FOR U,V,W,K,EPSILON
      DO I=1,5
        CALCFLU(I)=.FALSE.
      ENDDO
!     DEPENDING ON BILMAS FOR TRACERS
      IF(NTRAC.GT.0) THEN
        DO I=6,5+NTRAC
          CALCFLU(I)=BILMAS
        ENDDO
      ENDIF
!
!=======================================================================
! FOR TAKING INTO ACCOUNT RAIN IN ADVECTION OF VARIOUS VARIABLES
!=======================================================================
!
!     NO RAIN FOR U,V,W,K,EPSILON
      DO I=1,5
        CALCRAIN(I)=.FALSE.
      ENDDO
!     DEPENDING OF RAIN FOR TRACERS
      IF(NTRAC.GT.0) THEN
        DO I=6,5+NTRAC
          CALCRAIN(I)=RAIN
        ENDDO
      ENDIF
!
!=======================================================================
! INITIALISATION: READS, PREPARES AND CHECKS
!=======================================================================
!
      LT     = 0       ! INITIALISES TIMESTEP
!     INITIALISES NUMBER OF SUB-ITERATIONS, LOOK IN PREADV
      ISOUSI = 0
      NVARCL = 0
      IF(NTRAC.GT.0) THEN
        TRAC=.TRUE.
      ELSE
        TRAC=.FALSE.
      ENDIF
!
!     DATE AND TIME (NOW TAKEN FROM KEYWORDS MARTIM AND MARDAT)
!
      DATE(1) = MARDAT(1)
      DATE(2) = MARDAT(2)
      DATE(3) = MARDAT(3)
      TIME(1) = MARTIM(1)
      TIME(2) = MARTIM(2)
      TIME(3) = MARTIM(3)
!
      INFOGR = LISTIN
!
!-----------------------------------------------------------------------
!
! 2D BOUNDARY CONDITIONS:
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECLIM'
      CALL LECLIM
     & (LIHBOR%I,LIUBOL%I,LIVBOL%I,IT4%I,HBOR%R,UBOR2D%R,VBOR2D%R,
     &  T2_01%R,T2_02%R,T2_03%R,T2_04%R,NPTFR2,3,.FALSE.,
     &  T3D_FILES(T3DCLI)%LU,
     &  KENT,KENTU,KSORT,KADH,KLOG,KINC,NUMLIQ%I,MESH2D)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LECLIM'
!
! MESH ORGANISATION - 2D LEVEL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INBIEF POUR MESH2D'
      CALL INBIEF(LIHBOR%I,KLOG,IT1,IT2,IT3,
     &            LVMAC,IELMX,LATIT,SPHERI,MESH2D,
     &            T2_01,T2_02,OPTASS2D,PRODUC,EQUA)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INBIEF'
!
! CORRECTS THE NORMAL VECTORS AT THE POINTS
! WHERE LIQUID AND SOLID BOUNDARIES MEET
!
      CALL CORNOR(MESH2D%XNEBOR%R,MESH2D%YNEBOR%R,
     &            MESH2D%XSGBOR%R,MESH2D%YSGBOR%R,
     &            NPTFR2,KLOG,LIHBOR%I,
     &            T2_01,T2_02,MESH2D,MESH2D%IKLBOR%I,
     &            MESH2D%NELEB,MESH2D%NELEBX)
!
! 3D BOUNDARY CONDITIONS (SO FAR SAME FILE AS 2D)
! T2_02 IS AUBOR IN T2D, COULD BE KEPT
! THIS TIME BOUNDARY COLOURS ARE READ
!
      CALL LECLIM
     & (LIHBOR%I,LIUBOL%I,LIVBOL%I,LITABL%ADR(1)%P%I,
     &  HBOR%R,UBORL%R,VBORL%R,TABORL%ADR(1)%P%R,AUBORL%R,
     &  ATABOL%ADR(1)%P%R,BTABOL%ADR(1)%P%R,
     &  NPTFR2,3,TRAC,T3D_FILES(T3DCLI)%LU,KENT,KENTU,
     &  KSORT,KADH,KLOG,KINC,
     &  NUMLIQ%I,MESH3D,BOUNDARY_COLOUR%I)
!
! MESH ORGANISATION - 3D LEVEL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INBIEF POUR MESH3D'
      CALL INBIEF(LIHBOR%I,KLOG,IT1,IT2,IT3,
     &            LVMAC,IELM3,LATIT,SPHERI,MESH3D,
     &            T3_01,T3_02,OPTASS,PRODUC,EQUA,MESH2D)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INBIEF'
!
! INITIALISES 3D BOUNDARY CONDITION ATTRIBUTES FOR BOUNDARY NODES
! DUPLICATES 2D CONDITIONS ON THE VERTICAL
!
      CALL LIMI3D
!
! COMPLETES IFABOR IN 3D
!
      IF(IELM3.EQ.41) THEN
        CALL IFAB3D
     &   (MESH3D%IFABOR%I,LIUBOF%I,LIUBOL%I,LIUBOS%I,
     &    MESH2D%KP1BOR%I,MESH2D%NELBOR%I,
     &    MESH2D%NULONE%I,IKLE2%I,
     &    NELEM2,NPOIN2,NPTFR2,NPLAN,NETAGE,
     &    KLOG,TRANSF)
      ELSEIF(IELM3.EQ.51) THEN
        CALL IFAB3DT
     &   (MESH3D%IFABOR%I, 
     &    MESH2D%IFABOR%I,LIUBOF%I,LIUBOL%I,LIUBOS%I,
     &    MESH2D%KP1BOR%I,MESH2D%NELBOR%I,
     &    MESH2D%NULONE%I,IKLE2%I,IKLE3%I,
     &    NELEM2,MESH3D%NELMAX,NPOIN2,NPTFR2,NPLAN,NETAGE,
     &    KLOG,TRANSF)
      ENDIF
!
! CONTROLS MESH
!
      CALL CHECK(IKLE2%I,NBOR2%I,MESH2D%NELBOR%I,MESH3D%IKLBOR%I,
     &           MESH3D%NELEB,MESH3D%NELEBX,
     &           IKLE3%I,MESH3D%NELBOR%I,MESH3D%NULONE%I,
     &           MESH3D%NULONE%DIM1,MESH3D%NULONE%DIM2,
     &           NBOR3%I,NELEM2,NPOIN2,NPTFR2,NELEM3,NPTFR3,LISTIN)
!
! LOOKS FOR THE BOTTOM AND BOTTOM FRICTION VARIABLES IN THE GEOMETRY FILE
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FONSTR'
      CALL FONSTR(T2_01,ZF,T2_02,RUGOF,T3D_FILES(T3DGEO)%LU,
     &            T3D_FILES(T3DGEO)%FMT,
     &            T3D_FILES(T3DFON)%LU,T3D_FILES(T3DFON)%NAME,
     &            MESH2D,RUGOF0,LISTIN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FONSTR'
      I=11
      IF(RUGOF%ELM.NE.I) CALL CHGDIS(RUGOF,I,RUGOF%ELM,MESH2D)
!
! INITIALISES PRIVATE VECTOR BLOCK
!
      IF(NPRIV.GT.0) CALL OS('X=0     ',X=PRIVE)
!
!-----------------------------------------------------------------------
! CORRECTS THE BOTTOM
!
!  - SMOOTHES ACCORDING TO THE LISFON VALUE
!  - CHANGES THE BOTTOM TOPOGRAPHY (FORTRAN)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING T3D_CORFON'
      CALL T3D_CORFON(ZF,T2_01,T2_02,ZF%R,T2_01%R,T2_02%R,
     &                X,Y,PRIVE,NPOIN2,LISFON,.FALSE.,MASKEL,
     &                MATR2H,MESH2D,SVIDE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETURN FROM T3D_CORFON'
!
! FINELY ANALYSES THE BATHYMETRY
! IN T2D CALLED IF (OPTBAN == 2)
!
      IF(MSK) CALL TOPOGR(ZF%R,T2_01%R,ZFE%R,IKLE2%I,MESH2D%IFABOR%I,
     &  MESH2D%NBOR%I, MESH2D%NELBOR%I, MESH2D%NULONE%I,
     &  IT1%I, IT2%I, IT3%I, NELEM2, NPTFR2, NPOIN2, MXPTVS2)
!
!=======================================================================
! VARIOUS INITIALISATIONS
!=======================================================================
!
!     COUNTS THE LIQUID BOUNDARIES
!
      IF(NCSIZE.GT.1) THEN
        NFRLIQ=0
        DO I=1,NPTFR2
          NFRLIQ=MAX(NFRLIQ,NUMLIQ%I(I))
        ENDDO
        NFRLIQ=P_IMAX(NFRLIQ)
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) WRITE(LU,*) 'NOMBRE DE FRONTIERES LIQUIDES :',
     &                            NFRLIQ
        IF(LNG.EQ.2) WRITE(LU,*) 'NUMBER OF LIQUID BOUNDARIES:',NFRLIQ
      ELSE
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FRONT2'
        CALL FRONT2(NFRLIQ,NFRSOL,DEBLIQ,FINLIQ,DEBSOL,FINSOL,
     &              LIHBOR%I,LIUBOL%I,X,Y,NBOR2%I,MESH2D%KP1BOR%I,
     &              IT1%I,NPOIN2,NPTFR2,KLOG,LISTIN,NUMLIQ%I,MAXFRO)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FRONT2'
      ENDIF
!
!     3D EXTENSION OF NUMLIQ
!
      CALL NUMLIQ_3D(NUMLIQ%I,NUMLIQ_ELM,NPLAN,MESH2D%NPTFR,
     &               MESH2D%IKLBOR%I,MESH2D%NELEB,MESH2D%NELEBX)
!
!     SAVING AND MODIFYING BOUNDARY CONDITIONS FOR THOMPSON METHOD
!
      IF(THOMFR.AND.NFRLIQ.GT.0) THEN
        CALL THOMPS_BC(1)
        CALL THOMPS_BC(2)
      ENDIF
!
!=======================================================================
!
!     READS THE STAGE-DISCHARGE CURVES FILE
!
      IF(T3D_FILES(T3DPAR)%NAME(1:1).NE.' ') THEN
        CALL T3D_READ_FIC_CURVES(T3D_FILES(T3DPAR)%LU,NFRLIQ,
     &                           STA_DIS_CURVES,PTS_CURVES)
      ENDIF
!
!     SETS TURBULENCE CONSTANTS (ALL MODELS)
!
      CALL CSTKEP
!
!-----------------------------------------------------------------------
! READS INITIAL CONDITIONS FROM A PREVIOUS 3D COMPUTATION FILE
! OR SETS THEM IN FORTRAN
!
      AKEP = .TRUE.
      AKOM = .TRUE.
!
!     STARTS FROM A 2D FILE (U,V AND H ARE READ TO BE USED IN CONDIM)
!                            AT IS ALSO INITIALISED
!     IF A COMPUTATION CONTINUED IS ALSO ASKED, WHAT IS READ HERE MAY
!     NOT BE USED, BUT AS CONDIM IS ALWAYS CALLED, WILL AVOID A CRASH
!     IN CONDIM.
!
      IF(SUIT2) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SUITE AVEC UN FICHIER 2D'
          CALL BIEF_SUITE(VARSOR,VARCL,IBID,
     &                    T3D_FILES(T3DS2D)%LU,T3D_FILES(T3DS2D)%FMT,
     &                    HIST,0,NPOIN2,AT,TEXTPR,VARCLA,
     &                    NVARCL,TROUVE,ALIRE2D,LISTIN,.TRUE.,MAXVAR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SUITE'
        IF(TROUVE(1).NE.1.OR.TROUVE(2).NE.1.OR.
     &     TROUVE(4).NE.1) THEN
          WRITE(LU,*)
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'TELEMAC3D : VARIABLES U2D, V2D OU H ABSENTES'
            WRITE(LU,*) '            SUITE 2D IMPOSSIBLE'
            WRITE(LU,*) '            EN CAS DE PROBLEME DE LANGUE'
            WRITE(LU,*) '            MODIFIER NOMVAR_2D_IN_3D'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'TELEMAC3D: VARIABLES U2D, V2D OR H NOT FOUND'
            WRITE(LU,*) '           2D CONTINUATION IMPOSSIBLE'
            WRITE(LU,*) '           IF IT IS A LANGUAGE PROBLEM'
            WRITE(LU,*) '           YOU CAN MODIFY NOMVAR_2D_IN_3D'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!     COPIES THE BOTTOM TOPOGRAPHY INTO Z (= MESH3D%Z%R)
!    (IF IT IS A CONTINUATION, Z WILL BE ALSO FOUND
!     IN THE PREVIOUS RESULTS FILE. ANYWAY THE COPY IS USEFUL HERE
!     TO AVOID A CRASH IN CONDIM)
!
      CALL OV('X=Y     ',Z(1:NPOIN2),ZF%R,ZF%R,0.D0,NPOIN2)
!
!     NOW CALLS CONDIM EVEN IF A COMPUTATION IS CONTINUED
!    (DONE TO RETRIEVE ZSTAR)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CONDIM'
      CALL CONDIM
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CONDIM'
!
!     COMPUTES TRANSF AND ZCHAR
!
      CALL TRANSF_ZCHAR(TRANSF,ZCHAR,ZSTAR,TRANSF_PLANE,NPLAN)
!
!     CLIPS POSSIBLE NEGATIVE DEPTHS SET BY USER
!
      CALL OS('X=+(Y,C)',X=H,Y=H,C=0.D0)
!
!     IF COMPUTATION CONTINUED, RETRIEVES SOME VARIABLES + Z + DEPTH
!
      IF(.NOT.DEBU) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SUITE'
        CALL BIEF_SUITE(VARSO3,VARCL,START_RECORD,
     &                  T3D_FILES(T3DPRE)%LU,T3D_FILES(T3DPRE)%FMT,
     &                  HIST,0,NPOIN3,AT,TEXTP3,VARCLA,NVARCL,
     &                  TROUVE,ALIRE3D,LISTIN,
     &                  START_RECORD.EQ.0,MAXVAR,NPLAN)
        IF(TROUVE(1).NE.1) THEN
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'COORDONNEE Z MANQUANTE DANS LE'
            WRITE(LU,*) 'FICHIER DU CALCUL PRECEDENT'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'Z COORDINATE MISSING IN THE'
            WRITE(LU,*) 'PREVIOUS COMPUTATION FILE'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SUITE'
!
        DO K=1,NPOIN2
          H%R(K)=Z(K+NPOIN2*(NPLAN-1))-Z(K)
          ZF%R(K)=Z(K)
        ENDDO
!
!       SEE VARSO3 IN POINT FOR INDICES 8 AND 9 (K AND EPSILON)
        IF(TROUVE(8).EQ.1.AND.TROUVE(9).EQ.1) THEN
          AKEP=.FALSE.
          AKOM=.FALSE.
!         WILL BE USED BY FIRST CALL TO PREADV
          CALL OS('X=Y     ',X=AKN,Y=AK)
          CALL OS('X=Y     ',X=EPN,Y=EP)
        ENDIF
!
        IF(TROUVE(19).EQ.1) THEN
!         RETRIEVING DH AND HN
          DO I=1,NPOIN2
            DH%R(I)=DHHN%R(I)
            HN%R(I)=DHHN%R(I+NPOIN2)
          ENDDO
        ELSE
          DO I=1,NPOIN2
            HN%R(I)=H%R(I)
          ENDDO          
        ENDIF
!
      ENDIF 
!
!     TIME OPTIONALLY RESET TO ZERO
!
      IF(RAZTIM) THEN
        AT=0.D0
        IF(LNG.EQ.1) WRITE(LU,*) 'TEMPS ECOULE REMIS A ZERO'
        IF(LNG.EQ.2) WRITE(LU,*) 'ELAPSED TIME RESET TO ZERO'
      ENDIF
!
!     ONLY TA IS INITIALISED IN CONDIM OR BIEF_SUITE
!
      IF(NTRAC.GT.0) CALL OS ('X=Y     ', X=TRN, Y=TA)
!
!     INITIALISES SEDIMENT PROPERTIES
!
      IF(SEDI) THEN
!        
        IF(T3D_FILES(T3DSUS)%NAME(1:1).EQ.' ') THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CONDIS'
          CALL CONDIS(IVIDE%R,EPAI,TREST,CONC,TEMP%R,HDEP%R,
     &                ZR%R,ZF%R,X,Y,NPOIN2,NPOIN3,NPF%I,
     &                NCOUCH,TASSE,ITASS, RHOS,XKV,CFDEP,
     &                ESOMT, TOCE, SEDCO,CONC_LAYER, 
     &                TOCE_LAYER, ES_LAYER)
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CONDIS'
! CV...        ELSE
! Will have to be rewritten
!
!          CALL SUISED(IVIDE%R,EPAI,HDEP%R,CONC,TEMP%R,FLUER%R,
!     &                PDEPO%R,ZR%R,ZF%R,NPF%I,
!     &                NPOIN2,NPOIN3,NPFMAX,NCOUCH,TASSE,GIBSON,
!     &                T3D_FILES(T3DSUS)%LU,BISUIS)
        ENDIF
! ...CV
!###> DMK/TBE MOD -
! COULD REPLACE SUISED
        IF (READ_TOCE) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MUDSTRESS3D'
          CALL MUDSTRESS3D(T3D_FILES(T3DGEO)%LU,T3D_FILES(T3DGEO)%FMT,
     &                 LAYTOCE,NCOUCH,MESH2D,.TRUE.)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MUDSTRESS3D'
        ENDIF
!
!###< DMK/TBE MOD END 
!       SO FAR CONSTANT MEAN DIAMETER=D50
        CALL OS('X=C     ',X=DMOY,C=D50)
      ENDIF
!
! CLIPS H AND COMPUTES Z, HPROP AND ZPROP
! NOTE : HMIN = -1000.0 IN DICTIONARY BUT HMIN IS AT LEAST 0.0
!        IF OPTBAN=2
!
      IF(OPTBAN.EQ.2) THEN
        CALL CLIP (H, HMIN, .TRUE., 1.D6, .FALSE., 0)
      ENDIF
!
      CALL CALCOT(Z,H%R)
!     BOTTOM OF ZPROP UPDATED
      CALL OV('X=Y     ',ZPROP%R(1:NPOIN2),Z3%R(1:NPOIN2),
     &        Z3%R(1:NPOIN2),0.D0,NPOIN2)
!
!-----------------------------------------------------------------------
! MASKING:
!
      IF(MSK) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASK3D'
        CALL MASK3D(MESH3D%IFABOR%I,MASKEL%R,MASKPT,MASKBR%R,
     &              X2%R,Y2%R,ZF%R,ZFE%R,H%R,HMIN,AT,LT,IT1%I,
     &              MESH3D%NELBOR%I,NELMAX2,NELEM2,NPOIN2,NPTFR2,
     &              NPLAN,NETAGE,IELM3,MESH2D)
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASK3D'
      ENDIF
!
! MESH FOR PROPAGATION (IF NOT FOUND IN PREVIOUS RESULTS FILE)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MESH_PROP'
      CALL MESH_PROP(HPROP,HN,H,PROLIN,HAULIN,TETAH,NSOUSI,ZPROP,
     &               IPBOT,NPOIN2,NPLAN,OPTBAN,SIGMAG,OPT_HNEG,
     &               MDIFF,MESH3D,VOLU3D,VOLU3DPAR,
     &               UNSV3D,MSK,MASKEL,IELM3)   
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MESH_PROP'
!
! INITIALISES THE MEAN VELOCITY IN 2D
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VERMOY'
      CALL VERMOY(U2D%R,V2D%R,U%R,V%R,2,Z,
     &            T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VERMOY'
!
!-----------------------------------------------------------------------
! HARMONISES  BOUNDARY CONDITIONS
! INITIALISES BOUNDARY CONDITIONS FOR TELEMAC
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LICHEK'
      CALL LICHEK(LIMPRO%I,NPTFR2,
     &            MESH2D%IKLBOR%I,MESH2D%NELEB,MESH2D%NELEBX)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LICHEK'
!
!-----------------------------------------------------------------------
! INITIALISES THE VOLUMES ASSOCIATED WITH THE NODES
!
      CALL VECTOR(VOLU, '=', 'MASBAS          ',IELM3,1.D0-AGGLOH,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
      IF(AGGLOH.GT.1.D-6) THEN
        CALL VECTOR(VOLU, '+', 'MASBAS2         ',IELM3,AGGLOH,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
      ENDIF
!
!     IF NEW COMPUTATION OR DHHN NOT IN PREVIOUS RESULT FILE
!
      IF(DEBU.OR.(.NOT.DEBU.AND.TROUVE(19).NE.1)) THEN
        CALL OS('X=Y     ',X=VOLUN,Y=VOLU)
      ELSE
!       COMPUTING THE CORRECT VOLUN (ASSUMING AGGLOH HAS NOT CHANGED..)
        CALL CALCOT(Z,HN%R)
        CALL VECTOR(VOLUN, '=', 'MASBAS          ',IELM3,1.D0-AGGLOH,
     &    SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
        IF(AGGLOH.GT.1.D-6) THEN
          CALL VECTOR(VOLUN, '+', 'MASBAS2         ',IELM3,AGGLOH,
     &    SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
        ENDIF
!       RESTORING Z DONE WITH H
        CALL CALCOT(Z,H%R)
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        CALL OS('X=Y     ',X=VOLUPAR,Y=VOLU)
        CALL PARCOM(VOLUPAR,2,MESH3D)
        CALL OS('X=Y     ',X=VOLUNPAR,Y=VOLUPAR)
      ENDIF
!
!     IN 2D
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASBAS2D'
      CALL MASBAS2D(VOLU2D,V2DPAR,UNSV2D,
     &              IELM2H,MESH2D,MSK,MASKEL,T2_01,SVIDE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MASBAS2D'
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! FREE SURFACE AND BOTTOM GRADIENTS
! INITIALISES DSSUDT = 0
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE GRAD2D'
      CALL GRAD2D(GRADZF%ADR(1)%P,GRADZF%ADR(2)%P,ZPROP,NPLAN,SVIDE,
     &            UNSV2D,T2_02,T2_03,T2_04,IELM2H,MESH2D,MSK,MASKEL)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE GRAD2D'
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FSGRAD'
      CALL FSGRAD(GRADZS,ZFLATS,Z(NPOIN3-NPOIN2+1:NPOIN3),
     &            ZF,IELM2H,MESH2D,MSK,MASKEL,
     &            UNSV2D,T2_01,NPOIN2,OPTBAN,SVIDE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FSGRAD'
!
      CALL OS('X=C     ',X=DSSUDT,C=0.D0)
!
! INITIALISES THE METEOROLOGICAL VARIABLES
!
      IF (VENT.OR.ATMOS) CALL METEO
     &   (PATMOS%R,WIND%ADR(1)%P%R,WIND%ADR(2)%P%R,FUAIR,FVAIR,
     &    X2%R,Y2%R,AT,LT,NPOIN2,VENT,ATMOS,H%R,T2_01%R,
     &    GRAV,RHO0,0.D0,PRIVE,T3DFO1,T3D_FILES,LISTIN)
!
!-----------------------------------------------------------------------
! INITIALISES K AND EPSILON
! IF AKEP = .FALSE. K AND EPSILON HAVE BEEN GIVEN IN LECSUI OR CONDIM
!
      IF(ITURBV.EQ.3.AND.AKEP) THEN
        CALL KEPINI(AK%R,EP%R,U%R,V%R,Z,
     &             ZF%R,NPOIN2,NPLAN,DNUVIH,DNUVIV,KARMAN,CMU,KMIN,EMIN)
!       WILL BE USED BY FIRST CALL TO PREADV
        CALL OS('X=Y     ',X=AKN,Y=AK)
        CALL OS('X=Y     ',X=EPN,Y=EP)
      ENDIF
!
      IF(ITURBV.EQ.7.AND.AKOM) THEN
        CALL OS('X=C     ',X=AK,C=KMIN)
        CALL OS('X=C     ',X=EP,C=EMIN)
        CALL OS('X=0     ',X=ROTAT)
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTES (DELTA RHO)/RHO FOR THE INITIAL OUTPUT
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DRSURR'
      CALL DRSURR(DELTAR,TA,BETAC,T0AC,T3_01,RHO0,RHOS,DENLAW,SEDI,
     &            NTRAC,IND_T,IND_S)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DRSURR'
!
!-----------------------------------------------------------------------
!
! INITIALISES U* FOR OUTPUT OF INITIAL CONDITIONS AND SISYPHE
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE COEFRO'
      CALL COEFRO(CF,H,U2D,V2D,KARMAN,KFROT,RUGOF,GRAV,MESH2D,T2_01)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE COEFRO'
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TFOND'
      CALL TFOND(AUBORF%R,CF%R,U2D%R,V2D%R,U%R,V%R,W%R,KARMAN,
     &           LISRUF,DNUVIV,Z,NPOIN2,KFROT,RUGOF%R,UETCAR%R,
     &           NONHYD,OPTBAN,HN%R,GRAV,IPBOT%I,NPLAN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TFOND'
!
!-----------------------------------------------------------------------
!
! COMPUTES THE VISCOSITIES VISCVI AND VISCTA
!
      IF(ITURBH.EQ.1.OR.ITURBV.EQ.1) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCOS'
        CALL VISCOS(VISCVI,VISCTA,DNUTAV,DNUTAH,
     &              DNUVIV,DNUVIH,NTRAC,ITURBH,ITURBV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCOS'
!
      ENDIF
!
      IF(ITURBV.EQ.2) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCLM'
        CALL VISCLM(VISCVI,VISCTA,RI,U,V,DELTAR,X3,Y3,Z3,H,
     &              T3_01, T3_02, T3_03, T3_04, T3_05, T3_06, T3_07,
     &              SVIDE, MESH3D, IELM3, GRAV, NPLAN,
     &              NPOIN3, NPOIN2, NTRAC, MSK, MASKEL,
     &              TA,MIXING,DAMPING,IND_T,DNUVIV,DNUTAV,KARMAN,
     &              PRANDTL,UETCAR,KFROT,RUGOF,ZF,LINLOG,IPBOT%I)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCLM'
!
      ENDIF
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCKE'
!       FOR FIRST PRINTOUT (RI ONLY DONE IN SOUKEP LATER)
        CALL OS('X=0     ',X=RI)
        CALL VISCKE(VISCVI,VISCTA,AK,EP,NTRAC,CMU,
     &              DNUVIH,DNUVIV,DNUTAH,DNUTAV,KMIN,EMIN,
     &              ITURBH,ITURBV,PRANDTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCKE'
!
      ENDIF
!
      IF(ITURBH.EQ.4) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISSMA'
        CALL VISSMA(VISCVI,VISCTA,DNUTAH,DNUVIH,DNUVIV,DNUTAV,
     &              U,V,W,T3_01,T3_02,T3_03,T3_04,T3_05,T3_06,
     &              SVIDE,MESH3D,
     &              IELM3,NTRAC,MSK,MASKEL,ITURBV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISSMA'
!
      ENDIF
!
      IF(ITURBV.EQ.7) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCKO'
        CALL VISCKO(VISCVI,VISCTA,ROTAT,AK,EP,NTRAC,CMU,
     &              DNUVIH,DNUVIV,DNUTAH,DNUTAV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCKO'
!
      ENDIF
!
      IF(OPTBAN.EQ.1) THEN
!
        CALL VISCLIP(VISCVI,VISCTA,H,NPLAN,NPOIN3,NPOIN2,NTRAC,HLIM)
!
      ENDIF
!
!------------------------------------
! PREPARES THE 3D OUTPUT FILE :
!------------------------------------
!
!     OUTPUT FOR THE INITIAL CONDITIONS
!
      IF (INFOGR) CALL MITTIT(1,AT,LT)
!
!     PREPARES THE 2D AND 3D OUTPUT FOR INITIAL CONDITIONS
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRERES_TELEMAC3D'
      CALL PRERES_TELEMAC3D(LT)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRERES_TELEMAC3D'
!
!-----------------------------------------------------------------------
!
!     COUPLING WITH DELWAQ
!
      IF(INCLUS(COUPLING,'DELWAQ')) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TEL4DEL'
        CALL TEL4DEL(NPOIN3,NPOIN2,NELEM3,MESH2D%NSEG,IKLE2%I,
     &  MESH2D%ELTSEG%I,MESH2D%GLOSEG%I,MESH2D%ORISEG%I,
     &  MESH2D%GLOSEG%DIM1,X,Y,MESH3D%NPTFR,LIHBOR%I,MESH3D%NBOR%I,
     &  NPLAN,AT,DT,LT,NIT,H%R,HPROP%R,MESH3D%Z%R,U%R,V%R,
     &  TA%ADR(MAX(IND_S,1))%P%R,
     &  TA%ADR(MAX(IND_T,1))%P%R,VISCVI%ADR(3)%P%R,TITCAS,
     &T3D_FILES(T3DGEO)%NAME,T3D_FILES(T3DCLI)%NAME,WAQPRD,
     &T3D_FILES(T3DDL1)%LU,T3D_FILES(T3DDL1)%NAME,T3D_FILES(T3DDL2)%LU,
     &T3D_FILES(T3DDL2)%NAME,T3D_FILES(T3DDL3)%LU,
     &T3D_FILES(T3DDL3)%NAME,T3D_FILES(T3DDL5)%LU,
     &T3D_FILES(T3DDL5)%NAME,
     &  T3D_FILES(T3DDL6)%LU,T3D_FILES(T3DDL6)%NAME,
     &  T3D_FILES(T3DDL7)%LU,T3D_FILES(T3DDL7)%NAME,
     &  T3D_FILES(T3DL11)%LU,T3D_FILES(T3DL11)%NAME,
     &  T3D_FILES(T3DDL4)%LU,T3D_FILES(T3DDL4)%NAME,
     &  T3D_FILES(T3DDL8)%LU,T3D_FILES(T3DDL8)%NAME,
     &T3D_FILES(T3DDL9)%LU,T3D_FILES(T3DDL9)%NAME,T3D_FILES(T3DL10)%LU,
     &T3D_FILES(T3DL10)%NAME,INFOGR,NELEM2,SALI_DEL,TEMP_DEL,VELO_DEL,
     &DIFF_DEL,MARDAT,MARTIM,FLODEL%R,V2DPAR%R,MESH2D%KNOLG%I)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TEL4DEL'
!
      ENDIF
!
!     3D OUTPUT
!
      CALL CREATE_DATASET(T3D_FILES(T3DRES)%FMT, ! RESULT FILE FORMAT
     &                    T3D_FILES(T3DRES)%LU,  ! RESULT FILE LU
     &                    TITCAS,     ! TITLE
     &                    MAXVA3,     ! MAX NUMBER OF OUTPUT VARIABLES
     &                    TEXT3,      ! NAMES OF OUTPUT VARIABLES
     &                    SORG3D)     ! OUTPUT OR NOT
      CALL WRITE_MESH(T3D_FILES(T3DRES)%FMT, ! RESULT FILE FORMAT
     &                T3D_FILES(T3DRES)%LU,  ! RESULT FILE LU
     &                MESH3D,          ! MESH
     &                NPLAN,           ! NUMBER OF PLANE /NA/
     &                DATE,            ! START DATE
     &                TIME,            ! START HOUR
     &                I_ORIG,J_ORIG)   ! COORDINATES OF THE ORIGIN
!
!     3D OUTPUT FOR RESTART
!
      IF(RESTART_MODE.AND.T3D_FILES(T3DRST)%NAME(1:1).NE.' ') THEN
        CALL CREATE_DATASET(T3D_FILES(T3DRST)%FMT, ! RESULT FILE FORMAT
     &                      T3D_FILES(T3DRST)%LU,  ! RESULT FILE LU
     &                      TITCAS,     ! TITLE
     &                      MAXVA3,     ! MAX NUMBER OF OUTPUT VARIABLES
     &                      TEXT3,      ! NAMES OF OUTPUT VARIABLES
     &                      SOREST)     ! OUTPUT OR NOT
        CALL WRITE_MESH(T3D_FILES(T3DRST)%FMT, ! RESULT FILE FORMAT
     &                  T3D_FILES(T3DRST)%LU,  ! RESULT FILE LU
     &                  MESH3D,          ! MESH
     &                  NPLAN,           ! NUMBER OF PLANE /NA/
     &                  DATE,            ! START DATE
     &                  TIME,            ! START HOUR
     &                  I_ORIG,J_ORIG)   ! COORDINATES OF THE ORIGIN
      ENDIF
!
!     THESE VARIABLES ARE INITIALISED FOR THE FIRST CALL TO BIEF_DESIMP
!
      IF(SORG3D(14)) CALL OS('X=0     ',X=UCONV)
      IF(SORG3D(15)) CALL OS('X=0     ',X=VCONV)
      IF(SORG3D(16)) CALL OS('X=0     ',X=WCONV)
      IF(SORG3D(18)) CALL OS('X=0     ',X=DM1)
      IF(SORG3D(19)) CALL OS('X=0     ',X=DHHN)
      IF(SORG3D(20)) CALL OS('X=0     ',X=UCONVC)
      IF(SORG3D(21)) CALL OS('X=0     ',X=VCONVC)
      IF(SORG3D(22)) CALL OS('X=0     ',X=UD)
      IF(SORG3D(23)) CALL OS('X=0     ',X=VD)
      IF(SORG3D(24)) CALL OS('X=0     ',X=WD)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_DESIMP'
        CALL BIEF_DESIMP(T3D_FILES(T3DRES)%FMT,VARSO3,
     &                   HIST,0,NPOIN3,T3D_FILES(T3DRES)%LU,'STD',AT,LT,
     &                   LISPRD,GRAPRD,
     &                   SORG3D,SORIM3,MAXVA3,TEXT3,GRADEB,LISDEB)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF DESIMP'
!
!     SEDIMENTOLOGY OUTPUT
!
      IF(SEDI.AND.T3D_FILES(T3DSED)%NAME(1:1).NE.' ') THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DESSED'
        CALL DESSED(NPF%I,IVIDE%R,EPAI,HDEP%R,
     &              CONC,TEMP%R,ZR%R,NPOIN2,NPFMAX,
     &              NCOUCH,NIT,GRAPRD,LT,DTC,TASSE,GIBSON,
     &              T3D_FILES(T3DSED)%LU,TITCAS,BIRSED,0)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DESSED'
      ENDIF
!
! PREPARES THE 2D OUTPUT FILE : NHYD CHANNEL, NSOR VARIABLES (?)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRGEO'
      CALL CREATE_DATASET(T3D_FILES(T3DHYD)%FMT, ! FORMAT FICHIER RESULTAT
     &                    T3D_FILES(T3DHYD)%LU,  ! LU FICHIER RESULTAT
     &                    TITCAS,     ! TITRE DE L'ETUDE
     &                    MAXVAR,     ! MAX VARIABLES SORTIE
     &                    TEXTE,      ! NOMS VARIABLES SORTIE
     &                    SORG2D)     ! SORTIE OU PAS DES VARIABLES
      CALL WRITE_MESH(T3D_FILES(T3DHYD)%FMT, ! FORMAT FICHIER RESULTAT
     &                T3D_FILES(T3DHYD)%LU,  ! LU FICHIER RESULTAT
     &                MESH2D,          ! DESCRIPTEUR MAILLAGE
     &                1,               ! NOMBRE DE PLAN /NA/
     &                DATE,            ! DATE DEBUT
     &                TIME,            ! HEURE DEBUT
     &                I_ORIG,J_ORIG)   ! COORDONNEES DE L'ORIGINE.
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRGEO'
!
! 2D OUTPUT
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_DESIMP POUR 2D'
      CALL BIEF_DESIMP(T3D_FILES(T3DHYD)%FMT,VARSOR,
     &                 HIST,0,NPOIN2,T3D_FILES(T3DHYD)%LU,'STD',AT,LT,
     &                 LISPRD,GRAPRD,
     &                 SORG2D,SORIMP,MAXVAR,TEXTE,GRADEB,LISDEB)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_DESIMP POUR 2D'
!
!-----------------------------------------------------------------------
! INITIALISES MASS BALANCE AND CUMULATIVE FLUXES
!
      IF(BILMAS) THEN
!
        CALL MITTIT(10,AT,LT)
!
        CALL MASS3D(.TRUE.,LT)
!
        CALL OS ( 'X=Y     ', X=MASINI, Y=MASSE)
        CALL OS ( 'X=0     ', X=FLUCUM         )
        MASINI_WATER=MASSE_WATER
        FLUXTOTCUM=0.D0
!       MAYBE NOT USEFUL
        CALL OS ( 'X=0     ', X=FLUX           )
!
!       INITIALIZATION OF BED PROPERTIES
! 	MASBED0 INITIAL MASS
!       ESOMT : CUMULATED BED EVOLUTION 
!       --> Set to Zero but could be read from 2D file
!       MASDEP : Set to Zero 
!
        IF(SEDI) THEN      
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASSED'
          CALL MASSED(MASBED0,EPAI,CONC,HDEP%R,T2_01%R,NPOIN2,NPFMAX,
     &                NCOUCH,NPF%I,TASSE,GIBSON,RHOS,VOLU2D%R)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MASSED'
          MASDEP = 0.D0        
          CALL OS('X=0     ',X=ESOMT)
!         PRINT INITIAL MASS 
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'MASSE INITIALE DU LIT :',MASBED0
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'INITIAL MASS OF SEDIMENT BED :',MASBED0
          ENDIF
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
! RETURNS WHEN THE NUMBER OF REQUIRED TIMESTEPS IS 0
!
      IF(NIT.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,11)
        IF(LNG.EQ.2) WRITE(LU,12)
        RETURN
      ENDIF
!
11    FORMAT(' ARRET DANS TELEMAC-3D, NOMBRE D''ITERATIONS DEMANDE NUL')
12    FORMAT(' BREAK IN TELEMAC-3D, NUMBER OF ITERATIONS ASKED NULL')
!
!-----------------------------------------------------------------------
!
! INITIALISES THE HORIZONTAL VELOCITY AFTER DIFFUSION
! IN ORDER TO ACCELERATE THE SOLVER CONVERGENCE
!
      IF(DEBU.OR.(.NOT.DEBU.AND.TROUVE(22).NE.1)) THEN
        CALL OS ( 'X=Y     ', X=UD, Y=U)
      ENDIF
      IF(DEBU.OR.(.NOT.DEBU.AND.TROUVE(23).NE.1)) THEN
        CALL OS ( 'X=Y     ', X=VD, Y=V)
      ENDIF
!
! INITIALISES THE FREE SURFACE AND DIFFERENT VERTICAL VELOCITIES
!
      IF(NONHYD) THEN
        IF(DEBU.OR.(.NOT.DEBU.AND.TROUVE(24).NE.1)) THEN
          CALL OS ( 'X=Y     ', X=WD,  Y=W  )
        ENDIF
      ENDIF
!
! SOURCE TERMS : FINDS LOCATION OF SOURCES (USED IN PREADV HEREAFTER)
!                WILL SUBSEQUENTLY BE DONE AT EACH TIMESTEP
!
      IF(NSCE.GT.0) THEN
!
!       IN THE 2D MESH -> ISCE
        CALL PROXIM(ISCE,XSCE,YSCE,MESH2D%X%R,MESH2D%Y%R,NSCE,NPOIN2,
     &              IKLE2%I,NELEM2,NELMAX2)
!       ON THE VERTICAL -> KSCE
        CALL FINDKSCE(NPOIN2,NPLAN,Z3%R,NSCE,ISCE,ZSCE,KSCE,INFOGR)
!
      ENDIF
!
!=======================================================================
!     PREPARATION OF ADVECTION FOR THE FIRST TIME STEP 
!=======================================================================
!
!     WSCONV IS NOT INITIALISED BEFORE GOING INTO TRIDW2
      CALL OS('X=0     ',X=WSCONV)
!
      IF(DEBU.OR.(.NOT.DEBU.AND.TROUVE(14).NE.1)) THEN
        CALL OS('X=Y     ',X=UCONV,Y=U)
      ENDIF
      IF(DEBU.OR.(.NOT.DEBU.AND.TROUVE(15).NE.1)) THEN
        CALL OS('X=Y     ',X=VCONV,Y=V)
      ENDIF
!     USED ONLY FOR TRIDW3 IN PREADV
      IF(NONHYD) THEN
        IF(DEBU.OR.(.NOT.DEBU.AND.TROUVE(16).NE.1)) THEN
          CALL OS('X=Y     ',X=WCONV,Y=W)
        ENDIF
      ENDIF
!
      IF(.NOT.DEBU.AND.TROUVE(19).EQ.1) THEN
!       DH AND HN HAVE BEEN RECOVERED
!       OLD Z TEMPORARILY REDONE IN T3_01
        CALL OV('X=Y     ',T3_01%R(1:NPOIN2),Z(1:NPOIN2),
     &          Z(1:NPOIN2),0.D0,NPOIN2)
        CALL CALCOT(T3_01%R,HN%R)
!       COMPUTING ZFLATS WITH THE OLD Z IN T3_01
        CALL FSGRAD(GRADZS,ZFLATS,T3_01%R(NPOIN3-NPOIN2+1:NPOIN3),
     &              ZF,IELM2H,MESH2D,MSK,MASKEL,
     &              UNSV2D,T2_01,NPOIN2,OPTBAN,SVIDE)
!       COMPUTING OLD ZCONV AND GRAZCO
        CALL MAKE_ZCONV(ZCONV,GRAZCO,ZFLATS,DH,HN,ZF,
     &                  TETAZCOMP,TETAH,
     &                  NELEM2,OPTBAN,MESH2D%IKLE%I,MESH2D)     
!       RESETTING NEW ZFLATS AND GRADZS
        CALL FSGRAD(GRADZS,ZFLATS,Z(NPOIN3-NPOIN2+1:NPOIN3),
     &              ZF,IELM2H,MESH2D,MSK,MASKEL,
     &              UNSV2D,T2_01,NPOIN2,OPTBAN,SVIDE)     
      ELSE
        CALL OS('X=0     ',X=DH)
        CALL OS('X=0     ',X=ZCONV)
        CALL OS('X=0     ',X=GRAZCO)     
      ENDIF
!
      IF(N_ADV(ADV_CAR).GT.0) THEN
        IF(DEBU.OR.TROUVE(20).NE.1.OR.TROUVE(21).NE.1) THEN
          CALL OS('X=Y     ',X=UCONVC,Y=U)
          CALL OS('X=Y     ',X=VCONVC,Y=V)
        ENDIF
      ENDIF
!
      IF(DEBU.OR.(.NOT.DEBU.AND.TROUVE(18).NE.1)) THEN
        CALL OS('X=0     ',X=DM1)
      ENDIF
!     
!     INITIALISING SOURCES AND SMH 
      CALL OS ('X=0     ',X=SMH)
!     SOURCES : COMPUTATION OF INITIAL INPUTS WHEN VARYING IN TIME
      IF(NSCE.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPELS DE T3D_DEBSCE' 
        DO I=1,NSCE
          QSCE2(I)=T3D_DEBSCE(AT,I,QSCE)
        ENDDO
        IF(DEBUG.GT.0) WRITE(LU,*) 'FIN DES APPELS DE T3D_DEBSCE'
        IF(NTRAC.GT.0) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPELS DE T3D_TRSCE'          
          DO I=1,NSCE
            DO ITRAC=1,NTRAC            
              TA_SCE%ADR(ITRAC)%P%R(I)=T3D_TRSCE(AT,I,ITRAC)
            ENDDO
          ENDDO
          IF(DEBUG.GT.0) WRITE(LU,*) 'FIN DES APPELS DE T3D_TRSCE'
        ENDIF
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'PREMIER APPEL DE SOURCES_SINKS'  
      CALL SOURCES_SINKS
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOURCES_SINKS'
!
!     FLULIM NOT INITIALISED AND USED IN PREADV, THROUGH FLUX3D
!
      IF(OPT_HNEG.EQ.2) THEN
        DO I=1,MESH2D%NSEG
          FLULIM%R(I)=1.D0
        ENDDO
      ENDIF
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'PREMIER APPEL DE PREADV'
!
      CALL PREADV(W,WS,ZPROP,ISOUSI,LT,VOLU,VOLUN)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DU PREMIER APPEL DE PREADV' 
!
!     NOW SETTING VOLUN=VOLU (IN CASE OF COMPUTATION CONTINUED IT HAS
!     BEEN RETRIEVED FROM THE PREVIOUS COMPUTATION)
!
      IF(.NOT.DEBU.AND.TROUVE(17).EQ.1) THEN
        CALL OS('X=Y     ',X=VOLUN,Y=VOLU)
      ENDIF
!
!=======================================================================
!     COUPLING WITH SISYPHE
!=======================================================================
!
!     COUPLING WITH SISYPHE
!     WRITES THE INITIAL CONDITIONS FOR U(Z=0), V(Z=0) AND H
!
      IF(COUPLING.NE.' ') THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'TELEMAC3D COUPLE AVEC : ',COUPLING
        IF(LNG.EQ.2) WRITE(LU,*) 'TELEMAC3D COUPLED WITH: ',COUPLING
      ENDIF
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
!       U AND V WITH 2D STRUCTURE : BOTTOM VELOCITY AS A 2D VARIABLE
        CALL CPSTVC(U2D,U)
        CALL CPSTVC(V2D,V)
!
        CALL CONFIG_CODE(2)
!       INOUT VARIABLES IN SISYPHE CANNOT BE HARDCODED
        IBID=1
        LBID=.FALSE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'PREMIER APPEL DE SISYPHE'
        CALL SISYPHE(0,LT,GRAPRD,LISPRD,NIT,U2D,V2D,H,H,ZF,UETCAR,CF,
     &               RUGOF,
     &               LBID,IBID,LBID,CODE1,PERCOU_SIS,
     &               U,V,AT,VISCVI,DT,CHARR,SUSP,
!                          1 PRECLUDES THE USE OF THE FOLLOWING ARGUMENTS
     &               FLBOR,1,DM1,UCONV,VCONV,ZCONV,DIRMOY,HM0,TPR5)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DU PREMIER APPEL DE SISYPHE'
        CALL CONFIG_CODE(1)
!
!       RETRIEVES ORIGINAL U AND V STRUCTURE
!
        CALL CPSTVC(UN,U)
        CALL CPSTVC(VN,V)
!
      ENDIF
!
!=======================================================================
!     COUPLING WITH TOMAWAC
!=======================================================================
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
!
        IF(LNG.EQ.1) THEN
          WRITE (LU,*) 'TELEMAC-3D : COUPLAGE INTERNE AVEC TOMAWAC'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE (LU,*) 'TELEMAC-3D: INTERNAL COUPLING WITH TOMAWAC'
        ENDIF
        CALL CONFIG_CODE(3)
        IF(DEBUG.GT.0) WRITE(LU,*) 'PREMIER APPEL DE TOMAWAC'
!       CALL WAC(0,U2D,V2D,H,FXH,FYH,WINDX,WINDY,CODE1,AT,DT,NIT,
!                PERCOU_WAC,DIRMOY,HM0,TPR5)
        CALL WAC(0,U2D,V2D,H,FXH,FYH,T2_01,T2_02,CODE1,AT,DT,NIT,
     &           PERCOU_WAC,DIRMOY,HM0,TPR5)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TOMAWAC'
        CALL CONFIG_CODE(1)
!
      ENDIF
!
!=======================================================================
!
!     INITIALISES THE SEDIMENT SETTLING VELOCITY
!     NEGLECTS TURBULENCE HERE
!     WCHU COMPUTED HERE IS USED IN BORD3D FOR ROUSE PROFILES
!     CV:Floculation and  hindered settling
!        Soulsby flocculation and hindered settling is now coded in VITCHU
!        along with all the previous telemac methods
!        NOTE: moved settling calculation to after CLSEDI since 
!        soulsby floc requires bed shear stress
!
      IF(SEDI) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VITCHU'
        CALL VITCHU(WCHU,WCHU0,TURBWC,U,V,W,H,RUGOF,LISRUF,
     &              TURBA,TURBB,T3_01,T3_02,T3_03,SVIDE,MESH3D,
     &              IELM3,NPOIN2,NPOIN3,NPLAN,NTRAC,MSK,MASKEL,
     &              UETCAR,TA,HN,HMIN,FLOC, FLOC_TYPE,
     &              HINDER,HIND_TYPE,CGEL,CINI)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VITCHU'
      ENDIF
! 
!=======================================================================
! THE TIME LOOP BEGINS HERE
!=======================================================================
!
      TIMELOOP: DO LT=1,NIT
!
      AT = AT + DT
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'BOUCLE EN TEMPS LT=',LT
      INFOGR = .FALSE.
      IF (MOD(LT,LISPRD) == 0) INFOGR = .TRUE.
      INFOGR = LISTIN .AND. INFOGR
      IF (INFOGR) CALL MITTIT(1,AT,LT)
!
!=======================================================================
!
!     CORRECTION JMH 20/09/2013
!            
!     IPBOT HAS BEEN MODIFIED FOR CVDF3D IN THE PREVIOUS TIME STEP, 
!     IT IS RESTORED HERE WITH ZPROP
!     NOTE: DIFFERENT IPBOT_Z AND IPBOT_ZPROP WOULD BE CLEARER....
      IF(LT.GT.1) THEN
        CALL PLANE_BOTTOM(IPBOT%I,ZPROP%R,NPOIN2,NPLAN,SIGMAG,OPTBAN)
      ENDIF             
!
!=======================================================================
! SOURCES : COMPUTES INPUTS WHEN VARYING IN TIME
!           IF NO VARIATION IN TIME QSCE2=QSCE AND TASCE2=TASCE
!=======================================================================
!
      IF(NSCE.GT.0) THEN
        DO I=1,NSCE
          QSCE2(I)=T3D_DEBSCE(AT,I,QSCE)
        ENDDO
        IF(NTRAC.GT.0) THEN
          DO I=1,NSCE
            DO ITRAC=1,NTRAC
              TA_SCE%ADR(ITRAC)%P%R(I)=T3D_TRSCE(AT,I,ITRAC)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! INTERNAL COUPLING WITH SISYPHE
!=======================================================================
!
      IF( INCLUS(COUPLING,'SISYPHE')   .AND.
     &   (PERCOU_SIS*(LT/PERCOU_SIS).EQ.LT.OR.LT.EQ.1) ) THEN
!
!       U AND V WITH 2D STRUCTURE : BOTTOM VELOCITY AS A 2D VARIABLE
        CALL CPSTVC(U2D,U)
        CALL CPSTVC(V2D,V)
!
!       NOW RUNS ONE TURN OF SISYPHE'S TIME LOOP AND RETURNS
        CALL CONFIG_CODE(2)
        IBID=1
        LBID=.FALSE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SISYPHE'     
        CALL SISYPHE(1,LT,GRAPRD,LISPRD,NIT,U2D,V2D,H,HN,ZF,UETCAR,
     &               CF,RUGOF,LBID,IBID,LBID,CODE1,PERCOU_SIS,
     &               U,V,AT,VISCVI,DT*PERCOU_SIS,CHARR,SUSP,
!                          1 PRECLUDES THE USE OF THE 4 FOLLOWING ARGUMENTS
     &               FLBOR,1,DM1,UCONV,VCONV,ZCONV,
!                    VARIABLES TRANSMITTED FROM TOMAWAC
     &               DIRMOY,HM0,TPR5)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SISYPHE'
        CALL CONFIG_CODE(1)
!
!       HDEP MUST BE UPDATED BECAUSE SISYPHE CHANGED ZF
        IF(SEDI) CALL OS('X=Y-Z   ',X=HDEP,Y=ZF,Z=ZR)     
!
!       RETRIEVES ORIGINAL U AND V STRUCTURE
        CALL CPSTVC(UN,U)
        CALL CPSTVC(VN,V)
!
      ENDIF
!
!=======================================================================
! END OF CAMILLE LEQUETTE'S MODIFICATIONS
!=======================================================================
!
!     COUPLING WITH TOMAWAC
!
      IF(INCLUS(COUPLING,'TOMAWAC').AND.
     &   PERCOU_WAC*((LT-1)/PERCOU_WAC).EQ.LT-1) THEN
!
        CALL CONFIG_CODE(3)
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TOMAWAC'
!       CALL WAC(1,U2D,V2D,H,FXH,FYH,WINDX,WINDY,CODE1,AT,
!    *           DT,NIT,PERCOU_WAC,DIRMOY,HM0,TPR5)
        CALL WAC(1,U2D,V2D,H,FXH,FYH,T2_01,T2_02,CODE1,AT,
     &           DT,NIT,PERCOU_WAC,DIRMOY,HM0,TPR5)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TOMAWAC'
        CALL CONFIG_CODE(1)
!
      ENDIF
!
!=======================================================================
!
! SAVES H, TA, TP, AK, EP
! IN    HN,TRN,TPN,AKN,EPN
!
      CALL OS ( 'X=Y     ', X=HN,    Y=H     )
      CALL OS ( 'X=Y     ', X=VOLUN, Y=VOLU  )
      IF(NCSIZE.GT.1) CALL OS('X=Y     ',X=VOLUNPAR,Y=VOLUPAR)
      CALL OS ( 'X=Y     ', X=UN,    Y=U     )
      CALL OS ( 'X=Y     ', X=VN,    Y=V     )
      IF(NONHYD) CALL OS ( 'X=Y     ' , X=WN, Y=W)
      CALL OS ( 'X=Y     ', X=GRADZN,Y=GRADZS)
!     TRACERS (IF LT=1 DONE AFTER CALL CONDIM AND BIEF_SUITE)
      IF(NTRAC.GT.0.AND.LT.GT.1) CALL OS ('X=Y     ', X=TRN, Y=TA)
!
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
        CALL OS ( 'X=Y     ', X=AKN, Y=AK )
        CALL OS ( 'X=Y     ', X=EPN, Y=EP )
      ENDIF
!
      IF(BILMAS) THEN
        MASSEN_WATER = MASSE_WATER
        CALL OS ( 'X=Y     ', X=MASSEN, Y=MASSE )
      ENDIF
!
! COMPUTES MEAN UN AND VN IN THE VERTICAL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VERMOY'
      CALL VERMOY(UN2D%R,VN2D%R,UN%R,VN%R,2,Z,
     &            T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VERMOY'
!
!-----------------------------------------------------------------------
!
! COMPUTES FRICTION COEFFICIENT
!
!     TIME VARIATIONS OF RUGOF (CORSTR IS IN TELEMAC-2D LIBRARY)
!     MUST BE USER-IMPLEMENTED - NOTHING DONE IN STANDARD
      CALL CORSTR
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE COEFRO'
      CALL COEFRO(CF,H,UN2D,VN2D,KARMAN,KFROT,RUGOF,GRAV,MESH2D,T2_01)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE COEFRO'
!
!-----------------------------------------------------------------------
!
! CHECKS AND HARMONISES THE BOUNDARY CONDITION TYPES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LICHEK'
      CALL LICHEK(LIMPRO%I,NPTFR2,
     &             MESH2D%IKLBOR%I,MESH2D%NELEB,MESH2D%NELEBX)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LICHEK'
!
! BOUNDARY CONDITIONS FOR THE K-EPSILON MODEL
!
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
        CALL KEPICL(LIKBOF%I, LIEBOF%I, LIUBOF%I,
     &              LIKBOL%I, LIEBOL%I, LIUBOL%I,
     &              LIKBOS%I, LIEBOS%I, LIUBOS%I,
     &              NPTFR2, NPLAN, NPOIN2, KENT, KSORT, KADH, KLOG)
      ENDIF
!
!-----------------------------------------------------------------------
! FORCING AT THE BOUNDARIES
!
! METEOROLOGICAL CONDITIONS
!
      IF (VENT.OR.ATMOS) CALL METEO
     &   (PATMOS%R, WIND%ADR(1)%P%R, WIND%ADR(2)%P%R, FUAIR, FVAIR,
     &    X2%R, Y2%R, AT, LT, NPOIN2, VENT, ATMOS, H%R, T2_01%R,
     &    GRAV, RHO0, 0.D0, PRIVE,T3DFO1,T3D_FILES,LISTIN)
!
!-----------------------------------------------------------------------
!
!     SEDIMENT
!
      IF(SEDI) THEN
!
!       COMPUTES THE SEDIMENT SETTLING VELOCITY
!
        CALL VITCHU(WCHU,WCHU0,TURBWC,U,V,W,H,RUGOF,LISRUF,
     &  TURBA,TURBB,T3_01,T3_02,T3_03,SVIDE,MESH3D,IELM3,NPOIN2,NPOIN3,
     &  NPLAN,NTRAC,MSK,MASKEL,UETCAR,TA,HN,HMIN,FLOC, FLOC_TYPE,
     &  HINDER,HIND_TYPE,CGEL,CINI)
!
!       BOUNDARY CONDITIONS FOR THE SEDIMENTOLOGY
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CLSEDI'
        CALL CLSEDI
     &   (ATABOF%ADR(NTRAC)%P%R,BTABOF%ADR(NTRAC)%P%R,
     &    ATABOS%ADR(NTRAC)%P%R,BTABOS%ADR(NTRAC)%P%R,
     &    TA%ADR(NTRAC)%P%R,WCHU%R,
     &    GRADZF%ADR(1)%P%R,GRADZF%ADR(2)%P%R,
     &    GRADZS%ADR(1)%P%R,GRADZS%ADR(2)%P%R,
     &    X, Y, Z, H, DELTAR%R, T3_01, T3_02%R, T3_03%R,
     &    EPAI, CFDEP,CONC, HDEP%R, FLUER%R,
     &    FLUDPT%R, LITABF%ADR(NTRAC)%P%I, LITABS%ADR(NTRAC)%P%I,
     &    KLOG, NPOIN3, NPOIN2, NPLAN, NCOUCH,
     &    ITURBV, DT, RHO0, RHOS,
     &    TOCD,MPART,TOCE,UETCAR%R,
     &    GRAV,SEDCO,DMOY,CREF,ZREF,CF,AC,KSPRATIO,ICR,ICQ,
     &    RUGOF,SETDEP,HSED)
!
!       ATABOF AND BTABOF ARE NO LONGER 0 FOLLOWING CLSEDI
        ATABOF%ADR(NTRAC)%P%TYPR='Q'
        BTABOF%ADR(NTRAC)%P%TYPR='Q'
!       ATABOS%ADR(NTRAC)%P%TYPR='Q'
!       BTABOS%ADR(NTRAC)%P%TYPR='Q'
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CLSEDI'
      ENDIF
!
!     PREPARING BOUNDARY CONDITIONS FOR THOMPSON METHOD
!
      IF(THOMFR.AND.NFRLIQ.GT.0) THEN
!
        CALL CPSTVC(H,T2_01)
        CALL PREBOR(HBOR%R,UBOR2D%R,VBOR2D%R,TABORL,U2D%R,V2D%R,H%R,
     &              T2_01%R,TA,MESH2D%NBOR%I,
     &              MESH2D%NPOIN,MESH2D%NPTFR,
!    &              NTRAC SET TO ZERO PROVISIONALLY
     &              0    ,NFRLIQ,FRTYPE,NUMLIQ%I)
!       RESTORING USER BOUNDARY CONDITIONS BEFORE CALLING BORD3D
!       TO AVOID UNDUE CALLS TO SL3, ETC. 
        CALL THOMPS_BC(3)
!
      ENDIF
!
! UPDATES BOUNDARY CONDITION VALUES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BORD3D'
      CALL BORD3D(AT,LT,INFOGR,NPTFR2,NFRLIQ)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BORD3D' 
!
      IF(THOMFR.AND.NFRLIQ.GT.0) THEN
!
!       NOW THAT BORD3D HAS BEEN CALLED
!       CHANGING AGAIN BOUNDARY CONDITIONS FOR THOMPSON
        CALL THOMPS_BC(2)
!
      ENDIF
!
! BOUNDARY CONDITIONS FOR THE VELOCITY ON LATERAL BOUNDARIES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TBORD'
      CALL TBORD(AUBORL%R,LIUBOL%I,
     &           RUGOL%R,
     &           MESH2D%DISBOR%R,MESH2D%NELBOR%I,MESH2D%NULONE%I,
     &           MESH2D%IKLE%I,NELEM2,
     &           U%R,V%R,W%R,
     &           NBOR2%I,NPOIN2,NPLAN,NPTFR2,DNUVIH,DNUVIV,
     &           KARMAN,LISRUL,KFROTL,
     &           KENT,KENTU,KSORT,KADH,KLOG,UETCAL%R,NONHYD,
     &           T2_02%R,MESH2D)
      IF(KFROTL.EQ.0) THEN
        AUBORL%TYPR='0'
      ELSE
        AUBORL%TYPR='Q'
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TBORD, APPEL DE TFOND'
!
! BOUNDARY CONDITIONS FOR THE VELOCITY ON THE BOTTOM
!
      CALL TFOND(AUBORF%R,
     &           CF%R,UN2D%R,VN2D%R,U%R,V%R,W%R,KARMAN,
     &           LISRUF,DNUVIV,Z,NPOIN2,KFROT,RUGOF%R,UETCAR%R,
     &           NONHYD,OPTBAN,HN%R,GRAV,IPBOT%I,NPLAN)
      AUBORF%TYPR='Q'
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TFOND'
!
! BOUNDARY CONDITIONS FOR K-EPSILON MODEL + COMPUTES CONSTRAINTS
! AT THE BOTTOM AND LATERAL BOUNDARIES IF K-EPSILON IS REQUIRED
!
      IF(ITURBV.EQ.3) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE KEPCL3'
        CALL KEPCL3(KBORF%R,EBORF%R,LIKBOF%I,LIEBOF%I,LIUBOF%I,
     &              KBORL%R,EBORL%R,LIKBOL%I,LIEBOL%I,LIUBOL%I,
     &              RUGOL%R,KBORS%R,EBORS%R,
     &              LIKBOS%I,LIEBOS%I,LIUBOS%I,
     &              MESH2D%DISBOR%R,AK%R,U%R,V%R,H%R,ZPROP%R,
     &              NBOR2%I,NPOIN2,NPLAN,NPTFR2,DNUVIH,DNUVIV,
     &              KARMAN,CMU,LISRUF,LISRUL,
     &              VIRT,KMIN,KMAX,EMIN,EMAX,
     &              KENT,KENTU,KSORT,KADH,KLOG,
     &              UETCAR%R,UETCAL%R, FICT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE KEPCL3'
!
      ELSEIF(ITURBV.EQ.7) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE KOMCL3'
        CALL KOMCL3(KBORF%R,EBORF%R,LIKBOF%I,LIEBOF%I,LIUBOF%I,
     &              KBORL%R,EBORL%R,LIKBOL%I,LIEBOL%I,LIUBOL%I,
     &              RUGOL%R,KBORS%R,EBORS%R,LIKBOS%I,LIEBOS%I,
     &              LIUBOS%I,MESH2D%DISBOR%R,AK%R,EP%R,
     &              U%R,V%R,W%R,H%R,ZPROP%R,
     &              NBOR2%I,NPOIN2,NPLAN,NPTFR2,DNUVIH,DNUVIV,
     &              KARMAN,ALPHA,BETA,BETAS,OMSTAR,SCHMIT,LISRUF,
     &              LISRUL,VIRT,GRAV,KMIN,KMAX,EMIN,EMAX,
     &              KENTU,KENT,KSORT,KADH,KLOG,UETCAR%R,UETCAL%R)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE KOMCL3'
!
      ENDIF
!
! CLIPS HBOR
!
      IF(OPTBAN.EQ.2) THEN
        CALL CLIP(HBOR,HMIN,.TRUE.,1.D6,.FALSE.,0)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     THOMPSON BOUNDARY CONDITIONS
!
      IF(THOMFR) THEN
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE THOMPS'
!     T2_01 IS HERE A VALUE OF H SAVED BEFORE CALLING BORD3D
      CALL THOMPS(HBOR%R,UBOR2D%R,VBOR2D%R,TABORL,U2D,V2D,T2_01,
     &            TA,ZF,MESH2D%X%R,MESH2D%Y%R,MESH2D%NBOR%I,
     &            FRTYPE,T2_02,T2_03,T2_04,T2_05,T2_06,T2_07,
     &            LIHBOR%I,LIUBOL%I,LIVBOL%I,LITABL,IT1%I,
     &            IT2%I,T2_08%R,T2_09%R,W1%R,T2_21,T2_22,T2_23,
     &            TAC,T2_10,MESH2D%SURDET%R,MESH2D%IKLE%I,
     &            MESH2D%IFABOR%I,MESH2D%NELEM,MESH2D,
     &            MESH2D%XNEBOR%R,MESH2D%YNEBOR%R,
!                                                   NTRAC SET TO ZERO
     &            MESH2D%NPOIN,MESH2D%NPTFR,DT,GRAV,0    ,
     &            NFRLIQ,KSORT,KINC,KENT,KENTU,MESH2D%LV,MSK,MASKEL,
     &            MESH2D%NELMAX,11,T2_11%R,NUMLIQ%I,MAT2D%ADR(1)%P%X%R,
     &            T2_12%R,T2_13%R,T2_14%R,IT3,IT4,
     &            T2_15,T2_16,T2_17,T2_18,T2_19,T2_20,T3_01)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE THOMPS'
!
!     DUPLICATING ON THE VERTICAL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE THOMPS_2DTO3D'
      CALL THOMPS_2DTO3D
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE THOMPS_2DTO3D'
!
      ENDIF  
!
!-----------------------------------------------------------------------
! SOURCE TERMS
!
      IF(NSCE.GT.0) THEN
        CALL FINDKSCE(NPOIN2,NPLAN,Z3%R,NSCE,ISCE,ZSCE,KSCE,INFOGR)
      ENDIF
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TRISOU'
      CALL TRISOU
     & (S0U%R,S0V%R, S0U,S0V,UN%R,VN%R,TA,X,Y,Z,
     &  T3_01%R, DELTAR, MESH3D, FCOR, CORIOL, NTRAC, LT,
     &  AT, DT, SURFA2%R, T3_02%R, T3_02, W1%R,
     &  MESH3D%M%X%R(1:6*NELEM3),MESH3D%M%X%R(6*NELEM3+1:12*NELEM3),
     &  SEDI, GRAV, NPOIN3, NELEM3, NPOIN2, NELEM2, NPLAN, NETAGE,
     &  IKLE3%I, PRIVE, LV, MSK, MASKEL%R, INCHYD,
     &  VOLU,VOLU%R,SVIDE,IELM3,MASKEL,NREJEU,ISCE,KSCE,QSCE2,
     &  U_SCE%R,V_SCE%R,
     &  IELM2H,GRADZS%ADR(1)%P,GRADZS%ADR(2)%P,Z3,T2_01, T2_02,MESH2D,
     &  T3_03, T3_03%R, T3_04, T3_04%R, LATIT, LONGIT, NORD,SMU,SMV,
     &  YASEM3D,SCHCVI,DENLAW,FXH,FYH,COUROU,NPTH,T3D_FILES,T3DBI1)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TRISOU, APPEL DE SOURCE'
!
      CALL SOURCE(S0U, S0V, S0W, S1U, S1V, S1W,
     &            U, V, WS, W,
     &            VOLU, VOLUN,T3_01,
     &            NPOIN3, NTRAC, LT, AT, DT, PRIVE, NONHYD,
     &            NPOIN2, NSCE,ISCE,KSCE,QSCE2,U_SCE%R,V_SCE%R,MAXSCE)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOURCE'
!
!     SAVES BOUNDARY VALUES FOR TIME TN
!
      IF(NSOUSI.GT.1) THEN
        DO IP=1,NPTFR3
          UBORSAVE%R(IP)=UN%R(NBOR3%I(IP))
          VBORSAVE%R(IP)=VN%R(NBOR3%I(IP))
        ENDDO
        IF(NONHYD) THEN
          DO IP=1,NPTFR3
            WBORSAVE%R(IP)=WN%R(NBOR3%I(IP))
          ENDDO
        ENDIF
        IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
          DO IP=1,NPTFR3
            KBORSAVE%R(IP)=AKN%R(NBOR3%I(IP))
            EBORSAVE%R(IP)=EPN%R(NBOR3%I(IP))
          ENDDO
        ENDIF
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            DO IP=1,NPTFR3
              TRBORSAVE%ADR(ITRAC)%P%R(IP)=
     &        TRN%ADR(ITRAC)%P%R(NBOR3%I(IP))
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! THE SUB-ITERATIONS LOOP BEGINS HERE
!=======================================================================
!
      SUBITER: DO ISOUSI = 1,NSOUSI
!
!     RESTORES BOUNDARY VALUES FOR TIME TN
!
      IF(ISOUSI.GT.1) THEN
        DO IP=1,NPTFR3
          UN%R(NBOR3%I(IP))=UBORSAVE%R(IP)
          VN%R(NBOR3%I(IP))=VBORSAVE%R(IP)
        ENDDO
        IF(NONHYD) THEN
          DO IP=1,NPTFR3
            WN%R(NBOR3%I(IP))=WBORSAVE%R(IP)
          ENDDO
        ENDIF
        IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
          DO IP=1,NPTFR3
            AKN%R(NBOR3%I(IP))=KBORSAVE%R(IP)
            EPN%R(NBOR3%I(IP))=EBORSAVE%R(IP)
          ENDDO
        ENDIF
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            DO IP=1,NPTFR3
          TRN%ADR(ITRAC)%P%R(NBOR3%I(IP))=TRBORSAVE%ADR(ITRAC)%P%R(IP)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!     BUILDS THE MESH FOR PROPAGATION STEP
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MESH_PROP'
      CALL MESH_PROP(HPROP,HN,H,PROLIN,HAULIN,TETAH,NSOUSI,ZPROP,
     &               IPBOT,NPOIN2,NPLAN,OPTBAN,SIGMAG,OPT_HNEG,
     &               MDIFF,MESH3D,VOLU3D,VOLU3DPAR,
     &               UNSV3D,MSK,MASKEL,IELM3)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MESH_PROP'
!
      IF(ISOUSI.GT.1) THEN
!       REBUILDS THE INITIAL MESH
!       NOTE: EVOLUTION OF ZF IS NOT TAKEN INTO ACCOUNT HERE - INVESTIGATE
        CALL CALCOT(Z,HN%R)
        CALL OS('X=Y     ',X=VOLU,Y=VOLUN)
        IF(NCSIZE.GT.1) CALL OS('X=Y     ',X=VOLUPAR,Y=VOLUNPAR)
        CALL GRAD2D(GRADZF%ADR(1)%P,GRADZF%ADR(2)%P,ZPROP,NPLAN,SVIDE,
     &              UNSV2D,T2_02,T2_03,T2_04,
     &              IELM2H,MESH2D,MSK,MASKEL)
        CALL FSGRAD(GRADZS,ZFLATS,Z(NPOIN3-NPOIN2+1:NPOIN3),
     &              ZF,IELM2H,MESH2D,MSK,MASKEL,
     &              UNSV2D,T2_01,NPOIN2,OPTBAN,SVIDE)
      ENDIF
!
!     SOURCES AND SINKS OF WATER
!
!     TEMPORARILY PUTS ZPROP IN MESH3D%Z
      SAVEZ     =>MESH3D%Z%R
      MESH3D%Z%R=>ZPROP%R
      CALL SOURCES_SINKS
!     RESTORES Z
      MESH3D%Z%R=>SAVEZ
!
!     SETS ADVECTION AND DIFFUSION PARAMETERS TO MONITOR CVDF3D
!     DIFFUSION AND SOURCE TERMS ARE DONE IN WAVE_EQUATION
!     IN CVDF3D (THIS IS DONE IN WAVE_EQUATION)
!
!     DIFFUSION OF U AND V IS DONE IN WAVE_EQUATION
      SCHDVI_HOR = 0
      SCHDVI_VER = SCHDVI
!
      SCHCVI_HOR = SCHCVI
      SCHCVI_VER = SCHCVI
!     ADVECTION IS NOT DONE AT THE FIRST TIME-STEP (THIS WAS VERSION 6.1)
!     IF(LT.EQ.1.AND.ISOUSI.EQ.1) THEN
!       SCHCVI_HOR = 0
!       SCHCVI_VER = 0
!     ENDIF
!
!     WHEN SCHCVI=ADV_SUP DIFF3D IS CALLED AND
!     SOURCE TERMS WOULD BE TREATED TWICE
      YAS0U=.FALSE.
      YAS1U=.FALSE.
!
!-----------------------------------------------------------------------
! ADVECTION-DIFFUSION STEP FOR VELOCITY COMPONENTS
!-----------------------------------------------------------------------
!
!     HERE DIFFUSION IS DONE IN MESH3D%Z, IT IS DIFFERENT FROM ZPROP IF
!     FROM THE SECOND SUB-ITERATION ON. SO IPBOT IS REDONE HERE
!
      IF(ISOUSI.GT.1) THEN
        CALL PLANE_BOTTOM(IPBOT%I,Z,NPOIN2,NPLAN,SIGMAG,OPTBAN)
      ENDIF
!
      IF(INFOGR) THEN
        IF (NONHYD) THEN
          CALL MITTIT(17,AT,LT)
        ELSE
          CALL MITTIT(4,AT,LT)
        ENDIF
      ENDIF
!
      SIGMAU = 1.D0
      UMIN   = 0.D0
      UMAX   = 1.D0
      CLUMIN = .FALSE.
      CLUMAX = .FALSE.
      YAWCHU = .FALSE.
!     YASEM3D = DONE IN TRISOU
      NEWDIF=.TRUE.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR U'
      CALL CVDF3D
     & (UD,UC,UN,VISCVI,SIGMAU,S0U,YAS0U,S1U,YAS1U,
     &  UBORL, UBORF, UBORS, AUBORL, AUBORF, AUBORS,
     &  BUBORL, BUBORF, BUBORS, LIUBOL, LIUBOF, LIUBOS,
     &  FLUX%R(1), FLUEXT,FLUEXTPAR,UMIN, CLUMIN, UMAX, CLUMAX,
     &  SCHCVI_HOR,SCHDVI_HOR,SLVDVI,TRBAVI,INFOGR,NEWDIF,
     &  CALCFLU(1),T2_01,T2_02,T2_03,
     &  T3_01,T3_02,T3_03,T3_04,MESH3D,IKLE3,MASKEL,MTRA1,
!    *  W1 , NPTFR3 , MMURD , VOLU , VOLUN ,
     &  W1,NPTFR3,MMURD,MURD_TF,VOLU3D,VOLU3DPAR,VOLU3D,VOLU3DPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     &  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SMU,YASEM3D,SVIDE,IT1,IT2,
     &  TRAV3,MESH2D,MATR2H,H,OPTBAN,OPTDIF,TETADI,YAWCHU,WCHU,
     &  AGGLOD,NSCE,SOURCES,U_SCE%R,NUMLIQ%I,DIRFLU,NFRLIQ,
     &  VOLUT,ZT,ZPROP,CALCRAIN(1),PLUIE,PARAPLUIE,0.D0,
     &  FLODEL,FLOPAR,SIGMAG,IPBOT%I,MAXADV,FLUDPT,FLUDP, FLUER,
     &  VOLU2D, V2DPAR, SETDEP)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR U'
!
      SIGMAV = 1.D0
      VMIN   = 0.D0
      VMAX   = 1.D0
      CLVMIN = .FALSE.
      CLVMAX = .FALSE.
      YAWCHU = .FALSE.
!     YASEM3D = DONE IN TRISOU
!     MDIFF ALREADY COMPUTED FOR U
      NEWDIF=.FALSE.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR V'
!     USE OF AUBORL,AUBORF,AUBORS IS NOT A MISTAKE
      CALL CVDF3D
     & (VD,VC,VN,VISCVI,SIGMAV,S0V,YAS0U,S1V,YAS1U,
     &  VBORL, VBORF, VBORS, AUBORL,AUBORF,AUBORS,
     &  BVBORL, BVBORF, BVBORS, LIVBOL, LIVBOF, LIVBOS,
     &  FLUX%R(2), FLUEXT,FLUEXTPAR,VMIN, CLVMIN, VMAX, CLVMAX,
     &  SCHCVI_HOR,SCHDVI_HOR,SLVDVI,TRBAVI,INFOGR,NEWDIF,
     &  CALCFLU(2),T2_01,T2_02,T2_03,
     &  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
!    *  W1 , NPTFR3 , MMURD , VOLU , VOLUN ,
     &  W1,NPTFR3,MMURD,MURD_TF,VOLU3D,VOLU3DPAR,VOLU3D,VOLU3DPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     &  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SMV,YASEM3D,SVIDE,IT1,IT2,
     &  TRAV3,MESH2D,MATR2H,H,OPTBAN,OPTDIF,TETADI,YAWCHU,WCHU,
     &  AGGLOD,NSCE,SOURCES,V_SCE%R,NUMLIQ%I,DIRFLU,NFRLIQ,
     &  VOLUT,ZT,ZPROP,CALCRAIN(2),PLUIE,PARAPLUIE,0.D0,
     &  FLODEL,FLOPAR,SIGMAG,IPBOT%I,MAXADV,FLUDPT,FLUDP, FLUER,
     &  VOLU2D, V2DPAR, SETDEP)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR V'
!
      IF(NONHYD) THEN
!
        SIGMAW = 1.D0
        WMIN   = 0.D0
        WMAX   = 1.D0
        CLWMIN = .FALSE.
        CLWMAX = .FALSE.
        YASEM3D= .FALSE.
        YAWCHU = .FALSE.
        NEWDIF=.TRUE.
!       TETADI MAY BE EQUAL TO 2 FOR U AND V, WHEN THE WAVE EQUATION
!       IS USED - NOT DONE ON W SO FAR
        TETADIVER = MIN(TETADI,1.D0)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR W'
!       USE OF AUBORL,AUBORF,AUBORS IS NOT A MISTAKE
        CALL CVDF3D
     & (WD,WC,WN,VISCVI,SIGMAW,S0W,.TRUE.,S1W,.TRUE.,
     &  WBORL, WBORF, WBORS, AUBORL, AUBORF, AUBORS,
     &  BWBORL, BWBORF, BWBORS, LIWBOL, LIWBOF, LIWBOS,
     &  FLUX%R(3), FLUEXT,FLUEXTPAR,WMIN, CLWMIN, WMAX, CLWMAX,
     &  SCHCVI_VER,SCHDVI_VER,SLVDVI,TRBAVI,INFOGR,NEWDIF,
     &  CALCFLU(3),T2_01,T2_02,T2_03,
     &  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
!    *  W1 , NPTFR3 , MMURD , VOLU , VOLUN ,
     &  W1,NPTFR3,MMURD,MURD_TF,VOLU3D,VOLU3DPAR,VOLU3D,VOLU3DPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     &  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,IT2,
     &  TRAV3,MESH2D,MATR2H,H,OPTBAN,OPTDIF,
     &  TETADIVER,YAWCHU,WCHU,AGGLOD,NSCE,SOURCES,W_SCE%R,NUMLIQ%I,
     &  DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,CALCRAIN(3),PLUIE,
     &  PARAPLUIE,0.D0,
     &  FLODEL,FLOPAR,SIGMAG,IPBOT%I,MAXADV,FLUDPT,FLUDP, FLUER,
     &  VOLU2D, V2DPAR, SETDEP)
!
      ENDIF
!
!-----------------------------------------------------------------------
! DIFFUSION AND PROPAGATION STEP BY WAVE_EQUATION
!-----------------------------------------------------------------------
!
      IF(INFOGR) THEN
        CALL MITTIT(6,AT,LT)
      ENDIF
!     TEMPORARILY PUTS ZPROP IN MESH3D%Z
      SAVEZ     =>MESH3D%Z%R
!     ALL PROPAGATION WILL BE DONE WITH ZPROP INSTEAD OF Z
      MESH3D%Z%R=>ZPROP%R      
!     IPBOT HAS BEEN MODIFIED FOR CVDF3D, IT IS RESTORED HERE WITH ZPROP
      IF(ISOUSI.GT.1) THEN
        CALL PLANE_BOTTOM(IPBOT%I,ZPROP%R,NPOIN2,NPLAN,SIGMAG,OPTBAN)
      ENDIF            
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WAVE_EQUATION'
!
      CALL WAVE_EQUATION(LT,ISOUSI)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WAVE_EQUATION'
!     RESTORES Z
      MESH3D%Z%R=>SAVEZ
!
!-----------------------------------------------------------------------
! CLIPS NEGATIVE DEPTHS
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORRECTION_DEPTH_3D'
      CALL CORRECTION_DEPTH_3D(MESH2D%W%R,MESH3D%W%R,MESH2D%GLOSEG%I,
     &                         MESH2D%GLOSEG%DIM1)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORRECTION_DEPTH_3D'
!
!-----------------------------------------------------------------------
! BUILDS NEW MESH WITH THE NEW FREE SURFACE
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CALCOT'
      CALL CALCOT(Z,H%R)
!     IPBOT UPDATED ACCORDINGLY, E.G. FOR CALLS TO PREDIV AND CVDF3D
      CALL PLANE_BOTTOM(IPBOT%I,Z,NPOIN2,NPLAN,SIGMAG,OPTBAN) 
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CALCOT'
!
!----------------------------------------------------------------------
!
!     GENERATES DATA FOR DELWAQ
!
      IF(INCLUS(COUPLING,'DELWAQ')) THEN
!
!     COMPUTING FLODEL (POINT TO POINT FLUXES)
!
      FORMUL = 'VGRADP       HOR'
      FORMUL(8:8) = '2'
!     ADVECTION FLUXES PER NODE (STORED IN MESH3D%W%R)
!     THE ASSEMBLED RESULT IN T3_04 IS NOT USED HERE
      SAVEZ     =>MESH3D%Z%R
      MESH3D%Z%R=>ZPROP%R
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VECTOR'
      CALL VECTOR(T3_04,'=',FORMUL,IELM3,-1.D0,DM1,SVIDE,GRAZCO,
     &            UCONV,VCONV,SVIDE,MESH3D,MSK,MASKEL)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VECTOR'
      CALL FLUX_EF_VF_3D(FLODEL%R,MESH2D%W%R,MESH3D%W%R,
     &                   MESH2D%NSEG,MESH3D%NSEG,NELEM2,
     &                   MESH3D%NELEM,MESH2D,.TRUE.,
     &                   2,2,MESH3D%TYPELM+1,NPLAN,
!                          2: HORIZONTAL FLUXES FROM TOP TO BOTTOM
     &                   MESH3D%IKLE%I,MESH3D%NELMAX,MESH2D%KNOLG%I)
!     FLUX LIMITATION (FLULIM IS 2D, SO NUMBERING FROM TOP TO BOTTOM
!                      MAKES NO PROBLEM)
      IF(OPT_HNEG.EQ.2) THEN
        CALL FLUX3DLIM(FLODEL%R,FLULIM%R,NPLAN,MESH2D%NSEG,NPOIN2,1)
      ENDIF
      MESH3D%Z%R=>SAVEZ
!
!     NOW CALLING TEL4DEL WITH FLODEL COMPLETED
!
!     SENDS UCONV AND VCONV AS ADVECTING FIELD (SEE WAVE_EQUATION)
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TEL4DEL'
      CALL TEL4DEL(NPOIN3,NPOIN2,NELEM3,MESH2D%NSEG,
     &  MESH2D%IKLE%I,MESH2D%ELTSEG%I,MESH2D%GLOSEG%I,MESH2D%ORISEG%I,
     &  MESH2D%GLOSEG%DIM1,X,Y,MESH3D%NPTFR,LIHBOR%I,MESH3D%NBOR%I,
     &  NPLAN,AT,DT,LT,NIT,H%R,HPROP%R,MESH3D%Z%R,UCONV%R,
     &  VCONV%R,TA%ADR(MAX(IND_S,1))%P%R,TA%ADR(MAX(IND_T,1))%P%R,
     &  VISCVI%ADR(3)%P%R,TITCAS,
     &  T3D_FILES(T3DGEO)%NAME,T3D_FILES(T3DCLI)%NAME,WAQPRD,
     &T3D_FILES(T3DDL1)%LU,T3D_FILES(T3DDL1)%NAME,T3D_FILES(T3DDL2)%LU,
     &T3D_FILES(T3DDL2)%NAME,T3D_FILES(T3DDL3)%LU,
     &T3D_FILES(T3DDL3)%NAME,T3D_FILES(T3DDL5)%LU,
     &T3D_FILES(T3DDL5)%NAME,
     &  T3D_FILES(T3DDL6)%LU,T3D_FILES(T3DDL6)%NAME,
     &  T3D_FILES(T3DDL7)%LU,T3D_FILES(T3DDL7)%NAME,
     &  T3D_FILES(T3DL11)%LU,T3D_FILES(T3DL11)%NAME,
     &  T3D_FILES(T3DDL4)%LU,T3D_FILES(T3DDL4)%NAME,
     &  T3D_FILES(T3DDL8)%LU,T3D_FILES(T3DDL8)%NAME,
     &T3D_FILES(T3DDL9)%LU,T3D_FILES(T3DDL9)%NAME,T3D_FILES(T3DL10)%LU,
     &T3D_FILES(T3DL10)%NAME,INFOGR,NELEM2,SALI_DEL,TEMP_DEL,VELO_DEL,
     &DIFF_DEL,MARDAT,MARTIM,FLODEL%R,V2DPAR%R,MESH2D%KNOLG%I)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TEL4DEL'
!
      ENDIF
!
!----------------------------------------------------------------------
!
! MASKING
!
      IF(ISOUSI.EQ.NSOUSI) THEN
        IF(MSK) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASK3D'
        IF(MSK) CALL MASK3D(MESH3D%IFABOR%I,MASKEL%R,MASKPT,MASKBR%R,
     &          X2%R,Y2%R,ZF%R,ZFE%R,H%R,HMIN,AT,LT,IT1%I,
     &          MESH3D%NELBOR%I,NELMAX2,NELEM2,NPOIN2,NPTFR2,
     &          NPLAN,NETAGE,IELM3,MESH2D)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MASK3D'
        ENDIF
      ENDIF
!
! COMPUTES SURFACE GRADIENTS AT TIME LEVEL N+1 AND DSSUDT
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FSGRAD'
      CALL FSGRAD(GRADZS,ZFLATS,Z(NPOIN3-NPOIN2+1:NPOIN3),
     &            ZF,IELM2H,MESH2D,MSK,MASKEL,
     &            UNSV2D,T2_01,NPOIN2,OPTBAN,SVIDE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FSGRAD'
!
      CALL OS( 'X=Y-Z   ', X=DSSUDT, Y=H, Z=HN )
      CALL OS( 'X=CX    ', X=DSSUDT, C=1.D0/DT )
!
! COMPUTES THE VOLUMES ASSOCIATED WITH NODES
!
      CALL VECTOR(VOLU, '=', 'MASBAS          ',IELM3,1.D0-AGGLOH,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
      IF(AGGLOH.GT.1.D-6) THEN
        CALL VECTOR(VOLU, '+', 'MASBAS2         ',IELM3,AGGLOH,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
      ENDIF
      IF(NCSIZE.GT.1) THEN
        CALL OS('X=Y     ',X=VOLUPAR,Y=VOLU)
        CALL PARCOM(VOLUPAR,2,MESH3D)
      ENDIF
!
! IN 2D, ONLY IF MASKING (OTHERWISE NOTHING CHANGED)
!
      IF(MSK) CALL MASBAS2D(VOLU2D,V2DPAR,UNSV2D,
     &                      IELM2H,MESH2D,MSK,MASKEL,T2_01,SVIDE)
!
!-----------------------------------------------------------------------
! CONTINUITY STEP (NON-HYDROSTATIC OPTION) IN NEW MESH
!-----------------------------------------------------------------------
!
      IF(NONHYD.AND..NOT.DPWAVEQ) THEN
!
        IF(INFOGR) CALL MITTIT(19,AT,LT)
!
        CALL OS ('X=Y     ', X=W , Y=WD  )
!
!-----------------------------------------------------------------------
!
! COMPUTES THE DYNAMIC PRESSURE
!
!       WITH WAVE EQUATION, DYNAMIC PRESSURE HERE IS INCREMENTAL
!       THUS WITHOUT BOUNDARY CONDITIONS
        BC=.NOT.DPWAVEQ
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREDIV'
        CALL PREDIV(DP,U,V,W,INFOGR,BC,1,.TRUE.,.TRUE.,.TRUE.)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREDIV'
!
!-----------------------------------------------------------------------
! VELOCITY PROJECTION STEP
!-----------------------------------------------------------------------
!
        IF(INFOGR) CALL MITTIT(20,AT,LT)
!
        CALL VELRES(U%R,V%R,W%R,DP,
     &             T3_01,T3_02,T3_03,MSK,MASKEL,MESH3D,
     &             SVIDE,IELM3,NPLAN,OPTBAN,T3_04,.TRUE.,NPOIN3,NPOIN2,
     &             SIGMAG,IPBOT%I,AGGLOH)  
!
!       BOUNDARY CONDITIONS ON W AT THE BOTTOM AND FREE SURFACE
!
!       FREE SURFACE (NOT ALWAYS TO BE DONE, DSSUDT IS SOMETIMES TOO BIG)
!
        IF(CLDYN) THEN
!
          CALL OV('X=Y     ',W%R(NPOIN3-NPOIN2+1:NPOIN3),DSSUDT%R,
     &                       DSSUDT%R,0.D0,NPOIN2)
          CALL OV('X=X+YZ  ',W%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       GRADZS%ADR(1)%P%R,
     &                       U%R(NPOIN3-NPOIN2+1:NPOIN3),0.D0,NPOIN2)
          CALL OV('X=X+YZ  ',W%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       GRADZS%ADR(2)%P%R,
     &                       V%R(NPOIN3-NPOIN2+1:NPOIN3),0.D0,NPOIN2)
!
        ENDIF
!
!       BOTTOM
!
        IF(VELPROBOT) THEN
          IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
            DO I=1,NPOIN2
              DO IP=0,IPBOT%I(I)
                I3D=IP*NPOIN2+I
                W%R(I3D)=GRADZF%ADR(1)%P%R(I)*U%R(I3D)
     &                  +GRADZF%ADR(2)%P%R(I)*V%R(I3D)
              ENDDO
            ENDDO
          ELSE
            DO I=1,NPOIN2
              W%R(I)=GRADZF%ADR(1)%P%R(I)*U%R(I)
     &              +GRADZF%ADR(2)%P%R(I)*V%R(I)
            ENDDO
          ENDIF
        ENDIF
!
!       RE-ENSURES THE DIRICHLET BOUNDARY CONDITIONS AND U.N = 0
!
        CALL AIRWIK2(LIHBOR%I, UBORF%R, VBORF%R, WBORF%R,
     &               LIUBOF%I, LIVBOF%I, LIWBOF%I,
     &               UBORL%R, VBORL%R, WBORL%R,
     &               LIUBOL%I, LIVBOL%I, LIWBOL%I,
     &               UBORS%R, VBORS%R, WBORS%R,
     &               LIUBOS%I, LIVBOS%I, LIWBOS%I,
     &               U%R,V%R,W%R,MESH2D%XNEBOR%R,MESH2D%YNEBOR%R,
     &               NBOR2%I,NPTFR2,NPLAN,NPOIN2,KENT,KADH,KLOG,KENTU,
     &               MESH2D%KP1BOR%I,VELPROLAT)
!
      ENDIF ! IF NONHYD
!
!-----------------------------------------------------------------------
!     PREPARING SOURCE TERMS FOR ADVECTION-DIFFUSION STEP 
!-----------------------------------------------------------------------
!
!     PREPARING SOURCE TERMS FOR K-EPSILON AND K-OMEGA MODELS
!
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
!
        IF (INFOGR) CALL MITTIT(7,AT,LT)
!
        S0AK%TYPR='Q'
        S0EP%TYPR='Q'
        S1AK%TYPR='Q'
        S1EP%TYPR='Q'
!
        IF(ITURBV.EQ.3) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOUKEP'
        CALL SOUKEP(S0AK%R,S0EP%R,S1AK%R,S1EP%R,
     &              U,V,W,DELTAR,RI%R,T3_01,T3_02,T3_03,T3_04,
     &              T3_05,T3_06,T3_07,T3_08,T3_09,
     &              T3_10,AK%R,EP%R,C1,C2,CMU,GRAV,
     &              T3_11,NPOIN3,MSK,MASKEL,MESH3D,IELM3,SVIDE,DT,
     &              VENT,WIND,H,EBORS,NPOIN2,KMIN,EMIN,PRANDTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOUKEP'
!
        ENDIF
!
        IF(ITURBV.EQ.7) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOUKOM'
        CALL SOUKOM(S0AK,S0EP,S1AK,S1EP,U,V,W,
     &              DELTAR,T3_01,T3_02,T3_03,
     &              T3_04,T3_05,T3_06,T3_07,T3_08,
     &              T3_09,T3_10,T3_12,T3_13,
     &              T3_14,T3_15,T3_16,T3_17,
     &              ROTAT,AK,EP,ALPHA,BETA,BETAS,GRAV,
     &              T3_11,NPOIN3,MSK,MASKEL,MESH3D,IELM3,SVIDE,PRANDTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOUKOM'
!
        ENDIF
!
      ENDIF
!
!     PREPARING SOURCE TERMS FOR TRACERS
!
      IF(NTRAC.GT.0) CALL SOURCE_TRAC
!
!-----------------------------------------------------------------------
! ADVECTION-DIFFUSION STEP FOR ALL ADVECTED VARIABLES
!-----------------------------------------------------------------------
!
!     ALL ADVECTION SCHEMES EXCEPT SUPG
!
      IF (INFOGR .AND. (.NOT.NONHYD)) CALL MITTIT(9,AT,LT)
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREADV'
      CALL PREADV(W,WS,ZPROP,ISOUSI,LT,VOLU,VOLUN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREADV'
!
!-----------------------------------------------------------------------
!     NOW CVDF3D WILL DO SUPG AND DIFFUSION
!-----------------------------------------------------------------------
!
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
!
        CLKMIN = .TRUE.
        CLKMAX = .TRUE.
        YASEM3D = .FALSE.
        YAWCHU = .FALSE.
        NEWDIF = .TRUE.
        TETATRA=MIN(TETADI,1.D0)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR AK'
        CALL CVDF3D
     & (AK,AKC,AKN,VISCVI,SIGMAK,S0AK,.TRUE.,S1AK,.TRUE.,
     &  KBORL, KBORF, KBORS, AKBORL, AKBORF, AKBORS,
     &  BKBORL, BKBORF, BKBORS, LIKBOL, LIKBOF, LIKBOS,
     &  FLUX%R(1), FLUEXT,FLUEXTPAR,KMIN, CLKMIN, KMAX, CLKMAX,
     &  SCHCKE,SCHDKE,SLVDKE,TRBAKE,INFOGR,NEWDIF,CALCFLU(4),
     &  T2_01,T2_02,T2_03,
     &  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     &  W1,NPTFR3,MMURD,MURD_TF,VOLU,VOLUPAR,VOLUN ,VOLUNPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     &  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,IT2,
     &  TRAV3,MESH2D,MATR2H,H,OPTBAN,OPTDIF,TETATRA,
     &  YAWCHU,WCHU,AGGLOD,NSCE,SOURCES,AK_SCE%R,
     &  NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,CALCRAIN(4),
     &  PLUIE,PARAPLUIE,0.D0,
     &  FLODEL,FLOPAR,SIGMAG,IPBOT%I,MAXADV,FLUDPT,FLUDP, FLUER,
     &  VOLU2D, V2DPAR, SETDEP)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR AK'
!
        CLEMIN  = .TRUE.
        CLEMAX  = .TRUE.
        YASEM3D = .FALSE.
        YAWCHU  = .FALSE.
!
!       NEGLECTS MOLECULAR DIFFUSIVITY...
!       DIFFUSION MATRIX NOT RECOMPUTED
        NEWDIF = .FALSE.
        CALL OM('M=CN    ',MDIFF,MDIFF,SVIDE,SIGMAE/SIGMAK,MESH3D)
!
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR EP'
        CALL CVDF3D
     & (EP,EPC,EPN,VISCVI,SIGMAE,S0EP,.TRUE.,S1EP,.TRUE.,
     &  EBORL, EBORF, EBORS, AEBORL, AEBORF, AEBORS,
     &  BEBORL, BEBORF, BEBORS, LIEBOL, LIEBOF, LIEBOS,
     &  FLUX%R(1), FLUEXT,FLUEXTPAR,EMIN, CLEMIN, EMAX, CLEMAX,
     &  SCHCKE,SCHDKE,SLVDKE,TRBAKE,INFOGR,NEWDIF,CALCFLU(5),
     &  T2_01,T2_02,T2_03,
     &  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     &  W1,NPTFR3,MMURD,MURD_TF,VOLU,VOLUPAR,VOLUN,VOLUNPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     &  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,IT2,
     &  TRAV3,MESH2D,MATR2H,H,OPTBAN,OPTDIF,TETATRA,
     &  YAWCHU,WCHU,AGGLOD,NSCE,SOURCES,EP_SCE%R,
     &  NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,CALCRAIN(5),
     &  PLUIE,PARAPLUIE,0.D0,FLODEL,FLOPAR,SIGMAG,IPBOT%I,
     &  MAXADV,FLUDPT,FLUDP, FLUER, VOLU2D, V2DPAR, SETDEP)
!
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR EP'
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTES THE VISCOSITIES VISCVI, VISCTA AND VISCTP
!
      IF(ITURBH.EQ.1.OR.ITURBV.EQ.1) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCOS'
        CALL VISCOS(VISCVI,VISCTA,DNUTAV,DNUTAH,
     &              DNUVIV,DNUVIH,NTRAC,ITURBH,ITURBV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCOS'
!
      ENDIF
!
      IF(ITURBV.EQ.2) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCLM'
        CALL VISCLM(VISCVI,VISCTA,RI,U,V,DELTAR,X3,Y3,Z3,H,
     &              T3_01, T3_02, T3_03, T3_04, T3_05, T3_06, T3_07,
     &              SVIDE, MESH3D, IELM3, GRAV, NPLAN,
     &              NPOIN3, NPOIN2, NTRAC, MSK, MASKEL,
     &              TA,MIXING,DAMPING,IND_T,DNUVIV,DNUTAV,KARMAN,
     &              PRANDTL,UETCAR,KFROT,RUGOF,ZF,LINLOG,IPBOT%I)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCLM'
!
      ENDIF
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCKE'
        CALL VISCKE(VISCVI,VISCTA,AK,EP,NTRAC,CMU,
     &              DNUVIH,DNUVIV,DNUTAH,DNUTAV,KMIN,EMIN,
     &              ITURBH,ITURBV,PRANDTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCKE'
!
      ENDIF
!
      IF(ITURBH.EQ.4) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISSMA'
        CALL VISSMA(VISCVI,VISCTA,
     &              DNUTAH,DNUVIH,DNUVIV,DNUTAV,
     &              U,V,W,T3_01,T3_02,T3_03,T3_04,T3_05,T3_06,
     &              SVIDE,MESH3D,
     &              IELM3,NTRAC,MSK,MASKEL,ITURBV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISSMA'
!
      ENDIF
!
      IF(ITURBH.EQ.7) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCKO'
        CALL VISCKO(VISCVI,VISCTA,ROTAT,AK,EP,NTRAC,CMU,
     &              DNUVIH,DNUVIV,DNUTAH,DNUTAV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCKO'
!
      ENDIF
!
      IF(OPTBAN.EQ.1) THEN
!
        CALL VISCLIP(VISCVI,VISCTA,H,NPLAN,NPOIN3,NPOIN2,NTRAC,HLIM)
!
      ENDIF
!
!=======================================================================
!     OIL SPILL MODEL (UNDER DEVELOPMENT IN MYGRHYCAR PROJECT)
!=======================================================================
!
      IF(SPILL_MODEL) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE OIL_SPILL_3D'
          CALL OIL_SPILL_3D(LT,IELM2H,MESH2D,NFLOT_MAX,T3D_FILES,
     &                     MAXLU_T3D,NPOIN2,T3DMIG,UCONV,VCONV,WCONV,
     &                     NFLOT,NPLAN,MESH3D,AT,DT,GRAV,CF,X,Y,Z,H,HN,
     &                     IELM3,NPOIN3,NELEM2,XFLOT,YFLOT,ZFLOT,SHPFLO,
     &                     SHZFLO,TAGFLO,ELTFLO,ETAFLO,FLOPRD,T3DFLO,
     &                     IT1,IT2,T3_01,T3_02,T3_03,MTRA1,MTRA2,VISCVI,
     &                     WIND,UNSV3D,NTRAC,TRN,TRAV3,ATABOS,T2_17,
     &                     T2_18,VENT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE OIL_SPILL_3D'
!
      ENDIF
!
!-----------------------------------------------------------------------
! ADVECTION-DIFFUSION OF TRACERS
!
      IF(NTRAC.GT.0) THEN
!
        IF (INFOGR) CALL MITTIT(5,AT,LT)
!
          SIGMTA = 1.D0
          TAMIN  = 0.D0
          TAMAX  = 1.D0
          CTAMIN = .FALSE.
          CTAMAX = .FALSE.
          YASEM3D = .FALSE.
          NEWDIF = .TRUE.
          TETATRA=MIN(TETADI,1.D0)
!
        DO ITRAC = 1,NTRAC
!
          IF(SEDI.AND.ITRAC.EQ.NTRAC) THEN
            YAWCHU=.TRUE.
!           SOLVER STRUCTURE
            SLVD=SLVDSE
          ELSE
            YAWCHU=.FALSE.
!           SOLVER STRUCTURE
            SLVD=SLVDTA(ITRAC)
          ENDIF
!
          IF(DEBUG.GT.0) THEN
            WRITE(LU,*) 'APPEL DE CVDF3D POUR TRACEUR ',ITRAC
          ENDIF
!
          CALL CVDF3D
     &   (TA%ADR(ITRAC)%P,TAC%ADR(ITRAC)%P,TRN%ADR(ITRAC)%P,
     &   VISCTA%ADR(ITRAC)%P,SIGMTA,
     &   S0TA%ADR(ITRAC)%P,.TRUE.,S1TA%ADR(ITRAC)%P,.TRUE.,
     &   TABORL%ADR(ITRAC)%P,TABORF%ADR(ITRAC)%P,TABORS%ADR(ITRAC)%P,
     &   ATABOL%ADR(ITRAC)%P,ATABOF%ADR(ITRAC)%P,ATABOS%ADR(ITRAC)%P,
     &   BTABOL%ADR(ITRAC)%P,BTABOF%ADR(ITRAC)%P,BTABOS%ADR(ITRAC)%P,
     &   LITABL%ADR(ITRAC)%P,LITABF%ADR(ITRAC)%P,LITABS%ADR(ITRAC)%P,
     &   FLUX%R(5+ITRAC),FLUEXT,FLUEXTPAR,
     &   TAMIN,CTAMIN,TAMAX,CTAMAX,SCHCTA(ITRAC),
     &   SCHDTA,SLVD,TRBATA,INFOGR,NEWDIF,CALCFLU(5+ITRAC),
     &   T2_01,T2_02,T2_03,T3_01,T3_02,T3_03,T3_04,MESH3D,IKLE3,MASKEL,
     &   MTRA1,W1,NPTFR3,MMURD,MURD_TF,VOLU,VOLUPAR,VOLUN,VOLUNPAR,
     &   NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,NPLAN,LV,IELM3,MSUPG,
     &   IELM2H,IELM2V,MDIFF,MTRA2,INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,
     &   SVIDE,IT1,IT2,TRAV3,MESH2D,MATR2H,H,OPTBAN,OPTDIF,TETATRA,
     &   YAWCHU,WCHU,AGGLOD,NSCE,SOURCES,TA_SCE%ADR(ITRAC)%P%R,
     &   NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,CALCRAIN(5+ITRAC),
     &   PLUIE,PARAPLUIE,TRAIN(ITRAC), FLODEL,FLOPAR,SIGMAG,IPBOT%I,
     &    MAXADV,FLUDPT,FLUDP, FLUER, VOLU2D, V2DPAR, SETDEP)
!
!         NEWDIF=.FALSE. (POSSIBLE IF SIGMTA UNCHANGED)
!
          IF(DEBUG.GT.0) THEN
            WRITE(LU,*) 'RETOUR DE CVDF3D POUR TRACEUR ',ITRAC
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
! COMPUTES DELRA RHO / RHO FOR THE BUOYANCY TERMS
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DRSURR'
        CALL DRSURR(DELTAR,TA,BETAC,T0AC,T3_01,RHO0,RHOS,DENLAW,
     &              SEDI,NTRAC,IND_T,IND_S)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DRSURR'
!
      ENDIF
!
      END DO SUBITER
!
!-----------------------------------------------------------------------
!
! SEDIMENTOLOGY : BOTTOM TREATMENT
!
      IF(SEDI) THEN
!
!       FONVAS DOES ZF=ZR+HDEP, THUS HDEP MUST INCLUDE BEDLOAD
!       EROSION, HAS BEEN TAKEN INTO ACCOUNT INTO CLSEDI ABOVE
!
        CALL FONVAS(IVIDE%R,EPAI,CONC,TREST,TEMP%R,HDEP%R, 
     &              FLUDP%R,FLUDPT%R,FLUER%R,ZF%R,TA%ADR(NTRAC)%P%R,
     &              WCHU%R,T3_01%R,T3_02%R,T3_03%R,NPOIN2,NPOIN3,NPFMAX,
     &              NCOUCH,NPF%I,LT,DT,DTC,GRAV,RHOS,CFMAX,TASSE,ITASS,
     &              ZF_S%R,ESOMT%R,VOLU2D,MASDEP,SETDEP,ZR%R)  
!
      ENDIF
! TBE 
! modification for tilting flume (put before calculation of bottom gradient)
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TILT'
      CALL TILT(ZF%R, ZR%R, X, Y, NPOIN2, DT, AT, H%R)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TILT'
!
! UPDATES GEOMETRY IF THE BOTTOM HAS EVOLVED
!
      IF(INCLUS(COUPLING,'SISYPHE').OR.SEDI) THEN
!
!       COPIES MODIFIED BOTTOM TOPOGRAPHY INTO Z AND ZPROP
        CALL OV('X=Y     ',      Z(1:NPOIN2),ZF%R,ZF%R,0.D0,NPOIN2)
        CALL OV('X=Y     ',ZPROP%R(1:NPOIN2),ZF%R,ZF%R,0.D0,NPOIN2)
!       COMPUTES NEW BOTTOM GRADIENTS AFTER SEDIMENTATION
        CALL GRAD2D(GRADZF%ADR(1)%P,GRADZF%ADR(2)%P,ZPROP,NPLAN,SVIDE,
     &              UNSV2D,T2_02,T2_03,T2_04,
     &              IELM2H,MESH2D,MSK,MASKEL)
!       COMPUTES NEW Z COORDINATES
        CALL CALCOT(Z,H%R)
!       USEFUL ? NOT SURE, IS DONE AT EACH TIMESTEP ELSEWHERE, SO..
!       CALL CALCOT(ZPROP%R,HPROP%R)
        CALL FSGRAD(GRADZS,ZFLATS,Z(NPOIN3-NPOIN2+1:NPOIN3),
     &              ZF,IELM2H,MESH2D,MSK,MASKEL,
     &              UNSV2D,T2_01,NPOIN2,OPTBAN,SVIDE)
        CALL VECTOR(VOLU, '=', 'MASBAS          ',IELM3,1.D0-AGGLOH,
     &       SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
        IF(AGGLOH.GT.1.D-6) THEN
          CALL VECTOR(VOLU, '+', 'MASBAS2         ',IELM3,AGGLOH,
     &        SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
        ENDIF
        IF(NCSIZE.GT.1) THEN
          CALL OS('X=Y     ',X=VOLUPAR,Y=VOLU)
          CALL PARCOM(VOLUPAR,2,MESH3D)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! PREPARES 2D AND 3D OUTPUT
!
!     ALWAYS CALLED (SEE E.G. OUTPUT MAXZ)
!
      CALL PRERES_TELEMAC3D(LT)
!
!     CALLED IF OUTPUTS REQUESTED
!
      IF(MOD(LT,GRAPRD).EQ.0.AND.LT.GE.GRADEB) THEN
!
! 3D OUTPUT
!
      CALL BIEF_DESIMP(T3D_FILES(T3DRES)%FMT,VARSO3,
     &                 HIST,0,NPOIN3,T3D_FILES(T3DRES)%LU,BINRES,AT,LT,
     &                 LISPRD,GRAPRD,
     &                 SORG3D,SORIM3,MAXVA3,TEXT3,GRADEB,LISDEB)
!
! 3D OUTPUT FOR RESTART
!
      IF(LT.EQ.NIT.AND.RESTART_MODE
     &            .AND.T3D_FILES(T3DRST)%NAME(1:1).NE.' ') THEN
        CALL BIEF_DESIMP(T3D_FILES(T3DRST)%FMT,VARSO3,HIST,0,NPOIN3,
     &                   T3D_FILES(T3DRST)%LU,BINRES,AT,LT,
     &                   1,NIT,
     &                   SOREST,SORIS3,MAXVA3,TEXT3,1,NIT)
      ENDIF
!
! 2D OUTPUT
!
      CALL BIEF_DESIMP(T3D_FILES(T3DHYD)%FMT,VARSOR,
     &                 HIST,0,NPOIN2,T3D_FILES(T3DHYD)%LU,BINHYD,AT,LT,
     &                 LISPRD,GRAPRD,
     &                 SORG2D,SORIMP,MAXVAR,TEXTE,GRADEB,LISDEB)
!
      ENDIF
!
! SEDIMENTOLOGY OUTPUT
!
      IF(SEDI.AND.T3D_FILES(T3DSED)%NAME(1:1).NE.' ') THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DESSED'
        CALL DESSED(NPF%I,IVIDE%R,EPAI,HDEP%R,
     &              CONC,TEMP%R,ZR%R,NPOIN2,NPFMAX,
     &              NCOUCH,NIT,GRAPRD,LT,DTC,TASSE,GIBSON,
     &              T3D_FILES(T3DSED)%LU,TITCAS,BIRSED,0)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DESSED'
      ENDIF
!
! SCOPE OUTPUT: CROSS-SECTIONS, SECTIONS, ETC.
!
      CALL SCOPE(U%R,V%R,W%R,H%R,ZF%R,X,Y,Z,T3_01%R,T3_02%R,T3_03%R,
     &           SURFA2%R,IKLE3%I,MESH2D%IFABOR%I,NELEM3,NELEM2,
     &           NPOIN2,NPOIN3,NETAGE,NPLAN,LT,AT,DT,NIT,
     &           T3D_FILES(T3DSCO)%LU,PRIVE)
!
! OPTIONAL USER OUTPUT
!
      CALL UTIMP(LT,AT,GRADEB,GRAPRD,LISDEB,LISPRD)
!
! SEDIMENT OUTPUT
!
      IF(SEDI) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE IMPSED'
        CALL IMPSED(IVIDE%R,EPAI,CONC,TEMP%R,HDEP%R,PDEPO%R,FLUER%R,
     &              ZR%R,ZF%R,TA%ADR(NTRAC)%P%R,WCHU%R,X,Y,
     &              NPOIN2,NPOIN3,NPFMAX,NCOUCH,NPF%I,LT,RHOS,CFMAX,
     &              CFDEP,EPAI0,TASSE,GIBSON,PRIVE,LISPRD)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE IMPSED'
      ENDIF
!
!=======================================================================
! DROGUES/FLOATS/BUOYS
!=======================================================================
!
      IF(NFLOT_MAX.GT.0.AND..NOT.SPILL_MODEL) THEN
!
        IF(SPHERI) THEN
          CALL OS('X=Y/Z   ',UCONV,UCONV,MESH2D%COSLAT)
          CALL OS('X=Y/Z   ',VCONV,VCONV,MESH2D%COSLAT)
        ENDIF
!
!       ADDING AND REMOVING DROGUES
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING FLO3D'
        CALL FLOT3D(XFLOT%R,YFLOT%R,ZFLOT%R,NFLOT,NFLOT_MAX,X,Y,Z,
     &              MESH3D%IKLE%I,
     &              MESH3D%NELEM,MESH3D%NELMAX,NPOIN3,NPLAN,
     &              TAGFLO%I,SHPFLO%R,SHZFLO%R,ELTFLO%I,ETAFLO%I,
     &              MESH3D,LT,NIT,AT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM FLO3D'
!
        IF(INFOGR) CALL MITTIT(12,AT,LT)
!
!       MOVING THEM
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DERIVE'
        CALL DERIVE(UCONV%R,VCONV%R,WCONV%R,DT,AT,
     &              X,Y,Z,
     &              MESH2D%IKLE%I,MESH3D%IFABOR%I,LT,IELM3,UCONV%ELM,
     &              3,NPOIN3,NPOIN2,NELEM2,MESH2D%NELMAX,
     &              MESH2D%SURDET%R,XFLOT%R,YFLOT%R,ZFLOT%R,
     &              SHPFLO%R,SHZFLO%R,TAGFLO%I,ELTFLO%I,ETAFLO%I,
     &              NFLOT,NFLOT_MAX,FLOPRD,MESH3D,T3D_FILES(T3DFLO)%LU,
     &              IT1%I,T3_01%R,T3_02%R,T3_03%R,IT2%I,
!                                              NO STOCHASTIC DIFFUSION
     &              MTRA1%X%R,MTRA2%X%R,NPOIN3,0,SVIDE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DERIVE'
!
        IF(SPHERI) THEN
          CALL OS('X=XY    ',UCONV,MESH2D%COSLAT)
          CALL OS('X=XY    ',VCONV,MESH2D%COSLAT)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! MASS BALANCE FOR THE CURRENT TIMESTEP
!
      IF (BILMAS) THEN
!
        IF (.NOT.INFMAS) INFOGR = .FALSE.
        INFOGR = INFOGR .AND. LISTIN
        IF (INFOGR) CALL MITTIT(10,AT,LT)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASS3D'
        CALL MASS3D(INFOGR,LT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MASS3D'
!
!
        IF(SEDI) THEN
!           
!         DETERMINE MASSUSP: MASS IN SUSPENSION
!         MASBED: MASS OF SEDIMENT BED
!         MASDEP: DEPOSITED MASS
          MASSUSP = MASSE%R(5+NTRAC)
!         INITIALISATION
          IF(LT.EQ.1) MASSUSP0 = MASSUSP
!
          IF(INFOGR) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SED3D'
            CALL SED3D(MASBED,MASBED0, MASDEP, WCHU%R,TA%ADR(NTRAC)%P%R,
     &                 EPAI,HDEP%R,CONC,FLUER%R,FLUDP%R,T2_01%R,
     &                 NELEM2,NPOIN2,NPOIN3,NPFMAX,NCOUCH,
     &                 NPF%I,AT,TASSE,GIBSON,RHOS,VOLU2D%R)
!
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SED3D'
          ENDIF
!
        ENDIF
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIL3D'
        CALL BIL3D(LT,MESH3D%IKLBOR%I,NPTFR2,NETAGE,NELEM2)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIL3D'
!
      ENDIF
!
! COMPARES WITH REFERENCE FILE
!
      IF(VALID) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_VALIDA'
        CALL BIEF_VALIDA(TRAV3,TEXTP3,
     &                   T3D_FILES(T3DREF)%LU,T3D_FILES(T3DREF)%FMT,
     &                   VARSO3,TEXT3,
     &                   T3D_FILES(T3DRES)%LU,T3D_FILES(T3DRES)%FMT,
     &                   MAXVA3,NPOIN3,LT,NIT,ALIRE3D)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_VALIDA'
      ENDIF
!
!
! CHECKS VALUES SHARED BETWEEN SUBDOMAINS
!
!     CALL CHECK_DIGITS(H ,T2_01,MESH2D)
!     CALL CHECK_DIGITS(U ,T3_01,MESH3D)
!     CALL CHECK_DIGITS(V ,T3_01,MESH3D)
!     CALL CHECK_DIGITS(W ,T3_01,MESH3D)
!     IF(NTRAC.GT.0) THEN
!       DO ITRAC=1,NTRAC
!         CALL CHECK_DIGITS(TA%ADR(ITRAC)%P,T3_01,MESH3D)
!       ENDDO
!     ENDIF
!
!
!
! END OF TIME LOOP
!
      END DO TIMELOOP
!
!=======================================================================
! THE TIME LOOP ENDS HERE
!=======================================================================
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    
!                    *****************
                     SUBROUTINE TILT
!                    *****************
!
     &(ZF, ZR, X, Y, NPOIN2, DT, AT, H)
!
!***********************************************************************
! TELEMAC3D   V6P3                                   29/10/2013
!***********************************************************************
!
!brief   TILTING FLUME
!
!history  T BENSON (modified version of WEIR from LTOA project)
!+        29/10/13
!+        V6P3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| DT             |-->| TIMESTEP
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| X              |-->| MESH COORDINATE
!| Y              |-->| MESH COORDINATE
!| ZF             |<->| ELEVATION OF BOTTOM
!| ZR             |<->| level of non-erodable bed
!| H              |<->| water depth
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: ZF, ZR
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: H
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(IN) :: X, Y
      DOUBLE PRECISION, INTENT(IN) :: AT
      DOUBLE PRECISION, INTENT(IN) :: DT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! local variables
!
      INTEGER I
      DOUBLE PRECISION OMEGA,T,PI,M,DZ,FPRE,FNOW
!
      PI  = 4.D0*ATAN(1.D0)
      M = 12.0E-5  ! scaling factor relative to x for bed 
                   ! (gives +/-3.0m at +/-25km)
      T = 43200.0  ! tidal tilt period
!      
!
! WAIT A BIT BEFORE TILTING (FOR STABILITY)      
      IF ((AT.LT.T+T*3.0/4.0).AND.
     &   ((AT.LE.T/4.0).OR.(AT.GE.(T+T/4.0)))) THEN
         OMEGA = (2.0*PI)/T
         FNOW = SIN(OMEGA*AT)*M 
         FPRE = SIN(OMEGA*(AT-DT))*M 
!
         DO I = 1,NPOIN2
            ! calculate the new zr and zf
            DZ = FNOW*X(i) - FPRE*X(i)
            ZR(i) = ZR(i) + DZ
            ZF(i) = ZF(i) + DZ
         ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE TILT