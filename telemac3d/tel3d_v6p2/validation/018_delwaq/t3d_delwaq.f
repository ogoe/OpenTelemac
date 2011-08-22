!                         ********************
                          SUBROUTINE TELEMAC3D
!                         ********************
!
!
!=======================================================================
! TELEMAC 3D VERSION 5.9  22/07/2008  J-M HERVOUET (LNHE) 01 30 87 80 18   
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!
! K-OMEGA MODEL BY HOLGER WEILBEER (ISEB/UHA) NOW AT BAW HAMBURG
!
!
! 24/08/2005 (JMH) : NON-HYDROSTATIC STEP IN THE NEW MESH
! 22/02/2007 (JMH) : AIRWIK1 AND COUPLING WITH DELWAQ CHANGED
! 26/06/2008 (JMH) : ARGUMENTS DE TBORD
! 22/07/2008 (JMH) : CALL TO LIMI3D MOVED UP BEFORE CALL IFAB3D
! 11/09/2008 (JMH) : MESH3D%Z%R INSTEAD OF T3_01%R SENT TO TEL4DEL
! 25/09/2008 (JMH) : MESH3D%W%R ADDED AS LAST ARGUMENT OF TEL4DEL
!   
!=======================================================================
!   
!   THIS SUB. IS CALLED BY HOMERE_TELEMAC3D
!   CALLS ALL SUB. FROM LIBRARIES TELEMAC3D, SEDI, 
!   PARAVOID OR PARALLEL; 
!   AND SOME SUB. FROM LIBRARIES BIEF, TELEMAC2D, DAMOCLES, UTILE 
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE INTERFACE_TELEMAC3D
!     NE MARCHE PAS AVEC COMPILATEUR PGI
      USE INTERFACE_TELEMAC2D
!     USE INTERFACE_SISYPHE, ONLY: SISYPHE
!
      IMPLICIT NONE 
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!----------------------------------------------------------------------- 
! DECLARATIONS 
!----------------------------------------------------------------------- 
! VARIABLES FOR PARALLEL VERSION ARE DECLARED LOCALLY
!
      DOUBLE PRECISION P_TIME
      INTEGER P_IMAX
      EXTERNAL P_TIME,P_IMAX
!
!-----------------------------------------------------------------------
! DECLARATION OF LOCAL VARIABLES FOR TELEMAC3D
!-----------------------------------------------------------------------
!
      INTEGER LT, KINT, KEXT, DATE(3), TIME(3),NFRLIQ,NFRSOL
      INTEGER ITRAC, NVARCL, NVAR, ISOUSI,SCHDVI_HOR
      INTEGER, PARAMETER :: NSOR = 26 ! HERE MAXVAR FOR 2D
      INTEGER ALIRE2D(MAXVAR),TROUVE(MAXVAR+10),ALIRE3D(MAXVAR)
      INTEGER IBID,I,K
!
      DOUBLE PRECISION LAMBD0,TETADIVER
      DOUBLE PRECISION CDUM,MASSES
      DOUBLE PRECISION UMIN,  UMAX,  SIGMAU, VMIN,  VMAX, SIGMAV
      DOUBLE PRECISION WMIN,  WMAX,  SIGMAW
      DOUBLE PRECISION TAMIN, TAMAX, SIGMTA,TETATRA
!
      DOUBLE PRECISION HIST(1)
      DATA HIST /9999.D0/
!
      LOGICAL CALFLU
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
      INTEGER II,ITE
      DOUBLE PRECISION DT2D,ATT
!
      TYPE(BIEF_OBJ), POINTER :: UTILD,VTILD
      TYPE(SLVCFG) :: SLVD
!
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
!=======================================================================
!
!  VARIABLES TO BE READ WHEN CALLING SUITE :
!  0 : DO NOT READ    1 : READ  (SAME NUMBERING AS IN NOMVAR)
! 
!                  U V   H   ZF
      DATA ALIRE2D/1,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
! 
!     IN 3D FILES 
!                  Z U V W       K E     DP
      DATA ALIRE3D/1,1,1,1,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
!
!     READING TRACERS IN PREVIOUS FILES
!     WARNING, 14 BELOW IS NEXT IN NOMVAR, MAY CHANGE IF VARIABLES ARE
!     ADDED IN NOMVAR
!
      IF(NTRAC.GT.0) THEN
        DO I=14,14+NTRAC-1
          ALIRE3D(I)=1
        ENDDO
      ENDIF
!
!=======================================================================
! INITIALISATION: READING, PREPARING AND CHECKING 
!=======================================================================
! 
      LT     = 0       ! TIMESTEP INITIALISED
!     NUMBER OF SUB-ITERATIONS INITIALISED, LOOK IN PRECON
      ISOUSI = 0       
      NVARCL = 0 
      IF(NTRAC.GT.0) THEN
        TRAC=.TRUE.
      ELSE
        TRAC=.FALSE.
      ENDIF  
!
      DATE(1) = 0      ! DATE/TIME
      DATE(2) = 0      ! A THOROUGHLY Y2K-PROOF SOLUTION 
      DATE(3) = 0
      TIME(1) = 0
      TIME(2) = 0
      TIME(3) = 0
      INFOGR = LISTIN 
!
!-----------------------------------------------------------------------
!  
! 2D BOUNDARY CONDITIONS:
!
      CALL LECLIM                                                
     & (LIHBOR%I,LIUBOL%I,LIVBOL%I,IT4%I,HBOR%R,UBOR2D%R,VBOR2D%R,                                  
     &  T2_01%R,T2_02%R,T2_03%R,T2_04%R,NPTFR2,STDGEO,TRA2,NLIM,            
     &  KENT,KENTU,KSORT,KADH,KLOG,KINC,NUMLIQ%I,MESH2D)
!
! MESH ORGANISATION - 2D LEVEL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INBIEF POUR MESH2D'  
      CALL INBIEF                                                 
     & (LIHBOR%I, KLOG, IT1, IT2, IT3,                              
     &  LVMAC, IELMX, LAMBD0, SPHERI, MESH2D,                      
     &  T2_01, T2_02, OPTASS2D, PRODUC, EQUA)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INBIEF'
!
! CORRECTION OF THE NORMAL VECTORS AT THE POINTS WHERE 
! LIQUID AND SOLID BOUNDARIES MEET
!
      CALL CORNOR(MESH2D%XNEBOR%R,MESH2D%YNEBOR%R,                          
     *            MESH2D%XSGBOR%R,MESH2D%YSGBOR%R,                        
     *            MESH2D%KP1BOR%I,NPTFR2,KLOG,LIHBOR%I,
     *            T2_01,T2_02,MESH2D)
!     
! 3D BOUNDARY CONDITIONS (SO FAR SAME FILE AS 2D)
! T2_02 is AUBOR IN T2D, COULD BE KEPT
! THIS TIME BOUNDARY COLOURS ARE READ
!
      CALL LECLIM                                                
     & (LIHBOR%I,LIUBOL%I,LIVBOL%I,LITABL%ADR(1)%P%I,                       
     &  HBOR%R,UBORL%R,VBORL%R,TABORL%ADR(1)%P%R,AUBORL%R,
     &  ATABOL%ADR(1)%P%R,BTABOL%ADR(1)%P%R,                        
     &  NPTFR2,STDGEO,TRAC,NLIM,KENT,KENTU,KSORT,KADH,KLOG,KINC,                       
     &  NUMLIQ%I,MESH3D,BOUNDARY_COLOUR%I)
!
! MESH ORGANISATION - 3D LEVEL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INBIEF POUR MESH3D'
      CALL INBIEF(LIHBOR%I,KLOG,IT1,IT2,IT3,                              
     &            LVMAC,IELM3,LAMBD0,SPHERI,MESH3D,                      
     &            T3_01,T3_02,OPTASS,PRODUC,EQUA)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INBIEF'
!
! INITIALISATION OF 3D BOUNDARY CONDITION ATTRIBUTES FOR BOUNDARY NODES
! 2D CONDITIONS DUPLICATED ON THE VERTICAL 
!    
      CALL LIMI3D
!
! COMPLETING IFABOR IN 3D
!
      CALL IFAB3D                                                      
     & (MESH3D%IFABOR%I, LIUBOF%I, LIUBOL%I, LIUBOS%I,             
     &  MESH2D%KP1BOR%I, MESH2D%NELBOR%I,             
     &  MESH3D%NULONE%I, IKLE2%I,                     
     &  NELEM2, NPOIN2, NPTFR2, NPLAN, NPLINT, NETAGE,        
     &  KLOG,TRANSF)
!
! MESH CONTROL
! 
      CALL CHECK                                                    
     & (IKLE2%I, NBOR2%I, MESH2D%NELBOR%I, MESH3D%IKLBOR%I,        
     &  IKLE3%I, MESH3D%NELBOR%I, MESH3D%NULONE%I, NBOR3%I,       
     &  NELEM2, NPOIN2, NPTFR2, NETAGE, NELEM3, NPTFR3, NTRAC, 
     &  LISTIN )
!
! SEARCH FOR THE BOTTOM AND BOTTOM ROUGHNESS FIELDS IN THE GEOMETRY FILE
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FONSTR'
      CALL FONSTR                                                    
     & (T2_01,ZF,T2_02,RUGOF,NGEO,NFON,NOMFON,MESH2D,RUGOF0,LISTIN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FONSTR'
      IF(RUGOF%ELM.NE.11) CALL CHGDIS(RUGOF,11,RUGOF%ELM,MESH2D)
!
! PRIVATE VECTOR BLOCK INITIALISATION
! 
      IF(NPRIV.GT.0) CALL OS('X=0     ',X=PRIVE)
!
!-----------------------------------------------------------------------
! CORRECTION OF THE BOTTOM 
!
!  - SMOOTHEN ACCORDING TO THE LISFON VALUE
!  - CHANGE THE BOTTOM TOPOGRAPHY (FORTRAN)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORFON'
      CALL CORFON                                                     
     & (ZF,T2_01,T2_02,ZF%R,T2_01%R,T2_02%R,                         
     &  X,Y,PRIVE,NPOIN2,LISFON,.FALSE.,MASKEL,                           
     &  MATR2H,MESH2D,SVIDE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORFON'
!
! FINE ANALYSIS OF THE BATHYMETRY
! IN T2D CALLED IF (OPTBAN == 2)
! 
      IF(MSK) CALL TOPOGR                                                     
     & (ZF%R, T2_01%R, ZFE%R, IKLE2%I, MESH2D%IFABOR%I,                
     &  MESH2D%NBOR%I, MESH2D%NELBOR%I, MESH2D%NULONE%I,               
     &  IT1%I, IT2%I, IT3%I, NELEM2, NPTFR2, NPOIN2, MXPTVS2)
! 
!=======================================================================
! VARIOUS INITIALISATIONS
!=======================================================================
!
!     COUNTING LIQUID BOUNDARIES
!
      IF(NCSIZE.GT.1) THEN
       NFRLIQ=0
       DO I=1,NPTFR2
         NFRLIQ=MAX(NFRLIQ,NUMLIQ%I(I))
       ENDDO
       NFRLIQ=P_IMAX(NFRLIQ)
       WRITE(LU,*) ' '
       IF(LNG.EQ.1) WRITE(LU,*) 'NOMBRE DE FRONTIERES LIQUIDES :',NFRLIQ
       IF(LNG.EQ.2) WRITE(LU,*) 'NUMBER OF LIQUID BOUNDARIES:',NFRLIQ
      ELSE
       IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FRONT2'
       CALL FRONT2(NFRLIQ,NFRSOL,DEBLIQ,FINLIQ,DEBSOL,FINSOL,
     *             LIHBOR%I,LIUBOL%I,X,Y,NBOR2%I,MESH2D%KP1BOR%I,
     *             IT1%I,NPOIN2,NPTFR2,KLOG,LISTIN,NUMLIQ%I,MAXFRO)
       IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FRONT2'
      ENDIF
C
C=======================================================================
C
C     READING THE STAGE-DISCHARGE CURVES FILE
C
      IF(NOMPAR(1:1).NE.' ') THEN
        CALL T3D_READ_FIC_CURVES(NPAR,NFRLIQ,STA_DIS_CURVES,PTS_CURVES)
      ENDIF
!
!     TURBULENCE CONSTANTS (ALL MODELS)
! 
      CALL CSTKEP(KARMAN,CMU,C1,C2,SIGMAK,SIGMAE,VIRT,SCHMIT,
     *            KMIN,KMAX,EMIN,EMAX,PRANDTL,ALPHA,BETA,BETAS,OMSTAR,
     *            ITURBV)
!
! INITIALISATION OF DILATATION COEFFICIENTS AND THE REFERENCE VALUES
! FOR ALL ACTIVE TRACERS, DEFAULT RHO = RHO0 
!
!
! COMPUTE VERTICAL AND HORIZONTAL CORIOLIS PARAMETERS
!
      IF(NONHYD .AND. CORIOL) CALL CORPAR (FVER, FHOR, PHILAT)
!
!-----------------------------------------------------------------------
! INITIAL CONDITIONS READ FROM A 3D PREVIOUS COMPUTAION FILE
! OR SET IN FORTRAN 
!
      AKEP = .TRUE.
      AKOM = .TRUE.
!
!     STARTING FROM A 2D FILE (U,V AND H ARE READ TO BE USED IN CONDIM)
!                              AT IS ALSO INITIALISED
!
      IF(DEBU.AND.SUIT2) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SUITE AVEC UN FICHIER 2D'
        CALL SUITE (VARSOR,VARCL,IBID,NBI1,BINPRE,
     *              HIST,0,NPOIN2,AT,TEXTPR,VARCLA,
     *              NVARCL,TROUVE,ALIRE2D,LISTIN,.TRUE.,MAXVAR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SUITE'
      ENDIF
!
!     COPY OF THE BOTTOM TOPOGRAPHY INTO Z (= MESH3D%Z%R)
!     (IF IT IS A CONTINUATION Z WILL BE ALSO FOUND
!      IN THE PREVIOUS RESULTS FILE. ANYWAY THE COPY IS USEFUL HERE
!      TO AVOID A CRASH IN CONDIM)
!
      CALL OV('X=Y     ',Z(1:NPOIN2),ZF%R,ZF%R,0.D0,NPOIN2)
!
!     CONDIM IS NOW CALLED EVEN IF A COMPUTATION IS CONTINUED
!     (TO RETRIEVE ZSTAR)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CONDIM'
      CALL CONDIM
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CONDIM'
!
!     CHECKING WHEN ZSTAR HAS BEEN GIVEN BY USER
!     (THE MORE SO IF USER IS PIERRE LANG)
!
      IF(TRANSF.EQ.2) THEN
       DO I=2,NPLAN
        IF(ZSTAR%R(I-1).GE.ZSTAR%R(I)) THEN
         IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'ZSTAR FAUX, VERIFIER CONDIM'
          WRITE(LU,*) 'LE PLAN ',I,' DOIT ETRE AU-DESSUS DU PRECEDENT'
         ENDIF
         IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'ZSTAR NOT CORRECT, CHECK SUBROUTINE CONDIM'
          WRITE(LU,*) 'PLANE ',I,' MUST BE ABOVE PLANE ',I-1
         ENDIF
         CALL PLANTE(1)
         STOP
        ENDIF
       ENDDO
      ENDIF
!
!     POSSIBLE NEGATIVE DEPTHS SET BY USER CLIPPED HERE
!     
      CALL OS('X=+(Y,C)',X=H,Y=H,C=0.D0)
!
!     IF COMPUTATION CONTINUED, RETRIEVING SOME VARIABLES + Z + DEPTH
!
      IF(.NOT.DEBU) THEN
!
        CALL SUITE (VARSO3,VARCL,IBID,NPRE,BINPRE,
     *              HIST,0,NPOIN3,AT,TEXTP3,VARCLA,
     *              NVARCL,TROUVE,ALIRE3D,LISTIN,.TRUE.,MAXVA3,NPLAN)
!
        IF(RAZTIM) THEN
          AT=0.D0
          IF(LNG.EQ.1) WRITE(LU,*) 'TEMPS ECOULE REMIS A ZERO'
          IF(LNG.EQ.2) WRITE(LU,*) 'ELAPSED TIME RESET TO ZERO'
        ENDIF
!
        DO K=1,NPOIN2
          H%R(K)=Z(K+NPOIN2*(NPLAN-1))-Z(K)
        ENDDO
        CALL OV('X=Y     ',ZF%R,Z(1:NPOIN2),ZF%R,0.D0,NPOIN2)
!
!       SEE VARSO3 IN POINT FOR INDICES 8 AND 9 (K AND EPSILON)
        IF(TROUVE(8).EQ.1.AND.TROUVE(9).EQ.1) THEN
          AKEP=.FALSE.
        ENDIF
!
      ENDIF
!
!     SEDIMENT INITIALIZATION
!
      IF(SEDI) THEN
        IF(NOSUIS(1:1).EQ.' ') THEN
          CALL CONDIS(IVIDE%R,EPAI%R,TREST,CONC%R,TEMP%R,HDEP%R,          
     &                ZR%R,ZF%R,X,Y,NPOIN2,NPOIN3,NPF%I,NPFMAX,          
     &                NCOUCH,TASSE,GIBSON,PRIVE,CONSOL)
C
        ELSE
          CALL SUISED(IVIDE%R,EPAI%R,HDEP%R,CONC%R,TEMP%R,FLUER%R, 
     &                PDEPO%R,ZR%R,ZF%R,NPF%I,                   
     &                NPOIN2,NPOIN3,NPFMAX,NCOUCH,TASSE,GIBSON, 
     &                NSUIS,BISUIS)
        ENDIF
C       SO FAR CONSTANT MEAN DIAMETER=D50
        CALL OS('X=C     ',X=DMOY,C=D50)
      ENDIF
C
C CLIPPING OF H AND COMPUTATION OF Z, HPROP AND ZPROP
C NOTE : HMIN = -1000. IN DICTIONARY BUT HMIN = 0. IF OPTBAN=2
C
      IF(HMIN.GT.-999.D0) THEN
        CALL CLIP (H, HMIN, .TRUE., 1.D6, .FALSE., 0)
      ENDIF
C
C     INITIALISATION DE C0 (AJOUTE PAR JMH LE 26/08/99)
C     UTILISE DANS CONTIN ET PROPAG SI ONDE INCIDENTE
C     NORMALEMENT ANNULE PAR MASQUE MAIS PAS SI C0=INFINI
C     COMME SUR CRAY SI ON N'INITIALISE PAS
C     VOIR TELEMAC-2D OU ON APPELLE CONDIN MEME EN CAS DE
C     SUITE POUR AVOIR UN C0 CORRECT
C
      CALL OSBD( 'X=CY    ' , C0 , H  , H , GRAV , MESH2D )
      CALL OS  ( 'X=+(Y,C)' , X=C0 , Y=C0 , C=0.D0        )
      CALL OS  ( 'X=SQR(Y)' , X=C0 , Y=C0                 )
C
C     FIN D'INITIALISATION DE C0
C
      CALL CALCOT(Z,H%R)
!
      CALL HPROPA(HPROP, H, H, PROLIN, HAULIN, TETAH, NSOUSI)
!     A QUOI SERT LE OS SI ON REFAIT ZPROP APRES AVEC CALCOT ???
!     REPONSE : POUR FAIRE LE FOND (POURQUOI PAS OV AVEC NPOIN2 ?)
      CALL OS ( 'X=Y     ' ,X=ZPROP,Y=Z3)
      CALL CALCOT(ZPROP%R,HPROP%R)
!
! INITIALISATION OF THE MEAN VELOCITY IN 2D
!
      CALL VERMOY(U2D%R,V2D%R,U%R,V%R,2,Z,
     *            T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
      IF(QUABUB) THEN
        CALL CHGDIS(U2D,11,12,MESH2D)
        CALL CHGDIS(V2D,11,12,MESH2D)
      ENDIF
!
!-----------------------------------------------------------------------
! MASKING:
!
      IF(MSK) CALL MASK3D                                                      
     & (MESH3D%IFABOR%I, MASKEL%R, MASKPT%R, MASKBR%R,                 
     &  X2%R,Y2%R,ZF%R,ZFE%R,H%R,            
     &  HMIN,AT,LT,IT1%I,MESH2D%IFABOR%I,              
     &  MESH3D%NELBOR%I,IKLE2%I,                     
     &  NELMAX2,NELEM2,NPOIN2,NPTFR2,NPLAN,NETAGE,IELM3)
!
!-----------------------------------------------------------------------
! HARMONISATION  OF BOUNDARY CONDITIONS
! INITIALISATION OF BOUNDARY CONDITIONS FOR TELEMAC
!
      CALL LICHEK(LIMPRO%I,NPTFR2)
!
!-----------------------------------------------------------------------
! INITIALISATION DES VOLUMES ASSOCIES AUX NOEUDS
!
      CALL VECTOR(VOLU, '=', 'MASBAS          ',IELM3,1.D0-AGGLOH,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
      IF(AGGLOH.GT.1.D-6) THEN
        CALL VECTOR(VOLU, '+', 'MASBAS2         ',IELM3,AGGLOH,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
      ENDIF
!
      CALL OS('X=Y     ',X=VOLUN,Y=VOLU)
!
!     EN 2D     
!
      CALL MASBAS2D(VOLU2D,V2DPAR,UNSV2D,
     *              IELM2H,MESH2D,MSK,MASKEL,T2_01,SVIDE)
!
!-----------------------------------------------------------------------
! FREE SURFACE AND BOTTOM GRADIENTS
! INITIALISE DSSUDT = 0 
!
      CALL GRAD2D(GRADZF%ADR(1)%P,GRADZF%ADR(2)%P,Z3,1,SVIDE,
     *            UNSV2D,T2_02,IELM2H,MESH2D,MSK,MASKEL)
      CALL FSGRAD(GRADZS,ZFLATS,Z,ZF,IELM2H,MESH2D,MSK,MASKEL,
     *            UNSV2D,T2_01,NPOIN2,NPOIN3,OPTBAN,SVIDE)
!
      CALL OS('X=C     ',X=DSSUDT,C=0.D0)
!
! INITIALISE THE METEOROLOGICAL VARIABLES
!
      IF (VENT.OR.ATMOS) CALL METEO                                    
     &   (PATMOS%R, WIND%ADR(1)%P%R, WIND%ADR(2)%P%R, FUAIR, FVAIR,    
     &    X2%R, Y2%R, AT, LT, NPOIN2, VENT, ATMOS, H%R, T2_01%R,       
     &    GRAV, RHO0, 0.D0, PRIVE)
!
!-----------------------------------------------------------------------
! INITIALISATION OF K AND EPSILON 
! "SI AKEP = .FALSE. K ET EPSILON ONT ETE FAITS DANS LECSUI OU CONDIM"
!
      IF (ITURBV.EQ.3.AND.AKEP) CALL KEPINI(AK%R,EP%R,U%R,V%R,Z,                                
     &             ZF%R,NPOIN2,NPLAN,DNUVIH,DNUVIV,KARMAN,CMU,KMIN,EMIN)
!
      IF(ITURBV.EQ.7.AND.AKOM) THEN
        CALL OS('X=C     ',X=AK,C=KMIN)
        CALL OS('X=C     ',X=EP,C=EMIN)
        CALL OS('X=0     ',X=ROTAT)
      ENDIF    
!
!-----------------------------------------------------------------------
!
! CALCULATE (DELTA RHO)/RHO FOR THE INITIAL OUTPUT 
!
      CALL DRSURR(DELTAR,TA,BETAC,T0AC,T3_01,RHO0,RHOS,DENLAW,SEDI,
     &            NTRAC,IND_T,IND_S)
!
!-----------------------------------------------------------------------
!
!     INITIALISING U* FOR OUTPUT OF INITIAL CONDITIONS AND SISYPHE
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE COEFRO'
      CALL COEFRO(CF,H,U2D,V2D,KARMAN,KFROT,RUGOF,GRAV,MESH2D,T2_01) 
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE COEFRO' 
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TFOND'      
      CALL TFOND(AUBORF%R,CF%R,U2D%R,V2D%R,U%R,V%R,W%R,KARMAN,
     *           LISRUF,DNUVIV,Z,NPOIN2,KFROT,RUGOF%R,UETCAR%R,
     *           NONHYD)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TFOND'
!
!-----------------------------------------------------------------------
! 
!   COMPUTE THE VISCOSITIES VISCVI AND VISCTA
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
     &               T3_01, T3_02, T3_03, T3_04, T3_05, T3_06, T3_07,            
     &               SVIDE, MESH3D, IELM3, GRAV, NPLAN,                          
     &               NPOIN3, NPOIN2, NTRAC, MSK, MASKEL,        
     &               TA,MIXING,DAMPING,IND_T,DNUVIV,DNUTAV,KARMAN,  
     &               PRANDTL,UETCAR)
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCLM'
!      
      ENDIF
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCKE'
        CALL VISCKE(VISCVI,VISCTA,AK,EP,NTRAC,CMU,                 
     *              DNUVIH,DNUVIV,DNUTAH,DNUTAV,KMIN,EMIN,
     *              ITURBH,ITURBV)
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
        CALL VISCLIP(VISCVI,VISCTA,H,NPLAN,NPOIN3,NPOIN2,NTRAC)
!
      ENDIF
!
!-----------------------------------------------------------------------
! FLOATERS (EHM... TRACERS...)
!
      IF (NFLOT.NE.0) CALL FLOT3D                                      
     &   (XFLOT%R, YFLOT%R, ZFLOT%R, NFLOT, NITFLO, FLOPRD, X, Y, Z,   
     &    NPOIN3, DEBFLO%I, FINFLO%I, NIT)
!
!------------------------------------
! PREPARATION OF THE 3D OUTPUT FILE : 
!------------------------------------
!
!     OUTPUT FOR THE INITIAL CONDITIONS 
!
      IF (INFOGR) CALL MITTIT(1,AT,LT)
!
!     PREPARING THE 2D AND 3D OUTPUT FOR INITIAL CONDITIONS
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
         CALL TEL4DEL(NPOIN3,NPOIN2,NELEM3,MESH2D%NSEG,
     *                MESH2D%IKLE%I,MESH2D%ELTSEG%I,
     *                MESH2D%GLOSEG%I,MESH2D%GLOSEG%DIM1,
     *                X,Y,MESH3D%NPTFR,LIHBOR%I,
     *                MESH3D%NBOR%I,NPLAN,
     *                AT,DT,LT,NIT,MESH3D%Z%R,HPROP%R,U%R,V%R,
     *                TA%ADR(MAX(IND_S,1))%P%R,
     *                TA%ADR(MAX(IND_T,1))%P%R,
     *                VISCVI%ADR(3)%P%R,
     *                TITCAS,NOMGEO,NOMLIM,WAQPRD,
     *                NSOU,NOMSOU,NMAB,NOMMAB,NCOU,NOMCOU,
     *                NINI,NOMINI,NVEB,NOMVEB,NMAF,NOMMAF,
     *                NCOB,NOMCOB,NFRC,NOMFRC,NECO,NOMECO,
     *                NCOF,NOMCOF,NPARI,NOMPRI,
     *                INFOGR,DM1%R,ZCONV%R,HYDSTEP,NELEM2,
     *                SALI_DEL,TEMP_DEL,VELO_DEL,DIFF_DEL,MARDAT,MARTIM,
     *                FLODEL%R,.TRUE.,MESH3D%W%R)
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TEL4DEL'
!
      ENDIF
!
!     3D OUTPUT
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRGEO' 
      CALL ECRGEO(MESH3D%X%R,MESH3D%Y%R,NPOIN3,NBOR3%I,
     *            NRES,NVAR,TEXT3,VARCLA,NVARCL,TITCAS,SORG3D,MAXVA3,
     *            IKLE3%I,NELEM3,NPTFR3,NBPEL(IELM3),DATE,TIME,
     *            NCSIZE,NPTIR,MESH3D%KNOLG%I,NPLAN,
     *            I3=I_ORIG,I4=J_ORIG)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRGEO'
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DESIMP' 
      CALL DESIMP(VARSO3,HIST,0,NPOIN3,NRES,BINRES,AT,LT,LISPRD,GRAPRD,
     *            SORG3D,SORIM3,MAXVA3,TEXT3,GRADEB,LISDEB)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DESIMP' 
!
! SEDIMENTOLOGY OUTPUT
! 
      IF(SEDI.AND.NORSED(1:1).NE.' ') THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DESSED' 
        CALL DESSED(NPF%I,IVIDE%R,EPAI%R,HDEP%R,
     &              CONC%R,TEMP%R,ZR%R,NPOIN2,NPFMAX,
     &              NCOUCH,NIT,GRAPRD,LT,DTC,TASSE,GIBSON,
     &              NRSED,TITCAS,BIRSED,0)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DESSED'
      ENDIF
!
! COMPLETING THE DEPTH-AVERAGED VARIABLES IF QUASIBUBBLE 
!
      IF(QUABUB) THEN
        CALL CHGDIS(U2D,11,12,MESH2D)
        CALL CHGDIS(V2D,11,12,MESH2D)
      ENDIF
!
! PREPARATION OF THE 2D OUTPUT FILE : NHYD CHANNEL, NSOR VARIABLES (?)
!
      NVAR = NSOR
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRGEO' 
      CALL ECRGEO(MESH2D%X%R,MESH2D%Y%R,NPOIN2,NBOR2%I,
     &            NHYD,NVAR,TEXTE,VARCLA,NVARCL,TITCAS,SORG2D,MAXVAR,
     &            IKLE2%I,NELEM2,NPTFR2,3,DATE,TIME,NCSIZE,NPTIR,
     &            MESH2D%KNOLG%I,I3=I_ORIG,I4=J_ORIG)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRGEO' 
!
! 2D OUTPUT 
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DESIMP POUR 2D'
      CALL DESIMP(VARSOR,HIST,0,NPOIN2,NHYD,BINHYD,AT,LT,LISPRD,GRAPRD,
     &            SORG2D,SORIMP,MAXVAR,TEXTE,GRADEB,LISDEB)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DESIMP POUR 2D'
!
!-----------------------------------------------------------------------
! MASS BALANCE AND CUMULATIVE FLUXES INITIALISATION 
!
      IF (BILMAS) THEN
!
         CALL MITTIT(10,AT,LT)
!
         CALL MASS3D(.TRUE.,LT)
!
         CALL OS ( 'X=Y     ', X=MASINI, Y=MASSE)
         CALL OS ( 'X=0     ', X=FLUCUM         ) 
         CALL OS ( 'X=0     ', X=FLUX           )
!
         IF(SEDI) THEN 
           IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASSED'
           CALL MASSED(MASSE%R,TA%ADR(NTRAC)%P,X,Y,Z,
     &                 IVIDE%R,EPAI%R,CONC%R,HDEP%R,MESH2D%SURFAC%R,
     &                 T3_01,T3_02%R,SVIDE,IKLE2%I,MESH3D,
     &                 IELM3,NPLAN,NELEM2,NELEM3,NPOIN2,NPOIN3,
     &                 NTRAC,NVBIL,NPFMAX,NCOUCH,NPF%I,TASSE,GIBSON,
     &                 RHOS,CFDEP,MSK,MASKEL)
           IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MASSED'
         ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
! RETURN WHEN THE NUMBER OF REQUIRED TIME STEPS IS 0
!
      IF(NIT.EQ.0) THEN
         IF (LNG == 1) WRITE(LU,11)
         IF (LNG == 2) WRITE(LU,12)
         RETURN
      ENDIF
!
11    FORMAT(' ARRET DANS TELEMAC-3D, NOMBRE D''ITERATIONS DEMANDE NUL')
12    FORMAT(' BREAK IN TELEMAC-3D, NUMBER OF ITERATIONS ASKED NULL')
!
!-----------------------------------------------------------------------
!
      CALL OS ( 'X=Y     ' , X=UC, Y=U )
      CALL OS ( 'X=Y     ' , X=VC, Y=V )
!
      IF (NTRAC.NE.0) CALL OS ('X=Y     ', X=TAC, Y=TA )
!
! THE SAME FOR K AND EPSILON
!
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
        CALL OS ( 'X=Y     ', X=AKC, Y=AK )
        CALL OS ( 'X=Y     ', X=EPC, Y=EP )
      ENDIF
!
! INITIALISE THE HORIZONTAL VELOCITY AFTER DIFFUSION 
! IN ORDER TO ACCELERATE THE SOLVER CONVERGENCE
!
      CALL OS ( 'X=Y     ', X=UD, Y=U)
      CALL OS ( 'X=Y     ', X=VD, Y=V)
!
! INITIALIZE THE FREE SURFACE AND DIFFERENT VERTICAL VELOCITIES
!
      IF(NONHYD) THEN
        CALL OS ( 'X=Y     ', X=WC,  Y=W  )  
        CALL OS ( 'X=Y     ', X=WD,  Y=W  )  
      ENDIF
!
! SOURCE TERMS : FINDING LOCATION OF SOURCES (USED IN PRECON HEREAFTER)
!                WILL BE DONE AFTER AT EVERY TIME-STEP
!
      IF(NSCE.GT.0) THEN
!
!       IN THE 2D MESH -> ISCE
        CALL PROXIM(ISCE,XSCE,YSCE,MESH2D%X%R,MESH2D%Y%R,NSCE,NPOIN2,
     *              IKLE2%I,NELEM2,NELMAX2)
!       ON THE VERTICAL -> KSCE
        CALL FINDKSCE(NPOIN2,NPLAN,Z3%R,NSCE,ISCE,ZSCE,KSCE,INFOGR)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! PREPARATION OF THE VELOCITY ADVECTION BEFORE THE FIRST TIME STEP 
!
!     NEXT "CALL OS" ADDED BY JMH ON 19/12/2000
!     (WSCONV WAS NOT INITIALISED BEFORE GOING INTO SOLVE IN TRIDW2)
      CALL OS('X=0     ',X=WSCONV)
!
      IF(HYDSTEP.EQ.2) THEN
        CALL OS('X=Y     ',X=UCONV,Y=U)
        CALL OS('X=Y     ',X=VCONV,Y=V)
        CALL OS('X=0     ',X=NUWAVE)
        CALL OS('X=0     ',X=ZCONV)
        CALL OS('X=0     ',X=DM1)
      ENDIF
!     FIN PROVISOIRE
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
              TASCE2(I,ITRAC)=T3D_TRSCE(AT,I,ITRAC)
            ENDDO
          ENDDO
          IF(DEBUG.GT.0) WRITE(LU,*) 'FIN DES APPELS DE T3D_TRSCE'
        ENDIF
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'PREMIER APPEL DE SOURCES_SINKS'  
      CALL SOURCES_SINKS
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOURCES_SINKS'
      IF(DEBUG.GT.0) WRITE(LU,*) 'PREMIER APPEL DE PRECON'   
      CALL PRECON(U,V,W,WS,U,V,U2D,V2D,U2D,V2D,ZPROP,ISOUSI,LT)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DU PREMIER APPEL DE PRECON' 
!
!=======================================================================
!  MODIFS CAMILLE LEQUETTE POUR COUPLAGE SISYPHE
!=======================================================================
!
!     CAS D'UN COUPLAGE AVEC SISYPHE
!     ECRITURE DES CONDITIONS INITIALES DE U(Z=0), V(Z=0) ET H
!
      IF(COUPLING.NE.' ') THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'TELEMAC3D COUPLE AVEC : ',COUPLING
        IF(LNG.EQ.2) WRITE(LU,*) 'TELEMAC3D COUPLED WITH: ',COUPLING
      ENDIF
! 
!     IF(INCLUS(COUPLING,'INTER-SISYPHE')) THEN      
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
!       INITIALISATION DE U* AVANT CALCUL DANS LA BOUCLE EN TEMPS
!
        CALL COEFRO(CF,H,U2D,V2D,KARMAN,KFROT,RUGOF,GRAV,MESH2D,T2_01)        
        CALL TFOND(AUBORF%R,CF%R,U2D%R,V2D%R,U%R,V%R,W%R,KARMAN,
     *             LISRUF,DNUVIV,Z,NPOIN2,KFROT,RUGOF%R,UETCAR%R,
     *             NONHYD)
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
     *               LBID,IBID,LBID,CODE1,1,U,V,AT,VISCVI,DT,CHARR,SUSP,
!                          1 PRECLUDES THE USE OF THE FOLLOWING ARGUMENTS
     *               FLBOR,1,DM1,UCONV,VCONV,ZCONV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DU PREMIER APPEL DE SISYPHE' 
        CALL CONFIG_CODE(1)
!
!       ORIGINAL U AND V STRUCTURE RETRIEVED
!
        CALL CPSTVC(UN,U)
        CALL CPSTVC(VN,V)
!	   
      ENDIF
!
!=======================================================================
! FIN MODIFS CAMILLE LEQUETTE POUR INITIALISATION EN MODE COUPLAGE
!=======================================================================
!
!     INITIALISING THE SEDIMENT SETTLING VELOCITY
!     TURBULENCE NEGLECTED HERE. WCHU DONE HERE IS USED IN BORD3D
!     FOR ROUSE PROFILES.
!
      IF(SEDI) CALL VITCHU(WCHU,WCHU0)
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
!  SOURCES : COMPUTATION OF INPUTS WHEN VARYING IN TIME
!            IF NO VARIATION IN TIME QSCE2=QSCE AND TASCE2=TASCE
!=======================================================================
!
      IF(NSCE.GT.0) THEN
        DO I=1,NSCE
          QSCE2(I)=T3D_DEBSCE(AT,I,QSCE)
        ENDDO
        IF(NTRAC.GT.0) THEN
          DO I=1,NSCE 
            DO ITRAC=1,NTRAC                       
              TASCE2(I,ITRAC)=T3D_TRSCE(AT,I,ITRAC)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!  MODIFS CAMILLE LEQUETTE POUR COUPLAGE SISYPHE DANS LA BOUCLE EN TEMPS
!=======================================================================
!
!     CAS D'UN COUPLAGE INTERNE AVEC SISYPHE
!      
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
!       .AND.
!!!!!  PERCOU A RENTRER EN MOT-CLEF = PERIODE DE COUPLAGE !!!!!
!     &     (PERCOU*(LT/PERCOU)==LT.OR.LT==1)) THEN
!!!!!  METTRE ALORS LT*PERCOU EN ARGUMENT D'APPEL DE SISYPHE !!!!!
!
!       U AND V WITH 2D STRUCTURE : BOTTOM VELOCITY AS A 2D VARIABLE
        CALL CPSTVC(U2D,U)
        CALL CPSTVC(V2D,V)
!
!       HDEP MUST INCLUDE BEDLOAD AND SUSPENSION
        IF(SEDI) CALL OS('X=X-Y   ',X=HDEP,Y=ZF)
!
!       NOW RUN ONE TURN OF SISYPHE'S TIME LOOP AND RETURN
        CALL CONFIG_CODE(2)
        IBID=1
        LBID=.FALSE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SISYPHE'
        CALL SISYPHE(1,LT,GRAPRD,LISPRD,NIT,U2D,V2D,H,HN,ZF,UETCAR,
     *               CF,LBID,IBID,LBID,CODE1,1,U,V,AT,VISCVI,
     *               DT,CHARR,SUSP,
!                          1 PRECLUDES THE USE OF THE FOLLOWING ARGUMENTS
     *               FLBOR,1,DM1,UCONV,VCONV,ZCONV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SISYPHE'
        CALL CONFIG_CODE(1)
!
!       HDEP MUST INCLUDE BEDLOAD AND SUSPENSION
        IF(SEDI) CALL OS('X=X+Y   ',X=HDEP,Y=ZF)
!
!       ORIGINAL U AND V STRUCTURE RETRIEVED
        CALL CPSTVC(UN,U)
        CALL CPSTVC(VN,V)
!	   
      ENDIF
!
!=======================================================================
!  FIN MODIFS CAMILLE LEQUETTE POUR INITIALISATION EN MODE COUPLAGE
!=======================================================================
!  
! SAVING H, TA, TP, AK, EP 
! IN     HN,TAN,TPN,AKN,EPN
!
      CALL OS ( 'X=Y     ', X=HN,    Y=H     )
      CALL OS ( 'X=Y     ', X=VOLUN, Y=VOLU  )
      CALL OS ( 'X=Y     ', X=UN,    Y=U     )
      CALL OS ( 'X=Y     ', X=VN,    Y=V     )
      CALL OS ( 'X=Y     ', X=GRADZN,Y=GRADZS)
!
      IF (NONHYD) CALL OS ( 'X=Y     ' , X=WN, Y=W)
!
! IS IT OK FOR THE WHOLE BLOCKS (THEIR STRUCTURE IS IDENTIC!)
!
      IF (NTRAC.NE.0) CALL OS ('X=Y     ', X=TAN, Y=TA)
!
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
        CALL OS ( 'X=Y     ', X=AKN, Y=AK )
        CALL OS ( 'X=Y     ', X=EPN, Y=EP )
      ENDIF
!
      IF(BILMAS) CALL OS ( 'X=Y     ', X=MASSEN, Y=MASSE )
!
! COMPUTE MEAN UN AND VN IN THE VERTICAL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VERMOY'
      CALL VERMOY(UN2D%R,VN2D%R,UN%R,VN%R,2,Z,
     *            T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VERMOY'
!
! COMPLETING THE DEPTH-AVERAGED VARIABLES IF QUASIBUBBLE 
!
      IF(QUABUB) THEN
        CALL CHGDIS(UN2D,11,12,MESH2D)
        CALL CHGDIS(VN2D,11,12,MESH2D)
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTATION OF FRICTION COEFFICIENT
!
!     TIME VARIATIONS OF RUGOF (CORSTR IS IN TELEMAC-2D LIBRARY)
!     MUST BE USER-IMPLEMENTED OR DOES NOTHING
      CALL CORSTR
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE COEFRO'
      CALL COEFRO(CF,H,UN2D,VN2D,KARMAN,KFROT,RUGOF,GRAV,MESH2D,T2_01) 
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE COEFRO'
!
!-----------------------------------------------------------------------
!
! CHECKING AND HARMONISING THE BOUNDARY CONDITION TYPES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LICHEK'
      CALL LICHEK(LIMPRO%I,NPTFR2)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LICHEK'
!
! BOUNDARY CONDITIONS FOR THE K-EPSILON MODEL
!
      IF (ITURBV.EQ.3.OR.ITURBV.EQ.7) CALL KEPICL                                       
     &   (LIKBOF%I, LIEBOF%I, LIUBOF%I,                                 
     &    LIKBOL%I, LIEBOL%I, LIUBOL%I,                                 
     &    LIKBOS%I, LIEBOS%I, LIUBOS%I,                                
     &    NPTFR2, NPLAN, NPOIN2, KENT, KSORT, KADH, KLOG)
!
!-----------------------------------------------------------------------
! FORCING AT THE BOUNDARIES
!
! METEOROLOGICAL CONDITIONS
!
      IF (VENT.OR.ATMOS) CALL METEO                                   
     &   (PATMOS%R, WIND%ADR(1)%P%R, WIND%ADR(2)%P%R, FUAIR, FVAIR,   
     &    X2%R, Y2%R, AT, LT, NPOIN2, VENT, ATMOS, H%R, T2_01%R,      
     &    GRAV, RHO0, 0.D0, PRIVE)
!
!-----------------------------------------------------------------------
!
!     SEDIMENT
!
      IF(SEDI) THEN
!
!       COMPUTE THE SEDIMENT SETTLING VELOCITY
!
        IF(.NOT.TURBWC) THEN 
          CALL VITCHU(WCHU,WCHU0)
        ELSE
          CALL WCTURB(WCHU,WCHU0,U,V,W,H,RUGOF,LISRUF,T3_01,T3_02,T3_03,
     &    SVIDE,MESH3D,IELM3,NPOIN2,NPLAN,TURBA,TURBB,MSK,MASKEL,UETCAR)
        ENDIF
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
     &    IVIDE%R, EPAI%R, CONC%R, HDEP%R, FLUER%R,           
     &    PDEPO%R, LITABF%ADR(NTRAC)%P%I, LITABS%ADR(NTRAC)%P%I,       
     &    KLOG, NPOIN3, NPOIN2, NPLAN, NPFMAX, NCOUCH,                 
     &    NPF%I, ITURBV, DT, RHO0, RHOS,          
     &    CFDEP,TOCD,MPART,TOCE,TASSE,GIBSON, PRIVE,UETCAR%R,
     &    GRAV,SEDCO,DMOY,CREF,CF,AC,KSPRATIO)
!  
!         AFTER CLSEDI ATABOF AND BTABOF ARE NO LONGER NIL
          ATABOF%ADR(NTRAC)%P%TYPR='Q'
          BTABOF%ADR(NTRAC)%P%TYPR='Q'
!         ATABOS%ADR(NTRAC)%P%TYPR='Q'
!         BTABOS%ADR(NTRAC)%P%TYPR='Q'
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CLSEDI'
      ENDIF
!
! BOUNDARY CONDITION VALUES ACTUALISATION
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BORD3D'
      CALL BORD3D(AT,LT,INFOGR,NPTFR2,NFRLIQ)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BORD3D'
!
! BOUNDARY CONDITIONS FOR THE VELOCITY ON LATERAL BOUNDARIES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TBORD'
      CALL TBORD(AUBORL%R,LIUBOL%I,
     *           RUGOL%R,                     
     *           MESH2D%DISBOR%R,MESH2D%NELBOR%I,MESH2D%NULONE%I,
     *           MESH2D%IKLE%I,NELEM2,
     *           U%R,V%R,W%R,
     *           NBOR2%I,NPOIN2,NPLAN,NPTFR2,DNUVIH,DNUVIV,         
     *           KARMAN,LISRUL,KFROTL,         
     *           KENT,KENTU,KSORT,KADH,KLOG,UETCAL%R,NONHYD,
     *           T2_01%R,MESH2D)
      AUBORL%TYPR='Q'
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TBORD, APPEL DE TFOND'
!
! BOUNDARY CONDITIONS FOR THE VELOCITY ON THE BOTTOM
!
      CALL TFOND(AUBORF%R,
     *           CF%R,UN2D%R,VN2D%R,U%R,V%R,W%R,KARMAN,
     *           LISRUF,DNUVIV,Z,NPOIN2,KFROT,RUGOF%R,UETCAR%R,
     *           NONHYD)
      AUBORF%TYPR='Q'
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TFOND'
!
! BOUNDARY CONDITIONS FOR K-EPSILON MODEL AND COMPUTATION OF CONSTRAINTS 
! AT THE BOTTOM AND LATERAL BOUNDARIES IF K-EPSILON IS REQUIRED
!
      IF (ITURBV.EQ.3) THEN
         IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE KEPCL3'
         CALL KEPCL3(KBORF%R,  EBORF%R,LIKBOF%I, LIEBOF%I, LIUBOF%I,               
     *    KBORL%R,  EBORL%R,  LIKBOL%I, LIEBOL%I, LIUBOL%I,
     *    RUGOL%R,  KBORS%R,  EBORS%R,        
     *    LIKBOS%I, LIEBOS%I, LIUBOS%I,                              
     *    MESH2D%DISBOR%R, AK%R, U%R, V%R, H%R, Z, 
     *    NBOR2%I, NPOIN2, NPLAN, NPTFR2, DNUVIH, DNUVIV,         
     *    KARMAN, CMU, LISRUF, LISRUL,         
     *    VIRT,KMIN,KMAX,EMIN,EMAX,KENT,KENTU,KSORT,KADH,KLOG,
     *    UETCAR%R,UETCAL%R)
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE KEPCL3'
!
      ELSEIF (ITURBV.EQ.7) THEN
!
         IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE KOMCL3'
         CALL KOMCL3(KBORF%R,  EBORF%R,
     *    LIKBOF%I, LIEBOF%I, LIUBOF%I,             
     *    KBORL%R,  EBORL%R,      
     *    LIKBOL%I, LIEBOL%I, LIUBOL%I, RUGOL%R,                    
     *    KBORS%R,  EBORS%R,        
     *    LIKBOS%I, LIEBOS%I, LIUBOS%I,                              
     *    MESH2D%DISBOR%R,AK%R,EP%R,U%R,V%R,W%R,H%R,Z, 
     *    NBOR2%I, NPOIN2, NPLAN, NPTFR2, DNUVIH, DNUVIV,         
     *    KARMAN, ALPHA,BETA,BETAS,OMSTAR, SCHMIT, LISRUF, LISRUL,            
     *    VIRT,GRAV,KMIN,KMAX,EMIN,EMAX,KENTU,KENT,KSORT,KADH,KLOG,
     *    UETCAR%R,UETCAL%R)
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE KOMCL3'
!
      ENDIF
!
! CLIPPING OF HBOR
! NOTE : HMIN = -1000. IN DICTIONARY BUT HMIN = 0. IF OPTBAN=2
!
      IF(HMIN.GT.-999.D0) THEN
        CALL CLIP(HBOR, HMIN, .TRUE., 1.D6, .FALSE., 0)
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
     * (S0U%R,S0V%R, S0U,S0V,UN%R,VN%R,TA,X,Y,Z,
     *  T3_01%R, DELTAR, MESH3D, FCOR, CORIOL, NTRAC, LT,
     *  AT, DT, SURFA2%R, T3_02%R, T3_02, W1%R, 
     *  MESH3D%M%X%R(1:6*NELEM3),MESH3D%M%X%R(6*NELEM3+1:12*NELEM3), 
     *  SEDI, GRAV, NPOIN3, NELEM3, NPOIN2, NELEM2, NPLAN, NETAGE, 
     *  IKLE3%I, PRIVE, LV, MSK, MASKEL%R, INCHYD,
     *  VOLU,VOLU%R,SVIDE,IELM3,MASKEL,NREJEU,ISCE,KSCE,QSCE2,USCE,VSCE,
     *  IELM2H,GRADZS%ADR(1)%P,GRADZS%ADR(2)%P,Z3,T2_01, T2_02,MESH2D,
     *  T3_03, T3_03%R, T3_04, T3_04%R, LATIT, LONGIT, NORD,SMU,SMV,
     *  YASEM3D,SCHCVI,DENLAW)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TRISOU, APPEL DE SOURCE'
!
      CALL SOURCE(S0U, S0V, S0W, S1U, S1V, S1W,   
     *            U, V, WS, W,         
     *            VOLU, VOLUN,T3_01,
     *            NPOIN3, NTRAC, LT, AT, DT, PRIVE, NONHYD,
     *            NPOIN2, NSCE,ISCE,KSCE,QSCE2,USCE,VSCE,MAXSCE)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOURCE'
!
!=======================================================================
! THE SUB-ITERATIONS LOOP BEGINS HERE
!=======================================================================
!
      SUBITER: DO ISOUSI = 1,NSOUSI
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE HPROPA'
      CALL HPROPA(HPROP,HN,H,PROLIN,HAULIN,TETAH,NSOUSI)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE HPROPA APPEL DE CALCOT'
      CALL CALCOT(ZPROP%R,HPROP%R)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CALCOT'
!
      IF(ISOUSI.NE.1) THEN
!       REBUILDING THE INITIAL MESH
!       NOTE: EVOLUTION DE ZF NON PRISE EN COMPTE ICI, A VOIR
        CALL CALCOT(Z,HN%R)
        CALL OS('X=Y     ',X=VOLU,Y=VOLUN)
        CALL GRAD2D(GRADZF%ADR(1)%P,GRADZF%ADR(2)%P,Z3,1    ,SVIDE,
     &              UNSV2D,T2_02,IELM2H,MESH2D,MSK,MASKEL)
        CALL FSGRAD(GRADZS,ZFLATS,Z,ZF,IELM2H,MESH2D,MSK,MASKEL,
     *              UNSV2D,T2_01,NPOIN2,NPOIN3,OPTBAN,SVIDE)
      ENDIF
!
!     SOURCES AND SINKS OF WATER
!
      CALL OS ('X=0     ',X=SMH)
!     ZPROP IS TEMPORARILY PUT IN MESH3D%Z
      SAVEZ     =>MESH3D%Z%R
      MESH3D%Z%R=>ZPROP%R      
      CALL SOURCES_SINKS
!     RESTORING Z
      MESH3D%Z%R=>SAVEZ
!
!     WAVE EQUATION
!
      SCHDVI_HOR=SCHDVI
      YAS0U=.TRUE.
      YAS1U=.TRUE.
      IF(HYDSTEP.EQ.2) THEN
!
!     CONFIGURATION OF DIFFUSION PARAMETERS TO SKIP DIFFUSION 
!     AND SOURCE TERMS IN CVDF3D (THIS IS DONE IN WAVE_EQUATION)
!                                                   
      SCHDVI_HOR = 0
!     WHEN SCHCVI=2 DIFF3D IS CALLED AND SOURCE TERMS WOULD BE TREATED TWICE
      YAS0U=.FALSE.
      YAS1U=.FALSE.
!
!     DONE IN LECDON:
!
!     NO QUASI-BUBBLE WITH WAVE EQUATION
!     QUABUB = .FALSE.
!     NO DIFFERENCE BETWEEN 2D AND 3D TIME-STEPS
!     R3D2D = 1
!     NO PROPAGATION STEP (REPLACED BY CALL TO WAVE_EQUATION)
!     PROP = .FALSE.
!
      ENDIF
!
!
!
!
!-----------------------------------------------------------------------
! ADVECTION-DIFFUSION STEP FOR VELOCITY COMPONENTS 
!-----------------------------------------------------------------------
!
!
!
! USING AUBORL, AUBORF, AUBORS INSTEAD OF AVBORL, AVBORF, AVBORS
! IS NOT AN ERROR (?)
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
       CALFLU = .FALSE.
       YAWCHU = .FALSE.
!      YASEM3D = DONE IN TRISOU
       NEWDIF=.TRUE.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR U'
      CALL CVDF3D                                               
     * (UD,UC,UN,VISCVI,SIGMAU,S0U,YAS0U,S1U,YAS1U,
     *  UBORL, UBORF, UBORS, AUBORL, AUBORF, AUBORS,              
     *  BUBORL, BUBORF, BUBORS, LIUBOL, LIUBOF, LIUBOS,             
     *  FLUX%R(1), FLUEXT,  UMIN, CLUMIN, UMAX, CLUMAX,            
     *  SCHCVI,SCHDVI_HOR,SLVDVI,TRBAVI,INFOGR,NEWDIF,CALFLU,
     *  T2_01,T2_02,T2_03,
     *  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     *  W1 , NPTFR3 , MMURD , VOLU , VOLUN ,
     *  KADH,KENT,KSORT,KENTU,KDIR,KDDL,
     *  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     *  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     *  INCHYD,MASKBR,MASKPT,SMU,YASEM3D,SVIDE,IT1,IT2,
     *  TRAV3,MESH2D,MATR2H,MATR2V,H,OPTBAN,OPTDIF,TETADI,YAWCHU,WCHU,
     *  AGGLOD,NREJEU,SOURCES,USCE,NUMLIQ%I,DIRFLU,NFRLIQ,
     *  VOLUT,ZT,ZPROP,.FALSE.,PLUIE) 
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR U'
!
       SIGMAV = 1.D0
       VMIN   = 0.D0
       VMAX   = 1.D0
       CLVMIN = .FALSE.
       CLVMAX = .FALSE.
       CALFLU = .FALSE.
       YAWCHU = .FALSE.
!      YASEM3D = DONE IN TRISOU
!      MDIFF DEJA CALCULE POUR U    
       NEWDIF=.FALSE.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR V'
!     USE OF AUBORL,AUBORF,AUBORS IS NOT A MISTAKE
      CALL CVDF3D                                                 
     * (VD,VC,VN,VISCVI,SIGMAV,S0V,YAS0U,S1V,YAS1U,
     *  VBORL, VBORF, VBORS, AUBORL,AUBORF,AUBORS,               
     *  BVBORL, BVBORF, BVBORS, LIVBOL, LIVBOF, LIVBOS,         
     *  FLUX%R(1), FLUEXT,  VMIN, CLVMIN, VMAX, CLVMAX,      
     *  SCHCVI, SCHDVI_HOR, SLVDVI, TRBAVI, INFOGR,NEWDIF, CALFLU,    
     *  T2_01,T2_02,T2_03,
     *  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     *  W1 , NPTFR3 , MMURD , VOLU , VOLUN ,
     *  KADH,KENT,KSORT,KENTU,KDIR,KDDL,
     *  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     *  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     *  INCHYD,MASKBR,MASKPT,SMV,YASEM3D,SVIDE,IT1,IT2,
     *  TRAV3,MESH2D,MATR2H,MATR2V,H,OPTBAN,OPTDIF,TETADI,YAWCHU,WCHU,
     *  AGGLOD,NREJEU,SOURCES,VSCE,NUMLIQ%I,DIRFLU,NFRLIQ,
     *  VOLUT,ZT,ZPROP,.FALSE.,PLUIE) 
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR V'
!
      IF(NONHYD) THEN
!
       SIGMAW = 1.D0
       WMIN   = 0.D0
       WMAX   = 1.D0
       CLWMIN = .FALSE.
       CLWMAX = .FALSE.
       CALFLU = .FALSE.
       YASEM3D= .FALSE.
       YAWCHU = .FALSE.
!      MDIFF DEJA CALCULE POUR U ET V, SAUF SI EQUATION D'ONDE
       NEWDIF = .FALSE.
       IF(HYDSTEP.EQ.2) NEWDIF=.TRUE.
!      TETADI MAY BE EQUAL TO 2 FOR U AND V, WHEN THE WAVE EQUATION
!      IS USED, WE DO NOT DO THAT ON W SO FAR
       TETADIVER = MIN(TETADI,1.D0)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR W'
!       USE OF AUBORL,AUBORF,AUBORS IS NOT A MISTAKE
        CALL CVDF3D                                              
     * (WD,WC,WN,VISCVI,SIGMAW,S0W,.TRUE.,S1W,.TRUE., 
     *  WBORL, WBORF, WBORS, AUBORL, AUBORF, AUBORS,         
     *  BWBORL, BWBORF, BWBORS, LIWBOL, LIWBOF, LIWBOS,          
     *  FLUX%R(1), FLUEXT,  WMIN, CLWMIN, WMAX, CLWMAX,           
     *  SCHCVI, SCHDVI, SLVDVI, TRBAVI, INFOGR,NEWDIF,CALFLU,   
     *  T2_01,T2_02,T2_03,
     *  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     *  W1 , NPTFR3 , MMURD , VOLU , VOLUN ,
     *  KADH,KENT,KSORT,KENTU,KDIR,KDDL,
     *  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     *  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     *  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,IT2,
     *  TRAV3,MESH2D,MATR2H,MATR2V,H,OPTBAN,OPTDIF,
!                                    0 BECAUSE WSCE NOT PROGRAMMED YET
     *  TETADIVER,YAWCHU,WCHU,AGGLOD,0,SOURCES,VSCE,
     *  NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,.FALSE.,PLUIE) 
!
      ENDIF
!
!-----------------------------------------------------------------------
! COMPUTE MEAN UD AND VD IN THE VERTICAL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VERMOY POUR UD2D ET VD2D'
      CALL VERMOY(UD2D%R,VD2D%R,UD%R,VD%R,2,Z,
     *            T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VERMOY'
!
! COMPLETING THE DEPTH-AVERAGED VARIABLES IF QUASIBUBBLE 
!
      IF(QUABUB) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CHGDIS POUR UD2D ET VD2D'
        CALL CHGDIS(UD2D,11,12,MESH2D)
        CALL CHGDIS(VD2D,11,12,MESH2D)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CHGDIS'
      ENDIF
!
!-----------------------------------------------------------------------
! PRESSURE - CONTINUITY - FREE SURFACE STEP 
!-----------------------------------------------------------------------
!
      DT2D=DT/R3D2D
!
      IF(PROP) THEN 
!
!       SUB-ITERATIONS FOR 2D
!
        DO 301 ITE=1,R3D2D
!
        ATT=AT-DT+ITE*DT2D
!
        IF(ITE.GT.1) CALL OS ('X=Y     ', X=HN , Y=H )
        IF(INFOGR) CALL MITTIT(8,AT,LT)     
!
        IF(ITE.EQ.1) THEN
!
!         COMPUTE MEAN UCONV AND VCONV IN THE VERTICAL 
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VERMOY POUR UCONV2'
          CALL VERMOY(UCONV2%R,VCONV2%R,UCONV%R,VCONV%R,2,Z,
     *         T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VERMOY'
!
! COMPLETING THE DEPTH-AVERAGED VARIABLES IF QUASIBUBBLE 
!
          IF(QUABUB) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CHGDIS POUR UCONV2'
            CALL CHGDIS(UCONV2,11,12,MESH2D)
            CALL CHGDIS(VCONV2,11,12,MESH2D)
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CHGDIS'
          ENDIF
!
        ELSE
!
          CALL OS ( 'X=Y     ' , X=UCONV2 , Y=U2D )
          CALL OS ( 'X=Y     ' , X=VCONV2 , Y=V2D )
          CALL OS ( 'X=Y     ' , X=UD2D   , Y=U2D )
          CALL OS ( 'X=Y     ' , X=VD2D   , Y=V2D )
          CALL OS ( 'X=Y     ' , X=UN2D   , Y=U2D )
          CALL OS ( 'X=Y     ' , X=VN2D   , Y=V2D )
!
        ENDIF
! 
        IF(OPTT2D.EQ.2) THEN
          CALL OS ( 'X=Y     ', X=UC2D, Y=UN2D )
          CALL OS ( 'X=Y     ', X=VC2D, Y=VN2D )
        ENDIF
!
! 2D ADVECTION OF WATER DEPTH H USING CHARACTERISTICS
!
        IF(ICON2D(1).EQ.1.AND.CONV2D(1)) THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CHARAC'
          CALL CHARAC(FN2D,FC2D,FN2D%N,UCONV2,VCONV2,SVIDE,SVIDE,  
     &                DT,MESH2D%IFABOR,IELM2H,NPOIN2,1,1,  
     &                MSK,MASKEL,MATR2H%X,MATR2H%D,TRAV2,                 
     &                IT1%I,IT2%I,IT3%I,IT4%I,IT5%I,MESH2D,
     &                MESH2D%NELEM,MESH2D%NELMAX,
     &                MESH2D%IKLE,MESH2D%SURDET)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CHARAC'
!
! DIRICHLET BC ARE IMPOSED AFTER THE ADVECTION 
!
          IF(OPTT2D.EQ.2) THEN
          CALL MITCL2(UC2D%R, LIUBOL%I, NBOR2%I, UBOR2D%R,                          
     &               AT, NPOIN2, NPTFR2, KENT, KSORT, KADH, KLOG, KENTU)
          CALL MITCL2(VC2D%R, LIVBOL%I, NBOR2%I, VBOR2D%R,                          
     &               AT, NPOIN2, NPTFR2, KENT, KSORT, KADH, KLOG, KENTU)
          ENDIF
!
        ENDIF
!
!-----------------------------------------------------------------------
! 2D BOUNDARY CONDITIONS
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LIMTEL'
        CALL LIMTEL(UBOR2D%R,VBOR2D%R,UBORL%R,VBORL%R,Z, 
     *              T3_01%R,T3_02%R,T3_03%R,T3_04%R,
     *              NBOR2%I,NPTFR2,NPLAN,NPOIN2,OPTBAN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LIMTEL'
!
! PREPARE FINAL 3D U,V : U3D = UD3D + TS3D
! (SEE AIRWICK FOR FINAL U AND V 3D)
!
        CALL  OS ('X=Y     ', X=U, Y=UD )
        CALL  OS ('X=Y     ', X=V, Y=VD )
!
!-----------------------------------------------------------------------
! PRESSURE - CONTINUITY 2D WITH THE VARIABLE H : TELEMAC2D'S PROPAG
! 
CDECOU
CDECOU     INTERPOLATION DES GRANDEURS HBOR, UBOR2D ET VBOR2D
CDECOU
         IF (R3D2D.NE.1) THEN
CDECOU    INTERPOLATION DE HBOR
          DO 101 II=1,NPTFR2
C
          IF(LIHBOR%I(II).EQ.KENT) THEN
C
               IF (ITE.EQ.1) THEN
                 H1%R(II)=HN%R(NBOR2%I(II))
                 H2%R(II)=HBOR%R(II)
               ENDIF
C
               HBOR%R(II)=H1%R(II)+(ATT-(AT-DT))*
     *                    (H2%R(II)-H1%R(II))/DT
C
          ENDIF
101       ENDDO
C
CDECOU    INTERPOLATION DE UBOR2D
          DO II=1,NPTFR2
C
          IF(LIUBOL%I(II).EQ.5) THEN
C
               IF (ITE.EQ.1) THEN
                 U1%R(II)=UN2D%R(NBOR2%I(II))
                 U2%R(II)=UBOR2D%R(II)
               ENDIF
C
               UBOR2D%R(II)=U1%R(II)+(ATT-(AT-DT))*
     *                    (U2%R(II)-U1%R(II))/DT
C
          ENDIF
C
          ENDDO
C
CDECOU    INTERPOLATION DE VBOR2D
          DO II=1,NPTFR2
C
          IF(LIVBOL%I(II).EQ.5) THEN
C
               IF (ITE.EQ.1) THEN
                 V1%R(II)=VN2D%R(NBOR2%I(II))
                 V2%R(II)=VBOR2D%R(II)
               ENDIF
C
               VBOR2D%R(II)=V1%R(II)+(ATT-(AT-DT))*
     *                    (V2%R(II)-V1%R(II))/DT
C
          ENDIF
          ENDDO
C
CDECOU   FIN DE L'INTERPOLATION
         ENDIF
CDECOU
!
        CALL OV ( 'X=Y     ' , AUBOR2%R(1:NPTFR2), AUBORL%R(1:NPTFR2),
     &           AUBORL%R(1:NPTFR2), CDUM, NPTFR2 )
!
        IF(OPTT2D.EQ.2) THEN
!         COMPLETE SYSTEM IN TELEMAC-2D, NEED OF AN AVERAGE DIFFUSION
          UTILD => UC2D
          VTILD => VC2D
          IF(ROVAR2) THEN
!           AVERAGING VISCOSITY AND DENSITY VARIATIONS
!           AVERAGED DELTAR (PUT INTO DU2D WHICH IS NOT USED)
            CALL VERMOY(VISC2D%R,DU2D%R,VISCVI%ADR(1)%P%R,DELTAR%R,2, 
     &      Z,T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
          ELSE
!           AVERAGING ONLY VISCOSITY
            CALL VERMOY(VISC2D%R,DU2D%R,VISCVI%ADR(1)%P%R,DELTAR%R,1, 
     &      Z,T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
          ENDIF
!
!         BUILDING THE SOURCE TERMS FOR TELEMAC-2D
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROSOU'
!                           SMH : T2_02 PUT THERE TO AVOID OVERWRITING SMH
          CALL PROSOU(FU,FV,T2_02,    UN2D,VN2D,HN,GRAV,NORD,
     *                FAIR,WIND%ADR(1)%P,WIND%ADR(2)%P,VENT,HWIND,
     *                CORIOL,FCOR,.FALSE.,TSI,SVIDE,SVIDE,AT,LT,
     *                NSCE,NREJEU,QSCE2,ISCE,T2_01,MESH2D,MSK,MASKEL,
     *                .FALSE.,MARDAT,MARTIM,0.D0,OPTSOU,.FALSE.,0,
     *                SVIDE,NVARCL,VARCLA)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROSOU'
!
!         SMH IS ALWAYS USED AND TSI MAY BE SET TO .FALSE. BY PROSOU
          TSI=.TRUE.
!          
        ELSE
!         PARTIAL SYSTEM IN TELEMAC-2D
          UTILD => UD2D
          VTILD => VD2D
          ROVAR2=.FALSE.
          CALL OS('X=C     ',X=FU,C=0.D0)
          CALL OS('X=C     ',X=FV,C=0.D0)
        ENDIF
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROPAG'
        CALL PROPAG                                                   
     &  ( U2D, V2D, H, UCONV2, VCONV2, CONV2D, H0, C0, COTOND, PATMOS, 
     &    ATMOS, HPROP, UN2D, VN2D, HN, UTILD, VTILD, HC,           
     &    DH,DU2D,DV2D,DHN,VISC2D,VISC2D,FU,FV,SMH,MESH2D,ZF,
     &    MAT2D%ADR(1)%P, MAT2D%ADR(5)%P, MAT2D%ADR(9)%P,            
     &    MAT2D%ADR(2)%P, MAT2D%ADR(3)%P,                             
     &    MAT2D%ADR(4)%P, MAT2D%ADR(7)%P, MATR2H,                   
     &    MAT2D%ADR(6)%P, MAT2D%ADR(8)%P, MBOR2D,                    
     &    SEM2D%ADR(1)%P, SEM2D%ADR(2)%P, SEM2D%ADR(3)%P,            
     &    W1, UBOR2D, VBOR2D, AUBOR2, HBOR, DIRBOR,                    
!    ATTENTION TE3 MIS 3 FOIS (VOIR POUR QUELLE OPTION DE TELEMAC-2D)
     &    TE1,TE2,TE3,TE3,TE3,T2_01, T2_02, T2_03, T2_04, T2_05, T2_06, 
     &    T2_07, T2_08, LIMPRO, MASK, GRAV, RHO0, CF, DIFTEL,
     &    IORDRH, IORDRU, LT, ATT, DT2D, TETAH, TETAH, TETAU, TETAD,
     &    AGGLOH, AGGLOU, KDIR, INFOGR,KFROT2D, ICON2D, PRIVE, ISOUSI,
     &    .FALSE.,MASSES, TSI, OPTBAN, CORCON, OPTSUP, MSK, MASKEL, 
!                 DU2D = AVERAGE RHO ON THE VERTICAL
     &    MASKPT, DU2D, ROVAR2, MAT2D, SEM2D, UNK2D, TRAV2, SVIDE, 
!         TO AVOID INTEGRATING SMH AGAIN  :  OPTSOU = 2
     &    BVIDE, PRECCU, SOLSYS,CDUM, OPDVIT,OPTSOU,            
     &    NFRLIQ,SLVPRO,EQUA,.FALSE.,.FALSE.,ZFLATS,TETAZCOMP,
!         NFRLIQ             VERTIC    ADJO      
     &    UDEL,VDEL,DM12D,ZCONV,' ',FLBOR,
     &    SVIDE,SVIDE,SVIDE,VOLU2D,V2DPAR,UNSV2D)
!         BM1S   BM2S  CV1S
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROPAG'
!
! (MASKING, GRADIENT COMPUTATION, ETC. MOVED FURTHER)
!-----------------------------------------------------------------------
!                                                       ___
!  FINALISE THE CALCULATION OF 3D U AND V:  U3D = U3D - U3D + U2D
!  TREAT THE BOUNDARY CONDITIONS
!
        CALL AIRWIK1
     *   (LIHBOR%I, UBORF%R, VBORF%R, LIUBOF%I, LIVBOF%I,
     *    UBORL%R, VBORL%R, LIUBOL%I, LIVBOL%I,
     *    UBORS%R, VBORS%R, LIUBOS%I, LIVBOS%I,
     *    U%R,V%R,UD%R,VD%R,INCLUS(COUPLING,'DELWAQ'),U2D%R,V2D%R,
     *    MESH2D%XNEBOR%R,MESH2D%YNEBOR%R,NBOR2%I,
     *    T3_01%R, T3_02%R, T3_03%R, T3_04%R, T3_05%R,Z,
     *    NPTFR2, NPLAN, NPOIN2, KENT, KADH, KLOG, KENTU,OPTBAN,
     *    MESH2D%KP1BOR%I)
!
CDECOU
301     ENDDO
CDECOU  FIN DU DECOUPLAGE
C
      ELSEIF(HYDSTEP.NE.2) THEN  ! SO WHEN NO PROPAGATION STEP
!                                  AND NO WAVE EQUATION
!
        CALL OS ('X=Y     ', X=U   , Y=UD   )
        CALL OS ('X=Y     ', X=V   , Y=VD   )
        CALL OS ('X=Y     ', X=U2D , Y=UD2D )
        CALL OS ('X=Y     ', X=V2D , Y=VD2D )     
!
      ENDIF ! IF(PROP)
!
!-----------------------------------------------------------------------
! DIFFUSION AND PROPAGATION STEP BY WAVE_EQUATION
!-----------------------------------------------------------------------
!
      IF(HYDSTEP.EQ.2) THEN
        IF(INFOGR) CALL MITTIT(6,AT,LT)
!       ZPROP IS TEMPORARILY PUT IN MESH3D%Z
        SAVEZ     =>MESH3D%Z%R
!       ALL PROPAGATION WILL BE DONE WITH ZPROP INSTEAD OF Z
        MESH3D%Z%R=>ZPROP%R 
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WAVE_EQUATION'
        CALL WAVE_EQUATION(LT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WAVE_EQUATION' 
!       RESTORING Z
        MESH3D%Z%R=>SAVEZ
      ENDIF
!
!-----------------------------------------------------------------------
! LIMITATION OF NEGATIVE DEPTHS
!-----------------------------------------------------------------------
!
      IF(OPTBAN.EQ.1.AND.(PROP.OR.HYDSTEP.EQ.2)) THEN
!
!       LISSAGE CONSERVATIF DES VALEURS NEGATIVES DE LA HAUTEUR
!
!       1) VALEURS NEGATIVES MISES DANS T2_01 ET ENLEVEES DE H
!
        CALL OS( 'X=-(Y,C)' , X=T2_01 , Y=H     , C=0.D0 )
        CALL OS( 'X=X-Y   ' , X=H     , Y=T2_01 )
!
!       2) VALEURS NEGATIVES LISSEES (ICI DEUX FOIS)
!          ET MASQUAGE POUR NE PAS ETALER SUR LES BANCS DECOUVRANTS
!
        IF(OPTBAN.EQ.1) THEN
          CALL FILTER(T2_01,.TRUE.,T2_02,T2_03,
     *                MAT2D%ADR(1)%P,'MATMAS          ',
     *                1.D0,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     *                MESH2D,MSK,MASKEL,2)
        ENDIF
!
!       3) LES VALEURS NEGATIVES LISSEES SONT REMISES SUR H
!
        CALL OS( 'X=X+Y   ' , X=H , Y=T2_01 )
!
      ENDIF
!
! CLIPPING OF H AND COMPUTING Z
! NOTE : HMIN = -1000. IN DICTIONARY BUT HMIN = 0. IF OPTBAN=2
!
      IF(HMIN.GT.-999.D0) THEN
        CALL CLIP (H,HMIN,.TRUE., 1.D6, .FALSE., 0)
      ENDIF
!
!     NOUVEAU MAILLAGE AVEC NOUVELLE SURFACE LIBRE
!
      CALL CALCOT(Z,H%R)
!
!----------------------------------------------------------------------
! 
!     GENERATING DATA FOR DELWAQ
!
      IF(INCLUS(COUPLING,'DELWAQ')) THEN
!
      FORMUL = 'VGRADP       HOR'
      IF(SIGMAG.AND.HYDSTEP.EQ.1) FORMUL(7:7) = '2'
      IF(HYDSTEP.EQ.2)            FORMUL(8:8) = '2'
!
        IF(HYDSTEP.EQ.1) THEN
C
C       COMPATIBLE ADVECTING FIELD COMPUTED (SEE AIRWIK1 FOR UD AND VD)
        CALL OS ('X=CY    ',X=T3_02,Y=UD,C=     TETAU)
        CALL OS ('X=CY    ',X=T3_03,Y=VD,C=     TETAU)
        CALL OS ('X=X+CY  ',X=T3_02,Y=UN,C=1.D0-TETAU)
        CALL OS ('X=X+CY  ',X=T3_03,Y=VN,C=1.D0-TETAU)
!       ADVECTION FLUXES PER NODE (STORED IN MESH3D%W%R)
!       THE ASSEMBLED RESULT IN T3_04 IS NOT USED HERE
        SAVEZ     =>MESH3D%Z%R
        MESH3D%Z%R=>ZPROP%R  
        CALL VECTOR(T3_04,'=',FORMUL,IELM3,-1.D0,DM1,ZCONV,SVIDE,  
     &              T3_02,T3_03,SVIDE,MESH3D,MSK,MASKEL)
        MESH3D%Z%R=>SAVEZ
!
        CALL TEL4DEL(NPOIN3,NPOIN2,NELEM3,MESH2D%NSEG,
     *               MESH2D%IKLE%I,MESH2D%ELTSEG%I,
     *               MESH2D%GLOSEG%I,MESH2D%GLOSEG%DIM1,
     *               X,Y,MESH3D%NPTFR,LIHBOR%I,
     *               MESH3D%NBOR%I,NPLAN,
     *               AT,DT,LT,NIT,MESH3D%Z%R,HPROP%R,T3_02%R,T3_03%R,
     *               TA%ADR(MAX(IND_S,1))%P%R,
     *               TA%ADR(MAX(IND_T,1))%P%R,
     *               VISCVI%ADR(3)%P%R,
     *               TITCAS,NOMGEO,NOMLIM,WAQPRD,
     *               NSOU,NOMSOU,NMAB,NOMMAB,NCOU,NOMCOU,
     *               NINI,NOMINI,NVEB,NOMVEB,NMAF,NOMMAF,
     *               NCOB,NOMCOB,NFRC,NOMFRC,NECO,NOMECO,
     *               NCOF,NOMCOF,NPARI,NOMPRI,
     *               INFOGR,DM1%R,ZCONV%R,HYDSTEP,NELEM2,
     *               SALI_DEL,TEMP_DEL,VELO_DEL,DIFF_DEL,
     *               MARDAT,MARTIM,FLODEL%R,.TRUE.,MESH3D%W%R)
C
        ELSEIF(HYDSTEP.EQ.2) THEN
C
!       ADVECTION FLUXES PER NODE (STORED IN MESH3D%W%R)
!       THE ASSEMBLED RESULT IN T3_04 IS NOT USED HERE
        SAVEZ     =>MESH3D%Z%R
        MESH3D%Z%R=>ZPROP%R  
        CALL VECTOR(T3_04,'=',FORMUL,IELM3,-1.D0,DM1,ZCONV,SVIDE,  
     &              UCONV,VCONV,SVIDE,MESH3D,MSK,MASKEL)
        MESH3D%Z%R=>SAVEZ
C       UCONV AND VCONV SENT AS ADVECTING FIELD (SEE WAVE_EQUATION)
        CALL TEL4DEL(NPOIN3,NPOIN2,NELEM3,MESH2D%NSEG,
     *               MESH2D%IKLE%I,MESH2D%ELTSEG%I,
     *               MESH2D%GLOSEG%I,MESH2D%GLOSEG%DIM1,
     *               X,Y,MESH3D%NPTFR,LIHBOR%I,
     *               MESH3D%NBOR%I,NPLAN,
     *               AT,DT,LT,NIT,MESH3D%Z%R,HPROP%R,UCONV%R,VCONV%R,
     *               TA%ADR(MAX(IND_S,1))%P%R,
     *               TA%ADR(MAX(IND_T,1))%P%R,
     *               VISCVI%ADR(3)%P%R,
     *               TITCAS,NOMGEO,NOMLIM,WAQPRD,
     *               NSOU,NOMSOU,NMAB,NOMMAB,NCOU,NOMCOU,
     *               NINI,NOMINI,NVEB,NOMVEB,NMAF,NOMMAF,
     *               NCOB,NOMCOB,NFRC,NOMFRC,NECO,NOMECO,
     *               NCOF,NOMCOF,NPARI,NOMPRI,
     *               INFOGR,DM1%R,ZCONV%R,HYDSTEP,NELEM2,
     *               SALI_DEL,TEMP_DEL,VELO_DEL,DIFF_DEL,MARDAT,
     *               MARTIM,FLODEL%R,.TRUE.,MESH3D%W%R)
C
        ENDIF
C
      ENDIF
!
!----------------------------------------------------------------------
! 
! MASKING  
!
      IF(ISOUSI.EQ.NSOUSI) THEN
!
          IF(MSK) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASK3D'
          CALL MASK3D(MESH3D%IFABOR%I, MASKEL%R, MASKPT%R, MASKBR%R,
     &                X2%R,Y2%R,ZF%R,ZFE%R,H%R,
     &                HMIN,AT,LT,IT1%I,MESH2D%IFABOR%I,
     &                MESH3D%NELBOR%I,IKLE2%I,
     &                NELMAX2,NELEM2,NPOIN2,NPTFR2,NPLAN,NETAGE,IELM3)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MASK3D'
          ENDIF
!
      ENDIF
!
! COMPUTE SURFACE GRADIENTS AT TIME LEVEL N+1 AND DSSUDT
!
      CALL FSGRAD(GRADZS,ZFLATS,Z,ZF,IELM2H,MESH2D,MSK,MASKEL,
     *            UNSV2D,T2_01,NPOIN2,NPOIN3,OPTBAN,SVIDE)
!
      CALL OS( 'X=Y-Z   ', X=DSSUDT, Y=H, Z=HN )
      CALL OS( 'X=CX    ', X=DSSUDT, C=1.D0/DT )
!
!  COMPUTE THE VOLUMES ASSOCIATED WITH NODES
!
      CALL VECTOR(VOLU, '=', 'MASBAS          ',IELM3,1.D0-AGGLOH,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
      IF(AGGLOH.GT.1.D-6) THEN
        CALL VECTOR(VOLU, '+', 'MASBAS2         ',IELM3,AGGLOH,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
      ENDIF
!
!  IN 2D, ONLY IF MASKING (OTHERWISE NOTHING CHANGED)     
!
      IF(MSK) CALL MASBAS2D(VOLU2D,V2DPAR,UNSV2D,
     *                      IELM2H,MESH2D,MSK,MASKEL,T2_01,SVIDE)
!
!-----------------------------------------------------------------------
! CONTINUITY STAGE (NON-HYDROSTATIC OPTION) IN NEW MESH
!-----------------------------------------------------------------------
!
      IF(NONHYD) THEN
!
        IF(INFOGR) CALL MITTIT(18,AT,LT)
!
        CALL OS ('X=Y     ', X=W , Y=WD  )
!
!-----------------------------------------------------------------------
!
! COMPUTING THE DYNAMIC PRESSURE
!
        IF(HYDSTEP.EQ.2) THEN
!         WITH WAVE EQUATION, DYNAMIC PRESSURE HERE IS INCREMENTAL
!         THUS WITHOUT BOUNDARY CONDITIONS
          BC=.FALSE.
        ELSE
          BC=.TRUE.
        ENDIF
        CALL PREDIV(DP,U,V,W,INFOGR,BC)
!
!-----------------------------------------------------------------------
! VELOCITY PROJECTION STEP
!-----------------------------------------------------------------------
!      
        IF(INFOGR) CALL MITTIT(19,AT,LT)
!
C     CALL VECTOR                                                
C    * (T3_01, '=', 'GRADF          X', IELM3,1.D0, U, SVIDE, SVIDE,
C    *  SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
C     CALL VECTOR                                         
C    * (T3_01, '+', 'GRADF          Y', IELM3,1.D0, V, SVIDE, SVIDE,
C    *  SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
C     CALL VECTOR                                       
C    * (T3_01, '+', 'GRADF          Z', IELM3,1.D0, W, SVIDE, SVIDE,
C    *  SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
C     PRINT*,'DIV(U) AVANT PROJECTION =',DOTS(T3_01,T3_01)
!
!      KEY-WORD : CONSISTENT PROJECTION
!
       IF(CONPRO) THEN
!
         CALL PROVEL ( U, DP, UBORF, UBORL, UBORS,
     &                 LIUBOF, LIUBOL, LIUBOS, 
     &                 UMIN, CLUMIN, UMAX, CLUMAX, INFOGR )
         CALL PROVEL ( V, DP, VBORF, VBORL, VBORS, 
     &                 LIVBOF, LIVBOL, LIVBOS, 
     &                 VMIN, CLVMIN, VMAX, CLVMAX, INFOGR )  
         CALL PROVEL ( W, DP, WBORF, WBORL, WBORS,  
     &                 LIWBOF, LIWBOL, LIWBOS, 
     &                 WMIN, CLWMIN, WMAX, CLWMAX, INFOGR )
!  
        ELSE
!
         CALL VELRES( U,V,W,DP, 
     *                T3_01,T3_02,T3_03,T3_04,MSK, MASKEL, MESH3D,
     *                SVIDE,IELM3,NPTFR2,LIHBOR%I,NPLAN,KLOG,
     *                OPTBAN,TE3,ZF,H,T2_01,MESH2D,NELEM2)
!
        ENDIF
!
!     BOUNDARY CONDITIONS ON W AT THE BOTTOM AND THE FREE SURFACE
!
!     FREE SURFACE (NOT ALWAYS TO BE DONE, DSSUDT IS SOMETIMES TOO BIG)
!
      IF(CLDYN) THEN
!
      CALL OV('X=Y     ',W%R(NPOIN3-NPOIN2+1:NPOIN3),DSSUDT%R,
     *                   DSSUDT%R,0.D0,NPOIN2)
      CALL OV('X=X+YZ  ',W%R(NPOIN3-NPOIN2+1:NPOIN3),
     *                   GRADZS%ADR(1)%P%R, 
     *                   U%R(NPOIN3-NPOIN2+1:NPOIN3),0.D0,NPOIN2) 
      CALL OV('X=X+YZ  ',W%R(NPOIN3-NPOIN2+1:NPOIN3),
     *                   GRADZS%ADR(2)%P%R, 
     *                   V%R(NPOIN3-NPOIN2+1:NPOIN3),0.D0,NPOIN2)
!
      ENDIF
!
!     BOTTOM
!
      CALL OV('X=YZ    ',W%R,GRADZF%ADR(1)%P%R,U%R,0.D0,NPOIN2) 
      CALL OV('X=X+YZ  ',W%R,GRADZF%ADR(2)%P%R,V%R,0.D0,NPOIN2)
! 
!     RE-ENSURES THE DIRICHLET BOUNDARY CONDITIONS AND U.N = 0
!
      CALL AIRWIK2(LIHBOR%I, UBORF%R, VBORF%R, WBORF%R,
     &             LIUBOF%I, LIVBOF%I, LIWBOF%I,
     &             UBORL%R, VBORL%R, WBORL%R,
     &             LIUBOL%I, LIVBOL%I, LIWBOL%I,
     &             UBORS%R, VBORS%R, WBORS%R, 
     &             LIUBOS%I, LIVBOS%I, LIWBOS%I,
     &             U%R,V%R,W%R,MESH2D%XNEBOR%R,MESH2D%YNEBOR%R, 
     &             NBOR2%I,NPTFR2,NPLAN,NPOIN2,KENT,KADH,KLOG,KENTU,
     &             MESH2D%KP1BOR%I)    
!
!  PROVISOIRE : VERIFICATION
!
C     CALL VECTOR                                                
C    * (PRIVE%ADR(1)%P, '=', 'GRADF          X',
C    *  IELM3,1.D0, U, SVIDE, SVIDE,
C    *  SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
C     CALL VECTOR                                         
C    * (PRIVE%ADR(1)%P, '+', 'GRADF          Y',
C    *  IELM3,1.D0, V, SVIDE, SVIDE,
C    *  SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
C     CALL VECTOR                                       
C    * (PRIVE%ADR(1)%P, '+', 'GRADF          Z',
C    *  IELM3,1.D0, W, SVIDE, SVIDE,
C    *  SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
C     PRINT*,'DIV(U) APRES =',DOTS(PRIVE%ADR(1)%P,PRIVE%ADR(1)%P)
!
      ENDIF ! IF NONHYD
!
!-----------------------------------------------------------------------
! ADVECTION-DIFFUSION STEP FOR OTHER VARIABLES
!-----------------------------------------------------------------------
! PREPARE ADVECTION FOR ALL VARIABLES
!
      IF (INFOGR .AND. (.NOT.NONHYD)) CALL MITTIT(9,AT,LT)
      IF(DEBUG.GT.0) WRITE(LU,*) 'SECOND APPEL DE PRECON'
      CALL PRECON(U,V,W,WS,UN,VN,U2D,V2D,UN2D,VN2D,ZPROP,ISOUSI,LT)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DU SECOND APPEL DE PRECON'
!
! ADVECTION-DIFFUSION FOR K-EPSILON
!       
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
!
        IF (INFOGR) CALL MITTIT(7,AT,LT)
!
        IF(ITURBV.EQ.3) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOUKEP'
        S0AK%TYPR='Q'
        S0EP%TYPR='Q'
        S1AK%TYPR='Q'
        S1EP%TYPR='Q'
        CALL SOUKEP(S0AK%R,S0EP%R,S1AK%R,S1EP%R, 
     *              U,V,W,DELTAR,RI%R,T3_01,T3_02,T3_03,T3_04,
     *              T3_05,T3_06,T3_07,T3_08,T3_09,
     *              T3_10,AK%R,EP%R,C1,C2,CMU,GRAV,
     *              T3_11,NPOIN3,MSK,MASKEL,MESH3D,IELM3,SVIDE,DT,
     *              VENT,WIND,H,EBORS,NPOIN2,KMIN,EMIN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOUKEP'
!
        ENDIF
!
        IF(ITURBV.EQ.7) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOUKOM'
        CALL SOUKOM(S0AK,S0EP,S1AK,S1EP,U,V,W,
     *              DELTAR,T3_01,T3_02,T3_03,
     *              T3_04,T3_05,T3_06,T3_07,T3_08,
     *              T3_09,T3_10,T3_12,T3_13,
     *              T3_14,T3_15,T3_16,T3_17,
     *              ROTAT,AK,EP,ALPHA,BETA,BETAS,GRAV,
     *              T3_11,NPOIN3,MSK,MASKEL,MESH3D,IELM3,SVIDE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOUKOM'
!
        ENDIF
!
        CLKMIN = .TRUE.
        CLKMAX = .TRUE.
        CALFLU = .FALSE.
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
     &  FLUX%R(1), FLUEXT,  KMIN, CLKMIN, KMAX, CLKMAX,                
     &  SCHCKE, SCHDKE, SLVDKE, TRBAKE, INFOGR,NEWDIF,CALFLU,
     &  T2_01,T2_02,T2_03,
     &  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     &  W1,NPTFR3,MMURD,VOLU,VOLUN ,
     &  KADH,KENT,KSORT,KENTU,KDIR,KDDL,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     &  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,IT2,
     &  TRAV3,MESH2D,MATR2H,MATR2V,H,OPTBAN,OPTDIF,TETATRA,
!                          0 BECAUSE AKSCE NOT PROGRAMMED
     &  YAWCHU,WCHU,AGGLOD,0,SOURCES,USCE,
     &  NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,.FALSE.,PLUIE)
! 
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR AK'
!
        CLEMIN  = .TRUE.
        CLEMAX  = .TRUE.
        CALFLU  = .FALSE.
        YASEM3D = .FALSE.
        YAWCHU  = .FALSE.
!
!       NEGLECTING THE MOLECULAR DIFFUSIVITY...
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
     &  FLUX%R(1), FLUEXT, EMIN, CLEMIN, EMAX, CLEMAX,
     &  SCHCKE,SCHDKE,SLVDKE,TRBAKE,INFOGR,NEWDIF,CALFLU,
     &  T2_01,T2_02,T2_03,
     &  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     &  W1 , NPTFR3 , MMURD , VOLU , VOLUN ,
     &  KADH,KENT,KSORT,KENTU,KDIR,KDDL,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     &  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,IT2,
     &  TRAV3,MESH2D,MATR2H,MATR2V,H,OPTBAN,OPTDIF,TETATRA,
!                          0 BECAUSE EPSCE NOT PROGRAMMED YET
     &  YAWCHU,WCHU,AGGLOD,0,SOURCES,USCE,
     &  NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,.FALSE.,PLUIE)
! 
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR EP'
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   COMPUTE THE VISCOSITIES VISCVI, VISCTA AND VISCTP
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
     &               T3_01, T3_02, T3_03, T3_04, T3_05, T3_06, T3_07,            
     &               SVIDE, MESH3D, IELM3, GRAV, NPLAN,                          
     &               NPOIN3, NPOIN2, NTRAC, MSK, MASKEL,        
     &               TA,MIXING,DAMPING,IND_T,DNUVIV,DNUTAV,KARMAN,
     &               PRANDTL,UETCAR)
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCLM'
!      
      ENDIF
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCKE'
        CALL VISCKE(VISCVI,VISCTA,AK,EP,NTRAC,CMU,                 
     *              DNUVIH,DNUVIV,DNUTAH,DNUTAV,KMIN,EMIN,
     *              ITURBH,ITURBV)
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
        CALL VISCLIP(VISCVI,VISCTA,H,NPLAN,NPOIN3,NPOIN2,NTRAC)
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
          CALFLU = BILMAS
          YASEM3D = .FALSE.
          NEWDIF = .TRUE.
          TETATRA=MIN(TETADI,1.D0)
!
          CALL SOURCE_TRAC
!
        DO ITRAC = 1,NTRAC
!          
          IF(SEDI.AND.ITRAC.EQ.NTRAC) THEN
            YAWCHU=.TRUE.
            SLVD%SLV=SLVDSE%SLV
            SLVD%NITMAX=SLVDSE%NITMAX
            SLVD%PRECON=SLVDSE%PRECON
            SLVD%KRYLOV=SLVDSE%KRYLOV
            SLVD%EPS=SLVDSE%EPS
          ELSE
            YAWCHU=.FALSE.
            SLVD%SLV=SLVDTA%SLV
            SLVD%NITMAX=SLVDTA%NITMAX
            SLVD%PRECON=SLVDTA%PRECON
            SLVD%KRYLOV=SLVDTA%KRYLOV
            SLVD%EPS=SLVDTA%EPS
          ENDIF
!
          IF(DEBUG.GT.0) THEN
            WRITE(LU,*) 'APPEL DE CVDF3D POUR TRACEUR ',ITRAC
          ENDIF
!
          CALL CVDF3D                                             
     *   (TA%ADR(ITRAC)%P,TAC%ADR(ITRAC)%P,TAN%ADR(ITRAC)%P,   
     *    VISCTA%ADR(ITRAC)%P,SIGMTA,      
     *    S0TA%ADR(ITRAC)%P,.TRUE.,S1TA%ADR(ITRAC)%P,.TRUE.,                     
     *    TABORL%ADR(ITRAC)%P, TABORF%ADR(ITRAC)%P, TABORS%ADR(ITRAC)%P,      
     *    ATABOL%ADR(ITRAC)%P, ATABOF%ADR(ITRAC)%P, ATABOS%ADR(ITRAC)%P,     
     *    BTABOL%ADR(ITRAC)%P, BTABOF%ADR(ITRAC)%P, BTABOS%ADR(ITRAC)%P,
     *    LITABL%ADR(ITRAC)%P, LITABF%ADR(ITRAC)%P, LITABS%ADR(ITRAC)%P,
     *    FLUX%R(1+ITRAC), FLUEXT, TAMIN, CTAMIN, TAMAX, CTAMAX,        
     *    SCHCTA, SCHDTA, SLVD  , TRBATA, INFOGR,NEWDIF, CALFLU ,
     *    T2_01,T2_02,T2_03,T3_01,T3_02,T3_03,T3_04,MESH3D,IKLE3,MASKEL,
     *    MTRA1,W1,NPTFR3,MMURD,VOLU,VOLUN,
     *    KADH,KENT,KSORT,KENTU,KDIR,KDDL,
     *    NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     *    NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     *    INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,IT2,
     *    TRAV3,MESH2D,MATR2H,MATR2V,H,OPTBAN,OPTDIF,TETATRA,
     *    YAWCHU,WCHU,AGGLOD,NSCE,SOURCES,TASCE2(1,ITRAC),
     *    NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,RAIN,PLUIE)
!
!         NEWDIF=.FALSE. (POSSIBLE SI SIGMTA INCHANGE)
!
          IF(DEBUG.GT.0) THEN
            WRITE(LU,*) 'RETOUR DE CVDF3D POUR TRACEUR ',ITRAC
          ENDIF  
!
        ENDDO
!
!-----------------------------------------------------------------------
! COMPUTE DELRA RHO / RHO FOR THE BUOYANCY TERMS
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DRSURR'
        CALL DRSURR(DELTAR,TA,BETAC,T0AC,T3_01,RHO0,RHOS,DENLAW,
     &              SEDI,NTRAC,IND_T,IND_S)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DRSURR'

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
!
        CALL FONVAS
     &  (IVIDE%R, EPAI%R, CONC%R, TREST, TEMP%R, HDEP%R, PDEPO%R,
     &   ZR%R, ZF%R,TA%ADR(NTRAC)%P%R, WCHU%R,
     &   T3_01%R, T3_02%R, T3_03%R, NPOIN2, NPOIN3, NPFMAX, NCOUCH,
     &   NPF%I, LT, DT, DTC, GRAV, RHOS, CFMAX, CFDEP, EPAI0,
     &   TASSE,GIBSON)
!
      ENDIF
!
! IF THE BOTTOM HAS EVOLVED, UPDATING GEOMETRY
!
      IF(INCLUS(COUPLING,'SISYPHE').OR.SEDI) THEN
!
!       COPY OF MODIFIED BOTTOM TOPOGRAPHY INTO Z AND ZPROP
        CALL OV('X=Y     ',      Z(1:NPOIN2),ZF%R,ZF%R,0.D0,NPOIN2)
        CALL OV('X=Y     ',ZPROP%R(1:NPOIN2),ZF%R,ZF%R,0.D0,NPOIN2)
!       COMPUTE NEW BOTTOM GRADIENTS AFTER SEDIMENTATION
        CALL GRAD2D(GRADZF%ADR(1)%P,GRADZF%ADR(2)%P,Z3,1,SVIDE,
     &              UNSV2D,T2_02,IELM2H,MESH2D,MSK,MASKEL)
!       COMPUTE NEW Z COORDINATES
        CALL CALCOT(Z,H%R)
!       USEFUL ? NOT SURE, IS DONE AT EACH TIME-STEP ELSEWHERE, SO..
        CALL CALCOT(ZPROP%R,HPROP%R)
        CALL FSGRAD(GRADZS,ZFLATS,Z,ZF,IELM2H,MESH2D,MSK,MASKEL,
     *              UNSV2D,T2_01,NPOIN2,NPOIN3,OPTBAN,SVIDE)
!
      ENDIF
!
!-----------------------------------------------------------------------
! RESULT OUTPUT 
!
! PREPARE 3D OUTPUT 
!
      IF (MOD(LT,GRAPRD).EQ.0.AND.LT.GE.GRADEB) THEN
!
! PREPARE 2D AND 3D OUTPUT 
!
      CALL PRERES_TELEMAC3D(LT)
!
! 3D OUTPUT
! 
      CALL DESIMP(VARSO3,HIST,0,NPOIN3,NRES,BINRES,AT,LT,LISPRD,GRAPRD,
     &            SORG3D,SORIM3,MAXVA3,TEXT3,GRADEB,LISDEB)
!
! 2D OUTPUT 
!   
      CALL DESIMP(VARSOR,HIST,0,NPOIN2,NHYD,BINHYD,AT,LT,LISPRD,GRAPRD,
     &            SORG2D,SORIMP,MAXVAR,TEXTE,GRADEB,LISDEB)
!
      ENDIF
!
! SEDIMENTOLOGY OUTPUT
! 
      IF(SEDI.AND.NORSED(1:1).NE.' ') THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DESSED'
        CALL DESSED(NPF%I,IVIDE%R,EPAI%R,HDEP%R,
     &              CONC%R,TEMP%R,ZR%R,NPOIN2,NPFMAX,
     &              NCOUCH,NIT,GRAPRD,LT,DTC,TASSE,GIBSON,
     &              NRSED,TITCAS,BIRSED,0)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DESSED'
      ENDIF
!
! SCOPE OUTPUT: CROSS-SECTIONS, SECTIONS, ETC.
!
      CALL SCOPE
     & (U%R, V%R, W%R, H%R, ZF%R, X, Y, Z, T3_01%R, T3_02%R, T3_03%R,
     &  SURFA2%R, IKLE3%I, MESH2D%IFABOR%I, NELEM3, NELEM2,
     &  NPOIN2, NPOIN3, NETAGE, NPLAN, LT, AT, DT, NIT, NSCO, PRIVE)
!
! CALL THE SUBROUT. FOR AN OPTIONAL USER OUTPUT
!
      CALL UTIMP(LT,AT,GRADEB,GRAPRD,LISDEB,LISPRD) 
!
! SEDIMENT OUTPUT
!
      IF (SEDI) CALL IMPSED
     &   (IVIDE%R, EPAI%R, CONC%R, TEMP%R, HDEP%R, PDEPO%R, FLUER%R,
     &    ZR%R, ZF%R, TA%ADR(NTRAC)%P%R, WCHU%R, X, Y,
     &    NPOIN2, NPOIN3, NPFMAX, NCOUCH, NPF%I, LT, RHOS, CFMAX,
     &    CFDEP, EPAI0, TASSE, GIBSON, PRIVE, LISPRD)
!
!-----------------------------------------------------------------------
! DROGUES/FLOATERS/BUOYS
!
      IF (NFLOT.NE.0) THEN
!
         IF (INFOGR) CALL MITTIT(12,AT,LT)
!
         CALL DERI3D
     & ( UCONV%R, VCONV%R, WSCONV%R, DT, X, Y, ZSTAR%R, Z,
     &   IKLE2%I, MESH3D%IFABOR%I, LT, NPOIN2, NELEM2, NPLAN, NPLINT,
     &   MESH2D%SURDET%R, XFLOT%R, YFLOT%R, ZFLOT%R, SHPFLO%R, SHZFLO%R,
     &   DEBFLO%I, FINFLO%I, ELTFLO%I, ETAFLO%I, NFLOT, NITFLO, FLOPRD)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! BILAN DE MASSE DU PAS DE TEMPS COURANT
!
      IF (BILMAS) THEN
!
        IF (.NOT.INFMAS) INFOGR = .FALSE.
        INFOGR = INFOGR .AND. LISTIN
        IF (INFOGR) CALL MITTIT(10,AT,LT)
!
        CALL MASS3D(INFOGR,LT)
!
        IF(SEDI) CALL SED3D
     &     (MASSE%R, U%R, V%R, W%R, WCHU%R, TA%ADR(NTRAC)%P%R, X, Y, Z,
     &      IVIDE%R, EPAI%R, HDEP%R, CONC%R, FLUER%R, PDEPO%R,
     &      SURFA2%R, T3_01%R, T3_02%R, IKLE2%I,
     &      NELEM2, NPOIN2, NPOIN3, NTRAC, NVBIL, NPFMAX, NCOUCH,
     &      NPF%I, LT, AT, DT, INFOGR, TASSE, GIBSON, RHOS, CFDEP) 
!
        CALL BIL3D(LT,MESH3D%IKLBOR%I,IKLE2%I,NPTFR2,NETAGE,NELEM2,
     *             NFRLIQ)
!
      ENDIF
!
! COMPARAISON AVEC UN FICHIER DE REFERENCE
!
      IF(VALID) THEN
        CALL VALIDA(PRIV3,TEXTP3,NREF,VARSO3,TEXT3,NRES,MAXVA3,NPOIN3,
     *              LT,NIT,ALIRE3D)
      ENDIF
!
!
!  VERIFICATION DES VALEURS PARTAGEES ENTRE SOUS-DOMAINES
!
C     CALL CHECK_DIGITS(H ,T2_01,MESH2D)
C     CALL CHECK_DIGITS(U ,T3_01,MESH3D)
C     CALL CHECK_DIGITS(V ,T3_01,MESH3D)
C     CALL CHECK_DIGITS(W ,T3_01,MESH3D)
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
! WRITE THE TRACER/FLOATER FILE DOWN
!
! NOTE: BINHYD DOES NOT CORRESPOND TO NOMRBI (BUT THERE IS NO SPECIFIC
!                                             KEY-WORD FOR WHAT SHOULD
!                                             BE BINRBI)
!
      IF (NFLOT.NE.0) CALL SFLO3D
     &   (XFLOT%R, YFLOT%R, ZFLOT%R, IKLFLO%I, TRAFLO%I, DEBFLO%I,
     &    FINFLO%I, NFLOT, NITFLO, FLOPRD, NRBI, KINT, KEXT,
     &    LISTIN, TITCAS, BINHYD, NOMRBI, NIT,I_ORIG,J_ORIG)
!
!-----------------------------------------------------------------------
! 
      RETURN
      END
!                       ******************
                        SUBROUTINE TEL4DEL
!                       ******************
!
     *(NPOIN,NPOIN2,NELEM,NSEG,IKLE,ELTSEG,GLOSEG,MAXSEG,
     * X,Y,NPTFR,LIHBOR,
     * NBOR,NOLAY,AAT,DDT,LLT,NNIT,HNEW,HPROP,
     * U,V,SALI,TEMP,VISC,TITRE,NOMGEO,NOMLIM,NSTEPA,
     * NSOU,NOMSOU,NMAB,NOMMAB,NCOU,NOMCOU,NINI,NOMINI,NVEB,NOMVEB,
     * NMAF,NOMMAF,NCOB,NOMCOB,NSAL,NOMSAL,NTEM,NOMTEM,NVEL,NOMVEL,
     * NVIS,NOMVIS,INFOGR,DM1,
     * ZCONV,HYDSTEP,NELEM2,SALI_DEL,TEMP_DEL,VELO_DEL,DIFF_DEL,
     * MARDAT,MARTIM,FLOW,INIFLOW,W)
!
!***********************************************************************
! TELEMAC 3D VERSION 5.9    24/09/08       LEO POSTMA (DELFT HYDRAULICS)
! FORTRAN 95 VERSION                             CHARLES MOULINEC (LNHE)
! 
!
! CURRENT LIMITATIONS : WILL NOT WORK WITH TIDAL FLATS
!                       WILL NOT WORK WITH GENERALISED 
!                       SIGMA TRANSFORMATION
!
! 11/09/2007 : SALINITY AND TEMPERATURE ADDED (JMH)
! 20/12/2007 : RESFIL CHANGED INTO NOMGEO, CLIFIL INTO NOMLIM (JMH)
! 20/12/2007 : FIRST DIMENSION OF STOSEG (MAXSEG) ADDED (JMH)
! 20/12/2007 : MARDAT AND MARTIM ADDED (JMH)
! 20/05/2008 : FLOW IS NOW AN ARGUMENT AND IS INITIALIZED ONLY IF INIFLOW
!              IT MAY CONTAIN FLUXES DUE TO TIDAL FLATS TREATMENT
! 24/09/2008 : F AND G VARIABLE IN TIME AND SPACE FOR GENERALISED
!              SIGMA TRANSFORMATION (JMH)
!                                     
!***********************************************************************
!
!      FONCTION:
!      =========  
!
!     COUPLES LNH-TELEMAC-3D TO DELFT-WAQ ONLINE
!
!     ORIGINAL VERSION NOVEMBER 2004
!     MODIFICATION  07 MARCH    2005 BY LEO POSTMA
!     MODIFICATION  14 NOVEMBER 2005 BY LEO POSTMA, MAKING IT ON LINE
!     MODIFICATION  22 FEBRUARY 2007 (LEO'S VISIT IN LNHE)
!     MODIFICATION  18 MAY      2007 (LAST VERTICAL FLOWS LOOP)
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !  NOM           !MODE!                  ROLE                        !
! !________________!____!______________________________________________!
! !  NPOIN         ! -->! NUMBER OF 3D POINTS IN THE MESH 
! !  NPOIN2        ! -->! NUMBER OF 2D POINTS IN THE MESH
! !  NELEM         ! -->! NUMBER OF ELEMENTS IN THE MESH 
! !  NSEG          ! -->! NUMBER OF 2D SEGMENTS IN THE MESH 
! !  IKLE          ! -->! CONNECTIVITY TABLE
! !  ELTSEG        ! -->! SEGMENTS COMPOSING AN ELEMENT 
! !  GLOSEG        ! -->! GLOBAL NUMBERS OF POINTS OF A SEGMENT
! !  X,Y           ! -->! COORDINATES OF HORIZONTAL MESH
! !  NPTFR         ! -->! NUMBER OF 3D BOUNDARY POINTS 
! !  LIHBOR        ! -->! TYPE OF 2D BOUNDARIES FOR DEPTH
! !  NBOR          ! -->! GLOBAL NUMBERS OF BOUNDARY NODES 
! !  NOLAY         ! -->! NUMBER OF PLANES
! !  AAT,DDT       ! -->! CURRENT TIME, TIME STEP
! !  LLT,NNIT      ! -->! ITERATION NUMBER,NUMBER OF ITERATIONS
! !  HNEW          ! -->! DEPTH AT NEW TIME (2D) ELEVATION Z (3D) 
! !  HPROP         ! -->! DEPTH IN THE DIV(HU) TERM
! !  U,V           ! -->! COMPONENTS OF HORIZONTAL VELOCITY
! !  SALI,TEMP     ! -->! SALINITY, TEMPERATURE (IF SALI_DEL, IF TEMP_DEL)
! !  TITRE         ! -->! TITLE OF STUDY 
! !  NOMGEO        ! -->! RESULT FILE OF THE SIMULATION 
! !  NOMLIM        ! -->! BOUNDARY FILE OF THE SIMULATION
! !  NSTEPA        ! -->! NUMBER OF TIME-STEPS FOR TIME AGGREGATION
! !  NSOU,NOMSOU   ! -->! VOLUME CANAL AND FILE 
! !  NMAB,NOMMAB   ! -->! AREA CANAL AND FILE  
! !  NCOU,NOMCOU   ! -->! FLUX CANAL AND FILE 
! !  NINI,NOMINI   ! -->! HORIZONTAL SURFACE CANAL AND FILE
! !  NVEB,NOMVEB   ! -->! NODE EXCHANGE CANAL AND FILE
! !  NMAF,NOMMAF   ! -->! NODE DISTANCE CANAL AND FILE 
! !  NCOB,NOMCOB   ! -->! DELWAQ STEERING FILE CANAL AND FILE
! !  NSAL,NOMSAL   ! -->! SALINITY FOR DELWAQ, CANAL AND FILE 
! !  NTEM,NOMTEM   ! -->! TEMPERATURE FOR DELWAQ, CANAL AND FILE  
! !  INFOGR        ! -->! IF YES, INFORMATION PRINTED ON LISTING
! !  DM1           ! -->! SEE HYDSTEP 
! !  ZCONV         ! -->! SEE HYDSTEP 
! !  HYDSTEP       ! -->! IF HYDSTEP=1 PRIMITIVE EQUATIONS HAVE BEEN USED
! !                !    ! IF HYDSTEP=2 WAVE EQUATION HAS BEEN USED 
! !                !    ! IN THIS LATTER CASE, THE ADVECTING FIELD IS:
! !                !    ! U+DM1*GRAD(ZCONV)
! !                !    ! ZCONV IS A PIECEWISE LINEAR 2D FUNCTION 
! !                !    ! HYDSTEP IS HYDSTEP IN TELEMAC-3D AND SOLSYS
! !                !    ! IN TELEMAC-2D  
! !  NELEM2        ! -->! NUMBER OF ELEMENTS IN 2D  
! !  SALI_DEL      ! -->! IF YES, THERE IS SALINITY 
! !  TEMP_DEL      ! -->! IF YES, THERE IS TEMPERATURE         
! !________________!____!_______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : MITRID
!
!***********************************************************************
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_TEL4DEL => TEL4DEL
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      SAVE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: NPOIN,NPOIN2,NELEM,NSEG,NPTFR
      INTEGER, INTENT(IN)            :: NOLAY,LLT,NNIT,NELEM2,HYDSTEP
      INTEGER, INTENT(IN)            :: MAXSEG,MARDAT(3),MARTIM(3)
      INTEGER, INTENT(INOUT)         :: NSTEPA
      INTEGER, INTENT(IN)            :: IKLE(NELEM2,3),LIHBOR(*)
      INTEGER, INTENT(IN)            :: ELTSEG(NELEM2,3)
      INTEGER, INTENT(IN)            :: GLOSEG(MAXSEG,2)
      INTEGER, INTENT(IN)            :: NBOR(*)
      INTEGER, INTENT(IN)            :: NSOU,NMAB,NCOU,NINI,NVEL,NVIS
      INTEGER, INTENT(IN)            :: NVEB,NMAF,NCOB,NSAL,NTEM
      DOUBLE PRECISION  , INTENT(IN) :: X(NPOIN2),Y(NPOIN2),HNEW(NPOIN)
      DOUBLE PRECISION  , INTENT(IN) :: HPROP(NPOIN2)
      DOUBLE PRECISION  , INTENT(IN) :: AAT,DDT
      DOUBLE PRECISION  , INTENT(IN) :: U(NPOIN),V(NPOIN),DM1(NPOIN)
      DOUBLE PRECISION  , INTENT(IN) :: SALI(NPOIN),TEMP(NPOIN)
      DOUBLE PRECISION  , INTENT(IN) :: VISC(NPOIN)
      DOUBLE PRECISION  , INTENT(IN) :: ZCONV(NELEM2,3)
!                                               NSEG EN 2D, NOQ EN 3D
      DOUBLE PRECISION  , INTENT(INOUT) :: FLOW(*),W(NELEM,*)
      CHARACTER(LEN=72) , INTENT(IN) :: TITRE
      CHARACTER(LEN=144), INTENT(IN) :: NOMSOU,NOMMAB,NOMCOU,NOMINI
      CHARACTER(LEN=144), INTENT(IN) :: NOMVEB,NOMMAF,NOMCOB,NOMSAL
      CHARACTER(LEN=144), INTENT(IN) :: NOMTEM,NOMGEO,NOMLIM,NOMVEL
      CHARACTER(LEN=144), INTENT(IN) :: NOMVIS
      LOGICAL           , INTENT(IN) :: INFOGR,SALI_DEL,TEMP_DEL,INIFLOW
      LOGICAL           , INTENT(IN) :: VELO_DEL,DIFF_DEL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ERR
      DOUBLE PRECISION, ALLOCATABLE :: AREA(:)  ! I  HORIZONTAL SURFACE PER NODE IN 2D
      INTEGER ISTEPA            ! I  ITERATION NUMBER FOR TIME AGGREGATION
C
      INTEGER ITSTRT            ! O  STARTTIME
      INTEGER ITSTOP            ! O  STOPTIME
      INTEGER ITSTEP            ! O  TIMESTEPSIZE
C
      LOGICAL TRICK             ! I  IF TRUE, TRICK FOR DDT < 1 SECOND, NOW TO BE DELEVERED
C
C     LOCAL ALLOCATABLE ARRAYS:                DESCRIPTION
C
      INTEGER, ALLOCATABLE :: NODENRS(:)  ! WAQ ARRAY FOR OPEN BOUNDARIES
      DOUBLE PRECISION , ALLOCATABLE :: HTOT   (:)  ! NEW 2D TOTAL-DEPTH
      DOUBLE PRECISION , ALLOCATABLE :: HOLD   (:)  ! OLD 2D TOTAL-DEPTH
      DOUBLE PRECISION , ALLOCATABLE :: VOLOLD (:)  ! WAQ 3D VOLUMES PREV. TIME LEVEL
      DOUBLE PRECISION , ALLOCATABLE :: VOLUME (:)  ! WAQ 3D VOLUMES
      DOUBLE PRECISION , ALLOCATABLE :: VOL2   (:)  ! WAQ 2D TOTAL VOLUME WORK ARRAY
      DOUBLE PRECISION , ALLOCATABLE :: AREAWQ (:)  ! WAQ EXCHANGE SURFACES OF LINKS
      DOUBLE PRECISION , ALLOCATABLE :: VELO   (:)  ! WAQ 3D VELOCITIES AT NODES
      DOUBLE PRECISION , ALLOCATABLE :: LENGTH(:,:) ! WAQ 3D DISTANCES OF NODES
      DOUBLE PRECISION , ALLOCATABLE :: MAT1(:,:), MAT2(:,:) ! 2D ELEMENT MATRICES U & V
      DOUBLE PRECISION , ALLOCATABLE :: G(:,:)      ! DEPTH AND INTERPOLATION FACTOR IN SIGMA
      DOUBLE PRECISION , ALLOCATABLE :: DIST  (:,:) ! LENGTH OF GRAVITY LINE FROM POINT I
      DOUBLE PRECISION , ALLOCATABLE :: F(:,:)      ! ARRAY TO STORE FRACTION OF DEPTH PER LAYER
      DOUBLE PRECISION , ALLOCATABLE :: SUMAREA(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: SUMFLOW(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: SUMSALI(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: SUMTEMP(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: SUMVISC(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: SUMVELO(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: W2D(:,:)    ! FOR FLUXES IN 3D
C
C     LOCAL SINGLE VARIABLES OR ARRAYS WITH FIXED LENGTH
C
      INTEGER NPTFR2                    !  NUMBER OF 2D BOUNDARY POINTS
      INTEGER STATIO                    !  IF 1, THEN STATIONARY OUTPUT
      INTEGER NLAY                      !  NUMBER OF LAYERS AND LOOP COUNTER OF IT
      INTEGER ISEG, ISEGL               !  LOOP COUNTER TELEMAC SEGMENTS, VALUE AT A LAYER
      INTEGER NOQ1 , NOQ3 , NOQ   !  NUMBER OF FLOWS IN 3 DIRECTIONS + TOTAL
      INTEGER IFROM, ITO , IFRM1, ITOP1 !  FROM AND TO NODE-NRS OF AN EXCHANGE
      INTEGER IELEM                     !  LOOP COUNTER TELEMAC ELEMENTS
      INTEGER INODE                     !  LOOP COUNTER TELEMAC NODES
      INTEGER IBOR, MBND                !  LOOP AND SEQUENTIAL COUNTER OPEN BOUNDARIES
      DOUBLE PRECISION  DX, DY          !  DISTANCES IN X AND Y BETWEEN NODES
      DOUBLE PRECISION  ATOLD           !  TIME IN THE FILE, ITS OLD VALUE 
      INTEGER I,J,I2D,I3D               !  GENERAL, MULTI USE LOOP COUNTER
      INTEGER ND1 , ND2 , ND3           !  2D NODE NUMBER HELP VARIABLES WITHIN LOOPS
      INTEGER ND1L, ND2L, ND3L          !  3D NODE NUMBER HELP VARIABLES WITHIN LOOPS
      DOUBLE PRECISION  HTOT1, HTOT2, HTOT3 !  HELP VARIABLES TOTAL DEPTH WITHIN LOOPS
      DOUBLE PRECISION  H1 ,H2 ,H3, H       !  HELP VARIABLES ORIGINAL MASS MATRIX
      DOUBLE PRECISION  U1, U2, U3, V1, V2, V3 ! HELP VARIABLES U AND V AT THE 3 NODES
      DOUBLE PRECISION  AM11,AM21,AM12,AM22,AM13,AM23 ! HELP VARIABLES ELEMENT MATRIX TERMS
      DOUBLE PRECISION  AU , AV         !  AVERAGE U, V  FOR ELEMENT
      DOUBLE PRECISION  F1 , F2 , F3    ! (MASS MAT)*(ELT MAT)*(NODE VELOS)
      DOUBLE PRECISION  A1 , A2 , A3    !  PROJECTIONS OF THE ELEMENT VELOCITY
      DOUBLE PRECISION  SSUM,MASINI,ERRTOT
      DOUBLE PRECISION  X2, Y2, X3, Y3
      DOUBLE PRECISION SURFACC,ZCONVX,ZCONVY,ZCONV1,ZCONV2,ZCONV3
      DOUBLE PRECISION UU1  ,UU2  ,UU3  ,VV1  ,VV2  ,VV3
      DOUBLE PRECISION UU1M1,UU2M1,UU3M1,VV1M1,VV2M1,VV3M1
      DOUBLE PRECISION UU1P1,UU2P1,UU3P1,VV1P1,VV2P1,VV3P1,W123,W456
C
      INTEGER ITOLD,IOPT1
C
      CHARACTER(LEN=16) FORMUL
      INTRINSIC MAX,REAL,ABS
C
C-----------------------------------------------------------------------
C
      IF(DDT.LT.1.D0) THEN
        TRICK=.TRUE.
      ELSE
        TRICK=.FALSE.
      ENDIF
C
C     FIRST CALL: INITIALIZATIONS AND MEMORY ALLOCATION
C
      IF(LLT.EQ.0) THEN
C
         NPTFR2 = NPTFR/NOLAY
C
C        DERIVE THE HORIZONTAL PART (NOQ1) OF THE FROM-TO EXCHANGE TABLE
C            FOR COMPUTATIONAL ELEMENTS. THE ELEMENT NUMBERS AT THE LAYERS
C            DIFFER NPOIN2 IN IN COMPUTATIONAL ELEMENT NUMBER AND MBND IN
C            BOUNDARY NUMBER (MBND ITSELF IS POSITIVE)
C        COMPUTE HORIZONTAL 'FROM' AND 'TO' HALF DISTANCES ON THE FLY
C            THEY ARE THE SAME FOR ALL LAYERS, SO DO ONLY FOR LAYER 1
C
         NOQ1 =  NOLAY   *NSEG  ! NUMBER OF FLOWS IN FIRST DIRECTION
         NOQ3 = (NOLAY-1)*NPOIN2 ! NUMBER OF VERTICAL FLOW TERMS
         NOQ  = NOQ1 + NOQ3 ! TOTAL NUMBER OF FLOW TERMS FOR WAQ
C
C        ALLOCATION OF ALL THE ARRAYS USED IN THE SUBROUTINE
C
C                           NOLAY-1 BUT WELL IF NOLAY=1...
         ALLOCATE( G(NPOIN2,NOLAY) ,STAT=ERR)
         ALLOCATE( HTOT(NPOIN2)    ,STAT=ERR)
         ALLOCATE( AREA(NPOIN2)    ,STAT=ERR)
         ALLOCATE( DIST(3,NELEM2)   ,STAT=ERR)
         ALLOCATE( NODENRS(NPOIN2) ,STAT=ERR)
         ALLOCATE( LENGTH (2,NSEG) ,STAT=ERR)
         ALLOCATE( MAT1(3,NELEM2)   ,STAT=ERR)
         ALLOCATE( MAT2(3,NELEM2)   ,STAT=ERR)
         ALLOCATE( W2D(NELEM2,3)   ,STAT=ERR)
C
C        ARRAYS FOR ALL TRANSPORT AND OTHER ADVECTION DIFFUSION TERMS
C
         ALLOCATE( HOLD(NPOIN2)  ,STAT=ERR)
         ALLOCATE( F(NPOIN2,NOLAY)      ,STAT=ERR)
         ALLOCATE( VELO(NPOIN)   ,STAT=ERR)
         ALLOCATE( VOL2(NPOIN2)  ,STAT=ERR)
         ALLOCATE( VOLUME(NPOIN) ,STAT=ERR)
         ALLOCATE( AREAWQ(NOQ)   ,STAT=ERR)
         ALLOCATE( VOLOLD(NPOIN) ,STAT=ERR)
         ALLOCATE( SUMAREA(NOQ)  ,STAT=ERR)
         SUMAREA = 0.D0
         ALLOCATE( SUMFLOW(NOQ)  ,STAT=ERR)
         SUMFLOW = 0.D0
         IF(SALI_DEL) THEN
           ALLOCATE( SUMSALI(NPOIN),STAT=ERR)
           SUMSALI = 0.D0
         ENDIF
         IF(TEMP_DEL) THEN
           ALLOCATE( SUMTEMP(NPOIN),STAT=ERR)
           SUMTEMP = 0.D0
         ENDIF
         IF(VELO_DEL) THEN
           ALLOCATE( SUMVELO(NPOIN),STAT=ERR)
           SUMVELO = 0.D0
         ENDIF
         IF(DIFF_DEL) THEN
           ALLOCATE( SUMVISC(NPOIN),STAT=ERR)
           SUMVISC = 0.D0
         ENDIF
C
      ELSE
C
        HOLD = HTOT
C
      ENDIF
C
      IF(NOLAY.EQ.1) THEN
C       AJOUT JMH (HPROP IS THE DEPTH IN THE DIV(HU) TERM
        HOLD=HPROP
C
        DO INODE = 1 , NPOIN2
          HTOT(INODE) = HNEW(INODE)
        ENDDO
      ELSE
        DO INODE = 1 , NPOIN2
          HTOT(INODE) = HNEW(INODE+(NOLAY-1)*NPOIN2) - HNEW(INODE)
        ENDDO
      ENDIF
C
      IF(LLT.EQ.0) THEN
C
         STATIO = 0
C
C        COMPUTING THE AREA ASSOCIATED TO EACH NODE
C        AS 1/3 OF EVERY NEIGHBOURING TRIANGLE
C
C        COMPUTING 1/3 OF THE HEIGHT OF THE TRIANGLES FROM NODE I
C        HEIGHT = 2*SURFACE/(LENGTH OF THE SEGMENT)
C
         DO I=1,NPOIN2
           AREA(I)=0.D0
         ENDDO
         DO IELEM=1,NELEM2
           ND1 = IKLE(IELEM,1)
           ND2 = IKLE(IELEM,2)
           ND3 = IKLE(IELEM,3)
           X2=X(ND2)-X(ND1)
           X3=X(ND3)-X(ND1)
           Y2=Y(ND2)-Y(ND1)
           Y3=Y(ND3)-Y(ND1)
           SURFACC=0.5D0*(X2*Y3-X3*Y2)
           AREA(ND1)=AREA(ND1)+SURFACC/3.D0
           AREA(ND2)=AREA(ND2)+SURFACC/3.D0
           AREA(ND3)=AREA(ND3)+SURFACC/3.D0
           DO I = 1 , 3
             ISEG = ELTSEG(IELEM,I)
             A1   = X(GLOSEG(ISEG,1))-X(GLOSEG(ISEG,2))
             A2   = Y(GLOSEG(ISEG,1))-Y(GLOSEG(ISEG,2))
             DIST(I,IELEM)=2.D0*SURFACC/SQRT(A1**2+A2**2)/3.D0
           ENDDO
         ENDDO
C
C        CHECKING THE INITIAL MASS
C
         MASINI=0.D0
         DO I=1,NPOIN2
           MASINI=MASINI+AREA(I)*HTOT(I)
         ENDDO
C        WRITE(LU,*) 'INITIAL MASS=',MASINI
C
         WRITE(NINI) NPOIN2,0,NPOIN2,NPOIN2,NPOIN2,0
         WRITE(NINI) (REAL(AREA(I)),I=1,NPOIN2)
C
C        DERIVE THE OPEN BOUNDARY NODES AND NUMBER THEM NEGATIVELY
C
C        NODENRS : IF NOT OPEN BOUNDARY = NODE NUMBER
C                  IF     OPEN BOUNDARY = - (OPEN BOUNDARY NODE NUMBERING)
C
         DO INODE = 1, NPOIN2
           NODENRS(INODE) = INODE
         ENDDO
         MBND = 0
         DO IBOR = 1, NPTFR2
            IF ( LIHBOR(IBOR) .NE. 2 ) THEN
               MBND = MBND + 1
               NODENRS(NBOR(IBOR)) = -MBND
            ENDIF
         ENDDO
C
         DO NLAY  = 1, NOLAY
            DO ISEG  = 1, NSEG
               IFROM = NODENRS(GLOSEG(ISEG,1))
               ITO   = NODENRS(GLOSEG(ISEG,2))
               IF ( IFROM .LE. 0 .AND. ITO .LE. 0 ) THEN
                  WRITE ( NVEB ) 0, 0, 0, 0 ! NO FLOWS BETWEEN BOUNDARY CELLS
               ELSE
C                 NOTE JMH: THIS CALL NOLAY*NSEG TIMES MUST BE VERY
C                           EXPENSIVE (BETTER TO DO A COLLECTIVE FDNRST ?)
                  CALL FDNRST(GLOSEG(ISEG,1),GLOSEG(ISEG,2),X,Y,
     *                        NODENRS,NPOIN2,IFRM1,ITOP1)
                  IF ( IFROM .GT. 0 ) IFROM = IFROM + (NLAY-1)*NPOIN2
                  IF ( IFROM .LT. 0 ) IFROM = IFROM - (NLAY-1)*MBND
                  IF ( IFRM1 .GT. 0 ) IFRM1 = IFRM1 + (NLAY-1)*NPOIN2
                  IF ( IFRM1 .LT. 0 ) IFRM1 = IFRM1 - (NLAY-1)*MBND
                  IF ( ITO   .GT. 0 ) ITO   = ITO   + (NLAY-1)*NPOIN2
                  IF ( ITO   .LT. 0 ) ITO   = ITO   - (NLAY-1)*MBND
                  IF ( ITOP1 .GT. 0 ) ITOP1 = ITOP1 + (NLAY-1)*NPOIN2
                  IF ( ITOP1 .LT. 0 ) ITOP1 = ITOP1 - (NLAY-1)*MBND
                  WRITE ( NVEB ) IFROM,ITO,IFRM1,ITOP1
               ENDIF
C              NOTE JMH:
C              THIS COULD BE DONE IN AN EXTRA LOOP, AVOIDING IF(NLAY.EQ.1)
C              FOR EVERY SEGMENT. THIS PART IS INDEPENDENT FROM THE REST
C              OF THE LOOP
               IF ( NLAY .EQ. 1 ) THEN
                  DX = X(GLOSEG(ISEG,1)) - X(GLOSEG(ISEG,2))
                  DY = Y(GLOSEG(ISEG,1)) - Y(GLOSEG(ISEG,2))
                  LENGTH(1,ISEG) = SQRT(DX**2+DY**2)*0.5D0
                  LENGTH(2,ISEG) = LENGTH(1,ISEG)
               ENDIF
            ENDDO
         ENDDO
C
C        WRITE ALL HORIZONTAL 'FROM' 'TO' HALF LENGTHES
C
         WRITE(NMAF) 0
         DO NLAY = 1, NOLAY
           WRITE(NMAF) ((REAL(LENGTH(I,J)),I=1,2),J=1,NSEG)
         ENDDO
C
C        DERIVE THE FROM-TO EXCHANGE TABLE FOR COMPUTATIONAL ELEMENTS
C        VERTICALLY FOR ALL LAYERS. THE LAYERS DIFFER NPOIN2 IN
C        COMPUTATIONAL ELEMENT NUMBER. BOUNDARY NODES HAVE NO VERTICAL FLOW
C        WRITE 1.0 FOR THE VERTICAL 'FROM' AND 'TO' HALFDISTANCES
C        THEY ARE UPDATED BY WAQ TO BECOME VOLUME/AREA/2.0 DURING
C        SIMULATION TIME, SINCE VERTICAL DISTANCES CHANGE WITH VOLUME.
C
         DO NLAY = 1, NOLAY-1
            DO INODE = 1, NPOIN2
               IFROM = NODENRS(INODE)
               IF (IFROM.LE.0) THEN
                  WRITE(NVEB) 0, 0, 0, 0 ! NO FLOWS BETWEEN BOUNDARY CELLS
               ELSE
                  IFRM1 = IFROM +  MAX(NLAY-2,   0   )*NPOIN2
                  ITOP1 = IFROM +  MIN(NLAY+1,NOLAY-1)*NPOIN2
                  IFROM = IFROM + (    NLAY-1        )*NPOIN2
                  ITO   = IFROM +                      NPOIN2
                  WRITE ( NVEB ) IFROM,ITO,IFRM1,ITOP1
               ENDIF
            ENDDO
            WRITE(NMAF) (1.0, I=1,NPOIN2*2) ! VERTICAL LENGTHES AT A DUMMY 1.0
         ENDDO                  ! WAQ COMPUTES THEM ON THE FLY FROM
C
C        MAKE THE 2D MATRIX TERMS
C
         DO IELEM = 1, NELEM2
            ND1 = IKLE(IELEM,1)
            ND2 = IKLE(IELEM,2)
            ND3 = IKLE(IELEM,3)
            MAT1(1,IELEM) = (Y(ND3)-Y(ND2))/6.D0
            MAT1(2,IELEM) = (Y(ND1)-Y(ND3))/6.D0
            MAT1(3,IELEM) = (Y(ND2)-Y(ND1))/6.D0
            MAT2(1,IELEM) = (X(ND2)-X(ND3))/6.D0
            MAT2(2,IELEM) = (X(ND3)-X(ND1))/6.D0
            MAT2(3,IELEM) = (X(ND1)-X(ND2))/6.D0
         ENDDO
C
C        FILL IN THE HORIZONTAL AREA IN THE LAST DIRECTION EXCHANGE AREA
C
         DO NLAY = 1 , NOLAY-1
            AREAWQ( (NOLAY*NSEG)+(NLAY-1)*NPOIN2+1 :
     *              (NOLAY*NSEG)+ NLAY   *NPOIN2     ) = AREA
         ENDDO
C
         IF(TRICK) THEN
!           WRITE ( 4 , * )
!    *           "TIME STEP: ",DDT," SMALLER THAN ONE SECOND !"
!           WRITE ( 4 , * )
!    *           "SYSTEM WILL COMPUTE IN TIMESTEPS AND M3/TIMESTEP"
            ITSTRT = 0
            ITSTEP = 1
         ELSE
            ITSTRT = INT(AAT)
         ENDIF
C
         ISTEPA=0
C
      ENDIF   ! FOR LLT = 0
C
C-----------------------------------------------------------------------
C
C        TIME STEP
C
C-----------------------------------------------------------------------
C
C
C        GET THE DEPTH MULTIPLICATION FACTOR FOR AN ACTIVE POINT
C                   TO USE FOR THE WHOLE AREA (SIGMA COORDINATES)
C                   F AND G = IN TELEMAC ORDER (FIRST LAYER = BOTTOM)
C                   F IS THE LAYER THICKNESS AROUND THE PLANES
C                   G(I) IS HALF OF THE DISTANCE BETWEEN THE PLANES
C                        I+1 AND I, SO THERE IS NO G(NOLAY)
C
       IF(NOLAY.EQ.1) THEN !  2D
         DO I=1,NPOIN2
           F(I,1) = 1.D0
           G(I,1) = 1.D0
         ENDDO
       ELSE !  3D
         DO ND1=1,NPOIN2
           DO NLAY = 1 , NOLAY !  DETERMINE F WITH THIS NODE
             ND2 = ND1 + (NLAY-1)*NPOIN2
             IF(NLAY.EQ.1) THEN
                F(ND1,NLAY)=(HNEW(ND2+NPOIN2)-HNEW(ND2       ))
     $               /2.D0/HTOT(ND1)
                G(ND1,NLAY)=F(ND1,NLAY)
             ELSEIF(NLAY.EQ.NOLAY) THEN
                F(ND1,NLAY)=(HNEW(ND2       )-HNEW(ND2-NPOIN2))
     $               /2.D0/HTOT(ND1)
             ELSE
                F(ND1,NLAY)=(HNEW(ND2+NPOIN2)-HNEW(ND2-NPOIN2))
     $               /2.D0/HTOT(ND1)
                G(ND1,NLAY)=(HNEW(ND2+NPOIN2)-HNEW(ND2       ))
     $               /2.D0/HTOT(ND1)
             ENDIF
           ENDDO
         ENDDO
       ENDIF
C
C
C
      IF (.NOT.TRICK) ITSTEP = INT(DDT)
      IF (.NOT.TRICK .AND. DDT < 1.D0 )
     *     STOP "DT CHANGED FROM BIGGER THAN 1 TO SMALLER THAN 1"
      IF (       TRICK .AND. DDT > 1.D0 )
     *     STOP "DT CHANGED FROM SMALLER THAN 1 TO BIGGER THAN 1"
      IF ( .NOT. TRICK .AND. ( FLOAT(INT(DDT)) .NE. DDT ) )
     *     STOP "DT IN FRACTIONS OF SECONDS IS NOT SUPPORTED"
      ATOLD = AAT
      IF(LLT.NE.0) VOLOLD = VOLUME   ! SAVE VOLUME OF PREV. TIME LEVEL
      IF ( NOLAY .EQ. 1 ) THEN
C
C        THIS IS THE 2D VOLUME AT START OF TIME STEP
C
         VOLUME = HTOT*AREA(1:NPOIN2)
C
!        REMOVED BY JMH ON 20/05/2008. TIDAL FLATS NOW ALLOWED
!        DELWAQ WILL DO ITS OWN CLIPPING
!        DO INODE  = 1 , NPOIN  ! FORCE AT LEAST A MM WATER DEPTH
!          VOLUME(INODE)=MAX(1.D-3*AREA(INODE),VOLUME(INODE))
!        ENDDO
C
      ELSE
C
C        NOTE THAT WAQ NUMBERS 3D FROM TOP TO BOTTOM !!!!
C        NLAY IS THE TELEMAC CURRENT PLANE NUMBER
C        ND1  IS THE WAQ VOLUMES COUNTER
C
         ND1 = 1
         DO NLAY = NOLAY , 1 , -1 ! REVERSED ORDER FOR WAQ
           DO INODE = 1, NPOIN2
             VOLUME(ND1)=HTOT(INODE)*F(INODE,NLAY)*AREA(INODE)
             VOLUME(ND1)=MAX(1.D-3*AREA(INODE),VOLUME(ND1))
             ND1  =  ND1  + 1
           ENDDO
         ENDDO
      ENDIF
C
      IF ( MOD(ISTEPA,NSTEPA) .EQ. 0 ) THEN
         IF(STATIO.NE.1) THEN
            IF(TRICK) THEN
              ITOLD = LLT                ! <=== CHANGED BY LEO
            ELSE
              ITOLD = INT(ATOLD)         ! <=== CHANGED BY LEO
            ENDIF
         ENDIF
      ENDIF
      IF ( MOD(ISTEPA,NSTEPA) .EQ. 0 ) THEN
         IF ( STATIO .NE. 1 ) THEN
!           IF ( TRICK ) THEN
!              WRITE(NSOU) ITOLD, (REAL(VOLUME(I)),I=1,NPOIN)
!           ELSE
               WRITE(NSOU) ITOLD, (REAL(VOLUME(I)),I=1,NPOIN)
!           ENDIF
         ENDIF
      ENDIF
      IF(LLT.EQ.0) THEN
        ISTEPA=ISTEPA+1
        RETURN                 ! INITIALISATION DONE
      ENDIF
C
C     HORIZONTAL VELOCITIES IN THE NODES (FOR MORPHOLOGY, REAERATION, ETC.)
C
      ND1 = 1                   ! WAQ NUMBERING IS TOP TO BOTTOM
      DO NLAY  = 1, NOLAY
         ND2 = (NOLAY-NLAY)*NPOIN2 + 1 ! TELEMAC NUMBERING IS BOTTOM TO TOP
         DO INODE = 1, NPOIN2
            VELO(ND1) = SQRT(U(ND2)**2+V(ND2)**2)
            ND1 = ND1 + 1
            ND2 = ND2 + 1
         ENDDO
      ENDDO
C
C        ZERO THE FLOWS AND THE EXCHANGE AREAS
C        NB: LAST DIRECTION OF ECHANGE AREAS REMAINS AT HORSURF
C
      IF(INIFLOW) THEN
        DO I=1,NOQ
          FLOW(I) = 0.D0
        ENDDO
      ELSE
!       3D PART SET TO ZERO (MAYBE NOT NECESSARY FOR VERTICAL FLOWS)
        IF(NOQ.GT.NSEG) THEN
          DO I=NSEG+1,NOQ
            FLOW(I) = 0.D0
          ENDDO
        ENDIF
      ENDIF
C
C     MAKE EXCHANGE AREAS
C
      AREAWQ(1:NOLAY*NSEG) = 0.D0
      DO IELEM=1,NELEM2
        DO NLAY = 1 , NOLAY
        DO I = 1,3
          ISEG  = ELTSEG(IELEM,I)
          ISEGL = ISEG + (NOLAY-NLAY)*NSEG ! WAQ ORDER
          AREAWQ(ISEGL) = AREAWQ(ISEGL) + 
     *            DIST(I,IELEM)*H*( F(GLOSEG(ISEG,1),NLAY)+
     *                              F(GLOSEG(ISEG,2),NLAY) )*0.5D0
        ENDDO
        ENDDO
      ENDDO
C
C     NOW THE MOST IMPORTANT THING, THE TRANSPORT ITSELF
C
C     THE FINITE ELEMENT FLUXES PER NODE ARE IN W
C
C----------------------------------------
C DIFFERENT OPTIONS TO COMPUTE THE FLUXES
C----------------------------------------
C
      IF(NOLAY.EQ.1) THEN
!
!     IOPT1=2
!     IF(IOPT1.EQ.0) THEN
C       CONSTANT=0
!       CALL FLUXVF(FLOW,W,TE1%R,.FALSE.,MESH%NSEG,
!    *              MESH%NELEM,MESH%ORISEG%I,MESH%ELTSEG%I,
!                   INIFLOW)
!     ELSEIF(IOPT1.EQ.1) THEN
C       CHI-TUAN PHAM'S CONSTANT
!       CALL CSTEFLUXVF(TE1%R,HPROP%R,UCONV%R,VCONV%R,
!    *                  MESH%XEL%R,MESH%YEL%R,MESH%IKLE%I,
!    *                  MESH%NELEM,MESH%NPOIN)
!       CALL FLUXVF(FLOW,MESH%W%R,TE1%R,.TRUE.,MESH%NSEG,
!    *              MESH%NELEM,MESH%ORISEG%I,MESH%ELTSEG%I,
!                   INIFLOW)
!     ELSEIF(IOPT1.EQ.2) THEN
C       LEO POSTMA'S CONSTANT
        CALL FLUXVFLEO(FLOW,W,NSEG,NELEM2,IKLE,ELTSEG,GLOSEG,
     *                 MAXSEG,INIFLOW)
!     ENDIF
!
      ELSE
!
!       TREATMENT OF FLUXES PER NODE
!       IN AN ELEMENT WITH GENERALISED SIGMA TRANSFORMATION
!       WE HAVE W1+W2+W3+W4+W5+W6=0 BUT NOT W1+W2+W3=0 AND W4+W5+W6=0
!       HERE A CONSTANT IS REMOVED SO THAT W1+W2+W3=0 AND W4+W5+W6=0
!       IT WILL BE THEN INTERPRETED AS VERTICAL FLUXES
!
        DO IELEM=1,NELEM
          W123=(W(IELEM,1)+W(IELEM,2)+W(IELEM,3))/3.D0
          W456=(W(IELEM,4)+W(IELEM,5)+W(IELEM,6))/3.D0
          IF(ABS(W123+W456).GT.1.D-8) THEN
            PRINT*,'PB W123456 .NE. 0 POUR IELEM=',IELEM
            CALL PLANTE(1)
            STOP
          ENDIF
          W(IELEM,1)=W(IELEM,1)-W123
          W(IELEM,2)=W(IELEM,2)-W123
          W(IELEM,3)=W(IELEM,3)-W123
          W(IELEM,4)=W(IELEM,4)-W456
          W(IELEM,5)=W(IELEM,5)-W456
          W(IELEM,6)=W(IELEM,6)-W456
        ENDDO
!
!       NOW EVERY LAYER IS TREATED LIKE IN 2D 
!
        DO NLAY=1,NOLAY
!
!         ASSEMBLING THE 3D EBE FLUXES IN A 2D ARRAY
          IF(NLAY.EQ.1) THEN
!           BOTTOM
            DO IELEM=1,NELEM2
              W2D(IELEM,1)=W(IELEM,1)
              W2D(IELEM,2)=W(IELEM,2)
              W2D(IELEM,3)=W(IELEM,3)
            ENDDO
          ELSEIF(NLAY.LT.NOLAY) THEN
!           INTERMEDIATE PLANE, IF ANY
            DO IELEM=1,NELEM2
              W2D(IELEM,1)=
     *        W(IELEM+NELEM2*(NLAY-1),1)+W(IELEM+NELEM2*(NLAY-2),4)
              W2D(IELEM,2)=
     *        W(IELEM+NELEM2*(NLAY-1),2)+W(IELEM+NELEM2*(NLAY-2),5)
              W2D(IELEM,3)=
     *        W(IELEM+NELEM2*(NLAY-1),3)+W(IELEM+NELEM2*(NLAY-2),6)
            ENDDO
          ELSE
!           FREE SURFACE
            DO IELEM=1,NELEM2
              W2D(IELEM,1)=W(IELEM+NELEM2*(NOLAY-2),4)
              W2D(IELEM,2)=W(IELEM+NELEM2*(NOLAY-2),5)
              W2D(IELEM,3)=W(IELEM+NELEM2*(NOLAY-2),6)
            ENDDO
          ENDIF
!
!         NOTE THAT PLANES ARE INVERTED IN DELWAQ, HENCE FLOW(*:*)
          CALL FLUXVFLEO(FLOW(1+(NOLAY-NLAY)*NSEG:(1+NOLAY-NLAY)*NSEG),
     *              W2D,NSEG,NELEM2,IKLE,ELTSEG,GLOSEG,MAXSEG,INIFLOW)
        ENDDO
C
C     IF(NOLAY.EQ.1) THEN...
      ENDIF
C
C     WRITE THE EXCHANGE AREAS (LAST DIRECTION REMAINS AT HORSURF)
C
      SUMAREA = SUMAREA + AREAWQ
      IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
        SUMAREA=SUMAREA/NSTEPA
        IF(STATIO.NE.1) THEN
          IF(TRICK) THEN
            WRITE(NMAB) ITOLD-NSTEPA,(REAL(SUMAREA(I)),I=1,NOQ)
          ELSE
            WRITE(NMAB) INT(AAT-NSTEPA*DDT),(REAL(SUMAREA(I)),I=1,NOQ)
          ENDIF
        ENDIF
        SUMAREA=0.D0
      ENDIF
C
C     WRITE THE SALINITY (AND INVERT THE PLANES FOR DELWAQ)
C
      IF(SALI_DEL) THEN
        SUMSALI=SUMSALI+SALI
        IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
          SUMSALI=SUMSALI/NSTEPA
          IF(STATIO.NE.1) THEN
            IF(TRICK) THEN
              WRITE(NSAL) ITOLD-NSTEPA,
     *((REAL(SUMSALI(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ELSE
              WRITE(NSAL) INT(AAT-NSTEPA*DDT),
     *((REAL(SUMSALI(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ENDIF
          ENDIF
          SUMSALI=0.D0
        ENDIF
      ENDIF
C
C     WRITE THE TEMPERATURE (AND INVERT THE PLANES FOR DELWAQ)
C
      IF(TEMP_DEL) THEN
        SUMTEMP=SUMTEMP+TEMP
        IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
          SUMTEMP=SUMTEMP/NSTEPA
          IF(STATIO.NE.1) THEN
            IF(TRICK) THEN
              WRITE(NTEM) ITOLD-NSTEPA,
     *((REAL(SUMTEMP(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ELSE
              WRITE(NTEM) INT(AAT-NSTEPA*DDT),
     *((REAL(SUMTEMP(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ENDIF
          ENDIF
          SUMTEMP=0.D0
        ENDIF
      ENDIF
C
C     WRITE THE DIFFUSION (AND INVERT THE PLANES FOR DELWAQ)
C
      IF(DIFF_DEL) THEN
        SUMVISC=SUMVISC+VISC
        IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
          SUMVISC=SUMVISC/NSTEPA
          IF(STATIO.NE.1) THEN
            IF(TRICK) THEN
              WRITE(NVIS) ITOLD-NSTEPA,
     *((REAL(SUMVISC(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ELSE
              WRITE(NVIS) INT(AAT-NSTEPA*DDT),
     *((REAL(SUMVISC(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ENDIF
          ENDIF
          SUMVISC=0.D0
        ENDIF
      ENDIF
C
C     WRITE THE VELOCITY (AND NOT !!!!!!    INVERT THE PLANES FOR DELWAQ)
C                         AS IT HAS BEEN DONE WITH DELWAQ NUMBERING
C
      IF(VELO_DEL) THEN
        SUMVELO=SUMVELO+VELO
        IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
          SUMVELO=SUMVELO/NSTEPA
          IF(STATIO.NE.1) THEN
            IF(TRICK) THEN
              WRITE(NVEL) ITOLD-NSTEPA,(REAL(SUMVELO(I)),I=1,NPOIN)
            ELSE
              WRITE(NVEL) INT(AAT-NSTEPA*DDT),
     *                    (REAL(SUMVELO(I)),I=1,NPOIN)
            ENDIF
          ENDIF
          SUMVELO=0.D0
        ENDIF
      ENDIF
C
C     MAKE THE VERTICAL FLOWS HERE
C     FIRST THE NEW VOLUMES WITHOUT VERTICAL FLOWS
C
      DO NLAY = 1 , NOLAY       ! NOW IN WAQ ORDER
         DO ISEG = 1 , NSEG
            IFROM = NODENRS(GLOSEG(ISEG,1))
            ITO   = NODENRS(GLOSEG(ISEG,2))
            ISEGL = ISEG + (NLAY-1)*NSEG
            IF ( IFROM .GT. 0 ) THEN
               IFROM = IFROM + (NLAY-1)*NPOIN2
               VOLOLD(IFROM) = VOLOLD(IFROM) - FLOW(ISEGL) * DDT
            ENDIF
            IF(ITO.GT.0) THEN
               ITO = ITO + (NLAY-1)*NPOIN2
               VOLOLD(ITO) = VOLOLD(ITO) + FLOW(ISEGL) * DDT
            ENDIF
         ENDDO
      ENDDO
C
C     THEN THE TOTAL NEW VOLUMES: VOL2 IS THE SUM OF ALL
C     3D VOLUMES OF A VERTICAL
C
      DO INODE = 1 , NPOIN2
        VOL2(INODE)=0.D0
        DO NLAY=1,NOLAY
          ND1 = INODE + (NLAY-1)*NPOIN2
          VOL2(INODE) = VOL2(INODE) + VOLOLD(ND1)
        ENDDO
      ENDDO
C
C     NOW MAKE THE VERTICAL FLOWS (STORED AFTER THE HORIZONTAL FLOWS)
C
      ND2 = NOLAY*NSEG
      DO INODE = 1 , NPOIN2
        IF(NOLAY.GT.1) THEN
C       FROM BOTTOM TO LAYER BELOW THE FREE SURFACE
        DO NLAY=1,NOLAY-1
          ND1 = INODE + (NLAY-1)*NPOIN2
          FLOW(ND1+ND2)=(VOLOLD(ND1)-VOL2(INODE)*
     *                   F(INODE,NOLAY-NLAY+1))/DDT
          VOLOLD(ND1       )= VOLOLD(ND1       ) - FLOW(ND1+ND2)*DDT
          VOLOLD(ND1+NPOIN2)= VOLOLD(ND1+NPOIN2) + FLOW(ND1+ND2)*DDT
        ENDDO
        ENDIF
      ENDDO
C
C     TESTING MASS ERRORS ON INTERNAL NODES
C
      IF(INFOGR) THEN
        SSUM  =0.D0
        ERRTOT=0.D0
        DO I2D=1,NPOIN2
          IF(NODENRS(I2D).GT.0) THEN
            DO NLAY=1,NOLAY        
              I3D=I2D+NPOIN2*(NLAY-1)
              SSUM=MAX(SSUM,ABS(VOLOLD(I3D)-VOLUME(I3D)))
              ERRTOT=ERRTOT+VOLOLD(I3D)-VOLUME(I3D)        
            ENDDO
          ENDIF
        ENDDO
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'TEL4DEL : ERREUR DE MASSE MAXIMUM:',SSUM
         WRITE(LU,*) '          SOMME SUR LES POINTS INTERIEURS:',ERRTOT
        ELSE
         WRITE(LU,*) 'TEL4DEL: MAXIMUM MASS ERROR:',SSUM
         WRITE(LU,*) '         SUM OF ERRORS ON INTERNAL POINTS:',ERRTOT
        ENDIF
        WRITE(LU,*) ' '
      ENDIF
C 
C     WRITE THE FLOWS (MULTIPLICATION WITH DT IS TRICK TO MODEL < 1 SEC
C
CCCCCCAGREG
      DO I=1,NOQ
        SUMFLOW(I) = SUMFLOW(I) + FLOW(I)
      ENDDO
      IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
        SUMFLOW = SUMFLOW / NSTEPA
        IF(STATIO.NE.1) THEN
          IF(TRICK) THEN
            SUMFLOW = SUMFLOW*DDT
            WRITE(NCOU) ITOLD-NSTEPA , (REAL(SUMFLOW(I)),I=1,NOQ)
          ELSE
            WRITE(NCOU) INT(AAT-NSTEPA*DDT), (REAL(SUMFLOW(I)),I=1,NOQ)
          ENDIF
        ENDIF
        SUMFLOW = 0.D0
      ENDIF
CCCCCCAGREG
C
C        STATIONARY DATABASE IF REQUIRED
C
 20   CONTINUE
C
      IF ( LLT .LT. NNIT ) THEN
         ISTEPA = ISTEPA + 1
         RETURN                 !  FINALISATION
      ENDIF
      IF ( STATIO .EQ. 1 ) THEN
         IF ( TRICK ) THEN
            WRITE(NSOU) ITOLD , (REAL(VOLUME(I)),I=1,NPOIN)
            WRITE(NMAB) ITOLD , (REAL(SUMAREA(I)),I=1,NOQ)
            WRITE(NCOU) ITOLD , (REAL(SUMFLOW(I)),I=1,NOQ)
         ELSE
            WRITE(NSOU) INT(ATOLD), (REAL(VOLUME(I)),I=1,NPOIN)
            WRITE(NMAB) INT(ATOLD), (REAL(SUMAREA(I)),I=1,NOQ)
            WRITE(NCOU) INT(ATOLD), (REAL(SUMFLOW(I)),I=1,NOQ)
         ENDIF
         DO NLAY  = 1, NOLAY
            DO ISEG  = 1, NSEG
               ISEGL  = ISEG + (NOLAY-NLAY)*NSEG ! WAQ ORDER
               IFROM = NODENRS(GLOSEG(ISEG,1))
               IF ( IFROM .GT. 0 ) THEN
                  IFROM = IFROM + (NLAY-1)*NPOIN2
                  VOLUME(IFROM) = VOLUME(IFROM) - FLOW(ISEGL)*DDT
               ENDIF
               ITO   = NODENRS(GLOSEG(ISEG,2))
               IF ( ITO   .GT. 0 ) THEN
                  ITO   = ITO   + (NLAY-1)*NPOIN2
                  VOLUME(ITO  ) = VOLUME(ITO  ) + FLOW(ISEGL)*DDT
               ENDIF
            ENDDO
         ENDDO
         ND2 = NOLAY*NSEG
         DO INODE = 1 , NPOIN2
            DO ND1 = INODE , NPOIN-NPOIN2 , NPOIN2
               VOLUME(ND1)       = VOLUME(ND1)       - FLOW(ND1+ND2)*DDT
               VOLUME(ND1+NPOIN2)= VOLUME(ND1+NPOIN2)+ FLOW(ND1+ND2)*DDT
            ENDDO
         ENDDO
         IF(TRICK) THEN
           WRITE(NSOU) ITOLD,(REAL(VOLUME(I)),I=1,NPOIN)
         ELSE
           WRITE(NSOU) INT(AAT-NSTEPA*DDT),(REAL(VOLUME(I)),I=1,NPOIN)
         ENDIF
      ENDIF
C
C     DUMMY RECORDS AT THE END. THEY ARE NOT USED BY WAQ BUT SHOULD BE THERE
C
      SUMAREA = 0.D0
      SUMFLOW = 0.D0
      IF ( TRICK ) THEN
         WRITE ( NMAB ) ITOLD , (REAL(SUMAREA(I)),I=1,NOQ)
         WRITE ( NCOU ) ITOLD , (REAL(SUMFLOW(I)),I=1,NOQ)
         ITSTOP = ITOLD
      ELSE
         WRITE ( NMAB ) INT(AAT), (REAL(SUMAREA(I)),I=1,NOQ)
         WRITE ( NCOU ) INT(AAT), (REAL(SUMFLOW(I)),I=1,NOQ)
         ITSTOP = INT(AAT)
         NSTEPA = INT(NSTEPA*DDT)   ! FOR WRHYD ONLY
      ENDIF
C
C     WRITING A COMMAND FILE FOR DELWAQ
C
      CALL WRIHYD( TITRE  , ITSTRT , ITSTOP , ITSTEP , NPOIN ,
     *             NSEG   , NOLAY  , NOMGEO , NOMLIM ,
     *             F      , NSTEPA ,
     *             NOMSOU , NOMMAB , NOMCOU , NOMINI , NOMVEB,
     *             NOMMAF , NOMSAL , NOMTEM , NOMVEL , NOMVIS,
     *             NCOB   ,
     *             SALI_DEL,TEMP_DEL,VELO_DEL,DIFF_DEL,MARDAT , MARTIM)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
