C**************************************************************************
C                                                                         *
C  SSSSS  PPPPPP   AAAAA  RRRRRR  TTTTTTT  AAAAA   CCCCC  U     U  SSSSS  *
C S     S P     P A     A R     R    T    A     A C     C U     U S     S *
C S       P     P A     A R     R    T    A     A C       U     U S       *
C  SSSSS  PPPPPP  AAAAAAA RRRRRR     T    AAAAAAA C       U     U  SSSSS  *
C       S P       A     A R     R    T    A     A C       U     U       S *
C S     S P       A     A R     R    T    A     A C     C U     U S     S *
C  SSSSS  P       A     A R     R    T    A     A  CCCCC   UUUUU   SSSSS  *
C                                                                         *
C             22222  DDDDDD            V     V 555555 PPPPPP  9999999     *
C            2     2 D     D           V     V 5      P     P 9     9     *
C                  2 D     D           V     V 5      P     P 9     9     *
C             22222  D     D           V     V 555555 PPPPPP  9999999     *
C            2       D     D            V   V       5 P             9     *
C            2       D     D             V V        5 P             9     *
C            2222222 DDDDDD               V    555555 P       9999999     *
C                                                                         *
C**************************************************************************
C
C                        *********************
                          PROGRAM SPARTACUS2D
C                        *********************
C
C-----------------------------------------------------------------------
C
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2009
C
C FONCTION : programme principal
C FUNCTION : main program
C
C PROGRAMMES APPELANT : -
C CALLED BY            
C
C PROGRAMMES APPELES  : LECPARAM  , INITIAL  , OUVERTURE, INTERPOL ,
C CALLED PROGRAMS       ECRIT     , TABLIEN  , KERNEL   , GRADVIT  ,
C                       FROTTEMENT, IMPULSION, PASTEMPS , MOUVEMENT,
C                       CONTINUITE, ETAT     , ENTPAR   , SORPAR   ,
C                       PRIVEE
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    !UNITY !               MEANING                          !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! AX, AZ    ! MS-2 ! PARTICLE ACCELERATION COMPONENTS               !
C ! CK1, CMU  !  -   ! K-EPSILON MODEL COEFFICIENTS                   !
C ! CPAR      !M2S-2 ! WALL FORCE 1 COEFFICIENT                       !
C ! CR        !  -   ! COEFFICIENT RELATIVE TO A ROUGH LOG LAW        !
C ! CT        !  -   ! TECPLOT CURRENT ZONE NUMBER                    !
C ! CTHETA,                                                           !
C ! STHETA    !  -   ! COMPONENTS OF WALL NORMAL VECTORS              !
C ! DEB       !MS-1  ! COMPUTED MEAN BULK VELOCITY                    !
C ! DEBREF    !MS-1  ! PRESCRIBED MEAN BULK VELOCITY                  !
C ! DELTA     !  M   ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
C ! DELTADR   !  -   ! RATIO DELTA/DR                                 !
C ! DESTDR    !  -   ! RATIO DEST/DR                                  !
C ! DR        !  M   ! INITIAL INTERPARTICLE SPACING                  !
C ! DT        !  S   ! TIME STEP                                      !
C ! EPS       !M2S-3 ! DISSIPATION RATE                               !
C ! ETA2      !  M2  ! CORRECTION TERM FOR DIFFUSION                  !
C ! FACTLISS  ! -    ! DENSITY SMOOTHING COEFFICIENT                  !
C ! FEXTX,                                                            !
C ! FEXTZ     ! MS-2 ! EXTERNAL FORCE COMPONENTS                      !
C ! FICHINIT  !  -   ! INITIALIZING FILE                              !
C ! FICHMAILL !  -   ! MESH FILE FOR RUBENS                           !
C ! FICHPARAM !  -   ! PARAMETER FILE                                 !
C ! FICHPOS   !  -   ! POSITION FILE                                  !
C ! FICHRES   !  -   ! FIELD FILE                                     !
C ! FICHSUITE !  -   ! DATA FILE FOR COMPUTATION CONTINUED            !
C ! FMOT      ! MS-2 ! AXIAL DRIVING FORCE                            !
C ! FORCST    ! MS-2 ! PRESCRIBED AXIAL DRIVING FORCE                 !
C ! FPARX,                                                            !
C ! FPARZ     ! MS-2 ! WALL FORCE COMPONENTS                          !
C ! FPRES     !M5S-2 ! MOMENTUM FLUX RELATIVE TO PRESSURE FORCES      !
C !             KG-1                                                  !
C ! FVISQ     !M5S-2 ! MOMENTUM FLUX RELATIVE TO VISCOUS FORCES       !
C !             KG-1                                                  !
C ! GAMMAX,                                                           !
C ! GAMMAZ    ! S-1  ! DAMPING COEFFICIENTS                           !
C ! GKERX,                                                            !
C ! GKERZ     ! M-3  ! KERNEL DERIVATIVE COMPONENTS                   !
C ! GRAV      ! MS-2 ! GRAVITY                                        !
C ! GVXX, GVXZ                                                        !
C ! GVZX, GVZZ! S-1  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
C ! H         !  M   ! SMOOTHING LENGTH                               !
C ! HDR       !  -   ! RATIO H/DR                                     !
C ! I,J,L     !  -   ! PARTICLE INDEXS                                !
C ! ILIEN     !  -   ! PARTICLE LINK LIST                             !
C ! IPARTQ    !  -   ! INDEX OF PARTICLES LOCATED IN A SQUARE         !
C ! IT        !  -   ! INDEX OF THE CURRENT TIME STEP                 !
C ! KAPPA     !  -   ! VON KARMAN CONSTANT                            !
C ! KCPAR     !  -   ! CHOICE INDEX FOR WALL MODELLING                !
C ! KDEF      !  -   ! CHOICE INDEX FOR STRAIN MODEL                  !
C ! KENT      !  -   ! CONDITION TYPE AT FLUID BOUNDARIES             !
C ! KFLUID    !  -   ! FLUID TYPE                                     !
C ! KFPAR     !  -   ! CHOICE INDEX FOR WALL FORCES                   !
C ! KGAMMAX,                                                          !
C ! KGAMMAZ   !  -   ! LOGICAL INDEX FOR DAMPING                      !
C ! KGRAV     !  -   ! LOGICAL INDEX FOR THE GRAVITY                  !
C ! KKERNEL   !  -   ! CHOICE INDEX FOR KERNEL                        !
C ! KLIST     !  -   ! LOGICAL FOR LISTING PRINTOUT                   !
C ! KLISS     ! -    ! CHOICE INDEX FOR THE DENSITY SMOOTHING         !
C ! KMOT      !  -   ! CHOICE INDEX FOR THE FORCING TERM              !
C ! KPAR      !  -   ! PARTICLE  TYPE                                 !
C ! KPARM     !  -   ! LOGICAL INDEX FOR MOVING WALL                  !
C ! KPARMOB   !  -   ! MOVING WALL OR EDGE PARTICLE TYPE              !
C ! KPER      !  -   ! LOGICAL INDEX FOR PERIODICITY                  !
C ! KPRES     !  -   ! CHOICE INDEX FOR THE PRESSURE GRADIENT MODEL   !
C ! KPROD     !  -   ! CHOICE INDEX FOR PRODUCTION MODEL              !
C ! KSUITE    !  -   ! LOGICAL FOR CALCULATION CONTINUED              !
C ! KSORTP    !  -   ! LOGICAL FOR POSITION PRINTOUT                  !
C ! KSORTR    !  -   ! LOGICAL FOR FIELD PRINTOUT                     !
C ! KTURB     !  -   ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! KUSTAR    !  -   ! CHOICE INDEX FOR FRICTION VELOCITY COMPUTATION !
C ! KVISQ     !  -   ! CHOICE INDEX FOR THE VISCOUS MODEL             !
C ! KVISU     !  -   ! CHOICE INDEX FOR THE POSTPROCESSOR             !
C ! LM        !  M   ! MIXING LENGTH                                  !
C ! LNG       !  -   ! CHOICE INDEX FOR LANGUAGE                      !
C ! MASS      !  KG  ! PARTICLE MASS                                  !
C ! NBMAX     !  -   ! MAXIMUM NUMBER OF EDGE PARTICLES               !
C ! NELEM     !  -   ! NUMBER OF MESH ELEMENTS FOR RUBENS             !
C ! NFLUID    !  -   ! FLUID NUMBER                                   !
C ! NFLUIDMAX !  -   ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NLIEN     !  -   ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  !  -   ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NLIST     !  -   ! LISTING PRINTOUT PERIOD                        !
C ! NOMETUDE  !  -   ! NAME OF THE INVESTIGATED CASE                  !
C ! NPARB     !  -   ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     !  -   ! NUMBER OF FLUID PARTICLES                      !
C ! NPART     !  -   ! TOTAL PARTICLE NUMBER                          !
C ! NPARTQ    !  -   ! NUMBER OF PARTICLES IN A SQUARE                !
C ! NPARTQMAX !  -   ! MAXIMUM NUMBER OF PARTICLES IN A SQUARE        !
C ! NPMAX     !  -   ! MAXIMUM PARTICLE NUMBER                        !
C ! NPOIN     !  -   ! NUMBER OF MESH POINTS FOR RUBENS               !
C ! NQUADX,                                                           !
C ! NQUADZ    !  -   ! NUMBER OF SQUARES ALONG EACH DIRECTION         !
C ! NQUADXMAX,                                                        !
C ! NQUADZMAX !  -   ! MAXIMUM NUMBER OF SQUARES ALONG EACH DIRECTION !
C ! NSORTP    !  -   ! POSITION PRINTOUT PERIOD                       !
C ! NSORTR    !  -   ! FIELD PRINTOUT PERIOD                          !
C ! NT        !  -   ! NUMBER OF TIME STEPS                           !
C ! NU0       !M2S-1 ! MOLECULAR VISCOSITIES                          !
C ! NUT       !M2S-1 ! EDDY VISCOSITY                                 !
C ! NVARSOR   !  -   ! PRINTOUT VARIABLE NUMBER                       !
C ! NVARSORMAX!  -   ! MAXIMUM NUMBER OF PRINTOUT VARIABLES           !
C ! P         !KGM-1 ! PRESSURE                                       !
C !            S-2                                                    !
C ! PI        !  -   ! ARCHIMEDE'S NUMBER                             !
C ! PRIV      !  -   ! PRIVATE PRINTOUT VARIABLE FOR THE USER         !
C ! PROD      !M2S-3 ! PRODUCTION RATE                                !  
C ! R0        !  M   ! WALL ACTION DISTANCE                           !
C ! RAB       !  M   ! INTERPARTICLE DISTANCE                         !
C ! RHO       !KGM-2 ! DENSITY                                        !
C ! RHO0      !KGM-2 ! REFERENCE DENSITIES                            !
C ! RUG       !  M   ! WALL ROUGHNESS                                 !
C ! S         ! S-1  ! RATE OF STRAIN                                 !
C ! SUPP      !  -   ! KERNEL SUPPORT                                 !
C ! TEMPS     !  S   ! PHYSICAL TIME                                  !
C ! THETA     !  o   ! ANGLE BETWEEN WALL NORMAL VECTOR AND X-AXIS    !
C ! TKE       !M2S-2 ! TURBULENT KINETIC ENERGY                       !
C ! USTAR     ! MS-1 ! FRICTION VELOCITY                              !
C ! VITC0     ! MS-1 ! SPEED OF SOUND                                 !
C ! VX, VZ    ! MS-1 ! VELOCITY COMPONENTS                            ! 
C ! VXAB, VZAB! MS-1 ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
C ! X, Z      !  M   ! PARTICLE POSITION                              !
C ! XAB, ZAB  !  M   ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
C ! XINT, ZINT!  M   ! COORDINATES OF MESH POINTS                     !
C ! XMIN, XMAX!  M   ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
C ! ZMIN, ZMAX!  M   ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
C !___________!______!________________________________________________!
C
C Variables
C==========
C
      IMPLICIT NONE
C
C Variables primaires
C--------------------
C Primary variables
C------------------
C
      INTEGER NPMAX   , NLIENMAX , NVARSORMAX , NFLUIDMAX , NQUADZMAX
      INTEGER NPARF   , NPART    , NT         , NLIST     , NSORTR
      INTEGER NVARSOR , NSORTP   , NPARTQMAX  , NQUADXMAX , NQUADX
      INTEGER NQUADZ  , NPOIN    , NELEM      , NBMAX     , NPARB
      INTEGER KVISU   , KTURB    , KCPAR      , KVISQ     , KKERNEL
      INTEGER KFPAR   , KPRES    , KMOT       , KPROD     , KDEF
      INTEGER KUSTAR  , IT       , CT         , LNG       , LU   
      INTEGER NFLUID    
C
      COMMON/INFO/LNG,LU
C
      LOGICAL KSORTR , KSORTP , KLIST , KSUITE
      LOGICAL KGRAV  , KPER   , KPARM , KLISS
      LOGICAL KGAMMAX, KGAMMAZ
C
      DOUBLE PRECISION VITC0  , DEBREF  , CMU
      DOUBLE PRECISION XMIN   , XMAX    , GAMMAX , CK1
      DOUBLE PRECISION ZMIN   , ZMAX    , GAMMAZ , KAPPA
      DOUBLE PRECISION ETA2   , GRAV    , CPAR   , CR
      DOUBLE PRECISION R0     , TEMPS   , DT     , PI
      DOUBLE PRECISION H      , DR      , HDR    , FACTLISS
      DOUBLE PRECISION FORCST , DEB     , SUPP   , DESTDR
      DOUBLE PRECISION DELTA  , DELTADR , RUG    
C
      CHARACTER*40 NOMETUDE  , FICHPARAM
      CHARACTER*80 FICHINIT  , FICHSUITE
      CHARACTER*40 FICHRES   , FICHPOS
      CHARACTER*80 FICHMAILL
C
C Parametres modifiables
C-----------------------
C Adjustable parameters
C----------------------
C
      PARAMETER (NPMAX     = 35000 ,
     .           NBMAX     = 5000  ,
     .           NLIENMAX  = 100   ,
     .           NPARTQMAX = 100   ,
     .           NQUADXMAX = 1040  ,
     .           NQUADZMAX = 160   ,
     .           NPOIN     = 40501 ,
c     .           NPOIN     = 40401 ,
     .           NELEM     = 80000 )
C
C Parametres a ne pas modifier
C-----------------------------
C Non adjustable parameters
C--------------------------
C
      PARAMETER (NVARSORMAX = 16     ,
     .           NFLUIDMAX  = 3      )
C
C Tableaux
C---------
C Arrays
C-------
C
      INTEGER ILIEN   (NPMAX  , NLIENMAX)
      INTEGER NLIEN   (NPMAX) , KFLUID(NPMAX)
      INTEGER KENT    (NPMAX) , KPAR  (NPMAX)
      INTEGER KPARMOB (NPMAX)
C
      INTEGER IPARTQ(NQUADXMAX,NQUADZMAX,NPARTQMAX)
      INTEGER NPARTQ(NQUADXMAX,NQUADZMAX)
C
      REAL XINT(NPOIN) , ZINT(NPOIN)
C
      DOUBLE PRECISION X     (NPMAX) , Z     (NPMAX) , RHO  (NPMAX)
      DOUBLE PRECISION VX    (NPMAX) , VZ    (NPMAX) , P    (NPMAX)
      DOUBLE PRECISION AX    (NPMAX) , AZ    (NPMAX) , MASS (NPMAX)
      DOUBLE PRECISION GVXX  (NPMAX) , GVXZ  (NPMAX) , FMOT (NPMAX)
      DOUBLE PRECISION GVZX  (NPMAX) , GVZZ  (NPMAX) , USTAR(NBMAX)
      DOUBLE PRECISION NUT   (NPMAX) , TKE   (NPMAX) , EPS  (NPMAX)
      DOUBLE PRECISION S     (NPMAX) , LM    (NPMAX) , THETA(NBMAX)
      DOUBLE PRECISION CTHETA(NBMAX) , STHETA(NBMAX) , PRIV (NPMAX)
C
      DOUBLE PRECISION RHO0 (NFLUIDMAX)      ,NU0  (NFLUIDMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX) ,GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX) ,ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX) ,VZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX) ,KER4_VAL(NPMAX,0:NLIENMAX)
C
      PRINT*,''
      PRINT*,''
      PRINT*,''
      PRINT222,'================================================',
     .         '========================'      
      PRINT222,'                            SPARTACUS-2D V5P9'
      PRINT222,'================================================',
     .         '========================'   
C
 222  FORMAT (A48,A24)
C
C Lecture des parametres
C=======================
C Parameter reading
C==================
C
      CALL LECPARAM
C
     .  (NFLUIDMAX, NLIST    , NSORTP , NSORTR , NT       , NFLUID   , 
     .   KCPAR    , KDEF     , KFPAR  , KGAMMAZ, KGAMMAX  , KGRAV    , 
     .   KLISS    , KKERNEL  , KMOT   , KPARM  , KPER     , KPRES    , 
     .   KPROD    , KSUITE   , KTURB  , KUSTAR , KVISQ    , KVISU    ,
     .   CPAR     , DEBREF   , DELTADR, DESTDR , FORCST   , FACTLISS , 
     .   GAMMAX   , GAMMAZ   , HDR    , NU0    , RHO0     , RUG      , 
     .   VITC0    , XMIN     , XMAX   , ZMIN   , ZMAX     , FICHINIT , 
     .   FICHPARAM, FICHSUITE, FICHRES, FICHPOS, FICHMAILL, NOMETUDE )
C
C Initialisations
C================
C Initializations
C================
C
      CALL INITIAL
C
     .  (NBMAX, NFLUIDMAX, NPARB  , NPARF  , NPART, NPMAX ,
     .   CT   , KENT     , KFLUID , KKERNEL, KPAR , KSUITE,
     .   KTURB, CK1      , KPARMOB, CMU    , CR   , CTHETA, 
     .   DEB  , DELTA    , DELTADR, DR     , DT   , EPS   , 
     .   ETA2 , FMOT     , GRAV   , H      , HDR  , KAPPA , 
     .   LM   , MASS     , NUT    , P      , PI   , PRIV  , 
     .   R0   , RHO      , RHO0   , STHETA , SUPP , TEMPS ,
     .   THETA, TKE      , VITC0  , VX     , VZ   , X     , 
     .   Z    , S                                         )
C
C Ouvertures fichiers sorties
C============================
C Printout file opening
C======================
C
      CALL OUVERTURE
C
     .  (NELEM, NPOIN, NVARSOR,NVARSORMAX, KVISU, 
     .   XINT , XMIN , XMAX   ,ZINT      , ZMIN , 
     .   ZMAX , DR                              )
C
C Calcul d une nouvelle variable de sortie
C=========================================
C Computation of a new printout variable
C=======================================
C
      CALL PRIVEE 
C
     .  (NPMAX, NPART, EPS, MASS, NUT, P ,
     .   PRIV , RHO  , S  , TKE , VX , VZ,
     .   X    , Z                        )
C
C Ecritures fichiers sorties
C===========================
C Printout file writing
C======================
C
      KSORTR=.TRUE.
      KSORTP=.TRUE.
C
      IF (KVISU.EQ.1) THEN
C
        CALL TABLIEN
C
     .   (NLIEN , NLIENMAX , NPART , NPARTQ   , NPARTQMAX, NPMAX ,
     .    NQUADX, NQUADXMAX, NQUADZ, NQUADZMAX, ILIEN    , IPARTQ,
     .    KLIST , KPER     , H     , RAB      , SUPP     , VX    , 
     .    VXAB  , VZ       , VZAB  , X        , XMIN     , XMAX  , 
     .    XAB   , Z        , ZMIN  , ZAB                         )
C
        CALL INTERPOL
C
     .  (NPARTQ, NPARTQMAX, NPMAX , NPOIN, NQUADX, NQUADXMAX, 
     .   NQUADZ, NQUADZMAX, IPARTQ, KPER , EPS   , H        , 
     .   MASS  , NUT      , P     , PI   , PRIV  , RHO      , 
     .   S     , SUPP     , TEMPS , TKE  , VX    , VZ       , 
     .   X     , XINT     , XMIN  , XMAX , Z     , ZINT     , 
     .   ZMIN                                               )      
C
      ENDIF
C
      CALL ECRIT
C
     .  (NBMAX , NPARB, NPARF  , NPART , NPMAX, NT    ,
     .   CT    , IT   , KENT   , KFLUID, KPAR , KSORTP,
     .   KSORTR, KVISU, KPARMOB, EPS   , MASS , NUT   , 
     .   P     , PRIV , RHO    , S     , TEMPS, THETA , 
     .   TKE   , VX   , VZ     , X     , Z            )
C
C===============================
C Debut de la boucle en temps  *
C===============================
C Time loop beginning *
C======================
C
      DO 301 IT = 1,NT
C
        KSORTR=.FALSE.
        KSORTP=.FALSE.
        KLIST =.FALSE.
        IF (MOD(IT,NSORTR).EQ.0)            KSORTR=.TRUE.
        IF (MOD(IT,NSORTP).EQ.0)            KSORTP=.TRUE.
        IF (MOD(IT,NLIST ).EQ.0.OR.IT.EQ.1) KLIST =.TRUE.
C
      IF (KLIST) THEN      
        PRINT*
        PRINT*,'Iteration : ',IT
        PRINT*,'---------------------------------------------',
     .         '----------'
      ENDIF
C
C Construction du quadrillage et du tableau de liens
C===================================================
C Coarse grid and link list construction
C=======================================
C
      CALL TABLIEN
C
     .   (NLIEN , NLIENMAX , NPART , NPARTQ   , NPARTQMAX, NPMAX ,
     .    NQUADX, NQUADXMAX, NQUADZ, NQUADZMAX, ILIEN    , IPARTQ,
     .    KLIST , KPER     , H     , RAB      , SUPP     , VX    , 
     .    VXAB  , VZ       , VZAB  , X        , XMIN     , XMAX  , 
     .    XAB   , Z        , ZMIN  , ZAB                         )
C
C Calcul du gradient du noyau
C============================
C Kernel derivative computation
C==============================
C
      CALL KERNEL
C
     .  (NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, KKERNEL,
     .   GKERX, GKERZ   , H    , PI   , RAB  , XAB    ,
     .   ZAB                                          )
C
      IF (KTURB.NE.0) THEN
C
C Gradient de vitesse
C====================
C Velocity gradient
C==================
C
        CALL GRADVIT
C
     .   (NLIEN, NLIENMAX, NPARB, NPARF, NPART, NPMAX, 
     .    ILIEN, KPAR    , GKERX, GKERZ, GVXX , GVXZ , 
     .    GVZX , GVZZ    , MASS , RHO  , VXAB , VZAB )
C
C Vitesse de frottement
C======================
C Friction velocity
C==================
C
        CALL FROTTEMENT
C
     .  (NBMAX , NPARB    , NPARF , NPARTQ   , NPARTQMAX, NPMAX,
     .   NQUADX, NQUADXMAX, NQUADZ, NQUADZMAX, IPARTQ   , CR   ,
     .   KPER  , KUSTAR   , CTHETA, DELTA    , DESTDR   , DR   , 
     .   GVXX  , GVXZ     , GVZX  , GVZZ     , H        , KAPPA,
     .   MASS  , PI       , RHO   , RUG      , STHETA   , SUPP , 
     .   USTAR , VX       , VZ    , X        , XMIN     , XMAX ,
     .   Z     , ZMIN                                          )
C
      ENDIF
C
C Equation de quantite de mouvement
C==================================
C Momentum equation
C==================
C                       
      CALL IMPULSION
C
     .  (NBMAX , NLIEN    , NLIENMAX, NPARB  , NPARF, NPMAX ,
     .   NPART , NFLUIDMAX, ILIEN   , KAPPA  , KCPAR, KDEF  , 
     .   KFPAR , KFLUID   , KGAMMAX , KGAMMAZ, KGRAV, KMOT  , 
     .   KPAR  , KPRES    , KPROD   , KTURB  , KVISQ, AX    ,
     .   AZ    , CK1      , CMU     , CPAR   , DEB  , DEBREF,
     .   DELTA , DR       , DT      , EPS    , ETA2 , FMOT  ,
     .   FORCST, GAMMAX   , GAMMAZ  , GKERX  , GKERZ, GRAV  ,
     .   GVXX  , GVXZ     , GVZX    , GVZZ   , H    , LM    ,
     .   MASS  , NU0      , NUT     , P      , R0   , RAB   , 
     .   RHO   , S        , TKE     , USTAR  , VITC0, VX    ,
     .   VXAB  , VZ       , VZAB    , X      , XAB  , XMIN  , 
     .   ZAB   , Z                                          )
C
C Pas de temps
C=============
C Time step
C==========
C
      CALL PASTEMPS
C
     .   (NPARF, NPMAX, NFLUIDMAX, KLIST, KFLUID, AX   , 
     .    AZ   , DT   , H        , NU0  , NUT   , TEMPS,
     .    VITC0                                        )
C
C Mouvement des particules
C=========================
C Particle displacement
C======================
C 
      CALL MOUVEMENT
C
     .  (NBMAX , NPARB , NPARF, NPART  , NPMAX, CR  ,
     .   KPAR  , KPER  , KTURB, KPARMOB, KPARM, AX  , 
     .   AZ    , CTHETA, DELTA, DT     , KAPPA, RUG , 
     .   STHETA, USTAR , VX   , VZ     , X    , XMIN, 
     .   XMAX  , Z     , TEMPS, PI                  )
C
C Equation de continuite
C=======================
C Continuity equation
C====================
C
      CALL CONTINUITE
C
     .  (ILIEN   , NFLUIDMAX, NLIEN , NLIENMAX, NPARB, NPARF   ,
     .   NPART   , NPMAX    , KFLUID, KLISS   , KGRAV, FACTLISS, 
     .   KER4_VAL, DT       , GKERX , GKERZ   , H    , MASS    ,
     .   RAB     , RHO      , RHO0  , VX      , VZ             )
C
C Equation d'etat
C================	
C State equation
C===============
C
      CALL ETAT
C
     .  (NFLUIDMAX, NPART, NPMAX, KFLUID, P,
     .   RHO      , RHO0 , VITC0           )
C
C Particules entrantes
C=====================
C Ingoing particles
C==================
C
      IF ((KMOT.EQ.1).AND.(.NOT.KPER)) THEN
C
        CALL ENTPAR
C
     .   (NFLUIDMAX, NPARF, NPART, NPMAX, IT     , KENT ,
     .    KFLUID   , KLIST, KPAR , KTURB, KPARMOB, CK1  , 
     .    CMU      , DR   , EPS  , LM   , MASS   , NUT  , 
     .    P        , RHO  , RHO0 , S    , TKE    , VITC0,
     .    VX       , VZ   , X    , Z                    )
C
      ENDIF
C
C Particules sortantes
C=====================
C Outgoing particles
C===================
C
      CALL SORPAR
C
     .   (NPARF, NPART, NPMAX  , IT  , KENT, KFLUID,  
     .    KLIST, KPAR , KPARMOB, KPER, EPS , LM    ,
     .    MASS , NUT  , P      , RHO , S   , TKE   ,
     .    VX   , VZ   , X      , XMIN, XMAX, Z     , 
     .    ZMIN , ZMAX                              )
C
C Calcul d une nouvelle variable de sortie
C=========================================
C Computation of a new printout variable
C======================================= 
C
      IF (KSORTR) THEN
C
        CALL PRIVEE 
C
     .  (NPMAX, NPART, EPS, MASS, NUT, P ,
     .   PRIV , RHO  , S  , TKE , VX , VZ,
     .   X    , Z                        )
C
      ENDIF
C
C Ecritures
C==========
C Printouts
C==========
C
      IF (KSORTR.AND.KVISU.EQ.1) THEN
C
        CALL INTERPOL
C
     .  (NPARTQ, NPARTQMAX, NPMAX , NPOIN, NQUADX, NQUADXMAX, 
     .   NQUADZ, NQUADZMAX, IPARTQ, KPER , EPS   , H        , 
     .   MASS  , NUT      , P     , PI   , PRIV  , RHO      , 
     .   S     , SUPP     , TEMPS , TKE  , VX    , VZ       , 
     .   X     , XINT     , XMIN  , XMAX , Z     , ZINT     , 
     .   ZMIN                                               )     
C
      ENDIF
C
      IF (KSORTR.OR.KSORTP) THEN
C
        CALL ECRIT
C
     .  (NBMAX , NPARB, NPARF  , NPART , NPMAX, NT    ,
     .   CT    , IT   , KENT   , KFLUID, KPAR , KSORTP,
     .   KSORTR, KVISU, KPARMOB, EPS   , MASS , NUT   , 
     .   P     , PRIV , RHO    , S     , TEMPS, THETA , 
     .   TKE   , VX   , VZ     , X     , Z            )
C
      ENDIF
C
C============================
C Fin de la boucle en temps *
C============================
C Time loop end * 
C================

 301  CONTINUE
C
      PRINT*,''
      IF (LNG.EQ.1) THEN
        PRINT*,'FIN NORMALE DU PROGRAMME.'
      ELSEIF (LNG.EQ.2) THEN
        PRINT*,'PROGRAM NORMALLY STOPPED'
      ENDIF
      PRINT*,''
C
C Fermetures
C===========
C Closing
C========
C
      CLOSE (47)
      CLOSE (48)
      CLOSE (49)
C
      STOP
      END
