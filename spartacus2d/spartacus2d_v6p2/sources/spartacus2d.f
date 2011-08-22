!**************************************************************************
!                                                                         *
!  SSSSS  PPPPPP   AAAAA  RRRRRR  TTTTTTT  AAAAA   CCCCC  U     U  SSSSS  *
! S     S P     P A     A R     R    T    A     A C     C U     U S     S *
! S       P     P A     A R     R    T    A     A C       U     U S       *
!  SSSSS  PPPPPP  AAAAAAA RRRRRR     T    AAAAAAA C       U     U  SSSSS  *
!       S P       A     A R     R    T    A     A C       U     U       S *
! S     S P       A     A R     R    T    A     A C     C U     U S     S *
!  SSSSS  P       A     A R     R    T    A     A  CCCCC   UUUUU   SSSSS  *
!                                                                         *
!             22222  DDDDDD            V     V 555555 PPPPPP  9999999     *
!            2     2 D     D           V     V 5      P     P 9     9     *
!                  2 D     D           V     V 5      P     P 9     9     *
!             22222  D     D           V     V 555555 PPPPPP  9999999     *
!            2       D     D            V   V       5 P             9     *
!            2       D     D             V V        5 P             9     *
!            2222222 DDDDDD               V    555555 P       9999999     *
!                                                                         *
!**************************************************************************
!
!                        *********************
                          PROGRAM SPARTACUS2D
!                        *********************
!
!-----------------------------------------------------------------------
!
! SPARTACUS2D V5P9
! D. Violeau           & R. Issa
! +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28
! LNHE - 2009
!
! FONCTION : programme principal
! FUNCTION : main program
!
! PROGRAMMES APPELANT : -
! CALLED BY
!
! PROGRAMMES APPELES  : LECPARAM  , INITIAL  , OUVERTURE, INTERPOL ,
! CALLED PROGRAMS       ECRIT     , TABLIEN  , KERNEL   , GRADVIT  ,
!                       FROTTEMENT, IMPULSION, PASTEMPS , MOUVEMENT,
!                       CONTINUITE, ETAT     , ENTPAR   , SORPAR   ,
!                       PRIVEE
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    !UNITY !               MEANING                          !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! AX, AZ    ! MS-2 ! PARTICLE ACCELERATION COMPONENTS               !
! ! CK1, CMU  !  -   ! K-EPSILON MODEL COEFFICIENTS                   !
! ! CPAR      !M2S-2 ! WALL FORCE 1 COEFFICIENT                       !
! ! CR        !  -   ! COEFFICIENT RELATIVE TO A ROUGH LOG LAW        !
! ! CT        !  -   ! TECPLOT CURRENT ZONE NUMBER                    !
! ! CTHETA,                                                           !
! ! STHETA    !  -   ! COMPONENTS OF WALL NORMAL VECTORS              !
! ! DEB       !MS-1  ! COMPUTED MEAN BULK VELOCITY                    !
! ! DEBREF    !MS-1  ! PRESCRIBED MEAN BULK VELOCITY                  !
! ! DELTA     !  M   ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
! ! DELTADR   !  -   ! RATIO DELTA/DR                                 !
! ! DESTDR    !  -   ! RATIO DEST/DR                                  !
! ! DR        !  M   ! INITIAL INTERPARTICLE SPACING                  !
! ! DT        !  S   ! TIME STEP                                      !
! ! EPS       !M2S-3 ! DISSIPATION RATE                               !
! ! ETA2      !  M2  ! CORRECTION TERM FOR DIFFUSION                  !
! ! FACTLISS  ! -    ! DENSITY SMOOTHING COEFFICIENT                  !
! ! FEXTX,                                                            !
! ! FEXTZ     ! MS-2 ! EXTERNAL FORCE COMPONENTS                      !
! ! FICHINIT  !  -   ! INITIALIZING FILE                              !
! ! FICHMAILL !  -   ! MESH FILE FOR RUBENS                           !
! ! FICHPARAM !  -   ! PARAMETER FILE                                 !
! ! FICHPOS   !  -   ! POSITION FILE                                  !
! ! FICHRES   !  -   ! FIELD FILE                                     !
! ! FICHSUITE !  -   ! DATA FILE FOR COMPUTATION CONTINUED            !
! ! FMOT      ! MS-2 ! AXIAL DRIVING FORCE                            !
! ! FORCST    ! MS-2 ! PRESCRIBED AXIAL DRIVING FORCE                 !
! ! FPARX,                                                            !
! ! FPARZ     ! MS-2 ! WALL FORCE COMPONENTS                          !
! ! FPRES     !M5S-2 ! MOMENTUM FLUX RELATIVE TO PRESSURE FORCES      !
! !             KG-1                                                  !
! ! FVISQ     !M5S-2 ! MOMENTUM FLUX RELATIVE TO VISCOUS FORCES       !
! !             KG-1                                                  !
! ! GAMMAX,                                                           !
! ! GAMMAZ    ! S-1  ! DAMPING COEFFICIENTS                           !
! ! GKERX,                                                            !
! ! GKERZ     ! M-3  ! KERNEL DERIVATIVE COMPONENTS                   !
! ! GRAV      ! MS-2 ! GRAVITY                                        !
! ! GVXX, GVXZ                                                        !
! ! GVZX, GVZZ! S-1  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
! ! H         !  M   ! SMOOTHING LENGTH                               !
! ! HDR       !  -   ! RATIO H/DR                                     !
! ! I,J,L     !  -   ! PARTICLE INDEXS                                !
! ! ILIEN     !  -   ! PARTICLE LINK LIST                             !
! ! IPARTQ    !  -   ! INDEX OF PARTICLES LOCATED IN A SQUARE         !
! ! IT        !  -   ! INDEX OF THE CURRENT TIME STEP                 !
! ! KAPPA     !  -   ! VON KARMAN CONSTANT                            !
! ! KCPAR     !  -   ! CHOICE INDEX FOR WALL MODELLING                !
! ! KDEF      !  -   ! CHOICE INDEX FOR STRAIN MODEL                  !
! ! KENT      !  -   ! CONDITION TYPE AT FLUID BOUNDARIES             !
! ! KFLUID    !  -   ! FLUID TYPE                                     !
! ! KFPAR     !  -   ! CHOICE INDEX FOR WALL FORCES                   !
! ! KGAMMAX,                                                          !
! ! KGAMMAZ   !  -   ! LOGICAL INDEX FOR DAMPING                      !
! ! KGRAV     !  -   ! LOGICAL INDEX FOR THE GRAVITY                  !
! ! KKERNEL   !  -   ! CHOICE INDEX FOR KERNEL                        !
! ! KLIST     !  -   ! LOGICAL FOR LISTING PRINTOUT                   !
! ! KLISS     ! -    ! CHOICE INDEX FOR THE DENSITY SMOOTHING         !
! ! KMOT      !  -   ! CHOICE INDEX FOR THE FORCING TERM              !
! ! KPAR      !  -   ! PARTICLE  TYPE                                 !
! ! KPARM     !  -   ! LOGICAL INDEX FOR MOVING WALL                  !
! ! KPARMOB   !  -   ! MOVING WALL OR EDGE PARTICLE TYPE              !
! ! KPER      !  -   ! LOGICAL INDEX FOR PERIODICITY                  !
! ! KPRES     !  -   ! CHOICE INDEX FOR THE PRESSURE GRADIENT MODEL   !
! ! KPROD     !  -   ! CHOICE INDEX FOR PRODUCTION MODEL              !
! ! KSUITE    !  -   ! LOGICAL FOR CALCULATION CONTINUED              !
! ! KSORTP    !  -   ! LOGICAL FOR POSITION PRINTOUT                  !
! ! KSORTR    !  -   ! LOGICAL FOR FIELD PRINTOUT                     !
! ! KTURB     !  -   ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! KUSTAR    !  -   ! CHOICE INDEX FOR FRICTION VELOCITY COMPUTATION !
! ! KVISQ     !  -   ! CHOICE INDEX FOR THE VISCOUS MODEL             !
! ! KVISU     !  -   ! CHOICE INDEX FOR THE POSTPROCESSOR             !
! ! LM        !  M   ! MIXING LENGTH                                  !
! ! LNG       !  -   ! CHOICE INDEX FOR LANGUAGE                      !
! ! MASS      !  KG  ! PARTICLE MASS                                  !
! ! NBMAX     !  -   ! MAXIMUM NUMBER OF EDGE PARTICLES               !
! ! NELEM     !  -   ! NUMBER OF MESH ELEMENTS FOR RUBENS             !
! ! NFLUID    !  -   ! FLUID NUMBER                                   !
! ! NFLUIDMAX !  -   ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NLIEN     !  -   ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  !  -   ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NLIST     !  -   ! LISTING PRINTOUT PERIOD                        !
! ! NOMETUDE  !  -   ! NAME OF THE INVESTIGATED CASE                  !
! ! NPARB     !  -   ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     !  -   ! NUMBER OF FLUID PARTICLES                      !
! ! NPART     !  -   ! TOTAL PARTICLE NUMBER                          !
! ! NPARTQ    !  -   ! NUMBER OF PARTICLES IN A SQUARE                !
! ! NPARTQMAX !  -   ! MAXIMUM NUMBER OF PARTICLES IN A SQUARE        !
! ! NPMAX     !  -   ! MAXIMUM PARTICLE NUMBER                        !
! ! NPOIN     !  -   ! NUMBER OF MESH POINTS FOR RUBENS               !
! ! NQUADX,                                                           !
! ! NQUADZ    !  -   ! NUMBER OF SQUARES ALONG EACH DIRECTION         !
! ! NQUADXMAX,                                                        !
! ! NQUADZMAX !  -   ! MAXIMUM NUMBER OF SQUARES ALONG EACH DIRECTION !
! ! NSORTP    !  -   ! POSITION PRINTOUT PERIOD                       !
! ! NSORTR    !  -   ! FIELD PRINTOUT PERIOD                          !
! ! NT        !  -   ! NUMBER OF TIME STEPS                           !
! ! NU0       !M2S-1 ! MOLECULAR VISCOSITIES                          !
! ! NUT       !M2S-1 ! EDDY VISCOSITY                                 !
! ! NVARSOR   !  -   ! PRINTOUT VARIABLE NUMBER                       !
! ! NVARSORMAX!  -   ! MAXIMUM NUMBER OF PRINTOUT VARIABLES           !
! ! P         !KGM-1 ! PRESSURE                                       !
! !            S-2                                                    !
! ! PI        !  -   ! ARCHIMEDE'S NUMBER                             !
! ! PRIV      !  -   ! PRIVATE PRINTOUT VARIABLE FOR THE USER         !
! ! PROD      !M2S-3 ! PRODUCTION RATE                                !
! ! R0        !  M   ! WALL ACTION DISTANCE                           !
! ! RAB       !  M   ! INTERPARTICLE DISTANCE                         !
! ! RHO       !KGM-2 ! DENSITY                                        !
! ! RHO0      !KGM-2 ! REFERENCE DENSITIES                            !
! ! RUG       !  M   ! WALL ROUGHNESS                                 !
! ! S         ! S-1  ! RATE OF STRAIN                                 !
! ! SUPP      !  -   ! KERNEL SUPPORT                                 !
! ! TEMPS     !  S   ! PHYSICAL TIME                                  !
! ! THETA     !  o   ! ANGLE BETWEEN WALL NORMAL VECTOR AND X-AXIS    !
! ! TKE       !M2S-2 ! TURBULENT KINETIC ENERGY                       !
! ! USTAR     ! MS-1 ! FRICTION VELOCITY                              !
! ! VITC0     ! MS-1 ! SPEED OF SOUND                                 !
! ! VX, VZ    ! MS-1 ! VELOCITY COMPONENTS                            !
! ! VXAB, VZAB! MS-1 ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
! ! X, Z      !  M   ! PARTICLE POSITION                              !
! ! XAB, ZAB  !  M   ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
! ! XINT, ZINT!  M   ! COORDINATES OF MESH POINTS                     !
! ! XMIN, XMAX!  M   ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
! ! ZMIN, ZMAX!  M   ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
! !___________!______!________________________________________________!
!
! Variables
!==========
!
      IMPLICIT NONE
!
! Variables primaires
!--------------------
! Primary variables
!------------------
!
      INTEGER NPMAX   , NLIENMAX , NVARSORMAX , NFLUIDMAX , NQUADZMAX
      INTEGER NPARF   , NPART    , NT         , NLIST     , NSORTR
      INTEGER NVARSOR , NSORTP   , NPARTQMAX  , NQUADXMAX , NQUADX
      INTEGER NQUADZ  , NPOIN    , NELEM      , NBMAX     , NPARB
      INTEGER KVISU   , KTURB    , KCPAR      , KVISQ     , KKERNEL
      INTEGER KFPAR   , KPRES    , KMOT       , KPROD     , KDEF
      INTEGER KUSTAR  , IT       , CT         , LNG       , LU
      INTEGER NFLUID
!
      COMMON/INFO/LNG,LU
!
      LOGICAL KSORTR , KSORTP , KLIST , KSUITE
      LOGICAL KGRAV  , KPER   , KPARM , KLISS
      LOGICAL KGAMMAX, KGAMMAZ
!
      DOUBLE PRECISION VITC0  , DEBREF  , CMU
      DOUBLE PRECISION XMIN   , XMAX    , GAMMAX , CK1
      DOUBLE PRECISION ZMIN   , ZMAX    , GAMMAZ , KAPPA
      DOUBLE PRECISION ETA2   , GRAV    , CPAR   , CR
      DOUBLE PRECISION R0     , TEMPS   , DT     , PI
      DOUBLE PRECISION H      , DR      , HDR    , FACTLISS
      DOUBLE PRECISION FORCST , DEB     , SUPP   , DESTDR
      DOUBLE PRECISION DELTA  , DELTADR , RUG
!
      CHARACTER*40 NOMETUDE  , FICHPARAM
      CHARACTER*80 FICHINIT  , FICHSUITE
      CHARACTER*40 FICHRES   , FICHPOS
      CHARACTER*80 FICHMAILL
!
! Parametres modifiables
!-----------------------
! Adjustable parameters
!----------------------
!
      PARAMETER (NPMAX     = 35000 ,
     &           NBMAX     = 5000  ,
     &           NLIENMAX  = 100   ,
     &           NPARTQMAX = 100   ,
     &           NQUADXMAX = 1040  ,
     &           NQUADZMAX = 160   ,
     &           NPOIN     = 40501 ,
!     .           NPOIN     = 40401 ,
     &           NELEM     = 80000 )
!
! Parametres a ne pas modifier
!-----------------------------
! Non adjustable parameters
!--------------------------
!
      PARAMETER (NVARSORMAX = 16     ,
     &           NFLUIDMAX  = 3      )
!
! Tableaux
!---------
! Arrays
!-------
!
      INTEGER ILIEN   (NPMAX  , NLIENMAX)
      INTEGER NLIEN   (NPMAX) , KFLUID(NPMAX)
      INTEGER KENT    (NPMAX) , KPAR  (NPMAX)
      INTEGER KPARMOB (NPMAX)
!
      INTEGER IPARTQ(NQUADXMAX,NQUADZMAX,NPARTQMAX)
      INTEGER NPARTQ(NQUADXMAX,NQUADZMAX)
!
      REAL XINT(NPOIN) , ZINT(NPOIN)
!
      DOUBLE PRECISION X     (NPMAX) , Z     (NPMAX) , RHO  (NPMAX)
      DOUBLE PRECISION VX    (NPMAX) , VZ    (NPMAX) , P    (NPMAX)
      DOUBLE PRECISION AX    (NPMAX) , AZ    (NPMAX) , MASS (NPMAX)
      DOUBLE PRECISION GVXX  (NPMAX) , GVXZ  (NPMAX) , FMOT (NPMAX)
      DOUBLE PRECISION GVZX  (NPMAX) , GVZZ  (NPMAX) , USTAR(NBMAX)
      DOUBLE PRECISION NUT   (NPMAX) , TKE   (NPMAX) , EPS  (NPMAX)
      DOUBLE PRECISION S     (NPMAX) , LM    (NPMAX) , THETA(NBMAX)
      DOUBLE PRECISION CTHETA(NBMAX) , STHETA(NBMAX) , PRIV (NPMAX)
!
      DOUBLE PRECISION RHO0 (NFLUIDMAX)      ,NU0  (NFLUIDMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX) ,GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX) ,ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX) ,VZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX) ,KER4_VAL(NPMAX,0:NLIENMAX)
!
      PRINT*,''
      PRINT*,''
      PRINT*,''
      PRINT222,'================================================',
     &         '========================'
      PRINT222,'                            SPARTACUS-2D V5P9'
      PRINT222,'================================================',
     &         '========================'
!
 222  FORMAT (A48,A24)
!
! Lecture des parametres
!=======================
! Parameter reading
!==================
!
      CALL LECPARAM
!
     &  (NFLUIDMAX, NLIST    , NSORTP , NSORTR , NT       , NFLUID   ,
     &   KCPAR    , KDEF     , KFPAR  , KGAMMAZ, KGAMMAX  , KGRAV    ,
     &   KLISS    , KKERNEL  , KMOT   , KPARM  , KPER     , KPRES    ,
     &   KPROD    , KSUITE   , KTURB  , KUSTAR , KVISQ    , KVISU    ,
     &   CPAR     , DEBREF   , DELTADR, DESTDR , FORCST   , FACTLISS ,
     &   GAMMAX   , GAMMAZ   , HDR    , NU0    , RHO0     , RUG      ,
     &   VITC0    , XMIN     , XMAX   , ZMIN   , ZMAX     , FICHINIT ,
     &   FICHPARAM, FICHSUITE, FICHRES, FICHPOS, FICHMAILL, NOMETUDE )
!
! Initialisations
!================
! Initializations
!================
!
      CALL INITIAL
!
     &  (NBMAX, NFLUIDMAX, NPARB  , NPARF  , NPART, NPMAX ,
     &   CT   , KENT     , KFLUID , KKERNEL, KPAR , KSUITE,
     &   KTURB, CK1      , KPARMOB, CMU    , CR   , CTHETA,
     &   DEB  , DELTA    , DELTADR, DR     , DT   , EPS   ,
     &   ETA2 , FMOT     , GRAV   , H      , HDR  , KAPPA ,
     &   LM   , MASS     , NUT    , P      , PI   , PRIV  ,
     &   R0   , RHO      , RHO0   , STHETA , SUPP , TEMPS ,
     &   THETA, TKE      , VITC0  , VX     , VZ   , X     ,
     &   Z    , S                                         )
!
! Ouvertures fichiers sorties
!============================
! Printout file opening
!======================
!
      CALL OUVERTURE
!
     &  (NELEM, NPOIN, NVARSOR,NVARSORMAX, KVISU,
     &   XINT , XMIN , XMAX   ,ZINT      , ZMIN ,
     &   ZMAX , DR                              )
!
! Calcul d une nouvelle variable de sortie
!=========================================
! Computation of a new printout variable
!=======================================
!
      CALL PRIVEE
!
     &  (NPMAX, NPART, EPS, MASS, NUT, P ,
     &   PRIV , RHO  , S  , TKE , VX , VZ,
     &   X    , Z                        )
!
! Ecritures fichiers sorties
!===========================
! Printout file writing
!======================
!
      KSORTR=.TRUE.
      KSORTP=.TRUE.
!
      IF (KVISU.EQ.1) THEN
!
        CALL TABLIEN
!
     &   (NLIEN , NLIENMAX , NPART , NPARTQ   , NPARTQMAX, NPMAX ,
     &    NQUADX, NQUADXMAX, NQUADZ, NQUADZMAX, ILIEN    , IPARTQ,
     &    KLIST , KPER     , H     , RAB      , SUPP     , VX    ,
     &    VXAB  , VZ       , VZAB  , X        , XMIN     , XMAX  ,
     &    XAB   , Z        , ZMIN  , ZAB                         )
!
        CALL INTERPOL
!
     &  (NPARTQ, NPARTQMAX, NPMAX , NPOIN, NQUADX, NQUADXMAX,
     &   NQUADZ, NQUADZMAX, IPARTQ, KPER , EPS   , H        ,
     &   MASS  , NUT      , P     , PI   , PRIV  , RHO      ,
     &   S     , SUPP     , TEMPS , TKE  , VX    , VZ       ,
     &   X     , XINT     , XMIN  , XMAX , Z     , ZINT     ,
     &   ZMIN                                               )
!
      ENDIF
!
      CALL ECRIT
!
     &  (NBMAX , NPARB, NPARF  , NPART , NPMAX, NT    ,
     &   CT    , IT   , KENT   , KFLUID, KPAR , KSORTP,
     &   KSORTR, KVISU, KPARMOB, EPS   , MASS , NUT   ,
     &   P     , PRIV , RHO    , S     , TEMPS, THETA ,
     &   TKE   , VX   , VZ     , X     , Z            )
!
!===============================
! Debut de la boucle en temps  *
!===============================
! Time loop beginning *
!======================
!
      DO 301 IT = 1,NT
!
        KSORTR=.FALSE.
        KSORTP=.FALSE.
        KLIST =.FALSE.
        IF (MOD(IT,NSORTR).EQ.0)            KSORTR=.TRUE.
        IF (MOD(IT,NSORTP).EQ.0)            KSORTP=.TRUE.
        IF (MOD(IT,NLIST ).EQ.0.OR.IT.EQ.1) KLIST =.TRUE.
!
      IF (KLIST) THEN
        PRINT*
        PRINT*,'Iteration : ',IT
        PRINT*,'---------------------------------------------',
     &         '----------'
      ENDIF
!
! Construction du quadrillage et du tableau de liens
!===================================================
! Coarse grid and link list construction
!=======================================
!
      CALL TABLIEN
!
     &   (NLIEN , NLIENMAX , NPART , NPARTQ   , NPARTQMAX, NPMAX ,
     &    NQUADX, NQUADXMAX, NQUADZ, NQUADZMAX, ILIEN    , IPARTQ,
     &    KLIST , KPER     , H     , RAB      , SUPP     , VX    ,
     &    VXAB  , VZ       , VZAB  , X        , XMIN     , XMAX  ,
     &    XAB   , Z        , ZMIN  , ZAB                         )
!
! Calcul du gradient du noyau
!============================
! Kernel derivative computation
!==============================
!
      CALL KERNEL
!
     &  (NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, KKERNEL,
     &   GKERX, GKERZ   , H    , PI   , RAB  , XAB    ,
     &   ZAB                                          )
!
      IF (KTURB.NE.0) THEN
!
! Gradient de vitesse
!====================
! Velocity gradient
!==================
!
        CALL GRADVIT
!
     &   (NLIEN, NLIENMAX, NPARB, NPARF, NPART, NPMAX,
     &    ILIEN, KPAR    , GKERX, GKERZ, GVXX , GVXZ ,
     &    GVZX , GVZZ    , MASS , RHO  , VXAB , VZAB )
!
! Vitesse de frottement
!======================
! Friction velocity
!==================
!
        CALL FROTTEMENT
!
     &  (NBMAX , NPARB    , NPARF , NPARTQ   , NPARTQMAX, NPMAX,
     &   NQUADX, NQUADXMAX, NQUADZ, NQUADZMAX, IPARTQ   , CR   ,
     &   KPER  , KUSTAR   , CTHETA, DELTA    , DESTDR   , DR   ,
     &   GVXX  , GVXZ     , GVZX  , GVZZ     , H        , KAPPA,
     &   MASS  , PI       , RHO   , RUG      , STHETA   , SUPP ,
     &   USTAR , VX       , VZ    , X        , XMIN     , XMAX ,
     &   Z     , ZMIN                                          )
!
      ENDIF
!
! Equation de quantite de mouvement
!==================================
! Momentum equation
!==================
!
      CALL IMPULSION
!
     &  (NBMAX , NLIEN    , NLIENMAX, NPARB  , NPARF, NPMAX ,
     &   NPART , NFLUIDMAX, ILIEN   , KAPPA  , KCPAR, KDEF  ,
     &   KFPAR , KFLUID   , KGAMMAX , KGAMMAZ, KGRAV, KMOT  ,
     &   KPAR  , KPRES    , KPROD   , KTURB  , KVISQ, AX    ,
     &   AZ    , CK1      , CMU     , CPAR   , DEB  , DEBREF,
     &   DELTA , DR       , DT      , EPS    , ETA2 , FMOT  ,
     &   FORCST, GAMMAX   , GAMMAZ  , GKERX  , GKERZ, GRAV  ,
     &   GVXX  , GVXZ     , GVZX    , GVZZ   , H    , LM    ,
     &   MASS  , NU0      , NUT     , P      , R0   , RAB   ,
     &   RHO   , S        , TKE     , USTAR  , VITC0, VX    ,
     &   VXAB  , VZ       , VZAB    , X      , XAB  , XMIN  ,
     &   ZAB   , Z                                          )
!
! Pas de temps
!=============
! Time step
!==========
!
      CALL PASTEMPS
!
     &   (NPARF, NPMAX, NFLUIDMAX, KLIST, KFLUID, AX   ,
     &    AZ   , DT   , H        , NU0  , NUT   , TEMPS,
     &    VITC0                                        )
!
! Mouvement des particules
!=========================
! Particle displacement
!======================
!
      CALL MOUVEMENT
!
     &  (NBMAX , NPARB , NPARF, NPART  , NPMAX, CR  ,
     &   KPAR  , KPER  , KTURB, KPARMOB, KPARM, AX  ,
     &   AZ    , CTHETA, DELTA, DT     , KAPPA, RUG ,
     &   STHETA, USTAR , VX   , VZ     , X    , XMIN,
     &   XMAX  , Z     , TEMPS, PI                  )
!
! Equation de continuite
!=======================
! Continuity equation
!====================
!
      CALL CONTINUITE
!
     &  (ILIEN   , NFLUIDMAX, NLIEN , NLIENMAX, NPARB, NPARF   ,
     &   NPART   , NPMAX    , KFLUID, KLISS   , KGRAV, FACTLISS,
     &   KER4_VAL, DT       , GKERX , GKERZ   , H    , MASS    ,
     &   RAB     , RHO      , RHO0  , VX      , VZ             )
!
! Equation d'etat
!================
! State equation
!===============
!
      CALL ETAT
!
     &  (NFLUIDMAX, NPART, NPMAX, KFLUID, P,
     &   RHO      , RHO0 , VITC0           )
!
! Particules entrantes
!=====================
! Ingoing particles
!==================
!
      IF ((KMOT.EQ.1).AND.(.NOT.KPER)) THEN
!
        CALL ENTPAR
!
     &   (NFLUIDMAX, NPARF, NPART, NPMAX, IT     , KENT ,
     &    KFLUID   , KLIST, KPAR , KTURB, KPARMOB, CK1  ,
     &    CMU      , DR   , EPS  , LM   , MASS   , NUT  ,
     &    P        , RHO  , RHO0 , S    , TKE    , VITC0,
     &    VX       , VZ   , X    , Z                    )
!
      ENDIF
!
! Particules sortantes
!=====================
! Outgoing particles
!===================
!
      CALL SORPAR
!
     &   (NPARF, NPART, NPMAX  , IT  , KENT, KFLUID,
     &    KLIST, KPAR , KPARMOB, KPER, EPS , LM    ,
     &    MASS , NUT  , P      , RHO , S   , TKE   ,
     &    VX   , VZ   , X      , XMIN, XMAX, Z     ,
     &    ZMIN , ZMAX                              )
!
! Calcul d une nouvelle variable de sortie
!=========================================
! Computation of a new printout variable
!=======================================
!
      IF (KSORTR) THEN
!
        CALL PRIVEE
!
     &  (NPMAX, NPART, EPS, MASS, NUT, P ,
     &   PRIV , RHO  , S  , TKE , VX , VZ,
     &   X    , Z                        )
!
      ENDIF
!
! Ecritures
!==========
! Printouts
!==========
!
      IF (KSORTR.AND.KVISU.EQ.1) THEN
!
        CALL INTERPOL
!
     &  (NPARTQ, NPARTQMAX, NPMAX , NPOIN, NQUADX, NQUADXMAX,
     &   NQUADZ, NQUADZMAX, IPARTQ, KPER , EPS   , H        ,
     &   MASS  , NUT      , P     , PI   , PRIV  , RHO      ,
     &   S     , SUPP     , TEMPS , TKE  , VX    , VZ       ,
     &   X     , XINT     , XMIN  , XMAX , Z     , ZINT     ,
     &   ZMIN                                               )
!
      ENDIF
!
      IF (KSORTR.OR.KSORTP) THEN
!
        CALL ECRIT
!
     &  (NBMAX , NPARB, NPARF  , NPART , NPMAX, NT    ,
     &   CT    , IT   , KENT   , KFLUID, KPAR , KSORTP,
     &   KSORTR, KVISU, KPARMOB, EPS   , MASS , NUT   ,
     &   P     , PRIV , RHO    , S     , TEMPS, THETA ,
     &   TKE   , VX   , VZ     , X     , Z            )
!
      ENDIF
!
!============================
! Fin de la boucle en temps *
!============================
! Time loop end *
!================
!
 301  CONTINUE
!
      PRINT*,''
      IF (LNG.EQ.1) THEN
        PRINT*,'FIN NORMALE DU PROGRAMME.'
      ELSEIF (LNG.EQ.2) THEN
        PRINT*,'PROGRAM NORMALLY STOPPED'
      ENDIF
      PRINT*,''
!
! Fermetures
!===========
! Closing
!========
!
      CLOSE (47)
      CLOSE (48)
      CLOSE (49)
!
      STOP
      END