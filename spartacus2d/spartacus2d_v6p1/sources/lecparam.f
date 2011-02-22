C                        *********************
                          SUBROUTINE LECPARAM
C                        *********************
C
     .(NFLUIDMAX, NLIST    , NSORTP , NSORTR , NT       , NFLUID   , 
     . KCPAR    , KDEF     , KFPAR  , KGAMMAZ, KGAMMAX  , KGRAV    , 
     . KLISS    , KKERNEL  , KMOT   , KPARM  , KPER     , KPRES    , 
     . KPROD    , KSUITE   , KTURB  , KUSTAR , KVISQ    , KVISU    ,
     . CPAR     , DEBREF   , DELTADR, DESTDR , FORCST   , FACTLISS , 
     . GAMMAX   , GAMMAZ   , HDR    , NU0    , RHO0     , RUG      , 
     . VITC0    , XMIN     , XMAX   , ZMIN   , ZMAX     , FICHINIT , 
     . FICHPARAM, FICHSUITE, FICHRES, FICHPOS, FICHMAILL, NOMETUDE )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! CPAR      ! <--  ! WALL FORCE 1 COEFFICIENT                       !
C ! DEBREF    ! <--  ! PRESCRIBED MEAN BULK VELOCITY                  !
C ! DELTADR   ! <--  ! RATIO DELTA/DR                                 !
C ! DESTDR    ! <--  ! RATIO DEST/DR                                  !
C ! FACTLISS  ! <--  ! DENSITY SMOOTHING COEFFICIENT                  !
C ! FICHINIT  ! <--  ! INITIALIZING FILE                              !
C ! FICHPARAM ! -->  ! PARAMETER FILE                                 !
C ! FICHMAILL ! -->  ! MESH FILE FOR RUBENS                           !
C ! FICHPOS   ! -->  ! POSITION FILE                                  !
C ! FICHRES   ! -->  ! FIELD FILE                                     !
C ! FICHSUITE ! -->  ! DATA FILE FOR COMPUTATION CONTINUED            !
C ! FORCST    ! <--  ! PRESCRIBED AXIAL DRIVING FORCE                 !
C ! GAMMAX,                                                           !
C ! GAMMAZ    ! <--  ! DAMPING COEFFICIENTS                           !
C ! HDR       ! <--  ! RATIO H/DR                                     !
C ! KCPAR     ! <--  ! CHOICE INDEX FOR WALL MODELLING                !
C ! KDEF      ! <--  ! CHOICE INDEX FOR STRAIN MODEL                  !
C ! KFPAR     ! <--  ! CHOICE INDEX FOR WALL FORCES                   !
C ! KGAMMAX,                                                          !
C ! KGAMMAZ   ! <--  ! LOGICAL INDEX FOR DAMPING                      !
C ! KGRAV     ! <--  ! LOGICAL INDEX FOR THE GRAVITY                  !
C ! KKERNEL   ! <--  ! CHOICE INDEX FOR KERNEL                        !
C ! LNG       ! <--  ! CHOICE INDEX FOR LANGUAGE                      !
C ! KMOT      ! <--  ! CHOICE INDEX FOR THE FORCING TERM              !
C ! KLISS     ! <--  ! CHOICE INDEX FOR THE DENSITY SMOOTHING         !
C ! KPER      ! <--  ! LOGICAL INDEX FOR PERIODICITY                  !
C ! KPARM     ! <--  ! LOGICAL INDEX FOR MOVING WALL                  !
C ! KPRES     ! <--  ! CHOICE INDEX FOR THE PRESSURE GRADIENT MODEL   !
C ! KPROD     ! <--  ! CHOICE INDEX FOR PRODUCTION MODEL              !
C ! KSUITE    ! <--  ! LOGICAL FOR CALCULATION CONTINUED              !
C ! KTURB     ! <--  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! KUSTAR    ! <--  ! CHOICE INDEX FOR FRICTION VELOCITY COMPUTATION !
C ! KVISQ     ! <--  ! CHOICE INDEX FOR THE VISCOUS MODEL             !
C ! KVISU     ! <--  ! CHOICE INDEX FOR THE POSTPROCESSOR             !
C ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NLIST     ! <--  ! LISTING PRINTOUT PERIOD                        !
C ! NPARF     ! <--  ! NUMBER OF FLUID PARTICLES                      !
C ! NPART     ! <--  ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NSORTP    ! <--  ! POSITION PRINTOUT PERIOD                       !
C ! NSORTR    ! <--  ! FIELD PRINTOUT PERIOD                          !
C ! NT        ! <--  ! NUMBER OF TIME STEPS                           !
C ! NU0       ! <--  ! MOLECULAR VISCOSITIES                          !
C ! RHO0      ! <--  ! REFERENCE DENSITIES                            !
C ! RUG       ! <--  ! WALL ROUGHNESS                                 !
C ! VITC0     ! <--  ! SPEED OF SOUND                                 !
C ! XMIN, XMAX! <--  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
C ! ZMIN, ZMAX! <--  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
C !___________!______!________________________________________________!
C
C MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
C----------------------------------------------------------------------
C
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C
C FONCTION : lit les parametres de calcul
C FUNCTION : reads the parameter calculation
C
C PROGRAMMES APPELANT : SPARTACUS2D
C CALLED BY           
C
C PROGRAMMES APPELES  : LECDON_SPARTACUS2D
C CALLED PROGRAMS     
C
C----------------------------------------------------------------------
C
C Variables
C==========
C
      IMPLICIT NONE
C
      INTEGER NT        , NLIST   , NSORTR
      INTEGER NFLUIDMAX , NSORTP  , NFLUID
      INTEGER KFPAR     , KKERNEL , KVISQ
      INTEGER KCPAR     , KVISU   , KMOT  
      INTEGER KPRES     , KPROD   , KDEF   
      INTEGER KTURB     , KUSTAR  , KLISS 
      INTEGER LNG       , LU
C
      LOGICAL KGRAV  , KSUITE, KGAMMAX
      LOGICAL KGAMMAZ, KPER  , KPARM
C
      DOUBLE PRECISION XMIN , XMAX    , ZMIN  , ZMAX 
      DOUBLE PRECISION VITC0, CPAR    , GAMMAX, GAMMAZ 
      DOUBLE PRECISION HDR  , FORCST  , DEBREF, DELTADR
      DOUBLE PRECISION RUG  , FACTLISS, DESTDR
C
      DOUBLE PRECISION RHO0 (NFLUIDMAX)
      DOUBLE PRECISION NU0  (NFLUIDMAX)
C
      CHARACTER*80  FICHINIT
      CHARACTER*40  FICHPARAM
      CHARACTER*40  NOMETUDE
      CHARACTER*80  FICHSUITE
      CHARACTER*40  FICHRES , FICHPOS
      CHARACTER*80  FICHMAILL
C
      COMMON/INFO/LNG,LU
C
C Lecture des parametres par Damocles 
C====================================
C Parameter reading by Damocles
C==============================
C
       CALL LECDON_SPARTACUS2D
C       
     .   (NFLUIDMAX, NLIST    , NSORTP   , NSORTR   , NT     , 
     .    NFLUID   , KCPAR    , KDEF     , KFPAR    , KGAMMAZ, 
     .    KGAMMAX  , KGRAV    , KLISS    , KKERNEL  , KMOT   , 
     .    KPER     , KPARM    , KPRES    , KPROD    , KSUITE ,
     .    KTURB    , KUSTAR   , KVISQ    , KVISU    , CPAR   , 
     .    DEBREF   , DELTADR  , DESTDR   , FACTLISS , FORCST , 
     .    GAMMAX   , GAMMAZ   , HDR      , NU0      , RHO0   , 
     .    RUG      , VITC0    , XMIN     , XMAX     , ZMIN   , 
     .    ZMAX     , FICHINIT , FICHPARAM, FICHSUITE, FICHRES,
     .    FICHPOS  , FICHMAILL, NOMETUDE                     )
C
      RETURN
      END
