!                        *********************
                          SUBROUTINE LECPARAM
!                        *********************
!
     &(NFLUIDMAX, NLIST    , NSORTP , NSORTR , NT       , NFLUID   ,
     & KCPAR    , KDEF     , KFPAR  , KGAMMAZ, KGAMMAX  , KGRAV    ,
     & KLISS    , KKERNEL  , KMOT   , KPARM  , KPER     , KPRES    ,
     & KPROD    , KSUITE   , KTURB  , KUSTAR , KVISQ    , KVISU    ,
     & CPAR     , DEBREF   , DELTADR, DESTDR , FORCST   , FACTLISS ,
     & GAMMAX   , GAMMAZ   , HDR    , NU0    , RHO0     , RUG      ,
     & VITC0    , XMIN     , XMAX   , ZMIN   , ZMAX     , FICHINIT ,
     & FICHPARAM, FICHSUITE, FICHRES, FICHPOS, FICHMAILL, NOMETUDE )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! CPAR      ! <--  ! WALL FORCE 1 COEFFICIENT                       !
! ! DEBREF    ! <--  ! PRESCRIBED MEAN BULK VELOCITY                  !
! ! DELTADR   ! <--  ! RATIO DELTA/DR                                 !
! ! DESTDR    ! <--  ! RATIO DEST/DR                                  !
! ! FACTLISS  ! <--  ! DENSITY SMOOTHING COEFFICIENT                  !
! ! FICHINIT  ! <--  ! INITIALIZING FILE                              !
! ! FICHPARAM ! -->  ! PARAMETER FILE                                 !
! ! FICHMAILL ! -->  ! MESH FILE FOR RUBENS                           !
! ! FICHPOS   ! -->  ! POSITION FILE                                  !
! ! FICHRES   ! -->  ! FIELD FILE                                     !
! ! FICHSUITE ! -->  ! DATA FILE FOR COMPUTATION CONTINUED            !
! ! FORCST    ! <--  ! PRESCRIBED AXIAL DRIVING FORCE                 !
! ! GAMMAX,                                                           !
! ! GAMMAZ    ! <--  ! DAMPING COEFFICIENTS                           !
! ! HDR       ! <--  ! RATIO H/DR                                     !
! ! KCPAR     ! <--  ! CHOICE INDEX FOR WALL MODELLING                !
! ! KDEF      ! <--  ! CHOICE INDEX FOR STRAIN MODEL                  !
! ! KFPAR     ! <--  ! CHOICE INDEX FOR WALL FORCES                   !
! ! KGAMMAX,                                                          !
! ! KGAMMAZ   ! <--  ! LOGICAL INDEX FOR DAMPING                      !
! ! KGRAV     ! <--  ! LOGICAL INDEX FOR THE GRAVITY                  !
! ! KKERNEL   ! <--  ! CHOICE INDEX FOR KERNEL                        !
! ! LNG       ! <--  ! CHOICE INDEX FOR LANGUAGE                      !
! ! KMOT      ! <--  ! CHOICE INDEX FOR THE FORCING TERM              !
! ! KLISS     ! <--  ! CHOICE INDEX FOR THE DENSITY SMOOTHING         !
! ! KPER      ! <--  ! LOGICAL INDEX FOR PERIODICITY                  !
! ! KPARM     ! <--  ! LOGICAL INDEX FOR MOVING WALL                  !
! ! KPRES     ! <--  ! CHOICE INDEX FOR THE PRESSURE GRADIENT MODEL   !
! ! KPROD     ! <--  ! CHOICE INDEX FOR PRODUCTION MODEL              !
! ! KSUITE    ! <--  ! LOGICAL FOR CALCULATION CONTINUED              !
! ! KTURB     ! <--  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! KUSTAR    ! <--  ! CHOICE INDEX FOR FRICTION VELOCITY COMPUTATION !
! ! KVISQ     ! <--  ! CHOICE INDEX FOR THE VISCOUS MODEL             !
! ! KVISU     ! <--  ! CHOICE INDEX FOR THE POSTPROCESSOR             !
! ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NLIST     ! <--  ! LISTING PRINTOUT PERIOD                        !
! ! NPARF     ! <--  ! NUMBER OF FLUID PARTICLES                      !
! ! NPART     ! <--  ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NSORTP    ! <--  ! POSITION PRINTOUT PERIOD                       !
! ! NSORTR    ! <--  ! FIELD PRINTOUT PERIOD                          !
! ! NT        ! <--  ! NUMBER OF TIME STEPS                           !
! ! NU0       ! <--  ! MOLECULAR VISCOSITIES                          !
! ! RHO0      ! <--  ! REFERENCE DENSITIES                            !
! ! RUG       ! <--  ! WALL ROUGHNESS                                 !
! ! VITC0     ! <--  ! SPEED OF SOUND                                 !
! ! XMIN, XMAX! <--  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
! ! ZMIN, ZMAX! <--  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
! !___________!______!________________________________________________!
!
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!----------------------------------------------------------------------
!
! SPARTACUS2D V5P9
! D. Violeau           & R. Issa
! +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28
! LNHE - 2008
!
! FONCTION : lit les parametres de calcul
! FUNCTION : reads the parameter calculation
!
! PROGRAMMES APPELANT : SPARTACUS2D
! CALLED BY
!
! PROGRAMMES APPELES  : LECDON_SPARTACUS2D
! CALLED PROGRAMS
!
!----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER NT        , NLIST   , NSORTR
      INTEGER NFLUIDMAX , NSORTP  , NFLUID
      INTEGER KFPAR     , KKERNEL , KVISQ
      INTEGER KCPAR     , KVISU   , KMOT
      INTEGER KPRES     , KPROD   , KDEF
      INTEGER KTURB     , KUSTAR  , KLISS
      INTEGER LNG       , LU
!
      LOGICAL KGRAV  , KSUITE, KGAMMAX
      LOGICAL KGAMMAZ, KPER  , KPARM
!
      DOUBLE PRECISION XMIN , XMAX    , ZMIN  , ZMAX
      DOUBLE PRECISION VITC0, CPAR    , GAMMAX, GAMMAZ
      DOUBLE PRECISION HDR  , FORCST  , DEBREF, DELTADR
      DOUBLE PRECISION RUG  , FACTLISS, DESTDR
!
      DOUBLE PRECISION RHO0 (NFLUIDMAX)
      DOUBLE PRECISION NU0  (NFLUIDMAX)
!
      CHARACTER*80  FICHINIT
      CHARACTER*40  FICHPARAM
      CHARACTER*40  NOMETUDE
      CHARACTER*80  FICHSUITE
      CHARACTER*40  FICHRES , FICHPOS
      CHARACTER*80  FICHMAILL
!
      COMMON/INFO/LNG,LU
!
! Lecture des parametres par Damocles
!====================================
! Parameter reading by Damocles
!==============================
!
       CALL LECDON_SPARTACUS2D
!
     &   (NFLUIDMAX, NLIST    , NSORTP   , NSORTR   , NT     ,
     &    NFLUID   , KCPAR    , KDEF     , KFPAR    , KGAMMAZ,
     &    KGAMMAX  , KGRAV    , KLISS    , KKERNEL  , KMOT   ,
     &    KPER     , KPARM    , KPRES    , KPROD    , KSUITE ,
     &    KTURB    , KUSTAR   , KVISQ    , KVISU    , CPAR   ,
     &    DEBREF   , DELTADR  , DESTDR   , FACTLISS , FORCST ,
     &    GAMMAX   , GAMMAZ   , HDR      , NU0      , RHO0   ,
     &    RUG      , VITC0    , XMIN     , XMAX     , ZMIN   ,
     &    ZMAX     , FICHINIT , FICHPARAM, FICHSUITE, FICHRES,
     &    FICHPOS  , FICHMAILL, NOMETUDE                     )
!
      RETURN
      END