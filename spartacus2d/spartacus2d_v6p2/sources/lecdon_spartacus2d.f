!
!                      *******************************
                        SUBROUTINE LECDON_SPARTACUS2D
!                      *******************************
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
! ! KLISS     ! <--  ! CHOICE INDEX FOR THE DENSITY SMOOTHING         !
! ! KMOT      ! <--  ! CHOICE INDEX FOR THE FORCING TERM              !
! ! KPER      ! <--  ! LOGICAL INDEX FOR PERIODICITY                  !
! ! KPARM     ! <--  ! LOGICAL INDEX FOR MOVING WALL                  !
! ! KPRES     ! <--  ! CHOICE INDEX FOR THE PRESSURE GRADIENT MODEL   !
! ! KPROD     ! <--  ! CHOICE INDEX FOR PRODUCTION MODEL              !
! ! KSUITE    ! <--  ! LOGICAL FOR CALCULATION CONTINUED              !
! ! KTURB     ! <--  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! KUSTAR    ! <--  ! CHOICE INDEX FOR FRICTION VELOCITY COMPUTATION !
! ! KVISQ     ! <--  ! CHOICE INDEX FOR THE VISCOUS MODEL             !
! ! KVISU     ! <--  ! CHOICE INDEX FOR THE POSTPROCESSOR             !
! ! LNG       ! <--  ! CHOICE INDEX FOR LANGUAGE                      !
! ! MAXVAR    ! <--  ! MAXIMUM NUMBER OF PRINTOUT VARIABLES           !
! ! MOTINT    ! <--  ! INTEGER KEY-WORDS                              !
! ! MOTREA    ! <--  ! REAL KEY-WORDS                                 !
! ! MOTLOG    ! <--  ! LOGIC KEY-WORDS                                !
! ! MOTCAR    ! <--  ! CHARACTER KEY-WORDS                            !
! ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NLIST     ! <--  ! LISTING PRINTOUT PERIOD                        !
! ! NMAX      ! <--  ! MAXIMAL NUMBER OF KEY-WORDS ACCORDING TO TYPE  !
! ! NPARF     ! <--  ! NUMBER OF FLUID PARTICLES                      !
! ! NPART     ! <--  ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NSORTP    ! <--  ! POSITION PRINTOUT PERIOD                       !
! ! NSORTR    ! <--  ! FIELD PRINTOUT PERIOD                          !
! ! NT        ! <--  ! NUMBER OF TIME STEPS                           !
! ! NU0       ! <--  ! MOLECULAR VISCOSITIES                          !
! ! RHO0      ! <--  ! REFERENCE DENSITIES                            !
! ! RUG       ! <--  ! WALL ROUGHNESS                                 !
! ! SORLEO    ! <--  ! LOGICAL RELATIVE TO KEY WORDS                  !
! ! VITC0     ! <--  ! SPEED OF SOUND                                 !
! ! XMIN, XMAX! <--  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
! ! ZMIN, ZMAX! <--  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
! !___________!______!________________________________________________!
!
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!----------------------------------------------------------------------
!
! SPARTACUS2D V5P9
! D. Rouge
! D. Violeau           & R. Issa
! +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28
! LNHE - 2008
!
! FONCTION : lecture des parametres d entree par Damocles
! FUNCTION : Input parameter reading by Damocles
!
! PROGRAMMES APPELANT : LECPARAM
! CALLED BY
!
! PROGRAMMES APPELES  : READ_CONFIG, DAMOCLE
! CALLED PROGRAMS
!
!----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER KK
      INTEGER LNG   , LU
      INTEGER NCAR
      INTEGER K
!
!      CHARACTER*72 CHAINE
!
      COMMON/INFO/LNG,LU
!
! Declaration mots-cles entiers
!------------------------------
! Integer key word declaration
!-----------------------------
!
      INTEGER NT     , NLIST    , NSORTR
      INTEGER NFLUID
      INTEGER NSORTP , NFLUIDMAX
      INTEGER KFPAR  , KKERNEL  , KTURB
      INTEGER KDEF   , KPROD    , KUSTAR
      INTEGER KMOT   , KCPAR    , KPRES
      INTEGER KVISQ  , KVISU
!
! Declaration mots-cles reels
!----------------------------
! Real key word declaration
!--------------------------
!
      DOUBLE PRECISION CPAR  , DELTADR  , DEBREF
      DOUBLE PRECISION FORCST, GAMMAX   , GAMMAZ
      DOUBLE PRECISION HDR   , RUG      , VITC0
      DOUBLE PRECISION XMIN  , XMAX     , ZMIN
      DOUBLE PRECISION ZMAX  , FACTLISS , DESTDR
!
      DOUBLE PRECISION RHO0(NFLUIDMAX), NU0(NFLUIDMAX)
!
! Declaration mots-cles logiques
!-------------------------------
! Logical key word declaration
!-----------------------------
!
      LOGICAL KSUITE , KGRAV  , KPER
      LOGICAL KGAMMAX, KGAMMAZ, KPARM
      LOGICAL KLISS
!
! Declaration mots-cles chaines de caracteres
!--------------------------------------------
! Character string key word declaration
!--------------------------------------
!
      CHARACTER*40  NOMETUDE , FICHPARAM
      CHARACTER*80  FICHINIT , FICHSUITE
      CHARACTER*40  FICHRES  , FICHPOS
      CHARACTER*80  FICHMAILL
!
! Declaration des tableaux pour Damocles
!=======================================
! Array declaration for Damocles
!===============================
!
      INTEGER, PARAMETER :: NMAX = 300
!
      LOGICAL DOC
!
      LOGICAL MOTLOG (NMAX)
!
      INTEGER MOTINT (NMAX)
      INTEGER ADRESS (4,NMAX), DIMENS (4,NMAX)
      INTEGER TROUVE (4,NMAX)
!
      DOUBLE PRECISION MOTREA (NMAX)
!
      CHARACTER*250    CHAINE
      CHARACTER*144    MOTCAR (NMAX)
      CHARACTER*72     MOTCLE (4,NMAX,2)
!
      CHARACTER(LEN=144) FILE_DESC(4,300)
      CHARACTER(LEN=250) NOM_CAS, NOM_DIC
!
      NCAR = 0
      CHAINE=''
!
! Initialisation des tableaux pour Damocles
!==========================================
! Array initialization for Damocles
!==================================
!
      DO 10 KK=1,NMAX
!
        MOTCAR(KK)(1:1)=' '
!
        DIMENS(1,KK) = 0
        DIMENS(2,KK) = 0
        DIMENS(3,KK) = 0
        DIMENS(4,KK) = 0
!
10    CONTINUE
!
!     IMPRESSION DE LA DOC
!     nulle par defaut
!
      DOC = .FALSE.
!
! Langue et index des sorties
!============================
! Language and logical unit for outputs
!======================================
!
      CALL READ_CONFIG
!
     &   (LNG, LU, CHAINE, NCAR)
!
! Ouverture des fichiers dictionnaire et cas
!===========================================
! Dictionnary and case file opening
!==================================
!
        NOM_DIC='T2DDICO'
        NOM_CAS='T2CAS'
!
      OPEN(2,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      OPEN(3,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
      CALL DAMOCLE
!
     &   (ADRESS, DIMENS, NMAX   , DOC       , LNG    , LU    ,
     &    MOTINT, MOTREA, MOTLOG , MOTCAR    , MOTCLE , TROUVE,
     &    2     , 3     , .FALSE., FILE_DESC )
!
! Fermeture des fichiers dictionnaire et cas
!===========================================
! Dictionnary and case closing
!=============================
!
      CLOSE(2)
      CLOSE(3)
!
! Affectation des parametres sous leur nom en fortran
!====================================================
! Parameter assigning according to their fortran name
!====================================================
!
! Mots-cles entiers
!------------------
! Integer key words
!------------------
!
         NT         = MOTINT( ADRESS(1, 1) )
         NLIST      = MOTINT( ADRESS(1, 2) )
         NSORTR     = MOTINT( ADRESS(1, 3) )
         NFLUID     = MOTINT( ADRESS(1, 4) )
         KFPAR      = MOTINT( ADRESS(1, 5) )
         KKERNEL    = MOTINT( ADRESS(1, 6) )
         KTURB      = MOTINT( ADRESS(1, 7) )
         KDEF       = MOTINT( ADRESS(1, 8) )
         KPROD      = MOTINT( ADRESS(1, 9) )
         KUSTAR     = MOTINT( ADRESS(1, 10) )
         KCPAR      = MOTINT( ADRESS(1, 11) )
         KMOT       = MOTINT( ADRESS(1, 12) )
         KPRES      = MOTINT( ADRESS(1, 13) )
         KVISQ      = MOTINT( ADRESS(1, 14) )
         KVISU      = MOTINT( ADRESS(1, 15) )
         NSORTP     = MOTINT( ADRESS(1, 16) )
!
! Mots-cles reels
!----------------
! Real key words
!---------------
!
         XMIN      = MOTREA( ADRESS(2,  1) )
         XMAX      = MOTREA( ADRESS(2,  2) )
         ZMIN      = MOTREA( ADRESS(2,  3) )
         ZMAX      = MOTREA( ADRESS(2,  4) )
         IF(DIMENS(2,5).NE.0) THEN
           DO 50 K=1,DIMENS(2,5)
            RHO0(K) = MOTREA( ADRESS(2,5) + K-1 )
50         CONTINUE
         ENDIF
         VITC0     = MOTREA( ADRESS(2,  6) )
         IF(DIMENS(2,7).NE.0) THEN
           DO 40 K=1,DIMENS(2,7)
            NU0(K) = MOTREA( ADRESS(2,7) + K-1 )
40         CONTINUE
         ENDIF
         CPAR      = MOTREA( ADRESS(2,  8) )
         RUG       = MOTREA( ADRESS(2,  9) )
         DELTADR   = MOTREA( ADRESS(2, 10) )
         HDR       = MOTREA( ADRESS(2, 11) )
         GAMMAX    = MOTREA( ADRESS(2, 12) )
         GAMMAZ    = MOTREA( ADRESS(2, 13) )
         FORCST    = MOTREA( ADRESS(2, 14) )
         DEBREF    = MOTREA( ADRESS(2, 15) )
         FACTLISS  = MOTREA( ADRESS(2, 16) )
         DESTDR    = MOTREA( ADRESS(2, 17) )
!
! Mots-cles logiques
!-------------------
! Logical key words
!------------------
!
         KSUITE    = MOTLOG( ADRESS(3, 1) )
         KGRAV     = MOTLOG( ADRESS(3, 2) )
         KPER      = MOTLOG( ADRESS(3, 3) )
         KGAMMAX   = MOTLOG( ADRESS(3, 4) )
         KGAMMAZ   = MOTLOG( ADRESS(3, 5) )
         KPARM     = MOTLOG( ADRESS(3, 6) )
         KLISS     = MOTLOG( ADRESS(3, 7) )
!
! Mots-cles chaines de caracteres
!--------------------------------
! Character string key words
!---------------------------
!
         FICHPARAM    = MOTCAR( ADRESS(4, 1) )(1:40)
         FICHMAILL    = MOTCAR( ADRESS(4, 2) )(1:80)
         FICHINIT     = MOTCAR( ADRESS(4, 3) )(1:80)
         FICHSUITE    = MOTCAR( ADRESS(4, 4) )(1:80)
         FICHRES      = MOTCAR( ADRESS(4, 5) )(1:40)
         FICHPOS      = MOTCAR( ADRESS(4, 6) )(1:40)
         NOMETUDE     = MOTCAR( ADRESS(4, 8) )(1:40)
!
      RETURN
      END