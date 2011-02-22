c
C                      *******************************
                        SUBROUTINE LECDON_SPARTACUS2D 
C                      *******************************
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
C ! KLISS     ! <--  ! CHOICE INDEX FOR THE DENSITY SMOOTHING         !
C ! KMOT      ! <--  ! CHOICE INDEX FOR THE FORCING TERM              !
C ! KPER      ! <--  ! LOGICAL INDEX FOR PERIODICITY                  !
C ! KPARM     ! <--  ! LOGICAL INDEX FOR MOVING WALL                  !
C ! KPRES     ! <--  ! CHOICE INDEX FOR THE PRESSURE GRADIENT MODEL   !
C ! KPROD     ! <--  ! CHOICE INDEX FOR PRODUCTION MODEL              !
C ! KSUITE    ! <--  ! LOGICAL FOR CALCULATION CONTINUED              !
C ! KTURB     ! <--  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! KUSTAR    ! <--  ! CHOICE INDEX FOR FRICTION VELOCITY COMPUTATION !
C ! KVISQ     ! <--  ! CHOICE INDEX FOR THE VISCOUS MODEL             !
C ! KVISU     ! <--  ! CHOICE INDEX FOR THE POSTPROCESSOR             !
C ! LNG       ! <--  ! CHOICE INDEX FOR LANGUAGE                      !
C ! MAXVAR    ! <--  ! MAXIMUM NUMBER OF PRINTOUT VARIABLES           !
C ! MOTINT    ! <--  ! INTEGER KEY-WORDS                              !
C ! MOTREA    ! <--  ! REAL KEY-WORDS                                 !
C ! MOTLOG    ! <--  ! LOGIC KEY-WORDS                                !
C ! MOTCAR    ! <--  ! CHARACTER KEY-WORDS                            !
C ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NLIST     ! <--  ! LISTING PRINTOUT PERIOD                        !
C ! NMAX      ! <--  ! MAXIMAL NUMBER OF KEY-WORDS ACCORDING TO TYPE  !
C ! NPARF     ! <--  ! NUMBER OF FLUID PARTICLES                      !
C ! NPART     ! <--  ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NSORTP    ! <--  ! POSITION PRINTOUT PERIOD                       !
C ! NSORTR    ! <--  ! FIELD PRINTOUT PERIOD                          !
C ! NT        ! <--  ! NUMBER OF TIME STEPS                           !
C ! NU0       ! <--  ! MOLECULAR VISCOSITIES                          !
C ! RHO0      ! <--  ! REFERENCE DENSITIES                            !
C ! RUG       ! <--  ! WALL ROUGHNESS                                 !
C ! SORLEO    ! <--  ! LOGICAL RELATIVE TO KEY WORDS                  !
C ! VITC0     ! <--  ! SPEED OF SOUND                                 !
C ! XMIN, XMAX! <--  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
C ! ZMIN, ZMAX! <--  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
C !___________!______!________________________________________________!
C
C MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
C----------------------------------------------------------------------
C
C SPARTACUS2D V5P9
C D. Rouge
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C
C FONCTION : lecture des parametres d entree par Damocles
C FUNCTION : Input parameter reading by Damocles
C
C PROGRAMMES APPELANT : LECPARAM
C CALLED BY           
C
C PROGRAMMES APPELES  : READ_CONFIG, DAMOCLE
C CALLED PROGRAMS     
C
C----------------------------------------------------------------------
C
C Variables
C==========
C
      IMPLICIT NONE
C
      INTEGER KK
      INTEGER LNG   , LU
      INTEGER NCAR
      INTEGER K
C
c      CHARACTER*72 CHAINE 
C      
      COMMON/INFO/LNG,LU     
C
C Declaration mots-cles entiers
C------------------------------
C Integer key word declaration
C-----------------------------
C
      INTEGER NT     , NLIST    , NSORTR
      INTEGER NFLUID   
      INTEGER NSORTP , NFLUIDMAX
      INTEGER KFPAR  , KKERNEL  , KTURB       
      INTEGER KDEF   , KPROD    , KUSTAR
      INTEGER KMOT   , KCPAR    , KPRES
      INTEGER KVISQ  , KVISU    
C
C Declaration mots-cles reels
C----------------------------
C Real key word declaration
C--------------------------
C
      DOUBLE PRECISION CPAR  , DELTADR  , DEBREF
      DOUBLE PRECISION FORCST, GAMMAX   , GAMMAZ
      DOUBLE PRECISION HDR   , RUG      , VITC0
      DOUBLE PRECISION XMIN  , XMAX     , ZMIN
      DOUBLE PRECISION ZMAX  , FACTLISS , DESTDR
C
      DOUBLE PRECISION RHO0(NFLUIDMAX), NU0(NFLUIDMAX)
C
C Declaration mots-cles logiques
C-------------------------------
C Logical key word declaration
C-----------------------------
C
      LOGICAL KSUITE , KGRAV  , KPER
      LOGICAL KGAMMAX, KGAMMAZ, KPARM
      LOGICAL KLISS
C
C Declaration mots-cles chaines de caracteres
C--------------------------------------------
C Character string key word declaration
C--------------------------------------
C
      CHARACTER*40  NOMETUDE , FICHPARAM
      CHARACTER*80  FICHINIT , FICHSUITE
      CHARACTER*40  FICHRES  , FICHPOS
      CHARACTER*80  FICHMAILL
C
C Declaration des tableaux pour Damocles
C=======================================
C Array declaration for Damocles
C===============================
C
      INTEGER, PARAMETER :: NMAX = 300
C
      LOGICAL DOC
C
      LOGICAL MOTLOG (NMAX)
C
      INTEGER MOTINT (NMAX)
      INTEGER ADRESS (4,NMAX), DIMENS (4,NMAX)
      INTEGER TROUVE (4,NMAX) 
C      
      DOUBLE PRECISION MOTREA (NMAX)
C
      CHARACTER*250    CHAINE
      CHARACTER*144    MOTCAR (NMAX)
      CHARACTER*72     MOTCLE (4,NMAX,2)
C
      CHARACTER(LEN=144) FILE_DESC(4,300)
      CHARACTER(LEN=250) NOM_CAS, NOM_DIC
C
      NCAR = 0
      CHAINE=''
C
C Initialisation des tableaux pour Damocles
C==========================================
C Array initialization for Damocles
C==================================
C
      DO 10 KK=1,NMAX
C
        MOTCAR(KK)(1:1)=' '
C
        DIMENS(1,KK) = 0
        DIMENS(2,KK) = 0
        DIMENS(3,KK) = 0
        DIMENS(4,KK) = 0
C
10    CONTINUE
C
C     IMPRESSION DE LA DOC
C     nulle par defaut
C
      DOC = .FALSE.
C
C Langue et index des sorties
C============================
C Language and logical unit for outputs
C======================================
C
      CALL READ_CONFIG
C      
     .   (LNG, LU, CHAINE, NCAR)
C
C Ouverture des fichiers dictionnaire et cas
C===========================================
C Dictionnary and case file opening
C==================================
C
        NOM_DIC='T2DDICO'
        NOM_CAS='T2CAS'
C
      OPEN(2,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      OPEN(3,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
C
      CALL DAMOCLE
C      
     .   (ADRESS, DIMENS, NMAX   , DOC       , LNG    , LU    ,
     .    MOTINT, MOTREA, MOTLOG , MOTCAR    , MOTCLE , TROUVE,
     .    2     , 3     , .FALSE., FILE_DESC )
C     
C Fermeture des fichiers dictionnaire et cas
C===========================================
C Dictionnary and case closing
C=============================
C
      CLOSE(2)
      CLOSE(3)
C
C Affectation des parametres sous leur nom en fortran
C====================================================
C Parameter assigning according to their fortran name
C====================================================
C
C Mots-cles entiers
C------------------
C Integer key words
C------------------
C
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
C
C Mots-cles reels
C----------------
C Real key words
C---------------
C
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
C
C Mots-cles logiques
C-------------------
C Logical key words
C------------------
C
         KSUITE    = MOTLOG( ADRESS(3, 1) )
         KGRAV     = MOTLOG( ADRESS(3, 2) )
         KPER      = MOTLOG( ADRESS(3, 3) )
         KGAMMAX   = MOTLOG( ADRESS(3, 4) )
         KGAMMAZ   = MOTLOG( ADRESS(3, 5) )
         KPARM     = MOTLOG( ADRESS(3, 6) )
         KLISS     = MOTLOG( ADRESS(3, 7) )         
C
C Mots-cles chaines de caracteres
C--------------------------------
C Character string key words
C---------------------------
C
         FICHPARAM    = MOTCAR( ADRESS(4, 1) )(1:40)
         FICHMAILL    = MOTCAR( ADRESS(4, 2) )(1:80)
         FICHINIT     = MOTCAR( ADRESS(4, 3) )(1:80)
         FICHSUITE    = MOTCAR( ADRESS(4, 4) )(1:80)
         FICHRES      = MOTCAR( ADRESS(4, 5) )(1:40)
         FICHPOS      = MOTCAR( ADRESS(4, 6) )(1:40)
         NOMETUDE     = MOTCAR( ADRESS(4, 8) )(1:40)
C
      RETURN
      END
