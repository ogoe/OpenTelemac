!                        *********************
                          SUBROUTINE VISCTURB
!                        *********************
!
     &  (NBMAX, NLIEN    , NLIENMAX, NPARB, NPARF, NPMAX ,
     &   NPART, NFLUIDMAX, ILIEN   , KAPPA, KDEF , KFLUID,
     &   KPAR , KPROD    , KTURB   , CK1  , CMU  , DELTA ,
     &   DIFF , DT       , DR      , EPS  , ETA2 , GKERX ,
     &   GKERZ, GVXX     , GVXZ    , GVZX , GVZZ , LM    ,
     &   MASS , NU0      , NUT     , RAB  , RHO  , S     ,
     &   TKE  , USTAR    , VXAB    , VZAB , XAB  , ZAB   ,
     &   X    , Z                                        )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! CK1, CMU  ! -->  ! K-EPSILON MODEL COEFFICIENTS                   !
! ! CPAR      ! -->  ! WALL FORCE 1 COEFFICIENT                       !
! ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
! ! DIFF      ! -->  ! INTERMEDIATE FOR DIFFUSION TERM COMPUTATION    !
! ! DT        ! -->  ! TIME STEP                                      !
! ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
! ! EPS       ! <--> ! DISSIPATION RATE                               !
! ! ETA2      ! -->  ! CORRECTION TERM FOR DIFFUSION                  !
! ! GKERX,                                                            !
! ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
! ! GVXX, GVXZ                                                        !
! ! GVZX, GVZZ! -->  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! KAPPA     ! -->  ! VON KARMAN CONSTANT                            !
! ! KFLUID    ! -->  ! FLUID TYPE                                     !
! ! KDEF      ! -->  ! CHOICE INDEX FOR THE STRAIN MODEL              !
! ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
! ! KPROD     ! -->  ! CHOICE INDEX FOR THE PRODUCTION MODEL          !
! ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! LM        ! -->  ! MIXING LENGTH                                  !
! ! MASS      ! -->  ! PARTICLE MASS                                  !
! ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
! ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
! ! NUT       ! <--> ! EDDY VISCOSITY                                 !
! ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
! ! RHO       ! -->  ! DENSITY                                        !
! ! S         ! <--  ! RATE OF STRAIN                                 !
! ! TKE       ! <--> ! TURBULENT KINETIC ENERGY                       !
! ! USTAR     ! -->  ! FRICTION VELOCITY                              !
! ! VXAB, VZAB! -->  ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
! ! XAB, ZAB  ! -->  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
! ! X, Z      ! -->  ! PARTICLE POSITION                              !
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
! FONCTION : calcule la viscosite turbulente
! FUNCTION : computes the eddy viscosity
!
! PROGRAMMES APPELANT : IMPULSION
! CALLED BY
!
! PROGRAMMES APPELES  : TAUXDEF, KEQUATION, MELANGE
! CALLED PROGRAMS
!
!----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER NPMAX, NPARF, NLIENMAX
      INTEGER NBMAX, NPARB, NPART
      INTEGER KDEF , KTURB, KPROD
      INTEGER I
      INTEGER NFLUIDMAX
!
      DOUBLE PRECISION ETA2 , DT , CMU, KAPPA
      DOUBLE PRECISION DELTA, CK1, DR
!
      INTEGER NLIEN (NPMAX)
      INTEGER KPAR  (NPMAX)
      INTEGER KFLUID(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION TKE  (NPMAX), EPS (NPMAX), USTAR(NBMAX)
      DOUBLE PRECISION LM   (NPMAX), S   (NPMAX)
      DOUBLE PRECISION RHO  (NPMAX), NUT (NPMAX), MASS (NPMAX)
      DOUBLE PRECISION GVXX (NPMAX), GVXZ(NPMAX)
      DOUBLE PRECISION GVZX (NPMAX), GVZZ(NPMAX)
      DOUBLE PRECISION X    (NPMAX), Z   (NPMAX)
!
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX), DIFF (NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX), VZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION NU0 (NFLUIDMAX)
!
! Calcul de la viscosite turbulente
!==================================
! Eddy viscosity computation
!===========================
!
! Calcul du taux de deformation
!------------------------------
! Rate of strain computation
!---------------------------
!
      CALL TAUXDEF
!
     &(NLIEN, NLIENMAX, NPARB, NPARF, NPMAX, ILIEN,
     & KDEF , KPAR    , ETA2 , GKERX, GKERZ, GVXX ,
     & GVXZ , GVZX    , GVZZ , MASS , RAB  , RHO  ,
     & S    , VXAB    , VZAB , XAB  , ZAB         )
!
! Viscosite turbulente
!---------------------
! Eddy viscosity
!---------------
!
      IF (KTURB.EQ.1) THEN
!
! Modele de longueur de melange
!..............................
! Mixing length model
!....................
!
        CALL MELANGE
!
     &   (NPART , NPMAX , DR , LM , X , Z , DELTA , KPAR)
!
        DO 115 I=1,NPARF+NPARB
          NUT(I) = S(I)*LM(I)**2
 115    CONTINUE
!
      ELSE
!
! Modeles a une ou deux equations
!................................
! One or two equation model
!..........................
!
        IF (KTURB.EQ.2) THEN
!
          CALL MELANGE
!
     &   (NPART , NPMAX , DR , LM , X , Z , DELTA , KPAR)
!
        ENDIF
!
        CALL KEQUATION
!
     &(NBMAX    , NLIEN, NLIENMAX, NPARB, NPARF, NPMAX,
     & NFLUIDMAX, ILIEN, KAPPA   , KPAR , KPROD, KTURB,
     & KFLUID   , CK1  , CMU     , DELTA, DIFF , DT   ,
     & EPS      , ETA2 , GKERX   , GKERZ, LM   , MASS ,
     & NU0      , NUT  , RAB     , RHO  , S    , TKE  ,
     & USTAR    , XAB  , ZAB                          )
!
      ENDIF
!
      RETURN
      END