C                        *********************
                          SUBROUTINE VISCTURB
C                        *********************
C     
     .  (NBMAX, NLIEN    , NLIENMAX, NPARB, NPARF, NPMAX ,  
     .   NPART, NFLUIDMAX, ILIEN   , KAPPA, KDEF , KFLUID, 
     .   KPAR , KPROD    , KTURB   , CK1  , CMU  , DELTA ,
     .   DIFF , DT       , DR      , EPS  , ETA2 , GKERX ,
     .   GKERZ, GVXX     , GVXZ    , GVZX , GVZZ , LM    ,
     .   MASS , NU0      , NUT     , RAB  , RHO  , S     ,
     .   TKE  , USTAR    , VXAB    , VZAB , XAB  , ZAB   ,
     .   X    , Z                                        )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! CK1, CMU  ! -->  ! K-EPSILON MODEL COEFFICIENTS                   !
C ! CPAR      ! -->  ! WALL FORCE 1 COEFFICIENT                       !
C ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
C ! DIFF      ! -->  ! INTERMEDIATE FOR DIFFUSION TERM COMPUTATION    !
C ! DT        ! -->  ! TIME STEP                                      !
C ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
C ! EPS       ! <--> ! DISSIPATION RATE                               !
C ! ETA2      ! -->  ! CORRECTION TERM FOR DIFFUSION                  !
C ! GKERX,                                                            !
C ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
C ! GVXX, GVXZ                                                        !
C ! GVZX, GVZZ! -->  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! KAPPA     ! -->  ! VON KARMAN CONSTANT                            !
C ! KFLUID    ! -->  ! FLUID TYPE                                     !
C ! KDEF      ! -->  ! CHOICE INDEX FOR THE STRAIN MODEL              !
C ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
C ! KPROD     ! -->  ! CHOICE INDEX FOR THE PRODUCTION MODEL          !
C ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! LM        ! -->  ! MIXING LENGTH                                  !
C ! MASS      ! -->  ! PARTICLE MASS                                  !
C ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
C ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
C ! NUT       ! <--> ! EDDY VISCOSITY                                 !
C ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
C ! RHO       ! -->  ! DENSITY                                        !
C ! S         ! <--  ! RATE OF STRAIN                                 !
C ! TKE       ! <--> ! TURBULENT KINETIC ENERGY                       !
C ! USTAR     ! -->  ! FRICTION VELOCITY                              !
C ! VXAB, VZAB! -->  ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
C ! XAB, ZAB  ! -->  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
C ! X, Z      ! -->  ! PARTICLE POSITION                              !
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
C FONCTION : calcule la viscosite turbulente
C FUNCTION : computes the eddy viscosity
C
C PROGRAMMES APPELANT : IMPULSION
C CALLED BY
C
C PROGRAMMES APPELES  : TAUXDEF, KEQUATION, MELANGE
C CALLED PROGRAMS    
C
C----------------------------------------------------------------------
C     
C Variables
C==========
C
      IMPLICIT NONE
C     
      INTEGER NPMAX, NPARF, NLIENMAX
      INTEGER NBMAX, NPARB, NPART
      INTEGER KDEF , KTURB, KPROD
      INTEGER I
      INTEGER NFLUIDMAX
C
      DOUBLE PRECISION ETA2 , DT , CMU, KAPPA
      DOUBLE PRECISION DELTA, CK1, DR
C 
      INTEGER NLIEN (NPMAX)
      INTEGER KPAR  (NPMAX)
      INTEGER KFLUID(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C     
      DOUBLE PRECISION TKE  (NPMAX), EPS (NPMAX), USTAR(NBMAX)
      DOUBLE PRECISION LM   (NPMAX), S   (NPMAX)
      DOUBLE PRECISION RHO  (NPMAX), NUT (NPMAX), MASS (NPMAX)
      DOUBLE PRECISION GVXX (NPMAX), GVXZ(NPMAX)
      DOUBLE PRECISION GVZX (NPMAX), GVZZ(NPMAX)
      DOUBLE PRECISION X    (NPMAX), Z   (NPMAX)      
C
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX), DIFF (NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX), VZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
C          
      DOUBLE PRECISION NU0 (NFLUIDMAX)
C
C Calcul de la viscosite turbulente
C==================================
C Eddy viscosity computation
C===========================
C
C Calcul du taux de deformation
C------------------------------
C Rate of strain computation
C---------------------------
C
      CALL TAUXDEF
C
     .(NLIEN, NLIENMAX, NPARB, NPARF, NPMAX, ILIEN,
     . KDEF , KPAR    , ETA2 , GKERX, GKERZ, GVXX ,
     . GVXZ , GVZX    , GVZZ , MASS , RAB  , RHO  ,
     . S    , VXAB    , VZAB , XAB  , ZAB         )
C
C Viscosite turbulente
C---------------------
C Eddy viscosity
C---------------
C
      IF (KTURB.EQ.1) THEN
C
C Modele de longueur de melange
C..............................
C Mixing length model
C....................
C
        CALL MELANGE
C
     .   (NPART , NPMAX , DR , LM , X , Z , DELTA , KPAR)
C
        DO 115 I=1,NPARF+NPARB
          NUT(I) = S(I)*LM(I)**2
 115    CONTINUE
C
      ELSE
C
C Modeles a une ou deux equations
C................................
C One or two equation model
C..........................
C
        IF (KTURB.EQ.2) THEN
C
          CALL MELANGE
C
     .   (NPART , NPMAX , DR , LM , X , Z , DELTA , KPAR)
C        
        ENDIF 
         
        CALL KEQUATION
C
     .(NBMAX    , NLIEN, NLIENMAX, NPARB, NPARF, NPMAX,
     . NFLUIDMAX, ILIEN, KAPPA   , KPAR , KPROD, KTURB, 
     . KFLUID   , CK1  , CMU     , DELTA, DIFF , DT   , 
     . EPS      , ETA2 , GKERX   , GKERZ, LM   , MASS , 
     . NU0      , NUT  , RAB     , RHO  , S    , TKE  , 
     . USTAR    , XAB  , ZAB                          )
C
      ENDIF
C
      RETURN
      END
