!                        **********************
                          SUBROUTINE KEQUATION
!                        **********************
!
     &(NBMAX    , NLIEN, NLIENMAX, NPARB, NPARF, NPMAX,
     & NFLUIDMAX, ILIEN, KAPPA   , KPAR , KPROD, KTURB,
     & KFLUID   , CK1  , CMU     , DELTA, DIFF , DT   ,
     & EPS      , ETA2 , GKERX   , GKERZ, LM   , MASS ,
     & NU0      , NUT  , RAB     , RHO  , S    , TKE  ,
     & USTAR    , XAB  , ZAB                          )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! CK1, CMU  ! -->  ! K-EPSILON MODEL COEFFICIENTS                   !
! ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
! ! DIFF      ! <--  ! INTERMEDIATE FOR DIFFUSION TERM COMPUTATION    !
! ! DT        ! -->  ! TIME STEP                                      !
! ! EPS       ! <--> ! DISSIPATION RATE                               !
! ! ETA2      ! -->  ! CORRECTION TERM FOR DIFFUSION                  !
! ! GKERX,                                                            !
! ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! KAPPA     ! -->  ! VON KARMAN CONSTANT                            !
! ! KFLUID    ! -->  ! FLUID TYPE                                     !
! ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
! ! KPROD     ! -->  ! CHOICE INDEX FOR THE PRODUCTION MODEL          !
! ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! LM        ! -->  ! MIXING LENGTH                                  !
! ! MASS      ! -->  ! PARTICLE MASS                                  !
! ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
! ! NUT       ! <--> ! EDDY VISCOSITY                                 !
! ! PROD      ! <--  ! PRODUCTION RATE                                !
! ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
! ! RHO       ! -->  ! DENSITY                                        !
! ! S         ! -->  ! RATE OF STRAIN                                 !
! ! TKE       ! <--> ! TURBULENT KINETIC ENERGY                       !
! ! USTAR     ! -->  ! FRICTION VELOCITY                              !
! ! XAB, ZAB  ! -->  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
! !___________!______!________________________________________________!
!
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!-----------------------------------------------------------------------
!
! SPARTACUS2D V5P9
! D. Violeau           & R. Issa
! +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28
! LNHE - 2008
!
! FONCTION : calcule l'energie cinetique turbulente
! FUNCTION : computes the kinetic turbulent energy
!
! PROGRAMMES APPELANT : VISCTURB
! CALLED BY
!
! PROGRAMMES APPELES  : EPSILON
! CALLED PROGRAMS
!
!-----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER NPMAX, NPARF, NLIENMAX
      INTEGER NPARB, NBMAX, KPROD   , KTURB
      INTEGER I    , L    , J       , NFLUIDMAX
      INTEGER M    , N
!
      DOUBLE PRECISION DT    , TEMPO, ETA2
      DOUBLE PRECISION CMU   , CK1  , RCMU, DELTA
      DOUBLE PRECISION SIGMAK
      DOUBLE PRECISION KAPPA
!
      INTEGER NLIEN (NPMAX)
      INTEGER KPAR  (NPMAX)
      INTEGER KFLUID(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION RHO  (NPMAX), MASS(NPMAX), NUT  (NPMAX)
      DOUBLE PRECISION TKE  (NPMAX), EPS (NPMAX), LM   (NPMAX)
      DOUBLE PRECISION DIFFK(NPMAX), S   (NPMAX), USTAR(NBMAX)
      DOUBLE PRECISION PROD (NPMAX)
!
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX), DIFF (NPMAX,NLIENMAX)
!
      DOUBLE PRECISION NU0 (NFLUIDMAX)
!
      PARAMETER (RCMU  =0.30D0,
     &           SIGMAK=1.00D0)
!
! Calcul de k
!============
! k computation
!==============
!
! Terme de production
!--------------------
! Production term
!----------------
!
      IF (KPROD.EQ.1) THEN
!
! Modele classique
!..................
! Classical model
!................
!
        DO 806 I=1,NPARF+NPARB
          PROD(I) = CMU*TKE(I)/EPS(I)*S(I) *TKE(I)*S(I)
 806    CONTINUE
!
      ELSE
!
! Modele de Laurence
!...................
! Laurence's model
!.................
!
        DO 807 I=1,NPARF+NPARB
          PROD(I) = MIN(RCMU,CMU*TKE(I)/EPS(I)*S(I))*TKE(I)*S(I)
 807    CONTINUE
!
      ENDIF
!
! Terme de diffusion pour k
!--------------------------
! Diffusion term relative to k
!-----------------------------
!
      DO 407 I=1,NPARF
        DIFFK(I) = 0.D0
 407  CONTINUE
!
      DO 103 I=1,NPARF
        M=KFLUID(I)
          DO 104 L=1,NLIEN(I)
            J=ILIEN(I,L)
            N=KFLUID(J)
!
            IF (KPAR(J).LE.2) THEN
!
              DIFF(I,L)=(GKERX(I,L)*XAB(I,L)
     &                  +GKERZ(I,L)*ZAB(I,L))
     &                  /(RAB(I,L)**2+ETA2)
              TEMPO    =((RHO(I)*((NUT(I)/SIGMAK)+NU0(M)))
     &                  +(RHO(J)*((NUT(J)/SIGMAK)+NU0(N))))
     &                  *(TKE(I)-TKE(J))
     &                  *DIFF(I,L)
     &                  /(RHO(I)*RHO(J))
              DIFFK(I) = DIFFK(I) + MASS(J)*TEMPO
              DIFFK(J) = DIFFK(J) - MASS(I)*TEMPO
!
            ENDIF
!
 104      CONTINUE
 103  CONTINUE
!
! Condition en surface
!.....................
! Condition at the surface
!.........................
!
! Determination de k
!-------------------
! k computation
!--------------
!
! Particules fluides
!...................
! Fluid particles
!................
!
      DO 105 I=1,NPARF
        TEMPO  = PROD(I)+DIFFK(I)-EPS(I)
        TKE(I) = TKE(I)+DT*TEMPO
 105  CONTINUE
!
! Condition aux parois
!.....................
! Wall condition
!...............
!
      DO 115 I=NPARF+1,NPARF+NPARB
        TKE(I) = USTAR(I-NPARF)**2/RCMU
 115  CONTINUE
!
! Determination de epsilon
!-------------------------
! Epsilon determination
!----------------------
!
      IF (KTURB.EQ.2) THEN
!
! Modele a une equation en k
!...........................
! k model
!........
!
        DO 111 I=1,NPARF+NPARB
          EPS(I)=CK1*SQRT(TKE(I)**3)/LM(I)
 111    CONTINUE
!
      ELSE
!
! Modele k-epsilon
!.................
! k-epsilon model
!................
!
        CALL EPSILON
!
     &(NBMAX    , NLIEN, NLIENMAX, NPARB, NPARF , NPMAX,
     & NFLUIDMAX, ILIEN, KAPPA   , KPAR , KFLUID, DELTA,
     & DIFF     , DT   , EPS     , MASS , NU0   , NUT  ,
     & PROD     , RHO  , TKE     , USTAR               )
!
      ENDIF
!
! Determination de nut
!---------------------
! nut computation
!----------------
!
      DO 605 I=1,NPARF+NPARB
        TKE(I) = MAX(TKE(I),1.D-5)
        EPS(I) = MAX(EPS(I),1.D-5)
        NUT(I) = CMU*TKE(I)**2/EPS(I)
 605  CONTINUE
!
      RETURN
      END