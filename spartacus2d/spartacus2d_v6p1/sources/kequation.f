C                        **********************
                          SUBROUTINE KEQUATION
C                        **********************
C
     .(NBMAX    , NLIEN, NLIENMAX, NPARB, NPARF, NPMAX,
     . NFLUIDMAX, ILIEN, KAPPA   , KPAR , KPROD, KTURB, 
     . KFLUID   , CK1  , CMU     , DELTA, DIFF , DT   , 
     . EPS      , ETA2 , GKERX   , GKERZ, LM   , MASS , 
     . NU0      , NUT  , RAB     , RHO  , S    , TKE  , 
     . USTAR    , XAB  , ZAB                          )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! CK1, CMU  ! -->  ! K-EPSILON MODEL COEFFICIENTS                   !
C ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
C ! DIFF      ! <--  ! INTERMEDIATE FOR DIFFUSION TERM COMPUTATION    !
C ! DT        ! -->  ! TIME STEP                                      !
C ! EPS       ! <--> ! DISSIPATION RATE                               !
C ! ETA2      ! -->  ! CORRECTION TERM FOR DIFFUSION                  !
C ! GKERX,                                                            !
C ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! KAPPA     ! -->  ! VON KARMAN CONSTANT                            !
C ! KFLUID    ! -->  ! FLUID TYPE                                     !
C ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
C ! KPROD     ! -->  ! CHOICE INDEX FOR THE PRODUCTION MODEL          !
C ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! LM        ! -->  ! MIXING LENGTH                                  !
C ! MASS      ! -->  ! PARTICLE MASS                                  !
C ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
C ! NUT       ! <--> ! EDDY VISCOSITY                                 !
C ! PROD      ! <--  ! PRODUCTION RATE                                !
C ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
C ! RHO       ! -->  ! DENSITY                                        !
C ! S         ! -->  ! RATE OF STRAIN                                 !
C ! TKE       ! <--> ! TURBULENT KINETIC ENERGY                       !
C ! USTAR     ! -->  ! FRICTION VELOCITY                              !
C ! XAB, ZAB  ! -->  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
C !___________!______!________________________________________________!
C
C MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
C-----------------------------------------------------------------------
C     
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C     
C FONCTION : calcule l'energie cinetique turbulente
C FUNCTION : computes the kinetic turbulent energy
C
C PROGRAMMES APPELANT : VISCTURB
C CALLED BY
C
C PROGRAMMES APPELES  : EPSILON
C CALLED PROGRAMS
C     
C-----------------------------------------------------------------------
C     
C Variables
C==========
C
      IMPLICIT NONE
C     
      INTEGER NPMAX, NPARF, NLIENMAX
      INTEGER NPARB, NBMAX, KPROD   , KTURB
      INTEGER I    , L    , J       , NFLUIDMAX
      INTEGER M    , N
C     
      DOUBLE PRECISION DT    , TEMPO, ETA2
      DOUBLE PRECISION CMU   , CK1  , RCMU, DELTA
      DOUBLE PRECISION SIGMAK
      DOUBLE PRECISION KAPPA
C
      INTEGER NLIEN (NPMAX)
      INTEGER KPAR  (NPMAX)
      INTEGER KFLUID(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C     
      DOUBLE PRECISION RHO  (NPMAX), MASS(NPMAX), NUT  (NPMAX)
      DOUBLE PRECISION TKE  (NPMAX), EPS (NPMAX), LM   (NPMAX)
      DOUBLE PRECISION DIFFK(NPMAX), S   (NPMAX), USTAR(NBMAX)
      DOUBLE PRECISION PROD (NPMAX)
C
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX), DIFF (NPMAX,NLIENMAX)
C
      DOUBLE PRECISION NU0 (NFLUIDMAX)
C
      PARAMETER (RCMU  =0.30D0,
     .           SIGMAK=1.00D0)
C     
C Calcul de k
C============
C k computation
C==============
C
C Terme de production
C--------------------
C Production term
C----------------
C
      IF (KPROD.EQ.1) THEN
C
C Modele classique
C..................
C Classical model
C................
C
        DO 806 I=1,NPARF+NPARB
          PROD(I) = CMU*TKE(I)/EPS(I)*S(I) *TKE(I)*S(I)
 806    CONTINUE
C
      ELSE
C
C Modele de Laurence
C...................
C Laurence's model
C.................
C
        DO 807 I=1,NPARF+NPARB
          PROD(I) = MIN(RCMU,CMU*TKE(I)/EPS(I)*S(I))*TKE(I)*S(I)
 807    CONTINUE
C
      ENDIF
C
C Terme de diffusion pour k
C--------------------------
C Diffusion term relative to k
C-----------------------------
C
      DO 407 I=1,NPARF
        DIFFK(I) = 0.D0
 407  CONTINUE
C
      DO 103 I=1,NPARF
        M=KFLUID(I)
          DO 104 L=1,NLIEN(I)
            J=ILIEN(I,L)
            N=KFLUID(J)
C            
            IF (KPAR(J).LE.2) THEN
C
              DIFF(I,L)=(GKERX(I,L)*XAB(I,L)
     .                  +GKERZ(I,L)*ZAB(I,L))
     .                  /(RAB(I,L)**2+ETA2)
              TEMPO    =((RHO(I)*((NUT(I)/SIGMAK)+NU0(M)))
     .                  +(RHO(J)*((NUT(J)/SIGMAK)+NU0(N))))
     .                  *(TKE(I)-TKE(J))
     .                  *DIFF(I,L)
     .                  /(RHO(I)*RHO(J))
              DIFFK(I) = DIFFK(I) + MASS(J)*TEMPO
              DIFFK(J) = DIFFK(J) - MASS(I)*TEMPO
C
            ENDIF
C
 104      CONTINUE
 103  CONTINUE
C
C Condition en surface
C.....................
C Condition at the surface
C.........................
C
C Determination de k
C-------------------
C k computation
C--------------
C
C Particules fluides
C...................
C Fluid particles
C................
C
      DO 105 I=1,NPARF
        TEMPO  = PROD(I)+DIFFK(I)-EPS(I)
        TKE(I) = TKE(I)+DT*TEMPO
 105  CONTINUE
C
C Condition aux parois
C.....................
C Wall condition
C...............
C
      DO 115 I=NPARF+1,NPARF+NPARB
        TKE(I) = USTAR(I-NPARF)**2/RCMU
 115  CONTINUE
C
C Determination de epsilon
C-------------------------
C Epsilon determination
C----------------------
C
      IF (KTURB.EQ.2) THEN
C
C Modele a une equation en k
C...........................
C k model
C........
C
        DO 111 I=1,NPARF+NPARB
          EPS(I)=CK1*SQRT(TKE(I)**3)/LM(I)
 111    CONTINUE
C
      ELSE
C
C Modele k-epsilon
C.................
C k-epsilon model
C................
C
        CALL EPSILON
C
     .(NBMAX    , NLIEN, NLIENMAX, NPARB, NPARF , NPMAX,
     . NFLUIDMAX, ILIEN, KAPPA   , KPAR , KFLUID, DELTA, 
     . DIFF     , DT   , EPS     , MASS , NU0   , NUT  ,
     . PROD     , RHO  , TKE     , USTAR               )
C
      ENDIF
C
C Determination de nut
C---------------------
C nut computation
C----------------
C
      DO 605 I=1,NPARF+NPARB
        TKE(I) = MAX(TKE(I),1.D-5)
        EPS(I) = MAX(EPS(I),1.D-5)
        NUT(I) = CMU*TKE(I)**2/EPS(I)
 605  CONTINUE
C
      RETURN
      END
