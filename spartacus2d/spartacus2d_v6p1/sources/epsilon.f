C                        ********************
                          SUBROUTINE EPSILON
C                        ********************
C
     .(NBMAX    , NLIEN, NLIENMAX, NPARB, NPARF , NPMAX,
     . NFLUIDMAX, ILIEN, KAPPA   , KPAR , KFLUID, DELTA, 
     . DIFF     , DT   , EPS     , MASS , NU0   , NUT  ,
     . PROD     , RHO  , TKE     , USTAR               )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
C ! DIFF      ! -->  ! INTERMEDIATE FOR DIFFUSION TERM COMPUTATION    !
C ! DT        ! -->  ! TIME STEP                                      !
C ! EPS       ! <--  ! DISSIPATION RATE                               !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! KAPPA     ! -->  ! VON KARMAN CONSTANT                            !
C ! KFLUID    ! -->  ! FLUID TYPE                                     !
C ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
C ! MASS      ! -->  ! PARTICLE MASS                                  !
C ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
C ! NUT       ! -->  ! EDDY VISCOSITY                                 !
C ! PROD      ! -->  ! PRODUCTION RATE                                !
C ! RHO       ! -->  ! DENSITY                                        !
C ! TKE       ! -->  ! TURBULENT KINETIC ENERGY                       !
C ! USTAR     ! -->  ! FRICTION VELOCITY                              !
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
C FONCTION : calcule le taux de dissipation
C FUNCTION : computes the dissipation rate
C
C PROGRAMMES APPELANT : KEQUATION
C CALLED BY
C
C PROGRAMMES APPELES  : -
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
      INTEGER NPARB, NBMAX, NFLUIDMAX
      INTEGER I    , L    , J
      INTEGER M    , N
C     
      DOUBLE PRECISION DT , TEMPO, KAPPA
      DOUBLE PRECISION CE1, CE2  , SIGMAE, DELTA
C
      INTEGER NLIEN (NPMAX)
      INTEGER KPAR  (NPMAX)
      INTEGER KFLUID(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C     
      DOUBLE PRECISION RHO  (NPMAX), MASS(NPMAX)
      DOUBLE PRECISION EPS  (NPMAX), NUT (NPMAX), TKE  (NPMAX)
      DOUBLE PRECISION DIFFE(NPMAX), PROD(NPMAX), USTAR(NBMAX)
C
      DOUBLE PRECISION NU0 (NFLUIDMAX)
C
      DOUBLE PRECISION DIFF (NPMAX,NLIENMAX)
C
      PARAMETER (CE1   =1.44D0,
     .           CE2   =1.92D0,
     .           SIGMAE=1.30D0)
C     
C Calcul de epsilon
C==================
C
C Terme de diffusion pour epsilon
C--------------------------------
C
      DO 407 I=1,NPARF
        DIFFE(I) = 0.D0
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
              TEMPO=((RHO(I)*((NUT(I)/SIGMAE)+NU0(M)))
     .	            +(RHO(J)*((NUT(J)/SIGMAE)+NU0(N))))
     .              *(EPS(I)-EPS(J))
     .              *DIFF(I,L)
     .              /(RHO(I)*RHO(J))
              DIFFE(I) = DIFFE(I) + MASS(J)*TEMPO
              DIFFE(J) = DIFFE(J) - MASS(I)*TEMPO
C
            ENDIF
C
 104      CONTINUE
 103  CONTINUE
C
C Condition aux parois
C.....................
C
      DO 805 I=NPARF+1,NPARF+NPARB
        DIFFE(I) = USTAR(I-NPARF)**4/(SIGMAE*DELTA**2)
 805  CONTINUE
C
C Determination de epsilon
C-------------------------
C
C Particules fluides
C...................
C
      DO 705 I=1,NPARF
        TEMPO  = (CE1*PROD(I)-CE2*EPS(I))*EPS(I)/TKE(I)+DIFFE(I)
        EPS(I) =  EPS(I)+DT*TEMPO
 705  CONTINUE
C
C Condition aux parois
C.....................
C
      DO 706 I=NPARF+1,NPARF+NPARB
        TEMPO  = (CE1*PROD(I)-CE2*EPS(I))*EPS(I)/TKE(I)+DIFFE(I)
        EPS(I) = EPS(I)+DT*TEMPO
        EPS(I) = MAX(EPS(I),ABS(USTAR(I-NPARF)**3)/(KAPPA*DELTA))
 706  CONTINUE
C
      RETURN
      END
