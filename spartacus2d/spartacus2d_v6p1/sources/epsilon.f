!                        ********************
                          SUBROUTINE EPSILON
!                        ********************
!
     &(NBMAX    , NLIEN, NLIENMAX, NPARB, NPARF , NPMAX,
     & NFLUIDMAX, ILIEN, KAPPA   , KPAR , KFLUID, DELTA,
     & DIFF     , DT   , EPS     , MASS , NU0   , NUT  ,
     & PROD     , RHO  , TKE     , USTAR               )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
! ! DIFF      ! -->  ! INTERMEDIATE FOR DIFFUSION TERM COMPUTATION    !
! ! DT        ! -->  ! TIME STEP                                      !
! ! EPS       ! <--  ! DISSIPATION RATE                               !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! KAPPA     ! -->  ! VON KARMAN CONSTANT                            !
! ! KFLUID    ! -->  ! FLUID TYPE                                     !
! ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
! ! MASS      ! -->  ! PARTICLE MASS                                  !
! ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
! ! NUT       ! -->  ! EDDY VISCOSITY                                 !
! ! PROD      ! -->  ! PRODUCTION RATE                                !
! ! RHO       ! -->  ! DENSITY                                        !
! ! TKE       ! -->  ! TURBULENT KINETIC ENERGY                       !
! ! USTAR     ! -->  ! FRICTION VELOCITY                              !
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
! FONCTION : calcule le taux de dissipation
! FUNCTION : computes the dissipation rate
!
! PROGRAMMES APPELANT : KEQUATION
! CALLED BY
!
! PROGRAMMES APPELES  : -
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
      INTEGER NPARB, NBMAX, NFLUIDMAX
      INTEGER I    , L    , J
      INTEGER M    , N
!
      DOUBLE PRECISION DT , TEMPO, KAPPA
      DOUBLE PRECISION CE1, CE2  , SIGMAE, DELTA
!
      INTEGER NLIEN (NPMAX)
      INTEGER KPAR  (NPMAX)
      INTEGER KFLUID(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION RHO  (NPMAX), MASS(NPMAX)
      DOUBLE PRECISION EPS  (NPMAX), NUT (NPMAX), TKE  (NPMAX)
      DOUBLE PRECISION DIFFE(NPMAX), PROD(NPMAX), USTAR(NBMAX)
!
      DOUBLE PRECISION NU0 (NFLUIDMAX)
!
      DOUBLE PRECISION DIFF (NPMAX,NLIENMAX)
!
      PARAMETER (CE1   =1.44D0,
     &           CE2   =1.92D0,
     &           SIGMAE=1.30D0)
!
! Calcul de epsilon
!==================
!
! Terme de diffusion pour epsilon
!--------------------------------
!
      DO 407 I=1,NPARF
        DIFFE(I) = 0.D0
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
              TEMPO=((RHO(I)*((NUT(I)/SIGMAE)+NU0(M)))
     &	            +(RHO(J)*((NUT(J)/SIGMAE)+NU0(N))))
     &              *(EPS(I)-EPS(J))
     &              *DIFF(I,L)
     &              /(RHO(I)*RHO(J))
              DIFFE(I) = DIFFE(I) + MASS(J)*TEMPO
              DIFFE(J) = DIFFE(J) - MASS(I)*TEMPO
!
            ENDIF
!
 104      CONTINUE
 103  CONTINUE
!
! Condition aux parois
!.....................
!
      DO 805 I=NPARF+1,NPARF+NPARB
        DIFFE(I) = USTAR(I-NPARF)**4/(SIGMAE*DELTA**2)
 805  CONTINUE
!
! Determination de epsilon
!-------------------------
!
! Particules fluides
!...................
!
      DO 705 I=1,NPARF
        TEMPO  = (CE1*PROD(I)-CE2*EPS(I))*EPS(I)/TKE(I)+DIFFE(I)
        EPS(I) =  EPS(I)+DT*TEMPO
 705  CONTINUE
!
! Condition aux parois
!.....................
!
      DO 706 I=NPARF+1,NPARF+NPARB
        TEMPO  = (CE1*PROD(I)-CE2*EPS(I))*EPS(I)/TKE(I)+DIFFE(I)
        EPS(I) = EPS(I)+DT*TEMPO
        EPS(I) = MAX(EPS(I),ABS(USTAR(I-NPARF)**3)/(KAPPA*DELTA))
 706  CONTINUE
!
      RETURN
      END