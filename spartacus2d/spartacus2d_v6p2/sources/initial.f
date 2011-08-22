!                        ********************
                          SUBROUTINE INITIAL
!                        ********************
!
     &  (NBMAX, NFLUIDMAX, NPARB  , NPARF  , NPART, NPMAX ,
     &   CT   , KENT     , KFLUID , KKERNEL, KPAR , KSUITE,
     &   KTURB, CK1      , KPARMOB, CMU    , CR   , CTHETA,
     &   DEB  , DELTA    , DELTADR, DR     , DT   , EPS   ,
     &   ETA2 , FMOT     , GRAV   , H      , HDR  , KAPPA ,
     &   LM   , MASS     , NUT    , P      , PI   , PRIV  ,
     &   R0   , RHO      , RHO0   , STHETA , SUPP , TEMPS ,
     &   THETA, TKE      , VITC0  , VX     , VZ   , X     ,
     &   Z    , S                                         )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! CK1, CMU  ! <--  ! K-EPSILON MODEL COEFFICIENTS                   !
! ! CR        ! <--  ! COEFFICIENT RELATIVE TO A ROUGH LOG LAW        !
! ! CT        ! <--  ! TECPLOT CURRENT ZONE NUMBER                    !
! ! CTHETA,                                                           !
! ! STHETA    ! <--  ! COMPONENTS OF WALL NORMAL VECTORS              !
! ! DEB       ! <--  ! COMPUTED MEAN BULK VELOCITY                    !
! ! DELTA     ! <--  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
! ! DELTADR   ! -->  ! RATIO DELTA/DR                                 !
! ! DR        ! <--  ! INITIAL INTERPARTICLE SPACING                  !
! ! EPS       ! <--  ! DISSIPATION RATE                               !
! ! DT        ! <--  ! TIME STEP                                      !
! ! ETA2      ! <--  ! CORRECTION TERM FOR DIFFUSION                  !
! ! FMOT      ! <--  ! AXIAL DRIVING FORCE                            !
! ! GRAV      ! <--  ! GRAVITY                                        !
! ! H         ! <--  ! SMOOTHING LENGTH                               !
! ! HDR       ! -->  ! RATIO H/DR                                     !
! ! KAPPA     ! <--  ! VON KARMAN CONSTANT                            !
! ! KENT      ! <--  ! CONDITION TYPE AT FLUID BOUNDARIES             !
! ! KFLUID    ! <--  ! FLUID TYPE                                     !
! ! KKERNEL   ! -->  ! CHOICE INDEX FOR KERNEL                        !
! ! KPAR      ! <--  ! PARTICLE  TYPE                                 !
! ! KPARMOB   ! <--  ! MOVING WALL OR EDGE PARTICLE TYPE              !
! ! KSUITE    ! -->  ! LOGICAL FOR CALCULATION CONTINUED              !
! ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! LM        ! <--  ! MIXING LENGTH                                  !
! ! LNG       ! -->  ! CHOICE INDEX FOR LANGUAGE                      !
! ! MASS      ! <--  ! PARTICLE MASS                                  !
! ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
! ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NPARB     ! <--  ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     ! <--  ! NUMBER OF FLUID PARTICLES                      !
! ! NPART     ! <--  ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NUT       ! <--  ! EDDY VISCOSITY                                 !
! ! P         ! <--  ! PRESSURE                                       !
! ! PI        ! <--  ! ARCHIMEDE'S NUMBER                             !
! ! PRIV      ! <--  ! PRIVATE PRINTOUT VARIABLE FOR THE USER         !
! ! R0        ! <--  ! WALL ACTION DISTANCE                           !
! ! RHO       ! <--  ! DENSITY                                        !
! ! RHO0      ! -->  ! REFERENCE DENSITIES                            !
! ! S         ! <--  ! RATE OF STRAIN                                 !
! ! SUPP      ! <--  ! KERNEL SUPPORT                                 !
! ! TEMPS     ! <--  ! PHYSICAL TIME                                  !
! ! THETA     ! <--  ! ANGLE BETWEEN WALL NORMAL VECTOR AND X-AXIS    !
! ! TKE       ! <--  ! TURBULENT KINETIC ENERGY                       !
! ! VITC0     ! -->  ! SPEED OF SOUND                                 !
! ! VX, VZ    ! <--  ! VELOCITY COMPONENTS                            !
! ! X, Z      ! <--  ! PARTICLE POSITION                              !
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
! FONCTION : effectue les initialisations
! FUNCTION : achieved all initializations
!
! PROGRAMMES APPELANT : SPARTACUS2D
! CALLED BY
!
! PROGRAMMES APPELES  : LECINIT, LECSUIT, MELANGE
! CALLED PROGRAMS
!
!----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER NPMAX    , NPARF, NPART, NPARB
      INTEGER NFLUIDMAX, NBMAX, KTURB, KKERNEL
      INTEGER I        , CT
      INTEGER LNG      , LU
      COMMON/INFO/LNG,LU
!
      LOGICAL KSUITE
!
      DOUBLE PRECISION ETA2   , CR   , GRAV
      DOUBLE PRECISION H      , DR   , TEMPS
      DOUBLE PRECISION R0     , HDR  , DT
      DOUBLE PRECISION DEB    , SUPP , CK1
      DOUBLE PRECISION CMU    , VITC0, DELTA
      DOUBLE PRECISION DELTADR, PI   , KAPPA
!
      INTEGER KFLUID(NPMAX), KPAR   (NPMAX)
      INTEGER KENT  (NPMAX), KPARMOB(NPMAX)
!
      DOUBLE PRECISION X     (NPMAX), Z    (NPMAX), RHO   (NPMAX)
      DOUBLE PRECISION P     (NPMAX), VX   (NPMAX), VZ    (NPMAX)
      DOUBLE PRECISION MASS  (NPMAX), FMOT (NPMAX), RHO0  (NFLUIDMAX)
      DOUBLE PRECISION LM    (NPMAX), EPS  (NPMAX), TKE   (NPMAX)
      DOUBLE PRECISION NUT   (NPMAX), PRIV (NPMAX), S     (NPMAX)
      DOUBLE PRECISION THETA (NBMAX), CTHETA(NBMAX),STHETA(NBMAX)
!
! Initialisations
!================
! Initializations
!================
!
! Constantes
!-----------
! Constants
! ---------
!
      PI    = 3.141592653589793D0
      KAPPA = 0.41D0
      CR    = 8.50D0
      CMU   = 0.09D0
      CK1   = 0.16D0
      GRAV  = 9.807D0
!
      IF (.NOT.KSUITE) THEN
!
! Nouveau calcul
!---------------
! New calculation
!----------------
!
        CALL LECINIT
!
     &  (NPARB , NPARF  , NPART, NPMAX, NBMAX, KENT,
     &   KFLUID, KPARMOB, KPAR , DR   , THETA, X   ,
     &   Z                                         )
!
        IF (KTURB.NE.0) THEN
!
          CALL MELANGE
!
     &    (NPART, NPMAX, DR, LM, X, Z, DELTA, KPAR)
!
        ENDIF
!
        TEMPS=0.D0
        CT   =0
!
        DO 447 I=1,NPART
          VX  (I) = 0.D0
          VZ  (I) = 0.D0
          RHO (I) = RHO0(KFLUID(I))
          P   (I) = 0.D0
          MASS(I) = RHO(I)*DR*DR
          PRIV(I) = 0.D0
          S   (I) = 0.D0
          IF (KPAR(I).NE.3.AND.KTURB.NE.0) THEN
            TKE (I) = (0.002D0*VITC0)**2
            EPS (I) = CK1*SQRT(TKE(I)**3)/LM(I)
            NUT (I) = CMU*TKE(I)**2/EPS(I)
          ELSE
            TKE (I) = 0.D0
            EPS (I) = 0.D0
            NUT (I) = 0.D0
          ENDIF
 447    CONTINUE
!
      ELSE
!
! Suite de calcul
!----------------
! Calculation continued
!----------------------
!
        CALL LECSUIT
!
     &  (NPARB, NPARF , NPART, NPMAX  , NBMAX, CT   ,
     &   KENT , KFLUID, KPAR , KPARMOB, DR   , EPS  ,
     &   MASS , NUT   , P    , RHO    , TEMPS, THETA,
     &   TKE  , VX    , VZ   , X      , Z           )
!
        IF (KTURB.EQ.1.OR.KTURB.EQ.2) THEN
!
          CALL MELANGE
!
     &    (NPART, NPMAX, DR, LM, X, Z, DELTA, KPAR)
!
          DO 647 I=1,NPARF+NPARB
              TKE (I) = MAX(TKE(I),1.0D-5)
              EPS (I) = MAX(EPS(I),1.0D-5)
 647      CONTINUE
!
        ELSE IF (KTURB.EQ.3) THEN
!
          DO 547 I=1,NPARF+NPARB
              TKE (I) = MAX(TKE(I),1.0D-5)
              EPS (I) = MAX(EPS(I),1.0D-5)
 547      CONTINUE
!
        ENDIF
!
      ENDIF
!
! Parametres divers
!------------------
! Miscellaneous parameters
! -----------------------
!
      H    = DR*HDR
      ETA2 = 0.01D0*H*H
      R0   = DR
      DELTA= DR*DELTADR
      DT   = 0.4D0*H/VITC0
      IF      (KKERNEL.EQ.1) THEN
        SUPP = 2.D0
      ELSE IF (KKERNEL.EQ.2) THEN
        SUPP = 2.5D0
      ELSE IF (KKERNEL.EQ.3) THEN
        SUPP = 3.D0
      ENDIF
!
      DEB  = 0.D0
      DO 448 I=1,NPART
        FMOT(I) = 0.D0
 448  CONTINUE
!
      DO 449 I=1,NPARB
        CTHETA(I)=COS(THETA(I)*PI/180.D0)
        STHETA(I)=SIN(THETA(I)*PI/180.D0)
 449  CONTINUE
!
      IF (LNG.EQ.1) THEN
        PRINT*,'Nombre de particules'
        PRINT*,'===================='
        PRINT900,NPARF            ,'particules fluides'
        PRINT900,NPARB            ,'particules de bord'
        PRINT900,NPART-NPARF-NPARB,'particules fictives'
        PRINT900,NPART            ,'particules'
      ELSEIF (LNG.EQ.2) THEN
        PRINT*,'Particle number'
        PRINT*,'==============='
        PRINT900,NPARF            ,'fluid  particles'
        PRINT900,NPARB            ,'edge   particles'
        PRINT900,NPART-NPARF-NPARB,'mirror particles'
        PRINT900,NPART            ,'       particles'
      ENDIF
!
 900  FORMAT('    ',I6,' ',A)
!
      RETURN
      END