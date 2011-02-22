C                        ********************
                          SUBROUTINE INITIAL
C                        ********************
C
     .  (NBMAX, NFLUIDMAX, NPARB  , NPARF  , NPART, NPMAX ,
     .   CT   , KENT     , KFLUID , KKERNEL, KPAR , KSUITE,
     .   KTURB, CK1      , KPARMOB, CMU    , CR   , CTHETA, 
     .   DEB  , DELTA    , DELTADR, DR     , DT   , EPS   , 
     .   ETA2 , FMOT     , GRAV   , H      , HDR  , KAPPA , 
     .   LM   , MASS     , NUT    , P      , PI   , PRIV  , 
     .   R0   , RHO      , RHO0   , STHETA , SUPP , TEMPS ,
     .   THETA, TKE      , VITC0  , VX     , VZ   , X     , 
     .   Z    , S                                         )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! CK1, CMU  ! <--  ! K-EPSILON MODEL COEFFICIENTS                   !
C ! CR        ! <--  ! COEFFICIENT RELATIVE TO A ROUGH LOG LAW        !
C ! CT        ! <--  ! TECPLOT CURRENT ZONE NUMBER                    !
C ! CTHETA,                                                           !
C ! STHETA    ! <--  ! COMPONENTS OF WALL NORMAL VECTORS              !
C ! DEB       ! <--  ! COMPUTED MEAN BULK VELOCITY                    !
C ! DELTA     ! <--  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
C ! DELTADR   ! -->  ! RATIO DELTA/DR                                 !
C ! DR        ! <--  ! INITIAL INTERPARTICLE SPACING                  !
C ! EPS       ! <--  ! DISSIPATION RATE                               !
C ! DT        ! <--  ! TIME STEP                                      !
C ! ETA2      ! <--  ! CORRECTION TERM FOR DIFFUSION                  !
C ! FMOT      ! <--  ! AXIAL DRIVING FORCE                            !
C ! GRAV      ! <--  ! GRAVITY                                        !
C ! H         ! <--  ! SMOOTHING LENGTH                               !
C ! HDR       ! -->  ! RATIO H/DR                                     !
C ! KAPPA     ! <--  ! VON KARMAN CONSTANT                            !
C ! KENT      ! <--  ! CONDITION TYPE AT FLUID BOUNDARIES             !
C ! KFLUID    ! <--  ! FLUID TYPE                                     !
C ! KKERNEL   ! -->  ! CHOICE INDEX FOR KERNEL                        !
C ! KPAR      ! <--  ! PARTICLE  TYPE                                 !
C ! KPARMOB   ! <--  ! MOVING WALL OR EDGE PARTICLE TYPE              !
C ! KSUITE    ! -->  ! LOGICAL FOR CALCULATION CONTINUED              !
C ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! LM        ! <--  ! MIXING LENGTH                                  !
C ! LNG       ! -->  ! CHOICE INDEX FOR LANGUAGE                      !
C ! MASS      ! <--  ! PARTICLE MASS                                  !
C ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
C ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NPARB     ! <--  ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     ! <--  ! NUMBER OF FLUID PARTICLES                      !
C ! NPART     ! <--  ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NUT       ! <--  ! EDDY VISCOSITY                                 !
C ! P         ! <--  ! PRESSURE                                       !
C ! PI        ! <--  ! ARCHIMEDE'S NUMBER                             !
C ! PRIV      ! <--  ! PRIVATE PRINTOUT VARIABLE FOR THE USER         !
C ! R0        ! <--  ! WALL ACTION DISTANCE                           !
C ! RHO       ! <--  ! DENSITY                                        !
C ! RHO0      ! -->  ! REFERENCE DENSITIES                            !
C ! S         ! <--  ! RATE OF STRAIN                                 !
C ! SUPP      ! <--  ! KERNEL SUPPORT                                 !
C ! TEMPS     ! <--  ! PHYSICAL TIME                                  !
C ! THETA     ! <--  ! ANGLE BETWEEN WALL NORMAL VECTOR AND X-AXIS    !
C ! TKE       ! <--  ! TURBULENT KINETIC ENERGY                       !
C ! VITC0     ! -->  ! SPEED OF SOUND                                 !
C ! VX, VZ    ! <--  ! VELOCITY COMPONENTS                            ! 
C ! X, Z      ! <--  ! PARTICLE POSITION                              !
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
C FONCTION : effectue les initialisations
C FUNCTION : achieved all initializations
C
C PROGRAMMES APPELANT : SPARTACUS2D
C CALLED BY         
C
C PROGRAMMES APPELES  : LECINIT, LECSUIT, MELANGE
C CALLED PROGRAMS     
C
C----------------------------------------------------------------------
C
C Variables
C==========
C
      IMPLICIT NONE
C
      INTEGER NPMAX    , NPARF, NPART, NPARB
      INTEGER NFLUIDMAX, NBMAX, KTURB, KKERNEL
      INTEGER I        , CT
      INTEGER LNG      , LU
      COMMON/INFO/LNG,LU      
C
      LOGICAL KSUITE
C
      DOUBLE PRECISION ETA2   , CR   , GRAV
      DOUBLE PRECISION H      , DR   , TEMPS
      DOUBLE PRECISION R0     , HDR  , DT
      DOUBLE PRECISION DEB    , SUPP , CK1
      DOUBLE PRECISION CMU    , VITC0, DELTA
      DOUBLE PRECISION DELTADR, PI   , KAPPA
C
      INTEGER KFLUID(NPMAX), KPAR   (NPMAX)
      INTEGER KENT  (NPMAX), KPARMOB(NPMAX)
C
      DOUBLE PRECISION X     (NPMAX), Z    (NPMAX), RHO   (NPMAX)
      DOUBLE PRECISION P     (NPMAX), VX   (NPMAX), VZ    (NPMAX)
      DOUBLE PRECISION MASS  (NPMAX), FMOT (NPMAX), RHO0  (NFLUIDMAX)
      DOUBLE PRECISION LM    (NPMAX), EPS  (NPMAX), TKE   (NPMAX)
      DOUBLE PRECISION NUT   (NPMAX), PRIV (NPMAX), S     (NPMAX) 
      DOUBLE PRECISION THETA (NBMAX), CTHETA(NBMAX),STHETA(NBMAX)
C
C Initialisations
C================
C Initializations
C================
C
C Constantes
C-----------
C Constants
C ---------

      PI    = 3.141592653589793D0
      KAPPA = 0.41D0
      CR    = 8.50D0
      CMU   = 0.09D0
      CK1   = 0.16D0
      GRAV  = 9.807D0
C
      IF (.NOT.KSUITE) THEN
C
C Nouveau calcul
C---------------
C New calculation
C----------------
C
        CALL LECINIT
C     
     .  (NPARB , NPARF  , NPART, NPMAX, NBMAX, KENT, 
     .   KFLUID, KPARMOB, KPAR , DR   , THETA, X   , 
     .   Z                                         )
C
        IF (KTURB.NE.0) THEN
C
          CALL MELANGE
C
     .    (NPART, NPMAX, DR, LM, X, Z, DELTA, KPAR)
C
        ENDIF
C	
        TEMPS=0.D0
        CT   =0
C
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
C
      ELSE
C
C Suite de calcul
C----------------
C Calculation continued
C----------------------
C
        CALL LECSUIT
C
     .  (NPARB, NPARF , NPART, NPMAX  , NBMAX, CT   , 
     .   KENT , KFLUID, KPAR , KPARMOB, DR   , EPS  , 
     .   MASS , NUT   , P    , RHO    , TEMPS, THETA,  
     .   TKE  , VX    , VZ   , X      , Z           ) 
C
        IF (KTURB.EQ.1.OR.KTURB.EQ.2) THEN
C
          CALL MELANGE
C
     .    (NPART, NPMAX, DR, LM, X, Z, DELTA, KPAR)
C
          DO 647 I=1,NPARF+NPARB
              TKE (I) = MAX(TKE(I),1.0D-5)
              EPS (I) = MAX(EPS(I),1.0D-5)
 647      CONTINUE
C
        ELSE IF (KTURB.EQ.3) THEN
C
          DO 547 I=1,NPARF+NPARB
              TKE (I) = MAX(TKE(I),1.0D-5)
              EPS (I) = MAX(EPS(I),1.0D-5)              
 547      CONTINUE
C  
        ENDIF
C
      ENDIF
C
C Parametres divers
C------------------
C Miscellaneous parameters
C -----------------------
C
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
C
      DEB  = 0.D0
      DO 448 I=1,NPART
        FMOT(I) = 0.D0
 448  CONTINUE
C
      DO 449 I=1,NPARB
        CTHETA(I)=COS(THETA(I)*PI/180.D0)
        STHETA(I)=SIN(THETA(I)*PI/180.D0)
 449  CONTINUE
C     
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
C      
 900  FORMAT('    ',I6,' ',A)
C
      RETURN
      END
