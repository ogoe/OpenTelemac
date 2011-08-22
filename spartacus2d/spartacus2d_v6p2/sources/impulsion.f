!                        **********************
                          SUBROUTINE IMPULSION
!                        **********************
!
     &  (NBMAX , NLIEN    , NLIENMAX, NPARB  , NPARF, NPMAX ,
     &   NPART , NFLUIDMAX, ILIEN   , KAPPA  , KCPAR, KDEF  ,
     &   KFPAR , KFLUID   , KGAMMAX , KGAMMAZ, KGRAV, KMOT  ,
     &   KPAR  , KPRES    , KPROD   , KTURB  , KVISQ, AX    ,
     &   AZ    , CK1      , CMU     , CPAR   , DEB  , DEBREF,
     &   DELTA , DR       , DT      , EPS    , ETA2 , FMOT  ,
     &   FORCST, GAMMAX   , GAMMAZ  , GKERX  , GKERZ, GRAV  ,
     &   GVXX  , GVXZ     , GVZX    , GVZZ   , H    , LM    ,
     &   MASS  , NU0      , NUT     , P      , R0   , RAB   ,
     &   RHO   , S        , TKE     , USTAR  , VITC0, VX    ,
     &   VXAB  , VZ       , VZAB    , X      , XAB  , XMIN  ,
     &   ZAB   , Z                                          )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! AX, AZ    ! <--  ! PARTICLE ACCELERATION COMPONENTS               !
! ! CK1, CMU  ! -->  ! K-EPSILON MODEL COEFFICIENTS                   !
! ! CPAR      ! -->  ! WALL FORCE 1 COEFFICIENT                       !
! ! DEB       ! <--  ! COMPUTED MEAN BULK VELOCITY                    !
! ! DEBREF    ! -->  ! PRESCRIBED MEAN BULK VELOCITY                  !
! ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
! ! DIFF      ! -->  ! INTERMEDIATE FOR DIFFUSION TERM COMPUTATION    !
! ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
! ! DT        ! -->  ! TIME STEP                                      !
! ! EPS       ! <--> ! DISSIPATION RATE                               !
! ! ETA2      ! -->  ! CORRECTION TERM FOR DIFFUSION                  !
! ! FMOT      ! <--  ! AXIAL DRIVING FORCE                            !
! ! FORCST    ! -->  ! PRESCRIBED AXIAL DRIVING FORCE                 !
! ! FPARX,                                                            !
! ! FPARZ     ! <--  ! WALL FORCE COMPONENTS                          !
! ! FEXTX,                                                            !
! ! FEXTZ     ! <--  ! EXTERNAL FORCE COMPONENTS                      !
! ! FPRES     ! <--  ! MOMENTUM FLUX RELATIVE TO PRESSURE FORCES      !
! ! FVISQ     ! <--  ! MOMENTUM FLUX RELATIVE TO VISCOUS FORCES       !
! ! GAMMAX,                                                           !
! ! GAMMAZ    ! -->  ! DAMPING COEFFICIENTS                           !
! ! GKERX,                                                            !
! ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
! ! GRAV      ! -->  ! GRAVITY                                        !
! ! GVXX, GVXZ                                                        !
! ! GVZX, GVZZ! -->  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
! ! H         ! -->  ! SMOOTHING LENGTH                               !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! KAPPA     ! -->  ! VON KARMAN CONSTANT                            !
! ! KCPAR     ! -->  ! CHOICE INDEX FOR WALL MODELLING                !
! ! KDEF      ! -->  ! CHOICE INDEX FOR STRAIN MODEL                  !
! ! KFPAR     ! -->  ! CHOICE INDEX FOR WALL FORCES                   !
! ! KFLUID    ! -->  ! FLUID TYPE                                     !
! ! KGAMMAX,                                                          !
! ! KGAMMAZ   ! -->  ! LOGICAL INDEX FOR DAMPING                      !
! ! KGRAV     ! -->  ! LOGICAL RELATIVE TO THE GRAVITY                !
! ! KMOT      ! -->  ! CHOICE INDEX FOR THE FORCING TERM              !
! ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
! ! KPRES     ! -->  ! CHOICE INDEX FOR THE PRESSURE GRADIENT MODEL   !
! ! KPROD     ! -->  ! CHOICE INDEX FOR THE PRODUCTION MODEL          !
! ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! KVISQ     ! -->  ! CHOICE INDEX FOR THE VISCOUS MODEL             !
! ! LM        ! -->  ! MIXING LENGTH                                  !
! ! MASS      ! -->  ! PARTICLE MASS                                  !
! ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
! ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
! ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
! ! NUT       ! <--> ! EDDY VISCOSITY                                 !
! ! P         ! -->  ! PRESSURE                                       !
! ! R0        ! -->  ! WALL ACTION DISTANCE                           !
! ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
! ! RHO       ! -->  ! DENSITY                                        !
! ! S         ! -->  ! RATE OF STRAIN                                 !
! ! TKE       ! <--> ! TURBULENT KINETIC ENERGY                       !
! ! USTAR     ! -->  ! FRICTION VELOCITY                              !
! ! VITC0     ! -->  ! SPEED OF SOUND                                 !
! ! VX, VZ    ! -->  ! VELOCITY COMPONENTS                            !
! ! VXAB, VZAB! -->  ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
! ! X, Z      ! -->  ! PARTICLE POSITION                              !
! ! XAB, ZAB  ! -->  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
! ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
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
! FONCTION : calcule l'acceleration des particules
! FUNCTION : computes particle acceleration
!
! PROGRAMMES APPELANT : SPARTACUS2D
! CALLED BY
!
! PROGRAMMES APPELES  : FORCPAROIS, FORCEXT, VISCTURB,
! CALLED PROGRAMS       FLUXPRES  , FLUXVISQ
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
      INTEGER KVISQ, KPRES, KMOT
      INTEGER KCPAR, KFPAR, KTURB
      INTEGER KDEF , KPROD
      INTEGER I    , L    , J
      INTEGER NFLUIDMAX
!
      INTEGER NLIEN(NPMAX)
      INTEGER KPAR (NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION TEMPO , TEMPX , TEMPZ
      DOUBLE PRECISION GAMMAX, GAMMAZ
      DOUBLE PRECISION DEB   , DEBREF
      DOUBLE PRECISION GRAV  , XMIN  , DR
      DOUBLE PRECISION FORCST, ETA2  , R0
      DOUBLE PRECISION CPAR  , VITC0 , H
      DOUBLE PRECISION DT    , DELTA
      DOUBLE PRECISION CMU   , CK1
      DOUBLE PRECISION KAPPA
!
      INTEGER KFLUID (NPMAX)
!
      DOUBLE PRECISION AX   (NPMAX), AZ   (NPMAX), MASS  (NPMAX)
      DOUBLE PRECISION FEXTX(NPMAX), FEXTZ(NPMAX), VX    (NPMAX)
      DOUBLE PRECISION VZ   (NPMAX), FPARX(NPMAX), FPARZ (NPMAX)
      DOUBLE PRECISION FMOT (NPMAX), P    (NPMAX), RHO   (NPMAX)
      DOUBLE PRECISION X    (NPMAX), TKE  (NPMAX), EPS   (NPMAX)
      DOUBLE PRECISION NUT  (NPMAX), USTAR(NBMAX), LM    (NPMAX)
      DOUBLE PRECISION GVXX (NPMAX), GVXZ (NPMAX), S     (NPMAX)
      DOUBLE PRECISION GVZX (NPMAX), GVZZ (NPMAX), Z     (NPMAX)
!
      DOUBLE PRECISION NU0 (NFLUIDMAX)
!
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX), VZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION FVISQ(NPMAX,NLIENMAX), FPRES(NPMAX,NLIENMAX)
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX), DIFF (NPMAX,NLIENMAX)
!
      LOGICAL KGAMMAX, KGAMMAZ, KGRAV
!
! Equation de quantite de mouvement
!==================================
! Momentum equation
!==================
!
! Forces exterieures
!-------------------
! External forces
!----------------
!
! Forces de parois
!.................
! Wall forces
!............
!
      IF (KCPAR.NE.1) THEN
!
        CALL FORCPAROIS
!
     &  (NLIEN, NLIENMAX, NPARF, NPMAX, ILIEN, KFPAR,
     &   KPAR , CPAR    , FPARX, FPARZ, H    , R0   ,
     &   RAB  , VITC0   , XAB  , ZAB                )
!
      ENDIF
!
! Force motrice et gravite
!.........................
! Driving force and gravity
!..........................
!
      CALL FORCEXT
!
     &  (NPARF, NPMAX, NPARB , KCPAR, KGRAV, KMOT ,
     &   KTURB, DEB  , DEBREF, DR   , DT   , FEXTX,
     &   FEXTZ, FMOT , FORCST, FPARX, FPARZ, GRAV ,
     &   VX   , X    , XMIN                       )
!
      DO 111 I=1,NPARF
        AX(I)=FEXTX(I)
        AZ(I)=FEXTZ(I)
 111  CONTINUE
!
! Forces interieures
!-------------------
! Internal forces
!----------------
!
! Calcul de la viscosite turbulente
!..................................
! Eddy viscosity computation
!...........................
!
      IF (KTURB.NE.0) THEN
!
        CALL VISCTURB
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
      ENDIF
!
! Calcul des flux d'impulsion
!............................
! Momentum computation
!......................
!
      CALL FLUXPRES
!
     &(NLIEN, NLIENMAX, NPARF, NPMAX, ILIEN, KCPAR,
     & KPAR , KPRES   , KTURB, FPRES, P    , RHO  ,
     & TKE                                        )
!
      CALL FLUXVISQ
!
     &(NLIEN , NLIENMAX , NPARF, NPMAX, ILIEN,
     & KFLUID, KTURB    , KVISQ, DIFF , ETA2 ,
     & FVISQ , GKERX    , GKERZ, NU0  , NUT  ,
     & RAB   , RHO      , VXAB , VZAB , XAB  ,
     & ZAB   , NFLUIDMAX                     )
!
      IF (KVISQ.EQ.1) THEN
!
! Cas du modele visqueux de Monaghan
!...................................
! Monaghan's viscous model
!.........................
!
        DO 103 I=1,NPARF
          DO 523 L=1,NLIEN(I)
            J=ILIEN(I,L)
!
            TEMPO = FPRES(I,L)+FVISQ(I,L)
            TEMPX = TEMPO*GKERX(I,L)
            TEMPZ = TEMPO*GKERZ(I,L)
!
            AX(I) = AX(I)+MASS(J)*TEMPX
            AZ(I) = AZ(I)+MASS(J)*TEMPZ
!
            AX(J) = AX(J)-MASS(I)*TEMPX
            AZ(J) = AZ(J)-MASS(I)*TEMPZ
!
 523      CONTINUE
 103    CONTINUE
!
      ELSE
!
! Cas du modele visqueux de Morris
!.................................
! Morris' viscous term
!.....................
!
        DO 203 I=1,NPARF
          DO 223 L=1,NLIEN(I)
            J=ILIEN(I,L)
!
            TEMPX = FPRES(I,L)*GKERX(I,L)+FVISQ(I,L)*VXAB(I,L)
            TEMPZ = FPRES(I,L)*GKERZ(I,L)+FVISQ(I,L)*VZAB(I,L)
!
            AX(I) = AX(I)+MASS(J)*TEMPX
            AZ(I) = AZ(I)+MASS(J)*TEMPZ
!
            AX(J) = AX(J)-MASS(I)*TEMPX
            AZ(J) = AZ(J)-MASS(I)*TEMPZ
!
 223      CONTINUE
 203    CONTINUE
!
      ENDIF
!
! Termes de damping
!------------------
! Damping term
!-------------
!
! Horizontal
!...........
! Horizontal
!............
!
      IF (KGAMMAX) THEN
!
        DO 301 I=1,NPARF
          AX(I) = AX(I) - GAMMAX*VX(I)
 301    CONTINUE
!
      ENDIF
!
! Vertical
!.........
! Vertical
!.........
!
      IF (KGAMMAZ) THEN
!
        DO 302 I=1,NPARF
          AZ(I) = AZ(I) - GAMMAZ*VZ(I)
 302    CONTINUE
!
      ENDIF
!
      RETURN
      END