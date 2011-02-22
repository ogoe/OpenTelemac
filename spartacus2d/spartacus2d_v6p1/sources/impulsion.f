C                        **********************
                          SUBROUTINE IMPULSION
C                        **********************
C    
     .  (NBMAX , NLIEN    , NLIENMAX, NPARB  , NPARF, NPMAX ,
     .   NPART , NFLUIDMAX, ILIEN   , KAPPA  , KCPAR, KDEF  , 
     .   KFPAR , KFLUID   , KGAMMAX , KGAMMAZ, KGRAV, KMOT  , 
     .   KPAR  , KPRES    , KPROD   , KTURB  , KVISQ, AX    ,
     .   AZ    , CK1      , CMU     , CPAR   , DEB  , DEBREF,
     .   DELTA , DR       , DT      , EPS    , ETA2 , FMOT  ,
     .   FORCST, GAMMAX   , GAMMAZ  , GKERX  , GKERZ, GRAV  ,
     .   GVXX  , GVXZ     , GVZX    , GVZZ   , H    , LM    ,
     .   MASS  , NU0      , NUT     , P      , R0   , RAB   , 
     .   RHO   , S        , TKE     , USTAR  , VITC0, VX    ,
     .   VXAB  , VZ       , VZAB    , X      , XAB  , XMIN  , 
     .   ZAB   , Z                                          )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! AX, AZ    ! <--  ! PARTICLE ACCELERATION COMPONENTS               !
C ! CK1, CMU  ! -->  ! K-EPSILON MODEL COEFFICIENTS                   !
C ! CPAR      ! -->  ! WALL FORCE 1 COEFFICIENT                       !
C ! DEB       ! <--  ! COMPUTED MEAN BULK VELOCITY                    !
C ! DEBREF    ! -->  ! PRESCRIBED MEAN BULK VELOCITY                  !
C ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
C ! DIFF      ! -->  ! INTERMEDIATE FOR DIFFUSION TERM COMPUTATION    !
C ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
C ! DT        ! -->  ! TIME STEP                                      !
C ! EPS       ! <--> ! DISSIPATION RATE                               !
C ! ETA2      ! -->  ! CORRECTION TERM FOR DIFFUSION                  !
C ! FMOT      ! <--  ! AXIAL DRIVING FORCE                            !
C ! FORCST    ! -->  ! PRESCRIBED AXIAL DRIVING FORCE                 !
C ! FPARX,                                                            !
C ! FPARZ     ! <--  ! WALL FORCE COMPONENTS                          !
C ! FEXTX,                                                            !
C ! FEXTZ     ! <--  ! EXTERNAL FORCE COMPONENTS                      !
C ! FPRES     ! <--  ! MOMENTUM FLUX RELATIVE TO PRESSURE FORCES      !
C ! FVISQ     ! <--  ! MOMENTUM FLUX RELATIVE TO VISCOUS FORCES       !
C ! GAMMAX,                                                           !
C ! GAMMAZ    ! -->  ! DAMPING COEFFICIENTS                           !
C ! GKERX,                                                            !
C ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
C ! GRAV      ! -->  ! GRAVITY                                        !
C ! GVXX, GVXZ                                                        !
C ! GVZX, GVZZ! -->  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
C ! H         ! -->  ! SMOOTHING LENGTH                               !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! KAPPA     ! -->  ! VON KARMAN CONSTANT                            !
C ! KCPAR     ! -->  ! CHOICE INDEX FOR WALL MODELLING                !
C ! KDEF      ! -->  ! CHOICE INDEX FOR STRAIN MODEL                  !
C ! KFPAR     ! -->  ! CHOICE INDEX FOR WALL FORCES                   !
C ! KFLUID    ! -->  ! FLUID TYPE                                     !
C ! KGAMMAX,                                                          !
C ! KGAMMAZ   ! -->  ! LOGICAL INDEX FOR DAMPING                      !
C ! KGRAV     ! -->  ! LOGICAL RELATIVE TO THE GRAVITY                !
C ! KMOT      ! -->  ! CHOICE INDEX FOR THE FORCING TERM              !
C ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
C ! KPRES     ! -->  ! CHOICE INDEX FOR THE PRESSURE GRADIENT MODEL   !
C ! KPROD     ! -->  ! CHOICE INDEX FOR THE PRODUCTION MODEL          !
C ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! KVISQ     ! -->  ! CHOICE INDEX FOR THE VISCOUS MODEL             !
C ! LM        ! -->  ! MIXING LENGTH                                  !
C ! MASS      ! -->  ! PARTICLE MASS                                  !
C ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
C ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
C ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
C ! NUT       ! <--> ! EDDY VISCOSITY                                 !
C ! P         ! -->  ! PRESSURE                                       !
C ! R0        ! -->  ! WALL ACTION DISTANCE                           !
C ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
C ! RHO       ! -->  ! DENSITY                                        !
C ! S         ! -->  ! RATE OF STRAIN                                 !
C ! TKE       ! <--> ! TURBULENT KINETIC ENERGY                       !
C ! USTAR     ! -->  ! FRICTION VELOCITY                              !
C ! VITC0     ! -->  ! SPEED OF SOUND                                 !
C ! VX, VZ    ! -->  ! VELOCITY COMPONENTS                            ! 
C ! VXAB, VZAB! -->  ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
C ! X, Z      ! -->  ! PARTICLE POSITION                              !
C ! XAB, ZAB  ! -->  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
C ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
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
C FONCTION : calcule l'acceleration des particules
C FUNCTION : computes particle acceleration
C
C PROGRAMMES APPELANT : SPARTACUS2D
C CALLED BY  
C
C PROGRAMMES APPELES  : FORCPAROIS, FORCEXT, VISCTURB,
C CALLED PROGRAMS       FLUXPRES  , FLUXVISQ
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
      INTEGER KVISQ, KPRES, KMOT
      INTEGER KCPAR, KFPAR, KTURB
      INTEGER KDEF , KPROD
      INTEGER I    , L    , J
      INTEGER NFLUIDMAX
C
      INTEGER NLIEN(NPMAX)
      INTEGER KPAR (NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C     
      DOUBLE PRECISION TEMPO , TEMPX , TEMPZ 
      DOUBLE PRECISION GAMMAX, GAMMAZ
      DOUBLE PRECISION DEB   , DEBREF
      DOUBLE PRECISION GRAV  , XMIN  , DR
      DOUBLE PRECISION FORCST, ETA2  , R0
      DOUBLE PRECISION CPAR  , VITC0 , H
      DOUBLE PRECISION DT    , DELTA 
      DOUBLE PRECISION CMU   , CK1
      DOUBLE PRECISION KAPPA
C     
      INTEGER KFLUID (NPMAX)
C
      DOUBLE PRECISION AX   (NPMAX), AZ   (NPMAX), MASS  (NPMAX)
      DOUBLE PRECISION FEXTX(NPMAX), FEXTZ(NPMAX), VX    (NPMAX)
      DOUBLE PRECISION VZ   (NPMAX), FPARX(NPMAX), FPARZ (NPMAX)
      DOUBLE PRECISION FMOT (NPMAX), P    (NPMAX), RHO   (NPMAX)
      DOUBLE PRECISION X    (NPMAX), TKE  (NPMAX), EPS   (NPMAX)
      DOUBLE PRECISION NUT  (NPMAX), USTAR(NBMAX), LM    (NPMAX)
      DOUBLE PRECISION GVXX (NPMAX), GVXZ (NPMAX), S     (NPMAX)
      DOUBLE PRECISION GVZX (NPMAX), GVZZ (NPMAX), Z     (NPMAX)
C      
      DOUBLE PRECISION NU0 (NFLUIDMAX)     
C
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX), VZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION FVISQ(NPMAX,NLIENMAX), FPRES(NPMAX,NLIENMAX)
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX), DIFF (NPMAX,NLIENMAX)
C
      LOGICAL KGAMMAX, KGAMMAZ, KGRAV
C
C Equation de quantite de mouvement
C==================================
C Momentum equation
C==================
C
C Forces exterieures
C-------------------
C External forces
C----------------
C
C Forces de parois
C.................
C Wall forces
C............
C
      IF (KCPAR.NE.1) THEN

        CALL FORCPAROIS
C
     .  (NLIEN, NLIENMAX, NPARF, NPMAX, ILIEN, KFPAR,
     .   KPAR , CPAR    , FPARX, FPARZ, H    , R0   ,
     .   RAB  , VITC0   , XAB  , ZAB                )
C
      ENDIF
C
C Force motrice et gravite
C.........................
C Driving force and gravity
C..........................
C
      CALL FORCEXT
C
     .  (NPARF, NPMAX, NPARB , KCPAR, KGRAV, KMOT , 
     .   KTURB, DEB  , DEBREF, DR   , DT   , FEXTX, 
     .   FEXTZ, FMOT , FORCST, FPARX, FPARZ, GRAV , 
     .   VX   , X    , XMIN                       )
C
      DO 111 I=1,NPARF
        AX(I)=FEXTX(I)
        AZ(I)=FEXTZ(I)
 111  CONTINUE
C
C Forces interieures
C-------------------
C Internal forces
C----------------
C
C Calcul de la viscosite turbulente
C..................................
C Eddy viscosity computation
C...........................
C
      IF (KTURB.NE.0) THEN
C
        CALL VISCTURB
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
      ENDIF
C
C Calcul des flux d'impulsion
C............................
C Momentum computation
C......................
C
      CALL FLUXPRES
C
     .(NLIEN, NLIENMAX, NPARF, NPMAX, ILIEN, KCPAR,
     . KPAR , KPRES   , KTURB, FPRES, P    , RHO  ,
     . TKE                                        )
C
      CALL FLUXVISQ
C
     .(NLIEN , NLIENMAX , NPARF, NPMAX, ILIEN, 
     . KFLUID, KTURB    , KVISQ, DIFF , ETA2 ,
     . FVISQ , GKERX    , GKERZ, NU0  , NUT  ,
     . RAB   , RHO      , VXAB , VZAB , XAB  , 
     . ZAB   , NFLUIDMAX                     )
C
      IF (KVISQ.EQ.1) THEN
C
C Cas du modele visqueux de Monaghan
C...................................
C Monaghan's viscous model
C.........................
C
        DO 103 I=1,NPARF
          DO 523 L=1,NLIEN(I)
            J=ILIEN(I,L)
C
            TEMPO = FPRES(I,L)+FVISQ(I,L)
            TEMPX = TEMPO*GKERX(I,L)
            TEMPZ = TEMPO*GKERZ(I,L)
C
            AX(I) = AX(I)+MASS(J)*TEMPX
            AZ(I) = AZ(I)+MASS(J)*TEMPZ
C
            AX(J) = AX(J)-MASS(I)*TEMPX
            AZ(J) = AZ(J)-MASS(I)*TEMPZ
C
 523      CONTINUE
 103    CONTINUE
C
      ELSE
C
C Cas du modele visqueux de Morris
C.................................
C Morris' viscous term
C.....................
C
        DO 203 I=1,NPARF
          DO 223 L=1,NLIEN(I)
            J=ILIEN(I,L)
C
            TEMPX = FPRES(I,L)*GKERX(I,L)+FVISQ(I,L)*VXAB(I,L)
            TEMPZ = FPRES(I,L)*GKERZ(I,L)+FVISQ(I,L)*VZAB(I,L)
C
            AX(I) = AX(I)+MASS(J)*TEMPX
            AZ(I) = AZ(I)+MASS(J)*TEMPZ
C
            AX(J) = AX(J)-MASS(I)*TEMPX
            AZ(J) = AZ(J)-MASS(I)*TEMPZ
C
 223      CONTINUE
 203    CONTINUE
C
      ENDIF
C
C Termes de damping
C------------------
C Damping term
C-------------
C
C Horizontal
C...........
C Horizontal
C............
C
      IF (KGAMMAX) THEN
C
        DO 301 I=1,NPARF
          AX(I) = AX(I) - GAMMAX*VX(I)
 301    CONTINUE
C
      ENDIF
C
C Vertical
C.........
C Vertical
C.........
C
      IF (KGAMMAZ) THEN
C
        DO 302 I=1,NPARF
          AZ(I) = AZ(I) - GAMMAZ*VZ(I)
 302    CONTINUE
C
      ENDIF
C
      RETURN
      END
