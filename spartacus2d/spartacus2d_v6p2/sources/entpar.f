!                        *******************
                          SUBROUTINE ENTPAR
!                        *******************
!
     &   (NFLUIDMAX, NPARF, NPART, NPMAX, IT     , KENT ,
     &    KFLUID   , KLIST, KPAR , KTURB, KPARMOB, CK1  ,
     &    CMU      , DR   , EPS  , LM   , MASS   , NUT  ,
     &    P        , RHO  , RHO0 , S    , TKE    , VITC0,
     &    VX       , VZ   , X    , Z                    )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! CK1, CMU  ! -->  ! K-EPSILON MODEL COEFFICIENTS                   !
! ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
! ! EPS       ! <--> ! DISSIPATION RATE                               !
! ! IT        ! -->  ! INDEX OF THE CURRENT TIME STEP                 !
! ! KENT      ! <--> ! CONDITION TYPE AT FLUID BOUNDARIES             !
! ! KFLUID    ! <--> ! FLUID TYPE                                     !
! ! KLIST     ! -->  ! LOGICAL FOR LISTING PRINTOUT                   !
! ! KPAR      ! <--> ! PARTICLE  TYPE                                 !
! ! KPARMOB   ! <-->  ! MOVING WALL OR EDGE PARTICLE TYPE             !
! ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! LM        ! <--> ! MIXING LENGTH                                  !
! ! MASS      ! <--> ! PARTICLE MASS                                  !
! ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NPARF     ! <--> ! NUMBER OF FLUID PARTICLES                      !
! ! NPART     ! <--> ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NUT       ! <--> ! EDDY VISCOSITY                                 !
! ! P         ! <--> ! PRESSURE                                       !
! ! RHO       ! <--> ! DENSITY                                        !
! ! RHO0      ! <--> ! REFERENCE DENSITIES                            !
! ! S         ! <--> ! RATE OF STRAIN                                 !
! ! TKE       ! <--> ! TURBULENT KINETIC ENERGY                       !
! ! VITC0     ! -->  ! SPEED OF SOUND                                 !
! ! VX, VZ    ! <--> ! VELOCITY COMPONENTS                            !
! ! X, Z      ! <--> ! PARTICLE POSITION                              !
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
! FONCTION : gere les entrees de particules en cas de profil de vitesse impose
! FUNCTION : deals with ingoing particles when velocity profile prescribed
!
! PROGRAMMES APPELANT : SPARTACUS2D
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
      INTEGER NPMAX, NPARF , NPART
      INTEGER NPARE, NPARET, NFLUIDMAX
      INTEGER I    , J     , IT
      INTEGER KTURB, LNG   , LU
      COMMON/INFO/LNG,LU
!
      LOGICAL KLIST
!
      DOUBLE PRECISION DR, VITC0, CMU, CK1
!
      INTEGER KPAR    (NPMAX), KENT(NPMAX), KFLUID(NPMAX)
      INTEGER KPARMOB (NPMAX)
!
      DOUBLE PRECISION X   (NPMAX), Z  (NPMAX), RHO(NPMAX)
      DOUBLE PRECISION VX  (NPMAX), VZ (NPMAX), P  (NPMAX)
      DOUBLE PRECISION MASS(NPMAX), TKE(NPMAX), S  (NPMAX)
      DOUBLE PRECISION EPS (NPMAX), NUT(NPMAX), LM (NPMAX)
      DOUBLE PRECISION RHO0(NFLUIDMAX)
!
      SAVE NPARET
!
! Particules entrantes
!=====================
! Ingoing particles
!==================
!
      IF (IT.EQ.1) THEN
        NPARET=0
      ENDIF
!
      NPARE=0
      DO 668 I=1,NPARF
        IF (KENT(I).EQ.1) THEN
!
! Definition du critere d'apparition de nouvelles particules
! (a modifier par l'utilisateur)
!-----------------------------------------------------------
! The user has to define a criterion relative to new particles
!-------------------------------------------------------------
!
          IF (X(I).GT.0.D0+2.D0*DR) THEN
!
!-----------------------------------------------------------
!
            KENT(I)=0
            NPARF=NPARF+1
            NPART=NPART+1
            DO 669 J=NPART,I+1,-1
              KENT   (J)=KENT   (J-1)
              KPAR   (J)=KPAR   (J-1)
              KFLUID (J)=KFLUID (J-1)
              KPARMOB(J)=KPARMOB(J-1)
              X      (J)=X      (J-1)
              Z      (J)=Z      (J-1)
              VX     (J)=VX     (J-1)
              VZ     (J)=VZ     (J-1)
              RHO    (J)=RHO    (J-1)
              P      (J)=P      (J-1)
              TKE    (J)=TKE    (J-1)
              EPS    (J)=EPS    (J-1)
              NUT    (J)=NUT    (J-1)
              S      (J)=S      (J-1)
              LM     (J)=LM     (J-1)
 669        CONTINUE
!
            KENT (I)=1
            KPAR (I)=0
!
! Definition des caracteristiques des nouvelles particules
! (a modifier par l'utilisateur)
!---------------------------------------------------------
! New particle characteristic definition
!---------------------------------------
!
            X     (I)=0.D0
            Z     (I)=Z(I+1)
            VX    (I)=1.D0
            VZ    (I)=0.D0
            KFLUID(I)=1
            RHO   (I)=RHO0(KFLUID(I))
            P     (I)=0.D0
            S     (I)=0.D0
            LM    (I)=DR
            IF (KTURB.NE.0) THEN
              TKE (I) = (0.002D0*VITC0)**2
              EPS (I) = CK1*SQRT(TKE(I)**3)/LM(I)
              NUT (I) = CMU*TKE(I)**2/EPS(I)
           ELSE
              TKE (I) = 0.D0
              EPS (I) = 0.D0
              NUT (I) = 0.D0
            ENDIF
!
!---------------------------------------------------------
!
            MASS  (I)=RHO(I)*DR*DR
            NPARE=NPARE+1
!
          ENDIF
!
        ENDIF
 668  CONTINUE
!
      NPARET=NPARET+NPARE
!
! Impressions
!============
!
      IF (KLIST) THEN
        IF (LNG.EQ.1) THEN
          PRINT*,NPARE,' particules entrees'
          PRINT*,'Nombre total de particules entrees : ',NPARET
          PRINT*,'Nombre de particules fluides       : ',NPARF
        ELSEIF (LNG.EQ.2) THEN
          PRINT*,NPARE,' ingoing particles'
          PRINT*,'Total number of ingoing particles : ',NPARET
          PRINT*,'Number of fluid particles         : ',NPARF
        ENDIF
      ENDIF
!
      IF (NPART.GT.NPMAX) THEN
        IF (LNG.EQ.1) THEN
          PRINT*,'Erreur : NPMAX trop petit !'
        ELSEIF (LNG.EQ.2) THEN
          PRINT*,'Error : NPMAX too tiny !'
        ENDIF
        STOP
      ENDIF
!
      RETURN
      END