C                        *******************
                          SUBROUTINE ENTPAR
C                        *******************
C
     .   (NFLUIDMAX, NPARF, NPART, NPMAX, IT     , KENT ,
     .    KFLUID   , KLIST, KPAR , KTURB, KPARMOB, CK1  , 
     .    CMU      , DR   , EPS  , LM   , MASS   , NUT  , 
     .    P        , RHO  , RHO0 , S    , TKE    , VITC0,
     .    VX       , VZ   , X    , Z                    )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! CK1, CMU  ! -->  ! K-EPSILON MODEL COEFFICIENTS                   !
C ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
C ! EPS       ! <--> ! DISSIPATION RATE                               !
C ! IT        ! -->  ! INDEX OF THE CURRENT TIME STEP                 !
C ! KENT      ! <--> ! CONDITION TYPE AT FLUID BOUNDARIES             !
C ! KFLUID    ! <--> ! FLUID TYPE                                     !
C ! KLIST     ! -->  ! LOGICAL FOR LISTING PRINTOUT                   !
C ! KPAR      ! <--> ! PARTICLE  TYPE                                 !
C ! KPARMOB   ! <-->  ! MOVING WALL OR EDGE PARTICLE TYPE             !
C ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! LM        ! <--> ! MIXING LENGTH                                  !
C ! MASS      ! <--> ! PARTICLE MASS                                  !
C ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NPARF     ! <--> ! NUMBER OF FLUID PARTICLES                      !
C ! NPART     ! <--> ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NUT       ! <--> ! EDDY VISCOSITY                                 !
C ! P         ! <--> ! PRESSURE                                       !
C ! RHO       ! <--> ! DENSITY                                        !
C ! RHO0      ! <--> ! REFERENCE DENSITIES                            !
C ! S         ! <--> ! RATE OF STRAIN                                 !
C ! TKE       ! <--> ! TURBULENT KINETIC ENERGY                       !
C ! VITC0     ! -->  ! SPEED OF SOUND                                 !
C ! VX, VZ    ! <--> ! VELOCITY COMPONENTS                            ! 
C ! X, Z      ! <--> ! PARTICLE POSITION                              !
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
C FONCTION : gere les entrees de particules en cas de profil de vitesse impose
C FUNCTION : deals with ingoing particles when velocity profile prescribed
C
C PROGRAMMES APPELANT : SPARTACUS2D
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
      INTEGER NPMAX, NPARF , NPART
      INTEGER NPARE, NPARET, NFLUIDMAX
      INTEGER I    , J     , IT
      INTEGER KTURB, LNG   , LU
      COMMON/INFO/LNG,LU      
C
      LOGICAL KLIST
C
      DOUBLE PRECISION DR, VITC0, CMU, CK1
C 
      INTEGER KPAR    (NPMAX), KENT(NPMAX), KFLUID(NPMAX)
      INTEGER KPARMOB (NPMAX)
C
      DOUBLE PRECISION X   (NPMAX), Z  (NPMAX), RHO(NPMAX)
      DOUBLE PRECISION VX  (NPMAX), VZ (NPMAX), P  (NPMAX)
      DOUBLE PRECISION MASS(NPMAX), TKE(NPMAX), S  (NPMAX)
      DOUBLE PRECISION EPS (NPMAX), NUT(NPMAX), LM (NPMAX)
      DOUBLE PRECISION RHO0(NFLUIDMAX)
C     
      SAVE NPARET
C     
C Particules entrantes
C=====================
C Ingoing particles
C==================
C     
      IF (IT.EQ.1) THEN
        NPARET=0
      ENDIF
C     
      NPARE=0
      DO 668 I=1,NPARF
        IF (KENT(I).EQ.1) THEN
C
C Definition du critere d'apparition de nouvelles particules
C (a modifier par l'utilisateur)
C-----------------------------------------------------------
C The user has to define a criterion relative to new particles
C-------------------------------------------------------------
C
          IF (X(I).GT.0.D0+2.D0*DR) THEN
C
C-----------------------------------------------------------
C
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
C
            KENT (I)=1
            KPAR (I)=0
C
C Definition des caracteristiques des nouvelles particules
C (a modifier par l'utilisateur)
C---------------------------------------------------------
C New particle characteristic definition
C---------------------------------------
C
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
C
C---------------------------------------------------------
C
            MASS  (I)=RHO(I)*DR*DR
            NPARE=NPARE+1
C
          ENDIF
C
        ENDIF
 668  CONTINUE
C     
      NPARET=NPARET+NPARE
C     
C Impressions
C============
C     
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
C     
      IF (NPART.GT.NPMAX) THEN
        IF (LNG.EQ.1) THEN
          PRINT*,'Erreur : NPMAX trop petit !'
        ELSEIF (LNG.EQ.2) THEN
          PRINT*,'Error : NPMAX too tiny !'
        ENDIF
        STOP
      ENDIF
C     
      RETURN
      END
