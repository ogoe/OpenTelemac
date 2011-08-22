!                        *******************
                          SUBROUTINE SORPAR
!                        *******************
!
     &   (NPARF, NPART, NPMAX  , IT  , KENT, KFLUID,
     &    KLIST, KPAR , KPARMOB, KPER, EPS , LM    ,
     &    MASS , NUT  , P      , RHO , S   , TKE   ,
     &    VX   , VZ   , X      , XMIN, XMAX, Z     ,
     &    ZMIN , ZMAX                              )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! EPS       ! <--> ! DISSIPATION RATE                               !
! ! IT        ! -->  ! INDEX OF THE CURRENT TIME STEP                 !
! ! KENT      ! <--> ! CONDITION TYPE AT FLUID BOUNDARIES             !
! ! KFLUID    ! <--> ! FLUID TYPE                                     !
! ! KLIST     ! -->  ! LOGICAL FOR LISTING PRINTOUT                   !
! ! KPAR      ! <--> ! PARTICLE  TYPE                                 !
! ! KPARMOB   ! <--> ! MOVING WALL OR EDGE PARTICLE TYPE              !
! ! KPER      ! -->  ! LOGICAL INDEX FOR PERIODICITY                  !
! ! LM        ! <--> ! MIXING LENGTH                                  !
! ! MASS      ! <--> ! PARTICLE MASS                                  !
! ! NPARF     ! <--> ! NUMBER OF FLUID PARTICLES                      !
! ! NPART     ! <--> ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NUT       ! <--> ! EDDY VISCOSITY                                 !
! ! P         ! <--> ! PRESSURE                                       !
! ! RHO       ! <--> ! DENSITY                                        !
! ! S         ! <--> ! RATE OF STRAIN                                 !
! ! TKE       ! <--> ! TURBULENT KINETIC ENERGY                       !
! ! VX, VZ    ! <--> ! VELOCITY COMPONENTS                            !
! ! X, Z      ! <--> ! PARTICLE POSITION                              !
! ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
! ! ZMIN, ZMAX! -->  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
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
! FONCTION : gere les sorties de particules
! FUNCTION : deals with outgoing particles
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
      INTEGER NPMAX , NPARF, NPART, NPARS
      INTEGER NPARST, IT   , I    , J
      INTEGER LNG   , LU
      COMMON/INFO/LNG,LU
!
      LOGICAL KLIST, KPER
!
      DOUBLE PRECISION XMIN, ZMIN, XMAX, ZMAX
!
      INTEGER KPAR   (NPMAX), KENT(NPMAX), KFLUID(NPMAX)
      INTEGER KPARMOB(NPMAX)
!
!
      DOUBLE PRECISION X   (NPMAX), Z  (NPMAX), RHO(NPMAX)
      DOUBLE PRECISION VX  (NPMAX), VZ (NPMAX), P  (NPMAX)
      DOUBLE PRECISION MASS(NPMAX), TKE(NPMAX), EPS(NPMAX)
      DOUBLE PRECISION NUT (NPMAX), S  (NPMAX), LM (NPMAX)
!
      SAVE NPARST
!
! Particules sortantes
!=====================
! Outgoing particles
!===================
!
      IF (IT.EQ.1) THEN
        NPARST=0
      ENDIF
!
      NPARS=0
      DO 666 I=1,NPARF
!
        IF ((.NOT.KPER
     &      .AND.(X(I).GT.XMAX.OR.X(I).LT.XMIN))
     &      .OR.  Z(I).GT.ZMAX.OR.Z(I).LT.ZMIN) THEN
!
          NPARS=NPARS+1
          DO 667 J=I,NPART-1
            KENT   (J)=KENT   (J+1)
            KPAR   (J)=KPAR   (J+1)
            KFLUID (J)=KFLUID (J+1)
            KPARMOB(J)=KPARMOB(J+1)
            X      (J)=X      (J+1)
            Z      (J)=Z      (J+1)
            VX     (J)=VX     (J+1)
            VZ     (J)=VZ     (J+1)
            RHO    (J)=RHO    (J+1)
            P      (J)=P      (J+1)
            MASS   (J)=MASS   (J+1)
            TKE    (J)=TKE    (J+1)
            EPS    (J)=EPS    (J+1)
            NUT    (J)=NUT    (J+1)
            S      (J)=S      (J+1)
            LM     (J)=LM     (J+1)
 667      CONTINUE
          NPARF=NPARF-1
          NPART=NPART-1
!
        ENDIF
!
 666  CONTINUE
!
      NPARST=NPARST+NPARS
!
! Impressions
!============
! Writing
!========
!
      IF (KLIST) THEN
        IF (NPARS.NE.0) THEN
          IF (LNG.EQ.1) THEN
            PRINT*,NPARS,' particules sorties'
            PRINT*,'Nombre total de particules sorties : ',NPARST
          ELSEIF (LNG.EQ.2) THEN
            PRINT*,NPARS,' outgoing particles'
            PRINT*,'Total number of outgoing particles : ',NPARST
          ENDIF
        ENDIF
        IF (LNG.EQ.1) THEN
          PRINT*,'Nombre de particules fluides       : ',NPARF
        ELSEIF (LNG.EQ.2) THEN
          PRINT*,'Number of fluid particles          : ',NPARF
        ENDIF
      ENDIF
!
      RETURN
      END