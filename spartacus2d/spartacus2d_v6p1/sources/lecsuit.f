!                        ********************
                          SUBROUTINE LECSUIT
!                        ********************
!
     &  (NPARB, NPARF , NPART, NPMAX  , NBMAX, CT   ,
     &   KENT , KFLUID, KPAR , KPARMOB, DR   , EPS  ,
     &   MASS , NUT   , P    , RHO    , TEMPS, THETA,
     &   TKE  , VX    , VZ   , X      , Z           )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! CT        ! <--  ! TECPLOT CURRENT ZONE NUMBER                    !
! ! DR        ! <--  ! INITIAL INTERPARTICLE SPACING                  !
! ! EPS       ! <--  ! DISSIPATION RATE                               !
! ! KENT      ! <--  ! CONDITION TYPE AT FLUID BOUNDARIES             !
! ! KFLUID    ! <--  ! FLUID TYPE                                     !
! ! KPARMOB   ! <--  ! MOVING WALL OR EDGE PARTICLE TYPE              !
! ! MASS      ! <--  ! PARTICLE MASS                                  !
! ! NPARB     ! <--  ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     ! <--  ! NUMBER OF FLUID PARTICLES                      !
! ! NPART     ! <--  ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
! ! NUT       ! <--  ! EDDY VISCOSITY                                 !
! ! P         ! <--  ! PRESSURE                                       !
! ! RHO       ! <--  ! DENSITY                                        !
! ! TEMPS     ! <--  ! PHYSICAL TIME                                  !
! ! THETA     ! <--  ! ANGLE BETWEEN WALL NORMAL VECTOR AND X-AXIS    !
! ! TKE       ! <--  ! TURBULENT KINETIC ENERGY                       !
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
! FONCTION : lit les parametres initiaux pour une suite de calcul
! FUNCTION : reads the initial parameters to continue a calculation
!
! PROGRAMMES APPELANT : INITIAL
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
      INTEGER NPART, NPMAX, NPARF
      INTEGER NPARB, NBMAX
      INTEGER CT   , I
!
      DOUBLE PRECISION TEMPS, DR, TEMPO
!
      INTEGER KPAR(NPMAX), KFLUID (NPMAX)
      INTEGER KENT(NPMAX), KPARMOB(NPMAX)
!
      DOUBLE PRECISION X    (NPMAX), RHO    (NPMAX)
      DOUBLE PRECISION VX   (NPMAX), P      (NPMAX)
      DOUBLE PRECISION VZ   (NPMAX), Z      (NPMAX)
      DOUBLE PRECISION MASS (NPMAX), TKE    (NPMAX)
      DOUBLE PRECISION EPS  (NPMAX), NUT    (NPMAX)
      DOUBLE PRECISION THETA(NBMAX)
!
! Lecture du fichier resultats precedents
!========================================
! Reading of the former printout file
!====================================
!
      OPEN (77,FILE='FORT.77',STATUS='old')
!
      READ (77,131) DR
      READ (77,130) NPARF
      READ (77,130) NPARB
      READ (77,130) NPART
      READ (77,131) TEMPS
      READ (77,130) CT
!
      DO 137 I=1,NPART
        READ (77,133) MASS  (I), X   (I), Z      (I),
     &                VX    (I), VZ  (I), RHO    (I),
     &                P     (I), TKE (I), EPS    (I),
     &                NUT   (I), TEMPO  , KPAR   (I),
     &                KFLUID(I), KENT(I), KPARMOB(I)
        IF (I.GE.NPARF+1.AND.I.LE.NPARF+NPARB) THEN
          THETA(I-NPARF)=TEMPO
        ENDIF
 137  CONTINUE
!
      CLOSE (77)
!
!----------------------------------------
!
 130  FORMAT(I6)
 131  FORMAT(D16.8)
 133  FORMAT(11(D16.8),4(I6))
!
      RETURN
      END