!                        **********************
                          SUBROUTINE MOUVEMENT
!                        **********************
!
     &  (NBMAX , NPARB , NPARF, NPART  , NPMAX, CR  ,
     &   KPAR  , KPER  , KTURB, KPARMOB, KPARM, AX  ,
     &   AZ    , CTHETA, DELTA, DT     , KAPPA, RUG ,
     &   STHETA, USTAR , VX   , VZ     , X    , XMIN,
     &   XMAX  , Z     , TEMPS, PI                  )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! AX, AZ    ! -->  ! PARTICLE ACCELERATION COMPONENTS               !
! ! CR        ! -->  ! COEFFICIENT RELATIVE TO A ROUGH LOG LAW        !
! ! CTHETA,                                                           !
! ! STHETA    ! -->  ! COMPONENTS OF WALL NORMAL VECTORS              !
! ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
! ! DT        ! -->  ! TIME STEP                                      !
! ! KAPPA     ! -->  ! VON KARMAN CONSTANT                            !
! ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
! ! KPARM     ! -->  ! LOGICAL INDEX FOR MOVING WALL                  !
! ! KPARMOB   ! -->  ! MOVING WALL OR EDGE PARTICLE TYPE              !
! ! KPER      ! -->  ! LOGICAL INDEX FOR PERIODICITY                  !
! ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
! ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! RUG       ! -->  ! WALL ROUGHNESS                                 !
! ! TEMPS     ! -->  ! PHYSICAL TIME                                  !
! ! USTAR     ! -->  ! FRICTION VELOCITY                              !
! ! VX, VZ    ! <--> ! VELOCITY COMPONENTS                            !
! ! X, Z      ! <--> ! PARTICLE POSITION                              !
! ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
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
! FONCTION : calcule les vitesses et les positions des particules
! FUNCTION : computes velocity and position particles
!
! PROGRAMMES APPELANT : SPARTACUS2D
! CALLED BY
!
! PROGRAMMES APPELES  : PAROIMOBILE
! CALLED PROGRAMS
!
!----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER NPMAX, NPART, NPARF, NPARB
      INTEGER KTURB, NBMAX
      INTEGER I    , J
!
      LOGICAL KPER, KPARM
!
      DOUBLE PRECISION DT   , DELTA, KAPPA, RUG
      DOUBLE PRECISION XMIN , XMAX , CR   , TEMPO
      DOUBLE PRECISION TEMPS, PI
!
      INTEGER KPAR (NPMAX), KPARMOB (NPMAX)
!
      DOUBLE PRECISION X     (NPMAX), Z     (NPMAX)
      DOUBLE PRECISION VX    (NPMAX), VZ    (NPMAX)
      DOUBLE PRECISION AX    (NPMAX), AZ    (NPMAX)
      DOUBLE PRECISION VXMOB (NPMAX), VZMOB (NPMAX)
!
      DOUBLE PRECISION USTAR (NBMAX), CTHETA(NBMAX)
      DOUBLE PRECISION STHETA(NBMAX)
!
! Vitesse d une ou plusieurs parois
!==================================
! Velocity of one or several moving walls
!========================================
!
      IF (KPARM) THEN
!
        CALL PAROIMOBILE
!
     &  (KPARMOB, NPMAX, NPART, PI, TEMPS, VXMOB, VZMOB)
!
      ENDIF
!
! Calcul des vitesses
!====================
! Velocity computation
!=====================
!
      DO 113 I=1,NPART
!
        IF (KPAR(I).EQ.1) THEN
!
! Particules fluides libres
!--------------------------
! Free fluid particles
!---------------------
!
          VX(I) = VX(I) + DT*AX(I)
          VZ(I) = VZ(I) + DT*AZ(I)
!
        ELSE
!
          IF (KPARM.AND.KPARMOB(I).EQ.1) THEN
!
             VX(I) = VXMOB(I)
             VZ(I) = VZMOB(I)
!
          ELSE
!
             VX(I) = 0.
             VZ(I) = 0.
!
          ENDIF
!
        ENDIF
!
 113  CONTINUE
!
! Mouvement des particules
!=========================
! Particle moving
!================
!
      DO 107 I=1,NPART
!
! Particules fluides et parois mobiles
!-------------------------------------
! Fluid particles and moving walls
!---------------------------------
!
        X(I) = X(I) + DT*VX(I)
        Z(I) = Z(I) + DT*VZ(I)
!
! Periodicite
!------------
! Periodicity
!------------
!
        IF (KPER) THEN
!
            IF (X(I).GT.XMAX) X(I) = X(I)-(XMAX-XMIN)
            IF (X(I).LT.XMIN) X(I) = X(I)+(XMAX-XMIN)
!
        ENDIF
!
 107  CONTINUE
!
! Conditions turbulentes aux parois
!.................................
! Wall turbulent conditions
!..........................
!
       IF (KTURB.NE.0) THEN
!
         DO 696 I=NPARF+1,NPARF+NPARB
!
            J=I-NPARF
            TEMPO = USTAR(J)*(LOG(DELTA/RUG)/KAPPA+CR)
            VX(I) =  VX(I)+TEMPO*STHETA(J)
            VZ(I) =  VZ(I)-TEMPO*CTHETA(J)
!
 696     CONTINUE
!
       ENDIF
!
      RETURN
      END