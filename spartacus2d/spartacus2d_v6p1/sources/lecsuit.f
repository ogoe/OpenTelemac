C                        ********************
                          SUBROUTINE LECSUIT
C                        ********************
C
     .  (NPARB, NPARF , NPART, NPMAX  , NBMAX, CT   , 
     .   KENT , KFLUID, KPAR , KPARMOB, DR   , EPS  ,
     .   MASS , NUT   , P    , RHO    , TEMPS, THETA,
     .   TKE  , VX    , VZ   , X      , Z           )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! CT        ! <--  ! TECPLOT CURRENT ZONE NUMBER                    !
C ! DR        ! <--  ! INITIAL INTERPARTICLE SPACING                  !
C ! EPS       ! <--  ! DISSIPATION RATE                               !
C ! KENT      ! <--  ! CONDITION TYPE AT FLUID BOUNDARIES             !
C ! KFLUID    ! <--  ! FLUID TYPE                                     !
C ! KPARMOB   ! <--  ! MOVING WALL OR EDGE PARTICLE TYPE              !
C ! MASS      ! <--  ! PARTICLE MASS                                  !
C ! NPARB     ! <--  ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     ! <--  ! NUMBER OF FLUID PARTICLES                      !
C ! NPART     ! <--  ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
C ! NUT       ! <--  ! EDDY VISCOSITY                                 !
C ! P         ! <--  ! PRESSURE                                       !
C ! RHO       ! <--  ! DENSITY                                        !
C ! TEMPS     ! <--  ! PHYSICAL TIME                                  !
C ! THETA     ! <--  ! ANGLE BETWEEN WALL NORMAL VECTOR AND X-AXIS    !
C ! TKE       ! <--  ! TURBULENT KINETIC ENERGY                       !
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
C FONCTION : lit les parametres initiaux pour une suite de calcul
C FUNCTION : reads the initial parameters to continue a calculation
C
C PROGRAMMES APPELANT : INITIAL
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
      INTEGER NPART, NPMAX, NPARF
      INTEGER NPARB, NBMAX 
      INTEGER CT   , I
C
      DOUBLE PRECISION TEMPS, DR, TEMPO
C
      INTEGER KPAR(NPMAX), KFLUID (NPMAX)
      INTEGER KENT(NPMAX), KPARMOB(NPMAX)
C
      DOUBLE PRECISION X    (NPMAX), RHO    (NPMAX)
      DOUBLE PRECISION VX   (NPMAX), P      (NPMAX)
      DOUBLE PRECISION VZ   (NPMAX), Z      (NPMAX)
      DOUBLE PRECISION MASS (NPMAX), TKE    (NPMAX)
      DOUBLE PRECISION EPS  (NPMAX), NUT    (NPMAX)
      DOUBLE PRECISION THETA(NBMAX) 
C
C Lecture du fichier resultats precedents
C========================================
C Reading of the former printout file
C====================================
C
      OPEN (77,FILE='FORT.77',STATUS='old')
C
      READ (77,131) DR
      READ (77,130) NPARF
      READ (77,130) NPARB
      READ (77,130) NPART
      READ (77,131) TEMPS
      READ (77,130) CT
C
      DO 137 I=1,NPART
        READ (77,133) MASS  (I), X   (I), Z      (I),
     .                VX    (I), VZ  (I), RHO    (I),
     .                P     (I), TKE (I), EPS    (I),
     .                NUT   (I), TEMPO  , KPAR   (I),
     .                KFLUID(I), KENT(I), KPARMOB(I)
        IF (I.GE.NPARF+1.AND.I.LE.NPARF+NPARB) THEN
          THETA(I-NPARF)=TEMPO
        ENDIF
 137  CONTINUE
C
      CLOSE (77)
C
C----------------------------------------
C
 130  FORMAT(I6)
 131  FORMAT(D16.8)
 133  FORMAT(11(D16.8),4(I6))
C
      RETURN
      END
