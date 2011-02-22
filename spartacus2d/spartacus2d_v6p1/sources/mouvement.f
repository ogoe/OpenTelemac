C                        **********************
                          SUBROUTINE MOUVEMENT
C                        **********************
C
     .  (NBMAX , NPARB , NPARF, NPART  , NPMAX, CR  ,
     .   KPAR  , KPER  , KTURB, KPARMOB, KPARM, AX  , 
     .   AZ    , CTHETA, DELTA, DT     , KAPPA, RUG , 
     .   STHETA, USTAR , VX   , VZ     , X    , XMIN, 
     .   XMAX  , Z     , TEMPS, PI                  )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! AX, AZ    ! -->  ! PARTICLE ACCELERATION COMPONENTS               !
C ! CR        ! -->  ! COEFFICIENT RELATIVE TO A ROUGH LOG LAW        !
C ! CTHETA,                                                           !
C ! STHETA    ! -->  ! COMPONENTS OF WALL NORMAL VECTORS              !
C ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
C ! DT        ! -->  ! TIME STEP                                      !
C ! KAPPA     ! -->  ! VON KARMAN CONSTANT                            !
C ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
C ! KPARM     ! -->  ! LOGICAL INDEX FOR MOVING WALL                  !
C ! KPARMOB   ! -->  ! MOVING WALL OR EDGE PARTICLE TYPE              !
C ! KPER      ! -->  ! LOGICAL INDEX FOR PERIODICITY                  !
C ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
C ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! RUG       ! -->  ! WALL ROUGHNESS                                 !
C ! TEMPS     ! -->  ! PHYSICAL TIME                                  !
C ! USTAR     ! -->  ! FRICTION VELOCITY                              !
C ! VX, VZ    ! <--> ! VELOCITY COMPONENTS                            ! 
C ! X, Z      ! <--> ! PARTICLE POSITION                              !
C ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
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
C FONCTION : calcule les vitesses et les positions des particules
C FUNCTION : computes velocity and position particles
C
C PROGRAMMES APPELANT : SPARTACUS2D
C CALLED BY
C
C PROGRAMMES APPELES  : PAROIMOBILE
C CALLED PROGRAMS
C     
C----------------------------------------------------------------------
C     
C Variables
C==========
C
      IMPLICIT NONE
C
      INTEGER NPMAX, NPART, NPARF, NPARB
      INTEGER KTURB, NBMAX
      INTEGER I    , J    
C
      LOGICAL KPER, KPARM
C
      DOUBLE PRECISION DT   , DELTA, KAPPA, RUG
      DOUBLE PRECISION XMIN , XMAX , CR   , TEMPO
      DOUBLE PRECISION TEMPS, PI
C
      INTEGER KPAR (NPMAX), KPARMOB (NPMAX)
C
      DOUBLE PRECISION X     (NPMAX), Z     (NPMAX)
      DOUBLE PRECISION VX    (NPMAX), VZ    (NPMAX)
      DOUBLE PRECISION AX    (NPMAX), AZ    (NPMAX)
      DOUBLE PRECISION VXMOB (NPMAX), VZMOB (NPMAX)
C      
      DOUBLE PRECISION USTAR (NBMAX), CTHETA(NBMAX)
      DOUBLE PRECISION STHETA(NBMAX)
C
C Vitesse d une ou plusieurs parois
C==================================
C Velocity of one or several moving walls
C========================================
C
      IF (KPARM) THEN
C
        CALL PAROIMOBILE
C
     .  (KPARMOB, NPMAX, NPART, PI, TEMPS, VXMOB, VZMOB)
C
      ENDIF
C
C Calcul des vitesses
C====================
C Velocity computation
C=====================
C
      DO 113 I=1,NPART
C
        IF (KPAR(I).EQ.1) THEN
C
C Particules fluides libres
C--------------------------
C Free fluid particles
C---------------------
C
          VX(I) = VX(I) + DT*AX(I)
          VZ(I) = VZ(I) + DT*AZ(I)
C
        ELSE 
C	
          IF (KPARM.AND.KPARMOB(I).EQ.1) THEN
C
             VX(I) = VXMOB(I)
             VZ(I) = VZMOB(I)
C
          ELSE
C
             VX(I) = 0.
             VZ(I) = 0.
C
          ENDIF
C
        ENDIF
C
 113  CONTINUE
C
C Mouvement des particules 
C=========================
C Particle moving
C================
C        
      DO 107 I=1,NPART
C
C Particules fluides et parois mobiles
C-------------------------------------
C Fluid particles and moving walls
C---------------------------------
C
        X(I) = X(I) + DT*VX(I)
        Z(I) = Z(I) + DT*VZ(I)
C
C Periodicite
C------------
C Periodicity
C------------
C
        IF (KPER) THEN
C
            IF (X(I).GT.XMAX) X(I) = X(I)-(XMAX-XMIN)
            IF (X(I).LT.XMIN) X(I) = X(I)+(XMAX-XMIN)
C 
        ENDIF
C
 107  CONTINUE
C
C Conditions turbulentes aux parois
C.................................
C Wall turbulent conditions
C..........................
C
       IF (KTURB.NE.0) THEN
C	  
         DO 696 I=NPARF+1,NPARF+NPARB
C       
            J=I-NPARF
            TEMPO = USTAR(J)*(LOG(DELTA/RUG)/KAPPA+CR)
            VX(I) =  VX(I)+TEMPO*STHETA(J)
            VZ(I) =  VZ(I)-TEMPO*CTHETA(J)
C
 696     CONTINUE
C
       ENDIF 
C	  
      RETURN
      END
