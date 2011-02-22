C                        ***********************
                          SUBROUTINE CONTINUITE
C                        ***********************
C     
     .(ILIEN   , NFLUIDMAX, NLIEN , NLIENMAX, NPARB, NPARF   ,
     . NPART   , NPMAX    , KFLUID, KLISS   , KGRAV, FACTLISS, 
     . KER4_VAL, DT       , GKERX , GKERZ   , H    , MASS    ,
     . RAB     , RHO      , RHO0  , VX      , VZ             )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! DT        ! -->  ! TIME STEP                                      !
C ! FACTLISS  ! -->  ! DENSITY SMOOTHING FACTOR                       !
C ! GKERX,                                                            !
C ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! KER4_VAL  ! -->  ! FOURTH KERNEL VALUE                            !
C ! KGRAV     ! -->  ! LOGICAL INDEX FOR THE GRAVITY                  !
C ! KLISS     ! -->  ! LOGICAL RELATIVE TO DENSITY SMOOTHING          !
C ! MASS      ! -->  ! PARTICLE MASS                                  !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! RHO       ! <--> ! DENSITY                                        !
C ! VX, VZ    ! MS-1 ! VELOCITY COMPONENTS                            ! 
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
C FONCTION : calcule les densites
C FUNCTION : computes the densities
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
      INTEGER NPMAX , NPARF, NPARB, NLIENMAX, NFLUIDMAX
      INTEGER I     , L    , J    , NPART
C
      LOGICAL KLISS , KGRAV
C     
      DOUBLE PRECISION DT       , TEMPO
      DOUBLE PRECISION FACTLISS, H
C
      INTEGER NLIEN (NPMAX)
      INTEGER KFLUID(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C     
      DOUBLE PRECISION VX (NPMAX), VZ  (NPMAX), RHOTI(NPMAX)
      DOUBLE PRECISION RHO(NPMAX) ,MASS(NPMAX), RHO0 (NFLUIDMAX)
C
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION KER4_VAL(NPMAX,0:NLIENMAX)
      DOUBLE PRECISION RAB     (NPMAX,NLIENMAX)
C     
C Equation de continuite
C=======================
C Continuity equation
C====================
C
C Reinitialisation de la densite au bord pour un ecoulement gravitaire
C .....................................................................
C Boundary density reinitialisation for a gravitational flow
C ..........................................................
C
      IF (KGRAV) THEN      
        DO 208 I=NPARF+1,NPARF+NPARB
          RHO(I) = RHO0(KFLUID(I))
 208    CONTINUE
      ENDIF
C
      DO 108 I=1,NPARF
        DO 235 L=1,NLIEN(I)
          J = ILIEN(I,L)
C            
          TEMPO=((VX(I)-VX(J))*GKERX(I,L)
     .          +(VZ(I)-VZ(J))*GKERZ(I,L))*DT
C            
          RHO(I) = RHO(I) + MASS(J) * TEMPO 
          RHO(J) = RHO(J) + MASS(I) * TEMPO
C     
 235    CONTINUE
 108  CONTINUE
C
C Lissage de la densite
C .....................
C Density smoothing
C .................
C
      IF (KLISS) THEN
C
C Calcul des valeurs du kernel 4
C ..............................
C Fourth order kernel value
C .........................
C
        CALL KERNEL4_VAL
C
     .   (NLIEN   , NLIENMAX, NPART, NPMAX, ILIEN, 
     .    KER4_VAL, H       , RAB                )
C
      DO 106 I=1,NPARF
        RHOTI(I) = 0.D0
 106  CONTINUE
C
      DO 107 I=1,NPARF
        DO 234 L=1,NLIEN(I)
          J = ILIEN(I,L)
C  
          TEMPO = 2.D0/(RHO(I) + RHO(J))*(RHO(J)-RHO(I)) 
C	           
          RHOTI(I) = RHOTI(I) + MASS(J) * TEMPO * KER4_VAL(I,L)   
          RHOTI(J) = RHOTI(J) - MASS(I) * TEMPO * KER4_VAL(I,L)
C     
 234    CONTINUE   
 107  CONTINUE
C      
      DO 109 I=1,NPARF
        RHOTI(I) = RHO  (I) + FACTLISS*RHOTI(I)
        RHO  (I) = RHOTI(I)
 109  CONTINUE 
C
      ENDIF
C   
      RETURN
      END
