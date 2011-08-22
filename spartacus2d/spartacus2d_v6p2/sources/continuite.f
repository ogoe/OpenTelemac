!                        ***********************
                          SUBROUTINE CONTINUITE
!                        ***********************
!
     &(ILIEN   , NFLUIDMAX, NLIEN , NLIENMAX, NPARB, NPARF   ,
     & NPART   , NPMAX    , KFLUID, KLISS   , KGRAV, FACTLISS,
     & KER4_VAL, DT       , GKERX , GKERZ   , H    , MASS    ,
     & RAB     , RHO      , RHO0  , VX      , VZ             )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! DT        ! -->  ! TIME STEP                                      !
! ! FACTLISS  ! -->  ! DENSITY SMOOTHING FACTOR                       !
! ! GKERX,                                                            !
! ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! KER4_VAL  ! -->  ! FOURTH KERNEL VALUE                            !
! ! KGRAV     ! -->  ! LOGICAL INDEX FOR THE GRAVITY                  !
! ! KLISS     ! -->  ! LOGICAL RELATIVE TO DENSITY SMOOTHING          !
! ! MASS      ! -->  ! PARTICLE MASS                                  !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! RHO       ! <--> ! DENSITY                                        !
! ! VX, VZ    ! MS-1 ! VELOCITY COMPONENTS                            !
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
! FONCTION : calcule les densites
! FUNCTION : computes the densities
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
      INTEGER NPMAX , NPARF, NPARB, NLIENMAX, NFLUIDMAX
      INTEGER I     , L    , J    , NPART
!
      LOGICAL KLISS , KGRAV
!
      DOUBLE PRECISION DT       , TEMPO
      DOUBLE PRECISION FACTLISS, H
!
      INTEGER NLIEN (NPMAX)
      INTEGER KFLUID(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION VX (NPMAX), VZ  (NPMAX), RHOTI(NPMAX)
      DOUBLE PRECISION RHO(NPMAX) ,MASS(NPMAX), RHO0 (NFLUIDMAX)
!
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION KER4_VAL(NPMAX,0:NLIENMAX)
      DOUBLE PRECISION RAB     (NPMAX,NLIENMAX)
!
! Equation de continuite
!=======================
! Continuity equation
!====================
!
! Reinitialisation de la densite au bord pour un ecoulement gravitaire
! .....................................................................
! Boundary density reinitialisation for a gravitational flow
! ..........................................................
!
      IF (KGRAV) THEN
        DO 208 I=NPARF+1,NPARF+NPARB
          RHO(I) = RHO0(KFLUID(I))
 208    CONTINUE
      ENDIF
!
      DO 108 I=1,NPARF
        DO 235 L=1,NLIEN(I)
          J = ILIEN(I,L)
!
          TEMPO=((VX(I)-VX(J))*GKERX(I,L)
     &          +(VZ(I)-VZ(J))*GKERZ(I,L))*DT
!
          RHO(I) = RHO(I) + MASS(J) * TEMPO
          RHO(J) = RHO(J) + MASS(I) * TEMPO
!
 235    CONTINUE
 108  CONTINUE
!
! Lissage de la densite
! .....................
! Density smoothing
! .................
!
      IF (KLISS) THEN
!
! Calcul des valeurs du kernel 4
! ..............................
! Fourth order kernel value
! .........................
!
        CALL KERNEL4_VAL
!
     &   (NLIEN   , NLIENMAX, NPART, NPMAX, ILIEN,
     &    KER4_VAL, H       , RAB                )
!
      DO 106 I=1,NPARF
        RHOTI(I) = 0.D0
 106  CONTINUE
!
      DO 107 I=1,NPARF
        DO 234 L=1,NLIEN(I)
          J = ILIEN(I,L)
!
          TEMPO = 2.D0/(RHO(I) + RHO(J))*(RHO(J)-RHO(I))
!
          RHOTI(I) = RHOTI(I) + MASS(J) * TEMPO * KER4_VAL(I,L)
          RHOTI(J) = RHOTI(J) - MASS(I) * TEMPO * KER4_VAL(I,L)
!
 234    CONTINUE
 107  CONTINUE
!
      DO 109 I=1,NPARF
        RHOTI(I) = RHO  (I) + FACTLISS*RHOTI(I)
        RHO  (I) = RHOTI(I)
 109  CONTINUE
!
      ENDIF
!
      RETURN
      END