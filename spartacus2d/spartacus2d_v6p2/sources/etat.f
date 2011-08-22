!                         *****************
                           SUBROUTINE ETAT
!                         *****************
!
     &  (NFLUIDMAX, NPART, NPMAX, KFLUID, P,
     &   RHO      , RHO0 , VITC0           )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! KFLUID    !  --> ! FLUID TYPE                                     !
! ! NFLUIDMAX !  --> ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NPART     !  --> ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     !  --> ! MAXIMUM PARTICLE NUMBER                        !
! ! P         !  <-- ! PRESSURE                                       !
! ! RHO       !  --> ! DENSITY                                        !
! ! RHO0      !  --> ! REFERENCE DENSITIES                            !
! ! VITC0     !  --> ! SPEED OF SOUND                                 !
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
! FONCTION : calcule les pressions
! FUNCTION : computes the pressure
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
      INTEGER NPMAX    , NPART
      INTEGER NFLUIDMAX, GAMMA
      INTEGER I
!
      DOUBLE PRECISION VITC0, RHOZ
!
      INTEGER KFLUID(NPMAX)
!
      DOUBLE PRECISION RHO (NPMAX)    , P(NPMAX)
      DOUBLE PRECISION RHO0(NFLUIDMAX)
!
      PARAMETER (GAMMA=7)
!
! Equation d'etat
!================
! State equation
!===============
!
      DO 100 I=1,NPART
        RHOZ=RHO0(KFLUID(I))
        P(I)=VITC0**2*RHOZ/DBLE(GAMMA)*((RHO(I)/RHOZ)**GAMMA-1.D0)
 100  CONTINUE
!
      RETURN
      END