C                         *****************
                           SUBROUTINE ETAT
C                         *****************
C
     .  (NFLUIDMAX, NPART, NPMAX, KFLUID, P,
     .   RHO      , RHO0 , VITC0           )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! KFLUID    !  --> ! FLUID TYPE                                     !
C ! NFLUIDMAX !  --> ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NPART     !  --> ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     !  --> ! MAXIMUM PARTICLE NUMBER                        !
C ! P         !  <-- ! PRESSURE                                       !
C ! RHO       !  --> ! DENSITY                                        !
C ! RHO0      !  --> ! REFERENCE DENSITIES                            !
C ! VITC0     !  --> ! SPEED OF SOUND                                 !
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
C FONCTION : calcule les pressions
C FUNCTION : computes the pressure
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
      INTEGER NPMAX    , NPART    
      INTEGER NFLUIDMAX, GAMMA
      INTEGER I       
C
      DOUBLE PRECISION VITC0, RHOZ
c
      INTEGER KFLUID(NPMAX)
C
      DOUBLE PRECISION RHO (NPMAX)    , P(NPMAX)
      DOUBLE PRECISION RHO0(NFLUIDMAX)
C
      PARAMETER (GAMMA=7)
C
C Equation d'etat
C================
C State equation
C===============
C
      DO 100 I=1,NPART
        RHOZ=RHO0(KFLUID(I))
        P(I)=VITC0**2*RHOZ/DBLE(GAMMA)*((RHO(I)/RHOZ)**GAMMA-1.D0)
 100  CONTINUE
C     
      RETURN
      END
