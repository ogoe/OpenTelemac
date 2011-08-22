!                        ************************
                          SUBROUTINE KERNEL4_VAL
!                        ************************
!
     &(NLIEN   , NLIENMAX, NPART, NPMAX, ILIEN,
     & KER4_VAL, H       , RAB                )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! !                                                                   !
! ! KER4_VAL  ! <--  ! FOURTH ORDER SPLINE KERNEL VALUES              !
! ! H         ! -->  ! SMOOTHING LENGTH                               !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! PI        ! -->  ! ARCHIMEDE'S NUMBER                             !
! ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
! !                                                                   !
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
! FONCTION : calcule les valeurs du noyau d'ordre 4
! FUNCTION : computes the values of the fourth order spline kernel
!
! PROGRAMMES APPELANT : CONTINUITE
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
      INTEGER NPMAX, NPART, NLIENMAX
      INTEGER I    , L    , J
!
      DOUBLE PRECISION H    , Q
      DOUBLE PRECISION TEMPO, SIGMA
!
      INTEGER NLIEN(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION RAB     (NPMAX,NLIENMAX)
      DOUBLE PRECISION KER4_VAL(NPMAX,0:NLIENMAX)
!
      SIGMA=51.D0/2000.D0/H**2
!
! Calcul du noyau 'SPLINE BASED KERNEL' ordre 4
!==============================================
! Computation of the fourth order spline kernel
!==============================================
!
      DO 201 I=1,NPART
!
        KER4_VAL(I,0) = SIGMA * 230.D0/16.D0
!
        DO 202 L=1,NLIEN(I)
          J=ILIEN(I,L)
!
          Q=RAB(I,L)/H
!
          IF     (Q.LT.0.5D0) THEN
            TEMPO=((2.5D0-Q)**4-5.D0*(1.5D0-Q)**4+
     &                           10.D0*(0.5D0-Q)**4)
          ELSEIF (Q.LT.1.5D0) THEN
            TEMPO=((2.5D0-Q)**4-5.D0*(1.5D0-Q)**4)
          ELSEIF (Q.LT.2.5D0) THEN
            TEMPO= (2.5D0-Q)**4
          ELSE
            TEMPO=0.D0
          ENDIF
!
          KER4_VAL(I,L)=SIGMA*TEMPO
!
 202     CONTINUE
 201   CONTINUE
!
      RETURN
      END