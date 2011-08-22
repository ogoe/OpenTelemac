!                        ********************
                          SUBROUTINE KERNEL4
!                        ********************
!
     &   (NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, GKERX,
     &    GKERZ, H       , RAB  , XAB  , ZAB         )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! GKERX,                                                            !
! ! GKERZ     ! <--  ! KERNEL DERIVATIVE COMPONENTS                   !
! ! H         ! -->  ! SMOOTHING LENGTH                               !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! PI        ! -->  ! ARCHIMEDE'S NUMBER                             !
! ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
! ! XAB, ZAB  ! -->  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
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
! FONCTION : calcule le gradient du noyau d'ordre 4
! FUNCTION : computes the derivative of the fourth order spline kernel
!
! PROGRAMMES APPELANT : KERNEL
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
      INTEGER LNG  , LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION H    , Q
      DOUBLE PRECISION TEMPO, SIGMA
!
      INTEGER NLIEN(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
!
      SIGMA=0.102D0/H**4
!
! Calcul du gradient du noyau 'SPLINE BASED KERNEL' ordre 4
!==========================================================
! Computation of the fourth order spline kernel derivative
!=========================================================
!
      DO 201 I=1,NPART
        DO 202 L=1,NLIEN(I)
          J=ILIEN(I,L)
!
          Q=RAB(I,L)/H
          IF (Q.EQ.0) THEN
            IF (LNG.EQ.1) THEN
              PRINT*,'ATTENTION : 2 particules coincident'
              PRINT*,'Particules :',I,J
           ELSEIF (LNG.EQ.2) THEN
              PRINT*,'WATCH OUT : 2 particles at the same position'
              PRINT*,'Particles :',I,J
           ENDIF
           Q=0.1*H
         ENDIF
!
          IF     (Q.LT.0.5D0) THEN
            TEMPO=(-(2.5D0-Q)**3+5.D0*(1.5D0-Q)**3-10.D0*(0.5D0-Q)**3)/Q
          ELSEIF (Q.LT.1.5D0) THEN
            TEMPO=(-(2.5D0-Q)**3+5.D0*(1.5D0-Q)**3                   )/Q
          ELSEIF (Q.LT.2.5D0) THEN
            TEMPO= -(2.5D0-Q)**3                                      /Q
          ELSE
            TEMPO=0.D0
          ENDIF
!
          GKERX(I,L)=SIGMA*TEMPO*XAB(I,L)
          GKERZ(I,L)=SIGMA*TEMPO*ZAB(I,L)
!
 202     CONTINUE
 201   CONTINUE
!
      RETURN
      END