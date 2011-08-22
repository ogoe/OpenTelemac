!                        *******************
                          SUBROUTINE KERNEL
!                        *******************
!
     &  (NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, KKERNEL,
     &   GKERX, GKERZ   , H    , PI   , RAB  , XAB    ,
     &   ZAB                                          )
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
! ! KKERNEL   ! -->  ! CHOICE INDEX FOR KERNEL                        !
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
!-----------------------------------------------------------------------
!
! SPARTACUS2D V5P9
! D. Violeau           & R. Issa
! +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28
! LNHE - 2008
!
! FONCTION : calcule le gradient du noyau
! FUNCTION : computes the kernel derivative
!
! PROGRAMMES APPELANT : SPARTACUS2D
! CALLED BY
!
! PROGRAMMES APPELES  : KERNEL3, KERNEL4, KERNEL5
! CALLED PROGRAMS
!
!-----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER NPMAX, NPART, NLIENMAX, KKERNEL
      INTEGER LNG, LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION H, PI
!
      INTEGER NLIEN(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ (NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB   (NPMAX,NLIENMAX)
!
! Calcul du gradient du noyau
!============================
! Kernel derivative computation
!==============================
!
      IF (KKERNEL.EQ.1) THEN
!
! Kernel d'ordre 3
!-----------------
! Third order spline kernel
!--------------------------
!
        CALL KERNEL3
!
     &   (NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, GKERX,
     &    GKERZ, H       , PI   , RAB  , XAB  , ZAB  )
!
      ELSE IF (KKERNEL.EQ.2) THEN
!
! Kernel d'ordre 4
!-----------------
! Fourth order spline kernel
!---------------------------
!
        CALL KERNEL4
!
     &   (NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, GKERX,
     &    GKERZ, H       , RAB  , XAB  , ZAB         )
!
      ELSE IF (KKERNEL.EQ.3) THEN
!
! Kernel d'ordre 5
!-----------------
! Fifth order spline kernel
!--------------------------
!
        CALL KERNEL5
!
     &(NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, GKERX,
     & GKERZ, H       , PI   , RAB  , XAB  , ZAB  )
!
      ENDIF
!
      RETURN
      END