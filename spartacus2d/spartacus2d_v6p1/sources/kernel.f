C                        *******************
                          SUBROUTINE KERNEL
C                        *******************
C
     .  (NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, KKERNEL,
     .   GKERX, GKERZ   , H    , PI   , RAB  , XAB    ,
     .   ZAB                                          )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! GKERX,                                                            !
C ! GKERZ     ! <--  ! KERNEL DERIVATIVE COMPONENTS                   !
C ! H         ! -->  ! SMOOTHING LENGTH                               !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! KKERNEL   ! -->  ! CHOICE INDEX FOR KERNEL                        !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! PI        ! -->  ! ARCHIMEDE'S NUMBER                             !
C ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
C ! XAB, ZAB  ! -->  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
C !___________!______!________________________________________________!
C
C MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
C-----------------------------------------------------------------------
C
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C
C FONCTION : calcule le gradient du noyau
C FUNCTION : computes the kernel derivative
C
C PROGRAMMES APPELANT : SPARTACUS2D
C CALLED BY 
C
C PROGRAMMES APPELES  : KERNEL3, KERNEL4, KERNEL5
C CALLED PROGRAMS     
C
C-----------------------------------------------------------------------
C
C Variables
C==========
C
      IMPLICIT NONE
C
      INTEGER NPMAX, NPART, NLIENMAX, KKERNEL
      INTEGER LNG, LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION H, PI
C
      INTEGER NLIEN(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ (NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB   (NPMAX,NLIENMAX)
C     
C Calcul du gradient du noyau
C============================
C Kernel derivative computation
C==============================
C
      IF (KKERNEL.EQ.1) THEN
C
C Kernel d'ordre 3
C-----------------
C Third order spline kernel
C--------------------------
C
        CALL KERNEL3
C
     .   (NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, GKERX,
     .    GKERZ, H       , PI   , RAB  , XAB  , ZAB  )
C
      ELSE IF (KKERNEL.EQ.2) THEN
C
C Kernel d'ordre 4
C-----------------
C Fourth order spline kernel
C---------------------------
C
        CALL KERNEL4
C
     .   (NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, GKERX,
     .    GKERZ, H       , RAB  , XAB  , ZAB         )
C
      ELSE IF (KKERNEL.EQ.3) THEN
C
C Kernel d'ordre 5
C-----------------
C Fifth order spline kernel
C--------------------------
C
        CALL KERNEL5
C
     .(NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, GKERX,
     . GKERZ, H       , PI   , RAB  , XAB  , ZAB  )
C
      ENDIF  
C     
      RETURN
      END
