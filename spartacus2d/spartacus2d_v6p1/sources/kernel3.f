C                        ********************
                          SUBROUTINE KERNEL3
C                        ********************
C
     .   (NLIEN, NLIENMAX, NPART, NPMAX, ILIEN, GKERX,
     .    GKERZ, H       , PI   , RAB  , XAB  , ZAB  )
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
C----------------------------------------------------------------------
C
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C
C FONCTION : calcule le gradient du noyau d'ordre 3
C FUNCTION : computes the derivative of the third order spline kernel
C
C PROGRAMMES APPELANT : KERNEL
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
      INTEGER NPMAX, NPART, NLIENMAX
      INTEGER I    , L    , J
      INTEGER LNG  , LU
      COMMON/INFO/LNG,LU      
C
      DOUBLE PRECISION H    , Q
      DOUBLE PRECISION TEMPO, SIGMA, PI
C
      INTEGER NLIEN(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
C     
      SIGMA=30.D0/7.D0/PI/H**4
C
C Calcul du gradient du noyau 'SPLINE BASED KERNEL' ordre 3
C==========================================================
C Computation of the third order spline kernel derivative
C========================================================
C
      DO 201 I=1,NPART
        DO 202 L=1,NLIEN(I)
          J=ILIEN(I,L)
C     
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
C
          IF    (Q.LE.1.) THEN
            TEMPO=(-1.D0+3.D0/4.D0*Q)
          ELSEIF(Q.LE.2.) THEN
            TEMPO=-(1.D0/Q-1.D0+Q/4.D0)
          ELSE
            TEMPO=0.D0
          ENDIF
C
          GKERX(I,L)=SIGMA*TEMPO*XAB(I,L)
          GKERZ(I,L)=SIGMA*TEMPO*ZAB(I,L)
C
 202    CONTINUE
 201  CONTINUE
C     
      RETURN
      END
