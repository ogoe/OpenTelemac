C                        ************************
                          SUBROUTINE KERNEL4_VAL
C                        ************************
C
     .(NLIEN   , NLIENMAX, NPART, NPMAX, ILIEN,
     . KER4_VAL, H       , RAB                )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C !                                                                   !
C ! KER4_VAL  ! <--  ! FOURTH ORDER SPLINE KERNEL VALUES              !
C ! H         ! -->  ! SMOOTHING LENGTH                               !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! PI        ! -->  ! ARCHIMEDE'S NUMBER                             !
C ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
C !                                                                   !
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
C FONCTION : calcule les valeurs du noyau d'ordre 4
C FUNCTION : computes the values of the fourth order spline kernel
C
C PROGRAMMES APPELANT : CONTINUITE
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
C
      DOUBLE PRECISION H    , Q
      DOUBLE PRECISION TEMPO, SIGMA
C
      INTEGER NLIEN(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C
      DOUBLE PRECISION RAB     (NPMAX,NLIENMAX)
      DOUBLE PRECISION KER4_VAL(NPMAX,0:NLIENMAX)   
C     
      SIGMA=51.D0/2000.D0/H**2
C
C Calcul du noyau 'SPLINE BASED KERNEL' ordre 4
C==============================================
C Computation of the fourth order spline kernel 
C==============================================
C
      DO 201 I=1,NPART
C      
        KER4_VAL(I,0) = SIGMA * 230.D0/16.D0
C	 
        DO 202 L=1,NLIEN(I)
          J=ILIEN(I,L)
C
          Q=RAB(I,L)/H
C
          IF     (Q.LT.0.5D0) THEN
            TEMPO=((2.5D0-Q)**4-5.D0*(1.5D0-Q)**4+
     .                           10.D0*(0.5D0-Q)**4)
          ELSEIF (Q.LT.1.5D0) THEN
            TEMPO=((2.5D0-Q)**4-5.D0*(1.5D0-Q)**4)   
          ELSEIF (Q.LT.2.5D0) THEN
            TEMPO= (2.5D0-Q)**4                     
          ELSE
            TEMPO=0.D0
          ENDIF
C     
          KER4_VAL(I,L)=SIGMA*TEMPO
C     
 202     CONTINUE
 201   CONTINUE            
C     
      RETURN
      END
