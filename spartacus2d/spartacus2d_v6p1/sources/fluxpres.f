C                        *********************
                          SUBROUTINE FLUXPRES
C                        *********************
C    
     .(NLIEN, NLIENMAX, NPARF, NPMAX, ILIEN, KCPAR,
     . KPAR , KPRES   , KTURB, FPRES, P    , RHO  ,
     . TKE                                        )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! FPRES     ! <--  ! MOMENTUM FLUX DUE TO THE PRESSURE              !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! KCPAR     ! -->  ! CHOICE INDEX FOR WALL MODELLING                !
C ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
C ! KPRES     ! -->  ! CHOICE INDEX FOR THE PRESSURE GRADIENT MODEL   !
C ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! P         ! -->  ! PRESSURE                                       !
C ! RHO       ! -->  ! DENSITY                                        !
C ! TKE       ! -->  ! TURBULENT KINETIC ENERGY                       !
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
C FONCTION : calcule les flux de pression
C FUNCTION : computes pressure flux
C
C PROGRAMMES APPELANT : IMPULSION
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
      INTEGER NPMAX, NLIENMAX, NPARF
      INTEGER KPRES, KCPAR   , KTURB
      INTEGER I    , L       , J
C
      INTEGER NLIEN(NPMAX), KPAR(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C     
      DOUBLE PRECISION P  (NPMAX), RHO(NPMAX)
      DOUBLE PRECISION TKE(NPMAX)
C
      DOUBLE PRECISION FPRES(NPMAX,NLIENMAX)
C 
C Calcul des flux de pression
C============================
C Pressure flux computation
C==========================
C
      IF (KPRES.EQ.1) THEN
C
C Forme 1
C--------
C Formulation 1
C--------------
C
        DO 103 I=1,NPARF
          DO 104 L=1,NLIEN(I)
            J=ILIEN(I,L)
            IF (KPAR(J).EQ.1.OR.KCPAR.EQ.1) THEN
              IF (KTURB.GE.2) THEN
                FPRES(I,L)=-P(I)/RHO(I)/RHO(I)-P(J)/RHO(J)/RHO(J)-
     .          (2.D0/3.D0)*((TKE(I)/RHO(I))+(TKE(J)/RHO(J)))          
              ELSE
                FPRES(I,L)=-P(I)/RHO(I)/RHO(I)-P(J)/RHO(J)/RHO(J)
              ENDIF
            ELSE
              FPRES(I,L)=0.D0
            ENDIF
 104      CONTINUE
 103    CONTINUE
C
      ELSE
C
C Forme 2
C--------
C Formulation 2
C--------------
C
        DO 203 I=1,NPARF
          DO 204 L=1,NLIEN(I)
            J=ILIEN(I,L)
            IF (KPAR(J).EQ.1.OR.KCPAR.EQ.1) THEN
              IF (KTURB.GE.2) THEN
                FPRES(I,L)=-(P(J)+P(I)+(2.D0/3.D0)*(RHO(I)*TKE(I)+
     .          RHO(J)*TKE(J)))/(RHO(J)*RHO(I))
              ELSE
                FPRES(I,L)=-(P(J)+P(I))/(RHO(J)*RHO(I))
              ENDIF
            ELSE
              FPRES(I,L)=0.D0
            ENDIF
 204      CONTINUE
 203    CONTINUE
C
      ENDIF
C
      RETURN
      END
