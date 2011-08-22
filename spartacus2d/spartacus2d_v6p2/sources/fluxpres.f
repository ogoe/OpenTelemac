!                        *********************
                          SUBROUTINE FLUXPRES
!                        *********************
!
     &(NLIEN, NLIENMAX, NPARF, NPMAX, ILIEN, KCPAR,
     & KPAR , KPRES   , KTURB, FPRES, P    , RHO  ,
     & TKE                                        )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! FPRES     ! <--  ! MOMENTUM FLUX DUE TO THE PRESSURE              !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! KCPAR     ! -->  ! CHOICE INDEX FOR WALL MODELLING                !
! ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
! ! KPRES     ! -->  ! CHOICE INDEX FOR THE PRESSURE GRADIENT MODEL   !
! ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! P         ! -->  ! PRESSURE                                       !
! ! RHO       ! -->  ! DENSITY                                        !
! ! TKE       ! -->  ! TURBULENT KINETIC ENERGY                       !
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
! FONCTION : calcule les flux de pression
! FUNCTION : computes pressure flux
!
! PROGRAMMES APPELANT : IMPULSION
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
      INTEGER NPMAX, NLIENMAX, NPARF
      INTEGER KPRES, KCPAR   , KTURB
      INTEGER I    , L       , J
!
      INTEGER NLIEN(NPMAX), KPAR(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION P  (NPMAX), RHO(NPMAX)
      DOUBLE PRECISION TKE(NPMAX)
!
      DOUBLE PRECISION FPRES(NPMAX,NLIENMAX)
!
! Calcul des flux de pression
!============================
! Pressure flux computation
!==========================
!
      IF (KPRES.EQ.1) THEN
!
! Forme 1
!--------
! Formulation 1
!--------------
!
        DO 103 I=1,NPARF
          DO 104 L=1,NLIEN(I)
            J=ILIEN(I,L)
            IF (KPAR(J).EQ.1.OR.KCPAR.EQ.1) THEN
              IF (KTURB.GE.2) THEN
                FPRES(I,L)=-P(I)/RHO(I)/RHO(I)-P(J)/RHO(J)/RHO(J)-
     &          (2.D0/3.D0)*((TKE(I)/RHO(I))+(TKE(J)/RHO(J)))
              ELSE
                FPRES(I,L)=-P(I)/RHO(I)/RHO(I)-P(J)/RHO(J)/RHO(J)
              ENDIF
            ELSE
              FPRES(I,L)=0.D0
            ENDIF
 104      CONTINUE
 103    CONTINUE
!
      ELSE
!
! Forme 2
!--------
! Formulation 2
!--------------
!
        DO 203 I=1,NPARF
          DO 204 L=1,NLIEN(I)
            J=ILIEN(I,L)
            IF (KPAR(J).EQ.1.OR.KCPAR.EQ.1) THEN
              IF (KTURB.GE.2) THEN
                FPRES(I,L)=-(P(J)+P(I)+(2.D0/3.D0)*(RHO(I)*TKE(I)+
     &          RHO(J)*TKE(J)))/(RHO(J)*RHO(I))
              ELSE
                FPRES(I,L)=-(P(J)+P(I))/(RHO(J)*RHO(I))
              ENDIF
            ELSE
              FPRES(I,L)=0.D0
            ENDIF
 204      CONTINUE
 203    CONTINUE
!
      ENDIF
!
      RETURN
      END