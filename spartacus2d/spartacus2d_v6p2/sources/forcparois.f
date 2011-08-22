!                        ***********************
                          SUBROUTINE FORCPAROIS
!                        ***********************
!
     &(NLIEN, NLIENMAX, NPARF, NPMAX, ILIEN, KFPAR,
     & KPAR , CPAR    , FPARX, FPARZ, H    , R0   ,
     & RAB  , VITC0   , XAB  , ZAB                )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! CPAR      ! -->  ! WALL FORCE 1 COEFFICIENT                       !
! ! FPARX,                                                            !
! ! FPARZ     ! <--  ! WALL FORCE COMPONENTS                          !
! ! H         ! -->  ! SMOOTHING LENGTH                               !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! KFPAR     ! -->  ! CHOICE INDEX FOR WALL FORCES                   !
! ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMAL NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! R0        ! -->  ! WALL ACTION DISTANCE                           !
! ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
! ! VITC0     ! -->  ! SPEED OF SOUND                                 !
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
! FONCTION : calcule les forces de parois
! FUNCTION : computes wall force contribution
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
      INTEGER NPMAX, NPARF, NLIENMAX, KFPAR
      INTEGER I    , L    , J
      INTEGER MP   , NP
!
      INTEGER NLIEN(NPMAX), KPAR(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION R0   , CPAR , TEMPO, VITC0, ETA
      DOUBLE PRECISION H    , HR0  , RR0  , LPAR
      DOUBLE PRECISION APAR2, DPAR2, APAR3, DPAR3
!
      DOUBLE PRECISION FPARX(NPMAX), FPARZ(NPMAX)
!
      DOUBLE PRECISION XAB(NPMAX,NLIENMAX), ZAB(NPMAX,NLIENMAX)
      DOUBLE PRECISION RAB(NPMAX,NLIENMAX)
!
! Coefficients
!=============
!
! Forme liee a la vitesse du son
!-------------------------------
! Formulation linked to the speed of sound
!-----------------------------------------
!
      APAR2= 4.D0*(VITC0**2)/(81.D0*H)
      DPAR2= 9.D0*APAR2
!
! Forme avec coeur attractif
!---------------------------
! Formulation with an attractive core
!------------------------------------
!
      MP   = 4
      NP   = 2
      ETA  = 2.D0*SQRT(2.D0)*VITC0**2/(27.D0*H)
      HR0  = H/R0
      LPAR = (NP/MP)**(1./(MP-NP))
      DPAR3= 2.D0*ETA*(LPAR**MP-LPAR**NP)/(HR0-1.D0)
      APAR3= -DPAR3/(2.D0*(HR0-1.D0))
!
! Forces de parois
!=================
! Wall forces
!============
!
      DO 117 I=1,NPARF
!
        FPARX(I)=0.D0
        FPARZ(I)=0.D0
!
        DO 116 L=1,NLIEN(I)
          J=ILIEN(I,L)
!
          IF (KPAR(J).EQ.2) THEN
            RR0=RAB(I,L)/R0
!
            IF (KFPAR.EQ.1) THEN
!
! Forme de Lennard-Jones
!-----------------------
! Lennard-Jones force
!--------------------
!
              IF (RR0.LE.1.D0) THEN
                TEMPO = CPAR*(1.D0/RR0**4-1.D0/RR0**2)/RAB(I,L)
              ELSE
                TEMPO = 0.D0
              ENDIF
!
            ELSE IF (KFPAR.EQ.2) THEN
!
! Forme liee a la vitesse du son
!-------------------------------
! Formulation linked to the speed of sound
!-----------------------------------------
!
              IF      (RR0.LE.2.D0/3.D0) THEN
                TEMPO = APAR2*(2.D0/3.D0/RR0)**4
              ELSE IF (RR0.LE.1.D0     ) THEN
                TEMPO = DPAR2*(RR0-1.D0)**2
              ELSE
                TEMPO = 0.D0
              ENDIF
!
            ELSE IF (KFPAR.EQ.3) THEN
!
! Forme avec coeur attractif
!---------------------------
! Formulation with an attrative core
!-----------------------------------
!
              IF      (RR0.LE.1.D0) THEN
                TEMPO = ETA*((LPAR/RR0)**MP-(LPAR/RR0)**NP)
              ELSE IF (RR0.LE.HR0 ) THEN
                TEMPO = APAR3*(HR0-RR0)**2+DPAR3*(HR0-RR0)
              ELSE
                TEMPO = 0.D0
              ENDIF
!
!---------------------------
!
            ENDIF
!
            FPARX(I) = FPARX(I) + TEMPO*XAB(I,L)/RAB(I,L)
            FPARZ(I) = FPARZ(I) + TEMPO*ZAB(I,L)/RAB(I,L)
!
          ENDIF
!
 116    CONTINUE
 117  CONTINUE
!
      RETURN
      END