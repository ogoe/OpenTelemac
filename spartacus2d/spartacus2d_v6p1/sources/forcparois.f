C                        ***********************
                          SUBROUTINE FORCPAROIS
C                        ***********************
C     
     .(NLIEN, NLIENMAX, NPARF, NPMAX, ILIEN, KFPAR,
     . KPAR , CPAR    , FPARX, FPARZ, H    , R0   ,
     . RAB  , VITC0   , XAB  , ZAB                )
C     
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! CPAR      ! -->  ! WALL FORCE 1 COEFFICIENT                       !
C ! FPARX,                                                            !
C ! FPARZ     ! <--  ! WALL FORCE COMPONENTS                          !
C ! H         ! -->  ! SMOOTHING LENGTH                               !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! KFPAR     ! -->  ! CHOICE INDEX FOR WALL FORCES                   !
C ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMAL NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! R0        ! -->  ! WALL ACTION DISTANCE                           !
C ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
C ! VITC0     ! -->  ! SPEED OF SOUND                                 !
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
C FONCTION : calcule les forces de parois
C FUNCTION : computes wall force contribution
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
      INTEGER NPMAX, NPARF, NLIENMAX, KFPAR
      INTEGER I    , L    , J
      INTEGER MP   , NP
C
      INTEGER NLIEN(NPMAX), KPAR(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C     
      DOUBLE PRECISION R0   , CPAR , TEMPO, VITC0, ETA
      DOUBLE PRECISION H    , HR0  , RR0  , LPAR
      DOUBLE PRECISION APAR2, DPAR2, APAR3, DPAR3
C
      DOUBLE PRECISION FPARX(NPMAX), FPARZ(NPMAX)
C
      DOUBLE PRECISION XAB(NPMAX,NLIENMAX), ZAB(NPMAX,NLIENMAX)
      DOUBLE PRECISION RAB(NPMAX,NLIENMAX)
C    
C Coefficients
C=============
C
C Forme liee a la vitesse du son
C-------------------------------
C Formulation linked to the speed of sound
C-----------------------------------------
C
      APAR2= 4.D0*(VITC0**2)/(81.D0*H)
      DPAR2= 9.D0*APAR2
C
C Forme avec coeur attractif
C---------------------------
C Formulation with an attractive core
C------------------------------------
C
      MP   = 4
      NP   = 2
      ETA  = 2.D0*SQRT(2.D0)*VITC0**2/(27.D0*H)
      HR0  = H/R0
      LPAR = (NP/MP)**(1./(MP-NP))
      DPAR3= 2.D0*ETA*(LPAR**MP-LPAR**NP)/(HR0-1.D0)
      APAR3= -DPAR3/(2.D0*(HR0-1.D0))
C 
C Forces de parois
C=================
C Wall forces
C============
C
      DO 117 I=1,NPARF
C
        FPARX(I)=0.D0
        FPARZ(I)=0.D0
C
        DO 116 L=1,NLIEN(I)
          J=ILIEN(I,L)
C     
          IF (KPAR(J).EQ.2) THEN
            RR0=RAB(I,L)/R0
C
            IF (KFPAR.EQ.1) THEN
C
C Forme de Lennard-Jones
C-----------------------
C Lennard-Jones force
C--------------------
C
              IF (RR0.LE.1.D0) THEN
                TEMPO = CPAR*(1.D0/RR0**4-1.D0/RR0**2)/RAB(I,L)
              ELSE
                TEMPO = 0.D0
              ENDIF
C
            ELSE IF (KFPAR.EQ.2) THEN
C
C Forme liee a la vitesse du son
C-------------------------------
C Formulation linked to the speed of sound
C-----------------------------------------
C
              IF      (RR0.LE.2.D0/3.D0) THEN
                TEMPO = APAR2*(2.D0/3.D0/RR0)**4	     
              ELSE IF (RR0.LE.1.D0     ) THEN
                TEMPO = DPAR2*(RR0-1.D0)**2
              ELSE
                TEMPO = 0.D0
              ENDIF
C
            ELSE IF (KFPAR.EQ.3) THEN
C
C Forme avec coeur attractif
C---------------------------
C Formulation with an attrative core
C-----------------------------------
C
              IF      (RR0.LE.1.D0) THEN
                TEMPO = ETA*((LPAR/RR0)**MP-(LPAR/RR0)**NP)	     
              ELSE IF (RR0.LE.HR0 ) THEN
                TEMPO = APAR3*(HR0-RR0)**2+DPAR3*(HR0-RR0)
              ELSE
                TEMPO = 0.D0
              ENDIF
C
C---------------------------
C                  
            ENDIF
C
            FPARX(I) = FPARX(I) + TEMPO*XAB(I,L)/RAB(I,L)
            FPARZ(I) = FPARZ(I) + TEMPO*ZAB(I,L)/RAB(I,L)
C
          ENDIF
C
 116    CONTINUE
 117  CONTINUE
C
      RETURN
      END
