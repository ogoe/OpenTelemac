C                        ********************
                          SUBROUTINE FORCEXT
C                        ********************
C     
     .(NPARF, NPMAX, NPARB , KCPAR, KGRAV, KMOT , 
     . KTURB, DEB  , DEBREF, DR   , DT   , FEXTX,  
     . FEXTZ, FMOT , FORCST, FPARX, FPARZ, GRAV ,
     . VX   , X     , XMIN                      )
C     
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! DEB       ! <--  ! COMPUTED MEAN BULK VELOCITY                    !
C ! DEBREF    ! -->  ! PRESCRIBED MEAN BULK VELOCITY                  !
C ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
C ! DT        ! -->  ! TIME STEP                                      !
C ! FEXTX,                                                            !
C ! FEXTZ     ! <--  ! EXTERNAL FORCE COMPONENTS                      !
C ! FMOT      ! <--  ! AXIAL DRIVING FORCE                            !
C ! FORCST    ! -->  ! PRESCRIBED AXIAL DRIVING FORCE                 !
C ! FPARX,                                                            !
C ! FPARZ     ! -->  ! WALL FORCE COMPONENTS                          !
C ! GRAV      ! -->  ! GRAVITY                                        !
C ! KCPAR     ! -->  ! CHOICE INDEX FOR WALL MODELLING                !
C ! KGRAV     ! -->  ! LOGICAL RELATIVE TO THE GRAVITY                !
C ! KMOT      ! -->  ! CHOICE INDEX FOR THE FORCING TERM              !
C ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! VX        ! -->  ! AXIAL VELOCITY                                 !
C ! X         ! -->  ! PARTICLE POSITION ALONG X                      !
C ! XMIN      ! -->  ! MINIMUM X OF THE DOMAIN                        !
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
C FONCTION : calcule les forces exterieures
C FUNCTION : computes external forces
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
      INTEGER NPARF, NPMAX, NP
      INTEGER NPARB, KCPAR, KMOT 
      INTEGER KTURB, I
      INTEGER NVIT
C
      LOGICAL KGRAV
C
      DOUBLE PRECISION DEB   , FCORR, DEB2
      DOUBLE PRECISION DEBREF, DT   , GRAV
      DOUBLE PRECISION XMIN  , DR   , FORCST
C
      DOUBLE PRECISION VX   (NPMAX), X    (NPMAX)
      DOUBLE PRECISION FEXTX(NPMAX), FEXTZ(NPMAX), FMOT(NPMAX)
      DOUBLE PRECISION FPARX(NPMAX), FPARZ(NPMAX)
C
C Calcul de la force motrice eventuelle
C======================================
C Computation of the driving force if selected
C=============================================
C
      IF (KMOT.EQ.2) THEN
C
C Force constante
C----------------
C Constant force
C---------------
C
        DO 112 I=1,NPARF
          FMOT(I)=FORCST
 112    CONTINUE
C
      ELSE IF (KMOT.GE.3) THEN
C
C Force correspondant a une vitesse moyenne debitante 
C----------------------------------------------------
C Driving force relative to a prescribed mean bulk velocity
C----------------------------------------------------------
C
        DEB2=0.D0
        NP=0
C Prise en compte des particules de bord pour un ecoulement turbulent
C ...................................................................
C Edge particles taken into account for a turbulent flow
C ......................................................
        IF (KTURB.EQ.0) THEN
          NVIT=NPARF
        ELSE
          NVIT=NPARF+NPARB
        ENDIF
C
        DO 111 I=1,NVIT
          IF (X(I).GE.XMIN.AND.X(I).LE.XMIN+(2*DR)) THEN
            DEB2 = DEB2 + VX(I)
            NP   = NP + 1
          ENDIF
 111    CONTINUE
C
        DEB2 = DEB2/NP
C
        FCORR=(2.D0*(DEB2-DEBREF)-(DEB-DEBREF))/(2.D0*DT)
C
        DO 113 I=1,NPARF
          FMOT(I)=FMOT(I)-FCORR
 113    CONTINUE
C
        DEB=DEB2
C
      ENDIF
C
C Calcul des forces exterieures
C==============================
C External force computation
C===========================
C
      DO 114 I=1,NPARF
        FEXTX(I)=0.D0
        IF (KMOT .GE.2) FEXTX(I)=FEXTX(I)+FMOT (I)
        IF (KCPAR.NE.1) FEXTX(I)=FEXTX(I)+FPARX(I)
          FEXTZ(I)=0.D0
        IF (KCPAR.NE.1) FEXTZ(I)=FEXTZ(I)+FPARZ(I)
        IF (KGRAV     ) FEXTZ(I)=FEXTZ(I)-GRAV
 114  CONTINUE
C
      RETURN
      END
