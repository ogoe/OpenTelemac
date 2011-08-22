!                        ********************
                          SUBROUTINE FORCEXT
!                        ********************
!
     &(NPARF, NPMAX, NPARB , KCPAR, KGRAV, KMOT ,
     & KTURB, DEB  , DEBREF, DR   , DT   , FEXTX,
     & FEXTZ, FMOT , FORCST, FPARX, FPARZ, GRAV ,
     & VX   , X     , XMIN                      )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! DEB       ! <--  ! COMPUTED MEAN BULK VELOCITY                    !
! ! DEBREF    ! -->  ! PRESCRIBED MEAN BULK VELOCITY                  !
! ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
! ! DT        ! -->  ! TIME STEP                                      !
! ! FEXTX,                                                            !
! ! FEXTZ     ! <--  ! EXTERNAL FORCE COMPONENTS                      !
! ! FMOT      ! <--  ! AXIAL DRIVING FORCE                            !
! ! FORCST    ! -->  ! PRESCRIBED AXIAL DRIVING FORCE                 !
! ! FPARX,                                                            !
! ! FPARZ     ! -->  ! WALL FORCE COMPONENTS                          !
! ! GRAV      ! -->  ! GRAVITY                                        !
! ! KCPAR     ! -->  ! CHOICE INDEX FOR WALL MODELLING                !
! ! KGRAV     ! -->  ! LOGICAL RELATIVE TO THE GRAVITY                !
! ! KMOT      ! -->  ! CHOICE INDEX FOR THE FORCING TERM              !
! ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! VX        ! -->  ! AXIAL VELOCITY                                 !
! ! X         ! -->  ! PARTICLE POSITION ALONG X                      !
! ! XMIN      ! -->  ! MINIMUM X OF THE DOMAIN                        !
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
! FONCTION : calcule les forces exterieures
! FUNCTION : computes external forces
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
      INTEGER NPARF, NPMAX, NP
      INTEGER NPARB, KCPAR, KMOT
      INTEGER KTURB, I
      INTEGER NVIT
!
      LOGICAL KGRAV
!
      DOUBLE PRECISION DEB   , FCORR, DEB2
      DOUBLE PRECISION DEBREF, DT   , GRAV
      DOUBLE PRECISION XMIN  , DR   , FORCST
!
      DOUBLE PRECISION VX   (NPMAX), X    (NPMAX)
      DOUBLE PRECISION FEXTX(NPMAX), FEXTZ(NPMAX), FMOT(NPMAX)
      DOUBLE PRECISION FPARX(NPMAX), FPARZ(NPMAX)
!
! Calcul de la force motrice eventuelle
!======================================
! Computation of the driving force if selected
!=============================================
!
      IF (KMOT.EQ.2) THEN
!
! Force constante
!----------------
! Constant force
!---------------
!
        DO 112 I=1,NPARF
          FMOT(I)=FORCST
 112    CONTINUE
!
      ELSE IF (KMOT.GE.3) THEN
!
! Force correspondant a une vitesse moyenne debitante
!----------------------------------------------------
! Driving force relative to a prescribed mean bulk velocity
!----------------------------------------------------------
!
        DEB2=0.D0
        NP=0
! Prise en compte des particules de bord pour un ecoulement turbulent
! ...................................................................
! Edge particles taken into account for a turbulent flow
! ......................................................
        IF (KTURB.EQ.0) THEN
          NVIT=NPARF
        ELSE
          NVIT=NPARF+NPARB
        ENDIF
!
        DO 111 I=1,NVIT
          IF (X(I).GE.XMIN.AND.X(I).LE.XMIN+(2*DR)) THEN
            DEB2 = DEB2 + VX(I)
            NP   = NP + 1
          ENDIF
 111    CONTINUE
!
        DEB2 = DEB2/NP
!
        FCORR=(2.D0*(DEB2-DEBREF)-(DEB-DEBREF))/(2.D0*DT)
!
        DO 113 I=1,NPARF
          FMOT(I)=FMOT(I)-FCORR
 113    CONTINUE
!
        DEB=DEB2
!
      ENDIF
!
! Calcul des forces exterieures
!==============================
! External force computation
!===========================
!
      DO 114 I=1,NPARF
        FEXTX(I)=0.D0
        IF (KMOT .GE.2) FEXTX(I)=FEXTX(I)+FMOT (I)
        IF (KCPAR.NE.1) FEXTX(I)=FEXTX(I)+FPARX(I)
          FEXTZ(I)=0.D0
        IF (KCPAR.NE.1) FEXTZ(I)=FEXTZ(I)+FPARZ(I)
        IF (KGRAV     ) FEXTZ(I)=FEXTZ(I)-GRAV
 114  CONTINUE
!
      RETURN
      END