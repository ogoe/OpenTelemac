!                        *********************
                          SUBROUTINE PASTEMPS
!                        *********************
!
     &   (NPARF, NPMAX, NFLUIDMAX, KLIST, KFLUID, AX   ,
     &    AZ   , DT   , H        , NU0  , NUT   , TEMPS,
     &    VITC0                                        )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! AX, AZ    ! -->  ! PARTICLE ACCELERATION COMPONENTS               !
! ! DT        ! <--  ! TIME STEP                                      !
! ! KFLUID    ! -->  ! FLUID TYPE                                     !
! ! H         ! -->  ! SMOOTHING LENGTH                               !
! ! KLIST     ! -->  ! LOGICAL FOR LISTING PRINTOUT                   !
! ! LNG       ! -->  ! CHOICE INDEX FOR LANGUAGE                      !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
! ! NUT       ! -->  ! EDDY VISCOSITY                                 !
! ! TEMPS     ! <--> ! PHYSICAL TIME                                  !
! ! VITC0     ! -->  ! SPEED OF SOUND                                 !
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
! FONCTION : calcule le pas de temps
! FUNCTION : computes the time step
!
! PROGRAMMES APPELANT : SPARTACUS2D
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
      INTEGER NPMAX, NPARF
      INTEGER I    , J
      INTEGER LNG  , LU
      INTEGER NFLUIDMAX
      COMMON/INFO/LNG,LU
!
      LOGICAL KLIST
!
      DOUBLE PRECISION DTF, DTTEST, DTC  , DT, TEMPS
      DOUBLE PRECISION DTV, VITC0, H
!
      INTEGER KFLUID (NPMAX)
!
      DOUBLE PRECISION AX (NPMAX), AZ (NPMAX), NUT (NPMAX)
!
      DOUBLE PRECISION NU0 (NFLUIDMAX)
!
! Pas de temps
!=============
! Time step
!==========
!
      DTF=0.D0
      DO 111 I=1,NPARF
        DTTEST = (AX(I)**2+AZ(I)**2)
        IF (DTTEST.GE.DTF) DTF=DTTEST
 111  CONTINUE
!
      DTV=0.D0
      DO 211 I=1,NPARF
        J = KFLUID(I)
        DTTEST = NUT(I)+NU0(J)
        IF (DTTEST.GE.DTV) DTV=DTTEST
 211  CONTINUE
!
      DTC=H/VITC0
!
      DT=MIN(0.25D0*SQRT(H/SQRT(DTF)),0.4D0*DTC,0.125D0*H*H/DTV)
      TEMPS=TEMPS+DT
!
      IF (DT.LT.1.0D-32) THEN
        IF (LNG.EQ.1) THEN
            PRINT*,'ERREUR : pas de temps trop petit'
        ELSEIF (LNG.EQ.2) THEN
            PRINT*,'ERROR : time step too tiny'
        ENDIF
        PRINT*,''
        STOP
      ENDIF
!
      IF (KLIST) THEN
        IF (LNG.EQ.1) THEN
          PRINT900,'Pas de temps     :',DT,' s'
          PRINT900,'Temps ecoule     :',TEMPS,' s'
        ELSEIF (LNG.EQ.2) THEN
          PRINT900,'Time step         :',DT,' s'
          PRINT900,'Total time        :',TEMPS,' s'
        ENDIF
      ENDIF
 900  FORMAT (' ',A,E10.3,A)
!
      RETURN
      END