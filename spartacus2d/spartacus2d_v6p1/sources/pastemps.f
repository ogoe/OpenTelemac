C                        *********************
                          SUBROUTINE PASTEMPS
C                        *********************
C     
     .   (NPARF, NPMAX, NFLUIDMAX, KLIST, KFLUID, AX   , 
     .    AZ   , DT   , H        , NU0  , NUT   , TEMPS,
     .    VITC0                                        )
C     
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! AX, AZ    ! -->  ! PARTICLE ACCELERATION COMPONENTS               !
C ! DT        ! <--  ! TIME STEP                                      !
C ! KFLUID    ! -->  ! FLUID TYPE                                     !
C ! H         ! -->  ! SMOOTHING LENGTH                               !
C ! KLIST     ! -->  ! LOGICAL FOR LISTING PRINTOUT                   !
C ! LNG       ! -->  ! CHOICE INDEX FOR LANGUAGE                      !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
C ! NUT       ! -->  ! EDDY VISCOSITY                                 !
C ! TEMPS     ! <--> ! PHYSICAL TIME                                  !
C ! VITC0     ! -->  ! SPEED OF SOUND                                 !
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
C FONCTION : calcule le pas de temps
C FUNCTION : computes the time step
C
C PROGRAMMES APPELANT : SPARTACUS2D
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
      INTEGER NPMAX, NPARF
      INTEGER I    , J
      INTEGER LNG  , LU
      INTEGER NFLUIDMAX
      COMMON/INFO/LNG,LU
C
      LOGICAL KLIST
C
      DOUBLE PRECISION DTF, DTTEST, DTC  , DT, TEMPS
      DOUBLE PRECISION DTV, VITC0, H
C
      INTEGER KFLUID (NPMAX) 
C
      DOUBLE PRECISION AX (NPMAX), AZ (NPMAX), NUT (NPMAX)
C
      DOUBLE PRECISION NU0 (NFLUIDMAX)
C
C Pas de temps
C=============
C Time step
C==========
C
      DTF=0.D0
      DO 111 I=1,NPARF
        DTTEST = (AX(I)**2+AZ(I)**2)
        IF (DTTEST.GE.DTF) DTF=DTTEST
 111  CONTINUE
C
      DTV=0.D0
      DO 211 I=1,NPARF
        J = KFLUID(I)
        DTTEST = NUT(I)+NU0(J)
        IF (DTTEST.GE.DTV) DTV=DTTEST
 211  CONTINUE
C
      DTC=H/VITC0
C
      DT=MIN(0.25D0*SQRT(H/SQRT(DTF)),0.4D0*DTC,0.125D0*H*H/DTV)
      TEMPS=TEMPS+DT
C
      IF (DT.LT.1.0D-32) THEN
        IF (LNG.EQ.1) THEN
            PRINT*,'ERREUR : pas de temps trop petit'
        ELSEIF (LNG.EQ.2) THEN
            PRINT*,'ERROR : time step too tiny'
        ENDIF
        PRINT*,''
        STOP
      ENDIF
C
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
C
      RETURN
      END
