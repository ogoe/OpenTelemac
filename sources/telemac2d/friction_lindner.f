!                    ***************************
                     SUBROUTINE FRICTION_LINDNER
!                    ***************************
!
     &(VA,HA,CF,VK,G,DP,SP,CP)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES FRICTION COEFFICIENT FOR NON-SUBMERGED
!+                VEGETATION FROM PARAMETERS.
!
!history  MICHAEL SCHROEDER, BAW
!+        **/11/1992
!+
!+   THE ALGORITHM WAS DEVELOPED BY LINDNER (1982) AND PASCHE
!
!history  F. HUVELIN
!+        20/04/2004
!+
!+   WRITTEN FROM THE C++ PROGRAM: RISMO2D OF THE BAW
!
!history  J-M HERVOUET (LNHE)
!+
!+        V5P5
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| FRICTION COEFFICIENT FOR BOTTOM ROUGHNESS
!| CP             |<--| FRICTION COEFFICIENT FOR NON-SUBMERGED VEGETATION
!| DP             |-->| DIAMETER OF ROUGHNESS ELEMENT
!| G              |-->| GRAVITY ACCELERATION
!| HA             |-->| FLOW DEPTH
!| SP             |-->| SPACING OF ROUGHNESS ELEMENT
!| VA             |-->| VELOCITY
!| VK             |-->| KINEMTIC VISCOSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: VA,HA,CF,VK,G,DP,SP
      DOUBLE PRECISION, INTENT(OUT) :: CP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          PARAMETER :: KMAXITER = 200
      DOUBLE PRECISION, PARAMETER :: KPRECISION = 1.0D-3
      INTEGER                     :: CWRAV
      INTEGER                     :: CWRMAX
      INTEGER                     :: CWRCOUNT
      INTEGER                     :: ANLAV
      INTEGER                     :: ANLMAX
      INTEGER                     :: ANLCOUNT
      INTEGER                     :: ITERR
!
      INTEGER :: I, J
      INTEGER :: ICWR               ! ITERATION COUNTER: CWR
      INTEGER :: IANL               ! ITERATION COUNTER: ANL
      INTEGER :: REALROOTS
      LOGICAL :: LCWR
!
      DOUBLE PRECISION :: CW, CWR, RCWR, DCWR, ANL, ANB, RANL
      DOUBLE PRECISION :: CWR1, CWR2, DCWR1, DCWR2
      DOUBLE PRECISION :: LAMBDA, FR
      DOUBLE PRECISION :: X(3), VRATIO, HRATIO
      DOUBLE PRECISION :: ALFA, ACOF, BCOF, CCOF, DCOF
!
      DOUBLE PRECISION :: TMP1, TMP2
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      LCWR     = .TRUE.
      CWRAV    = 0
      CWRMAX   = 0
      CWRCOUNT = 0
      ANLAV    = 0
      ANLMAX   = 0
      ANLCOUNT = 0
      ITERR    = 0
      IF ((DP < 1.0E-3)     .OR.(SP < 1.0E-2).OR.
     &    (ABS(VA) < 1.0E-3).OR.(HA < 1.0E-3)     ) THEN
        CP = 0.D0
      ELSE
        ! INITIALIZATION
        ! --------------
        CWR   = 1.0      ! DRAG COEFFICIENT
        ANL   = SP/2.0   ! WAKE LENGTH OF A CYLINDER
        CWR1  = 1.0
        CWR2  = 1.0
        DCWR1 = 0.0
        DCWR2 = 0.0
        ! START OF ITERATION FOR CWR
        ! --------------------------
        DO ICWR = 1, KMAXITER
          ! SUPERPOSED FRICTION COEFFICIENT
          ! -------------------------------
          LAMBDA = 8.0D0*CF  +  4.0D0*CWR*HA*DP/SP/SP
          ! DRAG COEFFICIENT CW FOR ONE CYLINDER
          ! ------------------------------------
          CALL DRAGCOEFF(VA, DP, VK, CW)
          ! WAKE LENGTH OF A CYLINDER (ITERATIVE COMPUTATION)
          ! -------------------------------------------------
          DO J=1, KMAXITER
            TMP1 = 1.0D0  +  ANL*LAMBDA/4.0D0/HA
            TMP2 = 30.0D0/ABS(TMP1)**(1.5)
            RANL = CW*DP*ABS(TMP2)**(1.429)
            ! TEST FOR CONVERGENCE
            ! --------------------
            IF (ABS((RANL-ANL)/RANL) < KPRECISION) THEN
              ANL  = RANL
              IANL = -1*J
              EXIT
            ENDIF
            ANL = 0.5 * (RANL + ANL)
          ENDDO
          ! STATISTICS OF CWR ITERATION
          ! ---------------------------
          IF ( IANL > 0 ) THEN
            ANL = SP/2.0D0
          ELSE
            IANL = ABS(IANL)
            ANLCOUNT = ANLCOUNT + 1
            ANLAV = IANL + ANLAV
            IF (IANL > ANLMAX) ANLMAX = IANL
          ENDIF
          ! WAKE WIDTH
          ! ----------
          ANB = 0.24 * ABS(ANL)**(0.59) * ABS(CW*DP)**(0.41)
          ! RATIO OF VELOCITY IN FRONT OF AND BEHIND CYLINDER
          ! -------------------------------------------------
          VRATIO = 1.151 * ABS(ANL/SP)**(-0.483)
     &           +   0.5 * ABS(ANB/SP)**(1.1)
          ! RATIO OF FLOW DEPTH
          ! -------------------
          FR = VA / SQRT( G * HA ) ! FROUDE NUMBER
          ALFA = DP / SP
          ACOF =  FR * FR * (1.0D0 - ALFA * CWR/2.0D0)
          BCOF = -FR * FR - (1.0D0 - ALFA) / 2.0D0
          CCOF =  0.0D0
          DCOF = (1.0D0 - ALFA) / 2.0D0
          HRATIO = 1.0D0
          IF (ABS(ACOF) < 1.0E-10) THEN
            HRATIO = SQRT( -DCOF / BCOF)
          ELSE
            CALL CUBEEQUATION(ACOF, BCOF, CCOF, DCOF, REALROOTS, X)
            DO I = 1, REALROOTS
              IF (X(I) > 0.0  .AND.  X(I) < 1.0)  THEN
                HRATIO = X(I)
                EXIT
              ENDIF
            ENDDO
          ENDIF
          ! REVISE DRAG COEFFICIENT CWR
          ! ---------------------------
          RCWR = 1.3124D0*CW*VRATIO + 2.0D0*(1.0D0-HRATIO)/FR/FR
          ! TEST FOR CONVERGENCE
          ! --------------------
          IF ( ABS((RCWR-CWR)/RCWR) < KPRECISION ) THEN
            LCWR = .FALSE.
!           ICWR = -1/ICWR
            EXIT
          ENDIF
          ! USE PEGASUS ALGORITHM FOR CWR ITERATION
          ! ---------------------------------------
          DCWR = RCWR - CWR
          IF ((ICWR >= 3) .AND. (DCWR1*DCWR2 < 0.0D0)) THEN
            IF (DCWR2*DCWR < 0.0D0) THEN
              DCWR1 = DCWR2/(DCWR2+DCWR)*DCWR1
            ELSE
              CWR1  = CWR2
              DCWR1 = DCWR2
            ENDIF
            CWR2  = CWR
            DCWR2 = DCWR
            CWR   = CWR2 - DCWR2*(CWR2-CWR1)/(DCWR2-DCWR1)
          ELSE
            CWR1 = CWR2
            DCWR1 = DCWR2
            CWR2 = CWR
            DCWR2 = DCWR
            IF ((ICWR >= 2) .AND. (DCWR1*DCWR2 < 0.0 )) THEN
              CWR = CWR2 - DCWR2*(CWR2-CWR1)/(DCWR2-DCWR1)
            ELSE
              CWR = RCWR
            ENDIF
          ENDIF
        ENDDO !ICWR = 1, KMAXITER
!
        IF (LCWR) THEN
          ITERR = ITERR + 1
          CP = -1.D0
        ELSE
          ! STATISTICS OF CWR ITERATION
          ! ---------------------------
          ICWR = -1/ICWR ! AS THE PROGRAM RISMO2D FROM THE BAW
          ICWR = -ICWR
          CWRCOUNT = CWRCOUNT + 1
          CWRAV = ICWR + CWRAV
          IF (ICWR > CWRMAX) CWRMAX = ICWR
          CP = LAMBDA/8.0D0 - CF
        ENDIF
      ENDIF
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END
