!                    *******************
                     SUBROUTINE IDWM_T2D
!                    *******************
!
     &(ELEV,POINTS,IDWM,NPOIN,NUMSTA)
!
!***********************************************************************
! TELEMAC2D   V7P0
!***********************************************************************
!
!brief    USES INVERSE DISTANCE WEIGHTING METHOD TO COMPUTE A WIND FIELD
!+               THAT VARIES IN TIME AND SPACE
!
!history  P. PRODANOVIC (RIGGS ENGINEERING LTD)
!+        23/04/2014
!+        V7P0
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        16/02/2015
!+        V7P0
!+   Managing the divisions by 0 + optimization.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VARIABLES THAT ARE USED BY THE SUBROUTINE TO PASS DATA IN AND OUT
      INTEGER,INTENT(IN) :: NPOIN, NUMSTA
      DOUBLE PRECISION, INTENT(IN) :: POINTS(NPOIN,2),ELEV(NUMSTA,3)
      DOUBLE PRECISION, INTENT(INOUT) :: IDWM(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N, M, I, J, K
!     WEIGHTS, DENOMINATOR, DISTANCE
      DOUBLE PRECISION W1, W2, W3, W4, DEN, DIST
!     CURRENT MINS
      DOUBLE PRECISION MIN1CUR, MIN2CUR, MIN3CUR, MIN4CUR
!     LOCATIONS OF THE MININIMS (USED FOR ARRAY REFERENCING)
      INTEGER MIN1LOC, MIN2LOC, MIN3LOC, MIN4LOC
!
!-----------------------------------------------------------------------
!
!     DEFINE N AND M, THIS IS DONE TO HARMONIZE PASTING OF CODE
      N = NUMSTA
      M = NPOIN
!
!     MAIN LOOP TO DO THE INTERPOLATION
!
      DO J = 1, M
!
!       SET MIN_PREV VARS TO LARGE NUMBERS
!
        MIN1CUR = 1.D30
        MIN2CUR = 1.D30
        MIN3CUR = 1.D30
        MIN4CUR = 1.D30
!
!       INITIALIZE MIN_LOC
!
        MIN1LOC = -1
        MIN2LOC = -1
        MIN3LOC = -1
        MIN4LOC = -1
!
        DO I = 1, N
!
          IF(ELEV(I,3).GT.0.D0) THEN
!
            DIST=(ELEV(I,1)-POINTS(J,1))**2+(ELEV(I,2)-POINTS(J,2))**2
!
!           FIND MIN DIST IN EACH OF THE FOUR QUADRANTS
!           2 | 1
!           -----
!           3 | 4
!
!          QUADRANT 1
            IF(ELEV(I,1).GE.POINTS(J,1).AND.
     &         ELEV(I,2).GE.POINTS(J,2)      ) THEN
              IF(DIST.LT.MIN1CUR) THEN
                MIN1CUR = DIST
                MIN1LOC = I
              ENDIF
            ENDIF
!           QUADRANT 2
            IF(ELEV(I,1).LT.POINTS(J,1).AND.
     &         ELEV(I,2).GE.POINTS(J,2)      ) THEN
              IF(DIST.LT.MIN2CUR) THEN
                MIN2CUR = DIST
                MIN2LOC = I
              ENDIF
            ENDIF
!           QUADRANT 3
            IF(ELEV(I,1).LT.POINTS(J,1).AND.
     &         ELEV(I,2).LT.POINTS(J,2)      ) THEN
              IF(DIST.LT.MIN3CUR) THEN
                MIN3CUR = DIST
                MIN3LOC = I
              ENDIF
            ENDIF
!           QUADRANT 4
            IF(ELEV(I,1).GT.POINTS(J,1).AND.
     &         ELEV(I,2).LT.POINTS(J,2)      ) THEN
              IF(DIST.LT.MIN4CUR) THEN
                MIN4CUR = DIST
                MIN4LOC = I
              ENDIF
            ENDIF
!
          ENDIF
!
        ENDDO
!
!       CALCULATE WEIGHTS
!
!       AVOIDING DIVISIONS BY 0
!
        MIN1CUR=MAX(MIN1CUR,1.D-6)
        MIN2CUR=MAX(MIN2CUR,1.D-6)
        MIN3CUR=MAX(MIN3CUR,1.D-6)
        MIN4CUR=MAX(MIN4CUR,1.D-6)
!
        DEN = 1.D0/MIN1CUR+1.D0/MIN2CUR+1.D0/MIN3CUR+1.D0/MIN4CUR
!
!       IN CASE WHEN ALL INPUT DATA IS MISSING, ALSO OUTPUT MISSING
!
        IF(MIN1LOC.EQ.-1.AND.MIN2LOC.EQ.-1.AND.
     &     MIN3LOC.EQ.-1.AND.MIN4LOC.EQ.-1      ) THEN
          IDWM(J) = -999.D0
        ELSE
          IDWM(J) = 0.D0
          IF(MIN1LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN1LOC,3)/MIN1CUR
          ENDIF
          IF(MIN2LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN2LOC,3)/MIN2CUR
          ENDIF
          IF(MIN3LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN3LOC,3)/MIN3CUR
          ENDIF
          IF(MIN4LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN4LOC,3)/MIN4CUR
          ENDIF
          IDWM(J) = IDWM(J)/DEN
        ENDIF
!
      ENDDO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END

