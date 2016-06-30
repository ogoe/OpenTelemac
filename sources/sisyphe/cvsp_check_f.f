!                    *****************************
                     LOGICAL FUNCTION CVSP_CHECK_F
!                    *****************************
!
     &(J,K, SOMETEXT)
!
!***********************************************************************
! SISYPHE   V6P3                                   14/03/2013
!***********************************************************************
!
!brief   CHECKS IF SUM OF FRACTIONS = 1 FOR
!+        A SECTION IN THE VERTICAL SORTING PROFILE
!
!history UWE MERKEL
!+        19/08/2011
!+        V6P2
!+
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX OF A POINT IN MESH
!| K              |<--| INDEX OF A SECTION IN VERTICAL SORTING PROFILE
!| SOMETEXT       |<--| DEBUGING TEXT FOR LOG-OUTPUT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY : NCSIZE
      USE BIEF
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: J
      INTEGER,          INTENT(IN) :: K
      CHARACTER(LEN=10),INTENT(IN) :: SOMETEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION TEMP, AT
      INTEGER I, JG
!
!-----------------------------------------------------------------------
!
      AT = DT*LT/PERCOU
      JG = J
      IF (NCSIZE > 1) JG = MESH%KNOLG%I(J)
!
      CVSP_CHECK_F = .TRUE.
      TEMP = 0.D0
!
!-----------------------------------------------------------------------
!SUM UP AND SLIGHT CORRECTION
!-----------------------------------------------------------------------
!
      DO I=1,NSICLA
        IF (PRO_F(J,K,I).LT.0.D0) THEN
          IF(PRO_F(J,K,I).GE.-1.D-8) THEN
            PRO_F(J,K,I) = 0.D0
          ELSE
            WRITE(LU,*) 'CF:,PRO_F <0: BAD !!'
     &                  ,SOMETEXT,JG,K,I,PRO_F(J,K,I)
          ENDIF
        ENDIF
!
        TEMP = TEMP + PRO_F(J,K,I)
      ENDDO
!
!-----------------------------------------------------------------------
! CORRECT DEVIATIONS
!-----------------------------------------------------------------------
!
      IF(ABS(TEMP-1.D0).GT.0.D0) THEN
        IF(ABS(TEMP-1.D0).GT.1.D-6) THEN
!         STRONG DIFFERENCES ARE CORRECTED BY NORMALIZING ALL FRACTIONS
          CVSP_CHECK_F = .FALSE.
          DO I=1,NSICLA
            IF(PRO_F(J,K,I).GT.0.D0) THEN
              PRO_F(J,K,I) = PRO_F(J,K,I) / TEMP
              EXIT
            ENDIF
          ENDDO
        ELSE
!         SLIGHT DIFFERENCES TO 0 ARE CORRECTED BY CHANGING ONLY
!         THE FIRST FRACTION BIG ENOUGH
          DO I=1,NSICLA
            IF(PRO_F(J,K,I).GT.ZERO) THEN
              PRO_F(J,K,I) = 1.D0-(TEMP-PRO_F(J,K,I))
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION CVSP_CHECK_F

