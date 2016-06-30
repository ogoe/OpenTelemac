!            ***********************************************
             DOUBLE PRECISION FUNCTION CVSP_INTEGRATE_VOLUME
!            ***********************************************
!
     &(J,I,Z_HIGH,Z_LOW,A)
!
!***********************************************************************
! SISYPHE   V6P3                                   14/03/2013
!***********************************************************************
!
!brief   INTEGRATES THE VOLUME OF A FRACTION WITHIN THE
!+       VERTICAL SORTING PROFIL BETWEEN 2 DEPTH Z-COORDINATES Z_HIGH & Z_LOW
!+
!
!history UWE MERKEL
!+        2011
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
!| I              |<--| INDEX OF A FRACTION IN VERTICAL SORTING PROFILE
!| Z_HIGH         |<--| HIGHER DEPTH COORDINATE
!| Z_LOW          |<--| LOWER  DEPTH COORDINATE
!| A1...A10       |<--| INTEGRATED VOLUMES PER FRACTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)  :: J
      INTEGER,          INTENT(IN)  :: I
      DOUBLE PRECISION, INTENT(IN)  :: Z_HIGH
      DOUBLE PRECISION, INTENT(IN)  :: Z_LOW
      DOUBLE PRECISION, INTENT(OUT) :: A(10)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION TEMP,DHIG, DLOW, AT, SUMUP, FLOW, FHIG
      DOUBLE PRECISION TEMP2, TEMP2MAX, SUMUP2,TEMP3, TEMP3MAX, SUMUP3
      DOUBLE PRECISION CORRECT, CHSUM
      INTEGER L_CNT, MYCASE, F_CNT, REVCNT, HELPER, LASTCASE, JG, K
      LOGICAL RET,CVSP_CHECK_F
!
!-----------------------------------------------------------------------
!
      JG = J
      IF (NCSIZE.GT.1) JG = MESH%KNOLG%I(J)
!
      AT = DT*LT/PERCOU
      SUMUP = 0.D0
      SUMUP2  = 0.D0
      SUMUP3  = 0.D0
      MYCASE  = 0
!
!-----------------------------------------------------------------------
! DOING ALL FRACTIONS
!-----------------------------------------------------------------------
!
      DO F_CNT = 1, NSICLA
        TEMP = 0.D0
        TEMP2 = 0.D0
        TEMP2MAX = 0.D0
        TEMP3 = 0.D0
        TEMP3MAX = 0.D0
        CHSUM = 0.D0
!
!-----------------------------------------------------------------------
! GOING THROUGH ALL SECTIONS
!-----------------------------------------------------------------------
!
        DO L_CNT = 0,(PRO_MAX(J)-2)
!
          REVCNT = PRO_MAX(J)-L_CNT
!
!-----------------------------------------------------------------------
! DEPTH COORDINATES OF THE SECTION TO CHECK
!-----------------------------------------------------------------------
!
          DHIG = PRO_D(J,REVCNT,F_CNT)
          DLOW = PRO_D(J,REVCNT-1,F_CNT)
!
!-----------------------------------------------------------------------
! SORTING PROFILE SECTION TOTALLY INSIDE (CASE ZDDZ)
!-----------------------------------------------------------------------
!
          IF ( (DHIG <= Z_HIGH ) .AND.
     &        (DLOW >= Z_LOW  ) ) THEN

            FLOW = PRO_F(J,REVCNT-1,F_CNT)
            FHIG = PRO_F(J,REVCNT,F_CNT)
!
            MYCASE  = MYCASE  + 1
            LASTCASE = 1
!
            TEMP2 = 0.5D0*(FHIG+FLOW)*(DHIG-DLOW)  + TEMP2
            TEMP2MAX = Z_HIGH - DLOW
!
            DO HELPER = 1, NSICLA
              CHSUM = PRO_F(J,REVCNT, HELPER) + CHSUM
            ENDDO
!
            CHSUM = 1.D0 - CHSUM
!
!-----------------------------------------------------------------------
! SORTING PROFILE SECTION PARTIALLY LOWER (CASE ZDZD)
!-----------------------------------------------------------------------
!
          ELSEIF ((DHIG <= Z_HIGH) .AND.
     &            (DHIG >  Z_LOW ) .AND.
     &            (DLOW <  Z_LOW ) )  THEN

            FHIG = PRO_F(J,REVCNT,F_CNT)
            FLOW = PRO_F(J,REVCNT-1,F_CNT)
            FLOW = - ((FHIG-FLOW)/(DHIG-DLOW))*(DHIG-Z_LOW) + FHIG
!
!-----------------------------------------------------------------------
! CUT THE SECTION
!-----------------------------------------------------------------------
!
            DLOW = Z_LOW
!
            MYCASE  = MYCASE  + 1000
            LASTCASE = 2
!
            TEMP3 = 0.5D0*(FHIG+FLOW)*(DHIG-DLOW)  + TEMP3
            TEMP3MAX = DHIG - DLOW
!
!-----------------------------------------------------------------------
! SORTING PROFILE SECTION PARTIALLY HIGHER (CASE DZDZ)
!-----------------------------------------------------------------------
!
          ELSEIF ((DHIG > Z_HIGH) .AND.
     &            (DLOW >= Z_LOW) .AND.
     &            (DLOW <  Z_HIGH) ) THEN
!
            FLOW = PRO_F(J,REVCNT-1,F_CNT)
!
            FHIG = PRO_F(J,REVCNT,F_CNT)
            FHIG = ((FHIG-FLOW)/(DHIG-DLOW))*(Z_HIGH-DLOW) + FLOW
!
!-----------------------------------------------------------------------
!INSERT
!-----------------------------------------------------------------------
!
!CUT THE SECTION
            DHIG = Z_HIGH
!
            MYCASE  = MYCASE + 1000000
            LASTCASE = 3
!
!-----------------------------------------------------------------------
! LAYER TOTALLY INSIDE ONE SECTION (CASE DZZD)
!-----------------------------------------------------------------------
!
          ELSEIF ((DHIG >= Z_HIGH) .AND.
     &            (DLOW <= Z_LOW) ) THEN
!
            FHIG =
     &           - (PRO_F(J,REVCNT,F_CNT)-PRO_F(J,REVCNT-1,F_CNT)) /
     &           (DHIG-DLOW) *
     &           (DHIG-Z_HIGH)
     &           + PRO_F(J,REVCNT,F_CNT)
!
            FLOW =
     &           - (PRO_F(J,REVCNT,F_CNT)-PRO_F(J,REVCNT-1,F_CNT)) /
     &           (DHIG-DLOW) *
     &           (DHIG-Z_LOW)
     &           + PRO_F(J,REVCNT,F_CNT)
!
!-----------------------------------------------------------------------
! CUT THE SECTION
!-----------------------------------------------------------------------
!
            DHIG = Z_HIGH
            DLOW = Z_LOW
!
            LASTCASE = 8
            MYCASE  = MYCASE + 100000000
!
!-----------------------------------------------------------------------
! SECTION WITH 0 STRENGTH
!-----------------------------------------------------------------------
!
          ELSEIF (DHIG == DLOW) THEN
            FLOW = 0.D0
            FHIG = 0.D0
!
            MYCASE  = MYCASE + 1000000000
            LASTCASE = 4
!
!A TRUE BUG!
          ELSEIF (Z_LOW > Z_HIGH) THEN
            FLOW = 0.D0
            FHIG = 0.D0
!
            WRITE(LU,*) 'Z_LOW >= Z_HIGH', DHIG, DLOW, Z_HIGH, Z_LOW
            CALL CVSP_P('./','ZLOHI',JG)
            MYCASE  = MYCASE + 1000000000
            LASTCASE = 5
!
!A TRUE BUG!
          ELSEIF (DHIG < DLOW) THEN
            FLOW = 0.D0
            FHIG = 0.D0
!
            WRITE(LU,*) 'DHIG <= DLOW', J,DHIG, DLOW, Z_HIGH, Z_LOW
            CALL CVSP_P('./','DLOHI',JG)
            MYCASE  = MYCASE + 1000000000
            LASTCASE = 6
!
!A SECTION THAT IS NOT INVOLVED / NOT A BUG!!
          ELSE
!
            FLOW = 0.D0
            FHIG = 0.D0
!
            MYCASE  = MYCASE + 1000000000
            LASTCASE = 7
!
          ENDIF
!
!-----------------------------------------------------------------------
! TRAPEZOID FORMULA
!-----------------------------------------------------------------------
!
          TEMP = 0.5D0*(FHIG+FLOW)*(DHIG-DLOW) + TEMP

!DEBUG!DEBUG!DEBUG!DEBUG!DEBUG!DEBUG
          IF (0.5D0*(FHIG+FLOW)*(DHIG-DLOW) < 0.D0) THEN
            WRITE(LU,FMT='(A,1X,2(I11,1X),11(G20.10,1X),1X,I11)')
     &           'INTEGRATE_VOL_ER_TMP:<0:'
     &           ,JG, I, AT, FHIG,FLOW,DHIG,DLOW, DHIG-DLOW, REVCNT,
     &           PRO_F(J,REVCNT-1,F_CNT),PRO_F(J,REVCNT,F_CNT),
     &           PRO_D(J,REVCNT-1,F_CNT),PRO_D(J,REVCNT,F_CNT),
     &           LASTCASE
            CALL CVSP_P('./','IVKT',JG)
            CALL PLANTE(1)
          ENDIF
!DEBUG!DEBUG!DEBUG!DEBUG!DEBUG!DEBUG
!
        ENDDO                 !SECTION
!
!-----------------------------------------------------------------------
! ADDING UP FRACTIONS FOR DEBUGGING PURPOSES
!-----------------------------------------------------------------------
!
        SUMUP = TEMP + SUMUP
        SUMUP2 = TEMP2 + SUMUP2
        SUMUP3 = TEMP3 + SUMUP3
!
        A(F_CNT) = TEMP
!
      ENDDO                    !FRACTION
!
      CORRECT = (SUMUP / (Z_HIGH-Z_LOW))
      CVSP_INTEGRATE_VOLUME = A(I) / CORRECT
!
!DEBUG!DEBUG!DEBUG!DEBUG!DEBUG!DEBUG
      IF (CVSP_INTEGRATE_VOLUME < 0.D0) THEN
        CALL CVSP_P('./','IVK0',JG)
        WRITE(*,FMT='(A,2(I11),14(G20.10))')'INTEGRATE_VOL_ER:<0:'
     &       ,JG, I, AT, CVSP_INTEGRATE_VOLUME, MYCASE ,Z_HIGH,Z_LOW,
     &       SUMUP,SUMUP2,SUMUP3,CHSUM,A(1),A(2),A(3),A(4),A(5)
        CALL PLANTE(1)
      ENDIF
!DEBUG!DEBUG!DEBUG!DEBUG!DEBUG!DEBUG
!
!-----------------------------------------------------------------------
! CHECKSUM OVER ALL FRACTIONS AND LAYERS
!-----------------------------------------------------------------------
!
      SUMUP  = SUMUP  - ABS(Z_HIGH-Z_LOW)
      SUMUP2 = SUMUP2 - ABS(TEMP2MAX)
      SUMUP3 = SUMUP3 - ABS(TEMP3MAX)
      CHSUM =  CHSUM / 5.D0
!
!-----------------------------------------------------------------------
! BEWARE: 1.D-5 IS UP TO 10G OF A 1000KG BUT HIGHER ACCURACY
! LEADS AGAIN TO FLOATING POINT TRUNCATION ERRORS ...
!-----------------------------------------------------------------------
!
      IF (ABS(SUMUP).GT.1.D-5) THEN
        CALL CVSP_P('./','IV_E',JG)
        WRITE(LU,*) 'INTEGRATE VOLUME ACCURRACY!!!', SUMUP, JG
        DO K = 1, PRO_MAX(J)
!
!-----------------------------------------------------------------------
! REMOVES NUMERIC INSTABILITIES
!-----------------------------------------------------------------------
!
          RET =  CVSP_CHECK_F(J,K,' IV_FIX:   ')
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION CVSP_INTEGRATE_VOLUME
