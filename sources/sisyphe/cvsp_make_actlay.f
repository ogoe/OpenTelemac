!                    ***************************
                     SUBROUTINE CVSP_MAKE_ACTLAY
!                    ***************************
!
!
!***********************************************************************
! SISYPHE   V6P3                                   12/02/2013
!***********************************************************************
!
!brief  BUILD A NEW ACTIVE LAYER WITH DATA FROM VERTICAL SORTING PROFILE
!+      AND A NEW ACTIVE STRATUM WITH DATA FROM VERTICAL SORTING PROFILE
!
!
!history U.MERKEL & REBEKKA KOPMANN
!+        2012
!+        V6P2
!+
!
!history PABLO TASSI PAT (EDF-LNHE)
!+        12/02/2013
!+        V6P3
!+ PREPARING FOR THE USE OF A HIGHER NSICLM VALUE
!+ (BY REBEKKA KOPMANN)
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELAY0              |--| ALTHICKNESS INIT
!| AVAIL(J,K,I)       |--| FRACTIONS
!| ELAY%R(J)          |--| ALTHICKNESS REAL FOR POINT(J)
!| ESTRAT%R(J)        |--| ASTHICKNESS REAL FOR POINT(J)
!| ES(J,K)            |--| LAYERTHICKNESS FOR POINT(J) / LAYER(K)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE BIEF_DEF
      USE BIEF
      USE DECLARATIONS_SISYPHE

      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL DB
      INTEGER  I,J,K,M,JG, II
      DOUBLE PRECISION TEMP, Z_HIGH, Z_LOW, CVSP_INTEGRATE_VOLUME, AT
      DOUBLE PRECISION ASUM, NEW_ALT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, EXTERNAL :: CVSP_ALT
      DOUBLE PRECISION SUMES, SUMAV, ALT
      CHARACTER(LEN=30) DEBUGFILE
!
!-----------------------------------------------------------------------
!
      AT = DT*LT/PERCOU
!
      DO J=1,NPOIN
        NLAYER%I(J) = NOMBLAY
        NEW_ALT =  CVSP_ALT(J,ALT_MODEL)
!
        IF(NEW_ALT.GT.(ZF%R(J)-ZR%R(J))) THEN
          NEW_ALT = (ZF%R(J)-ZR%R(J))
        ENDIF
!
        SUMES = 0.D0
        DO K=1,1               !NLAYER%I(J)
          IF(K.EQ.1) THEN
            ALT = NEW_ALT
          ELSE
            ALT = ELAY0
          ENDIF
          SUMES = SUMES + ALT
!
!-----------------------------------------------------------------------
! ALT DOESN'T USE THE FULL DEPTH
!-----------------------------------------------------------------------
!
          IF((ZF%R(J)-ZR%R(J)).GE.NEW_ALT) THEN
!
!-----------------------------------------------------------------------
! SUMES < FULL DEPTH
!-----------------------------------------------------------------------
!
            IF(SUMES.LE.(ZF%R(J)-ZR%R(J))) THEN
              IF(K.EQ.1) THEN
                ES(J,K) = NEW_ALT
              ELSE
                IF(((ZF%R(J)-ZR%R(J))-SUMES).GT.2.D0*ELAY0) THEN
                  ES(J,K) = ELAY0
                ENDIF
                IF(((ZF%R(J)-ZR%R(J))-SUMES).LT.2.D0*ELAY0) THEN
                   ES(J,K) = (ZF%R(J)-ZR%R(J)) - SUMES
                  NLAYER%I(J) = K
                ELSE
                  ES(J,K) = 0.D0
                ENDIF
              ENDIF         ! K=1

              IF (K == 1) THEN
                Z_HIGH = PRO_D(J,PRO_MAX(J),1)
              ELSE
                Z_HIGH = PRO_D(J,PRO_MAX(J),1) - SUMES + ALT
                IF (Z_HIGH.GT.PRO_D(J,PRO_MAX(J),1)) THEN
                  Z_HIGH=PRO_D(J,PRO_MAX(J),1)
                ENDIF
              ENDIF

              Z_LOW =  Z_HIGH - ES(J,K)
!
!-----------------------------------------------------------------------
! THIS IS THE CORE!
!-----------------------------------------------------------------------
!
              ASUM = 0.D0
              TEMP = CVSP_INTEGRATE_VOLUME(J,1,Z_HIGH,Z_LOW,T1%R)
!
!-----------------------------------------------------------------------
! ASSIGN FRACTIONS
!-----------------------------------------------------------------------
!
              DO I=1,NSICLA
                AVAIL(J,K,I) = T1%R(I) / ES(J,K)
                ASUM = AVAIL(J,K,I) + ASUM

                IF ((AVAIL(J,K,I)>1+ZERO)) THEN
                  WRITE(LU,*) "MAKE_AL_", J, K, I, AT,
     &                 AVAIL(J,K,I), T1%R(I), ES(J,K),Z_HIGH,Z_LOW
     &                 , ES(J,K)
                  WRITE(LU,*) "ES,ALT,ELAY0,NEWALT",ES(J,K),ALT
     &                 ,ELAY0,NEW_ALT
                  CALL PLANTE(1)
                ENDIF
              ENDDO
!
!-----------------------------------------------------------------------
! TRUNCATION ERRORS NORMALIZED FOR ALL FRACTIONS
!-----------------------------------------------------------------------
!
              IF(ABS(ASUM - 1.D0).NE.0.D0) THEN
                DO I=1,NSICLA
                  AVAIL(J,K,I) = AVAIL(J,K,I) / ASUM
                END DO
              ENDIF

            ELSE
              WRITE(LU,*) 'UHM_NOW_OBSOLETE',J, K, I, AT
              CALL PLANTE(1)
              STOP
!
              ES(J,K) = MAX(-1.D0*(SUMES-(ZF%R(J)-ZR%R(J))), 0.D0)
!
              IF(ES(J,K).EQ.0.D0) THEN
                DO I=1,NSICLA
                  AVAIL(J,K,I) = 1.D0 / NSICLA
                ENDDO
              ELSE
                IF(DB(JG,0)) CALL CVSP_P('./ERR/','IVES3_',JG)
!
!-----------------------------------------------------------------------
! ASSIGN Z-COORDINATE FOR UPPER AND LOWER BOUNDARY OF LAYER
!-----------------------------------------------------------------------
!
                Z_LOW = PRO_D(J,PRO_MAX(J),1)
                DO M=1,K
                  Z_HIGH = Z_LOW
                  Z_LOW = Z_LOW - ES(J,M)
                ENDDO
!
                IF(DB(JG,0)) WRITE(LU,*) 'UHM_L_',K,I
                TEMP=CVSP_INTEGRATE_VOLUME(J,1,Z_HIGH,Z_LOW,T1%R)
!
                DO I=1,NSICLA
                  AVAIL(J,K,I) = T1%R(I) / ES(J,K)
                  ASUM = AVAIL(J,K,I) + ASUM
!
                  IF ((AVAIL(J,K,I)>1+ZERO)) THEN
                    WRITE(LU,*) "MAKE_AL_", J, K, I, AT,
     &                   AVAIL(J,K,I), T1%R(I), ES(J,K)
     &                   ,Z_HIGH,Z_LOW
                  ENDIF
                END DO            ! NSICLA

                IF(ABS(ASUM - 1) > ZERO) THEN
                  WRITE(LU,*) "MAKE_AL_ABS2", J, K, I, AT,
     &                 AVAIL(J,K,I), TEMP, ES(J,K),Z_HIGH,Z_LOW,ASUM
!
                  DO I=1,NSICLA
                    AVAIL(J,K,I) = AVAIL(J,K,I) / ASUM
                  ENDDO
                ENDIF
              ENDIF     ! ES(J,K).EQ.0.D0
            ENDIF    ! SUMES.LE.(ZF%R(J)-ZR%R(J))
!
          ELSE
            ES(J,K) = 0.D0
            ES(J,1) = (ZF%R(J)-ZR%R(J))
            NLAYER%I(J) = 1
            DO I=1,NSICLA
              AVAIL(J,K,I) = 1.D0 / NSICLA
            ENDDO
!
          ENDIF ! (ZF%R(J)-ZR%R(J)).GE.NEW_ALT
!
        IF (K == 1) ELAY%R(J) = ES(J,K)
        IF (K == 2) ESTRAT%R(J) = ES(J,K)
!
!-----------------------------------------------------------------------
! STOPS IF THE NUMBER OF LAYERS IS REDUCED DURING CALCULATION
!-----------------------------------------------------------------------
!
        IF(NLAYER%I(J) == K) EXIT
!
      ENDDO                     ! K
!
!-----------------------------------------------------------------------
! NOT ENOUGH SPACE FOR NLAYER
!-----------------------------------------------------------------------
!
      DO K=1,1                  !NLAYER%I(J)
        IF(ES(J,K).EQ.0.D0) THEN
          WRITE(LU,*) 'NOT ENOUGH SPACE FOR NLAYER',J
          WRITE(LU,*) 'POSSIBLE ERROR! RIGID BED? ',NLAYER%I(J)-1
          NLAYER%I(J) = NLAYER%I(J)-1
          EXIT
        ENDIF
      END DO
!
      ENDDO                     ! J
!
!-----------------------------------------------------------------------
! CHECKS
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
        SUMES = 0.D0
        JG = J
        IF (NCSIZE > 1) JG = MESH%KNOLG%I(J)
        DO K=1,9
          SUMES = SUMES+ES(J,K)
          SUMAV = 0.D0
          DO I=1,NSICLA
            SUMAV=AVAIL(J,K,I)+SUMAV
            IF((AVAIL(J,K,I).GT.1.D0+1.D-12).OR.
     &          (AVAIL(J,K,I).LT.0.D0-1.D-12)) THEN
              WRITE(LU,*) 'ERROR AVAIL',J,K,I,AVAIL(J,K,I)
              CALL CVSP_P('./ERR/','ERR_AVAIL_VSP_',  JG)
              CALL LAYERS_P('./ERR/ERR_AVAIL_LAY_', JG)
              CALL PLANTE(1)
            ENDIF
          ENDDO              !I
!
          IF((ABS(SUMAV-1.D0).LT.1.D-12).AND.
     &        (ABS(SUMAV-1.D0).GT.0.D0)) THEN
            AVAIL(J,K,1) = AVAIL(J,K,1) - (1.D0 - SUMAV)
          ENDIF
!
          IF(ABS(SUMAV-1.D0).GT.1.D-12) THEN
            IF(ABS(SUMAV-1.D0).GT.1.D-4) THEN
              WRITE(LU,*) 'ERROR SUMAV TOO BAD:',J,K,SUMAV
              WRITE(UNIT=DEBUGFILE, FMT='(A,I4,A)')
     &             './ERR/','SUMAV_',JG,'_VSP_'
              WRITE(UNIT=DEBUGFILE, FMT='(A,I4,A)')
     &             './ERR/','SUMAV_',JG,'_LAY_'
              DO II=1,NSICLA
                AVAIL(J,K,II) = AVAIL(J,K,II) / SUMAV
              ENDDO
            ENDIF
          ENDIF
        ENDDO                 !K
!
        IF( ABS(SUMES-(ZF%R(J)-ZR%R(J))).LT.ZERO) THEN
          WRITE(LU,*) 'ERROR SUM ES',J,SUMES,ZF%R(J)-ZR%R(J)
          CALL CVSP_P('./ERR/','ERR_SUM_ES_VSP_', JG)
          CALL LAYERS_P('./ERR/ERR_SUM_ES_LAY_', JG)
          CALL PLANTE(1)
        ENDIF
!
      ENDDO                    !J
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_MAKE_ACTLAY
