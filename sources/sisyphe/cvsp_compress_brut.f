!                   ********************************
                    SUBROUTINE CVSP_COMPRESS_BRUT(J)
!                   ********************************
!
!
!***********************************************************************
! SISYPHE   V6P3                                   12/03/2013
!***********************************************************************
!
!brief    COMPRESSES A VERTICAL SORTING PROFILE IN POINT J TO PREVENT
!+        EXTENSIV GROTH OF SECTION / NODE NUMBERS
!+
!+        BRUTAL VERSION
!+        IN CASE OF EMERGENCY, IF NO OTHER ALGORITHM IS ALLOWED TO COMPRESS,
!+        TO PREVENT PRO_MAX_MAX OVERFLOW
!
!history UWE MERKEL
!+        23/12/2011
!+        V6P2
!+
!
!history PABLO TASSI PAT (EDF-LNHE)
!+        12/02/2013
!+        V6P3
!+ PREPARING FOR THE USE OF A HIGHER NSICLM VALUE
!+ (BY REBEKKA KOPMANN)
!+
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX OF A POINT IN MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: J
      DOUBLE PRECISION Z_LOW ,Z_HIGH, SECHIGHT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! USING T1 INSTEAD, ASSUMING THAT NUMBER OF NODES ALWAYS BIGGER THAN NUMBER OF GRAIN SIZE CLASSES
!
      INTEGER NEWPRO_MAX, K, I, JG
      LOGICAL DB,RET
      LOGICAL, EXTERNAL          :: CVSP_CHECK_F
      DOUBLE PRECISION, EXTERNAL :: CVSP_INTEGRATE_VOLUME
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!TEMPORARY VERTICAL SORTING PROFILE: FRACTION FOR EACH LAYER, CLASS, POINT
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE::PRO_FNEW
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!TEMPORARY VERTICAL SORTING PROFILE: DEPTH FOR EACH LAYER, CLASS, POINT
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE::PRO_DNEW
!
!-----------------------------------------------------------------------
!
      ALLOCATE(PRO_DNEW(PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_FNEW(PRO_MAX_MAX,NSICLA))

      JG = J
      IF(NCSIZE.GT.1) JG = MESH%KNOLG%I(J)
      IF(DB(JG,0)) CALL CVSP_P('./','BRUA',JG)
!
!-----------------------------------------------------------------------
! WORKS LIKE THE MAKE_ACT LAYER ROUTINE BUT FOR VSP
!-----------------------------------------------------------------------
!
      NEWPRO_MAX=INT(MAX(8.D0,(DBLE(PRO_MAX_MAX - 4 * NSICLA)*0.7D0)))
!
!-----------------------------------------------------------------------
! NEW VSP SECTION HEIGHT
!-----------------------------------------------------------------------
!
      SECHIGHT = (PRO_D(J,PRO_MAX(J),1)-PRO_D(J,1,1)) / (NEWPRO_MAX - 1)
!
      DO K = 1, NEWPRO_MAX
        DO I = 1, NSICLA
          PRO_DNEW(K,I) = (K-1)*SECHIGHT + PRO_D(J,1,1)
          Z_LOW  = PRO_DNEW(K,1) - 0.5D0*SECHIGHT
          Z_HIGH = PRO_DNEW(K,1) + 0.5D0*SECHIGHT
          IF(K.EQ.1) Z_LOW = PRO_D(J,1,1)
          IF(K.EQ.NEWPRO_MAX) Z_HIGH = PRO_D(J,PRO_MAX(J),1)
          PRO_FNEW(K,I)=CVSP_INTEGRATE_VOLUME(J,I, Z_HIGH, Z_LOW,T1%R)
     &         / SECHIGHT
          IF(K.EQ.1) PRO_FNEW(K,I) = PRO_FNEW(K,I) * 2.0D0
          IF(K.EQ.NEWPRO_MAX) PRO_FNEW(K,I) = PRO_FNEW(K,I) * 2.0D0
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
! RESUBSTITUTE
!-----------------------------------------------------------------------
!
      DO I = 1, NSICLA
        DO K = 1, NEWPRO_MAX
          PRO_D(J,K,I) = PRO_DNEW(K,I)
          PRO_F(J,K,I) = PRO_FNEW(K,I)
        ENDDO
      ENDDO
!
      PRO_MAX(J) = NEWPRO_MAX
!
      IF(PRO_MAX(J).LE.2) THEN
        WRITE(LU,*) ' COMPRESSBRUT: NOT ENOUGH PRO_MAX '
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(DB(JG,0)) CALL CVSP_P('./','BRUE',JG)
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(PRO_DNEW)
      DEALLOCATE(PRO_FNEW)
!
!-----------------------------------------------------------------------
! REMOVES NUMERIC INSTABILITIES
!-----------------------------------------------------------------------
!
      DO K = 1, PRO_MAX(J)
        RET = CVSP_CHECK_F(J,K,'AFTERBRUT:   ')
      ENDDO
      CALL CVSP_CHECK_STEADY(J)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_COMPRESS_BRUT

