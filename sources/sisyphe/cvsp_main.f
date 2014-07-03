!                    ********************
                     SUBROUTINE CVSP_MAIN
!                    ********************
!
     &(ZFCL_W,NLAYER,ZR,ZF,ESTRAT,ELAY,MASBAS,ACLADM,NSICLA,NPOIN,
     & ELAY0,VOLTOT,ES,AVAIL,CONST_ALAYER,DTS,ESTRATNEW,NLAYNEW)
!
!***********************************************************************
! SISYPHE   V6P3                                   14/03/2013
!***********************************************************************
!
!BRIEF    CONTINOUS VERTICAL SORTING MODEL
!+        COMPUTES FRACTIONS FOR EACH CLASS AND EACH SECTION OF A C-VSM;
!+
!
!HISTORY  U.MERKEL (BAW), R.KOPMANN (BAW)
!+        01/06/2012
!+        V6P2
!+
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACLADM         |---| CALCULATED GEOMETRICAL MEAN DIAMETER OF ACT LAY
!| AVAIL          |<--| SEDIMENT FRACTION FOR EACH LAYER, CLASS, POINT
!| CONST_ALAYER   |---|
!| DTS            |---| TIMESTEP LENGTH IN [S]
!| ELAY           |<--| ACTIVE LAYER THICKNESS FOR EACH POINT
!| ELAY0          |---| WANTED ACTIVE LAYER THICKNESS
!| ES             |---| LAYER THICKNESS
!| ESTRAT         |<--| ACTIVE STRATUM THICKNESS FOR EACH POINT
!| ESTRATNEW      |---| TEMPORARY ACTIVE STRATUM THICKNESS
!| MASBAS         |---| AREA AROUND NODE
!| NLAYER         |<--| NUMBER OF LAYER FOR EACH POINT
!| NLAYNEW        |---| TEMPORARY NUMBER OF LAYER FOR EACH POINT
!| NPOIN          |---| NUMBER OF MESH POINTS
!| NSICLA         |---| NUMBER OF GRAIN CLASSES (FRACTIONS)
!| VOLTOT         |---| TOTAL VOLUME AROUND ONE POINT
!| ZF             |---| BOTTOM ELEVATION
!| ZFCL_W         |-->| EVOLUTION FOR EACH SEDIMENT CLASS
!| ZR             |---| RIGID BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY: CVSMOUTPUT,CVSM_OUT,CVSM_OUT_FULL,
     &                                PRO_D,PRO_MAX,PRO_MAX_MAX,PERCOU,
     &                                HN,LT,DT,MESH,Z
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IAMCASE, ISICLA, JG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZFCL_W,ZR,ZF
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASBAS,ACLADM
      INTEGER,          INTENT(IN)    :: NSICLA,NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DTS
      LOGICAL,          INTENT(IN)    :: CONST_ALAYER
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: NLAYER,ESTRAT,ELAY
      DOUBLE PRECISION, INTENT(INOUT) :: ELAY0
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLTOT(10),ESTRATNEW(NPOIN)
      INTEGER         , INTENT(INOUT) :: NLAYNEW(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
      INTEGER  P_ISUM
      EXTERNAL P_ISUM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL CVSP_CHECK_F, DB, RET
      INTEGER I,J,K,ARRET,ARRET2
      DOUBLE PRECISION DZFCL,EVL,AT,DELTA
      INTEGER KK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      ARRET=0
      AT = DT*LT/PERCOU
!     
!-----------------------------------------------------------------------
!     CHECK FOR RIGID BED ERRORS 
!-----------------------------------------------------------------------
!     
      DO J=1,NPOIN
        IF(Z%R(J)-ZF%R(J).LT.0.D0) THEN
           WRITE(LU,*) 'UHM_Z.LT.ZF_BEF ',AT,Z%R(J),ZF%R(J),HN%R(J),
     &                 (Z%R(J)-ZF%R(J))-HN%R(J)
           CALL CVSP_P('./','Z_', J)
        ENDIF
      ENDDO
!     
!-----------------------------------------------------------------------     
!     FOR ALL POINTS AND FOR ALL CLASSES
!-----------------------------------------------------------------------
!     
      DO J=1,NPOIN
        JG = J
        IF (NCSIZE.GT.1) JG = MESH%KNOLG%I(J)
        EVL = 0.D0
        DO ISICLA = 1,NSICLA
          EVL = ZFCL_W%ADR(ISICLA)%P%R(J) + EVL
        END DO
!      
! DEBUG INFO
        IAMCASE = 0
        IF (DB(JG,0)) CALL CVSP_P('./','V_A',JG)
! DEBUG INFO
!     
!-----------------------------------------------------------------------     
! DEPOSITION IN SUM OVER ALL CASES
!-----------------------------------------------------------------------
!
        IF(EVL.GT.0) THEN
          CALL CVSP_ADD_SECTION(J)
        ENDIF
!         
        DO I=1,NSICLA
          DZFCL = ZFCL_W%ADR(I)%P%R(J)
          IF (EVL.GT.0D0) THEN
            IF (DZFCL.GT.0.D0) THEN
              CALL CVSP_ADD_FRACTION(J,I,DZFCL,EVL)
              IAMCASE = 1 + IAMCASE !DEBUG INFO
            ELSEIF( DZFCL.LT.0.D0) THEN
              CALL CVSP_RM_FRACTION(J,I,DZFCL,EVL)
              IAMCASE = 10 + IAMCASE !DEBUG INFO
            ENDIF
          ENDIF
!     
!-----------------------------------------------------------------------     
! END DEPOSITION
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------     
! START EROSION IN SUM OVER ALL CASES
!-----------------------------------------------------------------------
!
          IF(EVL.LT.0.D0) THEN
            IF (DZFCL.GT.0.D0) THEN
              CALL CVSP_ADD_FRACTION(J,I,DZFCL,EVL)
              IAMCASE = 100 + IAMCASE !DEBUG INFO
            ELSEIF(DZFCL.LT.0.D0) THEN
              CALL CVSP_RM_FRACTION(J,I,DZFCL,EVL)
              IAMCASE = 1000 + IAMCASE !DEBUG INFO
            ENDIF                      ! DZFCL
          ENDIF                        ! EVL < 0
!     
!-----------------------------------------------------------------------     
! END EROSION
!-----------------------------------------------------------------------
!
        ENDDO
!
!-----------------------------------------------------------------------     
! END FOR ALL CLASSES
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------     
! WE ARE RUNNING OUT OF SECTION MEMORY! COMPRESS NOW!
!-----------------------------------------------------------------------
!
        IF ((PRO_MAX(J).GT.PRO_MAX_MAX/4*3).OR.
     &       (PRO_MAX_MAX-PRO_MAX(J).LT.8*NSICLA)) THEN
           CALL CVSP_COMPRESS_DP(J, 1.0D-5)
        ENDIF
!     
!-----------------------------------------------------------------------     
! SYNCHRONICE VSP WITH LAYER (FOR DEBUGGING ...)
!-----------------------------------------------------------------------
!
        DELTA = ZF%R(J) - PRO_D(J, PRO_MAX(J), 1)
!      
        IF (DELTA.NE.0.D0) THEN
          DO I = 1 , NSICLA
            DO K = 1, PRO_MAX(J)
              PRO_D(J, K, I) = PRO_D(J, K, I) + DELTA
            ENDDO
          ENDDO
        ENDIF
!     
!-----------------------------------------------------------------------     
!FINAL CHECK ON NEW FRACTIONS AND STEADY STADE
!-----------------------------------------------------------------------
!
        DO K = 1, PRO_MAX(J)
!         REMOVES NUMERIC INSTABILITIES
          RET =  CVSP_CHECK_F(J,K,' FINAL:   ')
        ENDDO
        CALL CVSP_CHECK_STEADY(J)
!      
! END FOR ALL POINTS
      ENDDO
!
!-----------------------------------------------------------------------     
! PRINT OUT SORTING PROFILE FOR SELECTED GLOBAL POINT NUMBERS!INSERT
!-----------------------------------------------------------------------
!
      IF((CVSM_OUT).OR.(DB(-1,-1).EQV..TRUE.)) THEN
! WRITES THE FULL VSP AS SERAFIN
        IF (CVSM_OUT_FULL) CALL CVSP_WRITE_PROFILE()
! WRITES THE VSP FOR SINGLE POINTS
        DO KK = 1, 100
          IF (CVSMOUTPUT(KK).GT.0) THEN
            CALL CVSP_P('./','V_', CVSMOUTPUT(KK))
          ENDIF
        ENDDO
      END IF
!     
!-----------------------------------------------------------------------     
! GENERATE NEW LAYERS FROM SORTING PROFILE
!-----------------------------------------------------------------------
!
      CALL CVSP_MAKE_ACTLAY()
!
!-----------------------------------------------------------------------     
! CHECK FOR RIGID BED ERRORS
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
        IF (Z%R(J)-ZF%R(J).LT.0.D0) THEN
          WRITE(LU,*) 'UHM_Z.LT.ZF ', I,AT,Z%R(J),ZF%R(J),HN%R(J),
     &         (Z%R(J)-ZF%R(J))-HN%R(J)
          CALL CVSP_P('./','Z_', J)
        END IF
      ENDDO
!     
!-----------------------------------------------------------------------     
! PRINT OUT NEW LAYERS FOR SELECTED GLOBAL POINT NUMBERS
!-----------------------------------------------------------------------
!
      IF((CVSM_OUT).OR.(DB(-1,-1).EQV..TRUE.)) THEN
        DO KK = 1,100
          IF (CVSMOUTPUT(KK).GT.0) THEN
            CALL LAYERS_P('./VSP_', CVSMOUTPUT(KK))
          ENDIF
        ENDDO
      END IF
!
!-----------------------------------------------------------------------     
!     CLEAN STOP FOR ALL PROCESSORS IF PROBLEM
!-----------------------------------------------------------------------
!
      ARRET2=ARRET
      IF(NCSIZE.GT.1) ARRET2=P_ISUM(ARRET)
      IF(ARRET2.GT.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'ARRET APRES ERREUR DANS LAYER'
        IF(LNG.EQ.2) WRITE(LU,*) 'STOP AFTER AN ERROR IN LAYER'
        IF(ARRET.EQ.0) THEN
          IF(LNG.EQ.1) WRITE(LU,*) 'DANS ',ARRET2,' PROCESSEUR(S)'
          IF(LNG.EQ.2) WRITE(LU,*) 'IN ',ARRET2,' PROCESSOR(S)'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE

