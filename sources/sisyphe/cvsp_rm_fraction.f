!                    ***************************
                     SUBROUTINE CVSP_RM_FRACTION
!                    ***************************
!
     &(J,I,DZFCL,EVL)
!
!***********************************************************************
! SISYPHE   V6P3                                   14/03/2013
!***********************************************************************
!
!brief   REMOVES (PARTS) OF A FRACTION AFTER EROSION
!+       FROM THE VERTICAL SORTING PROFILE;
!
!history UWE MERKEL
!+        02/02/2012
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
!| I              |<--| INDEX OF A FRACTION
!| DZFCL          |<--| VALUE OF A FRACTION IN CM !
!| EVL            |<--| SUM OF ALL FRACTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
      USE CVSP_OUTPUTFILES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: J
      INTEGER,          INTENT(IN)    :: I
      DOUBLE PRECISION, INTENT(IN)    :: DZFCL
      DOUBLE PRECISION, INTENT(IN)    :: EVL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, EXTERNAL :: CVSP_CHECK_F, DB
!
      DOUBLE PRECISION EROSUM,PROF,PROTH,PROV,REST1,DFRAC
      DOUBLE PRECISION EROSTRENGTH,ERODEPTH
      DOUBLE PRECISION PRO_D_LOW,PRO_F_LOW,PROV_TOTAL,F1,F2,D1,D2
!
      INTEGER CNTR, CNTRB, II, KK,  LOWPNT, JG
      DOUBLE PRECISION  ST_S, ST_A, ST_M, ST_C
!
!-----------------------------------------------------------------------
!
      JG = J
      IF (NCSIZE > 1) JG = MESH%KNOLG%I(J)
      IF (DB(JG,0)) CALL CVSP_P('./VSP/','RFT_IN_',JG)
!
      EROSUM = - DZFCL    !EROSION SUM POSITIV = EROSION
      CNTR = -1                 !SECTION CNTR BELOW PRO_MAX
!
!-----------------------------------------------------------------------
! LOOP THROUGH VSP SECTIONS UNTIL ALL EROSION IS DONE
! COUNTING BACK EROSUM -> 0
! IDENTIFY THE LAST SECTION POINT AFFECTED
!-----------------------------------------------------------------------
!
      DO WHILE ((EROSUM > 0.D0.OR.CNTR==-1).AND.(CNTR.LT.PRO_MAX(J)-2))
        CNTR  =  CNTR + 1
!
        F1 = PRO_F(J,PRO_MAX(J)-CNTR,I)
        F2 = PRO_F(J,PRO_MAX(J)-(CNTR+1),I)
!
        D1 = PRO_D(J,PRO_MAX(J)-CNTR,I)
        D2 = PRO_D(J,PRO_MAX(J)-(CNTR+1),I)
!
        PROF  = (F1 + F2) * 0.5D0 ! MEAN FRACTION
        PROTH = D1 - D2           ! PROFILE SECTION THICKNESS
        PROV  =  PROF * PROTH     ! PROFILE SECTION VOLUME ( EQ. THICKNESS OF FRACTION)
        DFRAC =  F1 - F2          ! DELTA FRACTION = CHANGE OF FRACTION OVER THIS SECTION
!
        IF (PROTH.GT.0.D0) THEN
!
!-----------------------------------------------------------------------
! CASE 2
! THIS VSP SECTION HAS MORE THEN ENOUGH MATERIAL
! THEN SPLIT IT AT THE DEPTH OF MAXIMUM EROSION,
! TO KEEP ANTHING BELOW UNCHANGED
! => MAKES CASE 2 TO CASE 1
!-----------------------------------------------------------------------
!
          IF(EROSUM < PROV) THEN
            IF(DB(JG,0)) CALL CVSP_P('./VSP/','RFT_C2_',JG)
!
!-----------------------------------------------------------------------
! MAXIMUM DEPTH OF EROSION: SOLVING QUADRATIC PROBLEM BY CROSS-MULTIPLICATION
! LINEAR OR QUATRATIC ? PREVENTS RUNNING INTO FLOATING POINT CANCELLATION PROBLEMS
!-----------------------------------------------------------------------
!
            ST_M = ABS(DFRAC / PROTH)
!-QUADRATIC
            IF(ST_M.GT.1.D-7.AND.ABS(DFRAC).GT.1.D-12) THEN
              ST_C = F1 / ST_M
              ST_A = EROSUM + 0.5D0 * ST_M * ST_C**2
              ST_S = SQRT(2.D0 * ST_A / ST_M)
              EROSTRENGTH = ST_S - ST_C
!
!-LINEAR (SAVE MODE) FOR ALMOST NO CHANGE IN FRACTION OVER DEPTH
! = ACCEPTAPLE FRACTION SHIFT TO OTHER FRACTIONS (ILLEGAL!!)...
            ELSE
              EROSTRENGTH = EROSUM / PROV * PROTH
            ENDIF
!
            ERODEPTH = D1 - EROSTRENGTH
!
!     ATTENTION
            IF((ERODEPTH-D2).LT.0.D0) ERODEPTH = D2
!
!           IF(DB(JG,0)) THEN
!             WRITE(LU,*) '  '
!           ENDIF
!
!-----------------------------------------------------------------------
! INSERT A NEW BREAKPOINT TO SPLIT THE VSP AND SHIFT THE REST
! SHIFT
!-----------------------------------------------------------------------
!
            DO KK = 0,CNTR
              DO II=1,NSICLA
                PRO_F(J,PRO_MAX(J)-KK+1,II) = PRO_F(J,PRO_MAX(J)-KK,II)
                PRO_D(J,PRO_MAX(J)-KK+1,II) = PRO_D(J,PRO_MAX(J)-KK,II)
              ENDDO
            ENDDO
!
            DO II=1,NSICLA
              PRO_D(J,PRO_MAX(J)-CNTR,II) = ERODEPTH
              PRO_F(J,PRO_MAX(J)-CNTR,II) = PRO_F(J,PRO_MAX(J)-CNTR,II)
     &             - ( PRO_F(J,PRO_MAX(J)-CNTR,II)
     &             -PRO_F(J,PRO_MAX(J)-(CNTR+1),II))/PROTH*EROSTRENGTH
            ENDDO
!
            IF(PRO_D(J,PRO_MAX(J)-CNTR,1).LT.
     &         PRO_D(J,PRO_MAX(J)-CNTR-1,1)) THEN
              WRITE(LU,*) 'DEPTHINVERSION!!!',JG,PRO_MAX(J)-CNTR,
     &             PRO_MAX(J), PROTH, EROSTRENGTH
              CALL PLANTE(1)
              STOP
            ENDIF
!
            PRO_MAX(J) = PRO_MAX(J) + 1
!
            IF(DB(JG,0)) CALL CVSP_P('./VSP/','RFT_C3_',JG)
!
            IF(CVSP_CHECK_F(J,PRO_MAX(J)-CNTR,'RMF:CAS2 B')) THEN
            ENDIF
!
!-----------------------------------------------------------------------
! AFTER SPLITTING: UPDATE SECTION PROPERTIES
!-----------------------------------------------------------------------
!
            PROF  = (PRO_F(J,PRO_MAX(J)-CNTR,I) + !PROFILE SECTION FRACTION
     &           PRO_F(J,PRO_MAX(J)-(CNTR+1),I)) * 0.5D0

            PROTH = (PRO_D(J,PRO_MAX(J)-CNTR,I) -
     &           PRO_D(J,PRO_MAX(J)-(CNTR+1),I)) !PROFILE SECTION THICKNESS

            PROV  =  PROF * PROTH    !PROFILE SECTION VOLUME ( EQ. THICKNESS OF FRACTION)
!
            !DELTA FRACTION = CHANGE OF FRACTION OVER THIS SECTION
            DFRAC = PRO_F(J,PRO_MAX(J)-CNTR,I) -
     &              PRO_F(J,PRO_MAX(J)-(CNTR+1),I)

            EROSUM = PROV            !NECESSARY! TO AVOID PROBLEMS WITH SLIVER POLYGONS
!
          ENDIF                     ! CASE 2 CONVERSION
!
!-----------------------------------------------------------------------
! CASE 1 (NOW ALL CASES, AS CASE II IS ALREADY CONVERTED)
! THIS VSP SECTION HAS NOT ENOUGH OR EXACTLY ENOUGH MATERIAL TO SATISFY THE HUNGER
! MEANS. IT IS REMOVED COMPLETLY
!-----------------------------------------------------------------------
!
        EROSUM = - PROV + EROSUM
        ENDIF
!     END LOOP THROUGH VSP SECTION
      END DO
!
!     DEEPEST/LAST TOUCHED SECTION POINT
      CNTRB = CNTR + 1
!
!-----------------------------------------------------------------------
! FIXING DEPTH AND FRACTION
! AFTER DIGGING OUT 1 FRACTION, EVERYTHING ABOVE FALLS DOWN
! TO FILL THE HOLE
!-----------------------------------------------------------------------
!
      IF (DB(JG,0)) CALL CVSP_P('./VSP/','RFT_C4_',JG)
!
!-----------------------------------------------------------------------
! SHIFT AND DUPLICATE DEEPEST POINT TO PRODUCE A CLEAR BREAK WHEN NORMALIZING THE FRACTIONS
! IN THE NEXT STEP
!-----------------------------------------------------------------------
!
      IF (CNTRB.EQ.PRO_MAX(J)) CNTRB = PRO_MAX(J) - 1 ! PREVENT RIGID BED DEMOLITION
      DO KK = 0,CNTRB
!       DEBUG
        IF(PRO_MAX(J)-KK+1 > PRO_MAX_MAX-1) THEN
          WRITE(LU,*) 'PRO_MAX_MAX_: ',J,PRO_MAX(J)
          CALL CVSP_P('./ERR/','MAX_I',JG)
          CALL PLANTE(1)
        ENDIF
!
        DO II=1,NSICLA
          PRO_F(J,PRO_MAX(J)-KK+1,II) = PRO_F(J,PRO_MAX(J)-KK,II)
          PRO_D(J,PRO_MAX(J)-KK+1,II) = PRO_D(J,PRO_MAX(J)-KK,II)
        ENDDO
      ENDDO
!
!     1 POINT MORE / MEANS ONE POINT DEEPER AFFECTED
      PRO_MAX(J) = PRO_MAX(J) + 1
!
      IF(DB(JG,0)) CALL CVSP_P('./VSP/','TEST4_',JG)
!
!-----------------------------------------------------------------------
!ALL LAYERS ABOVE FALL DOWN = EVERY FRACTION FALLS, GOING FROM DEEPEST TO HIGHEST AFFECTED SECTION
!-----------------------------------------------------------------------
!
      LOWPNT = PRO_MAX(J)-CNTRB
!
      PRO_F_LOW = PRO_F(J,LOWPNT,I)
      PRO_D_LOW = PRO_D(J,LOWPNT,I) ! START AT LOWESET LEVEL
      PROV_TOTAL = 0.D0             ! ACCUMULATING THE EROSION THE HIGHER WE GO
!
      DO KK = LOWPNT+1,PRO_MAX(J)                     ! LOOP OVER UPPER SECTION POINT
        PROF  = ( PRO_F(J,KK,I) + PRO_F_LOW) * 0.5D0 ! PROFILE SECTION FRACTION
        PROTH = ( PRO_D(J,KK,I) - PRO_D_LOW)         ! PROFILE SECTION THICKNESS
        PROV  =  PROF * PROTH                        ! PROFILE SECTION VOLUME (EQ. THICKNESS OF FRACTION)
!
        IF (PROV < ZERO) PROV = 0.D0
        PROV_TOTAL = PROV + PROV_TOTAL
        REST1 = ( - PRO_F(J,KK,I) + 1.D0) ! SUM OF FRACTIONS AFTER EROSION
        PRO_D_LOW = PRO_D(J,KK,I)         ! REMEMBER THIS FOR NEXT SECTION STEP
                                          ! CAUSE PRO_D(J,KK,II) IS NOT AVAILABEL ANY MORE
        PRO_F_LOW = PRO_F(J,KK,I)         ! KEEP THIS FOR THE NEXT SECTION !
!
        DO II=1,NSICLA
          PRO_D(J,KK,II) = -PROV_TOTAL + PRO_D(J,KK,II) ! SHIFT DEPTH
          IF (PRO_D(J,KK,II).LE.PRO_D(J,KK-1,II)) THEN  ! CORRECTING 1D-15 ERRORS WHICH PRODUCE UNSTEADY PDFS
            PRO_D(J,KK,II) = PRO_D(J,KK-1,II)
          ENDIF
          IF (REST1.GT.ZERO) THEN
            PRO_F(J,KK,II) = PRO_F(J,KK,II)/ REST1 ! NORMALIZE FRACTION
          ELSE
            PRO_F(J,KK,II) = 1.D0 / NSICLA         ! IN CASE OF ALMOST TOTAL LOSS
          ENDIF
        ENDDO
!
        IF(REST1.GT.ZERO) THEN
          PRO_F(J,KK,I) = 0.D0          ! FRACTION I IS REMOVED
        ELSE
          PRO_F(J,KK,I) = 1.D0 / NSICLA ! IN CASE OF ALMOST TOTAL LOSS
        ENDIF
!
      ENDDO
!
      IF(DB(JG,0)) CALL CVSP_P('./VSP/','TEST5_',JG)
!
!-----------------------------------------------------------------------
! SPECIAL TREATMENT LOWEST POINT
!-----------------------------------------------------------------------
!
      REST1 = (-PRO_F(J,LOWPNT,I)+1.D0)
!
      DO II=1,NSICLA
        IF (REST1.GT.ZERO) THEN
          PRO_F(J,LOWPNT,II) =
     &         PRO_F(J,LOWPNT,II) / REST1    ! NORMALIZE FRACTION
        ELSE
          PRO_F(J,LOWPNT,II) = 1.D0 / NSICLA ! IN CASE OF ALMOST TOTAL LOSS
        ENDIF
      ENDDO
!
      PRO_F(J,LOWPNT,I) = 0.D0                 ! FRACTION I IS REMOVED
      IF (REST1.LE.ZERO) PRO_F(J,LOWPNT,I) =
     &     1.D0 / NSICLA                       ! IN CASE OF ALMOST TOTAL LOSS
!
      IF(DB(JG,0)) CALL CVSP_P('./VSP/','RFT_OU_',JG)
!
      CALL CVSP_COMPRESS_CLEAN(J)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_RM_FRACTION
