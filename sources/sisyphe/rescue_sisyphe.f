!                    *************************
                     SUBROUTINE RESCUE_SISYPHE
!                    *************************
!
     &(QU,QV,Q,U,V,H,S,ZF,ZR,ES,HW,TW,THETAW,NPOIN,NOMBLAY,NSICLA,
     & TROUVE,ALIRE,PASS,ICF,LISTI,MAXVAR)
!
!***********************************************************************
! SISYPHE   V7P2
!***********************************************************************
!
!brief    COMPUTES MISSING DATA/VARIABLES FOR HYDRODYNAMIC
!+                AND/OR SEDIMENTOLOGICAL CONTINUATION RUN.
!
!history  C. LENORMANT
!+
!+        V6P0
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
!history M.SECHER (EDF-CIH)
!+       22/08/2016
!+       V7P2
!+   Add calculation of the rigid bed from bottom elevation and
!+   layers thickness of a previous sedimentology computation file
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALIRE          |-->| LIST VARIABLES TO BE READ
!| H              |<->| WATER DEPTH
!| HW             |<->| WAVE DEPTH
!| ICF            |-->| BED-LOAD OR TOTAL LOAD TRANSPORT FORMULAS
!| LISTI          |-->| LOGICAL, IF YES PRINT MESSAGES
!| MAXVAR         |-->| MAXIMUM NUMBER OF OUTPUT VARIABLES
!| NPOIN          |-->| NUMBER OF MESH NODES
!| PASS           |-->| LOGICAL, IF YES BEGIN OF COMPUTATION
!| Q              |<->| LIQUID DISCHARGE
!| QU             |<->| LIQUID DISCHARGE X
!| QV             |<->| LIQUID DISCHARGE Y
!| S              |<->| WATER SURFACE ELEVATION
!| THETAW         |<->| ANGLE BETWEEN WAVE AND CURRENT
!| TROUVE         |-->| LOGIQUE INDIQUANT LES VARIABLES TROUVEES
!|                |   | DANS LE SOUS-PROGRAMME SUITE
!| TW             |<->| WAVE PERIOD
!| U              |<->| VELOCITY COMPONENT X
!| V              |<->| VELOCITY COMPONENT Y
!| ZF             |<->| BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE INTERFACE_SISYPHE, EX_RESCUE_SISYPHE
     &           => RESCUE_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: MAXVAR,NOMBLAY,NSICLA
      INTEGER, INTENT(IN) :: ALIRE(MAXVAR),NPOIN,ICF
      LOGICAL, INTENT(IN) :: PASS,LISTI
      DOUBLE PRECISION, INTENT(IN) :: ES(NPOIN,NOMBLAY)
!
      INTEGER, INTENT(INOUT) :: TROUVE(MAXVAR)
      DOUBLE PRECISION, INTENT(INOUT) :: QU(NPOIN), QV(NPOIN), Q(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN) , V(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: S(NPOIN) , ZF(NPOIN), H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: ZR(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HW(NPOIN), TW(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: THETAW(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,I,J
      INTEGER CHECK_ES
!
!-----------------------------------------------------------------------
!
! PRINTOUTS :
! -----------
      IF(PASS.AND.LISTI) THEN
        WRITE(LU,200)
200     FORMAT(80('-'))
        IF(ALIRE(8).EQ.1) THEN
          IF(LNG.EQ.1) WRITE(LU,300)
          IF(LNG.EQ.2) WRITE(LU,301)
300       FORMAT(1X,'RESCUE : FICHIER HYDRODYNAMIQUE')
301       FORMAT(1X,'RESCUE : HYDRODYNAMIC FILE')
        ELSE
          IF(LNG.EQ.1) WRITE(LU,310)
          IF(LNG.EQ.2) WRITE(LU,311)
310       FORMAT(1X,'RESCUE : FICHIER SEDIMENTOLOGIQUE')
311       FORMAT(1X,'RESCUE : SEDIMENTOLOGICAL FILE')
        ENDIF
      ENDIF
!
! ------------------------------------------------------------------
!  WATER DEPTH :
!  -------------
      IF((ALIRE(3).EQ.1).AND.(TROUVE(3).NE.1)) THEN
        IF(TROUVE(4).EQ.1.AND.TROUVE(5).EQ.1) THEN
          IF (LISTI) THEN
            IF(LNG.EQ.1) WRITE(LU,400)
            IF(LNG.EQ.2) WRITE(LU,401)
          ENDIF
          CALL OV( 'X=Y-Z   ' , H , S , ZF , 0.D0 , NPOIN )
        ELSE
          IF (LISTI) THEN
            IF(LNG.EQ.1) WRITE(LU,420)
            IF(LNG.EQ.2) WRITE(LU,421)
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
400       FORMAT(1X,'HAUTEUR D''EAU CALCULEE AVEC LE FOND',
     &         /,1X,'ET LA SURFACE LIBRE')
401       FORMAT(1X,'WATER DEPTH COMPUTED WITH BATHYMETRY',
     &         /,1X,' AND SURFACE ELEVATION')
420       FORMAT(1X,'IMPOSSIBLE DE CALCULER LA HAUTEUR D''EAU')
421       FORMAT(1X,'WATER DEPTH UNABLE TO BE COMPUTED')
!
! ----------------------------------------------------------------------
!
! CLIPS NEGATIVE WATER DEPTHS :
! -------------------------------------
!
      DO K = 1,NPOIN
        H(K) = MAX(H(K),0.D0)
      ENDDO
!
!------------------------------------------------------------------------
!
!  WAVE HEIGHT AND PERIOD
!
      IF(ICF==4.OR.ICF==5.OR.ICF==8.OR.ICF==9) THEN
!
        IF(ALIRE(12).EQ.1.AND.TROUVE(12).EQ.0) THEN
          IF(LNG.EQ.1) WRITE(LU,900)
          IF(LNG.EQ.2) WRITE(LU,901)
          CALL OV( 'X=C     ' , HW , U , V , 0.D0 , NPOIN )
        ENDIF
!
900     FORMAT(1X,'CALCUL PRECEDENT SANS LA HAUTEUR DE HOULE : ON',
     &          ' PREND ZERO')
901     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE HEIGHT : IT IS',
     &          ' FIXED TO ZERO')
!
        IF(ALIRE(13).EQ.1.AND.TROUVE(13).EQ.0) THEN
          IF(LNG.EQ.1) WRITE(LU,902)
          IF(LNG.EQ.2) WRITE(LU,903)
          CALL OV( 'X=C     ' , TW , U , V , 0.D0 , NPOIN )
        ENDIF
902     FORMAT(1X,'CALCUL PRECEDENT SANS LA PERIODE DE HOULE : ON',
     &          ' PREND ZERO')
903     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE PERIOD : IT IS',
     &          ' FIXED TO ZERO')
!
        IF(ALIRE(14).EQ.1.AND.TROUVE(14).EQ.0) THEN
          IF(LNG.EQ.1) WRITE(LU,902)
          IF(LNG.EQ.2) WRITE(LU,903)
          CALL OV( 'X=C     ' , THETAW , U , V , 90.D0  , NPOIN )
        ENDIF
      ENDIF
!909   FORMAT(1X,'CALCUL PRECEDENT SANS ANGLE DE HOULE : ON',
!     &          ' PREND ZERO')
!910   FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE ANGLE : IT IS',
!     &          ' FIXED TO ZERO')
!
!-----------------------------------------------------------------------
!  NON-ERODABLE BED
!
      IF(ALIRE(9).EQ.1.AND.TROUVE(9).EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,907)
        IF(LNG.EQ.2) WRITE(LU,908)
        CHECK_ES=0
        DO I = 1,NOMBLAY
          IF(TROUVE(5).EQ.1.AND.
     &       TROUVE(28+I+NSICLA*(NOMBLAY+4)).EQ.1) THEN
            CHECK_ES=CHECK_ES+1
          ENDIF
        ENDDO
        IF(CHECK_ES.EQ.NOMBLAY) THEN
          IF(LNG.EQ.1) WRITE(LU,909)
          IF(LNG.EQ.2) WRITE(LU,910)
          DO I = 1,NPOIN
            ZR(I)=ZF(I)
            DO J = 1,NOMBLAY
              ZR(I)=ZR(I)-ES(I,J)
            ENDDO
          ENDDO
          TROUVE(9) = 1
        ENDIF
      ENDIF
907   FORMAT(1X,'CALCUL PRECEDENT SANS FOND NON ERODABLE')
908   FORMAT(1X,'PREVIOUS CALCULATION WITHOUT NON ERODABLE',
     &         /,1X,'BOTTOM')
909   FORMAT(1X,'CALCUL PRECEDENT CONTIENT TOUTES LES COUCHES',
     &         /,1X,'LE FOND NON ERODABLE CALCULE AVEC LES EPAISSEURS',
     &         /,1X,'DES COUCHES ET LE FOND')
910   FORMAT(1X,'PREVIOUS CALCULATION CONTAINS ALL LAYERS',
     &         /,1X,'RIGID BED COMPUTED FROM LAYERS THICKNESS',
     &         /,1X,'AND BOTTOM')
!
!-----------------------------------------------------------------------
!  BED ELEVATION
!
      IF(ALIRE(5).EQ.1.AND.TROUVE(5).EQ.0) THEN
!
        IF(TROUVE(4).EQ.1.AND.TROUVE(3).EQ.1) THEN
          IF (LISTI) THEN
          IF(LNG.EQ.1) WRITE(LU,410)
          IF(LNG.EQ.2) WRITE(LU,411)
410       FORMAT(1X,'FOND CALCULE AVEC LA HAUTEUR D''EAU',
     &         /,1X,'ET LA SURFACE LIBRE')
411       FORMAT(1X,'BATHYMETRY COMPUTED FROM WATER DEPTH',
     &         /,1X,'AND SURFACE ELEVATION')
          ENDIF
          CALL OV( 'X=Y-Z   ' , ZF , S , H , 0.D0 , NPOIN )
        ELSE
          CALL  OV( 'X=C     ' , ZF , ZF, ZF, 0.D0 , NPOIN )
          IF(LNG.EQ.1) WRITE(LU,960)
          IF(LNG.EQ.2) WRITE(LU,961)
        ENDIF
960     FORMAT(1X,'COTE DU FOND NON TROUVEE',/,
     &            'LA COTE EST INITIALISEE A ZERO')
961     FORMAT(1X,'BOTTOM TOPOGRAPHY NOT FOUND',/,
     &            'IT IS SET TO ZERO')
!
      ENDIF
!
      IF (PASS.AND.LISTI) THEN
        WRITE(LU,970)
970     FORMAT(80('-'))
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE RESCUE_SISYPHE
