!                    *****************
                     SUBROUTINE NOUDON
!                    *****************
!
     &(F1,NAME1FR,NAME1GB,MODE1,
     & F2,NAME2FR,NAME2GB,MODE2,
     & F3,NAME3FR,NAME3GB,MODE3,X,Y,NPOIN,NDON,FFORMAT,NBOR,NPTFR,
     & AT,DDC,TV1,TV2,F11,F12,F21,F22,F31,F32,INDIC,CHDON,NVAR,TEXTE,
     & TROUVE,UNITIME,PHASTIME)
!
!***********************************************************************
! TOMAWAC   V6P3                                  21/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CURRENT / WIND VELOCITY
!+                FOR THE CURRENT TIME STEP AND ON THE COMPUTATION MESH.
!+
!+           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D)
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
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        16/11/2012
!+        V6P3
!+   Only SELAFIN format with same mesh kept. Arguments removed.
!
!history  E. GAGNAIRE-RENOU & J-M HERVOUET (EDF - LNHE)
!+        16/05/2013
!+        V6P3
!+   In the case where a new record is not read, array TROUVE must be
!+   however built.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| FFORMAT         |-->| DATA FILE FORMAT
!| CHDON          |-->| NAME OF THE VARIABLE READ FROM THE DATA FILE
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| F1             |<--| FIRST VARIABLE TO READ
!| F2             |<--| SECOND VARIABLE TO READ
!| F3             |<--| THIRD VARIABLE TO READ
!| F11            |<->| DATA VALUES AT TIME TV1 IN THE DATA FILE FOR F1
!| F12            |<->| DATA VALUES AT TIME TV2 IN THE DATA FILE FOR F1
!| F21            |<->| DATA VALUES AT TIME TV1 IN THE DATA FILE FOR F2
!| F22            |<->| DATA VALUES AT TIME TV2 IN THE DATA FILE FOR F2
!| F31            |<->| DATA VALUES AT TIME TV1 IN THE DATA FILE FOR F3
!| F32            |<->| DATA VALUES AT TIME TV2 IN THE DATA FILE FOR F3
!| INDIC          |-->| FILE FORMAT
!| MODE1          |-->| MODE: 0= DO NOT READ
!|                |   |       1= READ IF PRESENT
!| MODE2          |-->| LIKE MODE1 FOR SECOND VARIABLE
!| MODE3          |-->| LIKE MODE1 FOR THIRD VARIABLE
!| NAME1FR        |-->| FRENCH NAME OF FIRST VARIABLE
!| NAME2FR        |-->| FRENCH NAME OF SECOND VARIABLE
!| NAME3FR        |-->| FRENCH NAME OF THIRD VARIABLE
!| NAME1GB        |-->| ENGLISH NAME OF FIRST VARIABLE
!| NAME2GB        |-->| ENGLISH NAME OF SECOND VARIABLE
!| NAME3GB        |-->| ENGLISH NAME OF THIRD VARIABLE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDON           |-->| LOGICAL UNIT NUMBER OF THA DATA FILE
!| NPOIN          |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NVAR           |-->| NUMBER OF VARIABLES TO BE READ
!| PHASTIME       |-->| TIME SHIFT IN FILE
!| TV1            |<->| TIME T1 IN THE DATA FILE
!| TV2            |<->| TIME T2 IN THE DATA FILE
!| UNITIME        |-->| UNIT OF TIME IN FILE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC,ONLY: DEBUG
      USE INTERFACE_TOMAWAC, EX_NOUDON => NOUDON
      USE INTERFACE_HERMES
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NDON,NPOIN,NPTFR,INDIC
      INTEGER, INTENT(INOUT)          :: NVAR
      INTEGER, INTENT(IN)             :: MODE1,MODE2,MODE3
      INTEGER, INTENT(IN)             :: NBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: F1(NPOIN),F2(NPOIN),F3(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: F11(NPOIN),F21(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: F12(NPOIN),F22(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: F31(NPOIN),F32(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: AT,DDC,UNITIME,PHASTIME
      DOUBLE PRECISION, INTENT(INOUT) :: TV1,TV2
      CHARACTER(LEN=8), INTENT(IN)    :: FFORMAT
      CHARACTER(LEN=7), INTENT(IN)    :: CHDON
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1FR,NAME2FR,NAME3FR
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1GB,NAME2GB,NAME3GB
      CHARACTER(LEN=32),INTENT(IN)    :: TEXTE(30)
      LOGICAL, INTENT(INOUT)          :: TROUVE(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,MODE(3)
      DOUBLE PRECISION COEF
      CHARACTER(LEN=32) NAMEFR(3),NAMEGB(3),FULL_NAME(3)
      CHARACTER(LEN=16), ALLOCATABLE :: VAR_NAME(:), VAR_UNIT(:)
      CHARACTER(LEN=32) COMPONENT
      LOGICAL COUUT,VENUT,MARUT
      DATA COUUT/.FALSE./
      DATA VENUT/.FALSE./
      DATA MARUT/.FALSE./
      INTEGER :: RECORD1,RECORD2
      DOUBLE PRECISION :: TIME1,TIME2
      INTEGER :: IERR
!
      INTRINSIC TRIM
!
!-----------------------------------------------------------------------
!
      MODE(1)=MODE1
      MODE(2)=MODE2
      MODE(3)=MODE3
      NAMEFR(1)=NAME1FR
      NAMEFR(2)=NAME2FR
      NAMEFR(3)=NAME3FR
      NAMEGB(1)=NAME1GB
      NAMEGB(2)=NAME2GB
      NAMEGB(3)=NAME3GB
!
!-----------------------------------------------------------------------
!
      IF(AT.GT.TV2) THEN
!
        IF(DEBUG.GT.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) '   NOUDON : LECTURE D''UN NOUVEL ENREGISTREMENT'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) '   NOUDON : READING A NEW RECORD'
        ENDIF
        ENDIF
!
        IF(INDIC.EQ.3) THEN
!
!     ------------------------------------------------------------------
!       READS A SELAFIN FILE OF TYPE: TELEMAC
!     ------------------------------------------------------------------
!
!
!     The test is useless as fields have already been checked for the initial value
!
        CALL GET_DATA_NVAR(FFORMAT,NDON,NVAR,IERR)
        CALL CHECK_CALL(IERR,'LECDOI:GET_DATA_NVAR')
        !
        ALLOCATE(VAR_NAME(NVAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'LECDOI:VAR_NAME')
        ALLOCATE(VAR_UNIT(NVAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'LECDOI:VAR_UNIT')
        CALL GET_DATA_VAR_LIST(FFORMAT,NDON,NVAR,VAR_NAME,VAR_UNIT,IERR)
        CALL CHECK_CALL(IERR,'LECDOI:GET_DATA_VAR_LIST')
        DO I=1,NVAR
          COMPONENT (1:16) = VAR_NAME(I)
          COMPONENT (17:32) = VAR_UNIT(I)
          ! CHECK IF THE VARIABLE ARE IN THE FILE
          DO J=1,3
            IF((COMPONENT.EQ.NAMEFR(J)).AND.
     &        MODE(J).GT.0) THEN
              TROUVE(J) = .TRUE.
              FULL_NAME(J) = NAMEFR(J)
            ENDIF
            IF((COMPONENT.EQ.NAMEGB(J)).AND.
     &        MODE(J).GT.0) THEN
              TROUVE(J) = .TRUE.
              FULL_NAME(J) = NAMEGB(J)
            ENDIF
          ENDDO
        ENDDO
        DEALLOCATE(VAR_NAME)
        DEALLOCATE(VAR_UNIT)
!
! Look for the two record before and after at for the interpolation
        RECORD1 = 0
        RECORD2 = 1
        CALL GET_DATA_TIME(FFORMAT,NDON,RECORD1,TIME1,IERR)
        CALL CHECK_CALL(IERR,'NOUDON:GET_DATA_TIME')
        TV1=(TIME1-PHASTIME)*UNITIME
        DO
          CALL GET_DATA_TIME(FFORMAT,NDON,RECORD2,TIME2,IERR)
          CALL CHECK_CALL(IERR,'NOUDON:GET_DATA_TIME')
          TV2=(TIME2-PHASTIME)*UNITIME
          IF(TV2.LT.AT) THEN
            IF(DEBUG.GT.0) THEN
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) ' NOUDON : ON SAUTE 1 ENREGISTREMENT'
              ELSEIF(LNG.EQ.2) THEN
                WRITE(LU,*) ' NOUDON: JUMP OF 1 DATA RECORD'
              ENDIF
            ENDIF
            RECORD1 = RECORD2
            RECORD2 = RECORD2 + 1
            TV1 = TV2
          ELSE
            EXIT
          ENDIF
        ENDDO
!       Check if all the variables are found for record1
        DO J=1,3
          IF(MODE(J).EQ.2.AND..NOT.TROUVE(J)) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'NOUDON : VARIABLE ',J,' NON TROUVEE'
              WRITE(LU,*) TRIM(NAMEFR(J)(1:16)),' OU ',
     &                    TRIM(NAMEGB(J)(1:16))
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'NOUDON: VARIABLE ',NAME1GB,' NOT FOUND'
              WRITE(LU,*) TRIM(NAMEFR(J)(1:16)),' OR ',
     &                    TRIM(NAMEGB(J)(1:16))
            ENDIF
            CALL PLANTE(1)
            STOP
          ELSEIF(MODE(J).GT.0.AND.TROUVE(J)) THEN
            IF(DEBUG.GT.0) THEN
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'VARIABLE ',J,' LUE (',
     &          TRIM(NAMEFR(J)(1:16)),' OU ',
     &          TRIM(NAMEGB(J)(1:16)),') AU TEMPS ',TV1
              ELSEIF(LNG.EQ.2) THEN
                WRITE(LU,*) 'VARIABLE ',J,' READ (',
     &          TRIM(NAMEFR(J)(1:16)),' OR ',
     &          TRIM(NAMEGB(J)(1:16)),') AT TIME ',TV1
              ENDIF
            ENDIF
!           Read the data for varialbe j on record1
            IF(J.EQ.1) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD1,
     &                            FULL_NAME(J),F11,NPOIN,IERR)
            ELSEIF(J.EQ.2) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD1,
     &                            FULL_NAME(J),F21,NPOIN,IERR)
            ELSEIF(J.EQ.3) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD1,
     &                            FULL_NAME(J),F31,NPOIN,IERR)
            ENDIF
          ENDIF
        ENDDO
!       Read the variables
!        
!       Check if all the variables are found for record2
        DO J=1,3
          IF(MODE(J).EQ.2.AND..NOT.TROUVE(J)) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'LECDON : VARIABLE ',J,' NON TROUVEE'
              WRITE(LU,*) NAMEFR(J),' OU ',NAMEGB(J)
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'LECDON: VARIABLE ',NAME1GB,' NOT FOUND'
              WRITE(LU,*) NAMEFR(J),' OR ',NAMEGB(J)
            ENDIF
            CALL PLANTE(1)
            STOP
          ELSEIF(MODE(J).GT.0.AND.TROUVE(J)) THEN
            IF(DEBUG.GT.0) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'VARIABLE ',J,' LUE (',
     &          TRIM(NAMEFR(J)(1:16)),' OU ',
     &          TRIM(NAMEGB(J)(1:16)),') AU TEMPS ',TV2
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'VARIABLE ',J,' READ (',
     &          TRIM(NAMEFR(J)(1:16)),' OR ',
     &          TRIM(NAMEGB(J)(1:16)),') AT TIME ',TV2
            ENDIF
            ENDIF
!           Read the data for varialbe j on record2
            IF(J.EQ.1) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD2,
     &                            FULL_NAME(J),F12,NPOIN,IERR)
            ELSEIF(J.EQ.2) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD2,
     &                            FULL_NAME(J),F22,NPOIN,IERR)
            ELSEIF(J.EQ.3) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD2,
     &                            FULL_NAME(J),F32,NPOIN,IERR)
            ENDIF
          ENDIF
        ENDDO
!
        ELSEIF (INDIC.EQ.4) THEN
!
!     ---------------------------------------------------------------------
!           READS A USER-DEFINED FILE FORMAT
!     ---------------------------------------------------------------------
!
          IF(CHDON(1:1).EQ.'C') THEN
            TROUVE(1)=.TRUE.
            TROUVE(2)=.TRUE.
            COUUT=.TRUE.
            CALL COUUTI(X,Y,NPOIN,NDON,FFORMAT,NBOR,NPTFR,AT,DDC,
     &                  TV1,TV2,F11,F21,F12,F22)
          ELSEIF(CHDON(1:1).EQ.'V'.OR.CHDON(1:1).EQ.'W') THEN
            TROUVE(1)=.TRUE.
            TROUVE(2)=.TRUE.
            VENUT=.TRUE.
            CALL VENUTI(X,Y,NPOIN,NDON,FFORMAT,NBOR,NPTFR,AT,DDC,
     &                  TV1,TV2,F11,F21,F12,F22)
          ELSEIF(CHDON(1:1).EQ.'H') THEN
            TROUVE(3)=.TRUE.
            MARUT=.TRUE.
            CALL MARUTI(X,Y,NPOIN,NDON,FFORMAT,NBOR,NPTFR,AT,DDC,
     &                  TV1,TV2,F31,F32)
          ENDIF
!
        ELSE
!
          WRITE(LU,*) '************************************************'
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'NOUDON : INDICATEUR DE FORMAT INCONNU : ',INDIC
          ELSE
            WRITE(LU,*)'NOUDON : UNKNOWN INDICATOR OF FORMAT : ',INDIC
          ENDIF
          WRITE(LU,*) '************************************************'
          CALL PLANTE(1)
        ENDIF
!
      ELSE
!
        TROUVE(1)=.FALSE.
        TROUVE(2)=.FALSE.
        TROUVE(3)=.FALSE.
        DO I =1,NVAR
          DO J=1,3
            IF((TEXTE(I).EQ.NAMEFR(J).OR.TEXTE(I).EQ.NAMEGB(J)).AND.
     &        MODE(J).GT.0) THEN
              TROUVE(J)=.TRUE.
            ENDIF
          ENDDO
        ENDDO
        IF(COUUT.OR.VENUT) THEN
          TROUVE(1)=.TRUE.
          TROUVE(2)=.TRUE.
        ENDIF
        IF(MARUT) TROUVE(3)=.TRUE.
!
      ENDIF
!
!     --------------------------------------------------------------
!        INTERPOLATES
!     --------------------------------------------------------------
!
      COEF=(AT-TV1)/(TV2-TV1)
!
      IF(TROUVE(1)) THEN
        DO I=1,NPOIN
          F1(I)=(F12(I)-F11(I))*COEF+F11(I)
        ENDDO
      ENDIF
!
      IF(TROUVE(2)) THEN
        DO I=1,NPOIN
          F2(I)=(F22(I)-F21(I))*COEF+F21(I)
        ENDDO
      ENDIF
!
      IF(TROUVE(3)) THEN
        DO I=1,NPOIN
          F3(I)=(F32(I)-F31(I))*COEF+F31(I)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
!     IF FAILED TO READ THE FILE ...
!
      WRITE(LU,*)'*********************************************'
      IF (LNG.EQ.1) THEN
        WRITE(LU,*)'  ERREUR A LA LECTURE DU FICHIER DE DONNEES  '
        WRITE(LU,*)'      OU FIN DE FICHIER PREMATUREE           '
      ELSE
        WRITE(LU,*)'  ERROR WHILE READING DATA FILE '
        WRITE(LU,*)'    OR UNEXPECTED END OF FILE           '
      ENDIF
      WRITE(LU,*)'*********************************************'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END

