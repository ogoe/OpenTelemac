!                    *****************
                     SUBROUTINE LECDON
!                    *****************
!
     &(F1,NAME1FR,NAME1GB,MODE1,
     & F2,NAME2FR,NAME2GB,MODE2,
     & F3,NAME3FR,NAME3GB,MODE3,
     & X,Y,NPOIN2,NDON,FFORMAT,NBOR,NPTFR,NPTT,INDIC,CHDON,TEXTE,TROUVE)
!
!***********************************************************************
! TOMAWAC   V6P3                                   21/06/2011
!***********************************************************************
!
!brief    THIS SUBROUTINE PROJECTS THE CURRENTS / WINDS ON THE
!+                COMPUTATION MESH.
!+
!+           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D)
!
!history  F.MARCOS (LNH)
!+        01/02/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        16/11/2012
!+        V6P3
!+   Only SELAFIN format with same mesh kept.
!
!history  J-M HERVOUET (EDF - LNHE)
!+        21/01/2013
!+        V6P3
!+   Generalised for reading 3 variables with given names.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHDON          |-->| NAME OF THE VARIABLE READ FROM THE DATA FILE
!| F1             |<--| FIRST VARIABLE TO READ
!| F2             |<--| SECOND VARIABLE TO READ
!| F3             |<--| THIRD VARIABLE TO READ
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
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTT           |-->| TIME STEP NUMBER IN TELEMAC FILE
!| TEXTE          |<->| NAME OF VARIABLES IN THE SERAFIN FILE
!| TROUVE         |<->| 3 LOGICAL, WILL SAY IF VARIABLES HAVE BEEN FOUND
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NDON,NPOIN2,NPTFR,INDIC
      INTEGER, INTENT(INOUT)          :: NPTT
      INTEGER, INTENT(IN)             :: MODE1,MODE2,MODE3
      INTEGER, INTENT(IN)             :: NBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F1(NPOIN2),F2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F3(NPOIN2)
      CHARACTER(LEN=8), INTENT(IN)    :: FFORMAT
      CHARACTER(LEN=7), INTENT(IN)    :: CHDON
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1FR,NAME2FR,NAME3FR
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1GB,NAME2GB,NAME3GB
      CHARACTER(LEN=32),INTENT(INOUT) :: TEXTE(30)
      LOGICAL, INTENT(INOUT)          :: TROUVE(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=16), ALLOCATABLE :: VAR_NAME(:), VAR_UNIT(:)
      INTEGER NP,I,J,NVAR,MODE(3)
      DOUBLE PRECISION T1VOID,T2VOID,TIME
      CHARACTER(LEN=32) NAMEFR(3),NAMEGB(3),FULL_NAME(3)
      CHARACTER(LEN=80) TITCAS
      INTEGER :: RECORD, IERR
!
      INTRINSIC TRIM
!
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
!     READS THE POINTS FROM LOGICAL UNIT NDON
!-----------------------------------------------------------------------
!
      IF(INDIC.EQ.3) THEN
!
!       -----------------------------------------------------------------
!       TELEMAC FORMAT,
!       VARIABLES 1 AND 2 ARE THE X AND Y COMPONENTS OF THE WIND
!       -----------------------------------------------------------------
!
        ! Getting title
        CALL GET_MESH_TITLE(FFORMAT,NDON,TITCAS,IERR)
        CALL CHECK_CALL(IERR,'LECDON:GET_MESH_TITLE')
        !
        CALL GET_DATA_NVAR(FFORMAT,NDON,NVAR,IERR)
        CALL CHECK_CALL(IERR,'LECDON:GET_DATA_NVAR')
        !
        ALLOCATE(VAR_NAME(NVAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'LECDON:VAR_NAME')
        ALLOCATE(VAR_UNIT(NVAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'LECDON:VAR_UNIT')
        CALL GET_DATA_VAR_LIST(FFORMAT,NDON,NVAR,VAR_NAME,VAR_UNIT,IERR)
        CALL CHECK_CALL(IERR,'LECDON:GET_DATA_VAR_LIST')
        DO I=1,NVAR
          TEXTE(I)(1:16) = VAR_NAME(I)
          TEXTE(I)(17:32) = VAR_UNIT(I)
          ! CHECK IF THE VARIABLE ARE IN THE FILE
          DO J=1,3
            IF((TEXTE(I).EQ.NAMEFR(J)).AND.
     &        MODE(J).GT.0) THEN
              TROUVE(J) = .TRUE.
              FULL_NAME(J) = NAMEFR(J)
            ENDIF
            IF((TEXTE(I).EQ.NAMEGB(J)).AND.
     &        MODE(J).GT.0) THEN
              TROUVE(J) = .TRUE.
              FULL_NAME(J) = NAMEGB(J)
            ENDIF
          ENDDO
        ENDDO
        DEALLOCATE(VAR_NAME)
        DEALLOCATE(VAR_UNIT)
        ! get the number of point
        CALL GET_MESH_NPOIN(FFORMAT,NDON,POINT_BND_ELT_TYPE,NP,IERR)
        CALL CHECK_CALL(IERR,'LECDON:GET_MESH_NPOIN')
        WRITE(LU,*) '--------------------------------------------'
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECDON : LECTURE DU FICHIER TELEMAC'
          WRITE(LU,*) '         TITRE DU CAS LU : ',TITCAS
          WRITE(LU,*) '         NOMBRE DE POINTS   : ',NP
        ELSE
          WRITE(LU,*) 'LECDON : READING OF TELEMAC DATA FILE '
          WRITE(LU,*) '         FILE TITLE : ',TITCAS
          WRITE(LU,*) '         NUMBER OF POINTS   : ',NP
        ENDIF
        WRITE(LU,*) '--------------------------------------------'
        IF(NP.NE.NPOIN2) THEN
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'LE MAILLAGE DU FICHIER DES COURANTS EST'
            WRITE(LU,*) 'DIFFERENT DE CELUI DU FICHIER DE GEOMETRIE'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*) 'THE MESH OF THE CURRENTS FILE'
            WRITE(LU,*) 'IS DIFFERENT FROM THE GEOMETRY FILE'
          ENDIF
          WRITE(LU,*) ' '
          CALL PLANTE(1)
          STOP
        ENDIF
        ! Timesteps
        CALL GET_DATA_NTIMESTEP(FFORMAT,NDON,NPTT,IERR)
        RECORD = NPTT - 1
        CALL GET_DATA_TIME(FFORMAT,NDON,RECORD,TIME,IERR)
        ! Check if all the variables are found for record1
        DO J=1,3
          IF(MODE(J).EQ.2.AND..NOT.TROUVE(J)) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'LECDON : VARIABLE ',J,' NON TROUVEE'
              WRITE(LU,*) TRIM(NAMEFR(J)(1:16)),' OU ',
     &                    TRIM(NAMEGB(J)(1:16))
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'LECDON: VARIABLE ',NAME1GB,' NOT FOUND'
              WRITE(LU,*) TRIM(NAMEFR(J)(1:16)),' OR ',
     &                    TRIM(NAMEGB(J)(1:16))
            ENDIF
            CALL PLANTE(1)
            STOP
          ELSEIF(MODE(J).GT.0.AND.TROUVE(J)) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'VARIABLE ',J,' LUE (',
     &        TRIM(NAMEFR(J)(1:16)),' OU ',
     &        TRIM(NAMEGB(J)(1:16)),') AU TEMPS ',TIME
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'VARIABLE ',J,' READ (',
     &        TRIM(NAMEFR(J)(1:16)),' OR ',
     &        TRIM(NAMEGB(J)(1:16)),') AT TIME ',TIME
            ENDIF
            ! Read the data for varialbe j on record1
            IF(J.EQ.1) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD,
     &                            FULL_NAME(J),F1,NP,IERR)
            ELSEIF(J.EQ.2) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD,
     &                            FULL_NAME(J),F2,NP,IERR)
            ELSEIF(J.EQ.3) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD,
     &                            FULL_NAME(J),F3,NP,IERR)
            ENDIF
          ENDIF
        ENDDO
!
      ELSEIF(INDIC.EQ.4) THEN
!
!     ------------------------------------------------------------------
!       READS A USER-DEFINED FORMAT
!     ------------------------------------------------------------------
!
        IF(CHDON(1:1).EQ.'C') THEN
!         READS A CURRENT FIELD
          CALL COUUTI(X,Y,NPOIN2,NDON,FFORMAT,NBOR,NPTFR,
     &                0.D0,0.D0,0.D0,0.D0,F1,F2,F1,F2)
        ELSEIF(CHDON(1:1).EQ.'W') THEN
!         READS A WIND FIELD
          CALL VENUTI(X,Y,NPOIN2,NDON,FFORMAT,NBOR,NPTFR,
     &                0.D0,0.D0,T1VOID,T2VOID,F1,F2,F1,F2)
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'LE TYPE DE DONNEES A LIRE EST INCONNU'
          ELSE
            WRITE(LU,*) 'UNKNOWN DATA'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
        WRITE(LU,*)'***********************************************'
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'LECDON : INDICATEUR DE FORMAT INCONNU   '
          WRITE(LU,*)'         POUR LE FICHIER DES DONNEES :',INDIC
        ELSE
          WRITE(LU,*)'LECDON : INDICATOR OF FORMAT FOR THE   '
          WRITE(LU,*)'         DATA FILE UNKNOWN :',INDIC
        ENDIF
        WRITE(LU,*)'***********************************************'
        CALL PLANTE(1)
        STOP
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
!
!-----------------------------------------------------------------------
!
      RETURN
      END
