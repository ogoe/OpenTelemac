!                    *****************
                     SUBROUTINE LECDOI
!                    *****************
!
     &(F1,NAME1FR,NAME1GB,MODE1,
     & F2,NAME2FR,NAME2GB,MODE2,
     & F3,NAME3FR,NAME3GB,MODE3,
     & X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,
     & AT,DDC,TV1,TV2,F11,F12,F21,F22,F31,F32,INDIC,CHDON,NVAR,TEXTE,
     & TROUVE)
!
!***********************************************************************
! TOMAWAC   V6P3                                   20/06/2011
!***********************************************************************
!
!brief    THIS SUBROUTINE PROJECTS THE CURRENTS / WINDS ON THE
!+                COMPUTATION MESH AND INTERPOLATES TO FIRST TIME STEP.
!+
!+           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D AMONGST OTHERS)
!
!history
!+
!+        V5P0
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
!history  G.MATTAROLO (EDF)
!+        05/2011
!+        V6P1
!+   Bug correction in the reading of the TELEMAC format file
!
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        16/11/2012
!+        V6P3
!+   Only SELAFIN format with same mesh kept. A number of arguments
!+   removed
!
!history  J-M HERVOUET (EDF - LNHE)
!+        24/01/2013
!+        V6P3
!+   Generalised for reading 3 variables with given names.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| BINDON         |-->| DATA FILE BINARY
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
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NVAR           |<--| NUMBER OF VARIABLES READ
!| TEXTE          |<->| NAMES OF VARIABLES IN SERAFIN FILE
!| TROUVE         |<->| 3 LOGICAL, WILL SAY IF VARIABLES HAVE BEEN FOUND
!| TV1            |<->| DATA TIME T1
!| TV2            |<->| DATA TIME T2
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_LECDOI => LECDOI
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NDON,NPOIN2,NPTFR,INDIC
      INTEGER, INTENT(IN)             :: MODE1,MODE2,MODE3
      INTEGER, INTENT(INOUT)          :: NVAR
      INTEGER, INTENT(IN)             :: NBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F1(NPOIN2),F2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F3(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F11(NPOIN2),F12(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F21(NPOIN2),F22(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F31(NPOIN2),F32(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: AT,DDC
      DOUBLE PRECISION, INTENT(INOUT) :: TV1,TV2
      CHARACTER(LEN=3), INTENT(IN)    :: BINDON
      CHARACTER(LEN=7), INTENT(IN)    :: CHDON
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1FR,NAME2FR,NAME3FR
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1GB,NAME2GB,NAME3GB
      CHARACTER(LEN=32),INTENT(INOUT) :: TEXTE(30)
      LOGICAL, INTENT(INOUT)          :: TROUVE(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NP,I,J,MODE(3),ISTAT,IB(10)
      CHARACTER(LEN=3) C
      DOUBLE PRECISION COEF,ATB(1),Z(1)
      CHARACTER(LEN=72) TITCAS
      CHARACTER(LEN=32) NAMEFR(3),NAMEGB(3)
      LOGICAL VOID
!
      INTRINSIC TRIM
!
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPOIN2))
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
        REWIND NDON
!
!       READS TITLE
!
        CALL LIT(Z,W,IB,TITCAS,72,'CH',NDON,BINDON,ISTAT)
!
!       READS NUMBER OF VARIABLES AND THEIR NAMES
!
        CALL LIT(Z,W,IB,C,2,'I ',NDON,BINDON,ISTAT)
        NVAR=IB(1)
        DO I=1,NVAR
          CALL LIT(Z,W,IB,TEXTE(I),32,'CH',NDON,BINDON,ISTAT)
        ENDDO
!
!       FORMAT AND GEOMETRY
!
        CALL LIT(Z,W,IB,C,10,'I ',NDON,BINDON,ISTAT)
        IF(IB(10).EQ.1) THEN
!         THIS IS THE DATE : YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
          CALL LIT(Z,W,IB,C,6,'I ',NDON,BINDON,ISTAT)
        ENDIF
        CALL LIT(Z,W,IB,C, 4,'I ',NDON,BINDON,ISTAT)
        NP=IB(2)
        WRITE(LU,*) '--------------------------------------------'
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECDOI : LECTURE DU FICHIER TELEMAC'
          WRITE(LU,*) '         TITRE DU CAS LU : ',TITCAS
          WRITE(LU,*) '         NOMBRE DE POINTS   : ',NP
        ELSE
          WRITE(LU,*) 'LECDOI : READING OF TELEMAC DATA FILE '
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
!
!       ARRAY OF INTEGERS IKLE
!
        READ(NDON)
!
!       ARRAY OF INTEGERS IPOBO
!
        READ(NDON)
!
!       X AND Y
!
        READ(NDON)
        READ(NDON)
!
!       TIME STEP AND VARIABLES
!
        CALL LIT(ATB,W,IB,C,1,'R4',NDON,BINDON,ISTAT)
        TV1=ATB(1)
!
!       HERE THE DATE, IF PRESENT, SHOULD BE TAKEN INTO ACCOUNT
!
        IF(TV1.GT.AT) THEN
          WRITE(LU,*) '************************************************'
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'LE PREMIER ENREGISTREMENT DU FICHIER DE ',CHDON
            WRITE(LU,*) '  ',TV1,' EST POSTERIEUR AU TEMPS '
            WRITE(LU,*) '  DU DEBUT DU CALCUL',AT
          ELSE
            WRITE(LU,*) 'THE FIRST RECORDING OF THE ',CHDON,' FILE '
            WRITE(LU,*) '  ',TV1,' IS OLDER THAN THE BEGINNING '
            WRITE(LU,*) '  OF THE COMPUTATION',AT
          ENDIF
          WRITE(LU,*) '************************************************'
          CALL PLANTE(1)
          STOP
        ENDIF
!
110     CONTINUE
!
        TROUVE(1)=.FALSE.
        TROUVE(2)=.FALSE.
        TROUVE(3)=.FALSE.
        DO I=1,NVAR
          VOID=.TRUE.
          DO J=1,3
            IF((TEXTE(I).EQ.NAMEFR(J).OR.TEXTE(I).EQ.NAMEGB(J)).AND.
     &        MODE(J).GT.0) THEN
              IF(J.EQ.1) THEN
                CALL LIT(F11,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
              ELSEIF(J.EQ.2) THEN
                CALL LIT(F21,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
              ELSEIF(J.EQ.3) THEN
                CALL LIT(F31,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
              ENDIF
              TROUVE(J)=.TRUE.
              VOID=.FALSE.
            ENDIF
          ENDDO
          IF(VOID) READ(NDON)
        ENDDO
!
        DO J=1,3
          IF(MODE(J).EQ.2.AND..NOT.TROUVE(J)) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'LECDOI : VARIABLE ',J,' NON TROUVEE'
              WRITE(LU,*) TRIM(NAMEFR(J)(1:16)),' OU ',
     &                    TRIM(NAMEGB(J)(1:16))
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'LECDOI: VARIABLE ',NAME1GB,' NOT FOUND'
              WRITE(LU,*) TRIM(NAMEFR(J)(1:16)),' OR ',
     &                    TRIM(NAMEGB(J)(1:16))
            ENDIF
            CALL PLANTE(1)
            STOP
          ELSEIF(MODE(J).GT.0.AND.TROUVE(J)) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'VARIABLE ',J,' LUE (',
     &        TRIM(NAMEFR(J)(1:16)),' OU ',
     &        TRIM(NAMEGB(J)(1:16)),') AU TEMPS ',TV1
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'VARIABLE ',J,' READ (',
     &        TRIM(NAMEFR(J)(1:16)),' OR ',
     &        TRIM(NAMEGB(J)(1:16)),') AT TIME ',TV1
            ENDIF
          ENDIF
        ENDDO
!
        CALL LIT(ATB,W,IB,C,1,'R4',NDON,BINDON,ISTAT)
        TV2=ATB(1)
        IF(TV2.LT.AT) THEN
          TV1=TV2
          GOTO 110
        ENDIF
!
        TROUVE(1)=.FALSE.
        TROUVE(2)=.FALSE.
        TROUVE(3)=.FALSE.
        DO I=1,NVAR
          VOID=.TRUE.
          DO J=1,3
            IF((TEXTE(I).EQ.NAMEFR(J).OR.TEXTE(I).EQ.NAMEGB(J)).AND.
     &        MODE(J).GT.0) THEN
              IF(J.EQ.1) THEN
                CALL LIT(F12,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
              ELSEIF(J.EQ.2) THEN
                CALL LIT(F22,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
              ELSEIF(J.EQ.3) THEN
                CALL LIT(F32,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
              ENDIF
              TROUVE(J)=.TRUE.
              VOID=.FALSE.
            ENDIF
          ENDDO
          IF(VOID) READ(NDON)
        ENDDO
!
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
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'VARIABLE ',J,' LUE (',
     &        TRIM(NAMEFR(J)(1:16)),' OU ',
     &        TRIM(NAMEGB(J)(1:16)),') AU TEMPS ',TV2
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'VARIABLE ',J,' READ (',
     &        TRIM(NAMEFR(J)(1:16)),' OR ',
     &        TRIM(NAMEGB(J)(1:16)),') AT TIME ',TV2
            ENDIF
          ENDIF
        ENDDO
!
      ELSEIF (INDIC.EQ.4) THEN
!
!       READS A USER-DEFINED FORMAT
!
        IF(CHDON(1:1).EQ.'C') THEN
!         READS A CURRENT FIELD
          TROUVE(1)=.TRUE.
          TROUVE(2)=.TRUE.
          CALL COUUTI(X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &                F11,F21,F12,F22)
        ELSEIF(CHDON(1:1).EQ.'V') THEN
!         READS A WIND FIELD
          TROUVE(1)=.TRUE.
          TROUVE(2)=.TRUE.
          CALL VENUTI(X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &                F11,F21,F12,F22)
        ELSEIF(CHDON(1:1).EQ.'H') THEN
!         READS A DEPTH FIELD
          TROUVE(3)=.TRUE.
          CALL MARUTI(X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &                F31,F32)
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'LE TYPE DE DONNEES A LIRE EST INCONNU'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*) 'UNKNOWN DATA'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
        WRITE(LU,*) '************************************************'
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECDOI : INDICATEUR DE FORMAT INCONNU : ',INDIC
        WRITE(LU,*) '         POUR LE FICHIER DE ',CHDON
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*)'LECDOI : UNKNOWN INDICATOR OF FORMAT : ',INDIC
          WRITE(LU,*)'         FOR THE ',CHDON,' DATA FILE '
        ENDIF
        WRITE(LU,*) '************************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!   INTERPOLATES IN TIME
!-----------------------------------------------------------------------
!
      COEF=(AT-TV1)/(TV2-TV1)
!
      IF(TROUVE(1)) THEN
        DO I=1,NPOIN2
          F1(I)=(F12(I)-F11(I))*COEF+F11(I)
        ENDDO
      ENDIF
      IF(TROUVE(2)) THEN
        DO I=1,NPOIN2
          F2(I)=(F22(I)-F21(I))*COEF+F21(I)
        ENDDO
      ENDIF
      IF(TROUVE(3)) THEN
        DO I=1,NPOIN2
          F3(I)=(F32(I)-F31(I))*COEF+F31(I)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FORMATS
!
10    FORMAT (2I4,4F9.3,2I2)
20    FORMAT (10F6.2)
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(W)
!
      RETURN
!
!     IF FAILED TO READ THE FILE ...
!
100   CONTINUE
      WRITE(LU,*)'*********************************************'
      IF(LNG.EQ.1) THEN
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

