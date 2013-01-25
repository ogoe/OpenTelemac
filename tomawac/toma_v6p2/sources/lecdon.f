!                    *****************
                     SUBROUTINE LECDON
!                    *****************
!
     &(F1,NAME1FR,NAME1GB,MODE1,
     & F2,NAME2FR,NAME2GB,MODE2,
     & F3,NAME3FR,NAME3GB,MODE3,
     & X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,NPTT,INDIC,CHDON,TEXTE,TROUVE)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BINDON         |-->| BINAIRE DU FICHIER DES DONNEES  (INDIC>2)
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
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NDON,NPOIN2,NPTFR,INDIC,NPTT
      INTEGER, INTENT(IN)             :: MODE1,MODE2,MODE3
      INTEGER, INTENT(IN)             :: NBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F1(NPOIN2),F2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F3(NPOIN2)
      CHARACTER(LEN=3), INTENT(IN)    :: BINDON
      CHARACTER(LEN=7), INTENT(IN)    :: CHDON
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1FR,NAME2FR,NAME3FR
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1GB,NAME2GB,NAME3GB
      CHARACTER(LEN=32),INTENT(INOUT) :: TEXTE(30)
      LOGICAL, INTENT(INOUT)          :: TROUVE(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NP,I,J,NVAR,IB(10),ISTAT,MODE(3)
      DOUBLE PRECISION ATT,BDX(2),Z(1)
      CHARACTER(LEN=3) C
      CHARACTER(LEN=32) NAMEFR(3),NAMEGB(3)
      CHARACTER(LEN=72) TITCAS
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
!     ------------------------------------------------------------------
!     SERAFIN FORMAT 
!     ------------------------------------------------------------------
!
!         READS TITLE
!
          CALL LIT(Z,W,IB,TITCAS,72,'CH',NDON,BINDON,ISTAT)
!
!         READS NUMBER OF VARIABLES AND THEIR NAMES
!
          CALL LIT(Z,W,IB,C,2,'I ',NDON,BINDON,ISTAT)
          NVAR=IB(1)
          DO I=1,NVAR
            CALL LIT(Z,W,IB,TEXTE(I),32,'CH',NDON,BINDON,ISTAT)
          ENDDO
!
!         FORMAT AND GEOMETRY
!
          CALL LIT(Z,W,IB,C,10,'I ',NDON,BINDON,ISTAT)
          IF(IB(10).EQ.1) THEN
!           THIS IS THE DATE : YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
            CALL LIT(Z,W,IB,C,6,'I ',NDON,BINDON,ISTAT)
          ENDIF
          CALL LIT(Z,W,IB,C, 4,'I ',NDON,BINDON,ISTAT)
          NP=IB(2)
          WRITE(LU,*)
     &        '-----------------------------------------------------'
          IF (LNG.EQ.1) THEN
             WRITE(LU,*)'LECDON : LECTURE DU FICHIER TELEMAC'
             WRITE(LU,*)'         NOMBRE DE POINTS   : ',NP
          ELSE
             WRITE(LU,*)'LECDON : READING OF TELEMAC DATA FILE '
             WRITE(LU,*)'         NUMBER OF POINTS   : ',NP
          ENDIF
          IF(NP.NE.NPOIN2) THEN
            WRITE(LU,*) ' '
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'LECDON : LE MAILLAGE DU'
              WRITE(LU,*) 'FICHIER DES COURANTS EST'
              WRITE(LU,*) 'DIFFERENT DE CELUI DU FICHIER DE GEOMETRIE'
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'LECDON: THE MESH OF THE CURRENTS FILE'
              WRITE(LU,*) 'IS DIFFERENT FROM THE GEOMETRY FILE'
            ENDIF
            WRITE(LU,*) ' '
            CALL PLANTE(1)
          ENDIF
          READ(NDON)
          READ(NDON)
!
!         X AND Y
!
          READ(NDON)
          READ(NDON)
!
!         TIME STEP AND VARIABLES
!
          DO J=1,(NPTT-1)*(NVAR+1)
            READ(NDON)
          ENDDO
!
          CALL LIT(BDX(1),W,IB,C,1,'R4',NDON,BINDON,ISTAT)
          ATT=BDX(1)
!
!         HERE THE DATE SHOULD BE TAKEN INTO ACCOUNT IF PRESENT
!
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'         TITRE DU CAS TELEMAC : '
            WRITE(LU,*)'           ',TITCAS
            WRITE(LU,*)'         TEMPS DE TELEMAC : ',ATT
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*)'         TITLE OF TELEMAC CASE : '
            WRITE(LU,*)'           ',TITCAS
            WRITE(LU,*)'         TIME OF TELEMAC RECORD : ',ATT
          ENDIF
!
          TROUVE(1)=.FALSE.
          TROUVE(2)=.FALSE.
          TROUVE(3)=.FALSE.
          DO I=1,NVAR
            VOID=.TRUE.
            DO J=1,3
              IF((TEXTE(I).EQ.NAMEFR(J).OR.TEXTE(I).EQ.NAMEGB(J)).AND.
     &          MODE(J).GT.0) THEN
                IF(J.EQ.1) THEN
                  CALL LIT(F1,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
                ELSEIF(J.EQ.2) THEN
                  CALL LIT(F2,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
                ELSEIF(J.EQ.3) THEN
                  CALL LIT(F3,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
                ENDIF
                TROUVE(J)=.TRUE.
                VOID=.FALSE.
              ENDIF
            ENDDO
            IF(VOID) READ(NDON)
          ENDDO
          DO J=1,3
            IF(MODE(J).EQ.2.AND..NOT.TROUVE(J)) THEN
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'LECDON : VARIABLE ',J,' NON TROUVEE'
                WRITE(LU,*) TRIM(NAMEFR(J)(1:16)),' OU ',
     &                      TRIM(NAMEGB(J)(1:16))
              ELSEIF(LNG.EQ.2) THEN
                WRITE(LU,*) 'LECDON: VARIABLE ',NAME1GB,' NOT FOUND'
                WRITE(LU,*) TRIM(NAMEFR(J)(1:16)),' OR ',
     &                      TRIM(NAMEGB(J)(1:16))
              ENDIF
              CALL PLANTE(1)
              STOP
            ELSEIF(MODE(J).GT.0.AND.TROUVE(J)) THEN
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'VARIABLE ',J,' LUE (',
     &                      TRIM(NAMEFR(J)(1:16)),' OU ',
     &                      TRIM(NAMEGB(J)(1:16))
              ELSEIF(LNG.EQ.2) THEN
                WRITE(LU,*) 'VARIABLE ',J,' READ (',
     &                      TRIM(NAMEFR(J)(1:16)),' OR ',
     &                      TRIM(NAMEGB(J)(1:16))
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
          CALL COUUTI(X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,
     &                0.D0,0.D0,0.D0,0.D0,F1,F2,F1,F2)
        ELSEIF(CHDON(1:1).EQ.'W') THEN
!         READS A WIND FIELD
          CALL VENUTI(X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,
     &                0.D0,0.D0,0.D0,0.D0,F1,F2,F1,F2)
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
      DEALLOCATE(W)
!
      RETURN
!
!     IF FAILED TO READ THE FILE ...
!
100   CONTINUE
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
