!                    *****************
                     SUBROUTINE NOUDON
!                    *****************
!
     &(F1,NAME1FR,NAME1GB,MODE1,
     & F2,NAME2FR,NAME2GB,MODE2,
     & F3,NAME3FR,NAME3GB,MODE3,X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,
     & AT,DDC,TV1,TV2,F11,F12,F21,F22,F31,F32,INDIC,CHDON,NVAR,TEXTE,
     & TROUVE,UNITIME)
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
!| NPOIN          |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NVAR           |-->| NUMBER OF VARIABLES TO BE READ
!| TV1            |<->| TIME T1 IN THE DATA FILE
!| TV2            |<->| TIME T2 IN THE DATA FILE
!| UNITIME        |-->| UNIT OF TIME IN FILE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_NOUDON => NOUDON
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NDON,NPOIN,NPTFR,INDIC,NVAR
      INTEGER, INTENT(IN)             :: MODE1,MODE2,MODE3
      INTEGER, INTENT(IN)             :: NBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: F1(NPOIN),F2(NPOIN),F3(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: F11(NPOIN),F21(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: F12(NPOIN),F22(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: F31(NPOIN),F32(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: AT,DDC,UNITIME
      DOUBLE PRECISION, INTENT(INOUT) :: TV1,TV2
      CHARACTER(LEN=3), INTENT(IN)    :: BINDON
      CHARACTER(LEN=7), INTENT(IN)    :: CHDON
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1FR,NAME2FR,NAME3FR
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1GB,NAME2GB,NAME3GB
      CHARACTER(LEN=32),INTENT(IN)    :: TEXTE(30)
      LOGICAL, INTENT(INOUT)          :: TROUVE(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,ISTAT,IW(1),MODE(3)
      DOUBLE PRECISION DAT2,DAT2B(1),Z(1),C,COEF
      CHARACTER(LEN=3) C1
      CHARACTER(LEN=32) NAMEFR(3),NAMEGB(3)
      LOGICAL VOID
!
      INTRINSIC TRIM
!
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPOIN))
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
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) '   NOUDON : LECTURE D''UN NOUVEL ENREGISTREMENT'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) '   NOUDON : READING A NEW RECORD'
        ENDIF
!
        IF(INDIC.EQ.3) THEN
!
!     ------------------------------------------------------------------
!       READS A SELAFIN FILE OF TYPE: TELEMAC
!     ------------------------------------------------------------------
!
 95     CONTINUE
!
!       ----------------------------------------------------------------
!       GOES TO NEXT RECORD : 2 BECOMES 1 AND READS A NEW 2
!       ----------------------------------------------------------------    
!
        TV1=TV2   
!
!       READS THE DATE OF THE RECORD
        CALL LIT(DAT2B,W,IW,C1,1,'R4',NDON,BINDON,ISTAT)
        TV2=DAT2B(1)*UNITIME
!
!       HERE THE POSSIBLE DATE IN THE FILE SHOULD BE TRANSMITTED
!
!       READS THE DATA
!
        TROUVE(1)=.FALSE.
        TROUVE(2)=.FALSE.
        TROUVE(3)=.FALSE.
        DO I =1,NVAR
          VOID=.TRUE.
          DO J=1,3
           IF((TEXTE(I).EQ.NAMEFR(J).OR.TEXTE(I).EQ.NAMEGB(J)).AND.
     &       MODE(J).GT.0) THEN
             IF(J.EQ.1) THEN
               CALL OV('X=Y     ', F11 , F12 , Z , C , NPOIN)
               CALL LIT(F12,W,IW,C1,NPOIN,'R4',NDON,BINDON,ISTAT)
             ELSEIF(J.EQ.2) THEN
               CALL OV('X=Y     ', F21 , F22 , Z , C , NPOIN)
               CALL LIT(F22,W,IW,C1,NPOIN,'R4',NDON,BINDON,ISTAT)
             ELSEIF(J.EQ.3) THEN
               CALL OV('X=Y     ', F31 , F32 , Z , C , NPOIN)
               CALL LIT(F32,W,IW,C1,NPOIN,'R4',NDON,BINDON,ISTAT)
             ENDIF
             TROUVE(J)=.TRUE.
             VOID=.FALSE.
           ENDIF
         ENDDO
         IF(VOID) READ(NDON)
       ENDDO
!
       IF(TV2.LT.AT) THEN
         IF(LNG.EQ.1) THEN
           WRITE(LU,*) ' NOUDON : ON SAUTE 1 ENREGISTREMENT'
         ELSEIF(LNG.EQ.2) THEN
           WRITE(LU,*) ' NOUDON: JUMP OF 1 DATA RECORD'
         ENDIF
         GO TO 95
       ENDIF
!
       DO J=1,3
         IF(MODE(J).EQ.2.AND..NOT.TROUVE(J)) THEN
           IF(LNG.EQ.1) THEN
             WRITE(LU,*) 'NOUDON : VARIABLE ',J,' NON TROUVEE'
             WRITE(LU,*) TRIM(NAMEFR(J)(1:16)),' OU ',
     &                   TRIM(NAMEGB(J)(1:16))
           ELSEIF(LNG.EQ.2) THEN
             WRITE(LU,*) 'NOUDON: VARIABLE ',NAME1GB,' NOT FOUND'
             WRITE(LU,*) TRIM(NAMEFR(J)(1:16)),' OR ',
     &                   TRIM(NAMEGB(J)(1:16))
           ENDIF
           CALL PLANTE(1)
           STOP
         ELSEIF(MODE(J).GT.0.AND.TROUVE(J)) THEN
           IF(LNG.EQ.1) THEN
             WRITE(LU,*) 'VARIABLE ',J,' LUE (',
     &       TRIM(NAMEFR(J)(1:16)),' OU ',
     &       TRIM(NAMEGB(J)(1:16)),') AU TEMPS ',AT
             WRITE(LU,*) 'PAR INTERPOLATION ENTRE T=',TV1,' ET ',TV2
           ELSEIF(LNG.EQ.2) THEN
             WRITE(LU,*) 'VARIABLE ',J,' READ (',
     &       TRIM(NAMEFR(J)(1:16)),' OR ',
     &       TRIM(NAMEGB(J)(1:16)),') AT TIME ',AT
             WRITE(LU,*) 'BY INTERPOLATION BETWEEN T=',TV1,' AND ',TV2
           ENDIF
         ENDIF
       ENDDO
!
       ELSEIF (INDIC.EQ.4) THEN
!
!     ------------------------------------------------------------------
!        READS A USER-DEFINED FILE FORMAT
!     ------------------------------------------------------------------
!
          IF(CHDON(1:1).EQ.'C') THEN
            CALL COUUTI(X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &                  F11,F21,F12,F22)
          ELSEIF(CHDON(1:1).EQ.'V'.OR.CHDON(1:1).EQ.'W') THEN
            CALL VENUTI(X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &                  F11,F21,F12,F22)
          ELSEIF(CHDON(1:1).EQ.'H') THEN
            CALL MARUTI(X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &                  F31,F32)
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
      ENDIF
!
!       --------------------------------------------------------------
!          INTERPOLATES
!       --------------------------------------------------------------
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
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END

