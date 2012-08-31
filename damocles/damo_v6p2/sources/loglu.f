!                    **********************
                     LOGICAL FUNCTION LOGLU
!                    **********************
!
     &( ICOL , LIGNE )
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    DECODES A LOGICAL VALUE, FROM COLUMN ICOL+1 OF THE LINE.
!+             IF THE STRING IS NOT COMPLETE, GOES TO THE NEXT LINE
!+             IF NEED BE.
!+             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER.
!+             OR TO ICOL=0 IF THE NEXT LINE WAS READ.
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!
!warning  ACCEPTED VALUES ARE (UPPER OR LOWER CASE):
!+            VRAI OUI TRUE  YES .TRUE.  1
!+            FAUX NON FALSE NO  .FALSE. 0
!
!history  O. QUIQUEMPOIX (LNH)
!+        15/12/1993
!+
!+
!
!history  J.M. HERVOUET (LNH); A. YESSAYAN
!+        16/08/1994
!+        V5P1
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ICOL           |<->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
!| LIGNE          |<->| LIGNE EN COURS DE DECODAGE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER       ICOL
      CHARACTER*(*) LIGNE
!
      INTEGER  NEXT,PRECAR
      EXTERNAL NEXT,PRECAR
!
      INTEGER       LNG,LU
      INTEGER       NLIGN,LONGLI
      INTEGER       NFIC
      LOGICAL       ERREUR,RETOUR
!
!-----------------------------------------------------------------------
!
      INTEGER       I1,I2
      CHARACTER*1   TABUL
      CHARACTER*7   L
      CHARACTER*72  LIGNE2
      LOGICAL       LUFIC,LISUIV
!
!-----------------------------------------------------------------------
!
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR , RETOUR
      COMMON / DCMLIG / NLIGN , LONGLI
      COMMON / DCCHIE / NFIC
!
      INTRINSIC CHAR
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
      LUFIC  = .FALSE.
      LISUIV = .FALSE.
      LIGNE2 = ' '
      TABUL  = CHAR(9)
!
      I1     = NEXT( ICOL+1 , LIGNE )
      L(1:7) = LIGNE(I1:I1+6)
      I2 = PRECAR(I1,LIGNE,' ',';',TABUL)
!
! CASE WHERE MIGHT HAVE TO READ THE FOLLOWING LINE
!
      IF (I2.GT.LONGLI.AND.(I1+6).GT.LONGLI) THEN
         LUFIC=.TRUE.
         READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
         IF (I1.LE.LONGLI) THEN
           L(1:7)=LIGNE(I1:LONGLI)//LIGNE2(1:(7-(LONGLI-I1+1)))
         ELSE
           L(1:7)=LIGNE2(1:7)
         ENDIF
         I2 = 0
         I2 = PRECAR(I2+1,LIGNE2,' ',';',TABUL)
      ENDIF
      CALL MAJUS(L)
      GO TO 910
!
 900  CONTINUE
      RETOUR = .TRUE.
!
 910  CONTINUE
!
! ORDERED IN THE MOST PROBABLE ORDER: NON OUI NO YES 0 1 ...
!
      IF (L(1:3).EQ.'NON') THEN
            LOGLU = .FALSE.
            ICOL = I1 + 2
      ELSE IF (L(1:2).EQ.'NO') THEN
            LOGLU = .FALSE.
            ICOL = I1 + 1
      ELSE IF ( L(1:3).EQ.'OUI' ) THEN
            LOGLU = .TRUE.
            ICOL = I1 + 2
      ELSE IF ( L(1:3).EQ.'YES' ) THEN
            LOGLU = .TRUE.
            ICOL = I1 + 2
      ELSE IF (L(1:1).EQ.'0') THEN
            LOGLU = .FALSE.
            ICOL = I1
      ELSE IF (L(1:1).EQ.'1') THEN
            LOGLU = .TRUE.
            ICOL = I1
      ELSE IF (L(1:7).EQ.'.FALSE.' ) THEN
            LOGLU = .FALSE.
            ICOL = I1 + 6
      ELSE IF (L(1:5).EQ.'FALSE' ) THEN
            LOGLU = .FALSE.
            ICOL = I1 + 4
      ELSE IF (L(1:4).EQ.'FAUX') THEN
            LOGLU = .FALSE.
            ICOL = I1 + 3
      ELSE IF ( L(1:6).EQ.'.TRUE.' ) THEN
            LOGLU = .TRUE.
            ICOL = I1 + 5
      ELSE IF ( L(1:4).EQ.'TRUE' ) THEN
            LOGLU = .TRUE.
            ICOL = I1 + 3
      ELSE IF ( L(1:4).EQ.'VRAI' ) THEN
            LOGLU = .TRUE.
            ICOL = I1 + 3
      ELSE
!
!     ERROR: NOT A LOGICAL VALUE
!
            ERREUR = .TRUE.
            WRITE(LU,'(1X,A)') LIGNE(1:LONGLI)
            IF (LUFIC) WRITE(LU,'(1X,A)') LIGNE2(1:LONGLI)
            WRITE(LU,*) ' '
            IF(LNG.EQ.1) THEN
              WRITE(LU,'(1X,A6,I4,A)') 'LOGLU (UTILE) : LIGNE: ',NLIGN,
     &                                 ' ERREUR, LOGIQUE MAL CODE'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,'(1X,A6,I4,A)') 'LOGLU (UTILE) : LINE: ',NLIGN,
     &                                 ' WRONG LOGICAL VALUE'
            ENDIF
            LOGLU = .FALSE.
            GO TO 1000
!
      ENDIF
!
!     //// UPDATES THE POINTER ////
!
      IF (LUFIC) THEN
        NLIGN = NLIGN + 1
        LIGNE = LIGNE2
        IF (ICOL.GT.LONGLI) LISUIV = .TRUE.
        IF (LISUIV) THEN
          ICOL = I2-1
        ELSE
          ICOL = 0
        ENDIF
      ELSE
        ICOL = I2 - 1
      ENDIF
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
!
998   CONTINUE
      IF(LNG.EQ.1) WRITE(6,999) NFIC,NLIGN+1
      IF(LNG.EQ.2) WRITE(6,1999) NFIC,NLIGN+1
999   FORMAT(1X,'UNITE LOGIQUE ',1I2,'   ERREUR LIGNE ',1I6)
1999  FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR LINE ',1I6)
      RETOUR = .TRUE.
      RETURN
      END