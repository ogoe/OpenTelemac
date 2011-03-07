!                    *****************
                     SUBROUTINE NOUDON
!                    *****************
!
     &(UV , VV , X  , Y  , NPOIN, NDON , BINDON, NBOR, NPTFR,
     & AT , DDC, TV1, TV2, NP   , XRELV, YRELV , UR  , VR   ,
     & TRA, U1 , V1 , U2 , V2   , INDIC, CHDON , NVAR)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE CURRENT / WIND VELOCITY
!+                FOR THE CURRENT TIME STEP AND ON THE COMPUTATION MESH.
!+
!+           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TEMPS
!| BINDON         |-->| BINAIRE DU FICHIER DE DONNEES
!| CHDON          |---|
!| DDC            |-->| DATE DU DEBUT DU CALCUL
!| INDIC          |-->| TYPE DE FORMAT DE LECTURE
!| NBOR           |-->| NUMEROTATION DES POINTS FRONTIERE
!| NDON           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DE DONNEES
!| NP             |<->| NOMBRE DE POINTS DU MAILLAGE DES DONNEES
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
!| NPTFR          |-->| NOMBRE DE  POINTS FRONTIERE
!| NVAR           |---|
!| TRA            |---|
!| TV1            |<->| TEMPS DU CHAMPS DE DONNEES 1
!| TV2            |<->| TEMPS DU CHAMPS DE DONNEES 2
!| U1,V1,U2,V2    |<->| DONNEES AUX NOEUDS DU MAILLAGE
!| UR,VR          |<->| TABLEAU DES COURANTS RELEVES
!| UV,VV          |<--| DONNEE AUX NOEUDS DU MAILLAGE
!| X,Y            |-->| COORDONNEES DU MAILLAGE
!| XRELV          |<--| TABLEAU DES ABSCISSES DES POINTS RELEVES
!| YRELV          |<--| TABLEAU DES ORDONNEES DES POINTS RELEVES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      USE INTERFACE_TOMAWAC, EX_NOUDON => NOUDON
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NP,NDON,NPOIN,NPTFR,INDIC,I,ISTAT,NVAR,IW(1)
!
      INTEGER NBOR(NPTFR,2),ID(2)
!
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION UV(NPOIN),VV(NPOIN),UR(NP),VR(NP)
      DOUBLE PRECISION U1(NPOIN),V1(NPOIN),U2(NPOIN),V2(NPOIN)
      DOUBLE PRECISION XRELV(NP),YRELV(NP),TRA(NP)
      DOUBLE PRECISION AT,TV1,TV2
      DOUBLE PRECISION DDC,DAT2,DAT2B(1),Z(1),C,COEF
!
      CHARACTER*3 BINDON, C1
      CHARACTER*7 CHDON
!
!-----------------------------------------------------------------------
!
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NP))
!
!-----------------------------------------------------------------------
!
      IF (AT.GT.TV2) THEN
!
!       ----------------------------------------------------------------
!       GOES TO NEXT RECORD : 2 BECOMES 1 AND READS A NEW 2
!       ----------------------------------------------------------------
        TV1=TV2
        CALL OV('X=Y     ', U1 , U2 , Z , C , NPOIN)
        CALL OV('X=Y     ', V1 , V2 , Z , C , NPOIN)
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) '   NOUDON : LECTURE D''UN NOUVEL ENREGISTREMENT'
        ELSE
          WRITE(LU,*) '   NOUDON : READING A NEW RECORDING'
        ENDIF
!
        IF (INDIC.EQ.1) THEN
!
!     ------------------------------------------------------------------
!          READS A FORMATTED FINITE DIFFERENCES FILE OF TYPE: WAM CYCLE 4
!     ------------------------------------------------------------------
 90        CONTINUE
!          READS THE DATE OF THE RECORD
           READ(NDON,*,END=100,ERR=100) DAT2
           CALL TEMP(TV2,DAT2,DDC)
!          READS THE DATA
           READ(NDON,*,END=100,ERR=100)
           READ(NDON,20,END=100,ERR=100) (UR(I),I=1,NP)
           READ(NDON,*,END=100,ERR=100)
           READ(NDON,20,END=100,ERR=100) (VR(I),I=1,NP)
!
           IF (TV2.LT.AT) THEN
             IF(LNG.EQ.1) THEN
               WRITE(LU,*) ' NOUDON : ON SAUTE 1 ENREGISTREMENT ..'
             ELSE
               WRITE(LU,*) ' NOUDON : JUMP OF 1 RECORDED DATA SERIES'
             ENDIF
             TV1=TV2
             CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,
     &                                       MESH%KP1BOR%I,NPTFR,0.D0)
             CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,
     &                                       MESH%KP1BOR%I,NPTFR,0.D0)
             GOTO 90
           ENDIF
!
           CALL FASP(X,Y,U2,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
           CALL FASP(X,Y,V2,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
!
        ELSEIF (INDIC.EQ.3) THEN
!
!     ------------------------------------------------------------------
!       READS A SELAFIN FILE OF TYPE: TELEMAC
!     ------------------------------------------------------------------
!
        ID(1)=1
        ID(2)=2
 95     CONTINUE
!       READS THE DATE OF THE RECORD
        CALL LIT(DAT2B,W,IW,C1,1,'R4',NDON,BINDON,ISTAT)
        IF(CHDON(1:1).EQ.'C') THEN
         TV2=DAT2B(1)
        ELSE
         DAT2=DAT2B(1)*1.D2
         CALL TEMP(TV2,DAT2,DDC)
        ENDIF
!      READS THE DATA
       DO I =1,NVAR
        IF(I.EQ.ID(1)) THEN
         CALL LIT(UR,W,IW,C1,NP,'R4',NDON,BINDON,ISTAT)
        ELSEIF(I.EQ.ID(2)) THEN
         CALL LIT(VR,W,IW,C1,NP,'R4',NDON,BINDON,ISTAT)
        ELSE
         READ(NDON)
        ENDIF
       ENDDO
!
        IF (TV2.LT.AT) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) ' NOUDON : ON SAUTE 1 ENREGISTREMENT ..'
          ELSE
            WRITE(LU,*) ' NOUDON : JUMP OF 1 RECORDED DATA SERIES'
          ENDIF
          TV1=TV2
!         INTERPOLATES IN SPACE
          CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
          CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
          GOTO 95
        ENDIF
!
        WRITE(LU,*) 'T',CHDON,'1:',TV1
        WRITE(LU,*) 'T',CHDON,'2:',TV2
!
!       INTERPOLATES IN SPACE
        CALL FASP(X,Y,U2,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
        CALL FASP(X,Y,V2,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
!
        ELSEIF (INDIC.EQ.4) THEN
!
!     ------------------------------------------------------------------
!        READS A USER-DEFINED FILE FORMAT
!     ------------------------------------------------------------------
!
          IF(CHDON(1:1).EQ.'C') THEN
          CALL COUUTI
     &    (X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NP)
          ELSE
          CALL VENUTI
     &    (X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NP)
          ENDIF
!
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
      DO 60 I=1,NPOIN
         UV(I)=(U2(I)-U1(I))*COEF+U1(I)
         VV(I)=(V2(I)-V1(I))*COEF+V1(I)
60    CONTINUE
!
!-----------------------------------------------------------------------
!
!     FORMATS
!
20    FORMAT (10F6.2)
!
      DEALLOCATE(W)
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
      RETURN
      END