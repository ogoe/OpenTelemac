!                    *****************
                     SUBROUTINE LECDOI
!                    *****************
!
!    XRELV, YRELV, UR, VR, TRA, NPMAX A SUPPRIMER
!
     &( UD , VD  , X  , Y  , NPOIN2, NDON , BINDON, NBOR , NPTFR,
     &  AT , DDC , TV1, TV2, NP   , XRELV, YRELV , UR   , VR   ,
     &  TRA, U1  , V1 , U2 , V2   , INDIC, NPMAX , CHDON, NVAR )
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
!+   Only SELAFIN format with same mesh kept.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| BINDON         |-->| DATA FILE BINARY
!| CHDON          |-->| NAME OF THE VARIABLE READ FROM THE DATA FILE
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| INDIC          |-->| FILE FORMAT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDON           |-->| LOGICAL UNIT NUMBER OF THA DATA FILE
!| NP             |<->| NUMBER OF POINTS READ FROM THE FILE
!| NPMAX          |-->| MAXIMUM NUMBER OF POINTS THAT CAN BE READ
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NVAR           |<--| NUMBER OF VARIABLES READ
!| TRA            |-->| WORK TABLE
!| TV1            |<->| DATA TIME T1
!| TV2            |<->| DATA TIME T2
!| U1,V1          |<->| DATA VALUES AT TIME TV1 IN THE DATA FILE
!| U2,V2          |<->| DATA VALUES AT TIME TV2 IN THE DATA FILE
!| UD,VD          |<--| DATA VALUES INTERPOLATED OVEER THE MESH NODES
!| UR,VR          |<->| TABLE OF THE VALUES READ IN THE DATA FILE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XRELV          |<->| TABLE OF THE ABSCISSES OF DATA FILE POINTS
!| X              |-->| ORDINATES OF POINTS IN THE MESH
!| YRELV          |<->| TABLE OF THE ORDINATES OF DATA FILE POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      USE INTERFACE_TOMAWAC, EX_LECDOI => LECDOI
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NP,NDON,NPOIN2,NPTFR,INDIC,NCOL,NLIG,BID,I,J
      INTEGER NVAR,ISTAT,IB(10),ID(2)
!
      INTEGER NPMAX,NBOR(NPTFR,2)
!
      DOUBLE PRECISION X(NPOIN2)    , Y(NPOIN2)
      DOUBLE PRECISION UD(NPOIN2)   , VD(NPOIN2)
      DOUBLE PRECISION XRELV(NPMAX) , YRELV(NPMAX)
      DOUBLE PRECISION UR(NPMAX)    , VR(NPMAX), TRA(NPMAX)
      DOUBLE PRECISION U1(NPOIN2)   , V1(NPOIN2)
      DOUBLE PRECISION U2(NPOIN2)   , V2(NPOIN2)
      DOUBLE PRECISION AT,TV1,TV2
      DOUBLE PRECISION DDC,DAT1,DAT2,COEF,Z(1),ATT, ATB(1)
!
      CHARACTER*3  BINDON,C
      CHARACTER*7  CHDON
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTE(10)
!
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPOIN2))
!
!
!-----------------------------------------------------------------------
!     READS THE POINTS FROM LOGICAL UNIT NDON
!-----------------------------------------------------------------------
!
      IF(INDIC.EQ.3) THEN
!
!      -----------------------------------------------------------------
!      TELEMAC FORMAT,
!      VARIABLES 1 AND 2 ARE THE X AND Y COMPONENTS OF THE WIND
!      -----------------------------------------------------------------
!
       REWIND NDON
       ID(1)=1
       ID(2)=2
!
!      READS TITLE
!
       CALL LIT(X,W,IB,TITCAS,72,'CH',NDON,BINDON,ISTAT)
!
!      READS NUMBER OF VARIABLES AND THEIR NAMES
!
       CALL LIT(X,W,IB,C,2,'I ',NDON,BINDON,ISTAT)
       NVAR=IB(1)
       DO I=1,NVAR
         CALL LIT(X,W,IB,TEXTE(I),32,'CH',NDON,BINDON,ISTAT)
       ENDDO
!
!      FORMAT AND GEOMETRY
!
       CALL LIT(X,W,IB,C,10,'I ',NDON,BINDON,ISTAT)
       IF(IB(10).EQ.1) THEN
         CALL LIT(X,W,IB,C, 4,'I ',NDON,BINDON,ISTAT)
       ENDIF
       CALL LIT(X,W,IB,C, 4,'I ',NDON,BINDON,ISTAT)
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
!      ARRAY OF INTEGERS IKLE
!
       READ(NDON)
!
!      ARRAY OF INTEGERS IPOBO
!
       READ(NDON)
!
!      X AND Y
!
!      CALL LIT(XRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
!      CALL LIT(YRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
       READ(NDON)
       READ(NDON)
!
!      TIME STEP AND VARIABLES
!
       CALL LIT(ATB,W,IB,C,1,'R4',NDON,BINDON,ISTAT)
       IF(CHDON(1:1).EQ.'C') THEN
         TV1=ATB(1)
       ELSE
         ATT=ATB(1)*1.D2
         CALL TEMP(TV1,ATT,DDC)
       ENDIF
       IF(TV1.GT.AT) THEN
         WRITE(LU,*) '*************************************************'
         IF(LNG.EQ.1) THEN
           WRITE(LU,*) ' LE PREMIER ENREGISTREMENT DU FICHIER DE ',CHDON
           WRITE(LU,*) '   ',ATT,' EST POSTERIEUR AU TEMPS '
           WRITE(LU,*) '   DU DEBUT DU CALCUL',DDC
         ELSE
           WRITE(LU,*) ' THE FIRST RECORDING OF THE ',CHDON,' FILE '
           WRITE(LU,*) '   ',ATT,' IS OLDER THAN THE BEGINNING '
           WRITE(LU,*) '   OF THE COMPUTATION',DDC
         ENDIF
         WRITE(LU,*) '*************************************************'
         CALL PLANTE(1)
         STOP
       ENDIF
!
110    CONTINUE
       DO I=1,NVAR
         IF(I.EQ.ID(1)) THEN
           CALL LIT(U1,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
         ELSEIF(I.EQ.ID(2)) THEN
           CALL LIT(V1,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
         ELSE
           READ(NDON)
         ENDIF
       ENDDO
!
       CALL LIT(ATB,W,IB,C,1,'R4',NDON,BINDON,ISTAT)
       IF(CHDON(1:1).EQ.'C') THEN
         TV2=ATB(1)
       ELSE
         ATT=ATB(1)*1.D2
         CALL TEMP(TV2,ATT,DDC)
       ENDIF
       IF(TV2.LT.AT) THEN
         TV1=TV2
         GOTO 110
       ENDIF
!
       DO I =1,NVAR
         IF(I.EQ.ID(1)) THEN
           CALL LIT(U2,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
         ELSEIF(I.EQ.ID(2)) THEN
           CALL LIT(V2,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
         ELSE
           READ(NDON)
         ENDIF
       ENDDO
       WRITE(LU,*) 'T',CHDON,'1:',TV1
       WRITE(LU,*) 'T',CHDON,'2:',TV2                                                NPTFR,1.D-6)
!
      ELSEIF (INDIC.EQ.4) THEN
!       READS A USER-DEFINED FORMAT
        IF(CHDON(1:1).EQ.'C') THEN
!         READS A CURRENT FIELD
          CALL COUUTI(X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &                NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
        ELSEIF(CHDON(1:1).EQ.'V' .OR. CHDON(1:1).EQ.'W') THEN
!         READS A WIND FIELD
          CALL VENUTI(X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &                NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
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
        WRITE(LU,*) '************************************************'
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECDOI : INDICATEUR DE FORMAT INCONNU : ',INDIC
        WRITE(LU,*) '         POUR LE FICHIER DE ',CHDON
        ELSE
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
      DO I=1,NPOIN2
        UD(I)=(U2(I)-U1(I))*COEF+U1(I)
        VD(I)=(V2(I)-V1(I))*COEF+V1(I)
      ENDDO
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

