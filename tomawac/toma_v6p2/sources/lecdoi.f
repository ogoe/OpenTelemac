!                    *****************
                     SUBROUTINE LECDOI
!                    *****************
!
     &( UD , VD  , X  , Y  , NPOIN2, NDON , BINDON, NBOR , NPTFR,
     &  AT , DDC , TV1, TV2, NP   , XRELV, YRELV , UR   , VR   ,
     &  TRA, U1  , V1 , U2 , V2   , INDIC, NPMAX , CHDON, NVAR )
!
!***********************************************************************
! TOMAWAC   V6P1                                   20/06/2011
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
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
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
      DOUBLE PRECISION XMAX,XMIN,YMAX,YMIN,DX,DY,AT,TV1,TV2
      DOUBLE PRECISION DDC,DAT1,DAT2,COEF,Z(1),ATT, ATB(1)
!
      CHARACTER*3  BINDON,C
      CHARACTER*7  CHDON
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTE(10)
!
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPMAX))
!
!
!-----------------------------------------------------------------------
!     READS THE POINTS FROM LOGICAL UNIT NDON
!-----------------------------------------------------------------------
!
      IF (INDIC.EQ.1) THEN
!
!      -----------------------------------------------------------------
!      WAM FORMAT, FINITE DIFFERENCES + INTERPOLATION TO THE MESH POINTS
!
!      -----------------------------------------------------------------
!
       REWIND NDON
!
       READ(NDON,10,END=100,ERR=100)
     &      NCOL,NLIG,YMIN,YMAX,XMIN,XMAX,BID,BID
       DX=(XMAX-XMIN)/REAL(NCOL-1)
       DY=(YMAX-YMIN)/REAL(NLIG-1)
       NP=NCOL*NLIG
       IF(LNG.EQ.1) THEN
        WRITE(LU,*) '--------------------------------------------------'
        WRITE(LU,*) 'LECDOI : LECTURE DU FICHIER DE ',CHDON
        WRITE(LU,*) '         NOMBRE DE LIGNES   : ',NLIG
        WRITE(LU,*) '         NOMBRE DE COLONNES :',NCOL
        WRITE(LU,*) '         ABSCISSE OU LONGITUDE MINIMALE : ',XMIN
        WRITE(LU,*) '         ABSCISSE OU LONGITUDE MAXIMALE : ',XMAX
        WRITE(LU,*) '         ORDONNEE OU LATITUDE MINIMALE  : ',YMIN
        WRITE(LU,*) '         ORDONNEE OU LATITUDE MAXIMALE  : ',YMAX
        IF (NP.GT.NPMAX) THEN
         WRITE(LU,*) '*************************************************'
         WRITE(LU,*) ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU '
         WRITE(LU,*) ' DE DONNEES :',NPMAX,' EST TROP FAIBLE POUR '
         WRITE(LU,*) ' CONTENIR LA TOTALITE DES DONNEES :',NP
         WRITE(LU,*) '*************************************************'
         CALL PLANTE(1)
         STOP
        ENDIF
       ELSE
        WRITE(LU,*) '--------------------------------------------------'
        WRITE(LU,*)'LECDOI : READING OF THE ',CHDON,' DATA FILE '
        WRITE(LU,*)'         NUMBER OF LINES   : ',NLIG
        WRITE(LU,*)'         NUMBER OF COLUMNS : ',NCOL
        WRITE(LU,*)'         MINIMAL ABSCISSAE : ',XMIN
        WRITE(LU,*)'         MAXIMAL ABSCISSAE : ',XMAX
        WRITE(LU,*)'         MINIMAL ORDINATES : ',YMIN
        WRITE(LU,*)'         MAXIMAL ORDINATES : ',YMAX
        IF (NP.GT.NPMAX) THEN
         WRITE(LU,*) '*************************************************'
         WRITE(LU,*) ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
         WRITE(LU,*) ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
         WRITE(LU,*) ' ALL THE DATA :',NP
         WRITE(LU,*) '*************************************************'
         CALL PLANTE(1)
         STOP
        ENDIF
       ENDIF
!      READS THE DATE OF THE FIRST RECORD
       READ(NDON,*) DAT1
       CALL TEMP(TV1,DAT1,DDC)
       IF (TV1.GT.AT) THEN
        WRITE(LU,*) '*************************************************'
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) ' LE PREMIER ENREGISTREMENT DU FICHIER DE ',CHDON
         WRITE(LU,*) '   ',DAT1,' EST POSTERIEUR AU TEMPS '
         WRITE(LU,*) '   DU DEBUT DU CALCUL',DDC
        ELSE
         WRITE(LU,*) ' THE FIRST RECORDING OF THE ',CHDON,' FILE '
         WRITE(LU,*) '   ',DAT1,' IS OLDER THAN THE DEGINNING '
         WRITE(LU,*) '   OF THE COMPUTATION',DDC
        ENDIF
        WRITE(LU,*) '*************************************************'
        CALL PLANTE(1)
        STOP
       ENDIF
!
       DO 50 I=1,NCOL
          DO 40 J=1,NLIG
                XRELV((I-1)*NLIG+J)=XMIN+DX*(I-1)
                YRELV((I-1)*NLIG+J)=YMIN+DY*(J-1)
40        CONTINUE
50     CONTINUE
!
90     CONTINUE
       READ(NDON,*,END=100,ERR=100)
       READ(NDON,20,END=100,ERR=100) (UR(I),I=1,NP)
       READ(NDON,*)
       READ(NDON,20,END=100,ERR=100) (VR(I),I=1,NP)
       CALL OV( 'X=C     ' , U1 , Y , Z , 0.D0 , NPOIN2)
       CALL OV( 'X=C     ' , V1 , Y , Z , 0.D0 , NPOIN2)
!
       READ(NDON,*) DAT2
       CALL TEMP(TV2,DAT2,DDC)
       IF (TV2.LT.AT) THEN
         TV1=TV2
         GOTO 90
       ENDIF
       CALL FASP(X,Y,U1,NPOIN2,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
       CALL FASP(X,Y,V1,NPOIN2,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
!
       READ(NDON,*,END=100,ERR=100)
       READ(NDON,20,END=100,ERR=100) (UR(I),I=1,NP)
       READ(NDON,*,END=100,ERR=100)
       READ(NDON,20,END=100,ERR=100) (VR(I),I=1,NP)
       CALL FASP(X,Y,U2,NPOIN2,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
       CALL FASP(X,Y,V2,NPOIN2,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
!
!
      ELSEIF(INDIC.EQ.3) THEN
!
!      -----------------------------------------------------------------
!      TELEMAC FORMAT,
!          VARIABLES 1 AND 2 ARE THE X AND Y COMPONENTS OF THE WIND
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
       IF (IB(10).EQ.1) THEN
          CALL LIT(X,W,IB,C, 4,'I ',NDON,BINDON,ISTAT)
       ENDIF
       CALL LIT(X,W,IB,C, 4,'I ',NDON,BINDON,ISTAT)
       NP=IB(2)
       WRITE(LU,*) '--------------------------------------------'
       IF (LNG.EQ.1) THEN
        WRITE(LU,*)'LECDOI : LECTURE DU FICHIER TELEMAC'
        WRITE(LU,*) '         TITRE DU CAS LU : ',TITCAS
        WRITE(LU,*)'         NOMBRE DE POINTS   : ',NP
       ELSE
        WRITE(LU,*)'LECDOI : READING OF TELEMAC DATA FILE '
        WRITE(LU,*) '         FILE TITLE : ',TITCAS
        WRITE(LU,*)'         NUMBER OF POINTS   : ',NP
       ENDIF
       WRITE(LU,*) '--------------------------------------------'
       IF (NP.GT.NPMAX) THEN
        WRITE(LU,*) '**************************************************'
        IF(LNG.EQ.1) THEN
         WRITE(LU,*)
     &             ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU DE'
         WRITE(LU,*)
     &             ' DONNEES :',NPMAX,' EST TROP FAIBLE POUR CONTENIR'
         WRITE(LU,*) ' LA TOTALITE DES DONNEES :',NCOL*NLIG
        ELSE
         WRITE(LU,*) ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
         WRITE(LU,*) ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
         WRITE(LU,*) ' ALL THE DATA :',NP
        ENDIF
        WRITE(LU,*) '**************************************************'
        CALL PLANTE(1)
        STOP
       ENDIF
!      ARRAY OF INTEGERS IKLE
       READ(NDON)
!      ARRAY OF INTEGERS IPOBO
       READ(NDON)
!
!      X AND Y
!
       CALL LIT(XRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
       CALL LIT(YRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
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
       IF (TV1.GT.AT) THEN
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
       DO I =1,NVAR
        IF(I.EQ.ID(1)) THEN
         CALL LIT(UR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
        ELSEIF(I.EQ.ID(2)) THEN
         CALL LIT(VR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
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
       IF (TV2.LT.AT) THEN
        TV1=TV2
        GOTO 110
       ENDIF
!
!      INTERPOLATES IN SPACE THE VARIABLES GIVEN AT TIME 'TV1'
       CALL FASP(X,Y,U1,NPOIN2,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
       CALL FASP(X,Y,V1,NPOIN2,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
!
       DO I =1,NVAR
        IF(I.EQ.ID(1)) THEN
         CALL LIT(UR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
        ELSEIF(I.EQ.ID(2)) THEN
         CALL LIT(VR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
        ELSE
         READ(NDON)
        ENDIF
       ENDDO
       WRITE(LU,*) 'T',CHDON,'1:',TV1
       WRITE(LU,*) 'T',CHDON,'2:',TV2
!
!      INTERPOLATES IN SPACE THE VARIABLES GIVEN AT TIME 'TV2'
       CALL FASP(X,Y,U2,NPOIN2,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
       CALL FASP(X,Y,V2,NPOIN2,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
!
      ELSEIF (INDIC.EQ.4) THEN
!       READS A USER-DEFINED FORMAT
        IF(CHDON(1:1).EQ.'C') THEN
!         READS A CURRENT FIELD
              CALL COUUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
        ELSEIF(CHDON(1:1).EQ.'V' .OR. CHDON(1:1).EQ.'W') THEN
!         READS A WIND FIELD
          CALL VENUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
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
