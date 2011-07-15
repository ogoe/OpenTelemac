!                    *****************
                     SUBROUTINE LECHAM
!                    *****************
!
     &( ZM , DZHDT, X    , Y     , NPOIN2, NDON , BINDON, NBOR  , NPTFR,
     &  AT , DDC  , TM1  , TM2   , NP   , XRELV , YRELV , ZR   ,
     &  Z1 , Z2   , INDIM, NPMAX , IDHMA, NVAR  )
!
!***********************************************************************
! TOMAWAC   V6P1                                   21/06/2011
!***********************************************************************
!
!brief    THIS SUBROUTINE PROJECTS THE TIDE DATA ON THE
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
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| DZHDT          |-->| WATER DEPTH DERIVATIVE WITH RESPECT TO T
!| IDHMA          |-->| RANK OF THE WATER LEVEL DATA IN THE TELEMAC FILE
!| INDIM          |-->| FILE FORMAT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDON           |-->| LOGICAL UNIT NUMBER OF THA DATA FILE
!| NP             |<->| NUMBER OF POINTS READ FROM THE FILE
!| NPMAX          |-->| MAXIMUM NUMBER OF POINTS THAT CAN BE READ
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NVAR           |<--| NUMBER OF VARIABLES READ FROM THE DATA FILE
!| TM1            |<--| TIME T1 IN THE DATA FILE
!| TM2            |<--| TIME T2 IN THE DATA FILE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XRELV          |<->| TABLE OF THE ABSCISSES OF DATA FILE POINTS
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YRELV          |<->| TABLE OF THE ORDINATES OF DATA FILE POINTS
!| Z1             |<->| DATA AT TIME TM1 AT THE MESH POINTS
!| Z2             |<->| DATA AT TIME TM2 AT THE MESH POINTS
!| ZM             |<--| DATA AT TIME AT, AT THE MESH POINTS
!| ZR             |<->| TABLE OF THE VALUES READ IN THE DATA FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      USE INTERFACE_TOMAWAC, EX_LECHAM => LECHAM
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NP,NDON,NPOIN2,NPTFR,INDIM,NCOL,NLIG,BID,I,J
      INTEGER NVAR,NI,ISTAT,IB(10),IDHMA
!
      INTEGER NPMAX,NBOR(NPTFR,2)
!
      DOUBLE PRECISION X(NPOIN2)    , Y(NPOIN2)
      DOUBLE PRECISION ZM(NPOIN2)   , DZHDT(NPOIN2)
      DOUBLE PRECISION XRELV(NPMAX), YRELV(NPMAX)
      DOUBLE PRECISION ZR(NPMAX)   , Z1(NPMAX)   , Z2(NPMAX)
      DOUBLE PRECISION XMAX,XMIN,YMAX,YMIN,DX,DY,AT,TM1,TM2
      DOUBLE PRECISION DDC,DAT1,DAT2,Z(1),ATT, ATB(1)
      DOUBLE PRECISION COE1, COE2
      REAL, ALLOCATABLE :: W(:)
!
      CHARACTER*3  BINDON,C
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTE(10)
!
      ALLOCATE(W(MAX(NPMAX,72)))
!
!-----------------------------------------------------------------------
!     READS THE POINTS FROM LOGICAL UNIT NDON
!-----------------------------------------------------------------------
!
      IF (INDIM.EQ.1) THEN
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
        WRITE(LU,*) 'LECHAM : LECTURE DU FICHIER DE LA MAREE '
        WRITE(LU,*) '         NOMBRE DE LIGNES   : ',NLIG
        WRITE(LU,*) '         NOMBRE DE COLONNES : ',NCOL
        WRITE(LU,*) '         ABSCISSE OU LONGITUDE MINIMALE : ',XMIN
        WRITE(LU,*) '         ABSCISSE OU LONGITUDE MAXIMALE : ',XMAX
        WRITE(LU,*) '         ORDONNEE OU LATITUDE MINIMALE  : ',YMIN
        WRITE(LU,*) '         ORDONNEE OU LATITUDE MAXIMALE  : ',YMAX
        IF(NP.GT.NPMAX) THEN
         WRITE(LU,*) '*************************************************'
         WRITE(LU,*) ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU '
         WRITE(LU,*) ' DE DONNEES :',NPMAX,' EST TROP FAIBLE POUR '
         WRITE(LU,*) ' CONTENIR LA TOTALITE DES DONNEES :',NP
         WRITE(LU,*) '*************************************************'
         CALL PLANTE(1)
        ENDIF
       ELSE
        WRITE(LU,*) '--------------------------------------------------'
        WRITE(LU,*)'LECHAM : READING OF THE TIDE DATA FILE '
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
         CALL PLANTE(0)
        ENDIF
       ENDIF
!      READS THE DATE OF THE FIRST RECORD
       READ(NDON,*) DAT1
       CALL TEMP(TM1,DAT1,DDC)
       IF (TM1.GT.AT) THEN
        WRITE(LU,*) '*************************************************'
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'LE PREMIER ENREGISTREMENT DU FICHIER DE LA MAREE'
         WRITE(LU,*) '   ',DAT1,' EST POSTERIEUR AU TEMPS '
         WRITE(LU,*) '   DU DEBUT DU CALCUL',DDC
        ELSE
         WRITE(LU,*) ' THE FIRST RECORDING OF THE TIDE DATA FILE '
         WRITE(LU,*) '   ',DAT1,' IS OLDER THAN THE DEGINNING '
         WRITE(LU,*) '   OF THE COMPUTATION',DDC
        ENDIF
        WRITE(LU,*) '*************************************************'
        CALL PLANTE(0)
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
       READ(NDON,20,END=100,ERR=100) (ZR(I),I=1,NP)
       CALL OV( 'X=C     ' , Z1 , Y , Z , 0.D0 , NPOIN2)
!
       READ(NDON,*) DAT2
       CALL TEMP(TM2,DAT2,DDC)
       IF (TM2.LE.AT) THEN
         TM1=TM2
         GOTO 90
       ENDIF
       CALL FASP(X,Y,Z1,NPOIN2,XRELV,YRELV,ZR,NP,NBOR,MESH%KP1BOR%I,
     &                                                   NPTFR,0.D0)
!
       READ(NDON,*,END=100,ERR=100)
       READ(NDON,20,END=100,ERR=100) (ZR(I),I=1,NP)
       CALL FASP(X,Y,Z2,NPOIN2,XRELV,YRELV,ZR,NP,NBOR,MESH%KP1BOR%I,
     &                                                   NPTFR,0.D0)
!
!
      ELSEIF (INDIM.EQ.2) THEN
!
!      -----------------------------------------------------------------
!      TELEMAC FORMAT
!      -----------------------------------------------------------------
!
       REWIND NDON
!
!      READS TITLE
!
       CALL LIT(X,W,IB,TITCAS,72,'CH',NDON,BINDON,ISTAT)
!
!      READS NUMBER OF VARIABLES AND THEIR NAMES
!
       CALL LIT(X,W,IB,C,2,'I ',NDON,BINDON,ISTAT)
       NVAR=IB(1)
       DO 80 I=1,NVAR
         CALL LIT(X,W,IB,TEXTE(I),32,'CH',NDON,BINDON,ISTAT)
80     CONTINUE
!
!      FORMAT AND GEOMETRY
!
       CALL LIT(X,W,IB,C,10,'I ',NDON,BINDON,ISTAT)
       IF (IB(10).EQ.1) THEN
          CALL LIT(X,W,IB,C, 4,'I ',NDON,BINDON,ISTAT)
       ENDIF
       CALL LIT(X,W,IB,C, 4,'I ',NDON,BINDON,ISTAT)
       NP=IB(2)
       NI=IB(1)*IB(3)
       WRITE(LU,*) '--------------------------------------------'
       IF (LNG.EQ.1) THEN
        WRITE(LU,*)'LECHAM : LECTURE DU FICHIER TELEMAC'
        WRITE(LU,*) '         TITRE DU CAS LU : ',TITCAS
        WRITE(LU,*)'         NOMBRE DE POINTS   : ',NP
       ELSE
        WRITE(LU,*)'LECHAM : READING OF TELEMAC DATA FILE '
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
        CALL PLANTE(0)
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
       TM1=ATB(1)
!FBG       ATT=ATB(1)*1.D2
!FBG       CALL TEMP(TM1,ATT,DDC)
       IF (TM1.GT.AT) THEN
        WRITE(LU,*) '*************************************************'
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) ' LE PREMIER ENREGISTREMENT DU FICHIER '
         WRITE(LU,*) '   CONTENANT LE NIVEAU DE LA MAREE    '
!         WRITE(LU,*) '   ',ATT,' EST POSTERIEUR AU TEMPS '
!         WRITE(LU,*) '   DU DEBUT DU CALCUL',DDC
         WRITE(LU,*) '   ',AT,' EST POSTERIEUR AU TEMPS '
         WRITE(LU,*) '   DU DEBUT DU CALCUL',TM1
        ELSE
         WRITE(LU,*) ' THE FIRST RECORDING OF THE TIDE LEVEL FILE '
         WRITE(LU,*) '   ',ATT,' DATES LATER THAN THE DEGINNING '
         WRITE(LU,*) '   OF THE COMPUTATION',DDC
        ENDIF
        WRITE(LU,*) '*************************************************'
        CALL PLANTE(0)
       ENDIF
!
110    CONTINUE
       DO I =1,NVAR
        IF(I.EQ.IDHMA) THEN
         CALL LIT(Z1,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
        ELSE
         READ(NDON)
        ENDIF
       ENDDO
!
       CALL LIT(ATB,W,IB,C,1,'R4',NDON,BINDON,ISTAT)
       TM2=ATB(1)
       IF (TM2.LE.AT) THEN
        TM1=TM2
        GOTO 110
       ENDIF
        CALL FASP(X,Y,ZR,NPOIN2,XRELV,YRELV,Z1,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
!
        CALL OV( 'X=Y     ' , Z1 , ZR , Z , 0.D0 , NPOIN2)
       DO I =1,NVAR
        IF(I.EQ.IDHMA) THEN
          CALL LIT(Z2,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
        ELSE
         READ(NDON)
        ENDIF
       ENDDO
!
       WRITE(LU,*) 'TMAREE1:',TM1
       WRITE(LU,*) 'TMAREE2:',TM2
!
!       INTERPOLATES IN SPACE
!
        CALL FASP(X,Y,ZR,NPOIN2,XRELV,YRELV,Z2,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
        CALL OV( 'X=Y     ' , Z2 , ZR , Z , 0.D0 , NPOIN2)
!
!
      ELSEIF (INDIM.EQ.3) THEN
!     READS A USER-DEFINED FORMAT
              CALL MARUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TM1,TM2,
     &     NP,XRELV,YRELV,ZR,Z1,Z2,NPMAX)
!
      ELSE
        WRITE(LU,*) '************************************************'
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECHAM : INDICATEUR DE FORMAT INCONNU : ',INDIM
        WRITE(LU,*) '         POUR LE FICHIER DU NIVEAU DE LA MAREE '
        ELSE
          WRITE(LU,*)'LECHAM : UNKNOWN INDICATOR OF FORMAT : ',INDIM
          WRITE(LU,*)'         FOR THE TIDE LEVEL DATA FILE '
        ENDIF
        WRITE(LU,*) '************************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!   INTERPOLATES IN TIME
!   AND COMPUTES THE TEMPORAL GRADIENT OF THE TIDE
!-----------------------------------------------------------------------
!
      COE1=(TM2-TM1)
      IF (COE1.LT.1.D-4) THEN
         WRITE(LU,*) '****************************************'
         IF(LNG.EQ.1) THEN
           WRITE(LU,*) ' DEUX TEMPS IDENTIQUES                '
           WRITE(LU,*) ' DANS LE FICHIER DES HAUTEURS DE MAREE'
         ELSE
           WRITE(LU,*) ' TWO IDENTICAL TIMES IN THE TIDAL FILE'
         ENDIF
         WRITE(LU,*) '****************************************'
         CALL PLANTE(0)
      ENDIF
      COE2=(AT-TM1)/COE1
      DO 120 I=1,NPOIN2
         ATT     = (Z2(I)-Z1(I))
         ZM(I)   = ATT*COE2+Z1(I)
         DZHDT(I)= ATT/COE1
120   CONTINUE
!
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(W)
!
!-----------------------------------------------------------------------
!
!     FORMATS
!
10    FORMAT (2I4,4F9.3,2I2)
20    FORMAT (10F6.2)
!
      RETURN
!
!     IF FAILED TO READ THE FILE ...
!
100   CONTINUE
      WRITE(LU,*)'**********************************************'
      IF (LNG.EQ.1) THEN
         WRITE(LU,*)'  ERREUR A LA LECTURE DU FICHIER DE LA MAREE '
         WRITE(LU,*)'      OU FIN DE FICHIER PREMATUREE           '
      ELSE
         WRITE(LU,*)'  ERROR WHILE READING TIDAL DATA '
         WRITE(LU,*)'    OR UNEXPECTED END OF FILE           '
      ENDIF
      WRITE(LU,*)'**********************************************'
      CALL PLANTE(0)
!
      RETURN
      END
