!                    *****************
                     SUBROUTINE INIVEN
!                    *****************
!
     &(UV,VV,X,Y,NPOIN,NVEN, BINVEN,NBOR,NPTFR,AT,DDC,TV1,TV2,
     & NP,XRELV,YRELV,U1,V1,U2,V2,INDIC,NPMAX,ITR01)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    THIS SUBROUTINE PROJECTS THE WINDS ON THE COMPUTATION
!+                MESH AND INTERPOLATES TO FIRST TIME STEP.
!+
!+           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D AMONGST OTHERS)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TEMPS
!| BINVEN         |-->| BINAIRE DU FICHIER DES VENTS (SI INDIC>=2)
!| DDC            |-->| DATE DU DEBUT DU CALCUL
!| INDIC          |-->| TYPE DE FORMAT DE LECTURE
!| ITR01          |-->| TABLEAU DE TRAVAIL ENTIER
!| NBOR           |-->| NUMEROTATION DES POINTS FRONTIERE
!| NP             |<->| NOMBRE DE POINTS RELEVES
!| NPMAX          |-->| NOMBRE DE POINTS RELEVES MAXIMUM
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
!| NPTFR          |-->| NOMBRE DE  POINTS FRONTIERE
!| NVEN           |---|
!| TV1            |<->| TEMPS DU CHAMPS DE VENT 1
!| TV2            |<->| TEMPS DU CHAMPS DE VENT 2
!| U1,V1,U2,V2    |<->| VENT AUX NOEUDS DU MAILLAGE DU VENT
!| UV,VV          |<--| VENT AUX NOEUDS DU MAILLAGE
!| X,Y            |-->| COORDONNEES DU MAILLAGE
!| XRELV          |<->| TABLEAU DES ABSCISSES DES POINTS RELEVES
!| YRELV          |<->| TABLEAU DES ORDONNEES DES POINTS RELEVES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NP,NVEN,NPOIN,NPTFR,INDIC,NCOL,NLIG,BID,I,J
      INTEGER NVAR,NI,ISTAT,IB(10),ITR01(*)
!
      INTEGER NPMAX,NBOR(NPTFR,2)
!
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION UV(NPOIN),VV(NPOIN)
      DOUBLE PRECISION XRELV(NPMAX),YRELV(NPMAX)
      DOUBLE PRECISION U1(NPMAX),V1(NPMAX),U2(NPMAX),V2(NPMAX)
      DOUBLE PRECISION XMAX,XMIN,YMAX,YMIN,DX,DY,AT,TV1,TV2
      DOUBLE PRECISION DDC,DAT1,DAT2,COEF,Z(1),ATT, ATB(1)
!
      CHARACTER*3 BINVEN,C
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTE(10)
!
      DOUBLE PRECISION, ALLOCATABLE :: UR(:),VR(:)
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(MAX(NPMAX,72)))
!
!-----------------------------------------------------------------------
!        READS THE POINTS FROM LOGICAL UNIT NVEN
!-----------------------------------------------------------------------
!
      IF(INDIC.EQ.1) THEN
!
      REWIND NVEN
!
!     ------------------------------------------------------------------
!     WAM FORMAT, FINITE DIFFERENCES + INTERPOLATION TO THE MESH POINTS
!
!     ------------------------------------------------------------------
!
       READ(NVEN,10,END=100,ERR=100)
     & NCOL,NLIG,YMIN,YMAX,XMIN,XMAX,BID,BID
       DX=(XMAX-XMIN)/REAL(NCOL-1)
       DY=(YMAX-YMIN)/REAL(NLIG-1)
       NP=NCOL*NLIG
       ALLOCATE(UR(1:NPMAX),VR(1:NPMAX))
       WRITE(LU,*) '---------------------------------------------------'
       WRITE(LU,*) 'INIVEN : LECTURE DU FICHIER DE VENT'
       WRITE(LU,*) '         NOMBRE DE LIGNES   : ',NLIG
       WRITE(LU,*) '         NOMBRE DE COLONNES :',NCOL
       WRITE(LU,*) '         ABSCISSE OU LONGITUDE MINIMALE : ',XMIN
       WRITE(LU,*) '         ABSCISSE OU LONGITUDE MAXIMALE : ',XMAX
       WRITE(LU,*) '         ORDONNEE OU LATITUDE MINIMALE  : ',YMIN
       WRITE(LU,*) '         ORDONNEE OU LATITUDE MAXIMALE  : ',YMAX
       IF (NP.GT.NPMAX) THEN
        WRITE(LU,*) '**************************************************'
        WRITE(LU,*) ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU   '
        WRITE(LU,*) ' DE VENT :',NPMAX,' EST TROP FAIBLE POUR CONTENIR'
        WRITE(LU,*) ' CONTENIR LA TOTALITE DES DONNEES :',NCOL*NLIG
        WRITE(LU,*) '**************************************************'
        CALL PLANTE(0)
       ENDIF
       READ(NVEN,*) DAT1
       CALL TEMP(TV1,DAT1,DDC)
       IF (TV1.GT.AT) THEN
        WRITE(LU,*) '******************************************'
        WRITE(LU,*) ' LE PREMIER ENREGISTREMENT DU FICHIER DES'
        WRITE(LU,*) ' VENTS : ',DAT1,' EST POSTERIEUR AU TEMPS'
        WRITE(LU,*) ' DU DEBUT DU CALCUL',DDC
        WRITE(LU,*) '******************************************'
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
90    CONTINUE
      READ(NVEN,*,END=100,ERR=100)
      READ(NVEN,20,END=100,ERR=100)
     &       (UR(I),I=1,NP)
          READ(NVEN,*)
         READ(NVEN,20,END=100,ERR=100)
     &       (VR(I),I=1,NP)
          CALL OV( 'X=C     ' , U1 , Y , Z , 0.D0 , NPMAX)
          CALL OV( 'X=C     ' , V1 , Y , Z , 0.D0 , NPMAX)
          CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
          CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
!
           READ(NVEN,*) DAT2
           CALL TEMP(TV2,DAT2,DDC)
           IF (TV2.LT.AT) THEN
              TV1=TV2
              GOTO 90
           ENDIF
!
          READ(NVEN,*,END=100,ERR=100)
          READ(NVEN,20,END=100,ERR=100)
     &      (UR(I),I=1,NP)
        READ(NVEN,*,END=100,ERR=100)
        READ(NVEN,20,END=100,ERR=100)
     &      (VR(I),I=1,NP)
          CALL FASP(X,Y,U2,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
          CALL FASP(X,Y,V2,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
!
!
      ELSEIF (INDIC.EQ.2) THEN
!
       REWIND NVEN
!
!     ------------------------------------------------------------------
!     TELEMAC FORMAT, DISCRETISATION ON THE SAME MESH
!         VARIABLES 1 AND 2 ARE THE X AND Y COMPONENTS OF THE WIND
!     ------------------------------------------------------------------
!
!      READS TITLE
!
       CALL LIT(X,W,IB,TITCAS,72,'CH',NVEN,BINVEN,ISTAT)
!
!      READS NUMBER OF VARIABLES AND THEIR NAMES
!
       CALL LIT(X,W,IB,C,2,'I ',NVEN,BINVEN,ISTAT)
       NVAR=IB(1)
       DO 80 I=1,NVAR
          CALL LIT(X,W,IB,TEXTE(I),32,'CH',NVEN,BINVEN,ISTAT)
80     CONTINUE
!
!      FORMAT AND GEOMETRY
!
       CALL LIT(X,W,IB,C,10,'I ',NVEN,BINVEN,ISTAT)
       CALL LIT(X,W,IB,C, 4,'I ',NVEN,BINVEN,ISTAT)
       NP=IB(2)
       NI=IB(1)*IB(3)
       WRITE(LU,*) '--------------------------------------------'
       WRITE(LU,*) 'INIVEN : LECTURE DU FICHIER TELEMAC'
       WRITE(LU,*) '         TITRE DU CAS LU : ',TITCAS
       WRITE(LU,*) '         NOMBRE DE POINTS   : ',NP
       WRITE(LU,*) '--------------------------------------------'
       IF (NP.NE.NPOIN) THEN
       WRITE(LU,*) '***************************************************'
       WRITE(LU,*)'VOUS UTILISEZ UN FORMAT DE LECTURE TELEMAC SUPPOSANT'
       WRITE(LU,*)' L''EGALITE DES MAILLAGES LUS ET UTILISES. CECI EST '
       WRITE(LU,*)' IMPOSSIBLE CAR LE NOMBRE DE POINTS LUS :',NP,' EST'
       WRITE(LU,*)' DIFFERENT DU NOMBRE DE POINTS DU MAILLAGE :',NPOIN
       WRITE(LU,*) '***************************************************'
       CALL PLANTE(1)
       ENDIF
       ALLOCATE(UR(1:NPMAX),VR(1:NPMAX))
       CALL LIT(X,W,ITR01,C,NI,'I ',NVEN,BINVEN,ISTAT)
       CALL LIT(X,W,ITR01,C,NP,'I ',NVEN,BINVEN,ISTAT)
!
!      X AND Y
!
       CALL LIT(XRELV,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
       CALL LIT(YRELV,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
!
!      TIME STEP AND VARIABLES
!
       CALL LIT(ATB,W,IB,C,1,'R4',NVEN,BINVEN,ISTAT)
       ATT=ATB(1)*1.D2
       CALL TEMP(TV1,ATT,DDC)
       IF (TV1.GT.AT) THEN
          WRITE(LU,*) 'ERREUR DEMARAGE LECTURE',TV1,AT
        CALL PLANTE(0)
          ENDIF
110       CONTINUE
        CALL LIT(U1,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
        CALL LIT(V1,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
!
        CALL LIT(ATB,W,IB,C,1,'R4',NVEN,BINVEN,ISTAT)
        ATT=ATB(1)*1.D2
        CALL TEMP(TV2,ATT,DDC)
        IF (TV2.LT.AT) THEN
           TV1=TV2
           GOTO 110
          ENDIF
          CALL LIT(U2,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
          CALL LIT(V2,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
        WRITE(LU,*) 'TVENT1:',TV1
        WRITE(LU,*) 'TVENT2:',TV2
!
!
      ELSEIF (INDIC.EQ.3) THEN
!
        ALLOCATE(UR(1:NPMAX),VR(1:NPMAX))
        CALL VENUTI
     & (X,Y,NPOIN,NVEN,BINVEN,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &  NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
!
      ELSE
!
        WRITE(LU,*) '***********************************************'
        WRITE(LU,*) 'INIVEN : INDICATEUR DE FORMAT INCONNU   '
        WRITE(LU,*) '         POUR LE FICHIER DES VENTS :',INDIC
        WRITE(LU,*) '***********************************************'
        CALL PLANTE(1)
      ENDIF
!
!-----------------------------------------------------------------------
!   INTERPOLATES
!-----------------------------------------------------------------------
!
        COEF=(AT-TV1)/(TV2-TV1)
        DO 120 I=1,NPOIN
           UV(I)=(U2(I)-U1(I))*COEF+U1(I)
           VV(I)=(V2(I)-V1(I))*COEF+V1(I)
120     CONTINUE
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(UR,VR)
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
      WRITE(LU,*) '*********************************************'
      WRITE(LU,*) '  ERREUR A LA LECTURE DU FICHIER DE VENT     '
      WRITE(LU,*) '      OU FIN DE FICHIER PREMATUREE           '
      WRITE(LU,*) '*********************************************'
      CALL PLANTE(1)
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(W)
!
!-----------------------------------------------------------------------
!
      RETURN
      END