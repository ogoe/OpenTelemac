!                    *****************
                     SUBROUTINE LECDON
!                    *****************
!
     &( U , V , X, Y, NPOIN2, NDON, BINDON, NBOR, NPTFR, XRELV, YRELV,
     &  UR, VR, TRA01,TRA02,TRA03,IDTEL,NPTT,DONTEL,COURAN,INDIC,NPMAX,
     &  CHDON)
!
!***********************************************************************
! TOMAWAC   V6P1                                   21/06/2011
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BINDON         |-->| BINAIRE DU FICHIER DES DONNEES  (INDIC>2)
!| CHDON          |-->| NAME OF THE VARIABLE READ FROM THE DATA FILE
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| DONTEL         |-->| LOGICAL INDICATING RECOVERY OF TELEMAC DATA ITEM
!| IDTEL          |-->| RANK OF THE TELEMAC DATA ITEM TO BE RECOVERED
!| INDIC          |-->| FILE FORMAT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDON           |-->| LOGICAL UNIT NUMBER OF THA DATA FILE
!| NPMAX          |-->| MAXIMUM NUMBER OF POINTS THAT CAN BE READ
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTT           |-->| TIME STEP NUMBER IN TELEMAC FILE
!| TRA01          |<->| WORK TABLE
!| TRA02          |<->| WORK TABLE
!| TRA03          |<->| WORK TABLE
!| U              |<--| CURRENT OR WIND ALONG X AT THE MESH POINTS
!| UR,VR          |<->| TABLE OF THE VALUES READ IN THE DATA FILE
!| V              |<--| CURRENT OR WIND ALONG Y AT THE MESH POINTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| XRELV          |<->| TABLE OF THE ABSCISSES OF DATA FILE POINTS
!| YRELV          |<->| TABLE OF THE ABSCISSES OF DATA FILE POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NP,NDON,NPOIN2,NPTFR,INDIC,NCOL,NLIG,BID,I,J
!
      INTEGER NPMAX,NBOR(NPTFR,2)
!
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2),U(NPOIN2),V(NPOIN2)
      DOUBLE PRECISION TRA01(NPMAX),TRA02(NPMAX),TRA03(NPOIN2)
      DOUBLE PRECISION XRELV(NPMAX),YRELV(NPMAX),UR(NPMAX),VR(NPMAX)
      DOUBLE PRECISION XMAX,XMIN,YMAX,YMIN,DX,DY,ATT,BDX(2)
!
      INTEGER NVAR,IB(10),ISTAT,NPTT,ID(3),IDTEL
!
      CHARACTER*3  BINDON,C
      CHARACTER*7  CHDON
      CHARACTER*32 TEXTE(20)
      CHARACTER*72 TITCAS
!
      LOGICAL DONTEL,COURAN
!
!-----------------------------------------------------------------------
!
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPMAX))
!
!-----------------------------------------------------------------------
!        READS THE POINTS FROM LOGICAL UNIT NDON
!-----------------------------------------------------------------------
!
      IF (INDIC.EQ.1) THEN
!
!     ------------------------------------------------------------------
!     WAM-LIKE FORMAT - FINITE DIFFERENCES
!     ------------------------------------------------------------------
!
         READ(NDON,10,END=100,ERR=100)
     &      NCOL,NLIG,YMIN,YMAX,XMIN,XMAX,BID,BID
         DX=(XMAX-XMIN)/REAL(NCOL-1)
         DY=(YMAX-YMIN)/REAL(NLIG-1)
         NP=NCOL*NLIG
         WRITE(LU,*)
     &    '-----------------------------------------------------'
         IF (LNG.EQ.1) THEN
            WRITE(LU,*)'LECDON : LECTURE DU FICHIER DE DONNEES'
            WRITE(LU,*)'         NOMBRE DE LIGNES   : ',NLIG
            WRITE(LU,*)'         NOMBRE DE COLONNES : ',NCOL
            WRITE(LU,*)'         ABSCISSE MINIMALE : ',XMIN
            WRITE(LU,*)'         ABSCISSE MAXIMALE : ',XMAX
            WRITE(LU,*)'         ORDONNEE MINIMALE : ',YMIN
            WRITE(LU,*)'         ORDONNEE MAXIMALE : ',YMAX
         ELSE
            WRITE(LU,*)'LECDON : READING OF THE DATA FILE '
            WRITE(LU,*)'         NUMBER OF LINES   : ',NLIG
            WRITE(LU,*)'         NUMBER OF COLUMNS : ',NCOL
            WRITE(LU,*)'         MINIMAL ABSCISSAE : ',XMIN
            WRITE(LU,*)'         MAXIMAL ABSCISSAE : ',XMAX
            WRITE(LU,*)'         MINIMAL ORDINATES : ',YMIN
            WRITE(LU,*)'         MAXIMAL ORDINATES : ',YMAX
         ENDIF
         WRITE(LU,*)
     &    '-----------------------------------------------------'
         IF (NP.GT.NPMAX) THEN
          WRITE(LU,*)
     &     '*****************************************************'
          IF (LNG.EQ.1) THEN
             WRITE(LU,*)
     &        ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU '
             WRITE(LU,*)
     &        ' DE DONNEES :',NPMAX,' EST TROP FAIBLE POUR '
             WRITE(LU,*)
     &        ' CONTENIR LA TOTALITE DES DONNEES :',NP
          ELSE
             WRITE(LU,*)
     &        ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
             WRITE(LU,*)
     &        ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
             WRITE(LU,*)
     &        ' ALL THE DATA :',NP
          ENDIF
          WRITE(LU,*)
     &        '*****************************************************'
          CALL PLANTE(0)
         ENDIF
         READ(NDON,*)
         READ(NDON,20,END=100,ERR=100)
     &      (UR(I),I=1,NCOL*NLIG)
         READ(NDON,*)
         READ(NDON,20,END=100,ERR=100)
     &      (VR(I),I=1,NCOL*NLIG)
         DO 30 I=1,NCOL
             DO 40 J=1,NLIG
                XRELV((I-1)*NLIG+J)=XMIN+DX*(I-1)
                YRELV((I-1)*NLIG+J)=YMIN+DY*(J-1)
40       CONTINUE
30       CONTINUE
!
!
      ELSEIF (INDIC.EQ.2) THEN
!
!     ------------------------------------------------------------------
!     SINUSX-LIKE FORMAT - SCATTER OF POINTS
!     ------------------------------------------------------------------
!
          DO 50 I=1,100000
            READ(NDON,*,END=60,ERR=100)
            NP=I
50        CONTINUE
60        CONTINUE
          WRITE(LU,*)
     &     '-----------------------------------------------------'
          IF (LNG.EQ.1) THEN
             WRITE(LU,*)'LECDON : LECTURE DU FICHIER DE DONNEES'
             WRITE(LU,*)'         NOMBRE DE POINTS   : ',NP
          ELSE
             WRITE(LU,*)'LECDON : READING OF THE DATA FILE '
             WRITE(LU,*)'         NUMBER OF POINTS   : ',NP
          ENDIF
          WRITE(LU,*)
     &     '-----------------------------------------------------'
          IF (NP.GT.NPMAX) THEN
           WRITE(LU,*)
     &     '*****************************************************'
           IF (LNG.EQ.1) THEN
             WRITE(LU,*)
     &        ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU '
             WRITE(LU,*)
     &        ' DE DONNEES :',NPMAX,' EST TROP FAIBLE POUR '
             WRITE(LU,*)
     &        ' CONTENIR LA TOTALITE DES DONNEES :',NP
           ELSE
             WRITE(LU,*)
     &        ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
             WRITE(LU,*)
     &        ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
             WRITE(LU,*)
     &        ' ALL THE DATA :',NP
           ENDIF
           WRITE(LU,*)
     &        '*****************************************************'
           CALL PLANTE(0)
          ENDIF
          REWIND NDON
          DO I=1,NP
            READ(NDON,*,ERR=100) XRELV(I),YRELV(I),UR(I),VR(I)
          ENDDO
!
      ELSEIF (INDIC.EQ.3) THEN
!
!     ------------------------------------------------------------------
!     TELEMAC-LIKE FORMAT - MESH CAN BE DIFFERENT
!          (BINARY)                 FROM COWADIS MESH
!     ------------------------------------------------------------------
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
          READ(NDON)
          CALL LIT(X,W,IB,C,4,'I ',NDON,BINDON,ISTAT)
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
          IF (NP.GT.NPMAX) THEN
           WRITE(LU,*)
     &        '*****************************************************'
           IF (LNG.EQ.1) THEN
             WRITE(LU,*)
     &        ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU '
             WRITE(LU,*)
     &        ' DE DONNEES :',NPMAX,' EST TROP FAIBLE POUR '
             WRITE(LU,*)
     &        ' CONTENIR LA TOTALITE DES DONNEES :',NP
           ELSE
             WRITE(LU,*)
     &        ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
             WRITE(LU,*)
     &        ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
             WRITE(LU,*)
     &        ' ALL THE DATA :',NP
           ENDIF
           WRITE(LU,*)
     &        '*****************************************************'
           CALL PLANTE(1)
           STOP
           ENDIF
          READ(NDON)
          READ(NDON)
!
!      X AND Y
!
          CALL LIT(XRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
          CALL LIT(YRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
!
!      TIME STEP AND VARIABLES
!
          DO 110 J=1,(NPTT-1)*(NVAR+1)
             READ(NDON)
110       CONTINUE
!
          IF (DONTEL) ID(3)=IDTEL
          IF (COURAN) THEN
             ID(1)=1
             ID(2)=2
          ENDIF
!
          CALL LIT(BDX(1),W,IB,C,1,'R4',NDON,BINDON,ISTAT)
          ATT=BDX(1)
          DO 90 I=1,NVAR
            IF (I.EQ.ID(1)) THEN
               CALL LIT(UR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
            ELSEIF (I.EQ.ID(2)) THEN
               CALL LIT(VR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
            ELSEIF ((I.EQ.ID(3)).AND.(DONTEL)) THEN
               CALL LIT(TRA01,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
            ELSE
               READ(NDON)
            ENDIF
90        CONTINUE
!
!      WRITES TO THE LISTING
!
          IF (LNG.EQ.1) THEN
            WRITE(LU,*)'         TITRE DU CAS TELEMAC : '
            WRITE(LU,*)'           ',TITCAS
            WRITE(LU,*)'         TEMPS DE TELEMAC : ',ATT
            WRITE(LU,*)'         VARIABLES DE TELEMAC RETENUE(S) : '
          ELSE
            WRITE(LU,*)'         TITLE OF TELEMAC CASE : '
            WRITE(LU,*)'           ',TITCAS
            WRITE(LU,*)'         TIME OF TELEMAC RECORD : ',ATT
            WRITE(LU,*)'         VARIABLE(S) OF TELEMAC READ : '
          ENDIF
          IF (COURAN) THEN
              WRITE(LU,*)'           ',TEXTE(ID(1))
              WRITE(LU,*)'           ',TEXTE(ID(2))
          ENDIF
          IF (DONTEL)
     &            WRITE(LU,*)'           ',TEXTE(ID(3))
          WRITE(LU,*)
     &        '-----------------------------------------------------'
!
!
      ELSEIF (INDIC.EQ.4) THEN
!
!     ------------------------------------------------------------------
!       READS A USER-DEFINED FORMAT
!     ------------------------------------------------------------------
        IF(CHDON(1:1).EQ.'C') THEN
!         READS A CURRENT FIELD
              CALL COUUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,0.,0.,0.,0.,
     &     NP,XRELV,YRELV,UR,VR,TRA03,TRA03,TRA03,TRA03,NPMAX)
        ELSEIF(CHDON(1:1).EQ.'V' .OR. CHDON(1:1).EQ.'W') THEN
!         READS A WIND FIELD
          CALL VENUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,0.,0.,0.,0.,
     &     NP,XRELV,YRELV,UR,VR,TRA03,TRA03,TRA03,TRA03,NPMAX)
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
        IF (LNG.EQ.1) THEN
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
!   THE CURRENTS ARE INTERPOLATED ONTO ALL THE INTERIOR POINTS
!                         TO THE DOMAIN
!-----------------------------------------------------------------------
!
      IF(COURAN) THEN
        CALL FASP(X,Y,U,NPOIN2,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
        CALL FASP(X,Y,V,NPOIN2,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
      ENDIF
      IF(DONTEL) THEN
        CALL FASP(X,Y,TRA03,NPOIN2,XRELV,YRELV,TRA01,NP,NBOR,
     &            MESH%KP1BOR%I,NPTFR,1.D-6)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FORMATS
!
10    FORMAT (2I4,4F9.3,2I2)
20    FORMAT (10F6.2)
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
      RETURN
      END
