!                    *****************
                     SUBROUTINE LECDON
!                    *****************
!
     &( U , V , X, Y, NPOIN2, NDON, BINDON, NBOR, NPTFR,TRA03,
     &  IDTEL,NPTT,DONTEL,COURAN,INDIC,CHDON)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BINDON         |-->| BINAIRE DU FICHIER DES DONNEES  (INDIC>2)
!| CHDON          |-->| NAME OF THE VARIABLE READ FROM THE DATA FILE
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| DONTEL         |-->| LOGICAL INDICATING RECOVERY OF TELEMAC DATA ITEM
!| IDTEL          |-->| RANK OF THE TELEMAC DATA ITEM TO BE RECOVERED
!| INDIC          |-->| FILE FORMAT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDON           |-->| LOGICAL UNIT NUMBER OF THA DATA FILE
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTT           |-->| TIME STEP NUMBER IN TELEMAC FILE
!| TRA03          |<->| WORK TABLE
!| U              |<--| CURRENT OR WIND ALONG X AT THE MESH POINTS
!| UR,VR          |<->| TABLE OF THE VALUES READ IN THE DATA FILE
!| V              |<--| CURRENT OR WIND ALONG Y AT THE MESH POINTS
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
      INTEGER, INTENT(IN)             :: IDTEL
      INTEGER, INTENT(IN)             :: NBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN2),V(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA03(NPOIN2)
      CHARACTER(LEN=3), INTENT(IN)    :: BINDON
      CHARACTER(LEN=7), INTENT(IN)    :: CHDON
      LOGICAL, INTENT(IN)             :: DONTEL,COURAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NP,I,J,NVAR,IB(10),ISTAT,ID(3)
      DOUBLE PRECISION ATT,BDX(2),Z(1)
      CHARACTER(LEN=3) C
      CHARACTER(LEN=32) TEXTE(20)
      CHARACTER(LEN=72) TITCAS
!
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPOIN2))
!
!-----------------------------------------------------------------------
!     READS THE POINTS FROM LOGICAL UNIT NDON
!-----------------------------------------------------------------------
!
      IF(INDIC.EQ.3) THEN
!
!     ------------------------------------------------------------------
!     TELEMAC-LIKE FORMAT - MESH CAN BE DIFFERENT
!          (BINARY)                 FROM COWADIS MESH
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
          READ(NDON)
          CALL LIT(Z,W,IB,C,4,'I ',NDON,BINDON,ISTAT)
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
          DO 110 J=1,(NPTT-1)*(NVAR+1)
            READ(NDON)
110       CONTINUE
!
          IF(DONTEL) ID(3)=IDTEL
          IF(COURAN) THEN
            ID(1)=1
            ID(2)=2
          ENDIF
!
          CALL LIT(BDX(1),W,IB,C,1,'R4',NDON,BINDON,ISTAT)
          ATT=BDX(1)
          DO 90 I=1,NVAR
            IF(I.EQ.ID(1)) THEN
              CALL LIT(U,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
            ELSEIF (I.EQ.ID(2)) THEN
              CALL LIT(V,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
            ELSEIF ((I.EQ.ID(3)).AND.(DONTEL)) THEN
              CALL LIT(TRA03,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
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
          IF(COURAN) THEN
            WRITE(LU,*)'           ',TEXTE(ID(1))
            WRITE(LU,*)'           ',TEXTE(ID(2))
          ENDIF
          IF(DONTEL)  WRITE(LU,*) '           ',TEXTE(ID(3))
          WRITE(LU,*)
     &        '-----------------------------------------------------'
!
      ELSEIF (INDIC.EQ.4) THEN
!
!     ------------------------------------------------------------------
!       READS A USER-DEFINED FORMAT
!     ------------------------------------------------------------------
!
        IF(CHDON(1:1).EQ.'C') THEN
!         READS A CURRENT FIELD
          CALL COUUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,0.D0,0.D0,0.D0,0.D0,
     &     TRA03,TRA03,TRA03,TRA03)
        ELSEIF(CHDON(1:1).EQ.'V'.OR.CHDON(1:1).EQ.'W') THEN
!         READS A WIND FIELD
!         NOTE JMH : THERE SHOULD BE U AND V SOMEWHERE HERE ????
          CALL VENUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,0.D0,0.D0,0.D0,0.D0,
     &     TRA03,TRA03,TRA03,TRA03)
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

