!                    **************
                     SUBROUTINE LIT
!                    **************
!
     &( X , W , I , C , NVAL , TYPE , CANAL , STD2 , ISTAT )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS VALUES ACCORDING TO VARIOUS STANDARDS.
!
!warning  IF THE CHARACTER STRING STD EQUALS IBM OR I3E, CALLS THE
!+            SUBROUTINES LECIBM OR LECI3E, WHICH DEPEND ON THE TYPE
!+            OF MACHINE USED
!
!history  J-M HERVOUET (LNHE)
!+        01/04/2009
!+        V6P0
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
!| C              |<--| CHARACTER STRING TO BE READ
!| CANAL          |-->| LOGICAL UNIT FOR READING
!| I              |-->| INTEGER ARRAY TO BE READ
!| ISTAT          |<--| ERROR NUMBER
!| NVAL           |-->| NUMBER OF VALUES (INTEGER, CHARACTER, ETC.)
!|                |   | TO BE READ
!| STD2           |-->| INPUT STANDARD : STD , IBM OU I3E, ETC.
!| TYPE           |-->| TYPE OF DATA : 'I' , 'CH' , 'R4' , 'R8'
!| W              |<--| REAL WORK ARRAY (IN CASE OF
!|                |   | CONVERSION FROM SIMPLE TO DOUBLE PRECISION)
!| X              |-->| DOUBLE PRECISION ARRAY TO BE READ
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NVAL,CANAL
      INTEGER, INTENT(INOUT)          :: ISTAT
      CHARACTER*(*), INTENT(IN)       :: TYPE,STD2
      INTEGER, INTENT(INOUT)          :: I(NVAL)
      DOUBLE PRECISION, INTENT(INOUT) :: X(NVAL)
      REAL, INTENT(INOUT)             :: W(NVAL)
      CHARACTER*(*), INTENT(INOUT)    :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER J
      CHARACTER(LEN=8) STD
!
      INTRINSIC DBLE,MIN,LEN
!
!-----------------------------------------------------------------------
!
      ISTAT = 0
!
!-----------------------------------------------------------------------
!
!     STD2 MAY BE SHORTER THAN 8 CHARACTERS
      STD='        '
      STD(1:MIN(8,LEN(STD2)))=STD2(1:MIN(8,LEN(STD2)))
!
!-----------------------------------------------------------------------
!
      IF(STD(1:3).EQ.'STD'.OR.STD(1:7).EQ.'SERAFIN') THEN
!
         IF(TYPE(1:2).EQ.'R4') THEN
            IF(STD(1:8).EQ.'SERAFIND') THEN
!             IF SERAFIN DOUBLE, R4 SHOULD BE R8
              READ(CANAL,END=100,ERR=101)(X(J),J=1,NVAL)
            ELSE
              READ(CANAL,END=100,ERR=101)(W(J),J=1,NVAL)
              DO J=1,NVAL
                X(J) = DBLE(W(J))
              ENDDO
            ENDIF
         ELSEIF(TYPE(1:2).EQ.'R8') THEN
            READ(CANAL,END=100,ERR=101)(X(J),J=1,NVAL)
         ELSEIF (TYPE(1:1).EQ.'I') THEN
            READ(CANAL,END=100,ERR=101)(I(J),J=1,NVAL)
         ELSEIF(TYPE(1:2).EQ.'CH') THEN
            READ(CANAL,END=100,ERR=101) C(1:NVAL)
         ELSE
            IF(LNG.EQ.1) WRITE(LU,20) TYPE
            IF(LNG.EQ.2) WRITE(LU,21) TYPE
20          FORMAT(1X,'LIT : TYPE INCONNU :',A2)
21          FORMAT(1X,'LIT : UNKNOWN TYPE :',A2)
            CALL PLANTE(1)
            STOP
         ENDIF
!
         GO TO 102
!
100      CONTINUE
         IF(LNG.EQ.1) THEN
          WRITE(LU,'(1X,A)')       'LIT : FIN DE FICHIER ANORMALE'
          WRITE(LU,'(1X,A)')       'ON VOULAIT LIRE UN'
          WRITE(LU,'(1X,A,1I6,A)') 'ENREGISTREMENT DE ',NVAL,' VALEURS'
          WRITE(LU,'(1X,A,A)')     'DE TYPE : ',TYPE
          WRITE(LU,'(1X,A,1I6)')   'SUR LE CANAL : ',CANAL
         ENDIF
         IF(LNG.EQ.2) THEN
          WRITE(LU,'(1X,A)')       'LIT : ABNORMAL END OF FILE'
          WRITE(LU,'(1X,A)')       'ONE INTENDED TO READ'
          WRITE(LU,'(1X,A,1I6,A)') 'A RECORD OF ',NVAL,' VALUES'
          WRITE(LU,'(1X,A,A)')     'OF TYPE : ',TYPE
          WRITE(LU,'(1X,A,1I6)')   'ON LOGICAL UNIT : ',CANAL
         ENDIF
!        ISTAT = -6
         CALL PLANTE(1)
         STOP
!
101      CONTINUE
         IF(LNG.EQ.1) THEN
          WRITE(LU,'(1X,A)')       'LIT : ERREUR DE LECTURE'
          WRITE(LU,'(1X,A)')       'ON VOULAIT LIRE UN'
          WRITE(LU,'(1X,A,1I6,A)') 'ENREGISTREMENT DE ',NVAL,' VALEURS'
          WRITE(LU,'(1X,A,A)')     'DE TYPE : ',TYPE
          WRITE(LU,'(1X,A,1I6)')   'SUR LE CANAL : ',CANAL
         ENDIF
         IF(LNG.EQ.2) THEN
          WRITE(LU,'(1X,A)')       'LIT : READ ERROR'
          WRITE(LU,'(1X,A)')       'ONE INTENDED TO READ'
          WRITE(LU,'(1X,A,1I6,A)') 'A RECORD OF ',NVAL,' VALUES'
          WRITE(LU,'(1X,A,A)')     'OF TYPE : ',TYPE
          WRITE(LU,'(1X,A,1I6)')   'ON LOGICAL UNIT : ',CANAL
         ENDIF
!        ISTAT = -6
         CALL PLANTE(1)
         STOP
!
102      CONTINUE
!
!-----------------------------------------------------------------------
!
!     ELSEIF(STD(1:3).EQ.'IBM') THEN
!
!        IF (TYPE(1:2).EQ.'R4') THEN
!           CALL LECIBM( W , NVAL , TYPE , CANAL )
!           DO 77 J=1,NVAL
!             X(J)=DBLE(W(J))
!77          CONTINUE
!        ELSEIF (TYPE(1:2).EQ.'R8') THEN
!           CALL LECIBM( X , NVAL , TYPE , CANAL )
!        ELSEIF (TYPE(1:1).EQ.'I') THEN
!           CALL LECIBM( I , NVAL , TYPE , CANAL )
!        ELSEIF (TYPE(1:2).EQ.'CH') THEN
!           CALL LECIBM( C , NVAL , TYPE , CANAL )
!        ELSE
!           IF(LNG.EQ.1) WRITE(LU,20) TYPE
!           IF(LNG.EQ.2) WRITE(LU,21) TYPE
!           CALL PLANTE(0)
!           STOP
!        ENDIF
!
!-----------------------------------------------------------------------
!
!     ELSEIF(STD(1:3).EQ.'I3E') THEN
!  READS R4 AND R8 - TO BE CHECKED
!        IF (TYPE(1:2).EQ.'R4') THEN
!           CALL LECI3E( W , NVAL , 'F' , CANAL , ISTAT )
!           DO 78 J=1,NVAL
!             X(J)=DBLE(W(J))
!78          CONTINUE
!        ELSEIF (TYPE(1:2).EQ.'R8') THEN
!           CALL LECI3E( X , NVAL , 'F' , CANAL , ISTAT )
!        ELSEIF (TYPE(1:1).EQ.'I') THEN
!           CALL LECI3E( I , NVAL , 'I' , CANAL , ISTAT )
!        ELSEIF (TYPE(1:2).EQ.'CH') THEN
!           CALL LECI3E( C , NVAL , 'C' , CANAL , ISTAT )
!        ELSE
!           IF(LNG.EQ.1) WRITE(LU,20) TYPE
!           IF(LNG.EQ.2) WRITE(LU,21) TYPE
!           CALL PLANTE(0)
!           STOP
!        ENDIF
!
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,10) STD
        IF(LNG.EQ.2) WRITE(LU,11) STD
10      FORMAT(1X,'LIT : STANDARD DE LECTURE INCONNU :',A8)
11      FORMAT(1X,'LIT : UNKNOWN STANDARD:',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
