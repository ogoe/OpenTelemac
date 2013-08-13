!                    ****************
                     SUBROUTINE ECRI2
!                    ****************
!
     &(X , I , C , NVAL , TYPE , CANAL , STD , ISTAT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES OUT VALUES ACCORDING TO VARIOUS STANDARDS.
!
!note     FORMER SUBROUTINE ECRIT;
!+         NAME CHANGED BECAUSE THIS NAME EXISTS IN THE CALCIUM LIBRARY
!
!warning  SUBROUTINES ECRIBM AND ECRI3E ARE DEPENDENT ON THE
!+            MACHINE USED.
!+            FOR EXAMPLE ECRIBM APPEARS IN THE GENERAL IMA LIBRARY
!+           (EDF). ON WORKSTATION, ECRIBM IS AN EMPTY SUBROUTINE.
!
!history  J-M HERVOUET (LNH)
!+        17/08/94
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
!| C              |-->| CHARACTER STRING TO BE WRITTEN
!| CANAL          |-->| LOGICAL UNIT FOR WRITING
!| I              |-->| INTEGER ARRAY TO BE WRITTEN
!| ISTAT          |<--| ERROR NUMBER
!| NVAL           |-->| NUMBER OF VALUES (INTEGER, CHARACTER, ETC.)
!|                |   | TO BE WRITTEN
!| STD            |-->| OUTPUT STANDARD : STD , IBM OU I3E, ETC.
!| TYPE           |-->| TYPE OF DATA : 'I' , 'CH' , 'R4' , 'R8'
!| X              |-->| DOUBLE PRECISION ARRAY TO BE WRITTEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NVAL,CANAL
      DOUBLE PRECISION, INTENT(IN) :: X(NVAL)
      INTEGER, INTENT(IN) :: I(NVAL)
      CHARACTER*(*), INTENT(IN) :: TYPE,STD,C
      INTEGER, INTENT(OUT) :: ISTAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER J
!
      INTRINSIC REAL
!
!-----------------------------------------------------------------------
!
      ISTAT = 0
!
!-----------------------------------------------------------------------
!
      IF(STD(1:3).EQ.'STD') THEN
!
         IF (TYPE(1:2).EQ.'R4') THEN
            WRITE(CANAL)(REAL(X(J)),J=1,NVAL)
         ELSEIF (TYPE(1:2).EQ.'R8') THEN
            WRITE(CANAL)(X(J),J=1,NVAL)
         ELSEIF (TYPE(1:1).EQ.'I') THEN
            WRITE(CANAL)(I(J),J=1,NVAL)
         ELSEIF (TYPE(1:2).EQ.'CH') THEN
            WRITE(CANAL) C(1:NVAL)
         ELSE
            IF(LNG.EQ.1) THEN
              WRITE(LU,20) TYPE
20            FORMAT(1X,'ECRI2 : TYPE INCONNU :',A2)
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,21) TYPE
21            FORMAT(1X,'ECRI2 : UNKNOWN TYPE:',A2)
            ENDIF
            CALL PLANTE(0)
            STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
!     ELSEIF(STD(1:3).EQ.'IBM') THEN
!
! BEWARE : CRAY DOUBLE PRECISION IS NOT ENVISAGED HERE
!
!        IF (TYPE(1:2).EQ.'R4') THEN
!           CALL ECRIBM( X , NVAL , TYPE , CANAL )
!        ELSEIF (TYPE(1:2).EQ.'R8') THEN
!           CALL ECRIBM( X , NVAL , TYPE , CANAL )
!        ELSEIF (TYPE(1:1).EQ.'I') THEN
!           CALL ECRIBM( I , NVAL , TYPE , CANAL )
!        ELSEIF (TYPE(1:2).EQ.'CH') THEN
!           THIS COPY APPEAR TO AVOID A BUG IN ECRIBM
!           CHAINE(1:NVAL) = C(1:NVAL)
!           CALL ECRIBM( CHAINE , NVAL , TYPE , CANAL )
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
!
! BEWARE : CRAY DOUBLE PRECISION IS NOT ENVISAGED HERE
!
!        IF (TYPE(1:2).EQ.'R4') THEN
!           CALL ECRI3E( X , NVAL , 'F' , CANAL , ISTAT )
!        ELSEIF (TYPE(1:2).EQ.'R8') THEN
!           CALL ECRI3E( X , NVAL , 'F' , CANAL , ISTAT )
!        ELSEIF (TYPE(1:1).EQ.'I') THEN
!           CALL ECRI3E( I , NVAL , 'I' , CANAL , ISTAT )
!        ELSEIF (TYPE(1:2).EQ.'CH') THEN
!           CALL ECRI3E( C(1:NVAL) , NVAL , 'C' , CANAL , ISTAT )
!        ELSE
!           IF(LNG.EQ.1) WRITE(LU,20) TYPE
!           IF(LNG.EQ.2) WRITE(LU,21) TYPE
!           CALL PLANTE(0)
!           STOP
!        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,10) STD
10        FORMAT(1X,'ECRI2 : STANDARD D''ECRITURE INCONNU :',A8)
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,11) STD
11        FORMAT(1X,'ECRI2 : UNKNOWN STANDARD:',A8)
        ENDIF
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
