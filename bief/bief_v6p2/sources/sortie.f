!                    *****************
                     SUBROUTINE SORTIE
!                    *****************
!
     &( CHAINE , MNEMO , NBRE , SORLEO )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SETS VARIABLES SORLEO AND SORIMP.
!
!history  J-M HERVOUET (LNHE)
!+        03/11/2009
!+        V6P0
!+   JOKER '*' ALLOWED IN NAMES
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
!| CHAINE         |<->| STRING OF VARIABLES FOR GRAPHIC OUTPUTS
!| MNEMO          |<->| MNEMO OF VARIABLES
!| NBRE           |-->| NUMBER OF VARIABLES
!| SORLEO         |<->| LOGICAL ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NBRE
!
      CHARACTER*(*), INTENT(INOUT) :: CHAINE
      CHARACTER(LEN=8), INTENT(IN) :: MNEMO(NBRE)
!
      LOGICAL, INTENT(INOUT) :: SORLEO(NBRE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! INTERNAL VARIABLES:
!
      CHARACTER C(2)
      CHARACTER(LEN=8) MOT(100)
      INTEGER I,J,LONG,I1,I2,NMOT,L
      LOGICAL OK
!
      INTRINSIC LEN
!
!-----------------------------------------------------------------------
!
!  RECOGNISED SEPARATORS IN 'CHAINE'
!
      C(1) = ','
      C(2) = ';'
      LONG = LEN(CHAINE)
      IF (LONG.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1002)
        IF(LNG.EQ.2) WRITE(LU,1003)
1002    FORMAT(1X,'SORTIE (BIEF) : CHAINE VIDE')
1003    FORMAT(1X,'SORTIE (BIEF): EMPTY STRING')
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I=1,LONG
        DO J=1,2
          IF(CHAINE(I:I).EQ.C(J)) CHAINE(I:I) = ' '
        ENDDO
      ENDDO
!
! 'CHAINE' NOW IS MADE UP OF WORDS SEPARATED BY WHITE SPACES
!
      I1 = 0
      NMOT=0
!
 10   CONTINUE
      IF (I1.GE.LONG) GOTO 30
      I1=I1+1
      IF (CHAINE(I1:I1).EQ.' ') GOTO 10
!
      I2=0
!
 20   CONTINUE
      I2=I2+1
      IF (CHAINE(I1+I2:I1+I2).NE.' ') GOTO 20
!
      NMOT=NMOT+1
      IF (I2.GT.8) THEN
        IF(LNG.EQ.1) WRITE(LU,1004) CHAINE
        IF(LNG.EQ.2) WRITE(LU,1005) CHAINE
1004    FORMAT(1X,'SORTIE (BIEF) : PLUS DE 8 CARACTERES PAR MOT',/,1X,
     &            '                 DANS LA CHAINE :',A)
1005    FORMAT(1X,'SORTIE (BIEF): MORE THAN 8 LETTERS PER WORD',/,1X,
     &            '                 IN THE CHAIN: ',A)
        CALL PLANTE(1)
        STOP
      ENDIF
      MOT(NMOT)=CHAINE(I1:I1+I2)
      I1=I1+I2
      GOTO 10
!
30    CONTINUE
!
!     COMPARES 'MOT' AND 'MNEMO'
!
      DO I=1,NBRE
        DO J=1,NMOT
          OK=.TRUE.
          DO L=1,8
!           A JOKER '*' IS ALLOWED
            IF(MOT(J)(L:L).NE.MNEMO(I)(L:L).AND.MOT(J)(L:L).NE.'*') THEN
              OK=.FALSE.
              EXIT
            ENDIF
          ENDDO
          SORLEO(I)=OK
          IF(SORLEO(I)) EXIT
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
