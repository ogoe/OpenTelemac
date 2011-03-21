!                    **************************
                     SUBROUTINE ALLBLO_IN_BLOCK
!                    **************************
!
     &( BLO , N , NOMGEN )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ALLOCATES MEMORY FOR N BLOCKS, WHICH WILL BE PART
!+                OF A GIVEN BLOCK.
!
!history  J-M HERVOUET (LNH)
!+        11/07/95
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
!| BLO            |<->| BLOCK WHERE TO ALLOCATE THE BLOCK STRUCTURES
!| N              |-->| NUMBER OF BLOCKS TO BE ADDED IN BLO
!| NOMGEN         |-->| GENERIC FORTRAN NAME OF THE BLOCKS
!|                |   | IT WILL BE COMPLETED WITH THEIR RANK
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ALLBLO_IN_BLOCK => ALLBLO_IN_BLOCK
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: BLO
      INTEGER         , INTENT(IN)    :: N
      CHARACTER(LEN=6), INTENT(IN)    :: NOMGEN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IDEB,I,II
!
      CHARACTER(LEN=6) :: NOM
      CHARACTER(LEN=1) :: CHIFFRE(0:9)
      DATA CHIFFRE/'0','1','2','3','4','5','6','7','8','9'/
      SAVE CHIFFRE
!
!-----------------------------------------------------------------------
!
      IDEB = 6
      DO 5 I=5,2,-1
        IF(NOMGEN(I:I).EQ.' ') IDEB = I
5     CONTINUE
!
!-----------------------------------------------------------------------
!
      IF(N.LE.BLO%MAXBLOCK) THEN
!
      DO I = 1 , N
!
!  NAME OF THE BLOCK
!
        NOM=NOMGEN
        IF(I.LT.10) THEN
          IDEB = MIN(6,IDEB)
          NOM(IDEB:IDEB) = CHIFFRE(I)
        ELSEIF(I.LT.100) THEN
          IDEB = MIN(5,IDEB)
          NOM(IDEB  :IDEB  ) = CHIFFRE(I/10)
          NOM(IDEB+1:IDEB+1) = CHIFFRE(I-10*(I/10))
        ELSEIF(I.LT.1000) THEN
          IDEB = MIN(4,IDEB)
          NOM(IDEB  :IDEB  ) = CHIFFRE(I/100)
          II=I-100*(I/100)
          NOM(IDEB+1:IDEB+1) = CHIFFRE(II/10)
          NOM(IDEB+2:IDEB+2) = CHIFFRE(II-10*(II/10))
        ELSE
          STOP 'TOO MANY BLOCKS IN ALLBLO_IN_BLOCK'
        ENDIF
!
!  ALLOCATES THE BLOCK
!
        ALLOCATE(BLO%ADR(I)%P)
        CALL ALLBLO(BLO%ADR(I)%P,NOM)
        BLO%N=BLO%N+1
!
      ENDDO
!
      ELSE
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'ALLBLO_IN_BLOCK : PLUS DE ',BLO%MAXBLOCK,' (',N,')'
        WRITE(LU,*) '                  BLOCS DEMANDES'
        WRITE(LU,*) '                  CHANGER MAXBLOCK DANS ALLBLO'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'ALLBLO_IN_BLOCK : MORE THAN '
        WRITE(LU,*) '                 ',BLO%MAXBLOCK,' (',N,')'
        WRITE(LU,*) '                  BLOCKS TO BE ALLOCATED'
        WRITE(LU,*) '                  CHANGE MAXBLOCK IN ALLBLO'
      ENDIF
      STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
