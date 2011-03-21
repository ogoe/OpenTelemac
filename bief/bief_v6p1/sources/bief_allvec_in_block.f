!                    *******************************
                     SUBROUTINE BIEF_ALLVEC_IN_BLOCK
!                    *******************************
!
     &( BLO , N , NAT , NOMGEN , IELM , NDIM , STATUT , MESH )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ALLOCATES MEMORY FOR N VECTORS, WHICH WILL BE PART
!+                OF A GIVEN BLOCK.
!
!note     THIS MODIFICATION OF ALLVEC_IN_BLOCK ALLOWS ADDING A NUMBER
!+         OF IDENTICALLY NUMBERED VECTORS TO AN ALREADY EXISTING BLOCK
!+         WITHOUT DESTROYING THE PREVIOUS STRUCTURE.
!
!history  J-M HERVOUET (LNH)
!+        11/07/1995
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
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
!| BLO            |<->| BLOCK WHERE THE VECTORS WILL BE ALLOCATED
!| IELM           |-->| TYPE OF ELEMENT OF VECTORS, OR DIMENSION
!|                |   | (DEPENDING ON 'STATUT', SEE BELOW)
!| N              |-->| NUMBER OF VECTORS TO BE ALLOCATED
!| NAT            |<--| 1: REAL VECTOR   2:VECTOR OF INTEGERS
!| NDIM           |-->| SECOND DIMENSION OF VECTORS
!| NOMGEN         |-->| GENERIC NAME OF VECTORS
!|                |   | WILL BE COMPLETED WITH RANK
!| STATUT         |-->| VECTOR STATUS:
!|                |   | 0 : FREE VECTOR, IELM IS ITS DIMENSION
!|                |   | 1 : VECTOR DEFINED ON A MESH
!|                |   | IELM IS THEN THE ELEMENT TYPE
!|                |   | CHANGING DISCRETISATION FORBIDDEN
!|                |   | 2 : LIKE 1 BUT CHANGING DISCRETISATION ALLOWED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_ALLVEC_IN_BLOCK => BIEF_ALLVEC_IN_BLOCK
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: BLO
      INTEGER         , INTENT(IN)    :: IELM,NDIM,STATUT,NAT,N
      CHARACTER(LEN=6), INTENT(IN)    :: NOMGEN
      TYPE(BIEF_MESH) , INTENT(IN)    :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IDEB,I,II
!
      CHARACTER(LEN=6) :: NOM
      CHARACTER*1 CHIFFRE(0:9)
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
      IF(BLO%N+N.LE.BLO%MAXBLOCK) THEN
!
      IF(N.GT.0) THEN
!
      DO 10 I = BLO%N+1 , BLO%N+N
!
!  NAME OF THE VECTOR
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
          STOP 'MORE THAN 999 VECTORS ASKED IN ALLVEC_IN_BLOCK'
        ENDIF
!
!  ALLOCATES THE VECTOR
!
        ALLOCATE(BLO%ADR(I)%P)
        CALL BIEF_ALLVEC(NAT,BLO%ADR(I)%P,NOM,IELM,NDIM,STATUT,MESH)
!
10    CONTINUE
!
      BLO%N=BLO%N+N
!
      ENDIF
!
      ELSE
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BIEF_ALLVEC_IN_BLOCK :'
        WRITE(LU,*) 'PLUS DE ',BLO%MAXBLOCK,' (',N,')'
        WRITE(LU,*) 'VECTEURS DEMANDES, CHANGER MAXBLOCK DANS ALLBLO.'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BIEF_ALLVEC_IN_BLOCK:'
        WRITE(LU,*) 'MORE THAN ',BLO%MAXBLOCK,'(',N,')'
        WRITE(LU,*) 'VECTORS TO BE ALLOCATED'
        WRITE(LU,*) 'CHANGE MAXBLOCK IN ALLBLO.'
      ENDIF
      STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
