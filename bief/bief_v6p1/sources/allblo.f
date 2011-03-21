!                    *****************
                     SUBROUTINE ALLBLO
!                    *****************
!
     &( BLO , NOM )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ALLOCATES MEMORY FOR A BLOCK STRUCTURE.
!
!history  J-M HERVOUET (LNH)
!+        10/01/95
!+        V5P5
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
!| BLO            |-->| THE BLOCK TO BE ALLOCATED
!| NOM            |-->| FORTRAN NAME OF THIS BLOCK
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ALLBLO => ALLBLO
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: BLO
      CHARACTER(LEN=6), INTENT(IN)    :: NOM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ERR
!
!-----------------------------------------------------------------------
!  COMMON PART FOR ALL OBJECTS
!-----------------------------------------------------------------------
!
!     KEY OF THE OBJECT
!
      BLO%KEY = 123456
!
!     TYPE OF THE OBJECT
!
      BLO%TYPE = 4
!
!     NAME OF THE OBJECT
!
      BLO%NAME = NOM
!
!-----------------------------------------------------------------------
!  PART SPECIFIC TO BLOCKS
!-----------------------------------------------------------------------
!
!     NUMBER OF OBJECTS IN THE BLOCK
!
      BLO%N = 0
!
!     ALLOCATES THE POINTERS ARRAY ADR
!
      BLO%MAXBLOCK = 128
      ALLOCATE(BLO%ADR(BLO%MAXBLOCK),STAT=ERR)
!
!-----------------------------------------------------------------------
!
      IF(ERR.EQ.0) THEN
!       IF(LNG.EQ.1) WRITE(LU,*) 'BLOC : ',NOM,' ALLOUE'
!       IF(LNG.EQ.2) WRITE(LU,*) 'BLOCK: ',NOM,' ALLOCATED'
      ELSE
        IF(LNG.EQ.1) WRITE(LU,10) NOM,ERR
        IF(LNG.EQ.2) WRITE(LU,20) NOM,ERR
10      FORMAT(1X,'ERREUR A L''ALLOCATION DU BLOC : ',A6,/,1X,
     &            'CODE D''ERREUR : ',1I6)
20      FORMAT(1X,'ERROR DURING ALLOCATION OF BLOCK: ',A6,/,1X,
     &            'ERROR CODE: ',1I6)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
