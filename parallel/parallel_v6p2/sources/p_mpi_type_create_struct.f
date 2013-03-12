!                    ***********************************
                     SUBROUTINE P_MPI_TYPE_CREATE_STRUCT
!                    ***********************************
!
     &(NBLOCK,NELEM,DISPL,ELEM_TYPE,NEW_DATATYPE,IERR)
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_TYPE_STRUCT.
!
!history  C. DENIS (SINETICS)
!+        27/10/2009
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
!| NBLOCK           |-->| NUMBER OF BLOCKS 
!| NELEM            |-->| NUMBER OF ELEMENTS IN EACH BLOCK
!| DISPL            |-->| BYTE DISLACEMENT   IN EACH BLOCK
!| ELEM_TYPE        |-->| TYPE OF ELEMENTS   IN EACH BLOCK 
!| NEW_DATATYPE     |<--| NEW DATATYPE
!| IERR             |<--| ERROR VALUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      include "mpif.h"
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)                           :: NBLOCK,IERR
      INTEGER, INTENT(OUT)                          :: NEW_DATATYPE
      INTEGER, INTENT(IN)                           :: NELEM(NBLOCK)
      INTEGER, INTENT(IN)                           :: ELEM_TYPE(NBLOCK)
      INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(INOUT) :: DISPL(NBLOCK)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL MPI_TYPE_CREATE_STRUCT(NBLOCK,NELEM,DISPL,ELEM_TYPE,
     &                            NEW_DATATYPE,IERR)
!
      IF(IERR.NE.0) THEN
        WRITE(LU,*) 'P_MPI_TYPE_STRUCT:'
        WRITE(LU,*) 'MPI ERROR ',IERR
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
