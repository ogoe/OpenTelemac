!                    *****************************
                     SUBROUTINE BUILD_GLOBAL_FRONT
!                    *****************************
!
     &(MESH)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    RE-BUILDS THE GLOBAL MESH BOUNDARY.
!
!warning  USE ONLY IN PARALLEL
!
!history  C. DENIS (SINETICS)
!+        16/01/2010
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
!| MESH           |<->| MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INCLUDE 'dmumps_struc.h'
      INTEGER :: IER,I,POSITION
      INTEGER, ALLOCATABLE :: TEMP1(:)
      INTEGER, ALLOCATABLE :: TEMP2(:)
      INTEGER :: NPOIN_MAX
      INTEGER :: NPTFR_TOT
      INTEGER P_ISUM
      COMMON/INFO/LNG,LU
      INTEGER LNG,LU
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      IF(NCSIZE.LE.1) THEN
         IF(LNG.EQ.1) WRITE(LU,2018)
         IF(LNG.EQ.2) WRITE(LU,2019)
 2018    FORMAT(1X,'BUILD_GLOBAL_FRONT,',/,1X,
     &        'A UTILISER SEULEMENT EN MODE PARALLELE',///)
 2019    FORMAT(1X,'BULID_GLOBAL_FRONT,',/,1X,
     &        'USE ONLY IN THE PARALLEL VERSION',///)
         CALL PLANTE(1)
         STOP
      ENDIF
      NPOIN_MAX=P_ISUM(MESH%NPOIN)
      WRITE(35,*) 'NPOIN', MESH%NPOIN, NPOIN_MAX
      STOP
      ALLOCATE(TEMP1(NPOIN_MAX),STAT=IER)
      IF (IER .NE. 0) STOP 'ERREUR'
      ALLOCATE(TEMP2(NPOIN_MAX))
      IF (IER .NE. 0) STOP 'ERREUR'
      TEMP1(:)=0
      TEMP2(:)=0
      DO I=1,MESH%NPTFR
         POSITION=MESH%KNOLG%I(MESH%NBOR%I(I))
         TEMP1(POSITION)=1
      END DO
      CALL MPI_ALLREDUCE(TEMP1,TEMP2,NPOIN_MAX,MPI_INTEGER,
     &     MPI_MAX,
     &     MPI_COMM_WORLD,IER)
      NPTFR_TOT=0
      DO I=1,NPOIN_MAX
         IF (TEMP2(I) .NE. 0)  THEN
            NPTFR_TOT=NPTFR_TOT+1
         END IF
      END DO
!$$$      MESH%NPTFR=NPTFR_TOT
!$$$      DEALLOCATE(MESH%NBOR%I)
!$$$      ALLOCATE(MESH%NBOR%I(NPTFR)
!$$$      NBOR%I(:)=0
!$$$      DO I=1,MESH%NPTFR
!$$$         IF (TEMP2(I) .NE. 0) THEN
!$$$              MESH%NPTFR=NPTFR_TOT
!$$$
!$$$
!$$$
!$$$      NPTFR_TOT=0
      WRITE(*,*) 'NPTFR_TOT',NPTFR_TOT
      DEALLOCATE(TEMP1)
      DEALLOCATE(TEMP2)
      END
