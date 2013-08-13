!
!                         *****************************************
                          SUBROUTINE GRETEL_READ_DATASET_ELEM
!                         *****************************************
     &(LOCAL_VALUELEM,NPROC,NELEM,NBV1,AT,FU,IPID,ENDE)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
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
!| AT             |---|
!| ENDE           |---|
!| FU             |---|
!| IPID           |---|
!| LOCAL_VALUELEM |---|
!| NBV1           |---|
!| NELEM          |---|
!| NPROC          |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPROC,NELEM,NBV1,FU,IPID
      INTEGER IELEM,IVALUE
!
      REAL,INTENT(OUT) :: AT
      REAL, INTENT(OUT) :: LOCAL_VALUELEM(0:NPROC-1,1:NELEM,1:NBV1)
!
      LOGICAL, INTENT(OUT) :: ENDE
!
      ENDE = .TRUE.
!
      READ(FU,END=9099) AT
      DO IVALUE = 1,NBV1
         READ(FU,END=9099) (LOCAL_VALUELEM(IPID,IELEM,IVALUE)
     &   ,IELEM=1,NELEM)
      END DO
!
      ENDE = .FALSE.
!
 9099  RETURN
      END SUBROUTINE GRETEL_READ_DATASET_ELEM

