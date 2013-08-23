!                         ******************************
                          SUBROUTINE GRETEL_READ_DATASET
!                         ******************************
!
     &(LOCAL_VALUE,LOCAL_VALUE_D,SERAFIND,
     & NPOINMAX,NPOIN,NVALUE,AT,AT_D,FU,ENDE)
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
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
!| LOCAL_VALUE    |---|
!| NPOIN          |---|
!| NPOINMAX       |---|
!| NVALUE         |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOINMAX,NPOIN,NVALUE,FU
      INTEGER IPOIN,IVALUE
!
      LOGICAL, INTENT(IN) :: SERAFIND
!
      REAL, INTENT(OUT) :: AT
      DOUBLE PRECISION, INTENT(OUT) :: AT_D
      REAL, INTENT(OUT) :: LOCAL_VALUE(NPOINMAX,NVALUE)
      DOUBLE PRECISION, INTENT(OUT) :: LOCAL_VALUE_D(NPOINMAX,NVALUE)
!
      LOGICAL, INTENT(OUT) :: ENDE
!
      ENDE = .TRUE.
!
      IF(SERAFIND) THEN
        READ(FU,END=999) AT_D
        DO IVALUE = 1,NVALUE
          READ(FU,END=999) (LOCAL_VALUE_D(IPOIN,IVALUE),IPOIN=1,NPOIN)
        ENDDO
      ELSE
        READ (FU,END=999) AT
        DO IVALUE = 1,NVALUE
          READ(FU,END=999) (LOCAL_VALUE(IPOIN,IVALUE),IPOIN=1,NPOIN)
        ENDDO
      ENDIF
!
      ENDE = .FALSE.
!
!-----------------------------------------------------------------------
!
 999  RETURN
      END SUBROUTINE GRETEL_READ_DATASET

