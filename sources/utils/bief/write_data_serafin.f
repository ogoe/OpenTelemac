!                    *****************************
                     SUBROUTINE WRITE_DATA_SERAFIN
!                    *****************************
!
     &(NFIC,NVARS,TIME,TIMESTEP,OUTVAR,BVARSOR,FFORMAT,N)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES RECORDS OF RESULTS IN A SERAFIN FORMAT FILE.
!
!history  R NEBAUER (LNHE)
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
!| BVARSOR        |-->| BIEF_OBJ BLOCK WITH DATA VALUES
!| FFORMAT        |-->| FILE FORMAT
!| N              |-->| NUMBER OF VALUES (MAY BE DIFFERENT FROM
!|                |   | THE NUMBER OF DEGREES OF FREEDOM, E.G. FOR
!|                |   | QUADRATIC ELEMENTS ONLY THE LINEAR VALUES
!|                |   | ARE EXITED)
!| NFIC           |-->| LOGICAL UNIT OF FILE
!| NVARS          |-->| NUMBER OF VARIABLES
!| OUTVAR         |-->| INDICATES FOR EACH VARIABLE IF WE SHOULD
!|                |   | PRINT IT OUT OR NOT
!| TIME           |-->| LOGICAL UNIT OF FILE
!| TIMESTEP       |-->| TIME STEP (INTEGER, NOT DT)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER        ,  INTENT(IN)          :: NFIC,NVARS,N,TIMESTEP
      DOUBLE PRECISION, INTENT(IN)          :: TIME
      LOGICAL, DIMENSION(NVARS), INTENT(IN) :: OUTVAR
      TYPE(BIEF_OBJ),            INTENT(IN) :: BVARSOR
      CHARACTER(LEN=8), INTENT(IN)          :: FFORMAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=2)               :: RF
      DOUBLE PRECISION, DIMENSION(1) :: TTIME
      INTEGER                        :: K,ISTAT
      INTEGER                        :: IBID(1)
      CHARACTER*2                    :: CBID
!
!***********************************************************************
!     IF(DEBUG) CALL PROC_BEGIN('WRITE_DATA_SERAFIN')
!***********************************************************************
!
      IF(FFORMAT.EQ.'SERAFIND') THEN
        RF = 'R8'
      ELSE
        RF = 'R4'
      ENDIF
!
      TTIME(1) = TIME
!
      CALL ECRI2(TTIME,IBID,CBID,1,RF,NFIC,'STD',ISTAT)
!
      DO K=1,NVARS
        IF(OUTVAR(K)) THEN
          ! HOPING THAT IT WILL WORK ...
          ! GIVEN THAT N IS NOT AN ARGUMENT ...
          ! N = BVARSOR%ADR(K)%P%DIM1
!  CORRECTION JMH 21/04/2009 NO, N IS GIVEN AND MAY BE DIFFERENT
!  FROM BVARSOR%ADR(K)%P%DIM1 (QUASI-BUBBLE AND QUADRATIC ELEMENTS)
          IF(ASSOCIATED(BVARSOR%ADR(K)%P%R)) THEN
            CALL ECRI2(BVARSOR%ADR(K)%P%R,IBID,CBID,N,RF,NFIC,'STD',
     &                 ISTAT)
          ELSE
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'WRITE_DATA_SERAFIN : VARIABLE NO : ',K
              WRITE(LU,*) '        PAS OU MAL ALLOUEE'
              WRITE(LU,*) '        OU POINTEUR NON ASSOCIE'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'WRITE_DATA_SERAFIN: VARIABLE NO: ',K
              WRITE(LU,*) '        NOT OR NOT WELL ALLOCATED'
              WRITE(LU,*) '        OR POINTER NOT ASSOCIATED '
            ENDIF
          ENDIF
        ENDIF
      ENDDO
!
!***********************************************************************
!     IF(DEBUG) CALL PROC_END('WRITE_DATA_SERAFIN')
!***********************************************************************
!
      RETURN
      END
