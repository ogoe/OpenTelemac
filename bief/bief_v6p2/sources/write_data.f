!                    *********************
                     SUBROUTINE WRITE_DATA
!                    *********************
!
     &(FFORMAT,FILERES,NVARS,TIME,TIMESTEP,OUTVAR,NOMVAR,BVARSOR,N)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES DATA VALUES ON A MESH INTO THE DATA FILE OF THE
!+                GIVEN FILE FORMAT.
!+
!+            DATA VALUES ARE STORED IN A BIEF_OBJ BLOCK (BVARSOR),
!+                AND THE LOGICAL OUTVAR INDICATES FOR EACH VARIABLE IF
!+                WE SHOULD PRINT IT OUT OR NOT.
!
!history  R NEBAUER (LNHE)
!+        25/11/08
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
!| BVARSOR        |-->| BIEF BLOCK CONTAINING THE VARIABLES VARIABLES
!| FFORMAT        |-->| FILE FORMAT
!| FILERES        |-->| LOGICAL UNIT OF FILE
!| N              |-->| NUMBER OF VALUES (MAY BE DIFFERENT FROM
!|                |   | THE NUMBER OF DEGREES OF FREEDOM, E.G. FOR
!|                |   | QUADRATIC ELEMENTS ONLY THE LINEAR VALUES
!|                |   | ARE EXITED)
!| NOMVAR         |-->| NAME OF VARIABLES
!| NVARS          |-->| NUMBER OF VARIABLES
!| OUTVAR         |-->| VARIABLES TO BE PUT IN THE FILE
!| TIME           |-->| TIME
!| TIMESTEP       |-->| TIME STEP (INTEGER), NOT DT.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE M_MED
      USE BIEF     !, EX_WRITE_DATA => WRITE_DATA
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)          :: FFORMAT
      INTEGER,          INTENT(IN)          :: FILERES,N
      INTEGER,          INTENT(IN)          :: NVARS
      DOUBLE PRECISION, INTENT(IN)          :: TIME
      INTEGER,          INTENT(IN)          :: TIMESTEP
      CHARACTER(LEN=32),DIMENSION(NVARS), INTENT(IN) :: NOMVAR
      LOGICAL, DIMENSION(NVARS), INTENT(IN) :: OUTVAR
      TYPE(BIEF_OBJ),            INTENT(IN) :: BVARSOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!***********************************************************************
!     IF(DEBUG) CALL PROC_BEGIN('WRITE_DATA')
!***********************************************************************
!
      SELECT CASE (FFORMAT)
        CASE ('SERAFIN ','SERAFIND')
          CALL WRITE_DATA_SERAFIN(FILERES,NVARS,TIME,TIMESTEP,
     &                            OUTVAR,BVARSOR,FFORMAT,N)
        CASE ('MED     ')
          CALL WRITE_DATA_MED(FILERES,NVARS,TIME,TIMESTEP,
     &                        NOMVAR,OUTVAR,BVARSOR)
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'WRITE_DATA : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'WRITE_DATA: BAD FILE FORMAT : ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT
!
!***********************************************************************
!     IF(DEBUG) CALL PROC_END('WRITE_DATA')
!***********************************************************************
!
      RETURN
      END
