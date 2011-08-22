!                    *************************
                     SUBROUTINE CREATE_DATASET
!                    *************************
!
     &(FFORMAT,NRES,TITLE,NVAR,NOMVAR,OUTVAR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CREATES A DATA SET FOR A GIVEN FILE FORMAT IN THE FILE
!+                WITH THE LOGICAL UNIT NFILE. THE TITLE OF THE DATASET
!+                IS GIVEN AS A 72 CHARACTER STRING.
!+
!+            THE ARRAY NOMVAR CONTAINS ALL POSSIBLE VARIABLES TO
!+                OUTPUT (IE THE NAME OF ALL VARIABLES IN THE OUTPUT
!+                BLOCK). THE LOGICAL OUTVAR INDICATES FOR EACH
!+                VARIABLE WHETHER IT WILL BE WRITTEN OR NOT TO THE
!+                DATA FILE.
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
!| FFORMAT        |-->| FILE FORMAT
!| NOMVAR         |-->| NAME OF VARIABLES
!| NRES           |-->| LOGICAL UNIT OF FILE
!| NVAR           |-->| TOTAL NUMBER OF VARIABLES
!| OUTVAR         |-->| VARIABLES TO BE PUT IN THE FILE
!| TITLE          |-->| TITLE OF FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE M_MED
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8)                 , INTENT(IN) :: FFORMAT
      INTEGER                          , INTENT(IN) :: NRES
      CHARACTER(LEN=72)                , INTENT(IN) :: TITLE
      INTEGER                          , INTENT(IN) :: NVAR
      CHARACTER(LEN=32),DIMENSION(NVAR), INTENT(IN) :: NOMVAR
      LOGICAL          ,DIMENSION(NVAR), INTENT(IN) :: OUTVAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!***********************************************************************
!     IF(DEBUG) CALL PROC_BEGIN('CREATE_DATASET')
!***********************************************************************
!
      SELECT CASE (FFORMAT)
        CASE ('SERAFIN ','SERAFIND') !SERAFIN)
            CALL CREATE_DATASET_SERAFIN(
     &                          NRES,
     &                          TITLE,
     &                          NVAR,
     &                          NOMVAR,
     &                          OUTVAR)
!
        CASE ('MED     ') !MED)
            CALL CREATE_DATASET_MED(
     &                          NRES,
     &                          TITLE,
     &                          NVAR,
     &                          NOMVAR,
     &                          OUTVAR)
!
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'CREATE_DATASET : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'CREATE_DATASET: BAD FILE FORMAT : ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT
!
!***********************************************************************
!     IF(DEBUG) CALL PROC_END('CREATE_DATASET')
!***********************************************************************
!
      RETURN
      END
