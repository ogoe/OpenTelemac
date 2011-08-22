!                    *********************************
                     SUBROUTINE CREATE_DATASET_SERAFIN
!                    *********************************
!
     &(NFIC,TITRE,NVAR,NOMVAR,OUTVAR)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
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
!+        05/09/2008
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
!| NFIC           |-->| LOGICAL UNIT OF THE FILE
!| NOMVAR         |-->| NAMES AND UNITS OF VARIABLES
!| NVAR           |-->| NUMBER OF VARIABLES
!| OUTVAR         |-->| LOGICAL ARRAY SAYING IF A VARIABLE HAS TO BE
!|                |   | PRINTED IN THE FILE.
!| TITRE          |-->| TITLE TO BE PRINTED IN THE FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                          , INTENT(IN) :: NFIC
      CHARACTER(LEN=72)                , INTENT(IN) :: TITRE
      INTEGER                          , INTENT(IN) :: NVAR
      CHARACTER(LEN=32),DIMENSION(NVAR), INTENT(IN) :: NOMVAR
      LOGICAL          ,DIMENSION(NVAR), INTENT(IN) :: OUTVAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER           :: NSOR ! NUMBER OF VARIABLES TO WRITE
      CHARACTER(LEN=80) :: TITSEL
      DOUBLE PRECISION XBID(2)
      INTEGER IB(10),ISTAT,I,IBID(1)
      CHARACTER*2 CBID
!
!***********************************************************************
!     IF(DEBUG) CALL PROC_BEGIN('CREATE_DATASET_SERAFIN')
!***********************************************************************
!
      REWIND NFIC
!
!   LEC/ECR 1   : NAME OF THE GEOMETRY FILE
!
      TITSEL = TITRE // 'SERAFIN '
      CALL ECRI2(XBID,IBID,TITSEL,80,'CH',NFIC,'STD',ISTAT)
!
!   LEC/ECR 2   : NUMBER OF DISCRETISATION FUNCTIONS 1 AND 2
!   NOTA : THIS FUNCTIONALITY OF SERAFIN FILES IS NOT USED. ALL THE
!          VARIABLES HAVE THE SAME (NODAL) DISCRETISATION.
!
      IB(1)=0
      IB(2)=0
      DO I=1,NVAR
        IF(OUTVAR(I)) IB(1) = IB(1) + 1
      ENDDO
      CALL ECRI2(XBID,IB,CBID,2,'I ',NFIC,'STD',ISTAT)
      NSOR =  IB(1)  +  IB(2)
!
!   LEC/ECR 3 : NAMES AND UNITS OF THE VARIABLES
!
      IF(NVAR.GE.1) THEN
       DO I=1,NVAR
         IF(OUTVAR(I)) THEN
          CALL ECRI2(XBID,IBID,NOMVAR(I)(1:32),32,'CH',NFIC,'STD',ISTAT)
         ENDIF
       ENDDO
      ENDIF
!
!***********************************************************************
!     IF(DEBUG) CALL PROC_END('CREATE_DATASET_SERAFIN')
!***********************************************************************
!
      RETURN
      END
