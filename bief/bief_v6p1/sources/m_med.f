!                    ************
                     MODULE M_MED
!                    ************
!
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FAKE MODULE M_MED, SHOULD NOT BE CALLED
!+     SEE M_MED.EDF FOR THE REAL STUFF,
!+     BUT MED AND HDF5 LIBRARIES NECESSARY
!+     FOR MORE INFORMATION: HTTP://WWW.CODE-ASTER.ORG/OUTILS/MED/
!
!history  J-M HERVOUET (LNH)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      PUBLIC OPEN_FILE_MED
      PUBLIC CLOSE_FILE_MED
      PUBLIC WRITE_MESH_MED
      PUBLIC CREATE_DATASET_MED
      PUBLIC WRITE_DATA_MED
      PUBLIC SUITE_MED
!
      CONTAINS
!                       ************************
                        SUBROUTINE OPEN_FILE_MED
!                       ************************
!
     &(MEDNAME,MEDFILE,ACTION)
!
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
! ARGUMENTS :
!
      INTEGER, INTENT(INOUT)       :: MEDFILE
      CHARACTER(LEN=*), INTENT(IN) :: MEDNAME
      CHARACTER(LEN=9)             :: ACTION
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LE SOUS-PROGRAMME OPEN_FILE_MED'
        WRITE(LU,*) 'NE DOIT PAS ETRE APPELE SANS BIBLIOTHEQUE MED'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'SUBROUTINE OPEN_FILE_MED'
        WRITE(LU,*) 'SHOULD NOT BE CALLED WITHOUT LIBRARY MED'
      ENDIF
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE OPEN_FILE_MED
!
!                       *************************
                        SUBROUTINE CLOSE_FILE_MED
!                       *************************
!
     &(MEDFILE)
!
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
! ARGUMENTS :
!
      INTEGER, INTENT(IN) :: MEDFILE
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LE SOUS-PROGRAMME CLOSE_FILE_MED'
        WRITE(LU,*) 'NE DOIT PAS ETRE APPELE SANS BIBLIOTHEQUE MED'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'SUBROUTINE CLOSE_FILE_MED'
        WRITE(LU,*) 'SHOULD NOT BE CALLED WITHOUT LIBRARY MED'
      ENDIF
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CLOSE_FILE_MED
!                       *************************
                        SUBROUTINE WRITE_MESH_MED
!                       *************************
!
     &(RES_FILE,MESH,X_ORIG,Y_ORIG)
!
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
! ARGUMENTS :
!
      INTEGER        , INTENT(IN)  :: RES_FILE
      TYPE(BIEF_MESH), INTENT(IN)  :: MESH
      DOUBLE PRECISION,INTENT(IN)  :: X_ORIG
      DOUBLE PRECISION,INTENT(IN)  :: Y_ORIG
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LE SOUS-PROGRAMME WRITE_MESH_MED'
        WRITE(LU,*) 'NE DOIT PAS ETRE APPELE SANS BIBLIOTHEQUE MED'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'SUBROUTINE WRITE_MESH_MED'
        WRITE(LU,*) 'SHOULD NOT BE CALLED WITHOUT LIBRARY MED'
      ENDIF
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE WRITE_MESH_MED
!                       *****************************
                        SUBROUTINE CREATE_DATASET_MED
!                       *****************************
!
     &(FILERES,TITLE,NVAR,NOMVAR,OUTVAR)
!
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                          , INTENT(IN) :: FILERES
      CHARACTER(LEN=72)                , INTENT(IN) :: TITLE
      INTEGER                          , INTENT(IN) :: NVAR
      CHARACTER(LEN=32),DIMENSION(NVAR), INTENT(IN) :: NOMVAR
      LOGICAL          ,DIMENSION(NVAR), INTENT(IN) :: OUTVAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LE SOUS-PROGRAMME WRITE_MESH_MED'
        WRITE(LU,*) 'NE DOIT PAS ETRE APPELE SANS BIBLIOTHEQUE MED'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'SUBROUTINE WRITE_MESH_MED'
        WRITE(LU,*) 'SHOULD NOT BE CALLED WITHOUT LIBRARY MED'
      ENDIF
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CREATE_DATASET_MED
!                       *************************
                        SUBROUTINE WRITE_DATA_MED
!                       *************************
!
     &(FILERES,NVARS,TIME,TIMESTEP,NOMVAR,OUTVAR,BVARSOR)
!
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)                   :: FILERES
      INTEGER,          INTENT(IN)                   :: NVARS
      DOUBLE PRECISION, INTENT(IN)                   :: TIME
      INTEGER,          INTENT(IN)                   :: TIMESTEP
      CHARACTER(LEN=32),DIMENSION(NVARS), INTENT(IN) :: NOMVAR
      LOGICAL, DIMENSION(NVARS), INTENT(IN)          :: OUTVAR
      TYPE(BIEF_OBJ),            INTENT(IN)          :: BVARSOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LE SOUS-PROGRAMME WRITE_DATA_MED'
        WRITE(LU,*) 'NE DOIT PAS ETRE APPELE SANS BIBLIOTHEQUE MED'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'SUBROUTINE WRITE_DATA_MED'
        WRITE(LU,*) 'SHOULD NOT BE CALLED WITHOUT LIBRARY MED'
      ENDIF
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE WRITE_DATA_MED
!                       ********************
                        SUBROUTINE SUITE_MED
!                       ********************
!
     &(VARSOR,CLAND,NUMDEB,
     & FILERES,STD,HIST,NHIST,NPOIN,AT,TEXTPR,VARCLA,NVARCL,
     & TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NPLAN,DT,NDT)
!
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARSOR,CLAND
      INTEGER, INTENT(IN), OPTIONAL :: NPLAN
      INTEGER, INTENT(IN)           :: NHIST,NVARCL,MAXVAR,FILERES
      INTEGER                       :: NUMDEB,NPOIN,TROUVE(MAXVAR)
      INTEGER                       :: ALIRE(MAXVAR)
      CHARACTER(LEN=*)              :: STD
      CHARACTER(LEN=32)             :: TEXTPR(MAXVAR),VARCLA(NVARCL)
      DOUBLE PRECISION              :: HIST(*),AT,DT
      LOGICAL                       :: FIN,LISTIN
      INTEGER, INTENT(OUT), OPTIONAL :: NDT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     AVOIDS A COMPILER WARNING
      IF(PRESENT(NDT)) NDT=0
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LE SOUS-PROGRAMME SUITE_MED'
        WRITE(LU,*) 'NE DOIT PAS ETRE APPELE SANS BIBLIOTHEQUE MED'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'SUBROUTINE SUITE_MED'
        WRITE(LU,*) 'SHOULD NOT BE CALLED WITHOUT LIBRARY MED'
      ENDIF
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SUITE_MED
!
      END MODULE M_MED
