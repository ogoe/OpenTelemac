!                    *********************
                     SUBROUTINE WRITE_MESH
!                    *********************
!
     &(FFORMAT,NFILE,MESH,NPLAN,DATE,TIME,I_ORIG,J_ORIG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES THE MESH, DESCRIBED BY THE BIEF_MESH STRUCTURE
!+                INTO THE FILE. BIEF_MESH STRUCTURE CONTAINS INFORMATIONS
!+                ABOUT CONNECTIVITY, COORDINATES, BOUNDARY NODES. OTHER
!+                INFORMATIONS NEEDED : THE DATE AND TIME INFORMATION, AND
!+                THE ORIGIN OF THE COORDINATE SYSTEM (X_ORIG,Y_ORIG).
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
!| DATE           |-->| 3 INTEGERS (YEAR, MONTH, DAY)
!| FFORMAT        |-->| FILE FORMAT
!| I_ORIG         |-->| OFFSET OF ABSCISSAE IN METRES
!| J_ORIG         |-->| OFFSET OF ORDINATES IN METRES
!| MESH           |-->| MESH STRUCTURE
!| NFILE          |-->| LOGICAL UNIT OF FILE
!| NPLAN          |-->| NUMBER OF PLANES (3D)
!| TIME           |-->| 3 INTEGERS (HOUR, MINUTE, SECOND)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE M_MED
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8) ,     INTENT(IN) :: FFORMAT
      INTEGER          ,     INTENT(IN) :: NFILE,NPLAN
      TYPE(BIEF_MESH),       INTENT(IN) :: MESH
      INTEGER, DIMENSION(3), INTENT(IN) :: DATE
      INTEGER, DIMENSION(3), INTENT(IN) :: TIME
      INTEGER,               INTENT(IN) :: I_ORIG,J_ORIG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!***********************************************************************
!     IF(DEBUG) CALL PROC_BEGIN('WRITE_MESH')
!***********************************************************************
!
      SELECT CASE (FFORMAT)
        CASE ('SERAFIN','SERAFIND')
           CALL WRITE_MESH_SERAFIN(NFILE,
     &                             MESH,
     &                             NPLAN,
     &                             DATE,
     &                             TIME,
     &                             I_ORIG,J_ORIG,
     &                             FFORMAT)
        CASE ('MED     ')
           CALL WRITE_MESH_MED(NFILE,MESH,DBLE(I_ORIG),DBLE(J_ORIG))
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'WRITE_MESH : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'WRITE_MESH: BAD FILE FORMAT : ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT
!
!***********************************************************************
!     IF(DEBUG) CALL PROC_END('WRITE_MESH')
!***********************************************************************
!
      RETURN
      END
