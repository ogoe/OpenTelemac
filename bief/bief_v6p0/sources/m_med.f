C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief        FAKE MODULE M_MED, SHOULD NOT BE CALLED
!>     SEE M_MED.EDF FOR THE REAL STUFF,
!>     BUT MED AND HDF5 LIBRARIES NECESSARY
!>     FOR MORE INFORMATION: HTTP://WWW.CODE-ASTER.ORG/OUTILS/MED/

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 01/04/2009
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
      MODULE M_MED
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
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
C ARGUMENTS :
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
C ARGUMENTS :
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
C ARGUMENTS :
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
C                       *****************************
                        SUBROUTINE CREATE_DATASET_MED
C                       *****************************
C
     &(FILERES,TITLE,NVAR,NOMVAR,OUTVAR)
C
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER                          , INTENT(IN) :: FILERES
      CHARACTER(LEN=72)                , INTENT(IN) :: TITLE
      INTEGER                          , INTENT(IN) :: NVAR
      CHARACTER(LEN=32),DIMENSION(NVAR), INTENT(IN) :: NOMVAR
      LOGICAL          ,DIMENSION(NVAR), INTENT(IN) :: OUTVAR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
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
C                       *************************
                        SUBROUTINE WRITE_DATA_MED
C                       *************************
C
     &(FILERES,NVARS,TIME,TIMESTEP,NOMVAR,OUTVAR,BVARSOR)
C
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN)                   :: FILERES
      INTEGER,          INTENT(IN)                   :: NVARS
      DOUBLE PRECISION, INTENT(IN)                   :: TIME
      INTEGER,          INTENT(IN)                   :: TIMESTEP
      CHARACTER(LEN=32),DIMENSION(NVARS), INTENT(IN) :: NOMVAR
      LOGICAL, DIMENSION(NVARS), INTENT(IN)          :: OUTVAR
      TYPE(BIEF_OBJ),            INTENT(IN)          :: BVARSOR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
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
C                       ********************
                        SUBROUTINE SUITE_MED
C                       ********************
C
     &(VARSOR,CLAND,NUMDEB,
     & FILERES,STD,HIST,NHIST,NPOIN,AT,TEXTPR,VARCLA,NVARCL,
     & TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NPLAN,DT,NDT)
C
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
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
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     AVOIDS A COMPILER WARNING
      IF(PRESENT(NDT)) NDT=0
C
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
C
      END MODULE M_MED
C
C#######################################################################
C