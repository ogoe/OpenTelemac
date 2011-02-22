C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CREATES A DATA SET FOR A GIVEN FILE FORMAT IN THE FILE
!>                WITH THE LOGICAL UNIT NFILE. THE TITLE OF THE DATASET
!>                IS GIVEN AS A 72 CHARACTER STRING.
!><br>            THE ARRAY NOMVAR CONTAINS ALL POSSIBLE VARIABLES TO
!>                OUTPUT (IE THE NAME OF ALL VARIABLES IN THE OUTPUT
!>                BLOCK). THE LOGICAL OUTVAR INDICATES FOR EACH
!>                VARIABLE WHETHER IT WILL BE WRITTEN OR NOT TO THE
!>                DATA FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>M_MED
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FFORMAT, NOMVAR, NRES, NVAR, OUTVAR, TITLE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CREATE_DATASET_MED(), CREATE_DATASET_SERAFIN(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS(), ECRSPE(), SISYPHE(), TELEMAC2D(), TELEMAC3D(), WAC()

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
!> </td><td> 25/11/08
!> </td><td> R NEBAUER (LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FFORMAT
!></td><td>--></td><td>FILE FORMAT
!>    </td></tr>
!>          <tr><td>NOMVAR
!></td><td>--></td><td>NAME OF VARIABLES
!>    </td></tr>
!>          <tr><td>NRES
!></td><td>--></td><td>LOGICAL UNIT OF FILE
!>    </td></tr>
!>          <tr><td>NVAR
!></td><td>--></td><td>TOTAL NUMBER OF VARIABLES
!>    </td></tr>
!>          <tr><td>OUTVAR
!></td><td>--></td><td>VARIABLES TO BE PUT IN THE FILE
!>    </td></tr>
!>          <tr><td>TITLE
!></td><td>--></td><td>TITLE OF FILE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CREATE_DATASET
     &(FFORMAT,NRES,TITLE,NVAR,NOMVAR,OUTVAR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FFORMAT        |-->| FILE FORMAT
C| NOMVAR         |-->| NAME OF VARIABLES
C| NRES           |-->| LOGICAL UNIT OF FILE
C| NVAR           |-->| TOTAL NUMBER OF VARIABLES
C| OUTVAR         |-->| VARIABLES TO BE PUT IN THE FILE
C| TITLE          |-->| TITLE OF FILE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE M_MED
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=8)                 , INTENT(IN) :: FFORMAT
      INTEGER                          , INTENT(IN) :: NRES
      CHARACTER(LEN=72)                , INTENT(IN) :: TITLE
      INTEGER                          , INTENT(IN) :: NVAR
      CHARACTER(LEN=32),DIMENSION(NVAR), INTENT(IN) :: NOMVAR
      LOGICAL          ,DIMENSION(NVAR), INTENT(IN) :: OUTVAR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
!
!***********************************************************************
C     IF(DEBUG) CALL PROC_BEGIN('CREATE_DATASET')
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
C     IF(DEBUG) CALL PROC_END('CREATE_DATASET')
!***********************************************************************
!
      RETURN
      END
C
C#######################################################################
C