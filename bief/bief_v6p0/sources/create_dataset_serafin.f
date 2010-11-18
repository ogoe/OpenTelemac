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

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NFIC, NOMVAR, NVAR, OUTVAR, TITRE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CBID, I, IB, IBID, ISTAT, NSOR, TITSEL, XBID
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ECRI2()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CREATE_DATASET()

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
!> </td><td> 05/09/2008
!> </td><td> R NEBAUER (LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NFIC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMVAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NVAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OUTVAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TITRE
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CREATE_DATASET_SERAFIN
     &(NFIC,TITRE,NVAR,NOMVAR,OUTVAR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NFIC           |---| 
C| NOMVAR         |---| 
C| NVAR           |---| 
C| OUTVAR         |---| 
C| TITRE          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER                          , INTENT(IN) :: NFIC
      CHARACTER(LEN=72)                , INTENT(IN) :: TITRE
      INTEGER                          , INTENT(IN) :: NVAR
      CHARACTER(LEN=32),DIMENSION(NVAR), INTENT(IN) :: NOMVAR
      LOGICAL          ,DIMENSION(NVAR), INTENT(IN) :: OUTVAR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER           :: NSOR ! NUMBER OF VARIABLES TO WRITE
      CHARACTER(LEN=80) :: TITSEL
      DOUBLE PRECISION XBID(2)
      INTEGER IB(10),ISTAT,I,IBID(1)
      CHARACTER*2 CBID
!
!***********************************************************************
C     IF(DEBUG) CALL PROC_BEGIN('CREATE_DATASET_SERAFIN')
!***********************************************************************
!
      REWIND NFIC
!
C   LEC/ECR 1   : NAME OF THE GEOMETRY FILE
!
      TITSEL = TITRE // 'SERAFIN '
      CALL ECRI2(XBID,IBID,TITSEL,80,'CH',NFIC,'STD',ISTAT)
!
C   LEC/ECR 2   : NUMBER OF DISCRETISATION FUNCTIONS 1 AND 2
C NOTA : THIS FUNCTIONALITY OF SERAFIN FILES IS NOT USED. ALL THE
C        VARIABLES HAVE THE SAME (NODAL) DISCRETISATION.
!
      IB(1)=0
      IB(2)=0
      DO I=1,NVAR
        IF(OUTVAR(I)) IB(1) = IB(1) + 1
      ENDDO
      CALL ECRI2(XBID,IB,CBID,2,'I ',NFIC,'STD',ISTAT)
      NSOR =  IB(1)  +  IB(2)
!
C   LEC/ECR 3 : NAMES AND UNITS OF THE VARIABLES
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
C     IF(DEBUG) CALL PROC_END('CREATE_DATASET_SERAFIN')
!***********************************************************************
!
      RETURN
      END
C
C#######################################################################
C