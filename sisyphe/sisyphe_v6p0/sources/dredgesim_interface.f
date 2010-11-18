C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       THIS IS THE INTERFACE TO DREDGESIM, CONTAINING ALL
!>                DEPENDENCIES TO DREDGESIM LIBRARIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  FOR REAL INTERFACING WITH DREDGESIM, COMMENTS "CDSIM"
!>            MUST BE REMOVED

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> OPTION
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::IPID IPID@endlink, 
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::AVAIL AVAIL@endlink, 
!> @link DECLARATIONS_SISYPHE::DZF_GF DZF_GF@endlink, 
!> @link DECLARATIONS_SISYPHE::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_SISYPHE::NSICLA NSICLA@endlink, 
!> @link DECLARATIONS_SISYPHE::ZFCL_C ZFCL_C@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AVAI_GF, DREDGEINP, I, J, SEDGEO
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> EXTENS(), OS(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_MAIN(), SISYPHE()

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
!> </td><td> 02/02/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>OPTION
!></td><td>--></td><td>1 : INITIALISATION (CALLED IN SISYPHE)
!>                  2 : CALLED EVERY TIME STEP (FROM
!>                  BEDLOAD_POSTTREATMENT)
!>                  3 : END  (CALLED IN SISYPHE)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DREDGESIM_INTERFACE
     &(OPTION)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| OPTION         |-->| 1 : INITIALISATION (CALLED IN SISYPHE)
C|                |   | 2 : CALLED EVERY TIME STEP (FROM
C|                |   | BEDLOAD_POSTTREATMENT)
C|                |   | 3 : END  (CALLED IN SISYPHE)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : EXTENS
      USE DECLARATIONS_SISYPHE, ONLY : DREDGESIM,DT,NPOIN,NSICLA,DZF_GF,
     &                                 ZFCL_C,AVAIL,MESH,SIS_FILES
CDSIM     USE P_SISYPHE_UI, ONLY : INIT_AND_SETUP_DS,CLEAR_SISYDREDGE
CDSIM     USE P_DREDGESIM_UI, ONLY : STOP_DREDGESIM,CLEAR_DREDGESIM
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: OPTION
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=250) :: DREDGEINP,SEDGEO
      DOUBLE PRECISION,ALLOCATABLE :: AVAI_GF(:,:)
      INTEGER I,J
C
C-----------------------------------------------------------------------
C
      IF(OPTION.EQ.1) THEN
C
C     INITIALISES
C
        DREDGEINP = ''
        SEDGEO = ''
        IF(NCSIZE.GT.1) THEN
C         INPUT FILE FOR DREDGESIM
          DREDGEINP = TRIM('SISMAF'//EXTENS(NCSIZE-1,IPID))
          SEDGEO = TRIM('SISGEO'//EXTENS(NCSIZE-1,IPID))
        ELSE
          DREDGEINP = 'SISMAF'
          SEDGEO = 'SISGEO'
        ENDIF
CDSIM       CALL INIT_AND_SETUP_DS(SIS_FILES(SISMAF)%LU,DREDGEINP,
CDSIM                              SIS_FILES(SISGEO)%LU,
CDSIM                              SEDGEO,
CDSIM    &                         NCSIZE,IPID,MESH%ILMAX,
CDSIM    &                         MESH%IKP%I,MESH%IKM%I,MESH%NACHB%I,
CDSIM    &                         MESH%INDPU%I,MESH%NHP%I,MESH%NHM%I)
C
      ELSEIF(OPTION.EQ.2) THEN
C
C     CALL FROM WITHIN BEDLOAD_POSTTREATMENT
C
C       ALLOCATES AVAI_GF
        ALLOCATE(AVAI_GF(NPOIN,NSICLA))
C       INITIALISES THE DEPTH TO ADD
        CALL OS('X=0     ',X=DZF_GF)
CDSIM       CALL RUN_DREDGESIM(DT)
CDSIM       AVAI_GF = GET_SM_NODE_SEDIMENT_FRACTION()
        DO J = 1, NPOIN
          IF(DZF_GF%R(J).GT.0.D0) THEN
            DO I = 1, NSICLA
              ZFCL_C%ADR(I)%P%R(J) = ZFCL_C%ADR(I)%P%R(J) +
     &                               DZF_GF%R(J)*AVAI_GF(J,I)
            ENDDO
          ELSE
            DO I = 1, NSICLA
              ZFCL_C%ADR(I)%P%R(J) = ZFCL_C%ADR(I)%P%R(J) +
     &                               DZF_GF%R(J)*AVAIL(J,1,I)
            ENDDO
          ENDIF
        ENDDO
        DEALLOCATE(AVAI_GF)
C
      ELSEIF(OPTION.EQ.3) THEN
C
C     CLOSES
C
CDSIM       CALL STOP_DREDGESIM()
CDSIM       CALL CLEAR_DREDGESIM()
CDSIM       CALL CLEAR_SISYDREDGE()
C
      ELSE
C
C     ERROR
C
        IF(LNG.EQ.1) WRITE(LU,*) 'MAUVAISE OPTION POUR DREDGESIM'
        IF(LNG.EQ.2) WRITE(LU,*) 'BAD OPTION FOR DREDGESIM'
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C