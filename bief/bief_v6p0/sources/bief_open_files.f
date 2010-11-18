C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPENS FILES DECLARED IN THE STEERING FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  STEERING AND DICTIONARY FILES ARE OPENED AND CLOSED
!>         IN LECDON

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, M_MED
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CODE, FILES, FLOT, ICODE, IFLOT, NCAR, NFILES, PATH
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::IPID IPID@endlink, 
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::NAMECODE NAMECODE@endlink, 
!> @link DECLARATIONS_TELEMAC::NNAMECODE NNAMECODE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FORME, I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BIEF_OPEN_FILES
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> EXTENS(), OPEN_FILE_MED()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ARTEMIS(), HOMERE_SISYPHE(), HOMERE_TELEMAC2D(), HOMERE_TELEMAC3D(), HOMERE_TOMAWAC()

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
!> </td><td> 12/10/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CODE
!></td><td>--></td><td>NAME OF CALLING PROGRAMME
!>    </td></tr>
!>          <tr><td>FILES
!></td><td>--></td><td>STRUCTURES OF CODE FILES
!>    </td></tr>
!>          <tr><td>FLOT
!></td><td>--></td><td>LOGICAL, IF YES LOGICAL UNITS DECIDED BY
!>                  THIS SUBROUTINE, IF NO, TAKEN IN SUBMIT
!>    </td></tr>
!>          <tr><td>ICODE
!></td><td>---</td><td>NUMERO DU CODE EN CAS DE COUPLAGE
!>    </td></tr>
!>          <tr><td>IFLOT
!></td><td>--></td><td>IF FLOT=YES, START NEW LOGICAL UNIT NUMBERS
!>                  AT IFLOT+1
!>    </td></tr>
!>          <tr><td>NCAR
!></td><td>--></td><td>NUMBER OF CHARACTERS IN THE PATH
!>    </td></tr>
!>          <tr><td>NFILES
!></td><td>--></td><td>NUMBER OF FILES
!>    </td></tr>
!>          <tr><td>PATH
!></td><td>--></td><td>FULL NAME OF THE PATH WHERE THE CASE IS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BIEF_OPEN_FILES
     &(CODE,FILES,NFILES,PATH,NCAR,FLOT,IFLOT,ICODE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CODE           |-->| NAME OF CALLING PROGRAMME
C| FILES          |-->| STRUCTURES OF CODE FILES
C| FLOT           |-->| LOGICAL, IF YES LOGICAL UNITS DECIDED BY
C|                |   | THIS SUBROUTINE, IF NO, TAKEN IN SUBMIT
C| ICODE          |---| NUMERO DU CODE EN CAS DE COUPLAGE
C| IFLOT          |-->| IF FLOT=YES, START NEW LOGICAL UNIT NUMBERS
C|                |   | AT IFLOT+1
C| NCAR           |-->| NUMBER OF CHARACTERS IN THE PATH
C| NFILES         |-->| NUMBER OF FILES
C| PATH           |-->| FULL NAME OF THE PATH WHERE THE CASE IS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_BIEF_OPEN_FILES => BIEF_OPEN_FILES
      USE DECLARATIONS_TELEMAC
      USE M_MED
C
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER           , INTENT(IN)    :: NFILES
      CHARACTER(LEN=24) , INTENT(IN)    :: CODE
      TYPE(BIEF_FILE)   , INTENT(INOUT) :: FILES(NFILES)
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
      INTEGER           , INTENT(IN)    :: NCAR,ICODE
      INTEGER           , INTENT(INOUT) :: IFLOT
      LOGICAL           , INTENT(IN)    :: FLOT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
      CHARACTER(LEN=11) :: FORME
C
C-----------------------------------------------------------------------
C
C     MESSAGE
C
      IF(LNG.EQ.1) WRITE(LU,*) 'OUVERTURE DES FICHIERS POUR ',CODE
      IF(LNG.EQ.2) WRITE(LU,*) 'OPENING FILES FOR ',CODE
C
C
C     DECODES THE SUBMIT STRING FOR THE FILES IN THE STEERING FILE
C
      DO I=1,NFILES
C
        IF(FILES(I)%NAME(1:1).NE.' ') THEN
C
C         LOGICAL UNIT MODIFIED WHEN COUPLING
C
          IF(FLOT) THEN
            IFLOT=IFLOT+1
C           2 AND 3 SKIPPED (DICTIONARY AND STEERING FILES)
            IF(IFLOT.EQ.2) IFLOT=4
C           5 AND 6 SKIPPED (STANDARD INPUT AND OUTPUT)
            IF(IFLOT.EQ.5) IFLOT=7
            FILES(I)%LU=IFLOT
          ENDIF
C
          IF(FILES(I)%BINASC.EQ.'ASC') THEN
            FORME='FORMATTED  '
          ELSE
            FORME='UNFORMATTED'
          ENDIF
C
C         OPENS THE FILE
C
          IF(FILES(I)%FMT.EQ.'MED     ') THEN
C
            IF(NCSIZE.LE.1) THEN
              CALL OPEN_FILE_MED(FILES(I)%TELNAME,FILES(I)%LU,
     &                           FILES(I)%ACTION)
            ELSE
C             PARALLEL MODE, FILE TYPE: SCAL
              IF(FILES(I)%TYPE(1:4).EQ.'SCAL') THEN
              CALL OPEN_FILE_MED(PATH(1:NCAR)//TRIM(FILES(I)%TELNAME)
     &               ,FILES(I)%LU,FILES(I)%ACTION)
C             PARALLEL MODE, OTHER FILE TYPE
              ELSE
              CALL OPEN_FILE_MED(PATH(1:NCAR)//TRIM(FILES(I)%TELNAME)
     &               //EXTENS(NCSIZE-1,IPID)//'.MED',FILES(I)%LU,
     &               FILES(I)%ACTION)
              ENDIF
            ENDIF
C
          ELSE
C
            IF(NCSIZE.LE.1) THEN
C             SCALAR
              OPEN(FILES(I)%LU,FILE=FILES(I)%TELNAME,
     &             FORM=FORME,ACTION=FILES(I)%ACTION)
            ELSE
C             PARALLEL, FILE TYPE: SCAL
              IF(FILES(I)%TYPE(1:4).EQ.'SCAL') THEN
                OPEN(FILES(I)%LU,
     &               FILE=PATH(1:NCAR)//TRIM(FILES(I)%TELNAME),
     &               FORM=FORME,ACTION=FILES(I)%ACTION)
C             PARALLEL, OTHER FILE TYPE
              ELSE
                OPEN(FILES(I)%LU,
     &               FILE=PATH(1:NCAR)//TRIM(FILES(I)%TELNAME)
     &               //EXTENS(NCSIZE-1,IPID),
     &               FORM=FORME,ACTION=FILES(I)%ACTION)
              ENDIF
            ENDIF
C
          ENDIF
C
        ENDIF
C
      ENDDO
C
C     SETS AND STORES THE CODE NAME
C
      NAMECODE = CODE
      NNAMECODE(ICODE) = CODE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C