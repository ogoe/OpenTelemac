C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPENS THE FILES DECLARED IN THE STEERING FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THE STEERING AND DICTIONNARY FILES ARE OPENED AND
!>         CLOSED IN LECDON.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CODE, FILES, NFILES, NMOT, SUBMIT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CANAL, I, I1, ICOL, LITECR, NOMCANAL
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_READ_SUBMIT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> INTLU(), PLANTE(), PREVAL()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>LECDON_ARTEMIS(), LECDON_SISYPHE(), LECDON_TELEMAC2D(), LECDON_TELEMAC3D(), LECDON_TOMAWAC()

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
!> </td><td> 27/03/2009
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
!></td><td>--></td><td>FILES STRUCTURES
!>    </td></tr>
!>          <tr><td>NFILES
!></td><td>--></td><td>NUMBER OF FILES IN ARRAY FILES
!>    </td></tr>
!>          <tr><td>NMOT
!></td><td>--></td><td>SECOND DIMENSION OF SUBMIT AND MOTCAR
!>    </td></tr>
!>          <tr><td>SUBMIT
!></td><td>--></td><td>CHARACTER STRINGS STEMMING FROM DICTIONARY
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE READ_SUBMIT
     &(FILES,NFILES,CODE,SUBMIT,NMOT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CODE           |-->| NAME OF CALLING PROGRAMME
C| FILES          |-->| FILES STRUCTURES
C| NFILES         |-->| NUMBER OF FILES IN ARRAY FILES
C| NMOT           |-->| SECOND DIMENSION OF SUBMIT AND MOTCAR
C| SUBMIT         |-->| CHARACTER STRINGS STEMMING FROM DICTIONARY
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_READ_SUBMIT => READ_SUBMIT
      USE DECLARATIONS_TELEMAC
C
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER           , INTENT(IN) :: NFILES,NMOT
      TYPE(BIEF_FILE), INTENT(INOUT) :: FILES(NFILES)
      CHARACTER(LEN=24) , INTENT(IN) :: CODE
      CHARACTER(LEN=144), INTENT(IN) :: SUBMIT(4,NMOT)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,ICOL,I1,CANAL
C
      CHARACTER(LEN=7) :: NOMCANAL
      CHARACTER(LEN=9) :: LITECR
C
      INTEGER  PREVAL,INTLU
      EXTERNAL PREVAL,INTLU
C
C-----------------------------------------------------------------------
C
      DO I=1,NFILES
        FILES(I)%LU=0
        FILES(I)%TELNAME='      '
        FILES(I)%NAME(1:1)=' '
      ENDDO
C
C-----------------------------------------------------------------------
C
C     DECODES THE SUBMIT STRING FOR THE FILES DECLARED IN THE
C     STEERING FILE
C
      DO I=1,NMOT
C
C EXAMPLE SUBMIT STRING : 'NGEO-READ-01;T2DGEO;OBLIG;BIN;LIT;SELAFIN-GEOM'
C
        IF(     SUBMIT(4,I).NE.' '
C       IF(     SUBMIT(4,I).NE.' '.AND.MOTCAR(I)(1:1).NE.' '
     &     .AND.SUBMIT(4,I)(1:7).NE.'INUTILE'  ) THEN
C         SCANS FOR CHANNEL FORTRAN NAME (FOR EXAMPLE NGEO)
          ICOL=PREVAL(1,SUBMIT(4,I),'-','-','-')
          NOMCANAL=SUBMIT(4,I)(1:ICOL-1)
C         SCANS FOR THE READ OR WRITE OR READWRITE STRING
C         LOCATED BEFORE THE NEXT - SIGN
          I1=ICOL+1
          ICOL=PREVAL(I1,SUBMIT(4,I),'-','-','-')
          LITECR=SUBMIT(4,I)(I1:ICOL-1)
C         READS THE CHANNEL AFTER THE - SIGN
          CANAL=INTLU(ICOL,SUBMIT(4,I))
          IF(CANAL.GT.NFILES) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'READ_SUBMIT : NFILES TROP PETIT : ',NFILES
              WRITE(LU,*) '              IL FAUT AU MOINS ',CANAL
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'READ_SUBMIT: NFILES TOO SMALL : ',NFILES
              WRITE(LU,*) '             IT SHOULD BE AT LEAST ',CANAL
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          FILES(CANAL)%LU=CANAL
          FILES(CANAL)%ACTION=LITECR
C
          ICOL=PREVAL(ICOL,SUBMIT(4,I),';',';',';')
C         READS THE NAME OF THE FILE TO BE COPIED TO THE TMP FOLDER
          I1=PREVAL(ICOL+1,SUBMIT(4,I),';',';',';')
          FILES(CANAL)%TELNAME=SUBMIT(4,I)(ICOL+1:I1-1)
C         SKIPS ;FACUL; OR ;OBLIG;
          ICOL=PREVAL(I1+1,SUBMIT(4,I),';',';',';')
C         BINARY OR ASCII
          FILES(CANAL)%BINASC=SUBMIT(4,I)(ICOL+1:ICOL+3)
C         NOTE : SUBMIT(4,I)(ICOL+5:ICOL+7) CONTAINS LIT OU ECR
C                NOT USED HERE
C         MODE SELAFIN-GEOM, PARAL, SCAL, ETC.
          FILES(CANAL)%TYPE=TRIM(SUBMIT(4,I)(ICOL+9:MIN(144,ICOL+20)))
C
        ENDIF
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C