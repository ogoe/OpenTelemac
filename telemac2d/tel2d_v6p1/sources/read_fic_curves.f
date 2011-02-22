C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS STAGE-DISCHARGE CURVES IN THEIR FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NFIC, NFRLIQ, PTS_CURVES, STA_DIS_CURVES
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::QZ QZ@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ICURVE, IDEB, IFIN, LIGNE, NMAXPTS, OK, PASS, WHAT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 27/03/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
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
!>          <tr><td>NFRLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PTS_CURVES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>STA_DIS_CURVES
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE READ_FIC_CURVES
     &(NFIC,NFRLIQ,STA_DIS_CURVES,PTS_CURVES)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NFIC           |---| 
C| NFRLIQ         |---| 
C| PTS_CURVES     |---| 
C| STA_DIS_CURVES |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_TELEMAC2D, ONLY : QZ
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NFIC,NFRLIQ
      INTEGER, INTENT(IN)    :: STA_DIS_CURVES(NFRLIQ)
      INTEGER, INTENT(INOUT) :: PTS_CURVES(NFRLIQ)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NMAXPTS,IDEB,IFIN,ICURVE,PASS,I,OK
C
      CHARACTER(LEN=144) :: LIGNE
      CHARACTER(LEN=1)   :: WHAT
C
C-----------------------------------------------------------------------
C
      NMAXPTS=0
C     FILE WILL BE READ TWICE, THE FIRST TIME (PASS=0) TO COUNT DATA
C                              THE SECOND TIME (PASS=1) TO READ THEM
      PASS=0
C
10    CONTINUE
      REWIND(NFIC)
C     SKIPS COMMENTS
1     READ(NFIC,FMT='(A)',END=1000,ERR=999) LIGNE
      IF(LIGNE(1:1).EQ.'#') GO TO 1
C
C     NOW A LINE ANNOUNCING Q(??) OR Z(??)
C
C     IDENTIFIES FIRST CHARACTER OF NAME
2     CONTINUE
      IDEB=1
      IF(LIGNE(IDEB:IDEB).EQ.' ') THEN
        IDEB=IDEB+1
        IF(IDEB.EQ.145) THEN
          READ(NFIC,FMT='(A)') LIGNE
          IDEB=1
        ENDIF
        GO TO 2
      ENDIF
      IF(LIGNE(IDEB:IDEB+1).EQ.'Q('.OR.LIGNE(IDEB:IDEB+1).EQ.'Z(') THEN
        WHAT=LIGNE(IDEB:IDEB)
C       WHICH BOUNDARY NUMBER ?
        IDEB=IDEB+2
        IFIN=IDEB+1
3       IF(LIGNE(IFIN:IFIN).NE.')') THEN
          IFIN=IFIN+1
          IF(IFIN.GT.144) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'ERREUR DANS LE FICHIER DES COURBES DE TARAGE'
              WRITE(LU,*) 'MANQUE PARENTHESE DANS LA LIGNE :',LIGNE
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'ERROR IN THE STAGE-DISCHARGE CURVES FILE'
              WRITE(LU,*) 'MISSING PARENTHESIS IN LINE:',LIGNE
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          GO TO 3
        ENDIF
        READ(LIGNE(IDEB:IFIN-1),*) ICURVE
C       SKIPS UNITS (UNITS NOT CHECKED)
        READ(NFIC,FMT='(A)',END=1000,ERR=999) LIGNE
        PTS_CURVES(ICURVE)=0
4       READ(NFIC,FMT='(A)',END=1001,ERR=999) LIGNE
        IF(LIGNE(1:1).NE.'#') THEN
          PTS_CURVES(ICURVE)=PTS_CURVES(ICURVE)+1
          IF(PASS.EQ.1) THEN
C           READS AND STORES
            IF(WHAT.EQ.'Q') THEN
            READ(LIGNE,*,ERR=999) QZ(1,ICURVE,PTS_CURVES(ICURVE)),
     &                            QZ(2,ICURVE,PTS_CURVES(ICURVE))
            ENDIF
            IF(WHAT.EQ.'Z') THEN
            READ(LIGNE,*,ERR=999) QZ(2,ICURVE,PTS_CURVES(ICURVE)),
     &                            QZ(1,ICURVE,PTS_CURVES(ICURVE))
            ENDIF
          ENDIF
          GO TO 4
        ENDIF
C       END OF BLOCK FOR CURVE ICURVE
1001    NMAXPTS=MAX(NMAXPTS,PTS_CURVES(ICURVE))
C       TREATS THE NEXT CURVE
        GO TO 1
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'ERREUR DANS LE FICHIER DES COURBES DE TARAGE'
          WRITE(LU,*) 'LA PREMIERE LIGNE APRES COMMENTAIRES :',LIGNE
          WRITE(LU,*) 'DOIT ANNONCER Q(..) ET Z(..)'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'ERROR IN THE STAGE-DISCHARGE CURVES FILE'
          WRITE(LU,*) 'THE FIRST LINE AFTER COMMENTS:',LIGNE
          WRITE(LU,*) 'MUST ANNOUNCE Q(..) AND Z(..)'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
999   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'ERREUR DANS LE FICHIER DES COURBES DE TARAGE'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'ERROR IN THE STAGE-DISCHARGE CURVES FILE'
      ENDIF
      CALL PLANTE(1)
      STOP
1000  CONTINUE
C
C     CHECKING
C
      DO ICURVE=1,NFRLIQ
        IF(STA_DIS_CURVES(ICURVE).GT.0.AND.PTS_CURVES(ICURVE).EQ.0) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'ERREUR DANS LE FICHIER DES COURBES DE TARAGE'
            WRITE(LU,*) 'COURBE :',ICURVE,' MANQUANTE'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'ERROR IN THE STAGE-DISCHARGE CURVES FILE'
            WRITE(LU,*) 'CURVE:',ICURVE,' MISSING'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
C
C     DYNAMICALLY ALLOCATES QZ
C
      IF(PASS.EQ.0) THEN
        ALLOCATE(QZ(2,NFRLIQ,NMAXPTS),STAT=OK)
        IF(OK.NE.0) THEN
          WRITE(LU,*) 'MEMORY ALLOCATION ERROR FOR QZ'
          CALL PLANTE(1)
          STOP
        ENDIF
        PASS=1
C       SHOOT AGAIN
        GO TO 10
      ENDIF
C
C     REPORTS IN LISTING
C
      DO ICURVE=1,NFRLIQ
        IF(PTS_CURVES(ICURVE).GT.0) THEN
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) WRITE(LU,*) 'COURBE DE TARAGE :',ICURVE
          IF(LNG.EQ.2) WRITE(LU,*) 'STAGE-DISCHARGE CURVE:',ICURVE
          WRITE(LU,*) ' '
          DO I=1,PTS_CURVES(ICURVE)
            WRITE(LU,*) 'Q=',QZ(1,ICURVE,I),' Z=',QZ(2,ICURVE,I)
          ENDDO
        ENDIF
        WRITE(LU,*) ' '
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C