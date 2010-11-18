C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS VALUES ACCORDING TO VARIOUS STANDARDS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  IF THE CHARACTER STRING STD EQUALS IBM OR I3E, CALLS THE
!>            SUBROUTINES LECIBM OR LECI3E, WHICH DEPEND ON THE TYPE
!>            OF MACHINE USED

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, CANAL, I, ISTAT, NVAL, STD2, TYPE, W, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> J, STD
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FIND_IN_SEL(), INIVEN(), LECDOI(), LECDON(), LECHAM(), LECSUI(), LITENR(), NOUDON(), NOUMAR(), READGEO1(), READGEO2(), READGEO3(), SKIPGEO(), SUISED(), SUITE_SERAFIN()

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
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td><--</td><td>CHAINE DE CARACTERES A LIRE
!>    </td></tr>
!>          <tr><td>CANAL
!></td><td>--></td><td>UNITE LOGIQUE POUR L'ECRITURE
!>    </td></tr>
!>          <tr><td>I
!></td><td><--</td><td>TABLEAU A LIRE S'IL EST ENTIER
!>    </td></tr>
!>          <tr><td>ISTAT
!></td><td><--</td><td>ENTIER EN CAS D'ERREUR
!>    </td></tr>
!>          <tr><td>NVAL
!></td><td>--></td><td>NOMBRE DE VALEURS DANS LE TABLEAU
!>                  OU NOMBRE DE CARACTERES DE LA CHAINE
!>    </td></tr>
!>          <tr><td>STD
!></td><td>--></td><td>STANDARD DE LECTURE : STD , IBM OU I3E
!>    </td></tr>
!>          <tr><td>STD2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TYPE
!></td><td>--></td><td>TYPE DES DONNEES A LIRE
!>    </td></tr>
!>          <tr><td>W
!></td><td><--</td><td>TABLEAU DE TRAVAIL (UTILISE EN CAS DE
!>                  CONVERSION DE SIMPLE EN DOUBLE PRECISION)
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>TABLEAU A LIRE S'IL EST REEL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LIT
     &( X , W , I , C , NVAL , TYPE , CANAL , STD2 , ISTAT )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |<--| CHAINE DE CARACTERES A LIRE
C| CANAL          |-->| UNITE LOGIQUE POUR L'ECRITURE
C| I             |<--| TABLEAU A LIRE S'IL EST ENTIER
C| ISTAT          |<--| ENTIER EN CAS D'ERREUR
C| NVAL           |-->| NOMBRE DE VALEURS DANS LE TABLEAU
C|                |   | OU NOMBRE DE CARACTERES DE LA CHAINE
C| STD            |-->| STANDARD DE LECTURE : STD , IBM OU I3E
C| STD2           |---| 
C| TYPE           |-->| TYPE DES DONNEES A LIRE
C| W             |<--| TABLEAU DE TRAVAIL (UTILISE EN CAS DE
C|                |   | CONVERSION DE SIMPLE EN DOUBLE PRECISION)
C| X             |<--| TABLEAU A LIRE S'IL EST REEL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NVAL,CANAL
      INTEGER, INTENT(INOUT)          :: ISTAT
      CHARACTER*(*), INTENT(IN)       :: TYPE,STD2
      INTEGER, INTENT(INOUT)          :: I(NVAL)
      DOUBLE PRECISION, INTENT(INOUT) :: X(NVAL)
      REAL, INTENT(INOUT)             :: W(NVAL)
      CHARACTER*(*), INTENT(INOUT)    :: C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER J
      CHARACTER(LEN=8) STD
C
      INTRINSIC DBLE,MIN,LEN
C
C-----------------------------------------------------------------------
C
      ISTAT = 0
C
C-----------------------------------------------------------------------
C
C     STD2 MAY BE SHORTER THAN 8 CHARACTERS
      STD='        '
      STD(1:MIN(8,LEN(STD2)))=STD2(1:MIN(8,LEN(STD2)))
C
C-----------------------------------------------------------------------
C
      IF(STD(1:3).EQ.'STD'.OR.STD(1:7).EQ.'SERAFIN') THEN
C
         IF(TYPE(1:2).EQ.'R4') THEN
            IF(STD(1:8).EQ.'SERAFIND') THEN
C             IF SERAFIN DOUBLE, R4 SHOULD BE R8
              READ(CANAL,END=100,ERR=101)(X(J),J=1,NVAL)
            ELSE
              READ(CANAL,END=100,ERR=101)(W(J),J=1,NVAL)
              DO J=1,NVAL
                X(J) = DBLE(W(J))
              ENDDO
            ENDIF
         ELSEIF(TYPE(1:2).EQ.'R8') THEN
            READ(CANAL,END=100,ERR=101)(X(J),J=1,NVAL)
         ELSEIF (TYPE(1:1).EQ.'I') THEN
            READ(CANAL,END=100,ERR=101)(I(J),J=1,NVAL)
         ELSEIF(TYPE(1:2).EQ.'CH') THEN
            READ(CANAL,END=100,ERR=101) C(1:NVAL)
         ELSE
            IF(LNG.EQ.1) WRITE(LU,20) TYPE
            IF(LNG.EQ.2) WRITE(LU,21) TYPE
20          FORMAT(1X,'LIT : TYPE INCONNU :',A2)
21          FORMAT(1X,'LIT : UNKNOWN TYPE :',A2)
            CALL PLANTE(1)
            STOP
         ENDIF
C
         GO TO 102
C
100      CONTINUE
         IF(LNG.EQ.1) THEN
          WRITE(LU,'(1X,A)')       'LIT : FIN DE FICHIER ANORMALE'
          WRITE(LU,'(1X,A)')       'ON VOULAIT LIRE UN'
          WRITE(LU,'(1X,A,1I6,A)') 'ENREGISTREMENT DE ',NVAL,' VALEURS'
          WRITE(LU,'(1X,A,A)')     'DE TYPE : ',TYPE
          WRITE(LU,'(1X,A,1I6)')   'SUR LE CANAL : ',CANAL
         ENDIF
         IF(LNG.EQ.2) THEN
          WRITE(LU,'(1X,A)')       'LIT : ABNORMAL END OF FILE'
          WRITE(LU,'(1X,A)')       'ONE INTENDED TO READ'
          WRITE(LU,'(1X,A,1I6,A)') 'A RECORD OF ',NVAL,' VALUES'
          WRITE(LU,'(1X,A,A)')     'OF TYPE : ',TYPE
          WRITE(LU,'(1X,A,1I6)')   'ON LOGICAL UNIT : ',CANAL
         ENDIF
C        ISTAT = -6
         CALL PLANTE(1)
         STOP
C
101      CONTINUE
         IF(LNG.EQ.1) THEN
          WRITE(LU,'(1X,A)')       'LIT : ERREUR DE LECTURE'
          WRITE(LU,'(1X,A)')       'ON VOULAIT LIRE UN'
          WRITE(LU,'(1X,A,1I6,A)') 'ENREGISTREMENT DE ',NVAL,' VALEURS'
          WRITE(LU,'(1X,A,A)')     'DE TYPE : ',TYPE
          WRITE(LU,'(1X,A,1I6)')   'SUR LE CANAL : ',CANAL
         ENDIF
         IF(LNG.EQ.2) THEN
          WRITE(LU,'(1X,A)')       'LIT : READ ERROR'
          WRITE(LU,'(1X,A)')       'ONE INTENDED TO READ'
          WRITE(LU,'(1X,A,1I6,A)') 'A RECORD OF ',NVAL,' VALUES'
          WRITE(LU,'(1X,A,A)')     'OF TYPE : ',TYPE
          WRITE(LU,'(1X,A,1I6)')   'ON LOGICAL UNIT : ',CANAL
         ENDIF
C        ISTAT = -6
         CALL PLANTE(1)
         STOP
C
102      CONTINUE
C
C-----------------------------------------------------------------------
C
C     ELSEIF(STD(1:3).EQ.'IBM') THEN
C
C        IF (TYPE(1:2).EQ.'R4') THEN
C           CALL LECIBM( W , NVAL , TYPE , CANAL )
C           DO 77 J=1,NVAL
C             X(J)=DBLE(W(J))
C77          CONTINUE
C        ELSEIF (TYPE(1:2).EQ.'R8') THEN
C           CALL LECIBM( X , NVAL , TYPE , CANAL )
C        ELSEIF (TYPE(1:1).EQ.'I') THEN
C           CALL LECIBM( I , NVAL , TYPE , CANAL )
C        ELSEIF (TYPE(1:2).EQ.'CH') THEN
C           CALL LECIBM( C , NVAL , TYPE , CANAL )
C        ELSE
C           IF(LNG.EQ.1) WRITE(LU,20) TYPE
C           IF(LNG.EQ.2) WRITE(LU,21) TYPE
C           CALL PLANTE(0)
C           STOP
C        ENDIF
C
C-----------------------------------------------------------------------
C
C     ELSEIF(STD(1:3).EQ.'I3E') THEN
C  READS R4 AND R8 - TO BE CHECKED
C        IF (TYPE(1:2).EQ.'R4') THEN
C           CALL LECI3E( W , NVAL , 'F' , CANAL , ISTAT )
C           DO 78 J=1,NVAL
C             X(J)=DBLE(W(J))
C78          CONTINUE
C        ELSEIF (TYPE(1:2).EQ.'R8') THEN
C           CALL LECI3E( X , NVAL , 'F' , CANAL , ISTAT )
C        ELSEIF (TYPE(1:1).EQ.'I') THEN
C           CALL LECI3E( I , NVAL , 'I' , CANAL , ISTAT )
C        ELSEIF (TYPE(1:2).EQ.'CH') THEN
C           CALL LECI3E( C , NVAL , 'C' , CANAL , ISTAT )
C        ELSE
C           IF(LNG.EQ.1) WRITE(LU,20) TYPE
C           IF(LNG.EQ.2) WRITE(LU,21) TYPE
C           CALL PLANTE(0)
C           STOP
C        ENDIF
C
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,10) STD
        IF(LNG.EQ.2) WRITE(LU,11) STD
10      FORMAT(1X,'LIT : STANDARD DE LECTURE INCONNU :',A8)
11      FORMAT(1X,'LIT : UNKNOWN STANDARD:',A8)
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