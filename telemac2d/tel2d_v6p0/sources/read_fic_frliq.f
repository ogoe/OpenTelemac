C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS AND INTERPOLATES VALUES FROM THE LIQUID BOUNDARY FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, LISTIN, NFIC, Q, STAT, WHAT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CHOIX, DEJA, IDEB, IFIN, IL1, IL2, ILIG, INFIC, IVALUE, IWHAT, J, LASTAT, LASTWHAT, LIGNE, MAXVAL, NLIG, NVALUE, OK, SIZELIGN, TETA, TIME, TL1, TL2, TOL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>Q(), Q3(), SL(), SL3(), TR(), TR3(), VIT(), VIT3()

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
!> </td><td> 28/06/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> SIZE OF LIGN PARAMETERIZED (SEE SIZELIGN)
!> </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 10/08/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TIME
!>    </td></tr>
!>          <tr><td>LISTIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Q
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>STAT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WHAT
!></td><td>--></td><td>VARIABLE TO LOOK FOR IN 8 CHARACTERS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE READ_FIC_FRLIQ
     &( Q , WHAT , AT , NFIC , LISTIN , STAT )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TIME
C| LISTIN         |---| 
C| NFIC           |---| 
C| Q             |---| 
C| STAT           |---| 
C| WHAT           |-->| VARIABLE TO LOOK FOR IN 8 CHARACTERS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER*8     , INTENT(IN)       :: WHAT
      DOUBLE PRECISION, INTENT(IN)       :: AT
      DOUBLE PRECISION, INTENT(INOUT)    :: Q
      INTEGER         , INTENT(IN)       :: NFIC
      LOGICAL         , INTENT(IN)       :: LISTIN
      LOGICAL         , INTENT(OUT)      :: STAT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL DEJA
      DATA DEJA /.FALSE./
C
C     MAXIMUM NUMBER OF CHARACTERS PER LIGN (MAY BE CHANGED)
C
      INTEGER, PARAMETER :: SIZELIGN = 3000
C
      INTEGER IVALUE,NVALUE,ILIG,NLIG,OK,J,IWHAT,IDEB,IFIN,IL1,IL2
      INTEGER, PARAMETER :: MAXVAL=50
      DOUBLE PRECISION TL1,TL2,TETA,TOL,LASTAT
C
      CHARACTER(LEN=SIZELIGN) :: LIGNE
      CHARACTER*8 CHOIX(MAXVAL),LASTWHAT
C
      DATA TOL /1.D-3/
C
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: INFIC
      DOUBLE PRECISION, DIMENSION(:)  , ALLOCATABLE :: TIME
C
      SAVE DEJA,INFIC,TIME,CHOIX,IL1,IL2,TL1,TL2,NVALUE,LASTWHAT,LASTAT
      SAVE NLIG
C
      INTRINSIC ABS
C
C-----------------------------------------------------------------------
C
C     1) (AT FIRST CALL)
C        READS THE LIQUID BOUNDARY FILE
C        INITIALISES CURRENT LINES AND INTERVAL OF TIME
C
      IF(.NOT.DEJA) THEN
        REWIND(NFIC)
C       SKIPS COMMENTS
1       READ(NFIC,FMT='(A)',ERR=10) LIGNE
        GO TO 20
10      CONTINUE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'ERREUR DE LECTURE DANS LE'
          WRITE(LU,*) 'FICHIER DES FRONTIERES LIQUIDES'
          WRITE(LU,*) 'SANS DOUTE UN PROBLEME DE FORMAT DE FICHIER'
          WRITE(LU,*) 'RETOURS CHARRIOTS WINDOWS SUR UNIX OU LINUX ?'
          WRITE(LU,*) 'LIGNE FAUTIVE :'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'READ ERROR IN THE'
          WRITE(LU,*) 'LIQUID BOUNDARIES FILE'
          WRITE(LU,*) 'PROBABLY A PROBLEM OF FORMAT'
          WRITE(LU,*) 'ANY WINDOWS CARRIAGE RETURNS ON UNIX OR LINUX ?'
          WRITE(LU,*) 'GUILTY LINE:'
        ENDIF
        WRITE(LU,*) LIGNE
        CALL PLANTE(1)
        STOP
20      CONTINUE
        IF(LIGNE(1:1).EQ.'#') GO TO 1
C
C       FINDS OUT WHAT AND HOW MANY VALUES ARE GIVEN IN THE FILE
C
        NVALUE = -1
        IFIN = 1
40      IDEB = IFIN
C
C       IDENTIFIES FIRST CHARACTER OF NAME
50      IF(LIGNE(IDEB:IDEB).EQ.' '.AND.IDEB.LT.SIZELIGN) THEN
          IDEB=IDEB+1
          GO TO 50
        ENDIF
C       IDENTIFIES LAST CHARACTER OF NAME
        IFIN = IDEB
60      IF(LIGNE(IFIN:IFIN).NE.' '.AND.IFIN.LT.SIZELIGN) THEN
          IFIN=IFIN+1
          GO TO 60
        ENDIF
C
        IF(IDEB.EQ.IFIN) GO TO 4
C
        NVALUE = NVALUE + 1
        IF(NVALUE.EQ.0) THEN
          IF(LIGNE(IDEB:IFIN-1).NE.'T') THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'LA PREMIERE VARIABLE DOIT ETRE LE TEMPS T'
            WRITE(LU,*) 'DANS LE FICHIER DES FRONTIERES LIQUIDES'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'FIRST VALUE MUST BE TIME, DENOTED T'
            WRITE(LU,*) 'IN FILE OF LIQUID BOUNDARIES'
          ENDIF
          CALL PLANTE(1)
          STOP
          ENDIF
        ELSEIF(NVALUE.LE.MAXVAL) THEN
          CHOIX(NVALUE)='        '
          CHOIX(NVALUE)(1:IFIN-IDEB+1)=LIGNE(IDEB:IFIN-1)
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'AUGMENTER MAXVAL DANS READ_FIC_FRLIQ'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'INCREASE MAXVAL IN READ_FIC_FRLIQ'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(IFIN.LT.SIZELIGN) GO TO 40
C
C       SKIPS THE LINE WITH UNITS OR NAMES
4       READ(NFIC,FMT='(A)',ERR=10) LIGNE
        IF(LIGNE(1:1).EQ.'#') GO TO 4
C
C       COUNTS LINES OF DATA
        NLIG = 0
998     READ(NFIC,*,END=1000,ERR=999) LIGNE
        IF(LIGNE(1:1).NE.'#') NLIG=NLIG+1
        GO TO 998
999     CONTINUE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'ERREUR DE LECTURE SUR LE FICHIER DES FRONTIERES'
          WRITE(LU,*) 'LIQUIDES A LA LIGNE DE DONNEES : ',NLIG
          WRITE(LU,*) '(COMMENTAIRES NON COMPTES)'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'READING ERROR ON THE LIQUID BOUNDARIES FILE'
          WRITE(LU,*) 'AT LINE OF DATA : ',NLIG
          WRITE(LU,*) '(COMMENTS EXCLUDED)'
        ENDIF
        CALL PLANTE(1)
        STOP
1000    CONTINUE
C
C       DYNAMICALLY ALLOCATES TIME AND INFIC
C
        ALLOCATE(TIME(NLIG),STAT=OK)
        IF(OK.NE.0) WRITE(LU,*) 'MEMORY ALLOCATION ERROR FOR TIME'
        ALLOCATE(INFIC(NVALUE,NLIG),STAT=OK)
        IF(OK.NE.0) WRITE(LU,*) 'MEMORY ALLOCATION ERROR FOR INFIC'
C
C       FINAL READ OF TIME AND INFIC
C
        REWIND(NFIC)
C       SKIPS COMMENTS AND FIRST TWO MANDATORY LINES
2       READ(NFIC,FMT='(A)') LIGNE
        IF(LIGNE(1:1).EQ.'#') GO TO 2
        READ(NFIC,FMT='(A)') LIGNE
C
        DO ILIG=1,NLIG
3         READ(NFIC,FMT='(A)') LIGNE
          IF(LIGNE(1:1).EQ.'#') THEN
            GO TO 3
          ELSE
            BACKSPACE(NFIC)
            READ(NFIC,*) TIME(ILIG),(INFIC(IVALUE,ILIG),IVALUE=1,NVALUE)
          ENDIF
        ENDDO
C
        CLOSE(NFIC)
        DEJA = .TRUE.
C
        IL1 = 1
        IL2 = 2
        TL1 = TIME(1)
        TL2 = TIME(2)
C
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LE FICHIER DES FRONTIERES LIQUIDES CONTIENT'
          WRITE(LU,*) NLIG,' LIGNES AVEC :'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'THE LIQUID BOUNDARIES FILE CONTAINS'
          WRITE(LU,*) NLIG,' LINES WITH:'
        ENDIF
        WRITE(LU,*) (CHOIX(IVALUE),IVALUE=1,NVALUE)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     2) INTERPOLATES THE DATA TO GET THE CORRECT TIME
C
C     2.A) FINDS THE ADDRESS IN THE ARRAY OF STORED DATA
C
C     2.B) INTERPOLATES DATA FROM THE ARRAY INFIC
C
C-----------------------------------------------------------------------
C
C
C     WHICH VARIABLE ?
      IWHAT = 0
      DO J=1,NVALUE
        IF(WHAT.EQ.CHOIX(J)) IWHAT=J
      ENDDO
      IF(IWHAT.EQ.0) THEN
        STAT=.FALSE.
        RETURN
      ENDIF
C
70    IF(AT.GE.TL1-TOL.AND.AT.LE.TL2+TOL) THEN
        TETA = (AT-TL1)/(TL2-TL1)
      ELSE
         DO J=1,NLIG-1
            IF(AT.GE.TIME(J)-TOL.AND.AT.LE.TIME(J+1)+TOL) THEN
               TL1=TIME(J)
               TL2=TIME(J+1)
               IL1=J
               IL2=J+1
               GO TO 70
            ENDIF
         ENDDO
         IL1=IL2
         IL2=IL2+1
        IF(IL2.GT.NLIG) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'T=',AT,' HORS LIMITES'
            WRITE(LU,*) 'DU FICHIER DES FRONTIERES LIQUIDES'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'T=',AT,' OUT OF RANGE'
            WRITE(LU,*) 'OF THE FILE OF LIQUID BOUNDARIES'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        TL1=TIME(IL1)
        TL2=TIME(IL2)
        GO TO 70
      ENDIF
C
      Q = (1.D0-TETA)*INFIC(IWHAT,IL1)
     &  +       TETA *INFIC(IWHAT,IL2)
C
      STAT=.TRUE.
C
C     PRINTS ONLY IF NEW TIME OR NEW VALUE IS ASKED
C
      IF(LISTIN) THEN
        IF(ABS(AT-LASTAT).GT.TOL.OR.LASTWHAT.NE.WHAT) THEN
          IF(LNG.EQ.1) WRITE(LU,*) 'FRONTIERE LIQUIDE : ',WHAT,'=',Q
          IF(LNG.EQ.2) WRITE(LU,*) 'LIQUID BOUNDARY: ',WHAT,'=',Q
        ENDIF
      ENDIF
      LASTAT=AT
      LASTWHAT=WHAT
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C