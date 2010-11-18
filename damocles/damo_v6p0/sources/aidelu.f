C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    DECODES A CHARACTER STRING FROM COLUMN ICOL+1
!>             OF A LINE  (80 CHARACTERS MAXIMUM PER LINE).
!>             THIS STRING CAN RUN OVER SEVERAL LINES.
!>             AIDELU IS USED TO DECODE THE HELP SECTION OF THE
!>             DICTIONARY ONLY, AND THE WORDS IGNORED FOR EDAMOX.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN

!>  @warning  FOLLOWS THE FORTRAN CONVENTION : '' IS READ AS
!>            ' WHEN WITHIN A CHARACTER STRING IF THE STRING
!>            IS WRITTEN BETWEEN QUOTES
!>
!>  @warning  QUOTES AT THE BEGINNING AND END OF LINES ARE POSSIBLE
!>            SOURCES OF ERRORS

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DOC, ICOL, LIGNE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> DCINFO : LNG, LU<hr>
!> DCRARE : ERREUR, RETOUR<hr>
!> DCMLIG : NLIGN, LONGLI<hr>
!> DCCHIE : NFIC
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IDEB, IFIN, JCOL, PTVIRG, QUOTE, TABUL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NEXT(), PRECAR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DAMOC()

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
!>  <tr>
!>    <td><center> 5.8                                    </center></td>
!>    <td> 14/01/2008                                              </td>
!>    <td> J.M. HERVOUET (LNH); A. YESSAYAN; L. LEGUE              </td>
!>    <td> BETTER CONTROL OF 'LONG' LINES                          </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DOC
!></td><td>--></td><td>LOGIQUE DE DOCUMENTATION DE LA SORTIE
!>                  = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)
!>                  = FAUX : N'IMPRIME PAS L'AIDE
!>    </td></tr>
!>          <tr><td>ERREUR
!></td><td><--</td><td>SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
!>    </td></tr>
!>          <tr><td>ICOL
!></td><td><-></td><td>INDICE DU CARACTERE COURANT DANS LA LIGNE
!>    </td></tr>
!>          <tr><td>LIGNE
!></td><td><-></td><td>LIGNE EN COURS DE DECODAGE.
!>    </td></tr>
!>          <tr><td>LNG
!></td><td>--></td><td>NUMERO DE LA LANGUE DE DECODAGE
!>    </td></tr>
!>          <tr><td>LONGLI
!></td><td>--></td><td>LONGUEUR DES LIGNES
!>    </td></tr>
!>          <tr><td>LU
!></td><td>--></td><td>NUMERO DE L'UNITE LOGIQUE DES SORTIES
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
!>    </td></tr>
!>          <tr><td>NLIGN
!></td><td><-></td><td>NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
!>    </td></tr>
!>          <tr><td>RETOUR
!></td><td><--</td><td>SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
!>                  FIN DE FICHIER OU D'ERREUR DE LECTURE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE AIDELU
     &( ICOL , LIGNE, DOC )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DOC            |-->| LOGIQUE DE DOCUMENTATION DE LA SORTIE
C|                |   | = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)
C|                |   | = FAUX : N'IMPRIME PAS L'AIDE
C| ERREUR         |<--| SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
C| ICOL           |<->| INDICE DU CARACTERE COURANT DANS LA LIGNE
C| LIGNE          |<->| LIGNE EN COURS DE DECODAGE.
C| LNG            |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LONGLI         |-->| LONGUEUR DES LIGNES
C| LU             |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| NFIC           |-->| NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
C| NLIGN          |<->| NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
C| RETOUR         |<--| SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
C|                |   | FIN DE FICHIER OU D'ERREUR DE LECTURE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C
      INTEGER       ICOL
      LOGICAL       DOC
      CHARACTER*(*) LIGNE
C
      INTEGER  NEXT,PRECAR
      EXTERNAL NEXT,PRECAR
C
      INTEGER       LNG,LU
      INTEGER       NLIGN,LONGLI
      INTEGER       NFIC
      LOGICAL       ERREUR,RETOUR
C
C-----------------------------------------------------------------------
C
      INTEGER       IDEB,IFIN,JCOL
      CHARACTER*1   QUOTE,TABUL,PTVIRG
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR,RETOUR
      COMMON / DCMLIG / NLIGN,LONGLI
      COMMON / DCCHIE / NFIC
C
      INTRINSIC CHAR
C
C***********************************************************************
C                                   MARKS RCS AND SCCS
C
C***********************************************************************
C
      QUOTE  = ''''
      PTVIRG = ';'
      TABUL =CHAR(9)
9     ICOL   = NEXT( ICOL+1 , LIGNE )
C
C        //// FINDS THE ENDS OF THE STRING ////
C
C    NOTE: THE STRING CAN BE BETWEEN QUOTES OR NOT.
C          IF NOT, IT CANNOT CONTAIN WHITE CHARACTERS.
C
C
C
      IF ( LIGNE(ICOL:ICOL).NE.QUOTE ) THEN
           IDEB = ICOL
C                 PRECAR: SAME FUNCTION AS PREVAL, BUT DOES NOT JUMP
C                         OVER COMMENTED LINES
           ICOL = PRECAR (ICOL+1,LIGNE,' ',PTVIRG,TABUL) - 1
           IFIN = ICOL
           IF (DOC) WRITE(LU,10) LIGNE(IDEB:IFIN)
10         FORMAT(1X,A)
      ELSE
C
C IF THE STRING IS BETWEEN QUOTES
C
         IDEB = ICOL + 1
C
C WHILE THERE IS NO QUOTE ON THE LINE
C
100      ICOL = PRECAR(ICOL+1,LIGNE,QUOTE,QUOTE,QUOTE)
         IF (ICOL.GT.LONGLI) THEN
C         NO QUOTE ON THE LINE, IT'S WRITTEN OUT AND GOES TO NEXT
          IF (DOC) WRITE(LU,10) LIGNE(IDEB:LONGLI)
C         READS NEXT LINE
          READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE
          NLIGN = NLIGN + 1
          ICOL = 1
          IDEB = 1
          GO TO 100
         ELSEIF(ICOL.EQ.LONGLI) THEN
C        QUOTE AT THE END OF THE LINE, THE LINE IS WRITTEN OUT (EXCEPT
C        THE QUOTE) AND THAT'S IT
          IF (DOC) WRITE(LU,10) LIGNE(IDEB:ICOL-1)
         ELSE
C         NEXT QUOTE
          JCOL = PRECAR(ICOL+1,LIGNE,QUOTE,QUOTE,QUOTE)
C         IF THERE IS A DOUBLE QUOTE, IT IS DELETED
          IF ((JCOL-ICOL).EQ.1) THEN
            ICOL=JCOL
            LIGNE(JCOL:LONGLI)=LIGNE(JCOL+1:LONGLI) // ' '
            GO TO 100
          ELSE
C           PRINTS OUT THE 'HELP' WHEN DELETING THE LAST QUOTE
            IF (DOC) WRITE(LU,10) LIGNE(IDEB:ICOL-1)
          ENDIF
         ENDIF
      ENDIF
      ICOL = NEXT(ICOL+1,LIGNE)
      IF(ICOL.LE.LONGLI) THEN
        IF(LIGNE(ICOL:ICOL).EQ.PTVIRG(1:1)) GO TO 9
      ENDIF
      GO TO 1000
C
C WRITES OUT ERRORS
C
998   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,999) NFIC, NLIGN
999     FORMAT(1X,'UNITE LOGIQUE ',1I2,'   ERREUR LIGNE ',1I6)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,1999) NFIC, NLIGN
1999    FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR ON LINE ',1I6)
      ENDIF
900   CONTINUE
      RETOUR = .TRUE.
C
C END OF THE WRITING OF ERRORS
C
1000  CONTINUE
C
C TWO EMPTY LINES FOR THE PAGE LAYOUT
C
      IF (DOC) WRITE(LU,*) ' '
      IF (DOC) WRITE(LU,*) ' '
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C