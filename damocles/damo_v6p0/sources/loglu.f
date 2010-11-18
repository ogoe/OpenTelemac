C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    DECODES A LOGICAL VALUE, FROM COLUMN ICOL+1 OF THE LINE.
!>             IF THE STRING IS NOT COMPLETE, GOES TO THE NEXT LINE
!>             IF NEED BE.
!>             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER.
!>             OR TO ICOL=0 IF THE NEXT LINE WAS READ.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN

!>  @warning  ACCEPTED VALUES ARE (UPPER OR LOWER CASE):
!>            VRAI OUI TRUE  YES .TRUE.  1
!>            FAUX NON FALSE NO  .FALSE. 0

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ICOL, LIGNE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> DCINFO : LNG, LU<hr>
!> DCRARE : ERREUR, RETOUR<hr>
!> DCMLIG : NLIGN, LONGLI<hr>
!> DCCHIE : NFIC
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, L, LIGNE2, LISUIV, LUFIC, TABUL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> MAJUS(), NEXT(), PRECAR()
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
!>    <td><center> 5.1                                    </center></td>
!>    <td> 16/08/1994                                              </td>
!>    <td> J.M. HERVOUET (LNH); A. YESSAYAN                        </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 15/12/1993                                              </td>
!>    <td> O. QUIQUEMPOIX (LNH) 30.87.78.70                        </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ERREUR
!></td><td><-></td><td>SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
!>    </td></tr>
!>          <tr><td>ICOL
!></td><td><-></td><td>POSITION COURANTE DU POINTEUR DANS LA LIGNE
!>    </td></tr>
!>          <tr><td>LIGNE
!></td><td><-></td><td>LIGNE EN COURS DE DECODAGE
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
!></td><td><-></td><td>SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
!>                  FIN DE FICHIER OU D'ERREUR DE LECTURE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        LOGICAL FUNCTION LOGLU
     &( ICOL , LIGNE )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ERREUR         |<->| SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
C| ICOL           |<->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
C| LIGNE          |<->| LIGNE EN COURS DE DECODAGE
C| LNG            |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LONGLI         |-->| LONGUEUR DES LIGNES
C| LU             |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| NFIC           |-->| NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
C| NLIGN          |<->| NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
C| RETOUR         |<->| SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
C|                |   | FIN DE FICHIER OU D'ERREUR DE LECTURE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER       ICOL
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
      INTEGER       I1,I2
      CHARACTER*1   TABUL
      CHARACTER*7   L
      CHARACTER*72  LIGNE2
      LOGICAL       LUFIC,LISUIV
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR , RETOUR
      COMMON / DCMLIG / NLIGN , LONGLI
      COMMON / DCCHIE / NFIC
C
      INTRINSIC CHAR
C
C***********************************************************************
C                                    RCS AND SCCS MARKING
C
C***********************************************************************
C
      LUFIC  = .FALSE.
      LISUIV = .FALSE.
      LIGNE2 = ' '
      TABUL  = CHAR(9)
C
      I1     = NEXT( ICOL+1 , LIGNE )
      L(1:7) = LIGNE(I1:I1+6)
      I2 = PRECAR(I1,LIGNE,' ',';',TABUL)
C
C CASE WHERE MIGHT HAVE TO READ THE FOLLOWING LINE
C
      IF (I2.GT.LONGLI.AND.(I1+6).GT.LONGLI) THEN
         LUFIC=.TRUE.
         READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
         IF (I1.LE.LONGLI) THEN
           L(1:7)=LIGNE(I1:LONGLI)//LIGNE2(1:(7-(LONGLI-I1+1)))
         ELSE
           L(1:7)=LIGNE2(1:7)
         ENDIF
         I2 = 0
         I2 = PRECAR(I2+1,LIGNE2,' ',';',TABUL)
      ENDIF
      CALL MAJUS(L)
      GO TO 910
C
 900  CONTINUE
      RETOUR = .TRUE.
C
 910  CONTINUE
C
C ORDERED IN THE MOST PROBABLE ORDER: NON OUI NO YES 0 1 ...
C
      IF (L(1:3).EQ.'NON') THEN
            LOGLU = .FALSE.
            ICOL = I1 + 2
      ELSE IF (L(1:2).EQ.'NO') THEN
            LOGLU = .FALSE.
            ICOL = I1 + 1
      ELSE IF ( L(1:3).EQ.'OUI' ) THEN
            LOGLU = .TRUE.
            ICOL = I1 + 2
      ELSE IF ( L(1:3).EQ.'YES' ) THEN
            LOGLU = .TRUE.
            ICOL = I1 + 2
      ELSE IF (L(1:1).EQ.'0') THEN
            LOGLU = .FALSE.
            ICOL = I1
      ELSE IF (L(1:1).EQ.'1') THEN
            LOGLU = .TRUE.
            ICOL = I1
      ELSE IF (L(1:7).EQ.'.FALSE.' ) THEN
            LOGLU = .FALSE.
            ICOL = I1 + 6
      ELSE IF (L(1:5).EQ.'FALSE' ) THEN
            LOGLU = .FALSE.
            ICOL = I1 + 4
      ELSE IF (L(1:4).EQ.'FAUX') THEN
            LOGLU = .FALSE.
            ICOL = I1 + 3
      ELSE IF ( L(1:6).EQ.'.TRUE.' ) THEN
            LOGLU = .TRUE.
            ICOL = I1 + 5
      ELSE IF ( L(1:4).EQ.'TRUE' ) THEN
            LOGLU = .TRUE.
            ICOL = I1 + 3
      ELSE IF ( L(1:4).EQ.'VRAI' ) THEN
            LOGLU = .TRUE.
            ICOL = I1 + 3
      ELSE
C
C     ERROR: NOT A LOGICAL VALUE
C
            ERREUR = .TRUE.
            WRITE(LU,'(1X,A)') LIGNE(1:LONGLI)
            IF (LUFIC) WRITE(LU,'(1X,A)') LIGNE2(1:LONGLI)
            WRITE(LU,*) ' '
            IF(LNG.EQ.1) THEN
              WRITE(LU,'(1X,A6,I4,A)') 'LOGLU (UTILE) : LIGNE: ',NLIGN,
     &                                 ' ERREUR, LOGIQUE MAL CODE'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,'(1X,A6,I4,A)') 'LOGLU (UTILE) : LINE: ',NLIGN,
     &                                 ' WRONG LOGICAL VALUE'
            ENDIF
            LOGLU = .FALSE.
            GO TO 1000
C
      ENDIF
C
C     //// UPDATES THE POINTER ////
C
      IF (LUFIC) THEN
        NLIGN = NLIGN + 1
        LIGNE = LIGNE2
        IF (ICOL.GT.LONGLI) LISUIV = .TRUE.
        IF (LISUIV) THEN
          ICOL = I2-1
        ELSE
          ICOL = 0
        ENDIF
      ELSE
        ICOL = I2 - 1
      ENDIF
C
1000  CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
C
998   CONTINUE
      IF(LNG.EQ.1) WRITE(6,999) NFIC,NLIGN+1
      IF(LNG.EQ.2) WRITE(6,1999) NFIC,NLIGN+1
999   FORMAT(1X,'UNITE LOGIQUE ',1I2,'   ERREUR LIGNE ',1I6)
1999  FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR LINE ',1I6)
      RETOUR = .TRUE.
      RETURN
      END

C
C#######################################################################
C